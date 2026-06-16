open Definitions.Tokenizer


module Make (InitialTokenizer:TOKENIZER) = struct

  let max_vocab_size = ref 1000

  type merge_rule = ((int * int) * int)
  (* exemple 
     supposons que 
        - dans le vocabulaire initial l'id de "l" est 13   
        - dans le vocabulaire initial l'id de "e" est 4   
        - on veut représenter la règle "l+e -> le" 
        - on a choisi l'id 4 pour le nouveau token "le"
      alors on utilise `((13, 4), 7)`
    *)

  type vocabulary = {
    initial_vocabulary: InitialTokenizer.vocabulary;
    merge_rules: merge_rule list
  }

  let voc_size voc = 
    InitialTokenizer.voc_size voc.initial_vocabulary +
    List.length voc.merge_rules

  exception EncodingError of string

  let encode voc s =
    (*
      A priori encodage en deux étapes :
      1/ On encode avec l'intiial tokenizer
      2/ On applique les merge rules jusqu'à stabilité
    *)
    ignore (voc, s);
    failwith "todo 1"  

  exception DecodingError of int

  let decode voc ids =
    let to_hashtable (rules: merge_rule list): (int, (int * int)) Hashtbl.t =
      let table = Hashtbl.create (List.length rules) in
      List.iter (fun ((a,b),c) -> Hashtbl.replace table c (a,b)) rules;
      table
    in
    let rec expand table id buffer: int list =
      match Hashtbl.find_opt table id with
      | None -> id :: buffer
      | Some(a,b) -> expand table b (expand table a buffer)
    in
    let rec uncompress table ids buffer: int list =
      match ids with
      | [] -> List.rev buffer
      | id::tl -> uncompress table tl (expand table id buffer)
    in
    let table = to_hashtable voc.merge_rules in
    let uncompressed = uncompress table ids [] in
    InitialTokenizer.decode voc.initial_vocabulary uncompressed

  module PairHash = struct
    type t = int * int

    let equal (a1, b1) (a2, b2) = a1 = a2 && b1 = b2

    let hash (a, b) = a * 65599 + b
  end

  module PairHashTbl = Hashtbl.Make(PairHash)

  let find_most_common (batch: int list list): (int * int * int) option =
    let table = PairHashTbl.create !max_vocab_size in
    let rec add_all_in_table table batch =
      let aux table (sequence: int list) =
        let rec follow table current rest =
          match rest with
          | [] -> ()
          | hd::tl ->
              begin
                (
                match PairHashTbl.find_opt table (current, hd) with
                | None -> PairHashTbl.replace table (current, hd) 1
                | Some(v) -> PairHashTbl.replace table (current, hd) (v + 1)
                );
                follow table hd tl
              end
        in
        match sequence with
        | [] -> ()
        | start::tl -> follow table start tl
      in
      match batch with
      | [] -> ()
      | hd::tl -> begin aux table hd; add_all_in_table table tl end
    in
    let find_greatest table =
      PairHashTbl.fold (
        fun (src, dst) v acc ->
          match acc with
          | None -> Some(src, dst, v)
          | Some(_,_,v_acc) -> if v > v_acc then Some(src,dst,v) else acc 
      ) table None
    in
    add_all_in_table table batch;
    find_greatest table

  let learn batch =
    let initial_vocabulary = InitialTokenizer.learn batch in
    let apply_new_rule (new_rule: merge_rule) (encoded_batch: int list list): int list list =
      let single_impl (new_rule: merge_rule) (l: int list) =
        let (src, dst), new_val = new_rule in

        let rec aux l acc =
          match l with
          | [] -> List.rev acc
          | x :: y :: tl when x = src && y = dst -> aux tl ( new_val :: acc )
          | x::tl -> aux tl (x::acc)
        in
        aux l [] 
      in
      let rec impl (new_rule: merge_rule) (rest: int list list) (buffer: int list list): int list list =
        match rest with
        | [] -> buffer (* We return the inverted list because maintaining the right order is useless *)
        | hd::tl -> impl new_rule tl ((single_impl new_rule hd) :: buffer)
      in
      impl new_rule encoded_batch []
    in
    let rec improve_voc (vocab: vocabulary) (encoded_batch: int list list) =
      if voc_size vocab >= !max_vocab_size then vocab else
      match find_most_common encoded_batch with
      | None -> vocab
      | Some(s, d, v) -> if v = 1 then vocab else
        begin
          let new_rule = ((s, d), voc_size vocab) in
          let new_vocab = {
            initial_vocabulary = vocab.initial_vocabulary ;
            merge_rules = new_rule :: vocab.merge_rules
          } in
          let new_batch = apply_new_rule new_rule encoded_batch in
          improve_voc new_vocab new_batch
        end
    in
    let encoded_batch = List.map (InitialTokenizer.encode initial_vocabulary) batch
    in
    let initial_my_voc = {
      initial_vocabulary = initial_vocabulary;
      merge_rules = []
    }
    in
    improve_voc initial_my_voc encoded_batch

end

module MakeCheckType : functor (InitialTokenizer:TOKENIZER) -> TOKENIZER = Make