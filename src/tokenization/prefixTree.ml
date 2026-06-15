module Tokenizer = struct

  type prefix_tree = Node of node

  and node = {
    mutable id: int option;
    mutable successors: (char * prefix_tree) list
  }

  type vocabulary = {
    prefix_tree: prefix_tree;
    token_of_id: string option array
  }

  let voc_size voc = 
    voc.token_of_id 
    |> Array.map (function None -> 0 | Some _ -> 1)
    |> Array.fold_left (+) 0

  exception EncodingError of string

  let rec find_best_prefix
    (prefix_tree: prefix_tree)
    (s: string)
    (s_i: int)
    (best: (int * int) option)
    (depth: int)
    : (int * int) option
    =
    (* Are we on a new id? *)
    let Node(pf) = prefix_tree in
    let new_best = match pf.id with
      | None -> best
      | Some(id) -> Some(id, depth)
    in
    (* Where do we go now?*)
    let rec find_successor
      (succ: (char * prefix_tree) list)
      (s: string)
      (s_i: int) =
      match succ with
      | [] -> None
      | (c,pf)::_tl when c = s.[s_i] -> Some(pf)
      | _::tl -> find_successor tl s s_i
    in
    (* Nowhere bc end of string *)
    if s_i = String.length s then new_best else
    (* Maybe somewhere *)
    match find_successor pf.successors s s_i with
    | None -> new_best
    | Some(succ) -> find_best_prefix succ s (s_i + 1) new_best (depth + 1)

  let encode_aux prefix_tree s = 
    let rec encore_aux_aux
      (prefix_tree: prefix_tree)
      (s: string)
      (s_i: int)
      (buffer: int list): int list =
      if String.length s = s_i then List.rev buffer else
      match find_best_prefix prefix_tree s s_i None 0 with
      | None -> raise (EncodingError (String.sub s s_i ((String.length s) - s_i)))
      | Some(id, prefix_len) -> encore_aux_aux prefix_tree s (s_i + prefix_len) (id::buffer)
    in
    encore_aux_aux prefix_tree s 0 []


  let encode voc s = encode_aux voc.prefix_tree s

  exception DecodingError of int

  let decode voc l = 
    let token_of_id = voc.token_of_id in
    let words = List.map (fun id -> 
      if id < 0 || id >= (Array.length token_of_id)
        then raise (DecodingError id)
        else
          match token_of_id.(id) with
          | None -> raise (DecodingError id)
          | Some(v) -> v
      ) l
    in String.concat "" words

  let vocabulary_of_assoc_list l =
    (*
      Pas de test, pas d'occurrence de vocabulary_of_assoc_list,
      pas présent dans l'énoncé
      Au pire, ça a l'air d'etre la fin du TP3
    *)
    ignore l;
    failwith "todo!"

  let learn _batch = failwith "not implemented" 

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer