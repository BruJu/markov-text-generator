

let rec read_all ic acc =
  try
    let line = input_line ic in
    read_all ic (line::acc)
  with
  | End_of_file -> String.concat " " (List.rev acc)

let load_files (files: string list): string list =
  let rec load_files_tlr files (already_loaded: string list): string list =
    match files with
    | [] -> List.rev already_loaded
    | hd::tl ->
      begin
        try
          begin
            let ic = open_in hd in
            let content = read_all ic [] in
            close_in ic;
            load_files_tlr tl (content::already_loaded)
          end
        with
        | Sys_error _ -> 
          begin
            Format.printf "Bah il est où le fichier %s" hd;
            load_files_tlr tl already_loaded
          end
      end
  in
  load_files_tlr files []


let print_markov (markov: string Definitions.MarkovChain.markov_chain) =
  let MarkovChain(x) = markov in
  for i=0 to ((Array.length x) - 1)
  do
  begin
    let edge = x.(i) in
    Format.printf "- Node %d\n" i;
    List.iter (
      fun (e: string Definitions.MarkovChain.edge) -> 
        Format.printf "%s %d (weight= %d)\n" e.token e.dest e.weight
    ) edge;
    print_newline ()
    end
  done


(*
Manque encode de BPE
module Tokenizer = Tokenization.Bpe.Make(Tokenization.PrefixTree.Tokenizer)
*)
module Tokenizer = Tokenization.PrefixTree.Tokenizer


let ngrammes_list_to_table_state (ngrammes: int list list list)
: 
  ( int list list
  * ((string, int) Hashtbl.t)
  * int
  ) = 
  let number_of_ngrammes = List.length (List.concat ngrammes) in
  let ngramme_to_state = Hashtbl.create number_of_ngrammes in
  let state_x_state_to_token_id = Hashtbl.create (number_of_ngrammes * 2) in
  let ngramme_to_state_id_x_last_token (ngramme: int list): (int * int) =
    let rec ngramme_to_key_x_last_token (ngramme: int list) =
      match ngramme with
      | [] -> invalid_arg "impossible case"
      | [elem] -> "", elem
      | hd::tl ->
          begin
            let key, last = ngramme_to_key_x_last_token tl in
            ((string_of_int hd) ^ "," ^ key), last
          end
    in
    let key, last = ngramme_to_key_x_last_token ngramme in
    match Hashtbl.find_opt ngramme_to_state key with
    | None ->
        begin
          let new_state_id = Hashtbl.length ngramme_to_state in
          Hashtbl.replace ngramme_to_state key new_state_id;
          new_state_id, last
        end
    | Some(state) -> state, last
  in
  let aux1 (ngrammes: int list list) =
    let rec aux1_rec (current_state: int) (rest: int list list) (buffer: int list): int list =
      match rest with
      | [] -> List.rev buffer
      | hd::tl ->
          begin
            let next_state, token = ngramme_to_state_id_x_last_token hd in
            let key = (string_of_int current_state) ^ "x" ^ (string_of_int next_state) in
            Hashtbl.replace state_x_state_to_token_id key token;
            aux1_rec next_state tl (next_state::buffer)
          end
    in
    match ngrammes with
    | [] -> []
    | hd::tl ->
        begin
          let start_state, _ = ngramme_to_state_id_x_last_token hd in
          aux1_rec start_state tl [start_state]
        end
  in
  let rec aux (ngrammess: int list list list) (buffer: int list list): int list list =
    match ngrammess with
    | [] -> List.rev buffer
    | ngrammes::tl -> aux tl ((aux1 ngrammes)::buffer)
  in
  let stated_ngrammes = aux ngrammes [] in
  stated_ngrammes, state_x_state_to_token_id, (Hashtbl.length ngramme_to_state)

let run
  ~(files: string list)
  ~(window_size: int)
  ~(output_length: int)
  : string
  = 
  if window_size <= 1 then "window size should be at least 2" else
  (* charger les fichiers dans une liste de chaînes de caractères *)
  let texts = load_files files in
  (* calculer un vocabulaire à partir de ces textes avec le tokenizer de votre choix *)
  let voc = Tokenizer.learn texts in
  Format.printf "Voc size = %d\n" (Tokenizer.voc_size voc);
  (* encoder ces textes *)
  let encoded_texts = List.map (Tokenizer.encode voc) texts in
  Format.printf "Encoded texts size = %d\n" (List.length encoded_texts);
  (* en déduire les k-grammes d'identifiants qu'on y trouve *)
  let ngrammes = List.map (Tokenization.Ngrammes.ngrammes window_size) encoded_texts in
  Format.printf "ngrammes size = %d\n" (List.length ngrammes);
  let ngrammes_flat = List.concat ngrammes in  
  Format.printf "ngrammes flat size = %d\n" (List.length ngrammes_flat);
  let stated_ngrammess, token_of_arc_tbl, number_of_states = ngrammes_list_to_table_state ngrammes in 
  (* en déduire une chaîne de Markov *)
  let token_of_arc (src:int) (dst:int) = 
    let key = (string_of_int src) ^ "x" ^ (string_of_int dst) in
    Hashtbl.find token_of_arc_tbl key
  in
  let chain = Learner.learn_markov_chain
    ~token_of_arc:token_of_arc
    ~max_state_id:number_of_states
    ~walks:stated_ngrammess in
  ignore(print_markov, chain);
  (* générer un nouveau texte par marche aléatoire sur cette chaîne de Markov *)
  let new_text_tokens_id = RandomWalk.random_walk ~length:output_length chain in
  Tokenizer.decode voc new_text_tokens_id