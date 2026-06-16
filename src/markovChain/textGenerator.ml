

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

module Tokenizer = Tokenization.Words.Tokenizer

let run
  ~(files: string list)
  ~(window_size: int)
  ~(output_length: int)
  : string
  = 
  (* charger les fichiers dans une liste de chaînes de caractères *)
  let texts = load_files files in
  (* calculer un vocabulaire à partir de ces textes avec le tokenizer de votre choix *)
  let voc = Tokenizer.learn texts in
  Format.printf "Voc size = %d\n" (Tokenizer.voc_size voc);
  (* encoder ces textes *)
  let encoded_texts = List.map (Tokenizer.encode voc) texts in
  Format.printf "Encoded texts size = %d\n" (List.length encoded_texts);
  (* en déduire les k-grammes d'identifiants qu'on y trouve *)
  let encoded_texts_flat = List.concat encoded_texts in 
  let ngrammes = Tokenization.Ngrammes.ngrammes window_size encoded_texts_flat in
  Format.printf "ngrammes size = %d\n" (List.length ngrammes);
  let ngrammes_flat = List.concat ngrammes in  
  Format.printf "ngrammes flat size = %d\n" (List.length ngrammes_flat);
  (* en déduire une chaîne de Markov *)
  let token_of_arc (_a:int) (b:int) = 
    Tokenizer.decode voc [b]
  in
  let chain = Learner.learn_markov_chain
    ~token_of_arc:token_of_arc
    ~max_state_id:(Tokenizer.voc_size voc)
    ~walks:ngrammes in
  (* générer un nouveau texte par marche aléatoire sur cette chaîne de Markov *)
  let new_text_ids = RandomWalk.random_walk ~length:output_length chain in
  String.concat "" new_text_ids
