open Tokenization.Characters.Tokenizer
open Utils.Characters
open Utils.Std


let print_context voc ids = 
  Format.printf "voc = %s@.ids = %s@.decode voc ids = "
    (string_of_vocabulary voc) 
    (string_of_list string_of_int ids)

let () = 
  let voc = ['a'; 'b'; 'r'; 'c'; 'd'] in
  let ids = [0; 1; 2; 0; 3; 0; 4; 0; 1; 2; 0] in
  print_context voc ids;
  Format.printf "\"%s\"" (decode voc ids);
  Format.printf "@."

let () = 
  let voc = ['a'; 'b'; 'r'; 'c'; 'd'] in
  let ids = [0; 1; 7; 3] in
  print_context voc ids;
  try ignore @@ decode voc ids
  with DecodingError id -> 
    Format.printf "DecodingError %d@." id 

let () = 
  let voc = ['a'; 'b'; 'r'; 'c'; 'd'] in
  let ids = [0; -1; 0; 3] in
  print_context voc ids;
  try ignore @@ decode voc ids
  with DecodingError id -> 
    Format.printf "DecodingError %d@." id 