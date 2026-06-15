open Tokenization.Characters.Tokenizer
open Utils.Characters
open Utils.Std

let print_context voc s = 
  Format.printf "voc = %s@.s = \"%s\"@.encode voc s = "
    (string_of_vocabulary voc) s

let () = 
  let voc = ['a'; 'b'; 'r'; 'c'; 'd'] in
  let s = "abracadabra" in
  print_context voc s;
  print_id_list (encode voc s);
  Format.printf "@."

let () = 
  let voc = ['a'; 'b'; 'r'; 'c'; 'd'] in
  let s = "arbre barbara" in
  print_context voc s;
  try ignore @@ encode voc s
  with EncodingError s -> 
    Format.printf "EncodingError \"%s\"@." s 