open Tokenization.Assoc.Tokenizer
open Utils.Assoc
open Utils.Std

let print_context voc ids = 
  Format.printf "voc = %s@.ids = %s@.decode voc ids = "
    (string_of_vocabulary voc) 
    (string_of_list string_of_int ids)

let () = 
  let voc = [("hello", 13); (" ", 4); ("world", 7)] in
  let ids = [13; 4; 7] in
  print_context voc ids;
  Format.printf "\"%s\"" (decode voc ids);
  Format.printf "@."

let () = 
  let voc = [("hello", 13); (" ", 4); ("world", 7)] in
  let ids = [13; 5; 7] in
  print_context voc ids;
  try ignore @@ decode voc ids
  with DecodingError id -> 
    Format.printf "DecodingError %d@." id 