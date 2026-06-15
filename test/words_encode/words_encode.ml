open Tokenization.Words.Tokenizer
open Utils.Assoc
open Utils.Std

let print_context voc s = 
  Format.printf "voc = %s@.s = \"%s\"@.encode voc s = "
    (string_of_vocabulary voc) s

let () = 
  let voc = [("hello", 13); (" ", 4); ("world", 7); ("!", 12)] in
  let s = "hello world!!" in
  print_context voc s;
  print_id_list (encode voc s);
  Format.printf "@."

let () = 
  let voc = [("hello", 13); ("world", 7); (" ", 4); ("!", 12)] in
  let s = "hello helloworld!!" in
  print_context voc s;
  try ignore @@ encode voc s
  with EncodingError s -> 
    Format.printf "EncodingError \"%s\"@." s 