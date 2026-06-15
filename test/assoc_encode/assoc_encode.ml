open Tokenization.Assoc.Tokenizer
open Utils.Assoc
open Utils.Std

let print_context voc s = 
  Format.printf "voc = %s@.s = \"%s\"@.encode voc s = "
    (string_of_vocabulary voc) s

let () = 
  let voc = [("Hello, ", 13); ("world", 7); ("!", 12)] in
  let s = "Hello, world!!" in
  print_context voc s;
  print_id_list (encode voc s);
  Format.printf "@."

let () = 
let voc = [("world", 7); ("!", 12); ("Hello, ", 13)] in
let s = "Hello, world!!" in
print_context voc s;
print_id_list (encode voc s);
Format.printf "@."