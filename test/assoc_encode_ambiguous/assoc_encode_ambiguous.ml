open Tokenization.Assoc.Tokenizer
open Utils.Assoc
open Utils.Std

let print_context voc s = 
  Format.printf "voc = %s@.s = \"%s\"@.encode voc s = "
    (string_of_vocabulary voc) s

let () = 
  let voc = [("ca", 13); ("can", 7); ("ada", 12); ("nada", 72)] in
  let s = "canada" in
  print_context voc s;
  print_id_list (encode voc s);
  Format.printf "@."

  let () = 
  let voc = [("c", 1); ("can", 7); ("canal", 92); ("ada", 12); ("ca", 13); ("nada", 72)] in
  let s = "canada" in
  print_context voc s;
  print_id_list (encode voc s);
  Format.printf "@."
