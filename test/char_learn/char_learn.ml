open Tokenization.Characters.Tokenizer
open Utils.Characters
open Utils.Std

let runtest batch = 
  Format.printf "batch = %s@.learn batch = %s@.@."
    (string_of_list string_of_string batch)  
    (string_of_vocabulary (learn batch))

let () = runtest ["facile"]

let () = runtest ["abracadabra"; "pffouit"]

let () = runtest ["difficile"; "pas facile"]
    