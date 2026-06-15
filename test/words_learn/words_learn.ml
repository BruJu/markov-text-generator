open Tokenization.Words.Tokenizer
open Utils.Assoc
open Utils.Std

let runtest batch = 
  Format.printf "batch = %s@.learn batch = %s@.@."
    (string_of_list string_of_string batch)  
    (string_of_vocabulary (learn batch))

let () = runtest ["hello world"]

let () = runtest ["Le \"coupe-faim,\""; "c'est pour la pause."; ""]