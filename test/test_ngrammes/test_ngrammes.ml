open Tokenization.Ngrammes
open Utils.Std

let runtest k l to_string =
  Format.printf "l = %s@.ngrammes %d l = %s@.@."
    (string_of_list to_string l)
    k 
    (
      try string_of_list (string_of_list to_string) (ngrammes k l)
      with Invalid_argument s -> Format.sprintf "Invalid_argument (\"%s\")" s
    )

let () = runtest 3 ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] string_of_char

let () = runtest 2 ["longtemps"; "je"; "me"; "suis"; "couché"] string_of_string

let () = runtest 10 ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] string_of_char

let () = runtest 1 ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] string_of_char

let () = runtest 1 [] string_of_char

let () = runtest (-1) ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] string_of_char

let () = runtest 0 ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] string_of_char
