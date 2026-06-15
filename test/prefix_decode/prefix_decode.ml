open Tokenization.PrefixTree.Tokenizer
open Utils.PrefixTree
open Utils.Std

(**
encodage du vocabulaire
 0
sol 5
la 6
si 7
**)

let prefix_tree =
  let node1 = {id=Some(0); successors=[]} in
  let node3 = {id=Some(6); successors=[]} in
  let node2 = {id=None; successors=[('a', Node node3)]} in
  let node5 = {id=Some(7); successors=[]} in
  let node7 = {id=Some(5); successors=[]} in
  let node6 = {id=None; successors=[('l', Node node7)]} in
  let node4 = {id=None; successors=[('i', Node node5); ('o', Node node6)]} in
  Node {id=None; successors=[(' ', Node node1); ('l', Node node2); ('s', Node node4)]}

let voc = {
  token_of_id = [|
    Some " ";
    None;
    None;
    None;
    None;
    Some "sol";
    Some "la";
    Some "si"
  |];
  prefix_tree;
}

let () =
  Format.printf "%s@." (string_of_vocabulary voc)

let print_context _voc ids = 
  Format.printf "ids = %s@.decode voc ids = "
    (string_of_list string_of_int ids)

let () = 
  let ids = [5; 0; 6; 0; 7;] in
  print_context voc ids;
  Format.printf "\"%s\"" (decode voc ids);
  Format.printf "@."

let () = 
  let ids = [5; 3; 7] in
  print_context voc ids;
  try ignore @@ decode voc ids
  with DecodingError id -> 
    Format.printf "DecodingError %d@." id

let () = 
let ids = [5; 42; 7] in
print_context voc ids;
try ignore @@ decode voc ids
with DecodingError id -> 
  Format.printf "DecodingError %d@." id

let () = 
let ids = [5; -3; 7] in
print_context voc ids;
try ignore @@ decode voc ids
with DecodingError id -> 
  Format.printf "DecodingError %d@." id 