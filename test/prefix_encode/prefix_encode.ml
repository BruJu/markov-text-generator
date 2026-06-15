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
  
  
let print_context prefix_tree s = 
  Format.printf "%s@.s = \"%s\"@.encode_aux prefix_tree s = "
    (string_of_prefix_tree prefix_tree) s


let () = 
  let s = "sol la si" in
  print_context prefix_tree s;
  print_id_list (encode_aux prefix_tree s);
  Format.printf "@."