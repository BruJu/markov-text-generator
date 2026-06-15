open Tokenization.PrefixTree.Tokenizer
open Utils.PrefixTree
open Utils.Std

(**
encodage du vocabulaire

nada 9
ada 5
c 1
ca 6
can 7
canal 3

**)

let prefix_tree = Node {id = None; successors = [
  ('n', Node {id = None; successors = [
    ('a', Node {id = None; successors = [
      ('d', Node {id = None; successors =[
        ('a', Node {id = Some 9; successors = []})]})]})]});
  ('a', Node {id = None; successors = [
    ('d', Node {id = None; successors = [
      ('a', Node {id = Some 5; successors = []})]})]});
  ('c',Node {id = Some 1; successors = [
    ('a', Node {id = Some 6; successors =[
      ('n', Node {id = Some 7; successors = [
        ('a', Node {id = None; successors = [
          ('l', Node {id = Some 3; successors = []})]})]})]})]})]}

let print_context prefix_tree s = 
  Format.printf "%s@.s = \"%s\"@.encode_aux prefix_tree s = "
    (string_of_prefix_tree prefix_tree) s


let () = 
  let s = "canada" in
  print_context prefix_tree s;
  print_id_list (encode_aux prefix_tree s);
  Format.printf "@."