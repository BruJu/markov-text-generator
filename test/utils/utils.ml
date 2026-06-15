module Std = struct

  let string_of_string = Format.sprintf "\"%s\""

  let string_of_char = Format.sprintf "'%c'" 

  let string_of_list ?(sep = " ") to_string l =
    l |> List.map to_string
    |> String.concat (";" ^ sep) |> Format.sprintf "[%s]"

  let string_of_array ?(sep = " ") to_string arr =
    arr |> Array.map to_string |> Array.to_list
    |> String.concat (";" ^ sep) |> Format.sprintf "[|%s|]"    

  let string_of_option to_string = function
  | None -> "None"
  | Some x -> Format.sprintf "Some(%s)" (to_string x)

  let print_id_list l = 
    Format.printf "%s" (string_of_list string_of_int l)
      
  let percent_of_frac num den = 
    int_of_float @@ Float.round (float num /. float den *. 100.)

end

module Characters = struct
  open Std

  let string_of_vocabulary = string_of_list (Format.sprintf "'%c'")

end

module Assoc = struct
  open Std

  let string_of_vocabulary = 
    string_of_list (fun (w, id) -> Format.sprintf "(\"%s\", %d)" w id)  

end

module PrefixTree = struct
  open Std
  open Tokenization.PrefixTree.Tokenizer

  let num_counter = ref 0

  let new_num () = incr num_counter; !num_counter

  let rec string_of_node {id; successors} =
    let prelude, numbers = string_of_successors successors in
    let succs = numbers |> List.map (fun (c, num)-> 
      Format.sprintf "('%c', Node node%d)" c num
      ) in
    (prelude, Format.sprintf "{id=%s; successors=%s}"
      (string_of_option string_of_int id)
      (string_of_list (fun x -> x) succs))

  and string_of_successors l =
    l |> List.sort compare |> List.map (function (c, (Node node)) ->
      let num = new_num() in
      let (prelude, decl) = string_of_node node in
      (prelude ^ Format.sprintf "let node%d = %s in\n" num decl, (c, num))
    ) |> List.split |> fun (preludes, numbers)-> 
    (String.concat "" preludes, numbers) 

  let unpack (Node node) = node

  let string_of_prefix_tree prefix_tree = 
    num_counter := 0;
    let prelude, decl = string_of_node @@ unpack prefix_tree in
    Format.sprintf "let prefix_tree =\n%sNode %s\n"
      prelude 
      decl

  let string_of_vocabulary voc = 
    num_counter := 0;
    let prelude, decl = string_of_node @@ unpack voc.prefix_tree in
    Format.sprintf "%s{\n  token_of_id = %s;\n  prefix_tree = Node %s\n}"
      prelude 
      (string_of_array (string_of_option string_of_string) voc.token_of_id)
      decl

end

module MarkovChain = struct
  open Std
  open Definitions.MarkovChain

  let string_of_edge string_of_token {token; weight; dest} = 
    Format.sprintf "{token=%s; weight=%d; dest=%d}"
      (string_of_token token)
      weight
      dest

  let string_of_edges string_of_token edges =
    (string_of_list (string_of_edge string_of_token) (List.sort compare edges))

  let string_of_markov_chain (MarkovChain mc) = 
    Format.sprintf "@.%s@."
    (string_of_array ~sep:"\n" (string_of_edges string_of_char) mc) 
end

module Bpe = struct
  open Definitions.Tokenizer
  module MakePrettyPrinter(M:TOKENIZER) = struct
    let string_of_merge_rule voc ((id1, id2), id3) =
      Format.sprintf "%s + %s -> %s"
        (M.decode voc [id1]) (M.decode voc [id2]) (M.decode voc [id3])
  end
end