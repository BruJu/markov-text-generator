module Tokenizer = struct

  type prefix_tree = Node of node

  and node = {
    mutable id: int option;
    mutable successors: (char * prefix_tree) list
  }

  type vocabulary = {
    prefix_tree: prefix_tree;
    token_of_id: string option array
  }

  let voc_size voc = 
    voc.token_of_id 
    |> Array.map (function None -> 0 | Some _ -> 1)
    |> Array.fold_left (+) 0

  exception EncodingError of string

  let encode_aux prefix_tree s = 
    ignore (prefix_tree, s);
    failwith "todo!"

  let encode voc s = encode_aux voc.prefix_tree s

  exception DecodingError of int

  let decode voc l = 
    ignore (voc, l);
    failwith "todo!"

  let vocabulary_of_assoc_list l =
    ignore l;
    failwith "todo!"

  let learn _batch = failwith "not implemented" 

end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer