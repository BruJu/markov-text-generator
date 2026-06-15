open Definitions.Tokenizer


module Make (InitialTokenizer:TOKENIZER) = struct

  let max_vocab_size = ref 1000

  type merge_rule = ((int * int) * int)
  (* exemple 
     supposons que 
        - dans le vocabulaire initial l'id de "l" est 13   
        - dans le vocabulaire initial l'id de "e" est 4   
        - on veut représenter la règle "l+e -> le" 
        - on a choisi l'id 4 pour le nouveau token "le"
      alors on utilise `((13, 4), 7)`
    *)

  type vocabulary = {
    initial_vocabulary: InitialTokenizer.vocabulary;
    merge_rules: merge_rule list
  }

  let voc_size voc = 
    InitialTokenizer.voc_size voc.initial_vocabulary +
    List.length voc.merge_rules

  exception EncodingError of string

  let encode voc s =
    ignore (voc, s);
    failwith "todo"  

  exception DecodingError of int

  let decode voc ids =
    ignore (voc, ids);
    failwith "todo"  

  let learn batch =
    ignore batch;
    failwith "todo"

end

module MakeCheckType : functor (InitialTokenizer:TOKENIZER) -> TOKENIZER = Make