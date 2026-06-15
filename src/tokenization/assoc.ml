module Tokenizer = struct

  type vocabulary = (string * int) list

  let voc_size voc = List.length voc

  let add_token voc tok = 
    let max_id = voc |> List.map snd |> List.fold_left max (-1) in
    (tok, max_id+1) :: voc

  exception EncodingError of string

  let encode voc s = 
    ignore(voc, s);
    failwith "todo"

  exception DecodingError of int

  let decode voc l = 
    ignore(voc, l);
    failwith "todo"
  

  let learn _batch = failwith "not implemented"

end


module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer