module Tokenizer = struct

  type vocabulary = (string * int) list

  let voc_size voc = List.length voc

  let add_token voc tok = 
    let max_id = voc |> List.map snd |> List.fold_left max (-1) in
    (tok, max_id+1) :: voc

  let string_contains (s: string) (s_from: int) (prefix: string): bool =
    if s_from + (String.length prefix) > String.length s
      then false
      else
        let rec aux s s_i p p_i =
          if p_i = String.length p
            then true
            else (s.[s_i] = p.[p_i]) && (aux s (s_i + 1) p (p_i + 1))
        in aux s s_from prefix 0
  ;;

  exception EncodingError of string

  let encode voc s = 
    let rec find_best_prefix (voc: vocabulary) (s: string) (s_i: int) (best: (int * int) option): ((int * int) option) =
      match voc with
      | [] -> best
      | (prefix, id)::tl -> (
        let new_best = (
          match best with
          | None -> if string_contains s s_i prefix then Some(String.length prefix, id) else None
          | Some(best_len, _best_id) ->
              if (best_len < String.length prefix) && (string_contains s s_i prefix)
                then Some(String.length prefix, id)
                else best
        ) in
        find_best_prefix tl s s_i new_best
      )
    in
    let rec aux voc s s_i buffer =
      if s_i = String.length s then List.rev buffer else
      match find_best_prefix voc s s_i None with
      | None -> raise (EncodingError (String.sub s s_i ((String.length s) - s_i)))
      | Some(best_len, best_id) -> aux voc s (s_i + best_len) (best_id::buffer)
    in
    aux voc s 0 []

  exception DecodingError of int

  let decode voc l = 
    (* J'ai déjà vu ce film *)
    try
      Words.Tokenizer.decode voc l
    with 
    | Words.Tokenizer.DecodingError e -> raise (DecodingError e)

  let learn _batch = failwith "not implemented"

end


module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer