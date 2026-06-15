module Tokenizer = struct

  type vocabulary = char list

  let voc_size voc = List.length voc

  exception EncodingError of string

  let encode voc s =
    let build_hashtable_char_to_pos (voc: char list): (char, int) Hashtbl.t =
      let (tbl: (char, int) Hashtbl.t) = Hashtbl.create (voc_size voc) in
      List.iteri (fun pos c -> Hashtbl.add tbl c pos) voc ;
      tbl
    in
    let table = build_hashtable_char_to_pos voc in
    let rec aux table voc s (s_pos: int) (s_length: int) (buffer: int list) =
      if s_length = s_pos then List.rev buffer else
      match Hashtbl.find_opt table s.[s_pos] with
      | Some(res) -> aux table voc s (s_pos + 1) s_length (res::buffer)
      | None -> raise (EncodingError (String.sub s s_pos ((String.length s) - s_pos)))
    in
    aux table voc s 0 (String.length s) []

  exception DecodingError of int

  let decode voc ids =
    let build_hashtable_pos_to_char (voc: char list): (int, char) Hashtbl.t =
      let (tbl: (int, char) Hashtbl.t) = Hashtbl.create (voc_size voc) in
      List.iteri (fun pos c -> Hashtbl.add tbl pos c) voc ;
      tbl
    in
    let table = build_hashtable_pos_to_char voc in
    let rec aux table voc ids (buffer: char list): string =
      match ids with
      | [] -> buffer |> List.rev |> List.to_seq |> String.of_seq
      | id::tl -> (
          match Hashtbl.find_opt table id with
          | None -> raise (DecodingError id)
          | Some(c) -> aux table voc tl (c::buffer)
      )
    in
    aux table voc ids []
  
  let learn batch =
    let set = Array.make 256 false in
    let rec process_word (word: string) (i_word: int) (len_word: int) (set: bool array) (buffer: char list): char list = 
      if i_word = len_word
        then buffer
        else
          let char_pos = Char.code word.[i_word] in
          if set.(char_pos)
            then process_word word (i_word + 1) len_word set buffer
            else
              begin
                set.(char_pos) <- true;
                process_word word (i_word + 1) len_word set ((word.[i_word])::buffer) 
              end
    in
    let rec process_words (words: string list) (set: bool array) (buffer: char list): vocabulary =
      match words with
      | [] -> List.rev buffer
      | word::rest -> process_words rest set (process_word word 0 (String.length word) set buffer)
    in
    process_words batch set []
end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer