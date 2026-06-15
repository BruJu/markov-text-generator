module Tokenizer = struct

  type vocabulary = (string * int) list

  let voc_size voc = List.length voc

  exception EncodingError of string


  let is_alpha = function 
  | 'a' .. 'z' | 'A' .. 'Z'  -> true 
  | c -> begin
    let accented_characters = 
      "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæ" ^ 
      "çèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČč" ^
      "ĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴ" ^
      "ĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚś" ^
      "ŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽž"
    in String.contains accented_characters c
  end

    
  let first_non_alpha_from (s: string) start =
    (* renvoie l'indice du premier caractère non alphabetique 
       de la chaine s en partant de l'indice start
       si ce caractère n'existe pas, renvoie la longueur de s 
       exemples:
        - `first_non_alpha_from "hello world" 3` renvoie 5
        - `first_non_alpha_from "hello world" 5` renvoie 5
        - `first_non_alpha_from "hello world" 6` renvoie 11       
       *)
    let length = String.length s in
    let rec aux s start length = 
      if start = length then length else
      if not (is_alpha s.[start]) then start
      else aux s (start + 1) length
    in aux s start length

  let alpha_blocks s = 
    (* renvoie la liste des couples mot-position de la chaine s 
      exemples:
      - `alpha_blocks "hello world"` renvoie `[("hello", 0); (" ", 5); ("world", 6)]`
      - `alpha_blocks "a-b..."` renvoie 
        `[("a", 0); ("-", 1); ("b", 2); (".", 3); (".", 4); (".", 5)]`
    *)
    let rec decoupe (s: string) (s_pos: int) (s_length: int) (buffer: (string * int) list): (string * int) list =
      if s_pos = s_length then List.rev buffer else
      if is_alpha s.[s_pos]
        then
          let the_end = first_non_alpha_from s s_pos in
          decoupe s the_end s_length (((String.sub s s_pos (the_end - s_pos)), s_pos)::buffer)
        else
          decoupe s (s_pos + 1) s_length ((String.make 1 s.[s_pos], s_pos)::buffer)
    in
    decoupe s 0 (String.length s) []

  let encode voc s =
    let blocks = alpha_blocks s in
    let rec aux voc blocks buffer =
      match blocks with
      | [] -> List.rev buffer
      | block::rest -> (
          match List.assoc_opt (fst block) voc with
          | None -> raise (EncodingError (String.sub s (snd block) ((String.length s) - (snd block))) )
          | Some(v) -> aux voc rest ((v)::buffer)
      )
    in aux voc blocks []

  exception DecodingError of int

  let decode voc ids = 
    (* I'll do a slow implementation because I'm pretty sure these functions will be unused in the final implementation *)
    let rec reverse_assoc_opt value l =
      match l with
      | [] -> None
      | (fd_key, fd_val)::_ when fd_val = value -> Some(fd_key)
      | _::tl -> reverse_assoc_opt value tl
    in
    let rec aux voc ids =
      match ids with
      | [] -> ""
      | id::rest -> (
        match reverse_assoc_opt id voc with
        | None -> raise (DecodingError id)
        | Some(str) -> str ^ (aux voc rest)
      )
    in aux voc ids

  let learn (batch: string list): vocabulary = 
    let add_in_dict (dict: vocabulary) (word: string): vocabulary =
      let rec aux dict word pos =
        match dict with
        | [] -> [word, pos]
        | hd::_ when (fst hd) = word -> dict
        | hd::tl -> hd::(aux tl word (pos + 1))
      in aux dict word 0
    in
    let rec add_multiple_in_dict dict words =
      match words with
      | [] -> dict
      | word::rest -> add_multiple_in_dict (add_in_dict dict word) rest
    in
    let rec process_words words dict =
      match words with
      | [] -> dict
      | word::rest -> (
          let cutted = alpha_blocks word in
          let mapped = List.map (fun (a, _b) -> a) cutted in
          process_words rest (add_multiple_in_dict dict mapped)
      )
    in process_words batch []
end

module TokenizerCheckType : Definitions.Tokenizer.TOKENIZER = Tokenizer