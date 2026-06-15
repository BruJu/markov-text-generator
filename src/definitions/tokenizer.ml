module type TOKENIZER = sig
  type vocabulary
  val voc_size: vocabulary -> int

  exception EncodingError of string
  val encode: vocabulary -> string -> int list

  exception DecodingError of int
  val decode: vocabulary -> int list -> string

  val learn: string list -> vocabulary
end
