open Tokenization
open Utils.Std
open Utils.Bpe

module M = Bpe.Make(Characters.Tokenizer)
let learn = M.learn
module M2 = MakePrettyPrinter(M)
let string_of_rule = M2.string_of_merge_rule

let () = 
  Format.printf {|
TEST DE L'ALGO BPE.
⚠⚠⚠ Attention l'algo est non-déterministe! ⚠⚠⚠
Pusieurs digrammes peuvent avoir la même fréquence et
être considérés comme "plus fréquents". 
L'algo BPE doit en choisir un parmi tous.
Selon les choix faits, on obtient des vocabulaires différents.
Les tests ci-dessous évitent cette situation.
Cependant, si vous comparez votre algo avec celui d'un autre étudiant,
ou d'un prof, et que ce n'est pas sur des exemples "non ambigus" 
comme ci-dessus, méfiez-vous! Deux codes peuvent implémenter BPE 
correctement et donner néanmoins des résultats différents.@.@.
|}

let runtest_from_string s ?comment max_voc_size =
  M.max_vocab_size := max_voc_size;
  Format.printf "règles de fusion calculées par l'algo BPE sur la chaîne de caractères@.s = \"%s\"@.avec !max_vocab_size = %d@." 
    s max_voc_size;
  Option.iter (Format.printf "%s@.") comment;
  let voc = learn [s] in
  Format.printf "%s@." 
    (voc.M.merge_rules |> List.map (string_of_rule voc)
    |> List.sort compare 
    |> String.concat "\n")

let () = 
  runtest_from_string "ab,ab.ab;ab." 
    ~comment:"voir https://en.wikipedia.org/wiki/Byte_pair_encoding" 
    7

let runtest_from_batch batch ?comment max_voc_size =
  M.max_vocab_size := max_voc_size;
  Format.printf "algo BPE  sur le batch@.\"%s\"@.avec !max_vocab_size = %d@." 
    (string_of_list string_of_string batch) max_voc_size;
  Option.iter (Format.printf "%s@.") comment;
  let voc = learn batch in
  Format.printf "%s@." 
    (voc.M.merge_rules |> List.map (string_of_rule voc)
    |> List.sort compare 
    |> String.concat "\n")

let () = runtest_from_batch 
  [ "hugging"; "hug"; "hugger"; "hub"; "in"; "twin"; "sin"; "pin"]
  ~comment:"voir https://huggingface.co/learn/nlp-course/chapter6/5?fw=pt"
  16
