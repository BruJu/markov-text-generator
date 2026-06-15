open Definitions.MarkovChain
open MarkovChain.RandomWalk
open Utils.Std

let () = Random.self_init ()

let estimate_distribution sampler nb_samples =
  let res = Hashtbl.create 100 in
  let rec loop i =
    if i=nb_samples then res else
    let sample = sampler () in
    let n = try Hashtbl.find res sample with Not_found -> 0 in
    Hashtbl.replace res sample (n+1);
    loop (i+1)
  in loop 0 

let runtest length ?(start=0) nb_samples =
  Format.printf "`random_walk ~length:%d ~start:%d mc_example` renvoie@."
    length start;
  let dist = estimate_distribution (fun ()-> 
    random_walk ~length ~start mc_example |> List.map Char.escaped |> String.concat ""
    ) nb_samples in
  dist 
  |> Hashtbl.to_seq_keys |> List.of_seq |> List.sort compare 
  |> List.iter (fun walk ->
    let n = Hashtbl.find dist walk in
    let proba =  percent_of_frac n nb_samples in
    Format.printf "%s avec probabilité %d%%@." walk proba
  );
  Format.printf "@."

let () = runtest 1 5 

let () = runtest 2 100_000

let () = runtest 3 1_000_000 

let () = runtest 4 1_000_000

let () = runtest 5 1_000_000 