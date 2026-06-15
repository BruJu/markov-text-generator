open MarkovChain.Learner
open MarkovChain.RandomWalk
open Definitions.MarkovChain
open Utils.Std
open Utils.MarkovChain

let () = Random.self_init ()

let token_of_arc src dst = match (src, dst) with
| (0, 1) -> 'a'
| (1, 2) -> 'g'
| (1, 3) -> 'b'
| (3, 4) -> 'e'
| (3, 5) -> 'c'
| (3, 2) -> 'd'
| (4, 1) -> 'f'
| _ -> invalid_arg "token_of_arc"

let () = 
  let samples = [[0; 1]; [1; 3; 4; 1; 2]; [3; 4]; [3; 5]; [3; 5]; [3; 5]] in
  let mc_estimated = learn_markov_chain ~token_of_arc ~max_state_id: 5 ~walks: samples in
  Format.printf "learn_markov_chain token_of_arc 5 %s =@.%s@.@."
    (string_of_list (string_of_list string_of_int) samples)
    (string_of_markov_chain mc_estimated)

let runtest var_name (MarkovChain mc) string_of_token nb_samples = 
  Format.printf "apprentissage à partir de marches aléatoires aléatoires sur %s`@." var_name;
  let proba_of_arc (MarkovChain mc) src dst = 
    try
      let total_weight = 
        mc.(src) |> List.map (fun {weight; _} -> weight) |> List.fold_left (+) 0 in
      let weight = (List.find (fun {dest; _} -> dest=dst) mc.(src)).weight in
      percent_of_frac weight total_weight
    with
      _ -> 0 in
  let max_state_id = Array.length mc in
  Format.printf "echantillonage...@.";
  let samples = 
    let mc2 = mc |> Array.map (List.map (fun (edge) -> {edge with token=edge.dest})) in
    List.init nb_samples (fun _ -> 0 ::
      random_walk ~length:(Random.int 20) (MarkovChain mc2)) in
  let token_of_arc src dst = 
    (List.find (fun {dest; _} -> dest=dst) mc.(src)).token in
  Format.printf "apprentissage...@.";
  let mc_estimated = learn_markov_chain ~token_of_arc ~max_state_id ~walks: samples in
  Format.printf "résultat@.";
  for i=0 to max_state_id - 1 do
    mc.(i) |> List.iter (fun {token;dest; _} -> 
      Format.printf "arc %d-%s->%d : proba estimée %d%%, proba réelle %d%%@."
       i (string_of_token token) dest (proba_of_arc mc_estimated i dest) (proba_of_arc (MarkovChain mc) i dest)
    )
  done;
  Format.printf "@."

let () = runtest "mc_smaller_example" mc_smaller_example (fun s -> s) 1_000_000
  
let () = runtest "mc_example" mc_example Char.escaped 1_000_000