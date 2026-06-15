open Definitions.MarkovChain


let random_walk ~length ?(start = 0) mc =
  (* renvoie la suite de token étiquettant une marche aléatoire de longueur
     `length` au plus; si la marche aléatoire atteint un état
     sans successeurs, on s'arrête et on renvoie la marche obtenue 
  *)
  let compute_total_prob (next_steps: 'token edge list): int =
    next_steps
    |> List.map (fun e -> e.weight)
    |> List.fold_left (+) 0
  in
  let rec find_edge next_steps prob =
    match next_steps with
    | [] -> invalid_arg ("impossible case")
    | hd::tl -> if prob < hd.weight then hd else find_edge tl (prob - hd.weight)
  in
  let rec walking (current_pos: int) (length: int) (mc: 'a markov_chain) (buffer: 'a list): 'a list =
    let MarkovChain(babar) = mc in
    if length = 0 then buffer else
    let next_steps = babar.(current_pos) in
    if List.length next_steps = 0 then buffer else
    let total_prob = compute_total_prob next_steps in
    if total_prob = 0 then buffer else
    let my_prob = Random.int total_prob in
    let edge = find_edge next_steps my_prob in
    walking edge.dest (length - 1) mc ((edge.token)::buffer)
  in
  let reverse_sentence = walking start length mc [] in
  List.rev reverse_sentence