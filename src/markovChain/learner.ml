

let add_occurrence
  (dst: int)
  (token: 'a)
  (list: 'a Definitions.MarkovChain.edge list)
  : 'a Definitions.MarkovChain.edge list
  =
  let rec modify_in_list
    (dst: int)
    (token: 'a)
    (list: 'a Definitions.MarkovChain.edge list)
    : bool =
    match list with
    | [] -> false
    | hd::_ when hd.dest = dst && hd.token = token ->
        begin
          hd.weight <- hd.weight + 1;
          true
        end
    | _::tl -> modify_in_list dst token tl
  in
  if modify_in_list dst token list
    then list
    else ({ token = token; weight = 1; dest = dst })::list

let learn_markov_chain
  ~(token_of_arc: int -> int -> 'a)
  ~(max_state_id: int)
  ~(walks: int list list):
  'a Definitions.MarkovChain.markov_chain =
  (* walks = liste de marches, i.e. de suites aléatoires d'états*)
  let array = Array.init (max_state_id + 1) (fun _ -> []) in

  let rec add_steps_in_array
    (array: 'a Definitions.MarkovChain.edge list array)
    (token_of_arc: int -> int -> 'a)
    (current: int)
    (rest: int list) =
    match rest with
    | [] -> ()
    | hd::tl ->
        begin
          array.(current) <- add_occurrence hd (token_of_arc current hd) array.(current);
          add_steps_in_array array token_of_arc hd tl
        end
  in
  let add_walk_in_array
    (array: 'a Definitions.MarkovChain.edge list array)
    (token_of_arc: int -> int -> 'a)
    (walk: int list)
  =
    match walk with
    | [] -> ()
    | start::rest -> add_steps_in_array array token_of_arc start rest
  in
  let rec add_walks_in_array
    (array: 'a Definitions.MarkovChain.edge list array)
    (token_of_arc: int -> int -> 'a)
    (walks: int list list) =
    match walks with
    | [] -> ()
    | walk::rest ->
        begin
          add_walk_in_array array token_of_arc walk;
          add_walks_in_array array token_of_arc rest
        end
  in
  add_walks_in_array array token_of_arc walks;
  Definitions.MarkovChain.MarkovChain(array)