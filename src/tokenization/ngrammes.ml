
let build_first (k: int) (l: 'a list): ('a list * 'a list) option =
  let rec aux (k: int) (l: 'a list) (current: 'a list) =
    if k = 0
      then Some(List.rev current, l)
      else
        match l with
        | [] -> None
        | hd::tl -> aux (k-1) tl (hd::current)
  in aux k l []

let ngrammes (k: int) (l: 'a list) : 'a list list =
  if k <= 0 then invalid_arg "ngrammes" else
  let m = build_first k l in
  match m with
  | None -> []
  | Some(first_list, rest) ->
  let rec aux (current: 'a list) (rest: 'a list) (buffer: 'a list list): 'a list list =
    match rest with
    | [] -> List.rev buffer
    | hd::tl -> 
        let new_current = (List.tl current)@[hd] in
        aux new_current tl (new_current::buffer)
  in
  aux first_list rest [first_list]
