(* Non-empty list *)

type 'a t = 'a * 'a list

let to_list (x, xs) = x::xs

let one x = (x, [])

let cons x0 (x1, xs) = (x0, x1::xs)

let iter f (x, xs) =
  f x;
  List.iter f xs

let map f (x, xs) = (f x, List.map f xs)

let rev (x, xs) =
  match List.rev (x::xs) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let rev_map f (x, xs) =
  match List.rev_map f (x::xs) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)
