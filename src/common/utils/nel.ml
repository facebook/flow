(* Non-empty list *)

type 'a t = 'a * 'a list

let to_list (x, xs) = x::xs

let one x = (x, [])

let cons x0 (x1, xs) = (x0, x1::xs)

let iter f (x, xs) =
  f x;
  List.iter f xs

let map f (x, xs) = (f x, List.map f xs)

let concat (xs, xss) =
  let xs = to_list xs in
  let xss = List.map to_list xss in
  match List.concat (xs::xss) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let map_concat f (x, xs) =
  let xss = List.map (fun x -> to_list (f x)) (x::xs) in
  match List.concat xss with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let rev (x, xs) =
  match List.rev (x::xs) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let rev_map f (x, xs) =
  match List.rev_map f (x::xs) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)

let hd (x, _) = x
