(* Non-empty list *)

type 'a t = 'a * 'a list

let to_list (x, xs) = x::xs

let one x = (x, [])

let cons x0 (x1, xs) = (x0, x1::xs)

let rev (x, xs) =
  match List.rev (x::xs) with
  | [] -> failwith "impossible"
  | x::xs -> (x, xs)
