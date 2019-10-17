(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Non-empty list *)

type 'a t = 'a * 'a list

let to_list (x, xs) = x :: xs

let of_list = function
  | x :: xs -> Some (x, xs)
  | [] -> None

let of_list_exn = function
  | x :: xs -> (x, xs)
  | [] -> failwith "of_list_exn given empty list"

let one x = (x, [])

let cons x0 (x1, xs) = (x0, x1 :: xs)

let mem ~equal y (x1, xs) = equal x1 y || Core_list.mem ~equal xs y

let exists f (x1, xs) = f x1 || Core_list.exists ~f xs

let iter f (x, xs) =
  f x;
  Core_list.iter ~f xs

let map f (x, xs) = (f x, Core_list.map ~f xs)

let ( >>| ) l f = map f l

let ident_map f ((x, xs) as original) =
  let x' = f x in
  let xs' = ListUtils.ident_map f xs in
  if x' == x && xs' == xs then
    original
  else
    (x', xs')

let concat (xs, xss) =
  let xs = to_list xs in
  let xss = Core_list.map ~f:to_list xss in
  match Core_list.join (xs :: xss) with
  | [] -> failwith "impossible"
  | x :: xs -> (x, xs)

let map_concat f (x, xs) =
  let xss = Core_list.map ~f:(fun x -> to_list (f x)) (x :: xs) in
  match Core_list.join xss with
  | [] -> failwith "impossible"
  | x :: xs -> (x, xs)

let ( >>= ) l f = map_concat f l

let rev (x, xs) =
  match Core_list.rev (x :: xs) with
  | [] -> failwith "impossible"
  | x :: xs -> (x, xs)

let rev_map f (x, xs) =
  match Core_list.rev_map ~f (x :: xs) with
  | [] -> failwith "impossible"
  | x :: xs -> (x, xs)

let rev_append xs ys =
  match Core_list.rev_append (to_list xs) (to_list ys) with
  | [] -> failwith "impossible"
  | z :: zs -> (z, zs)

let append xs ys =
  match Core_list.append (to_list xs) (to_list ys) with
  | [] -> failwith "impossible"
  | z :: zs -> (z, zs)

let length (_, xs) = 1 + Core_list.length xs

let fold_left f acc (x, xs) = Core_list.fold_left ~f ~init:acc (x :: xs)

let hd (x, _) = x

let tl (_, xs) = xs

let nth nel n = Core_list.nth_exn (to_list nel) n

let dedup ?(compare = Pervasives.compare) (x, xs) =
  let xs = Core_list.dedup ~compare (x :: xs) in
  match xs with
  | x :: xs -> (x, xs)
  | _ -> failwith "impossible: dedup must have removed a nonduplicate"

let result_all = function
  | (Ok x, rest) ->
    begin
      match Base.Result.all rest with
      | Ok rest -> Ok (x, rest)
      | Error _ as err -> err
    end
  | ((Error _ as err), _) -> err

let cat_maybes nel =
  let rev_result =
    fold_left
      begin
        fun acc elt ->
        match (acc, elt) with
        | (_, None) -> acc
        | (None, Some x) -> Some (one x)
        | (Some lst, Some x) -> Some (cons x lst)
      end
      None
      nel
  in
  match rev_result with
  | None -> None
  | Some lst -> Some (rev lst)
