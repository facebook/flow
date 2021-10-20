(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** this function takes a list and truncates it if needed to no more than
    the first n elements. If truncation happened, then the callback 'f'
    is used to generated a final element e.g. "shown 5/200" *)
let first_upto_n n f lst =
  let (first, total) =
    Base.List.fold lst ~init:([], 0) ~f:(fun (first, total) s ->
        let first =
          if total < n then
            s :: first
          else
            first
        in
        (first, total + 1)
    )
  in
  let r =
    if total <= n then
      first
    else
      match f total with
      | None -> first
      | Some e -> e :: first
  in
  Base.List.rev r

(** performs a map, but returns the original list if there is no change **)
let ident_map f lst =
  let (rev_lst, changed) =
    Base.List.fold_left
      ~f:(fun (lst_, changed) item ->
        let item_ = f item in
        (item_ :: lst_, changed || item_ != item))
      ~init:([], false)
      lst
  in
  if changed then
    Base.List.rev rev_lst
  else
    lst

let zipi xs ys = Base.List.zip_exn xs ys |> Base.List.mapi ~f:(fun i (x, y) -> (i, x, y))

(* Stringify a list given a separator and a printer for the element type *)
let to_string separator printer list = String.concat separator @@ Base.List.map ~f:printer list

(* Dedups a list in O(n) time and space. Unlike Base.List.dedup, this
   preserves order. Core's implementation is also O(n log n) *)
let dedup l =
  let tbl = Base.List.length l |> Hashtbl.create in
  let f l e =
    if Hashtbl.mem tbl e then
      l
    else (
      Hashtbl.add tbl e ();
      e :: l
    )
  in
  Base.List.fold_left ~f ~init:[] l |> Base.List.rev
