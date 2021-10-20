(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lints

type t = LintSet.t

let empty = LintSet.empty

let fold = LintSet.fold

let iter = LintSet.iter

let of_lines =
  let parse_line (label, line) =
    let line = line |> String.trim in
    match kinds_of_string line with
    | Some kinds -> Ok kinds
    | None -> Error (label, Printf.sprintf "Invalid strict mode lint \"%s\" encountered." line)
  in
  let rec loop acc = function
    | [] -> Ok acc
    | labeled_line :: labeled_lines ->
      Base.Result.bind (parse_line labeled_line) (fun kinds ->
          let acc = List.fold_left (fun acc kind -> LintSet.add kind acc) acc kinds in
          loop acc labeled_lines
      )
  in
  loop LintSet.empty
