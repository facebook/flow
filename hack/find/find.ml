(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Core

let fold_files (type t)
    ?max_depth ?(filter=(fun _ -> true)) ?(file_only = false)
    (paths: Path.t list) (action: string -> t -> t) (init: t) =
  let rec fold depth acc dir =
    let acc = if not file_only && filter dir then action dir acc else acc in
    if max_depth = Some depth then
      acc
    else
      let files = Sys.readdir dir in
      Array.fold_left
        (fun acc file ->
           let open Unix in
           let file = Filename.concat dir file in
           match (lstat file).st_kind with
           | S_REG when filter file -> action file acc
           | S_DIR -> fold (depth+1) acc file
           | _ -> acc)
        acc files in
  let paths = List.map paths Path.to_string in
  List.fold_left paths ~init ~f:(fold 0)

let iter_files ?max_depth ?filter ?file_only paths action =
  fold_files ?max_depth ?filter ?file_only paths (fun file _ -> action file) ()

let find ?max_depth ?filter ?file_only paths =
  fold_files ?max_depth ?filter ?file_only paths List.cons []

let find_with_name ?max_depth ?file_only paths name =
  find ?max_depth ?file_only ~filter:(fun x -> x = name) paths

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

type stack =
  | Nil
  | Dir of string list * string * stack

let max_files = 1000

let make_next_files ?name:_ ?(filter = fun _ -> true) ?(others=[]) root =
  let rec process sz acc files dir stack =
    if sz >= max_files then
      (acc, Dir (files, dir, stack))
    else
      match files with
      | [] -> process_stack sz acc stack
      | file :: files ->
          let file = if dir = "" then file else Filename.concat dir file in
          let open Unix in
          match (lstat file).st_kind with
          | S_REG when filter file ->
              process (sz+1) (file :: acc) files dir stack
          | S_DIR ->
              let dirfiles = Array.to_list @@ Sys.readdir file in
              process sz acc dirfiles file (Dir (files, dir, stack))
          | _ -> process sz acc files dir stack
  and process_stack sz acc = function
    | Nil -> (acc, Nil)
    | Dir (files, dir, stack) -> process sz acc files dir stack in
  let state =
    ref (Dir (Path.to_string root ::
              List.map ~f:Path.to_string others, "", Nil)) in
  fun () ->
    let res, st = process_stack 0 [] !state in
    state := st;
    res
