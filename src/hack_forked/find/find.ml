(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Hh_core

let lstat_kind file =
  Unix.(
    try Some (lstat file).st_kind
    with Unix_error (ENOENT, _, _) ->
      prerr_endline ("File not found: " ^ file);
      None)

external native_hh_readdir : string -> (string * int) list = "hh_readdir"

type dt_kind =
  | DT_REG
  | DT_DIR

(* Sys.readdir only returns `string list`, but we need to know if we have files
 * or directories, so if we use Sys.readdir we need to do an lstat on every
 * file/subdirectory. The C readdir function gives us both the name and kind,
 * so this version does 1 syscall per directory, instead of 1 syscall per file
 * and 2 per directory.
 *)
let hh_readdir path : (string * dt_kind) list =
  List.filter_map (native_hh_readdir path) (fun (name, kind) ->
      match (name, kind) with
      | (".", _) -> None
      | ("..", _) -> None
      | (".git", _) -> None
      | (".hg", _) -> None
      (* values from `man dirent` *)
      | (_, 4) -> Some (name, DT_DIR)
      | (_, 8) -> Some (name, DT_REG)
      | (_, 0) ->
        Unix.(
          (* DT_UNKNOWN - filesystem does not give us the type; do it slow *)
          (match lstat_kind (Filename.concat path name) with
          | Some S_DIR -> Some (name, DT_DIR)
          | Some S_REG -> Some (name, DT_REG)
          | _ -> None))
      | _ -> None)

let fold_files
    (type t)
    ?max_depth
    ?(filter = (fun _ -> true))
    ?(file_only = false)
    (paths : Path.t list)
    (action : string -> t -> t)
    (init : t) =
  let rec fold depth acc dir =
    let acc =
      if (not file_only) && filter dir then
        action dir acc
      else
        acc
    in
    if max_depth = Some depth then
      acc
    else
      let files = hh_readdir dir in
      List.fold_left
        ~f:(fun acc (file, kind) ->
          let file = Filename.concat dir file in
          match kind with
          | DT_REG when filter file -> action file acc
          | DT_DIR -> fold (depth + 1) acc file
          | _ -> acc)
        ~init:acc
        files
  in
  let paths = List.map paths Path.to_string in
  List.fold_left paths ~init ~f:(fold 0)

let iter_files ?max_depth ?filter ?file_only paths action =
  fold_files ?max_depth ?filter ?file_only paths (fun file _ -> action file) ()

let find ?max_depth ?filter ?file_only paths =
  List.rev @@ fold_files ?max_depth ?filter ?file_only paths List.cons []

let find_with_name ?max_depth ?file_only paths name =
  find ?max_depth ?file_only ~filter:(fun x -> x = name) paths

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

type stack =
  | Nil
  | Dir of (string * dt_kind) list * string * stack

let max_files = 1000

let make_next_files ?name:_ ?(filter = (fun _ -> true)) ?(others = []) root =
  let rec process sz (acc : string list) (files : (string * dt_kind) list) dir stack =
    if sz >= max_files then
      (acc, Dir (files, dir, stack))
    else
      match files with
      | [] -> process_stack sz acc stack
      | (name, kind) :: files ->
        let name =
          if dir = "" then
            name
          else
            Filename.concat dir name
        in
        (match kind with
        | DT_REG when filter name -> process (sz + 1) (name :: acc) files dir stack
        | DT_DIR ->
          let dirfiles = hh_readdir name in
          process sz acc dirfiles name (Dir (files, dir, stack))
        | _ -> process sz acc files dir stack)
  and process_stack sz acc = function
    | Nil -> (acc, Nil)
    | Dir (files, dir, stack) -> process sz acc files dir stack
  in
  let state =
    let dirs =
      Path.to_string root :: List.map ~f:Path.to_string others
      |> List.filter_map ~f:(fun path ->
             Unix.(
               match lstat_kind path with
               | Some S_REG -> Some (path, DT_REG)
               | Some S_DIR -> Some (path, DT_DIR)
               | _ -> None))
    in
    ref (Dir (dirs, "", Nil))
  in
  fun () ->
    let (res, st) = process_stack 0 [] !state in
    state := st;
    res
