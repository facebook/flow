(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type file_kind =
  | Dir of string
  | File of string

let lstat_kind file =
  Unix.(
    try Some (lstat file).st_kind with
    | Unix_error (ENOENT, _, _) ->
      prerr_endline ("File not found: " ^ file);
      None
  )

module FileSet = Flow_set.Make (struct
  type t = file_kind

  let compare a b =
    match (a, b) with
    | (Dir a', Dir b')
    | (File a', File b') ->
      String.compare a' b'
    | (Dir _, File _) -> 1
    | (File _, Dir _) -> -1
end)

let fold_files
    (type t)
    ?max_depth
    ?(filter = (fun _ -> true))
    ?(file_only = false)
    (paths : string list)
    (action : file_kind -> t -> t)
    (init : t) =
  let rec fold depth acc dir =
    let acc =
      if (not file_only) && filter dir then
        action (Dir dir) acc
      else
        acc
    in
    if max_depth = Some depth then
      acc
    else
      let files =
        Sys.readdir dir
        |> Array.fold_left
             (fun acc file ->
               Unix.(
                 let abs = Filename.concat dir file in
                 match lstat_kind abs with
                 | Some S_REG -> FileSet.add (File abs) acc
                 | Some S_DIR -> FileSet.add (Dir abs) acc
                 | _ -> acc
               ))
             FileSet.empty
      in
      FileSet.fold
        (fun entry acc ->
          match entry with
          | File file when filter file -> action (File file) acc
          | Dir file -> fold (depth + 1) acc file
          | _ -> acc)
        files
        acc
  in
  List.fold_left (fold 0) init paths
