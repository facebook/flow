(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type info = {
  offsets: Offset_utils.t;
  ends_in_newline: bool;
}

let info_cache : info SMap.t ref = ref SMap.empty

let info_of_file_key = function
  | File_key.LibFile file
  | File_key.SourceFile file
  | File_key.JsonFile file ->
    (match SMap.find_opt file !info_cache with
    | Some info -> Some info
    | None ->
      let contents = Sys_utils.cat file in
      let offsets = Offset_utils.make ~kind:Offset_utils.Utf8 contents in
      let ends_in_newline = contents.[String.length contents - 1] = '\n' in
      let info = { offsets; ends_in_newline } in
      info_cache := SMap.add file info !info_cache;
      Some info)
  | File_key.(ResourceFile _ | Builtins) -> None

open Base.Option.Let_syntax

let offset_table_of_file_key file_key =
  let%map { offsets; _ } = info_of_file_key file_key in
  offsets

let ends_in_newline_of_file_key file_key =
  let%map { ends_in_newline; _ } = info_of_file_key file_key in
  ends_in_newline
