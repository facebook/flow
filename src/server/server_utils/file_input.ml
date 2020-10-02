(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | FileName of string
  | FileContent of string option * string

(* filename, content *)

let path_of_file_input = function
  | FileName f -> Some f
  | FileContent (Some f, _) -> Some f
  | _ -> None

let filename_of_file_input = function
  | FileName fn -> fn
  | FileContent (Some fn, _) -> fn
  | FileContent (None, _) -> "-"

let content_of_file_input_unsafe = function
  | FileName fn -> Sys_utils.cat fn
  | FileContent (_, content) -> content

let content_of_file_input file =
  try Ok (content_of_file_input_unsafe file)
  with exn ->
    let exn = Exception.wrap exn in
    Error (Exception.to_string exn)
