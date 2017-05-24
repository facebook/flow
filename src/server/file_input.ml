(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
| FileName of string
| FileContent of string option * string (* filename, content *)

let path_of_file_input = function
  | FileName f -> Some f
  | FileContent (Some f, _) -> Some f
  | _ -> None

let filename_of_file_input = function
  | FileName fn -> fn
  | FileContent (Some fn, _) -> fn
  | FileContent (None, _) -> "-"

let content_of_file_input = function
  | FileName fn -> Sys_utils.cat fn
  | FileContent (_, content) -> content
