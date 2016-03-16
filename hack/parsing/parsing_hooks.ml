(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

let (file_parsed_hooks:
  (Relative_path.t -> Ast.program -> unit) list ref) = ref []

let attach_file_parsed_hook hook =
  file_parsed_hooks := hook :: !file_parsed_hooks

let dispatch_file_parsed_hook fn ast =
  List.iter !file_parsed_hooks (fun hook -> hook fn ast)

let remove_all_hooks () =
  file_parsed_hooks := [];
