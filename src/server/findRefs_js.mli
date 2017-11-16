(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_refs:
  options: Options.t ->
  workers: Worker.t list option ->
  env: ServerEnv.env ref ->
  file_input: File_input.t ->
  line: int ->
  col: int ->
  global: bool ->
  ServerProt.Response.find_refs_response
