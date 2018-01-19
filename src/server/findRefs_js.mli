(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_refs:
  genv: ServerEnv.genv ->
  env: ServerEnv.env ref ->
  profiling: Profiling_js.running ->
  file_input: File_input.t ->
  line: int ->
  col: int ->
  global: bool ->
  ServerProt.Response.find_refs_response * Hh_json.json option
