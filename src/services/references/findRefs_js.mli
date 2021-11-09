(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_local_refs :
  reader:State_reader.t ->
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  file_input:File_input.t ->
  line:int ->
  col:int ->
  (FindRefsTypes.find_refs_ok, string) result
