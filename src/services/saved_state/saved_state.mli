(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  parsed: Utils_js.FilenameSet.t;
  unparsed: Utils_js.FilenameSet.t;
  package_json_files: File_key.t list;
  node_modules_containers: SSet.t SMap.t;
  dependency_info: Dependency_info.t;
  ordered_non_flowlib_libs: string list;
  local_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  warnings: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  dirty_modules: Modulename.Set.t;
}

type invalid_reason =
  | Bad_header
  | Build_mismatch
  | Changed_files
  | Failed_to_marshal
  | Failed_to_decompress
  | File_does_not_exist
  | Flowconfig_mismatch

val invalid_reason_to_string : invalid_reason -> string

exception Invalid_saved_state of invalid_reason

val save :
  saved_state_filename:Path.t ->
  options:Options.t ->
  profiling:Profiling_js.running ->
  ServerEnv.env ->
  unit Lwt.t

val load : saved_state_filename:Path.t -> options:Options.t -> (Profiling_js.finished * t) Lwt.t
