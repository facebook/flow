(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type denormalized_file_data = {
  resolved_requires: Module_heaps.resolved_requires;
  exports: Exports.t;
  hash: Xx.hash;
}

type normalized_file_data

type parsed_file_data = {
  info: Module_heaps.info;
  normalized_file_data: normalized_file_data;
  sig_hash: Xx.hash option;
}

type unparsed_file_data = {
  unparsed_info: Module_heaps.info;
  unparsed_hash: Xx.hash;
}

type saved_state_dependency_graph =
  (Utils_js.FilenameSet.t * Utils_js.FilenameSet.t) Utils_js.FilenameMap.t

type saved_state_data = {
  flowconfig_hash: Xx.hash;
  parsed_heaps: (File_key.t * parsed_file_data) list;
  unparsed_heaps: (File_key.t * unparsed_file_data) list;
  package_heaps: (Package_json.t, unit) result Utils_js.FilenameMap.t;
  ordered_non_flowlib_libs: string list;
  local_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  warnings: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  node_modules_containers: SSet.t SMap.t;
  dependency_graph: saved_state_dependency_graph;
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
  genv:ServerEnv.genv ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  unit Lwt.t

val load :
  workers:MultiWorkerLwt.worker list option ->
  saved_state_filename:Path.t ->
  options:Options.t ->
  (Profiling_js.finished * saved_state_data) Lwt.t

val denormalize_file_data : root:string -> normalized_file_data -> denormalized_file_data
