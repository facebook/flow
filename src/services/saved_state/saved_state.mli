(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type denormalized_file_data = {
  package: Package_json.t option; (* Only package.json files have this *)
  file_sig: File_sig.With_Loc.t;
  resolved_requires: Module_heaps.resolved_requires;
  hash: Xx.hash;
}

type normalized_file_data

type parsed_file_data = {
  info: Module_heaps.info;
  normalized_file_data: normalized_file_data;
}

type unparsed_file_data = {
  unparsed_info: Module_heaps.info;
  unparsed_hash: Xx.hash;
}

type saved_state_data = {
  flowconfig_hash: Xx.hash;
  parsed_heaps: parsed_file_data Utils_js.FilenameMap.t;
  unparsed_heaps: unparsed_file_data Utils_js.FilenameMap.t;
  ordered_non_flowlib_libs: string list;
  local_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  warnings: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  coverage : Coverage.file_coverage Utils_js.FilenameMap.t;
  node_modules_containers: SSet.t;
}

type invalid_reason =
| Bad_header
| Build_mismatch
| Changed_files
| Failed_to_marshal
| Failed_to_decompress
| File_does_not_exist
| Flowconfig_mismatch

val invalid_reason_to_string: invalid_reason -> string

exception Invalid_saved_state of invalid_reason

val save:
  saved_state_filename:Path.t ->
  genv:ServerEnv.genv ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  unit Lwt.t

val load:
  workers:MultiWorkerLwt.worker list option ->
  saved_state_filename:Path.t ->
  options:Options.t ->
  (Profiling_js.finished * saved_state_data) Lwt.t

val denormalize_parsed_data: root:string -> normalized_file_data -> denormalized_file_data
