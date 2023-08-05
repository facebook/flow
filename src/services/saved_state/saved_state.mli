(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type denormalized_file_data = {
  requires: string array;
  resolved_modules: Parsing_heaps.resolved_module array;
  phantom_dependencies: Modulename.t array;
  exports: Exports.t;
  hash: Xx.hash;
  imports: Imports.t;
}
[@@deriving show]

type normalized_file_data [@@deriving show]

type parsed_file_data = {
  module_name: string option;
  normalized_file_data: normalized_file_data;
}
[@@deriving show]

type unparsed_file_data = {
  unparsed_module_name: string option;
  unparsed_hash: Xx.hash;
}

type package_file_data = {
  package_module_name: string option;
  package_hash: Xx.hash;
  package_info: (Package_json.t, unit) result;
}

type saved_state_dependency_graph

type saved_state_data = {
  flowconfig_hash: Xx.hash;
  parsed_heaps: (File_key.t * parsed_file_data) list;
  unparsed_heaps: (File_key.t * unparsed_file_data) list;
  package_heaps: (File_key.t * package_file_data) list;
  ordered_non_flowlib_libs: string list;
  local_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  node_modules_containers: SSet.t SMap.t;
  dependency_graph: saved_state_dependency_graph;
}

type invalid_reason =
  | Bad_header
  | Build_mismatch of {
      expected: string;
      actual: string;
    }
  | Changed_files
  | Failed_to_marshal of Exception.t
  | Failed_to_decompress of Exception.t
  | File_does_not_exist
  | Flowconfig_mismatch

val invalid_reason_to_string : invalid_reason -> string

val backtrace_of_invalid_reason : invalid_reason -> string option

exception Invalid_saved_state of invalid_reason

val save :
  saved_state_filename:File_path.t ->
  genv:ServerEnv.genv ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  unit Lwt.t

val load :
  workers:MultiWorkerLwt.worker list option ->
  saved_state_filename:File_path.t ->
  options:Options.t ->
  (Profiling_js.finished * saved_state_data) Lwt.t

val denormalize_file_data : root:string -> normalized_file_data -> denormalized_file_data

val restore_dependency_info : saved_state_dependency_graph -> Dependency_info.t
