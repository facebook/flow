(**
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type parsed_file_data = {
  package: Package_json.t option; (* Only package.json files have this *)
  info: Module_heaps.info;
  file_sig: File_sig.t;
  resolved_requires: Module_heaps.resolved_requires;
}

type unparsed_file_data = {
  unparsed_info: Module_heaps.info;
}

type saved_state_data = {
  flowconfig_hash: Xx.hash;
  parsed_heaps: parsed_file_data Utils_js.FilenameMap.t;
  unparsed_heaps: unparsed_file_data Utils_js.FilenameMap.t;
  ordered_non_flowlib_libs: string list;
  local_errors: Errors.ErrorSet.t Utils_js.FilenameMap.t;
  node_modules_containers: SSet.t;
}

exception Invalid_saved_state

val save:
  saved_state_filename:Path.t ->
  genv:ServerEnv.genv ->
  env:ServerEnv.env ->
  unit Lwt.t
val load:
  workers:MultiWorkerLwt.worker list option ->
  saved_state_filename:Path.t ->
  options:Options.t ->
  saved_state_data Lwt.t
