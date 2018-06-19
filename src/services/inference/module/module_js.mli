(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type resolved_requires = {
  resolved_modules: Modulename.t SMap.t;
  phantom_dependents: SSet.t;
}

type info = {
  module_name: Modulename.t;
  checked: bool;            (* in flow? *)
  parsed: bool;             (* if false, it's a tracking record only *)
}

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

type error =
  | ModuleDuplicateProviderError of {
    module_name: string;
    provider: File_key.t;
    conflict: File_key.t;
  }


val eponymous_module: File_key.t -> Modulename.t

(* export and import functions for the module system *)
val exported_module:
  options: Options.t ->
  File_key.t -> Docblock.t -> Modulename.t

type resolution_acc = {
  mutable paths: SSet.t;
  mutable errors: Flow_error.error_message list;
}
val imported_module:
  options: Options.t ->
  node_modules_containers: SSet.t ->
  File_key.t -> Loc.t Nel.t -> ?resolution_acc:resolution_acc -> string -> Modulename.t

val find_resolved_module:
  (File_key.t -> string -> Modulename.t) Expensive.t

val module_exists: Modulename.t -> bool

val get_file_unsafe: (Modulename.t -> File_key.t) Expensive.t

(* given a module name, returns either (Some filename) or None *)
val get_file: (Modulename.t -> File_key.t option) Expensive.t

val is_tracked_file: File_key.t -> bool

(* given a filename, returns resolved requires. unsafe *)
val get_resolved_requires_unsafe: (File_key.t -> resolved_requires) Expensive.t

(* given a filename, returns module info *)
val get_info_unsafe: (File_key.t -> info) Expensive.t
val get_info: (File_key.t -> info option) Expensive.t

val checked_file: (File_key.t -> bool) Expensive.t

(* add module records for given files;
   returns the set of modules added
*)
val introduce_files:
  MultiWorkerLwt.worker list option ->
  options: Options.t ->
  File_key.t list ->
  (File_key.t * Docblock.t) list ->
    (Modulename.t * File_key.t option) list Lwt.t

(* remove module records being tracked for given files;
   returns the set of modules removed
*)
val clear_files:
  MultiWorkerLwt.worker list option ->
  options:Options.t ->
  FilenameSet.t ->
    (Modulename.t * File_key.t option) list Lwt.t

(* repick providers for old and new modules *)
val commit_modules:
  MultiWorkerLwt.worker list option ->
  options: Options.t ->
  FilenameSet.t ->                    (* parsed / unparsed files *)
  (Modulename.t * File_key.t option) list -> (* dirty modules *)
    (File_key.t list *                   (* providers *)
    Modulename.Set.t *                  (* changed modules *)
    error list FilenameMap.t) Lwt.t            (* filenames to error sets *)

(* resolve and add requires from context to store *)
val add_parsed_resolved_requires: (
  options:Options.t ->
  node_modules_containers: SSet.t ->
  File_key.t ->
  Errors.ErrorSet.t
) Expensive.t

(* remove resolved requires from store *)
val remove_batch_resolved_requires: FilenameSet.t -> unit

val add_package: string -> Loc.t Ast.program -> unit

val package_incompatible: string -> Loc.t Ast.program -> bool

(***************************************************)

val clear_filename_cache: unit -> unit

(* APIs mainly intended for saving and loading saved state *)
val get_package_json_for_saved_state_unsafe: string -> Package_json.t
