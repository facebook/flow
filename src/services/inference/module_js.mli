(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

type resolved_requires = {
  required: Modulename.Set.t;      (* required module names *)
  require_loc: Loc.t SMap.t;  (* statement locations *)
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
    provider: Loc.filename;
    conflict: Loc.filename;
  }


val eponymous_module: filename -> Modulename.t

(* export and import functions for the module system *)
val exported_module:
  options: Options.t ->
  filename -> Docblock.t -> Modulename.t

type resolution_acc = {
  mutable paths: SSet.t;
  mutable errors: Flow_error.error_message list;
}
val imported_module:
  options: Options.t ->
  node_modules_containers: SSet.t ->
  filename -> Loc.t -> ?resolution_acc:resolution_acc -> string -> Modulename.t

val find_resolved_module:
  (filename -> string -> Modulename.t) Expensive.t

val module_exists: Modulename.t -> bool

val get_file_unsafe: (Modulename.t -> filename) Expensive.t

(* given a module name, returns either (Some filename) or None *)
val get_file: (Modulename.t -> filename option) Expensive.t

val is_tracked_file: filename -> bool

(* given a filename, returns resolved requires. unsafe *)
val get_resolved_requires_unsafe: (filename -> resolved_requires) Expensive.t

(* given a filename, returns module info *)
val get_info_unsafe: (filename -> info) Expensive.t
val get_info: (filename -> info option) Expensive.t

val checked_file: (filename -> bool) Expensive.t

(* add module records for given files;
   returns the set of modules added
*)
val introduce_files:
  Worker.t list option ->
  options: Options.t ->
  filename list ->
  (filename * Docblock.t) list ->
    (Modulename.t * filename option) list

(* remove module records being tracked for given files;
   returns the set of modules removed
*)
val clear_files:
  Worker.t list option ->
  options:Options.t ->
  FilenameSet.t ->
    (Modulename.t * filename option) list

(* repick providers for old and new modules *)
val commit_modules:
  Worker.t list option ->
  options: Options.t ->
  filename list ->                    (* parsed / unparsed files *)
  (Modulename.t * filename option) list ->      (* dirty modules *)
    Utils_js.filename list *            (* providers *)
    Modulename.Set.t *                  (* changed modules *)
    error list FilenameMap.t            (* filenames to error sets *)

(* resolve and add requires from context to store *)
val add_parsed_resolved_requires:
  (options:Options.t ->
   node_modules_containers: SSet.t ->
   filename -> Loc.t SMap.t -> Errors.ErrorSet.t) Expensive.t
(* remove resolved requires from store *)
val remove_batch_resolved_requires: FilenameSet.t -> unit

val add_package: string -> Loc.t Ast.program -> unit

val package_incompatible: string -> Loc.t Ast.program -> bool

(***************************************************)

val clear_filename_cache: unit -> unit
