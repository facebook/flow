(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js



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

val checked_file: (File_key.t -> bool) Expensive.t

(* add module records for given files;
   returns the set of modules added
*)
val introduce_files:
  mutator:Module_heaps.Introduce_files_mutator.t ->
  all_providers_mutator:Module_hashtables.All_providers_mutator.t ->
  workers:MultiWorkerLwt.worker list option ->
  options: Options.t ->
  parsed:File_key.t list ->
  unparsed:(File_key.t * Docblock.t) list ->
    (Modulename.t * File_key.t option) list Lwt.t

(* remove module records being tracked for given files;
   returns the set of modules removed
*)
val calc_old_modules:
  MultiWorkerLwt.worker list option ->
  all_providers_mutator:Module_hashtables.All_providers_mutator.t ->
  options:Options.t ->
  FilenameSet.t ->
    (Modulename.t * File_key.t option) list Lwt.t

(* repick providers for old and new modules *)
val commit_modules:
  transaction: Transaction.t ->
  workers: MultiWorkerLwt.worker list option ->
  options: Options.t ->
  is_init: bool ->
  FilenameSet.t ->                    (* parsed / unparsed files *)
  (Modulename.t * File_key.t option) list -> (* dirty modules *)
    (File_key.t list *                   (* providers *)
    Modulename.Set.t *                  (* changed modules *)
    error list FilenameMap.t) Lwt.t            (* filenames to error sets *)

(* resolve and add requires from context to store *)
val add_parsed_resolved_requires:
  mutator:Module_heaps.Resolved_requires_mutator.t ->
  options:Options.t ->
  node_modules_containers: SSet.t ->
  File_key.t ->
  Errors.ErrorSet.t

val add_package: string -> (Loc.t, Loc.t) Ast.program -> unit

val package_incompatible: string -> (Loc.t, Loc.t) Ast.program -> bool

(***************************************************)

val clear_filename_cache: unit -> unit

(* APIs mainly intended for saving and loading saved state *)
val introduce_files_from_saved_state:
  mutator:Module_heaps.Introduce_files_mutator.t ->
  all_providers_mutator:Module_hashtables.All_providers_mutator.t ->
  workers:MultiWorkerLwt.worker list option ->
  options: Options.t ->
  parsed:(File_key.t * Module_heaps.info) list ->
  unparsed:(File_key.t * Module_heaps.info) list ->
    (Modulename.t * File_key.t option) list Lwt.t
