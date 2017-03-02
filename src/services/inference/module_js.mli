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

module NameSet: Set.S with type elt = Modulename.t
module NameMap: MyMap.S with type key = Modulename.t

type resolved_requires = {
  required: NameSet.t;      (* required module names *)
  require_loc: Loc.t SMap.t;  (* statement locations *)
  resolved_modules: Modulename.t SMap.t;
  phantom_dependents: SSet.t;
}

type info = {
  _module: Modulename.t;    (* module name *)
  checked: bool;            (* in flow? *)
  parsed: bool;             (* if false, it's a tracking record only *)
}

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

type error =
  | ModuleDuplicateProviderError of duplicate_provider_error

and duplicate_provider_error = {
  module_name: string;
  provider: Loc.filename;
  conflict: Loc.filename;
}

(* export and import functions for the module system *)
val exported_module:
  options: Options.t ->
  filename -> Docblock.t -> Modulename.t
val imported_module:
  options: Options.t ->
  Context.t -> Loc.t -> ?path_acc: SSet.t ref -> string -> Modulename.t

val find_resolved_module:
  (options: Options.t ->
   Context.t -> Loc.t -> string -> Modulename.t) Expensive.t

val module_exists: Modulename.t -> bool

val get_file_unsafe: (Modulename.t -> filename) Expensive.t

(* given a module name, returns either (Some filename) or None *)
val get_file: (Modulename.t -> filename option) Expensive.t

(* given a filename, returns resolved requires. unsafe *)
val get_resolved_requires_unsafe: (filename -> resolved_requires) Expensive.t
(* given a filename, returns module info. unsafe *)
val get_info_unsafe: (filename -> info) Expensive.t

(* calculate modules that might have new providers *)
val calc_dirty_modules:
  Worker.t list option ->
  options: Options.t ->
  filename list ->                    (* parsed / unparsed files *)
  NameSet.t ->                        (* cleared modules *)
    NameSet.t                           (* dirty modules *)

(* commit providers for dirty modules *)
val commit_modules:
  Worker.t list option ->
  options: Options.t ->
  NameSet.t ->                        (* dirty modules *)
    Utils_js.filename list *            (* providers *)
    error list FilenameMap.t            (* filenames to error sets *)

(* add info for parsed file to store *)
val add_parsed_info:
  (options:Options.t ->
   filename ->
   Docblock.t ->
   unit) Expensive.t

(* resolve and add requires from context to store *)
val add_parsed_resolved_requires: (options:Options.t -> Context.t -> unit) Expensive.t

(* add info for unparsed file to store *)
val add_unparsed_info:
  (options:Options.t ->
   filename ->
   Docblock.t ->
   unit) Expensive.t

(* remove module record being tracked for given file set;
   returns the set of modules removed
*)
val clear_files:
  Options.t -> Worker.t list option -> FilenameSet.t -> NameSet.t
val remove_batch_resolved_requires: FilenameSet.t -> unit

val add_package: string -> Spider_monkey_ast.program -> unit

val package_incompatible: string -> Spider_monkey_ast.program -> bool

(***************************************************)

val clear_filename_cache: unit -> unit
