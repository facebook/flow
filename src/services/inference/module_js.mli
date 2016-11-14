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

type info = {
  _module: Modulename.t;    (* module name *)
  required: NameSet.t;      (* required module names *)
  require_loc: Loc.t SMap.t;  (* statement locations *)
  resolved_modules: Modulename.t SMap.t;
  phantom_dependents: SSet.t;
  checked: bool;            (* in flow? *)
  parsed: bool;             (* if false, it's a tracking record only *)
}

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

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

val get_file: (Modulename.t -> filename) Expensive.t

(* given a module name, returns either (Some filename) or None *)
val get_module_file: (Modulename.t -> filename option) Expensive.t

(* given a filename, returns module info. unsafe *)
val get_module_info: (filename -> info) Expensive.t

(* given a filename, returns module name *)
val get_module_names: (filename -> Modulename.t list) Expensive.t

(* given a module name, returns Some set of modules importing it or None *)
val get_reverse_imports: (Modulename.t -> NameSet.t option) Expensive.t

(* commit new and removed modules, after local inference *)
val commit_modules:
  Worker.t list option ->
  options: Options.t ->
  filename list ->                    (* inferred modules *)
  NameSet.t ->                        (* removed files *)
  Utils_js.filename list *            (* providers *)
    Errors.ErrorSet.t FilenameMap.t   (* filenames to error sets *)

(* add file represented by context to module info store *)
val add_module_info: (options:Options.t -> Context.t -> unit) Expensive.t

(* add info for unparsed file to module info store *)
val add_unparsed_info:
  (options:Options.t ->
   filename ->
   Docblock.t ->
   unit) Expensive.t

(* remove module info being tracked for given file set;
   returns the set of modules removed
*)
val remove_files:
  Options.t -> Worker.t list option -> FilenameSet.t -> NameSet.t
val clear_infos: FilenameSet.t -> unit

val add_package: string -> Spider_monkey_ast.program -> unit

val package_incompatible: string -> Spider_monkey_ast.program -> bool

(***************************************************)

val clear_filename_cache: unit -> unit
