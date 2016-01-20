(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Utils_js

module NameSet: Set.S with type elt = Modulename.t
module NameMap: Utils.MapSig with type key = Modulename.t

type info = {
  file: filename;           (* file name *)
  _module: Modulename.t;    (* module name *)
  required: NameSet.t;      (* required module names *)
  require_loc: Loc.t SMap.t;  (* statement locations *)
  resolved_modules: Modulename.t SMap.t;
  phantom_dependents: SSet.t;
  checked: bool;            (* in flow? *)
  parsed: bool;             (* if false, it's a tracking record only *)
}

val resolution_path_dependency: FilenameSet.t -> FilenameSet.t -> filename ->
  FilenameSet.t

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

val module_name_candidates: string -> string list

(* initialize to a module system, given the name of the module system *)
val init: Options.options -> unit

(* export and import functions for the module system *)
val exported_module: filename -> Docblock.t -> Modulename.t
val imported_module: ?path_acc: SSet.t ref -> filename -> string -> Modulename.t

val find_resolved_module: filename -> string -> Modulename.t

val module_exists: Modulename.t -> bool

val get_file: Modulename.t -> filename

(* given a module name, returns either (Some filename) or None *)
val get_module_file: Modulename.t -> filename option

(* given a filename, returns module info. unsafe *)
val get_module_info: filename -> info

(* given a filename, returns module name *)
val get_module_name: filename -> Modulename.t

(* for a list of files add all imports for reverse import tracking *)
val add_reverse_imports: filename list -> unit

(* given a module name, returns Some set of modules importing it or None *)
val get_reverse_imports: Modulename.t -> NameSet.t option

(* commit new and removed modules, after local inference *)
val commit_modules:
  ?debug: bool ->
  filename list ->                    (* inferred modules *)
  NameSet.t ->                           (* removed files *)
  Errors_js.ErrorSet.t FilenameMap.t  (* filenames to error sets *)

(* add file represented by context to module info store *)
val add_module_info: Context.t -> unit

(* add info for unparsed file to module info store *)
val add_unparsed_info: force_check:bool -> filename -> unit

(* remove module info being tracked for given file set;
   returns the set of modules removed
*)
val remove_files: FilenameSet.t -> NameSet.t
val clear_infos: FilenameSet.t -> unit

val add_package: string -> Spider_monkey_ast.program -> unit

(***************************************************)

val clear_filename_cache: unit -> unit
