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

type info = {
  file: filename;           (* file name *)
  _module: string;          (* module name *)
  required: SSet.t;         (* required module names *)
  require_loc: Loc.t SMap.t;  (* statement locations *)
  checked: bool;            (* in flow? *)
  parsed: bool;             (* if false, it's a tracking record only *)
}

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

val module_name_candidates: string -> string list

(* initialize to a module system, given the name of the module system *)
val init: Options.options -> unit

(* export and import functions for the module system *)
val exported_module: filename -> Docblock.t -> string
val imported_module: filename -> string -> string

val module_exists: string -> bool

val get_file: string -> filename

(* given a module name, returns either (Some filename) or None *)
val get_module_file: string -> filename option

(* given a filename, returns module info. unsafe *)
val get_module_info: filename -> info

(* given a filename, returns module name *)
val get_module_name: filename -> string

(* for a list of files add all imports for reverse import tracking *)
val add_reverse_imports: filename list -> unit

(* given a module name, returns Some set of modules importing it or None *)
val get_reverse_imports: string -> SSet.t option

(* commit new and removed modules, after local inference *)
val commit_modules:
  ?debug: bool ->
  filename list ->                    (* inferred modules *)
  SSet.t ->                           (* removed files *)
  Errors_js.ErrorSet.t FilenameMap.t  (* filenames to error sets *)

(* add file represented by context to module info store *)
val add_module_info: Context.t -> unit

(* add info for unparsed file to module info store *)
val add_unparsed_info: force_check:bool -> filename -> unit

(* remove module info being tracked for given file set;
   returns the set of modules removed
*)
val remove_files: FilenameSet.t -> SSet.t

val add_package: string -> Errors_js.ErrorSet.t option

(***************************************************)

val clear_filename_cache: unit -> unit
