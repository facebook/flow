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

type info = {
  file: string;             (* file name *)
  _module: string;          (* module name *)
  required: SSet.t;         (* required module names *)
  require_loc: Loc.t SMap.t;  (* statement locations *)
  checked: bool;            (* in flow? *)
  parsed: bool;             (* if false, it's a tracking record only *)
}

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

val module_name_candidates: string -> string list

val parse_flow: Spider_monkey_ast.Comment.t list -> mode

(* initialize to a module system, given the name of the module system *)
val init: Options.options -> unit

(* export and import functions for the module system *)
val exported_module: string -> Spider_monkey_ast.Comment.t list -> string
val imported_module: string -> string -> string

val module_exists: string -> bool

val get_file: string -> string

(* given a module name, returns either (Some filename) or None *)
val get_module_file: string -> string option

(* given a filename, returns module info. unsafe *)
val get_module_info: string -> info

(* given a filename, returns module name *)
val get_module_name: string -> string

(* for a list of files add all imports for reverse import tracking *)
val add_reverse_imports: string list -> unit

(* given a module name, returns Some set of modules importing it or None *)
val get_reverse_imports: string -> SSet.t option

(* commit new and removed modules, after local inference *)
val commit_modules:
  string list ->                (* inferred modules *)
  SSet.t ->                     (* removed files *)
  Errors_js.ErrorSet.t SMap.t   (* filenames to error sets *)

(* add file represented by context to module info store *)
val add_module_info: Constraint_js.context -> unit

(* add info for unparsed file to module info store *)
val add_unparsed_info: string -> unit

(* remove module info being tracked for given file set;
   returns the set of modules removed
*)
val remove_files: SSet.t -> SSet.t

val add_package: string -> Errors_js.ErrorSet.t option

(***************************************************)

val clear_filename_cache: unit -> unit
