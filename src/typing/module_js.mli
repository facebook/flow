(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

open Utils

type info = {
  file: string;             (* file name *)
  _module: string;          (* module name *)
  required: SSet.t;         (* required module names *)
  require_loc: Spider_monkey_ast.Loc.t SMap.t;  (* statement locations *)
  strict_required: SSet.t;  (* strict requires (flow to export types) *)
  checked: bool;            (* in flow? *)
}

val parse_flow: Spider_monkey_ast.Comment.t list -> bool

(* initialize to a module system, given the name of the module system *)
val init: string -> unit

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
  string list ->                        (* inferred modules *)
  SSet.t ->                             (* removed files *)
  string list * Errors_js.ErrorSet.t list

(* add file represented by context to module info store *)
val add_module_info: Constraint_js.context -> unit

(* remove module info being tracked for given file set;
   returns the set of modules removed
*)
val remove_files: SSet.t -> SSet.t
