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

(* name of library directory defining builtins *)
val init: string option -> unit

val get_flowlib_root: unit -> string

(* names of library files defining builtins *)
val lib_files: string list ref

val is_lib_file: string -> bool
val lib_module: string

(* regexp for Filename constants *)
val dir_sep: Str.regexp
val current_dir_name: Str.regexp
val parent_dir_name: Str.regexp

(* given a root, make a filter for file names *)
val wanted: FlowConfig.config -> string -> bool

(* given a root, make a next_files function for MultiWorker *)
val make_next_files: Path.path -> unit -> string list

(* given a base directory and a relative path, return an absolute path *)
val normalize_path: string -> string -> string
