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

open Reason_js
open Constraint_js

val infer_ast:
  Spider_monkey_ast.program ->
  string ->                     (* filename *)
  string ->                     (* module name *)
  bool ->                       (* force_check *)
  context

val infer_module: string -> context

val merge_module_list: context list -> unit

val merge_module_strict: context ->
  context list -> (context * context) list ->
  context -> unit

val mk_object: context -> reason -> Type.t

val body_loc: Spider_monkey_ast.Statement.FunctionDeclaration.body ->
    Spider_monkey_ast.Loc.t

val query_type: context -> Pos.t -> Pos.t * Type.t option * Type.t list

val fill_types: context -> (int * int * string) list

val init: string -> Spider_monkey_ast.Statement.t list -> unit
