(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val json_of_t: ?depth:int -> Context.t -> Type.t -> Hh_json.json
val jstr_of_t: ?depth:int -> Context.t -> Type.t -> string
val json_of_use_t: ?depth:int -> Context.t -> Type.use_t -> Hh_json.json
val jstr_of_use_t: ?depth:int -> Context.t -> Type.use_t -> string
val json_of_graph: ?depth:int -> Context.t -> Hh_json.json
val jstr_of_graph: ?depth:int -> Context.t -> string
val json_of_scope: ?depth:int -> Context.t -> Scope.t -> Hh_json.json
val json_of_env: ?depth:int -> Context.t -> Scope.t list -> Hh_json.json

val string_of_scope_entry: Context.t -> Scope.Entry.t -> string
val string_of_scope_entries:
  Context.t ->
  Scope.Entry.t SMap.t ->
  string
val string_of_scope_refi: Context.t -> Scope.refi_binding -> string
val string_of_scope_refis:
  Context.t ->
  Scope.refi_binding Scope.KeyMap.t ->
  string
val string_of_scope: Context.t -> Scope.t -> string

val string_of_reason: Context.t -> Reason_js.t -> string
val string_of_file: Context.t -> string
val string_of_selector: Type.TypeTerm.selector -> string
val string_of_default: Spider_monkey_ast.Expression.t Default.t -> string

val dump_t: ?depth:int -> Context.t -> Type.t -> string
val dump_use_t: ?depth:int -> Context.t -> Type.use_t -> string
val dump_reason: Context.t -> Reason_js.t -> string

val debug_flow: Type.t * Type.use_t -> string
val debug_count: (unit -> string) -> unit
