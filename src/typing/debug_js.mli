(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val json_of_t: ?size:int -> ?depth:int -> ?strip_root:Path.t option -> Context.t -> Type.t -> Hh_json.json
val jstr_of_t: ?size:int -> ?depth:int -> ?strip_root:Path.t option -> Context.t -> Type.t -> string
val json_of_use_t: ?size:int -> ?depth:int -> ?strip_root:Path.t option -> Context.t -> Type.use_t -> Hh_json.json
val jstr_of_use_t: ?size:int -> ?depth:int -> ?strip_root:Path.t option -> Context.t -> Type.use_t -> string
val json_of_graph: ?size:int -> ?depth:int -> ?strip_root:Path.t option -> Context.t -> Hh_json.json
val jstr_of_graph: ?size:int -> ?depth:int -> ?strip_root:Path.t option -> Context.t -> string
val json_of_scope: ?size:int -> ?depth:int -> ?strip_root:Path.t option -> Context.t -> Scope.t -> Hh_json.json
val json_of_env: ?size:int -> ?depth:int -> Context.t -> Scope.t list -> Hh_json.json

val string_of_scope_entry: Context.t -> Scope.Entry.t -> string
val string_of_scope_entries:
  Context.t ->
  Scope.Entry.t SMap.t ->
  string
val string_of_scope_refi: Context.t -> Scope.refi_binding -> string
val string_of_scope_refis:
  Context.t ->
  Scope.refi_binding Key_map.t ->
  string
val string_of_scope: Context.t -> Scope.t -> string

val string_of_reason: Context.t -> Reason.t -> string
val string_of_file: Context.t -> string
val string_of_selector: Type.TypeTerm.selector -> string
val string_of_destructor: Type.TypeTerm.destructor -> string
val string_of_default: (Loc.t, Loc.t) Ast.Expression.t Default.t -> string

val dump_t: ?depth:int -> Context.t -> Type.t -> string
val dump_use_t: ?depth:int -> Context.t -> Type.use_t -> string
val dump_tvar: ?depth:int -> Context.t -> Constraint.ident -> string
val dump_prop: ?depth:int -> Context.t -> Type.Property.t -> string
val dump_reason: Context.t -> Reason.t -> string
val dump_flow_error: Context.t -> Flow_error.error_message -> string
