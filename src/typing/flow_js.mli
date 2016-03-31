(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason_js

val find_constraints:
  Context.t ->
  Constraint_js.ident ->
  Constraint_js.ident * Constraint_js.constraints
val find_graph: Context.t -> Constraint_js.ident -> Constraint_js.constraints

(* propagates sources to sinks following a subtype relation *)
val flow: Context.t -> (Type.t * Type.use_t) -> unit
val flow_t: Context.t -> (Type.t * Type.t) -> unit

(* given a use type, return a tvar constrained by the use type *)
val tvar_with_constraint: Context.t -> Type.use_t -> Type.t

val unify: Context.t -> Type.t -> Type.t -> unit

val reposition: Context.t -> ?trace:Trace.t -> reason -> Type.t -> Type.t

(* constraint utils *)
val filter_optional: Context.t -> ?trace:Trace.t -> reason -> Type.t -> Type.t

module Cache: sig
  val clear: unit -> unit
  val stats: unit -> Hashtbl.statistics
end

val mk_tvar: Context.t -> reason -> Type.t
val mk_tvar_where: Context.t -> reason -> (Type.t -> unit) -> Type.t
val mk_tvar_derivable_where: Context.t -> reason -> (Type.t -> unit) -> Type.t

val get_builtin_typeapp: Context.t -> reason -> string -> Type.t list -> Type.t

(* polymorphism *)

val subst: Context.t -> ?force:bool -> (Type.t SMap.t) -> Type.t -> Type.t
val generate_tests: Context.t -> reason -> Type.typeparam list -> (Type.t SMap.t -> unit)
  -> unit

val check_polarity: Context.t -> Type.polarity -> Type.t -> unit

(* selectors *)

val eval_selector: Context.t -> reason -> Type.t -> Type.TypeTerm.selector -> int -> Type.t

(* property maps *)

val mk_propmap : Context.t -> Type.t SMap.t -> int

val has_prop : Context.t -> int -> SMap.key -> bool

val read_prop : Context.t -> int -> SMap.key -> Type.t

val write_prop : Context.t -> int -> SMap.key -> Type.t -> unit

val iter_props : Context.t -> int -> (string -> Type.t -> unit) -> unit

val visit_eval_id : Context.t -> int -> (Type.t -> unit) -> unit

(* object/method types *)

val mk_methodtype : Type.t -> Type.t list -> ?params_names:string list ->
  Type.t -> Type.funtype

val mk_methodtype2 : Type.t -> Type.t list -> ?params_names:string list ->
  Type.t -> int -> Type.funtype

val mk_functiontype : Type.t list -> ?params_names:string list -> Type.t ->
  Type.funtype

val mk_functiontype2 : Type.t list -> ?params_names:string list -> Type.t ->
  int -> Type.funtype

val dummy_this : Type.t
val dummy_static : reason -> Type.t
val dummy_prototype : Type.t

val mk_objecttype : ?flags:Type.flags ->
  Type.dicttype option -> int -> Type.t -> Type.objtype

val mk_object_with_proto : Context.t -> reason ->
  ?dict:Type.dicttype ->
  Type.t -> Type.t
val mk_object_with_map_proto : Context.t -> reason ->
  ?sealed:bool ->
  ?frozen:bool ->
  ?dict:Type.dicttype -> (Type.t SMap.t) -> Type.t -> Type.t

val mk_object: Context.t -> reason -> Type.t

(* ... *)

val mk_nominal: Context.t -> int

(* val graph: bounds IMap.t ref *)
val lookup_module: Context.t -> string -> Type.t

(* contexts *)
val fresh_context: Context.metadata -> Loc.filename -> Modulename.t -> Context.t

(* builtins *)

val builtins: Context.t -> Type.t
val get_builtin: Context.t -> string -> reason -> Type.t
val lookup_builtin: Context.t -> string -> reason -> reason option -> Type.t -> unit
val get_builtin_type: Context.t -> reason -> string -> Type.t
val resolve_builtin_class: Context.t -> Type.t -> Type.t
val set_builtin: Context.t -> string -> Type.t -> unit

val mk_instance: Context.t -> reason -> ?for_type:bool -> Type.t -> Type.t
val mk_typeof_annotation: Context.t -> ?trace:Trace.t -> Type.t -> Type.t

(* strict *)
val check_types: Context.t -> Constraint_js.ident -> (Type.t -> bool) -> bool
val enforce_strict: Context.t -> Constraint_js.ident -> unit
val merge_type: Context.t -> (Type.t * Type.t) -> Type.t
val resolve_type: Context.t -> Type.t -> Type.t
val possible_types: Context.t -> Constraint_js.ident -> Type.t list
val possible_types_of_type: Context.t -> Type.t -> Type.t list

module Autocomplete : sig
  type member_result =
    | Success of Type.t SMap.t
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  val command_result_of_member_result: member_result ->
    (Type.t SMap.t) Utils_js.command_result

  val extract_members: Context.t -> Type.t -> member_result
end
