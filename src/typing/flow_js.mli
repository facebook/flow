(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason

val find_constraints:
  Context.t ->
  Constraint.ident ->
  Constraint.ident * Constraint.constraints
val find_graph: Context.t -> Constraint.ident -> Constraint.constraints

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
  val stats_poly_instantiation: unit -> Hashtbl.statistics
  val summarize_flow_constraint: unit -> (string * int) list
end

val mk_tvar: Context.t -> reason -> Type.t
val mk_tvar_where: Context.t -> reason -> (Type.t -> unit) -> Type.t
val mk_tvar_derivable_where: Context.t -> reason -> (Type.t -> unit) -> Type.t

val get_builtin_typeapp: Context.t -> ?trace:Trace.t -> reason -> string -> Type.t list -> Type.t

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

val find_props : Context.t -> int -> Type.t SMap.t

val visit_eval_id : Context.t -> int -> (Type.t -> unit) -> unit

(* object/method types *)

val mk_methodtype : ?frame:int -> Type.t -> Type.t list -> ?params_names:string list ->
  Type.t -> Type.funtype

val mk_boundfunctiontype : ?frame:int -> Type.t list -> ?params_names:string list -> Type.t ->
  Type.funtype

val mk_functiontype : ?frame:int -> Type.t list -> ?params_names:string list -> Type.t ->
  Type.funtype

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

val mk_default: Context.t -> reason ->
  expr:(Context.t -> 'a -> Type.t) ->
  'a Default.t -> Type.t

(* val graph: bounds IMap.t ref *)
val lookup_module: Context.t -> string -> Type.t

(* contexts *)
val fresh_context: Context.metadata -> Loc.filename -> Modulename.t -> Context.t

(* builtins *)

val builtins: Context.t -> Type.t
val restore_builtins: Context.t -> Type.t -> unit
val get_builtin: Context.t -> ?trace:Trace.t -> string -> reason -> Type.t
val lookup_builtin: Context.t -> ?trace:Trace.t -> string -> reason -> reason option -> Type.t -> unit
val get_builtin_type: Context.t -> ?trace:Trace.t -> reason -> string -> Type.t
val resolve_builtin_class: Context.t -> ?trace:Trace.t -> Type.t -> Type.t
val set_builtin: Context.t -> ?trace:Trace.t -> string -> Type.t -> unit

val mk_instance: Context.t -> ?trace:Trace.t -> reason -> ?for_type:bool -> Type.t -> Type.t
val mk_typeof_annotation: Context.t -> ?trace:Trace.t -> Type.t -> Type.t

(* strict *)
val enforce_strict: Context.t -> Constraint.ident -> unit
val merge_type: Context.t -> (Type.t * Type.t) -> Type.t
val resolve_type: Context.t -> Type.t -> Type.t
val possible_types: Context.t -> Constraint.ident -> Type.t list
val possible_types_of_type: Context.t -> Type.t -> Type.t list

module Autocomplete : sig
  type member_result =
    | Success of Type.t SMap.t
    | SuccessModule of Type.t SMap.t * (Type.t option)
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  val command_result_of_member_result: member_result ->
    (Type.t SMap.t, string) Utils_js.ok_or_err

  val extract_members: Context.t -> Type.t -> member_result
end
