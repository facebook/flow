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
open Reason_js

val new_warning: (reason * string) list -> Errors_js.error
val new_error: (reason * string) list -> Errors_js.error

val add_warning: Context.t -> ?trace:Trace.t -> (reason * string) list -> unit
val add_error: Context.t -> ?trace:Trace.t -> (reason * string) list -> unit

val find_graph: Context.t -> Constraint_js.ident -> Constraint_js.constraints

(* propagates sources to sinks following a subtype relation *)
val flow: Context.t -> (Type.t * Type.t) -> unit

val unify: Context.t -> Type.t -> Type.t -> unit

val reposition: Context.t -> ?trace:Trace.t -> reason -> Type.t -> Type.t

val master_cx: unit -> Context.t

(* constraint utils *)
val filter_optional: Context.t -> ?trace:Trace.t -> reason -> Type.t -> Type.t

module Cache: sig
  val clear: unit -> unit
end

val mk_tvar: Context.t -> reason -> Type.t
val mk_tvar_where: Context.t -> reason -> (Type.t -> unit) -> Type.t
val mk_tvar_derivable_where: Context.t -> reason -> (Type.t -> unit) -> Type.t

val get_builtin_typeapp: Context.t -> reason -> string -> Type.t list -> Type.t

(* polymorphism *)

val subst: Context.t -> ?force:bool -> (Type.t SMap.t) -> Type.t -> Type.t
val generate_tests: Context.t -> reason -> Type.typeparam list -> (Type.t SMap.t -> unit)
  -> unit

(* property maps *)

val mk_propmap : Context.t -> Type.t SMap.t -> int

val has_prop : Context.t -> int -> SMap.key -> bool

val read_prop : Context.t -> int -> SMap.key -> Type.t

val write_prop : Context.t -> int -> SMap.key -> Type.t -> unit

val iter_props : Context.t -> int -> (string -> Type.t -> unit) -> unit

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
val dummy_static : Type.t
val dummy_prototype : Type.t

val mk_objecttype : ?flags:Type.flags ->
  Type.dicttype option -> int -> Type.t -> Type.objtype

val mk_object_with_proto : Context.t -> reason -> Type.t -> Type.t
val mk_object_with_map_proto : Context.t -> reason ->
  ?sealed:bool ->
  ?frozen:bool ->
  ?dict:Type.dicttype -> (Type.t SMap.t) -> Type.t -> Type.t

val static_method_call: Context.t -> string -> reason -> reason -> string
  -> Type.t list -> Type.t

(* ... *)

val mk_nominal: Context.t -> int

(* val graph: bounds IMap.t ref *)
val lookup_module: Context.t -> string -> Type.t
val do_gc: Context.t -> string list -> unit

(* contexts *)
val fresh_context: Context.metadata -> Loc.filename -> string -> Context.t

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
val suggested_type_cache: Type.t IMap.t ref
val merge_type: Context.t -> (Type.t * Type.t) -> Type.t
val resolve_type: Context.t -> Type.t -> Type.t
val possible_types: Context.t -> Constraint_js.ident -> Type.t list
val possible_types_of_type: Context.t -> Type.t -> Type.t list

val ground_type: Context.t -> Type.t -> Type.t
val normalize_type: Context.t -> Type.t -> Type.t
(* this optimizes a normalized type for printability *)
val printify_type: Context.t -> Type.t -> Type.t
(* returns a grounded(, normalized) and printified version of the type *)
val printified_type: Context.t -> Type.t -> Type.t

module Autocomplete : sig
  type member_result =
    | Success of Type.t SMap.t
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  val map_of_member_result: member_result -> Type.t SMap.t

  val extract_members: Context.t -> Type.t -> member_result
end

module ContextOptimizer: sig
  val sig_context : Context.t list -> unit
end

val restore_master_cx: Context.t -> unit
val restore_builtins: Context.t -> Type.t -> unit
