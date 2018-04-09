(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

(* propagates sources to sinks following a subtype relation *)
val flow: Context.t -> (Type.t * Type.use_t) -> unit
val flow_t: Context.t -> (Type.t * Type.t) -> unit

(* given a use type, return a tvar constrained by the use type *)
val tvar_with_constraint: Context.t -> ?trace:Trace.t -> ?derivable:bool -> Type.use_t -> Type.t

val unify: Context.t -> Type.t -> Type.t -> unit

val reposition: Context.t -> ?trace:Trace.t -> Loc.t -> ?desc:reason_desc -> ?annot_loc:Loc.t -> Type.t -> Type.t

(* constraint utils *)
val filter_optional: Context.t -> ?trace:Trace.t -> reason -> Type.t -> Type.t

module Cache: sig
  val clear: unit -> unit
  val stats_poly_instantiation: unit -> Hashtbl.statistics
  val summarize_flow_constraint: unit -> (string * int) list
end

val get_builtin_typeapp: Context.t -> ?trace:Trace.t -> reason -> string -> Type.t list -> Type.t

val resolve_spread_list:
  Context.t ->
  use_op:Type.use_op ->
  reason_op:Reason.t ->
  Type.unresolved_param list ->
  Type.spread_resolve ->
  unit

(* polymorphism *)

val subst: Context.t -> ?use_op:Type.use_op -> ?force:bool -> (Type.t SMap.t) -> Type.t -> Type.t
val generate_tests: Context.t -> Type.typeparam list -> (Type.t SMap.t -> unit)
  -> unit
val match_this_binding: Type.t SMap.t -> (Type.t -> bool) -> bool

val check_polarity:
  Context.t -> ?trace:Trace.t -> Type.polarity -> Type.t -> unit

(* selectors *)

val eval_selector : Context.t -> ?trace:Trace.t -> reason -> Type.t -> Type.selector -> int -> Type.t
val visit_eval_id : Context.t -> int -> (Type.t -> unit) -> unit

(* ... *)

val mk_default: Context.t -> reason ->
  expr:(Context.t -> 'a -> Type.t) ->
  'a Default.t -> Type.t

(* val graph: bounds IMap.t ref *)
val lookup_module: Context.t -> string -> Type.t

(* contexts *)
val mk_builtins: Context.t -> unit
val add_output: Context.t -> ?trace:Trace.t -> Flow_error.error_message -> unit

(* builtins *)

val builtins: Context.t -> Type.t
val get_builtin: Context.t -> ?trace:Trace.t -> string -> reason -> Type.t
val lookup_builtin: Context.t -> ?trace:Trace.t -> string -> reason -> Type.lookup_kind -> Type.t -> unit
val get_builtin_type: Context.t -> ?trace:Trace.t -> reason -> ?use_desc:bool -> string -> Type.t
val resolve_builtin_class: Context.t -> ?trace:Trace.t -> Type.t -> Type.t
val set_builtin: Context.t -> ?trace:Trace.t -> string -> Type.t -> unit

val mk_instance: Context.t -> ?trace:Trace.t -> reason -> ?for_type:bool -> ?use_desc:bool -> Type.t -> Type.t
val mk_typeof_annotation: Context.t -> ?trace:Trace.t -> reason -> ?use_desc:bool -> Type.t -> Type.t

(* strict *)
val enforce_strict: Context.t -> Constraint.ident -> unit
val merge_type: Context.t -> (Type.t * Type.t) -> Type.t
val resolve_type: Context.t -> Type.t -> Type.t
val resolve_tvar: Context.t -> Type.tvar -> Type.t
val possible_types: Context.t -> Constraint.ident -> Type.t list
val possible_types_of_type: Context.t -> Type.t -> Type.t list
val possible_uses: Context.t -> Constraint.ident -> Type.use_t list

module Members : sig
  type ('success, 'success_module) generic_t =
    | Success of 'success
    | SuccessModule of 'success_module
    | FailureNullishType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  type t = (
    (* Success *) (Loc.t option * Type.t) SMap.t,
    (* SuccessModule *) (Loc.t option * Type.t) SMap.t * (Type.t option)
  ) generic_t

  (* For debugging purposes *)
  val string_of_extracted_type: (Type.t, Type.t) generic_t -> string

  val to_command_result: t -> ((Loc.t option * Type.t) SMap.t, string) result

  val extract: Context.t -> Type.t -> t
  val extract_type: Context.t -> Type.t -> (Type.t, Type.t) generic_t
  val extract_members: Context.t -> (Type.t, Type.t) generic_t -> t
end
