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
open Constraint_js

val new_warning: (reason * string) list -> Errors_js.error
val new_error: (reason * string) list -> Errors_js.error

val add_warning: context -> ?trace:trace -> (reason * string) list -> unit
val add_error: context -> ?trace:trace -> (reason * string) list -> unit

val find_graph: context -> ident -> constraints

(* propagates sources to sinks following a subtype relation *)
val flow: context -> (Type.t * Type.t) -> unit

val unify: context -> Type.t -> Type.t -> unit

val master_cx: unit -> context

module Cache: sig
  val clear: unit -> unit
end

val mk_tvar: context -> reason -> Type.t
val mk_tvar_where: context -> reason -> (Type.t -> unit) -> Type.t

val get_builtin_typeapp: context -> reason -> string -> Type.t list -> Type.t

(* frames *)

val havoc_ctx : context -> int -> int -> unit

(* polymorphism *)

val subst: context -> ?force:bool -> (Type.t SMap.t) -> Type.t -> Type.t
val generate_tests: context -> reason -> Type.typeparam list -> (Type.t SMap.t -> unit)
  -> unit

(* property maps *)

val mk_propmap : context -> Type.t SMap.t -> int

val has_prop : context -> int -> SMap.key -> bool

val read_prop : context -> int -> SMap.key -> Type.t

val write_prop : context -> int -> SMap.key -> Type.t -> unit

val iter_props : context -> int -> (string -> Type.t -> unit) -> unit

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

val mk_object_with_proto : context -> reason -> Type.t -> Type.t
val mk_object_with_map_proto : context -> reason -> ?sealed:bool ->
  ?dict:Type.dicttype -> (Type.t SMap.t) -> Type.t -> Type.t

val static_method_call: context -> string -> reason -> string
  -> Type.t list -> Type.t

(* ... *)

val mk_nominal: context -> int

(* val graph: bounds IMap.t ref *)
val lookup_module: context -> string -> Type.t
val analyze_dependencies: context -> string list -> string -> SSet.t
val do_gc: context -> string list -> unit

(* contexts *)
val fresh_context:
  ?checked:bool -> ?weak:bool ->
  file:string -> _module:string ->
  context

(* builtins *)

val builtins: unit -> Type.t
val get_builtin: context -> string -> reason -> Type.t
val lookup_builtin: context -> string -> reason -> reason option -> Type.t -> unit
val get_builtin_type: context -> reason -> string -> Type.t
val resolve_builtin_class: context -> Type.t -> Type.t
val set_builtin: context -> string -> Type.t -> unit

val mk_instance: context -> reason -> ?for_type:bool -> Type.t -> Type.t
val mk_typeof_annotation: context -> ?trace:trace -> Type.t -> Type.t

(* strict *)
val check_types: context -> ident -> (Type.t -> bool) -> bool
val enforce_strict: context -> ident -> constraints -> unit
val suggested_type_cache: Type.t IMap.t ref
val merge_type: context -> (Type.t * Type.t) -> Type.t
val resolve_type: context -> Type.t -> Type.t
val possible_types: context -> ident -> Type.t list
val possible_types_of_type: context -> Type.t -> Type.t list

val ground_type: context -> Type.t -> Type.t
val normalize_type: context -> Type.t -> Type.t
(* this optimizes a normalized type for printability *)
val printify_type: context -> Type.t -> Type.t
(* returns a grounded(, normalized) and printified version of the type *)
val printified_type: context -> Type.t -> Type.t

(* class/ojc *)
val extract_members: context -> Type.t -> Type.t SMap.t
