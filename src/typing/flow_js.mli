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
open Reason_js
open Constraint_js

val new_warning: (reason * string) list -> Errors_js.error
val new_error: (reason * string) list -> Errors_js.error

val add_warning: context -> (reason * string) list -> unit
val add_error: context -> (reason * string) list -> unit

val find_graph: context -> ident -> bounds

(* propagates sources to sinks following a subtype relation *)
val flow: context -> (Type.t * Type.t) -> trace -> unit

val unit_flow: context -> (Type.t * Type.t) -> unit

val unify: context -> Type.t -> Type.t -> unit

val master_cx: context

module Cache: sig
  val clear: unit -> unit
end

val mk_var: context -> int
val mk_tvar: context -> reason -> Type.t
val mk_tvar_where: context -> reason -> (Type.t -> unit) -> Type.t

val mk_typeapp_instance: context -> reason -> string -> Type.t list -> Type.t

(* frames *)

val frames : stack ref

val mk_frame : context -> int list -> block list -> unit

val havoc_ctx : context -> int -> int -> unit

(* polymorphism *)

val subst: context -> (Type.t IMap.t) -> Type.t -> Type.t

(* property maps *)

val mk_propmap : context -> Type.t SMap.t -> int

val has_prop : context -> int -> SMap.key -> bool

val read_prop : context -> int -> SMap.key -> Type.t

val write_prop : context -> int -> SMap.key -> Type.t -> unit

val iter_props : context -> int -> (string -> Type.t -> unit) -> unit

(* object/method types *)

val mk_methodtype : Type.t -> Type.t list -> string list option ->
  Type.t -> Type.funtype

val mk_methodtype2 : Type.t -> Type.t list -> string list option ->
  Type.t -> int -> Type.funtype

val mk_functiontype : Type.t list -> string list option -> Type.t ->
  Type.funtype

val mk_functiontype2 : Type.t list -> string list option -> Type.t ->
  int -> Type.funtype

val dummy_static : Type.t
val dummy_prototype : Type.t

val mk_objecttype : ?sealed:bool -> (Type.t * Type.t) -> int -> Type.t ->
  Type.objtype

val mk_object_with_proto : context -> reason -> Type.t -> Type.t
val mk_object_with_map_proto : context -> reason -> (Type.t SMap.t) ->
  Type.t -> Type.t

val static_method_call: context -> string -> reason -> string
  -> Type.t list -> Type.t

(* ... *)

val mk_nominal: context -> int

(* val graph: bounds IMap.t ref *)

val do_gc: context -> Type.t list -> Type.t list -> unit

(* builtins *)

val builtins: Type.t
val get_builtin: context -> string -> reason -> Type.t
val lookup_builtin: context -> string -> reason -> reason option -> Type.t -> unit
val get_builtin_type: context -> reason -> string -> Type.t
val resolve_builtin_class: context -> Type.t -> Type.t
val set_builtin: context -> string -> Type.t -> unit
val mk_annot: context -> reason -> Type.t -> Type.t

(* strict *)
val check_upper_bound: context -> ident -> (Type.t -> bool) -> bool
val check_lower_bound: context -> ident -> (Type.t -> bool) -> bool
val enforce_strict: context -> ident -> bounds -> unit
val suggested_type_cache: Type.t IMap.t ref
val merge_type: context -> (Type.t * Type.t) -> Type.t
val ground_type: context -> ISet.t ->
  Type.t -> Type.t
val get_ground_type: context -> Type.t -> Type.t
val resolve_type: context -> Type.t -> Type.t
val possible_types_of_type: context -> Type.t -> Type.t list

(* class/ojc *)
val extract_members: context -> Type.t -> Type.t SMap.t
