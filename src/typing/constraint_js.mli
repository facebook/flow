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

type ident = int
type name = string

(**************************************************)

module Type :
  sig
    type t =
        OpenT of reason * ident

      | NumT of reason * number_literal option
      | StrT of reason * literal
      | BoolT of reason * bool option
      | UndefT of reason
      | MixedT of reason
      | AnyT of reason
      | NullT of reason
      | VoidT of reason

      | FunT of reason * static * prototype * funtype
      | ObjT of reason * objtype
      | ArrT of reason * t * t list

      | ClassT of t
      | InstanceT of reason * static * super * insttype

      | OptionalT of t
      | RestT of t

      | PolyT of typeparam list * t
      | TypeAppT of t * t list
      | BoundT of typeparam
      | ExistsT of reason

      | MaybeT of t

      | IntersectionT of reason * t list

      | UnionT of reason * t list

      | UpperBoundT of t
      | LowerBoundT of t

      | AnyObjT of reason
      | AnyFunT of reason

      | ShapeT of t
      | DiffT of t * t

      | KeysT of reason * t
      | SingletonStrT of reason * string
      | SingletonNumT of reason * number_literal
      | SingletonBoolT of reason * bool

      | TypeT of reason * t
      | AnnotT of t * t

      | SpeculativeMatchFailureT of reason * t * t

      | ModuleT of reason * exporttypes

      | SummarizeT of reason * t

      | CallT of reason * funtype
      | MethodT of reason * name * funtype
      | SetT of reason * name * t
      | GetT of reason * name * t
      | SetElemT of reason * t * t
      | GetElemT of reason * t * t

      | ConstructorT of reason * t list * t
      | SuperT of reason * insttype
      | ExtendsT of t * t

      | AdderT of reason * t * t
      | ComparatorT of reason * t

      | PredicateT of predicate * t
      | EqT of reason * t
      | AndT of reason * t * t
      | OrT of reason * t * t

      | SpecializeT of reason * bool * t list * t

      | LookupT of reason * reason option * string * t

      | ObjAssignT of reason * t * t * string list * bool
      | ObjFreezeT of reason * t
      | ObjRestT of reason * string list * t
      | ObjSealT of reason * t
      | ObjTestT of reason * t * t

      | UnifyT of t * t
      | BecomeT of reason * t

      | ConcretizeT of t * t list * t list * t
      | ConcreteT of t

      | GetKeysT of reason * t
      | HasKeyT of reason * string

      | ElemT of reason * t * t

      | CJSRequireT of reason * t
      | ImportModuleNsT of reason * t
      | ImportTypeT of reason * t
      | ImportTypeofT of reason * t

      | CJSExtractNamedExportsT of reason * t * t_out
      | SetCJSExportT of reason * t * t_out
      | SetNamedExportsT of reason * t SMap.t * t_out

    and predicate =
        AndP of predicate * predicate
      | OrP of predicate * predicate
      | NotP of predicate
      | LeftP of binary_test * t
      | RightP of binary_test * t
      | ExistsP
      | IsP of string

    and binary_test =
        Instanceof
      | SentinelProp of string

    and literal = string option
    and number_literal = (float * string)

    and funtype = {
      this_t: t;
      params_tlist: t list;
      params_names: string list option;
      return_t: t;
      closure_t: int;
    }
    and objtype = {
      flags: flags;
      dict_t: dicttype option;
      props_tmap: int;
      proto_t: prototype;
    }
    and flags = {
      frozen: bool;
      sealed: bool;
      exact: bool;
    }
    and dicttype = {
      dict_name: string option;
      key: t;
      value: t;
    }
    and polarity =
      | Negative      (* contravariant *)
      | Neutral       (* invariant *)
      | Positive      (* covariant *)
    and insttype = {
      class_id: ident;
      type_args: t SMap.t;
      arg_polarities: polarity SMap.t;
      fields_tmap: int;
      methods_tmap: int;
      mixins: bool;
      structural: bool;
    }
    and exporttypes = {
      exports_tmap: int;
      cjs_export: t option;
    }
    and typeparam = {
      reason: reason;
      name: string;
      bound: t;
      polarity: polarity
    }
    and prototype = t
    and static = t
    and super = t
    and properties = t SMap.t
    and t_out = t

    val compare : 'a -> 'a -> int

    val open_tvar: t -> (reason * ident)
  end

module TypeSet : Set.S with type elt = Type.t
module TypeMap : MapSig with type key = Type.t

(***************************************)

val is_use: Type.t -> bool

(***************************************)

type trace

val trace_depth : trace -> int
val unit_trace : Type.t -> Type.t -> trace
val rec_trace: Type.t -> Type.t -> trace -> trace
val concat_trace: trace list -> trace

val reasons_of_trace : ?level:int -> ?tab:int -> trace -> reason list
val locs_of_trace : trace -> reason list

(***************************************)

module type PrimitiveT = sig
  val desc: string
  val t: Type.t
  val at: Loc.t -> Type.t
  val why: reason -> Type.t
  val tag: string -> Type.t
end

module NumT: PrimitiveT
module StrT: PrimitiveT
module BoolT: PrimitiveT
module MixedT: PrimitiveT
module UndefT: PrimitiveT
module AnyT: PrimitiveT
module NullT: PrimitiveT
module VoidT: PrimitiveT

type node =
| Goto of ident
| Root of root

and root = {
  rank: int;
  constraints: constraints;
}

and constraints =
| Resolved of Type.t
| Unresolved of bounds

and bounds = {
  mutable lower: trace TypeMap.t;
  mutable upper: trace TypeMap.t;
  mutable lowertvars: trace IMap.t;
  mutable uppertvars: trace IMap.t;
}

val new_unresolved_root: unit -> node
val bounds_of_unresolved_root: node -> bounds

val copy_node: node -> node

(***************************************)

(* scopes *)
(* Note: this is basically owned by Env_js, but are here
   to break circularity between Env_js and Flow_js.
   Longer-term solution is to break up Flow_js.
 *)
module Scope : sig

  type entry = {
    specific: Type.t;
    general: Type.t;
    def_loc: Loc.t option;
    for_type: bool;
  }

  type var_scope_attrs = {
    async: bool
  }

  type kind = VarScope of var_scope_attrs | LexScope

  type t = {
    kind: kind;
    mutable entries: entry SMap.t;
  }

  val fresh: unit -> t

  val fresh_async: unit -> t

  val fresh_lex: unit -> t

  val clone: t -> t

  val update: (string -> entry -> entry) -> t -> unit

  val update_opt: (string -> entry -> entry option) -> t -> unit

  val iter: (string -> entry -> unit) -> t -> unit

  val add: string -> entry -> t -> unit

  val remove: string -> t -> unit

  val mem: string -> t -> bool

  val get: string -> t -> entry option

  val get_unsafe: string -> t -> entry

  val create_entry:
    ?for_type: bool ->
    Type.t -> Type.t ->
    Loc.t option ->
    entry

  val refinement_key: string list -> string

  val is_refinement: string -> bool

  val havoc_entry:
    ?make_specific:(Type.t -> Type.t) ->
    string ->
    entry ->
    entry option

end

(***************************************)

type stack = int list

(* TODO this has a bunch of stuff in it that should be localized *)
type context = {
  file: string;
  _module: string;
  checked: bool;
  weak: bool;

  (* required modules, and map to their locations *)
  mutable required: SSet.t;
  mutable require_loc: Loc.t SMap.t;
  mutable module_exports_type: module_exports_type;

  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: node IMap.t;

  (* obj types point to mutable property maps *)
  mutable property_maps: Type.properties IMap.t;

  (* map from closure ids to env snapshots *)
  mutable closures: (stack * Scope.t list) IMap.t;

  (* map from module names to their types *)
  mutable modulemap: Type.t SMap.t;

  (* A subset of required modules on which the exported type depends *)
  mutable strict_required: SSet.t;

  mutable errors: Errors_js.ErrorSet.t;
  mutable globals: SSet.t;

  mutable error_suppressions: Errors_js.ErrorSuppressions.t;

  type_table: (Loc.t, Type.t) Hashtbl.t;
  annot_table: (Loc.t, Type.t) Hashtbl.t;
}
and module_exports_type =
  | CommonJSModule of Loc.t option
  | ESModule

val new_context:
  ?checked:bool -> ?weak:bool ->
  file:string -> _module:string ->
  context

(**************************************)

(* printing *)

val reason_of_t : Type.t -> reason

val mod_reason_of_t : (reason -> reason) -> Type.t -> Type.t

val to_op_reason : reason -> Type.t -> Type.t

val repos_t_from_reason : reason -> Type.t -> Type.t

val reasonless_compare : Type.t -> Type.t -> int

val string_of_t : context -> Type.t -> string
val json_of_t : context -> Type.t -> Hh_json.json
val jstr_of_t : context -> Type.t -> string
val json_of_graph : context -> Hh_json.json
val jstr_of_graph : context -> string
val dump_t : context -> Type.t -> string

val parameter_name : context -> string -> Type.t -> string
val string_of_param_t : context -> Type.t -> string

val is_printed_type_parsable : ?weak:bool -> context -> Type.t -> bool
val is_printed_param_type_parsable : ?weak:bool -> context -> Type.t -> bool

val string_of_ctor : Type.t -> string

val string_of_scope : context -> Scope.t -> string

(* TEMP *)
val streason_of_t : Type.t -> string

val desc_of_t : Type.t -> string

val loc_of_t : Type.t -> Loc.t

val string_of_predicate : Type.predicate -> string

val loc_of_predicate : Type.predicate -> Loc.t

class ['a] type_visitor : object
  (* Only exposing a few methods for now. *)
  method type_ : context -> 'a -> Type.t -> 'a
  method id_ : context -> 'a -> ident -> 'a
end
