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

val assert_false: string -> 'a
val __DEBUG__: ?s: string -> (unit -> 'a) -> 'a

val time: (float -> bool) -> (float -> string) -> (unit -> 'a) -> 'a

type ident = int
type name = string

(**************************************************)

module Type :
  sig
    type t =
        OpenT of reason * ident

      | NumT of reason * literal
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

      | EnumT of reason * t
      | RecordT of reason * t

      | TypeT of reason * t
      | BecomeT of reason * t

      | SpeculativeMatchFailureT of reason * t * t

      | CJSExportDefaultT of reason * t

      | SummarizeT of reason * t

      | CallT of reason * funtype
      | MethodT of reason * name * t * t list * t * int
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

      | SpecializeT of reason * t list * t

      | LookupT of reason * reason option * string * t

      | ObjAssignT of reason * t * t * string list * bool
      | ObjFreezeT of reason * t
      | ObjRestT of reason * string list * t
      | ObjSealT of reason * t

      | UnifyT of t * t

      | ConcretizeT of t * t list * t list * t
      | ConcreteT of t

      | KeyT of reason * t
      | HasT of reason * string

      | ElemT of reason * t * t

      | ImportModuleNsT of reason * t
      | ImportTypeT of reason * t
      | ExportDefaultT of reason * t

    and predicate =
        AndP of predicate * predicate
      | OrP of predicate * predicate
      | NotP of predicate
      | ExistsP
      | InstanceofP of t
      | ConstructorP of t
      | IsP of string

    and literal = string option

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
    and insttype = {
      class_id: ident;
      type_args: t SMap.t;
      fields_tmap: int;
      methods_tmap: int;
      mixins: bool;
      structural: bool;
    }
    and typeparam = {
      reason: reason;
      name: string;
      bound: t;
    }
    and prototype = t
    and static = t
    and super = t
    and properties = t SMap.t
    val compare : 'a -> 'a -> int

    val open_tvar: t -> (reason * ident)
  end

module TypeMap : MapSig with type key = Type.t

(***************************************)

val is_use: Type.t -> bool

(***************************************)

type trace

val unit_trace : Type.t -> Type.t -> trace
val rec_trace: Type.t -> Type.t -> trace -> trace
val concat_trace: trace list -> trace

val reasons_of_trace : ?level:int -> trace -> reason list

(***************************************)

module type PrimitiveT = sig
  val desc: string
  val t: Type.t
  val at: Spider_monkey_ast.Loc.t -> Type.t
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

type scope_entry = {
  specific: Type.t;
  general: Type.t;
  def_loc: Spider_monkey_ast.Loc.t option;
  for_type: bool;
}
type scope = scope_entry SMap.t ref
type stack = int list

val create_env_entry :
  ?for_type: bool ->
  Type.t -> Type.t ->
  Spider_monkey_ast.Loc.t option ->
  scope_entry

(***************************************)

(* TODO this has a bunch of stuff in it that should be localized *)
type context = {
  file: string;
  _module: string;
  mutable checked: bool;
  mutable weak: bool;
  mutable required: SSet.t;
  mutable require_loc: Spider_monkey_ast.Loc.t SMap.t;

  mutable graph: node IMap.t;
  mutable closures: (stack * scope list) IMap.t;
  mutable property_maps: Type.properties IMap.t;
  mutable modulemap: Type.t SMap.t;

  mutable strict_required: SSet.t;

  mutable errors: Errors_js.ErrorSet.t;
  mutable globals: SSet.t;

  type_table: (Spider_monkey_ast.Loc.t, Type.t) Hashtbl.t;
  annot_table: (Pos.t, Type.t) Hashtbl.t;
}

val new_context: string -> string -> context

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

val string_of_scope_entry : context -> scope_entry -> string
val string_of_scope : context -> scope -> string

(* TEMP *)
val streason_of_t : Type.t -> string

val desc_of_t : Type.t -> string

val pos_of_t : Type.t -> Pos.t
val loc_of_t : Type.t -> Spider_monkey_ast.Loc.t

val string_of_predicate : Type.predicate -> string

(* TODO should be in constraint_js, ocaml scoping quirk stymies me for now *)
val pos_of_predicate : Type.predicate -> Pos.t
