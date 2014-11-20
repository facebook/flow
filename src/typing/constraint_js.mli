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

type ident = int
type name = string

(**************************************************)

module Type :
  sig
    type t =
        OpenT of reason * ident

      | NumT of reason * literal
      | StrT of reason * literal
      | BoolT of reason
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

      | MaybeT of t

      | IntersectionT of reason * t list

      | UnionT of reason * t list

      | UpperBoundT of t
      | LowerBoundT of t

      | EnumT of reason * t
      | RecordT of reason * t

      | CustomClassT of name * t list * t

      | TypeT of reason * t

      | ConcretizeT of reason * t list * t * t
      | ConcreteT of t

      | SummarizeT of reason * t

      | CallT of reason * funtype
      | MethodT of reason * name * t * t list * t * int
      | SetT of reason * name * t
      | GetT of reason * name * t
      | SetElemT of reason * t * t
      | GetElemT of reason * t * t

      | ConstructorT of reason * t list * t
      | SuperT of reason * insttype
      | ParentT of reason * insttype

      | AdderT of reason * t * t
      | ComparatorT of reason * t

      | PredicateT of predicate * t
      | EqT of reason * t

      | SpecializeT of reason * t list * t

      | LookupT of reason * reason option * string * t

      | MarkupT of reason * t * t

      | ObjAssignT of reason * t * t
      | ObjRestT of reason * string list * t
      | ObjExtendT of reason * t SMap.t * t

      | UnifyT of t * t

      | KeyT of reason * t
      | HasT of reason * string

      | ElemT of reason * t * t

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
      this_t : t;
      params_tlist : t list;
      params_names : string list option;
      return_t : t;
      closure_t : int;
    }
    and objtype = {
      sealed: bool;
      dict_t : t * t;
      props_tmap : int;
      proto_t : prototype;
    }
    and insttype = {
      class_id: ident;
      type_args: t IMap.t;
      fields_tmap: fields;
      methods_tmap: methods
    }
    and typeparam = {
      reason: reason;
      id: ident;
      name: string;
    }
    and prototype = t
    and static = t
    and super = t
    and fields = t SMap.t
    and methods = t SMap.t
    val compare : 'a -> 'a -> int

    val open_tvar: t -> (reason * ident)
    val mk_predicate: (predicate * t) -> t
  end

module TypeMap : MapSig with type key = Type.t

(***************************************)

val is_use: Type.t -> bool

(***************************************)

type rule =
  | FunThis
  | FunArg of int * int
  | FunRet
  | ObjProp of string
  | ObjKey
  | ArrIndex
  | ArrElem
  | DecomposeNullable
  | InstantiatePoly
  | ClassInst
  | FunInst
  | FunProto
  | CopyProto
  | InstanceProp of string
  | ObjProto
  | StrIndex
  | StrElem
  | InstanceSuper
  | Op of string
  | ReactProps
  | ReactComponent
  | LibMethod of string
  | FunStatics
  | ClassStatics

type trace

val string_of_trace : string -> bool -> trace -> string
(* TODO remove *)
val string_of_trace_old : string -> bool -> trace -> string

val unit_trace : Type.t -> Type.t -> trace
val select_trace: Type.t -> Type.t -> trace -> rule -> trace
val concat_trace: trace list -> trace

val reasons_of_trace : trace -> reason list

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

type unifier =
| Goto of ident
| Rank of int

type bounds = {
  mutable lower: trace TypeMap.t;
  mutable upper: trace TypeMap.t;
  mutable lowertvars: trace IMap.t;
  mutable uppertvars: trace IMap.t;
  mutable unifier: unifier option;
  mutable solution: Type.t option;
}

val new_bounds: int -> reason -> bounds

(***************************************)

type block_entry = {
  specific: Type.t;
  general: Type.t;
  def_loc: Spider_monkey_ast.Loc.t option;
}
type block = block_entry SMap.t ref
type stack = int list

val create_env_entry :
  Type.t -> Type.t ->
  Spider_monkey_ast.Loc.t option ->
  block_entry

(***************************************)

(* TODO this has a bunch of stuff in it that should be localized *)
type context = {
  file: string;
  _module: string;
  mutable checked: bool;
  mutable weak: bool;
  mutable required: SSet.t;
  mutable require_loc: Spider_monkey_ast.Loc.t SMap.t;

  mutable graph: bounds IMap.t;
  mutable parents: Type.t IMap.t IMap.t IMap.t;
  mutable closures: (stack * block list) IMap.t;
  mutable property_maps: Type.t SMap.t IMap.t;
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

(* commenting this function out, since it is not used anywhere *)
(* val string_of_t : context -> Type.t -> string *)

val reason_of_t : Type.t -> reason

val mod_reason_of_t : (reason -> reason) -> Type.t -> Type.t

val string_of_t : context -> Type.t -> string

val is_printed_type_parsable : context -> Type.t -> bool

val string_of_ctor : Type.t -> string

(* TEMP *)
val streason_of_t : Type.t -> string

val desc_of_t : Type.t -> string

val pos_of_t : Type.t -> Pos.t
val loc_of_t : Type.t -> Spider_monkey_ast.Loc.t

val string_of_predicate : Type.predicate -> string

(* TODO should be in constraint_js, ocaml scoping quirk stymies me for now *)
val pos_of_predicate : Type.predicate -> Pos.t
