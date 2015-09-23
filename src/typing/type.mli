(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason_js
open Utils

type ident = int
type name = string

type t =
    OpenT of reason * ident

  | NumT of reason * number_literal literal
  | StrT of reason * string literal
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
  | MethodT of reason * propname * funtype
  | SetPropT of reason * propname * t
  | GetPropT of reason * propname * t
  | SetElemT of reason * t * t
  | GetElemT of reason * t * t
  | ReposLowerT of reason * t
  | ReposUpperT of reason * t

  | ConstructorT of reason * t list * t
  | SuperT of reason * insttype
  | ExtendsT of t list * t * t

  | AdderT of reason * t * t
  | ComparatorT of reason * t

  | PredicateT of predicate * t
  | EqT of reason * t
  | AndT of reason * t * t
  | OrT of reason * t * t
  | NotT of reason * t

  | SpecializeT of reason * bool * t list * t

  | LookupT of reason * reason option * t list * string * t

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

and 'a literal =
  | Literal of 'a
  | Truthy
  | Falsy
  | AnyLiteral

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

and propname = reason * name

and sealtype =
  | UnsealedInFile of string option
  | Sealed

and flags = {
  frozen: bool;
  sealed: sealtype;
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

val compare: 'a -> 'a -> int
val desc_of_t: t -> string
val is_use: t -> bool
val loc_of_predicate: predicate -> Loc.t
val loc_of_t: t -> Loc.t
val mod_reason_of_t: (reason -> reason) -> t -> t
val open_tvar: t -> (reason * ident)
val reason_of_t: t -> reason
val reasonless_compare: t -> t -> int
val repos_t_from_reason: reason -> t -> t
val string_of_ctor: t -> string
val string_of_predicate: predicate -> string

module type PrimitiveT = sig
  val desc: string
  val t: t
  val at: Loc.t -> t
  val why: reason -> t
  val tag: string -> t
end

module NumT: PrimitiveT
module StrT: PrimitiveT
module BoolT: PrimitiveT
module MixedT: PrimitiveT
module UndefT: PrimitiveT
module AnyT: PrimitiveT
module NullT: PrimitiveT
module VoidT: PrimitiveT

module TypeSet: Set.S with type elt = t
module TypeMap: MapSig with type key = t
