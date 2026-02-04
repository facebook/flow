(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module contains definitions for various intermediate representation of match pattern
 * for exhaustiveness checking. *)

open Loc_collections

type pattern_ast_list =
  ((ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t * (* guarded *) bool) list

(* Either a primitive literal or a Flow Enum member.
   Used in both `PatternUnion`s and `ValueUnion`s. *)
module Leaf : sig
  type enum_member = {
    enum_info: Type.enum_concrete_info;
    member_name: string;
  }

  val equal_enum_member : enum_member -> enum_member -> bool

  val compare_enum_member : enum_member -> enum_member -> int

  type leaf_ctor =
    | BoolC of bool
    | StrC of Reason.name
    | NumC of Type.number_literal
    | BigIntC of Type.bigint_literal
    | NullC
    | VoidC
    | EnumMemberC of enum_member

  type t = Reason.reason * leaf_ctor

  val compare : t -> t -> int

  val to_type : t -> Type.t

  val to_string : leaf_ctor -> string

  val to_ast : leaf_ctor -> Loc.t * ('a, Loc.t) Flow_ast.MatchPattern.t'
end

module LeafSet : Set.S with type elt = Leaf.t

val sort_object_patterns_by_index : (int * 'a) list -> (int * 'a) list

module ObjKind : sig
  type t =
    | Tuple of { length: int }
    | Obj
end

(* A pattern with properties: could be an object or tuple pattern. *)
module rec PatternObject : sig
  module Property : sig
    type t = {
      loc: ALoc.t;
      value: PatternUnion.t;
    }

    val compare : t -> t -> int
  end

  module Properties : sig
    type t = Property.t SMap.t
  end

  type t' = {
    kind: ObjKind.t;
    props: Properties.t;
    class_info: (ALoc.id * string option) option;
    keys_order: string list;
    rest: Reason.t option;
    contains_invalid_pattern: bool;
    guarded: bool;
  }

  type t = Reason.t * t'

  (* index for ordering *)
  type with_index = int * t

  val compare : t -> t -> int

  val to_string : t -> string

  val to_ast : t -> (Loc.t, Loc.t) Flow_ast.MatchPattern.t
end

(* A representation of a set of patterns. *)
and PatternUnion : sig
  type tuple_map = PatternObject.with_index list IMap.t

  type t = {
    leafs: LeafSet.t;
    guarded_leafs: Leaf.t list;
    tuples_exact: tuple_map;
    tuples_inexact: tuple_map;
    objects: PatternObject.with_index list;
    wildcard: Reason.t option;
    contains_invalid_pattern: bool;
  }

  val empty : t

  val all_tuples_and_objects : t -> PatternObject.with_index list

  (* If the only pattern in this pattern union is a wildcard, return that wildcard. *)
  val only_wildcard : t -> Reason.t option

  (* Whether this pattern union only contains leafs. *)
  val only_leafs : t -> bool

  val to_string : t -> string

  val to_ast : t -> (Loc.t, Loc.t) Flow_ast.MatchPattern.t
end

val wildcard_pattern : Reason.reason -> PatternUnion.t

val empty_inexact_tuple_pattern : Reason.reason -> PatternObject.t

(* A value with properties: could be an object, tuple, or array.
   Properties are converted to this representation lazily, so are only
   evaluated when required by the patterns. This is both for perf, and to
   handle recursive types (there are no recursive patterns).
   We only build up the initial `Properties.t` map for the exact objects
   and tuples, for other types we get these on demand from the stored type.
*)
module rec ValueObject : sig
  module Property : sig
    type t = {
      loc: ALoc.t;
      value: ValueUnion.t Lazy.t;
      optional: bool;
    }
  end

  module Properties : sig
    (* `None` represents a property that we know should not exist. This
        may occur while we are filtering `ValueObject`s by patterns, so
        we know not to attempt to get the property from the stored type. *)
    type t = Property.t option SMap.t

    val is_empty : t -> bool
  end

  type t' = {
    kind: ObjKind.t;
    t: Type.t;
    props: Properties.t;
    class_info: (ALoc.id * string option * ALocIDSet.t) option;
    rest: Reason.t option;
    (* We store a set of potential sentinel props, so that we can improve
       error messages, and also check these properties first when filtering
       by patterns. *)
    sentinel_props: SSet.t;
  }

  type t = Reason.t * t'

  val to_pattern : t -> PatternObject.t
end

(* A representation of a union of values. *)
and ValueUnion : sig
  type t = {
    leafs: LeafSet.t;
    tuples: ValueObject.t list;
    arrays: ValueObject.t list;
    objects: ValueObject.t list;
    enum_unknown_members: (Reason.t * LeafSet.t) list;
    inexhaustible: Type.t list;
  }

  val empty : t

  val is_empty : t -> bool

  val is_only_inexhaustible : t -> bool

  val to_pattern : t -> PatternUnion.t

  val to_type : Reason.t -> t -> Type.t

  val select : selector:Selector.t -> t -> t option
end
