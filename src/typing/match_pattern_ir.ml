(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections

type pattern_ast_list =
  ((ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t * (* guarded *) bool) list

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
end = struct
  type enum_member = {
    enum_info: Type.enum_concrete_info;
    member_name: string;
  }

  let equal_enum_member
      { enum_info = { Type.enum_id = enum_id_a; _ }; member_name = member_name_a }
      { enum_info = { Type.enum_id = enum_id_b; _ }; member_name = member_name_b } =
    ALoc.equal_id enum_id_a enum_id_b && member_name_a = member_name_b

  let compare_enum_member
      { enum_info = { Type.enum_id = enum_id_a; _ }; member_name = member_name_a }
      { enum_info = { Type.enum_id = enum_id_b; _ }; member_name = member_name_b } =
    let compare_id = ALoc.compare_id enum_id_a enum_id_b in
    if compare_id = 0 then
      String.compare member_name_a member_name_b
    else
      compare_id

  type leaf_ctor =
    | BoolC of bool
    | StrC of Reason.name
    | NumC of Type.number_literal
    | BigIntC of Type.bigint_literal
    | NullC
    | VoidC
    | EnumMemberC of enum_member
  [@@deriving eq, ord]

  type t = Reason.t * leaf_ctor

  let compare (_, a_ctor) (_, b_ctor) = compare_leaf_ctor a_ctor b_ctor

  let to_type (reason, leaf) =
    match leaf with
    | BoolC value -> Type.DefT (reason, Type.SingletonBoolT { value; from_annot = false })
    | StrC value -> Type.DefT (reason, Type.SingletonStrT { value; from_annot = false })
    | NumC value -> Type.DefT (reason, Type.SingletonNumT { value; from_annot = false })
    | BigIntC value -> Type.DefT (reason, Type.SingletonBigIntT { value; from_annot = false })
    | NullC -> Type.NullT.make reason
    | VoidC -> Type.VoidT.make reason
    | EnumMemberC { enum_info; member_name = _ } ->
      Type.DefT (reason, Type.EnumValueT (Type.ConcreteEnum enum_info))

  let to_string leaf =
    match leaf with
    | BoolC true -> "true"
    | BoolC false -> "false"
    | StrC name ->
      Js_layout_generator.quote_string
        ~prefer_single_quotes:true
        (Reason.display_string_of_name name)
    | NumC (_, s) -> s
    | BigIntC (_, s) -> s
    | NullC -> "null"
    | VoidC -> "undefined"
    | EnumMemberC { enum_info = { Type.enum_name; _ }; member_name } ->
      Utils_js.spf "%s.%s" enum_name member_name

  let to_ast leaf =
    match leaf with
    | BoolC value ->
      ( Loc.none,
        Flow_ast.MatchPattern.BooleanPattern { Flow_ast.BooleanLiteral.value; comments = None }
      )
    | StrC name ->
      let value = Reason.display_string_of_name name in
      ( Loc.none,
        Flow_ast.MatchPattern.StringPattern
          {
            Flow_ast.StringLiteral.value;
            raw = Js_layout_generator.quote_string ~prefer_single_quotes:true value;
            comments = None;
          }
      )
    | NumC (value, raw) ->
      ( Loc.none,
        Flow_ast.MatchPattern.NumberPattern { Flow_ast.NumberLiteral.value; raw; comments = None }
      )
    | BigIntC (value, raw) ->
      ( Loc.none,
        Flow_ast.MatchPattern.BigIntPattern { Flow_ast.BigIntLiteral.value; raw; comments = None }
      )
    | NullC -> (Loc.none, Flow_ast.MatchPattern.NullPattern None)
    | VoidC ->
      ( Loc.none,
        Flow_ast.MatchPattern.IdentifierPattern
          (Loc.none, { Flow_ast.Identifier.name = "undefined"; comments = None })
      )
    | EnumMemberC { enum_info = { Type.enum_name; _ }; member_name } ->
      let open Flow_ast.MatchPattern.MemberPattern in
      let base =
        BaseIdentifier (Loc.none, { Flow_ast.Identifier.name = enum_name; comments = None })
      in
      let property =
        PropertyIdentifier (Loc.none, { Flow_ast.Identifier.name = member_name; comments = None })
      in
      (Loc.none, Flow_ast.MatchPattern.MemberPattern (Loc.none, { base; property; comments = None }))
end

module LeafSet = Set.Make (struct
  type t = Leaf.t

  let compare a b = Leaf.compare a b
end)

let sort_object_patterns_by_index = Base.List.sort ~compare:(fun (a, _) (b, _) -> a - b)

module ObjKind = struct
  type t =
    | Tuple of { length: int }
    | Obj
end

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
end = struct
  module Property = struct
    type t = {
      loc: ALoc.t;
      value: PatternUnion.t;
    }

    let compare a b =
      let { value = value_a; loc = _ } = a in
      let { value = value_b; loc = _ } = b in
      (* Sort leafs only props first. Sort wildcard props last. *)
      match (PatternUnion.only_leafs value_a, PatternUnion.only_leafs value_b) with
      | (true, false) -> -1
      | (false, true) -> 1
      | _ ->
        (match (PatternUnion.only_wildcard value_a, PatternUnion.only_wildcard value_b) with
        | (None, Some _) -> -1
        | (Some _, None) -> 1
        | _ -> 0)
  end

  module Properties = struct
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

  type with_index = int * t

  (* We aim to sort more specific patterns before less specific ones. *)
  let compare a b =
    let compare_rest rest1 rest2 =
      (* without rest before with rest *)
      match (rest1, rest2) with
      | (None, Some _) -> -1
      | (Some _, None) -> 1
      | _ -> 0
    in
    let (_, { kind = kind1; props = props1; rest = rest1; _ }) = a in
    let (_, { kind = kind2; props = props2; rest = rest2; _ }) = b in
    match (kind1, kind2) with
    (* tuples before objects *)
    | (ObjKind.Tuple _, ObjKind.Obj) -> -1
    | (ObjKind.Obj, ObjKind.Tuple _) -> 1
    | (ObjKind.Tuple { length = length1 }, ObjKind.Tuple { length = length2 }) ->
      (* longer first *)
      let length_compare = -Int.compare length1 length2 in
      if length_compare <> 0 then
        length_compare
      else
        compare_rest rest1 rest2
    | (ObjKind.Obj, ObjKind.Obj) ->
      (* more props first *)
      let props_size_compare = -Int.compare (SMap.cardinal props1) (SMap.cardinal props2) in
      if props_size_compare <> 0 then
        props_size_compare
      else
        compare_rest rest1 rest2

  let to_string (_, { kind; props; class_info; keys_order; rest; _ }) =
    let constructor =
      match class_info with
      | Some (_, Some class_name) -> Utils_js.spf "%s " class_name
      | _ -> ""
    in
    match kind with
    | ObjKind.Obj ->
      let props =
        Base.List.map keys_order ~f:(fun key ->
            let { Property.value; _ } = SMap.find key props in
            Utils_js.spf "%s: %s" key (PatternUnion.to_string value)
        )
      in
      let props =
        if Base.Option.is_some rest then
          props @ ["..."]
        else
          props
      in
      Utils_js.spf "%s{%s}" constructor (String.concat ", " props)
    | ObjKind.Tuple { length } ->
      let elements =
        Base.List.init length ~f:(fun i ->
            let { Property.value; _ } = SMap.find (string_of_int i) props in
            PatternUnion.to_string value
        )
      in
      let elements =
        if Base.Option.is_some rest then
          elements @ ["..."]
        else
          elements
      in
      Utils_js.spf "[%s]" (String.concat ", " elements)

  let to_ast (_, { kind; props; class_info; keys_order; rest; _ }) =
    let open Flow_ast in
    let rest =
      if Base.Option.is_some rest then
        Some (Loc.none, { MatchPattern.RestPattern.argument = None; comments = None })
      else
        None
    in
    match kind with
    | ObjKind.Obj ->
      let properties =
        Base.List.map keys_order ~f:(fun key ->
            let { Property.value; _ } = SMap.find key props in
            let key =
              if Parser_flow.string_is_valid_identifier_name key then
                MatchPattern.ObjectPattern.Property.Identifier
                  (Loc.none, { Identifier.name = key; comments = None })
              else
                let str_lit =
                  {
                    StringLiteral.value = key;
                    raw = Js_layout_generator.quote_string ~prefer_single_quotes:true key;
                    comments = None;
                  }
                in
                MatchPattern.ObjectPattern.Property.StringLiteral (Loc.none, str_lit)
            in
            let pattern = PatternUnion.to_ast value in
            ( Loc.none,
              MatchPattern.ObjectPattern.Property.Valid
                {
                  MatchPattern.ObjectPattern.Property.key;
                  pattern;
                  shorthand = false;
                  comments = None;
                }
            )
        )
      in
      let obj_pattern = { MatchPattern.ObjectPattern.properties; rest; comments = None } in
      (match class_info with
      | Some (_, Some name) ->
        let constructor =
          MatchPattern.InstancePattern.IdentifierConstructor
            (Loc.none, { Flow_ast.Identifier.name; comments = None })
        in
        ( Loc.none,
          MatchPattern.InstancePattern
            {
              MatchPattern.InstancePattern.constructor;
              fields = (Loc.none, obj_pattern);
              comments = None;
            }
        )
      | _ -> (Loc.none, MatchPattern.ObjectPattern obj_pattern))
    | ObjKind.Tuple { length } ->
      let elements =
        Base.List.init length ~f:(fun i ->
            let { Property.value; _ } = SMap.find (string_of_int i) props in
            let pattern = PatternUnion.to_ast value in
            { MatchPattern.ArrayPattern.Element.index = Loc.none; pattern }
        )
      in
      ( Loc.none,
        MatchPattern.ArrayPattern { MatchPattern.ArrayPattern.elements; rest; comments = None }
      )
end

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

  val only_wildcard : t -> Reason.t option

  val only_leafs : t -> bool

  val to_string : t -> string

  val to_ast : t -> (Loc.t, Loc.t) Flow_ast.MatchPattern.t
end = struct
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

  let empty : t =
    {
      leafs = LeafSet.empty;
      guarded_leafs = [];
      tuples_exact = IMap.empty;
      tuples_inexact = IMap.empty;
      objects = [];
      wildcard = None;
      contains_invalid_pattern = false;
    }

  let only_wildcard
      ({
         leafs;
         guarded_leafs;
         tuples_exact;
         tuples_inexact;
         objects;
         wildcard;
         contains_invalid_pattern = _;
       } :
        t
        ) : Reason.t option =
    if
      LeafSet.is_empty leafs
      && Base.List.is_empty guarded_leafs
      && IMap.is_empty tuples_exact
      && IMap.is_empty tuples_inexact
      && Base.List.is_empty objects
    then
      wildcard
    else
      None

  let only_leafs
      {
        leafs;
        guarded_leafs;
        tuples_exact;
        tuples_inexact;
        objects;
        wildcard;
        contains_invalid_pattern = _;
      } =
    (not @@ LeafSet.is_empty leafs)
    && Base.List.is_empty guarded_leafs
    && IMap.is_empty tuples_exact
    && IMap.is_empty tuples_inexact
    && Base.List.is_empty objects
    && Base.Option.is_none wildcard

  let all_tuples_and_objects { tuples_exact; tuples_inexact; objects; _ } =
    let all_tuples =
      Base.List.rev_append (IMap.values tuples_exact) (IMap.values tuples_inexact)
      |> Base.List.concat
    in
    Base.List.rev_append all_tuples objects |> sort_object_patterns_by_index

  let to_string pattern_union =
    let { leafs; wildcard; _ } = pattern_union in
    let leafs = LeafSet.elements leafs |> Base.List.map ~f:(fun (_, leaf) -> Leaf.to_string leaf) in
    let tuples_and_objects =
      all_tuples_and_objects pattern_union
      |> Base.List.map ~f:(fun (_, pattern_object) -> PatternObject.to_string pattern_object)
    in
    let wildcard =
      if Base.Option.is_some wildcard then
        ["_"]
      else
        []
    in
    Base.List.concat [leafs; tuples_and_objects; wildcard] |> String.concat " | "

  let to_ast pattern_union =
    let { leafs; wildcard; _ } = pattern_union in
    let leafs = LeafSet.elements leafs |> Base.List.map ~f:(fun (_, leaf) -> Leaf.to_ast leaf) in
    let tuples_and_objects =
      all_tuples_and_objects pattern_union
      |> Base.List.map ~f:(fun (_, pattern_object) -> PatternObject.to_ast pattern_object)
    in
    let wildcard =
      if Base.Option.is_some wildcard then
        [
          ( Loc.none,
            Flow_ast.MatchPattern.WildcardPattern
              {
                Flow_ast.MatchPattern.WildcardPattern.comments = None;
                invalid_syntax_default_keyword = false;
              }
          );
        ]
      else
        []
    in
    let patterns = Base.List.concat [leafs; tuples_and_objects; wildcard] in
    match patterns with
    | [single] -> single
    | _ ->
      ( Loc.none,
        Flow_ast.MatchPattern.OrPattern
          { Flow_ast.MatchPattern.OrPattern.patterns; comments = None }
      )
end

(* `_` *)
let wildcard_pattern reason = { PatternUnion.empty with PatternUnion.wildcard = Some reason }

(* `[...]` *)
let empty_inexact_tuple_pattern (reason : Reason.t) : PatternObject.t =
  ( reason,
    {
      PatternObject.kind = ObjKind.Tuple { length = 0 };
      props = SMap.empty;
      class_info = None;
      keys_order = [];
      rest = Some reason;
      contains_invalid_pattern = false;
      guarded = false;
    }
  )

module rec ValueObject : sig
  module Property : sig
    type t = {
      loc: ALoc.t;
      value: ValueUnion.t Lazy.t;
      optional: bool;
    }
  end

  module Properties : sig
    type t = Property.t option SMap.t

    val is_empty : t -> bool
  end

  type t' = {
    kind: ObjKind.t;
    t: Type.t;
    props: Properties.t;
    class_info: (ALoc.id * string option * ALocIDSet.t) option;
    rest: Reason.t option;
    sentinel_props: SSet.t;
  }

  type t = Reason.t * t'

  val to_pattern : t -> PatternObject.t
end = struct
  module Property = struct
    type t = {
      loc: ALoc.t;
      value: ValueUnion.t Lazy.t;
      optional: bool;
    }
  end

  module Properties = struct
    type t = Property.t option SMap.t

    let is_empty props = not @@ SMap.exists (fun _ prop -> Base.Option.is_some prop) props
  end

  type t' = {
    kind: ObjKind.t;
    t: Type.t;
    props: Properties.t;
    class_info: (ALoc.id * string option * ALocIDSet.t) option;
    rest: Reason.t option;
    sentinel_props: SSet.t;
  }

  type t = Reason.t * t'

  let to_pattern (reason, { props; class_info; rest; kind; sentinel_props; _ }) =
    let loc = Reason.loc_of_reason reason in
    let (props, keys_order, rest) =
      match kind with
      | ObjKind.Obj ->
        let (props, wildcard_props, rest) =
          SMap.fold
            (fun key prop acc ->
              match prop with
              | Some { Property.value; optional; loc; _ } ->
                let (props, wildcard_props, rest) = acc in
                let is_sentinel_prop = SSet.mem key sentinel_props in
                if optional then
                  ( props,
                    wildcard_props,
                    match rest with
                    | Some _ -> rest
                    | None -> Some reason
                  )
                else if Lazy.is_val value || is_sentinel_prop then
                  let value = ValueUnion.to_pattern (Lazy.force value) in
                  let prop = { PatternObject.Property.loc; value } in
                  if Base.Option.is_some (PatternUnion.only_wildcard value) then
                    let wildcard_props = SMap.add key prop wildcard_props in
                    (props, wildcard_props, rest)
                  else
                    let props = SMap.add key prop props in
                    (props, wildcard_props, rest)
                else
                  let prop = { PatternObject.Property.loc; value = wildcard_pattern reason } in
                  let wildcard_props = SMap.add key prop wildcard_props in
                  (props, wildcard_props, rest)
              | _ -> acc)
            props
            (SMap.empty, SMap.empty, rest)
        in
        (* If we have over a certain amount of wildcard props, suggest an inexact
           object pattern rather than many wildcard properties. *)
        let (props, rest) =
          if SMap.cardinal wildcard_props >= 5 then
            ( props,
              match rest with
              | Some _ -> rest
              | None -> Some reason
            )
          else
            (SMap.union props wildcard_props, rest)
        in
        let keys_order =
          SMap.keys props
          |> Base.List.sort ~compare:(fun key1 key2 ->
                 (* Sentinel props come first, then normal pattern prop order. *)
                 match (SSet.mem key1 sentinel_props, SSet.mem key2 sentinel_props) with
                 | (true, false) -> -1
                 | (false, true) -> 1
                 | _ ->
                   let prop1 = SMap.find key1 props in
                   let prop2 = SMap.find key2 props in
                   PatternObject.Property.compare prop1 prop2
             )
        in
        (props, keys_order, rest)
      | ObjKind.Tuple { length } ->
        let props =
          SMap.map
            (fun prop ->
              let (loc, value) =
                match prop with
                | Some { Property.value; loc; _ } when Lazy.is_val value ->
                  (loc, ValueUnion.to_pattern (Lazy.force value))
                | _ -> (loc, wildcard_pattern reason)
              in
              { PatternObject.Property.loc; value })
            props
        in
        let keys_order = Base.List.init length ~f:(fun i -> string_of_int i) in
        (props, keys_order, rest)
    in
    let class_info = Base.Option.map class_info ~f:(fun (class_id, name, _) -> (class_id, name)) in
    ( reason,
      {
        PatternObject.kind;
        props;
        class_info;
        keys_order;
        rest;
        contains_invalid_pattern = false;
        guarded = false;
      }
    )
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

  val to_pattern : t -> PatternUnion.t
end = struct
  type t = {
    leafs: LeafSet.t;
    tuples: ValueObject.t list;
    arrays: ValueObject.t list;
    objects: ValueObject.t list;
    enum_unknown_members: (Reason.t * LeafSet.t) list;
    inexhaustible: Type.t list;
  }

  let empty =
    {
      leafs = LeafSet.empty;
      tuples = [];
      arrays = [];
      objects = [];
      enum_unknown_members = [];
      inexhaustible = [];
    }

  let is_empty { leafs; tuples; arrays; objects; enum_unknown_members; inexhaustible } =
    LeafSet.is_empty leafs
    && Base.List.is_empty tuples
    && Base.List.is_empty arrays
    && Base.List.is_empty objects
    && Base.List.is_empty enum_unknown_members
    && Base.List.is_empty inexhaustible

  let to_pattern { leafs; tuples; arrays; objects; enum_unknown_members; inexhaustible } =
    let (tuples_exact, tuples_inexact) =
      tuples
      |> Base.List.map ~f:ValueObject.to_pattern
      |> Base.List.stable_sort ~compare:PatternObject.compare
      |> Base.List.foldi ~init:(IMap.empty, IMap.empty) ~f:(fun i acc tuple_pattern ->
             let (_, { PatternObject.kind; rest; _ }) = tuple_pattern in
             let tuple_pattern_with_index = (i, tuple_pattern) in
             match kind with
             | ObjKind.Tuple { length } ->
               let (tuples_exact, tuples_inexact) = acc in
               if Base.Option.is_some rest then
                 ( tuples_exact,
                   IMap.adjust
                     length
                     (function
                       | Some xs -> tuple_pattern_with_index :: xs
                       | None -> [tuple_pattern_with_index])
                     tuples_inexact
                 )
               else
                 ( IMap.adjust
                     length
                     (function
                       | Some xs -> tuple_pattern_with_index :: xs
                       | None -> [tuple_pattern_with_index])
                     tuples_exact,
                   tuples_inexact
                 )
             | ObjKind.Obj ->
               (* Tuples are always `ObjKind.Tuple` *)
               acc
         )
    in
    (* If we have arrays, add the empty inexact tuple pattern. *)
    let tuples_inexact =
      match arrays with
      | [] -> tuples_inexact
      | (reason, _) :: _ ->
        let i = Base.List.length tuples in
        IMap.adjust
          0
          (function
            | Some xs -> (i, empty_inexact_tuple_pattern reason) :: xs
            | None -> [(i, empty_inexact_tuple_pattern reason)])
          tuples_inexact
    in
    let objects =
      objects
      |> Base.List.map ~f:ValueObject.to_pattern
      |> Base.List.stable_sort ~compare:PatternObject.compare
      |> Base.List.mapi ~f:(fun i x -> (i, x))
    in
    let wildcard =
      match (inexhaustible, enum_unknown_members) with
      | ([], []) -> None
      | (first_t :: _, _) -> Some (TypeUtil.reason_of_t first_t)
      | (_, (reason, _) :: _) -> Some reason
    in
    { PatternUnion.empty with PatternUnion.leafs; tuples_exact; tuples_inexact; objects; wildcard }
end
