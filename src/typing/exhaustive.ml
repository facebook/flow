(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ALocSet = Loc_collections.ALocSet
module ReasonSet = Reason.ReasonSet

(****************************************)
(* Helper functions for analyzing types *)
(****************************************)

let singleton_concrete_type cx t : Type.t =
  Flow_js.FlowJs.singleton_concrete_type_for_match_arg
    cx
    ~keep_unions:false
    (TypeUtil.reason_of_t t)
    t

let possible_concrete_types cx ~keep_unions t : Type.t list =
  Flow_js.FlowJs.possible_concrete_types_for_match_arg cx ~keep_unions (TypeUtil.reason_of_t t) t

let attempt_union_rep_optimization cx (rep : Type.UnionRep.t) : unit =
  if not @@ Type.UnionRep.is_optimized_finally rep then
    Type.UnionRep.optimize
      rep
      ~reason_of_t:TypeUtil.reason_of_t
      ~reasonless_eq:(Concrete_type_eq.eq cx)
      ~flatten:(Type_mapper.union_flatten cx)
      ~find_resolved:(Context.find_resolved cx)
      ~find_props:(Context.find_props cx)

(***********************)
(* Datatype defintions *)
(***********************)

type pattern_ast_list =
  ((ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t * (* guarded *) bool) list

(* Either a primitive literal or a Flow Enum member.
   Used in both `PatternUnion`s and `ValueUnion`s. *)
module Leaf = struct
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

  let to_string (_, { kind; props; keys_order; rest; _ }) =
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
      Utils_js.spf "{%s}" (String.concat ", " props)
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

  let to_ast (_, { kind; props; keys_order; rest; _ }) =
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
      ( Loc.none,
        MatchPattern.ObjectPattern { MatchPattern.ObjectPattern.properties; rest; comments = None }
      )
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

  val of_patterns_ast : Context.t -> pattern_ast_list -> t

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

  (* Builder helpers *)

  let not_seen_wildcard (cx : Context.t) (pattern_union : t) (reason : Reason.t) : bool =
    let { PatternUnion.wildcard; _ } = pattern_union in
    match wildcard with
    | Some already_seen ->
      Flow_js.add_output
        cx
        (Error_message.EMatchUnusedPattern { reason; already_seen = Some already_seen });
      false
    | None -> true

  let add_leaf (cx : Context.t) ~(guarded : bool) (pattern_union : t) (leaf : Leaf.t) : t =
    let (reason, _) = leaf in
    if not_seen_wildcard cx pattern_union reason then
      let { PatternUnion.leafs; guarded_leafs; _ } = pattern_union in
      match LeafSet.find_opt leaf leafs with
      | Some (already_seen, _) ->
        Flow_js.add_output
          cx
          (Error_message.EMatchUnusedPattern { reason; already_seen = Some already_seen });
        pattern_union
      | None ->
        if guarded then
          { pattern_union with PatternUnion.guarded_leafs = leaf :: guarded_leafs }
        else
          { pattern_union with PatternUnion.leafs = LeafSet.add leaf leafs }
    else
      pattern_union

  let add_wildcard
      (cx : Context.t) ~(guarded : bool) ~(last : bool) (pattern_union : t) (reason : Reason.t) : t
      =
    if not_seen_wildcard cx pattern_union reason then
      if guarded then (
        ( if last then
          let loc = Reason.loc_of_reason reason in
          (* We avoid more complex analysis by simply erroring when there is a
             guarded wilcard which is in the last case. *)
          Flow_js.add_output cx (Error_message.EMatchInvalidGuardedWildcard loc)
        );
        pattern_union
      ) else
        { pattern_union with PatternUnion.wildcard = Some reason }
    else
      pattern_union

  let add_tuple
      (cx : Context.t) (pattern_union : t) ~(length : int) (tuple : PatternObject.with_index) : t =
    let (_, (reason, { PatternObject.kind; rest; _ })) = tuple in
    if not_seen_wildcard cx pattern_union reason then
      let { tuples_exact; tuples_inexact; _ } = pattern_union in
      let (tuples_exact, tuples_inexact) =
        if Base.Option.is_some rest || kind = ObjKind.Obj then
          ( tuples_exact,
            IMap.adjust
              length
              (function
                | Some xs -> tuple :: xs
                | None -> [tuple])
              tuples_inexact
          )
        else
          ( IMap.adjust
              length
              (function
                | Some xs -> tuple :: xs
                | None -> [tuple])
              tuples_exact,
            tuples_inexact
          )
      in
      { pattern_union with tuples_exact; tuples_inexact }
    else
      pattern_union

  let add_object (cx : Context.t) (pattern_union : t) (obj : PatternObject.with_index) : t =
    let (_, (reason, _)) = obj in
    if not_seen_wildcard cx pattern_union reason then
      let { objects; _ } = pattern_union in
      (* Accumulate backwards, reverse at end of pattern_union creation. *)
      { pattern_union with objects = obj :: objects }
    else
      pattern_union

  (* Construction from AST *)

  let leaf_of_type cx (t : Type.t) pattern_ast : Leaf.t option =
    let open Flow_ast.MatchPattern in
    let (loc, _) = pattern_ast in
    let reason = Reason.mk_reason Reason.RMatchPattern loc in
    match t with
    | Type.DefT (_, Type.EnumValueT (Type.ConcreteEnum enum_info)) ->
      (match pattern_ast with
      | ( _,
          MemberPattern
            ( _,
              {
                MemberPattern.property =
                  MemberPattern.PropertyIdentifier (_, { Flow_ast.Identifier.name = member_name; _ });
                _;
              }
            )
        ) ->
        Some (reason, Leaf.EnumMemberC { Leaf.member_name; enum_info })
      | (loc, _) ->
        let { Type.members; _ } = enum_info in
        let example_member = SMap.choose_opt members |> Base.Option.map ~f:fst in
        Flow_js.add_output
          cx
          (Error_message.EEnumInvalidCheck
             { loc; enum_reason = TypeUtil.reason_of_t t; example_member; from_match = true }
          );
        None)
    | Type.DefT (_, Type.SingletonBoolT { value; _ }) -> Some (reason, Leaf.BoolC value)
    | Type.DefT (_, Type.SingletonNumT { value; _ }) -> Some (reason, Leaf.NumC value)
    | Type.DefT (_, Type.SingletonBigIntT { value; _ }) -> Some (reason, Leaf.BigIntC value)
    | Type.DefT (_, Type.SingletonStrT { value; _ }) -> Some (reason, Leaf.StrC value)
    | Type.DefT (_, Type.NullT) -> Some (reason, Leaf.NullC)
    | Type.DefT (_, Type.VoidT) -> Some (reason, Leaf.VoidC)
    | Type.AnyT _ ->
      (* Ignore any-typed patterns. *)
      None
    | _ ->
      Flow_js.add_output
        cx
        (Error_message.EMatchInvalidIdentOrMemberPattern
           { loc; type_reason = TypeUtil.reason_of_t t }
        );
      None

  let rec of_pattern_ast
      (cx : Context.t) (pattern_ast : (ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t) : t =
    let (pattern_union, _) = of_pattern_ast' cx (empty, 0) ~guarded:false ~last:false pattern_ast in
    pattern_union

  and of_pattern_ast'
      (cx : Context.t)
      ((pattern_union, i) : t * int)
      ~(guarded : bool)
      ~(last : bool)
      (pattern_ast : (ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t) : t * int =
    let open Flow_ast.MatchPattern in
    let (loc, pat) = pattern_ast in
    let reason = Reason.mk_reason Reason.RMatchPattern loc in
    let next_i = i + 1 in
    match pat with
    | BindingPattern _ ->
      let reason = Reason.mk_reason Reason.RMatchWildcard loc in
      let pattern_union = add_wildcard cx ~guarded ~last pattern_union reason in
      (pattern_union, next_i)
    | WildcardPattern _ ->
      let reason = Reason.mk_reason Reason.RMatchWildcard loc in
      let pattern_union = add_wildcard cx ~guarded ~last pattern_union reason in
      (pattern_union, next_i)
    | BooleanPattern { Flow_ast.BooleanLiteral.value; _ } ->
      let leaf = (reason, Leaf.BoolC value) in
      let pattern_union = add_leaf cx ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | NumberPattern { Flow_ast.NumberLiteral.value; raw; _ } ->
      let leaf = (reason, Leaf.NumC (value, raw)) in
      let pattern_union = add_leaf cx ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | BigIntPattern { Flow_ast.BigIntLiteral.value; raw; _ } ->
      let leaf = (reason, Leaf.BigIntC (value, raw)) in
      let pattern_union = add_leaf cx ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | StringPattern { Flow_ast.StringLiteral.value; _ } ->
      let leaf = (reason, Leaf.StrC (Reason.OrdinaryName value)) in
      let pattern_union = add_leaf cx ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | NullPattern _ ->
      let leaf = (reason, Leaf.NullC) in
      let pattern_union = add_leaf cx ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | UnaryPattern
        {
          UnaryPattern.operator;
          argument = (_, UnaryPattern.NumberLiteral { Flow_ast.NumberLiteral.value; raw; _ });
          _;
        } ->
      let open UnaryPattern in
      if value = 0. then
        (* We've already errored on unary operators on zero. *)
        ({ pattern_union with contains_invalid_pattern = true }, i)
      else
        let literal =
          match operator with
          | Plus -> (value, raw)
          | Minus -> Flow_ast_utils.negate_number_literal (value, raw)
        in
        let leaf = (reason, Leaf.NumC literal) in
        let pattern_union = add_leaf cx ~guarded pattern_union leaf in
        (pattern_union, next_i)
    | UnaryPattern
        {
          UnaryPattern.operator;
          argument = (_, UnaryPattern.BigIntLiteral { Flow_ast.BigIntLiteral.value; raw; _ });
          _;
        } ->
      let open UnaryPattern in
      (match operator with
      | Plus ->
        (* We've already errored on `+` on bigint literal patterns. *)
        ({ pattern_union with contains_invalid_pattern = true }, i)
      | Minus ->
        let literal = (value, raw) in
        let leaf = (reason, Leaf.BigIntC literal) in
        let pattern_union = add_leaf cx ~guarded pattern_union leaf in
        (pattern_union, next_i))
    | IdentifierPattern ((_, t), _)
    | MemberPattern ((_, t), _) ->
      let t = singleton_concrete_type cx t in
      (match leaf_of_type cx t pattern_ast with
      | Some leaf -> (add_leaf cx ~guarded pattern_union leaf, next_i)
      | None -> ({ pattern_union with contains_invalid_pattern = true }, i))
    | AsPattern { AsPattern.pattern; _ } ->
      of_pattern_ast' cx (pattern_union, i) ~guarded ~last pattern
    | OrPattern { OrPattern.patterns; _ } ->
      Base.List.fold patterns ~init:(pattern_union, i) ~f:(fun acc pattern_ast ->
          of_pattern_ast' ~guarded ~last cx acc pattern_ast
      )
    | ArrayPattern { ArrayPattern.elements; rest; _ } ->
      let length = Base.List.length elements in
      let (props, keys_order_rev, contains_invalid_pattern) =
        Base.List.foldi
          elements
          ~init:(SMap.empty, [], false)
          ~f:(fun
               i
               (props, keys_order_rev, contains_invalid_pattern)
               { ArrayPattern.Element.pattern; _ }
             ->
            let key = string_of_int i in
            let value = of_pattern_ast cx pattern in
            let contains_invalid_pattern =
              contains_invalid_pattern || value.PatternUnion.contains_invalid_pattern
            in
            ( SMap.add key { PatternObject.Property.loc; value } props,
              key :: keys_order_rev,
              contains_invalid_pattern
            )
        )
      in
      let rest =
        Base.Option.map rest ~f:(fun (loc, _) -> Reason.mk_reason Reason.RArrayPatternRestProp loc)
      in
      let keys_order =
        Base.List.rev keys_order_rev
        |> Base.List.stable_sort ~compare:(fun key_a key_b ->
               PatternObject.Property.compare (SMap.find key_a props) (SMap.find key_b props)
           )
      in
      let tuple =
        ( next_i,
          ( reason,
            {
              PatternObject.props;
              keys_order;
              rest;
              kind = ObjKind.Tuple { length };
              contains_invalid_pattern;
              guarded;
            }
          )
        )
      in
      let pattern_union = add_tuple cx pattern_union ~length tuple in
      (pattern_union, next_i)
    | ObjectPattern { ObjectPattern.properties; rest; _ } ->
      let (props, keys_order_rev, tuple_like, contains_invalid_pattern) =
        Base.List.fold properties ~init:(SMap.empty, [], Some 0., false) ~f:(fun acc prop ->
            let (props, keys_order_rev, tuple_like, contains_invalid_pattern) = acc in
            match prop with
            | (_, ObjectPattern.Property.Valid { ObjectPattern.Property.key; pattern; _ }) ->
              let (loc, propname) =
                let open ObjectPattern.Property in
                match key with
                | Identifier ((loc, _), { Flow_ast.Identifier.name; _ }) -> (loc, name)
                | StringLiteral (loc, { Flow_ast.StringLiteral.value; _ }) -> (loc, value)
                | NumberLiteral (loc, { Flow_ast.NumberLiteral.value; _ }) ->
                  (loc, Dtoa.ecma_string_of_float value)
                | BigIntLiteral (loc, { Flow_ast.BigIntLiteral.raw; _ }) -> (loc, raw)
              in
              let value = of_pattern_ast cx pattern in
              (* If the object patterns seems like it could also match tuples, record that. *)
              let tuple_like =
                match tuple_like with
                | None -> None
                | Some prev_i ->
                  (match (propname, Float.of_string_opt propname) with
                  | (_, Some value) when Js_number.is_float_safe_integer value ->
                    Some (Float.max prev_i (value +. 1.))
                  | ("length", _) when only_wildcard value |> Base.Option.is_some -> tuple_like
                  | _ -> None)
              in
              let property = { PatternObject.Property.loc; value } in
              let props = SMap.add propname property props in
              let contains_invalid_pattern =
                contains_invalid_pattern || value.PatternUnion.contains_invalid_pattern
              in
              (props, propname :: keys_order_rev, tuple_like, contains_invalid_pattern)
            | _ -> (props, keys_order_rev, tuple_like, true)
        )
      in
      let keys_order =
        Base.List.rev keys_order_rev
        |> Base.List.stable_sort ~compare:(fun key_a key_b ->
               PatternObject.Property.compare (SMap.find key_a props) (SMap.find key_b props)
           )
      in
      let pattern_union =
        let rest =
          Base.Option.map rest ~f:(fun (loc, _) ->
              Reason.mk_reason Reason.RObjectPatternRestProp loc
          )
        in
        let obj =
          ( next_i,
            ( reason,
              {
                PatternObject.props;
                keys_order;
                rest;
                kind = ObjKind.Obj;
                contains_invalid_pattern;
                guarded;
              }
            )
          )
        in
        let pattern_union = add_object cx pattern_union obj in
        match tuple_like with
        | None -> pattern_union
        | Some i -> add_tuple cx ~length:(Int.of_float i) pattern_union obj
      in
      (pattern_union, next_i)

  let of_patterns_ast cx patterns_ast =
    let last_i = Base.List.length patterns_ast - 1 in
    let (pattern_union, _) =
      Base.List.foldi patterns_ast ~init:(empty, 0) ~f:(fun i acc (pattern_ast, guarded) ->
          of_pattern_ast' cx acc ~guarded ~last:(i = last_i) pattern_ast
      )
    in
    let { objects; _ } = pattern_union in
    { pattern_union with objects = Base.List.rev objects }

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
      keys_order = [];
      rest = Some reason;
      contains_invalid_pattern = false;
      guarded = false;
    }
  )

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

    val of_type_prop : Context.t -> string -> Type.Property.t -> t option
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
    rest: Reason.t option;
    (* We store a set of potential sentinel props, so that we can improve
       error messages, and also check these properties first when filtering
       by patterns. *)
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

    let of_type_prop cx key p =
      match Type.Property.read_loc p with
      | Some loc ->
        let (t, optional) =
          match Type.Property.read_t p with
          | Some t ->
            let optional =
              match t with
              | Type.OptionalT _ -> true
              | _ -> false
            in
            (t, optional)
          | None ->
            let t =
              Type.MixedT.why
                (Reason.mk_reason (Reason.RProperty (Some (Reason.OrdinaryName key))) loc)
            in
            let optional =
              match Type.Property.write_t p with
              | Some (Type.OptionalT _) -> true
              | Some _ -> false
              | None -> true
            in
            (t, optional)
        in
        Some { ValueObject.Property.loc; value = lazy (ValueUnion.of_type cx t); optional }
      | None -> None
  end

  module Properties = struct
    type t = Property.t option SMap.t

    let is_empty props = not @@ SMap.exists (fun _ prop -> Base.Option.is_some prop) props
  end

  type t' = {
    kind: ObjKind.t;
    t: Type.t;
    props: Properties.t;
    rest: Reason.t option;
    sentinel_props: SSet.t;
  }

  type t = Reason.t * t'

  let to_pattern (reason, { props; rest; kind; sentinel_props; _ }) =
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
    ( reason,
      {
        PatternObject.kind;
        props;
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
    inexhaustible: Type.t list;
  }

  val empty : t

  val is_empty : t -> bool

  val of_type : Context.t -> Type.t -> t

  val get_prop : Context.t -> ALoc.t * string -> Type.t -> ValueObject.Property.t option

  val to_pattern : t -> PatternUnion.t
end = struct
  type t = {
    leafs: LeafSet.t;
    tuples: ValueObject.t list;
    arrays: ValueObject.t list;
    objects: ValueObject.t list;
    inexhaustible: Type.t list;
  }

  let empty = { leafs = LeafSet.empty; tuples = []; arrays = []; objects = []; inexhaustible = [] }

  let is_empty { leafs; tuples; arrays; objects; inexhaustible } =
    LeafSet.is_empty leafs
    && Base.List.is_empty tuples
    && Base.List.is_empty arrays
    && Base.List.is_empty objects
    && Base.List.is_empty inexhaustible

  let rec of_type' (cx : Context.t) (value_union : t) (t : Type.t) : t =
    let (ts, all_sentinel_props) =
      possible_concrete_types cx ~keep_unions:true t
      |> Base.List.fold ~init:([], NameUtils.Set.empty) ~f:(fun (ts, all_sentinel_props) t ->
             match t with
             | Type.UnionT (_, rep) ->
               attempt_union_rep_optimization cx rep;
               let all_sentinel_props =
                 match Type.UnionRep.disjoint_object_union_props rep with
                 | Some props -> NameUtils.Set.union all_sentinel_props props
                 | None -> all_sentinel_props
               in
               let union_ts = possible_concrete_types cx ~keep_unions:false t in
               (Base.List.rev_append union_ts ts, all_sentinel_props)
             | _ -> (t :: ts, all_sentinel_props)
         )
    in
    Base.List.fold ts ~init:value_union ~f:(fun value_union t ->
        let { leafs; tuples; arrays; objects; inexhaustible } = value_union in
        match t with
        | Type.DefT (enum_reason, Type.EnumValueT (Type.ConcreteEnum enum_info)) ->
          let { Type.members; has_unknown_members; _ } = enum_info in
          let leafs =
            SMap.fold
              (fun member_name loc leafs ->
                let reason =
                  Reason.mk_reason
                    (Reason.REnumMember { enum = Reason.desc_of_reason enum_reason; member_name })
                    loc
                in
                LeafSet.add (reason, Leaf.EnumMemberC { Leaf.member_name; enum_info }) leafs)
              members
              leafs
          in
          let inexhaustible =
            if has_unknown_members then
              let t =
                TypeUtil.mod_reason_of_t
                  (Reason.replace_desc_reason
                     (Reason.REnumUnknownMembers (Reason.desc_of_reason enum_reason))
                  )
                  t
              in
              t :: inexhaustible
            else
              inexhaustible
          in
          { value_union with leafs; inexhaustible }
        | Type.DefT (reason, Type.SingletonBoolT { value; _ }) ->
          let leafs = LeafSet.add (reason, Leaf.BoolC value) leafs in
          { value_union with leafs }
        | Type.DefT (reason, Type.SingletonNumT { value; _ }) ->
          let leafs = LeafSet.add (reason, Leaf.NumC value) leafs in
          { value_union with leafs }
        | Type.DefT (reason, Type.SingletonBigIntT { value; _ }) ->
          let leafs = LeafSet.add (reason, Leaf.BigIntC value) leafs in
          { value_union with leafs }
        | Type.DefT (reason, Type.SingletonStrT { value; _ }) ->
          let leafs = LeafSet.add (reason, Leaf.StrC value) leafs in
          { value_union with leafs }
        | Type.DefT (reason, Type.NullT) ->
          let leafs = LeafSet.add (reason, Leaf.NullC) leafs in
          { value_union with leafs }
        | Type.DefT (reason, Type.VoidT) ->
          let leafs = LeafSet.add (reason, Leaf.VoidC) leafs in
          { value_union with leafs }
        | Type.DefT (reason, Type.BoolGeneralT) ->
          let leafs = LeafSet.add (reason, Leaf.BoolC true) leafs in
          let leafs = LeafSet.add (reason, Leaf.BoolC false) leafs in
          { value_union with leafs }
        | Type.DefT
            (reason, Type.ArrT (Type.TupleAT { elements; arity = (num_req, num_total); inexact; _ }))
          ->
          let rest =
            if inexact then
              Some reason
            else
              None
          in
          let props_list =
            Base.List.map
              elements
              ~f:(fun (Type.TupleElement { t; optional; reason; polarity; name = _ }) ->
                let t =
                  match (optional, t) with
                  | (true, Type.OptionalT { type_; _ }) -> type_
                  | (_, t) -> t
                in
                let t =
                  if Polarity.compat (polarity, Polarity.Positive) then
                    t
                  else
                    Type.MixedT.why reason
                in
                let value = lazy (of_type cx t) in
                let loc = Reason.loc_of_reason reason in
                { ValueObject.Property.loc; value; optional = false }
            )
          in
          let tuples =
            Base.List.init (num_total - num_req + 1) ~f:(fun i -> num_req + i)
            |> Base.List.fold ~init:tuples ~f:(fun tuples length ->
                   let props =
                     Base.List.take props_list length
                     |> Base.List.foldi ~init:SMap.empty ~f:(fun i acc prop ->
                            let key = string_of_int i in
                            SMap.add key (Some prop) acc
                        )
                   in
                   let tuple =
                     ( reason,
                       {
                         ValueObject.kind = ObjKind.Tuple { length };
                         t;
                         props;
                         rest;
                         sentinel_props = SSet.empty;
                       }
                     )
                   in
                   tuple :: tuples
               )
          in
          { value_union with tuples }
        | Type.DefT (reason, Type.ArrT (Type.ArrayAT _ | Type.ROArrayAT _)) ->
          let arr =
            ( reason,
              {
                ValueObject.kind = ObjKind.Obj;
                t;
                props = SMap.empty;
                rest = Some reason;
                sentinel_props = SSet.empty;
              }
            )
          in
          { value_union with arrays = arr :: arrays }
        | Type.DefT
            ( reason,
              Type.ObjT
                {
                  Type.flags = { Type.obj_kind; _ };
                  props_tmap;
                  proto_t = _;
                  call_t = None;
                  reachable_targs = _;
                }
            ) ->
          let rest =
            match obj_kind with
            | Type.Exact -> None
            | Type.Inexact -> Some reason
            | Type.Indexed { Type.key; _ } -> Some (TypeUtil.reason_of_t key)
          in
          let (props, sentinel_props) =
            NameUtils.Map.fold
              (fun name p acc ->
                match name with
                | Reason.OrdinaryName key ->
                  let (props, sentinel_props) = acc in
                  let prop = ValueObject.Property.of_type_prop cx key p in
                  let props = SMap.add key prop props in
                  let sentinel_props =
                    if NameUtils.Set.mem name all_sentinel_props then
                      SSet.add key sentinel_props
                    else
                      sentinel_props
                  in
                  (props, sentinel_props)
                | _ -> acc)
              (Context.find_props cx props_tmap)
              (SMap.empty, SSet.empty)
          in
          let obj = (reason, { ValueObject.kind = ObjKind.Obj; t; props; rest; sentinel_props }) in
          { value_union with objects = obj :: objects }
        | Type.DefT (reason, Type.InstanceT _) ->
          let rest = Some reason in
          let obj =
            ( reason,
              {
                ValueObject.kind = ObjKind.Obj;
                t;
                props = SMap.empty;
                rest;
                sentinel_props = SSet.empty;
              }
            )
          in
          { value_union with objects = obj :: objects }
        | Type.DefT (_, Type.EmptyT) -> value_union
        | t -> { value_union with inexhaustible = t :: inexhaustible }
    )

  (* We implement this ourselves for now because we want to know if we are getting a
     property from a indexer or not. Also, to not trigger method unbinding errors.
     TODO: Update existing machinery to return the info we want and use that instead. *)
  and get_prop cx (key : ALoc.t * string) (obj : Type.t) : ValueObject.Property.t option =
    let get_prop_from_dict cx (key_loc, key_name) dict =
      match dict with
      | Some { Type.key = dict_key_t; value = dict_value_t; dict_polarity; dict_name = _ } ->
        let reason_key =
          Reason.mk_reason (Reason.RProperty (Some (Reason.OrdinaryName key_name))) key_loc
        in
        let key_t = Flow_js_utils.type_of_key_name cx (Reason.OrdinaryName key_name) reason_key in
        if
          Polarity.compat (dict_polarity, Polarity.Positive)
          && Flow_js.FlowJs.speculative_subtyping_succeeds cx key_t dict_key_t
        then
          let loc = TypeUtil.reason_of_t dict_key_t |> Reason.loc_of_reason in
          Some { ValueObject.Property.loc; value = lazy (of_type cx dict_value_t); optional = true }
        else
          None
      | None -> None
    in
    let find_key ~super ~props_list ~dict key =
      let (_, key_name) = key in
      let current_prop =
        Base.List.find_map props_list ~f:(fun id ->
            Context.get_prop cx id (Reason.OrdinaryName key_name)
            |> Base.Option.bind ~f:(fun p : ValueObject.Property.t option ->
                   ValueObject.Property.of_type_prop cx key_name p
               )
        )
      in
      if Base.Option.is_some current_prop then
        current_prop
      else
        let super_prop = get_prop cx key super in
        if Base.Option.is_some super_prop then
          super_prop
        else
          get_prop_from_dict cx key dict
    in
    let (_, key_name) = key in
    match singleton_concrete_type cx obj with
    | Type.DefT (_, Type.ObjT Type.{ flags = { obj_kind; _ }; props_tmap; proto_t; _ }) ->
      let dict = Obj_type.get_dict_opt obj_kind in
      find_key ~super:proto_t ~props_list:[props_tmap] ~dict key
    | Type.DefT
        (_, Type.InstanceT Type.{ super; inst = { own_props; proto_props; inst_dict; _ }; _ }) ->
      find_key ~super ~props_list:[own_props; proto_props] ~dict:inst_dict key
    | Type.NullProtoT _ -> None
    | Type.ObjProtoT reason
      when Flow_js_utils.is_object_prototype_method (Reason.OrdinaryName key_name) ->
      let t = Flow_js.get_builtin_type cx reason "Object" in
      get_prop cx key t
    | Type.DefT (reason, Type.ArrT arr) ->
      let elem_t = Type.elemt_of_arrtype arr in
      let t = Flow_js.get_builtin_typeapp cx reason "Array" [elem_t] in
      get_prop cx key t
    | Type.IntersectionT (_, rep) ->
      Type.InterRep.members rep |> Base.List.find_map ~f:(fun t -> get_prop cx key t)
    | _ -> None

  and of_type cx t =
    let { leafs; tuples; arrays; objects; inexhaustible } = of_type' cx empty t in
    (* The list members of the `ValueUnion` are accumulated in reverse order,
       put them back in their original order. *)
    {
      leafs;
      tuples = Base.List.rev tuples;
      arrays = Base.List.rev arrays;
      objects = Base.List.rev objects;
      inexhaustible = Base.List.rev inexhaustible;
    }

  let to_pattern { leafs; tuples; arrays; objects; inexhaustible } =
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
      match inexhaustible with
      | [] -> None
      | first_t :: _ -> Some (TypeUtil.reason_of_t first_t)
    in
    { PatternUnion.empty with PatternUnion.leafs; tuples_exact; tuples_inexact; objects; wildcard }
end

(*******************)
(* Filtering logic *)
(*******************)

let is_leaf_subtype_of_inexhaustible cx (leaf : Leaf.t) (inexhaustible : Type.t list) : bool =
  Base.List.exists inexhaustible ~f:(fun t ->
      Flow_js.FlowJs.speculative_subtyping_succeeds cx (Leaf.to_type leaf) t
  )

let is_object_subtype_of_inexhaustible (inexhaustible : Type.t list) : Reason.t option =
  let open Type in
  Base.List.find_map inexhaustible ~f:(function
      | DefT (_, MixedT Mixed_function) -> None
      | DefT (r, MixedT _)
      | AnyT (r, _) ->
        Some r
      | _ -> None
      )

type filter_object_result =
  | Match of {
      used_pattern_locs: ALocSet.t;
      (* If an object value was only partially matched by a pattern,
         we store the remainders so we can add them to the queue to check. *)
      queue_additions: ValueObject.t list;
      matched: ValueObject.t;
    }
  | NoMatch of {
      used_pattern_locs: ALocSet.t;
      left: ValueObject.t;
    }

type filter_union_result = {
  value_left: ValueUnion.t;
  value_matched: ValueUnion.t;
  used_pattern_locs: ALocSet.t;
}

(* Filter some values by some patterns. This results in values which were matched by the patterns,
   and those which were not matched and left over. We also computed the set of locs of patterns
   which were useful - that is used to computed the unnecessary patterns. *)
let rec filter_values_by_patterns cx ~(value_union : ValueUnion.t) ~(pattern_union : PatternUnion.t)
    : filter_union_result =
  let {
    ValueUnion.leafs = value_leafs;
    tuples = value_tuples;
    arrays;
    objects = value_objects;
    inexhaustible;
  } =
    value_union
  in
  let {
    PatternUnion.leafs = pattern_leafs;
    guarded_leafs;
    tuples_exact = pattern_tuples_exact;
    tuples_inexact = pattern_tuples_inexact;
    objects = pattern_objects;
    wildcard;
    contains_invalid_pattern = _;
  } =
    pattern_union
  in
  (* Leafs *)
  let (leafs_matched, leafs_left) =
    LeafSet.partition (fun leaf -> LeafSet.mem leaf pattern_leafs) value_leafs
  in
  let used_pattern_locs =
    LeafSet.fold (mark_leaf_pattern_used cx leafs_matched inexhaustible) pattern_leafs ALocSet.empty
  in
  let used_pattern_locs =
    Base.List.fold guarded_leafs ~init:used_pattern_locs ~f:(fun acc leaf ->
        mark_leaf_pattern_used cx value_leafs inexhaustible leaf acc
    )
  in
  (* The `undefined` pattern is always marked as used, so that we do not tell users
     to remove it in unsafe situations that could arise due to Flow's unsoundness. *)
  let used_pattern_locs =
    match LeafSet.find_opt (Reason.locationless_reason Reason.RVoid, Leaf.VoidC) pattern_leafs with
    | Some (reason, _) -> ALocSet.add (Reason.loc_of_reason reason) used_pattern_locs
    | None -> used_pattern_locs
  in
  (* Tuples *)
  let tuples_lte_length tuples length =
    (* We add `1` to the `length` so that `less_than` is "<=" *)
    let (less_than, _, _) = IMap.split (length + 1) tuples in
    IMap.values less_than |> Base.List.concat
  in
  let tuples_gte_length tuples length =
    (* We subtract `1` to the `length` so that `greater_than` is ">=" *)
    let (_, _, greater_than) = IMap.split (length - 1) tuples in
    IMap.values greater_than |> Base.List.concat
  in
  let (tuples_left, tuples_matched, used_pattern_locs) =
    Base.List.fold
      value_tuples
      ~init:([], [], used_pattern_locs)
      ~f:(fun (acc_tuples_left, acc_tuples_matched, acc_used_pattern_locs) tuple_value ->
        let (_, { ValueObject.rest = value_rest; kind = value_kind; _ }) = tuple_value in
        let value_is_inexact = Base.Option.is_some value_rest in
        let pattern_tuples =
          match value_kind with
          | ObjKind.Tuple { length } ->
            if value_is_inexact then
              let pattern_tuples = pattern_tuples_inexact |> IMap.values |> Base.List.concat in
              Base.List.rev_append pattern_tuples (tuples_gte_length pattern_tuples_exact length)
              |> sort_object_patterns_by_index
            else
              let exact_length_pattern_tuples =
                IMap.find_opt length pattern_tuples_exact |> Base.Option.value ~default:[]
              in
              let inexact_pattern_tuples = tuples_lte_length pattern_tuples_inexact length in
              Base.List.rev_append exact_length_pattern_tuples inexact_pattern_tuples
              |> sort_object_patterns_by_index
          | ObjKind.Obj ->
            (* Only `ObjKind.Tuple` are added to `value_tuples` *)
            []
        in
        if Base.List.is_empty pattern_tuples then
          (tuple_value :: acc_tuples_left, acc_tuples_matched, acc_used_pattern_locs)
        else
          let (tuples_left, tuples_matched, used_pattern_locs) =
            filter_objects_by_patterns cx [tuple_value] pattern_tuples
          in
          let acc_used_pattern_locs = ALocSet.union acc_used_pattern_locs used_pattern_locs in
          ( Base.List.rev_append tuples_left acc_tuples_left,
            Base.List.rev_append tuples_matched acc_tuples_matched,
            acc_used_pattern_locs
          )
    )
  in
  (* Arrays *)
  let (arrays_left, arrays_matched, array_used_pattern_locs) =
    if Base.List.is_empty arrays then
      (arrays, [], ALocSet.empty)
    else
      filter_objects_by_patterns cx arrays (PatternUnion.all_tuples_and_objects pattern_union)
  in
  let used_pattern_locs = ALocSet.union used_pattern_locs array_used_pattern_locs in
  (* Objects *)
  let (objects_left, objects_matched, obj_used_pattern_locs) =
    filter_objects_by_patterns cx value_objects pattern_objects
  in
  let used_pattern_locs = ALocSet.union used_pattern_locs obj_used_pattern_locs in
  let value_left =
    {
      ValueUnion.leafs = leafs_left;
      tuples = tuples_left;
      arrays = arrays_left;
      objects = objects_left;
      inexhaustible;
    }
  in
  (* Mixed/any *)
  let mixed_used_pattern_locs =
    match is_object_subtype_of_inexhaustible inexhaustible with
    | None -> ALocSet.empty
    | Some reason -> visit_mixed cx reason pattern_union
  in
  let used_pattern_locs = ALocSet.union used_pattern_locs mixed_used_pattern_locs in
  (* Wildcard *)
  match wildcard with
  | Some wildcard ->
    let used_pattern_locs =
      if not @@ ValueUnion.is_empty value_left then
        let loc = Reason.loc_of_reason wildcard in
        ALocSet.add loc used_pattern_locs
      else
        used_pattern_locs
    in
    { value_left = ValueUnion.empty; value_matched = value_union; used_pattern_locs }
  | None ->
    let value_matched =
      {
        ValueUnion.leafs = leafs_matched;
        tuples = tuples_matched;
        arrays = arrays_matched;
        objects = objects_matched;
        inexhaustible = [];
      }
    in
    { value_left; value_matched; used_pattern_locs }

(* Given a list of object values, and a list of object patterns, match the
   values against the patterns, and compute which objects were matched, and
   which were not matched. *)
and filter_objects_by_patterns
    cx (value_objects : ValueObject.t list) (pattern_objects : PatternObject.with_index list) :
    ValueObject.t list * ValueObject.t list * ALocSet.t =
  let rec f acc = function
    | [] -> acc
    | value_object :: rest_queue ->
      let (acc_left, acc_matched, acc_used_pattern_locs) = acc in
      (* Fold over the object patterns over the current object value. *)
      let (result, additional_used_pattern_locs) =
        Base.List.fold
          pattern_objects
          ~init:(NoMatch { used_pattern_locs = ALocSet.empty; left = value_object }, ALocSet.empty)
          ~f:(fun acc (_, pattern_object) ->
            let (result, additional_used_pattern_locs) = acc in
            match result with
            | Match _ -> acc
            | NoMatch { used_pattern_locs; left = _ } ->
              let additional_used_pattern_locs =
                ALocSet.union additional_used_pattern_locs used_pattern_locs
              in
              (filter_object_by_pattern cx value_object pattern_object, additional_used_pattern_locs))
      in
      let (queue, acc_left, acc_matched, used_pattern_locs) =
        match result with
        | NoMatch { used_pattern_locs; left } ->
          let acc_left = left :: acc_left in
          (rest_queue, acc_left, acc_matched, used_pattern_locs)
        | Match { used_pattern_locs; queue_additions; matched } ->
          let queue =
            if Base.List.is_empty queue_additions then
              rest_queue
            else
              Base.List.rev_append queue_additions rest_queue
          in
          let acc_matched = matched :: acc_matched in
          (queue, acc_left, acc_matched, used_pattern_locs)
      in
      let acc_used_pattern_locs = ALocSet.union acc_used_pattern_locs used_pattern_locs in
      let acc_used_pattern_locs =
        ALocSet.union acc_used_pattern_locs additional_used_pattern_locs
      in
      f (acc_left, acc_matched, acc_used_pattern_locs) queue
  in
  f ([], [], ALocSet.empty) value_objects

(* Filter an object value by an object pattern. *)
and filter_object_by_pattern cx (value_object : ValueObject.t) (pattern_object : PatternObject.t) :
    filter_object_result =
  let open Base.Continue_or_stop in
  let ( reason_value,
        { ValueObject.props = value_props; rest = value_rest; t; kind = value_kind; sentinel_props }
      ) =
    value_object
  in
  let ( reason_pattern,
        {
          PatternObject.props = pattern_props;
          keys_order;
          rest = pattern_rest;
          kind = pattern_kind;
          guarded;
          contains_invalid_pattern = _;
        }
      ) =
    pattern_object
  in
  (* Sort the keys that are sentinel props for the value first. *)
  let pattern_keys =
    keys_order |> Base.List.partition_tf ~f:(fun key -> SSet.mem key sentinel_props)
    |> fun (sentinel_keys, other_keys) -> sentinel_keys @ other_keys
  in
  (* If every key in the pattern also exists in the value, then return the props of the
     value, otherwise return `None`, so we can skip below directly to `NoMatch`. *)
  let value_props =
    Base.List.fold_until
      pattern_keys
      ~init:value_props
      ~f:(fun value_props key ->
        match SMap.find_opt key value_props with
        | Some (Some _) -> Continue value_props
        | Some None -> Stop None
        | None ->
          let { PatternObject.Property.loc = key_loc; _ } = SMap.find key pattern_props in
          let prop = ValueUnion.get_prop cx (key_loc, key) t in
          if Base.Option.is_none prop then
            Stop None
          else
            Continue (SMap.add key prop value_props))
      ~finish:Base.Option.some
  in
  match value_props with
  | None -> NoMatch { used_pattern_locs = ALocSet.empty; left = value_object }
  | Some value_props ->
    (* If this pattern is gaurded, then it can't filter out values. *)
    let no_match = guarded in
    (* We fold over the `pattern_keys` of the pattern and match the value at that key to the pattern
       at that key. We build up `head` which is the properties matched so far. The `remainder_value`
       is the properties of the value left to check. If we don't have a match, but need to continue
       checking for the purposes of marking patterns as used, we set `no_match` to true. *)
    Base.List.fold_until
      pattern_keys
      ~init:(ALocSet.empty, no_match, [], SMap.empty, value_props)
      ~f:(fun (used_pattern_locs, no_match, queue_additions, head, remainder_value) key ->
        let { PatternObject.Property.value = pattern; _ } = SMap.find key pattern_props in
        (* Checked above in `has_all_props` *)
        let prop_value = SMap.find key value_props |> Base.Option.value_exn in
        let { ValueObject.Property.loc = loc_value; value; optional } = prop_value in
        let remainder_value = SMap.remove key remainder_value in
        let (value_left, value_matched, value_matched_is_empty, new_used_pattern_locs) =
          match PatternUnion.only_wildcard pattern with
          | Some wildcard ->
            (ValueUnion.empty, value, false, ALocSet.singleton (Reason.loc_of_reason wildcard))
          | None ->
            let { value_left; value_matched; used_pattern_locs = new_used_pattern_locs } =
              filter_values_by_patterns cx ~value_union:(Lazy.force value) ~pattern_union:pattern
            in
            ( value_left,
              lazy value_matched,
              ValueUnion.is_empty value_matched,
              new_used_pattern_locs
            )
        in
        let used_pattern_locs = ALocSet.union used_pattern_locs new_used_pattern_locs in
        let property_matched =
          { ValueObject.Property.loc = loc_value; value = value_matched; optional }
        in
        let head = SMap.add key (Some property_matched) head in
        if no_match || value_matched_is_empty then
          if ALocSet.is_empty new_used_pattern_locs then
            Stop (NoMatch { used_pattern_locs = ALocSet.empty; left = value_object })
          else
            let no_match = true in
            Continue (used_pattern_locs, no_match, queue_additions, head, remainder_value)
        else if (not optional) && ValueUnion.is_empty value_left then
          (* Full match *)
          Continue (used_pattern_locs, no_match, queue_additions, head, remainder_value)
        else
          (* Partial match *)
          let (props_left, rest) =
            if ValueUnion.is_empty value_left then
              (* Optional value is ignored from left pattern, but then there are additional,
                 unknown properties. *)
              let value_rest =
                if Base.Option.is_some value_rest then
                  value_rest
                else
                  Some
                    (Reason.mk_reason
                       (Reason.RUnknownUnspecifiedProperty (Reason.desc_of_reason reason_value))
                       loc_value
                    )
              in
              (SMap.add key None head, value_rest)
            else
              let property_left =
                { ValueObject.Property.loc = loc_value; value = lazy value_left; optional }
              in
              (SMap.add key (Some property_left) head, value_rest)
          in
          let props_left = SMap.union props_left remainder_value in
          let object_left =
            ( reason_value,
              { ValueObject.props = props_left; rest; t; kind = value_kind; sentinel_props }
            )
          in
          let queue_additions = object_left :: queue_additions in
          Continue (used_pattern_locs, no_match, queue_additions, head, remainder_value))
      ~finish:(fun (used_pattern_locs, no_match, queue_additions, head, remainder_value) ->
        let pattern_loc = Reason.loc_of_reason reason_pattern in
        let used_pattern_locs = ALocSet.add pattern_loc used_pattern_locs in
        let used_pattern_locs =
          if
            Base.Option.is_some value_rest
            || (not @@ ValueObject.Properties.is_empty remainder_value)
          then
            match pattern_rest with
            | None ->
              ( if pattern_kind = ObjKind.Obj then
                let missing_props =
                  SMap.fold
                    (fun key prop acc ->
                      if Base.Option.is_some prop then
                        key :: acc
                      else
                        acc)
                    remainder_value
                    []
                  |> Base.List.rev
                in
                Flow_js.add_output
                  cx
                  (Error_message.EMatchNonExhaustiveObjectPattern
                     { loc = Reason.loc_of_reason reason_pattern; rest = value_rest; missing_props }
                  )
              );
              used_pattern_locs
            | Some pattern_rest -> ALocSet.add (Reason.loc_of_reason pattern_rest) used_pattern_locs
          else
            used_pattern_locs
        in
        let non_matching_rest =
          pattern_kind <> ObjKind.Obj
          && Base.Option.is_some value_rest
          && Base.Option.is_none pattern_rest
        in
        if no_match || non_matching_rest then
          NoMatch { used_pattern_locs; left = value_object }
        else
          let matched =
            ( reason_value,
              { ValueObject.props = head; rest = value_rest; t; kind = value_kind; sentinel_props }
            )
          in
          Match { used_pattern_locs; queue_additions; matched })

(* mixed/any values mark object and tuple patterns as used. *)
and visit_mixed cx (reason : Reason.t) (pattern_union : PatternUnion.t) : ALocSet.t =
  let {
    PatternUnion.tuples_exact = pattern_tuples_exact;
    tuples_inexact = pattern_tuples_inexact;
    objects = pattern_objects;
    _;
  } =
    pattern_union
  in
  let no_object_patterns = Base.List.is_empty pattern_objects in
  let array_used_pattern_locs =
    if
      no_object_patterns
      && IMap.is_empty pattern_tuples_exact
      && IMap.is_empty pattern_tuples_inexact
    then
      ALocSet.empty
    else
      (* As a shorthand to mark tuple patterns as used, if our value is mixed/any, we
         can match against `$ReadOnlyArray<mixed>` *)
      let mixed_array =
        ( reason,
          {
            ValueObject.kind = ObjKind.Obj;
            t = Type.DefT (reason, Type.ArrT (Type.ROArrayAT (Type.MixedT.why reason, None)));
            props = SMap.empty;
            rest = Some reason;
            sentinel_props = SSet.empty;
          }
        )
      in
      let pattern_objects = PatternUnion.all_tuples_and_objects pattern_union in
      let (_, _, used_pattern_locs) = filter_objects_by_patterns cx [mixed_array] pattern_objects in
      used_pattern_locs
  in
  let object_used_pattern_locs =
    if no_object_patterns then
      ALocSet.empty
    else
      (* As a shorthand to mark object patterns as used, if our value is mixed/any, we
         can match against `{+[string]: mixed}` *)
      let id = Context.generate_property_map cx NameUtils.Map.empty in
      let flags =
        {
          Type.default_flags with
          Type.obj_kind =
            Type.Indexed
              Type.
                {
                  dict_name = None;
                  key = StrModuleT.make reason;
                  value = MixedT.why reason;
                  dict_polarity = Polarity.Positive;
                };
        }
      in
      let t =
        Type.DefT (reason, Type.ObjT Type.(mk_objecttype ~flags ~call:None id dummy_prototype))
      in
      let mixed_object =
        ( reason,
          {
            ValueObject.kind = ObjKind.Obj;
            t;
            props = SMap.empty;
            rest = Some reason;
            sentinel_props = SSet.empty;
          }
        )
      in
      let (_, _, used_pattern_locs) =
        filter_objects_by_patterns cx [mixed_object] pattern_objects
      in
      used_pattern_locs
  in
  ALocSet.union array_used_pattern_locs object_used_pattern_locs

and mark_leaf_pattern_used
    (cx : Context.t)
    (value_leafs_matched : LeafSet.t)
    (inexhaustible : Type.t list)
    (leaf_pattern : Leaf.t)
    (used_pattern_locs : ALocSet.t) : ALocSet.t =
  let (reason, _) = leaf_pattern in
  let loc = Reason.loc_of_reason reason in
  if
    LeafSet.mem leaf_pattern value_leafs_matched
    || is_leaf_subtype_of_inexhaustible cx leaf_pattern inexhaustible
  then
    ALocSet.add loc used_pattern_locs
  else
    used_pattern_locs

(***************************)
(* Unused pattern analysis *)
(***************************)
(* We've accumulated a set of locs of every pattern that was used in some way.
   Now we iterate through the patterns, and we error on the outermost unused patterns.
   If there are patterns which contain invalid patterns, for which we have already emitted
   other errors, do not also emit an unused pattern error. *)
let rec check_for_unused_patterns cx (pattern_union : PatternUnion.t) (used_pattern_locs : ALocSet.t)
    : unit =
  let { PatternUnion.leafs; guarded_leafs; wildcard; _ } = pattern_union in
  let check reason =
    let loc = Reason.loc_of_reason reason in
    ALocSet.mem loc used_pattern_locs
  in
  let error reason =
    Flow_js.add_output cx (Error_message.EMatchUnusedPattern { reason; already_seen = None })
  in
  LeafSet.iter (fun (reason, _) -> if not @@ check reason then error reason) leafs;
  Base.List.iter guarded_leafs ~f:(fun (reason, _) -> if not @@ check reason then error reason);
  Base.List.iter
    (PatternUnion.all_tuples_and_objects pattern_union)
    ~f:(fun (_, (reason, { PatternObject.props; rest; contains_invalid_pattern; _ })) ->
      if contains_invalid_pattern then
        ()
      else if not @@ check reason then
        error reason
      else
        SMap.iter
          (fun _ { PatternObject.Property.value; loc = _ } ->
            check_for_unused_patterns cx value used_pattern_locs;
            Base.Option.iter rest ~f:(fun reason -> if not @@ check reason then error reason))
          props
  );
  Base.Option.iter wildcard ~f:(fun reason -> if not @@ check reason then error reason)

(***************)
(* Entry point *)
(***************)
(* If there is no values left over after filtering by the patterns, the check is exhaustive.
   Otherwise, build up examples of patterns that could be added to make the `match` exhaustive.
   Then, check the patterns to see if any are unused. *)
let analyze cx ~match_loc patterns arg_t =
  let pattern_union = PatternUnion.of_patterns_ast cx patterns in
  let value_union = ValueUnion.of_type cx arg_t in
  let { value_left; used_pattern_locs; value_matched = _ } =
    filter_values_by_patterns cx ~value_union ~pattern_union
  in
  ( if not @@ ValueUnion.is_empty value_left then
    let { ValueUnion.leafs; tuples; arrays; objects; inexhaustible } = value_left in
    let leafs =
      LeafSet.elements leafs
      |> Base.List.map ~f:(fun (reason, leaf) ->
             let example = Leaf.to_string leaf in
             (example, Leaf.to_ast leaf, [reason])
         )
    in
    (* Sort examples based on their original order *)
    let compare_examples (_, (a, _, _)) (_, (b, _, _)) = Int.compare a b in
    (* Build up map of example to reason set *)
    let tuple_examples_map =
      tuples
      |> Base.List.map ~f:ValueObject.to_pattern
      |> Base.List.stable_sort ~compare:PatternObject.compare
      |> Base.List.foldi ~init:SMap.empty ~f:(fun i acc tuple_pattern ->
             let (reason, _) = tuple_pattern in
             let example = PatternObject.to_string tuple_pattern in
             SMap.adjust
               example
               (function
                 | None -> (i, PatternObject.to_ast tuple_pattern, ReasonSet.singleton reason)
                 | Some (i, ast, reasons) -> (i, ast, ReasonSet.add reason reasons))
               acc
         )
    in
    (* Add the the pattern that matches all arrays to the tuple examples map *)
    let tuple_examples_map =
      match arrays with
      | [] -> tuple_examples_map
      | (reason, _) :: _ ->
        let reasons =
          Base.List.fold arrays ~init:ReasonSet.empty ~f:(fun acc (reason, _) ->
              ReasonSet.add reason acc
          )
        in
        let i = Base.List.length tuples in
        let pattern = empty_inexact_tuple_pattern reason in
        SMap.adjust
          (PatternObject.to_string pattern)
          (function
            | None -> (i, PatternObject.to_ast pattern, reasons)
            | Some (i, ast, existing_reasons) -> (i, ast, ReasonSet.union existing_reasons reasons))
          tuple_examples_map
    in
    (* Turn the map into a list of examples *)
    let tuple_examples_list =
      tuple_examples_map
      |> SMap.elements
      |> Base.List.sort ~compare:compare_examples
      |> Base.List.map ~f:(fun (example, (_, ast, reasons)) ->
             (example, ast, ReasonSet.elements reasons)
         )
    in
    (* Compute the list of object examples *)
    let object_examples_list =
      objects
      |> Base.List.map ~f:ValueObject.to_pattern
      |> Base.List.stable_sort ~compare:PatternObject.compare
      |> Base.List.foldi ~init:SMap.empty ~f:(fun i acc object_pattern ->
             let (reason, _) = object_pattern in
             let example = PatternObject.to_string object_pattern in
             SMap.adjust
               example
               (function
                 | None -> (i, PatternObject.to_ast object_pattern, ReasonSet.singleton reason)
                 | Some (i, ast, reasons) -> (i, ast, ReasonSet.add reason reasons))
               acc
         )
      |> SMap.elements
      |> Base.List.sort ~compare:compare_examples
      |> Base.List.map ~f:(fun (example, (_, ast, reasons)) ->
             (example, ast, ReasonSet.elements reasons)
         )
    in
    let wildcard =
      match inexhaustible with
      | [] -> []
      | first_t :: _ ->
        let pattern = wildcard_pattern (TypeUtil.reason_of_t first_t) in
        [
          ( PatternUnion.to_string pattern,
            PatternUnion.to_ast pattern,
            Base.List.fold inexhaustible ~init:ReasonSet.empty ~f:(fun acc t ->
                ReasonSet.add (TypeUtil.reason_of_t t) acc
            )
            |> ReasonSet.elements
          );
        ]
    in
    let examples = List.concat [leafs; tuple_examples_list; object_examples_list; wildcard] in
    Flow_js.add_output cx (Error_message.EMatchNotExhaustive { loc = match_loc; examples })
  );
  check_for_unused_patterns cx pattern_union used_pattern_locs
