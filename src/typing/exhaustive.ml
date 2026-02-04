(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ALocSet = Loc_collections.ALocSet
module ALocIDSet = Loc_collections.ALocIDSet
module ReasonSet = Reason.ReasonSet
open Match_pattern_ir

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

let get_class_info cx (t : Type.t) : (ALoc.id * string option) option =
  let instance_t =
    match singleton_concrete_type cx t with
    | Type.DefT (_, Type.ClassT t) -> singleton_concrete_type cx t
    | _ -> Type.EmptyT.why (TypeUtil.reason_of_t t)
  in
  match instance_t with
  | Type.DefT
      ( _,
        Type.InstanceT
          {
            Type.inst =
              { Type.inst_kind = Type.ClassKind | Type.RecordKind _; class_id; class_name; _ };
            _;
          }
      ) ->
    Some (class_id, class_name)
  | _ -> None

(***********************)
(* Datatype builders   *)
(***********************)

module PatternUnionBuilder : sig
  val of_patterns_ast : Context.t -> raise_errors:bool -> pattern_ast_list -> PatternUnion.t

  (* For incremental building: add a single pattern to an existing (unfinalized) PatternUnion *)
  val add_pattern :
    Context.t ->
    raise_errors:bool ->
    PatternUnion.t * int ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t * bool ->
    last:bool ->
    PatternUnion.t * int

  (* Finalize a PatternUnion by reversing the objects list *)
  val finalize : PatternUnion.t -> PatternUnion.t
end = struct
  open PatternUnion

  (* Builder helpers *)

  let not_seen_wildcard
      (cx : Context.t) ~(raise_errors : bool) (pattern_union : t) (reason : Reason.t) : bool =
    let { PatternUnion.wildcard; _ } = pattern_union in
    match wildcard with
    | Some already_seen ->
      if raise_errors then
        Flow_js.add_output
          cx
          (Error_message.EMatchUnusedPattern { reason; already_seen = Some already_seen });
      false
    | None -> true

  let add_leaf
      (cx : Context.t) ~(raise_errors : bool) ~(guarded : bool) (pattern_union : t) (leaf : Leaf.t)
      : t =
    let (reason, _) = leaf in
    if not_seen_wildcard cx ~raise_errors pattern_union reason then
      let { PatternUnion.leafs; guarded_leafs; _ } = pattern_union in
      match LeafSet.find_opt leaf leafs with
      | Some (already_seen, _) ->
        if raise_errors then
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
      (cx : Context.t)
      ~(raise_errors : bool)
      ~(guarded : bool)
      ~(last : bool)
      (pattern_union : t)
      (reason : Reason.t) : t =
    if not_seen_wildcard cx ~raise_errors pattern_union reason then
      if guarded then (
        ( if last then
          let loc = Reason.loc_of_reason reason in
          (* We avoid more complex analysis by simply erroring when there is a
             guarded wilcard which is in the last case. *)
          if raise_errors then Flow_js.add_output cx (Error_message.EMatchInvalidGuardedWildcard loc)
        );
        pattern_union
      ) else
        { pattern_union with PatternUnion.wildcard = Some reason }
    else
      pattern_union

  let add_tuple
      (cx : Context.t)
      ~(raise_errors : bool)
      (pattern_union : t)
      ~(length : int)
      (tuple : PatternObject.with_index) : t =
    let (_, (reason, { PatternObject.kind; rest; _ })) = tuple in
    if not_seen_wildcard cx ~raise_errors pattern_union reason then
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

  let add_object
      (cx : Context.t) ~(raise_errors : bool) (pattern_union : t) (obj : PatternObject.with_index) :
      t =
    let (_, (reason, _)) = obj in
    if not_seen_wildcard cx ~raise_errors pattern_union reason then
      let { objects; _ } = pattern_union in
      (* Accumulate backwards, reverse at end of pattern_union creation. *)
      { pattern_union with objects = obj :: objects }
    else
      pattern_union

  (* Construction from AST *)

  let leaf_of_type cx ~(raise_errors : bool) (t : Type.t) pattern_ast : Leaf.t option =
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
        if raise_errors then
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
      if raise_errors then
        Flow_js.add_output
          cx
          (Error_message.EMatchInvalidIdentOrMemberPattern
             { loc; type_reason = TypeUtil.reason_of_t t }
          );
      None

  let rec of_pattern_ast
      (cx : Context.t)
      ~(raise_errors : bool)
      (pattern_ast : (ALoc.t, ALoc.t * Type.t) Flow_ast.MatchPattern.t) : t =
    let (pattern_union, _) =
      of_pattern_ast' cx (empty, 0) ~raise_errors ~guarded:false ~last:false pattern_ast
    in
    pattern_union

  and of_pattern_ast'
      (cx : Context.t)
      ~(raise_errors : bool)
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
      let pattern_union = add_wildcard cx ~raise_errors ~guarded ~last pattern_union reason in
      (pattern_union, next_i)
    | WildcardPattern _ ->
      let reason = Reason.mk_reason Reason.RMatchWildcard loc in
      let pattern_union = add_wildcard cx ~raise_errors ~guarded ~last pattern_union reason in
      (pattern_union, next_i)
    | BooleanPattern { Flow_ast.BooleanLiteral.value; _ } ->
      let leaf = (reason, Leaf.BoolC value) in
      let pattern_union = add_leaf cx ~raise_errors ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | NumberPattern { Flow_ast.NumberLiteral.value; raw; _ } ->
      let leaf = (reason, Leaf.NumC (value, raw)) in
      let pattern_union = add_leaf cx ~raise_errors ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | BigIntPattern { Flow_ast.BigIntLiteral.value; raw; _ } ->
      let leaf = (reason, Leaf.BigIntC (value, raw)) in
      let pattern_union = add_leaf cx ~raise_errors ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | StringPattern { Flow_ast.StringLiteral.value; _ } ->
      let leaf = (reason, Leaf.StrC (Reason.OrdinaryName value)) in
      let pattern_union = add_leaf cx ~raise_errors ~guarded pattern_union leaf in
      (pattern_union, next_i)
    | NullPattern _ ->
      let leaf = (reason, Leaf.NullC) in
      let pattern_union = add_leaf cx ~raise_errors ~guarded pattern_union leaf in
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
        let pattern_union = add_leaf cx ~raise_errors ~guarded pattern_union leaf in
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
        let pattern_union = add_leaf cx ~raise_errors ~guarded pattern_union leaf in
        (pattern_union, next_i))
    | IdentifierPattern ((_, t), _)
    | MemberPattern ((_, t), _) ->
      let t = singleton_concrete_type cx t in
      (match leaf_of_type cx ~raise_errors t pattern_ast with
      | Some leaf -> (add_leaf cx ~raise_errors ~guarded pattern_union leaf, next_i)
      | None -> ({ pattern_union with contains_invalid_pattern = true }, i))
    | AsPattern { AsPattern.pattern; _ } ->
      of_pattern_ast' cx ~raise_errors (pattern_union, i) ~guarded ~last pattern
    | OrPattern { OrPattern.patterns; _ } ->
      Base.List.fold patterns ~init:(pattern_union, i) ~f:(fun acc pattern_ast ->
          of_pattern_ast' ~raise_errors ~guarded ~last cx acc pattern_ast
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
            let value = of_pattern_ast cx ~raise_errors pattern in
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
              class_info = None;
              keys_order;
              rest;
              kind = ObjKind.Tuple { length };
              contains_invalid_pattern;
              guarded;
            }
          )
        )
      in
      let pattern_union = add_tuple cx ~raise_errors pattern_union ~length tuple in
      (pattern_union, next_i)
    | ObjectPattern x ->
      object_pattern cx ~raise_errors ~reason ~next_i ~pattern_union ~guarded ~class_info:None x
    | InstancePattern _ when not @@ Context.enable_pattern_matching_instance_patterns cx ->
      (pattern_union, next_i)
    | InstancePattern { InstancePattern.constructor; properties = (_, properties); _ } ->
      let (constructor_t, constructor_name) =
        match constructor with
        | InstancePattern.IdentifierConstructor ((_, t), { Flow_ast.Identifier.name; _ })
        | InstancePattern.MemberConstructor
            ( (_, t),
              {
                MemberPattern.property =
                  MemberPattern.PropertyIdentifier (_, { Flow_ast.Identifier.name; _ });
                _;
              }
            ) ->
          (t, Some name)
        | InstancePattern.MemberConstructor ((_, t), _) -> (t, None)
      in
      (match get_class_info cx constructor_t with
      | Some (class_id, class_name) ->
        let class_name = Base.Option.first_some constructor_name class_name in
        let class_info = Some (class_id, class_name) in
        object_pattern
          cx
          ~raise_errors
          ~reason
          ~next_i
          ~pattern_union
          ~guarded
          ~class_info
          properties
      | _ -> (pattern_union, next_i))

  and object_pattern cx ~raise_errors ~reason ~next_i ~pattern_union ~guarded ~class_info pattern =
    let open Flow_ast.MatchPattern in
    let { ObjectPattern.properties; rest; _ } = pattern in
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
            let value = of_pattern_ast cx ~raise_errors pattern in
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
        Base.Option.map rest ~f:(fun (loc, _) -> Reason.mk_reason Reason.RObjectPatternRestProp loc)
      in
      let obj =
        ( next_i,
          ( reason,
            {
              PatternObject.props;
              class_info;
              keys_order;
              rest;
              kind = ObjKind.Obj;
              contains_invalid_pattern;
              guarded;
            }
          )
        )
      in
      let pattern_union = add_object cx ~raise_errors pattern_union obj in
      match (class_info, tuple_like) with
      | (None, Some i) -> add_tuple cx ~raise_errors ~length:(Int.of_float i) pattern_union obj
      | _ -> pattern_union
    in
    (pattern_union, next_i)

  (* Finalize by reversing the objects list *)
  let finalize pattern_union =
    let { objects; _ } = pattern_union in
    { pattern_union with objects = Base.List.rev objects }

  (* Add a single pattern to an existing (unfinalized) PatternUnion *)
  let add_pattern cx ~raise_errors acc (pattern_ast, guarded) ~last =
    of_pattern_ast' cx acc ~raise_errors ~guarded ~last pattern_ast

  let of_patterns_ast cx ~raise_errors patterns_ast =
    let last_i = Base.List.length patterns_ast - 1 in
    let (pattern_union, _) =
      Base.List.foldi patterns_ast ~init:(empty, 0) ~f:(fun i acc pattern ->
          add_pattern cx ~raise_errors acc pattern ~last:(i = last_i)
      )
    in
    finalize pattern_union
end

module rec ValueObjectPropertyBuilder : sig
  val of_type_prop : Context.t -> string -> Type.Property.t -> ValueObject.Property.t option
end = struct
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
      Some { ValueObject.Property.loc; value = lazy (ValueUnionBuilder.of_type cx t); optional }
    | None -> None
end

(* A representation of a union of values. *)
and ValueUnionBuilder : sig
  val of_type : Context.t -> Type.t -> ValueUnion.t

  val get_prop : Context.t -> ALoc.t * string -> Type.t -> ValueObject.Property.t option
end = struct
  open ValueUnion

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
        let { leafs; tuples; arrays; objects; enum_unknown_members; inexhaustible } = value_union in
        match t with
        | Type.DefT (enum_reason, Type.EnumValueT (Type.ConcreteEnum enum_info)) ->
          let { Type.members; has_unknown_members; _ } = enum_info in
          let enum_leafs =
            SMap.fold
              (fun member_name loc leafs ->
                let reason =
                  Reason.mk_reason
                    (Reason.REnumMember { enum = Reason.desc_of_reason enum_reason; member_name })
                    loc
                in
                LeafSet.add (reason, Leaf.EnumMemberC { Leaf.member_name; enum_info }) leafs)
              members
              LeafSet.empty
          in
          let enum_unknown_members =
            if has_unknown_members then
              let reason =
                Reason.replace_desc_reason
                  (Reason.REnumUnknownMembers (Reason.desc_of_reason enum_reason))
                  enum_reason
              in
              (reason, enum_leafs) :: enum_unknown_members
            else
              enum_unknown_members
          in
          { value_union with leafs = LeafSet.union leafs enum_leafs; enum_unknown_members }
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
                         class_info = None;
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
                class_info = None;
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
                  let prop = ValueObjectPropertyBuilder.of_type_prop cx key p in
                  let props = SMap.add key prop props in
                  let sentinel_props =
                    if NameUtils.Set.mem name all_sentinel_props then
                      SSet.add key sentinel_props
                    else
                      sentinel_props
                  in
                  (props, sentinel_props))
              (Context.find_props cx props_tmap)
              (SMap.empty, SSet.empty)
          in
          let obj =
            ( reason,
              { ValueObject.kind = ObjKind.Obj; t; props; class_info = None; rest; sentinel_props }
            )
          in
          { value_union with objects = obj :: objects }
        | Type.DefT
            ( reason,
              Type.InstanceT { Type.inst = { Type.class_id; class_name; inst_kind; _ }; super; _ }
            ) ->
          let rest =
            match inst_kind with
            | Type.RecordKind _ -> None
            | Type.ClassKind
            | Type.InterfaceKind _ ->
              Some reason
          in
          let class_info =
            if Context.enable_pattern_matching_instance_patterns cx then
              let rec get_super_ids acc t =
                match singleton_concrete_type cx t with
                | Type.DefT
                    ( _,
                      Type.InstanceT
                        {
                          Type.inst =
                            {
                              Type.inst_kind = Type.ClassKind | Type.RecordKind _;
                              class_id = super_class_id;
                              _;
                            };
                          super;
                          _;
                        }
                    ) ->
                  if ALoc.equal_id super_class_id class_id || ALocIDSet.mem super_class_id acc then
                    (* Recursive extends: stop looping *)
                    acc
                  else
                    let acc = ALocIDSet.add super_class_id acc in
                    get_super_ids acc super
                | _ -> acc
              in
              Some (class_id, class_name, get_super_ids ALocIDSet.empty super)
            else
              None
          in
          let obj =
            ( reason,
              {
                ValueObject.kind = ObjKind.Obj;
                t;
                props = SMap.empty;
                class_info;
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
                   ValueObjectPropertyBuilder.of_type_prop cx key_name p
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
    let { leafs; tuples; arrays; objects; enum_unknown_members; inexhaustible } =
      of_type' cx empty t
    in
    (* The list members of the `ValueUnion` are accumulated in reverse order,
       put them back in their original order. *)
    {
      leafs;
      tuples = Base.List.rev tuples;
      arrays = Base.List.rev arrays;
      objects = Base.List.rev objects;
      enum_unknown_members = Base.List.rev enum_unknown_members;
      inexhaustible = Base.List.rev inexhaustible;
    }
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
let rec filter_values_by_patterns
    cx ~(raise_errors : bool) ~(value_union : ValueUnion.t) ~(pattern_union : PatternUnion.t) :
    filter_union_result =
  let {
    ValueUnion.leafs = value_leafs;
    tuples = value_tuples;
    arrays;
    objects = value_objects;
    enum_unknown_members;
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
            filter_objects_by_patterns cx ~raise_errors [tuple_value] pattern_tuples
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
      filter_objects_by_patterns
        cx
        ~raise_errors
        arrays
        (PatternUnion.all_tuples_and_objects pattern_union)
  in
  let used_pattern_locs = ALocSet.union used_pattern_locs array_used_pattern_locs in
  (* Objects *)
  let (objects_left, objects_matched, obj_used_pattern_locs) =
    filter_objects_by_patterns cx ~raise_errors value_objects pattern_objects
  in
  let used_pattern_locs = ALocSet.union used_pattern_locs obj_used_pattern_locs in
  let value_left =
    {
      ValueUnion.leafs = leafs_left;
      tuples = tuples_left;
      arrays = arrays_left;
      objects = objects_left;
      enum_unknown_members;
      inexhaustible;
    }
  in
  (* Mixed/any *)
  let mixed_used_pattern_locs =
    match is_object_subtype_of_inexhaustible inexhaustible with
    | None -> ALocSet.empty
    | Some reason -> visit_mixed cx ~raise_errors reason pattern_union
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
        enum_unknown_members = [];
        inexhaustible = [];
      }
    in
    { value_left; value_matched; used_pattern_locs }

(* Given a list of object values, and a list of object patterns, match the
   values against the patterns, and compute which objects were matched, and
   which were not matched. *)
and filter_objects_by_patterns
    cx
    ~(raise_errors : bool)
    (value_objects : ValueObject.t list)
    (pattern_objects : PatternObject.with_index list) :
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
              ( filter_object_by_pattern cx ~raise_errors value_object pattern_object,
                additional_used_pattern_locs
              ))
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
and filter_object_by_pattern
    cx ~raise_errors (value_object : ValueObject.t) (pattern_object : PatternObject.t) :
    filter_object_result =
  let open Base.Continue_or_stop in
  let ( reason_value,
        {
          ValueObject.props = value_props;
          class_info = value_class_info;
          rest = value_rest;
          t;
          kind = value_kind;
          sentinel_props;
        }
      ) =
    value_object
  in
  let ( reason_pattern,
        {
          PatternObject.props = pattern_props;
          class_info = pattern_class_info;
          keys_order;
          rest = pattern_rest;
          kind = pattern_kind;
          guarded;
          contains_invalid_pattern = _;
        }
      ) =
    pattern_object
  in
  let possibly_matches =
    match (value_class_info, pattern_class_info) with
    (* Structural pattern: can match *)
    | (_, None) -> true
    (* Structural value, nominal pattern: cannot match *)
    | (None, Some _) -> false
    (* Nominal value, nominal pattern: potentially matches *)
    | (Some (value_class_id, _, value_super_ids), Some (pattern_class_id, _)) ->
      ALoc.equal_id value_class_id pattern_class_id
      || ALocIDSet.mem pattern_class_id value_super_ids
  in
  if not possibly_matches then
    NoMatch { used_pattern_locs = ALocSet.empty; left = value_object }
  else
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
            let prop = ValueUnionBuilder.get_prop cx (key_loc, key) t in
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
                filter_values_by_patterns
                  cx
                  ~raise_errors
                  ~value_union:(Lazy.force value)
                  ~pattern_union:pattern
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
                {
                  ValueObject.props = props_left;
                  class_info = value_class_info;
                  rest;
                  t;
                  kind = value_kind;
                  sentinel_props;
                }
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
                  if raise_errors then
                    Flow_js.add_output
                      cx
                      (Error_message.EMatchNonExhaustiveObjectPattern
                         {
                           loc = Reason.loc_of_reason reason_pattern;
                           rest = value_rest;
                           missing_props;
                           pattern_kind =
                             (match pattern_class_info with
                             | Some _ -> Flow_intermediate_error_types.MatchObjPatternKind.Instance
                             | None -> Flow_intermediate_error_types.MatchObjPatternKind.Object);
                         }
                      )
                );
                used_pattern_locs
              | Some pattern_rest ->
                ALocSet.add (Reason.loc_of_reason pattern_rest) used_pattern_locs
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
                {
                  ValueObject.props = head;
                  class_info = value_class_info;
                  rest = value_rest;
                  t;
                  kind = value_kind;
                  sentinel_props;
                }
              )
            in
            Match { used_pattern_locs; queue_additions; matched })

(* mixed/any values mark object and tuple patterns as used. *)
and visit_mixed cx ~(raise_errors : bool) (reason : Reason.t) (pattern_union : PatternUnion.t) :
    ALocSet.t =
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
            class_info = None;
            props = SMap.empty;
            rest = Some reason;
            sentinel_props = SSet.empty;
          }
        )
      in
      let pattern_objects = PatternUnion.all_tuples_and_objects pattern_union in
      let (_, _, used_pattern_locs) =
        filter_objects_by_patterns cx ~raise_errors [mixed_array] pattern_objects
      in
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
            class_info = None;
            rest = Some reason;
            sentinel_props = SSet.empty;
          }
        )
      in
      let (_, _, used_pattern_locs) =
        filter_objects_by_patterns cx ~raise_errors [mixed_object] pattern_objects
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
  let pattern_union = PatternUnionBuilder.of_patterns_ast cx ~raise_errors:true patterns in
  let value_union = ValueUnionBuilder.of_type cx arg_t in
  let { value_left; used_pattern_locs; value_matched } =
    filter_values_by_patterns cx ~raise_errors:true ~value_union ~pattern_union
  in
  ( if ValueUnion.is_empty value_left then
    let { ValueUnion.enum_unknown_members; _ } = value_matched in
    let { PatternUnion.leafs = pattern_leafs; wildcard; _ } = pattern_union in
    Base.Option.iter wildcard ~f:(fun wildcard_reason ->
        Base.List.iter enum_unknown_members ~f:(fun (_, enum_leafs) ->
            if not @@ LeafSet.subset enum_leafs pattern_leafs then
              let unchecked_members =
                LeafSet.diff enum_leafs pattern_leafs
                |> LeafSet.elements
                |> Base.List.map ~f:(fun (_, leaf) -> Leaf.to_string leaf)
              in
              Flow_js.add_output
                cx
                (Error_message.EMatchNonExplicitEnumCheck
                   { loc = match_loc; wildcard_reason; unchecked_members }
                )
        )
    )
  else
    let { ValueUnion.leafs; tuples; arrays; objects; enum_unknown_members; inexhaustible } =
      value_left
    in
    let (examples_rev, asts_rev) =
      LeafSet.elements leafs
      |> Base.List.fold ~init:([], []) ~f:(fun (examples_rev, asts_rev) (reason, leaf) ->
             let example = Leaf.to_string leaf in
             ((example, [reason]) :: examples_rev, lazy (Leaf.to_ast leaf) :: asts_rev)
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
                 | None -> (i, lazy (PatternObject.to_ast tuple_pattern), ReasonSet.singleton reason)
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
            | None -> (i, lazy (PatternObject.to_ast pattern), reasons)
            | Some (i, ast, existing_reasons) -> (i, ast, ReasonSet.union existing_reasons reasons))
          tuple_examples_map
    in
    (* Turn the map into a list of examples *)
    let (examples_rev, asts_rev) =
      tuple_examples_map
      |> SMap.elements
      |> Base.List.sort ~compare:compare_examples
      |> Base.List.fold
           ~init:(examples_rev, asts_rev)
           ~f:(fun (examples_rev, asts_rev) (example, (_, ast, reasons)) ->
             ((example, ReasonSet.elements reasons) :: examples_rev, ast :: asts_rev)
         )
    in
    (* Compute the list of object examples *)
    let (examples_rev, asts_rev) =
      objects
      |> Base.List.map ~f:ValueObject.to_pattern
      |> Base.List.stable_sort ~compare:PatternObject.compare
      |> Base.List.foldi ~init:SMap.empty ~f:(fun i acc object_pattern ->
             let (reason, _) = object_pattern in
             let example = PatternObject.to_string object_pattern in
             SMap.adjust
               example
               (function
                 | None ->
                   (i, lazy (PatternObject.to_ast object_pattern), ReasonSet.singleton reason)
                 | Some (i, ast, reasons) -> (i, ast, ReasonSet.add reason reasons))
               acc
         )
      |> SMap.elements
      |> Base.List.sort ~compare:compare_examples
      |> Base.List.fold
           ~init:(examples_rev, asts_rev)
           ~f:(fun (examples_rev, asts_rev) (example, (_, ast, reasons)) ->
             ((example, ReasonSet.elements reasons) :: examples_rev, ast :: asts_rev)
         )
    in
    let wildcard_example reason =
      let pattern = wildcard_pattern reason in
      ( ( PatternUnion.to_string pattern,
          Base.List.fold inexhaustible ~init:ReasonSet.empty ~f:(fun acc t ->
              ReasonSet.add (TypeUtil.reason_of_t t) acc
          )
          |> (fun init ->
               Base.List.fold enum_unknown_members ~init ~f:(fun acc (reason, _) ->
                   ReasonSet.add reason acc
               ))
          |> ReasonSet.elements
        )
        :: examples_rev,
        lazy (PatternUnion.to_ast pattern) :: asts_rev
      )
    in
    let (examples_rev, asts_rev) =
      match (inexhaustible, enum_unknown_members) with
      | ([], []) -> (examples_rev, asts_rev)
      | (first_t :: _, _) -> wildcard_example (TypeUtil.reason_of_t first_t)
      | (_, (reason, _) :: _) -> wildcard_example reason
    in
    let examples = Base.List.rev examples_rev in
    let asts =
      Base.List.rev asts_rev |> (fun asts -> Base.List.take asts 25) |> Base.List.map ~f:Lazy.force
    in
    Flow_js.add_output
      cx
      (Error_message.EMatchNotExhaustive { loc = match_loc; examples; missing_pattern_asts = asts })
  );
  check_for_unused_patterns cx pattern_union used_pattern_locs

(* Filter a type by a finalized PatternUnion *)
let filter_by_pattern_union cx root_t pattern_union =
  let value_union = ValueUnionBuilder.of_type cx root_t in
  let { value_left; used_pattern_locs = _; value_matched = _ } =
    filter_values_by_patterns cx ~raise_errors:false ~value_union ~pattern_union
  in
  value_left
