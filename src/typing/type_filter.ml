(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil

let recurse_into_union cx filter_fn ((r, ts) : reason * Type.t list) =
  let new_ts =
    List.fold_left
      (fun new_ts t ->
        let t =
          match t with
          | OpenT (_, id) ->
            let (_, constraints) = Context.find_constraints cx id in
            begin
              match constraints with
              | Constraint.FullyResolved (lazy t)
              | Constraint.Resolved t ->
                t
              | _ -> t
            end
          | _ -> t
        in
        match filter_fn cx t with
        | DefT (_, _, EmptyT) -> new_ts
        | filtered_type -> filtered_type :: new_ts)
      []
      ts
  in
  let new_ts = List.rev new_ts in
  match new_ts with
  | [] -> DefT (r, bogus_trust (), EmptyT)
  | [t] -> t
  | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)

let recurse_into_intersection =
  let rec helper filter_fn r acc = function
    | [] -> List.rev acc
    | t :: ts -> begin
      match filter_fn t with
      | DefT (_, _, EmptyT) -> []
      | filtered_type -> helper filter_fn r (filtered_type :: acc) ts
    end
  in
  fun filter_fn ((r, ts) : reason * Type.t list) ->
    match helper filter_fn r [] ts with
    | [] -> DefT (r, bogus_trust (), EmptyT)
    | [t] -> t
    | t0 :: t1 :: ts -> IntersectionT (r, InterRep.make t0 t1 ts)

let map_poly ~f t =
  match t with
  | DefT (r, tr, PolyT ({ t_out; _ } as poly)) -> begin
    match f t_out with
    | DefT (_, _, EmptyT) as empty -> empty
    | t_out -> DefT (r, tr, PolyT { poly with t_out })
  end
  | _ -> t

let rec exists cx = function
  (* falsy things get removed *)
  | DefT
      ( r,
        trust,
        ( NullT | VoidT
        | SingletonBoolT false
        | BoolT (Some false)
        | EnumT { representation_t = DefT (_, _, BoolT (Some false)); _ }
        | SingletonStrT (OrdinaryName "")
        | StrT (Literal (_, OrdinaryName ""))
        | SingletonNumT (0., _)
        | NumT (Literal (_, (0., _))) )
      ) ->
    DefT (r, trust, EmptyT)
  (* unknown things become truthy *)
  | UnionT (r, rep) -> recurse_into_union cx exists (r, UnionRep.members rep)
  | MaybeT (_, t) -> t
  | OptionalT { reason = _; type_ = t; use_desc = _ } -> exists cx t
  | DefT (r, trust, BoolT None) -> DefT (r, trust, BoolT (Some true))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (r, trust, StrT Truthy)
  | DefT (r, trust, NumT AnyLiteral) -> DefT (r, trust, NumT Truthy)
  | DefT (r, trust, MixedT _) -> DefT (r, trust, MixedT Mixed_truthy)
  (* an intersection passes through iff all of its members pass through *)
  | IntersectionT (r, rep) -> recurse_into_intersection (exists cx) (r, InterRep.members rep)
  (* truthy things pass through *)
  | t -> t

let rec not_exists cx t =
  match t with
  | DefT (_, _, PolyT _) -> map_poly ~f:(not_exists cx) t
  (* falsy things pass through *)
  | DefT
      ( _,
        _,
        ( NullT | VoidT
        | SingletonBoolT false
        | BoolT (Some false)
        | EnumT { representation_t = DefT (_, _, BoolT (Some false)); _ }
        | SingletonStrT (OrdinaryName "")
        | StrT (Literal (_, OrdinaryName ""))
        | SingletonNumT (0., _)
        | NumT (Literal (_, (0., _))) )
      ) ->
    t
  | AnyT (r, _) -> DefT (r, Trust.bogus_trust (), EmptyT)
  | UnionT (r, rep) -> recurse_into_union cx not_exists (r, UnionRep.members rep)
  (* truthy things get removed *)
  | DefT
      ( r,
        trust,
        ( SingletonBoolT _
        | BoolT (Some _)
        | EnumT { representation_t = DefT (_, _, BoolT (Some _)); _ }
        | SingletonStrT _
        | StrT (Literal _ | Truthy)
        | EnumT { representation_t = DefT (_, _, StrT Truthy); _ }
        | ArrT _ | ObjT _ | InstanceT _ | EnumObjectT _ | FunT _ | SingletonNumT _
        | NumT (Literal _ | Truthy)
        | EnumT { representation_t = DefT (_, _, NumT Truthy); _ }
        | EnumT { representation_t = DefT (_, _, BigIntT Truthy); _ }
        | MixedT Mixed_truthy )
      ) ->
    DefT (r, trust, EmptyT)
  | DefT (reason, trust, ClassT _) -> DefT (reason, trust, EmptyT)
  | ThisClassT (reason, _, _, _) -> DefT (reason, Trust.bogus_trust (), EmptyT)
  (* unknown boolies become falsy *)
  | MaybeT (r, t) ->
    UnionT
      ( r,
        UnionRep.make
          (Trust.bogus_trust () |> NullT.why r)
          (Trust.bogus_trust () |> VoidT.why r)
          [not_exists cx t]
      )
  | DefT (r, trust, BoolT None) -> DefT (r, trust, BoolT (Some false))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (r, trust, StrT (Literal (None, OrdinaryName "")))
  | DefT (r, trust, NumT AnyLiteral) -> DefT (r, trust, NumT (Literal (None, (0., "0"))))
  | ExactT (_, t) -> not_exists cx t
  (* an intersection passes through iff all of its members pass through *)
  | IntersectionT (r, rep) -> recurse_into_intersection (not_exists cx) (r, InterRep.members rep)
  (* things that don't track truthiness pass through *)
  | t -> t

let rec maybe cx = function
  | UnionT (r, rep) -> recurse_into_union cx maybe (r, UnionRep.members rep)
  | MaybeT (r, _) ->
    UnionT
      ( r,
        UnionRep.make (Trust.bogus_trust () |> NullT.why r) (Trust.bogus_trust () |> VoidT.why r) []
      )
  | DefT (r, trust, MixedT Mixed_everything) ->
    UnionT (r, UnionRep.make (NullT.why r trust) (VoidT.why r trust) [])
  | DefT (r, trust, MixedT Mixed_truthy) -> EmptyT.why r trust
  | DefT (r, trust, MixedT Mixed_non_maybe) -> EmptyT.why r trust
  | DefT (r, trust, MixedT Mixed_non_void) -> DefT (r, trust, NullT)
  | DefT (r, trust, MixedT Mixed_non_null) -> DefT (r, trust, VoidT)
  | DefT (_, _, NullT) as t -> t
  | DefT (_, _, VoidT) as t -> t
  | OptionalT { reason = r; type_ = _; use_desc } ->
    Trust.bogus_trust () |> VoidT.why_with_use_desc ~use_desc r
  | AnyT _ as t -> t
  | DefT (r, trust, _) -> EmptyT.why r trust
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason |> with_trust bogus_trust

let rec not_maybe cx = function
  | UnionT (r, rep) -> recurse_into_union cx not_maybe (r, UnionRep.members rep)
  | MaybeT (_, t) -> t
  | OptionalT { reason = _; type_ = t; use_desc = _ } -> not_maybe cx t
  | DefT (r, trust, (NullT | VoidT)) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, MixedT Mixed_truthy) -> DefT (r, trust, MixedT Mixed_truthy)
  | DefT (r, trust, MixedT Mixed_non_maybe) -> DefT (r, trust, MixedT Mixed_non_maybe)
  | DefT (r, trust, MixedT Mixed_everything)
  | DefT (r, trust, MixedT Mixed_non_void)
  | DefT (r, trust, MixedT Mixed_non_null) ->
    DefT (r, trust, MixedT Mixed_non_maybe)
  | t -> t

let null = function
  | OptionalT { reason = _; type_ = MaybeT (r, _); use_desc = _ }
  | MaybeT (r, _) ->
    Trust.bogus_trust () |> NullT.why r
  | DefT (_, _, NullT) as t -> t
  | DefT (r, trust, MixedT Mixed_everything)
  | DefT (r, trust, MixedT Mixed_non_void) ->
    NullT.why r trust
  | AnyT _ as t -> t
  | DefT (r, trust, _) -> EmptyT.why r trust
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason |> with_trust bogus_trust

let rec not_null cx = function
  | MaybeT (r, t) -> UnionT (r, UnionRep.make (Trust.bogus_trust () |> VoidT.why r) t [])
  | OptionalT { reason; type_ = t; use_desc } ->
    OptionalT { reason; type_ = not_null cx t; use_desc }
  | UnionT (r, rep) -> recurse_into_union cx not_null (r, UnionRep.members rep)
  | DefT (r, trust, NullT) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, MixedT Mixed_everything) -> DefT (r, trust, MixedT Mixed_non_null)
  | DefT (r, trust, MixedT Mixed_non_void) -> DefT (r, trust, MixedT Mixed_non_maybe)
  | t -> t

let undefined = function
  | MaybeT (r, _) -> VoidT.why r |> with_trust bogus_trust
  | DefT (_, _, VoidT) as t -> t
  | OptionalT { reason = r; type_ = _; use_desc } ->
    VoidT.why_with_use_desc ~use_desc r |> with_trust bogus_trust
  | DefT (r, trust, MixedT Mixed_everything)
  | DefT (r, trust, MixedT Mixed_non_null) ->
    VoidT.why r trust
  | AnyT _ as t -> t
  | DefT (r, trust, _) -> EmptyT.why r trust
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason |> with_trust bogus_trust

let rec not_undefined cx = function
  | MaybeT (r, t) -> UnionT (r, UnionRep.make (NullT.why r |> with_trust bogus_trust) t [])
  | OptionalT { reason = _; type_ = t; use_desc = _ } -> not_undefined cx t
  | UnionT (r, rep) -> recurse_into_union cx not_undefined (r, UnionRep.members rep)
  | DefT (r, trust, VoidT) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, MixedT Mixed_everything) -> DefT (r, trust, MixedT Mixed_non_void)
  | DefT (r, trust, MixedT Mixed_non_null) -> DefT (r, trust, MixedT Mixed_non_maybe)
  | t -> t

let string_literal expected_loc sense expected t =
  let expected_desc = RStringLit expected in
  let lit_reason = replace_desc_new_reason expected_desc in
  match t with
  | DefT (_, trust, StrT (Literal (_, actual))) ->
    if actual = expected then
      t
    else
      DefT (mk_reason expected_desc expected_loc, trust, StrT (Literal (Some sense, expected)))
  | DefT (r, trust, StrT Truthy) when expected <> OrdinaryName "" ->
    DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | AnyT _ as t -> t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_string_literal expected = function
  | DefT (r, trust, StrT (Literal (_, actual))) when actual = expected -> DefT (r, trust, EmptyT)
  | t -> t

let number_literal expected_loc sense expected t =
  let (_, expected_raw) = expected in
  let expected_desc = RNumberLit expected_raw in
  let lit_reason = replace_desc_new_reason expected_desc in
  match t with
  | DefT (_, trust, NumT (Literal (_, (_, actual_raw)))) ->
    if actual_raw = expected_raw then
      t
    else
      DefT (mk_reason expected_desc expected_loc, trust, NumT (Literal (Some sense, expected)))
  | DefT (r, trust, NumT Truthy) when snd expected <> "0" ->
    DefT (lit_reason r, trust, NumT (Literal (None, expected)))
  | DefT (r, trust, NumT AnyLiteral) -> DefT (lit_reason r, trust, NumT (Literal (None, expected)))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, NumT (Literal (None, expected)))
  | AnyT _ as t -> t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_number_literal expected = function
  | DefT (r, trust, NumT (Literal (_, actual))) when snd actual = snd expected ->
    DefT (r, trust, EmptyT)
  | t -> t

let bigint_literal expected_loc sense expected t =
  let (_, expected_raw) = expected in
  let expected_desc = RBigIntLit expected_raw in
  let lit_reason = replace_desc_new_reason expected_desc in
  match t with
  | DefT (_, trust, BigIntT (Literal (_, (_, actual_raw)))) ->
    if actual_raw = expected_raw then
      t
    else
      DefT (mk_reason expected_desc expected_loc, trust, BigIntT (Literal (Some sense, expected)))
  | DefT (r, trust, BigIntT Truthy) when snd expected <> "0n" ->
    DefT (lit_reason r, trust, BigIntT (Literal (None, expected)))
  | DefT (r, trust, BigIntT AnyLiteral) ->
    DefT (lit_reason r, trust, BigIntT (Literal (None, expected)))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, BigIntT (Literal (None, expected)))
  | AnyT _ as t -> t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_bigint_literal expected = function
  | DefT (r, trust, BigIntT (Literal (_, actual))) when snd actual = snd expected ->
    DefT (r, trust, EmptyT)
  | t -> t

let true_ t =
  let lit_reason = replace_desc_new_reason (RBooleanLit true) in
  match t with
  | DefT (r, trust, BoolT (Some true)) -> DefT (lit_reason r, trust, BoolT (Some true))
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some true))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, BoolT (Some true))
  | AnyT _ as t -> t
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_true t =
  let lit_reason = replace_desc_new_reason (RBooleanLit false) in
  match t with
  | DefT (r, trust, BoolT (Some true)) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some false))
  | t -> t

let false_ t =
  let lit_reason = replace_desc_new_reason (RBooleanLit false) in
  match t with
  | DefT (r, trust, BoolT (Some false)) -> DefT (lit_reason r, trust, BoolT (Some false))
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some false))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, BoolT (Some false))
  | AnyT _ as t -> t
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_false t =
  let lit_reason = replace_desc_new_reason (RBooleanLit true) in
  match t with
  | DefT (r, trust, BoolT (Some false)) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some true))
  | t -> t

let boolean loc t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason BoolT.desc r, trust, BoolT (Some true))
  | AnyT _
  | DefT (_, _, MixedT _) ->
    DefT (mk_reason RBoolean loc, bogus_trust (), BoolT None)
  | DefT (_, _, BoolT _)
  | DefT (_, _, EnumT { representation_t = DefT (_, _, BoolT _); _ }) ->
    t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_boolean t =
  match t with
  | DefT (_, trust, EnumT { representation_t = DefT (_, _, BoolT _); _ })
  | DefT (_, trust, BoolT _) ->
    DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let string loc t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason StrT.desc r, trust, StrT Truthy)
  | AnyT _
  | DefT (_, _, MixedT _) ->
    DefT (mk_reason RString loc, bogus_trust (), StrT AnyLiteral)
  | DefT (_, _, StrT _)
  | DefT (_, _, EnumT { representation_t = DefT (_, _, StrT _); _ }) ->
    t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_string t =
  match t with
  | DefT (_, trust, EnumT { representation_t = DefT (_, _, StrT _); _ })
  | DefT (_, trust, StrT _) ->
    DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let symbol loc t =
  match t with
  | DefT (_, _, SymbolT) -> t
  | DefT (_, _, MixedT _)
  | AnyT _ ->
    SymbolT.why (mk_reason RSymbol loc) (bogus_trust ())
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_symbol t =
  match t with
  | DefT (_, _, SymbolT) -> DefT (reason_of_t t, bogus_trust (), EmptyT)
  | _ -> t

let number loc t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason NumT.desc r, trust, NumT Truthy)
  | AnyT _
  | DefT (_, _, MixedT _) ->
    DefT (mk_reason RNumber loc, bogus_trust (), NumT AnyLiteral)
  | DefT (_, _, NumT _)
  | DefT (_, _, EnumT { representation_t = DefT (_, _, NumT _); _ }) ->
    t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_number t =
  match t with
  | DefT (_, trust, EnumT { representation_t = DefT (_, _, NumT _); _ })
  | DefT (_, trust, NumT _) ->
    DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let bigint loc t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason BigIntT.desc r, trust, BigIntT Truthy)
  | AnyT _
  | DefT (_, _, MixedT _) ->
    DefT (mk_reason RBigInt loc, bogus_trust (), BigIntT AnyLiteral)
  | DefT (_, _, BigIntT _)
  | DefT (_, _, EnumT { representation_t = DefT (_, _, BigIntT _); _ }) ->
    t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_bigint t =
  match t with
  | DefT (_, trust, EnumT { representation_t = DefT (_, _, BigIntT _); _ })
  | DefT (_, trust, BigIntT _) ->
    DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let rec object_ cx t =
  match t with
  | DefT (_, _, PolyT _) -> map_poly ~f:(object_ cx) t
  | DefT (r, trust, MixedT flavor) ->
    let reason = replace_desc_new_reason RObject r in
    let dict =
      {
        key = StrT.why r |> with_trust bogus_trust;
        value = DefT (replace_desc_new_reason MixedT.desc r, bogus_trust (), MixedT Mixed_everything);
        dict_name = None;
        dict_polarity = Polarity.Positive;
      }
    in
    let proto = ObjProtoT reason in
    let obj = Obj_type.mk_with_proto cx reason ~obj_kind:(Indexed dict) proto in
    begin
      match flavor with
      | Mixed_truthy
      | Mixed_non_maybe
      | Mixed_non_null ->
        obj
      | Mixed_function
      | Mixed_everything
      | Mixed_non_void ->
        let reason = replace_desc_new_reason RUnion (reason_of_t t) in
        UnionT (reason, UnionRep.make (NullT.why r trust) obj [])
    end
  | DefT (_, _, (ObjT _ | ArrT _ | NullT | InstanceT _ | EnumObjectT _))
  | AnyT _ ->
    t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let rec not_object t =
  match t with
  | DefT (_, _, PolyT _) -> map_poly ~f:not_object t
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT)
  | DefT (_, trust, (ObjT _ | ArrT _ | NullT | InstanceT _ | EnumObjectT _)) ->
    DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let rec function_ = function
  | DefT (_, _, PolyT _) as t -> map_poly ~f:function_ t
  | DefT (r, trust, MixedT _) ->
    DefT (replace_desc_new_reason (RFunction RUnknown) r, trust, MixedT Mixed_function)
  | (ThisClassT _ | DefT (_, _, (FunT _ | ClassT _)) | AnyT _) as t -> t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT)
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let rec not_function t =
  match t with
  | DefT (_, _, PolyT _) -> map_poly ~f:not_function t
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT)
  | ThisClassT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT)
  | DefT (_, trust, (FunT _ | ClassT _)) -> DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let array t =
  match t with
  | DefT (r, trust, MixedT _) ->
    let r =
      update_desc_reason
        (function
          (* If we always wrapped the reason desc in `RRefinedElement`, we may diverge
             when typechecking loops instead of being caught by the constraint cache. Instead
             we only wrap one element deep. *)
          | RRefinedElement _ as desc -> desc
          | d -> RRefinedElement d)
        r
    in
    DefT
      ( replace_desc_new_reason RROArrayType r,
        trust,
        ArrT (ROArrayAT (DefT (r, trust, MixedT Mixed_everything)))
      )
  | DefT (_, _, ArrT _)
  | AnyT _ ->
    t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_array t =
  match t with
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT)
  | DefT (_, trust, ArrT _) -> DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let sentinel_refinement =
  let open UnionEnum in
  let enum_match sense = function
    | (DefT (_, _, StrT (Literal (_, value))), Str sentinel) when value = sentinel != sense -> true
    | (DefT (_, _, NumT (Literal (_, (value, _)))), Num sentinel) when value = sentinel != sense ->
      true
    | (DefT (_, _, BoolT (Some value)), Bool sentinel) when value = sentinel != sense -> true
    | (DefT (_, _, BigIntT (Literal (_, (value, _)))), BigInt (sentinel, _))
      when value = sentinel != sense ->
      true
    | (DefT (_, _, NullT), Null)
    | (DefT (_, _, VoidT), Void) ->
      true
    | _ -> false
  in
  fun v reason l sense enum ->
    let rec loop enum =
      match (v, enum) with
      | (_, One e) when enum_match sense (v, e) && not sense -> []
      | (DefT (_, _, StrT _), One (Str sentinel)) when enum_match sense (v, Str sentinel) -> []
      | (DefT (_, _, NumT _), One (Num sentinel)) when enum_match sense (v, Num sentinel) -> []
      | (DefT (_, _, BoolT _), One (Bool sentinel)) when enum_match sense (v, Bool sentinel) -> []
      | (DefT (_, _, BigIntT _), One (BigInt sentinel)) when enum_match sense (v, BigInt sentinel)
        ->
        []
      | (DefT (_, _, (StrT _ | NumT _ | BoolT _ | BigIntT _ | NullT | VoidT)), Many enums)
        when sense ->
        UnionEnumSet.elements enums
        |> Base.List.concat_map ~f:(fun enum ->
               if enum_match sense (v, enum) |> not then
                 loop (One enum)
               else
                 []
           )
      | (DefT (_, _, StrT _), One (Str _))
      | (DefT (_, _, NumT _), One (Num _))
      | (DefT (_, _, BoolT _), One (Bool _))
      | (DefT (_, _, BigIntT _), One (BigInt _))
      | (DefT (_, _, NullT), One Null)
      | (DefT (_, _, VoidT), One Void)
      | (DefT (_, _, (StrT _ | NumT _ | BoolT _ | BigIntT _ | NullT | VoidT)), Many _) ->
        [l]
      (* types don't match (would've been matched above) *)
      (* we don't prune other types like objects or instances, even though
         a test like `if (ObjT === StrT)` seems obviously unreachable, but
         we have to be wary of toString and valueOf on objects/instances. *)
      | (DefT (_, _, (StrT _ | NumT _ | BoolT _ | BigIntT _ | NullT | VoidT)), _) when sense -> []
      | (DefT (_, _, (StrT _ | NumT _ | BoolT _ | BigIntT _ | NullT | VoidT)), _)
      | _ ->
        (* property exists, but is not something we can use for refinement *)
        [l]
    in
    TypeUtil.union_of_ts reason (loop enum)

(* Type guard filtering *)

(* These tags represent sets of values that have no overlap.
 * - ObjTag represents objects that are not callable. Callable objects are associated
 *   with both the ObjTag and the FunTag.
 * - Interfaces can be objects, class instances, arrays, functions and symbols *)
module TypeTag = struct
  type sentinel_val =
    | Str of Reason.name
    | Num of number_literal
    | Bool of bool

  type sentinel_map = sentinel_val SMap.t

  (* Compare sentinel maps on their common keys. Keys that do not appear in both
   * objects are ignored as they cannot be used to determine non-overlapping values. *)
  let compare_sentinel_map (s1 : sentinel_val SMap.t) (s2 : sentinel_val SMap.t) =
    let keyset1 = SMap.keys s1 |> SSet.of_list in
    let keyset2 = SMap.keys s2 |> SSet.of_list in
    let keyset = SSet.inter keyset1 keyset2 in
    if SSet.for_all (fun k -> SMap.find k s1 = SMap.find k s2) keyset then
      0
    else
      1

  type t =
    | BoolTag
    | StringTag
    | NumberTag
    | NullTag
    | SymbolTag
    | ObjTag of { sentinel: sentinel_map }
    | FunTag
    | BigIntTag
    | ClassInstanceTag
    | ArrTag
    | EnumTag
  [@@deriving ord]
end

open TypeTag

module TypeTagSet : Flow_set.S with type elt = TypeTag.t = Flow_set.Make (TypeTag)

let sentinel_of_obj cx id =
  Context.fold_real_props
    cx
    id
    (fun name prop acc ->
      match prop with
      | Field { type_; _ } ->
        let v_opt =
          match Context.find_resolved cx type_ with
          | Some (DefT (_, _, SingletonStrT name)) -> Some (TypeTag.Str name)
          | Some (DefT (_, _, SingletonNumT num_lit)) -> Some (TypeTag.Num num_lit)
          | Some (DefT (_, _, SingletonBoolT b)) -> Some (TypeTag.Bool b)
          | _ -> None
        in
        (match v_opt with
        | Some v -> SMap.add (Reason.display_string_of_name name) v acc
        | None -> acc)
      | _ -> acc)
    SMap.empty

let rec tag_of_def_t cx = function
  | NullT -> Some (TypeTagSet.singleton NullTag)
  | SymbolT -> Some (TypeTagSet.singleton SymbolTag)
  | FunT _ -> Some (TypeTagSet.singleton FunTag)
  | SingletonBoolT _
  | BoolT _ ->
    Some (TypeTagSet.singleton BoolTag)
  | SingletonStrT _
  | StrT _
  | CharSetT _ ->
    Some (TypeTagSet.singleton StringTag)
  | SingletonNumT _
  | NumT _ ->
    Some (TypeTagSet.singleton NumberTag)
  | BigIntT _
  | SingletonBigIntT _ ->
    Some (TypeTagSet.singleton BigIntTag)
  | ObjT { call_t = Some _; props_tmap; _ } ->
    Some (TypeTagSet.of_list [ObjTag { sentinel = sentinel_of_obj cx props_tmap }; FunTag])
  | ObjT { props_tmap; _ } ->
    Some (TypeTagSet.singleton (ObjTag { sentinel = sentinel_of_obj cx props_tmap }))
  | InstanceT { inst; _ } -> tag_of_inst inst
  | ArrT _ -> Some (TypeTagSet.singleton ArrTag)
  | PolyT { t_out; _ } -> tag_of_t cx t_out
  | EnumT _ -> Some (TypeTagSet.singleton EnumTag)
  | EmptyT -> Some TypeTagSet.empty
  | VoidT
  | MixedT _
  | ClassT _
  | TypeT _
  | IdxWrapper _
  | ReactAbstractComponentT _
  | EnumObjectT _ ->
    None

and tag_of_inst inst =
  let {
    inst_call_t;
    inst_kind;
    class_id = _;
    type_args = _;
    own_props = _;
    proto_props = _;
    initialized_fields = _;
    initialized_static_fields = _;
  } =
    inst
  in
  let tags =
    if Base.Option.is_some inst_call_t then
      TypeTagSet.singleton FunTag
    else
      TypeTagSet.empty
  in
  let tags =
    match inst_kind with
    | ClassKind -> TypeTagSet.add ClassInstanceTag tags
    | InterfaceKind _ ->
      tags
      |> TypeTagSet.add ClassInstanceTag
      |> TypeTagSet.add (ObjTag { sentinel = SMap.empty })
      |> TypeTagSet.add ArrTag
      |> TypeTagSet.add SymbolTag
      |> TypeTagSet.add FunTag
  in
  Some tags

and tag_of_t cx t =
  match t with
  | DefT (_, _, t) -> tag_of_def_t cx t
  | ExactT (_, t) -> tag_of_t cx t
  | OpenT _
  | AnnotT (_, _, _) ->
    Context.find_resolved cx t |> Base.Option.bind ~f:(tag_of_t cx)
  (* Most of the types below should have boiled away thanks to concretization. *)
  | EvalT _
  | GenericT _
  | ThisClassT _
  | ThisTypeAppT _
  | TypeAppT _
  | FunProtoT _
  | ObjProtoT _
  | NullProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | MatchingPropT _
  | IntersectionT _
  | UnionT _
  | MaybeT (_, _)
  | OptionalT _
  | KeysT (_, _)
  | OpaqueT (_, _)
  | ModuleT _
  | InternalT _
  | TypeDestructorTriggerT (_, _, _, _, _)
  | CustomFunT (_, _)
  | AnyT (_, _) ->
    None

let tags_overlap t1s t2s = TypeTagSet.inter t1s t2s |> TypeTagSet.is_empty |> not
