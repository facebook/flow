(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil

type filter_result =
  | TypeFilterResult of {
      type_: Type.t;
      changed: bool;
    }

let changed_result type_ = TypeFilterResult { type_; changed = true }

let unchanged_result type_ = TypeFilterResult { type_; changed = false }

let recurse_into_union cx filter_fn ((r, ts) : reason * Type.t list) =
  let (new_ts, changed) =
    List.fold_left
      (fun (new_ts, changed_acc) t ->
        let t =
          match t with
          | OpenT (_, id) ->
            let (_, constraints) = Context.find_constraints cx id in
            begin
              match constraints with
              | Constraint.FullyResolved s -> Context.force_fully_resolved_tvar cx s
              | Constraint.Resolved t -> t
              | _ -> t
            end
          | _ -> t
        in
        match filter_fn cx t with
        | TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> (new_ts, changed_acc || changed)
        | TypeFilterResult { type_ = filtered_type; changed } ->
          (filtered_type :: new_ts, changed_acc || changed))
      ([], false)
      ts
  in
  let new_ts = List.rev new_ts in
  match new_ts with
  | [] -> TypeFilterResult { type_ = DefT (r, EmptyT); changed }
  | [t] -> TypeFilterResult { type_ = t; changed }
  | t0 :: t1 :: ts -> TypeFilterResult { type_ = UnionT (r, UnionRep.make t0 t1 ts); changed }

let recurse_into_intersection cx =
  let rec helper filter_fn r (t_acc, changed_acc) = function
    | [] -> (List.rev t_acc, changed_acc)
    | t :: ts -> begin
      let t =
        match t with
        | OpenT (_, id) ->
          let (_, constraints) = Context.find_constraints cx id in
          begin
            match constraints with
            | Constraint.FullyResolved s -> Context.force_fully_resolved_tvar cx s
            | Constraint.Resolved t -> t
            | _ -> t
          end
        | _ -> t
      in
      match filter_fn t with
      | TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> ([], changed_acc || changed)
      | TypeFilterResult { type_ = filtered_type; changed } ->
        helper filter_fn r (filtered_type :: t_acc, changed_acc || changed) ts
    end
  in
  fun filter_fn ((r, ts) : reason * Type.t list) ->
    match helper filter_fn r ([], false) ts with
    | ([], changed) -> TypeFilterResult { type_ = DefT (r, EmptyT); changed }
    | ([t], changed) -> TypeFilterResult { type_ = t; changed }
    | (t0 :: t1 :: ts, changed) ->
      TypeFilterResult { type_ = IntersectionT (r, InterRep.make t0 t1 ts); changed }

let filter_opaque filter_fn reason ({ underlying_t; super_t; _ } as opq) =
  match underlying_t with
  | Some underlying_t
    when ALoc.source (loc_of_reason reason) = ALoc.source (def_loc_of_reason reason) -> begin
    match filter_fn underlying_t with
    | TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ } ->
      DefT (reason, EmptyT) |> changed_result
    | TypeFilterResult { type_ = t; changed } ->
      TypeFilterResult { type_ = OpaqueT (reason, { opq with underlying_t = Some t }); changed }
  end
  | _ -> begin
    let super_t = Base.Option.value ~default:(DefT (reason, MixedT Mixed_everything)) super_t in
    match filter_fn super_t with
    | TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ } ->
      DefT (reason, EmptyT) |> changed_result
    | TypeFilterResult { type_ = t; changed } ->
      TypeFilterResult { type_ = OpaqueT (reason, { opq with super_t = Some t }); changed }
  end

let map_poly ~f t =
  match t with
  | DefT (r, PolyT ({ t_out; _ } as poly)) -> begin
    match f t_out with
    | TypeFilterResult { type_ = DefT (_, EmptyT) as empty; changed = _ } -> changed_result empty
    | TypeFilterResult { type_ = t_out; changed } ->
      TypeFilterResult { type_ = DefT (r, PolyT { poly with t_out }); changed }
  end
  | _ -> unchanged_result t

let empty t =
  match t with
  | DefT (_, EmptyT) -> unchanged_result t
  | _ -> changed_result (DefT (reason_of_t t, EmptyT))

let rec truthy cx t =
  if TypeUtil.is_falsy t then
    changed_result (DefT (reason_of_t t, EmptyT))
  else
    match t with
    (* unknown things become truthy *)
    | OpaqueT (r, opq) -> filter_opaque (truthy cx) r opq
    | UnionT (r, rep) -> recurse_into_union cx truthy (r, UnionRep.members rep)
    | MaybeT (_, t) -> changed_result t
    | OptionalT { reason = _; type_ = t; use_desc = _ } -> truthy cx t
    | DefT (r, BoolGeneralT) -> DefT (r, BoolT_UNSOUND true) |> changed_result
    | DefT (r, StrGeneralT AnyLiteral) -> DefT (r, StrGeneralT Truthy) |> changed_result
    | DefT (r, NumGeneralT AnyLiteral) -> DefT (r, NumGeneralT Truthy) |> changed_result
    | DefT (r, MixedT _) -> DefT (r, MixedT Mixed_truthy) |> changed_result
    (* an intersection passes through iff all of its members pass through *)
    | IntersectionT (r, rep) -> recurse_into_intersection cx (truthy cx) (r, InterRep.members rep)
    (* truthy things pass through *)
    | t -> unchanged_result t

let rec not_truthy cx t =
  if TypeUtil.is_falsy t then
    (* falsy things pass through *)
    unchanged_result t
  else
    match t with
    | DefT (_, PolyT _) -> map_poly ~f:(not_truthy cx) t
    | OpaqueT (r, opq) -> filter_opaque (not_truthy cx) r opq
    | AnyT (r, _) -> DefT (r, EmptyT) |> changed_result
    | UnionT (r, rep) -> recurse_into_union cx not_truthy (r, UnionRep.members rep)
    (* truthy things get removed *)
    | DefT
        ( r,
          ( SingletonBoolT _ | BoolT_UNSOUND _
          | EnumValueT
              ( ConcreteEnum { representation_t = DefT (_, (SingletonBoolT _ | BoolT_UNSOUND _)); _ }
              | AbstractEnum { representation_t = DefT (_, (SingletonBoolT _ | BoolT_UNSOUND _)) }
                )
          | SingletonStrT _ | NumericStrKeyT _ | StrT_UNSOUND _
          | StrGeneralT Truthy
          | EnumValueT
              ( ConcreteEnum { representation_t = DefT (_, StrGeneralT Truthy); _ }
              | AbstractEnum { representation_t = DefT (_, StrGeneralT Truthy) } )
          | ArrT _ | ObjT _ | InstanceT _ | EnumObjectT _ | FunT _ | ReactAbstractComponentT _
          | SingletonNumT _
          | NumGeneralT Truthy
          | NumT_UNSOUND _
          | EnumValueT
              ( ConcreteEnum { representation_t = DefT (_, NumGeneralT Truthy); _ }
              | AbstractEnum { representation_t = DefT (_, NumGeneralT Truthy) } )
          | EnumValueT
              ( ConcreteEnum { representation_t = DefT (_, BigIntGeneralT Truthy); _ }
              | AbstractEnum { representation_t = DefT (_, BigIntGeneralT Truthy) } )
          | MixedT Mixed_truthy )
        ) ->
      DefT (r, EmptyT) |> changed_result
    | DefT (reason, ClassT _) -> DefT (reason, EmptyT) |> changed_result
    | ThisInstanceT (reason, _, _, _) -> DefT (reason, EmptyT) |> changed_result
    (* unknown boolies become falsy *)
    | MaybeT (r, t) ->
      let (TypeFilterResult { type_ = t; changed }) = not_truthy cx t in
      TypeFilterResult
        { type_ = UnionT (r, UnionRep.make (NullT.why r) (VoidT.why r) [t]); changed }
    | DefT (r, BoolGeneralT) -> DefT (r, BoolT_UNSOUND false) |> changed_result
    | DefT (r, StrGeneralT AnyLiteral) ->
      DefT (r, StrT_UNSOUND (None, OrdinaryName "")) |> changed_result
    | DefT (r, NumGeneralT AnyLiteral) -> DefT (r, NumT_UNSOUND (None, (0., "0"))) |> changed_result
    (* an intersection passes through iff all of its members pass through *)
    | IntersectionT (r, rep) ->
      recurse_into_intersection cx (not_truthy cx) (r, InterRep.members rep)
    (* things that don't track truthiness pass through *)
    | t -> unchanged_result t

let rec maybe cx = function
  | OpaqueT (r, opq) -> filter_opaque (maybe cx) r opq
  | UnionT (r, rep) -> recurse_into_union cx maybe (r, UnionRep.members rep)
  | MaybeT (r, _) -> UnionT (r, UnionRep.make (NullT.why r) (VoidT.why r) []) |> changed_result
  | DefT (r, MixedT Mixed_everything) ->
    UnionT (r, UnionRep.make (NullT.why r) (VoidT.why r) []) |> changed_result
  | DefT (r, MixedT Mixed_truthy) -> EmptyT.why r |> changed_result
  | DefT (r, MixedT Mixed_non_maybe) -> EmptyT.why r |> changed_result
  | DefT (r, MixedT Mixed_non_void) -> DefT (r, NullT) |> changed_result
  | DefT (r, MixedT Mixed_non_null) -> DefT (r, VoidT) |> changed_result
  | DefT (_, NullT) as t -> unchanged_result t
  | DefT (_, VoidT) as t -> unchanged_result t
  | OptionalT { reason = r; type_ = _; use_desc } ->
    VoidT.why_with_use_desc ~use_desc r |> changed_result
  | AnyT _ as t -> unchanged_result t
  | DefT (r, _) -> EmptyT.why r |> changed_result
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason |> changed_result

let rec not_maybe cx = function
  | OpaqueT (r, opq) -> filter_opaque (not_maybe cx) r opq
  | UnionT (r, rep) -> recurse_into_union cx not_maybe (r, UnionRep.members rep)
  | MaybeT (_, t) -> changed_result t
  | OptionalT { reason = _; type_ = t; use_desc = _ } -> not_maybe cx t
  | DefT (r, (NullT | VoidT)) -> DefT (r, EmptyT) |> changed_result
  | DefT (r, MixedT Mixed_truthy) -> DefT (r, MixedT Mixed_truthy) |> unchanged_result
  | DefT (r, MixedT Mixed_non_maybe) -> DefT (r, MixedT Mixed_non_maybe) |> unchanged_result
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_void)
  | DefT (r, MixedT Mixed_non_null) ->
    DefT (r, MixedT Mixed_non_maybe) |> changed_result
  | t -> unchanged_result t

let rec null = function
  | OpaqueT (r, opq) -> filter_opaque null r opq
  | OptionalT { reason = _; type_ = MaybeT (r, _); use_desc = _ }
  | MaybeT (r, _) ->
    NullT.why r |> changed_result
  | DefT (_, NullT) as t -> t |> unchanged_result
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_void) ->
    NullT.why r |> changed_result
  | AnyT _ as t -> t |> changed_result
  | DefT (r, _) -> EmptyT.why r |> changed_result
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason |> changed_result

let rec not_null cx = function
  | OpaqueT (r, opq) -> filter_opaque (not_null cx) r opq
  | MaybeT (r, t) -> UnionT (r, UnionRep.make (VoidT.why r) t []) |> changed_result
  | OptionalT { reason; type_ = t; use_desc } ->
    let (TypeFilterResult { type_ = t; changed }) = not_null cx t in
    TypeFilterResult { type_ = OptionalT { reason; type_ = t; use_desc }; changed }
  | UnionT (r, rep) -> recurse_into_union cx not_null (r, UnionRep.members rep)
  | DefT (r, NullT) -> DefT (r, EmptyT) |> changed_result
  | DefT (r, MixedT Mixed_everything) -> DefT (r, MixedT Mixed_non_null) |> changed_result
  | DefT (r, MixedT Mixed_non_void) -> DefT (r, MixedT Mixed_non_maybe) |> changed_result
  | t -> unchanged_result t

let rec undefined = function
  | OpaqueT (r, opq) -> filter_opaque undefined r opq
  | MaybeT (r, _) -> VoidT.why r |> changed_result
  | DefT (_, VoidT) as t -> unchanged_result t
  | OptionalT { reason = r; type_ = _; use_desc } ->
    VoidT.why_with_use_desc ~use_desc r |> changed_result
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_null) ->
    VoidT.why r |> changed_result
  | AnyT _ as t -> unchanged_result t
  | DefT (r, _) -> EmptyT.why r |> changed_result
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason |> changed_result

let rec not_undefined cx = function
  | OpaqueT (r, opq) -> filter_opaque (not_undefined cx) r opq
  | MaybeT (r, t) -> UnionT (r, UnionRep.make (NullT.why r) t []) |> changed_result
  | OptionalT { reason = _; type_ = t; use_desc = _ } -> not_undefined cx t
  | UnionT (r, rep) -> recurse_into_union cx not_undefined (r, UnionRep.members rep)
  | DefT (r, VoidT) -> DefT (r, EmptyT) |> changed_result
  | DefT (r, MixedT Mixed_everything) -> DefT (r, MixedT Mixed_non_void) |> changed_result
  | DefT (r, MixedT Mixed_non_null) -> DefT (r, MixedT Mixed_non_maybe) |> changed_result
  | t -> unchanged_result t

let mk_str_literal cx expected =
  if Context.allow_unsound_literal_coercsion cx then
    StrT_UNSOUND (None, expected)
  else
    SingletonStrT expected

let string_literal cx expected_loc sense expected t =
  let expected_desc = RStringLit expected in
  let lit_reason = replace_desc_new_reason expected_desc in
  match t with
  | DefT (_, SingletonStrT actual) ->
    if actual = expected then
      unchanged_result t
    else
      DefT (mk_reason expected_desc expected_loc, SingletonStrT expected) |> changed_result
  | DefT (_, StrT_UNSOUND (_, actual)) ->
    if actual = expected then
      unchanged_result t
    else
      DefT (mk_reason expected_desc expected_loc, StrT_UNSOUND (Some sense, expected))
      |> changed_result
  | DefT (r, StrGeneralT Truthy) when expected <> OrdinaryName "" ->
    DefT (lit_reason r, mk_str_literal cx expected) |> changed_result
  | DefT (r, StrGeneralT AnyLiteral) ->
    DefT (lit_reason r, mk_str_literal cx expected) |> changed_result
  | DefT (r, MixedT _) -> DefT (lit_reason r, mk_str_literal cx expected) |> changed_result
  | AnyT _ as t -> unchanged_result t
  | DefT (r, _) -> DefT (r, EmptyT) |> changed_result
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_string_literal expected = function
  | DefT (r, SingletonStrT actual)
  | DefT (r, StrT_UNSOUND (_, actual))
    when actual = expected ->
    DefT (r, EmptyT) |> changed_result
  | t -> unchanged_result t

let mk_num_literal cx expected =
  if Context.allow_unsound_literal_coercsion cx then
    NumT_UNSOUND (None, expected)
  else
    SingletonNumT expected

let number_literal cx expected_loc sense expected t =
  let (_, expected_raw) = expected in
  let expected_desc = RNumberLit expected_raw in
  let lit_reason = replace_desc_new_reason expected_desc in
  match t with
  | DefT (_, SingletonNumT (_, actual_raw)) ->
    if actual_raw = expected_raw then
      unchanged_result t
    else
      DefT (mk_reason expected_desc expected_loc, SingletonNumT expected) |> changed_result
  | DefT (_, NumT_UNSOUND (_, (_, actual_raw))) ->
    if actual_raw = expected_raw then
      unchanged_result t
    else
      DefT (mk_reason expected_desc expected_loc, NumT_UNSOUND (Some sense, expected))
      |> changed_result
  | DefT (r, NumGeneralT Truthy) when snd expected <> "0" ->
    DefT (lit_reason r, mk_num_literal cx expected) |> changed_result
  | DefT (r, NumGeneralT AnyLiteral) ->
    DefT (lit_reason r, mk_num_literal cx expected) |> changed_result
  | DefT (r, MixedT _) -> DefT (lit_reason r, mk_num_literal cx expected) |> changed_result
  | AnyT _ as t -> unchanged_result t
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_number_literal expected = function
  | DefT (r, SingletonNumT actual)
  | DefT (r, NumT_UNSOUND (_, actual))
    when snd actual = snd expected ->
    DefT (r, EmptyT) |> changed_result
  | t -> unchanged_result t

let mk_bigint_literal cx expected =
  if Context.allow_unsound_literal_coercsion cx then
    BigIntT_UNSOUND (None, expected)
  else
    SingletonBigIntT expected

let bigint_literal cx expected_loc sense expected t =
  let (_, expected_raw) = expected in
  let expected_desc = RBigIntLit expected_raw in
  let lit_reason = replace_desc_new_reason expected_desc in
  match t with
  | DefT (_, SingletonBigIntT (_, actual_raw)) ->
    if actual_raw = expected_raw then
      unchanged_result t
    else
      DefT (mk_reason expected_desc expected_loc, SingletonBigIntT expected) |> changed_result
  | DefT (_, BigIntT_UNSOUND (_, (_, actual_raw))) ->
    if actual_raw = expected_raw then
      unchanged_result t
    else
      DefT (mk_reason expected_desc expected_loc, BigIntT_UNSOUND (Some sense, expected))
      |> changed_result
  | DefT (r, BigIntGeneralT Truthy) when snd expected <> "0n" ->
    DefT (lit_reason r, mk_bigint_literal cx expected) |> changed_result
  | DefT (r, BigIntGeneralT AnyLiteral) ->
    DefT (lit_reason r, mk_bigint_literal cx expected) |> changed_result
  | DefT (r, MixedT _) -> DefT (lit_reason r, mk_bigint_literal cx expected) |> changed_result
  | AnyT _ as t -> unchanged_result t
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_bigint_literal expected = function
  | DefT (r, SingletonBigIntT actual)
  | DefT (r, BigIntT_UNSOUND (_, actual))
    when snd actual = snd expected ->
    DefT (r, EmptyT) |> changed_result
  | t -> unchanged_result t

let mk_bool_literal cx expected =
  if Context.allow_unsound_literal_coercsion cx then
    BoolT_UNSOUND expected
  else
    SingletonBoolT expected

let true_ cx t =
  let lit_reason = replace_desc_new_reason (RBooleanLit true) in
  match t with
  | DefT (r, SingletonBoolT true) -> DefT (lit_reason r, SingletonBoolT true) |> unchanged_result
  | DefT (r, BoolT_UNSOUND true) -> DefT (lit_reason r, BoolT_UNSOUND true) |> unchanged_result
  | DefT (r, BoolGeneralT) -> DefT (lit_reason r, mk_bool_literal cx true) |> changed_result
  | DefT (r, MixedT _) -> DefT (lit_reason r, mk_bool_literal cx true) |> changed_result
  | AnyT _ as t -> unchanged_result t
  | t -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_true cx t =
  let lit_reason = replace_desc_new_reason (RBooleanLit false) in
  match t with
  | DefT (r, SingletonBoolT true)
  | DefT (r, BoolT_UNSOUND true) ->
    DefT (r, EmptyT) |> changed_result
  | DefT (r, BoolGeneralT) -> DefT (lit_reason r, mk_bool_literal cx false) |> changed_result
  | t -> unchanged_result t

let false_ cx t =
  let lit_reason = replace_desc_new_reason (RBooleanLit false) in
  match t with
  | DefT (r, SingletonBoolT false) -> DefT (lit_reason r, SingletonBoolT false) |> unchanged_result
  | DefT (r, BoolT_UNSOUND false) -> DefT (lit_reason r, BoolT_UNSOUND false) |> unchanged_result
  | DefT (r, BoolGeneralT) -> DefT (lit_reason r, mk_bool_literal cx false) |> changed_result
  | DefT (r, MixedT _) -> DefT (lit_reason r, mk_bool_literal cx false) |> changed_result
  | AnyT _ as t -> unchanged_result t
  | t -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_false cx t =
  let lit_reason = replace_desc_new_reason (RBooleanLit true) in
  match t with
  | DefT (r, SingletonBoolT false)
  | DefT (r, BoolT_UNSOUND false) ->
    DefT (r, EmptyT) |> changed_result
  | DefT (r, BoolGeneralT) -> DefT (lit_reason r, mk_bool_literal cx true) |> changed_result
  | t -> unchanged_result t

let boolean cx loc t =
  match t with
  | DefT (r, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason BoolModuleT.desc r, mk_bool_literal cx true) |> changed_result
  | AnyT _
  | DefT (_, MixedT _) ->
    DefT (mk_reason RBoolean loc, BoolGeneralT) |> changed_result
  | DefT (_, BoolGeneralT)
  | DefT (_, SingletonBoolT _)
  | DefT (_, BoolT_UNSOUND _)
  | DefT
      ( _,
        EnumValueT
          ( ConcreteEnum
              {
                representation_t = DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _));
                _;
              }
          | AbstractEnum
              { representation_t = DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _)) }
            )
      ) ->
    unchanged_result t
  | DefT (r, _) -> DefT (r, EmptyT) |> changed_result
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_boolean t =
  match t with
  | DefT
      ( _,
        EnumValueT
          ( ConcreteEnum
              {
                representation_t = DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _));
                _;
              }
          | AbstractEnum
              { representation_t = DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _)) }
            )
      )
  | DefT (_, BoolGeneralT)
  | DefT (_, SingletonBoolT _)
  | DefT (_, BoolT_UNSOUND _) ->
    DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let string loc t =
  match t with
  | DefT (r, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason StrModuleT.desc r, StrGeneralT Truthy) |> changed_result
  | AnyT _
  | DefT (_, MixedT _) ->
    DefT (mk_reason RString loc, StrGeneralT AnyLiteral) |> changed_result
  | StrUtilT _
  | DefT (_, StrGeneralT _)
  | DefT (_, SingletonStrT _)
  | DefT (_, StrT_UNSOUND _)
  | DefT
      ( _,
        EnumValueT
          ( ConcreteEnum
              { representation_t = DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)); _ }
          | AbstractEnum
              { representation_t = DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)) } )
      ) ->
    unchanged_result t
  | DefT (r, _) -> DefT (r, EmptyT) |> changed_result
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_string t =
  match t with
  | StrUtilT _
  | DefT
      ( _,
        EnumValueT
          ( ConcreteEnum
              { representation_t = DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)); _ }
          | AbstractEnum
              { representation_t = DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)) } )
      )
  | DefT (_, StrGeneralT _)
  | DefT (_, SingletonStrT _)
  | DefT (_, StrT_UNSOUND _) ->
    DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let symbol loc t =
  match t with
  | DefT (_, SymbolT) -> unchanged_result t
  | DefT (_, MixedT _)
  | AnyT _ ->
    SymbolT.why (mk_reason RSymbol loc) |> changed_result
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_symbol t =
  match t with
  | DefT (_, SymbolT) -> DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let number loc t =
  match t with
  | DefT (r, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason NumModuleT.desc r, NumGeneralT Truthy) |> changed_result
  | AnyT _
  | DefT (_, MixedT _) ->
    DefT (mk_reason RNumber loc, NumGeneralT AnyLiteral) |> changed_result
  | DefT (_, NumGeneralT _)
  | DefT (_, SingletonNumT _)
  | DefT (_, NumT_UNSOUND _)
  | DefT
      ( _,
        EnumValueT
          ( ConcreteEnum
              { representation_t = DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)); _ }
          | AbstractEnum
              { representation_t = DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)) } )
      ) ->
    unchanged_result t
  | DefT (r, _) -> DefT (r, EmptyT) |> changed_result
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_number t =
  match t with
  | DefT
      ( _,
        EnumValueT
          ( ConcreteEnum
              { representation_t = DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)); _ }
          | AbstractEnum
              { representation_t = DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)) } )
      )
  | DefT (_, NumGeneralT _)
  | DefT (_, SingletonNumT _)
  | DefT (_, NumT_UNSOUND _) ->
    DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let bigint loc t =
  match t with
  | DefT (r, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason BigIntModuleT.desc r, BigIntGeneralT Truthy) |> changed_result
  | AnyT _
  | DefT (_, MixedT _) ->
    DefT (mk_reason RBigInt loc, BigIntGeneralT AnyLiteral) |> changed_result
  | DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _))
  | DefT
      ( _,
        EnumValueT
          ( ConcreteEnum
              {
                representation_t =
                  DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _));
                _;
              }
          | AbstractEnum
              {
                representation_t =
                  DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _));
              } )
      ) ->
    unchanged_result t
  | DefT (r, _) -> DefT (r, EmptyT) |> changed_result
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_bigint t =
  match t with
  | DefT
      ( _,
        EnumValueT
          ( ConcreteEnum
              {
                representation_t =
                  DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _));
                _;
              }
          | AbstractEnum
              {
                representation_t =
                  DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _));
              } )
      )
  | DefT (_, BigIntGeneralT _)
  | DefT (_, SingletonBigIntT _)
  | DefT (_, BigIntT_UNSOUND _) ->
    DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let rec object_ cx t =
  match t with
  | DefT (_, PolyT _) -> map_poly ~f:(object_ cx) t
  | DefT (r, MixedT flavor) ->
    let reason = replace_desc_new_reason RObject r in
    let dict =
      {
        key = StrModuleT.why r;
        value = DefT (replace_desc_new_reason MixedT.desc r, MixedT Mixed_everything);
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
        obj |> changed_result
      | Mixed_function -> DefT (reason_of_t t, EmptyT) |> changed_result
      | Mixed_everything
      | Mixed_non_void ->
        let reason = replace_desc_new_reason RUnion (reason_of_t t) in
        UnionT (reason, UnionRep.make (NullT.why r) obj []) |> changed_result
    end
  | DefT (_, (ObjT _ | ArrT _ | NullT | InstanceT _ | EnumObjectT _))
  | AnyT _ ->
    unchanged_result t
  | DefT (r, _) -> DefT (r, EmptyT) |> changed_result
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let rec not_object t =
  match t with
  | DefT (_, PolyT _) -> map_poly ~f:not_object t
  | AnyT _ -> DefT (reason_of_t t, EmptyT) |> changed_result
  | DefT (_, (ObjT _ | ArrT _ | NullT | InstanceT _ | EnumObjectT _)) ->
    DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let rec function_ = function
  | DefT (_, PolyT _) as t -> map_poly ~f:function_ t
  | DefT (r, MixedT _) ->
    DefT (replace_desc_new_reason (RFunction RUnknown) r, MixedT Mixed_function) |> changed_result
  | (DefT (_, (FunT _ | ClassT _)) | AnyT _) as t -> unchanged_result t
  | DefT (r, _) -> DefT (r, EmptyT) |> changed_result
  | t -> DefT (reason_of_t t, EmptyT) |> changed_result

let rec not_function t =
  match t with
  | DefT (_, PolyT _) -> map_poly ~f:not_function t
  | AnyT _ -> DefT (reason_of_t t, EmptyT) |> changed_result
  | DefT (_, (FunT _ | ClassT _)) -> DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let array t =
  match t with
  | DefT (r, MixedT _) ->
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
        ArrT (ROArrayAT (DefT (r, MixedT Mixed_everything), None))
      )
    |> changed_result
  | DefT (_, ArrT _)
  | AnyT _ ->
    unchanged_result t
  | _ -> DefT (reason_of_t t, EmptyT) |> changed_result

let not_array t =
  match t with
  | AnyT _ -> DefT (reason_of_t t, EmptyT) |> changed_result
  | DefT (_, ArrT _) -> DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let array_length ~sense ~op ~n t =
  match t with
  | DefT (_, ArrT (TupleAT { arity = (num_req, num_total); inexact; _ }))
  | DefT
      ( _,
        ArrT
          (ArrayAT { tuple_view = Some (TupleView { arity = (num_req, num_total); inexact; _ }); _ })
      ) ->
    (* `None` represents "maybe" a match *)
    let matches =
      match op with
      | ArrLenEqual ->
        if n = num_req && n = num_total && not inexact then
          Some true
        else if n >= num_req && (n <= num_total || inexact) then
          None
        else
          Some false
      | ArrLenGreaterThanEqual ->
        if n <= num_req then
          Some true
        else if n <= num_total || inexact then
          None
        else
          Some false
    in
    (match (matches, sense) with
    | (Some true, true)
    | (Some false, false)
    | (None, _) ->
      unchanged_result t
    | (Some false, true)
    | (Some true, false) ->
      DefT (reason_of_t t, EmptyT) |> changed_result)
  | DefT (_, ArrT (ArrayAT _ | ROArrayAT _)) ->
    (* `[...]` matches every length, so arrays are matched. *)
    let matches = n = 0 && op = ArrLenGreaterThanEqual in
    if matches = sense then
      unchanged_result t
    else
      DefT (reason_of_t t, EmptyT) |> changed_result
  | _ -> unchanged_result t

let sentinel_refinement =
  let open UnionEnum in
  let enum_match sense = function
    | (DefT (_, SingletonStrT value), Str sentinel)
    | (DefT (_, StrT_UNSOUND (_, value)), Str sentinel)
      when value = sentinel != sense ->
      true
    | (DefT (_, SingletonNumT (value, _)), Num (sentinel, _))
    | (DefT (_, NumT_UNSOUND (_, (value, _))), Num (sentinel, _))
      when value = sentinel != sense ->
      true
    | (DefT (_, SingletonBoolT value), Bool sentinel)
    | (DefT (_, BoolT_UNSOUND value), Bool sentinel)
      when value = sentinel != sense ->
      true
    | (DefT (_, SingletonBigIntT (value, _)), BigInt (sentinel, _))
    | (DefT (_, BigIntT_UNSOUND (_, (value, _))), BigInt (sentinel, _))
      when value = sentinel != sense ->
      true
    | (DefT (_, NullT), Null)
    | (DefT (_, VoidT), Void) ->
      true
    | _ -> false
  in
  fun v reason l sense enum ->
    let rec filtered_loop enum =
      match (v, enum) with
      | (_, One e) when enum_match sense (v, e) && not sense -> true
      | (DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)), One (Str sentinel))
        when enum_match sense (v, Str sentinel) ->
        true
      | (DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)), One (Num sentinel))
        when enum_match sense (v, Num sentinel) ->
        true
      | (DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _)), One (Bool sentinel))
        when enum_match sense (v, Bool sentinel) ->
        true
      | ( DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _)),
          One (BigInt sentinel)
        )
        when enum_match sense (v, BigInt sentinel) ->
        true
      | ( DefT
            ( _,
              ( StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _ | NumGeneralT _ | NumT_UNSOUND _
              | SingletonNumT _ | BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _
              | BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _ | NullT | VoidT )
            ),
          Many enums
        )
        when sense ->
        UnionEnumSet.elements enums
        |> Base.List.for_all ~f:(fun enum ->
               if enum_match sense (v, enum) |> not then
                 filtered_loop (One enum)
               else
                 true
           )
      | (DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)), One (Str _))
      | (DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)), One (Num _))
      | (DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _)), One (Bool _))
      | (DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _)), One (BigInt _))
      | (DefT (_, NullT), One Null)
      | (DefT (_, VoidT), One Void)
      | ( DefT
            ( _,
              ( StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _ | NumGeneralT _ | NumT_UNSOUND _
              | SingletonNumT _ | BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _
              | BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _ | NullT | VoidT )
            ),
          Many _
        ) ->
        false
      (* types don't match (would've been matched above) *)
      (* we don't prune other types like objects or instances, even though
         a test like `if (ObjT === StrT)` seems obviously unreachable, but
         we have to be wary of toString and valueOf on objects/instances. *)
      | ( DefT
            ( _,
              ( StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _ | NumGeneralT _ | NumT_UNSOUND _
              | SingletonNumT _ | BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _
              | BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _ | NullT | VoidT )
            ),
          _
        )
        when sense ->
        true
      | ( DefT
            ( _,
              ( StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _ | NumGeneralT _ | NumT_UNSOUND _
              | SingletonNumT _ | BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _
              | BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _ | NullT | VoidT )
            ),
          _
        )
      | _ ->
        (* property exists, but is not something we can use for refinement *)
        false
    in
    if filtered_loop enum then
      EmptyT.why reason |> changed_result
    else
      unchanged_result l

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
    let s1' = SMap.filter (fun k _ -> SMap.mem k s2) s1 in
    let s2' = SMap.filter (fun k _ -> SMap.mem k s1) s2 in
    compare s1' s2'

  type t =
    | BoolTag
    | StringTag
    | NumberTag
    | NullTag
    | VoidTag
    | SymbolTag
    | ObjTag of { sentinel: sentinel_map }
    | FunTag
    | BigIntTag
    | ClassInstanceTag
    | ArrTag of { sentinel: sentinel_map }
    | EnumTag
  [@@deriving ord]
end

open TypeTag

module TypeTagSet : Flow_set.S with type elt = TypeTag.t = Flow_set.Make (TypeTag)

let tag_of_value cx type_ =
  match Context.find_resolved cx type_ with
  | Some (DefT (_, NumericStrKeyT (_, s))) -> Some (TypeTag.Str (OrdinaryName s))
  | Some (DefT (_, SingletonStrT name)) -> Some (TypeTag.Str name)
  | Some (DefT (_, SingletonNumT num_lit)) -> Some (TypeTag.Num num_lit)
  | Some (DefT (_, SingletonBoolT b)) -> Some (TypeTag.Bool b)
  | _ -> None

let sentinel_of_obj cx id =
  Context.fold_real_props
    cx
    id
    (fun name prop acc ->
      match prop with
      | Field { type_; _ } ->
        (match tag_of_value cx type_ with
        | Some v -> SMap.add (Reason.display_string_of_name name) v acc
        | None -> acc)
      | _ -> acc)
    SMap.empty

let sentinel_of_tuple cx elements =
  Base.List.foldi
    ~init:SMap.empty
    ~f:(fun i acc (TupleElement { t; _ }) ->
      match tag_of_value cx t with
      | Some v -> SMap.add (string_of_int i) v acc
      | None -> acc)
    elements

let rec tag_of_def_t cx = function
  | NullT -> Some (TypeTagSet.singleton NullTag)
  | VoidT -> Some (TypeTagSet.singleton VoidTag)
  | SymbolT -> Some (TypeTagSet.singleton SymbolTag)
  | FunT _ -> Some (TypeTagSet.singleton FunTag)
  | SingletonBoolT _
  | BoolGeneralT
  | BoolT_UNSOUND _ ->
    Some (TypeTagSet.singleton BoolTag)
  | SingletonStrT _
  | NumericStrKeyT _
  | StrGeneralT _
  | StrT_UNSOUND _ ->
    Some (TypeTagSet.singleton StringTag)
  | SingletonNumT _
  | NumGeneralT _
  | NumT_UNSOUND _ ->
    Some (TypeTagSet.singleton NumberTag)
  | BigIntGeneralT _
  | BigIntT_UNSOUND _
  | SingletonBigIntT _ ->
    Some (TypeTagSet.singleton BigIntTag)
  | ObjT { call_t = Some _; props_tmap; _ } ->
    Some (TypeTagSet.of_list [ObjTag { sentinel = sentinel_of_obj cx props_tmap }; FunTag])
  | ObjT { props_tmap; _ } ->
    Some (TypeTagSet.singleton (ObjTag { sentinel = sentinel_of_obj cx props_tmap }))
  | InstanceT { inst; _ } -> tag_of_inst inst
  | ArrT (ArrayAT _ | ROArrayAT _) -> Some (TypeTagSet.singleton (ArrTag { sentinel = SMap.empty }))
  | ArrT (TupleAT { elements; _ }) ->
    Some (TypeTagSet.singleton (ArrTag { sentinel = sentinel_of_tuple cx elements }))
  | PolyT { t_out; _ } -> tag_of_t cx t_out
  | EnumValueT _ -> Some (TypeTagSet.singleton EnumTag)
  | EmptyT -> Some TypeTagSet.empty
  | MixedT _
  | ClassT _
  | TypeT _
  | ReactAbstractComponentT _
  | RendersT _
  | EnumObjectT _ ->
    None

and tag_of_inst inst =
  let {
    inst_react_dro = _;
    inst_call_t;
    inst_kind;
    class_id = _;
    class_name = _;
    type_args = _;
    own_props = _;
    proto_props = _;
    initialized_fields = _;
    initialized_static_fields = _;
    inst_dict = _;
    class_private_fields = _;
    class_private_methods = _;
    class_private_static_fields = _;
    class_private_static_methods = _;
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
      |> TypeTagSet.add (ArrTag { sentinel = SMap.empty })
      |> TypeTagSet.add SymbolTag
      |> TypeTagSet.add FunTag
  in
  Some tags

and tag_of_t cx t =
  match t with
  | DefT (_, t) -> tag_of_def_t cx t
  | ThisInstanceT (_, { inst; _ }, _, _) -> tag_of_inst inst
  | OpenT _
  | AnnotT (_, _, _) ->
    Context.find_resolved cx t |> Base.Option.bind ~f:(tag_of_t cx)
  | OpaqueT (r, { underlying_t = Some t; _ })
    when ALoc.source (loc_of_reason r) = ALoc.source (def_loc_of_reason r) ->
    tag_of_t cx t
  | OpaqueT (_, { super_t = Some t; _ }) -> tag_of_t cx t
  (* Most of the types below should have boiled away thanks to concretization. *)
  | NamespaceT { values_type; _ } -> tag_of_t cx values_type
  | StrUtilT _ -> Some (TypeTagSet.singleton StringTag)
  | EvalT _
  | GenericT _
  | ThisTypeAppT _
  | TypeAppT _
  | FunProtoT _
  | ObjProtoT _
  | NullProtoT _
  | FunProtoBindT _
  | IntersectionT _
  | UnionT _
  | MaybeT (_, _)
  | OptionalT _
  | KeysT (_, _)
  | OpaqueT (_, _)
  | AnyT (_, _) ->
    None

let tags_overlap t1s t2s = TypeTagSet.inter t1s t2s |> TypeTagSet.is_empty |> not
