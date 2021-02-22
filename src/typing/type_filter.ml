(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil

let recurse_into_union filter_fn ((r, ts) : reason * Type.t list) =
  let new_ts =
    List.fold_left
      (fun new_ts t ->
        match filter_fn t with
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
    | t :: ts ->
      begin
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

let rec exists = function
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
        | NumT (Literal (_, (0., _))) ) ) ->
    DefT (r, trust, EmptyT)
  (* unknown things become truthy *)
  | UnionT (r, rep) -> recurse_into_union exists (r, UnionRep.members rep)
  | MaybeT (_, t) -> t
  | OptionalT { reason = _; type_ = t; use_desc = _ } -> exists t
  | DefT (r, trust, BoolT None) -> DefT (r, trust, BoolT (Some true))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (r, trust, StrT Truthy)
  | DefT (r, trust, NumT AnyLiteral) -> DefT (r, trust, NumT Truthy)
  | DefT (r, trust, MixedT _) -> DefT (r, trust, MixedT Mixed_truthy)
  (* an intersection passes through iff all of its members pass through *)
  | IntersectionT (r, rep) -> recurse_into_intersection exists (r, InterRep.members rep)
  (* truthy things pass through *)
  | t -> t

let rec not_exists t =
  match t with
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
        | NumT (Literal (_, (0., _))) ) ) ->
    t
  | AnyT (r, _) -> DefT (r, Trust.bogus_trust (), EmptyT)
  | UnionT (r, rep) -> recurse_into_union not_exists (r, UnionRep.members rep)
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
        | MixedT Mixed_truthy ) ) ->
    DefT (r, trust, EmptyT)
  | DefT (reason, trust, ClassT _) -> DefT (reason, trust, EmptyT)
  (* unknown boolies become falsy *)
  | MaybeT (r, _) ->
    UnionT
      ( r,
        UnionRep.make (Trust.bogus_trust () |> NullT.why r) (Trust.bogus_trust () |> VoidT.why r) []
      )
  | DefT (r, trust, BoolT None) -> DefT (r, trust, BoolT (Some false))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (r, trust, StrT (Literal (None, OrdinaryName "")))
  | DefT (r, trust, NumT AnyLiteral) -> DefT (r, trust, NumT (Literal (None, (0., "0"))))
  (* an intersection passes through iff all of its members pass through *)
  | IntersectionT (r, rep) -> recurse_into_intersection not_exists (r, InterRep.members rep)
  (* things that don't track truthiness pass through *)
  | t -> t

let rec maybe = function
  | UnionT (r, rep) -> recurse_into_union maybe (r, UnionRep.members rep)
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

let rec not_maybe = function
  | UnionT (r, rep) -> recurse_into_union not_maybe (r, UnionRep.members rep)
  | MaybeT (_, t) -> t
  | OptionalT { reason = _; type_ = t; use_desc = _ } -> not_maybe t
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

let rec not_null = function
  | MaybeT (r, t) -> UnionT (r, UnionRep.make (Trust.bogus_trust () |> VoidT.why r) t [])
  | OptionalT { reason; type_ = t; use_desc } -> OptionalT { reason; type_ = not_null t; use_desc }
  | UnionT (r, rep) -> recurse_into_union not_null (r, UnionRep.members rep)
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

let rec not_undefined = function
  | MaybeT (r, t) -> UnionT (r, UnionRep.make (NullT.why r |> with_trust bogus_trust) t [])
  | OptionalT { reason = _; type_ = t; use_desc = _ } -> not_undefined t
  | UnionT (r, rep) -> recurse_into_union not_undefined (r, UnionRep.members rep)
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

let object_ cx t =
  match t with
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

let not_object t =
  match t with
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT)
  | DefT (_, trust, (ObjT _ | ArrT _ | NullT | InstanceT _ | EnumObjectT _)) ->
    DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let function_ = function
  | DefT (r, trust, MixedT _) ->
    DefT (replace_desc_new_reason (RFunction RUnknown) r, trust, MixedT Mixed_function)
  | (DefT (_, _, (FunT _ | ClassT _)) | AnyT _) as t -> t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT)
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_function t =
  match t with
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT)
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
        ArrT (ROArrayAT (DefT (r, trust, MixedT Mixed_everything))) )
  | DefT (_, _, ArrT _)
  | AnyT _ ->
    t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_array t =
  match t with
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT)
  | DefT (_, trust, ArrT _) -> DefT (reason_of_t t, trust, EmptyT)
  | _ -> t
