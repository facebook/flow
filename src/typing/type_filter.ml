(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type

let recurse_into_union filter_fn ((r, ts) : reason * Type.t list) =
  let new_ts =
    List.fold_left
      (fun new_ts t ->
        match filter_fn t with
        | DefT (_, _, EmptyT Bottom) -> new_ts
        | filtered_type -> filtered_type :: new_ts)
      []
      ts
  in
  let new_ts = List.rev new_ts in
  match new_ts with
  | [] -> DefT (r, bogus_trust (), EmptyT Bottom)
  | [t] -> t
  | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)

let recurse_into_intersection =
  let rec helper filter_fn r acc = function
    | [] -> List.rev acc
    | t :: ts ->
      begin
        match filter_fn t with
        | DefT (_, _, EmptyT Bottom) -> []
        | filtered_type -> helper filter_fn r (filtered_type :: acc) ts
      end
  in
  fun filter_fn ((r, ts) : reason * Type.t list) ->
    match helper filter_fn r [] ts with
    | [] -> DefT (r, bogus_trust (), EmptyT Bottom)
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
        | SingletonStrT ""
        | StrT (Literal (_, ""))
        | SingletonNumT (0., _)
        | NumT (Literal (_, (0., _))) ) ) ->
    DefT (r, trust, EmptyT Bottom)
  (* unknown things become truthy *)
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
        | SingletonStrT ""
        | StrT (Literal (_, ""))
        | SingletonNumT (0., _)
        | NumT (Literal (_, (0., _))) ) ) ->
    t
  | AnyT (r, _) -> DefT (r, Trust.bogus_trust (), EmptyT Bottom)
  (* truthy things get removed *)
  | DefT
      ( r,
        trust,
        ( SingletonBoolT _
        | BoolT (Some _)
        | SingletonStrT _
        | StrT (Literal _ | Truthy)
        | ArrT _ | ObjT _ | InstanceT _ | EnumObjectT _ | FunT _ | SingletonNumT _
        | NumT (Literal _ | Truthy)
        | MixedT Mixed_truthy ) ) ->
    DefT (r, trust, EmptyT Bottom)
  | DefT (reason, trust, ClassT _) -> DefT (reason, trust, EmptyT Bottom)
  (* unknown boolies become falsy *)
  | MaybeT (r, _) ->
    UnionT
      ( r,
        UnionRep.make (Trust.bogus_trust () |> NullT.why r) (Trust.bogus_trust () |> VoidT.why r) []
      )
  | DefT (r, trust, BoolT None) -> DefT (r, trust, BoolT (Some false))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (r, trust, StrT (Literal (None, "")))
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
  | DefT (r, trust, (NullT | VoidT)) -> DefT (r, trust, EmptyT Bottom)
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
  | DefT (r, trust, NullT) -> DefT (r, trust, EmptyT Bottom)
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
  | DefT (r, trust, VoidT) -> DefT (r, trust, EmptyT Bottom)
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
  | DefT (r, trust, StrT Truthy) when expected <> "" ->
    DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | AnyT _ as t -> t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT Bottom)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_string_literal expected = function
  | DefT (r, trust, StrT (Literal (_, actual))) when actual = expected ->
    DefT (r, trust, EmptyT Bottom)
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
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_number_literal expected = function
  | DefT (r, trust, NumT (Literal (_, actual))) when snd actual = snd expected ->
    DefT (r, trust, EmptyT Bottom)
  | t -> t

let true_ t =
  let lit_reason = replace_desc_new_reason (RBooleanLit true) in
  match t with
  | DefT (r, trust, BoolT (Some true)) -> DefT (lit_reason r, trust, BoolT (Some true))
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some true))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, BoolT (Some true))
  | AnyT _ as t -> t
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_true t =
  let lit_reason = replace_desc_new_reason (RBooleanLit false) in
  match t with
  | DefT (r, trust, BoolT (Some true)) -> DefT (r, trust, EmptyT Bottom)
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some false))
  | t -> t

let false_ t =
  let lit_reason = replace_desc_new_reason (RBooleanLit false) in
  match t with
  | DefT (r, trust, BoolT (Some false)) -> DefT (lit_reason r, trust, BoolT (Some false))
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some false))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, BoolT (Some false))
  | AnyT _ as t -> t
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_false t =
  let lit_reason = replace_desc_new_reason (RBooleanLit true) in
  match t with
  | DefT (r, trust, BoolT (Some false)) -> DefT (r, trust, EmptyT Bottom)
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some true))
  | t -> t

let boolean t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason BoolT.desc r, trust, BoolT (Some true))
  | DefT (r, trust, MixedT _) -> BoolT.why r trust
  | DefT (_, _, BoolT _)
  | AnyT _ ->
    t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT Bottom)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_boolean t =
  match t with
  (* TODO: this is wrong, AnyT can be a bool *)
  | DefT (_, trust, BoolT _) -> DefT (reason_of_t t, trust, EmptyT Bottom)
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT Bottom)
  | _ -> t

let string t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason StrT.desc r, trust, StrT Truthy)
  | DefT (r, trust, MixedT _) -> StrT.why r trust
  | DefT (_, _, StrT _)
  | AnyT _ ->
    t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT Bottom)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_string t =
  match t with
  (* TODO: this is wrong, AnyT can be a string *)
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT Bottom)
  | DefT (_, trust, StrT _) -> DefT (reason_of_t t, trust, EmptyT Bottom)
  | _ -> t

let symbol t =
  match t with
  | DefT (r, trust, MixedT _) -> DefT (replace_desc_new_reason RSymbol r, trust, MixedT Mixed_symbol)
  | _ ->
    (* TODO: since symbols aren't supported, `t` is never a symbol so always empty *)
    let reason = reason_of_t t in
    DefT (replace_desc_new_reason RSymbol reason, bogus_trust (), EmptyT Bottom)

let not_symbol t =
  (* TODO: since symbols aren't supported, `t` is never a symbol so always pass it through *)
  t

let number t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) ->
    DefT (replace_desc_new_reason NumT.desc r, trust, NumT Truthy)
  | DefT (r, trust, MixedT _) -> NumT.why r trust
  | DefT (_, _, NumT _)
  | AnyT _ ->
    t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT Bottom)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_number t =
  match t with
  (* TODO: this is wrong, AnyT can be a number *)
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT Bottom)
  | DefT (_, trust, NumT _) -> DefT (reason_of_t t, trust, EmptyT Bottom)
  | _ -> t

let object_ cx t =
  match t with
  | DefT (r, trust, MixedT flavor) ->
    let reason = replace_desc_new_reason RObject r in
    let dict =
      Some
        {
          key = StrT.why r |> with_trust bogus_trust;
          value =
            DefT (replace_desc_new_reason MixedT.desc r, bogus_trust (), MixedT Mixed_everything);
          dict_name = None;
          dict_polarity = Polarity.Positive;
        }
    in
    let proto = ObjProtoT reason in
    let obj = Obj_type.mk_with_proto cx reason ?dict proto in
    begin
      match flavor with
      | Mixed_symbol
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
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT Bottom)
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_object t =
  match t with
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT Bottom)
  | DefT (_, trust, (ObjT _ | ArrT _ | NullT | InstanceT _ | EnumObjectT _)) ->
    DefT (reason_of_t t, trust, EmptyT Bottom)
  | _ -> t

let function_ = function
  | DefT (r, trust, MixedT _) ->
    DefT (replace_desc_new_reason (RFunction RUnknown) r, trust, MixedT Mixed_function)
  | (DefT (_, _, (FunT _ | ClassT _)) | AnyT _) as t -> t
  | DefT (r, trust, _) -> DefT (r, trust, EmptyT Bottom)
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_function t =
  match t with
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT Bottom)
  | DefT (_, trust, (FunT _ | ClassT _)) -> DefT (reason_of_t t, trust, EmptyT Bottom)
  | _ -> t

let array t =
  match t with
  | DefT (r, trust, MixedT _) ->
    DefT
      ( replace_desc_new_reason RROArrayType r,
        trust,
        ArrT (ROArrayAT (DefT (r, trust, MixedT Mixed_everything))) )
  | DefT (_, _, ArrT _)
  | AnyT _ ->
    t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT Bottom)

let not_array t =
  match t with
  | AnyT _ -> DefT (reason_of_t t, Trust.bogus_trust (), EmptyT Bottom)
  | DefT (_, trust, ArrT _) -> DefT (reason_of_t t, trust, EmptyT Bottom)
  | _ -> t
