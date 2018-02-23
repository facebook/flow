(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type

let recurse_into_union filter_fn (r, ts) =
  let new_ts, updated = List.fold_left (fun (new_ts, updated) t ->
    let filtered_type, did_update = filter_fn t in
    let updated = updated || did_update in
    match filtered_type with
    | DefT (_, EmptyT) -> new_ts, updated
    | _ -> filtered_type::new_ts, updated
  ) ([], false) ts in
  let new_ts = List.rev new_ts in
  match new_ts with
  | [] -> DefT (r, EmptyT), updated
  | [t] -> t, updated
  | t0::t1::ts -> DefT (r, UnionT (UnionRep.make t0 t1 ts)), updated

let rec exists = function
  (* falsy things get removed *)
  | DefT (r, (
      NullT
    | VoidT
    | SingletonBoolT false
    | BoolT (Some false)
    | SingletonStrT ""
    | StrT (Literal (_, ""))
    | SingletonNumT (0., _)
    | NumT (Literal (_, (0., _)))
    )) -> DefT (r, EmptyT), true

  (* unknown things become truthy *)
  | DefT (_, MaybeT t) -> t, true
  | DefT (_, OptionalT t) -> t |> exists |> fst, true
  | DefT (r, BoolT None) -> DefT (r, BoolT (Some true)), true
  | DefT (r, StrT AnyLiteral) -> DefT (r, StrT Truthy), true
  | DefT (r, NumT AnyLiteral) -> DefT (r, NumT Truthy), true
  | DefT (r, MixedT _) -> DefT (r, MixedT Mixed_truthy), true

  (* truthy things pass through *)
  | t -> t, false

let not_exists t = match t with
  (* falsy things pass through *)
  | DefT (_, (
      NullT
    | VoidT
    | SingletonBoolT false
    | BoolT (Some false)
    | SingletonStrT ""
    | StrT (Literal (_, ""))
    | SingletonNumT (0., _)
    | NumT (Literal (_, (0., _)))
    )) -> t, false

  (* truthy things get removed *)
  | DefT (r, (
      SingletonBoolT _
    | BoolT (Some _)
    | SingletonStrT _
    | StrT (Literal _ | Truthy)
    | ArrT _
    | ObjT _
    | InstanceT _
    | AnyObjT
    | FunT _
    | AnyFunT
    | SingletonNumT _
    | NumT (Literal _ | Truthy)
    | MixedT Mixed_truthy
    )) -> DefT (r, EmptyT), true

  | DefT (reason, ClassT _) -> DefT (reason, EmptyT), true

  (* unknown boolies become falsy *)
  | DefT (r, MaybeT _) ->
    DefT (r, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) [])), true
  | DefT (r, BoolT None) -> DefT (r, BoolT (Some false)), true
  | DefT (r, StrT AnyLiteral) -> DefT (r, StrT (Literal (None, ""))), true
  | DefT (r, NumT AnyLiteral) -> DefT (r, NumT (Literal (None, (0., "0")))), true

  (* things that don't track truthiness pass through *)
  | t -> t, false

let maybe = function
  | DefT (r, MaybeT _) ->
    DefT (r, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) [])), true
  | DefT (r, MixedT Mixed_everything) ->
    DefT (r, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) [])), true
  | DefT (r, MixedT Mixed_truthy) -> EmptyT.why r, true
  | DefT (r, MixedT Mixed_non_maybe) -> EmptyT.why r, true
  | DefT (r, MixedT Mixed_non_void) -> DefT (r, NullT), true
  | DefT (r, MixedT Mixed_non_null) -> DefT (r, VoidT), true
  | DefT (_, NullT) as t -> t, false
  | DefT (_, VoidT) as t -> t, false
  | DefT (r, OptionalT _) -> VoidT.why r, true
  | DefT (_, AnyT) as t -> t, false
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason, true

let rec not_maybe = function
  | DefT (_, MaybeT t) -> t, true
  | DefT (_, OptionalT t) -> t |> not_maybe |> fst, true
  | DefT (r, (NullT | VoidT)) -> DefT (r, EmptyT), true
  | DefT (r, MixedT Mixed_truthy) -> DefT (r, MixedT Mixed_truthy), false
  | DefT (r, MixedT Mixed_non_maybe) -> DefT (r, MixedT Mixed_non_maybe), false
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_void)
  | DefT (r, MixedT Mixed_non_null)
    -> DefT (r, MixedT Mixed_non_maybe), true
  | t -> t, false

let null = function
  | DefT (_, OptionalT (DefT (r, MaybeT _)))
  | DefT (r, MaybeT _) -> NullT.why r, true
  | DefT (_, NullT) as t -> t, false
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_void) -> NullT.why r, true
  | DefT (_, AnyT) as t -> t, false
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason, true

let rec not_null = function
  | DefT (r, MaybeT t) ->
    DefT (r, UnionT (UnionRep.make (VoidT.why r) t [])), true
  | DefT (r, OptionalT t) ->
    let new_t, did_update = not_null t in
    DefT (r, OptionalT (new_t)), did_update
  | DefT (r, UnionT rep) ->
    recurse_into_union not_null (r, UnionRep.members rep)
  | DefT (r, NullT) -> DefT (r, EmptyT), true
  | DefT (r, MixedT Mixed_everything) -> DefT (r, MixedT Mixed_non_null), true
  | DefT (r, MixedT Mixed_non_void) -> DefT (r, MixedT Mixed_non_maybe), true
  | t -> t, false

let undefined = function
  | DefT (r, MaybeT _) -> VoidT.why r, true
  | DefT (_, VoidT) as t -> t, false
  | DefT (r, OptionalT _) -> VoidT.why r, true
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_null) -> VoidT.why r, true
  | DefT (_, AnyT) as t -> t, false
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason, true

let rec not_undefined = function
  | DefT (r, MaybeT t) ->
    DefT (r, UnionT (UnionRep.make (NullT.why r) t [])), true
  | DefT (_, OptionalT t) -> not_undefined t
  | DefT (r, UnionT rep) ->
    recurse_into_union not_undefined (r, UnionRep.members rep)
  | DefT (r, VoidT) -> DefT (r, EmptyT), true
  | DefT (r, MixedT Mixed_everything) -> DefT (r, MixedT Mixed_non_void), true
  | DefT (r, MixedT Mixed_non_null) -> DefT (r, MixedT Mixed_non_maybe), true
  | t -> t, false

let string_literal expected_loc sense expected t =
  let expected_desc = RStringLit expected in
  let lit_reason = replace_reason_const expected_desc in
  match t with
  | DefT (_, StrT (Literal (_, actual))) ->
    if actual = expected then t, false
    else DefT (mk_reason expected_desc expected_loc, StrT (Literal (Some sense, expected))), true
  | DefT (r, StrT Truthy) when expected <> "" ->
    DefT (lit_reason r, StrT (Literal (None, expected))), true
  | DefT (r, StrT AnyLiteral) ->
    DefT (lit_reason r, StrT (Literal (None, expected))), true
  | DefT (r, MixedT _) ->
    DefT (lit_reason r, StrT (Literal (None, expected))), true
  | DefT (_, AnyT) as t -> t, false
  | _ -> DefT (reason_of_t t, EmptyT), true

let not_string_literal expected = function
  | DefT (r, StrT (Literal (_, actual))) when actual = expected -> DefT (r, EmptyT), true
  | t -> t, false

let number_literal expected_loc sense expected t =
  let _, expected_raw = expected in
  let expected_desc = RNumberLit expected_raw in
  let lit_reason = replace_reason_const expected_desc in
  match t with
  | DefT (_, NumT (Literal (_, (_, actual_raw)))) ->
    if actual_raw = expected_raw then t, false
    else DefT (mk_reason expected_desc expected_loc, NumT (Literal (Some sense, expected))), true
  | DefT (r, NumT Truthy) when snd expected <> "0" ->
    DefT (lit_reason r, NumT (Literal (None, expected))), true
  | DefT (r, NumT AnyLiteral) ->
    DefT (lit_reason r, NumT (Literal (None, expected))), true
  | DefT (r, MixedT _) ->
    DefT (lit_reason r, NumT (Literal (None, expected))), true
  | DefT (_, AnyT) as t -> t, false
  | _ -> DefT (reason_of_t t, EmptyT), true

let not_number_literal expected = function
  | DefT (r, NumT (Literal (_, actual))) when snd actual = snd expected -> DefT (r, EmptyT), true
  | t -> t, false

let true_ t =
  let lit_reason = replace_reason_const (RBooleanLit true) in
  match t with
  | DefT (r, BoolT (Some true)) -> DefT (lit_reason r, BoolT (Some true)), false
  | DefT (r, BoolT None) -> DefT (lit_reason r, BoolT (Some true)), true
  | DefT (r, MixedT _) -> DefT (lit_reason r, BoolT (Some true)), true
  | DefT (_, AnyT) as t -> t, false
  | t -> DefT (reason_of_t t, EmptyT), true

let not_true t =
  let lit_reason = replace_reason_const (RBooleanLit false) in
  match t with
  | DefT (r, BoolT (Some true)) -> DefT (r, EmptyT), true
  | DefT (r, BoolT None) -> DefT (lit_reason r, BoolT (Some false)), true
  | t -> t, false

let false_ t =
  let lit_reason = replace_reason_const (RBooleanLit false) in
  match t with
  | DefT (r, BoolT (Some false)) -> DefT (lit_reason r, BoolT (Some false)), false
  | DefT (r, BoolT None) -> DefT (lit_reason r, BoolT (Some false)), true
  | DefT (r, MixedT _) -> DefT (lit_reason r, BoolT (Some false)), true
  | DefT (_, AnyT) as t -> t, false
  | t -> DefT (reason_of_t t, EmptyT), true

let not_false t =
  let lit_reason = replace_reason_const (RBooleanLit true) in
  match t with
  | DefT (r, BoolT (Some false)) -> DefT (r, EmptyT), true
  | DefT (r, BoolT None) -> DefT (lit_reason r, BoolT (Some true)), true
  | t -> t, false

let boolean t =
  match t with
  | DefT (r, MixedT Mixed_truthy) -> DefT (replace_reason_const BoolT.desc r, BoolT (Some true))
  | DefT (r, MixedT _) -> BoolT.why r
  | DefT (_, (AnyT | BoolT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_boolean t =
  match t with
  (* TODO: this is wrong, AnyT can be a bool *)
  | DefT (_, (AnyT | BoolT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t

let string t =
  match t with
  | DefT (r, MixedT Mixed_truthy) -> DefT (replace_reason_const StrT.desc r, StrT Truthy)
  | DefT (r, MixedT _) -> StrT.why r
  | DefT (_, (AnyT | StrT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_string t =
  match t with
  (* TODO: this is wrong, AnyT can be a string *)
  | DefT (_, (AnyT | StrT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t

let number t =
  match t with
  | DefT (r, MixedT Mixed_truthy) -> DefT (replace_reason_const NumT.desc r, NumT Truthy)
  | DefT (r, MixedT _) -> NumT.why r
  | DefT (_, (AnyT | NumT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_number t =
  match t with
  (* TODO: this is wrong, AnyT can be a number *)
  | DefT (_, (AnyT | NumT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t

let object_ cx t =
  match t with
  | DefT (r, MixedT flavor) ->
    let reason = replace_reason_const RObject r in
    let dict = Some {
      key = StrT.why r;
      value = DefT (replace_reason_const MixedT.desc r, MixedT Mixed_everything);
      dict_name = None;
      dict_polarity = Neutral;
    } in
    let proto = ObjProtoT reason in
    let obj = Obj_type.mk_with_proto cx reason ?dict proto in
    begin match flavor with
    | Mixed_truthy
    | Mixed_non_maybe
    | Mixed_non_null -> obj
    | Mixed_everything
    | Mixed_non_void ->
      let reason = replace_reason_const RUnion (reason_of_t t) in
      DefT (reason, UnionT (UnionRep.make (NullT.why r) obj []))
    | Empty_intersection -> DefT (r, EmptyT)
    end
  | DefT (_, (AnyT | AnyObjT | ObjT _ | ArrT _ | NullT | InstanceT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_object t =
  match t with
  | DefT (_, (AnyT | AnyObjT | ObjT _ | ArrT _ | NullT | InstanceT _)) ->
    DefT (reason_of_t t, EmptyT)
  | _ -> t

let function_ t =
  match t with
  | DefT (r, MixedT _) ->
    let desc = RFunction RNormal in
    DefT (replace_reason_const desc r, AnyFunT)
  | DefT (_, (AnyT | AnyFunT | FunT _ | ClassT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_function t =
  match t with
  | DefT (_, (AnyT | AnyFunT | FunT _ | ClassT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t

let array t =
  match t with
  | DefT (r, MixedT _) ->
    DefT (replace_reason_const RArray r,
      ArrT (ArrayAT (DefT (r, MixedT Mixed_everything), None))
    )
  | DefT (_, (AnyT | ArrT _)) ->
    t
  | _ ->
    DefT (reason_of_t t, EmptyT)

let not_array t =
  match t with
  | DefT (_, (AnyT | ArrT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t
