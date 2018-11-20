(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type

let recurse_into_union filter_fn ((r, ts): reason * Type.t list) =
  let new_ts = List.fold_left (fun new_ts t ->
    match filter_fn t with
    | DefT (_, EmptyT) -> new_ts
    | filtered_type -> filtered_type::new_ts
  ) [] ts in
  let new_ts = List.rev new_ts in
  match new_ts with
  | [] -> DefT (r, EmptyT)
  | [t] -> t
  | t0::t1::ts -> DefT (r, UnionT (UnionRep.make t0 t1 ts))

let recurse_into_intersection =
  let rec helper filter_fn r acc = function
  | [] -> List.rev acc
  | t::ts ->
    begin match filter_fn t with
    | DefT (_, EmptyT) -> []
    | filtered_type -> helper filter_fn r (filtered_type::acc) ts
    end
  in
  fun filter_fn ((r, ts): reason * Type.t list) ->
    match helper filter_fn r [] ts with
    | [] -> DefT (r, EmptyT)
    | [t] -> t
    | t0::t1::ts -> DefT (r, IntersectionT (InterRep.make t0 t1 ts))

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
    )) -> DefT (r, EmptyT)

  (* unknown things become truthy *)
  | DefT (_, MaybeT t) -> t
  | DefT (_, OptionalT t) -> exists t
  | DefT (r, BoolT None) -> DefT (r, BoolT (Some true))
  | DefT (r, StrT AnyLiteral) -> DefT (r, StrT Truthy)
  | DefT (r, NumT AnyLiteral) -> DefT (r, NumT Truthy)
  | DefT (r, MixedT _) -> DefT (r, MixedT Mixed_truthy)

  (* an intersection passes through iff all of its members pass through *)
  | DefT (r, IntersectionT rep) ->
    recurse_into_intersection exists (r, InterRep.members rep)

  (* truthy things pass through *)
  | t -> t

let rec not_exists t = match t with
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
    )) -> t

  (* truthy things get removed *)
  | DefT (r, (
      SingletonBoolT _
    | BoolT (Some _)
    | SingletonStrT _
    | StrT (Literal _ | Truthy)
    | ArrT _
    | ObjT _
    | InstanceT _
    | AnyT _
    | FunT _
    | SingletonNumT _
    | NumT (Literal _ | Truthy)
    | MixedT Mixed_truthy
    )) -> DefT (r, EmptyT)

  | DefT (reason, ClassT _) -> DefT (reason, EmptyT)

  (* unknown boolies become falsy *)
  | DefT (r, MaybeT _) ->
    DefT (r, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) []))
  | DefT (r, BoolT None) -> DefT (r, BoolT (Some false))
  | DefT (r, StrT AnyLiteral) -> DefT (r, StrT (Literal (None, "")))
  | DefT (r, NumT AnyLiteral) -> DefT (r, NumT (Literal (None, (0., "0"))))

  (* an intersection passes through iff all of its members pass through *)
  | DefT (r, IntersectionT rep) ->
    recurse_into_intersection not_exists (r, InterRep.members rep)

  (* things that don't track truthiness pass through *)
  | t -> t

let maybe = function
  | DefT (r, MaybeT _) ->
    DefT (r, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) []))
  | DefT (r, MixedT Mixed_everything) ->
    DefT (r, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) []))
  | DefT (r, MixedT Mixed_truthy) -> EmptyT.why r
  | DefT (r, MixedT Mixed_non_maybe) -> EmptyT.why r
  | DefT (r, MixedT Mixed_non_void) -> DefT (r, NullT)
  | DefT (r, MixedT Mixed_non_null) -> DefT (r, VoidT)
  | DefT (_, NullT) as t -> t
  | DefT (_, VoidT) as t -> t
  | DefT (r, OptionalT _) -> VoidT.why r
  | DefT (_, AnyT _) as t -> t
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

let rec not_maybe = function
  | DefT (_, MaybeT t) -> t
  | DefT (_, OptionalT t) -> not_maybe t
  | DefT (r, (NullT | VoidT)) -> DefT (r, EmptyT)
  | DefT (r, MixedT Mixed_truthy) -> DefT (r, MixedT Mixed_truthy)
  | DefT (r, MixedT Mixed_non_maybe) -> DefT (r, MixedT Mixed_non_maybe)
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_void)
  | DefT (r, MixedT Mixed_non_null)
    -> DefT (r, MixedT Mixed_non_maybe)
  | t -> t

let null = function
  | DefT (_, OptionalT (DefT (r, MaybeT _)))
  | DefT (r, MaybeT _) -> NullT.why r
  | DefT (_, NullT) as t -> t
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_void) -> NullT.why r
  | DefT (_, AnyT _) as t -> t
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

let rec not_null = function
  | DefT (r, MaybeT t) ->
    DefT (r, UnionT (UnionRep.make (VoidT.why r) t []))
  | DefT (r, OptionalT t) ->
    DefT (r, OptionalT (not_null t))
  | DefT (r, UnionT rep) ->
    recurse_into_union not_null (r, UnionRep.members rep)
  | DefT (r, NullT) -> DefT (r, EmptyT)
  | DefT (r, MixedT Mixed_everything) -> DefT (r, MixedT Mixed_non_null)
  | DefT (r, MixedT Mixed_non_void) -> DefT (r, MixedT Mixed_non_maybe)
  | t -> t

let undefined = function
  | DefT (r, MaybeT _) -> VoidT.why r
  | DefT (_, VoidT) as t -> t
  | DefT (r, OptionalT _) -> VoidT.why r
  | DefT (r, MixedT Mixed_everything)
  | DefT (r, MixedT Mixed_non_null) -> VoidT.why r
  | DefT (_, AnyT _) as t -> t
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

let rec not_undefined = function
  | DefT (r, MaybeT t) ->
    DefT (r, UnionT (UnionRep.make (NullT.why r) t []))
  | DefT (_, OptionalT t) -> not_undefined t
  | DefT (r, UnionT rep) ->
    recurse_into_union not_undefined (r, UnionRep.members rep)
  | DefT (r, VoidT) -> DefT (r, EmptyT)
  | DefT (r, MixedT Mixed_everything) -> DefT (r, MixedT Mixed_non_void)
  | DefT (r, MixedT Mixed_non_null) -> DefT (r, MixedT Mixed_non_maybe)
  | t -> t

let string_literal expected_loc sense expected t =
  let expected_desc = RStringLit expected in
  let lit_reason = replace_reason_const expected_desc in
  match t with
  | DefT (_, StrT (Literal (_, actual))) ->
    if actual = expected then t
    else DefT (mk_reason expected_desc expected_loc, StrT (Literal (Some sense, expected)))
  | DefT (r, StrT Truthy) when expected <> "" ->
    DefT (lit_reason r, StrT (Literal (None, expected)))
  | DefT (r, StrT AnyLiteral) ->
    DefT (lit_reason r, StrT (Literal (None, expected)))
  | DefT (r, MixedT _) ->
    DefT (lit_reason r, StrT (Literal (None, expected)))
  | DefT (_, AnyT _) as t -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_string_literal expected = function
  | DefT (r, StrT (Literal (_, actual))) when actual = expected -> DefT (r, EmptyT)
  | t -> t

let number_literal expected_loc sense expected t =
  let _, expected_raw = expected in
  let expected_desc = RNumberLit expected_raw in
  let lit_reason = replace_reason_const expected_desc in
  match t with
  | DefT (_, NumT (Literal (_, (_, actual_raw)))) ->
    if actual_raw = expected_raw then t
    else DefT (mk_reason expected_desc expected_loc, NumT (Literal (Some sense, expected)))
  | DefT (r, NumT Truthy) when snd expected <> "0" ->
    DefT (lit_reason r, NumT (Literal (None, expected)))
  | DefT (r, NumT AnyLiteral) ->
    DefT (lit_reason r, NumT (Literal (None, expected)))
  | DefT (r, MixedT _) ->
    DefT (lit_reason r, NumT (Literal (None, expected)))
  | DefT (_, AnyT _) as t -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_number_literal expected = function
  | DefT (r, NumT (Literal (_, actual))) when snd actual = snd expected -> DefT (r, EmptyT)
  | t -> t

let true_ t =
  let lit_reason = replace_reason_const (RBooleanLit true) in
  match t with
  | DefT (r, BoolT (Some true)) -> DefT (lit_reason r, BoolT (Some true))
  | DefT (r, BoolT None) -> DefT (lit_reason r, BoolT (Some true))
  | DefT (r, MixedT _) -> DefT (lit_reason r, BoolT (Some true))
  | DefT (_, AnyT _) as t -> t
  | t -> DefT (reason_of_t t, EmptyT)

let not_true t =
  let lit_reason = replace_reason_const (RBooleanLit false) in
  match t with
  | DefT (r, BoolT (Some true)) -> DefT (r, EmptyT)
  | DefT (r, BoolT None) -> DefT (lit_reason r, BoolT (Some false))
  | t -> t

let false_ t =
  let lit_reason = replace_reason_const (RBooleanLit false) in
  match t with
  | DefT (r, BoolT (Some false)) -> DefT (lit_reason r, BoolT (Some false))
  | DefT (r, BoolT None) -> DefT (lit_reason r, BoolT (Some false))
  | DefT (r, MixedT _) -> DefT (lit_reason r, BoolT (Some false))
  | DefT (_, AnyT _) as t -> t
  | t -> DefT (reason_of_t t, EmptyT)

let not_false t =
  let lit_reason = replace_reason_const (RBooleanLit true) in
  match t with
  | DefT (r, BoolT (Some false)) -> DefT (r, EmptyT)
  | DefT (r, BoolT None) -> DefT (lit_reason r, BoolT (Some true))
  | t -> t

let boolean t =
  match t with
  | DefT (r, MixedT Mixed_truthy) -> DefT (replace_reason_const BoolT.desc r, BoolT (Some true))
  | DefT (r, MixedT _) -> BoolT.why r
  | DefT (_, (AnyT _ | BoolT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_boolean t =
  match t with
  (* TODO: this is wrong, AnyT can be a bool *)
  | DefT (_, (AnyT _ | BoolT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t

let string t =
  match t with
  | DefT (r, MixedT Mixed_truthy) -> DefT (replace_reason_const StrT.desc r, StrT Truthy)
  | DefT (r, MixedT _) -> StrT.why r
  | DefT (_, (AnyT _ | StrT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_string t =
  match t with
  (* TODO: this is wrong, AnyT can be a string *)
  | DefT (_, (AnyT _ | StrT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t

let number t =
  match t with
  | DefT (r, MixedT Mixed_truthy) -> DefT (replace_reason_const NumT.desc r, NumT Truthy)
  | DefT (r, MixedT _) -> NumT.why r
  | DefT (_, (AnyT _ | NumT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_number t =
  match t with
  (* TODO: this is wrong, AnyT can be a number *)
  | DefT (_, (AnyT _ | NumT _)) -> DefT (reason_of_t t, EmptyT)
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
  | DefT (_, (AnyT _ | ObjT _ | ArrT _ | NullT | InstanceT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_object t =
  match t with
  | DefT (_, (AnyT _ | ObjT _ | ArrT _ | NullT | InstanceT _)) ->
    DefT (reason_of_t t, EmptyT)
  | _ -> t

let function_ t =
  match t with
  | DefT (r, MixedT _) ->
    let desc = RFunction RNormal in
    replace_reason_const desc r |> AnyT.make Unsound
  | DefT (_, (AnyT _ | FunT _ | ClassT _)) -> t
  | _ -> DefT (reason_of_t t, EmptyT)

let not_function t =
  match t with
  | DefT (_, (AnyT _ | FunT _ | ClassT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t

let array t =
  match t with
  | DefT (r, MixedT _) ->
    DefT (replace_reason_const RArray r,
      ArrT (ArrayAT (DefT (r, MixedT Mixed_everything), None))
    )
  | DefT (_, (AnyT _ | ArrT _)) ->
    t
  | _ ->
    DefT (reason_of_t t, EmptyT)

let not_array t =
  match t with
  | DefT (_, (AnyT _ | ArrT _)) -> DefT (reason_of_t t, EmptyT)
  | _ -> t
