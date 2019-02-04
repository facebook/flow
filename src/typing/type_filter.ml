(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type

let recurse_into_union filter_fn ((r, ts): reason * Type.t list) =
  let new_ts = List.fold_left (fun new_ts t ->
    match filter_fn t with
    | DefT (_, _, EmptyT) -> new_ts
    | filtered_type -> filtered_type::new_ts
  ) [] ts in
  let new_ts = List.rev new_ts in
  match new_ts with
  | [] -> DefT (r, bogus_trust (), EmptyT)
  | [t] -> t
  | t0::t1::ts -> DefT (r, bogus_trust (), UnionT (UnionRep.make t0 t1 ts))

let recurse_into_intersection =
  let rec helper filter_fn r acc = function
  | [] -> List.rev acc
  | t::ts ->
    begin match filter_fn t with
    | DefT (_, _, EmptyT) -> []
    | filtered_type -> helper filter_fn r (filtered_type::acc) ts
    end
  in
  fun filter_fn ((r, ts): reason * Type.t list) ->
    match helper filter_fn r [] ts with
    | [] -> DefT (r, bogus_trust (), EmptyT)
    | [t] -> t
    | t0::t1::ts -> DefT (r, bogus_trust (), IntersectionT (InterRep.make t0 t1 ts))

let rec exists = function
  (* falsy things get removed *)
  | DefT (r, trust, (
      NullT
    | VoidT
    | SingletonBoolT false
    | BoolT (Some false)
    | SingletonStrT ""
    | StrT (Literal (_, ""))
    | SingletonNumT (0., _)
    | NumT (Literal (_, (0., _)))
    )) -> DefT (r, trust, EmptyT)

  (* unknown things become truthy *)
  | DefT (_, _, MaybeT t) -> t
  | DefT (_, _, OptionalT t) -> exists t
  | DefT (r, trust, BoolT None) -> DefT (r, trust, BoolT (Some true))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (r, trust, StrT Truthy)
  | DefT (r, trust, NumT AnyLiteral) -> DefT (r, trust, NumT Truthy)
  | DefT (r, trust, MixedT _) -> DefT (r, trust, MixedT Mixed_truthy)

  (* an intersection passes through iff all of its members pass through *)
  | DefT (r, _, IntersectionT rep) ->
    recurse_into_intersection exists (r, InterRep.members rep)

  (* truthy things pass through *)
  | t -> t

let rec not_exists t = match t with
  (* falsy things pass through *)
  | DefT (_, _, (
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
  | DefT (r, trust, (
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
    )) -> DefT (r, trust, EmptyT)

  | DefT (reason, trust, ClassT _) -> DefT (reason, trust, EmptyT)

  (* unknown boolies become falsy *)
  | DefT (r, trust, MaybeT _) ->
    DefT (r, trust, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) []))
  | DefT (r, trust, BoolT None) -> DefT (r, trust, BoolT (Some false))
  | DefT (r, trust, StrT AnyLiteral) -> DefT (r, trust, StrT (Literal (None, "")))
  | DefT (r, trust, NumT AnyLiteral) -> DefT (r, trust, NumT (Literal (None, (0., "0"))))

  (* an intersection passes through iff all of its members pass through *)
  | DefT (r, _, IntersectionT rep) ->
    recurse_into_intersection not_exists (r, InterRep.members rep)

  (* things that don't track truthiness pass through *)
  | t -> t

let maybe = function
  | DefT (r, trust, MaybeT _) ->
    DefT (r, trust, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) []))
  | DefT (r, trust, MixedT Mixed_everything) ->
    DefT (r, trust, UnionT (UnionRep.make (NullT.why r) (VoidT.why r) []))
  | DefT (r, _, MixedT Mixed_truthy) -> EmptyT.why r
  | DefT (r, _, MixedT Mixed_non_maybe) -> EmptyT.why r
  | DefT (r, trust, MixedT Mixed_non_void) -> DefT (r, trust, NullT)
  | DefT (r, trust, MixedT Mixed_non_null) -> DefT (r, trust, VoidT)
  | DefT (_, _, NullT) as t -> t
  | DefT (_, _, VoidT) as t -> t
  | DefT (r, _, OptionalT _) -> VoidT.why r
  | DefT (_, _, AnyT _) as t -> t
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

let rec not_maybe = function
  | DefT (_, _, MaybeT t) -> t
  | DefT (_, _, OptionalT t) -> not_maybe t
  | DefT (r, trust, (NullT | VoidT)) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, MixedT Mixed_truthy) -> DefT (r, trust, MixedT Mixed_truthy)
  | DefT (r, trust, MixedT Mixed_non_maybe) -> DefT (r, trust, MixedT Mixed_non_maybe)
  | DefT (r, trust, MixedT Mixed_everything)
  | DefT (r, trust, MixedT Mixed_non_void)
  | DefT (r, trust, MixedT Mixed_non_null)
    -> DefT (r, trust, MixedT Mixed_non_maybe)
  | t -> t

let null = function
  | DefT (_, _, OptionalT (DefT (r, _, MaybeT _)))
  | DefT (r, _, MaybeT _) -> NullT.why r
  | DefT (_, _, NullT) as t -> t
  | DefT (r, _, MixedT Mixed_everything)
  | DefT (r, _, MixedT Mixed_non_void) -> NullT.why r
  | DefT (_, _, AnyT _) as t -> t
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

let rec not_null = function
  | DefT (r, trust, MaybeT t) ->
    DefT (r, trust, UnionT (UnionRep.make (VoidT.why r) t []))
  | DefT (r, trust, OptionalT t) ->
    DefT (r, trust, OptionalT (not_null t))
  | DefT (r, _, UnionT rep) ->
    recurse_into_union not_null (r, UnionRep.members rep)
  | DefT (r, trust, NullT) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, MixedT Mixed_everything) -> DefT (r, trust, MixedT Mixed_non_null)
  | DefT (r, trust, MixedT Mixed_non_void) -> DefT (r, trust, MixedT Mixed_non_maybe)
  | t -> t

let undefined = function
  | DefT (r, _, MaybeT _) -> VoidT.why r
  | DefT (_, _, VoidT) as t -> t
  | DefT (r, _, OptionalT _) -> VoidT.why r
  | DefT (r, _, MixedT Mixed_everything)
  | DefT (r, _, MixedT Mixed_non_null) -> VoidT.why r
  | DefT (_, _, AnyT _) as t -> t
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

let rec not_undefined = function
  | DefT (r, trust, MaybeT t) ->
    DefT (r, trust, UnionT (UnionRep.make (NullT.why r) t []))
  | DefT (_, _, OptionalT t) -> not_undefined t
  | DefT (r, _, UnionT rep) ->
    recurse_into_union not_undefined (r, UnionRep.members rep)
  | DefT (r, trust, VoidT) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, MixedT Mixed_everything) -> DefT (r, trust, MixedT Mixed_non_void)
  | DefT (r, trust, MixedT Mixed_non_null) -> DefT (r, trust, MixedT Mixed_non_maybe)
  | t -> t

let string_literal expected_loc sense expected t =
  let expected_desc = RStringLit expected in
  let lit_reason = replace_reason_const expected_desc in
  match t with
  | DefT (_, trust, StrT (Literal (_, actual))) ->
    if actual = expected then t
    else DefT (mk_reason expected_desc expected_loc, trust, StrT (Literal (Some sense, expected)))
  | DefT (r, trust, StrT Truthy) when expected <> "" ->
    DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | DefT (r, trust, StrT AnyLiteral) ->
    DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | DefT (r, trust, MixedT _) ->
    DefT (lit_reason r, trust, StrT (Literal (None, expected)))
  | DefT (_, _, AnyT _) as t -> t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_string_literal expected = function
  | DefT (r, trust, StrT (Literal (_, actual))) when actual = expected -> DefT (r, trust, EmptyT)
  | t -> t

let number_literal expected_loc sense expected t =
  let _, expected_raw = expected in
  let expected_desc = RNumberLit expected_raw in
  let lit_reason = replace_reason_const expected_desc in
  match t with
  | DefT (_, trust, NumT (Literal (_, (_, actual_raw)))) ->
    if actual_raw = expected_raw then t
    else DefT (mk_reason expected_desc expected_loc, trust, NumT (Literal (Some sense, expected)))
  | DefT (r, trust, NumT Truthy) when snd expected <> "0" ->
    DefT (lit_reason r, trust, NumT (Literal (None, expected)))
  | DefT (r, trust, NumT AnyLiteral) ->
    DefT (lit_reason r, trust, NumT (Literal (None, expected)))
  | DefT (r, trust, MixedT _) ->
    DefT (lit_reason r, trust, NumT (Literal (None, expected)))
  | DefT (_, _, AnyT _) as t -> t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_number_literal expected = function
  | DefT (r, trust, NumT (Literal (_, actual))) when snd actual = snd expected -> DefT (r, trust, EmptyT)
  | t -> t

let true_ t =
  let lit_reason = replace_reason_const (RBooleanLit true) in
  match t with
  | DefT (r, trust, BoolT (Some true)) -> DefT (lit_reason r, trust, BoolT (Some true))
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some true))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, BoolT (Some true))
  | DefT (_, _, AnyT _) as t -> t
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_true t =
  let lit_reason = replace_reason_const (RBooleanLit false) in
  match t with
  | DefT (r, trust, BoolT (Some true)) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some false))
  | t -> t

let false_ t =
  let lit_reason = replace_reason_const (RBooleanLit false) in
  match t with
  | DefT (r, trust, BoolT (Some false)) -> DefT (lit_reason r, trust, BoolT (Some false))
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some false))
  | DefT (r, trust, MixedT _) -> DefT (lit_reason r, trust, BoolT (Some false))
  | DefT (_, _, AnyT _) as t -> t
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_false t =
  let lit_reason = replace_reason_const (RBooleanLit true) in
  match t with
  | DefT (r, trust, BoolT (Some false)) -> DefT (r, trust, EmptyT)
  | DefT (r, trust, BoolT None) -> DefT (lit_reason r, trust, BoolT (Some true))
  | t -> t

let boolean t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) -> DefT (replace_reason_const BoolT.desc r, trust, BoolT (Some true))
  | DefT (r, _, MixedT _) -> BoolT.why r
  | DefT (_, _, (AnyT _ | BoolT _)) -> t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_boolean t =
  match t with
  (* TODO: this is wrong, AnyT can be a bool *)
  | DefT (_, trust, (AnyT _ | BoolT _)) -> DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let string t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) -> DefT (replace_reason_const StrT.desc r, trust, StrT Truthy)
  | DefT (r, _, MixedT _) -> StrT.why r
  | DefT (_, _, (AnyT _ | StrT _)) -> t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_string t =
  match t with
  (* TODO: this is wrong, AnyT can be a string *)
  | DefT (_, trust, (AnyT _ | StrT _)) -> DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let number t =
  match t with
  | DefT (r, trust, MixedT Mixed_truthy) -> DefT (replace_reason_const NumT.desc r, trust, NumT Truthy)
  | DefT (r, _, MixedT _) -> NumT.why r
  | DefT (_, _, (AnyT _ | NumT _)) -> t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_number t =
  match t with
  (* TODO: this is wrong, AnyT can be a number *)
  | DefT (_, trust, (AnyT _ | NumT _)) -> DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let object_ cx t =
  match t with
  | DefT (r, trust, MixedT flavor) ->
    let reason = replace_reason_const RObject r in
    let dict = Some {
      key = StrT.why r;
      value = DefT (replace_reason_const MixedT.desc r, bogus_trust (), MixedT Mixed_everything);
      dict_name = None;
      dict_polarity = Neutral;
    } in
    let proto = ObjProtoT reason in
    let obj = Obj_type.mk_with_proto cx reason ?dict proto in
    begin match flavor with
    | Mixed_truthy
    | Mixed_non_maybe
    | Mixed_non_null -> obj
    | Mixed_function
    | Mixed_everything
    | Mixed_non_void ->
      let reason = replace_reason_const RUnion (reason_of_t t) in
      DefT (reason, trust, UnionT (UnionRep.make (NullT.why r) obj []))
    | Empty_intersection -> DefT (r, trust, EmptyT)
    end
  | DefT (_, _, (AnyT _ | ObjT _ | ArrT _ | NullT | InstanceT _)) -> t
  | _ -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_object t =
  match t with
  | DefT (_, trust, (AnyT _ | ObjT _ | ArrT _ | NullT | InstanceT _)) ->
    DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let function_ = function
  | DefT (r, trust, MixedT _) ->
      DefT (replace_reason_const (RFunction RUnknown) r, trust, MixedT Mixed_function)
  | DefT (_, _, (AnyT _ | FunT _ | ClassT _)) as t -> t
  | t -> DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_function t =
  match t with
  | DefT (_, trust, (AnyT _ | FunT _ | ClassT _)) -> DefT (reason_of_t t, trust, EmptyT)
  | _ -> t

let array t =
  match t with
  | DefT (r, trust, MixedT _) ->
    DefT (replace_reason_const RArray r, trust,
      ArrT (ArrayAT (DefT (r, trust, MixedT Mixed_everything), None))
    )
  | DefT (_, _, (AnyT _ | ArrT _)) ->
    t
  | _ ->
    DefT (reason_of_t t, bogus_trust (), EmptyT)

let not_array t =
  match t with
  | DefT (_, trust, (AnyT _ | ArrT _)) -> DefT (reason_of_t t, trust, EmptyT)
  | _ -> t
