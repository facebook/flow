(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil
module U = Utils_js

(* Don't use Flow_js directly from here *)
module Flow_js = struct end

let mk_object_type
    cx
    ~reason
    ?(wrap_on_exact_obj = false)
    ~invalidate_aliases
    ~interface
    ~reachable_targs
    ~kind
    flags
    call
    id
    proto
    generics =
  let reason =
    if invalidate_aliases then
      update_desc_reason invalidate_rtype_alias reason
    else
      reason
  in
  let t =
    match interface with
    | Some (static, inst) ->
      let inst_dict =
        match flags.obj_kind with
        | Indexed dict -> Some dict
        | _ -> None
      in
      let inst = { inst with own_props = id; inst_dict } in
      (* Implemented/super interfaces are folded into the property map computed by the slice, so
           we effectively flatten the hierarchy in the output *)
      DefT (reason, InstanceT { static; super = ObjProtoT reason; implements = []; inst })
    | None ->
      let t = DefT (reason, ObjT (mk_objecttype ~reachable_targs ~flags ~call id proto)) in
      if wrap_on_exact_obj && Obj_type.is_exact flags.obj_kind then
        if Flow_js_utils.TvarVisitors.has_unresolved_tvars_or_placeholders cx t then
          t
        else
          Tvar.mk_fully_resolved cx (TypeUtil.reason_of_t t) t
      else
        t
  in
  Generic.make_op_id kind generics
  |> Base.Option.value_map ~default:t ~f:(fun id ->
         GenericT { bound = t; reason; id; name = Generic.subst_name_of_id id; no_infer = false }
     )

let type_optionality_and_missing_property { Object.prop_t; _ } =
  match prop_t with
  | OptionalT { reason; type_ = t; use_desc } ->
    let is_missing_property_desc =
      match desc_of_reason reason with
      | RPossiblyMissingPropFromObj _ -> true
      | _ -> false
    in
    (t, true, use_desc && is_missing_property_desc)
  | _ -> (prop_t, false, false)

(* Widening may create optional props because a property may not exist on some object. This
 * synthetic property lacks a good location in the code, since it represents the absence
 * of a key in some object. In order to point to a good location, we point to the object
 * missing the property in the reason desc. *)
let possibly_missing_prop propname obj_reason type_ =
  let reason =
    update_desc_new_reason (fun desc -> RPossiblyMissingPropFromObj (propname, desc)) obj_reason
  in
  OptionalT { reason; type_; use_desc = true }

(* When a property is optional due to widening (see possibly_missing_prop), we want to make sure
 * that the reason it is missing persists through interactions with other optional properties.
 *
 * This function preserves the possibly missing prop reason if it existed on both
 * of the optional properties. Otherwise, we have an explicit optional property, which
 * is better to use.
 *)
let make_optional_with_possible_missing_props propname missing_prop1 missing_prop2 r =
  if missing_prop1 && missing_prop2 then
    possibly_missing_prop propname r
  else
    TypeUtil.optional ?annot_loc:None ~use_desc:false

let merge_dro a b =
  match (a, b) with
  | (Some x, Some _) -> Some x
  | _ -> None

(*******************************)
(* Shared Object Kit Utilities *)
(*******************************)

let read_prop r flags x p =
  let is_method =
    match p with
    | Method _ -> true
    | _ -> false
  in
  let t =
    match Property.read_t p with
    | Some t -> t
    | None ->
      let reason = replace_desc_reason (RUnknownProperty (Some x)) r in
      let t = DefT (reason, MixedT Mixed_everything) in
      t
  in
  {
    Object.prop_t = t;
    is_own = Obj_type.is_exact flags.obj_kind;
    is_method;
    polarity = Property.polarity p;
    key_loc = Property.first_loc p;
  }

let read_dict r { value; dict_polarity; _ } =
  if Polarity.compat (dict_polarity, Polarity.Positive) then
    value
  else
    let reason = replace_desc_reason (RUnknownProperty None) r in
    DefT (reason, MixedT Mixed_everything)

let object_slice cx ~interface r id flags frozen reachable_targs generics =
  let props = Context.find_props cx id in
  let props = NameUtils.Map.mapi (read_prop r flags) props in
  let obj_kind =
    Obj_type.map_dict
      (fun dict ->
        {
          dict_name = None;
          key = dict.key;
          value = read_dict r dict;
          dict_polarity = Polarity.Neutral;
        })
      flags.obj_kind
  in
  let flags = { flags with obj_kind } in
  { Object.reason = r; props; flags; frozen; generics; interface; reachable_targs }

(* Treat dictionaries as optional, own properties. Dictionary reads should
 * be exact. TODO: Forbid writes to indexers through the photo chain.
 * Property accesses which read from dictionaries normally result in a
 * non-optional result, but that leads to confusing spread results. For
 * example, `p` in `{...{|p:T|},...{[]:U}` should `T|U`, not `U`. *)
let get_prop r p dict =
  match (p, dict) with
  | (Some _, _) -> p
  | (None, Some d) ->
    Some
      {
        Object.prop_t = TypeUtil.optional (read_dict r d);
        is_own = true;
        is_method = false;
        polarity = Polarity.Neutral;
        key_loc = Some (loc_of_reason (reason_of_t d.key));
      }
  | (None, None) -> None

(* Lift a pairwise function to a function over a resolved list *)
let merge (f : Object.slice -> Object.slice -> Object.slice) =
  let f' (x0 : Object.resolved) (x1 : Object.resolved) =
    Nel.map_concat (fun slice1 -> Nel.map (f slice1) x0) x1
  in
  let rec loop x0 = function
    | [] -> x0
    | x1 :: xs -> loop (f' x0 x1) xs
  in
  (fun x0 (x1, xs) -> loop (f' x0 x1) xs)

(* Lift a pairwise function that may return an error to a function over a resolved list
 * that may return an error, like spread2 *)
let merge_result (f : 'a -> 'a -> ('a, Error_message.t) result) (conv : 'b -> 'a Nel.t) =
  let bind f = function
    | Ok data -> f data
    | Error e -> Error e
  in
  let mapM f = function
    | Ok data -> Ok (f data)
    | Error e -> Error e
  in
  let f' (x0 : 'a Nel.t) (x1 : 'b) : ('a Nel.t, Error_message.t) result =
    let x1 = conv x1 in
    let resolved_list =
      Nel.fold_left
        (fun acc slice1 ->
          bind
            (fun acc ->
              let resolved =
                Nel.fold_left
                  (fun acc x ->
                    bind (fun acc -> bind (fun slice -> Ok (slice :: acc)) (f x slice1)) acc)
                  (Ok [])
                  x0
              in
              let resolved = resolved |> mapM List.rev in
              bind (fun resolved -> Ok (resolved :: acc)) resolved)
            acc)
        (Ok [])
        x1
    in
    let resolved_list = resolved_list |> mapM List.rev in
    (* Each of the lists were non-empty, so concatenating them creates a non-empty list. Thus,
     * Nel.of_list_exn is safe *)
    bind (fun lists -> Ok (Nel.of_list_exn (Base.List.join lists))) resolved_list
  in
  let rec loop (x0 : 'a Nel.t) (xs : 'b list) =
    match xs with
    | [] -> Ok x0
    | x1 :: xs -> bind (fun resolved -> loop resolved xs) (f' x0 x1)
  in
  (fun x0 (x1, xs) -> bind (fun resolved -> loop resolved xs) (f' (conv x0) x1))

(*****************)
(* Object Spread *)
(*****************)

exception CannotSpreadError of Error_message.t

(* Compute spread result
 * We keep around a few pieces of extra information in addition to the slices:
 * 1. Whether or not the slice was declared in line
 * 2. The most recent reason that the accumulator (object on the left) is inexact
 *
 * 1. Lets us infer types in spreads that are more inline with user expectations
 * by allowing inline declarations to overwrite properties on the left that
 * would cause an error if they were not inline. For example:
 * {...{| foo: number |}}, [string]: string}. If we did not account
 * for inline declarations, this spread would be an error even though there
 * is a reasonable way to interpret the type.
 *
 * 2. Lets us write better error messages when we spread an object with an optional
 * property after an inexact object. The accumulator may be inexact because we encountered
 * an inexact object earlier in the spread, but the reason for the accumulator refers to
 * the entire spread. Instead of pointing to that reason as the inexact object, we
 * can keep track of the most recently seen inexact object and point to that instead.
 *
 * We still have room for improvement for the error messages here. When we spread
 * an inexact object on the right, we may overwrite keys that came before, so
 * we error. Unfortunately, we point to the accumulator (the entire sperad) and say that
 * a key in the accumulator may be overwritten by the inexact object spread. That
 * is a little confusing when you have types like these:
 *
 * type A = {| foo: number |};
 * type B = {| bar: number |};
 * type C = {baz: number};
 * [1] type D = {...A, ...B, ...C} // Error, C is inexact and may overwrite foo in [1]
 *              ^^^^^^^^^^^^^^^^^
 *)
let spread2
    ~dict_check
    cx
    ~use_op
    reason
    ( _inline1,
      inexact_reason1,
      {
        Object.reason = r1;
        props = props1;
        flags = flags1;
        frozen = frozen1;
        generics = generics1;
        interface = _;
        reachable_targs = targs1;
      }
    )
    ( inline2,
      _inexact_reason2,
      {
        Object.reason = r2;
        props = props2;
        flags = flags2;
        frozen = frozen2;
        generics = generics2;
        interface = _;
        reachable_targs = targs2;
      }
    ) =
  let exact1 = Obj_type.is_exact flags1.obj_kind in
  let exact2 = Obj_type.is_exact flags2.obj_kind in
  let dict1 = Obj_type.get_dict_opt flags1.obj_kind in
  let dict2 = Obj_type.get_dict_opt flags2.obj_kind in
  let dict =
    match (dict1, dict2) with
    | (None, Some _) when inline2 -> Ok dict2
    | (None, Some _) when NameUtils.Map.is_empty props1 && exact1 -> Ok dict2
    | (Some d1, Some d2) when NameUtils.Map.is_empty props1 ->
      dict_check cx use_op d1 d2;
      (* We take dict1 because we want to use the key from d1 *)
      Ok dict1
    | (_, Some { key; value = _; dict_name = _; dict_polarity = _ }) ->
      Error
        (Error_message.ECannotSpreadIndexerOnRight
           { spread_reason = reason; object_reason = r2; key_reason = reason_of_t key; use_op }
        )
    | (Some { key; value; dict_name = _; dict_polarity = _ }, _) when not (exact2 || inline2) ->
      Error
        (Error_message.EInexactMayOverwriteIndexer
           {
             spread_reason = reason;
             key_reason = reason_of_t key;
             value_reason = reason_of_t value;
             object2_reason = r2;
             use_op;
           }
        )
    | _ -> Ok dict1
  in
  match dict with
  | Error e -> Error e
  | Ok dict ->
    let union t1 t2 = UnionT (reason, UnionRep.make t1 t2 []) in
    let merge_props
        propname
        ({ Object.is_method = method1; _ } as t1)
        ({ Object.is_method = method2; key_loc = kl2; _ } as t2) =
      let (t1, opt1, missing_prop1) = type_optionality_and_missing_property t1 in
      let (t2, opt2, missing_prop2) = type_optionality_and_missing_property t2 in
      if not opt2 then
        {
          Object.prop_t = t2;
          is_own = true;
          is_method = method2;
          polarity = Polarity.Neutral;
          key_loc = kl2;
        }
      else if opt1 && opt2 then
        let prop_t =
          make_optional_with_possible_missing_props
            propname
            missing_prop1
            missing_prop2
            r1
            (union t1 t2)
        in
        (* Since we cannot be sure which is spread, if either
         * are methods we must treat the result as a method *)
        let is_method = method2 || method1 in
        { Object.prop_t; is_own = true; is_method; polarity = Polarity.Neutral; key_loc = None }
      (* In this case, we know opt2 is true and opt1 is false *)
      else
        {
          Object.prop_t = union t1 t2;
          is_own = true;
          is_method = method1 || method2;
          polarity = Polarity.Neutral;
          key_loc = None;
        }
    in
    let props =
      try
        Ok
          (NameUtils.Map.merge
             (fun x p1 p2 ->
               match (p1, p2) with
               | (None, None) -> None
               | (_, Some { Object.prop_t = p2; is_method; is_own = _; polarity; key_loc })
                 when inline2 ->
                 Some { Object.prop_t = p2; is_own = true; is_method; polarity; key_loc }
               | (Some p1, Some p2) -> Some (merge_props x p1 p2)
               | (Some { Object.prop_t = p1; is_method; is_own = _; polarity; key_loc }, None) ->
                 if exact2 || inline2 then
                   Some { Object.prop_t = p1; is_own = true; is_method; polarity; key_loc }
                 else
                   raise
                     (CannotSpreadError
                        (Error_message.EUnableToSpread
                           {
                             spread_reason = reason;
                             object1_reason = r1;
                             object2_reason = r2;
                             propname = x;
                             error_kind = Flow_intermediate_error_types.UnexpectedInexact;
                             use_op;
                           }
                        )
                     )
               (* We care about a few cases here. We want to make sure that we can
                * infer a precise type. This is tricky when the left-hand slice is inexact,
                * since it may contain p2 even though it's not explicitly specified.
                *
                * If p2 is not optional, then we won't have to worry about anything because it will
                * definitely overwrite a property with a key matching p2's on the left.
                *
                * If p2 is optional, then we can split into a few more cases:
                *   1. o1 is inexact: error, we cannot infer a precise type since o1 might contain p2
                *   2. o1 has an indexer: error, we would have to infer a union with the indexer type.
                *      This would be sound, but it's not likely that anyone would intend it. If that
                *      assumption turns out to be false, we can easily add support for it later.
                *   3. o1 is exact: no problem, we don't need to worry about o1 having the
                *      same property.
                *
                *  The if statement below handles 1. and 2., and the else statement
                *  handles 3. and the case when p2 is not optional.
                *)
               | (None, Some ({ Object.prop_t = t; is_method; is_own = _; polarity; key_loc } as p2))
                 ->
                 let (_, opt2, _) = type_optionality_and_missing_property p2 in
                 (match flags1.obj_kind with
                 | Indexed _
                 | Inexact
                   when opt2 ->
                   let error_kind =
                     if dict1 <> None then
                       Flow_intermediate_error_types.UnexpectedIndexer
                     else
                       Flow_intermediate_error_types.UnexpectedInexact
                   in
                   let inexact_reason =
                     match inexact_reason1 with
                     | None -> r1
                     | Some r -> r
                   in
                   raise
                     (CannotSpreadError
                        (Error_message.EUnableToSpread
                           {
                             spread_reason = reason;
                             (* in this case, the object on the left is inexact. the error will say
                              * that object2_reason is inexact and may contain propname, so
                              * we should assign r2 to object1_reason and inexact_reason to object2_reason *)
                             object1_reason = r2;
                             object2_reason = inexact_reason;
                             propname = x;
                             error_kind;
                             use_op;
                           }
                        )
                     )
                 | _ -> Some { Object.prop_t = t; is_own = true; is_method; polarity; key_loc }))
             props1
             props2
          )
      with
      | CannotSpreadError e -> Error e
    in
    let obj_kind =
      match dict with
      | Some d -> Indexed d
      | None ->
        if Obj_type.is_exact flags1.obj_kind && Obj_type.is_exact flags2.obj_kind then
          Exact
        else
          Inexact
    in
    let flags = { obj_kind; react_dro = merge_dro flags1.react_dro flags2.react_dro } in
    let frozen = frozen1 && frozen2 in
    let generics = Generic.spread_append generics1 generics2 in
    let reachable_targs = targs1 @ targs2 in
    let inexact_reason =
      match (exact1, exact2) with
      (* If the inexact reason is None, that means we still haven't hit an inexact object yet, so we can
       * take that reason to propagate as the reason for the accumulator's inexactness.
       *
       * If it's already Some r, then the reason the object on the left is inexact
       * is because of an earlier inexact object. We would have already encountered that inexact
       * object on the right in a previous iteration of spread2, so the next case in the
       * match would have already updated the inexact reason to the most recent inexact object.
       * The only exception to this rule is if the first object is inexact, in which case
       * inexact_reason1 is already None anyway.
       *)
      | (false, true) when inexact_reason1 = None -> Some r1
      | (_, false) -> Some r2
      | _ -> inexact_reason1
    in
    (match props with
    | Ok props ->
      Ok
        ( false,
          inexact_reason,
          { Object.reason; props; flags; frozen; generics; interface = None; reachable_targs }
        )
    | Error e -> Error e)

let spread =
  let resolved_of_acc_element = function
    | Object.Spread.ResolvedSlice resolved -> Nel.map (fun x -> (false, None, x)) resolved
    | Object.Spread.InlineSlice { Object.Spread.reason; prop_map; dict; generics; reachable_targs }
      ->
      let obj_kind =
        match dict with
        | Some d -> Indexed d
        | None -> Exact
      in
      let flags = { obj_kind; react_dro = None } in
      let props = NameUtils.Map.mapi (read_prop reason flags) prop_map in
      Nel.one
        ( true,
          None,
          {
            Object.reason;
            props;
            flags;
            frozen = false;
            generics;
            interface = None;
            reachable_targs;
          }
        )
  in

  fun ~dict_check cx ~use_op reason nel ->
    match nel with
    | (x, []) -> Ok (resolved_of_acc_element x)
    | (x0, (x1 :: xs : Object.Spread.acc_element list)) ->
      merge_result (spread2 ~dict_check cx ~use_op reason) resolved_of_acc_element x0 (x1, xs)

let spread_mk_object
    cx
    reason
    target
    { Object.reason = _; props; flags; frozen = _; generics; interface = _; reachable_targs } =
  let open Object.Spread in
  let mk_dro t =
    match flags.react_dro with
    | Some l -> EvalT (t, TypeDestructorT (unknown_use, reason, ReactDRO l), Eval.generate_id ())
    | None -> t
  in
  let (obj_kind, as_const, frozen_seal) =
    let (exact, sealed) =
      match target with
      (* Type spread result is exact if annotated to be exact *)
      | Annot { make_exact } -> (make_exact, Sealed)
      (* Value spread result is exact if all inputs are exact *)
      | Value { make_seal } -> (Obj_type.is_exact flags.obj_kind, make_seal)
    in
    let as_const = sealed = Object.Spread.As_Const in
    let dict =
      Obj_type.get_dict_opt flags.obj_kind
      |> Base.Option.map ~f:(fun d ->
             { d with dict_polarity = Polarity.apply_const as_const d.dict_polarity }
         )
    in
    let obj_kind =
      match (exact, sealed, dict) with
      | (_, _, Some d) -> Indexed d
      | (true, _, _) -> Exact
      | _ -> Inexact
    in
    let frozen_seal = sealed = Object.Spread.Frozen in
    (obj_kind, as_const, frozen_seal)
  in
  let flags = { obj_kind; react_dro = None } in
  let positive_polarity = as_const || frozen_seal in
  let props =
    NameUtils.Map.map
      (fun { Object.prop_t; is_method; is_own = _; polarity = _; key_loc } ->
        if is_method then
          Method { key_loc; type_ = mk_dro prop_t }
        else
          Field
            {
              preferred_def_locs = None;
              key_loc;
              type_ = mk_dro prop_t;
              polarity = Polarity.object_literal_polarity positive_polarity;
            })
      props
  in
  let id = Context.generate_property_map cx props in
  let proto = ObjProtoT reason in
  let call = None in
  mk_object_type
    cx
    ~reason
    ~wrap_on_exact_obj:true
    ~invalidate_aliases:true
    ~interface:None
    ~reachable_targs
    ~kind:Subst_name.Spread
    flags
    call
    id
    proto
    generics

let object_spread
    (type a)
    ~dict_check
    ~add_output
    ~(return : _ -> _ -> Type.t -> a)
    ~(recurse : _ -> Type.use_op -> Reason.t -> Object.resolve_tool -> Object.tool -> _ -> a)
    options
    state
    cx
    use_op
    reason
    x =
  Object.Spread.(
    let reason = update_desc_reason invalidate_rtype_alias reason in
    let { todo_rev; acc; spread_id; union_reason; curr_resolve_idx } = state in
    Nel.iter
      (fun {
             Object.reason = r;
             props = _;
             flags = { obj_kind; _ };
             frozen = _;
             generics = _;
             interface = _;
             reachable_targs = _;
           } ->
        match options with
        | Annot { make_exact } when make_exact && obj_kind = Inexact ->
          add_output
            cx
            (Error_message.EIncompatibleWithExact
               ((r, reason), use_op, Flow_intermediate_error_types.UnexpectedInexact)
            )
        | _ -> ())
      x;
    let resolved = Object.Spread.ResolvedSlice x in
    let rec continue acc (resolved : Object.Spread.acc_element) curr_resolve_idx = function
      | [] ->
        let t =
          match spread ~dict_check cx ~use_op reason (resolved, acc) with
          | Ok ((_, _, x), []) -> spread_mk_object cx reason options x
          | Ok ((_, _, x0), (_, _, x1) :: xs) ->
            let xs = Base.List.map ~f:(fun (_, _, x) -> x) xs in
            UnionT
              ( reason,
                UnionRep.make
                  (spread_mk_object cx reason options x0)
                  (spread_mk_object cx reason options x1)
                  (Base.List.map ~f:(spread_mk_object cx reason options) xs)
              )
          | Error e ->
            add_output cx e;
            AnyT.error reason
        in
        return cx use_op t
      | Type t :: todo_rev ->
        let resolve_tool = Object.Resolve Object.Next in
        let state =
          { todo_rev; acc = resolved :: acc; spread_id; union_reason = None; curr_resolve_idx }
        in
        let tool = Object.Spread (options, state) in
        recurse cx use_op reason resolve_tool tool t
      | Slice operand_slice :: todo_rev ->
        let acc = resolved :: acc in
        continue acc (InlineSlice operand_slice) (curr_resolve_idx + 1) todo_rev
    in
    (* Before proceeding to the next spread step, we need to ensure that we aren't going to hit
     * exponential blowup due to multiple spread operands having multiple lower bounds. To do
     * that, we increment the amount of lower bounds found at this resolution index by
     * the amount of lower bounds found at this index. If multiple indices have multiple lower
     * bounds, Spread_cache.can_spread will return false and we can error instead of proceeding.
     *
     * Any other latent constraints involving this spread_id will also stop when they hit this
     * logic.
     *)
    let cache = Context.spread_cache cx in
    let prev_can_spread = Spread_cache.can_spread cache spread_id in
    if not prev_can_spread then
      return cx use_op (AnyT.error reason)
    else (
      begin
        match (union_reason, x) with
        | (None, x) ->
          Nel.iter
            (fun ({ Object.reason; _ } as slice) ->
              Spread_cache.add_lower_bound cache spread_id curr_resolve_idx reason (Nel.one slice))
            x
        | (Some reason, _) -> Spread_cache.add_lower_bound cache spread_id curr_resolve_idx reason x
      end;
      let can_spread = Spread_cache.can_spread cache spread_id in
      if prev_can_spread && not can_spread then (
        let (reasons_for_operand1, reasons_for_operand2) =
          Spread_cache.get_error_groups cache spread_id
        in
        add_output
          cx
          (Error_message.EExponentialSpread { reason; reasons_for_operand1; reasons_for_operand2 });
        return cx use_op (AnyT.error reason)
      ) else
        continue acc resolved (curr_resolve_idx + 1) todo_rev
    )
  )

(*****************************)
(* Component prop extraction *)
(*****************************)

let check_config2
    cx pmap { Object.reason; props; flags; frozen = _; generics; interface = _; reachable_targs } =
  let dict = Obj_type.get_dict_opt flags.obj_kind in
  let ((duplicate_props_in_spread, ref_prop_in_spread), props) =
    NameUtils.Map.merge_env
      ~combine:(fun (duplicate_props_in_spread, ref_prop_in_spread) x p1 p2 ->
        match (x, p1, p2) with
        | (Reason.OrdinaryName "ref", Some _, _) ->
          failwith "Ref should have been extracted elsewhere"
        | (_, Some p1, Some { Object.key_loc = key_loc2; prop_t; _ }) ->
          let first =
            Type.Property.first_loc p1 |> Base.Option.value ~default:(loc_of_reason reason)
          in
          let second = Base.Option.value ~default:(reason_of_t prop_t |> loc_of_reason) key_loc2 in
          let p1 = read_prop reason flags x p1 in
          (((first, x, second) :: duplicate_props_in_spread, ref_prop_in_spread), Some p1)
        | (Reason.OrdinaryName "ref", None, Some { Object.key_loc; prop_t; _ }) ->
          let loc = Base.Option.value ~default:(reason_of_t prop_t |> loc_of_reason) key_loc in
          ((duplicate_props_in_spread, Some loc), None)
        | (_, Some p1, None) ->
          let p1 = read_prop reason flags x p1 in
          ((duplicate_props_in_spread, ref_prop_in_spread), Some p1)
        | (_, None, p2) -> ((duplicate_props_in_spread, ref_prop_in_spread), p2))
      ([], None)
      pmap
      props
  in
  let obj_kind =
    match dict with
    | Some d -> Indexed d
    | None ->
      if Obj_type.is_exact flags.obj_kind then
        Exact
      else
        Inexact
  in
  let flags = { obj_kind; react_dro = flags.react_dro } in
  let props =
    NameUtils.Map.map
      (fun { Object.prop_t; is_method; is_own = _; polarity = _; key_loc } ->
        if is_method then
          Method { key_loc; type_ = prop_t }
        else
          Field { preferred_def_locs = None; key_loc; type_ = prop_t; polarity = Polarity.Positive })
      props
  in
  let id = Context.generate_property_map cx props in
  let proto = ObjProtoT reason in
  let call = None in
  let t =
    mk_object_type
      cx
      ~reason
      ~invalidate_aliases:true
      ~interface:None
      ~reachable_targs
      ~kind:Subst_name.CheckConfig
      flags
      call
      id
      proto
      generics
  in
  (t, List.rev duplicate_props_in_spread, ref_prop_in_spread)

let check_component_config
    (type a) ~add_output ~(return : _ -> _ -> Type.t -> a) pmap cx use_op reason x =
  let xs =
    Nel.map
      (fun xelt ->
        let (o, duplicate_props_in_spread, ref_prop_in_spread) = check_config2 cx pmap xelt in
        let { Object.reason; _ } = xelt in
        Base.Option.iter (Nel.of_list duplicate_props_in_spread) ~f:(fun duplicates ->
            add_output
              cx
              (Error_message.EDuplicateComponentProp { spread = loc_of_reason reason; duplicates })
        );
        Base.Option.iter ref_prop_in_spread ~f:(fun loc ->
            add_output cx (Error_message.ERefComponentProp { spread = loc_of_reason reason; loc })
        );
        o)
      x
  in
  let t =
    match xs with
    | (x, []) -> x
    | (x0, x1 :: xs) -> UnionT (reason, UnionRep.make x0 x1 xs)
  in
  return cx use_op t

(***************)
(* Object Rest *)
(***************)

let object_rest
    (type a)
    ~add_output
    ~(return : _ -> _ -> _ -> Type.t -> a)
    ~(recurse : _ -> _ -> _ -> _ -> _ -> Type.t -> a)
    ~subt_check
    options
    state
    cx
    use_op
    reason
    x =
  let open Object.Rest in
  let optional = function
    | OptionalT _ as t -> t
    | t -> TypeUtil.optional t
  in
  (* Subtract the second slice from the first slice and return the difference
   * slice. The runtime implementation of this type operation is:
   *
   *     const result = {};
   *
   *     for (const p in props1) {
   *       if (hasOwnProperty(props1, p)) {
   *         if (!hasOwnProperty(props2, p)) {
   *           result[p] = props1[p];
   *         }
   *       }
   *     }
   *
   * The resulting object only has a property if the property is own in props1 and
   * it is not an own property of props2.
   *)
  let rest
      cx
      ~use_op
      merge_mode
      {
        Object.reason = r1;
        props = props1;
        flags = flags1;
        frozen = _;
        generics = generics1;
        interface;
        reachable_targs;
      }
      {
        Object.reason = r2;
        props = props2;
        flags = flags2;
        frozen = _;
        generics = generics2;
        interface = _;
        reachable_targs = _;
      } =
    let dict1 = Obj_type.get_dict_opt flags1.obj_kind in
    let dict2 = Obj_type.get_dict_opt flags2.obj_kind in
    let props =
      NameUtils.Map.merge
        (fun k p1 p2 ->
          match
            ( merge_mode,
              get_prop r1 p1 dict1,
              get_prop r2 p2 dict2,
              Obj_type.is_exact flags2.obj_kind
            )
          with
          (* If neither object has the prop then we don't add a prop to our
           * result here. *)
          | ((SpreadReversal | Omit | ReactConfigMerge _), None, None, _) -> None
          | ( (SpreadReversal | ReactConfigMerge _),
              Some { Object.prop_t = t; is_method; is_own = _; polarity = _; key_loc },
              None,
              _
            ) ->
            let p =
              if is_method then
                Method { key_loc; type_ = t }
              else
                Field
                  { preferred_def_locs = None; key_loc; type_ = t; polarity = Polarity.Positive }
            in
            Some p
          | (Omit, Some { Object.prop_t = t; is_method; is_own = _; polarity; key_loc }, None, _) ->
            let p =
              if is_method then
                Method { key_loc; type_ = t }
              else
                Field { preferred_def_locs = None; key_loc; type_ = t; polarity }
            in
            Some p
          | ((SpreadReversal | Omit), Some _, Some _, _) -> None
          (* React config merging is special. We are trying to solve for C
           * in the equation (where ... represents spread instead of rest):
           *
           *     {...DP, ...C} = P
           *
           * Where DP and P are known. Consider this case:
           *
           *     {...{p?}, ...C} = {p}
           *
           * The solution for C here is {p} instead of {p?} since
           * {...{p?}, ...{p?}} is {p?} instead of {p}. This is inconsistent with
           * the behavior of other object rest merge modes implemented in this
           * pattern match. *)
          | ( ReactConfigMerge _,
              Some { Object.prop_t = t1; is_method; is_own = _; polarity = _; key_loc },
              Some { Object.prop_t = OptionalT { reason = _; type_ = t2; use_desc = _ }; _ },
              _
            ) ->
            (* We only test the subtyping relation of t1 and t2 if both t1 and t2
             * are optional types. If t1 is required then t2 will always
             * be overwritten. *)
            (match t1 with
            | OptionalT { reason = _; type_ = t1; use_desc = _ } ->
              subt_check ~use_op:unknown_use cx (t2, t1)
            | _ -> ());
            let p =
              if is_method then
                Method { key_loc; type_ = t1 }
              else
                Field
                  { preferred_def_locs = None; key_loc; type_ = t1; polarity = Polarity.Neutral }
            in
            Some p
          (* Using our same equation. Consider this case:
           *
           *     {...{p}, ...C} = {p}
           *
           * The solution for C here is {p?}. An empty object, {}, is not a valid
           * solution unless that empty object is exact. Even for exact objects,
           * {|p?|} is the best solution since it accepts more valid
           * programs then {||}. *)
          | ( ReactConfigMerge _,
              Some { Object.prop_t = t1; is_method; is_own = _; polarity = _; key_loc },
              Some { Object.prop_t = t2; _ },
              _
            ) ->
            (* The DP type for p must be a subtype of the P type for p. *)
            subt_check ~use_op:unknown_use cx (t2, t1);
            let p =
              if is_method then
                Method { key_loc; type_ = optional t1 }
              else
                Field
                  {
                    preferred_def_locs = None;
                    key_loc;
                    type_ = optional t1;
                    polarity = Polarity.Positive;
                  }
            in
            Some p
          | ((SpreadReversal | Omit), None, _, _) -> None
          (* Consider this case:
           *
           *     {...{p}, ...C} = {}
           *
           * For C there will be no prop. However, if the props object is exact
           * then we need to throw an error. *)
          | (ReactConfigMerge _, None, Some _, _) ->
            ( if Obj_type.is_exact flags1.obj_kind then
              let use_op =
                Frame (PropertyCompatibility { prop = Some k; lower = r2; upper = r1 }, unknown_use)
              in
              let r2 = replace_desc_reason (RProperty (Some k)) r2 in
              let err =
                Error_message.EPropNotFound
                  {
                    prop_name = Some k;
                    reason_prop = r2;
                    reason_obj = r1;
                    use_op;
                    suggestion = None;
                  }
              in
              add_output cx err
            );
            None)
        props1
        props2
    in
    let dict =
      match (dict1, dict2) with
      | (None, None) -> None
      | (Some dict, None) -> Some dict
      | (None, Some _) -> None
      (* If our first and second objects have a dictionary then we use our first
       * dictionary, but we make the value optional since any set of keys may have
       * been removed. *)
      | (Some dict1, Some dict2) ->
        subt_check ~use_op cx (dict1.value, dict2.value);
        Some
          {
            dict_name = None;
            key = dict1.key;
            value = optional dict1.value;
            dict_polarity = Polarity.Neutral;
          }
    in
    let obj_kind =
      match (flags1.obj_kind, dict, merge_mode) with
      | (Exact, _, _) -> Exact
      | (Indexed _, Some d, _) -> Indexed d
      | (Inexact, _, Omit) -> Exact
      | _ -> Inexact
    in
    let flags = { obj_kind; react_dro = flags1.react_dro } in
    let generics = Generic.spread_subtract generics1 generics2 in
    let id = Context.generate_property_map cx props in
    let proto = ObjProtoT r1 in
    let call = None in
    let (reason, interface) =
      match merge_mode with
      | Omit -> (reason, interface)
      | _ -> (r1, None)
    in
    mk_object_type
      cx
      ~reason
      ~invalidate_aliases:true
      ~interface
        (* Keep the reachable targs from o1, because we don't know whether all appearences of them were removed *)
      ~reachable_targs
      ~kind:Subst_name.Spread
      flags
      call
      id
      proto
      generics
  in
  match state with
  | One t ->
    let resolve_tool = Object.Resolve Object.Next in
    let state = Done x in
    let tool = Object.Rest (options, state) in
    recurse cx use_op reason resolve_tool tool t
  | Done base ->
    let xs = Nel.map_concat (fun slice -> Nel.map (rest cx ~use_op options slice) x) base in
    let t =
      match xs with
      | (x, []) -> x
      | (x0, x1 :: xs) -> UnionT (reason, UnionRep.make x0 x1 xs)
    in
    let use_op p = Frame (ReactGetConfig { polarity = p }, use_op) in
    return cx use_op options t

(********************)
(* Object Make Exact *)
(********************)
let object_make_exact =
  let mk_exact_object cx reason slice =
    let { Object.reason = r; props; flags; frozen = _; generics; interface; reachable_targs } =
      slice
    in
    match interface with
    | Some _ ->
      Flow_js_utils.add_output cx (Error_message.EUnsupportedExact (reason, r));
      AnyT.error reason
    | None ->
      let props =
        NameUtils.Map.map
          (fun { Object.prop_t; is_method; is_own = _; polarity; key_loc } ->
            if is_method then
              Method { key_loc; type_ = prop_t }
            else
              Field { preferred_def_locs = None; key_loc; type_ = prop_t; polarity })
          props
      in
      (* This case analysis aims at recovering a potential type alias associated
       * with an $Exact<> constructor. *)
      let reason_obj =
        match desc_of_reason ~unwrap:false reason with
        | RTypeAlias (n, loc, _) ->
          update_desc_reason
            (function
              | RTypeAlias (_, _, desc) -> RTypeAlias (n, loc, desc)
              | desc -> RTypeAlias (n, loc, desc))
            r
        | _ ->
          (* If [r] is an RTypeAlias, then this alias is no longer valid. *)
          update_desc_reason invalidate_rtype_alias r
      in
      let flags =
        {
          flags with
          obj_kind =
            (match flags.obj_kind with
            | Inexact -> Exact
            | k -> k);
        }
      in
      let call = None in
      let id = Context.generate_property_map cx props in
      let proto = ObjProtoT reason in
      mk_object_type
        cx
        ~reason:reason_obj
        ~invalidate_aliases:true
        ~interface:None
        ~reachable_targs
        ~kind:Subst_name.MakeExact
        flags
        call
        id
        proto
        generics
  in
  fun cx reason x ->
    match Nel.map (mk_exact_object cx reason) x with
    | (t, []) -> t
    | (t0, t1 :: ts) -> UnionT (reason, UnionRep.make t0 t1 ts)

(********************)
(* Object Read Only *)
(********************)
let object_read_only =
  let polarity = Polarity.Positive in
  let mk_read_only_object cx reason slice =
    let { Object.reason = r; props; flags; frozen = _; generics; interface; reachable_targs } =
      slice
    in
    let props =
      NameUtils.Map.map
        (fun { Object.prop_t; is_method; is_own = _; polarity = _; key_loc } ->
          if is_method then
            Method { key_loc; type_ = prop_t }
          else
            Field { preferred_def_locs = None; key_loc; type_ = prop_t; polarity })
        props
    in
    let flags =
      {
        flags with
        obj_kind =
          Obj_type.map_dict (fun dict -> { dict with dict_polarity = polarity }) flags.obj_kind;
      }
    in
    let call = None in
    let id = Context.generate_property_map cx props in
    let proto = ObjProtoT reason in
    (* Avoid referring directly to the $ReadOnly keyword *)
    let reason =
      match desc_of_reason ~unwrap:false reason with
      | RReadOnlyType -> r
      | _ -> reason
    in
    mk_object_type
      cx
      ~reason
      ~invalidate_aliases:true
      ~interface
      ~reachable_targs
      ~kind:Subst_name.ReadOnly
      flags
      call
      id
      proto
      generics
  in
  fun cx reason x ->
    match Nel.map (mk_read_only_object cx reason) x with
    | (t, []) -> t
    | (t0, t1 :: ts) -> UnionT (reason, UnionRep.make t0 t1 ts)

let object_update_optionality kind =
  let mk_object cx reason slice =
    let { Object.reason = r; props; frozen; flags; generics; interface; reachable_targs } = slice in
    let props =
      NameUtils.Map.map
        (fun { Object.prop_t; is_method; is_own = _; polarity; key_loc } ->
          let polarity = U.ite frozen Polarity.Positive polarity in
          if is_method then
            Method { key_loc; type_ = prop_t }
          else
            match (prop_t, kind) with
            | (OptionalT _, `Partial) ->
              Field { preferred_def_locs = None; key_loc; type_ = prop_t; polarity }
            | (_, `Partial) ->
              Field
                {
                  preferred_def_locs = None;
                  key_loc;
                  type_ =
                    OptionalT { reason = reason_of_t prop_t; type_ = prop_t; use_desc = false };
                  polarity;
                }
            | (OptionalT { type_; _ }, `Required) ->
              Field { preferred_def_locs = None; key_loc; type_; polarity }
            | (_, `Required) ->
              Field { preferred_def_locs = None; key_loc; type_ = prop_t; polarity })
        props
    in
    let call = None in
    let id = Context.generate_property_map cx props in
    let proto = ObjProtoT reason in
    let reason =
      match (desc_of_reason ~unwrap:false reason, kind) with
      | (RPartialOf _, `Partial) -> r
      | (RRequiredOf _, `Required) -> r
      | _ -> reason
    in
    let kind =
      match kind with
      | `Partial -> Subst_name.Partial
      | `Required -> Subst_name.Required
    in
    mk_object_type
      cx
      ~reason
      ~invalidate_aliases:true
      ~interface
      ~reachable_targs
      ~kind
      flags
      call
      id
      proto
      generics
  in
  fun cx reason x ->
    match Nel.map (mk_object cx reason) x with
    | (t, []) -> t
    | (t0, t1 :: ts) -> UnionT (reason, UnionRep.make t0 t1 ts)

(* Intersect two object slices: slice * slice -> slice
 *
 * In general it is unsound to combine intersection types, but since object
 * kit utilities never write to their arguments, it is safe in this specific
 * case.
 *
 * {...{p:T}&{q:U}} = {...{p:T,q:U}}
 * {...{p:T}&{p:U}} = {...{p:T&U}}
 * {...A&(B|C)} = {...(A&B)|(A&C)}
 * {...(A|B)&C} = {...(A&C)|(B&C)}
 *)
let intersect2
    cx
    reason
    {
      Object.reason = r1;
      props = props1;
      flags = flags1;
      frozen = frozen1;
      generics = generics1;
      interface = _;
      reachable_targs = targs1;
    }
    {
      Object.reason = r2;
      props = props2;
      flags = flags2;
      frozen = frozen2;
      generics = generics2;
      interface = _;
      reachable_targs = targs2;
    } =
  let dict1 = Obj_type.get_dict_opt flags1.obj_kind in
  let dict2 = Obj_type.get_dict_opt flags2.obj_kind in
  let intersection t1 t2 =
    if Concrete_type_eq.eq cx t1 t2 then
      t1
    else
      IntersectionT (reason, InterRep.make t1 t2 [])
  in
  let merge_props
      { Object.prop_t = t1; is_own = own1; is_method = m1; polarity = p1; key_loc }
      { Object.prop_t = t2; is_own = own2; is_method = m2; polarity = p2; key_loc = _ } =
    let (t1, t2, opt) =
      match (t1, t2) with
      | ( OptionalT { reason = _; type_ = t1; use_desc = _ },
          OptionalT { reason = _; type_ = t2; use_desc = _ }
        ) ->
        (t1, t2, true)
      | (OptionalT { reason = _; type_ = t1; use_desc = _ }, t2)
      | (t1, OptionalT { reason = _; type_ = t2; use_desc = _ })
      | (t1, t2) ->
        (t1, t2, false)
    in
    let t = intersection t1 t2 in
    let t =
      if opt then
        optional t
      else
        t
    in
    {
      Object.prop_t = t;
      is_own = own1 || own2;
      is_method = m1 || m2;
      polarity = Polarity.mult (p1, p2);
      key_loc;
    }
  in
  let props =
    NameUtils.Map.merge
      (fun _ p1 p2 ->
        let read_dict r d =
          {
            Object.prop_t = optional (read_dict r d);
            is_own = true;
            is_method = false;
            polarity = Polarity.Neutral;
            key_loc = Some (loc_of_reason (reason_of_t d.key));
          }
        in
        match (p1, p2) with
        | (None, None) -> None
        | (Some p1, Some p2) -> Some (merge_props p1 p2)
        | (Some p1, None) ->
          (match dict2 with
          | Some d2 -> Some (merge_props p1 (read_dict r2 d2))
          | None -> Some p1)
        | (None, Some p2) ->
          (match dict1 with
          | Some d1 -> Some (merge_props (read_dict r1 d1) p2)
          | None -> Some p2))
      props1
      props2
  in
  let dict =
    Base.Option.merge dict1 dict2 ~f:(fun d1 d2 ->
        {
          dict_name = None;
          key = intersection d1.key d2.key;
          value = intersection (read_dict r1 d1) (read_dict r2 d2);
          dict_polarity = Polarity.Neutral;
        }
    )
  in
  let obj_kind =
    Obj_type.obj_kind_from_optional_dict
      ~dict
      ~otherwise:
        ( if
          (* TODO(jmbrown): Audit this condition. Should this be a conjunction? *)
          Obj_type.is_exact flags1.obj_kind || Obj_type.is_exact flags2.obj_kind
        then
          Exact
        else
          Inexact
        )
  in
  let flags = { obj_kind; react_dro = Base.Option.first_some flags1.react_dro flags2.react_dro } in
  let frozen = frozen1 || frozen2 in
  let generics = Generic.spread_append generics1 generics2 in
  let reachable_targs = targs1 @ targs2 in
  (props, flags, frozen, generics, reachable_targs)

let intersect2_with_reason cx reason intersection_loc x1 x2 =
  let (props, flags, frozen, generics, reachable_targs) = intersect2 cx reason x1 x2 in
  let reason = mk_reason RObjectType intersection_loc in
  { Object.reason; props; flags; frozen; generics; interface = None; reachable_targs }

let resolved ~next ~recurse cx use_op reason resolve_tool tool x =
  Object.(
    match resolve_tool with
    | Next -> next cx use_op tool reason x
    | List0 ((t, todo), join) ->
      let resolve_tool = Resolve (List (todo, Nel.one x, join)) in
      recurse cx use_op reason resolve_tool tool t
    | List (todo, done_rev, join) ->
      (match todo with
      | [] ->
        let x =
          match join with
          | (_, Or) -> Nel.cons x done_rev |> Nel.concat
          | (loc, And) -> merge (intersect2_with_reason cx reason loc) x done_rev
        in
        next cx use_op tool reason x
      | t :: todo ->
        let done_rev = Nel.cons x done_rev in
        let resolve_tool = Resolve (List (todo, done_rev, join)) in
        recurse cx use_op reason resolve_tool tool t)
  )

let interface_slice cx r ~static ~inst id generics =
  let obj_kind =
    match inst.inst_dict with
    | Some dict -> Indexed dict
    | None -> Inexact
  in
  let flags = { obj_kind; react_dro = None } in
  object_slice cx ~interface:(Some (static, inst)) r id flags false [] generics

let resolve
    (type a)
    ~add_output
    ~(return : _ -> Type.use_op -> Type.t -> a)
    ~(next : _ -> Type.use_op -> Object.tool -> Reason.t -> Object.slice Nel.t -> a)
    ~(recurse : _ -> _ -> _ -> Object.resolve_tool -> Object.tool -> Type.t -> a)
    ~(statics : _ -> _ -> Type.t -> Type.t)
    cx
    use_op
    reason
    (resolve_tool : Object.resolve)
    (tool : Object.tool)
    t : a =
  let open Object in
  let (t_generic_id, t) =
    let rec loop t ls =
      match t with
      | GenericT { id; bound; reason; _ } ->
        loop
          (mod_reason_of_t (fun _ -> reason) bound)
          (Generic.spread_append (Generic.make_spread id) ls)
      | ThisInstanceT (r, i, is_this, this_name) ->
        (ls, Flow_js_utils.fix_this_instance cx r (r, i, is_this, this_name))
      | _ -> (ls, t)
    in
    loop t Generic.spread_empty
  in
  match t with
  (* We extract the props from an ObjT. *)
  | DefT (r, ObjT { props_tmap; Type.flags; reachable_targs; _ }) ->
    let x =
      Nel.one (object_slice cx ~interface:None r props_tmap flags false reachable_targs t_generic_id)
    in
    resolved ~next ~recurse cx use_op reason resolve_tool tool x
  (* We take the fields from an InstanceT excluding methods (because methods
   * are always on the prototype). We also want to resolve fields from the
   * InstanceT's super class so we recurse. *)
  | DefT (r, InstanceT { static; super; implements = _; inst = { own_props; inst_kind; _ } as inst })
    ->
    let resolve_tool =
      Super (interface_slice cx r ~static ~inst own_props t_generic_id, resolve_tool)
    in
    begin
      match (tool, inst_kind) with
      | (Spread _, InterfaceKind _) ->
        add_output
          cx
          (Error_message.ECannotSpreadInterface
             { spread_reason = reason; interface_reason = r; use_op }
          );
        return cx use_op (AnyT.error reason)
      | _ -> recurse cx use_op reason resolve_tool tool super
    end
  (* Statics of a class. TODO: This logic is unfortunately duplicated from the
   * top-level pattern matching against class lower bounds to object-like
   * uses. This duplication should be removed. *)
  | DefT (r, ClassT i) -> recurse cx use_op reason (Resolve resolve_tool) tool (statics cx r i)
  (* Resolve each member of a union. *)
  | UnionT (union_reason, rep) ->
    let union_loc = loc_of_reason union_reason in
    let members_filtered = Type_mapper.union_flatten cx (UnionRep.members rep) in
    let tool =
      match tool with
      | Spread (options, state) ->
        Spread (options, { state with Spread.union_reason = Some union_reason })
      | _ -> tool
    in
    begin
      match members_filtered with
      | [] -> return cx use_op (EmptyT.make union_reason)
      | t :: [] -> recurse cx use_op reason (Resolve Next) tool t
      | t :: t' :: ts ->
        let resolve_tool = Resolve (List0 ((t', ts), (union_loc, Or))) in
        recurse cx use_op reason resolve_tool tool t
    end
  (* Resolve each member of an intersection. *)
  | IntersectionT (intersection_reason, rep) ->
    let intersection_loc = loc_of_reason intersection_reason in
    let (t, todo) = InterRep.members_nel rep in
    let resolve_tool = Resolve (List0 (todo, (intersection_loc, And))) in
    recurse cx use_op reason resolve_tool tool t
  (* `null` and `void` should pass through Partial, Required, $Exact,
     since we would like e.g. Partial<?Foo> to be equivalent to ?Partial<Foo> *)
  | DefT (_, (NullT | VoidT))
    when match tool with
         | Partial
         | Required
         | MakeExact
         | Object.ObjectMap _ ->
           true
         | _ -> false ->
    return cx use_op t
  (* Mirroring Object.assign() and {...null} semantics, treat null/void as
   * empty objects. *)
  | DefT (_, (NullT | VoidT)) ->
    let flags = { obj_kind = Exact; react_dro = None } in
    let x =
      Nel.one
        {
          Object.reason;
          props = NameUtils.Map.empty;
          flags;
          frozen = true;
          generics = t_generic_id;
          interface = None;
          reachable_targs = [];
        }
    in
    resolved ~next ~recurse cx use_op reason resolve_tool tool x
  (* TODO(jmbrown): Investigate if these cases can be used for ReactConfig/ObjecRep/Rest.
   * In principle, we should be able to use it for Rest, but right now
   * `const {x, ...y} = 3;` tries to get `x` from Number.
   * They don't make sense with $ReadOnly's semantics, since $ReadOnly doesn't model
   * copying/spreading an object. *)
  | DefT (_, BoolGeneralT)
  | DefT (_, SingletonBoolT _)
    when match tool with
         | Spread _ -> true
         | _ -> false ->
    let flags = { obj_kind = Exact; react_dro = None } in
    let x =
      Nel.one
        {
          Object.reason;
          props = NameUtils.Map.empty;
          flags;
          frozen = true;
          generics = t_generic_id;
          interface = None;
          reachable_targs = [];
        }
    in
    resolved ~next ~recurse cx use_op reason resolve_tool tool x
  (* mixed is treated as {[string]: mixed} except in type spread and react config checking, where
   * it's treated as {}. Any JavaScript value may be treated as an object so this is safe.
   *
   * We ought to use {} for everything since it is a more sound representation
   * of `mixed` as an object. The fact that we don't today is technical debt that we should
   * clean up.
   *)
  | DefT (r, MixedT _) as t ->
    (* TODO(jmbrown): This should be Inexact *)
    let flags = { obj_kind = Exact; react_dro = None } in
    let x =
      match tool with
      | Spread _
      | ObjectRep
      | ReactConfig _ ->
        Nel.one
          {
            Object.reason;
            props = NameUtils.Map.empty;
            flags;
            frozen = true;
            generics = t_generic_id;
            interface = None;
            reachable_targs = [];
          }
      | _ ->
        let flags =
          {
            flags with
            obj_kind =
              Indexed
                {
                  dict_name = None;
                  key = StrModuleT.make r;
                  value = t;
                  dict_polarity = Polarity.Neutral;
                };
          }
        in
        Nel.one
          {
            Object.reason;
            props = NameUtils.Map.empty;
            flags;
            frozen = true;
            generics = t_generic_id;
            interface = None;
            reachable_targs = [];
          }
    in
    resolved ~next ~recurse cx use_op reason resolve_tool tool x
  | DefT (r, ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro })) when tool = ReadOnly ->
    let elements =
      Base.List.map elements ~f:(fun (TupleElement { t; name; polarity = _; optional; reason }) ->
          TupleElement { t; name; polarity = Polarity.Positive; optional; reason }
      )
    in
    let def_reason =
      match desc_of_reason ~unwrap:false reason with
      | RReadOnlyType -> r
      | _ -> reason
    in
    return
      cx
      use_op
      (DefT (def_reason, ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro })))
  | DefT (r, ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro })) when tool = Partial ->
    let elements =
      Base.List.map elements ~f:(fun (TupleElement { t; name; polarity; optional = _; reason }) ->
          let t = TypeUtil.optional t in
          TupleElement { t; name; polarity; optional = true; reason }
      )
    in
    let def_reason =
      match desc_of_reason ~unwrap:false reason with
      | RPartialOf _ -> r
      | _ -> reason
    in
    let elem_t = union_of_ts (reason_of_t elem_t) (tuple_ts_of_elements elements) in
    let (_, num_total) = arity in
    let arity = (0, num_total) in
    return
      cx
      use_op
      (DefT (def_reason, ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro })))
  | DefT (r, ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro })) when tool = Required ->
    let elements =
      Base.List.map elements ~f:(fun (TupleElement { t; name; polarity; optional = _; reason }) ->
          let t =
            match t with
            | OptionalT { type_; _ } -> type_
            | _ -> t
          in
          TupleElement { t; name; polarity; optional = false; reason }
      )
    in
    let def_reason =
      match desc_of_reason ~unwrap:false reason with
      | RRequiredOf _ -> r
      | _ -> reason
    in
    let elem_t = union_of_ts (reason_of_t elem_t) (tuple_ts_of_elements elements) in
    let (_, num_total) = arity in
    let arity = (num_total, num_total) in
    return
      cx
      use_op
      (DefT (def_reason, ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro })))
  (* If we see an empty then propagate empty to tout. *)
  | DefT (r, EmptyT) -> return cx use_op (EmptyT.make r)
  (* Propagate any. *)
  | AnyT (_, src) -> return cx use_op (AnyT.why src reason)
  (* Other types have reasonable object representations that may be added as
   * new uses of the object kit resolution code is found. *)
  | t ->
    (match tool with
    | MakeExact -> add_output cx (Error_message.EUnsupportedExact (reason, reason_of_t t))
    | _ ->
      add_output
        cx
        (Error_message.EInvalidObjectKit { reason = reason_of_t t; reason_op = reason; use_op }));
    return cx use_op (AnyT.error reason)

let super
    (type a)
    ~(return : _ -> Type.use_op -> Type.t -> a)
    ~(next : _ -> Type.use_op -> Object.tool -> Reason.t -> Object.slice Nel.t -> a)
    ~recurse
    cx
    use_op
    reason
    resolve_tool
    tool
    acc = function
  | DefT (r, InstanceT { static; super; implements = _; inst = { own_props; _ } as inst }) ->
    let { Object.reason; _ } = acc in
    let slice = interface_slice cx r ~static ~inst own_props Generic.spread_empty in
    let acc = intersect2 cx reason acc slice in
    let acc =
      let (props, flags, frozen, generics, reachable_targs) = acc in
      {
        Object.reason;
        props;
        flags;
        frozen;
        generics;
        interface = Some (static, inst);
        reachable_targs;
      }
    in
    let resolve_tool = Object.Super (acc, resolve_tool) in
    recurse cx use_op reason resolve_tool tool super
  | AnyT (_, src) -> return cx use_op (AnyT.why src reason)
  | _ -> next cx use_op tool reason (Nel.one acc)

let mk_mapped_prop_type ~use_op ~mapped_type_optionality ~poly_prop key_t prop_optional =
  (* We persist the original use_op here so that errors involving the typeapp are positioned
     * at the use site and not the typeapp site *)
  let t =
    typeapp_with_use_op
      ~from_value:true
      ~use_desc:false
      (reason_of_t poly_prop)
      use_op
      poly_prop
      [key_t]
  in
  match mapped_type_optionality with
  | MakeOptional -> optional t
  | RemoveOptional ->
    (* TODO(jmbrown): This is not supported yet and we error at the declaration site *)
    t
  | KeepOptionality ->
    if prop_optional then
      optional t
    else
      t

let is_prop_optional = function
  | OptionalT _ -> true
  | _ -> false

let map_object
    poly_prop
    { variance; optional = mapped_type_optionality }
    cx
    reason
    use_op
    selected_keys_and_indexers
    { Object.reason = _; props; flags; frozen; generics; interface; reachable_targs } =
  let mk_prop_type = mk_mapped_prop_type ~use_op ~mapped_type_optionality ~poly_prop in
  let mk_variance variance prop_polarity =
    match variance with
    | Polarity.Neutral -> prop_polarity
    | _ -> variance
  in
  let props =
    let keys =
      match selected_keys_and_indexers with
      | Some (keys_with_reason, _) -> List.map fst keys_with_reason
      | _ -> NameUtils.Map.keys props
    in
    keys
    |> List.fold_left
         (fun map key ->
           match NameUtils.Map.find_opt key props with
           | None ->
             (* This is possible if a key is passed that does not actually conform to the
              * $Keys/keyof upper bound. That already results in an error, so we refuse to evaluate
              * the mapped type and signal to return `any` here *)
             let field =
               Field
                 {
                   preferred_def_locs = None;
                   key_loc = None;
                   type_ = AnyT.why (AnyError None) reason;
                   polarity = U.ite frozen Polarity.Positive Polarity.Neutral;
                 }
             in
             NameUtils.Map.add key field map
           (* Methods have no special consideration. There is no guarantee that the prop inserted by
            * the mapped type is going to continue to be a function, so we transform it into a regular
            * field. *)
           | Some { Object.prop_t; is_own = _; is_method = _; polarity = prop_polarity; key_loc } ->
             let key_loc =
               match key_loc with
               | None -> loc_of_reason (reason_of_t prop_t)
               | Some loc -> loc
             in
             let key_t =
               DefT
                 ( mk_reason (RStringLit key) key_loc,
                   SingletonStrT { from_annot = true; value = key }
                 )
             in
             let prop_optional = is_prop_optional prop_t in
             let polarity =
               if frozen then
                 Polarity.Positive
               else
                 mk_variance variance prop_polarity
             in
             let field =
               Field
                 {
                   preferred_def_locs = None;
                   key_loc = Some key_loc;
                   type_ = mk_prop_type key_t prop_optional;
                   polarity;
                 }
             in
             NameUtils.Map.add key field map)
         NameUtils.Map.empty
  in
  let obj_kind =
    match (selected_keys_and_indexers, flags.obj_kind) with
    | (Some (_, []), _) -> Exact
    | (Some (_, xs), Indexed dict_t) ->
      let dict_optional = is_prop_optional dict_t.value in
      let dict_key = union_of_ts reason xs in
      let dict_t' =
        {
          dict_t with
          key = dict_key;
          value = mk_prop_type dict_t.key dict_optional;
          dict_polarity = mk_variance variance dict_t.dict_polarity;
        }
      in
      Indexed dict_t'
    | (Some (_, xs), _) ->
      let key = union_of_ts reason xs in
      let dict =
        {
          dict_name = None;
          key;
          (* Similar to the missing prop case above, this is only possible when a semi-homomorphic
           * mapped type violates the constraint on the key type. We don't attempt to evaluate the
           * prop because it will likely lead to an error *)
          value = AnyT.why (AnyError None) reason;
          dict_polarity = Polarity.Neutral;
        }
      in
      Indexed dict
    | (None, Indexed dict_t) ->
      let dict_optional = is_prop_optional dict_t.value in
      let dict_t' =
        {
          dict_t with
          value = mk_prop_type dict_t.key dict_optional;
          dict_polarity = mk_variance variance dict_t.dict_polarity;
        }
      in
      Indexed dict_t'
    | (None, _) -> flags.obj_kind
  in
  let call = None in
  let id = Context.generate_property_map cx props in
  let proto = ObjProtoT reason in
  let flags = { flags with obj_kind } in
  mk_object_type
    cx
    ~reason
    ~invalidate_aliases:true
    ~interface
    ~reachable_targs
    ~kind:Subst_name.Mapped
    flags
    call
    id
    proto
    generics

let run
    (type a)
    ~add_output
    ~(return : _ -> _ -> Type.t -> a)
    ~(next : _ -> Type.use_op -> Object.tool -> Reason.t -> Object.slice Nel.t -> a)
    ~(recurse : _ -> Type.use_op -> Reason.t -> Object.resolve_tool -> Object.tool -> _ -> a)
    ~statics
    cx
    use_op
    reason
    resolve_tool
    tool
    l =
  match resolve_tool with
  | Object.Resolve resolve_tool ->
    resolve ~add_output ~return ~next ~recurse ~statics cx use_op reason resolve_tool tool l
  | Object.Super (acc, resolve_tool) ->
    super ~return ~next ~recurse cx use_op reason resolve_tool tool acc l
