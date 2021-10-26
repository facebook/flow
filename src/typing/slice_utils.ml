(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil

(* Don't use Flow_js directly from here *)
module Flow_js = struct end

let mk_object_type
    ~def_reason ~exact_reason ~invalidate_aliases ~interface flags call id proto generics =
  let def_reason =
    if invalidate_aliases then
      update_desc_reason invalidate_rtype_alias def_reason
    else
      def_reason
  in
  let (t, reason) =
    match interface with
    | Some (static, inst) ->
      let inst = { inst with own_props = id } in
      (* Implemented/super interfaces are folded into the property map computed by the slice, so
           we effectively flatten the hierarchy in the output *)
      ( DefT (def_reason, bogus_trust (), InstanceT (static, ObjProtoT def_reason, [], inst)),
        def_reason
      )
    | None ->
      let t = DefT (def_reason, bogus_trust (), ObjT (mk_objecttype ~flags ~call id proto)) in
      (* Wrap the final type in an `ExactT` if we have an exact flag *)
      Base.Option.value_map
        ~f:(fun exact_reason ->
          if Obj_type.is_legacy_exact_DO_NOT_USE flags.obj_kind then
            let exact_reason =
              if invalidate_aliases then
                update_desc_reason invalidate_rtype_alias exact_reason
              else
                exact_reason
            in
            (ExactT (exact_reason, t), exact_reason)
          else
            (t, def_reason))
        ~default:(t, def_reason)
        exact_reason
  in
  Generic.make_spread_id generics
  |> Base.Option.value_map ~default:t ~f:(fun id ->
         GenericT { bound = t; reason; id; name = Generic.to_string id }
     )

let is_widened_reason_desc r =
  match desc_of_reason r with
  | RWidenedObjProp _ -> true
  | _ -> false

let type_optionality_and_missing_property (t, _, _) =
  match t with
  | OptionalT { reason; type_ = t; use_desc } ->
    let is_missing_property_desc =
      match desc_of_reason reason with
      | RPossiblyMissingPropFromObj _ -> true
      | _ -> false
    in
    (t, true, use_desc && is_missing_property_desc)
  | _ -> (t, false, false)

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
      let t = DefT (reason, bogus_trust (), MixedT Mixed_everything) in
      t
  in
  (t, Obj_type.is_legacy_exact_DO_NOT_USE flags.obj_kind, is_method)

let read_dict r { value; dict_polarity; _ } =
  if Polarity.compat (dict_polarity, Polarity.Positive) then
    value
  else
    let reason = replace_desc_reason (RUnknownProperty None) r in
    DefT (reason, bogus_trust (), MixedT Mixed_everything)

let object_slice cx ~interface r id flags generics =
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
  { Object.reason = r; props; flags; generics; interface }

(* Treat dictionaries as optional, own properties. Dictionary reads should
 * be exact. TODO: Forbid writes to indexers through the photo chain.
 * Property accesses which read from dictionaries normally result in a
 * non-optional result, but that leads to confusing spread results. For
 * example, `p` in `{...{|p:T|},...{[]:U}` should `T|U`, not `U`. *)
let get_prop r p dict =
  match (p, dict) with
  | (Some _, _) -> p
  | (None, Some d) -> Some (TypeUtil.optional (read_dict r d), true, false)
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
      { Object.reason = r1; props = props1; flags = flags1; generics = generics1; interface = _ }
    )
    ( inline2,
      _inexact_reason2,
      { Object.reason = r2; props = props2; flags = flags2; generics = generics2; interface = _ }
    ) =
  let exact1 = Obj_type.is_legacy_exact_DO_NOT_USE flags1.obj_kind in
  let exact2 = Obj_type.is_legacy_exact_DO_NOT_USE flags2.obj_kind in
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
    let merge_props propname ((_, _, method1) as t1) ((_, _, method2) as t2) =
      let (t1, opt1, missing_prop1) = type_optionality_and_missing_property t1 in
      let (t2, opt2, missing_prop2) = type_optionality_and_missing_property t2 in
      if not opt2 then
        (t2, true, method2)
      else if opt1 && opt2 then
        ( make_optional_with_possible_missing_props
            propname
            missing_prop1
            missing_prop2
            r1
            (union t1 t2),
          true,
          (* Since we cannot be sure which is spread, if either
           * are methods we must treat the result as a method *)
          method2 || method1
        )
      (* In this case, we know opt2 is true and opt1 is false *)
      else
        (union t1 t2, true, method2 || method1)
    in
    let props =
      try
        Ok
          (NameUtils.Map.merge
             (fun x p1 p2 ->
               match (p1, p2) with
               | (None, None) -> None
               | (_, Some (p2, _, m)) when inline2 -> Some (p2, true, m)
               | (Some p1, Some p2) -> Some (merge_props x p1 p2)
               | (Some (p1, _, m), None) ->
                 if exact2 || inline2 then
                   Some (p1, true, m)
                 else
                   raise
                     (CannotSpreadError
                        (Error_message.EUnableToSpread
                           {
                             spread_reason = reason;
                             object1_reason = r1;
                             object2_reason = r2;
                             propname = x;
                             error_kind = Error_message.Inexact;
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
               | (None, Some ((t, _, m) as p2)) ->
                 let (_, opt2, _) = type_optionality_and_missing_property p2 in
                 (match flags1.obj_kind with
                 | Indexed _
                 | Inexact
                   when opt2 ->
                   let error_kind =
                     if dict1 <> None then
                       Error_message.Indexer
                     else
                       Error_message.Inexact
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
                 | _ -> Some (t, true, m)))
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
        if
          Obj_type.is_exact_or_sealed reason flags1.obj_kind
          && Obj_type.is_exact_or_sealed reason flags2.obj_kind
        then
          Exact
        else
          Inexact
    in
    let flags = { frozen = flags1.frozen && flags2.frozen; obj_kind } in
    let generics = Generic.spread_append generics1 generics2 in
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
      Ok (false, inexact_reason, { Object.reason; props; flags; generics; interface = None })
    | Error e -> Error e)

let spread =
  let resolved_of_acc_element = function
    | Object.Spread.ResolvedSlice resolved -> Nel.map (fun x -> (false, None, x)) resolved
    | Object.Spread.InlineSlice { Object.Spread.reason; prop_map; dict; generics } ->
      let obj_kind =
        match dict with
        | Some d -> Indexed d
        | None -> Exact
      in
      let flags = { obj_kind; frozen = false } in
      let props = NameUtils.Map.mapi (read_prop reason flags) prop_map in
      Nel.one (true, None, { Object.reason; props; flags; generics; interface = None })
  in

  fun ~dict_check cx ~use_op reason nel ->
    match nel with
    | (x, []) -> Ok (resolved_of_acc_element x)
    | (x0, (x1 :: xs : Object.Spread.acc_element list)) ->
      merge_result (spread2 ~dict_check cx ~use_op reason) resolved_of_acc_element x0 (x1, xs)

let spread_mk_object cx reason target { Object.reason = _; props; flags; generics; interface = _ } =
  let open Object.Spread in
  let props =
    NameUtils.Map.map
      (fun (t, _, is_method) ->
        if is_method then
          Method (None, t)
        else
          Field (None, t, Polarity.Neutral))
      props
  in
  let id = Context.generate_property_map cx props in
  let flags =
    let (exact, sealed) =
      match target with
      (* Type spread result is exact if annotated to be exact *)
      | Annot { make_exact } -> (make_exact, Sealed)
      (* Value spread result is exact if all inputs are exact *)
      | Value { make_seal } -> (Obj_type.is_legacy_exact_DO_NOT_USE flags.obj_kind, make_seal)
    in
    let dict = Obj_type.get_dict_opt flags.obj_kind in
    let obj_kind =
      match (exact, sealed, dict) with
      | (_, _, Some d) -> Indexed d
      | (true, Object.Spread.UnsealedInFile x, _) -> UnsealedInFile x
      | (true, _, _) -> Exact
      | _ -> Inexact
    in
    let frozen = sealed = Object.Spread.Frozen in
    { obj_kind; frozen }
  in
  let proto = ObjProtoT reason in
  let call = None in
  mk_object_type
    ~def_reason:reason
    ~exact_reason:(Some reason)
    ~invalidate_aliases:true
    ~interface:None
    flags
    call
    id
    proto
    generics

let object_spread
    (type a)
    ~dict_check
    ~widen_obj_type
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
      (fun { Object.reason = r; props = _; flags = { obj_kind; _ }; generics = _; interface = _ } ->
        match options with
        | Annot { make_exact } when make_exact && obj_kind = Inexact ->
          add_output
            cx
            (Error_message.EIncompatibleWithExact ((r, reason), use_op, Error_message.Inexact))
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
        let l = widen_obj_type cx ~use_op reason t in
        let tool = Object.Spread (options, state) in
        recurse cx use_op reason resolve_tool tool l
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
      reason
      merge_mode
      { Object.reason = r1; props = props1; flags = flags1; generics = generics1; interface = _ }
      { Object.reason = r2; props = props2; flags = flags2; generics = generics2; interface = _ } =
    let dict1 = Obj_type.get_dict_opt flags1.obj_kind in
    let dict2 = Obj_type.get_dict_opt flags2.obj_kind in
    let props =
      NameUtils.Map.merge
        (fun k p1 p2 ->
          match
            ( merge_mode,
              get_prop r1 p1 dict1,
              get_prop r2 p2 dict2,
              Obj_type.is_legacy_exact_DO_NOT_USE flags2.obj_kind
            )
          with
          (* If the object we are using to subtract has an optional property, non-own
           * property, or is inexact then we should add this prop to our result, but
           * make it optional as we cannot know for certain whether or not at runtime
           * the property would be subtracted.
           *
           * Sound subtraction also considers exactness and owness to determine
           * optionality. If p2 is maybe-own then sometimes it may not be
           * subtracted and so is optional. If props2 is not exact then we may
           * optionally have some undocumented prop. *)
          | ((Sound | IgnoreExactAndOwn), Some (t1, _, m1), Some ((OptionalT _ as t2), _, _), _)
          | (Sound, Some (t1, _, m1), Some (t2, false, _), _)
          | (Sound, Some (t1, _, m1), Some (t2, _, _), false) ->
            subt_check ~use_op cx (t1, optional t2);
            let p =
              if m1 then
                Method (None, optional t1)
              else
                Field (None, optional t1, Polarity.Neutral)
            in
            Some p
          (* Otherwise if the object we are using to subtract has a non-optional own
           * property and the object is exact then we never add that property to our
           * source object. *)
          | ((Sound | IgnoreExactAndOwn), None, Some (t2, _, _), _) ->
            let reason = replace_desc_reason (RUndefinedProperty k) r1 in
            subt_check ~use_op cx (VoidT.make reason |> with_trust bogus_trust, t2);
            None
          | ((Sound | IgnoreExactAndOwn), Some (t1, _, _), Some (t2, _, _), _) ->
            subt_check ~use_op cx (t1, t2);
            None
          (* If we have some property in our first object and none in our second
           * object, but our second object is inexact then we want to make our
           * property optional and flow that type to mixed. *)
          | (Sound, Some (t1, _, m1), None, false) ->
            subt_check ~use_op cx (t1, MixedT.make r2 |> with_trust bogus_trust);
            let p =
              if m1 then
                Method (None, optional t1)
              else
                Field (None, optional t1, Polarity.Neutral)
            in
            Some p
          (* If neither object has the prop then we don't add a prop to our
           * result here. *)
          | ((Sound | IgnoreExactAndOwn | ReactConfigMerge _), None, None, _) -> None
          (* If our first object has a prop and our second object does not have that
           * prop then we will copy over that prop. If the first object's prop is
           * non-own then sometimes we may not copy it over so we mark it
           * as optional. *)
          | (IgnoreExactAndOwn, Some (t, _, m1), None, _) ->
            let p =
              if m1 then
                Method (None, t)
              else
                Field (None, t, Polarity.Neutral)
            in
            Some p
          | (ReactConfigMerge _, Some (t, _, m1), None, _) ->
            let p =
              if m1 then
                Method (None, t)
              else
                Field (None, t, Polarity.Positive)
            in
            Some p
          | (Sound, Some (t, true, m1), None, _) ->
            let p =
              if m1 then
                Method (None, t)
              else
                Field (None, t, Polarity.Neutral)
            in
            Some p
          | (Sound, Some (t, false, m1), None, _) ->
            let p =
              if m1 then
                Method (None, optional t)
              else
                Field (None, optional t, Polarity.Neutral)
            in
            Some p
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
              Some (t1, _, m1),
              Some (OptionalT { reason = _; type_ = t2; use_desc = _ }, _, _),
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
              if m1 then
                Method (None, t1)
              else
                Field (None, t1, Polarity.Neutral)
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
          | (ReactConfigMerge _, Some (t1, _, m1), Some (t2, _, _), _) ->
            (* The DP type for p must be a subtype of the P type for p. *)
            subt_check ~use_op:unknown_use cx (t2, t1);
            let p =
              if m1 then
                Method (None, optional t1)
              else
                Field (None, optional t1, Polarity.Positive)
            in
            Some p
          (* Consider this case:
           *
           *     {...{p}, ...C} = {}
           *
           * For C there will be no prop. However, if the props object is exact
           * then we need to throw an error. *)
          | (ReactConfigMerge _, None, Some (_, _, _), _) ->
            ( if Obj_type.is_legacy_exact_DO_NOT_USE flags1.obj_kind then
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
      match (flags1.obj_kind, dict) with
      | (Exact, _) -> Exact
      | (Indexed _, Some d) -> Indexed d
      | (UnsealedInFile _, _) when Obj_type.sealed_in_op reason flags1.obj_kind -> Exact
      | _ -> Inexact
    in
    let flags = { frozen = false; obj_kind } in
    let generics = Generic.spread_subtract generics1 generics2 in
    let id = Context.generate_property_map cx props in
    let proto = ObjProtoT r1 in
    let call = None in
    mk_object_type
      ~def_reason:r1
      ~exact_reason:(Some r1)
      ~invalidate_aliases:true
      ~interface:None
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
    let xs = Nel.map_concat (fun slice -> Nel.map (rest cx ~use_op reason options slice) x) base in
    let t =
      match xs with
      | (x, []) -> x
      | (x0, x1 :: xs) -> UnionT (reason, UnionRep.make x0 x1 xs)
    in
    let use_op p = Frame (ReactGetConfig { polarity = p }, use_op) in
    return cx use_op options t

(********************)
(* Object Read Only *)
(********************)
let object_read_only =
  let polarity = Polarity.Positive in
  let mk_read_only_object cx reason slice =
    let { Object.reason = r; props; flags; generics; interface } = slice in
    let props =
      NameUtils.Map.map
        (fun (t, _, is_method) ->
          if is_method then
            Method (None, t)
          else
            Field (None, t, polarity))
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
    let def_reason =
      match desc_of_reason ~unwrap:false reason with
      | RReadOnlyType -> r
      | _ -> reason
    in
    mk_object_type
      ~def_reason
      ~exact_reason:(Some reason)
      ~invalidate_aliases:false
      ~interface
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

let object_partial =
  let mk_partial_object cx reason slice =
    let { Object.reason = r; props; flags; generics; interface } = slice in
    let props =
      NameUtils.Map.map
        (fun (t, _, is_method) ->
          if is_method then
            Method (None, t)
          else
            match t with
            | OptionalT _ -> Field (None, t, Polarity.Neutral)
            | _ ->
              Field
                ( None,
                  OptionalT { reason = reason_of_t t; type_ = t; use_desc = false },
                  Polarity.Neutral
                ))
        props
    in
    let call = None in
    let id = Context.generate_property_map cx props in
    let proto = ObjProtoT reason in
    let def_reason =
      match desc_of_reason ~unwrap:false reason with
      | RPartialOf _ -> r
      | _ -> reason
    in
    mk_object_type
      ~def_reason
      ~exact_reason:(Some reason)
      ~invalidate_aliases:false
      ~interface
      flags
      call
      id
      proto
      generics
  in
  fun cx reason x ->
    match Nel.map (mk_partial_object cx reason) x with
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
    reason
    { Object.reason = r1; props = props1; flags = flags1; generics = generics1; interface = _ }
    { Object.reason = r2; props = props2; flags = flags2; generics = generics2; interface = _ } =
  let dict1 = Obj_type.get_dict_opt flags1.obj_kind in
  let dict2 = Obj_type.get_dict_opt flags2.obj_kind in
  let intersection t1 t2 =
    if reasonless_compare t1 t2 = 0 then
      t1
    else
      IntersectionT (reason, InterRep.make t1 t2 [])
  in
  let merge_props (t1, own1, m1) (t2, own2, m2) =
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
    (t, own1 || own2, m1 || m2)
  in
  let props =
    NameUtils.Map.merge
      (fun _ p1 p2 ->
        let read_dict r d = (optional (read_dict r d), true, false) in
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
    Base.Option.merge dict1 dict2 (fun d1 d2 ->
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
          Obj_type.is_legacy_exact_DO_NOT_USE flags1.obj_kind
          || Obj_type.is_legacy_exact_DO_NOT_USE flags2.obj_kind
        then
          Exact
        else
          Inexact
        )
  in
  let flags = { frozen = flags1.frozen || flags2.frozen; obj_kind } in
  let generics = Generic.spread_append generics1 generics2 in
  (props, flags, generics)

let intersect2_with_reason reason intersection_loc x1 x2 =
  let (props, flags, generics) = intersect2 reason x1 x2 in
  let reason = mk_reason RObjectType intersection_loc in
  { Object.reason; props; flags; generics; interface = None }

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
          | (loc, And) -> merge (intersect2_with_reason reason loc) x done_rev
        in
        next cx use_op tool reason x
      | t :: todo ->
        let done_rev = Nel.cons x done_rev in
        let resolve_tool = Resolve (List (todo, done_rev, join)) in
        recurse cx use_op reason resolve_tool tool t)
  )

let interface_slice cx r intf id generics =
  let (id, obj_kind) =
    let props = Context.find_props cx id in
    (* TODO $-prefixed names should be internal *)
    match
      ( NameUtils.Map.find_opt (OrdinaryName "$key") props,
        NameUtils.Map.find_opt (OrdinaryName "$value") props
      )
    with
    | (Some (Field (_, key, polarity)), Some (Field (_, value, polarity')))
      when polarity = polarity' ->
      let props =
        props
        (* TODO $-prefixed names should be internal *)
        |> NameUtils.Map.remove (OrdinaryName "$key")
        |> NameUtils.Map.remove (OrdinaryName "$value")
      in
      let id = Context.generate_property_map cx props in
      let dict = { dict_name = None; key; value; dict_polarity = polarity } in
      (id, Indexed dict)
    | _ -> (id, Inexact)
  in
  let flags = { frozen = false; obj_kind } in
  object_slice cx ~interface:(Some intf) r id flags generics

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
      | _ -> (ls, t)
    in
    loop t Generic.spread_empty
  in
  match t with
  (* We extract the props from an ObjT. *)
  | DefT (r, _, ObjT { props_tmap; Type.flags; _ }) ->
    let x = Nel.one (object_slice cx ~interface:None r props_tmap flags t_generic_id) in
    resolved ~next ~recurse cx use_op reason resolve_tool tool x
  (* We take the fields from an InstanceT excluding methods (because methods
   * are always on the prototype). We also want to resolve fields from the
   * InstanceT's super class so we recurse. *)
  | DefT (r, _, InstanceT (static, super, _, ({ own_props; inst_kind; _ } as inst))) ->
    let resolve_tool =
      Super (interface_slice cx r (static, inst) own_props t_generic_id, resolve_tool)
    in
    begin
      match (tool, inst_kind) with
      | (Spread _, InterfaceKind _)
      | (ObjectWiden _, InterfaceKind _) ->
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
  | DefT (r, _, ClassT i) -> recurse cx use_op reason (Resolve resolve_tool) tool (statics cx r i)
  (* Resolve each member of a union. *)
  | UnionT (union_reason, rep) ->
    let union_loc = aloc_of_reason union_reason in
    let (t, todo) = UnionRep.members_nel rep in
    let resolve_tool = Resolve (List0 (todo, (union_loc, Or))) in
    let tool =
      match tool with
      | Spread (options, state) ->
        Spread (options, { state with Spread.union_reason = Some union_reason })
      | _ -> tool
    in
    recurse cx use_op reason resolve_tool tool t
  (* Resolve each member of an intersection. *)
  | IntersectionT (intersection_reason, rep) ->
    let intersection_loc = aloc_of_reason intersection_reason in
    let (t, todo) = InterRep.members_nel rep in
    let resolve_tool = Resolve (List0 (todo, (intersection_loc, And))) in
    recurse cx use_op reason resolve_tool tool t
  (* Null and Void should pass through $Partial, since we would like $Partial<?Foo> to be equivalent to ?$Partial<Foo> *)
  | DefT (_, _, (NullT | VoidT)) when tool = Partial -> return cx use_op t
  (* Mirroring Object.assign() and {...null} semantics, treat null/void as
   * empty objects. *)
  | DefT (_, _, (NullT | VoidT)) ->
    let flags = { frozen = true; obj_kind = Exact } in
    let x =
      Nel.one
        {
          Object.reason;
          props = NameUtils.Map.empty;
          flags;
          generics = t_generic_id;
          interface = None;
        }
    in
    resolved ~next ~recurse cx use_op reason resolve_tool tool x
  (* TODO(jmbrown): Investigate if these cases can be used for ReactConfig/ObjecRep/Rest.
   * In principle, we should be able to use it for Rest, but right now
   * `const {x, ...y} = 3;` tries to get `x` from Number.
   * They don't make sense with $ReadOnly's semantics, since $ReadOnly doesn't model
   * copying/spreading an object. *)
  | DefT (_, _, (StrT _ | NumT _ | BoolT _))
    when match tool with
         | ObjectWiden _
         | Spread _ ->
           true
         | _ -> false ->
    let flags = { frozen = true; obj_kind = Exact } in
    let x =
      Nel.one
        {
          Object.reason;
          props = NameUtils.Map.empty;
          flags;
          generics = t_generic_id;
          interface = None;
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
  | DefT (r, _, MixedT _) as t ->
    (* TODO(jmbrown): This should be Inexact *)
    let flags = { frozen = true; obj_kind = Exact } in
    let x =
      match tool with
      | ObjectWiden _
      | Spread _
      | ObjectRep
      | ReactConfig _ ->
        Nel.one
          {
            Object.reason;
            props = NameUtils.Map.empty;
            flags;
            generics = t_generic_id;
            interface = None;
          }
      | _ ->
        let flags =
          {
            flags with
            obj_kind =
              Indexed
                {
                  dict_name = None;
                  key = StrT.make r |> with_trust bogus_trust;
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
            generics = t_generic_id;
            interface = None;
          }
    in
    resolved ~next ~recurse cx use_op reason resolve_tool tool x
  (* If we see an empty then propagate empty to tout. *)
  | DefT (r, trust, EmptyT) -> return cx use_op (EmptyT.make r trust)
  (* Propagate any. *)
  | AnyT (_, src) -> return cx use_op (AnyT.why src reason)
  (* Other types have reasonable object representations that may be added as
   * new uses of the object kit resolution code is found. *)
  | t ->
    add_output
      cx
      (Error_message.EInvalidObjectKit { reason = reason_of_t t; reason_op = reason; use_op });
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
  | DefT (r, _, InstanceT (st, super, _, ({ own_props; _ } as inst))) ->
    let interface = (st, inst) in
    let { Object.reason; _ } = acc in
    let slice = interface_slice cx r interface own_props Generic.spread_empty in
    let acc = intersect2 reason acc slice in
    let acc =
      let (props, flags, generics) = acc in
      { Object.reason; props; flags; generics; interface = Some interface }
    in
    let resolve_tool = Object.Super (acc, resolve_tool) in
    recurse cx use_op reason resolve_tool tool super
  | AnyT _ -> return cx use_op (AnyT.untyped reason)
  | _ -> next cx use_op tool reason (Nel.one acc)

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
