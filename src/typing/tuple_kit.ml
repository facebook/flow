(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type

module type TUPLE = sig
  val run: Context.t -> Trace.t -> use_op:Type.use_op -> Reason.t -> Type.Tuple.resolve_tool ->
    Type.Tuple.tool -> Type.t -> Type.t -> unit
end

module Kit (Flow: Flow_common.S): TUPLE = struct
  include Flow

  let run =
    let open Tuple in

    (* Lift a pairwise function like spread2 to a function over a resolved list *)
    let merge (f: slice -> slice -> slice) =
      let f' (x0: resolved) (x1: resolved) =
        Nel.map_concat (fun slice1 ->
          Nel.map (f slice1) x0
        ) x1
      in
      let rec loop x0 = function
        | [] -> x0
        | x1::xs -> loop (f' x0 x1) xs
      in
      fun x0 (x1,xs) -> loop (f' x0 x1) xs
    in

    (*****************)
    (* Tuple Spread  *)
    (*****************)

    let tuple_spread =
      let open Tuple.Spread in

      let mk_tuple (r, elem_t, elements) =
        match elements with
        | Some elements ->  DefT (r, bogus_trust (), ArrT (TupleAT (elem_t, elements)))
        | None -> DefT (r, bogus_trust (), ArrT (ArrayAT (elem_t, None)))
      in

      let mk_resolved reason (resolved: Tuple.resolved) = match resolved with
      | x, [] -> mk_tuple x
      | x0, x1::xs -> UnionT (
        reason,
        UnionRep.make
          (mk_tuple x0)
          (mk_tuple x1)
          (Core_list.map ~f:mk_tuple xs)
      )
      in

      fun state cx trace use_op reason tout x ->
        let reason = replace_reason invalidate_rtype_alias reason in
        let {todo_rev; acc} = state in
        match todo_rev with
        | [] ->
          let t = Tvar.mk_where cx reason (fun tout ->
            let element_reason = replace_reason_const Reason.inferred_union_elem_array_desc reason in
            let elem_t = Tvar.mk cx element_reason in
            let resolve_to = (ResolveSpreadsToTuple (mk_id (), elem_t, tout)) in
            let elem_spread_list =
              (x, acc)
              |> Nel.to_list
              |> Core_list.map ~f:(mk_resolved reason)
              |> Core_list.map ~f:(fun t -> UnresolvedSpreadArg t)
            in
            resolve_spread_list cx ~use_op:unknown_use ~reason_op:reason elem_spread_list resolve_to
          )
          in
          rec_flow_t cx trace (
            t,
            tout
          )
          (* let t = match spread reason (Nel.rev (x, acc)) with
          | x, [] -> mk_object cx reason options x
          | x0, x1::xs ->
            UnionT (reason, UnionRep.make
              (mk_object cx reason options x0)
              (mk_object cx reason options x1)
              (Core_list.map ~f:(mk_object cx reason options) xs))
          in
          (* Intentional UnknownUse here. *)
          rec_flow_t cx trace (t, tout) *)
        | t::todo_rev ->
          let tool = Resolve Next in
          let state = {todo_rev; acc = x::acc} in
          rec_flow cx trace (t, TupleKitT (use_op, reason, tool, Spread state, tout))
    in

    (********************)
    (* Tuple Resolution *)
    (********************)

    let next = function
    | Spread state -> tuple_spread state
    in

    let rec map2opt f =
      let cons_opt v ls = match v with Some x -> x :: ls | _ -> ls in
      function
      | x::xs, y::ys ->
        cons_opt (f (Some x) (Some y)) (map2opt f (xs, ys))
      | x::xs, [] ->
        cons_opt (f (Some x) None) (map2opt f (xs, []))
      | [], y::ys ->
        cons_opt (f None (Some y)) (map2opt f ([], ys))
      | [], [] -> []
    in

    (* Intersect two tuple slices: slice * slice -> slice
      *
      * In general it is unsound to combine intersection types, but since tuple
      * kit utilities never write to their arguments, it is safe in this specific
      * case.
      *
      * [...[T] & [T, U]] = [...[T & T, U]]
      * [...[T] & [U]] = [...[T & U]]
      * [...A & (B | C)] = [...(A & B) | (A & C)]
      * [...(A | B) & C] = [...(A & C) | (B & C)]
      *)
    let intersect2 reason (_, elem_t1, elements1) (_, elem_t2, elements2) =
      let intersection t1 t2 = IntersectionT (reason, InterRep.make t1 t2 []) in
      let merge_elements t1 t2 =
        let t = intersection t1 t2 in
        t
      in
      let elements = match elements1, elements2 with
      | None, None -> None
      | Some elements1, None -> Some elements1
      | None, Some elements2 -> Some elements2
      | Some elements1, Some elements2 ->
        Some (
          map2opt (fun p1 p2 ->
            match p1, p2 with
            | None, None -> None
            | Some p1, Some p2 -> Some (merge_elements p1 p2)
            | Some p1, None -> Some p1
            | None, Some p2 -> Some p2
          ) (elements1, elements2)
        )
      in
      let elem_t = intersection elem_t1 elem_t2 in
      elem_t, elements
    in

    let intersect2_with_reason reason intersection_loc x1 x2 =
      let elem_t, elements = intersect2 reason x1 x2 in
      let r = mk_reason RTupleType intersection_loc in
      r, elem_t, elements
    in

    let resolved cx trace use_op reason resolve_tool tool tout x =
      match resolve_tool with
      | Next -> next tool cx trace use_op reason tout x
      | List0 ((t, todo), join) ->
        let resolve_tool = Resolve (List (todo, Nel.one x, join)) in
        rec_flow cx trace (t, TupleKitT (use_op, reason, resolve_tool, tool, tout))
      | List (todo, done_rev, join) ->
        match todo with
        | [] ->
          let x = match join with
          | _, Or -> Nel.cons x done_rev |> Nel.concat
          | loc, And -> merge (intersect2_with_reason reason loc) x done_rev
          in
          next tool cx trace use_op reason tout x
        | t::todo ->
          let done_rev = Nel.cons x done_rev in
          let resolve_tool = Resolve (List (todo, done_rev, join)) in
          rec_flow cx trace (t, TupleKitT (use_op, reason, resolve_tool, tool, tout))
    in

    let tuple_slice _ r elem_t elements =
      (r, elem_t, elements)
    in

    let resolve cx trace use_op reason resolve_tool tool tout = function
      (* We extract the elements from an ArrT. *)
      | DefT (r, _, ArrT (TupleAT (elem_t, elements))) ->
        let x = Nel.one (tuple_slice cx r elem_t (Some elements)) in
        resolved cx trace use_op reason resolve_tool tool tout x
      | DefT (r, _, ArrT (ArrayAT (elem_t, _))) ->
        let x = Nel.one (tuple_slice cx r elem_t None) in
        resolved cx trace use_op reason resolve_tool tool tout x
      (* Resolve each member of a union. *)
      | UnionT (union_reason, rep) ->
        let union_loc = aloc_of_reason union_reason in
        let t, todo = UnionRep.members_nel rep in
        let resolve_tool = Resolve (List0 (todo, (union_loc, Or))) in
        rec_flow cx trace (t, TupleKitT (use_op, reason, resolve_tool, tool, tout))
      (* Resolve each member of an intersection. *)
      | IntersectionT (intersection_reason, rep) ->
        let intersection_loc = aloc_of_reason intersection_reason in
        let t, todo = InterRep.members_nel rep in
        let resolve_tool = Resolve (List0 (todo, (intersection_loc, And))) in
        rec_flow cx trace (t, TupleKitT (use_op, reason, resolve_tool, tool, tout))
      (* If we see an empty then propagate empty to tout. *)
      | DefT (r, trust, EmptyT _) ->
        rec_flow cx trace (EmptyT.make r trust, UseT (use_op, tout))
      (* Propagate any. *)
      | AnyT (_, src) ->
        rec_flow cx trace (AnyT.why src reason, UseT (use_op, tout))
      (* Other types have reasonable object representations that may be added as
        * new uses of the object kit resolution code is found. *)
      | t ->
        (* Non-array non-any iterables *)
        let r = reason_of_t t in
        let element_tvar = Tvar.mk cx r in
        let iterable =
          let targs = [element_tvar; Unsoundness.why ResolveSpread r;
                                    Unsoundness.why ResolveSpread r] in
          get_builtin_typeapp cx
            (replace_reason_const (RCustom "Iterable expected for spread") r)
            "$Iterable" targs
        in
        flow_t cx (t, iterable);
        let x = Nel.one (tuple_slice cx r element_tvar None) in
        resolved cx trace use_op reason resolve_tool tool tout x
        (* add_output cx ~trace (Error_message.EInvalidTupleKit {
          reason = reason_of_t t;
          reason_op = reason;
          use_op;
        }) *)
    in

    fun cx trace ~use_op reason resolve_tool tool tout l ->
      match resolve_tool with
      | Resolve resolve_tool -> resolve cx trace use_op reason resolve_tool tool tout l

end
