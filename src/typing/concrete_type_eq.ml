(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open TypeUtil
open Constraint

let rec swap_reason t2 t1 =
  match (t2, t1) with
  (* In reposition we also recurse and reposition some nested types. We need
   * to make sure we swap the types for these reasons as well. Otherwise our
   * optimized union ~> union check will not pass. *)
  | (MaybeT (_, t2), MaybeT (r, t1)) -> MaybeT (r, swap_reason t2 t1)
  | (OptionalT { reason = _; type_ = t2; use_desc = _ }, OptionalT { reason; type_ = t1; use_desc })
    ->
    OptionalT { reason; type_ = swap_reason t2 t1; use_desc }
  | (ExactT (_, t2), ExactT (r, t1)) -> ExactT (r, swap_reason t2 t1)
  | ( OpaqueT (_, { underlying_t = u2; super_t = s2; _ }),
      OpaqueT (r, ({ underlying_t = u1; super_t = s1; _ } as o))
    ) ->
    let underlying_t =
      match (u1, u2) with
      | (Some u1, Some u2) -> Some (swap_reason u2 u1)
      | _ -> u2
    in
    let super_t =
      match (s1, s2) with
      | (Some s1, Some s2) -> Some (swap_reason s2 s1)
      | _ -> s2
    in
    OpaqueT (r, { o with underlying_t; super_t })
  | _ -> mod_reason_of_t (fun _ -> reason_of_t t1) t2

(* This predicate attempts to flatten out OpenTs and AnnotTs before performing a
 * structural reasonless equality check of two types. *)
let rec eq cx t1 t2 =
  if t1 == t2 then
    true
  else
    match (t1, t2) with
    | (OpenT (_, id1), OpenT (_, id2))
      when let (root_id1, _c1) = Context.find_constraints cx id1 in
           let (root_id2, _c2) = Context.find_constraints cx id2 in
           root_id1 = root_id2 ->
      true
    | (OpenT (_, id1), t2) ->
      (match Context.find_graph cx id1 with
      | Resolved t1 -> eq cx t1 t2
      | FullyResolved s1 -> eq cx (Context.force_fully_resolved_tvar cx s1) t2
      | Unresolved _ -> compare t1 (swap_reason t2 t1) = 0)
    | (_, OpenT (_, id2)) ->
      (match Context.find_graph cx id2 with
      | Resolved t2 -> eq cx t1 t2
      | FullyResolved s2 -> eq cx t1 (Context.force_fully_resolved_tvar cx s2)
      | Unresolved _ -> compare t1 (swap_reason t2 t1) = 0)
    | (AnnotT (_, t1, _), _) -> eq cx t1 t2
    | (_, AnnotT (_, t2, _)) -> eq cx t1 t2
    | (UnionT (_, rep1), UnionT (_, rep2)) ->
      (match Base.List.for_all2 (UnionRep.members rep1) (UnionRep.members rep2) ~f:(eq cx) with
      | Base.List.Or_unequal_lengths.Ok result -> result
      | Base.List.Or_unequal_lengths.Unequal_lengths -> false)
    | _ -> compare t1 (swap_reason t2 t1) = 0
