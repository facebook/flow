(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** manipulation of function types *)
(** TODO move big chunks of Flow_js into here *)
(** TODO same thing with object types into an ObjType module *)

open Utils_js
open Reason_js
open Type

(** given a function, return an exemplar of its return type.
    Top-level reason is pinned to an internal blank, other
    reasons are left unmodified.

    We use this below to partition function lists on common return
    types ("common" meaning immediately equal, not unifiable).

    Note: non-FunTs are returned as-is. Reason scrubbing ensures
    that non-FunTs and return type exemplars are disjoint.
  *)
let return_type_ex =
  let blank_r = reason_of_string "function return type" in
  function
  | FunT (_, _, _, { return_t; _ }) ->
    mod_reason_of_t (fun _ -> blank_r) return_t
  | t -> t

(** partition a list of FunTs by return type.
    Note: we allow non-FunTs - they become singleton cells in
    the partition.
  *)
let return_type_partition =
  List.fold_left
    (fun p ft -> Partition.add ft p)
    (Partition.empty return_type_ex)

(** given a list of types, merge any FunTs that share an obviously
    equal return type, leaving everything else alone.

    The motivation for this is our current modeling of function
    overloads by intersection types: to preserve the correlation
    between param signatures and return types in an overload list,
    we avoid normalizing intersections of function types into a
    single merged signature - but this means that bona fide function
    intersections are mishandled. By merging only function types
    whose return types are manifestly equal, we service the most
    common use case while preserving the desired behavior for
    overloading.

    TODO use a new OverloadT to model overload selection lists
    (and maybe add surface syntax for it), and replace this
    with a full normalization over intersections.

    Note: here merging function types means taking their intersection:
    - taking the union of each param type in the shared arity
    - setting param type to MixedT outside the shared arity
    - in the general case, we would take the intersection of return
      types, but here we explicitly confine the merge to functions
      whose return types are equal. This is necessary while we
      continue to model function overloading with IntersectionT.
    - statics are currently set to MixedT. TODO
    - proto is currently set to MixedT. TODO
    - this is currently set to MixedT. TODO

    Selection order is preserved: a merged FunT will appear in
    the position of its first constituent. Eg

    merge_funtypes_by_return_type
      [(bool) => string; (number) => void; (Object) => bool; (string) => void]
    > [(bool) => string; (number | string) => void; (Object) => bool]

  *)
let merge_funtypes_by_return_type =

  (* merge pair of FunTs *)
  let merge_pair acc t =
    match acc, t with
    | FunT (reason_x, _static_x, _proto_x, {
        this_t = _this_x;
        params_tlist = params_x; params_names = names_x;
        return_t = ret_x; closure_t = _closure_x;
        changeset = _changeset_x }),
      FunT (_reason_y, _static_y, _proto_y, {
        this_t = _this_y;
        params_tlist = params_y; params_names = names_y;
        return_t = _ret_y; closure_t = _closure_y;
        changeset = _changeset_y }) ->

      let arity_x, arity_y = List.(length params_x, length params_y) in
      let shared_arity = min arity_x arity_y in
      let extra_arity = (max arity_x arity_y) - shared_arity in

      let r = mk_reason "function intersection" (loc_of_reason reason_x) in

      FunT (r,
        MixedT.t, (* statics TODO *)
        MixedT.t, (* proto TODO *)
        {
          this_t = MixedT.t; (* TODO *)

          params_tlist = List.map2 (fun p1 p2 ->
            match p1, p2 with
            | UnionT (r, rep), _ ->
              let ts = List.rev (p2 :: List.rev (UnionRep.members rep)) in
              UnionT (r, UnionRep.make ts)
            | _ ->
              let r = reason_of_t p1 in
              UnionT (r, UnionRep.make [p1; p2])
          ) (ListUtils.first_n shared_arity params_x)
            (ListUtils.first_n shared_arity params_y)

          @ ListUtils.copy_n extra_arity MixedT.t;

          params_names =
            if arity_x > arity_y then names_x else names_y;

          return_t = ret_x;

          closure_t = -1;
          changeset = Changeset.empty
        })
    | _ -> assert_false "non-funtypes sent to merge_pair"

  in fun ts ->
    let p = return_type_partition ts in
    if Partition.is_discrete p then ts else
    (* for each type in ts, merge and/or transfer to result list *)
    let ts, _ = List.fold_left (fun (ts, done_fts) -> function
      (* already merged, skip *)
      | FunT _ as t when TypeSet.mem t done_fts ->
        ts, done_fts
      (* previously-unmerged FunT: merge the cell, mark seen, add *)
      | FunT _ as t -> (
        match Partition.cell t p with
        | [] -> assert_false "funtype not found in intersection partition"
        | [t] -> t :: ts, TypeSet.add t done_fts
        | ft :: fts as cell ->
          let merged = List.fold_left merge_pair ft fts in
          merged :: ts,
          List.fold_left (fun acc t -> TypeSet.add t acc) done_fts cell
      )
      (* non-FunT *)
      | t -> t :: ts, done_fts
    ) ([], TypeSet.empty) ts
    in
    List.rev ts
