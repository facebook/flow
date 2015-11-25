(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Typing_defs
open Utils

module TUtils = Typing_utils
module Reason = Typing_reason

(* This should be configurable by client command args... eventually*)
let sample_rate = 0
let display_limit = 10
let samples_limit = 5

type level =
  | Unchecked (* Completely unchecked code, i.e. Tanys *)
  | Partial   (* Partially checked code, e.g. array, Awaitable<_> with no
                 concrete type parameters *)
  | Checked   (* Completely checked code *)

let string_of_level = function
  | Checked   -> "checked"
  | Partial   -> "partial"
  | Unchecked -> "unchecked"

module CLMap = MyMap(struct
  type t = level
  let compare x y = Pervasives.compare x y
end)

type pos_stats_entry = {
  (* How many times this reason position has occured. *)
  pos_count : int;
  (* Random sample of expressions where this reason position has occured, for
   * debugging purposes *)
  samples : Pos.t list;
}

type level_stats_entry = {
  (* Number of expressions of this level *)
  count : int;
  (* string of reason -> position of reason -> stats *)
  reason_stats : (pos_stats_entry Pos.Map.t) SMap.t;
}

let empty_pos_stats_entry = {
  pos_count = 0;
  samples = [];
}

let empty_level_stats_entry = {
  count = 0;
  reason_stats = SMap.empty;
}

let empty_counter =
  let m = CLMap.empty in
  let m = CLMap.add Checked empty_level_stats_entry m in
  let m = CLMap.add Partial empty_level_stats_entry m in
  CLMap.add Unchecked empty_level_stats_entry m

(* This is highly unscientific and not really uniform sampling, but for
 * debugging purposes should be enough. *)
let merge_pos_stats_samples l1 l2 =
  let rec pick_n acc n m l =
    if n == 0 then acc
    else if m <= n then l @ acc else match l with
      | [] -> acc
      | h::tl ->
        if Random.int m < n then pick_n (h::acc) (n-1) (m-1) tl
        else pick_n acc n (m-1) tl in
  pick_n [] samples_limit ((List.length l1) + (List.length l2)) (l1 @ l2)

let add_sample_pos p samples =
  merge_pos_stats_samples samples [p]

let incr_reason_stats r p reason_stats =
  if sample_rate = 0 || Random.int sample_rate <> 0 then reason_stats else
  let reason_pos = Reason.to_pos r in
  let string_key =
    let open Reason in match r with
    | Rnone -> "Rnone"
    | Rwitness _ -> "Rwitness"
    | Ridx _ -> "Ridx"
    | Ridx_vector _ -> "Ridx_vector"
    | Rappend _ -> "Rappend"
    | Rfield _ -> "Rfield"
    | Rforeach _ -> "Rforeach"
    | Rasyncforeach _ -> "Rasyncforeach"
    | Raccess _ -> "Raccess"
    | Rarith _ -> "Rarith"
    | Rarith_ret _ -> "Rarith_ret"
    | Rarray_plus_ret _ -> "Rarray_plus_ret"
    | Rstring2 _ -> "Rstring2"
    | Rcomp _ -> "Rcomp"
    | Rconcat _ -> "Rconcat"
    | Rconcat_ret _ -> "Rconcat_ret"
    | Rlogic _ -> "Rlogic"
    | Rlogic_ret _ -> "Rlogic_ret"
    | Rbitwise _ -> "Rbitwise"
    | Rbitwise_ret _ -> "Rbitwise_ret"
    | Rstmt _ -> "Rstmt"
    | Rno_return _ -> "Rno_return"
    | Rno_return_async _ -> "Rno_return_async"
    | Rret_fun_kind _ -> "Rret_fun_kind"
    | Rhint _ -> "Rhint"
    | Rnull_check _ -> "Rnull_check"
    | Rnot_in_cstr _ -> "Rnot_in_cstr"
    | Rthrow _ -> "Rthrow"
    | Rplaceholder _ -> "Rplaceholder"
    | Rattr _ -> "Rattr"
    | Rxhp _ -> "Rxhp"
    | Rret_div _ -> "Rret_div"
    | Ryield_gen _ -> "Ryield_gen"
    | Ryield_asyncgen _ -> "Ryield_asyncgen"
    | Ryield_asyncnull _ -> "Ryield_asyncnull"
    | Ryield_send _ -> "Ryield_send"
    | Rlost_info _ -> "Rlost_info"
    | Rcoerced _ -> "Rcoerced"
    | Rformat _ -> "Rformat"
    | Rclass_class _ -> "Rclass_class"
    | Runknown_class _ -> "Runknown_class"
    | Rdynamic_yield _ -> "Rdynamic_yield"
    | Rmap_append _ -> "Rmap_append"
    | Rvar_param _ -> "Rvar_param"
    | Runpack_param _ -> "Runpack_param"
    | Rinstantiate _ -> "Rinstantiate"
    | Rarray_filter _ -> "Rarray_filter"
    | Rtype_access _ -> "Rtype_access"
    | Rexpr_dep_type _ -> "Rexpr_dep_type"
    | Rnullsafe_op _ -> "Rnullsafe_op"
    | Rtconst_no_cstr _ -> "Rtconst_no_cstr"
    | Rused_as_map _ -> "Rused_as_map"
    | Rused_as_shape _ -> "Rused_as_shape" in
  let pos_stats_map = match SMap.get string_key reason_stats with
    | Some x -> x
    | None -> Pos.Map.empty in
  let pos_stats = match Pos.Map.get reason_pos pos_stats_map with
    | Some x -> x
    | None -> empty_pos_stats_entry in
  let pos_stats = {
    pos_count = pos_stats.pos_count + sample_rate;
    samples = add_sample_pos p pos_stats.samples
  } in
  SMap.add
    string_key
    (Pos.Map.add reason_pos pos_stats pos_stats_map)
    reason_stats

let incr_counter k (r, p, c) =
  let v = CLMap.find_unsafe k c in
  CLMap.add k {
    count = v.count + 1;
    reason_stats = incr_reason_stats r p v.reason_stats;
  } c

let merge_pos_stats p1 p2 = {
  pos_count = p1.pos_count + p2.pos_count;
  samples = merge_pos_stats_samples p1.samples p2.samples;
}

let merge_reason_stats s1 s2 =
  SMap.merge (fun _ s1 s2 ->
    Option.merge s1 s2 (fun s1 s2 ->
      Pos.Map.merge (fun _ p1 p2 ->
        Option.merge p1 p2 (fun p1 p2 ->
          merge_pos_stats p1 p2
        )
      ) s1 s2
    )
  ) s1 s2

let merge_and_sum cs1 cs2 =
  CLMap.merge (fun _ c1 c2 ->
    Option.merge c1 c2 (fun c1 c2 -> {
      count = c1.count + c2.count;
      reason_stats = merge_reason_stats c1.reason_stats c2.reason_stats
    })
  ) cs1 cs2

(* An assoc list that counts the number of expressions at each coverage level,
 * along with stats about their reasons *)
type level_stats = level_stats_entry CLMap.t

(* There is a trie in utils/, but it is not quite what we need ... *)

type 'a trie =
  | Leaf of 'a
  | Node of 'a * 'a trie SMap.t

let rec is_tany ty = match ty with
  | r, Tany -> Some r
  | _, Tunresolved [] -> None
  | _, Tunresolved (h::tl) -> begin match is_tany h with
    | Some r when
      List.for_all tl (compose (Option.is_some) (is_tany)) -> Some r
    | _ -> None
    end
  | _ -> None

let level_of_type fixme_map (p, ty) =
  let r, lvl = match ty with
    | r, Tobject -> r, Partial
    | r, _ -> match is_tany ty with
      | Some r -> r, Unchecked
      | None -> match TUtils.HasTany.check_why ty with
        | Some r -> r, Partial
        | _ -> r, Checked in
  let line = Pos.line p in
  (* If the line has a HH_FIXME, then mark it as (at most) partially checked *)
  match lvl with
  | Checked when IMap.mem line fixme_map ->
      r, Partial
  | Unchecked | Partial | Checked -> r, lvl

let level_of_type_mapper fn =
  let fixme_map = Parser_heap.HH_FIXMES.find_unsafe fn in
  level_of_type fixme_map
