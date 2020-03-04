(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Reason
open Type

(*
  Terminology:

   * A step records a single test of lower bound against
   upper bound, analogous to an invocation of the flow function.

   * A step may have a tvar as its lower or upper bound (or both).
   tvars act as conduits for concrete types, so steps which
   begin or end in tvars may be joined with other steps
   representing tests which adjoin the same tvar.

   The resulting sequence of steps, corresponding to an invocation
   of the flow function followed by the extension of the original
   lower/upper pair through any adjacent type variables, forms the
   basis of a trace. (In trace dumps this is called a "path".)

   * When a step has been induced recursively from a prior invocation
   of the flow function, it's said to have the trace associated with
   that invocation as a parent.

   (Note that each step in a path may have its own parent: consider
   an incoming, recursively induced step joining with a dormant step
   attached to some tvar in an arbitrarily removed invocation of the
   flow function.)

   * A trace is just a sequence of steps along with a (possibly empty)
   parent trace for each step. Since steps may share parents,
   a trace forms a graph, though it is naturally built up as a tree
   when recorded during evaluation of the flow function.
   (The formatting we do in reasons_of_trace recovers the graph
   structure for readability.)
 *)
type step =
  | Step of {
      lower: Type.t;
      upper: Type.use_t;
      parent: step list;
    }

(* A list of steps and the depth of the trace, trace depth is 1 + the length of
   the longest ancestor chain in the trace. We keep this precomputed because

   a) actual ancestors may be thrown away due to externally imposed limits on trace
      depth;

   b) the recursion limiter in the flow function checks this on every call.
 *)
and t = step list * int

let compare = Pervasives.compare

let trace_depth = snd

let dummy_trace = ([], 0)

(* Single-step trace with no parent. This corresponds to a
   top-level invocation of the flow function, e.g. due to
   a constraint generated in Type_inference_js *)
let unit_trace lower upper : t = ([Step { lower; upper; parent = [] }], 1)

(* Single-step trace with a parent. This corresponds to a
   recursive invocation of the flow function.
   Optimization: only embed when modes.trace > 0,
   because otherwise we're not going to see any traces anyway.
*)
let rec_trace ~max lower upper parent =
  let parent_depth = trace_depth parent in
  let parent =
    if max > 0 then
      fst parent
    else
      []
  in
  ([Step { lower; upper; parent }], parent_depth + 1)

(* Join a list of traces. If the maximum depth of traces is configured to 0, then
   agressively throw away traces and only compute the updated depth. *)
let concat_trace ~max ts =
  let d = List.fold_left (fun acc (_, d) -> Pervasives.max acc d) 0 ts in
  let steps =
    if max > 0 then
      Base.List.concat_map ~f:fst ts
    else
      []
  in
  (steps, d)

(* used to index trace nodes *)
module TraceMap : WrappedMap.S with type key = step list = WrappedMap.Make (struct
  type key = step list

  type t = key

  let compare = compare
end)

(* index the nodes in a trace down to a given level.
   returns two maps, trace -> index and index -> trace
 *)
let index_trace =
  let rec f (level, tmap, imap) trace =
    if level <= 0 || TraceMap.mem trace tmap then
      (level, tmap, imap)
    else
      let (tmap, imap) =
        let i = TraceMap.cardinal tmap in
        (TraceMap.(add trace i tmap), IMap.(add i trace imap))
      in
      List.fold_left
        (fun acc (Step { parent; _ }) ->
          match parent with
          | [] -> acc
          | _ -> f acc parent)
        (level - 1, tmap, imap)
        trace
  in
  fun level (trace : t) ->
    let (_, tmap, imap) = f (level, TraceMap.empty, IMap.empty) (fst trace) in
    (tmap, imap)

(* scan a trace tree, return maximum position length
   of reasons at or above the given depth limit, and
   min of that limit and actual max depth *)
let max_depth_of_trace limit (trace : t) =
  let rec f depth (Step { parent; _ }) =
    if depth > limit then
      depth
    else
      match parent with
      | [] -> depth
      | trace -> List.fold_left f (depth + 1) trace
  in
  List.fold_left f 1 (fst trace)

(* reformat a reason's description with
   - the given prefix and suffix: if either is nonempty,
     "desc" becomes "prefix[desc]suffix"
  *)
let pretty_r r prefix suffix =
  update_desc_new_reason
    (fun desc ->
      let desc_str = string_of_desc desc in
      let custom =
        if prefix = "" && suffix = "" then
          desc_str
        else
          spf "%s[%s]%s" prefix desc_str suffix
      in
      RCustom custom)
    r

(* prettyprint a trace. what we print:

   - a list of paths, numbered 1..n, root first.

   - for each path, its list of steps.
     usually a step is 2 main lines, one each for lower and upper.
     but we elide the former if its a tvar that was also the  prior
     step's upper.
     if the step was derived from another path, we append a note
     to that effect.
 *)
let reasons_of_trace ?(level = 0) trace =
  let max_depth = max_depth_of_trace level trace in
  let level = min level max_depth in
  let (tmap, imap) = index_trace level trace in
  let is_pipelined_tvar ~steps ~i lower =
    i > 0
    &&
    let upper =
      match List.nth steps (i - 1) with
      | Step { upper; _ } -> upper
    in
    match upper with
    | UseT (_, upper) -> lower = upper
    | _ -> false
  in
  let print_step (steps : step list) i (Step { lower; upper; parent }) =
    (* omit lower if it's a pipelined tvar *)
    ( if is_pipelined_tvar ~steps ~i lower then
      []
    else
      [pretty_r (reason_of_t_add_id lower) (spf "%s " (string_of_ctor lower)) ""] )
    @ [
        pretty_r
          (reason_of_use_t_add_id upper)
          (spf "~> %s " (string_of_use_ctor upper))
          ( if parent = [] then
            ""
          else
            match TraceMap.find_opt parent tmap with
            | Some i -> spf " (from path %d)" (i + 1)
            | None -> " (from [not shown])" );
      ]
  in
  let print_path i (steps : step list) =
    let desc = RCustom (spf "* path %d:" (i + 1)) in
    locationless_reason desc :: List.concat (List.mapi (print_step steps) steps)
  in
  List.concat (List.rev (IMap.fold (fun i flow acc -> print_path i flow :: acc) imap []))
