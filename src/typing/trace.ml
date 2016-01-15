(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Reason_js
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
type step = Type.t * Type.use_t * parent * int
and t = step list
and parent = Parent of t

let compare = Pervasives.compare

(* trace depth is 1 + the length of the longest ancestor chain
   in the trace. We keep this precomputed because a) actual ancestors
   may be thrown away due to externally imposed limits on trace depth;
   b) the recursion limiter in the flow function checks this on every
   call. *)
let trace_depth trace =
  List.fold_left (fun acc (_, _, _, d) -> max acc d) 0 trace

(* Single-step trace with no parent. This corresponds to a
   top-level invocation of the flow function, e.g. due to
   a constraint generated in Type_inference_js *)
let unit_trace lower upper =
  [lower, upper, Parent [], 1]

(* Single-step trace with a parent. This corresponds to a
   recursive invocation of the flow function.
   Optimization: only embed when modes.trace > 0,
   because otherwise we're not going to see any traces anyway.
*)
let rec_trace lower upper parent =
  let parent_depth = trace_depth parent in
  let parent = if Modes_js.(modes.traces) > 0 then parent else [] in
  [lower, upper, Parent parent, parent_depth + 1]

(* join a list of traces *)
let concat_trace = List.concat


(* used to index trace nodes *)
module TraceMap : MapSig with type key = t = MyMap(struct
  type key = t
  type t = key
  let compare = compare
end)


(* index the nodes in a trace down to a given level.
   returns two maps, trace -> index and index -> trace
 *)
let index_trace =
  let rec f (level, tmap, imap) trace =
    if level <= 0 || TraceMap.mem trace tmap
    then level, tmap, imap
    else (
      let tmap, imap =
        let i = TraceMap.cardinal tmap in
        TraceMap.(add trace i tmap), IMap.(add i trace imap)
      in
      List.fold_left (fun acc (_, _, Parent parent, _) ->
        match parent with [] -> acc | _ -> f acc parent
      ) (level - 1, tmap, imap) trace
    )
  in
  fun level trace ->
    let _, tmap, imap = f (level, TraceMap.empty, IMap.empty) trace in
    tmap, imap


let spaces n = String.make n ' '


let prep_path r =
  if not Modes_js.(modes.strip_root) then r
  else
    let path = FlowConfig.((get_unsafe ()).root) in
    Reason_js.strip_root path r


(* string length of printed position, as it would
   appear in an error *)
let pos_len r =
  let r = prep_path r in
  let loc = loc_of_reason r in
  let fmt = Errors_js.(format_reason_color (BlameM (loc, ""))) in
  let str = String.concat "" (List.map snd fmt) in
  String.length str


(* scan a trace tree, return maximum position length
   of reasons at or above the given depth limit, and
   min of that limit and actual max depth *)
let max_pos_len_and_depth limit trace =
  let rec f (len, depth) (lower, upper, parent, _) =
    let len = max len (pos_len (reason_of_t lower)) in
    let len = max len (pos_len (reason_of_use_t upper)) in
    if depth > limit then len, depth
    else (
      match parent with
      | Parent [] -> len, depth
      | Parent trace -> List.fold_left f (len, depth + 1) trace
    )
  in List.fold_left f (0, 0) trace


(* reformat a reason's description with
   - the given left margin
   - the given prefix and suffix: if either is nonempty,
     "desc" becomes "prefix[desc]suffix"
  *)
let pretty_r margin r prefix suffix =
  let len = pos_len r in
  let ind = if margin > len then spaces (margin - len) else "" in
  if prefix = "" && suffix = ""
  then prefix_reason ind r
  else wrap_reason (ind ^ (spf "%s[" prefix)) (spf "]%s" suffix) r


(* prettyprint a trace. what we print:

   - a list of paths, numbered 1..n, root first.

   - for each path, its list of steps.
     usually a step is 2 main lines, one each for lower and upper.
     but we elide the former if its a tvar that was also the  prior
     step's upper.
     if the step was derived from another path, we append a note
     to that effect.
 *)
let reasons_of_trace ?(level=0) trace =
  let max_pos_len, max_depth = max_pos_len_and_depth level trace in
  let level = min level max_depth in

  let tmap, imap = index_trace level trace in

  let print_step (steps: step list) i (lower, upper, Parent parent, _) =
    (* omit lower if it's a pipelined tvar *)
    (if i > 0 &&
      UseT lower = (match List.nth steps (i - 1) with (_, upper, _, _) -> upper)
    then []
    else [pretty_r max_pos_len (reason_of_t_add_id lower)
      (spf "%s " (string_of_ctor lower)) ""]
    )
    @
    [pretty_r max_pos_len (reason_of_use_t_add_id upper)
      (spf "~> %s " (string_of_use_ctor upper))
      (if parent = []
        then ""
        else match TraceMap.get parent tmap with
        | Some i -> spf " (from path %d)" (i + 1)
        | None -> " (from [not shown])"
      )
    ]
  in

  let print_path i (steps: step list) =
    (reason_of_string (spf "* path %d:" (i + 1))) ::
    List.concat (List.mapi (print_step steps) steps)
  in

  List.concat (List.rev (IMap.fold (
    fun i flow acc -> (print_path i flow) :: acc
  ) imap []))
