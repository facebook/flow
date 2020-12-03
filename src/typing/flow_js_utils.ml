(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Constraint

(* These possible_* functions would ideally be in constraint.ml, but since they use
 * Context and Context depends on Constraint we need to extract these functions
 * to a separate module in order to avoid a circular dependency *)

(* Def types that describe the solution of a type variable. *)
let possible_types cx id = types_of (Context.find_graph cx id) |> List.filter is_proper_def

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let possible_uses cx id = uses_of (Context.find_graph cx id) |> List.filter is_proper_use

let merge_tvar =
  let possible_types = possible_types in
  let rec collect_lowers ~filter_empty cx seen acc = function
    | [] -> Base.List.rev acc
    | t :: ts ->
      (match t with
      (* Recursively unwrap unseen tvars *)
      | OpenT (_, id) ->
        if ISet.mem id seen then
          collect_lowers ~filter_empty cx seen acc ts
        (* already unwrapped *)
        else
          let seen = ISet.add id seen in
          collect_lowers ~filter_empty cx seen acc (possible_types cx id @ ts)
      (* Ignore empty in existentials. This behavior is sketchy, but the error
           behavior without this filtering is worse. If an existential accumulates
           an empty, we error but it's very non-obvious how the empty arose. *)
      | DefT (_, _, EmptyT flavor) when filter_empty flavor ->
        collect_lowers ~filter_empty cx seen acc ts
      (* Everything else becomes part of the merge typed *)
      | _ -> collect_lowers ~filter_empty cx seen (t :: acc) ts)
  in
  fun ?filter_empty cx r id ->
    (* Because the behavior of existentials are so difficult to predict, they
         enjoy some special casing here. When existential types are finally
         removed, this logic can be removed. *)
    let existential =
      Reason.(
        match desc_of_reason r with
        | RExistential -> true
        | _ -> false)
    in
    let filter_empty flavor =
      existential
      ||
      match filter_empty with
      | Some filter_empty -> filter_empty flavor
      | None -> false
    in
    let lowers =
      let seen = ISet.singleton id in
      collect_lowers cx seen [] (possible_types cx id) ~filter_empty
    in
    match lowers with
    | [t] -> t
    | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)
    | [] ->
      let uses = possible_uses cx id in
      if uses = [] || existential then
        AnyT.locationless Unsoundness.existential
      else
        MergedT (r, uses)
