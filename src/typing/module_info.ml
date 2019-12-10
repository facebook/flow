(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * This module tracks the exports from a given file, or declared module.
 *
 * Initially, we assume we're dealing with a CommonJS module. If we see an
 * ES-style import/export, we switch over. If we see a combination of CommonJS
 * and ES-style, we complain that the module type is indeterminate.
 *
 * Note that both CommonJS and ES modules can import and export types.
 *)

type t = {
  ref: string;
  mutable kind: kind;
  mutable type_named: Type.Exports.t;
  mutable type_star: (ALoc.t * Type.t) list;
}

and kind =
  | CJS of ALoc.t option
  | ES of {
      named: Type.Exports.t;
      star: (ALoc.t * Type.t) list;
    }

let empty_cjs_module ref = { ref; kind = CJS None; type_named = SMap.empty; type_star = [] }

let export info name loc t =
  match info.kind with
  | CJS None ->
    info.kind <- ES { named = SMap.singleton name (Some loc, t); star = [] };
    Ok ()
  | ES { named; star } ->
    info.kind <- ES { named = SMap.add name (Some loc, t) named; star };
    Ok ()
  | CJS (Some _) -> Error (Error_message.EIndeterminateModuleType loc)

let export_star info loc ns =
  match info.kind with
  | CJS None ->
    info.kind <- ES { named = SMap.empty; star = [(loc, ns)] };
    Ok ()
  | ES { named; star } ->
    info.kind <- ES { named; star = (loc, ns) :: star };
    Ok ()
  | CJS (Some _) -> Error (Error_message.EIndeterminateModuleType loc)

let export_type info name loc t = info.type_named <- SMap.add name (loc, t) info.type_named

let export_type_star info loc ns = info.type_star <- (loc, ns) :: info.type_star

let cjs_clobber info loc =
  match info.kind with
  | CJS _ ->
    info.kind <- CJS (Some loc);
    Ok ()
  | ES _ -> Error (Error_message.EIndeterminateModuleType loc)

(* Re-exporting names from another file can lead to conflicts. We resolve
 * conflicts on a last-export-wins basis. Star exports are accumulated in
 * source order, so the head of each list is the last export. This helper
 * function interleaves the two reverse-sorted lists. *)
let rec fold_star2 f g acc = function
  | ([], []) -> acc
  | (xs, []) -> List.fold_left f acc xs
  | ([], ys) -> List.fold_left g acc ys
  | ((x :: xs' as xs), (y :: ys' as ys)) ->
    if ALoc.compare (fst x) (fst y) > 0 then
      fold_star2 f g (f acc x) (xs', ys)
    else
      fold_star2 f g (g acc y) (xs, ys')
