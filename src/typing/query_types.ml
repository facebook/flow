(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**************)
(* Query/Fill *)
(**************)

(* These computations should trigger ground_type calls on the types returned by
   query_type/fill_types: in general those types may not be ground (the only
   non-ground parts should be strict_requires).

   1. Look up ResolvedRequiresHeap(Context.file cx) to get strict_reqs.

   2. Look up ContextHeap(NameHeap(strict_req)) to get strict_cxs that cx
   depends on, and so on.

   3. Next, look up their exported types via recursive calls to
   lookup_type(lookup_module(strict_cx, Context.module_name strict_cx)).

   In fact, 2. and 3. could be optimized: we could store exported types
   (possibly not ground) in InfoHeap, so that they are cached. This means that
   we could look up InfoHeap(NameHeap(strict_req)) to get strict_req_types
   directly, instead of going through ContextHeap.

   Note that exported types do not need to be blown away unless their files
   change, since they are locally determined; instead, we ground them as
   necessary.
*)

open Utils_js

type result =
| FailureNoMatch
| FailureUnparseable of Loc.t * Type.t * Type.t list
| Success of Loc.t * Type.t * Type.t list

let query_type cx loc =
  let result = ref FailureNoMatch in
  let diff = ref (max_int, max_int) in
  Type_table.iter (fun range t ->
    if Reason.in_range loc range
    then (
      let d = Reason.diff_range range in
      if d < !diff then (
        diff := d;
        Type_normalizer.suggested_type_cache := IMap.empty;
        let ground_t = Type_normalizer.normalize_type cx t in
        let possible_ts = Flow_js.possible_types_of_type cx t in
        result := if Type_printer.is_printed_type_parsable cx ground_t
          then Success (range, ground_t, possible_ts)
          else FailureUnparseable (range, ground_t, possible_ts)
      )
    )
  ) (Context.type_table cx);
  !result

let dump_types printer cx =
  Type_normalizer.suggested_type_cache := IMap.empty;
  let lst = Type_table.fold (fun loc t list ->
    let ground_t = Type_normalizer.normalize_type cx t in
    let possible_ts = Flow_js.possible_types_of_type cx t in
    let possible_reasons = possible_ts
      |> List.map Type.reason_of_t
    in
    let ctor = Type.string_of_ctor ground_t in
    let pretty = printer cx ground_t in
    (loc, ctor, pretty, possible_reasons)::list
  ) (Context.type_table cx) [] in
  lst |> List.sort (fun
    (a_loc, _, _, _) (b_loc, _, _, _) -> Loc.compare a_loc b_loc
  )

let is_covered = function
  | Type.DefT (_, Type.AnyT)
  | Type.DefT (_, Type.EmptyT) -> false
  | _ -> true

let covered_types cx =
  Type_normalizer.suggested_type_cache := IMap.empty;
  let lst = Type_table.fold (fun loc t list ->
    let ground_t = Type_normalizer.normalize_type cx t in
    (loc, is_covered ground_t)::list
  ) (Context.type_table cx) [] in
  lst |> List.sort (fun
    (a_loc, _) (b_loc, _) -> Loc.compare a_loc b_loc
  )


(********)
(* Fill *)
(********)

let fill_types cx =
  Type_normalizer.suggested_type_cache := IMap.empty;
  Hashtbl.fold Loc.(fun loc t list ->
    let line = loc._end.line in
    let end_ = loc._end.column in
    let t = Type_normalizer.normalize_type cx t in
    if Type_printer.is_printed_type_parsable cx t then
      (line, end_, spf ": %s" (Type_printer.string_of_t cx t))::list
    else list
  ) (Context.annot_table cx) []
