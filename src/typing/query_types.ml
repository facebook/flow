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

type result =
| FailureNoMatch
| FailureUnparseable of Loc.t * Type.t * string
| Success of Loc.t * Ty.t

module QueryTypeNormalizer = Ty_normalizer.Make(struct
  let fall_through_merged = false
  let expand_internal_types = false
  let expand_annots = false
end)

let query_type cx loc =
  let pred = fun range -> Reason.in_range loc range in
  let type_table = Context.type_table cx in
  match Type_table.find_type_info ~pred type_table with
  | None -> FailureNoMatch
  | Some (loc, (_, t, _)) ->
    (match QueryTypeNormalizer.from_type ~cx t with
    | Ok ty -> Success (loc, ty)
    | Error msg ->
      FailureUnparseable (loc, t, Ty_normalizer.error_to_string msg))


module DumpTypeNormalizer = Ty_normalizer.Make(struct
  let fall_through_merged = false
  let expand_internal_types = false
  let expand_annots = false
end)

let dump_types ~printer cx =
  Type_table.coverage_to_list (Context.type_table cx)
  |> DumpTypeNormalizer.from_types ~cx
  |> Core_list.filter_map ~f:(function
    | l, Ok t -> Some (l, printer t)
    | _ -> None
  )
  |> List.sort (fun (a, _) (b, _) -> Loc.compare a b)

let is_covered = function
  | Ty.Any
  | Ty.Bot -> false
  | _ -> true

module CoverageTypeNormalizer = Ty_normalizer.Make(struct
  let fall_through_merged = true
  let expand_internal_types = false
  let expand_annots = false
end)

let covered_types cx ~should_check =
  let f =
    if should_check then
      fun acc (loc, result) ->
        match result with
        | Ok t -> (loc, is_covered t)::acc
        | _ -> (loc, false)::acc
    else
      fun acc (loc, _) -> (loc, false)::acc
  in
  Context.type_table cx
  |> Type_table.coverage_hashtbl
  |> CoverageTypeNormalizer.fold_hashtbl ~cx ~f ~init:[]
  |> List.sort (fun (a_loc, _) (b_loc, _) -> Loc.compare a_loc b_loc)


(********)
(* Fill *)
(********)

module FillTypeNormalizer = Ty_normalizer.Make(struct
  let fall_through_merged = false
  let expand_internal_types = false
  let expand_annots = false
end)

let fill_types cx =
  Type_table.coverage_to_list (Context.type_table cx)
  |> FillTypeNormalizer.from_types ~cx
  |> Core_list.filter_map ~f:(function
   | l, Ok s -> Some (l, s)
   | _ -> None
   )
  |> List.map Loc.(fun (l, t) -> (l._end.line, l._end.column, t))
