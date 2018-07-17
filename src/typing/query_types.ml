(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************)
(* Query/Suggest *)
(*****************)

(* These computations should trigger ground_type calls on the types returned by
   query_type/suggest_types: in general those types may not be ground (the only
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

let query_type ~expand_aliases ?type_table cx loc =

  let module QueryTypeNormalizer = Ty_normalizer.Make(struct
    let opt_fall_through_merged = false
    let opt_expand_internal_types = false
    let opt_expand_type_aliases = expand_aliases
    let opt_flag_shadowed_type_params = false
  end) in

  let pred = fun range -> Reason.in_range loc range in
  let type_table = match type_table with
    | Some type_table -> type_table
    | None -> Context.type_table cx in
  match Type_table.find_type_info ~pred type_table with
  | None -> FailureNoMatch
  | Some (loc, (_, scheme, _)) ->
    (match QueryTypeNormalizer.from_scheme ~cx scheme with
    | Ok ty -> Success (loc, ty)
    | Error err ->
      let msg = Ty_normalizer.error_to_string err in
      let Type_table.Scheme (_, t) = scheme in
      FailureUnparseable (loc, t, msg))

let query_coverage_type ~expand_aliases ?type_table cx loc =

  let module QueryTypeNormalizer = Ty_normalizer.Make(struct
    let opt_fall_through_merged = false
    let opt_expand_internal_types = false
    let opt_expand_type_aliases = expand_aliases
    let opt_flag_shadowed_type_params = false
  end) in

  let type_table = match type_table with
    | Some type_table -> type_table
    | None -> Context.type_table cx in
  match Type_table.find_unsafe_coverage type_table loc with
  | exception Not_found -> FailureNoMatch
  | scheme ->
    (match QueryTypeNormalizer.from_scheme ~cx scheme with
    | Ok ty -> Success (loc, ty)
    | Error err ->
      let msg = Ty_normalizer.error_to_string err in
      let Type_table.Scheme (_, t) = scheme in
      FailureUnparseable (loc, t, msg))

module DumpTypeNormalizer = Ty_normalizer.Make(struct
  let opt_fall_through_merged = false
  let opt_expand_internal_types = false
  let opt_expand_type_aliases = false
  let opt_flag_shadowed_type_params = false
end)

let dump_types ~printer cx =
  Type_table.coverage_to_list (Context.type_table cx)
  |> DumpTypeNormalizer.from_schemes ~cx
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
  let opt_fall_through_merged = true
  let opt_expand_internal_types = false
  let opt_expand_type_aliases = false
  let opt_flag_shadowed_type_params = false
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
  let htbl = Type_table.coverage_hashtbl (Context.type_table cx) in
  CoverageTypeNormalizer.fold_hashtbl ~cx ~f ~g:(fun t -> t) ~htbl []
  |> List.sort (fun (a_loc, _) (b_loc, _) -> Loc.compare a_loc b_loc)

module SuggestTypeNormalizer = Ty_normalizer.Make(struct
  let opt_fall_through_merged = false
  let opt_expand_internal_types = false
  let opt_expand_type_aliases = false
  let opt_flag_shadowed_type_params = true
end)

(* 'suggest' can use as many types in the type tables as possible, which is why
   we are querying the tables from both "coverage" and "type_info". Coverage
   should be enough on its own, but "type_info" stores method types more
   reliably. On the other hand "type_info" only stores information about
   identifiers, so anonymous functions and arrows are not captured.
*)
let suggest_types cx =
  let type_table = Context.type_table cx in
  let result = Utils_js.LocMap.empty in
  let result = SuggestTypeNormalizer.fold_hashtbl ~cx
    ~f:(fun acc (loc, t) -> Utils_js.LocMap.add loc t acc)
    ~g:(fun t -> t)
    ~htbl:(Type_table.coverage_hashtbl type_table) result in
  let result = SuggestTypeNormalizer.fold_hashtbl ~cx
    ~f:(fun acc (loc, t) -> Utils_js.LocMap.add loc t acc)
    ~g:(fun (_, t, _) -> t)
    ~htbl:(Type_table.type_info_hashtbl type_table) result in
  result
