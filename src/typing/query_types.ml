(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Typed_ast_utils

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

let sort_loc_pairs pair_list =
  List.sort (fun (a, _) (b, _) -> Loc.compare a b) pair_list

let types_in_file ~full_cx ~file ~file_sig ~expand_aliases ~type_table typed_ast =
  let options = {
    Ty_normalizer_env.
    fall_through_merged = false;
    expand_internal_types = false;
    expand_type_aliases = expand_aliases;
    flag_shadowed_type_params = false;
  } in
  let type_scheme_list = Typed_ast_utils.typed_ast_to_list typed_ast in
  let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~file_sig ~type_table in
  let ty_list = Ty_normalizer.from_schemes
    ~options
    ~genv
    type_scheme_list
  in
  List.fold_left (fun map (loc, result) ->
    match result with
    | Ok ty -> LocMap.add loc ty map
    | Error _ -> map
  ) LocMap.empty ty_list

let query_type ~full_cx ~file ~file_sig ~expand_aliases ~type_table loc typed_ast =
  let options = {
    Ty_normalizer_env.
    fall_through_merged = false;
    expand_internal_types = false;
    expand_type_aliases = expand_aliases;
    flag_shadowed_type_params = false;
  } in
  match find_type_at_pos_annotation typed_ast loc with
  | None -> FailureNoMatch
  | Some  (loc, scheme) ->
    let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~file_sig ~type_table in
    (match Ty_normalizer.from_scheme ~options ~genv scheme with
    | Ok ty -> Success (loc, ty)
    | Error err ->
      let msg = Ty_normalizer.error_to_string err in
      FailureUnparseable (loc, scheme.Type.TypeScheme.type_, msg))

let dump_types cx file_sig ~printer =
  let options = Ty_normalizer_env.default_opts in
  let file = Context.file cx in
  let type_table = Context.type_table cx in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~type_table ~file_sig in
  let result = Ty_normalizer.from_schemes ~options ~genv
    (Type_table.coverage_to_list (Context.type_table cx)) in
  let print_ok = function
    | l, Ok t -> Some (l, printer t)
    | _ -> None
  in
  sort_loc_pairs (Core_list.filter_map result ~f:print_ok)

let is_covered = function
  | Ty.Any
  | Ty.Bot -> false
  | _ -> true

let covered_types cx file_sig ~should_check =
  let options = {
    Ty_normalizer_env.
    fall_through_merged = true;
    expand_internal_types = false;
    expand_type_aliases = false;
    flag_shadowed_type_params = false;
  } in
  let file = Context.file cx in
  let type_table = Context.type_table cx in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~type_table ~file_sig in
  let f =
    if should_check then
      fun acc (loc, result) ->
        match result with
        | Ok t -> (loc, is_covered t)::acc
        | _ -> (loc, false)::acc
    else
      fun acc (loc, _) -> (loc, false)::acc
  in
  let g x = x in
  let htbl = Type_table.coverage_hashtbl (Context.type_table cx) in
  let coverage = Ty_normalizer.fold_hashtbl ~options ~genv ~f ~g ~htbl [] in
  sort_loc_pairs coverage

(* 'suggest' can use as many types in the type tables as possible, which is why
   we are querying the tables from both "coverage" and "type_info". Coverage
   should be enough on its own, but "type_info" stores method types more
   reliably. On the other hand "type_info" only stores information about
   identifiers, so anonymous functions and arrows are not captured.
*)
let suggest_types cx file_sig =
  let options = {
    Ty_normalizer_env.
    fall_through_merged = false;
    expand_internal_types = false;
    expand_type_aliases = false;
    flag_shadowed_type_params = true;
  } in
  let type_table = Context.type_table cx in
  let file = Context.file cx in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~type_table ~file_sig in
  let result = Utils_js.LocMap.empty in
  let result = Ty_normalizer.fold_hashtbl
    ~options ~genv
    ~f:(fun acc (loc, t) -> Utils_js.LocMap.add loc t acc)
    ~g:(fun t -> t)
    ~htbl:(Type_table.coverage_hashtbl type_table) result in
  let result = Ty_normalizer.fold_hashtbl
    ~options ~genv
    ~f:(fun acc (loc, t) -> Utils_js.LocMap.add loc t acc)
    ~g:(fun (_, t, _) -> t)
    ~htbl:(Type_table.type_info_hashtbl type_table) result in
  result
