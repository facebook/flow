(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Typed_ast_utils
open Utils_js

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

let concretize_loc_pairs pair_list =
  Core_list.map ~f:(fun (loc, x) -> ALoc.to_loc_exn loc, x) pair_list

let sort_loc_pairs pair_list =
  List.sort (fun (a, _) (b, _) -> Loc.compare a b) pair_list

let type_of_scheme ~options ~full_cx ~file ~file_sig typed_ast loc scheme =
  let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~file_sig ~typed_ast in
  match Ty_normalizer.from_scheme ~options ~genv scheme with
  | Ok ty -> Success (loc, ty)
  | Error err ->
    let msg = Ty_normalizer.error_to_string err in
    FailureUnparseable (loc, scheme.Type.TypeScheme.type_, msg)

let type_at_pos_type ~full_cx ~file ~file_sig ~expand_aliases ~omit_targ_defaults ~typed_ast loc =
  let options = {
    Ty_normalizer_env.
    fall_through_merged = false;
    expand_internal_types = false;
    expand_type_aliases = expand_aliases;
    flag_shadowed_type_params = false;
    preserve_inferred_literal_types = false;
    evaluate_type_destructors = false;
    optimize_types = true;
    omit_targ_defaults;
    simplify_empty = true;
  } in
  match find_type_at_pos_annotation typed_ast loc with
  | None -> FailureNoMatch
  | Some  (loc, scheme) ->
    type_of_scheme ~options ~full_cx ~file ~file_sig typed_ast loc scheme

let dump_types ~printer cx file_sig typed_ast =
  let options = {
    Ty_normalizer_env.
    fall_through_merged = false;
    expand_internal_types = false;
    expand_type_aliases = false;
    flag_shadowed_type_params = false;
    preserve_inferred_literal_types = false;
    evaluate_type_destructors = false;
    optimize_types = true;
    omit_targ_defaults = false;
    simplify_empty = true;
  } in
  let file = Context.file cx in
  let type_table = Context.type_table cx in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~typed_ast ~file_sig in
  let typed_locs = Type_table.coverage_to_list type_table in
  let result = Ty_normalizer.from_schemes ~options ~genv typed_locs in
  let print_ok = function
    | l, Ok t -> Some (l, printer t)
    | _ -> None
  in
  Core_list.filter_map result ~f:print_ok
  |> concretize_loc_pairs
  |> sort_loc_pairs

let covered_types cx ~should_check ~check_trust =
  let type_table = Context.type_table cx in
  let htbl = Type_table.coverage_hashtbl type_table in
  let check_trust =
    if check_trust then
      fun x -> x
    else
      function
      | Coverage.Tainted -> Coverage.Untainted
      | x -> x
  in
  let coverage = new Coverage.visitor in
  let compute_cov =
    if should_check
    then coverage#type_ cx %> Coverage.result_of_coverage %> check_trust
    else fun _ -> Coverage.Empty
  in
  let result_pairs =
    Hashtbl.fold (fun loc { Type.TypeScheme.type_; _ } acc ->
      (ALoc.to_loc_exn loc, compute_cov type_)::acc
    ) htbl []
  in
  sort_loc_pairs result_pairs

let component_coverage ~full_cx =
  let open Coverage in
  let coverage_computer = new visitor in
  Core_list.map ~f:(fun cx ->
    let type_table = Context.type_table cx in
    Type_table.fold_coverage (fun _ { Type.TypeScheme.type_; _ } coverage ->
      match coverage_computer#type_ full_cx type_ |> Coverage.result_of_coverage with
      | Uncovered -> { coverage with uncovered = coverage.uncovered + 1 }
      | Untainted -> { coverage with untainted = coverage.untainted + 1 }
      | Tainted   -> { coverage with tainted   = coverage.tainted   + 1 }
      | Empty     -> { coverage with empty     = coverage.empty     + 1 }
    ) type_table initial_coverage
  )

let suggest_types cx file_sig typed_ast loc =
  let options = {
    Ty_normalizer_env.
    fall_through_merged = false;
    expand_internal_types = false;
    expand_type_aliases = false;
    flag_shadowed_type_params = true;
    preserve_inferred_literal_types = false;
    evaluate_type_destructors = false;
    optimize_types = true;
    omit_targ_defaults = false;
    simplify_empty = true;
  } in
  let file = Context.file cx in
  match Typed_ast_utils.find_exact_match_annotation typed_ast loc with
  | None -> FailureNoMatch
  | Some (loc, scheme) ->
    type_of_scheme ~options ~full_cx:cx ~file ~file_sig typed_ast loc scheme
