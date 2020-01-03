(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

let concretize_loc_pairs pair_list =
  Base.List.map ~f:(fun (loc, x) -> (ALoc.to_loc_exn loc, x)) pair_list

let sort_loc_pairs pair_list = List.sort (fun (a, _) (b, _) -> Loc.compare a b) pair_list

let type_of_scheme ~options ~full_cx ~file ~file_sig typed_ast loc scheme =
  let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~file_sig ~typed_ast in
  match Ty_normalizer.from_scheme ~options ~genv scheme with
  | Ok ty -> Success (loc, ty)
  | Error err ->
    let msg = Ty_normalizer.error_to_string err in
    FailureUnparseable (loc, scheme.Type.TypeScheme.type_, msg)

let type_at_pos_type
    ~full_cx
    ~file
    ~file_sig
    ~expand_aliases
    ~omit_targ_defaults
    ~evaluate_type_destructors
    ~typed_ast
    loc =
  let options =
    {
      Ty_normalizer_env.fall_through_merged = false;
      expand_internal_types = false;
      expand_type_aliases = expand_aliases;
      flag_shadowed_type_params = false;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors;
      optimize_types = true;
      omit_targ_defaults;
      merge_bot_and_any_kinds = true;
    }
  in
  match find_type_at_pos_annotation typed_ast loc with
  | None -> FailureNoMatch
  | Some (loc, scheme) -> type_of_scheme ~options ~full_cx ~file ~file_sig typed_ast loc scheme

let dump_types ~printer ~expand_aliases ~evaluate_type_destructors cx file_sig typed_ast =
  let options =
    {
      Ty_normalizer_env.default_options with
      Ty_normalizer_env.expand_type_aliases = expand_aliases;
      evaluate_type_destructors;
    }
  in
  let file = Context.file cx in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~typed_ast ~file_sig in
  let result =
    Ty_normalizer.from_schemes ~options ~genv (Typed_ast_utils.typed_ast_to_list typed_ast)
  in
  let print_ok = function
    | (l, Ok t) -> Some (l, printer t)
    | _ -> None
  in
  Base.List.filter_map result ~f:print_ok |> concretize_loc_pairs |> sort_loc_pairs

let suggest_types cx file_sig typed_ast loc =
  let options =
    { Ty_normalizer_env.default_options with Ty_normalizer_env.flag_shadowed_type_params = true }
  in
  let file = Context.file cx in
  let aLoc = ALoc.of_loc loc in
  match Typed_ast_utils.find_exact_match_annotation typed_ast aLoc with
  | None -> FailureNoMatch
  | Some scheme -> type_of_scheme ~options ~full_cx:cx ~file ~file_sig typed_ast loc scheme

let insert_type_normalize
    ~full_cx
    ?(file = Context.file full_cx)
    ~file_sig
    ~expand_aliases
    ~omit_targ_defaults
    ~typed_ast
    loc
    scheme =
  let options =
    {
      Ty_normalizer_env.fall_through_merged = false;
      expand_internal_types = false;
      expand_type_aliases = expand_aliases;
      (* Shadowed type parameters won't be valid for type insertion *)
      flag_shadowed_type_params = true;
      (* Insert-Types filters out literals at the users request.
       * Setting this flag preserves literal information so the we later
       * have the option of presenting it to the user in specialized types. *)
      preserve_inferred_literal_types = true;
      (* Utility types won't are not serialized so it may be worth evaluating them away
       * if we find them in the resulting Ty.t. The trade off is that types might get
       * larger. *)
      evaluate_type_destructors = false;
      (* Optimize types is false because Insert_types manually calls the simplifier with
       a custom comparison operation *)
      optimize_types = false;
      omit_targ_defaults;
      merge_bot_and_any_kinds = true;
    }
  in
  type_of_scheme ~options ~full_cx ~file ~file_sig typed_ast loc scheme
