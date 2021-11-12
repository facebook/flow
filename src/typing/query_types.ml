(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Typed_ast_utils

type result =
  | FailureNoMatch
  | FailureUnparseable of Loc.t * Type.t * string
  | Success of Loc.t * Ty.elt

let concretize_loc_pairs pair_list =
  Base.List.map ~f:(fun (loc, x) -> (ALoc.to_loc_exn loc, x)) pair_list

let sort_loc_pairs pair_list = List.sort (fun (a, _) (b, _) -> Loc.compare a b) pair_list

let type_of_scheme ~options ~full_cx ~file ~file_sig typed_ast loc scheme =
  let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~file_sig ~typed_ast in
  match Ty_normalizer.from_scheme ~options ~genv scheme with
  | Ok elt -> Success (loc, elt)
  | Error err ->
    let msg = Ty_normalizer.error_to_string err in
    FailureUnparseable (loc, scheme.Type.TypeScheme.type_, msg)

let type_at_pos_type
    ~full_cx
    ~file
    ~file_sig
    ~omit_targ_defaults
    ~evaluate_type_destructors
    ~verbose_normalizer
    ~max_depth
    ~typed_ast
    loc =
  let options =
    {
      Ty_normalizer_env.expand_internal_types = false;
      flag_shadowed_type_params = false;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors;
      optimize_types = true;
      omit_targ_defaults;
      merge_bot_and_any_kinds = true;
      verbose_normalizer;
      max_depth = Some max_depth;
    }
  in
  match find_type_at_pos_annotation typed_ast loc with
  | None -> FailureNoMatch
  | Some (loc, scheme) -> type_of_scheme ~options ~full_cx ~file ~file_sig typed_ast loc scheme

let dump_types ~printer ~evaluate_type_destructors cx file_sig typed_ast =
  let options =
    { Ty_normalizer_env.default_options with Ty_normalizer_env.evaluate_type_destructors }
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

let insert_type_normalize
    ~full_cx ?(file = Context.file full_cx) ~file_sig ~omit_targ_defaults ~typed_ast loc scheme =
  let options =
    {
      Ty_normalizer_env.expand_internal_types = false;
      (* Shadowed type parameters won't be valid for type insertion *)
      flag_shadowed_type_params = true;
      (* We eventually want to elimitate literal types, so let's not expose them here. *)
      preserve_inferred_literal_types = false;
      (* Utility types won't are not serialized so it may be worth evaluating them away
       * if we find them in the resulting Ty.t. The trade off is that types might get
       * larger. *)
      evaluate_type_destructors = false;
      (* Optimize types is false because Insert_types manually calls the simplifier with
         a custom comparison operation *)
      optimize_types = false;
      omit_targ_defaults;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = None;
    }
  in
  type_of_scheme ~options ~full_cx ~file ~file_sig typed_ast loc scheme
