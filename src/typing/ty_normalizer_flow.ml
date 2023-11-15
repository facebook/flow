(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Ty_normalizer
open NormalizerMonad

(* Exposed API *)

let print_normalizer_banner env =
  if env.Env.verbose_normalizer then
    let banner =
      "\n========================================"
      ^ " Normalization "
      ^ "=======================================\n"
    in
    prerr_endlinef "%s" banner

let from_schemes ~options ~genv schemes =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (_, result) =
    Base.List.fold_map
      ~f:(fun state (a, scheme) ->
        let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
        match run_type ~options ~genv ~imported_names ~tparams_rev state t with
        | (Ok t, state) -> (state, (a, Ok t))
        | (Error s, state) -> (state, (a, Error s)))
      ~init:State.empty
      schemes
  in
  result

let from_types ~options ~genv ts =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (_, result) =
    Base.List.fold_map
      ~f:(fun state (a, t) ->
        match run_type ~options ~genv ~imported_names ~tparams_rev:[] state t with
        | (Ok t, state) -> (state, (a, Ok t))
        | (Error s, state) -> (state, (a, Error s)))
      ~init:State.empty
      ts
  in
  result

let from_scheme ~options ~genv scheme =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams_rev State.empty t in
  result

let from_type ~options ~genv t =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams_rev:[] State.empty t in
  result

let expand_members ~force_instance ~options ~genv scheme =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
  let (result, _) =
    run_expand_members ~options ~genv ~force_instance ~imported_names ~tparams_rev State.empty t
  in
  result

let expand_literal_union ~options ~genv scheme =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
  let (result, _) =
    run_expand_literal_union ~options ~genv ~imported_names ~tparams_rev State.empty t
  in
  result

let debug_string_of_t cx t =
  let typed_ast =
    ( ALoc.none,
      { Flow_ast.Program.statements = []; interpreter = None; comments = None; all_comments = [] }
    )
  in
  let file_sig = { File_sig.requires = []; module_kind = File_sig.ES } in
  let genv = Ty_normalizer_env.mk_genv ~cx ~file:(Context.file cx) ~file_sig ~typed_ast in
  match from_type ~options:Ty_normalizer_env.default_options ~genv t with
  | Error (e, _) -> Utils_js.spf "<Error %s>" (error_kind_to_string e)
  | Ok elt -> Ty_printer.string_of_elt_single_line ~exact_by_default:true elt
