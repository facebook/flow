(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
module Env = Ty_normalizer_env

module Normalizer = Ty_normalizer.Make (struct
  open Reason
  open Ty_normalizer

  let eval cx ~should_eval ~cont ~default ~non_eval (t, d, id) =
    let (Type.TypeDestructorT (use_op, reason, d)) = d in
    if should_eval then
      let tout = Flow_js.mk_type_destructor cx use_op reason t d id in
      match Lookahead.peek cx tout with
      | Lookahead.LowerBounds [t] -> cont t
      | _ -> default tout
    else
      non_eval t d

  let keys cx ~should_evaluate ~cont ~default r t =
    if should_evaluate then
      let tout =
        Tvar.mk_where cx r (fun tout ->
            Flow_js.flow cx (t, T.GetKeysT (r, T.UseT (T.unknown_use, tout)))
        )
      in
      match Lookahead.peek cx tout with
      | Lookahead.LowerBounds [t] ->
        cont (TypeUtil.mod_reason_of_t (replace_desc_reason (RCustom "get keys")) t)
      | _ -> default ()
    else
      default ()

  let typeapp cx ~cont ~type_:_ ~app:_ ~from_value reason t targs =
    let t =
      Flow_js.mk_typeapp_instance_annot
        cx
        ~use_op:Type.unknown_use
        ~reason_op:reason
        ~reason_tapp:reason
        ~from_value
        t
        targs
    in
    cont t

  let builtin_type cx ~cont reason name =
    let t = Flow_js.get_builtin_type cx reason name in
    cont t

  let builtin_typeapp cx ~cont ~type_:_ ~app:_ reason name targs =
    let t = Flow_js_utils.lookup_builtin_type cx name reason in
    let t = TypeUtil.typeapp ~from_value:false ~use_desc:false reason t targs in
    cont t
end)

open Normalizer

(* Exposed API *)

let print_normalizer_banner genv =
  if Env.(genv.options.verbose_normalizer) then
    let banner =
      "\n========================================"
      ^ " Normalization "
      ^ "=======================================\n"
    in
    prerr_endlinef "%s" banner

let from_types genv ts =
  print_normalizer_banner genv;
  let (_, result) =
    Base.List.fold_map
      ~f:(fun state (a, t) ->
        match run_type ~genv state t with
        | (Ok t, state) -> (state, (a, Ok t))
        | (Error s, state) -> (state, (a, Error s)))
      ~init:State.empty
      ts
  in
  result

let from_type_with_found_computed_type genv t =
  print_normalizer_banner genv;
  let (result, state) = run_type ~genv State.empty t in
  (result, State.found_computed_type state)

let from_type genv t =
  print_normalizer_banner genv;
  let (result, _) = run_type ~genv State.empty t in
  result

let expand_members ~force_instance genv t =
  print_normalizer_banner genv;
  let (result, _) = run_expand_members ~genv ~force_instance State.empty t in
  result

let expand_literal_union genv t =
  print_normalizer_banner genv;
  let (result, _) = run_expand_literal_union ~genv State.empty t in
  result

let mk_genv ~options ~cx ~typed_ast_opt ~file_sig =
  let imported_names =
    lazy
      (normalize_imports
         cx
         file_sig
         typed_ast_opt
         options
         (Ty_normalizer_imports.extract_types cx file_sig typed_ast_opt)
      )
  in
  { Ty_normalizer_env.options; cx; typed_ast_opt; file_sig; imported_names }

let debug_string_of_t cx t =
  let typed_ast =
    ( ALoc.none,
      { Flow_ast.Program.statements = []; interpreter = None; comments = None; all_comments = [] }
    )
  in
  let file_sig = File_sig.empty in
  let options = Ty_normalizer_env.default_options in
  let genv = mk_genv ~options ~cx ~file_sig ~typed_ast_opt:(Some typed_ast) in
  match from_type genv t with
  | Error (e, _) -> Utils_js.spf "<Error %s>" (Ty_normalizer.error_kind_to_string e)
  | Ok elt -> Ty_printer.string_of_elt_single_line ~exact_by_default:true elt
