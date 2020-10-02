(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base.Result

let ( >|= ) = Lwt.( >|= )

let type_at_pos
    ~cx
    ~file_sig
    ~typed_ast
    ~expand_aliases
    ~omit_targ_defaults
    ~evaluate_type_destructors
    ~max_depth
    ~verbose_normalizer
    file
    line
    col =
  let loc = Loc.make file line col in
  let (json_data, loc, ty) =
    let mk_data result_str loc ty_json =
      [
        ("result", Hh_json.JSON_String result_str);
        ("loc", Reason.json_of_loc ~offset_table:None loc);
        ("type", ty_json);
      ]
    in
    Query_types.(
      let file = Context.file cx in
      let result =
        type_at_pos_type
          ~full_cx:cx
          ~file
          ~file_sig:(File_sig.abstractify_locs file_sig)
          ~expand_aliases
          ~omit_targ_defaults
          ~evaluate_type_destructors
          ~verbose_normalizer
          ~max_depth
          ~typed_ast
          loc
      in
      match result with
      | FailureNoMatch -> ([("result", Hh_json.JSON_String "FAILURE_NO_MATCH")], Loc.none, None)
      | FailureUnparseable (loc, gt, _) ->
        let json = Hh_json.JSON_String (Type.string_of_ctor gt) in
        (mk_data "FAILURE_UNPARSEABLE" loc json, loc, None)
      | Success (loc, ty) ->
        (* TODO use Ty_debug.json_of_t after making it faster using
             count_calls *)
        let exact_by_default = Context.exact_by_default cx in
        let json = Hh_json.JSON_String (Ty_printer.string_of_elt ~exact_by_default ty) in
        (mk_data "SUCCESS" loc json, loc, Some ty))
  in
  ((loc, ty), json_data)

let dump_types ~expand_aliases ~evaluate_type_destructors cx file_sig typed_ast =
  (* Print type using Flow type syntax *)
  let exact_by_default = Context.exact_by_default cx in
  let printer = Ty_printer.string_of_elt_single_line ~exact_by_default in
  let abs_file_sig = File_sig.abstractify_locs file_sig in
  Query_types.dump_types
    ~printer
    ~expand_aliases
    ~evaluate_type_destructors
    cx
    abs_file_sig
    typed_ast

let coverage ~cx ~typed_ast ~force ~trust file content =
  let should_check =
    if force then
      true
    else
      (* We can't just use the docblock that type_contents returns because type_contents modifies
      * it and we want the original docblock. Fortunately this is a pure function, and pretty fast,
      * so recomputing it isn't a problem. *)
      let (_, docblock) = Parsing_service_js.(parse_docblock docblock_max_tokens file content) in
      Docblock.is_flow docblock
  in
  Coverage.covered_types cx ~should_check ~check_trust:trust typed_ast

let suggest ~options ~env ~profiling file_key file_content =
  Types_js.typecheck_contents ~options ~env ~profiling file_content file_key >|= function
  | (Some (cx, ast, file_sig, _, tast), tc_errors, tc_warnings) ->
    let file_sig = File_sig.abstractify_locs file_sig in
    let ty_query = Query_types.suggest_types cx file_sig tast in
    let exact_by_default = Options.exact_by_default options in
    let visitor = new Suggest.visitor ~exact_by_default ~ty_query in
    let ast_with_suggestions = visitor#program ast in
    let suggest_warnings = visitor#warnings () in
    let ast_diff = Flow_ast_differ.(program Standard ast ast_with_suggestions) in
    let file_patch = Replacement_printer.mk_patch_ast_differ ast_diff file_content in
    Ok (tc_errors, tc_warnings, suggest_warnings, file_patch)
  | (None, errors, _) -> Error errors
