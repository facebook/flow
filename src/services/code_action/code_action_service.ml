(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Types_js_types
open Code_action_utils

let add_missing_imports_kind = Lsp.CodeActionKind.kind_of_string "source.addMissingImports.flow"

let include_code_action ~only kind =
  match only with
  | None -> true
  | Some only -> Base.List.exists ~f:(fun prefix -> Lsp.CodeActionKind.is_kind prefix kind) only

let include_quick_fixes only = include_code_action ~only Lsp.CodeActionKind.quickfix

let include_extract_refactors only = include_code_action ~only Lsp.CodeActionKind.refactor_extract

let include_rewrite_refactors only = include_code_action ~only Lsp.CodeActionKind.refactor_rewrite

let include_add_missing_imports_action only = include_code_action ~only add_missing_imports_kind

let include_organize_imports_actions only =
  include_code_action ~only Lsp.CodeActionKind.source_organize_imports

let flow_loc_patch_to_lsp_edits =
  Base.List.map ~f:(fun (loc, text) ->
      { Lsp.TextEdit.range = Lsp.loc_to_lsp_range loc; newText = text }
  )

let mk_log_command ~title ~diagnostic_title =
  let open Lsp in
  {
    Command.title = "";
    command = Command.Command "log";
    arguments =
      [
        Hh_json.JSON_String "textDocument/codeAction";
        Hh_json.JSON_String diagnostic_title;
        Hh_json.JSON_String title;
      ];
  }

let autofix_insert_type_annotation_helper ~options ~ast ~diagnostics ~uri new_ast =
  let open Lsp in
  let diff = Insert_type.mk_diff ast new_ast in
  let opts = layout_options options in
  let edits =
    Replacement_printer.mk_loc_patch_ast_differ ~opts diff
    |> Base.List.map ~f:(fun (loc, text) ->
           { Lsp.TextEdit.range = Lsp.loc_to_lsp_range loc; newText = text }
       )
  in
  let title = "Insert type annotation to fix signature-verification-failure error" in
  [
    CodeAction.Action
      {
        CodeAction.title;
        kind = CodeActionKind.quickfix;
        (* Handing back the diagnostics we were given is a placeholder for
           eventually generating the diagnostics for the errors we are fixing *)
        diagnostics;
        action =
          CodeAction.BothEditThenCommand
            ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
              mk_log_command ~title ~diagnostic_title:"insert_type_for_sig_verification_failure"
            );
      };
  ]

let autofix_exports_code_actions
    ~options
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~ast
    ~file_sig
    ~tolerable_errors
    ~typed_ast
    ~diagnostics
    uri
    loc =
  let open Autofix_exports in
  let fixable_locs = set_of_fixable_signature_verification_locations tolerable_errors in
  if LocSet.mem loc fixable_locs then
    fix_signature_verification_error_at_loc
      ~cx
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_module_info
      ~get_type_sig
      ~file_sig
      ~typed_ast
      ast
      loc
    |> autofix_insert_type_annotation_helper ~options ~ast ~diagnostics ~uri
  else
    []

let autofix_missing_local_annot_code_actions
    ~options
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~ast
    ~file_sig
    ~tolerable_errors:_
    ~typed_ast
    ~diagnostics
    uri
    loc =
  let open Autofix_missing_local_annots in
  let fixable_locs = map_of_fixable_missing_local_params cx in
  let entry =
    Base.List.find ~f:(fun (err_loc, _) -> Loc.contains err_loc loc) (LocMap.elements fixable_locs)
  in
  match entry with
  | Some (_, type_t) ->
    fix_missing_param_annot_at_loc
      ~cx
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_module_info
      ~get_type_sig
      ~file_sig
      ~typed_ast
      ast
      loc
      type_t
    |> autofix_insert_type_annotation_helper ~options ~ast ~diagnostics ~uri
  | None -> []

let code_action_insert_inferred_render_type
    ~options
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~ast
    ~file_sig
    ~typed_ast
    uri
    loc =
  match
    Insert_inferred_render_type.insert_render_type_at_loc
      ~cx
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_module_info
      ~get_type_sig
      ~file_sig
      ~typed_ast
      ast
      loc
  with
  | Some new_ast ->
    let open Lsp in
    let diff = Insert_type.mk_diff ast new_ast in
    let opts = layout_options options in
    let edits =
      Replacement_printer.mk_loc_patch_ast_differ ~opts diff
      |> Base.List.map ~f:(fun (loc, text) ->
             { Lsp.TextEdit.range = Lsp.loc_to_lsp_range loc; newText = text }
         )
    in
    let title = "Insert inferred render type" in
    [
      CodeAction.Action
        {
          CodeAction.title;
          kind = CodeActionKind.refactor;
          diagnostics = [];
          action =
            CodeAction.BothEditThenCommand
              ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                mk_log_command ~title ~diagnostic_title:"insert_inferred_render_type"
              );
        };
    ]
  | None -> []

let refactor_extract_and_stub_out_code_actions
    ~options
    ~support_experimental_snippet_text_edit
    ~file_contents
    ~ast
    ~cx
    ~file_sig
    ~typed_ast
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~only
    uri
    loc =
  match loc.Loc.source with
  | None -> []
  | Some file ->
    let lsp_action_from_refactor
        ~diagnostic_title ~kind { Refactor_extract.title; new_ast; added_imports } =
      let diff = Insert_type.mk_diff ast new_ast in
      let opts = layout_options options in
      let edits =
        Autofix_imports.add_imports ~options:opts ~added_imports ast
        @ Replacement_printer.mk_loc_patch_ast_differ ~opts diff
        |> flow_loc_patch_to_lsp_edits
      in
      let open Lsp in
      CodeAction.Action
        {
          CodeAction.title;
          kind;
          diagnostics = [];
          action =
            CodeAction.BothEditThenCommand
              ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                mk_log_command ~title ~diagnostic_title
              );
        }
    in
    let refactors =
      if (not (include_extract_refactors only)) || Loc.(loc.start = loc._end) then
        []
      else
        let tokens =
          let use_strict = Options.modules_are_use_strict options in
          let module_ref_prefix = Options.haste_module_ref_prefix options in
          let assert_operator = Options.assert_operator options |> Options.AssertOperator.parse in
          let parse_options =
            {
              Parser_env.permissive_parse_options with
              Parser_env.use_strict;
              assert_operator;
              module_ref_prefix;
            }
          in
          Ast_extraction_utils.AstExtractor.tokens ~parse_options (Some file) file_contents
        in
        Refactor_extract.provide_available_refactors
          ~tokens
          ~ast
          ~cx
          ~file
          ~file_sig
          ~typed_ast
          ~loc_of_aloc
          ~get_ast_from_shared_mem
          ~get_haste_module_info
          ~get_type_sig
          ~support_experimental_snippet_text_edit
          ~extract_range:loc
        |> List.map
             (lsp_action_from_refactor
                ~diagnostic_title:"refactor_extract"
                ~kind:Lsp.CodeActionKind.refactor_extract
             )
    in
    let refactors =
      match
        Stub_unbound_name.stub
          ~ast
          ~cx
          ~file
          ~file_sig
          ~typed_ast
          ~loc_of_aloc
          ~get_ast_from_shared_mem
          ~get_haste_module_info
          ~get_type_sig
          loc
      with
      | None -> refactors
      | Some r ->
        lsp_action_from_refactor ~diagnostic_title:"stub_out" ~kind:Lsp.CodeActionKind.quickfix r
        :: refactors
    in
    refactors

let insert_inferred_type_as_cast_code_actions
    ~options
    ~file_contents
    ~ast
    ~cx
    ~file_sig
    ~typed_ast
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    uri
    loc =
  if Loc.(loc.start = loc._end) then
    []
  else
    match loc.Loc.source with
    | None -> []
    | Some file ->
      let tokens =
        let use_strict = Options.modules_are_use_strict options in
        let module_ref_prefix = Options.haste_module_ref_prefix options in
        let assert_operator = Options.assert_operator options |> Options.AssertOperator.parse in
        let parse_options =
          {
            Parser_env.permissive_parse_options with
            Parser_env.use_strict;
            assert_operator;
            module_ref_prefix;
          }
        in
        Ast_extraction_utils.AstExtractor.tokens ~parse_options (Some file) file_contents
      in
      let extraction_result = Ast_extraction_utils.AstExtractor.extract tokens ast loc in
      (match extraction_result.Ast_extraction_utils.AstExtractor.extracted_expression with
      | None -> []
      | Some { Ast_extraction_utils.AstExtractor.expression; _ } ->
        let remote_converter =
          new Insert_type_imports.ImportsHelper.remote_converter
            ~loc_of_aloc
            ~file_options:(Options.file_options options)
            ~get_haste_module_info
            ~get_type_sig
            ~iteration:0
            ~file
            ~reserved_names:SSet.empty
        in
        (match
           Insert_type.insert_type
             ~cx
             ~loc_of_aloc
             ~remote_converter
             ~get_ast_from_shared_mem
             ~get_haste_module_info
             ~get_type_sig
             ~file_sig
             ~typed_ast
             ~omit_targ_defaults:false
             ~strict:false
             ~ambiguity_strategy:Autofix_options.Generalize
             ast
             (fst expression)
         with
        | exception Insert_type.FailedToInsertType _ -> []
        | new_ast ->
          let added_imports = remote_converter#to_import_bindings in
          let diff = Insert_type.mk_diff ast new_ast in
          let opts = layout_options options in
          let edits =
            Autofix_imports.add_imports ~options:opts ~added_imports ast
            @ Replacement_printer.mk_loc_patch_ast_differ ~opts diff
            |> flow_loc_patch_to_lsp_edits
          in
          let title = "Insert inferred type as a type cast" in
          let code_action =
            let open Lsp in
            CodeAction.Action
              {
                CodeAction.title;
                kind = CodeActionKind.refactor;
                diagnostics = [];
                action =
                  CodeAction.BothEditThenCommand
                    ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                      mk_log_command ~title ~diagnostic_title:"insert_inferred_type_as_cast"
                    );
              }
          in
          [code_action]))

let insert_jsdoc_code_actions ~options ~ast uri loc =
  match Insert_jsdoc.insert_stub_for_target ~use_snippets:false loc ast with
  | Some (ast', _) ->
    ast'
    |> Flow_ast_differ.program ast
    |> Replacement_printer.mk_loc_patch_ast_differ ~opts:(layout_options options)
    |> flow_loc_patch_to_lsp_edits
    |> Base.List.map ~f:(fun edit ->
           (* This hack is needed because the differ doesn't differentiate between
              [comment; \n; node] and [comment; node] *)
           Lsp.TextEdit.{ edit with newText = edit.newText ^ "\n" }
       )
    |> fun edits ->
    let open Lsp in
    let title = "Add JSDoc documentation" in
    [
      CodeAction.Action
        {
          CodeAction.title;
          kind = CodeActionKind.refactor;
          diagnostics = [];
          action =
            CodeAction.BothEditThenCommand
              ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                mk_log_command ~title ~diagnostic_title:"add_jsdocs"
              );
        };
    ]
  | _ -> []

let refactor_arrow_function_code_actions ~ast ~scope_info ~options ~only uri loc =
  if include_rewrite_refactors only then
    match Refactor_arrow_functions.add_or_remove_braces ~ast ~scope_info loc with
    | Some (ast', title) ->
      ast'
      |> Flow_ast_differ.program ast
      |> Replacement_printer.mk_loc_patch_ast_differ ~opts:(layout_options options)
      |> flow_loc_patch_to_lsp_edits
      |> fun edits ->
      let open Lsp in
      [
        CodeAction.Action
          {
            CodeAction.title;
            kind = CodeActionKind.refactor_rewrite;
            diagnostics = [];
            action =
              CodeAction.BothEditThenCommand
                ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                  mk_log_command ~title ~diagnostic_title:"refactor_arrow_function"
                );
          };
      ]
    | _ -> []
  else
    []

let refactor_switch_to_match_statement_actions ~cx ~ast ~options ~only uri loc =
  if Context.enable_pattern_matching cx && include_rewrite_refactors only then
    match Refactor_switch_to_match_statement.refactor ast loc with
    | Some ast' ->
      Flow_ast_differ.program ast ast'
      |> Replacement_printer.mk_loc_patch_ast_differ ~opts:(layout_options options)
      |> flow_loc_patch_to_lsp_edits
      |> fun edits ->
      let open Lsp in
      let title = "Refactor `switch` to `match`" in
      [
        CodeAction.Action
          {
            CodeAction.title;
            kind = CodeActionKind.refactor_rewrite;
            diagnostics = [];
            action =
              CodeAction.BothEditThenCommand
                ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                  mk_log_command ~title ~diagnostic_title:"refactor_switch_to_match"
                );
          };
      ]
    | None -> []
  else
    []

let add_jsx_props_code_actions ~snippets_enabled ~cx ~ast ~typed_ast ~options uri loc =
  match Refactor_add_jsx_props.fill_props cx ~snippets_enabled ~ast ~tast:typed_ast loc with
  | None -> []
  | Some xs ->
    xs
    |> Base.List.map ~f:(fun (loc, attr) ->
           let text =
             Js_layout_generator.jsx_opening_attr ~opts:(layout_options options) attr
             |> Ast_diff_printer.text_of_layout
             |> ( ^ ) " " (* prefix with space *)
           in
           (loc, text)
       )
    |> flow_loc_patch_to_lsp_edits
    |> fun edits ->
    let open Lsp in
    let title = "Add missing attributes" in
    [
      CodeAction.Action
        {
          CodeAction.title;
          kind = CodeActionKind.quickfix;
          diagnostics = [];
          action =
            CodeAction.BothEditThenCommand
              ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                mk_log_command ~title ~diagnostic_title:"add_missing_jsx_props"
              );
        };
    ]

let preferred_import ~ast ~exports name loc =
  let files =
    if Autofix_imports.loc_is_type ~ast loc then
      Export_search.get_types name exports
    else
      Export_search.get_values name exports
  in
  if Export_index.ExportMap.cardinal files = 1 then
    (* there must be exactly 1 result to autofix it *)
    Some (fst (Export_index.ExportMap.choose files))
  else
    None

let maybe_sort_by_usage ~imports_ranked_usage imports =
  if imports_ranked_usage then
    Base.List.stable_sort imports ~compare:(fun (_, a) (_, b) -> Int.compare b a)
  else
    imports

let suggest_imports
    ~cx
    ~layout_options
    ~module_system_info
    ~ast
    ~diagnostics
    ~imports_ranked_usage
    ~exports
    ~name
    uri
    loc =
  let open Lsp in
  let files =
    if Autofix_imports.loc_is_type ~ast loc then
      Export_search.get_types name exports
    else
      Export_search.get_values name exports
  in
  if Export_index.ExportMap.is_empty files then
    []
  else
    let src_dir = Lsp_helpers.lsp_uri_to_path uri |> Filename.dirname |> Base.Option.return in
    let error_range = Lsp.loc_to_lsp_range loc in
    let relevant_diagnostics =
      let open PublishDiagnostics in
      let lsp_code = StringCode Error_codes.(string_of_code CannotResolveName) in
      Base.List.filter diagnostics ~f:(fun { source; code; range; _ } ->
          source = Some "Flow" && code = lsp_code && Lsp_helpers.ranges_overlap range error_range
      )
    in
    let is_available_autoimport_result = Lsp_import_edits.is_available_autoimport_result cx in
    files
    |> Export_index.ExportMap.bindings
    |> Base.List.filter ~f:(fun ((source, _), _) -> is_available_autoimport_result ~name ~source)
    |> maybe_sort_by_usage ~imports_ranked_usage
    |> Base.List.fold ~init:[] ~f:(fun acc ((source, export_kind), _num) ->
           match
             Lsp_import_edits.text_edits_of_import
               ~layout_options
               ~module_system_info
               ~src_dir
               ~ast
               export_kind
               name
               source
           with
           | None -> acc
           | Some { Code_action_text_edits.edits; title; from = _ } ->
             let command =
               CodeAction.Action
                 {
                   CodeAction.title;
                   kind = CodeActionKind.quickfix;
                   diagnostics = relevant_diagnostics;
                   action =
                     CodeAction.BothEditThenCommand
                       ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                         mk_log_command ~title ~diagnostic_title:"import"
                       );
                 }
             in
             command :: acc
       )
    |> Base.List.rev

type ast_transform =
  cx:Context.t ->
  file_sig:File_sig.t ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t option

type ast_transform_of_error = {
  title: string;
  diagnostic_title: string;
  transform: ast_transform;
  target_loc: Loc.t;
}

let untyped_ast_transform transform ~cx:_ ~file_sig:_ ~ast ~typed_ast:_ loc =
  Some (transform ast loc)

let autofix_in_upstream_file
    ~cx
    ~get_ast_from_shared_mem
    ~file_sig
    ~diagnostics
    ~ast
    ~typed_ast
    ~options
    ~title
    ~transform
    ~diagnostic_title
    uri
    loc =
  let (ast, uri) =
    let src = Loc.source loc in
    let ast_src = fst ast |> Loc.source in
    if ast_src <> src then
      (* load ast of upstream file
         In order to appear in an error, a loc must have a source *)
      let source_file = Base.Option.value_exn src in
      match get_ast_from_shared_mem source_file with
      | None -> (ast, uri)
      | Some ast ->
        (ast, source_file |> File_key.to_string |> File_url.create |> Lsp.DocumentUri.of_string)
    else
      (ast, uri)
  in
  let open Base.Option in
  transform ~cx ~file_sig ~ast ~typed_ast loc
  >>| Flow_ast_differ.program ast
  >>| Replacement_printer.mk_loc_patch_ast_differ ~opts:(layout_options options)
  >>| flow_loc_patch_to_lsp_edits
  >>= fun edits ->
  match edits with
  | [] -> None
  | _ :: _ ->
    let open Lsp in
    Some
      (CodeAction.Action
         {
           CodeAction.title;
           kind = CodeActionKind.quickfix;
           (* Handing back the diagnostics we were given is a placeholder for
              eventually generating the diagnostics for the errors we are fixing *)
           diagnostics;
           action =
             CodeAction.BothEditThenCommand
               ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                 mk_log_command ~title ~diagnostic_title
               );
         }
      )

let loc_opt_intersects ~loc ~error_loc =
  match loc with
  | None -> true
  | Some loc -> Loc.intersects error_loc loc

let ast_transforms_of_error
    ~loc_of_aloc
    ?lazy_error_loc
    ?(get_ast_from_shared_mem = (fun _ -> None))
    ?(get_haste_module_info = (fun _ -> None))
    ?(get_type_sig = (fun _ -> None))
    ?loc = function
  | Error_message.EDeprecatedBool error_loc ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Replace `bool` with `boolean`";
          diagnostic_title = "replace_bool";
          transform =
            untyped_ast_transform
              (Autofix_replace_type.replace_type ~f:(function
                  | Flow_ast.Type.Boolean { raw = _; comments } ->
                    Flow_ast.Type.Boolean { raw = `Boolean; comments }
                  | unexpected -> unexpected
                  )
                  );
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EDuplicateComponentProp { spread = error_loc; duplicates } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Wrap spread prop with Omit";
          diagnostic_title = "wrap_spread_prop_with_omit";
          transform =
            untyped_ast_transform
              (Autofix_replace_type.replace_type ~f:(fun t ->
                   let open Flow_ast.Type in
                   let string_lit_annot_of_duplicate (_, x, _) =
                     let n = Reason.display_string_of_name x in
                     ( Loc.none,
                       StringLiteral
                         {
                           Flow_ast.StringLiteral.value = n;
                           raw = "\"" ^ n ^ "^\n";
                           comments = None;
                         }
                     )
                   in
                   let omitted_keys_annot =
                     match duplicates with
                     | (n1, []) -> string_lit_annot_of_duplicate n1
                     | (n1, n2 :: ns) ->
                       ( Loc.none,
                         Union
                           {
                             Union.types =
                               ( string_lit_annot_of_duplicate n1,
                                 string_lit_annot_of_duplicate n2,
                                 List.map string_lit_annot_of_duplicate ns
                               );
                             comments = None;
                           }
                       )
                   in
                   Generic
                     {
                       Generic.id =
                         Generic.Identifier.Unqualified
                           (Loc.none, { Flow_ast.Identifier.name = "Omit"; comments = None });
                       targs =
                         Some
                           ( Loc.none,
                             {
                               TypeArgs.arguments = [(Loc.none, t); omitted_keys_annot];
                               comments = None;
                             }
                           );
                       comments = None;
                     }
               )
              );
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EEnumInvalidMemberAccess { reason; suggestion = Some fixed_prop_name; _ } ->
    let error_loc = Reason.loc_of_reason reason in
    if loc_opt_intersects ~error_loc ~loc then
      let original_prop_name = reason |> Reason.desc_of_reason |> Reason.string_of_desc in
      let title = Printf.sprintf "Replace %s with `%s`" original_prop_name fixed_prop_name in
      [
        {
          title;
          diagnostic_title = "replace_enum_prop_typo_at_target";
          transform =
            untyped_ast_transform (Autofix_prop_typo.replace_prop_typo_at_target ~fixed_prop_name);
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EClassToObject (reason_class, reason_obj, _) ->
    let error_loc = Reason.loc_of_reason reason_class in
    if loc_opt_intersects ~error_loc ~loc then
      let obj_loc = Reason.def_loc_of_reason reason_obj in
      let original = reason_obj |> Reason.desc_of_reason ~unwrap:false |> Reason.string_of_desc in
      let title = Utils_js.spf "Rewrite %s as an interface" original in
      let diagnostic_title = "replace_obj_with_interface" in
      [
        {
          title;
          diagnostic_title;
          transform = untyped_ast_transform Autofix_interface.replace_object_at_target;
          target_loc = obj_loc;
        };
      ]
    else
      []
  | Error_message.EMethodUnbinding { reason_op; reason_prop; _ } ->
    let error_loc = Reason.loc_of_reason reason_op in
    if loc_opt_intersects ~error_loc ~loc then
      let method_loc = Reason.def_loc_of_reason reason_prop in
      let original = reason_prop |> Reason.desc_of_reason ~unwrap:false |> Reason.string_of_desc in
      let title = Utils_js.spf "Rewrite %s as an arrow function" original in
      let diagnostic_title = "replace_method_with_arrow" in
      [
        {
          title;
          diagnostic_title;
          transform = untyped_ast_transform Autofix_method.replace_method_at_target;
          target_loc = method_loc;
        };
      ]
    else
      []
  | Error_message.EUnusedPromise { loc = error_loc; async } ->
    if loc_opt_intersects ~error_loc ~loc then
      let insert_async =
        {
          title = "Insert `await`";
          diagnostic_title = "insert_await";
          transform = untyped_ast_transform Autofix_unused_promise.insert_await;
          target_loc = error_loc;
        }
      in
      let insert_void =
        {
          title = "Insert `void`";
          diagnostic_title = "insert_void";
          transform = untyped_ast_transform Autofix_unused_promise.insert_void;
          target_loc = error_loc;
        }
      in
      if async then
        [insert_async; insert_void]
      else
        [insert_void]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSUnknown; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `mixed`";
          diagnostic_title = "convert_unknown_type";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_unknown_type;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSNever; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `empty`";
          diagnostic_title = "convert_never_type";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_never_type;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSUndefined; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `void`";
          diagnostic_title = "convert_undefined_type";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_undefined_type;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSKeyof; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `$Keys<T>`";
          diagnostic_title = "convert_keyof_type";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_keyof_type;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSTypeParamExtends; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `: T`";
          diagnostic_title = "convert_type_param_extends";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_type_param_extends;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSReadonlyVariance; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `+`";
          diagnostic_title = "convert_readonly_variance";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_readonly_variance;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSInOutVariance `In; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `-`";
          diagnostic_title = "convert_in_variance";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_in_variance;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSInOutVariance `Out; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `+`";
          diagnostic_title = "convert_out_variance";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_out_variance;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSInOutVariance `InOut; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Remove";
          diagnostic_title = "remove_in_out_variance";
          transform = untyped_ast_transform Autofix_ts_syntax.remove_in_out_variance;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax
      { kind = Error_message.TSSatisfiesType enabled_casting_syntax; loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      let title =
        let open Options.CastingSyntax in
        match enabled_casting_syntax with
        | As
        | Both ->
          "Convert to `as` expression `<expr> as <type>`"
      in
      [
        {
          title;
          diagnostic_title = "convert_satisfies_expression";
          transform =
            untyped_ast_transform
              (Autofix_casting_syntax.convert_satisfies_expression ~enabled_casting_syntax);
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSReadonlyType (Some `Array); loc = error_loc }
    ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `$ReadOnlyArray`";
          diagnostic_title = "convert_readonly_array_type";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_readonly_array_type;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.ETSSyntax { kind = Error_message.TSReadonlyType (Some `Tuple); loc = error_loc }
    ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Convert to `$ReadOnly`";
          diagnostic_title = "convert_readonly_tuple_type";
          transform = untyped_ast_transform Autofix_ts_syntax.convert_readonly_tuple_type;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EInvalidTypeCastSyntax { loc = error_loc; enabled_casting_syntax } ->
    if loc_opt_intersects ~error_loc ~loc then
      let (title, diagnostic_title, fix) =
        let open Options.CastingSyntax in
        match enabled_casting_syntax with
        | As
        | Both ->
          ( "Convert to `as` expression `<expr> as <type>`",
            "convert_colon_cast",
            Autofix_casting_syntax.convert_colon_cast
          )
      in
      [
        {
          title;
          diagnostic_title;
          transform = untyped_ast_transform (fix ~enabled_casting_syntax);
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EIncorrectTypeWithReplacement { kind; loc = error_loc } ->
    let incorrect_name = Flow_intermediate_error_types.IncorrectType.incorrect_of_kind kind in
    let replacement_name = Flow_intermediate_error_types.IncorrectType.replacement_of_kind kind in
    let title = Printf.sprintf "Convert to `%s`" replacement_name in
    let diagnostic_title = Printf.sprintf "convert_%s_type" incorrect_name in
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title;
          diagnostic_title;
          transform = untyped_ast_transform (Autofix_type_name.convert_incorrect_type kind);
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EInternalType
      ( error_loc,
        Flow_intermediate_error_types.DollarUtilityTypeWithNonDollarAliases replacement_name
      ) ->
    let incorrect_name = "$" ^ replacement_name in
    let title = Printf.sprintf "Convert to `%s`" replacement_name in
    let diagnostic_title = Printf.sprintf "convert_dollar_%s_type" replacement_name in
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title;
          diagnostic_title;
          transform =
            untyped_ast_transform (Autofix_type_name.convert_type ~incorrect_name ~replacement_name);
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EInternalType
      ( error_loc,
        Flow_intermediate_error_types.ReactDollarUtilityTypesWithNonDollarAliases
          replacement_name_without_react
      ) ->
    let incorrect_name = "React$" ^ replacement_name_without_react in
    let replacement_name = "React." ^ replacement_name_without_react in
    let title = Printf.sprintf "Convert to `%s`" replacement_name in
    let diagnostic_title =
      Printf.sprintf "convert_react_dollar_%s_type" replacement_name_without_react
    in
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title;
          diagnostic_title;
          transform =
            untyped_ast_transform (Autofix_type_name.convert_type ~incorrect_name ~replacement_name);
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EInvalidRendersTypeArgument
      { loc = error_loc; renders_variant; invalid_type_reasons = _; invalid_render_type_kind } ->
    if loc_opt_intersects ~error_loc ~loc then
      match (renders_variant, invalid_render_type_kind) with
      | ( Flow_ast.Type.Renders.Star,
          ( Flow_intermediate_error_types.InvalidRendersNullVoidFalse
          | Flow_intermediate_error_types.InvalidRendersIterable )
        ) ->
        [
          {
            title = "Simplify `renders*`";
            diagnostic_title = "simplify_renders_star";
            transform =
              untyped_ast_transform Autofix_renders_variant.to_renders_star_with_best_effort_fixes;
            target_loc = error_loc;
          };
        ]
      | (Flow_ast.Type.Renders.Maybe, Flow_intermediate_error_types.InvalidRendersNullVoidFalse) ->
        [
          {
            title = "Simplify `renders?`";
            diagnostic_title = "simplify_renders_maybe";
            transform =
              untyped_ast_transform Autofix_renders_variant.to_renders_maybe_with_best_effort_fixes;
            target_loc = error_loc;
          };
        ]
      | (_, Flow_intermediate_error_types.InvalidRendersNullVoidFalse) ->
        [
          {
            title = "Switch to `renders?`";
            diagnostic_title = "switch_to_renders_maybe";
            transform =
              untyped_ast_transform Autofix_renders_variant.to_renders_maybe_with_best_effort_fixes;
            target_loc = error_loc;
          };
        ]
      | (_, Flow_intermediate_error_types.InvalidRendersIterable) ->
        [
          {
            title = "Switch to `renders*`";
            diagnostic_title = "switch_to_renders_star";
            transform =
              untyped_ast_transform Autofix_renders_variant.to_renders_star_with_best_effort_fixes;
            target_loc = error_loc;
          };
        ]
      | _ -> []
    else
      []
  | Error_message.EBuiltinNameLookupFailed { loc = error_loc; name } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Prefix with `this.`";
          diagnostic_title = "prefix_with_this";
          transform = Autofix_class_member_access.fix ~loc_of_aloc ~member_name:name;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.(EBindingError (ETypeInValuePosition { name; _ }, error_loc, _, _)) ->
    if loc_opt_intersects ~error_loc ~loc then
      let transform ~cx:_ ~file_sig:_ ~ast ~typed_ast:_ loc =
        Autofix_type_to_value_import.convert_type_to_value_import ast loc
      in
      [
        {
          title = Utils_js.spf "Convert type import of `%s` to value import" name;
          diagnostic_title = "convert_type_to_value_import";
          transform;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EMatchInvalidObjectShorthand { loc = error_loc; name } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = Utils_js.spf "Convert to `const %s`" name;
          diagnostic_title = "convert_match_object_shorthand_to_const";
          transform = untyped_ast_transform Autofix_match_syntax.convert_object_shorthand_to_const;
          target_loc = error_loc;
        };
        {
          title = Utils_js.spf "Convert to `%s: %s`" name name;
          diagnostic_title = "convert_match_object_shorthand_to_reference";
          transform =
            untyped_ast_transform Autofix_match_syntax.convert_object_shorthand_to_reference;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EMatchStatementInvalidBody { loc = error_loc } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Wrap in a block";
          diagnostic_title = "fix_invalid_match_statement_body";
          transform = untyped_ast_transform Autofix_match_syntax.fix_invalid_match_statement_body;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EMatchInvalidBindingKind { loc = error_loc; kind = current_kind } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title =
            Utils_js.spf
              "Replace `%s` with `const`"
              (Flow_ast_utils.string_of_variable_kind current_kind);
          diagnostic_title = "fix_match_invalid_binding_kind";
          transform = untyped_ast_transform Autofix_match_syntax.fix_invalid_binding_kind;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EMatchInvalidWildcardSyntax error_loc ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Replace `default` with `_`";
          diagnostic_title = "fix_match_invalid_wildcard_syntax";
          transform = untyped_ast_transform Autofix_match_syntax.fix_invalid_wildcard_syntax;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EMatchInvalidCaseSyntax { loc = error_loc; kind = _ } ->
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Fix invalid match syntax";
          diagnostic_title = "fix_match_invalid_case_syntax";
          transform = untyped_ast_transform Autofix_match_syntax.fix_invalid_case_syntax;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EMatchNonExhaustiveObjectPattern { loc = error_loc; rest; missing_props } ->
    if loc_opt_intersects ~error_loc ~loc then
      let add_only_rest =
        {
          title = "Add rest `...` to object pattern";
          diagnostic_title = "fix_match_non_exhaustive_object_pattern_add_rest";
          transform =
            untyped_ast_transform
              (Autofix_match_syntax.fix_non_exhaustive_object_pattern ~add_rest:true []);
          target_loc = error_loc;
        }
      in
      let missing_props_prefix_text missing_props =
        let num_missing_props = Base.List.length missing_props in
        if num_missing_props = 1 then
          Utils_js.spf "Add the missing property"
        else
          Utils_js.spf "Add %d missing properties" num_missing_props
      in
      match (missing_props, rest) with
      | ([], _) -> [add_only_rest]
      | (_ :: _, None) ->
        [
          {
            title = Utils_js.spf "%s to object pattern" (missing_props_prefix_text missing_props);
            diagnostic_title = "fix_match_non_exhaustive_object_pattern_add_props";
            transform =
              untyped_ast_transform
                (Autofix_match_syntax.fix_non_exhaustive_object_pattern
                   ~add_rest:false
                   missing_props
                );
            target_loc = error_loc;
          };
          add_only_rest;
        ]
      | (_ :: _, Some _) ->
        [
          {
            title =
              Utils_js.spf
                "%s and rest `...` to object pattern"
                (missing_props_prefix_text missing_props);
            diagnostic_title = "fix_match_non_exhaustive_object_pattern_add_props_and_rest";
            transform =
              untyped_ast_transform
                (Autofix_match_syntax.fix_non_exhaustive_object_pattern ~add_rest:true missing_props);
            target_loc = error_loc;
          };
          add_only_rest;
        ]
    else
      []
  | Error_message.EMatchNotExhaustive { loc = error_loc; examples; missing_pattern_asts } ->
    if loc_opt_intersects ~error_loc ~loc then
      let num_examples = Base.List.length examples in
      let num_asts = Base.List.length missing_pattern_asts in
      let prefix =
        if num_asts = 1 then
          "Add the missing case"
        else
          Utils_js.spf "Add %d missing cases" num_asts
      in
      let suffix =
        if num_asts = num_examples then
          " to make `match` exhaustively checked"
        else
          Utils_js.spf " (out of a total of %d)" num_examples
      in
      [
        {
          title = Utils_js.spf "%s%s" prefix suffix;
          diagnostic_title = "fix_match_not_exhaustive";
          transform =
            untyped_ast_transform (Autofix_match_syntax.fix_not_exhaustive missing_pattern_asts);
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EMatchUnusedPattern { reason; already_seen = _ } ->
    let error_loc = Reason.loc_of_reason reason in
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Remove";
          diagnostic_title = "fix_match_unused_pattern";
          transform = untyped_ast_transform Autofix_match_syntax.remove_unused_pattern;
          target_loc = error_loc;
        };
      ]
    else
      []
  | Error_message.EInvariantSubtypingWithUseOp
      {
        explanation =
          Some
            Flow_intermediate_error_types.(
              ( LazyExplanationInvariantSubtypingDueToMutableArray
                  {
                    lower_array_loc = lower_loc;
                    lower_array_desc = TypeOrTypeDesc.TypeDesc (Error lower_desc);
                    upper_array_desc = TypeOrTypeDesc.TypeDesc (Ok upper_ty);
                    _;
                  }
              | LazyExplanationInvariantSubtypingDueToMutableProperty
                  {
                    lower_obj_loc = lower_loc;
                    lower_obj_desc = TypeOrTypeDesc.TypeDesc (Error lower_desc);
                    upper_obj_desc = TypeOrTypeDesc.TypeDesc (Ok upper_ty);
                    _;
                  }
              | LazyExplanationInvariantSubtypingDueToMutableProperties
                  {
                    lower_obj_loc = lower_loc;
                    lower_obj_desc = TypeOrTypeDesc.TypeDesc (Error lower_desc);
                    upper_obj_desc = TypeOrTypeDesc.TypeDesc (Ok upper_ty);
                    _;
                  } ));
        _;
      }
    when match lower_desc with
         | Reason.RObjectLit
         | Reason.RObjectLit_UNSOUND
         | Reason.RArrayLit
         | Reason.RArrayLit_UNSOUND ->
           true
         | _ -> false ->
    let error_loc_opt =
      match lazy_error_loc with
      | Some (lazy error_loc) when loc_opt_intersects ~error_loc ~loc -> Some error_loc
      | _ ->
        if loc_opt_intersects ~error_loc:lower_loc ~loc then
          Some lower_loc
        else
          None
    in
    (match error_loc_opt with
    | None -> []
    | Some _ ->
      let transform ~cx ~file_sig ~ast ~typed_ast _ =
        let ast' =
          Insert_type.insert_type_ty
            ~cx
            ~loc_of_aloc
            ~get_ast_from_shared_mem
            ~get_haste_module_info
            ~get_type_sig
            ~file_sig
            ~typed_ast
            ~strict:false
            ast
            lower_loc
            (Ty_utils.simplify_type ~merge_kinds:true upper_ty)
        in
        if ast == ast' then
          None
        else
          Some ast'
      in
      [
        {
          title = "Add suggested annotation to the literal";
          diagnostic_title = "fix_invariant_subtyping_error_with_annot";
          transform;
          target_loc = lower_loc;
        };
      ])
  | error_message ->
    (match error_message |> Error_message.friendly_message_of_msg with
    | Error_message.PropMissingInLookup
        { loc = error_loc; suggestion = Some suggestion; prop = Some prop_name; _ } ->
      if loc_opt_intersects ~error_loc ~loc then
        let title = Printf.sprintf "Replace `%s` with `%s`" prop_name suggestion in
        let diagnostic_title = "replace_prop_typo_at_target" in
        [
          {
            title;
            diagnostic_title;
            transform =
              untyped_ast_transform
                (Autofix_prop_typo.replace_prop_typo_at_target ~fixed_prop_name:suggestion);
            target_loc = error_loc;
          };
        ]
      else
        []
    | Error_message.IncompatibleUse
        { loc = error_loc; upper_kind = Error_message.IncompatibleGetPropT _; reason_lower; _ } ->
      (match (loc_opt_intersects ~error_loc ~loc, Reason.desc_of_reason reason_lower) with
      | (true, ((Reason.RVoid | Reason.RNull | Reason.RVoidedNull | Reason.RNullOrVoid) as r)) ->
        let title =
          Printf.sprintf
            "Add optional chaining for object that might be `%s`"
            (Reason.string_of_desc r)
        in
        let diagnostic_title = "add_optional_chaining" in
        [
          {
            title;
            diagnostic_title;
            transform = untyped_ast_transform Autofix_optional_chaining.add_optional_chaining;
            target_loc = error_loc;
          };
        ]
      | _ -> [])
    | _ -> [])

let code_actions_of_errors
    ~options
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~module_system_info
    ~get_type_sig
    ~cx
    ~file_sig
    ~env
    ~ast
    ~typed_ast
    ~diagnostics
    ~errors
    ~only
    ~imports_ranked_usage
    uri
    loc =
  let include_quick_fixes = include_quick_fixes only in
  let (actions, has_missing_import) =
    Flow_error.ErrorSet.fold
      (fun error (actions, has_missing_import) ->
        let error_message =
          Flow_error.msg_of_error error |> Error_message.map_loc_of_error_message loc_of_aloc
        in
        let (suggest_imports_actions, has_missing_import) =
          match (error_message, env) with
          | ( Error_message.EBuiltinNameLookupFailed { loc = error_loc; name },
              { ServerEnv.exports = Some exports; _ }
            )
            when Options.autoimports options ->
            let actions =
              if include_quick_fixes && Loc.intersects error_loc loc then
                suggest_imports
                  ~cx
                  ~layout_options:(Code_action_utils.layout_options options)
                  ~module_system_info
                  ~ast
                  ~diagnostics
                  ~imports_ranked_usage
                  ~exports (* TODO consider filtering out internal names *)
                  ~name
                  uri
                  loc
              else
                []
            in
            (actions, true)
          | _ -> ([], has_missing_import)
        in
        let quick_fix_actions =
          if include_quick_fixes then
            let lazy_error_loc =
              lazy
                (let { Flow_intermediate_error_types.loc; _ } =
                   Flow_intermediate_error.make_intermediate_error ~loc_of_aloc error
                 in
                 loc
                )
            in
            ast_transforms_of_error
              ~loc_of_aloc
              ~loc
              ~lazy_error_loc
              ~get_ast_from_shared_mem
              ~get_haste_module_info:module_system_info.Lsp_module_system_info.get_haste_module_info
              ~get_type_sig
              error_message
            |> Base.List.filter_map ~f:(fun { title; diagnostic_title; transform; target_loc } ->
                   autofix_in_upstream_file
                     ~cx
                     ~get_ast_from_shared_mem
                     ~file_sig
                     ~diagnostics
                     ~ast
                     ~typed_ast
                     ~options
                     ~title
                     ~diagnostic_title
                     ~transform
                     uri
                     target_loc
               )
          else
            []
        in
        let actions = suggest_imports_actions @ quick_fix_actions @ actions in
        (actions, has_missing_import))
      errors
      ([], false)
  in
  if include_add_missing_imports_action only && has_missing_import then
    let open Lsp in
    CodeAction.Action
      {
        CodeAction.title = "Add all missing imports";
        kind = add_missing_imports_kind;
        diagnostics = [];
        action =
          CodeAction.CommandOnly
            {
              Command.title = "";
              command = Command.Command "source.addMissingImports";
              arguments =
                (* Lsp.TextDocumentIdentifier *)
                [Hh_json.JSON_Object [("uri", Hh_json.JSON_String (Lsp.DocumentUri.to_string uri))]];
            };
      }
    :: actions
  else
    actions

let code_action_for_parser_error_with_suggestion
    acc diagnostics uri ~error_loc ~editor_loc title newText =
  if Loc.intersects error_loc editor_loc then
    let error_range = Lsp.loc_to_lsp_range error_loc in
    let open Lsp in
    let relevant_diagnostics =
      diagnostics |> List.filter (fun PublishDiagnostics.{ range; _ } -> range = error_range)
    in
    let textEdit = TextEdit.{ range = error_range; newText } in
    CodeAction.Action
      {
        CodeAction.title;
        kind = CodeActionKind.quickfix;
        diagnostics = relevant_diagnostics;
        action =
          CodeAction.BothEditThenCommand
            ( WorkspaceEdit.{ changes = UriMap.singleton uri [textEdit] },
              mk_log_command ~title ~diagnostic_title:"fix_parse_error"
            );
      }
    :: acc
  else
    acc

let code_actions_of_parse_errors ~diagnostics ~uri ~loc parse_errors =
  Base.List.fold_left
    ~f:(fun acc parse_error ->
      match parse_error with
      | (error_loc, Parse_error.UnexpectedTokenWithSuggestion (token, suggestion)) ->
        let title = Printf.sprintf "Replace `%s` with `%s`" token suggestion in
        code_action_for_parser_error_with_suggestion
          acc
          diagnostics
          uri
          ~error_loc
          ~editor_loc:loc
          title
          suggestion
      | (error_loc, Parse_error.InvalidComponentRenderAnnotation { has_nested_render = false }) ->
        let title = Printf.sprintf "Replace `:` with `renders`" in
        (* The leading " " is intentional here. Most people will write component Foo(): T and not component Foo() : T,
         * so it would be nice if we could return formatted code in the common case *)
        code_action_for_parser_error_with_suggestion
          acc
          diagnostics
          uri
          ~error_loc
          ~editor_loc:loc
          title
          " renders"
      | (error_loc, Parse_error.InvalidComponentRenderAnnotation { has_nested_render = true }) ->
        let title = Printf.sprintf "Remove `:`" in
        code_action_for_parser_error_with_suggestion
          acc
          diagnostics
          uri
          ~error_loc
          ~editor_loc:loc
          title
          ""
      | (error_loc, Parse_error.InvalidComponentStringParameterBinding { optional; name }) ->
        let title = "Use as-renaming" in
        let replacement =
          Utils_js.(
            spf
              " as %s%s"
              (camelize name)
              ( if optional then
                "?"
              else
                ":"
              )
          )
        in
        code_action_for_parser_error_with_suggestion
          acc
          diagnostics
          uri
          ~error_loc
          ~editor_loc:loc
          title
          replacement
      | _ -> acc)
    ~init:[]
    parse_errors

(** List of code actions we implement. *)
let supported_code_actions =
  [
    Lsp.CodeActionKind.quickfix;
    add_missing_imports_kind;
    Lsp.CodeActionKind.kind_of_string "source.organizeImports.flow";
    Lsp.CodeActionKind.refactor_extract;
  ]

(** Determines if at least one of the kinds in [only] is supported. *)
let kind_is_supported only =
  match only with
  | None -> true
  | Some only ->
    Base.List.exists
      ~f:(fun kind -> Lsp.CodeActionKind.contains_kind kind supported_code_actions)
      only

let organize_imports_code_action uri =
  let open Lsp in
  CodeAction.Action
    {
      CodeAction.title = "Organize imports";
      kind = CodeActionKind.kind_of_string "source.organizeImports.flow";
      diagnostics = [];
      action =
        CodeAction.CommandOnly
          {
            Command.title = "";
            command = Command.Command "source.organizeImports";
            arguments =
              (* Lsp.TextDocumentIdentifier *)
              [Hh_json.JSON_Object [("uri", Hh_json.JSON_String (Lsp.DocumentUri.to_string uri))]];
          };
    }

let code_actions_at_loc
    ~options
    ~lsp_init_params
    ~imports_ranked_usage
    ~env
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_type_sig
    ~module_system_info
    ~cx
    ~file_sig
    ~tolerable_errors
    ~file_contents
    ~ast
    ~typed_ast
    ~scope_info
    ~parse_errors
    ~diagnostics
    ~only
    ~uri
    ~loc =
  let autofix_exports_code_actions =
    autofix_exports_code_actions
      ~options
      ~cx
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_module_info:module_system_info.Lsp_module_system_info.get_haste_module_info
      ~get_type_sig
      ~ast
      ~file_sig
      ~tolerable_errors
      ~typed_ast
      ~diagnostics
      uri
      loc
  in
  let autofix_missing_local_annot_code_actions =
    autofix_missing_local_annot_code_actions
      ~options
      ~cx
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_module_info:module_system_info.Lsp_module_system_info.get_haste_module_info
      ~get_type_sig
      ~ast
      ~file_sig
      ~tolerable_errors
      ~typed_ast
      ~diagnostics
      uri
      loc
  in
  let insert_inferred_render_type_code_actions =
    code_action_insert_inferred_render_type
      ~options
      ~cx
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_module_info:module_system_info.Lsp_module_system_info.get_haste_module_info
      ~get_type_sig
      ~ast
      ~file_sig
      ~typed_ast
      uri
      loc
  in
  let refactor_code_actions =
    refactor_extract_and_stub_out_code_actions
      ~options
      ~support_experimental_snippet_text_edit:
        (Lsp_helpers.supports_experimental_snippet_text_edit lsp_init_params)
      ~file_contents
      ~ast
      ~cx
      ~file_sig
      ~typed_ast
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_module_info:module_system_info.Lsp_module_system_info.get_haste_module_info
      ~get_type_sig
      ~only
      uri
      loc
    @ insert_jsdoc_code_actions ~options ~ast uri loc
    @ refactor_arrow_function_code_actions ~ast ~scope_info ~options ~only uri loc
    @ refactor_switch_to_match_statement_actions ~cx ~ast ~options ~only uri loc
    @ add_jsx_props_code_actions
        ~snippets_enabled:(Lsp_helpers.supports_experimental_snippet_text_edit lsp_init_params)
        ~cx
        ~ast
        ~typed_ast
        ~options
        uri
        loc
  in
  let error_fixes =
    code_actions_of_errors
      ~options
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~module_system_info
      ~get_type_sig
      ~cx
      ~file_sig
      ~env
      ~ast
      ~typed_ast
      ~diagnostics
      ~errors:(Context.errors cx)
      ~only
      ~imports_ranked_usage
      uri
      loc
  in
  let inspection_related_code_actions =
    if autofix_exports_code_actions <> [] then
      (* If autofix_exports_code_actions provides some results, and
       * insert_inferred_type_as_cast_code_actions also provides some results,
       * then both actions are trying to insert a type based on an expression.
       * The result is going to be the same, so we don't need to duplicate
       * the effort. *)
      []
    else
      insert_inferred_type_as_cast_code_actions
        ~options
        ~file_contents
        ~ast
        ~cx
        ~file_sig
        ~typed_ast
        ~loc_of_aloc
        ~get_ast_from_shared_mem
        ~get_haste_module_info:module_system_info.Lsp_module_system_info.get_haste_module_info
        ~get_type_sig
        uri
        loc
  in
  let parse_error_fixes = code_actions_of_parse_errors ~diagnostics ~uri ~loc parse_errors in
  let actions =
    parse_error_fixes
    @ autofix_exports_code_actions
    @ autofix_missing_local_annot_code_actions
    @ error_fixes
    @ insert_inferred_render_type_code_actions
    @ refactor_code_actions
    @ inspection_related_code_actions
  in
  let actions =
    if include_organize_imports_actions only then
      organize_imports_code_action uri :: actions
    else
      actions
  in
  Ok actions

module ExportSourceMap = WrappedMap.Make (struct
  type t = Export_index.source

  let compare = Export_index.compare_source
end)

module ExportKindMap = WrappedMap.Make (struct
  type t = Export_index.kind

  let compare = Export_index.compare_kind
end)

(** insert imports for all undefined-variable errors that have only one suggestion *)
let autofix_imports ~options ~env ~loc_of_aloc ~module_system_info ~cx ~ast ~src_dir =
  let errors = Context.errors cx in
  (* collect imports for all of the undefined variables in the file *)
  let (imports, _) =
    Flow_error.ErrorSet.fold
      (fun error (imports, unbound_names) ->
        match
          (Flow_error.msg_of_error error |> Error_message.map_loc_of_error_message loc_of_aloc, env)
        with
        | ( Error_message.EBuiltinNameLookupFailed { loc = error_loc; name },
            { ServerEnv.exports = Some exports; _ }
          )
          when Options.autoimports options ->
          (match preferred_import ~ast ~exports name error_loc with
          | Some (source, export_kind) when not (SSet.mem name unbound_names) ->
            let bindings =
              match ExportSourceMap.find_opt source imports with
              | None -> ExportKindMap.empty
              | Some prev -> prev
            in
            let names =
              match ExportKindMap.find_opt export_kind bindings with
              | None -> [name]
              | Some prev -> name :: prev
            in
            ( ExportSourceMap.add source (ExportKindMap.add export_kind names bindings) imports,
              SSet.add name unbound_names
            )
          | _ -> (imports, unbound_names))
        | _ -> (imports, unbound_names))
      errors
      (ExportSourceMap.empty, SSet.empty)
  in
  let added_imports =
    ExportSourceMap.fold
      (fun source names_of_kinds added_imports ->
        let from = Lsp_import_edits.from_of_source ~module_system_info ~src_dir source in
        match from with
        | None -> added_imports
        | Some from ->
          ExportKindMap.fold
            (fun export_kind names added_imports ->
              match export_kind with
              | Export_index.DefaultType ->
                Base.List.fold_left
                  ~init:added_imports
                  ~f:(fun added_imports name ->
                    (from, Autofix_imports.DefaultType name) :: added_imports)
                  names
              | Export_index.Default ->
                Base.List.fold_left
                  ~init:added_imports
                  ~f:(fun added_imports name ->
                    (from, Autofix_imports.Default name) :: added_imports)
                  names
              | Export_index.Named ->
                let named_bindings =
                  Base.List.map
                    ~f:(fun name -> { Autofix_imports.remote_name = name; local_name = None })
                    names
                in
                (from, Autofix_imports.Named named_bindings) :: added_imports
              | Export_index.NamedType ->
                let named_bindings =
                  Base.List.map
                    ~f:(fun name -> { Autofix_imports.remote_name = name; local_name = None })
                    names
                in
                (from, Autofix_imports.NamedType named_bindings) :: added_imports
              | Export_index.Namespace ->
                Base.List.fold_left
                  ~init:added_imports
                  ~f:(fun added_imports name ->
                    (from, Autofix_imports.Namespace name) :: added_imports)
                  names)
            names_of_kinds
            added_imports)
      imports
      []
  in
  let opts = layout_options options in
  Autofix_imports.add_imports ~options:opts ~added_imports ast

let with_type_checked_file ~options ~profiling ~env ~file_key ~file_content ~f =
  let file_artifacts =
    let ((_, parse_errs) as intermediate_result) =
      Type_contents.parse_contents ~options ~profiling file_content file_key
    in
    if not (Flow_error.ErrorSet.is_empty parse_errs) then
      Error parse_errs
    else
      Type_contents.type_parse_artifacts
        ~options
        ~profiling
        env.ServerEnv.master_cx
        file_key
        intermediate_result
  in
  match file_artifacts with
  | Ok (Parse_artifacts { ast; _ }, Typecheck_artifacts { cx; _ }) -> f ~cx ~ast
  | _ -> Error "Failed to parse or check file"

let suggest_imports_cli
    ~options ~profiling ~env ~loc_of_aloc ~module_system_info ~file_key ~file_content =
  let uri = File_key.to_string file_key |> Lsp_helpers.path_to_lsp_uri ~default_path:"" in
  let get_edits ~cx ~ast =
    let errors = Context.errors cx in
    let (imports, _) =
      Flow_error.ErrorSet.fold
        (fun error (imports, unbound_names) ->
          match
            ( Flow_error.msg_of_error error |> Error_message.map_loc_of_error_message loc_of_aloc,
              env
            )
          with
          | ( Error_message.EBuiltinNameLookupFailed { loc = error_loc; name },
              { ServerEnv.exports = Some exports; _ }
            )
            when Options.autoimports options ->
            let ranked_imports =
              suggest_imports
                ~cx
                ~layout_options:(Code_action_utils.layout_options options)
                ~module_system_info
                ~ast
                ~diagnostics:[]
                ~imports_ranked_usage:true
                ~exports
                ~name
                uri
                error_loc
            in
            (SMap.add name ranked_imports imports, SSet.add name unbound_names)
          | _ -> (imports, unbound_names))
        errors
        (SMap.empty, SSet.empty)
    in
    Ok imports
  in
  with_type_checked_file ~options ~profiling ~env ~file_key ~file_content ~f:get_edits

let autofix_imports_cli
    ~options ~profiling ~env ~loc_of_aloc ~module_system_info ~file_key ~file_content =
  let src_dir = File_key.to_string file_key |> Filename.dirname |> Base.Option.return in
  let get_edits ~cx ~ast =
    let edits = autofix_imports ~options ~env ~loc_of_aloc ~module_system_info ~cx ~ast ~src_dir in
    Ok (Replacement_printer.loc_patch_to_patch file_content edits)
  in
  with_type_checked_file ~options ~profiling ~env ~file_key ~file_content ~f:get_edits

let autofix_imports_lsp ~options ~env ~loc_of_aloc ~module_system_info ~cx ~ast ~uri =
  let src_dir = Lsp_helpers.lsp_uri_to_path uri |> Filename.dirname |> Base.Option.return in
  let edits = autofix_imports ~options ~env ~loc_of_aloc ~module_system_info ~cx ~ast ~src_dir in
  flow_loc_patch_to_lsp_edits edits

let autofix_exports
    ~options
    ~master_cx
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_key
    ~file_content =
  let open Autofix_exports in
  let file_artifacts =
    let ((_, parse_errs) as intermediate_result) =
      Type_contents.parse_contents ~options ~profiling file_content file_key
    in
    if not (Flow_error.ErrorSet.is_empty parse_errs) then
      Error parse_errs
    else
      Type_contents.type_parse_artifacts ~options ~profiling master_cx file_key intermediate_result
  in
  match file_artifacts with
  | Ok
      ( Parse_artifacts { ast; file_sig; tolerable_errors; _ },
        Typecheck_artifacts { cx; typed_ast; obj_to_obj_map = _ }
      ) ->
    let sv_errors = set_of_fixable_signature_verification_locations tolerable_errors in
    let (new_ast, it_errs) =
      fix_signature_verification_errors
        ~file_key
        ~cx
        ~loc_of_aloc
        ~file_options:(Options.file_options options)
        ~get_ast_from_shared_mem
        ~get_haste_module_info
        ~get_type_sig
        ~file_sig
        ~typed_ast
        ast
        sv_errors
    in
    let opts = layout_options options in
    Ok (Insert_type.mk_patch ~opts ast new_ast file_content, it_errs)
  | Error _ -> Error "Failed to type-check file"

let autofix_missing_local_annot
    ~options
    ~master_cx
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_key
    ~file_content =
  let open Autofix_missing_local_annots in
  let file_artifacts =
    let ((_, parse_errs) as intermediate_result) =
      Type_contents.parse_contents ~options ~profiling file_content file_key
    in
    if not (Flow_error.ErrorSet.is_empty parse_errs) then
      Error parse_errs
    else
      Type_contents.type_parse_artifacts ~options ~profiling master_cx file_key intermediate_result
  in
  match file_artifacts with
  | Ok
      ( Parse_artifacts { ast; file_sig; _ },
        Typecheck_artifacts { cx; typed_ast; obj_to_obj_map = _ }
      ) ->
    let new_ast =
      fix_all_missing_param_annot_errors_in_file
        ~cx
        ~loc_of_aloc
        ~get_ast_from_shared_mem
        ~get_haste_module_info
        ~get_type_sig
        ~file_sig
        ~typed_ast
        ast
    in
    let opts = layout_options options in
    Ok (Insert_type.mk_patch ~opts ast new_ast file_content)
  | Error _ -> Error "Failed to type-check file"

let insert_type
    ~options
    ~env
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_key
    ~file_content
    ~target
    ~omit_targ_defaults
    ~location_is_strict:strict
    ~ambiguity_strategy =
  let open Insert_type in
  let file_artifacts =
    let ((_, parse_errs) as intermediate_result) =
      Type_contents.parse_contents ~options ~profiling file_content file_key
    in
    (* It's not clear to me (nmote) that we actually should abort when we see parse errors. Maybe
     * we should continue on here. I'm inserting this logic during the migration away from
     * typecheck_contents because it's behavior-preserving, but this may be worth revisiting. *)
    if not (Flow_error.ErrorSet.is_empty parse_errs) then
      Error parse_errs
    else
      Type_contents.type_parse_artifacts
        ~options
        ~profiling
        env.ServerEnv.master_cx
        file_key
        intermediate_result
  in
  match file_artifacts with
  | Ok
      ( Parse_artifacts { ast; file_sig; _ },
        Typecheck_artifacts { cx; typed_ast; obj_to_obj_map = _ }
      ) ->
    (try
       let new_ast =
         Insert_type.insert_type
           ~cx
           ~loc_of_aloc
           ~get_ast_from_shared_mem
           ~get_haste_module_info
           ~get_type_sig
           ~file_sig
           ~typed_ast
           ~omit_targ_defaults
           ~strict
           ~ambiguity_strategy
           ast
           target
       in
       let opts = layout_options options in
       Ok (mk_patch ~opts ast new_ast file_content)
     with
    | FailedToInsertType err -> Error (error_to_string err))
  | Error _ as result ->
    let (errs, _) =
      Type_contents.printable_errors_of_file_artifacts_result ~options ~env file_key result
    in
    Error (error_to_string (Expected (FailedToTypeCheck errs)))

let organize_imports ~options ~ast =
  let edits =
    let opts = layout_options options in
    Autofix_imports.organize_imports ~options:opts ast |> flow_loc_patch_to_lsp_edits
  in
  edits

let ast_transforms_of_error =
  ast_transforms_of_error
    ?lazy_error_loc:None
    ?get_ast_from_shared_mem:None
    ?get_haste_module_info:None
    ?get_type_sig:None
