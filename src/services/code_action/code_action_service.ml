(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Types_js_types

let include_quick_fixes only =
  Base.Option.value_map ~default:true ~f:Lsp.CodeActionKind.(contains_kind quickfix) only

let include_refactors only =
  Base.Option.value_map ~default:true ~f:Lsp.CodeActionKind.(contains_kind refactor) only

let layout_options options =
  Js_layout_generator.
    {
      default_opts with
      bracket_spacing = Options.format_bracket_spacing options;
      single_quotes = Options.format_single_quotes options;
    }

let autofix_exports_code_actions
    ~options ~full_cx ~ast ~file_sig ~tolerable_errors ~typed_ast ~diagnostics uri loc =
  let open Lsp in
  let open Autofix_exports in
  let fixable_locs = set_of_fixable_signature_verification_locations tolerable_errors in
  if LocSet.mem loc fixable_locs then
    match fix_signature_verification_error_at_loc ~full_cx ~file_sig ~typed_ast ast loc with
    | new_ast ->
      let diff = Insert_type.mk_diff ast new_ast in
      let opts = layout_options options in
      let edits =
        Replacement_printer.mk_loc_patch_ast_differ ~opts diff
        |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
      in
      [
        CodeAction.Action
          {
            CodeAction.title = "insert type annotation";
            kind = CodeActionKind.quickfix;
            (* Handing back the diagnostics we were given is a placeholder for
               eventually generating the diagnostics for the errors we are fixing *)
            diagnostics;
            action = CodeAction.EditOnly WorkspaceEdit.{ changes = UriMap.singleton uri edits };
          };
      ]
  else
    []

let refactor_extract_code_actions
    ~options
    ~support_experimental_snippet_text_edit
    ~ast
    ~full_cx
    ~file_sig
    ~typed_ast
    ~reader
    ~only
    uri
    loc =
  if Options.refactor options && include_refactors only then
    if Loc.(loc.start = loc._end) then
      []
    else
      match loc.Loc.source with
      | None -> []
      | Some file ->
        let lsp_action_from_refactor { Refactor_extract.title; new_ast; added_imports } =
          let diff = Insert_type.mk_diff ast new_ast in
          let opts = layout_options options in
          let edits =
            Autofix_imports.add_imports ~options:opts ~added_imports ast
            @ Replacement_printer.mk_loc_patch_ast_differ ~opts diff
            |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
          in
          let diagnostic_title = "refactor_extract" in
          let open Lsp in
          CodeAction.Action
            {
              CodeAction.title;
              kind = CodeActionKind.refactor_extract;
              diagnostics = [];
              action =
                CodeAction.BothEditThenCommand
                  ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                    {
                      Command.title = "";
                      command = Command.Command "log";
                      arguments =
                        ["textDocument/codeAction"; diagnostic_title; title]
                        |> List.map (fun str -> Hh_json.JSON_String str);
                    } );
            }
        in
        Refactor_extract.provide_available_refactors
          ~ast
          ~full_cx
          ~file
          ~file_sig:(File_sig.abstractify_locs file_sig)
          ~typed_ast
          ~reader
          ~support_experimental_snippet_text_edit
          ~extract_range:loc
        |> List.map lsp_action_from_refactor
  else
    []

let main_of_package ~reader package_dir =
  let json_path = package_dir ^ "/package.json" in
  match Package_heaps.Reader.get_package ~reader json_path with
  | Some (Ok package) -> Package_json.main package
  | Some (Error _)
  | None ->
    None

(** [find_ancestor_rev a_parts b_parts], where [a_parts] and [b_parts] are two paths split
    into segments (see [Files.split_path]), returns [(ancestor_parts, a_relative, b_relative)],
    where [ancestor_parts] are the common prefix parts **reversed**, [a_relative] is the
    remaining parts from the ancestor to [a_parts], and [b_relative] is the remaining parts
    from the ancestor to [b_parts].

    for example, [find_ancestor_rev ["/a"; "b"; "c"; "d"] ["/a"; "b"; "e"; "f"]] returns
    [(["b"; "/a"], ["c"; "d"], ["e"; "f"])] *)
let find_ancestor_rev =
  let rec helper acc = function
    | (dir1 :: rest1, dir2 :: rest2) when dir1 = dir2 -> helper (dir1 :: acc) (rest1, rest2)
    | (a_rel, b_rel) -> (acc, a_rel, b_rel)
  in
  (fun a_parts b_parts -> helper [] (a_parts, b_parts))

(** [path_matches expected actual] returns true if [actual] is the same as [expected], ignoring
    a potential leading [./] on [actual]. *)
let path_matches expected actual =
  expected = actual || (Filename.is_relative actual && actual = "./" ^ expected)

(** [node_path ~node_resolver_dirnames ~reader src_dir require_path] converts absolute path
    [require_path] into a Node-compatible "require" path relative to [src_dir], taking into
    account node's hierarchical search for [node_modules].

    That is, if [require_path] is within a [node_modules] folder in [src_dir] or one of
    [src_dir]'s parents, then the [node_modules] prefix is removed. If the package's
    [package.json] has a [main] field, that suffix is also removed.

    If not part of [node_modules], then [require_path] is relativized with respect to
    [src_dir].

    Lastly, if the path ends with [index.js] or [.js], those default suffixes are also
    removed. *)
let node_path ~node_resolver_dirnames ~reader ~src_dir require_path =
  let require_path = String_utils.rstrip require_path Files.flow_ext in
  let src_parts = Files.split_path src_dir in
  let req_parts = Files.split_path require_path in
  let string_of_parts parts =
    let str = String.concat "/" parts in
    let str' = String_utils.rstrip str "/index.js" in
    if str == str' then
      String_utils.rstrip str ".js"
    else
      str'
  in
  let (ancestor_rev, to_src, to_req) = find_ancestor_rev src_parts req_parts in
  match to_req with
  | node_modules :: package_dir :: rest when List.mem node_modules node_resolver_dirnames ->
    let package_path =
      package_dir :: node_modules :: ancestor_rev |> Base.List.rev |> String.concat "/"
    in
    (match main_of_package ~reader package_path with
    | Some main when path_matches (String.concat "/" rest) main -> package_dir
    | _ -> string_of_parts (package_dir :: rest))
  | _ ->
    let parts =
      if Base.List.is_empty to_src then
        Filename.current_dir_name :: to_req
      else
        (* add `..` for each dir in `to_src`, to relativize `to_req` *)
        Base.List.fold_left ~f:(fun path _ -> Filename.parent_dir_name :: path) ~init:to_req to_src
    in
    string_of_parts parts

(** [path_of_modulename src_dir t] converts the Modulename.t [t] to a string
    suitable for importing [t] from a file in [src_dir]. that is, if it is a
    filename, returns the path relative to [src_dir]. *)
let path_of_modulename ~node_resolver_dirnames ~reader src_dir = function
  | Modulename.String str -> Some str
  | Modulename.Filename file_key ->
    Base.Option.map
      ~f:(fun src_dir ->
        let path = File_key.to_string file_key in
        node_path ~node_resolver_dirnames ~reader ~src_dir path)
      src_dir

type text_edits = {
  title: string;
  edits: Lsp.TextEdit.t list;
  from: string;
}

let text_edits_of_import ~options ~reader ~src_dir ~ast kind name source =
  let from =
    match source with
    | Export_index.Global -> None
    | Export_index.Builtin from -> Some from
    | Export_index.File_key from ->
      (match Module_heaps.Reader.get_info ~reader ~audit:Expensive.ok from with
      | None -> None
      | Some info ->
        let node_resolver_dirnames = Options.file_options options |> Files.node_resolver_dirnames in
        (match
           path_of_modulename ~node_resolver_dirnames ~reader src_dir info.Module_heaps.module_name
         with
        | None -> None
        | Some from -> Some from))
  in
  match from with
  | None -> None
  | Some from ->
    let title =
      match kind with
      | Export_index.Default -> Printf.sprintf "Import default from %s" from
      | Export_index.Named -> Printf.sprintf "Import from %s" from
      | Export_index.NamedType -> Printf.sprintf "Import type from %s" from
      | Export_index.Namespace -> Printf.sprintf "Import * from %s" from
    in
    let bindings =
      match kind with
      | Export_index.Default -> Autofix_imports.Default name
      | Export_index.Named ->
        Autofix_imports.Named [{ Autofix_imports.remote_name = name; local_name = None }]
      | Export_index.NamedType ->
        Autofix_imports.NamedType [{ Autofix_imports.remote_name = name; local_name = None }]
      | Export_index.Namespace -> Autofix_imports.Namespace name
    in
    let edits =
      let options = layout_options options in
      Autofix_imports.add_import ~options ~bindings ~from ast
      |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
    in
    Some { title; edits; from }

let suggest_imports ~options ~reader ~ast ~diagnostics ~exports ~name uri loc =
  let open Lsp in
  let files =
    if Autofix_imports.loc_is_type ~ast loc then
      Export_search.get_types name exports
    else
      Export_search.get_values name exports
  in
  if Export_index.ExportSet.is_empty files then
    []
  else
    let src_dir = Lsp_helpers.lsp_uri_to_path uri |> Filename.dirname |> Base.Option.return in
    let error_range = Flow_lsp_conversions.loc_to_lsp_range loc in
    let relevant_diagnostics =
      let open PublishDiagnostics in
      let lsp_code = StringCode Error_codes.(string_of_code CannotResolveName) in
      Base.List.filter diagnostics ~f:(fun { source; code; range; _ } ->
          source = Some "Flow" && code = lsp_code && Lsp_helpers.ranges_overlap range error_range)
    in
    Export_index.ExportSet.fold
      (fun (source, export_kind) acc ->
        match text_edits_of_import ~options ~reader ~src_dir ~ast export_kind name source with
        | None -> acc
        | Some { edits; title; from = _ } ->
          let command =
            CodeAction.Action
              {
                CodeAction.title;
                kind = CodeActionKind.quickfix;
                diagnostics = relevant_diagnostics;
                action =
                  CodeAction.BothEditThenCommand
                    ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                      {
                        Command.title = "";
                        command = Command.Command "log";
                        arguments =
                          ["textDocument/codeAction"; "import"; title]
                          |> List.map (fun str -> Hh_json.JSON_String str);
                      } );
              }
          in
          command :: acc)
      files
      []

let autofix_in_upstream_file
    ~reader ~diagnostics ~ast ~options ~title ~transform ~diagnostic_title uri loc =
  let (ast, uri) =
    let src = Loc.source loc in
    let ast_src = fst ast |> Loc.source in
    if ast_src <> src then
      (* load ast of upstream file
         In order to appear in an error, a loc must have a source *)
      let source_file = Base.Option.value_exn src in
      ( Parsing_heaps.Reader.get_ast_unsafe ~reader source_file,
        source_file |> File_key.to_string |> File_url.create |> Lsp.DocumentUri.of_string )
    else
      (ast, uri)
  in
  let mk_diff ast new_ast = Flow_ast_differ.(program Standard ast new_ast) in
  let open Lsp in
  match transform ast loc with
  | new_ast ->
    let diff = mk_diff ast new_ast in
    let opts = layout_options options in
    let edits =
      Replacement_printer.mk_loc_patch_ast_differ ~opts diff
      |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
    in
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
              {
                Command.title = "";
                command = Command.Command "log";
                arguments =
                  ["textDocument/codeAction"; diagnostic_title; title]
                  |> List.map (fun str -> Hh_json.JSON_String str);
              } );
      }

type ast_transform_of_error = {
  title: string;
  diagnostic_title: string;
  transform: (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Program.t;
  target_loc: Loc.t;
}

let loc_opt_intersects ?loc ~error_loc =
  match loc with
  | None -> true
  | Some loc -> Loc.intersects error_loc loc

let ast_transform_of_error ?loc = function
  | Error_message.EEnumInvalidMemberAccess { reason; suggestion = Some fixed_prop_name; _ } ->
    let error_loc = Reason.loc_of_reason reason in
    if loc_opt_intersects ~error_loc ?loc then
      let original_prop_name = reason |> Reason.desc_of_reason |> Reason.string_of_desc in
      let title = Printf.sprintf "Replace %s with `%s`" original_prop_name fixed_prop_name in
      Some
        {
          title;
          diagnostic_title = "replace_enum_prop_typo_at_target";
          transform = Autofix_prop_typo.replace_prop_typo_at_target ~fixed_prop_name;
          target_loc = error_loc;
        }
    else
      None
  | Error_message.EClassToObject (reason_class, reason_obj, _) ->
    let error_loc = Reason.loc_of_reason reason_class in
    if loc_opt_intersects ~error_loc ?loc then
      let obj_loc = Reason.def_loc_of_reason reason_obj in
      let original = reason_obj |> Reason.desc_of_reason ~unwrap:false |> Reason.string_of_desc in
      let title = Utils_js.spf "Rewrite %s as an interface" original in
      let diagnostic_title = "replace_obj_with_interface" in
      Some
        {
          title;
          diagnostic_title;
          transform = Autofix_interface.replace_object_at_target;
          target_loc = obj_loc;
        }
    else
      None
  | Error_message.EMethodUnbinding { reason_op; reason_prop; _ } ->
    let error_loc = Reason.loc_of_reason reason_op in
    if loc_opt_intersects ~error_loc ?loc then
      let method_loc = Reason.def_loc_of_reason reason_prop in
      let original = reason_prop |> Reason.desc_of_reason ~unwrap:false |> Reason.string_of_desc in
      let title = Utils_js.spf "Rewrite %s as an arrow function" original in
      let diagnostic_title = "replace_method_with_arrow" in
      Some
        {
          title;
          diagnostic_title;
          transform = Autofix_method.replace_method_at_target;
          target_loc = method_loc;
        }
    else
      None
  | error_message ->
    (match error_message |> Error_message.friendly_message_of_msg with
    | Error_message.PropMissing
        { loc = error_loc; suggestion = Some suggestion; prop = Some prop_name; _ } ->
      if loc_opt_intersects ~error_loc ?loc then
        let title = Printf.sprintf "Replace `%s` with `%s`" prop_name suggestion in
        let diagnostic_title = "replace_prop_typo_at_target" in
        Some
          {
            title;
            diagnostic_title;
            transform = Autofix_prop_typo.replace_prop_typo_at_target ~fixed_prop_name:suggestion;
            target_loc = error_loc;
          }
      else
        None
    | Error_message.IncompatibleUse
        { loc = error_loc; upper_kind = Error_message.IncompatibleGetPropT _; reason_lower; _ } ->
      (match (loc_opt_intersects ~error_loc ?loc, Reason.desc_of_reason reason_lower) with
      | (true, ((Reason.RVoid | Reason.RNull | Reason.RVoidedNull | Reason.RNullOrVoid) as r)) ->
        let title =
          Printf.sprintf
            "Add optional chaining for object that might be `%s`"
            (Reason.string_of_desc r)
        in
        let diagnostic_title = "add_optional_chaining" in
        Some
          {
            title;
            diagnostic_title;
            transform = Autofix_optional_chaining.add_optional_chaining;
            target_loc = error_loc;
          }
      | _ -> None)
    | _ -> None)

let code_actions_of_errors ~options ~reader ~env ~ast ~diagnostics ~errors ~only uri loc =
  let include_quick_fixes = include_quick_fixes only in
  Flow_error.ErrorSet.fold
    (fun error actions ->
      match
        Flow_error.msg_of_error error
        |> Error_message.map_loc_of_error_message (Parsing_heaps.Reader.loc_of_aloc ~reader)
      with
      | Error_message.EBuiltinLookupFailed { reason; name = Some name }
        when Options.autoimports options ->
        let error_loc = Reason.loc_of_reason reason in
        if include_quick_fixes && Loc.intersects error_loc loc then
          let { ServerEnv.exports; _ } = env in
          suggest_imports
            ~options
            ~reader
            ~ast
            ~diagnostics
            ~exports (* TODO consider filtering out internal names *)
            ~name:(Reason.display_string_of_name name)
            uri
            loc
          @ actions
        else
          actions
      | error_message ->
        if include_quick_fixes then
          match ast_transform_of_error ~loc error_message with
          | None -> actions
          | Some { title; diagnostic_title; transform; target_loc } ->
            autofix_in_upstream_file
              ~reader
              ~diagnostics
              ~ast
              ~options
              ~title
              ~diagnostic_title
              ~transform
              uri
              target_loc
            :: actions
        else
          actions)
    errors
    []

let code_actions_of_parse_errors ~diagnostics ~uri ~loc parse_errors =
  Base.List.fold_left
    ~f:(fun acc parse_error ->
      match parse_error with
      | (error_loc, Parse_error.UnexpectedTokenWithSuggestion (token, suggestion)) ->
        if Loc.intersects error_loc loc then
          let title = Printf.sprintf "Replace `%s` with `%s`" token suggestion in
          let error_range = Flow_lsp_conversions.loc_to_lsp_range error_loc in
          let open Lsp in
          let relevant_diagnostics =
            diagnostics |> List.filter (fun PublishDiagnostics.{ range; _ } -> range = error_range)
          in
          let textEdit = TextEdit.{ range = error_range; newText = suggestion } in
          CodeAction.Action
            {
              CodeAction.title;
              kind = CodeActionKind.quickfix;
              diagnostics = relevant_diagnostics;
              action =
                CodeAction.BothEditThenCommand
                  ( WorkspaceEdit.{ changes = UriMap.singleton uri [textEdit] },
                    {
                      Command.title = "";
                      command = Command.Command "log";
                      arguments =
                        ["textDocument/codeAction"; "fix_parse_error"; title]
                        |> List.map (fun str -> Hh_json.JSON_String str);
                    } );
            }
          :: acc
        else
          acc
      | _ -> acc)
    ~init:[]
    parse_errors

(** List of code actions we implement. *)
let supported_code_actions options =
  let actions = [Lsp.CodeActionKind.quickfix] in
  if Options.refactor options then
    Lsp.CodeActionKind.refactor_extract :: actions
  else
    actions

(** Determines if at least one of the kinds in [only] is supported. *)
let kind_is_supported ~options only =
  match only with
  | None -> true
  | Some only ->
    let supported = supported_code_actions options in
    Base.List.exists ~f:(fun kind -> Lsp.CodeActionKind.contains_kind kind supported) only

let code_actions_at_loc
    ~options
    ~lsp_init_params
    ~env
    ~reader
    ~cx
    ~file_sig
    ~tolerable_errors
    ~ast
    ~typed_ast
    ~parse_errors
    ~diagnostics
    ~only
    ~uri
    ~loc =
  let experimental_code_actions =
    autofix_exports_code_actions
      ~options
      ~full_cx:cx
      ~ast
      ~file_sig
      ~tolerable_errors
      ~typed_ast
      ~diagnostics
      uri
      loc
    @ refactor_extract_code_actions
        ~options
        ~support_experimental_snippet_text_edit:
          (Lsp_helpers.supports_experimental_snippet_text_edit lsp_init_params)
        ~ast
        ~full_cx:cx
        ~file_sig
        ~typed_ast
        ~reader
        ~only
        uri
        loc
  in
  let error_fixes =
    code_actions_of_errors
      ~options
      ~reader
      ~env
      ~ast
      ~diagnostics
      ~errors:(Context.errors cx)
      ~only
      uri
      loc
  in
  let parse_error_fixes = code_actions_of_parse_errors ~diagnostics ~uri ~loc parse_errors in
  Lwt.return (Ok (parse_error_fixes @ experimental_code_actions @ error_fixes))

let autofix_exports ~options ~env ~profiling ~file_key ~file_content =
  let open Autofix_exports in
  let%lwt file_artifacts =
    let%lwt ((_, parse_errs) as intermediate_result) =
      Types_js.parse_contents ~options ~profiling file_content file_key
    in
    if not (Flow_error.ErrorSet.is_empty parse_errs) then
      Lwt.return (Error parse_errs)
    else
      Types_js.type_parse_artifacts ~options ~env ~profiling file_key intermediate_result
  in
  match file_artifacts with
  | Ok
      ( Parse_artifacts { ast; file_sig; tolerable_errors; _ },
        Typecheck_artifacts { cx = full_cx; typed_ast } ) ->
    let sv_errors = set_of_fixable_signature_verification_locations tolerable_errors in
    let (new_ast, it_errs) =
      fix_signature_verification_errors ~file_key ~full_cx ~file_sig ~typed_ast ast sv_errors
    in
    let opts = layout_options options in
    Lwt.return (Ok (Insert_type.mk_patch ~opts ast new_ast file_content, it_errs))
  | Error _ -> Lwt.return (Error "Failed to type-check file")

let insert_type
    ~options
    ~env
    ~profiling
    ~file_key
    ~file_content
    ~target
    ~expand_aliases
    ~omit_targ_defaults
    ~location_is_strict:strict
    ~ambiguity_strategy =
  let open Insert_type in
  let%lwt file_artifacts =
    let%lwt ((_, parse_errs) as intermediate_result) =
      Types_js.parse_contents ~options ~profiling file_content file_key
    in
    (* It's not clear to me (nmote) that we actually should abort when we see parse errors. Maybe
     * we should continue on here. I'm inserting this logic during the migration away from
     * typecheck_contents because it's behavior-preserving, but this may be worth revisiting. *)
    if not (Flow_error.ErrorSet.is_empty parse_errs) then
      Lwt.return (Error parse_errs)
    else
      Types_js.type_parse_artifacts ~options ~env ~profiling file_key intermediate_result
  in
  match file_artifacts with
  | Ok (Parse_artifacts { ast; file_sig; _ }, Typecheck_artifacts { cx = full_cx; typed_ast }) ->
    let result =
      try
        let new_ast =
          Insert_type.insert_type
            ~full_cx
            ~file_sig
            ~typed_ast
            ~expand_aliases
            ~omit_targ_defaults
            ~strict
            ~ambiguity_strategy
            ast
            target
        in
        let opts = layout_options options in
        Ok (mk_patch ~opts ast new_ast file_content)
      with
      | FailedToInsertType err -> Error (error_to_string err)
    in
    Lwt.return result
  | Error _ as result ->
    let (errs, _) =
      Types_js.printable_errors_of_file_artifacts_result ~options ~env file_key result
    in
    Lwt.return (Error (error_to_string (Expected (FailedToTypeCheck errs))))

let suggest ~options ~env ~profiling file_key file_content =
  let%lwt file_artifacts_result =
    let%lwt ((_, parse_errs) as intermediate_result) =
      Types_js.parse_contents ~options ~profiling file_content file_key
    in
    if not (Flow_error.ErrorSet.is_empty parse_errs) then
      Lwt.return (Error parse_errs)
    else
      Types_js.type_parse_artifacts ~options ~env ~profiling file_key intermediate_result
  in
  let (tc_errors, tc_warnings) =
    Types_js.printable_errors_of_file_artifacts_result ~options ~env file_key file_artifacts_result
  in
  match file_artifacts_result with
  | Ok (Parse_artifacts { ast; file_sig; _ }, Typecheck_artifacts { cx; typed_ast = tast }) ->
    let file_sig = File_sig.abstractify_locs file_sig in
    let ty_query = Query_types.suggest_types cx file_sig tast in
    let exact_by_default = Options.exact_by_default options in
    let visitor = new Suggest_visitor.visitor ~exact_by_default ~ty_query in
    let ast_with_suggestions = visitor#program ast in
    let suggest_warnings = visitor#warnings () in
    let ast_diff = Flow_ast_differ.(program Standard ast ast_with_suggestions) in
    let opts = layout_options options in
    let file_patch = Replacement_printer.mk_patch_ast_differ ~opts ast_diff file_content in
    Lwt.return (Ok (tc_errors, tc_warnings, suggest_warnings, file_patch))
  | Error _ -> Lwt.return (Error tc_errors)

module For_tests = struct
  let path_of_modulename = path_of_modulename
end
