(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Types_js_types

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

let layout_options options =
  let open Js_layout_generator in
  {
    default_opts with
    bracket_spacing = Options.format_bracket_spacing options;
    single_quotes = Options.format_single_quotes options;
  }

let autofix_insert_type_annotation_helper ~options ~ast ~diagnostics ~uri new_ast =
  let open Lsp in
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

let autofix_exports_code_actions
    ~options ~cx ~ast ~file_sig ~tolerable_errors ~typed_ast ~diagnostics uri loc =
  let open Autofix_exports in
  let fixable_locs = set_of_fixable_signature_verification_locations tolerable_errors in
  if LocSet.mem loc fixable_locs then
    fix_signature_verification_error_at_loc ~cx ~file_sig ~typed_ast ast loc
    |> autofix_insert_type_annotation_helper ~options ~ast ~diagnostics ~uri
  else
    []

let autofix_missing_local_annot_code_actions
    ~options ~cx ~ast ~file_sig ~tolerable_errors:_ ~typed_ast ~diagnostics uri loc =
  let open Autofix_missing_local_annots in
  let fixable_locs = map_of_fixable_missing_local_params cx in
  let entry =
    Base.List.find ~f:(fun (err_loc, _) -> Loc.contains err_loc loc) (LocMap.elements fixable_locs)
  in
  match entry with
  | Some (_, type_t) ->
    fix_missing_param_annot_at_loc ~cx ~file_sig ~typed_ast ast loc type_t
    |> autofix_insert_type_annotation_helper ~options ~ast ~diagnostics ~uri
  | None -> []

let refactor_extract_code_actions
    ~options
    ~support_experimental_snippet_text_edit
    ~file_contents
    ~ast
    ~cx
    ~file_sig
    ~typed_ast
    ~reader
    ~only
    uri
    loc =
  if include_extract_refactors only then
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
                    }
                  );
            }
        in
        let tokens =
          let use_strict = Options.modules_are_use_strict options in
          let module_ref_prefix = Options.haste_module_ref_prefix options in
          let module_ref_prefix_LEGACY_INTEROP =
            Options.haste_module_ref_prefix_LEGACY_INTEROP options
          in
          let parse_options =
            {
              Parser_env.components = true;
              enums = true;
              esproposal_decorators = true;
              types = true;
              use_strict;
              module_ref_prefix;
              module_ref_prefix_LEGACY_INTEROP;
            }
          in
          Refactor_extract_utils.AstExtractor.tokens ~parse_options (Some file) file_contents
        in
        Refactor_extract.provide_available_refactors
          ~tokens
          ~ast
          ~cx
          ~file
          ~file_sig
          ~typed_ast
          ~reader
          ~support_experimental_snippet_text_edit
          ~extract_range:loc
        |> List.map lsp_action_from_refactor
  else
    []

let insert_jsdoc_code_actions ~options ~ast uri loc =
  match Insert_jsdoc.insert_stub_for_target ~use_snippets:false loc ast with
  | Some (ast', _) ->
    ast'
    |> Flow_ast_differ.program ast
    |> Replacement_printer.mk_loc_patch_ast_differ ~opts:(layout_options options)
    |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
    |> Base.List.map ~f:(fun edit ->
           (* This hack is needed because the differ doesn't differentiate between
              [comment; \n; node] and [comment; node] *)
           Lsp.TextEdit.{ edit with newText = edit.newText ^ "\n" }
       )
    |> fun edits ->
    let open Lsp in
    [
      CodeAction.Action
        {
          CodeAction.title = "Add JSDoc documentation";
          kind = CodeActionKind.refactor;
          diagnostics = [];
          action = CodeAction.EditOnly WorkspaceEdit.{ changes = UriMap.singleton uri edits };
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
      |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
      |> fun edits ->
      let open Lsp in
      [
        CodeAction.Action
          {
            CodeAction.title;
            kind = CodeActionKind.refactor_rewrite;
            diagnostics = [];
            action = CodeAction.EditOnly WorkspaceEdit.{ changes = UriMap.singleton uri edits };
          };
      ]
    | _ -> []
  else
    []

let main_of_package ~reader package_dir =
  let file_key = File_key.JsonFile (package_dir ^ Filename.dir_sep ^ "package.json") in
  match Parsing_heaps.Reader.get_package_info ~reader file_key with
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

let string_of_path_parts parts =
  let str = String.concat "/" parts in
  let str' = String_utils.rstrip str "/index.js" in
  if str == str' then
    String_utils.rstrip str ".js"
  else
    str'

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
  let (ancestor_rev, to_src, to_req) = find_ancestor_rev src_parts req_parts in
  match to_req with
  | node_modules :: package_dir :: rest when List.mem node_modules node_resolver_dirnames ->
    let package_path =
      package_dir :: node_modules :: ancestor_rev |> Base.List.rev |> String.concat "/"
    in
    (match main_of_package ~reader package_path with
    | Some main when path_matches (String.concat "/" rest) main -> package_dir
    | _ -> string_of_path_parts (package_dir :: rest))
  | _ ->
    let parts =
      if Base.List.is_empty to_src then
        Filename.current_dir_name :: to_req
      else
        (* add `..` for each dir in `to_src`, to relativize `to_req` *)
        Base.List.fold_left ~f:(fun path _ -> Filename.parent_dir_name :: path) ~init:to_req to_src
    in
    string_of_path_parts parts

(** [path_of_modulename src_dir t] converts the Modulename.t [t] to a string
    suitable for importing [t] from a file in [src_dir]. that is, if it is a
    filename, returns the path relative to [src_dir]. *)
let path_of_modulename ~node_resolver_dirnames ~reader src_dir file_key = function
  | Some _ as string_module_name -> string_module_name
  | None ->
    Base.Option.map
      ~f:(fun src_dir ->
        let path = File_key.to_string (Files.chop_flow_ext file_key) in
        node_path ~node_resolver_dirnames ~reader ~src_dir path)
      src_dir

let haste_package_path ~reader ~src_dir require_path =
  match Files.split_path require_path |> Base.List.rev with
  | [] -> None
  | base :: parent_dir_names ->
    let src_parts = Files.split_path src_dir in
    let rec f acc remaining =
      match remaining with
      | [] -> None
      | package_name_candidate :: parent_dir_names ->
        let dependency = Parsing_heaps.get_dependency (Modulename.String package_name_candidate) in
        (match Option.bind dependency (Parsing_heaps.Reader.get_provider ~reader) with
        | Some addr when Parsing_heaps.Reader.is_package_file ~reader addr ->
          let package_path_parts = List.rev (package_name_candidate :: parent_dir_names) in
          let within_package =
            match find_ancestor_rev package_path_parts src_parts with
            (* src is completely within package_path if they have a common ancestor,
               and additional relative path required to get to package path is empty. *)
            | (_, [], _) -> true
            | _ -> false
          in
          if within_package then
            None
          else
            Some
              (match
                 main_of_package ~reader (String.concat Filename.dir_sep package_path_parts)
               with
              | Some main when path_matches (String.concat "/" acc) main -> package_name_candidate
              | _ -> string_of_path_parts (package_name_candidate :: acc))
        | _ -> f (package_name_candidate :: acc) parent_dir_names)
    in
    f [base] parent_dir_names

type text_edits = {
  title: string;
  edits: Lsp.TextEdit.t list;
  from: string;
}

(** Generates the 'from' part of 'import ... from ...' required to import [source] from
    a file in [src_dir] *)
let from_of_source ~options ~reader ~src_dir source =
  match source with
  | Export_index.Global -> None
  | Export_index.Builtin from -> Some from
  | Export_index.File_key from ->
    (match Parsing_heaps.get_file_addr from with
    | None -> None
    | Some addr ->
      (match Parsing_heaps.Reader.get_parse ~reader addr with
      | None -> None
      | Some _ ->
        let module_name =
          match Parsing_heaps.Reader.get_haste_name ~reader addr with
          | Some module_name -> Some module_name
          | None when Options.module_system options = Options.Haste ->
            Base.Option.bind src_dir ~f:(fun src_dir ->
                haste_package_path ~reader ~src_dir (File_key.to_string (Files.chop_flow_ext from))
            )
          | None -> None
        in
        let node_resolver_dirnames = Options.file_options options |> Files.node_resolver_dirnames in
        path_of_modulename ~node_resolver_dirnames ~reader src_dir from module_name))

let text_edits_of_import ~options ~reader ~src_dir ~ast kind name source =
  let from = from_of_source ~options ~reader ~src_dir source in
  match from with
  | None -> None
  | Some from ->
    let title =
      match kind with
      | Export_index.DefaultType -> Printf.sprintf "Import default type from %s" from
      | Export_index.Default -> Printf.sprintf "Import default from %s" from
      | Export_index.Named -> Printf.sprintf "Import from %s" from
      | Export_index.NamedType -> Printf.sprintf "Import type from %s" from
      | Export_index.Namespace -> Printf.sprintf "Import * from %s" from
    in
    let bindings =
      match kind with
      | Export_index.DefaultType -> Autofix_imports.DefaultType name
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

let suggest_imports ~options ~reader ~ast ~diagnostics ~imports_ranked_usage ~exports ~name uri loc
    =
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
    let error_range = Flow_lsp_conversions.loc_to_lsp_range loc in
    let relevant_diagnostics =
      let open PublishDiagnostics in
      let lsp_code = StringCode Error_codes.(string_of_code CannotResolveName) in
      Base.List.filter diagnostics ~f:(fun { source; code; range; _ } ->
          source = Some "Flow" && code = lsp_code && Lsp_helpers.ranges_overlap range error_range
      )
    in
    files
    |> Export_index.ExportMap.bindings
    |> maybe_sort_by_usage ~imports_ranked_usage
    |> Base.List.fold ~init:[] ~f:(fun acc ((source, export_kind), _num) ->
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
                         }
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
    ~reader
    ~cx
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
      ( Parsing_heaps.Reader.get_ast_unsafe ~reader source_file,
        source_file |> File_key.to_string |> File_url.create |> Lsp.DocumentUri.of_string
      )
    else
      (ast, uri)
  in
  let open Base.Option in
  transform ~cx ~file_sig ~ast ~typed_ast loc
  >>| Flow_ast_differ.program ast
  >>| Replacement_printer.mk_loc_patch_ast_differ ~opts:(layout_options options)
  >>| Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
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
                 {
                   Command.title = "";
                   command = Command.Command "log";
                   arguments =
                     ["textDocument/codeAction"; diagnostic_title; title]
                     |> List.map (fun str -> Hh_json.JSON_String str);
                 }
               );
         }
      )

let loc_opt_intersects ~loc ~error_loc =
  match loc with
  | None -> true
  | Some loc -> Loc.intersects error_loc loc

let ast_transforms_of_error ~loc_of_aloc ?loc = function
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
        | Colon -> "Convert to type cast `(<expr>: <type>)`"
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
        | Colon ->
          ( "Convert to type cast `(<expr>: <type>)`",
            "convert_as_expression",
            Autofix_casting_syntax.convert_as_expression
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
    let incorrect_name = Error_message.IncorrectType.incorrect_of_kind kind in
    let replacement_name = Error_message.IncorrectType.replacement_of_kind kind in
    let title = Printf.sprintf "Convert to `%s`" replacement_name in
    let diagnostic_title = Printf.sprintf "convert_%s_type" incorrect_name in
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title;
          diagnostic_title;
          transform = untyped_ast_transform (Autofix_type_name.convert_type kind);
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
          (Error_message.InvalidRendersNullVoidFalse | Error_message.InvalidRendersIterable)
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
      | (Flow_ast.Type.Renders.Maybe, Error_message.InvalidRendersNullVoidFalse) ->
        [
          {
            title = "Simplify `renders?`";
            diagnostic_title = "simplify_renders_maybe";
            transform =
              untyped_ast_transform Autofix_renders_variant.to_renders_maybe_with_best_effort_fixes;
            target_loc = error_loc;
          };
        ]
      | (_, Error_message.InvalidRendersNullVoidFalse) ->
        [
          {
            title = "Switch to `renders?`";
            diagnostic_title = "switch_to_renders_maybe";
            transform =
              untyped_ast_transform Autofix_renders_variant.to_renders_maybe_with_best_effort_fixes;
            target_loc = error_loc;
          };
        ]
      | (_, Error_message.InvalidRendersIterable) ->
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
  | Error_message.EBuiltinLookupFailed { name; reason; _ } ->
    let error_loc = Reason.loc_of_reason reason in
    if loc_opt_intersects ~error_loc ~loc then
      [
        {
          title = "Prefix with `this.`";
          diagnostic_title = "prefix_with_this";
          transform =
            Autofix_class_member_access.fix
              ~loc_of_aloc
              ~member_name:(Reason.display_string_of_name name);
          target_loc = error_loc;
        };
      ]
    else
      []
  | error_message ->
    (match error_message |> Error_message.friendly_message_of_msg Fun.id with
    | Error_message.PropMissing
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
    ~reader
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
          Flow_error.msg_of_error error
          |> Error_message.map_loc_of_error_message (Parsing_heaps.Reader.loc_of_aloc ~reader)
        in
        let (suggest_imports_actions, has_missing_import) =
          match error_message with
          | Error_message.EBuiltinLookupFailed { reason; name; potential_generator = None }
            when Options.autoimports options ->
            let error_loc = Reason.loc_of_reason reason in
            let actions =
              if include_quick_fixes && Loc.intersects error_loc loc then
                let { ServerEnv.exports; _ } = env in
                suggest_imports
                  ~options
                  ~reader
                  ~ast
                  ~diagnostics
                  ~imports_ranked_usage
                  ~exports (* TODO consider filtering out internal names *)
                  ~name:(Reason.display_string_of_name name)
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
            ast_transforms_of_error
              ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
              ~loc
              error_message
            |> Base.List.filter_map ~f:(fun { title; diagnostic_title; transform; target_loc } ->
                   autofix_in_upstream_file
                     ~reader
                     ~cx
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
    let error_range = Flow_lsp_conversions.loc_to_lsp_range error_loc in
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
              {
                Command.title = "";
                command = Command.Command "log";
                arguments =
                  ["textDocument/codeAction"; "fix_parse_error"; title]
                  |> List.map (fun str -> Hh_json.JSON_String str);
              }
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
      | (error_loc, Parse_error.InvalidComponentRenderAnnotation) ->
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
    ~reader
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
  let experimental_code_actions =
    autofix_exports_code_actions
      ~options
      ~cx
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
        ~file_contents
        ~ast
        ~cx
        ~file_sig
        ~typed_ast
        ~reader
        ~only
        uri
        loc
    @ autofix_missing_local_annot_code_actions
        ~options
        ~cx
        ~ast
        ~file_sig
        ~tolerable_errors
        ~typed_ast
        ~diagnostics
        uri
        loc
    @ insert_jsdoc_code_actions ~options ~ast uri loc
    @ refactor_arrow_function_code_actions ~ast ~scope_info ~options ~only uri loc
  in
  let error_fixes =
    code_actions_of_errors
      ~options
      ~reader
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
  let parse_error_fixes = code_actions_of_parse_errors ~diagnostics ~uri ~loc parse_errors in
  let actions = parse_error_fixes @ experimental_code_actions @ error_fixes in
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
let autofix_imports ~options ~env ~reader ~cx ~ast ~uri =
  let errors = Context.errors cx in
  let { ServerEnv.exports; _ } = env in
  let src_dir = Lsp_helpers.lsp_uri_to_path uri |> Filename.dirname |> Base.Option.return in
  (* collect imports for all of the undefined variables in the file *)
  let imports =
    Flow_error.ErrorSet.fold
      (fun error imports ->
        match
          Flow_error.msg_of_error error
          |> Error_message.map_loc_of_error_message (Parsing_heaps.Reader.loc_of_aloc ~reader)
        with
        | Error_message.EBuiltinLookupFailed { reason; name; potential_generator = None }
          when Options.autoimports options ->
          let name = Reason.display_string_of_name name in
          let error_loc = Reason.loc_of_reason reason in
          (match preferred_import ~ast ~exports name error_loc with
          | Some (source, export_kind) ->
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
            ExportSourceMap.add source (ExportKindMap.add export_kind names bindings) imports
          | None -> imports)
        | _ -> imports)
      errors
      ExportSourceMap.empty
  in
  let added_imports =
    ExportSourceMap.fold
      (fun source names_of_kinds added_imports ->
        let from = from_of_source ~options ~reader ~src_dir source in
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
  let edits =
    let opts = layout_options options in
    Autofix_imports.add_imports ~options:opts ~added_imports ast
    |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
  in
  edits

let autofix_exports ~options ~env ~profiling ~file_key ~file_content =
  let open Autofix_exports in
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
  | Ok
      ( Parse_artifacts { ast; file_sig; tolerable_errors; _ },
        Typecheck_artifacts { cx; typed_ast; obj_to_obj_map = _ }
      ) ->
    let sv_errors = set_of_fixable_signature_verification_locations tolerable_errors in
    let (new_ast, it_errs) =
      fix_signature_verification_errors ~file_key ~cx ~file_sig ~typed_ast ast sv_errors
    in
    let opts = layout_options options in
    Ok (Insert_type.mk_patch ~opts ast new_ast file_content, it_errs)
  | Error _ -> Error "Failed to type-check file"

let autofix_missing_local_annot ~options ~env ~profiling ~file_key ~file_content =
  let open Autofix_missing_local_annots in
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
  | Ok
      ( Parse_artifacts { ast; file_sig; _ },
        Typecheck_artifacts { cx; typed_ast; obj_to_obj_map = _ }
      ) ->
    let new_ast = fix_all_missing_param_annot_errors_in_file ~cx ~file_sig ~typed_ast ast in
    let opts = layout_options options in
    Ok (Insert_type.mk_patch ~opts ast new_ast file_content)
  | Error _ -> Error "Failed to type-check file"

let insert_type
    ~options
    ~env
    ~profiling
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
    Autofix_imports.organize_imports ~options:opts ast
    |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
  in
  edits

module For_tests = struct
  let path_of_modulename = path_of_modulename
end
