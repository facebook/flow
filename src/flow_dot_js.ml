(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Js = Js_of_ocaml.Js
module Sys_js = Js_of_ocaml.Sys_js

(* We do not expect to encounter a keyed location in flow_dot_js *)
let loc_of_aloc = ALoc.to_loc_exn

let error_of_parse_error source_file (loc, err) =
  Error_message.EParseError (ALoc.of_loc loc, err)
  |> Flow_error.error_of_msg ~source_file
  |> Flow_intermediate_error.make_intermediate_error ~loc_of_aloc
  |> Flow_intermediate_error.to_printable_error ~loc_of_aloc ~strip_root:None

let parse_content file content =
  let parse_options = Some Parser_env.permissive_parse_options in
  let (ast, parse_errors) =
    Parser_flow.program_file ~fail:false ~parse_options content (Some file)
  in
  if parse_errors <> [] then
    let converted =
      List.fold_left
        (fun acc parse_error ->
          Flow_errors_utils.ConcreteLocPrintableErrorSet.add
            (error_of_parse_error file parse_error)
            acc)
        Flow_errors_utils.ConcreteLocPrintableErrorSet.empty
        parse_errors
    in
    Error converted
  else
    let fsig = File_sig.program ~file_key:file ~ast ~opts:File_sig.default_opts in
    Ok (ast, fsig)

let array_of_list f lst = Array.of_list (List.map f lst)

let rec js_of_json = function
  | Hh_json.JSON_Object props ->
    let props = array_of_list (fun (k, v) -> (k, js_of_json v)) props in
    Js.Unsafe.inject (Js.Unsafe.obj props)
  | Hh_json.JSON_Array items ->
    let items = array_of_list js_of_json items in
    Js.Unsafe.inject (Js.array items)
  | Hh_json.JSON_String str -> Js.Unsafe.inject (Js.string str)
  | Hh_json.JSON_Number num -> Js.Unsafe.inject (Js.number_of_float (float_of_string num))
  | Hh_json.JSON_Bool value -> Js.Unsafe.inject (Js.bool value)
  | Hh_json.JSON_Null -> Js.Unsafe.inject Js.null

let load_lib_files files =
  (* iterate in reverse override order *)
  let asts =
    List.rev_map
      (fun file ->
        let lib_content = Sys_utils.cat file in
        let lib_file = File_key.LibFile file in
        let (ast, _) = parse_content lib_file lib_content |> Result.get_ok in
        ast)
      files
  in
  let sig_opts =
    let open Type_sig_options in
    {
      munge = false;
      facebook_keyMirror = false;
      enable_relay_integration = false;
      relay_integration_module_prefix = None;
      suppress_types = SSet.empty;
      facebook_fbt = None;
      max_literal_len = 100;
      exact_by_default = true;
      enable_enums = true;
      enable_component_syntax = true;
      component_syntax_enabled_in_config = true;
      enable_ts_syntax = true;
      hook_compatibility = true;
      casting_syntax = Options.CastingSyntax.Both;
      for_builtins = true;
      locs_to_dirtify = [];
    }
  in
  Merge_js.merge_lib_files ~sig_opts asts |> snd

let stub_metadata ~root ~checked =
  {
    Context.checked (* local *);
    include_suppressions = false;
    jsx = Options.Jsx_react;
    munge_underscores = false;
    strict = false;
    strict_local = false;
    available_platforms = None;
    has_explicit_supports_platform = false;
    verbose = None;
    slow_to_check_logging = Slow_to_check_logging.default;
    (* global *)
    automatic_require_default = false;
    babel_loose_array_spread = false;
    casting_syntax = Options.CastingSyntax.Both;
    component_syntax = true;
    hook_compatibility = true;
    hook_compatibility_includes = [];
    hook_compatibility_excludes = [];
    react_rules =
      Options.
        [ValidateRefAccessDuringRender; DeepReadOnlyProps; DeepReadOnlyHookReturns; RulesOfHooks];
    react_rules_always = false;
    dev_only_refinement_info_as_errors = false;
    enable_const_params = false;
    enable_enums = true;
    enable_jest_integration = false;
    enable_pattern_matching_expressions = true;
    enable_relay_integration = false;
    exact_by_default = true;
    facebook_fbs = None;
    facebook_fbt = None;
    facebook_module_interop = false;
    file_options = Files.default_options;
    ignore_non_literal_requires = false;
    max_literal_length = 100;
    max_workers = 0;
    missing_module_generators = [];
    no_unchecked_indexed_access = false;
    react_custom_jsx_typing = false;
    react_ref_as_prop = Options.ReactRefAsProp.PartialSupport;
    react_runtime = Options.ReactRuntimeAutomatic;
    recursion_limit = 10000;
    relay_integration_esmodules = false;
    relay_integration_excludes = [];
    relay_integration_module_prefix = None;
    relay_integration_module_prefix_includes = [];
    root;
    strict_es6_import_export = false;
    strip_root = true;
    suppress_types = SSet.of_list ["$FlowFixMe"; "$FlowIssue"; "$FlowIgnore"; "$FlowExpectedError"];
    ts_syntax = true;
    type_expansion_recursion_limit = 3;
    use_mixed_in_catch_variables = false;
    ban_spread_key_props = false;
  }

let master_cx_ref : (File_path.t * Context.master_context) option ref = ref None

let get_master_cx root =
  match !master_cx_ref with
  | None -> failwith "builtins not initialized"
  | Some (prev_root, master_cx) ->
    assert (prev_root = root);
    master_cx

let init_builtins filenames =
  let root = File_path.dummy_path in
  master_cx_ref := Some (root, load_lib_files filenames)

(* Keep this in sync with configSchema below.
 *
 * Precondition: the js_config_object must be validated by try-flow to match the schema shape. *)
let merge_custom_check_config js_config_object metadata =
  let babel_loose_array_spread =
    Js.Unsafe.get js_config_object "babel_loose_array_spread" |> Js.to_bool
  in
  let enable_const_params =
    Js.Unsafe.get js_config_object "experimental.const_params" |> Js.to_bool
  in
  let ts_syntax = Js.Unsafe.get js_config_object "experimental.ts_syntax" |> Js.to_bool in
  let enable_enums = Js.Unsafe.get js_config_object "enums" |> Js.to_bool in
  let exact_by_default = Js.Unsafe.get js_config_object "exact_by_default" |> Js.to_bool in
  let react_runtime =
    match Js.Unsafe.get js_config_object "react.runtime" |> Js.to_string with
    | "automatic" -> Options.ReactRuntimeAutomatic
    | "classic" -> Options.ReactRuntimeClassic
    | s -> failwith ("Unsupported config option: " ^ s)
  in
  let use_mixed_in_catch_variables =
    Js.Unsafe.get js_config_object "use_mixed_in_catch_variables" |> Js.to_bool
  in
  {
    metadata with
    Context.babel_loose_array_spread;
    enable_const_params;
    enable_enums;
    exact_by_default;
    react_runtime;
    ts_syntax;
    use_mixed_in_catch_variables;
  }

(* Keep this in sync with configSchema below. *)
let supported_lints =
  [
    "deprecated-type";
    "sketchy-null";
    "sketchy-number";
    "unclear-type";
    "unnecessary-invariant";
    "unnecessary-optional-chain";
    "unsafe-getters-setters";
    "unused-promise";
  ]

let merge_custom_lint_config js_config_object severities =
  Base.List.fold supported_lints ~init:severities ~f:(fun acc lint ->
      let enabled = Js.Unsafe.get js_config_object lint |> Js.to_bool in
      if enabled then
        match Lints.kinds_of_string lint with
        | Some kinds ->
          Base.List.fold kinds ~init:acc ~f:(fun acc lint_kind ->
              LintSettings.set_value lint_kind (Severity.Err, None) acc
          )
        | None -> acc
      else
        acc
  )

let init_infer_and_merge ~root filename js_config_object docblock ast file_sig =
  (* create cx *)
  let master_cx = get_master_cx root in
  let ccx = Context.make_ccx () in
  let metadata =
    stub_metadata ~root ~checked:true
    |> merge_custom_check_config js_config_object
    |> Context.docblock_overrides docblock filename
  in
  (* flow.js does not use abstract locations, so this is not used *)
  let aloc_table = lazy (ALoc.empty_table filename) in
  let resolved_requires = ref SMap.empty in
  let resolve_require mref = SMap.find mref !resolved_requires in
  let cx =
    Context.make
      ccx
      metadata
      filename
      aloc_table
      resolve_require
      (Merge_js.mk_builtins metadata master_cx)
  in
  resolved_requires :=
    SMap.mapi
      (fun mref _locs ->
        let builtins = Context.builtins cx in
        match Builtins.get_builtin_module_opt builtins mref with
        | Some t -> Context.TypedModule t
        | None -> Context.MissingModule mref)
      (File_sig.require_loc_map file_sig);
  (* infer ast *)
  let (_, { Flow_ast.Program.all_comments = comments; _ }) = ast in
  let ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  let lint_severities =
    let base_severities = LintSettings.empty_severities in
    let severities = merge_custom_lint_config js_config_object base_severities in
    let strict_mode = StrictModeSettings.empty in
    Merge_js.get_lint_severities metadata strict_mode severities
  in
  (cx, metadata, comments, ast, lint_severities)

let infer_and_merge ~root filename js_config_object docblock ast file_sig =
  let (cx, metadata, comments, ast, lint_severities) =
    init_infer_and_merge ~root filename js_config_object docblock ast file_sig
  in
  let typed_ast = Type_inference_js.infer_ast cx filename metadata comments ast ~lint_severities in
  (cx, typed_ast)

let ac_infer ~root filename js_config_object docblock ast file_sig =
  let (cx, _, _, aloc_ast, _) =
    init_infer_and_merge ~root filename js_config_object docblock ast file_sig
  in
  assert (Context.is_checked cx);
  Type_inference_js.initialize_env cx aloc_ast;
  (cx, aloc_ast)

let check_content ~filename ~content ~js_config_object =
  let stdin_file = Some (File_path.make_unsafe filename, content) in
  let root = File_path.dummy_path in
  let filename = File_key.SourceFile filename in
  let (errors, warnings) =
    match parse_content filename content with
    | Ok (ast, file_sig) ->
      let (_, docblock) =
        Docblock_parser.(
          parse_docblock
            ~max_tokens:docblock_max_tokens
            ~file_options:Files.default_options
            filename
            content
        )
      in
      let (cx, _) = infer_and_merge ~root filename js_config_object docblock ast file_sig in
      let suppressions = Context.error_suppressions cx in
      let errors = Context.errors cx in
      let severity_cover = Context.severity_cover cx in
      let include_suppressions = Context.include_suppressions cx in
      let aloc_tables = Utils_js.FilenameMap.empty in
      let (errors, warnings, suppressions) =
        Error_suppressions.filter_lints
          ~include_suppressions
          suppressions
          errors
          aloc_tables
          severity_cover
      in
      let (errors, _, suppressions) =
        Error_suppressions.filter_suppressed_errors
          ~root
          ~file_options:None
          ~loc_of_aloc
          suppressions
          errors
          ~unused:suppressions
      in
      let (warnings, _, _) =
        Error_suppressions.filter_suppressed_errors
          ~root
          ~file_options:None
          ~loc_of_aloc
          suppressions
          warnings
          ~unused:suppressions
      in
      (errors, warnings)
    | Error parse_errors -> (parse_errors, Flow_errors_utils.ConcreteLocPrintableErrorSet.empty)
  in
  let strip_root = Some root in
  Flow_errors_utils.Json_output.json_of_errors_with_context
    ~strip_root
    ~stdin_file
    ~offset_kind:Offset_utils.Utf8
    ~suppressed_errors:[]
    ~errors
    ~warnings
    ()
  |> js_of_json

let check filename =
  let content = Sys_utils.cat filename in
  check_content ~filename ~content

let init_builtins_js js_libs =
  Js.to_array js_libs |> Array.to_list |> List.map Js.to_string |> init_builtins

let check_js js_file = check (Js.to_string js_file)

let check_content_js js_file js_content js_config_object =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  check_content ~filename ~content ~js_config_object

let mk_loc file line col =
  {
    Loc.source = Some file;
    start = { Loc.line; column = col };
    _end = { Loc.line; column = col + 1 };
  }

let autocomplete filename content line col js_config_object :
    (ServerProt.Response.Completion.t, string) result =
  let filename = File_key.SourceFile filename in
  let root = File_path.dummy_path in
  let cursor_loc = Loc.cursor (Some filename) line col in
  let (content, _, canon_token) = Autocomplete_sigil.add (Some filename) content line col in
  let canon_cursor =
    Base.Option.value_map ~default:cursor_loc ~f:Autocomplete_sigil.Canonical.cursor canon_token
  in
  Autocomplete_js.autocomplete_set_hooks ~cursor:canon_cursor;
  let r =
    match parse_content filename content with
    | Error _ -> Error "parse error"
    | Ok (ast, file_sig) ->
      let (_, docblock) =
        Docblock_parser.(
          parse_docblock
            ~max_tokens:docblock_max_tokens
            ~file_options:Files.default_options
            filename
            content
        )
      in
      let (cx, aloc_ast) = ac_infer ~root filename js_config_object docblock ast file_sig in
      let loc = mk_loc filename line col in
      let open AutocompleteService_js in
      let artifacts =
        let empty_exports_search_result =
          { Export_search_types.results = []; is_incomplete = false }
        in
        mk_typing_artifacts
          ~layout_options:Js_layout_generator.default_opts
          ~get_ast_from_shared_mem:(fun _ -> None)
          ~module_system_info:
            Lsp_module_system_info.
              {
                file_options = Files.default_options;
                haste_module_system = false;
                get_haste_module_info = (fun _ -> None);
                get_package_info = (fun _ -> None);
                is_package_file = (fun ~module_path:_ ~module_name:_ -> false);
                resolves_to_real_path = (fun ~from:_ ~to_real_path:_ -> false);
              }
          ~loc_of_aloc
          ~search_exported_values:(fun ~ac_options:_ _ -> empty_exports_search_result)
          ~search_exported_types:(fun ~ac_options:_ _ -> empty_exports_search_result)
          ~cx
          ~file_sig
          ~ast
          ~aloc_ast
          ~canonical:canon_token
      in
      let (_, _, _, result) =
        autocomplete_get_results
          artifacts
          {
            imports = false;
            imports_min_characters = 0;
            imports_ranked_usage = true;
            imports_ranked_usage_boost_exact_match_min_length = 5;
            show_ranking_info = false;
          }
          None
          loc
      in
      (match result with
      | AcEmpty _ -> Ok { ServerProt.Response.Completion.items = []; is_incomplete = false }
      | AcFatalError msg -> Error msg
      | AcResult { result; errors_to_log = _ } -> Ok result)
  in
  Autocomplete_js.autocomplete_unset_hooks ();
  r

let get_def filename content line col js_config_object : (Loc.t list, string) result =
  let filename = File_key.SourceFile filename in
  let root = File_path.dummy_path in
  match parse_content filename content with
  | Error _ -> Error "parse error"
  | Ok (ast, file_sig) ->
    let (_, docblock) =
      Docblock_parser.(
        parse_docblock
          ~max_tokens:docblock_max_tokens
          ~file_options:Files.default_options
          filename
          content
      )
    in
    let (cx, typed_ast) = infer_and_merge ~root filename js_config_object docblock ast file_sig in
    let loc = mk_loc filename line col in
    (match
       GetDef_js.get_def
         ~loc_of_aloc:ALoc.to_loc_exn
         ~cx
         ~file_sig
         ~ast
         ~available_ast:(Typed_ast_utils.Typed_ast typed_ast)
         ~purpose:Get_def_types.Purpose.GoToDefinition
         loc
     with
    | GetDef_js.Get_def_result.Def (locs, _)
    | GetDef_js.Get_def_result.Partial (locs, _, _) ->
      Ok (Loc_collections.LocSet.elements locs)
    | GetDef_js.Get_def_result.Bad_loc err_msg
    | GetDef_js.Get_def_result.Def_error err_msg ->
      Error err_msg)

let infer_type filename content line col js_config_object :
    Loc.t * (string, string) result * Loc.t list * (Loc.t * Refinement_invalidation.reason) list =
  let filename = File_key.SourceFile filename in
  let root = File_path.dummy_path in
  match parse_content filename content with
  | Error _ -> failwith "parse error"
  | Ok (ast, file_sig) ->
    let (_, docblock) =
      Docblock_parser.(
        parse_docblock
          ~max_tokens:docblock_max_tokens
          ~file_options:Files.default_options
          filename
          content
      )
    in
    let (cx, typed_ast) = infer_and_merge ~root filename js_config_object docblock ast file_sig in
    let loc = mk_loc filename line col in
    let open Query_types in
    let open Loc_collections in
    let (refining_locs, refinement_invalidated) =
      let contains_cursor aloc _ = Loc.contains (loc_of_aloc aloc) loc in
      let refining_locs =
        ALocMap.filter contains_cursor (Context.refined_locations cx)
        |> ALocMap.values
        |> Base.List.fold ~init:ALocSet.empty ~f:ALocSet.union
        |> ALocSet.elements
        |> List.map loc_of_aloc
      in
      let refinement_invalidated =
        ALocMap.filter contains_cursor (Context.aggressively_invalidated_locations cx)
        |> ALocMap.values
        |> Base.List.fold ~init:ALocMap.empty ~f:ALocMap.union
        |> ALocMap.elements
        |> List.map (fun (loc, reason) -> (loc_of_aloc loc, reason))
      in
      (refining_locs, refinement_invalidated)
    in
    if Js.Unsafe.get js_config_object "dev_only.type_repr" |> Js.to_bool then
      match dump_type_at_pos ~cx ~typed_ast loc with
      | None -> (Loc.none, Error "No match", refining_locs, refinement_invalidated)
      | Some (loc, s) ->
        (loc, Ok (Utils_js.spf "type_repr: %s" s), refining_locs, refinement_invalidated)
    else
      let result =
        type_at_pos_type
          ~cx
          ~file_sig
          ~omit_targ_defaults:false
          ~typed_ast
          ~verbose_normalizer:false
          ~max_depth:40
          ~no_typed_ast_for_imports:false
          ~include_refs:None
          loc
      in
      begin
        match result with
        | FailureNoMatch -> (Loc.none, Error "No match", refining_locs, refinement_invalidated)
        | FailureUnparseable (loc, _, _) ->
          (loc, Error "Unparseable", refining_locs, refinement_invalidated)
        | Success (loc, result) ->
          let (result, _) = Ty_printer.string_of_type_at_pos_result ~exact_by_default:true result in
          (loc, Ok result, refining_locs, refinement_invalidated)
      end

let refined_locations filename content js_config_object =
  let filename = File_key.SourceFile filename in
  let root = File_path.dummy_path in
  match parse_content filename content with
  | Error _ -> []
  | Ok (ast, file_sig) ->
    let (_, docblock) =
      Docblock_parser.(
        parse_docblock
          ~max_tokens:docblock_max_tokens
          ~file_options:Files.default_options
          filename
          content
      )
    in
    let (cx, _) = infer_and_merge ~root filename js_config_object docblock ast file_sig in
    Context.refined_locations cx |> Loc_collections.ALocMap.keys |> Base.List.map ~f:loc_of_aloc

let signature_help filename content line col js_config_object :
    ((ServerProt.Response.func_details_result list * int) option, string) result =
  let filename = File_key.SourceFile filename in
  let root = File_path.dummy_path in
  match parse_content filename content with
  | Error _ -> Error "parse error"
  | Ok (ast, file_sig) ->
    let (_, docblock) =
      Docblock_parser.(
        parse_docblock
          ~max_tokens:docblock_max_tokens
          ~file_options:Files.default_options
          filename
          content
      )
    in
    let (cx, typed_ast) = infer_and_merge ~root filename js_config_object docblock ast file_sig in
    let cursor_loc = mk_loc filename line col in
    let func_details =
      Signature_help.find_signatures
        ~loc_of_aloc
        ~get_ast_from_shared_mem:(fun _ -> None)
        ~cx
        ~file_sig
        ~ast
        ~typed_ast
        cursor_loc
    in
    begin
      match func_details with
      | Ok details -> Ok details
      | Error _ -> Error "Failed to normalize type"
    end

let types_to_json types ~strip_root =
  Hh_json.(
    Reason.(
      let types_json =
        types
        |> List.map (fun (loc, str) ->
               let json_assoc =
                 ("type", JSON_String str)
                 :: ("reasons", JSON_Array [])
                 :: ("loc", json_of_loc ~strip_root ~offset_table:None loc)
                 :: Flow_errors_utils.deprecated_json_props_of_loc ~strip_root loc
               in
               JSON_Object json_assoc
           )
      in
      JSON_Array types_json
    )
  )

let dump_types js_file js_content js_config_object =
  let filename = File_key.SourceFile (Js.to_string js_file) in
  let root = File_path.dummy_path in
  let content = Js.to_string js_content in
  match parse_content filename content with
  | Error _ -> failwith "parse error"
  | Ok (ast, file_sig) ->
    let (_, docblock) =
      Docblock_parser.(
        parse_docblock
          ~max_tokens:docblock_max_tokens
          ~file_options:Files.default_options
          filename
          content
      )
    in
    let (cx, typed_ast) = infer_and_merge ~root filename js_config_object docblock ast file_sig in
    let printer = Ty_printer.string_of_elt_single_line ~exact_by_default:true in
    let types =
      Query_types.dump_types
        ~printer
        ~evaluate_type_destructors:Ty_normalizer_env.EvaluateNone
        cx
        file_sig
        typed_ast
    in
    let strip_root = None in
    let types_json = types_to_json types ~strip_root in
    js_of_json types_json

let loc_as_range_to_json loc =
  let open Hh_json in
  let open Loc in
  JSON_Object
    [
      ("startLineNumber", JSON_Number (string_of_int loc.start.line));
      ("startColumn", JSON_Number (string_of_int (loc.start.column + 1)));
      ("endLineNumber", JSON_Number (string_of_int loc._end.line));
      ("endColumn", JSON_Number (string_of_int loc._end.column));
    ]

let loc_as_range_end_inclusive_to_json loc =
  let open Hh_json in
  let open Loc in
  JSON_Object
    [
      ("startLineNumber", JSON_Number (string_of_int loc.start.line));
      ("startColumn", JSON_Number (string_of_int (loc.start.column + 1)));
      ("endLineNumber", JSON_Number (string_of_int loc._end.line));
      ("endColumn", JSON_Number (string_of_int (loc._end.column + 1)));
    ]

let completion_item_to_json
    {
      ServerProt.Response.Completion.name;
      kind;
      description = _;
      itemDetail;
      labelDetail = _;
      documentation;
      tags = _;
      preselect = _;
      sort_text = _;
      text_edit;
      insert_text_format = _;
      additional_text_edits;
      log_info = _;
    } =
  let open Hh_json in
  let props = [("label", JSON_String name)] in
  let props =
    Base.Option.value_map ~default:props kind ~f:(fun k ->
        ("kind", JSON_Number (Lsp.Completion.completionItemKind_to_enum k |> string_of_int))
        :: props
    )
  in
  (* { AutocompleteService_js.AcCompletion.insertText; edit; replace }; *)
  let props =
    Base.Option.value_map ~default:props itemDetail ~f:(fun s -> ("detail", JSON_String s) :: props)
  in
  let props =
    Base.Option.value_map ~default:props documentation ~f:(fun docs ->
        ("documentation", JSON_Object [("value", JSON_String docs)]) :: props
    )
  in
  let props =
    match text_edit with
    | None -> props
    | Some { ServerProt.Response.newText; insert; replace } ->
      ("insertText", JSON_String newText)
      :: ( "range",
           JSON_Object
             [("insert", loc_as_range_to_json insert); ("replace", loc_as_range_to_json replace)]
         )
      :: props
  in
  let props =
    ( "additionalTextEdits",
      JSON_Array
        (Base.List.map additional_text_edits ~f:(fun (loc, text) ->
             JSON_Object [("text", JSON_String text); ("range", loc_as_range_to_json loc)]
         )
        )
    )
    :: props
  in
  JSON_Object props

let signature_to_json =
  let open Hh_json in
  let open Utils_js in
  function
  | ServerProt.Response.SigHelpFunc { param_tys; return_ty; func_documentation } ->
    let documentation_props = function
      | None -> []
      | Some doc -> [("documentation", JSON_Object [("value", JSON_String doc)])]
    in
    let props = documentation_props func_documentation in
    let props =
      ( "parameters",
        JSON_Array
          (Base.List.map
             param_tys
             ~f:(fun { ServerProt.Response.param_documentation; param_name; param_ty } ->
               JSON_Object
                 (("label", JSON_String (spf "%s: %s" param_name param_ty))
                 :: documentation_props param_documentation
                 )
           )
          )
      )
      :: props
    in
    let props =
      let sig_str =
        Utils_js.spf
          "(%s): %s"
          (Base.List.map
             param_tys
             ~f:(fun { ServerProt.Response.param_documentation = _; param_name; param_ty } ->
               spf "%s: %s" param_name param_ty
           )
          |> Base.String.concat ~sep:", "
          )
          return_ty
      in
      ("label", JSON_String sig_str) :: props
    in
    JSON_Object props
  | ServerProt.Response.SigHelpJsxAttr { name; ty; optional; documentation = doc } ->
    let documentation = function
      | None -> []
      | Some doc -> [("documentation", JSON_Object [("value", JSON_String doc)])]
    in
    let props = [] in
    let label = ("label", JSON_String (spf "%s%s: %s" name (Utils_js.ite optional "?" "") ty)) in
    let props = ("parameters", JSON_Array [JSON_Object (label :: documentation doc)]) :: props in
    JSON_Object (label :: props)

let autocomplete js_file js_content js_line js_col js_config_object =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let line = Js.parseInt js_line in
  let col = Js.parseInt js_col in
  match autocomplete filename content line col js_config_object with
  | Ok { ServerProt.Response.Completion.items; is_incomplete } ->
    let open Hh_json in
    JSON_Object
      [
        ("incomplete", JSON_Bool is_incomplete);
        ("suggestions", JSON_Array (List.map completion_item_to_json items));
      ]
    |> js_of_json
  | Error msg -> failwith msg

let get_def js_file js_content js_line js_col js_config_object =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let line = Js.parseInt js_line in
  let col = Js.parseInt js_col in
  match get_def filename content line col js_config_object with
  | Ok locs ->
    Hh_json.JSON_Array (List.map (Reason.json_of_loc ~strip_root:None ~offset_table:None) locs)
    |> js_of_json
  | Error msg -> failwith msg

let semantic_decorations js_file js_content js_config_object =
  let open Hh_json in
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let json =
    let refined_locations = refined_locations filename content js_config_object in
    let decorations =
      Base.List.map refined_locations ~f:(fun loc ->
          JSON_Object
            [
              ("kind", JSON_String "refined-value");
              ("range", loc_as_range_end_inclusive_to_json loc);
            ]
      )
    in
    JSON_Object [("decorations", JSON_Array decorations)]
  in
  js_of_json json

let signature_help js_file js_content js_line js_col js_config_object =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let line = Js.parseInt js_line in
  let col = Js.parseInt js_col in
  match signature_help filename content line col js_config_object with
  | Error msg -> failwith msg
  | Ok None -> Js.Unsafe.inject Js.null
  | Ok (Some (signatures, n)) ->
    let open Hh_json in
    JSON_Object
      [
        ("signatures", JSON_Array (Base.List.map signatures ~f:signature_to_json));
        ("activeParameter", JSON_Number (string_of_int n));
        ("activeSignature", JSON_Number "0");
      ]
    |> js_of_json

let type_at_pos js_file js_content js_line js_col js_config_object =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let line = Js.parseInt js_line in
  let col = Js.parseInt js_col in
  let open Hh_json in
  let (_loc, resp, refining_locs, refinement_invalidated) =
    infer_type filename content line col js_config_object
  in
  let markdown_resp s =
    js_of_json (JSON_Object [("type", JSON_String "markdown"); ("value", JSON_String s)])
  in
  let flow_resp s =
    js_of_json (JSON_Object [("type", JSON_String "flow"); ("value", JSON_String s)])
  in
  let loc_to_location_in_markdown loc =
    let { Loc.source = _; start = { Loc.line; column; _ }; _ } = loc in
    Utils_js.spf "`%d:%d`" line column
  in
  let responses =
    let refining_locs = Base.List.map refining_locs ~f:loc_to_location_in_markdown in
    if Base.List.is_empty refining_locs then
      []
    else
      [markdown_resp (Utils_js.spf "Refined at %s" (Base.String.concat ~sep:", " refining_locs))]
  in
  let responses =
    match resp with
    | Ok s -> flow_resp s :: responses
    | Error _ -> responses
  in
  let responses =
    let invalidation_info =
      Base.List.map refinement_invalidated ~f:(fun (loc, reason) ->
          (loc_to_location_in_markdown loc, Refinement_invalidation.string_of_reason reason)
      )
    in
    if Base.List.is_empty invalidation_info then
      responses
    else
      let reasons_str =
        invalidation_info
        |> Base.List.map ~f:(fun (loc, reason) -> Utils_js.spf "%s at %s" reason loc)
        |> Base.String.concat ~sep:", "
      in
      markdown_resp
        (Utils_js.spf
           "Refinement invalidated due to %s. Refactor this property to a const variable to keep refinements."
           reasons_str
        )
      :: responses
  in
  match responses with
  | [] -> failwith "No responses"
  | _ -> Js.array (Array.of_list responses)

let exports =
  if Js.Unsafe.js_expr "typeof exports !== 'undefined'" then
    Js.Unsafe.js_expr "exports"
  else
    let exports = Js.Unsafe.obj [||] in
    Js.Unsafe.set Js.Unsafe.global "flow" exports;
    exports

let () =
  Js.Unsafe.set
    exports
    "configSchema"
    (Js.string
       {|
[
  {
    "key": "babel_loose_array_spread",
    "kind": "option",
    "type": "bool",
    "default": false,
    "desc": "Only allow array spread with arrays, not arbitrary iterables."
  },
  {
    "key": "enums",
    "kind": "option",
    "type": "bool",
    "default": true,
    "desc": "Enable support for Flow Enums."
  },
  {
    "key": "exact_by_default",
    "kind": "option",
    "type": "bool",
    "default": true,
    "desc": "Treat object types as exact by default."
  },
  {
    "key": "experimental.const_params",
    "kind": "option",
    "type": "bool",
    "default": false,
    "desc": "Treat all function parameters as const bindings, banning reassignment."
  },
  {
    "key": "experimental.ts_syntax",
    "kind": "option",
    "type": "bool",
    "default": true,
    "desc": "Make Flow accept and translate some TS syntax automatically."
  },
  {
    "key": "react.runtime",
    "kind": "option",
    "type": "enum",
    "choices": ["classic", "automatic"],
    "default": "automatic",
    "desc": "Selecting 'automatic' enables auto-importing of React functions required for JSX."
  },
  {
    "key": "use_mixed_in_catch_variables",
    "kind": "option",
    "type": "bool",
    "default": false,
    "desc": "Changes the default type of 'catch' variables from 'any' to 'mixed'."
  },
  {
    "key": "dev_only.type_repr",
    "kind": "option",
    "type": "bool",
    "default": false,
    "desc": "Show the underlying type representation for debugging purposes."
  },
  {
    "key": "deprecated-type",
    "kind": "lint",
    "type": "bool",
    "default": false,
    "desc": "Error on the deprecated type annotations."
  },
  {
    "key": "sketchy-null",
    "kind": "lint",
    "type": "bool",
    "default": false,
    "desc": "Error on conditional checks that can be either 'null'/'undefined' or falsy."
  },
  {
    "key": "sketchy-number",
    "kind": "lint",
    "type": "bool",
    "default": false,
    "desc": "Error when a number appears in the left hand side of an '&&' expression."
  },
  {
    "key": "unclear-type",
    "kind": "lint",
    "type": "bool",
    "default": false,
    "desc": "Error on the unsafe 'any', 'Object', and 'Function' type annotations."
  },
  {
    "key": "unnecessary-invariant",
    "kind": "lint",
    "type": "bool",
    "default": false,
    "desc": "Error when a usage of 'invariant' is unnecessary."
  },
  {
    "key": "unnecessary-optional-chain",
    "kind": "lint",
    "type": "bool",
    "default": false,
    "desc": "Error when an optional chain '?.' is unnecessary."
  },
  {
    "key": "unsafe-getters-setters",
    "kind": "lint",
    "type": "bool",
    "default": false,
    "desc": "Error on usage of object getters and setters."
  },
  {
    "key": "unused-promise",
    "kind": "lint",
    "type": "bool",
    "default": false,
    "desc": "Error when a promise is unused."
  }
]
|}
    )

let () =
  Js.Unsafe.set
    exports
    "registerFile"
    (Js.wrap_callback (fun name content ->
         let name = Js.to_string name in
         let content = Js.to_string content in
         Sys_js.create_file ~name ~content
     )
    )

let () = Js.Unsafe.set exports "initBuiltins" (Js.wrap_callback init_builtins_js)

let () = Js.Unsafe.set exports "check" (Js.wrap_callback check_js)

let () = Js.Unsafe.set exports "checkContent" (Js.wrap_callback check_content_js)

let () = Js.Unsafe.set exports "dumpTypes" (Js.wrap_callback dump_types)

let () = Js.Unsafe.set exports "jsOfOcamlVersion" (Js.string Sys_js.js_of_ocaml_version)

let () = Js.Unsafe.set exports "flowVersion" (Js.string Flow_version.version)

let () = Js.Unsafe.set exports "parse" (Js.wrap_callback Flow_parser_js.parse)

let () = Js.Unsafe.set exports "autocomplete" (Js.wrap_callback autocomplete)

let () = Js.Unsafe.set exports "getDef" (Js.wrap_callback get_def)

let () = Js.Unsafe.set exports "semanticDecorations" (Js.wrap_callback semantic_decorations)

let () = Js.Unsafe.set exports "signatureHelp" (Js.wrap_callback signature_help)

let () = Js.Unsafe.set exports "typeAtPos" (Js.wrap_callback type_at_pos)
