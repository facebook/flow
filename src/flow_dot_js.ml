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
  |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file
  |> Flow_error.make_error_printable loc_of_aloc ~strip_root:None

let parse_content file content =
  let parse_options =
    let open Parser_env in
    Some
      {
        components = true;
        enums = true;
        (*
         * Always parse ES proposal syntax. The user-facing config option to
         * ignore/warn/enable them is handled during inference so that a clean error
         * can be surfaced (rather than a more cryptic parse error).
         *)
        esproposal_decorators = true;
        types = true;
        use_strict = false;
        module_ref_prefix = None;
        module_ref_prefix_LEGACY_INTEROP = None;
      }
  in

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
      enable_ts_syntax = true;
      hooklike_functions = true;
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
    any_propagation = true;
    automatic_require_default = false;
    babel_loose_array_spread = false;
    casting_syntax = Options.CastingSyntax.Both;
    component_syntax = true;
    hooklike_functions = true;
    hooklike_functions_includes = [];
    react_rules =
      Options.
        [ValidateRefAccessDuringRender; DeepReadOnlyProps; DeepReadOnlyHookReturns; RulesOfHooks];
    react_rules_always = false;
    enable_as_const = false;
    enable_const_params = false;
    enable_enums = true;
    enable_relay_integration = false;
    exact_by_default = true;
    facebook_fbs = None;
    facebook_fbt = None;
    facebook_module_interop = false;
    file_options = Files.default_options;
    ignore_non_literal_requires = false;
    max_literal_length = 100;
    max_trace_depth = 0;
    max_workers = 0;
    missing_module_generators = [];
    namespaces = false;
    react_runtime = Options.ReactRuntimeAutomatic;
    recursion_limit = 10000;
    relay_integration_esmodules = false;
    relay_integration_excludes = [];
    relay_integration_module_prefix = None;
    relay_integration_module_prefix_includes = [];
    root;
    strict_es6_import_export = false;
    strict_es6_import_export_excludes = [];
    strip_root = true;
    suppress_types = SSet.of_list ["$FlowFixMe"; "$FlowIssue"; "$FlowIgnore"; "$FlowExpectedError"];
    ts_syntax = true;
    use_mixed_in_catch_variables = false;
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

let infer_and_merge ~root filename js_config_object docblock ast file_sig =
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
        | Some t -> Ok t
        | None -> Error (Reason.internal_module_name mref))
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
  let typed_ast = Type_inference_js.infer_ast cx filename comments ast ~lint_severities in
  Merge_js.post_merge_checks cx ast typed_ast metadata;
  Context.reset_errors cx (Flow_error.post_process_errors (Context.errors cx));
  (cx, typed_ast)

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

let infer_type filename content line col js_config_object : Loc.t * (string, string) result =
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
    let file = Context.file cx in
    let loc = mk_loc filename line col in
    let open Query_types in
    let result =
      type_at_pos_type
        ~cx
        ~file
        ~file_sig
        ~omit_targ_defaults:false
        ~typed_ast
        ~verbose_normalizer:false
        ~max_depth:50
        ~no_typed_ast_for_imports:false
        loc
    in
    begin
      match result with
      | FailureNoMatch -> (Loc.none, Error "No match")
      | FailureUnparseable (loc, _, _) -> (loc, Error "Unparseable")
      | Success (loc, result) ->
        (loc, Ok (Ty_printer.string_of_type_at_pos_result ~exact_by_default:true result))
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

let type_at_pos js_file js_content js_line js_col js_config_object =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let line = Js.parseInt js_line in
  let col = Js.parseInt js_col in
  match infer_type filename content line col js_config_object with
  | (_, Ok resp) -> Js.string resp
  | (_, _) -> failwith "Error"

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

let () = Js.Unsafe.set exports "typeAtPos" (Js.wrap_callback type_at_pos)
