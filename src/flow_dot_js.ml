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
  |> Flow_error.concretize_error loc_of_aloc
  |> Flow_error.make_error_printable

let parse_content file content =
  let parse_options =
    let open Parser_env in
    Some
      {
        enums = true;
        (*
         * Always parse ES proposal syntax. The user-facing config option to
         * ignore/warn/enable them is handled during inference so that a clean error
         * can be surfaced (rather than a more cryptic parse error).
         *)
        esproposal_decorators = true;
        types = true;
        use_strict = false;
      }
  in

  let (ast, parse_errors) =
    Parser_flow.program_file ~fail:false ~parse_options content (Some file)
  in
  if parse_errors <> [] then
    let converted =
      List.fold_left
        (fun acc parse_error ->
          Errors.ConcreteLocPrintableErrorSet.add (error_of_parse_error file parse_error) acc)
        Errors.ConcreteLocPrintableErrorSet.empty
        parse_errors
    in
    Error converted
  else
    let (fsig, _tolerable_errors) =
      File_sig.With_Loc.program ~ast ~opts:File_sig.With_Loc.default_opts
    in
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

let load_lib_files ~ccx ~metadata files =
  (* iterate in reverse override order *)
  let (leader, _) =
    List.rev files
    |> List.fold_left
         (fun (leader, exclude_syms) file ->
           let lib_content = Sys_utils.cat file in
           let lib_file = File_key.LibFile file in
           match parse_content lib_file lib_content with
           | Ok (ast, file_sig) ->
             (* Lib files use only concrete locations, so this is not used. *)
             let aloc_table = lazy (ALoc.empty_table lib_file) in
             let cx = Context.make ccx metadata lib_file aloc_table Context.Checking in
             let (syms, _) =
               Type_inference_js.infer_lib_file
                 cx
                 ast
                 ~exclude_syms
                 ~file_sig:(File_sig.abstractify_locs file_sig)
                 ~lint_severities:LintSettings.empty_severities
             in
             (* symbols loaded from this file are suppressed if found in later ones *)
             (Some cx, NameUtils.Set.union exclude_syms (NameUtils.Set.of_list syms))
           | Error _ -> (leader, exclude_syms))
         (None, NameUtils.Set.empty)
  in
  leader

let stub_metadata ~root ~checked =
  {
    Context.checked (* local *);
    include_suppressions = false;
    jsx = Options.Jsx_react;
    munge_underscores = false;
    strict = false;
    strict_local = false;
    verbose = None;
    (* global *)
    any_propagation = true;
    automatic_require_default = false;
    babel_loose_array_spread = false;
    cycle_errors = false;
    cycle_errors_includes = [];
    enable_const_params = false;
    enable_enums = true;
    enable_relay_integration = false;
    enforce_strict_call_arity = true;
    inference_mode = Options.ConstrainWrites;
    inference_mode_lti_includes = [];
    exact_by_default = false;
    facebook_fbs = None;
    facebook_fbt = None;
    facebook_module_interop = false;
    haste_module_ref_prefix = None;
    ignore_non_literal_requires = false;
    max_literal_length = 100;
    max_trace_depth = 0;
    max_workers = 0;
    missing_module_generators = [];
    array_literal_providers = false;
    array_literal_providers_includes = [];
    react_runtime = Options.ReactRuntimeClassic;
    react_server_component_exts = SSet.empty;
    recursion_limit = 10000;
    relay_integration_excludes = [];
    relay_integration_module_prefix = None;
    relay_integration_module_prefix_includes = [];
    root;
    run_post_inference_implicit_instantiation = false;
    enable_post_inference_targ_widened_check = false;
    save_implicit_instantiation_results = false;
    strict_es6_import_export = false;
    strict_es6_import_export_excludes = [];
    strip_root = true;
    suppress_types = SSet.empty;
    trust_mode = Options.NoTrust;
  }

let master_cx_ref : (Path.t * Context.master_context) option ref = ref None

let get_master_cx root =
  match !master_cx_ref with
  | None -> failwith "builtins not initialized"
  | Some (prev_root, master_cx) ->
    assert (prev_root = root);
    master_cx

let init_builtins filenames =
  let root = Path.dummy_path in
  let ccx = Context.(make_ccx (empty_master_cx ())) in
  let leader =
    let metadata = stub_metadata ~root ~checked:true in
    load_lib_files ~ccx ~metadata filenames
  in
  let master_cx =
    match leader with
    | None -> Context.empty_master_cx ()
    | Some cx ->
      Merge_js.optimize_builtins cx;
      { Context.master_sig_cx = Context.sig_cx cx; builtins = Context.builtins cx }
  in
  master_cx_ref := Some (root, master_cx)

let infer_and_merge ~root filename ast file_sig =
  (* create cx *)
  let master_cx = get_master_cx root in
  let ccx = Context.make_ccx master_cx in
  let metadata = stub_metadata ~root ~checked:true in
  (* flow.js does not use abstract locations, so this is not used *)
  let aloc_table = lazy (ALoc.empty_table filename) in
  let cx = Context.make ccx metadata filename aloc_table Context.Checking in
  (* connect requires *)
  Type_inference_js.add_require_tvars cx file_sig;
  let connect_requires mref =
    let module_name = Reason.internal_module_name mref in
    Nel.iter (fun loc ->
        let reason = Reason.(mk_reason (RCustom mref) loc) in
        let module_t = Flow_js_utils.lookup_builtin_strict cx module_name reason in
        let (_, require_id) = Context.find_require cx loc in
        Flow_js.resolve_id cx require_id module_t
    )
  in
  SMap.iter connect_requires File_sig.With_ALoc.(require_loc_map file_sig.module_sig);
  (* infer ast *)
  let (_, { Flow_ast.Program.all_comments = comments; _ }) = ast in
  let ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  let lint_severities =
    let base_severities = LintSettings.empty_severities in
    let strict_mode = StrictModeSettings.empty in
    Merge_js.get_lint_severities metadata strict_mode base_severities
  in
  let typed_ast = Type_inference_js.infer_ast cx filename comments ast ~lint_severities in
  Merge_js.post_merge_checks cx master_cx ast typed_ast metadata file_sig;
  (cx, typed_ast)

let check_content ~filename ~content =
  let stdin_file = Some (Path.make_unsafe filename, content) in
  let root = Path.dummy_path in
  let filename = File_key.SourceFile filename in
  let (errors, warnings) =
    match parse_content filename content with
    | Ok (ast, file_sig) ->
      let file_sig = File_sig.abstractify_locs file_sig in
      let (cx, _) = infer_and_merge ~root filename ast file_sig in
      let suppressions = Error_suppressions.empty in
      (* TODO: support suppressions *)
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
      let errors =
        errors |> Flow_error.concretize_errors loc_of_aloc |> Flow_error.make_errors_printable
      in
      let warnings =
        warnings |> Flow_error.concretize_errors loc_of_aloc |> Flow_error.make_errors_printable
      in
      let (errors, _, suppressions) =
        Error_suppressions.filter_suppressed_errors
          ~root
          ~file_options:None
          suppressions
          errors
          ~unused:suppressions
      in
      let (warnings, _, _) =
        Error_suppressions.filter_suppressed_errors
          ~root
          ~file_options:None
          suppressions
          warnings
          ~unused:suppressions
      in
      (errors, warnings)
    | Error parse_errors -> (parse_errors, Errors.ConcreteLocPrintableErrorSet.empty)
  in
  let strip_root = Some root in
  Errors.Json_output.json_of_errors_with_context
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

let check_content_js js_file js_content =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  check_content ~filename ~content

let mk_loc file line col =
  {
    Loc.source = Some file;
    start = { Loc.line; column = col };
    _end = { Loc.line; column = col + 1 };
  }

let infer_type filename content line col : Loc.t * (string, string) result =
  let filename = File_key.SourceFile filename in
  let root = Path.dummy_path in
  match parse_content filename content with
  | Error _ -> failwith "parse error"
  | Ok (ast, file_sig) ->
    let file_sig = File_sig.abstractify_locs file_sig in
    let (cx, typed_ast) = infer_and_merge ~root filename ast file_sig in
    let file = Context.file cx in
    let loc = mk_loc filename line col in
    Query_types.(
      let result =
        type_at_pos_type
          ~full_cx:cx
          ~file
          ~file_sig
          ~omit_targ_defaults:false
          ~typed_ast
          ~evaluate_type_destructors:false
          ~verbose_normalizer:false
          ~max_depth:50
          loc
      in
      (match result with
      | FailureNoMatch -> (Loc.none, Error "No match")
      | FailureUnparseable (loc, _, _) -> (loc, Error "Unparseable")
      | Success (loc, t) -> (loc, Ok (Ty_printer.string_of_elt_single_line ~exact_by_default:true t)))
    )

let types_to_json types ~strip_root =
  Hh_json.(
    Reason.(
      let types_json =
        types
        |> List.map (fun (loc, str) ->
               let json_assoc =
                 ("type", JSON_String str)
                 ::
                 ("reasons", JSON_Array [])
                 ::
                 ("loc", json_of_loc ~strip_root ~offset_table:None loc)
                 :: Errors.deprecated_json_props_of_loc ~strip_root loc
               in
               JSON_Object json_assoc
           )
      in
      JSON_Array types_json
    )
  )

let dump_types js_file js_content =
  let filename = File_key.SourceFile (Js.to_string js_file) in
  let root = Path.dummy_path in
  let content = Js.to_string js_content in
  match parse_content filename content with
  | Error _ -> failwith "parse error"
  | Ok (ast, file_sig) ->
    let file_sig = File_sig.abstractify_locs file_sig in
    let (cx, typed_ast) = infer_and_merge ~root filename ast file_sig in
    let printer = Ty_printer.string_of_elt_single_line ~exact_by_default:true in
    let types =
      Query_types.dump_types ~printer ~evaluate_type_destructors:false cx file_sig typed_ast
    in
    let strip_root = None in
    let types_json = types_to_json types ~strip_root in
    js_of_json types_json

let type_at_pos js_file js_content js_line js_col =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let line = Js.parseInt js_line in
  let col = Js.parseInt js_col in
  match infer_type filename content line col with
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
