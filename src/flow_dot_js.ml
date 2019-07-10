(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let lazy_table_of_aloc _ = lazy (failwith "Did not expect to encounter an abstract location in flow_dot_js")

let error_of_parse_error source_file (loc, err) =
  Error_message.EParseError (ALoc.of_loc loc, err)
  |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file
  |> Flow_error.concretize_error lazy_table_of_aloc
  |> Flow_error.make_error_printable lazy_table_of_aloc

let error_of_file_sig_error source_file e =
  File_sig.With_Loc.(match e with
    | IndeterminateModuleType loc -> Error_message.EIndeterminateModuleType (ALoc.of_loc loc)
  )
  |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file
  |> Flow_error.concretize_error lazy_table_of_aloc
  |> Flow_error.make_error_printable lazy_table_of_aloc

let parse_content file content =
  let parse_options = Some Parser_env.({
    enums = true;
    (**
     * Always parse ES proposal syntax. The user-facing config option to
     * ignore/warn/enable them is handled during inference so that a clean error
     * can be surfaced (rather than a more cryptic parse error).
     *)
    esproposal_class_instance_fields = true;
    esproposal_class_static_fields = true;
    esproposal_decorators = true;
    esproposal_export_star_as = true;
    esproposal_optional_chaining = true;
    esproposal_nullish_coalescing = true;
    esproposal_fsharp_pipeline_operator = true;
    types = true;
    use_strict = false;
  }) in
  let ast, parse_errors =
    Parser_flow.program_file ~fail:false ~parse_options content (Some file)
  in
  if parse_errors <> [] then
    let converted = List.fold_left (fun acc parse_error ->
      Errors.ConcreteLocPrintableErrorSet.add (error_of_parse_error file parse_error) acc
    ) Errors.ConcreteLocPrintableErrorSet.empty parse_errors in
    Error converted
  else
    match File_sig.With_Loc.program ~ast ~module_ref_prefix:None with
    | Error e -> Error (Errors.ConcreteLocPrintableErrorSet.singleton (error_of_file_sig_error file e))
    | Ok fsig -> Ok (ast, fsig)

let array_of_list f lst =
  Array.of_list (List.map f lst)

let rec js_of_json = function
  | Hh_json.JSON_Object props ->
      let props = array_of_list (fun (k, v) -> k, js_of_json v) props in
      Js.Unsafe.inject (Js.Unsafe.obj props)
  | Hh_json.JSON_Array items ->
      let items = array_of_list js_of_json items in
      Js.Unsafe.inject (Js.array items)
  | Hh_json.JSON_String str ->
      Js.Unsafe.inject (Js.string str)
  | Hh_json.JSON_Number num ->
      Js.Unsafe.inject (Js.number_of_float (float_of_string num))
  | Hh_json.JSON_Bool value ->
      Js.Unsafe.inject (Js.bool value)
  | Hh_json.JSON_Null ->
      Js.Unsafe.inject Js.null

let load_lib_files ~master_cx ~metadata files
    save_parse_errors save_infer_errors save_suppressions save_lint_suppressions =
  (* iterate in reverse override order *)
  let _, result = List.rev files |> List.fold_left (

    fun (exclude_syms, result) file ->
      let lib_content = Sys_utils.cat file in
      let lib_file = File_key.LibFile file in
      match parse_content lib_file lib_content with
      | Ok (ast, file_sig) ->
        let sig_cx = Context.make_sig () in
        let aloc_table = Utils_js.FilenameMap.empty in
        let cx = Context.make sig_cx metadata lib_file aloc_table Files.lib_module_ref in
        Flow_js.mk_builtins cx;
        let syms = Type_inference_js.infer_lib_file cx ast
          ~exclude_syms ~file_sig:(File_sig.abstractify_locs file_sig) ~lint_severities:LintSettings.empty_severities ~file_options:None
        in

        Context.merge_into (Context.sig_cx master_cx) sig_cx;

        let () =
          let from_t = Context.find_module master_cx Files.lib_module_ref in
          let to_t = Context.find_module cx Files.lib_module_ref in
          Flow_js.flow_t master_cx (from_t, to_t)
        in

        let errors = Context.errors cx in
        let suppressions = Context.error_suppressions cx in
        let severity_cover = Context.severity_cover cx in

        Context.remove_all_errors cx;
        Context.remove_all_error_suppressions cx;
        Context.remove_all_lint_severities cx;

        save_infer_errors lib_file errors;
        save_suppressions lib_file suppressions;
        save_lint_suppressions lib_file severity_cover;

        (* symbols loaded from this file are suppressed
           if found in later ones *)
        let exclude_syms = SSet.union exclude_syms (SSet.of_list syms) in
        let result = (lib_file, true) :: result in
        exclude_syms, result

      | Error parse_errors ->
        save_parse_errors lib_file parse_errors;
        exclude_syms, ((lib_file, false) :: result)

    ) (SSet.empty, [])

  in result

let stub_docblock = { Docblock.
  flow = None;
  typeAssert = false;
  preventMunge = None;
  providesModule = None;
  isDeclarationFile = false;
  jsx = None;
}

let stub_metadata ~root ~checked = { Context.
  (* local *)
  checked;
  munge_underscores = false;
  verbose = None;
  weak = false;
  jsx = Options.Jsx_react;
  strict = false;
  strict_local = false;
  include_suppressions = false;

  (* global *)
  max_literal_length = 100;
  enable_const_params = false;
  enable_enums = true;
  enforce_strict_call_arity = true;
  esproposal_class_static_fields = Options.ESPROPOSAL_ENABLE;
  esproposal_class_instance_fields = Options.ESPROPOSAL_ENABLE;
  esproposal_decorators = Options.ESPROPOSAL_ENABLE;
  esproposal_export_star_as = Options.ESPROPOSAL_ENABLE;
  esproposal_optional_chaining = Options.ESPROPOSAL_ENABLE;
  esproposal_nullish_coalescing = Options.ESPROPOSAL_ENABLE;
  esproposal_fsharp_pipeline_operator = Options.ESPROPOSAL_ENABLE;
  facebook_fbs = None;
  facebook_fbt = None;
  haste_module_ref_prefix = None;
  ignore_non_literal_requires = false;
  max_trace_depth = 0;
  max_workers = 0;
  recursion_limit = 10000;
  root;
  strip_root = true;
  suppress_comments = [];
  suppress_types = SSet.empty;
  default_lib_dir = None;
  trust_mode = Options.NoTrust;
  type_asserts = false;
}

let get_master_cx =
  let master_cx = ref None in
  fun root ->
    match !master_cx with
    | Some (prev_root, cx) -> assert (prev_root = root); cx
    | None ->
      let sig_cx = Context.make_sig () in
      let aloc_table = Utils_js.FilenameMap.empty in
      let cx = Context.make sig_cx
        (stub_metadata ~root ~checked:false)
        File_key.Builtins
        aloc_table
        Files.lib_module_ref in
      Flow_js.mk_builtins cx;
      master_cx := Some (root, cx);
      cx

let set_libs filenames =
  let root = Path.dummy_path in
  let master_cx = get_master_cx root in
  let metadata = stub_metadata ~root ~checked:true in
  let _: (File_key.t * bool) list = load_lib_files
    ~master_cx
    ~metadata
    filenames
    (fun _file _errs -> ())
    (fun _file _errs -> ())
    (fun _file _sups -> ())
    (fun _file _lint -> ()) in

  Flow_js.Cache.clear();
  let reason = Reason.builtin_reason (Reason.RCustom "module") in
  let builtin_module = Obj_type.mk master_cx reason in
  Flow_js.flow_t master_cx (builtin_module, Flow_js.builtins master_cx);
  Merge_js.ContextOptimizer.sig_context master_cx [Files.lib_module_ref]

let infer_and_merge ~root filename ast file_sig =
  (* this is a VERY pared-down version of Merge_service.merge_strict_context.
     it relies on the JS version only supporting libs + 1 file, so every
     module you can require() must come from a lib; this skips resolving
     module names and just adds them all to the `decls` list. *)
  Flow_js.Cache.clear();
  let metadata = stub_metadata ~root ~checked:true in
  let master_cx = get_master_cx root in
  let require_loc_map = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
  let reqs = SMap.fold (fun module_name locs reqs ->
    let m = Modulename.String module_name in
    let locs = locs |> Nel.to_list |> Loc_collections.ALocSet.of_list in
    Merge_js.Reqs.add_decl module_name filename (locs, m) reqs
  ) require_loc_map Merge_js.Reqs.empty in
  let lint_severities = LintSettings.empty_severities in
  let strict_mode = StrictModeSettings.empty in
  let file_sigs = Utils_js.FilenameMap.singleton filename file_sig in
  let (_, _, comments) = ast in
  let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  let ((cx, tast, _), _other_cxs) = Merge_js.merge_component
    ~metadata ~lint_severities ~file_options:None ~strict_mode ~file_sigs
    ~get_ast_unsafe:(fun _ -> (comments, aloc_ast))
    ~get_aloc_table_unsafe:(fun _ -> failwith "Did not expect to need an ALoc table")
    ~get_docblock_unsafe:(fun _ -> stub_docblock)
    (Nel.one filename) reqs [] (Context.sig_cx master_cx)
  in
  (cx, tast)

let check_content ~filename ~content =
  let stdin_file = Some (Path.make_unsafe filename, content) in
  let root = Path.dummy_path in
  let filename = File_key.SourceFile filename in
  let errors, warnings = match parse_content filename content with
  | Ok (ast, file_sig) ->
    let file_sig = File_sig.abstractify_locs file_sig in
    let cx, _ = infer_and_merge ~root filename ast file_sig in
    let suppressions = Error_suppressions.empty in (* TODO: support suppressions *)
    let errors = Context.errors cx in
    let severity_cover = Context.severity_cover cx in
    let include_suppressions = Context.include_suppressions cx in
    let aloc_tables = Utils_js.FilenameMap.empty in
    let errors, warnings, suppressions =
      Error_suppressions.filter_lints ~include_suppressions suppressions errors aloc_tables severity_cover in
    let errors = Flow_error.make_errors_printable lazy_table_of_aloc errors in
    let warnings = Flow_error.make_errors_printable lazy_table_of_aloc warnings in
    let errors, _, suppressions = Error_suppressions.filter_suppressed_errors
      ~root ~file_options:None suppressions errors ~unused:suppressions in
    let warnings, _, _ = Error_suppressions.filter_suppressed_errors
      ~root ~file_options:None suppressions warnings ~unused:suppressions in
    errors, warnings
  | Error parse_errors ->
    parse_errors, Errors.ConcreteLocPrintableErrorSet.empty
  in
  let strip_root = Some root in
  Errors.Json_output.json_of_errors_with_context
    ~strip_root ~stdin_file ~suppressed_errors:[] ~errors ~warnings ()
  |> js_of_json

let check filename =
  let content = Sys_utils.cat filename in
  check_content ~filename ~content

let set_libs_js js_libs =
  Js.to_array js_libs
  |> Array.to_list
  |> List.map Js.to_string
  |> set_libs

let check_js js_file =
  check (Js.to_string js_file)

let check_content_js js_file js_content =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  check_content ~filename ~content

let mk_loc file line col =
  {
    Loc.
    source = Some file;
    start = { Loc.line; column = col; };
    _end = { Loc.line; column = col + 1; };
  }

let infer_type filename content line col =
    let filename = File_key.SourceFile filename in
    let root = Path.dummy_path in
    match parse_content filename content with
    | Error _ -> failwith "parse error"
    | Ok (ast, file_sig) ->
      let file_sig = File_sig.abstractify_locs file_sig in
      let cx, typed_ast = infer_and_merge ~root filename ast file_sig in
      let file = Context.file cx in
      let loc = mk_loc filename line col in Query_types.(
        let result = type_at_pos_type ~full_cx:cx ~file ~file_sig ~expand_aliases:false
          ~omit_targ_defaults:false ~typed_ast loc in
        match result with
        | FailureNoMatch -> Loc.none, Error "No match"
        | FailureUnparseable (loc, _, _) -> loc, Error "Unparseable"
        | Success (loc, t) ->
          loc, Ok (Ty_printer.string_of_t ~force_single_line:true t)
      )

let types_to_json types ~strip_root =
  let open Hh_json in
  let open Reason in
  let types_json = types |> List.map (fun (loc, str) ->
    let json_assoc = (
      ("type", JSON_String str) ::
      ("reasons", JSON_Array []) ::
      ("loc", json_of_loc ~strip_root ~offset_table:None loc) ::
      (Errors.deprecated_json_props_of_loc ~strip_root loc)
    ) in
    JSON_Object json_assoc
  ) in
  JSON_Array types_json

let dump_types js_file js_content =
    let filename = File_key.SourceFile (Js.to_string js_file) in
    let root = Path.dummy_path in
    let content = Js.to_string js_content in
    match parse_content filename content with
    | Error _ -> failwith "parse error"
    | Ok (ast, file_sig) ->
      let file_sig = File_sig.abstractify_locs file_sig in
      let cx, typed_ast = infer_and_merge ~root filename ast file_sig in
      let printer = Ty_printer.string_of_t in
      let types = Query_types.dump_types ~printer cx file_sig typed_ast in
      let strip_root = None in
      let types_json = types_to_json types ~strip_root in

      js_of_json types_json

let type_at_pos js_file js_content js_line js_col =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let line = Js.parseInt js_line in
  let col = Js.parseInt js_col in
  match infer_type filename content line col with
  | (_, Ok resp) -> resp
  | (_, _) ->  failwith "Error"

let exports =
  if (Js.Unsafe.js_expr "typeof exports !== 'undefined'")
  then Js.Unsafe.js_expr "exports"
  else begin
    let exports = Js.Unsafe.obj [||] in
    Js.Unsafe.set Js.Unsafe.global "flow" exports;
    exports
  end

let () = Js.Unsafe.set exports "registerFile" (
  Js.wrap_callback (fun name content -> Sys_js.create_file ~name ~content)
)
let () = Js.Unsafe.set exports
  "setLibs" (Js.wrap_callback set_libs_js)
let () = Js.Unsafe.set exports
  "check" (Js.wrap_callback check_js)
let () = Js.Unsafe.set exports
  "checkContent" (Js.wrap_callback check_content_js)
let () = Js.Unsafe.set exports
  "dumpTypes" (Js.wrap_callback dump_types)
let () = Js.Unsafe.set exports
  "jsOfOcamlVersion" (Js.string Sys_js.js_of_ocaml_version)
let () = Js.Unsafe.set exports
  "flowVersion" (Js.string Flow_version.version)
let () = Js.Unsafe.set exports
  "parse" (Js.wrap_callback Flow_parser_js.parse)
let () = Js.Unsafe.set exports
  "typeAtPos" (Js.wrap_callback type_at_pos)
