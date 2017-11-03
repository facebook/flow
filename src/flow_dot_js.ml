(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let parse_content file content =
  let parse_options = Some Parser_env.({
    (**
     * Always parse ES proposal syntax. The user-facing config option to
     * ignore/warn/enable them is handled during inference so that a clean error
     * can be surfaced (rather than a more cryptic parse error).
     *)
    esproposal_class_instance_fields = true;
    esproposal_class_static_fields = true;
    esproposal_decorators = true;
    esproposal_export_star_as = true;
    types = true;
    use_strict = false;
  }) in
  Parser_flow.program_file ~fail:false ~parse_options content (Some file)

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

let error_of_parse_error source_file (loc, err) =
  let flow_err = Flow_error.EParseError (loc, err) in
  Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file flow_err

let load_lib_files ~master_cx ~metadata files
    save_parse_errors save_infer_errors save_suppressions save_lint_suppressions =
  (* iterate in reverse override order *)
  let _, result = List.rev files |> List.fold_left (

    fun (exclude_syms, result) file ->
      let lib_content = Sys_utils.cat file in
      let lib_file = File_key.LibFile file in
      match parse_content lib_file lib_content with
      | ast, [] ->
        let cx, syms = Type_inference_js.infer_lib_file
          ~metadata ~exclude_syms ~lint_severities:LintSettings.default_severities
          lib_file ast
        in

        let errs, suppressions, lint_suppressions = Merge_js.merge_lib_file cx master_cx in
        save_infer_errors lib_file errs;
        save_suppressions lib_file suppressions;
        save_lint_suppressions lib_file lint_suppressions;

        (* symbols loaded from this file are suppressed
           if found in later ones *)
        let exclude_syms = SSet.union exclude_syms (SSet.of_list syms) in
        let result = (lib_file, true) :: result in
        exclude_syms, result

      | _, parse_errors ->
        let converted = List.fold_left (fun acc parse_error ->
          Errors.ErrorSet.add (error_of_parse_error lib_file parse_error) acc
        ) Errors.ErrorSet.empty parse_errors in
        save_parse_errors lib_file converted;
        exclude_syms, ((lib_file, false) :: result)

    ) (SSet.empty, [])

  in result

let stub_docblock = { Docblock.
  flow = None;
  preventMunge = None;
  providesModule = None;
  isDeclarationFile = false;
  jsx = None;
}

let stub_metadata ~root ~checked = { Context.
  local_metadata = { Context.
    checked;
    munge_underscores = false;
    verbose = None;
    weak = false;
    jsx = None;
    strict = false;
  };
  global_metadata = { Context.
    enable_const_params = false;
    enable_unsafe_getters_and_setters = true;
    enforce_strict_type_args = true;
    enforce_strict_call_arity = true;
    esproposal_class_static_fields = Options.ESPROPOSAL_ENABLE;
    esproposal_class_instance_fields = Options.ESPROPOSAL_ENABLE;
    esproposal_decorators = Options.ESPROPOSAL_ENABLE;
    esproposal_export_star_as = Options.ESPROPOSAL_ENABLE;
    facebook_fbt = None;
    ignore_non_literal_requires = false;
    max_trace_depth = 0;
    max_workers = 0;
    root;
    strip_root = true;
    suppress_comments = [];
    suppress_types = SSet.empty;
  };
}

let get_master_cx =
  let master_cx = ref None in
  fun root ->
    match !master_cx with
    | Some (prev_root, cx) -> assert (prev_root = root); cx
    | None ->
      let cx = Flow_js.fresh_context
        (stub_metadata ~root ~checked:false)
        File_key.Builtins
        Files.lib_module_ref in
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

let infer_and_merge ~root filename ast =
  (* this is a VERY pared-down version of Merge_service.merge_strict_context.
     it relies on the JS version only supporting libs + 1 file, so every
     module you can require() must come from a lib; this skips resolving
     module names and just adds them all to the `decls` list. *)
  Flow_js.Cache.clear();
  let metadata = stub_metadata ~root ~checked:true in
  let master_cx = get_master_cx root in
  let file_sig = File_sig.program ~ast in
  let require_loc_map = File_sig.(require_loc_map file_sig.module_sig) in
  let decls = SMap.fold (fun module_name loc ->
    List.cons (module_name, loc, Modulename.String module_name, filename)
  ) require_loc_map [] in
  let reqs = Merge_js.Reqs.({ empty with decls }) in
  let lint_severities = LintSettings.default_severities in
  let strict_mode = StrictModeSettings.empty in
  let file_sigs = Utils_js.FilenameMap.singleton filename file_sig in
  Merge_js.merge_component_strict
    ~metadata ~lint_severities ~strict_mode ~file_sigs
    ~get_ast_unsafe:(fun _ -> ast)
    ~get_docblock_unsafe:(fun _ -> stub_docblock)
    [filename] reqs [] master_cx

let check_content ~filename ~content =
  let stdin_file = Some (Path.make_unsafe filename, content) in
  let root = Path.dummy_path in
  let filename = File_key.SourceFile filename in
  let errors, warnings = match parse_content filename content with
  | ast, [] ->
    let cx = infer_and_merge ~root filename ast in
    let errors, warnings, _, _ = Error_suppressions.filter_suppressed_errors
      Error_suppressions.empty (Context.severity_cover cx) (Context.errors cx)
    in errors, warnings
  | _, parse_errors ->
    let errors = List.fold_left (fun acc parse_error ->
      Errors.ErrorSet.add (error_of_parse_error filename parse_error) acc
    ) Errors.ErrorSet.empty parse_errors
    in errors, Errors.ErrorSet.empty
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
  |> List.map (fun x -> Js.to_string x)
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
    start = { Loc.line; column = col; offset = 0; };
    _end = { Loc.line; column = col + 1; offset = 0; };
  }

let infer_type filename content line col =
    let filename = File_key.SourceFile filename in
    let root = Path.dummy_path in
    match parse_content filename content with
    | ast, [] ->
      let cx = infer_and_merge ~root filename ast in
      let loc = mk_loc filename line col in
      let loc, ground_t, possible_ts = Query_types.(match query_type cx loc with
        | FailureNoMatch -> Loc.none, None, []
        | FailureUnparseable (loc, _, possible_ts) -> loc, None, possible_ts
        | Success (loc, gt, possible_ts) -> loc, Some gt, possible_ts
      ) in
      let ty = match ground_t with
        | None -> None
        | Some t -> Some (Type_printer.string_of_t cx t)
      in
      let reasons =
        possible_ts
        |> List.map Type.reason_of_t
      in
      (None, Some (loc, ty, reasons))
    | _, _ -> failwith "parse error"

let types_to_json types ~strip_root =
  let open Hh_json in
  let open Reason in
  let types_json = types |> List.map (fun (loc, _ctor, str, reasons) ->
    let json_assoc = (
      ("type", JSON_String str) ::
      ("reasons", JSON_Array (List.map (fun r ->
        let r_loc = loc_of_reason r in
        let r_def_loc = def_loc_of_reason r in
        JSON_Object (
          ("desc", JSON_String (string_of_desc (desc_of_reason r))) ::
          ("loc", json_of_loc ~strip_root r_loc) ::
          ((if r_def_loc = r_loc then [] else [
            "def_loc", json_of_loc ~strip_root r_def_loc
          ]) @ (Errors.deprecated_json_props_of_loc ~strip_root r_loc))
        )
      ) reasons)) ::
      ("loc", json_of_loc ~strip_root loc) ::
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
    | ast, [] ->
      let cx = infer_and_merge ~root filename ast in
      let printer = Type_printer.string_of_t in
      let types = Query_types.dump_types printer cx in

      let strip_root = None in
      let types_json = types_to_json types ~strip_root in

      js_of_json types_json
    | _, _ -> failwith "parse error"

let handle_inferred_result (_, inferred, _) =
  inferred

let type_at_pos js_file js_content js_line js_col =
  let filename = Js.to_string js_file in
  let content = Js.to_string js_content in
  let line = Js.parseInt js_line in
  let col = Js.parseInt js_col in
  match infer_type filename content line col with
  | (Some err, None) -> err
  | (None, Some resp) -> handle_inferred_result resp
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
