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


let load_lib_files ~master_cx ~metadata files
    save_parse_errors save_infer_errors save_suppressions =
  (* iterate in reverse override order *)
  let _, result = List.rev files |> List.fold_left (

    fun (exclude_syms, result) file ->
      let lib_content = Sys_utils.cat file in
      let lib_file = Loc.LibFile file in
      match parse_content lib_file lib_content with
      | (_, statements, comments), [] ->

        let cx, syms = Type_inference_js.infer_lib_file
          ~metadata ~exclude_syms
          lib_file statements comments
        in

        Merge_js.merge_lib_file cx master_cx save_infer_errors save_suppressions;

        (* symbols loaded from this file are suppressed
           if found in later ones *)
        let exclude_syms = SSet.union exclude_syms (Utils_js.set_of_list syms) in
        let result = (lib_file, true) :: result in
        exclude_syms, result

      | _, parse_errors ->
        let converted = List.fold_left (fun acc err ->
          Errors.(ErrorSet.add (parse_error_to_flow_error err) acc)
        ) Errors.ErrorSet.empty parse_errors in
        save_parse_errors lib_file converted;
        exclude_syms, ((lib_file, false) :: result)

    ) (SSet.empty, [])

  in result

let stub_metadata ~root ~checked = { Context.
  checked;
  enable_const_params = false;
  enable_unsafe_getters_and_setters = true;
  enforce_strict_type_args = true;
  esproposal_class_static_fields = Options.ESPROPOSAL_ENABLE;
  esproposal_class_instance_fields = Options.ESPROPOSAL_ENABLE;
  esproposal_decorators = Options.ESPROPOSAL_ENABLE;
  esproposal_export_star_as = Options.ESPROPOSAL_ENABLE;
  facebook_fbt = None;
  ignore_non_literal_requires = false;
  max_trace_depth = 0;
  max_workers = 0;
  munge_underscores = false;
  output_graphml = false;
  root;
  strip_root = true;
  suppress_comments = [];
  suppress_types = SSet.empty;
  verbose = None;
  weak = false;
  jsx = None;
}

let get_master_cx =
  let master_cx = ref None in
  fun root ->
    match !master_cx with
    | Some (prev_root, cx) -> assert (prev_root = root); cx
    | None ->
      let cx = Flow_js.fresh_context
        (stub_metadata ~root ~checked:false)
        Loc.Builtins
        (Modulename.String Files.lib_module) in
      master_cx := Some (root, cx);
      cx

let set_libs filenames =
  let root = Path.dummy_path in
  let master_cx = get_master_cx root in
  let metadata = stub_metadata ~root ~checked:true in
  let _ = load_lib_files
    ~master_cx
    ~metadata
    filenames
    (fun _file _errs -> ())
    (fun _file _errs -> ())
    (fun _file _sups -> ()) in

  Flow_js.Cache.clear();
  let reason = Reason.builtin_reason (Reason.RCustom "module") in
  let builtin_module = Flow_js.mk_object master_cx reason in
  Flow_js.flow_t master_cx (builtin_module, Flow_js.builtins master_cx);
  Merge_js.ContextOptimizer.sig_context [master_cx]

let check_content ~filename ~content =
  let stdin_file = Some (Path.make_unsafe filename, content) in
  let root = Path.dummy_path in
  let filename = Loc.SourceFile filename in
  let errors = match parse_content filename content with
  | ast, [] ->
    (* defaults *)
    let metadata = stub_metadata ~root ~checked:true in

    Flow_js.Cache.clear();

    let cx = Type_inference_js.infer_ast
      ~metadata ~filename ~module_name:(Modulename.String "-") ast
    in

    (* this is a VERY pared-down version of Merge_service.merge_strict_context.
       it relies on the JS version only supporting libs + 1 file, so every
       module you can require() must come from a lib; this skips resolving
       module names and just adds them all to the `decls` list. *)
    let decls = SSet.fold (fun module_name acc ->
      (module_name, Modulename.String module_name, cx) :: acc
    ) (Context.required cx) [] in

    let master_cx = get_master_cx root in
    Merge_js.merge_component_strict [cx] [] [] [] decls master_cx;

    Context.errors cx
  | _, parse_errors ->
    List.fold_left (fun acc err ->
      Errors.(ErrorSet.add (parse_error_to_flow_error err) acc)
    ) Errors.ErrorSet.empty parse_errors
  in
  errors
  |> Errors.ErrorSet.elements
  |> Errors.json_of_errors_with_context ~root ~stdin_file
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
    let filename = Loc.SourceFile filename in
    let root = Path.dummy_path in
    match parse_content filename content with
    | ast, [] ->
      (* defaults *)
      let metadata = stub_metadata ~root ~checked:true in

      Flow_js.Cache.clear();

      let cx = Type_inference_js.infer_ast
        ~metadata ~filename ~module_name:(Modulename.String "-") ast
      in
      let loc = mk_loc filename line col in
      let (loc, ground_t, possible_ts) = Query_types.query_type cx loc in
      let ty, raw_type = match ground_t with
        | None -> None, None
        | Some t ->
            let ty = Some (Type_printer.string_of_t cx t) in
            let raw_type = Some (Debug_js.jstr_of_t ~depth:10 cx t)
            in
            ty, raw_type
      in
      let reasons =
        possible_ts
        |> List.map Type.reason_of_t
      in
      (None, Some (loc, ty, raw_type, reasons))
    | _, _ -> failwith "parse error"


let handle_inferred_result (_, inferred, _, _) =
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
  if (Js.typeof (Js.Unsafe.js_expr "exports") != Js.string "undefined")
  then Js.Unsafe.js_expr "exports"
  else begin
    let exports = Js.Unsafe.obj [||] in
    Js.Unsafe.set Js.Unsafe.global "flow" exports;
    exports
  end
let () = Js.Unsafe.set exports "registerFile" (
  Js.wrap_callback (fun name content -> Sys_js.register_file ~name ~content)
)
let () = Js.Unsafe.set exports
  "setLibs" (Js.wrap_callback set_libs_js)
let () = Js.Unsafe.set exports
  "check" (Js.wrap_callback check_js)
let () = Js.Unsafe.set exports
  "checkContent" (Js.wrap_callback check_content_js)
let () = Js.Unsafe.set exports
  "jsOfOcamlVersion" (Js.string Sys_js.js_of_ocaml_version)
let () = Js.Unsafe.set exports
  "flowVersion" (Js.string FlowConfig.version)
let () = Js.Unsafe.set exports
  "parse" (Js.wrap_callback Flow_parser_js.parse)
let () = Js.Unsafe.set exports
  "typeAtPos" (Js.wrap_callback type_at_pos)
