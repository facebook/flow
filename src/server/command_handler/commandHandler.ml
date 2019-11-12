(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base.Result
open ServerEnv
open Utils_js
open Lsp

let status_log errors =
  if Errors.ConcreteLocPrintableErrorSet.is_empty errors then
    Hh_logger.info "Status: OK"
  else
    Hh_logger.info "Status: Error";
  flush stdout

let convert_errors ~errors ~warnings ~suppressed_errors =
  if
    Errors.ConcreteLocPrintableErrorSet.is_empty errors
    && Errors.ConcreteLocPrintableErrorSet.is_empty warnings
    && suppressed_errors = []
  then
    ServerProt.Response.NO_ERRORS
  else
    ServerProt.Response.ERRORS { errors; warnings; suppressed_errors }

let json_of_parse_error =
  let int x = Hh_json.JSON_Number (string_of_int x) in
  let position p = Hh_json.JSON_Object [("line", int p.Loc.line); ("column", int p.Loc.column)] in
  let location loc =
    Hh_json.JSON_Object [("start", position loc.Loc.start); ("end", position loc.Loc._end)]
  in
  fun (loc, err) ->
    Hh_json.JSON_Object
      [("loc", location loc); ("message", Hh_json.JSON_String (Parse_error.PP.error err))]

let fold_json_of_parse_errors parse_errors acc =
  match parse_errors with
  | err :: _ ->
    ("parse_error", json_of_parse_error err)
    :: ("parse_error_count", Hh_json.JSON_Number (parse_errors |> List.length |> string_of_int))
    :: acc
  | [] -> acc

let get_status ~reader genv env client_root =
  let options = genv.ServerEnv.options in
  let server_root = Options.root options in
  let lazy_stats = Rechecker.get_lazy_stats ~options env in
  let status_response =
    if server_root <> client_root then
      ServerProt.Response.DIRECTORY_MISMATCH
        { ServerProt.Response.server = server_root; ServerProt.Response.client = client_root }
    else
      (* collate errors by origin *)
      let (errors, warnings, suppressed_errors) = ErrorCollator.get ~reader ~options env in
      let warnings =
        if Options.should_include_warnings options then
          warnings
        else
          Errors.ConcreteLocPrintableErrorSet.empty
      in
      let suppressed_errors =
        if Options.include_suppressions options then
          suppressed_errors
        else
          []
      in
      (* TODO: check status.directory *)
      status_log errors;
      FlowEventLogger.status_response
        ~num_errors:(Errors.ConcreteLocPrintableErrorSet.cardinal errors);
      convert_errors ~errors ~warnings ~suppressed_errors
  in
  (status_response, lazy_stats)

let autocomplete ~trigger_character ~reader ~options ~env ~profiling ~broader_context file_input =
  let (path, content) =
    match file_input with
    | File_input.FileName _ -> failwith "Not implemented"
    | File_input.FileContent (_, content) -> (File_input.filename_of_file_input file_input, content)
  in
  let path = File_key.SourceFile path in
  Autocomplete_js.autocomplete_set_hooks trigger_character;
  let%lwt check_contents_result = Types_js.type_contents ~options ~env ~profiling content path in
  Autocomplete_js.autocomplete_unset_hooks ();
  let initial_json_props =
    let open Hh_json in
    [
      ("ac_trigger", JSON_String (Option.value trigger_character ~default:"None"));
      ("broader_context", JSON_String broader_context);
    ]
  in
  match check_contents_result with
  | Error err ->
    let json_data_to_log =
      let open Hh_json in
      JSON_Object
        ( ("errors", JSON_Array [JSON_String err])
        :: ("result", JSON_String "FAILURE_CHECK_CONTENTS")
        :: ("count", JSON_Number "0")
        :: initial_json_props )
    in
    Lwt.return (Error err, Some json_data_to_log)
  | Ok (cx, info, file_sig, tast, parse_errors) ->
    Profiling_js.with_timer_lwt profiling ~timer:"GetResults" ~f:(fun () ->
        try_with_json2 (fun () ->
            let (ac_type_string, results_res) =
              AutocompleteService_js.autocomplete_get_results
                ~reader
                cx
                file_sig
                tast
                trigger_character
            in
            let (results, errors_to_log, response) =
              let open AutocompleteService_js in
              match results_res with
              | AcResult { results; errors_to_log } -> (results, errors_to_log, Ok results)
              | AcFatalError error -> ([], [error], Error error)
            in
            let result_string =
              match (results, errors_to_log) with
              | (_, []) -> "SUCCESS"
              | ([], _ :: _) -> "FAILURE"
              | (_ :: _, _ :: _) -> "PARTIAL"
            in
            let json_props_to_log =
              let open Hh_json in
              ("ac_type", JSON_String ac_type_string)
              :: ("result", JSON_String result_string)
              :: ("count", JSON_Number (results |> List.length |> string_of_int))
              :: ("errors", JSON_Array (List.map (fun s -> JSON_String s) errors_to_log))
              :: ("docblock", Docblock.json_of_docblock info)
              :: initial_json_props
              |> fold_json_of_parse_errors parse_errors
            in
            Lwt.return (response, Some (Hh_json.JSON_Object json_props_to_log))))

let check_file ~options ~env ~profiling ~force file_input =
  let file = File_input.filename_of_file_input file_input in
  match file_input with
  | File_input.FileName _ -> failwith "Not implemented"
  | File_input.FileContent (_, content) ->
    let should_check =
      if force then
        true
      else
        let (_, docblock) =
          Parsing_service_js.(
            parse_docblock docblock_max_tokens (File_key.SourceFile file) content)
        in
        Docblock.is_flow docblock
    in
    if should_check then
      let file = File_key.SourceFile file in
      let%lwt (_, errors, warnings) =
        Types_js.typecheck_contents ~options ~env ~profiling content file
      in
      Lwt.return (convert_errors ~errors ~warnings ~suppressed_errors:[])
    else
      Lwt.return ServerProt.Response.NOT_COVERED

let infer_type
    ~(options : Options.t)
    ~(env : ServerEnv.env)
    ~(profiling : Profiling_js.running)
    ((file_input, line, col, verbose, expand_aliases, omit_targ_defaults, evaluate_type_destructors) :
      File_input.t * int * int * Verbose.t option * bool * bool * bool) :
    ((Loc.t * Ty.t option, string) result * Hh_json.json option) Lwt.t =
  let file = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile file in
  let options = { options with Options.opt_verbose = verbose } in
  match File_input.content_of_file_input file_input with
  | Error e -> Lwt.return (Error e, None)
  | Ok content ->
    let%lwt result =
      try_with_json (fun () ->
          Type_info_service.type_at_pos
            ~options
            ~env
            ~profiling
            ~expand_aliases
            ~omit_targ_defaults
            ~evaluate_type_destructors
            file
            content
            line
            col)
    in
    Lwt.return (split_result result)

let insert_type
    ~options
    ~env
    ~profiling
    ~file_input
    ~target
    ~verbose
    ~expand_aliases
    ~omit_targ_defaults
    ~location_is_strict
    ~ambiguity_strategy =
  let filename = File_input.filename_of_file_input file_input in
  let file_key = File_key.SourceFile filename in
  let options = { options with Options.opt_verbose = verbose } in
  File_input.content_of_file_input file_input
  %>>= fun file_content ->
  try_with (fun _ ->
      let%lwt result =
        Type_info_service.insert_type
          ~options
          ~env
          ~profiling
          ~file_key
          ~file_content
          ~target
          ~expand_aliases
          ~omit_targ_defaults
          ~location_is_strict
          ~ambiguity_strategy
      in
      Lwt.return result)

let autofix_exports ~options ~env ~profiling ~input =
  let filename = File_input.filename_of_file_input input in
  let file_key = File_key.SourceFile filename in
  File_input.content_of_file_input input
  %>>= fun file_content ->
  try_with (fun _ ->
      let%lwt result =
        Type_info_service.autofix_exports ~options ~env ~profiling ~file_key ~file_content
      in
      Lwt.return result)

let collect_rage ~options ~reader ~env ~files =
  let items = [] in
  (* options *)
  let data = Printf.sprintf "lazy_mode=%s\n" Options.(lazy_mode options |> lazy_mode_to_string) in
  let items = ("options", data) :: items in
  (* env: checked files *)
  let data =
    Printf.sprintf
      "%s\n\n%s\n"
      (CheckedSet.debug_counts_to_string env.checked_files)
      (CheckedSet.debug_to_string ~limit:200 env.checked_files)
  in
  let items = ("env.checked_files", data) :: items in
  (* env: dependency graph *)
  let dependency_to_string (file, deps) =
    let file = File_key.to_string file in
    let deps =
      Utils_js.FilenameSet.elements deps
      |> Base.List.map ~f:File_key.to_string
      |> ListUtils.first_upto_n 20 (fun t -> Some (Printf.sprintf " ...%d more" t))
      |> String.concat ","
    in
    file ^ ":" ^ deps ^ "\n"
  in
  let dependencies =
    Dependency_info.implementation_dependency_graph env.ServerEnv.dependency_info
    |> Utils_js.FilenameMap.bindings
    |> Base.List.map ~f:dependency_to_string
    |> ListUtils.first_upto_n 200 (fun t -> Some (Printf.sprintf "[shown 200/%d]\n" t))
    |> String.concat ""
  in
  let data = "DEPENDENCIES:\n" ^ dependencies in
  let items = ("env.dependencies", data) :: items in
  (* env: errors *)
  let (errors, warnings, _) = ErrorCollator.get ~reader ~options env in
  let json =
    Errors.Json_output.json_of_errors_with_context
      ~strip_root:None
      ~stdin_file:None
      ~offset_kind:Offset_utils.Utf8
      ~suppressed_errors:[]
      ~errors
      ~warnings
      ()
  in
  let data = "ERRORS:\n" ^ Hh_json.json_to_multiline json in
  let items = ("env.errors", data) :: items in
  (* Checking if file hashes are up to date *)
  let items =
    Option.value_map files ~default:items ~f:(fun files ->
        let buf = Buffer.create 1024 in
        Printf.bprintf
          buf
          "Does the content on the disk match the most recent version of the file?\n\n";
        List.iter
          (fun file ->
            (* TODO - this isn't exactly right. It could be something else, right? *)
            let file_key = File_key.SourceFile file in
            let file_state =
              if not (FilenameSet.mem file_key env.ServerEnv.files) then
                "FILE NOT PARSED BY FLOW (likely ignored implicitly or explicitly)"
              else
                match Sys_utils.cat_or_failed file with
                | None -> "ERROR! FAILED TO READ"
                | Some content ->
                  if Parsing_service_js.does_content_match_file_hash ~reader file_key content then
                    "OK"
                  else
                    "HASH OUT OF DATE"
            in
            Printf.bprintf buf "%s: %s\n" file file_state)
          files;
        ("file hash check", Buffer.contents buf) :: items)
  in
  let items =
    let buf = Buffer.create 127 in
    let log str =
      Buffer.add_string buf str;
      Buffer.add_char buf '\n'
    in
    LoggingUtils.dump_server_options ~server_options:options ~log;
    ("server_options", Buffer.contents buf) :: items
  in
  items

let dump_types ~options ~env ~profiling ~expand_aliases ~evaluate_type_destructors file_input =
  let file = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile file in
  File_input.content_of_file_input file_input
  %>>= fun content ->
  try_with (fun () ->
      Type_info_service.dump_types
        ~options
        ~env
        ~profiling
        ~expand_aliases
        ~evaluate_type_destructors
        file
        content)

let coverage ~options ~env ~profiling ~force ~trust file_input =
  if Options.trust_mode options = Options.NoTrust && trust then
    Error
      "Coverage cannot be run in trust mode if the server is not in trust mode. \n\nRestart the Flow server with --trust-mode=check' to enable this command."
    |> Lwt.return
  else
    let file = File_input.filename_of_file_input file_input in
    let file = File_key.SourceFile file in
    File_input.content_of_file_input file_input
    %>>= fun content ->
    try_with (fun () ->
        Type_info_service.coverage ~options ~env ~profiling ~force ~trust file content)

let batch_coverage ~options ~env ~trust ~batch =
  if Options.trust_mode options = Options.NoTrust && trust then
    Error
      "Batch Coverage cannot be run in trust mode if the server is not in trust mode. \n\nRestart the Flow server with --trust-mode=check' to enable this command."
    |> Lwt.return
  else if Options.lazy_mode options <> Options.NON_LAZY_MODE then
    Error
      "Batch coverage cannot be run in lazy mode.\n\nRestart the Flow server with '--lazy-mode none' to enable this command."
    |> Lwt.return
  else
    let is_checked key = CheckedSet.mem key env.checked_files in
    let filter key = Base.List.exists ~f:(fun elt -> Files.is_prefix elt key) batch in
    let coverage_map =
      FilenameMap.filter
        (fun key _ -> is_checked key && File_key.to_string key |> filter)
        env.coverage
    in
    let response =
      FilenameMap.fold (fun key coverage -> List.cons (key, coverage)) coverage_map []
    in
    Ok response |> Lwt.return

let serialize_graph graph =
  (* Convert from map/set to lists for serialization to client. *)
  FilenameMap.fold
    (fun f dep_fs acc ->
      let f = File_key.to_string f in
      let dep_fs = FilenameSet.fold (fun dep_f acc -> File_key.to_string dep_f :: acc) dep_fs [] in
      (f, dep_fs) :: acc)
    graph
    []

let output_dependencies ~env root strip_root types_only outfile =
  let strip_root =
    if strip_root then
      Files.relative_path root
    else
      fun x ->
    x
  in
  let dep_graph =
    if types_only then
      Dependency_info.sig_dependency_graph
    else
      Dependency_info.implementation_dependency_graph
  in
  let graph = serialize_graph (dep_graph env.ServerEnv.dependency_info) in
  Hh_logger.info "printing dependency graph to %s\n" outfile;
  let%lwt out = Lwt_io.open_file ~mode:Lwt_io.Output outfile in
  let%lwt () = LwtUtils.output_graph out strip_root graph in
  let%lwt () = Lwt_io.close out in
  ok_unit |> Lwt.return

let get_cycle ~env fn types_only =
  (* Re-calculate SCC *)
  let parsed = env.ServerEnv.files in
  let dependency_info = env.ServerEnv.dependency_info in
  let dependency_graph =
    if types_only then
      Dependency_info.sig_dependency_graph dependency_info
    else
      Dependency_info.implementation_dependency_graph dependency_info
  in
  Lwt.return
    (Ok
       (let components = Sort_js.topsort ~roots:parsed dependency_graph in
        (* Get component for target file *)
        let component = List.find (Nel.mem ~equal:File_key.equal fn) components in
        (* Restrict dep graph to only in-cycle files *)
        Nel.fold_left
          (fun acc f ->
            Option.fold (FilenameMap.find_opt f dependency_graph) ~init:acc ~f:(fun acc deps ->
                let subdeps =
                  FilenameSet.filter (fun f -> Nel.mem ~equal:File_key.equal f component) deps
                in
                if FilenameSet.is_empty subdeps then
                  acc
                else
                  FilenameMap.add f subdeps acc))
          FilenameMap.empty
          component
        |> serialize_graph))

let suggest ~options ~env ~profiling file =
  let file_name = File_input.filename_of_file_input file in
  File_input.content_of_file_input file
  %>>= fun file_content ->
  try_with (fun _ ->
      let%lwt result = Type_info_service.suggest ~options ~env ~profiling file_name file_content in
      match result with
      | Ok (tc_errors, tc_warnings, suggest_warnings, file_patch) ->
        Lwt.return
          (Ok
             (ServerProt.Response.Suggest_Ok
                { tc_errors; tc_warnings; suggest_warnings; file_patch }))
      | Error errors -> Lwt.return (Ok (ServerProt.Response.Suggest_Error errors)))

let find_module ~options ~reader (moduleref, filename) =
  let file = File_key.SourceFile filename in
  let loc = { Loc.none with Loc.source = Some file } in
  let module_name =
    Module_js.imported_module
      ~options
      ~reader:(Abstract_state_reader.State_reader reader)
      ~node_modules_containers:!Files.node_modules_containers
      file
      (Nel.one (ALoc.of_loc loc))
      moduleref
  in
  Module_heaps.Reader.get_file ~reader ~audit:Expensive.warn module_name

let convert_find_refs_result (result : FindRefsTypes.find_refs_ok) :
    ServerProt.Response.find_refs_success =
  Option.map result ~f:(fun (name, refs) -> (name, Base.List.map ~f:snd refs))

(* Find refs is a really weird command. Whereas other commands will cancel themselves if they find
 * unchecked code, find refs will focus that code and keep chugging along. It may therefore change
 * the env. Furthermore, it is written using a lot of `result`'s, which make it really hard to
 * properly pass through the env. Therefore, it uses an `ServerEnv.env ref` instead of an
 * `ServerEnv.env`. *)
let find_global_refs ~reader ~genv ~env ~profiling (file_input, line, col, multi_hop) =
  let env = ref env in
  let%lwt (result, dep_count) =
    FindRefs_js.find_global_refs ~reader ~genv ~env ~profiling ~file_input ~line ~col ~multi_hop
  in
  let env = !env in
  let result = Base.Result.map result ~f:convert_find_refs_result in
  Lwt.return (env, result, dep_count)

let find_local_refs ~reader ~options ~env ~profiling (file_input, line, col) =
  FindRefs_js.find_local_refs ~reader ~options ~env ~profiling ~file_input ~line ~col
  |> Lwt_result.map convert_find_refs_result

(* This returns result, json_data_to_log, where json_data_to_log is the json data from
 * getdef_get_result which we end up using *)
let get_def ~options ~reader ~env ~profiling (file_input, line, col) =
  let filename = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile filename in
  let loc = Loc.make file line col in
  let%lwt check_result =
    File_input.content_of_file_input file_input
    %>>= (fun content -> Types_js.type_contents ~options ~env ~profiling content file)
  in
  match check_result with
  | Error msg ->
    Lwt.return (Error msg, Some (Hh_json.JSON_Object [("error", Hh_json.JSON_String msg)]))
  | Ok (cx, _, file_sig, typed_ast, parse_errors) ->
    Profiling_js.with_timer_lwt profiling ~timer:"GetResult" ~f:(fun () ->
        try_with_json2 (fun () ->
            Lwt.return
              ( GetDef_js.get_def ~options ~reader cx file_sig typed_ast loc
              |> fun (result, request_history) ->
              let open GetDef_js.Get_def_result in
              let json_props =
                [
                  ( "request_history",
                    Hh_json.JSON_Array
                      (Base.List.map ~f:(fun str -> Hh_json.JSON_String str) request_history) );
                ]
                |> fold_json_of_parse_errors parse_errors
              in
              match result with
              | Def loc ->
                ( Ok loc,
                  Some
                    (Hh_json.JSON_Object (("result", Hh_json.JSON_String "SUCCESS") :: json_props))
                )
              | Partial (loc, msg) ->
                ( Ok loc,
                  Some
                    (Hh_json.JSON_Object
                       ( ("result", Hh_json.JSON_String "PARTIAL_FAILURE")
                       :: ("error", Hh_json.JSON_String msg)
                       :: json_props )) )
              | Bad_loc ->
                ( Ok Loc.none,
                  Some
                    (Hh_json.JSON_Object (("result", Hh_json.JSON_String "BAD_LOC") :: json_props))
                )
              | Def_error msg ->
                ( Error msg,
                  Some
                    (Hh_json.JSON_Object
                       ( ("result", Hh_json.JSON_String "FAILURE")
                       :: ("error", Hh_json.JSON_String msg)
                       :: json_props )) ) )))

let module_name_of_string ~options module_name_str =
  let file_options = Options.file_options options in
  let path = Path.to_string (Path.make module_name_str) in
  if Files.is_flow_file ~options:file_options path then
    Modulename.Filename (File_key.SourceFile path)
  else
    Modulename.String module_name_str

let get_imports ~options ~reader module_names =
  let add_to_results (map, non_flow) module_name_str =
    let module_name = module_name_of_string ~options module_name_str in
    match Module_heaps.Reader.get_file ~reader ~audit:Expensive.warn module_name with
    | Some file ->
      (* We do not process all modules which are stored in our module
       * database. In case we do not process a module its requirements
       * are not kept track of. To avoid confusing results we notify the
       * client that these modules have not been processed.
       *)
      let { Module_heaps.checked; _ } =
        Module_heaps.Reader.get_info_unsafe ~reader ~audit:Expensive.warn file
      in
      if checked then
        let { Module_heaps.resolved_modules; _ } =
          Module_heaps.Reader.get_resolved_requires_unsafe ~reader ~audit:Expensive.warn file
        in
        let fsig = Parsing_heaps.Reader.get_file_sig_unsafe ~reader file in
        let requires = File_sig.With_Loc.(require_loc_map fsig.module_sig) in
        let mlocs =
          SMap.fold
            (fun mref locs acc ->
              let m = SMap.find mref resolved_modules in
              Modulename.Map.add m locs acc)
            requires
            Modulename.Map.empty
        in
        (SMap.add module_name_str mlocs map, non_flow)
      else
        (map, SSet.add module_name_str non_flow)
    | None ->
      (* We simply ignore non existent modules *)
      (map, non_flow)
  in
  (* Our result is a tuple. The first element is a map from module names to
   * modules imported by them and their locations of import. The second
   * element is a set of modules which are not marked for processing by
   * flow. *)
  List.fold_left add_to_results (SMap.empty, SSet.empty) module_names

let save_state ~saved_state_filename ~genv ~env ~profiling =
  try_with (fun () ->
      let%lwt () = Saved_state.save ~saved_state_filename ~genv ~env ~profiling in
      Lwt.return (Ok ()))

let handle_autocomplete ~trigger_character ~reader ~options ~input ~profiling ~env ~broader_context
    =
  let%lwt (result, json_data) =
    autocomplete ~trigger_character ~reader ~options ~env ~profiling ~broader_context input
  in
  Lwt.return (ServerProt.Response.AUTOCOMPLETE result, json_data)

let handle_autofix_exports ~options ~input ~profiling ~env =
  let%lwt result = autofix_exports ~options ~env ~profiling ~input in
  Lwt.return (ServerProt.Response.AUTOFIX_EXPORTS result, None)

let handle_check_file ~options ~force ~input ~profiling ~env =
  let%lwt response = check_file ~options ~env ~force ~profiling input in
  Lwt.return (ServerProt.Response.CHECK_FILE response, None)

let handle_coverage ~options ~force ~input ~trust ~profiling ~env =
  let%lwt response = coverage ~options ~env ~profiling ~force ~trust input in
  Lwt.return (ServerProt.Response.COVERAGE response, None)

let handle_batch_coverage ~options ~profiling:_ ~env ~batch ~trust =
  let%lwt response = batch_coverage ~options ~env ~batch ~trust in
  Lwt.return (ServerProt.Response.BATCH_COVERAGE response, None)

let handle_cycle ~fn ~types_only ~profiling:_ ~env =
  let%lwt response = get_cycle ~env fn types_only in
  Lwt.return (env, ServerProt.Response.CYCLE response, None)

let handle_dump_types ~options ~input ~expand_aliases ~evaluate_type_destructors ~profiling ~env =
  let%lwt response =
    dump_types ~options ~env ~profiling ~expand_aliases ~evaluate_type_destructors input
  in
  Lwt.return (ServerProt.Response.DUMP_TYPES response, None)

let handle_find_module ~options ~reader ~moduleref ~filename ~profiling:_ ~env:_ =
  let response = find_module ~options ~reader (moduleref, filename) in
  Lwt.return (ServerProt.Response.FIND_MODULE response, None)

let handle_find_refs ~reader ~genv ~filename ~line ~char ~global ~multi_hop ~profiling ~env =
  let%lwt (env, result, dep_count) =
    if global || multi_hop then
      find_global_refs ~reader ~genv ~env ~profiling (filename, line, char, multi_hop)
    else
      let options = genv.ServerEnv.options in
      let%lwt result = find_local_refs ~reader ~options ~env ~profiling (filename, line, char) in
      Lwt.return (env, result, None)
  in
  let json_data =
    Some
      (Hh_json.JSON_Object
         ( ( "result",
             Hh_json.JSON_String
               (match result with
               | Ok _ -> "SUCCESS"
               | _ -> "FAILURE") )
         :: ("global", Hh_json.JSON_Bool global)
         ::
         (match dep_count with
         | Some count -> [("deps", Hh_json.JSON_Number (string_of_int count))]
         | None -> []) ))
  in
  Lwt.return (env, ServerProt.Response.FIND_REFS result, json_data)

let handle_force_recheck ~files ~focus ~profile ~profiling =
  let fileset = SSet.of_list files in
  let reason =
    LspProt.(
      match files with
      | [filename] -> Single_file_changed { filename }
      | _ -> Many_files_changed { file_count = List.length files })
  in
  (* `flow force-recheck --focus a.js` not only marks a.js as a focused file, but it also
   * tells Flow that `a.js` has changed. In that case we push a.js to be rechecked and to be
   * focused *)
  let push ?callback files =
    ServerMonitorListenerState.(
      if focus then
        push_files_to_force_focused_and_recheck ?callback ~reason files
      else
        push_files_to_recheck ?metadata:None ?callback ~reason files)
  in
  if profile then (
    let (wait_for_recheck_thread, wakener) = Lwt.task () in
    push ~callback:(fun profiling -> Lwt.wakeup wakener profiling) fileset;
    let%lwt recheck_profiling = wait_for_recheck_thread in
    Option.iter recheck_profiling ~f:(fun recheck_profiling ->
        Profiling_js.merge ~from:recheck_profiling ~into:profiling);
    Lwt.return (ServerProt.Response.FORCE_RECHECK recheck_profiling, None)
  ) else (
    (* If we're not profiling the recheck, then respond immediately *)
    push fileset;
    Lwt.return (ServerProt.Response.FORCE_RECHECK None, None)
  )

let handle_get_def ~reader ~options ~filename ~line ~char ~profiling ~env =
  let%lwt (result, json_data) = get_def ~reader ~options ~env ~profiling (filename, line, char) in
  Lwt.return (ServerProt.Response.GET_DEF result, json_data)

let handle_get_imports ~options ~reader ~module_names ~profiling:_ ~env:_ =
  let response = get_imports ~options ~reader module_names in
  Lwt.return (ServerProt.Response.GET_IMPORTS response, None)

let handle_graph_dep_graph ~root ~strip_root ~outfile ~types_only ~profiling:_ ~env =
  let%lwt response = output_dependencies ~env root strip_root types_only outfile in
  Lwt.return (env, ServerProt.Response.GRAPH_DEP_GRAPH response, None)

let handle_infer_type
    ~options
    ~input
    ~line
    ~char
    ~verbose
    ~expand_aliases
    ~omit_targ_defaults
    ~evaluate_type_destructors
    ~profiling
    ~env =
  let%lwt (result, json_data) =
    infer_type
      ~options
      ~env
      ~profiling
      (input, line, char, verbose, expand_aliases, omit_targ_defaults, evaluate_type_destructors)
  in
  Lwt.return (ServerProt.Response.INFER_TYPE result, json_data)

let handle_insert_type
    ~options
    ~file_input
    ~target
    ~verbose
    ~expand_aliases
    ~omit_targ_defaults
    ~location_is_strict
    ~ambiguity_strategy
    ~profiling
    ~env =
  let%lwt result =
    insert_type
      ~options
      ~env
      ~profiling
      ~file_input
      ~target
      ~verbose
      ~expand_aliases
      ~omit_targ_defaults
      ~location_is_strict
      ~ambiguity_strategy
  in
  Lwt.return (ServerProt.Response.INSERT_TYPE result, None)

let handle_rage ~reader ~options ~files ~profiling:_ ~env =
  let items = collect_rage ~options ~reader ~env ~files:(Some files) in
  Lwt.return (ServerProt.Response.RAGE items, None)

let handle_refactor
    ~reader ~genv ~input:file_input ~line ~char:col ~refactor_variant ~profiling ~env =
  (* Refactor is another weird command that may mutate the env by doing a bunch of rechecking,
   * since that's what find-refs does and refactor delegates to find-refs *)
  ServerProt.Response.(
    let env = ref env in
    let%lwt result =
      match refactor_variant with
      | ServerProt.Request.RENAME new_name ->
        Refactor_service.rename ~reader ~genv ~env ~profiling ~file_input ~line ~col ~new_name
    in
    let env = !env in
    let result =
      result |> Base.Result.map ~f:(Option.map ~f:(fun refactor_edits -> { refactor_edits }))
    in
    Lwt.return (env, REFACTOR result, None))

let handle_status ~reader ~genv ~client_root ~profiling:_ ~env =
  let (status_response, lazy_stats) = get_status ~reader genv env client_root in
  Lwt.return (env, ServerProt.Response.STATUS { status_response; lazy_stats }, None)

let handle_suggest ~options ~input ~profiling ~env =
  let%lwt result = suggest ~options ~env ~profiling input in
  Lwt.return (ServerProt.Response.SUGGEST result, None)

let handle_save_state ~saved_state_filename ~genv ~profiling ~env =
  let%lwt result = save_state ~saved_state_filename ~genv ~env ~profiling in
  Lwt.return (env, ServerProt.Response.SAVE_STATE result, None)

let find_code_actions ~options ~env ~profiling ~params ~client =
  CodeActionRequest.(
    Flow_lsp_conversions.(
      let { textDocument; range; _ } = params in
      (* The current ide-lsp-server/flow-lsp-client doesn't necisarrily get restart for every project.
       * Checking the option here ensures the the flow server doesn't do too much work for code
       * action requests on projects where code actions are not enabled in the `.flowconfig`.
       *)
      if not options.Options.opt_lsp_code_actions then
        Lwt.return (Ok [])
      else
        let (file_key, file, loc) = lsp_textDocument_and_range_to_flow textDocument range client in
        match File_input.content_of_file_input file with
        | Error msg -> Lwt.return (Error msg)
        | Ok file_contents ->
          Type_info_service.code_actions_at_loc
            ~options
            ~env
            ~profiling
            ~params
            ~file_key
            ~file_contents
            ~loc))

type command_handler =
  (* A command can be handled immediately if it is super duper fast and doesn't require the env.
   * These commands will be handled as soon as we read them off the pipe. Almost nothing should ever
   * be handled immediately *)
  | Handle_immediately of
      (profiling:Profiling_js.running ->
      (ServerProt.Response.response * Hh_json.json option) Lwt.t)
  (* A command is parallelizable if it passes four conditions
   *
   * 1. It is fast. Running it in parallel will block the current recheck, so it needs to be really
   *    fast.
   * 2. It doesn't use the workers. Currently we can't deal with the recheck using the workers at the
   *    same time as a command using the workers
   * 3. It doesn't return a new env. It really should be just a read-only job
   * 4. It doesn't mind using slightly out of date data. During a recheck, it will be reading the
   *    oldified data
   *)
  | Handle_parallelizable of
      (profiling:Profiling_js.running ->
      env:ServerEnv.env ->
      (ServerProt.Response.response * Hh_json.json option) Lwt.t)
  (* A command is nonparallelizable if it can't be handled immediately or parallelized. *)
  | Handle_nonparallelizable of
      (profiling:Profiling_js.running ->
      env:ServerEnv.env ->
      (ServerEnv.env * ServerProt.Response.response * Hh_json.json option) Lwt.t)

(* This command is parallelizable, but we will treat it as nonparallelizable if we've been told
 * to wait_for_recheck by the .flowconfig or CLI *)
let mk_parallelizable ~wait_for_recheck ~options f =
  let wait_for_recheck =
    Option.value wait_for_recheck ~default:(Options.wait_for_recheck options)
  in
  if wait_for_recheck then
    Handle_nonparallelizable
      (fun ~profiling ~env ->
        let%lwt (response, json_data) = f ~profiling ~env in
        Lwt.return (env, response, json_data))
  else
    Handle_parallelizable f

(* This function is called as soon as we read an ephemeral command from the pipe. It decides whether
 * the command should be handled immediately or deferred as parallelizable or nonparallelizable.
 * This function does NOT run any handling code itself. *)
let get_ephemeral_handler genv command =
  let options = genv.options in
  let reader = State_reader.create () in
  match command with
  | ServerProt.Request.AUTOCOMPLETE { trigger_character; input; wait_for_recheck; broader_context }
    ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_autocomplete ~trigger_character ~reader ~options ~input ~broader_context)
  | ServerProt.Request.AUTOFIX_EXPORTS { input; verbose; wait_for_recheck } ->
    let options = { options with Options.opt_verbose = verbose } in
    mk_parallelizable ~wait_for_recheck ~options (handle_autofix_exports ~input ~options)
  | ServerProt.Request.CHECK_FILE { input; verbose; force; include_warnings; wait_for_recheck } ->
    let options =
      {
        options with
        Options.opt_verbose = verbose;
        opt_include_warnings = options.Options.opt_include_warnings || include_warnings;
      }
    in
    mk_parallelizable ~wait_for_recheck ~options (handle_check_file ~options ~force ~input)
  | ServerProt.Request.COVERAGE { input; force; wait_for_recheck; trust } ->
    mk_parallelizable ~wait_for_recheck ~options (handle_coverage ~options ~force ~trust ~input)
  | ServerProt.Request.BATCH_COVERAGE { batch; wait_for_recheck; trust } ->
    mk_parallelizable ~wait_for_recheck ~options (handle_batch_coverage ~options ~trust ~batch)
  | ServerProt.Request.CYCLE { filename; types_only } ->
    (* The user preference is to make this wait for up-to-date data *)
    let file_options = Options.file_options options in
    let fn = Files.filename_from_string ~options:file_options filename in
    Handle_nonparallelizable (handle_cycle ~fn ~types_only)
  | ServerProt.Request.DUMP_TYPES
      { input; expand_aliases; evaluate_type_destructors; wait_for_recheck } ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_dump_types ~options ~input ~expand_aliases ~evaluate_type_destructors)
  | ServerProt.Request.FIND_MODULE { moduleref; filename; wait_for_recheck } ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_find_module ~options ~reader ~moduleref ~filename)
  | ServerProt.Request.FIND_REFS { filename; line; char; global; multi_hop } ->
    (* find-refs can take a while and may use MultiWorker. Furthermore, it may do a recheck and
     * change env. Each of these 3 facts disqualifies find-refs from being parallelizable *)
    Handle_nonparallelizable
      (handle_find_refs ~reader ~genv ~filename ~line ~char ~global ~multi_hop)
  | ServerProt.Request.FORCE_RECHECK { files; focus; profile } ->
    Handle_immediately (handle_force_recheck ~files ~focus ~profile)
  | ServerProt.Request.GET_DEF { filename; line; char; wait_for_recheck } ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_get_def ~reader ~options ~filename ~line ~char)
  | ServerProt.Request.GET_IMPORTS { module_names; wait_for_recheck } ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_get_imports ~options ~reader ~module_names)
  | ServerProt.Request.GRAPH_DEP_GRAPH { root; strip_root; outfile; types_only } ->
    (* The user preference is to make this wait for up-to-date data *)
    Handle_nonparallelizable (handle_graph_dep_graph ~root ~strip_root ~types_only ~outfile)
  | ServerProt.Request.INFER_TYPE
      {
        input;
        line;
        char;
        verbose;
        expand_aliases;
        omit_targ_defaults;
        evaluate_type_destructors;
        wait_for_recheck;
      } ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_infer_type
         ~options
         ~input
         ~line
         ~char
         ~verbose
         ~expand_aliases
         ~omit_targ_defaults
         ~evaluate_type_destructors)
  | ServerProt.Request.RAGE { files } ->
    mk_parallelizable ~wait_for_recheck:None ~options (handle_rage ~reader ~options ~files)
  | ServerProt.Request.INSERT_TYPE
      {
        input;
        target;
        wait_for_recheck;
        verbose;
        expand_aliases;
        omit_targ_defaults;
        location_is_strict;
        ambiguity_strategy;
      } ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_insert_type
         ~file_input:input
         ~options
         ~target
         ~verbose
         ~expand_aliases
         ~omit_targ_defaults
         ~location_is_strict
         ~ambiguity_strategy)
  | ServerProt.Request.REFACTOR { input; line; char; refactor_variant } ->
    (* refactor delegates to find-refs, which is not parallelizable. Therefore refactor is also not
     * parallelizable *)
    Handle_nonparallelizable (handle_refactor ~reader ~genv ~input ~line ~char ~refactor_variant)
  | ServerProt.Request.STATUS { client_root; include_warnings } ->
    let genv =
      {
        genv with
        options =
          Options.
            {
              options with
              opt_include_warnings = options.opt_include_warnings || include_warnings;
            };
      }
    in
    (* `flow status` is often used by users to get all the current errors. After talking to some
     * coworkers and users, glevi decided that users would rather that `flow status` always waits
     * for the current recheck to finish. So even though we could technically make `flow status`
     * parallelizable, we choose to make it nonparallelizable *)
    Handle_nonparallelizable (handle_status ~reader ~genv ~client_root)
  | ServerProt.Request.SUGGEST { input; wait_for_recheck } ->
    mk_parallelizable ~wait_for_recheck ~options (handle_suggest ~options ~input)
  | ServerProt.Request.SAVE_STATE { outfile } ->
    (* save-state can take awhile to run. Furthermore, you probably don't want to run this with out
     * of date data. So save-state is not parallelizable *)
    Handle_nonparallelizable (handle_save_state ~saved_state_filename:outfile ~genv)

(* This is the common code which wraps each command handler. It deals with stuff like logging and
 * catching exceptions *)
let wrap_ephemeral_handler handler ~genv ~request_id ~client_context ~workload ~cmd_str arg =
  try%lwt
    Hh_logger.info "Request: %s" cmd_str;
    MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;

    let%lwt (ret, profiling, json_data) = handler ~genv ~request_id ~workload arg in
    let event =
      ServerStatus.(
        Finishing_up
          {
            duration = Profiling_js.get_profiling_duration profiling;
            info = CommandSummary cmd_str;
          })
    in
    MonitorRPC.status_update ~event;
    FlowEventLogger.ephemeral_command_success ?json_data ~client_context ~profiling;
    Lwt.return (Ok ret)
  with
  | Lwt.Canceled as exn ->
    let exn = Exception.wrap exn in
    Exception.reraise exn
  | exn ->
    let exn = Exception.wrap exn in
    let exn_str = Exception.to_string exn in
    Hh_logger.error ~exn "Uncaught exception while handling a request (%s)" cmd_str;
    FlowEventLogger.ephemeral_command_failure
      ~client_context
      ~json_data:(Hh_json.JSON_Object [("exn", Hh_json.JSON_String exn_str)]);
    MonitorRPC.request_failed ~request_id ~exn_str;
    Lwt.return (Error ())

(* A few commands need to be handled immediately, as soon as they arrive from the monitor. An
 * `env` is NOT available, since we don't have the server's full attention *)
let handle_ephemeral_immediately_unsafe ~genv ~request_id ~workload () =
  let should_print_summary = Options.should_profile genv.options in
  let%lwt (profiling, (response, json_data)) =
    Profiling_js.with_profiling_lwt ~label:"Command" ~should_print_summary (fun profiling ->
        workload ~profiling)
  in
  MonitorRPC.respond_to_request ~request_id ~response;

  Lwt.return ((), profiling, json_data)

let handle_ephemeral_immediately = wrap_ephemeral_handler handle_ephemeral_immediately_unsafe

(* If command running in serial (i.e. not in parallel with a recheck) is canceled, it kicks off a
 * recheck itself and then reruns itself
 *
 * While parallelizable commands can be run out of order (some might get deferred),
 * nonparallelizable commands always run in order. So that's why we don't defer commands here.
 *
 * Since this might run a recheck, `workload ~profiling ~env` MUST return the new env.
 *)
let rec run_command_in_serial ~genv ~env ~profiling ~workload =
  try%lwt workload ~profiling ~env
  with Lwt.Canceled ->
    Hh_logger.info "Command successfully canceled. Running a recheck before restarting the command";
    let%lwt (recheck_profiling, env) = Rechecker.recheck_loop genv env in
    List.iter (fun from -> Profiling_js.merge ~into:profiling ~from) recheck_profiling;
    Hh_logger.info "Now restarting the command";
    run_command_in_serial ~genv ~env ~profiling ~workload

(* A command that is running in parallel with a recheck, if canceled, can't just run a recheck
 * itself. It needs to defer itself until later. *)
let run_command_in_parallel ~env ~profiling ~workload ~mk_workload =
  try%lwt workload ~profiling ~env
  with Lwt.Canceled as exn ->
    let exn = Exception.wrap exn in
    Hh_logger.info
      "Command successfully canceled. Requeuing the command for after the next recheck.";
    ServerMonitorListenerState.defer_parallelizable_workload (mk_workload ());
    Exception.reraise exn

let rec handle_parallelizable_ephemeral_unsafe
    ~client_context ~cmd_str ~genv ~request_id ~workload env =
  let should_print_summary = Options.should_profile genv.options in
  let%lwt (profiling, json_data) =
    Profiling_js.with_profiling_lwt ~label:"Command" ~should_print_summary (fun profiling ->
        let%lwt (response, json_data) =
          let mk_workload () =
            handle_parallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str
          in
          run_command_in_parallel ~env ~profiling ~workload ~mk_workload
        in
        MonitorRPC.respond_to_request ~request_id ~response;

        (* It sucks this has to live here. We need a better way to handle post-send logic
         * TODO - Do we actually need this error? Why do we even send the path? *)
        ServerProt.Response.(
          match response with
          | STATUS { lazy_stats = _; status_response = DIRECTORY_MISMATCH { server; client } } ->
            Hh_logger.fatal "Status: Error";
            Hh_logger.fatal
              "server_dir=%s, client_dir=%s"
              (Path.to_string server)
              (Path.to_string client);
            Hh_logger.fatal "flow server is not listening to the same directory. Exiting.";
            FlowExitStatus.(exit Server_client_directory_mismatch)
          | _ -> ());
        Lwt.return json_data)
  in
  Lwt.return ((), profiling, json_data)

and handle_parallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str env =
  try%lwt
    let handler = handle_parallelizable_ephemeral_unsafe ~client_context ~cmd_str in
    let%lwt result =
      wrap_ephemeral_handler handler ~genv ~request_id ~client_context ~workload ~cmd_str env
    in
    match result with
    | Ok ()
    | Error () ->
      Lwt.return_unit
  with Lwt.Canceled ->
    (* It's fine for parallelizable commands to be canceled - they'll be run again later *)
    Lwt.return_unit

let handle_nonparallelizable_ephemeral_unsafe ~genv ~request_id ~workload env =
  let should_print_summary = Options.should_profile genv.options in
  let%lwt (profiling, (env, json_data)) =
    Profiling_js.with_profiling_lwt ~label:"Command" ~should_print_summary (fun profiling ->
        let%lwt (env, response, json_data) =
          run_command_in_serial ~genv ~env ~profiling ~workload
        in
        MonitorRPC.respond_to_request ~request_id ~response;

        Lwt.return (env, json_data))
  in
  Lwt.return (env, profiling, json_data)

let handle_nonparallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str env =
  let%lwt result =
    wrap_ephemeral_handler
      handle_nonparallelizable_ephemeral_unsafe
      ~genv
      ~request_id
      ~client_context
      ~workload
      ~cmd_str
      env
  in
  match result with
  | Ok env -> Lwt.return env
  | Error () -> Lwt.return env

let enqueue_or_handle_ephemeral genv (request_id, command_with_context) =
  let { ServerProt.Request.client_logging_context = client_context; command } =
    command_with_context
  in
  let cmd_str = ServerProt.Request.to_string command in
  match get_ephemeral_handler genv command with
  | Handle_immediately workload ->
    let%lwt result =
      handle_ephemeral_immediately ~genv ~request_id ~client_context ~workload ~cmd_str ()
    in
    (match result with
    | Ok ()
    | Error () ->
      Lwt.return_unit)
  | Handle_parallelizable workload ->
    let workload =
      handle_parallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str
    in
    ServerMonitorListenerState.push_new_parallelizable_workload workload;
    Lwt.return_unit
  | Handle_nonparallelizable workload ->
    let workload =
      handle_nonparallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str
    in
    ServerMonitorListenerState.push_new_workload workload;
    Lwt.return_unit

let did_open ~reader genv env client (files : (string * string) Nel.t) : ServerEnv.env Lwt.t =
  let options = genv.ServerEnv.options in
  match Options.lazy_mode options with
  | Options.LAZY_MODE_IDE ->
    (* LAZY_MODE_IDE is a lazy mode which infers the focused files based on what the IDE
     * opens. So when an IDE opens a new file, that file is now focused.
     *
     * If the newly opened file was previously unchecked or checked as a dependency, then
     * we will do a new recheck.
     *
     * If the newly opened file was already checked, then we'll just send the errors to
     * the client
     *)
    let filenames = Nel.map (fun (fn, _content) -> fn) files in
    let%lwt (env, triggered_recheck) = Lazy_mode_utils.focus_and_check genv env filenames in
    ( if not triggered_recheck then
      (* This open doesn't trigger a recheck, but we'll still send down the errors *)
      let (errors, warnings, _) = ErrorCollator.get_with_separate_warnings ~reader ~options env in
      Persistent_connection.send_errors_if_subscribed
        ~client
        ~errors_reason:LspProt.Env_change
        ~errors
        ~warnings );
    Lwt.return env
  | Options.LAZY_MODE_FILESYSTEM
  | Options.LAZY_MODE_WATCHMAN
  | Options.NON_LAZY_MODE ->
    (* In filesystem lazy mode or in non-lazy mode, the only thing we need to do when
     * a new file is opened is to send the errors to the client *)
    let (errors, warnings, _) = ErrorCollator.get_with_separate_warnings ~reader ~options env in
    Persistent_connection.send_errors_if_subscribed
      ~client
      ~errors_reason:LspProt.Env_change
      ~errors
      ~warnings;
    Lwt.return env

let did_close ~reader genv env client : ServerEnv.env Lwt.t =
  let options = genv.options in
  let (errors, warnings, _) = ErrorCollator.get_with_separate_warnings ~reader ~options env in
  Persistent_connection.send_errors_if_subscribed
    ~client
    ~errors_reason:LspProt.Env_change
    ~errors
    ~warnings;
  Lwt.return env

let with_error ?(stack : Utils.callstack option) ~(reason : string) (metadata : LspProt.metadata) :
    LspProt.metadata =
  let open LspProt in
  let stack =
    match stack with
    | Some stack -> stack
    | None -> Utils.Callstack (Exception.get_current_callstack_string 100)
  in
  let error_info = Some (ExpectedError, reason, stack) in
  { metadata with error_info }

let keyvals_of_json (json : Hh_json.json option) : (string * Hh_json.json) list =
  match json with
  | None -> []
  | Some (Hh_json.JSON_Object keyvals) -> keyvals
  | Some json -> [("json_data", json)]

let with_data ~(extra_data : Hh_json.json option) (metadata : LspProt.metadata) : LspProt.metadata
    =
  LspProt.(
    let extra_data = metadata.extra_data @ keyvals_of_json extra_data in
    { metadata with extra_data })

type 'a persistent_handling_result = 'a * LspProt.response * LspProt.metadata

(* This is commonly called by persistent handlers when something goes wrong and we need to return
  * an error response *)
let mk_lsp_error_response ~ret ~id ~reason ?stack metadata =
  let metadata = with_error ?stack ~reason metadata in
  let (_, reason, Utils.Callstack stack) = Option.value_exn metadata.LspProt.error_info in
  let message =
    match id with
    | Some id ->
      let friendly_message =
        "Flow encountered an unexpected error while handling this request. "
        ^ "See the Flow logs for more details."
      in
      let e = Lsp_fmt.error_of_exn (Error.Unknown friendly_message) in
      ResponseMessage (id, ErrorResult (e, stack))
    | None ->
      let text = Printf.sprintf "%s [%i]\n%s" reason Error.Code.unknownErrorCode stack in
      NotificationMessage
        (TelemetryNotification { LogMessage.type_ = MessageType.ErrorMessage; message = text })
  in
  Lwt.return (ret, LspProt.LspFromServer (Some message), metadata)

let handle_persistent_canceled ~ret ~id ~metadata ~client:_ ~profiling:_ =
  let e = Lsp_fmt.error_of_exn (Error.RequestCancelled "cancelled") in
  let response = ResponseMessage (id, ErrorResult (e, "")) in
  let metadata = with_error metadata ~reason:"cancelled" in
  Lwt.return (ret, LspProt.LspFromServer (Some response), metadata)

let handle_persistent_subscribe ~reader ~options ~metadata ~client ~profiling:_ ~env =
  let (current_errors, current_warnings, _) =
    ErrorCollator.get_with_separate_warnings ~reader ~options env
  in
  Persistent_connection.subscribe_client ~client ~current_errors ~current_warnings;
  Lwt.return (env, LspProt.LspFromServer None, metadata)

(* A did_open notification can come in about N files, which is great. But sometimes we'll get
 * N did_open notifications in quick succession. Let's batch them up and run them all at once!
 *)
let (enqueue_did_open_files, handle_persistent_did_open_notification) =
  let pending = ref SMap.empty in
  let enqueue_did_open_files (files : (string * string) Nel.t) =
    (* Overwrite the older content with the newer content *)
    pending := Nel.fold_left (fun acc (fn, content) -> SMap.add fn content acc) !pending files
  in
  let get_and_clear_did_open_files () : (string * string) list =
    let ret = SMap.bindings !pending in
    pending := SMap.empty;
    ret
  in
  let handle_persistent_did_open_notification ~reader ~genv ~metadata ~client ~profiling:_ ~env =
    let%lwt env =
      match get_and_clear_did_open_files () with
      | [] -> Lwt.return env
      | first :: rest -> did_open ~reader genv env client (first, rest)
    in
    Lwt.return (env, LspProt.LspFromServer None, metadata)
  in
  (enqueue_did_open_files, handle_persistent_did_open_notification)

let handle_persistent_did_open_notification_no_op ~metadata ~client:_ ~profiling:_ =
  Lwt.return ((), LspProt.LspFromServer None, metadata)

let handle_persistent_did_change_notification ~params ~metadata ~client ~profiling:_ =
  Lsp.DidChange.(
    VersionedTextDocumentIdentifier.(
      Persistent_connection.(
        let fn = params.textDocument.uri |> Lsp_helpers.lsp_uri_to_path in
        match client_did_change client fn params.contentChanges with
        | Ok () -> Lwt.return ((), LspProt.LspFromServer None, metadata)
        | Error (reason, stack) -> mk_lsp_error_response ~ret:() ~id:None ~reason ~stack metadata)))

let handle_persistent_did_save_notification ~metadata ~client:_ ~profiling:_ =
  Lwt.return ((), LspProt.LspFromServer None, metadata)

let handle_persistent_did_close_notification ~reader ~genv ~metadata ~client ~profiling:_ ~env =
  let%lwt env = did_close ~reader genv env client in
  Lwt.return (env, LspProt.LspFromServer None, metadata)

let handle_persistent_did_close_notification_no_op ~metadata ~client:_ ~profiling:_ =
  Lwt.return ((), LspProt.LspFromServer None, metadata)

let handle_persistent_cancel_notification ~params ~metadata ~client:_ ~profiling:_ ~env =
  let id = params.CancelRequest.id in
  (* by the time this cancel request shows up in the queue, then it must already *)
  (* have had its effect if any on messages earlier in the queue, and so can be  *)
  (* removed. *)
  ServerMonitorListenerState.(cancellation_requests := IdSet.remove id !cancellation_requests);
  Lwt.return (env, LspProt.LspFromServer None, metadata)

let handle_persistent_get_def ~reader ~options ~id ~params ~loc ~metadata ~client ~profiling ~env =
  TextDocumentPositionParams.(
    let (file, line, char) =
      match loc with
      | Some loc -> loc
      | None ->
        (* We must have failed to get the client when we first tried. We could throw here, but this is
         * a little more defensive. The only danger here is that the file contents may have changed *)
        Flow_lsp_conversions.lsp_DocumentPosition_to_flow params client
    in
    let%lwt (result, extra_data) = get_def ~options ~reader ~env ~profiling (file, line, char) in
    let metadata = with_data ~extra_data metadata in
    match result with
    | Ok loc when loc = Loc.none ->
      let response = ResponseMessage (id, DefinitionResult []) in
      Lwt.return ((), LspProt.LspFromServer (Some response), metadata)
    | Ok loc ->
      let default_uri = params.textDocument.TextDocumentIdentifier.uri in
      let location = Flow_lsp_conversions.loc_to_lsp_with_default ~default_uri loc in
      let definition_location = { Lsp.DefinitionLocation.location; title = None } in
      let response = ResponseMessage (id, DefinitionResult [definition_location]) in
      Lwt.return ((), LspProt.LspFromServer (Some response), metadata)
    | Error reason -> mk_lsp_error_response ~ret:() ~id:(Some id) ~reason metadata)

let handle_persistent_infer_type ~options ~id ~params ~loc ~metadata ~client ~profiling ~env =
  TextDocumentPositionParams.(
    let (file, line, char) =
      match loc with
      | Some loc -> loc
      | None ->
        (* We must have failed to get the client when we first tried. We could throw here, but this is
         * a little more defensive. The only danger here is that the file contents may have changed *)
        Flow_lsp_conversions.lsp_DocumentPosition_to_flow params client
    in
    let verbose = None in
    (* if Some, would write to server logs *)
    let%lwt (result, extra_data) =
      infer_type ~options ~env ~profiling (file, line, char, verbose, false, false, false)
    in
    let metadata = with_data ~extra_data metadata in
    match result with
    | Ok (loc, content) ->
      (* loc may be the 'none' location; content may be None. *)
      (* If both are none then we'll return null; otherwise we'll return a hover *)
      let default_uri = params.textDocument.TextDocumentIdentifier.uri in
      let location = Flow_lsp_conversions.loc_to_lsp_with_default ~default_uri loc in
      let range =
        if loc = Loc.none then
          None
        else
          Some location.Lsp.Location.range
      in
      let contents =
        match content with
        | None -> [MarkedString "?"]
        | Some content -> [MarkedCode ("flow", Ty_printer.string_of_t content)]
      in
      let r =
        match (range, content) with
        | (None, None) -> None
        | (_, _) -> Some { Lsp.Hover.contents; range }
      in
      let response = ResponseMessage (id, HoverResult r) in
      Lwt.return ((), LspProt.LspFromServer (Some response), metadata)
    | Error reason -> mk_lsp_error_response ~ret:() ~id:(Some id) ~reason metadata)

let handle_persistent_code_action_request ~options ~id ~params ~metadata ~client ~profiling ~env =
  let%lwt result = find_code_actions ~options ~profiling ~env ~client ~params in
  match result with
  | Ok code_actions ->
    Lwt.return
      ( (),
        LspProt.LspFromServer (Some (ResponseMessage (id, CodeActionResult code_actions))),
        metadata )
  | Error reason -> mk_lsp_error_response ~ret:() ~id:(Some id) ~reason metadata

let handle_persistent_autocomplete_lsp
    ~reader ~options ~id ~params ~loc ~metadata ~client ~profiling ~env =
  let is_snippet_supported = Persistent_connection.client_snippet_support client in
  Completion.(
    let (file, line, char) =
      match loc with
      | Some loc -> loc
      | None ->
        (* We must have failed to get the client when we first tried. We could throw here, but this is
         * a little more defensive. The only danger here is that the file contents may have changed *)
        Flow_lsp_conversions.lsp_DocumentPosition_to_flow params.loc client
    in
    let trigger_character =
      Option.value_map
        ~f:(fun completionContext -> completionContext.triggerCharacter)
        ~default:None
        params.context
    in
    let fn_content =
      match file with
      | File_input.FileContent (fn, content) -> Ok (fn, content)
      | File_input.FileName fn ->
        (try Ok (Some fn, Sys_utils.cat fn)
         with e ->
           let e = Exception.wrap e in
           Error (Exception.get_ctor_string e, Utils.Callstack (Exception.get_backtrace_string e)))
    in
    match fn_content with
    | Error (reason, stack) -> mk_lsp_error_response ~ret:() ~id:(Some id) ~reason ~stack metadata
    | Ok (fn, content) ->
      let (content_with_token, broader_context) =
        AutocompleteService_js.add_autocomplete_token content line char
      in
      let file_with_token = File_input.FileContent (fn, content_with_token) in
      let%lwt (result, extra_data) =
        autocomplete
          ~trigger_character
          ~reader
          ~options
          ~env
          ~profiling
          ~broader_context
          file_with_token
      in
      let metadata = with_data ~extra_data metadata in
      begin
        match result with
        | Ok items ->
          let items =
            Base.List.map
              ~f:(Flow_lsp_conversions.flow_completion_to_lsp is_snippet_supported)
              items
          in
          let r = CompletionResult { Lsp.Completion.isIncomplete = false; items } in
          let response = ResponseMessage (id, r) in
          Lwt.return ((), LspProt.LspFromServer (Some response), metadata)
        | Error reason -> mk_lsp_error_response ~ret:() ~id:(Some id) ~reason metadata
      end)

let handle_persistent_document_highlight
    ~reader ~options ~id ~params ~metadata ~client ~profiling ~env =
  let (file, line, char) = Flow_lsp_conversions.lsp_DocumentPosition_to_flow params ~client in
  let%lwt result = find_local_refs ~reader ~options ~env ~profiling (file, line, char) in
  let extra_data =
    Some
      (Hh_json.JSON_Object
         [
           ( "result",
             Hh_json.JSON_String
               (match result with
               | Ok _ -> "SUCCESS"
               | _ -> "FAILURE") );
         ])
  in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Ok (Some (_name, locs)) ->
    (* All the locs are implicitly in the same file *)
    let loc_to_highlight loc =
      {
        DocumentHighlight.range = Flow_lsp_conversions.loc_to_lsp_range loc;
        kind = Some DocumentHighlight.Text;
      }
    in
    let r = DocumentHighlightResult (Base.List.map ~f:loc_to_highlight locs) in
    let response = ResponseMessage (id, r) in
    Lwt.return ((), LspProt.LspFromServer (Some response), metadata)
  | Ok None ->
    (* e.g. if it was requested on a place that's not even an identifier *)
    let r = DocumentHighlightResult [] in
    let response = ResponseMessage (id, r) in
    Lwt.return ((), LspProt.LspFromServer (Some response), metadata)
  | Error reason -> mk_lsp_error_response ~ret:() ~id:(Some id) ~reason metadata

let handle_persistent_coverage ~options ~id ~params ~file ~metadata ~client ~profiling ~env =
  let textDocument = params.TypeCoverage.textDocument in
  let file =
    match file with
    | Some file -> file
    | None ->
      (* We must have failed to get the client when we first tried. We could throw here, but this is
       * a little more defensive. The only danger here is that the file contents may have changed *)
      Flow_lsp_conversions.lsp_DocumentIdentifier_to_flow textDocument ~client
  in
  (* if it isn't a flow file (i.e. lacks a @flow directive) then we won't do anything *)
  let fkey = File_key.SourceFile (File_input.filename_of_file_input file) in
  let content = File_input.content_of_file_input file in
  let is_flow =
    match content with
    | Ok content ->
      let (_, docblock) = Parsing_service_js.(parse_docblock docblock_max_tokens fkey content) in
      Docblock.is_flow docblock
    | Error _ -> false
  in
  let%lwt result =
    if is_flow then
      let force = false in
      (* 'true' makes it report "unknown" for all exprs in non-flow files *)
      coverage ~options ~env ~profiling ~force ~trust:false file
    else
      Lwt.return (Ok [])
  in
  match (is_flow, result) with
  | (false, _) ->
    let range = { start = { line = 0; character = 0 }; end_ = { line = 1; character = 0 } } in
    let r =
      TypeCoverageResult
        {
          TypeCoverage.coveredPercent = 0;
          uncoveredRanges = [{ TypeCoverage.range; message = None }];
          defaultMessage = "Use @flow to get type coverage for this file";
        }
    in
    let response = ResponseMessage (id, r) in
    Lwt.return ((), LspProt.LspFromServer (Some response), metadata)
  | (true, Ok all_locs) ->
    (* Figure out the percentages *)
    let accum_coverage (covered, total) (_loc, cov) =
      let covered =
        match cov with
        | Coverage_response.Tainted
        | Coverage_response.Untainted ->
          covered + 1
        | Coverage_response.Uncovered
        | Coverage_response.Empty ->
          covered
      in
      (covered, total + 1)
    in
    let (covered, total) = Base.List.fold all_locs ~init:(0, 0) ~f:accum_coverage in
    let coveredPercent =
      if total = 0 then
        100
      else
        100 * covered / total
    in
    (* Figure out each individual uncovered span *)
    let uncovereds =
      Base.List.filter_map all_locs ~f:(fun (loc, cov) ->
          match cov with
          | Coverage_response.Tainted
          | Coverage_response.Untainted ->
            None
          | Coverage_response.Uncovered
          | Coverage_response.Empty ->
            Some loc)
    in
    (* Imagine a tree of uncovered spans based on range inclusion. *)
    (* This sorted list is a pre-order flattening of that tree. *)
    let sorted = Base.List.sort uncovereds ~compare:Loc.compare in
    (* We can use that sorted list to remove any span which contains another, so *)
    (* the user only sees actionable reports of the smallest causes of untypedness. *)
    (* The algorithm: accept a range if its immediate successor isn't contained by it. *)
    let f (candidate, acc) loc =
      if Loc.contains candidate loc then
        (loc, acc)
      else
        (loc, candidate :: acc)
    in
    let singles =
      match sorted with
      | [] -> []
      | first :: _ ->
        let (final_candidate, singles) = Base.List.fold sorted ~init:(first, []) ~f in
        final_candidate :: singles
    in
    (* Convert to LSP *)
    let loc_to_lsp loc =
      { TypeCoverage.range = Flow_lsp_conversions.loc_to_lsp_range loc; message = None }
    in
    let uncoveredRanges = Base.List.map singles ~f:loc_to_lsp in
    (* Send the results! *)
    let r =
      TypeCoverageResult
        {
          TypeCoverage.coveredPercent;
          uncoveredRanges;
          defaultMessage = "Un-type checked code. Consider adding type annotations.";
        }
    in
    let response = ResponseMessage (id, r) in
    Lwt.return ((), LspProt.LspFromServer (Some response), metadata)
  | (true, Error reason) -> mk_lsp_error_response ~ret:() ~id:(Some id) ~reason metadata

let handle_persistent_find_refs ~reader ~genv ~id ~params ~metadata ~client ~profiling ~env =
  FindReferences.(
    let { loc; context = { includeDeclaration = _; includeIndirectReferences = multi_hop } } =
      params
    in
    (* TODO: respect includeDeclaration *)
    let (file, line, char) = Flow_lsp_conversions.lsp_DocumentPosition_to_flow loc ~client in
    let%lwt (env, result, dep_count) =
      find_global_refs ~reader ~genv ~env ~profiling (file, line, char, multi_hop)
    in
    let extra_data =
      Some
        (Hh_json.JSON_Object
           ( ( "result",
               Hh_json.JSON_String
                 (match result with
                 | Ok _ -> "SUCCESS"
                 | _ -> "FAILURE") )
           :: ("global", Hh_json.JSON_Bool true)
           ::
           (match dep_count with
           | Some count -> [("deps", Hh_json.JSON_Number (string_of_int count))]
           | None -> []) ))
    in
    let metadata = with_data ~extra_data metadata in
    match result with
    | Ok (Some (_name, locs)) ->
      let lsp_locs =
        Base.List.fold locs ~init:(Ok []) ~f:(fun acc loc ->
            let location = Flow_lsp_conversions.loc_to_lsp loc in
            Base.Result.combine location acc ~ok:List.cons ~err:(fun e _ -> e))
      in
      begin
        match lsp_locs with
        | Ok lsp_locs ->
          let response = ResponseMessage (id, FindReferencesResult lsp_locs) in
          Lwt.return (env, LspProt.LspFromServer (Some response), metadata)
        | Error reason -> mk_lsp_error_response ~ret:env ~id:(Some id) ~reason metadata
      end
    | Ok None ->
      (* e.g. if it was requested on a place that's not even an identifier *)
      let r = FindReferencesResult [] in
      let response = ResponseMessage (id, r) in
      Lwt.return (env, LspProt.LspFromServer (Some response), metadata)
    | Error reason -> mk_lsp_error_response ~ret:env ~id:(Some id) ~reason metadata)

let handle_persistent_rename ~reader ~genv ~id ~params ~metadata ~client ~profiling ~env =
  let { Rename.textDocument; position; newName } = params in
  let file_input = Flow_lsp_conversions.lsp_DocumentIdentifier_to_flow textDocument ~client in
  let (line, col) = Flow_lsp_conversions.lsp_position_to_flow position in
  let env = ref env in
  let%lwt result =
    Refactor_service.rename ~reader ~genv ~env ~profiling ~file_input ~line ~col ~new_name:newName
  in
  let env = !env in
  let edits_to_response (edits : (Loc.t * string) list) =
    (* Extract the path from each edit and convert into a map from file to edits for that file *)
    let file_to_edits : ((Loc.t * string) list SMap.t, string) result =
      List.fold_left
        begin
          fun map edit ->
          map
          >>= fun map ->
          let (loc, _) = edit in
          let uri = Flow_lsp_conversions.file_key_to_uri Loc.(loc.source) in
          uri
          >>| fun uri ->
          let lst = Option.value ~default:[] (SMap.find_opt uri map) in
          (* This reverses the list *)
          SMap.add uri (edit :: lst) map
        end
        (Ok SMap.empty)
        edits
      (* Reverse the lists to restore the original order *)
      >>| SMap.map List.rev
    in
    (* Convert all of the edits to LSP edits *)
    let file_to_textedits : (TextEdit.t list SMap.t, string) result =
      file_to_edits >>| SMap.map (Base.List.map ~f:Flow_lsp_conversions.flow_edit_to_textedit)
    in
    let workspace_edit : (WorkspaceEdit.t, string) result =
      file_to_textedits >>| (fun file_to_textedits -> { WorkspaceEdit.changes = file_to_textedits })
    in
    match workspace_edit with
    | Ok x ->
      let response = ResponseMessage (id, RenameResult x) in
      Lwt.return (env, LspProt.LspFromServer (Some response), metadata)
    | Error reason -> mk_lsp_error_response ~ret:env ~id:(Some id) ~reason metadata
  in
  match result with
  | Ok (Some edits) -> edits_to_response edits
  | Ok None -> edits_to_response []
  | Error reason -> mk_lsp_error_response ~ret:env ~id:(Some id) ~reason metadata

let handle_persistent_rage ~reader ~genv ~id ~metadata ~client:_ ~profiling:_ ~env =
  let root = Path.to_string genv.ServerEnv.options.Options.opt_root in
  let items =
    collect_rage ~options:genv.ServerEnv.options ~reader ~env ~files:None
    |> List.map (fun (title, data) -> { Lsp.Rage.title = Some (root ^ ":" ^ title); data })
  in
  let response = ResponseMessage (id, RageResult items) in
  Lwt.return ((), LspProt.LspFromServer (Some response), metadata)

let handle_persistent_unsupported ?id ~unhandled ~metadata ~client:_ ~profiling:_ =
  let reason = Printf.sprintf "not implemented: %s" (Lsp_fmt.message_name_to_string unhandled) in
  mk_lsp_error_response ~ret:() ~id ~reason metadata

(* This tries to simulate the logic from elsewhere which determines whether we would report
 * errors for a given file. The criteria are
 *
 * 1) The file must be either implicitly included (be in the same dir structure as .flowconfig)
 *    or explicitly included
 * 2) The file must not be ignored
 * 3) The file path must be a Flow file (e.g foo.js and not foo.php or foo/)
 * 4) The file must either have `// @flow` or all=true must be set in the .flowconfig or CLI
 *)
let check_that_we_care_about_this_file =
  let check_file_not_ignored ~file_options ~env ~file_path () =
    if Files.wanted ~options:file_options env.ServerEnv.libs file_path then
      Ok ()
    else
      Error "File is ignored"
  in
  let check_file_included ~options ~file_options ~file_path () =
    let file_is_implicitly_included =
      let root_str = spf "%s%s" (Path.to_string (Options.root options)) Filename.dir_sep in
      String_utils.string_starts_with file_path root_str
    in
    if file_is_implicitly_included then
      Ok ()
    else if Files.is_included file_options file_path then
      Ok ()
    else
      Error "File is not implicitly or explicitly included"
  in
  let check_is_flow_file ~file_options ~file_path () =
    if Files.is_flow_file ~options:file_options file_path then
      Ok ()
    else
      Error "File is not a Flow file"
  in
  let check_flow_pragma ~options ~content ~file_path () =
    if Options.all options then
      Ok ()
    else
      let (_, docblock) =
        Parsing_service_js.(
          parse_docblock docblock_max_tokens (File_key.SourceFile file_path) content)
      in
      if Docblock.is_flow docblock then
        Ok ()
      else
        Error "File is missing @flow pragma and `all` is not set to `true`"
  in
  fun ~options ~env ~file_path ~content ->
    let file_path = Files.imaginary_realpath file_path in
    let file_options = Options.file_options options in
    Ok ()
    >>= check_file_not_ignored ~file_options ~env ~file_path
    >>= check_file_included ~options ~file_options ~file_path
    >>= check_is_flow_file ~file_options ~file_path
    >>= check_flow_pragma ~options ~content ~file_path

(* What should we do if we get multiple requests for the same URI? Each request wants the most
 * up-to-date live errors, so if we have 10 pending requests then we would want to send the same
 * response to each. And we could do that, but it might have some weird side effects:
 *
 * 1. Logging would make this look faster than it is, since we're doing a single check to respond
 *    to N requests
 * 2. The LSP process would have to integrate N responses into its store of errors
 *
 * So instead we just respond that the first N-1 requests were canceled and send a response to the
 * Nth request. Since it's very cheap to cancel a request, this shouldn't delay the LSP process
 * getting a response *)
let handle_live_errors_request =
  (* How do we know that we're responding to the latest request for a URI? Keep track of the
   * latest metadata object *)
  let uri_to_latest_metadata_map = ref SMap.empty in
  fun ~options ~uri ~metadata ->
    (* Immediately store the latest metadata *)
    uri_to_latest_metadata_map := SMap.add uri metadata !uri_to_latest_metadata_map;
    fun ~client ~profiling ~env ->
      let latest_metadata = SMap.find uri !uri_to_latest_metadata_map in
      if latest_metadata <> metadata then
        (* A more recent request for live errors has come in for this file. So let's cancel
         * this one and let the later one handle it *)
        Lwt.return
          ( (),
            LspProt.(
              LiveErrorsResponse
                (Error
                   {
                     live_errors_failure_kind = Canceled_error_response;
                     live_errors_failure_reason = "Subsumed by a later request";
                     live_errors_failure_uri = uri;
                   })),
            metadata )
      else
        (* This is the most recent live errors request we've received for this file. All the
         * older ones have already been responded to or canceled *)
        let file_path = Lsp_helpers.lsp_uri_to_path uri in
        let%lwt ret =
          match Persistent_connection.get_file client file_path with
          | File_input.FileName _ ->
            (* Maybe we've received a didClose for this file? Or maybe we got a request for a file
             * that wasn't open in the first place (that would be a bug). *)
            Lwt.return
              ( (),
                LspProt.(
                  LiveErrorsResponse
                    (Error
                       {
                         live_errors_failure_kind = Errored_error_response;
                         live_errors_failure_reason =
                           spf "Cannot get live errors for %s: File not open" file_path;
                         live_errors_failure_uri = uri;
                       })),
                metadata )
          | File_input.FileContent (_, content) ->
            let%lwt (live_errors, live_warnings) =
              match check_that_we_care_about_this_file ~options ~env ~file_path ~content with
              | Ok () ->
                let%lwt (_, live_errors, live_warnings) =
                  Types_js.typecheck_contents
                    ~options
                    ~env
                    ~profiling
                    content
                    (File_key.SourceFile file_path)
                in
                Lwt.return (live_errors, live_warnings)
              | Error reason ->
                Hh_logger.info "Not reporting live errors for file %S: %s" file_path reason;

                (* If the LSP requests errors for a file for which we wouldn't normally emit errors
                 * then just return empty sets *)
                Lwt.return
                  ( Errors.ConcreteLocPrintableErrorSet.empty,
                    Errors.ConcreteLocPrintableErrorSet.empty )
            in
            Lwt.return
              ( (),
                LspProt.LiveErrorsResponse
                  (Ok { LspProt.live_errors; live_warnings; live_errors_uri = uri }),
                metadata )
        in
        (* If we've successfully run and there isn't a more recent request for this URI,
         * then remove the entry from the map *)
        (match SMap.find_opt uri !uri_to_latest_metadata_map with
        | Some latest_metadata when latest_metadata = metadata ->
          uri_to_latest_metadata_map := SMap.remove uri !uri_to_latest_metadata_map
        | _ -> ());
        Lwt.return ret

type persistent_command_handler =
  (* A command can be handled immediately if it is super duper fast and doesn't require the env.
   * These commands will be handled as soon as we read them off the pipe. Almost nothing should ever
   * be handled immediately *)
  | Handle_persistent_immediately of
      (client:Persistent_connection.single_client ->
      profiling:Profiling_js.running ->
      unit persistent_handling_result Lwt.t)
  (* A command is parallelizable if it passes four conditions
   *
   * 1. It is fast. Running it in parallel will block the current recheck, so it needs to be really
   *    fast.
   * 2. It doesn't use the workers. Currently we can't deal with the recheck using the workers at the
   *    same time as a command using the workers
   * 3. It doesn't return a new env. It really should be just a read-only job
   * 4. It doesn't mind using slightly out of date data. During a recheck, it will be reading the
   *    oldified data
   *)
  | Handle_parallelizable_persistent of
      (client:Persistent_connection.single_client ->
      profiling:Profiling_js.running ->
      env:ServerEnv.env ->
      unit persistent_handling_result Lwt.t)
  (* A command is nonparallelizable if it can't be handled immediately or parallelized. *)
  | Handle_nonparallelizable_persistent of
      (client:Persistent_connection.single_client ->
      profiling:Profiling_js.running ->
      env:ServerEnv.env ->
      ServerEnv.env persistent_handling_result Lwt.t)

(* This command is parallelizable, but we will treat it as nonparallelizable if we've been told
 * to wait_for_recheck by the .flowconfig *)
let mk_parallelizable_persistent ~options f =
  let wait_for_recheck = Options.wait_for_recheck options in
  if wait_for_recheck then
    Handle_nonparallelizable_persistent
      (fun ~client ~profiling ~env ->
        let%lwt ((), msg, metadata) = f ~client ~profiling ~env in
        Lwt.return (env, msg, metadata))
  else
    Handle_parallelizable_persistent f

(* get_persistent_handler can do a tiny little bit of work, but it's main job is just returning the
 * persistent command's handler.
 *)
let get_persistent_handler ~genv ~client_id ~request : persistent_command_handler =
  LspProt.(
    let options = genv.ServerEnv.options in
    let reader = State_reader.create () in
    match request with
    | (LspToServer (RequestMessage (id, _)), metadata)
      when IdSet.mem id !ServerMonitorListenerState.cancellation_requests ->
      (* We don't do any work, we just immediately tell the monitor that this request was already
       * canceled *)
      Handle_persistent_immediately (handle_persistent_canceled ~ret:() ~id ~metadata)
    | (Subscribe, metadata) ->
      (* This mutates env, so it can't run in parallel *)
      Handle_nonparallelizable_persistent (handle_persistent_subscribe ~reader ~options ~metadata)
    | (LspToServer (NotificationMessage (DidOpenNotification params)), metadata) ->
      Lsp.DidOpen.(
        TextDocumentItem.(
          let content = params.textDocument.text in
          let fn = params.textDocument.uri |> Lsp_helpers.lsp_uri_to_path in
          let files = Nel.one (fn, content) in
          let did_anything_change =
            match Persistent_connection.get_client client_id with
            | None -> false
            | Some client ->
              (* We want to create a local copy of this file immediately, so we can respond to requests
               * about this file *)
              Persistent_connection.client_did_open client ~files
          in
          if did_anything_change then (
            enqueue_did_open_files files;

            (* This mutates env, so it can't run in parallel *)
            Handle_nonparallelizable_persistent
              (handle_persistent_did_open_notification ~reader ~genv ~metadata)
          ) else
            (* It's a no-op, so we can respond immediately *)
            Handle_persistent_immediately (handle_persistent_did_open_notification_no_op ~metadata)))
    | (LspToServer (NotificationMessage (DidChangeNotification params)), metadata) ->
      (* This just updates our copy of the file in question. We want to do this immediately *)
      Handle_persistent_immediately (handle_persistent_did_change_notification ~params ~metadata)
    | (LspToServer (NotificationMessage (DidSaveNotification _params)), metadata) ->
      (* No-op can be handled immediately *)
      Handle_persistent_immediately (handle_persistent_did_save_notification ~metadata)
    | (LspToServer (NotificationMessage (DidCloseNotification params)), metadata) ->
      Lsp.DidClose.(
        TextDocumentIdentifier.(
          let fn = params.textDocument.uri |> Lsp_helpers.lsp_uri_to_path in
          let filenames = Nel.one fn in
          let did_anything_change =
            match Persistent_connection.get_client client_id with
            | None -> false
            | Some client ->
              (* Close this file immediately in case another didOpen comes soon *)
              Persistent_connection.client_did_close client ~filenames
          in
          if did_anything_change then
            (* This mutates env, so it can't run in parallel *)
            Handle_nonparallelizable_persistent
              (handle_persistent_did_close_notification ~reader ~genv ~metadata)
          else
            (* It's a no-op, so we can respond immediately *)
            Handle_persistent_immediately
              (handle_persistent_did_close_notification_no_op ~metadata)))
    | (LspToServer (NotificationMessage (CancelRequestNotification params)), metadata) ->
      (* The general idea here is this:
       *
       * 1. As soon as we get a cancel notification, add the ID to the canceled requests set.
       * 2. When a request comes in or runs with the canceled ID, cancel that request and immediately
       *    respond that the request has been canceled.
       * 3. When we go to run a request that has been canceled, skip it's normal handler and instead
       *    respond that the request has been canceled.
       * 4. When the nonparallelizable cancel notification workload finally runs, remove the ID from
       *    the set. We're guaranteed that the canceled request will not show up later *)
      let id = params.CancelRequest.id in
      ServerMonitorListenerState.(cancellation_requests := IdSet.add id !cancellation_requests);
      Handle_nonparallelizable_persistent (handle_persistent_cancel_notification ~params ~metadata)
    | (LspToServer (RequestMessage (id, DefinitionRequest params)), metadata) ->
      (* Grab the file contents immediately in case of any future didChanges *)
      let loc =
        Option.map (Persistent_connection.get_client client_id) ~f:(fun client ->
            Flow_lsp_conversions.lsp_DocumentPosition_to_flow params ~client)
      in
      mk_parallelizable_persistent
        ~options
        (handle_persistent_get_def ~reader ~options ~id ~params ~loc ~metadata)
    | (LspToServer (RequestMessage (id, HoverRequest params)), metadata) ->
      (* Grab the file contents immediately in case of any future didChanges *)
      let loc =
        Option.map (Persistent_connection.get_client client_id) ~f:(fun client ->
            Flow_lsp_conversions.lsp_DocumentPosition_to_flow params ~client)
      in
      mk_parallelizable_persistent
        ~options
        (handle_persistent_infer_type ~options ~id ~params ~loc ~metadata)
    | (LspToServer (RequestMessage (id, CodeActionRequest params)), metadata) ->
      mk_parallelizable_persistent
        ~options
        (handle_persistent_code_action_request ~options ~id ~params ~metadata)
    | (LspToServer (RequestMessage (id, CompletionRequest params)), metadata) ->
      (* Grab the file contents immediately in case of any future didChanges *)
      let loc = params.Completion.loc in
      let loc =
        Option.map (Persistent_connection.get_client client_id) ~f:(fun client ->
            Flow_lsp_conversions.lsp_DocumentPosition_to_flow loc ~client)
      in
      mk_parallelizable_persistent
        ~options
        (handle_persistent_autocomplete_lsp ~reader ~options ~id ~params ~loc ~metadata)
    | (LspToServer (RequestMessage (id, DocumentHighlightRequest params)), metadata) ->
      mk_parallelizable_persistent
        ~options
        (handle_persistent_document_highlight ~reader ~options ~id ~params ~metadata)
    | (LspToServer (RequestMessage (id, TypeCoverageRequest params)), metadata) ->
      (* Grab the file contents immediately in case of any future didChanges *)
      let textDocument = params.TypeCoverage.textDocument in
      let file =
        Option.map (Persistent_connection.get_client client_id) ~f:(fun client ->
            Flow_lsp_conversions.lsp_DocumentIdentifier_to_flow textDocument ~client)
      in
      mk_parallelizable_persistent
        ~options
        (handle_persistent_coverage ~options ~id ~params ~file ~metadata)
    | (LspToServer (RequestMessage (id, FindReferencesRequest params)), metadata) ->
      (* Like `flow find-refs`, this is kind of slow and mutates env, so it can't run in parallel *)
      Handle_nonparallelizable_persistent
        (handle_persistent_find_refs ~reader ~genv ~id ~params ~metadata)
    | (LspToServer (RequestMessage (id, RenameRequest params)), metadata) ->
      (* rename delegates to find-refs, which can be kind of slow and might mutate the env, so it
       * can't run in parallel *)
      Handle_nonparallelizable_persistent
        (handle_persistent_rename ~reader ~genv ~id ~params ~metadata)
    | (LspToServer (RequestMessage (id, RageRequest)), metadata) ->
      (* Whoever is waiting for the rage results probably doesn't want to wait for a recheck *)
      mk_parallelizable_persistent ~options (handle_persistent_rage ~reader ~genv ~id ~metadata)
    | (LspToServer unhandled, metadata) ->
      let id =
        match unhandled with
        | RequestMessage (id, _) -> Some id
        | _ -> None
      in
      (* We can reject unsupported stuff immediately *)
      Handle_persistent_immediately (handle_persistent_unsupported ?id ~unhandled ~metadata)
    | (LiveErrorsRequest uri, metadata) ->
      (* We can handle live errors even during a recheck *)
      mk_parallelizable_persistent ~options (handle_live_errors_request ~options ~uri ~metadata))

let wrap_persistent_handler
    (type a b c)
    (handler :
      genv:ServerEnv.genv ->
      workload:a ->
      client:Persistent_connection.single_client ->
      profiling:Profiling_js.running ->
      b ->
      c persistent_handling_result Lwt.t)
    ~(genv : ServerEnv.genv)
    ~(client_id : LspProt.client_id)
    ~(request : LspProt.request_with_metadata)
    ~(workload : a)
    ~(default_ret : c)
    (arg : b) : c Lwt.t =
  LspProt.(
    let (request, metadata) = request in
    match Persistent_connection.get_client client_id with
    | None ->
      Hh_logger.error "Unknown persistent client %d. Maybe connection went away?" client_id;
      Lwt.return default_ret
    | Some client ->
      Hh_logger.info "Persistent request: %s" (string_of_request request);
      MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;

      let should_print_summary = Options.should_profile genv.options in
      let%lwt (profiling, result) =
        Profiling_js.with_profiling_lwt ~label:"Command" ~should_print_summary (fun profiling ->
            match request with
            | LspToServer (RequestMessage (id, _))
              when IdSet.mem id !ServerMonitorListenerState.cancellation_requests ->
              Hh_logger.info "Skipping canceled persistent request: %s" (string_of_request request);

              (* We can't actually skip a canceled request...we need to send a response. But we can
               * skip the normal handler *)
              handle_persistent_canceled ~ret:default_ret ~id ~metadata ~client ~profiling
            | _ ->
              (try%lwt handler ~genv ~workload ~client ~profiling arg with
              | Lwt.Canceled as e ->
                (* Don't swallow Lwt.Canceled. Parallelizable commands may be canceled and run again
                 * later. *)
                let e = Exception.wrap e in
                Exception.reraise e
              | e ->
                let e = Exception.wrap e in
                let exception_constructor = Exception.get_ctor_string e in
                let stack = Exception.get_backtrace_string e in
                Lwt.return
                  ( default_ret,
                    LspProt.UncaughtException { request; exception_constructor; stack },
                    metadata )))
      in
      (* we'll send this "Finishing_up" event only after sending the LSP response *)
      let event =
        ServerStatus.(
          Finishing_up
            {
              duration = Profiling_js.get_profiling_duration profiling;
              info = CommandSummary (string_of_request request);
            })
      in
      let server_profiling = Some profiling in
      let server_logging_context = Some (FlowEventLogger.get_context ()) in
      let (ret, lsp_response, metadata) = result in
      let metadata = { metadata with server_profiling; server_logging_context } in
      let response = (lsp_response, metadata) in
      Persistent_connection.send_response response client;
      Hh_logger.info "Persistent response: %s" (LspProt.string_of_response lsp_response);
      MonitorRPC.status_update ~event;
      Lwt.return ret)

let handle_persistent_immediately_unsafe ~genv:_ ~workload ~client ~profiling () =
  workload ~client ~profiling

let handle_persistent_immediately ~genv ~client_id ~request ~workload =
  wrap_persistent_handler
    handle_persistent_immediately_unsafe
    ~genv
    ~client_id
    ~request
    ~workload
    ~default_ret:()
    ()

let rec handle_parallelizable_persistent_unsafe ~request ~genv ~workload ~client ~profiling env :
    unit persistent_handling_result Lwt.t =
  let mk_workload () =
    let client_id = Persistent_connection.get_id client in
    handle_parallelizable_persistent ~genv ~client_id ~request ~workload
  in
  let workload = workload ~client in
  run_command_in_parallel ~env ~profiling ~workload ~mk_workload

and handle_parallelizable_persistent ~genv ~client_id ~request ~workload env : unit Lwt.t =
  try%lwt
    wrap_persistent_handler
      (handle_parallelizable_persistent_unsafe ~request)
      ~genv
      ~client_id
      ~request
      ~workload
      ~default_ret:()
      env
  with Lwt.Canceled ->
    (* It's fine for parallelizable commands to be canceled - they'll be run again later *)
    Lwt.return_unit

let handle_nonparallelizable_persistent_unsafe ~genv ~workload ~client ~profiling env =
  let workload = workload ~client in
  run_command_in_serial ~genv ~env ~profiling ~workload

let handle_nonparallelizable_persistent ~genv ~client_id ~request ~workload env =
  wrap_persistent_handler
    handle_nonparallelizable_persistent_unsafe
    ~genv
    ~client_id
    ~request
    ~workload
    ~default_ret:env
    env

let enqueue_persistent
    (genv : ServerEnv.genv)
    (client_id : LspProt.client_id)
    (request : LspProt.request_with_metadata) : unit Lwt.t =
  match get_persistent_handler ~genv ~client_id ~request with
  | Handle_persistent_immediately workload ->
    handle_persistent_immediately ~genv ~client_id ~request ~workload
  | Handle_parallelizable_persistent workload ->
    let workload = handle_parallelizable_persistent ~genv ~client_id ~request ~workload in
    ServerMonitorListenerState.push_new_parallelizable_workload workload;
    Lwt.return_unit
  | Handle_nonparallelizable_persistent workload ->
    let workload = handle_nonparallelizable_persistent ~genv ~client_id ~request ~workload in
    ServerMonitorListenerState.push_new_workload workload;
    Lwt.return_unit
