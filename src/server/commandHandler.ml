(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result
open ServerEnv
open Utils_js

let status_log errors =
  if Errors.ErrorSet.is_empty errors
    then Hh_logger.info "Status: OK"
    else Hh_logger.info "Status: Error";
  flush stdout

let convert_errors ~errors ~warnings =
  if Errors.ErrorSet.is_empty errors && Errors.ErrorSet.is_empty warnings then
    ServerProt.Response.NO_ERRORS
  else
    ServerProt.Response.ERRORS {errors; warnings}

let get_status genv env client_root =
  let server_root = Options.root genv.options in
  if server_root <> client_root then begin
    ServerProt.Response.DIRECTORY_MISMATCH {
      ServerProt.Response.server=server_root;
      ServerProt.Response.client=client_root
    }
  end else begin
    (* collate errors by origin *)
    let errors, warnings, _ = ErrorCollator.get env in
    let warnings = if Options.should_include_warnings genv.options
      then warnings
      else Errors.ErrorSet.empty
    in

    (* TODO: check status.directory *)
    status_log errors;
    FlowEventLogger.status_response
      ~num_errors:(Errors.ErrorSet.cardinal errors);
    convert_errors errors warnings
  end

let autocomplete ~options ~workers ~env command_context file_input =
  let path, content = match file_input with
    | File_input.FileName _ -> failwith "Not implemented"
    | File_input.FileContent (_, content) ->
        File_input.filename_of_file_input file_input, content
  in
  let state = Autocomplete_js.autocomplete_set_hooks () in
  let results =
    let path = File_key.SourceFile path in
    Types_js.basic_check_contents ~options ~workers ~env content path >>= fun (profiling, cx, info) ->
    try_with begin fun () ->
      AutocompleteService_js.autocomplete_get_results
        profiling
        command_context
        cx
        state
        info
    end in
  Autocomplete_js.autocomplete_unset_hooks ();
  results

let check_file ~options ~workers ~env ~force file_input =
  let file = File_input.filename_of_file_input file_input in
  match file_input with
  | File_input.FileName _ -> failwith "Not implemented"
  | File_input.FileContent (_, content) ->
      let should_check =
        if force then
          true
        else
          let (_, docblock) = Parsing_service_js.(
            get_docblock docblock_max_tokens (File_key.SourceFile file) content)
          in
          Docblock.is_flow docblock
      in
      if should_check then
        let file = File_key.SourceFile file in
        let errors, warnings = Types_js.typecheck_contents ~options ~workers ~env content file in
        convert_errors ~errors ~warnings
      else
        ServerProt.Response.NOT_COVERED

let infer_type
    ~options
    ~workers
    ~env
    client_context
    (file_input, line, col, verbose) =
  let file = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile file in
  File_input.content_of_file_input file_input >>= fun content ->
  let options = { options with Options.opt_verbose = verbose } in
  try_with begin fun () ->
    Type_info_service.type_at_pos
      ~options ~workers ~env ~client_context
      file content line col
  end

let dump_types ~options ~workers ~env file_input =
  let file = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile file in
  File_input.content_of_file_input file_input >>= fun content ->
  try_with begin fun () ->
    Type_info_service.dump_types
      ~options ~workers ~env file content
  end

let coverage ~options ~workers ~env ~force file_input =
  let file = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile file in
  File_input.content_of_file_input file_input >>= fun content ->
  try_with begin fun () ->
    Type_info_service.coverage ~options ~workers ~env ~force file content
  end

let get_cycle ~workers ~env fn =
  (* Re-calculate SCC *)
  let parsed = FilenameSet.elements !env.ServerEnv.files in
  let dependency_graph = Dep_service.calc_dependency_graph workers parsed in
  let partition = Sort_js.topsort dependency_graph in
  let component_map = Sort_js.component_map partition in

  (* Get component for target file *)
  let leader_map =
    FilenameMap.fold (fun file component acc ->
      List.fold_left (fun acc file_ ->
        FilenameMap.add file_ file acc
      ) acc component
    ) component_map FilenameMap.empty
  in
  let leader = FilenameMap.find_unsafe fn leader_map in
  let component = FilenameMap.find_unsafe leader component_map in

  (* Restrict dep graph to only in-cycle files *)
  let subgraph = List.fold_left (fun acc f ->
    Option.fold (FilenameMap.get f dependency_graph) ~init:acc ~f:(fun acc deps ->
      let subdeps = FilenameSet.filter (fun f -> List.mem f component) deps in
      if FilenameSet.is_empty subdeps
      then acc
      else FilenameMap.add f subdeps acc
    )
  ) FilenameMap.empty component in

  (* Convert from map/set to lists for serialization to client. *)
  let subgraph = FilenameMap.fold (fun f dep_fs acc ->
    let f = File_key.to_string f in
    let dep_fs = FilenameSet.fold (fun dep_f acc ->
      (File_key.to_string dep_f)::acc
    ) dep_fs [] in
    (f, dep_fs)::acc
  ) subgraph [] in

  Ok subgraph

let suggest =
  let suggest_for_file ~options ~workers ~env result_map (file, region) =
    SMap.add file (try_with begin fun () ->
      Type_info_service.suggest ~options ~workers ~env
        (File_key.SourceFile file) region (Sys_utils.cat file)
    end) result_map
  in fun ~options ~workers ~env files ->
    List.fold_left (suggest_for_file ~options ~workers ~env) SMap.empty files

(* NOTE: currently, not only returns list of annotations, but also writes a
   timestamped file with annotations *)
let port = Port_service_js.port_files

let find_module ~options (moduleref, filename) =
  let file = File_key.SourceFile filename in
  let metadata =
    let open Context in
    let metadata = metadata_of_options options in
    let local_metadata = { metadata.local_metadata with checked = false } in
    { metadata with local_metadata }
  in
  let cx = Context.make metadata file (Files.module_ref file) in
  let loc = {Loc.none with Loc.source = Some file;} in
  let module_name = Module_js.imported_module
    ~options ~node_modules_containers:!Files.node_modules_containers
    (Context.file cx) (Nel.one loc) moduleref in
  Module_js.get_file ~audit:Expensive.warn module_name

let gen_flow_files ~options env files =
  let errors, warnings, _ = ErrorCollator.get env in
  let warnings = if Options.should_include_warnings options
    then warnings
    else Errors.ErrorSet.empty
  in
  let result = if Errors.ErrorSet.is_empty errors
    then begin
      let (flow_files, non_flow_files, error) =
        List.fold_left (fun (flow_files, non_flow_files, error) file ->
          if error <> None then (flow_files, non_flow_files, error) else
          match file with
          | File_input.FileContent _ ->
            let error_msg = "This command only works with file paths." in
            let error =
              Some (ServerProt.Response.GenFlowFiles_UnexpectedError error_msg)
            in
            (flow_files, non_flow_files, error)
          | File_input.FileName fn ->
            let file = File_key.SourceFile fn in
            let checked =
              let open Module_js in
              match get_info file ~audit:Expensive.warn with
              | Some info -> info.checked
              | None -> false
            in
            if checked
            then file::flow_files, non_flow_files, error
            else flow_files, file::non_flow_files, error
        ) ([], [], None) files
      in
      begin match error with
      | Some e -> Error e
      | None ->
        try
          let flow_file_cxs = List.map (fun file ->
            let { Merge_service.cx; _ } = Merge_service.merge_strict_context ~options [file] in
            cx
          ) flow_files in

          (* Non-@flow files *)
          let result_contents = non_flow_files |> List.map (fun file ->
            (File_key.to_string file, ServerProt.Response.GenFlowFiles_NonFlowFile)
          ) in

          (* Codegen @flow files *)
          let result_contents = List.fold_left2 (fun results file cx ->
            let file_path = File_key.to_string file in
            try
              let code = FlowFileGen.flow_file cx in
              (file_path, ServerProt.Response.GenFlowFiles_FlowFile code)::results
            with exn ->
              failwith (spf "%s: %s" file_path (Printexc.to_string exn))
          ) result_contents flow_files flow_file_cxs in

          Ok result_contents
        with exn -> Error (
          ServerProt.Response.GenFlowFiles_UnexpectedError (Printexc.to_string exn)
        )
      end
    end else
      Error (ServerProt.Response.GenFlowFiles_TypecheckError {errors; warnings})
  in
  result

let find_refs ~genv ~env (file_input, line, col, global) =
  FindRefs_js.find_refs ~genv ~env ~file_input ~line ~col ~global

let get_def ~options ~workers ~env command_context (file_input, line, col) =
  let filename = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile filename in
  let loc = Loc.make file line col in
  let state = GetDef_js.getdef_set_hooks loc in
  let result =
    File_input.content_of_file_input file_input >>= fun content ->
    Types_js.basic_check_contents ~options ~workers ~env content file >>= fun (profiling, cx, _info) ->
    try_with begin fun () ->
      GetDef_js.getdef_get_result
        profiling
        command_context
        ~options
        cx
        state
    end in
  GetDef_js.getdef_unset_hooks ();
  result

let module_name_of_string ~options module_name_str =
  let file_options = Options.file_options options in
  let path = Path.to_string (Path.make module_name_str) in
  if Files.is_flow_file ~options:file_options path
  then Modulename.Filename (File_key.SourceFile path)
  else Modulename.String module_name_str

let get_imports ~options module_names =
  let add_to_results (map, non_flow) module_name_str =
    let module_name = module_name_of_string ~options module_name_str in
    match Module_js.get_file ~audit:Expensive.warn module_name with
    | Some file ->
      (* We do not process all modules which are stored in our module
       * database. In case we do not process a module its requirements
       * are not kept track of. To avoid confusing results we notify the
       * client that these modules have not been processed.
       *)
      let { Module_js.checked; _ } =
        Module_js.get_info_unsafe ~audit:Expensive.warn file in
      if checked then
        let { Module_js.resolved_modules; _ } =
          Module_js.get_resolved_requires_unsafe ~audit:Expensive.warn file in
        let fsig = Parsing_service_js.get_file_sig_unsafe file in
        let requires = File_sig.(require_loc_map fsig.module_sig) in
        let mlocs = SMap.fold (fun mref locs acc ->
          let m = SMap.find_unsafe mref resolved_modules in
          Modulename.Map.add m locs acc
        ) requires Modulename.Map.empty in
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


let handle_ephemeral_unsafe
  genv env (request_id, { ServerProt.Request.client_logging_context; command; }) =
  let env = ref env in
  let respond msg =
    MonitorRPC.respond_to_request ~request_id ~response:msg

  in
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in
  Hh_logger.debug "Request: %s" (ServerProt.Request.to_string command);
  MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;
  begin match command with
  | ServerProt.Request.AUTOCOMPLETE fn ->
      ServerProt.Response.AUTOCOMPLETE (
        autocomplete ~options ~workers ~env client_logging_context fn
      ) |> respond
  | ServerProt.Request.CHECK_FILE (fn, verbose, force, include_warnings) ->
      let options = { options with Options.
        opt_verbose = verbose;
        opt_include_warnings = options.Options.opt_include_warnings || include_warnings;
      } in
      ServerProt.Response.CHECK_FILE (
        check_file ~options ~workers ~env ~force fn: ServerProt.Response.status_response
      ) |> respond
  | ServerProt.Request.COVERAGE (fn, force) ->
      ServerProt.Response.COVERAGE (
        coverage ~options ~workers ~env ~force fn: ServerProt.Response.coverage_response
      ) |> respond
  | ServerProt.Request.CYCLE fn ->
      let file_options = Options.file_options options in
      let fn = Files.filename_from_string ~options:file_options fn in
      ServerProt.Response.CYCLE (
        get_cycle ~workers ~env fn: ServerProt.Response.cycle_response
      ) |> respond
  | ServerProt.Request.DUMP_TYPES (fn) ->
      ServerProt.Response.DUMP_TYPES (dump_types ~options ~workers ~env fn)
      |> respond
  | ServerProt.Request.FIND_MODULE (moduleref, filename) ->
      ServerProt.Response.FIND_MODULE (
        find_module ~options (moduleref, filename): File_key.t option
      ) |> respond
  | ServerProt.Request.FIND_REFS (fn, line, char, global) ->
      ServerProt.Response.FIND_REFS (
        find_refs
          ~genv ~env (fn, line, char, global): ServerProt.Response.find_refs_response
      ) |> respond
  | ServerProt.Request.FORCE_RECHECK (files, force_focus) ->
      respond ServerProt.Response.FORCE_RECHECK;
      let updates = Rechecker.process_updates genv !env (SSet.of_list files) in
      env := Rechecker.recheck genv !env ~force_focus updates
  | ServerProt.Request.GEN_FLOW_FILES (files, include_warnings) ->
      let options = { options with Options.
        opt_include_warnings = options.Options.opt_include_warnings || include_warnings;
      } in
      ServerProt.Response.GEN_FLOW_FILES (
        gen_flow_files ~options !env files: ServerProt.Response.gen_flow_files_response
      ) |> respond
  | ServerProt.Request.GET_DEF (fn, line, char) ->
      ServerProt.Response.GET_DEF (
        get_def ~options ~workers ~env client_logging_context (fn, line, char)
      ) |> respond
  | ServerProt.Request.GET_IMPORTS module_names ->
      ServerProt.Response.GET_IMPORTS (
        get_imports ~options module_names: ServerProt.Response.get_imports_response
      ) |> respond
  | ServerProt.Request.INFER_TYPE (fn, line, char, verbose) ->
      ServerProt.Response.INFER_TYPE (
        infer_type
          ~options ~workers ~env
          client_logging_context
          (fn, line, char, verbose)
      ) |> respond
  | ServerProt.Request.PORT (files) ->
      ServerProt.Response.PORT (port files: ServerProt.Response.port_response)
      |> respond
  | ServerProt.Request.STATUS (client_root, include_warnings) ->
      let genv = {genv with
        options = let open Options in {genv.options with
          opt_include_warnings = genv.options.opt_include_warnings || include_warnings
        }
      } in
      let status = get_status genv !env client_root in
      respond (ServerProt.Response.STATUS status);
      begin match status with
        | ServerProt.Response.DIRECTORY_MISMATCH {ServerProt.Response.server; client} ->
            Hh_logger.fatal "Status: Error";
            Hh_logger.fatal "server_dir=%s, client_dir=%s"
              (Path.to_string server)
              (Path.to_string client);
            Hh_logger.fatal "flow server is not listening to the same directory. Exiting.";
            FlowExitStatus.(exit Server_client_directory_mismatch)
        | _ -> ()
      end
  | ServerProt.Request.SUGGEST (files) ->
      ServerProt.Response.SUGGEST (
        suggest ~options ~workers ~env files: ServerProt.Response.suggest_response
      ) |> respond
  end;
  MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
  !env

let handle_ephemeral genv env (request_id, command) =
  try handle_ephemeral_unsafe genv env (request_id, command)
  with exn ->
    let backtrace = String.trim (Printexc.get_backtrace ()) in
    let exn_str = Printf.sprintf
      "%s%s%s"
      (Printexc.to_string exn)
      (if backtrace = "" then "" else "\n")
      backtrace in
    Hh_logger.error
      "Uncaught exception while handling a request (%s): %s"
      (ServerProt.Request.to_string command.ServerProt.Request.command)
      exn_str;
    MonitorRPC.request_failed ~request_id ~exn_str;
    env

let handle_persistent genv env client_id msg =
  let env = ref env in
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in
  Hh_logger.debug "Persistent request: %s" (Persistent_connection_prot.string_of_request msg);
  MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;
  let client = Persistent_connection.get_client !env.connections client_id in
  let env = match msg with
    | Persistent_connection_prot.Subscribe ->
        let current_errors, current_warnings, _ = ErrorCollator.get_with_separate_warnings !env in
        let new_connections = Persistent_connection.subscribe_client
          ~clients:!env.connections ~client ~current_errors ~current_warnings
        in
        { !env with connections = new_connections }
    | Persistent_connection_prot.Autocomplete (file_input, id) ->
        let client_logging_context = Persistent_connection.get_logging_context client in
        let results = autocomplete ~options ~workers ~env client_logging_context file_input in
        let wrapped = Persistent_connection_prot.AutocompleteResult (results, id) in
        Persistent_connection.send_message wrapped client;
        !env
    | Persistent_connection_prot.DidOpen filenames ->
        Persistent_connection.send_message Persistent_connection_prot.DidOpenAck client;

        begin match Persistent_connection.client_did_open !env.connections client ~filenames with
        | None -> !env (* No new files were opened, so do nothing *)
        | Some (connections, client) ->
          let env = {!env with connections} in

          match Options.lazy_mode options with
          | Some Options.LAZY_MODE_IDE ->
            (* LAZY_MODE_IDE is a lazy mode which infers the focused files based on what the IDE
             * opens. So when an IDE opens a new file, that file is now focused.
             *
             * If the newly opened file was previously unchecked or checked as a dependency, then
             * we will do a new recheck.
             *
             * If the newly opened file was already checked, then we'll just send the errors to
             * the client
             *)
            let env, triggered_recheck = Lazy_mode_utils.focus_and_check genv env filenames in
            if not triggered_recheck then begin
              (* This open doesn't trigger a recheck, but we'll still send down the errors *)
              let errors, warnings, _ = ErrorCollator.get_with_separate_warnings env in
              Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings
            end;
            env
          | Some Options.LAZY_MODE_FILESYSTEM
          | None ->
            (* In filesystem lazy mode or in non-lazy mode, the only thing we need to do when
             * a new file is opened is to send the errors to the client *)
            let errors, warnings, _ = ErrorCollator.get_with_separate_warnings env in
            Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings;
            env
          end

    | Persistent_connection_prot.DidClose filenames ->
        Persistent_connection.send_message Persistent_connection_prot.DidCloseAck client;
        begin match Persistent_connection.client_did_close !env.connections client ~filenames with
        | None -> !env (* No new files were closed, so do nothing *)
        | Some (connections, client) ->
          let errors, warnings, _ = ErrorCollator.get_with_separate_warnings !env in
          Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings;
          {!env with connections}
        end
  in
  MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
  env
