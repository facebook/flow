(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result
open ServerEnv
open Utils_js
open Lsp

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
  let lazy_stats = { ServerProt.Response.
    lazy_mode = Options.lazy_mode genv.options;
    checked_files = CheckedSet.all env.checked_files |> Utils_js.FilenameSet.cardinal;
    total_files = Utils_js.FilenameSet.cardinal env.files;
  } in
  let status_response =
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
  in
  status_response, lazy_stats

let autocomplete ~options ~workers ~env ~profiling file_input =
  let path, content = match file_input with
    | File_input.FileName _ -> failwith "Not implemented"
    | File_input.FileContent (_, content) ->
        File_input.filename_of_file_input file_input, content
  in
  let state = Autocomplete_js.autocomplete_set_hooks () in
  let path = File_key.SourceFile path in
  let%lwt check_contents_result =
    Types_js.basic_check_contents ~options ~workers ~env ~profiling content path
  in
  let%lwt autocomplete_result =
    map_error ~f:(fun str -> str, None) check_contents_result
    %>>= (fun (cx, info) ->
      Profiling_js.with_timer_lwt profiling ~timer:"GetResults" ~f:(fun () ->
        try_with_json (fun () ->
          Lwt.return (AutocompleteService_js.autocomplete_get_results cx state info)
        )
      )
    )
  in
  let results, json_data_to_log = split_result autocomplete_result in
  Autocomplete_js.autocomplete_unset_hooks ();
  Lwt.return (results, json_data_to_log)

let check_file ~options ~workers ~env ~profiling ~force file_input =
  let file = File_input.filename_of_file_input file_input in
  match file_input with
  | File_input.FileName _ -> failwith "Not implemented"
  | File_input.FileContent (_, content) ->
      let should_check =
        if force then
          true
        else
          let (_, docblock) = Parsing_service_js.(
            parse_docblock docblock_max_tokens (File_key.SourceFile file) content)
          in
          Docblock.is_flow docblock
      in
      if should_check then
        let file = File_key.SourceFile file in
        let%lwt errors, warnings =
          Types_js.typecheck_contents ~options ~workers ~env ~profiling content file
        in
        Lwt.return (convert_errors ~errors ~warnings)
      else
        Lwt.return (ServerProt.Response.NOT_COVERED)

let infer_type
    ~options
    ~workers
    ~env
    ~profiling
    (file_input, line, col, verbose) =
  let file = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile file in
  let options = { options with Options.opt_verbose = verbose } in
  match File_input.content_of_file_input file_input with
  | Error e -> Lwt.return (Error e, None)
  | Ok content ->
    let%lwt result = try_with_json (fun () ->
      Type_info_service.type_at_pos ~options ~workers ~env ~profiling file content line col
    ) in
    Lwt.return (split_result result)

let dump_types ~options ~workers ~env ~profiling file_input =
  let file = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile file in
  File_input.content_of_file_input file_input
  %>>= fun content ->
    try_with begin fun () ->
      Type_info_service.dump_types ~options ~workers ~env ~profiling file content
    end

let coverage ~options ~workers ~env ~profiling ~force file_input =
  let file = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile file in
  File_input.content_of_file_input file_input
  %>>= fun content ->
    try_with begin fun () ->
      Type_info_service.coverage ~options ~workers ~env ~profiling ~force file content
    end

let get_cycle ~workers ~env fn =
  (* Re-calculate SCC *)
  let parsed = !env.ServerEnv.files in
  let%lwt dependency_graph = Dep_service.calc_dependency_graph workers parsed in
  Lwt.return (
    let components = Sort_js.topsort ~roots:parsed dependency_graph in

    (* Get component for target file *)
    let component = List.find (Nel.mem fn) components in

    (* Restrict dep graph to only in-cycle files *)
    let subgraph = Nel.fold_left (fun acc f ->
      Option.fold (FilenameMap.get f dependency_graph) ~init:acc ~f:(fun acc deps ->
        let subdeps = FilenameSet.filter (fun f -> Nel.mem f component) deps in
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
  )

let suggest =
  let suggest_for_file ~options ~workers ~env ~profiling result_map (file, region) =
    let%lwt result = try_with begin fun () ->
      Type_info_service.suggest ~options ~workers ~env ~profiling
        (File_key.SourceFile file) region (Sys_utils.cat file)
    end in
    Lwt.return (SMap.add file result result_map)
  in

  fun ~options ~workers ~env ~profiling files ->
    Lwt_list.fold_left_s (suggest_for_file ~options ~workers ~env ~profiling) SMap.empty files

(* NOTE: currently, not only returns list of annotations, but also writes a
   timestamped file with annotations *)
let port = Port_service_js.port_files

let find_module ~options (moduleref, filename) =
  let file = File_key.SourceFile filename in
  let loc = {Loc.none with Loc.source = Some file} in
  let module_name = Module_js.imported_module
    ~options ~node_modules_containers:!Files.node_modules_containers
    file (Nel.one loc) moduleref in
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
            let component = Nel.one file in
            let { Merge_service.cx; _ } = Merge_service.merge_strict_context ~options component in
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

let find_refs ~genv ~env ~profiling (file_input, line, col, global) =
  FindRefs_js.find_refs ~genv ~env ~profiling ~file_input ~line ~col ~global

(* This returns result, json_data_to_log, where json_data_to_log is the json data from
 * getdef_get_result which we end up using *)
let get_def ~options ~workers ~env ~profiling position =
  GetDef_js.get_def ~options ~workers ~env ~profiling ~depth:0 position

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
  genv env (request_id, { ServerProt.Request.client_logging_context=_; command; }) =
  let env = ref env in
  let respond msg =
    MonitorRPC.respond_to_request ~request_id ~response:msg

  in
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in
  Hh_logger.debug "Request: %s" (ServerProt.Request.to_string command);
  MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;
  let should_print_summary = Options.should_profile genv.options in
  let%lwt profiling, json_data =
    Profiling_js.with_profiling_lwt ~should_print_summary begin fun profiling ->
      match command with
      | ServerProt.Request.AUTOCOMPLETE fn ->
          let%lwt result, json_data = autocomplete ~options ~workers ~env ~profiling fn in
          ServerProt.Response.AUTOCOMPLETE result
          |> respond;
          Lwt.return json_data
      | ServerProt.Request.CHECK_FILE (fn, verbose, force, include_warnings) ->
          let options = { options with Options.
            opt_verbose = verbose;
            opt_include_warnings = options.Options.opt_include_warnings || include_warnings;
          } in
          let%lwt response = check_file ~options ~workers ~env ~force ~profiling fn in
          ServerProt.Response.CHECK_FILE response
          |> respond;
          Lwt.return None
      | ServerProt.Request.COVERAGE (fn, force) ->
          let%lwt response = coverage ~options ~workers ~env ~profiling ~force fn in
          ServerProt.Response.COVERAGE response
          |> respond;
          Lwt.return None
      | ServerProt.Request.CYCLE fn ->
          let file_options = Options.file_options options in
          let fn = Files.filename_from_string ~options:file_options fn in
          let%lwt response = get_cycle ~workers ~env fn in
          ServerProt.Response.CYCLE response
          |> respond;
          Lwt.return None
      | ServerProt.Request.DUMP_TYPES (fn) ->
          let%lwt response = dump_types ~options ~workers ~env ~profiling fn in
          ServerProt.Response.DUMP_TYPES response
          |> respond;
          Lwt.return None
      | ServerProt.Request.FIND_MODULE (moduleref, filename) ->
          ServerProt.Response.FIND_MODULE (
            find_module ~options (moduleref, filename): File_key.t option
          ) |> respond;
          Lwt.return None
      | ServerProt.Request.FIND_REFS (fn, line, char, global) ->
          let%lwt result, json_data = find_refs ~genv ~env ~profiling (fn, line, char, global) in
          ServerProt.Response.FIND_REFS result |> respond;
          Lwt.return json_data
      | ServerProt.Request.FORCE_RECHECK { files; focus; profile; } ->
          (* If we're not profiling the recheck, then respond immediately *)
          if not profile then respond (ServerProt.Response.FORCE_RECHECK None);
          let updates = Rechecker.process_updates genv !env (SSet.of_list files) in
          let%lwt profiling, new_env = Rechecker.recheck genv !env ~force_focus:focus updates in
          env := new_env;
          if profile then respond (ServerProt.Response.FORCE_RECHECK profiling);
          Lwt.return None
      | ServerProt.Request.GEN_FLOW_FILES (files, include_warnings) ->
          let options = { options with Options.
            opt_include_warnings = options.Options.opt_include_warnings || include_warnings;
          } in
          ServerProt.Response.GEN_FLOW_FILES (
            gen_flow_files ~options !env files: ServerProt.Response.gen_flow_files_response
          ) |> respond;
          Lwt.return None
      | ServerProt.Request.GET_DEF (fn, line, char) ->
          let%lwt result, json_data = get_def ~options ~workers ~env ~profiling (fn, line, char) in
          ServerProt.Response.GET_DEF result
          |> respond;
          Lwt.return json_data
      | ServerProt.Request.GET_IMPORTS module_names ->
          ServerProt.Response.GET_IMPORTS (
            get_imports ~options module_names: ServerProt.Response.get_imports_response
          ) |> respond;
          Lwt.return None
      | ServerProt.Request.INFER_TYPE (fn, line, char, verbose) ->
          let%lwt result, json_data =
            infer_type ~options ~workers ~env ~profiling (fn, line, char, verbose)
          in
          ServerProt.Response.INFER_TYPE result
          |> respond;
          Lwt.return json_data
      | ServerProt.Request.PORT (files) ->
          ServerProt.Response.PORT (port files: ServerProt.Response.port_response)
          |> respond;
          Lwt.return None
      | ServerProt.Request.STATUS (client_root, include_warnings) ->
          let genv = {genv with
            options = let open Options in {genv.options with
              opt_include_warnings = genv.options.opt_include_warnings || include_warnings
            }
          } in
          let status_response, lazy_stats = get_status genv !env client_root in
          respond (ServerProt.Response.STATUS {status_response; lazy_stats});
          begin match status_response with
            | ServerProt.Response.DIRECTORY_MISMATCH {ServerProt.Response.server; client} ->
                Hh_logger.fatal "Status: Error";
                Hh_logger.fatal "server_dir=%s, client_dir=%s"
                  (Path.to_string server)
                  (Path.to_string client);
                Hh_logger.fatal "flow server is not listening to the same directory. Exiting.";
                FlowExitStatus.(exit Server_client_directory_mismatch)
            | _ -> ()
          end;
          Lwt.return None
      | ServerProt.Request.SUGGEST (files) ->
          let%lwt result = suggest ~options ~workers ~env ~profiling files in
          ServerProt.Response.SUGGEST result
          |> respond;
          Lwt.return None
    end
  in
  MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
  Lwt.return (!env, profiling, json_data)

let handle_ephemeral genv env (request_id, command) =
  try%lwt
    let%lwt env, profiling, json_data = handle_ephemeral_unsafe genv env (request_id, command) in
    FlowEventLogger.ephemeral_command_success
      ?json_data
      ~client_context:command.ServerProt.Request.client_logging_context
      ~profiling;
    Lwt.return env
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
    FlowEventLogger.ephemeral_command_failure
      ~client_context:command.ServerProt.Request.client_logging_context
      ~json_data:(Hh_json.JSON_Object [ "exn", Hh_json.JSON_String exn_str ]);
    MonitorRPC.request_failed ~request_id ~exn_str;
    Lwt.return env


(** handle_persistent_unsafe:
   either this method returns Ok (and optionally returns some logging data),
   or it returns Error for some well-understood reason string,
   or it might raise/Lwt.fail, indicating a misunderstood coding bug. *)
let handle_persistent_unsafe genv env client profiling msg
  : ((ServerEnv.env * Hh_json.json option, ServerEnv.env * string) result) Lwt.t =
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in
  let lsp_writer: (lsp_message -> unit) = fun payload ->
    Persistent_connection.send_message (Persistent_connection_prot.LspFromServer payload) client
  in

  match msg with
  | Persistent_connection_prot.Subscribe ->
      let current_errors, current_warnings, _ = ErrorCollator.get_with_separate_warnings env in
      let new_connections = Persistent_connection.subscribe_client
        ~clients:env.connections ~client ~current_errors ~current_warnings
      in
      Lwt.return (Ok ({ env with connections = new_connections }, None))

  | Persistent_connection_prot.Autocomplete (file_input, id) ->
      let env = ref env in
      let%lwt results, json_data = autocomplete ~options ~workers ~env ~profiling file_input in
      let wrapped = Persistent_connection_prot.AutocompleteResult (results, id) in
      Persistent_connection.send_message wrapped client;
      Lwt.return (Ok (!env, json_data))

  | Persistent_connection_prot.DidOpen filenames ->
      Persistent_connection.send_message Persistent_connection_prot.DidOpenAck client;

      begin match Persistent_connection.client_did_open env.connections client ~filenames with
      | None -> Lwt.return (Ok (env, None)) (* No new files were opened, so do nothing *)
      | Some (connections, client) ->
        let env = {env with connections} in

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
          let%lwt env, triggered_recheck = Lazy_mode_utils.focus_and_check genv env filenames in
          if not triggered_recheck then begin
            (* This open doesn't trigger a recheck, but we'll still send down the errors *)
            let errors, warnings, _ = ErrorCollator.get_with_separate_warnings env in
            Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings
          end;
          Lwt.return (Ok (env, None))
        | Some Options.LAZY_MODE_FILESYSTEM
        | None ->
          (* In filesystem lazy mode or in non-lazy mode, the only thing we need to do when
           * a new file is opened is to send the errors to the client *)
          let errors, warnings, _ = ErrorCollator.get_with_separate_warnings env in
          Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings;
          Lwt.return (Ok (env, None))
        end

  | Persistent_connection_prot.DidClose filenames ->
      Persistent_connection.send_message Persistent_connection_prot.DidCloseAck client;
      begin match Persistent_connection.client_did_close env.connections client ~filenames with
        | None -> Lwt.return (Ok (env, None)) (* No new files were closed, so do nothing *)
        | Some (connections, client) ->
          let errors, warnings, _ = ErrorCollator.get_with_separate_warnings env in
          Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings;
          Lwt.return (Ok ({env with connections}, None))
      end

  | Persistent_connection_prot.LspToServer (NotificationMessage (DidOpenNotification _params)) ->
    (* TODO: track per-client the open editor files, and DidChange/DidClose *)
    Lwt.return (Ok (env, None))

  | Persistent_connection_prot.LspToServer (RequestMessage (id, DefinitionRequest params)) ->
    (* TODO: use the open editor file contents, not FileName *)
    let env = ref env in
    let open TextDocumentPositionParams in
    let fn = Lsp_helpers.lsp_textDocumentIdentifier_to_filename params.textDocument in
    let line = params.position.line in
    let char = params.position.character in
    let%lwt (result, json_data) =
      get_def ~options ~workers ~env ~profiling (File_input.FileName fn, line, char) in
    begin match result with
      | Ok loc ->
        let default_path = params.textDocument.TextDocumentIdentifier.uri in
        let uri = match loc.Loc.source with
          | Some file_key ->
            file_key |> File_key.to_string |> Lsp_helpers.path_to_lsp_uri ~default_path
          | None ->
            default_path in
        let location = { Lsp.Location.
          uri;
          range = { Lsp.
            start = { Lsp.line=loc.Loc.start.Loc.line; character=loc.Loc.start.Loc.column; };
            end_ = { Lsp.line=loc.Loc._end.Loc.line; character=loc.Loc._end.Loc.column; };
          }
        } in
        lsp_writer (ResponseMessage (id, DefinitionResult [location]));
        Lwt.return (Ok (!env, json_data))
      | Error reason ->
        Lwt.return (Error (!env, reason))
    end

  | Persistent_connection_prot.LspToServer unhandled ->
    let reason = Printf.sprintf "not implemented: %s" (Lsp_fmt.message_to_string unhandled) in
    Lwt.return (Error (env, reason))


let report_lsp_error_if_necessary
    (client: Persistent_connection.single_client)
    (msg: Persistent_connection_prot.request)
    (e: exn)
    (stack: string)
  : unit =
  let lsp_writer: (lsp_message -> unit) = fun payload ->
    Persistent_connection.send_message (Persistent_connection_prot.LspFromServer payload) client
  in
  match msg with
  | Persistent_connection_prot.LspToServer (RequestMessage (id, _)) ->
    let response = ResponseMessage (id, ErrorResult (e,stack)) in
    lsp_writer response
  | Persistent_connection_prot.LspToServer _ ->
    let open LogMessage in
    let (code, reason, _original_data) = Lsp_fmt.get_error_info e in
    let message = (Printf.sprintf "%s [%i]\n%s" reason code stack) in
    let notification = NotificationMessage
      (LogMessageNotification {type_=MessageType.ErrorMessage; message;}) in
    lsp_writer notification
  | _ ->
    ()


let handle_persistent
    (genv: ServerEnv.genv)
    (env: ServerEnv.env)
    (client_id: Persistent_connection.Prot.client_id)
    (msg: Persistent_connection_prot.request)
  : ServerEnv.env Lwt.t =
  Hh_logger.debug "Persistent request: %s" (Persistent_connection_prot.string_of_request msg);
  MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;

  let client = Persistent_connection.get_client env.connections client_id in
  let client_context = Persistent_connection.get_logging_context client in
  let should_print_summary = Options.should_profile genv.options in
  let request = Persistent_connection_prot.denorm_string_of_request msg in
  try

    let%lwt profiling, result = Profiling_js.with_profiling_lwt ~should_print_summary
      (fun profiling -> handle_persistent_unsafe genv env client profiling msg) in

    MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
    match result with
    | Ok (env, json_data) ->
      FlowEventLogger.persistent_command_success ?json_data ~request ~client_context ~profiling;
      Lwt.return env
    | Error (env, reason) ->
      let json_data = Some (Hh_json.JSON_Object [ "Error", Hh_json.JSON_String reason ]) in
      FlowEventLogger.persistent_command_success ?json_data ~request ~client_context ~profiling;
      (* note that persistent_command_success is used even for Errors; *)
      (* we only use persistent_command_failure for exceptions, i.e. coding bugs *)
      report_lsp_error_if_necessary client msg (Failure reason) "[no stack]";
      Lwt.return env
  with exn ->
    let backtrace = String.trim (Printexc.get_backtrace ()) in
    let exn_str = Printf.sprintf "%s%s%s"
      (Printexc.to_string exn)
      (if backtrace = "" then "" else "\n")
      backtrace in
    Hh_logger.error "Uncaught exception handling persistent request (%s): %s" request exn_str;
    let json_data = Hh_json.JSON_Object [ "exn", Hh_json.JSON_String exn_str ] in
    FlowEventLogger.persistent_command_failure ~request ~client_context ~json_data;
    report_lsp_error_if_necessary client msg exn backtrace;
    MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
    Lwt.return env
