(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module TI = Type_inference_js
module Server = ServerFunctors
module Persistent_connection_prot = ServerProt.Persistent_connection_prot

module FlowProgram : Server.SERVER_PROGRAM = struct
  open Utils_js
  open Sys_utils
  open ServerEnv
  open ServerUtils

  let name = "flow server"

  let sample_init_memory profiling =
    let open SharedMem_js in
    let dep_stats = dep_stats () in
    let hash_stats = hash_stats () in
    let heap_size = heap_size () in
    let memory_metrics = [
      "heap.size", heap_size;
      "dep_table.nonempty_slots", dep_stats.nonempty_slots;
      "dep_table.used_slots", dep_stats.used_slots;
      "dep_table.slots", dep_stats.slots;
      "hash_table.nonempty_slots", hash_stats.nonempty_slots;
      "hash_table.used_slots", hash_stats.used_slots;
      "hash_table.slots", hash_stats.slots;
    ] in
    List.fold_left (fun profiling (metric, value) ->
      Profiling_js.sample_memory
        ~metric:("init_done." ^ metric)
        ~value:(float_of_int value)
         profiling
    ) profiling memory_metrics

  let init ~focus_target genv =
    (* write binary path and version to server log *)
    Hh_logger.info "executable=%s" (Sys_utils.executable_path ());
    Hh_logger.info "version=%s" Flow_version.version;

    let profiling = Profiling_js.empty in

    let workers = genv.ServerEnv.workers in
    let options = genv.ServerEnv.options in

    let profiling, parsed, libs, libs_ok, errors =
      Types_js.init ~profiling ~workers options in

    (* if any libs errored, we'll infer but not merge client code *)
    let should_merge = libs_ok in

    (* compute initial state *)
    let profiling, checked, errors =
      if Options.is_lazy_mode options then
        profiling, FilenameSet.empty, errors
      else
        let parsed = FilenameSet.elements parsed in
        Types_js.full_check ~profiling ~workers ~focus_target ~options ~should_merge parsed errors
    in

    let profiling = sample_init_memory profiling in

    SharedMem_js.init_done();

    (* Return an env that initializes invariants required and maintained by
       recheck, namely that `files` contains files that parsed successfully, and
       `errors` contains the current set of errors. *)
    profiling, { ServerEnv.
      files = parsed;
      checked_files = checked;
      libs;
      errors;
      connections = Persistent_connection.empty;
    }


  let status_log errors =
    if Errors.ErrorSet.is_empty errors
      then Hh_logger.info "Status: OK"
      else Hh_logger.info "Status: Error";
    flush stdout

(* combine error maps into a single error list *)
let collate_errors =
  let open Errors in
  let collate errset acc =
    FilenameMap.fold (fun _key -> ErrorSet.union) errset acc
  in
  let add_unused_suppression_errors suppressions errors =
    (* For each unused suppression, create an error *)
    Error_suppressions.unused suppressions
    |> List.fold_left
      (fun errset loc ->
        let err =
          let msg = Flow_error.EUnusedSuppression loc in
          let source_file = match Loc.source loc with Some x -> x | None -> Loc.SourceFile "-" in
          Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file msg in
        Errors.ErrorSet.add err errset
      )
      errors
  in
  fun { ServerEnv.local_errors; merge_errors; suppressions; lint_settings; } ->
    let open Error_suppressions in
    let suppressions = union_suppressions suppressions in

      (* union the errors from all files together, filtering suppressed errors *)
      let errors = ErrorSet.empty
        |> collate local_errors
        |> collate merge_errors
      in
      let lint_settings = SuppressionMap.union_settings lint_settings in
      let errors, suppressed_errors, suppressions =
        filter_suppressed_errors suppressions lint_settings errors in
      let errors = add_unused_suppression_errors suppressions errors in
      errors, suppressed_errors

  let convert_errorl el =
    if Errors.ErrorSet.is_empty el then
      ServerProt.NO_ERRORS
    else
      ServerProt.ERRORS el

  let get_status genv env client_root =
    let server_root = Options.root genv.options in
    if server_root <> client_root then begin
      ServerProt.DIRECTORY_MISMATCH {
        ServerProt.server=server_root;
        ServerProt.client=client_root
      }
    end else begin
      (* collate errors by origin *)
      let errors, _ = collate_errors env.ServerEnv.errors in

      (* TODO: check status.directory *)
      status_log errors;
      FlowEventLogger.status_response
        ~num_errors:(Errors.ErrorSet.cardinal errors);
      convert_errorl errors
    end

  let check_once _genv env =
    collate_errors env.ServerEnv.errors

  let die_nicely () =
    FlowEventLogger.killed ();
    Hh_logger.fatal "Status: Error";
    Hh_logger.fatal "Sent KILL command by client. Dying.";
    (* when we exit, the dfind process will attempt to read from the broken
       pipe and then exit with SIGPIPE, so it is unnecessary to kill it
       explicitly *)
    die ()

  let autocomplete ~options ~workers ~env command_context file_input =
    let path, content = match file_input with
      | File_input.FileName _ -> failwith "Not implemented"
      | File_input.FileContent (_, content) ->
          File_input.filename_of_file_input file_input, content
    in
    let state = Autocomplete_js.autocomplete_set_hooks () in
    let results =
      try
        let path = Loc.SourceFile path in
        let profiling, cx, parse_result =
          match Types_js.typecheck_contents ~options ~workers ~env content path with
          | profiling, Some cx, _, parse_result -> profiling, cx, parse_result
          | _  -> failwith "Couldn't parse file"
        in
        AutocompleteService_js.autocomplete_get_results
          profiling
          command_context
          cx
          state
          parse_result
      with exn ->
        Hh_logger.warn "Couldn't autocomplete: %s" (Printexc.to_string exn);
        Ok []
    in
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
              get_docblock docblock_max_tokens (Loc.SourceFile file) content)
            in
            Docblock.is_flow docblock
        in
        if should_check then
          let file = Loc.SourceFile file in
          let checked = Types_js.typecheck_contents
            ~options ~workers ~env ~check_syntax:true content file in
          let errors = match checked with
          | _, _, errors, _ -> errors
          in
          convert_errorl errors
        else
          ServerProt.NOT_COVERED

  let mk_loc file line col =
    {
      Loc.
      source = Some file;
      start = { Loc.line; column = col; offset = 0; };
      _end = { Loc.line; column = col + 1; offset = 0; };
    }

  let infer_type
      ~options
      ~workers
      ~env
      client_context
      (file_input, line, col, verbose, include_raw) =
    let file = File_input.filename_of_file_input file_input in
    let file = Loc.SourceFile file in
    let response = (try
      let content = File_input.content_of_file_input file_input in
      let options = { options with Options.opt_verbose = verbose } in
      Ok (Type_info_service.type_at_pos
        ~options ~workers ~env ~client_context ~include_raw
        file content line col
      )
    with exn ->
      let err = spf "%s\n%s"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ()) in
      Error err
    ) in
    response

  let dump_types ~options ~workers ~env ~include_raw ~strip_root file_input =
    let file = File_input.filename_of_file_input file_input in
    let file = Loc.SourceFile file in
    try
      let content = File_input.content_of_file_input file_input in
      Ok (Type_info_service.dump_types
        ~options ~workers ~env ~include_raw ~strip_root file content
      )
    with exn ->
      Error (Printexc.to_string exn)

  let coverage ~options ~workers ~env ~force file_input =
    let file = File_input.filename_of_file_input file_input in
    let file = Loc.SourceFile file in
    try
      let content = File_input.content_of_file_input file_input in
      Ok (Type_info_service.coverage ~options ~workers ~env ~force file content)
    with exn ->
      Error (Printexc.to_string exn)

  let suggest =
    let suggest_for_file ~options ~workers ~env result_map (file, region) =
      (try
         let insertions = Type_info_service.suggest ~options ~workers ~env
           (Loc.SourceFile file) region (cat file) in
         SMap.add file (Ok insertions) result_map
      with exn ->
        SMap.add file (Error (Printexc.to_string exn)) result_map
      )
    in fun ~options ~workers ~env files ->
      List.fold_left (suggest_for_file ~options ~workers ~env) SMap.empty files

  (* NOTE: currently, not only returns list of annotations, but also writes a
     timestamped file with annotations *)
  let port = Port_service_js.port_files

  let find_module ~options (moduleref, filename) =
    let file = Loc.SourceFile filename in
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
      (Context.file cx) loc moduleref in
    Module_js.get_file ~audit:Expensive.warn module_name

  let gen_flow_files ~options env files =
    let errors, _ = collate_errors env.ServerEnv.errors in
    let result = if Errors.ErrorSet.is_empty errors
      then begin
        let (flow_files, flow_file_cxs, non_flow_files, error) =
          List.fold_left (fun (flow_files, cxs, non_flow_files, error) file ->
            if error <> None then (flow_files, cxs, non_flow_files, error) else
            match file with
            | File_input.FileContent _ ->
              let error_msg = "This command only works with file paths." in
              let error =
                Some (ServerProt.GenFlowFile_UnexpectedError error_msg)
              in
              (flow_files, cxs, non_flow_files, error)
            | File_input.FileName file_path ->
              let src_file = Loc.SourceFile file_path in
              (* TODO: Use InfoHeap as the definitive way to detect @flow vs
               * non-@flow
               *)
              try
                let cx =
                  Context_cache.get_context_unsafe ~options src_file
                    ~audit:Expensive.warn
                in
                (src_file::flow_files, cx::cxs, non_flow_files, error)
              with Not_found ->
                (flow_files, cxs, src_file::non_flow_files, error)
          ) ([], [], [], None) files
        in
        begin match error with
        | Some e -> Error e
        | None ->
          try
            begin
              try List.iter (fun flow_file_cx ->
                let master_cx: Context.t =
                  Merge_service.merge_strict_context ~options [flow_file_cx] in
                ignore master_cx
              ) flow_file_cxs
              with exn -> failwith (
                spf "Error merging contexts: %s" (Printexc.to_string exn)
              )
            end;

            (* Non-@flow files *)
            let result_contents = non_flow_files |> List.map (fun file ->
              (Loc.string_of_filename file, ServerProt.GenFlowFile_NonFlowFile)
            ) in

            (* Codegen @flow files *)
            let result_contents = List.fold_left2 (fun results file cx ->
              let file_path = Loc.string_of_filename file in
              try
                let code = FlowFileGen.flow_file cx in
                (file_path, ServerProt.GenFlowFile_FlowFile code)::results
              with exn ->
                failwith (spf "%s: %s" file_path (Printexc.to_string exn))
            ) result_contents flow_files flow_file_cxs in

            Ok result_contents
          with exn -> Error (
            ServerProt.GenFlowFile_UnexpectedError (Printexc.to_string exn)
          )
        end
      end else
        Error (ServerProt.GenFlowFile_TypecheckError errors)
    in
    result

  let find_refs ~options ~workers ~env (file_input, line, col) =
    let filename = File_input.filename_of_file_input file_input in
    let file = Loc.SourceFile filename in
    let loc = mk_loc file line col in
    let state = FindRefs_js.set_hooks loc in
    let result =
      try
        let content = File_input.content_of_file_input file_input in
        let cx = match Types_js.typecheck_contents ~options ~workers ~env content file with
          | _, Some cx, _, _ -> cx
          | _  -> failwith "Couldn't parse file"
        in
        Ok (FindRefs_js.result cx state)
      with exn ->
        Error (Printexc.to_string exn)
    in
    FindRefs_js.unset_hooks ();
    result

  let get_def ~options ~workers ~env command_context (file_input, line, col) =
    let filename = File_input.filename_of_file_input file_input in
    let file = Loc.SourceFile filename in
    let loc = mk_loc file line col in
    let state = GetDef_js.getdef_set_hooks loc in
    let result = try
      let content = File_input.content_of_file_input file_input in
      let profiling, cx = match Types_js.typecheck_contents ~options ~workers ~env content file with
        | profiling, Some cx, _, _ -> profiling, cx
        | _  -> failwith "Couldn't parse file"
      in
      Ok (GetDef_js.getdef_get_result
        profiling
        command_context
        ~options
        cx
        state
      )
    with exn ->
      Error (Printexc.to_string exn)
    in
    GetDef_js.getdef_unset_hooks ();
    result

  let module_name_of_string ~options module_name_str =
    let file_options = Options.file_options options in
    let path = Path.to_string (Path.make module_name_str) in
    if Files.is_flow_file ~options:file_options path
    then Modulename.Filename (Loc.SourceFile path)
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
          let { Module_js.
              required = requirements;
              require_loc = req_locs;
              _ } =
          Module_js.get_resolved_requires_unsafe ~audit:Expensive.warn file in
          (SMap.add module_name_str (requirements, req_locs) map, non_flow)
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

  let get_watch_paths options =
    let root = Options.root options in
    Files.watched_paths ~root (Options.file_options options)

  (* filter a set of updates coming from dfind and return
     a FilenameSet. updates may be coming in from
     the root, or an include path. *)
  let process_updates genv env updates =
    let options = genv.ServerEnv.options in
    let file_options = Options.file_options options in
    let all_libs =
      let known_libs = env.ServerEnv.libs in
      let _, maybe_new_libs = Files.init file_options in
      SSet.union known_libs maybe_new_libs
    in
    let root = Options.root options in
    let config_path = Server_files_js.config_file root in
    let sroot = Path.to_string root in
    let want = Files.wanted ~options:file_options all_libs in

    (* Die if the .flowconfig changed *)
    if SSet.mem config_path updates then begin
      Hh_logger.fatal "Status: Error";
      Hh_logger.fatal
        "%s changed in an incompatible way. Exiting.\n%!"
        config_path;
      FlowExitStatus.(exit Server_out_of_date)
    end;

    let is_incompatible filename_str =
      let filename = Loc.JsonFile filename_str in
      let filename_set = FilenameSet.singleton filename in
      let ast_opt =
        (*
         * If the file no longer exists, this will log a harmless error to
         * stderr and the get_ast call below will return None, which will
         * cause the server to exit.
         *
         * If the file has come into existence, reparse (true to its name)
         * will not actually parse the file. Again, this will cause get_ast
         * to return None and the server to exit.
         *
         * In both cases, this is desired behavior since a package.json file
         * has changed considerably.
         *)
        let _ = Parsing_service_js.reparse_with_defaults
          options
          (* workers *) None
          filename_set
        in
        Parsing_service_js.get_ast filename
      in
      match ast_opt with
        | None -> true
        | Some ast -> Module_js.package_incompatible filename_str ast
    in

    (* Die if a package.json changed in an incompatible way *)
    let incompatible_packages = SSet.filter (fun f ->
      (String_utils.string_starts_with f sroot ||
        Files.is_included file_options f)
      && (Filename.basename f) = "package.json"
      && want f
      && is_incompatible f
    ) updates in
    if not (SSet.is_empty incompatible_packages)
    then begin
      Hh_logger.fatal "Status: Error";
      SSet.iter (Hh_logger.fatal "Modified package: %s") incompatible_packages;
      Hh_logger.fatal
        "Packages changed in an incompatible way. Exiting.\n%!";
      FlowExitStatus.(exit Server_out_of_date)
    end;

    let flow_typed_path = Path.to_string (Files.get_flowtyped_path root) in
    let is_changed_lib filename =
      let is_lib = SSet.mem filename all_libs || filename = flow_typed_path in
      is_lib &&
        let file = Loc.LibFile filename in
        let old_ast = Parsing_service_js.get_ast file in
        let new_ast =
          let filename_set = FilenameSet.singleton file in
          let _ = Parsing_service_js.reparse_with_defaults
            (* types are always allowed in lib files *)
            ~types_mode:Parsing_service_js.TypesAllowed
            (* lib files are always "use strict" *)
            ~use_strict:true
            options
            (* workers *) None
            filename_set
          in
          Parsing_service_js.get_ast file
        in
        old_ast <> new_ast
    in

    (* Die if a lib file changed *)
    let libs = updates |> SSet.filter is_changed_lib in
    if not (SSet.is_empty libs)
    then begin
      Hh_logger.fatal "Status: Error";
      SSet.iter (Hh_logger.fatal "Modified lib file: %s") libs;
      Hh_logger.fatal
        "Lib files changed in an incompatible way. Exiting.\n%!";
      FlowExitStatus.(exit Server_out_of_date)
    end;

    SSet.fold (fun f acc ->
      if Files.is_flow_file ~options:file_options f &&
        (* note: is_included may be expensive. check in-root match first. *)
        (String_utils.string_starts_with f sroot ||
          Files.is_included file_options f) &&
        (* removes excluded and lib files. the latter are already filtered *)
        want f
      then
        let filename = Files.filename_from_string ~options:file_options f in
        FilenameSet.add filename acc
      else acc
    ) updates FilenameSet.empty

  (* on notification, execute client commands or recheck files *)
  let recheck genv env updates =
    if FilenameSet.is_empty updates
    then env
    else begin
      let options = genv.ServerEnv.options in
      let root = Options.root options in
      let tmp_dir = Options.temp_dir options in
      let workers = genv.ServerEnv.workers in
      ignore(Lock.grab (Server_files_js.recheck_file ~tmp_dir root));
      let env = Types_js.recheck ~options ~workers ~updates env in
      ignore(Lock.release (Server_files_js.recheck_file ~tmp_dir root));
      env
    end

  let respond ~genv ~env ~client { ServerProt.client_logging_context; command; } =
    let env = ref env in
    let oc = client.oc in
    let marshal msg =
      Marshal.to_channel oc msg [];
      flush oc
    in
    let options = genv.ServerEnv.options in
    let workers = genv.ServerEnv.workers in
    begin match command with
    | ServerProt.AUTOCOMPLETE fn ->
        let results: ServerProt.autocomplete_response =
          autocomplete ~options ~workers ~env client_logging_context fn
        in
        marshal results
    | ServerProt.CHECK_FILE (fn, verbose, graphml, force) ->
        let options = { options with Options.
          opt_output_graphml = graphml;
          opt_verbose = verbose;
        } in
        (check_file ~options ~workers ~env ~force fn: ServerProt.response)
          |> marshal
    | ServerProt.COVERAGE (fn, force) ->
        (coverage ~options ~workers ~env ~force fn: ServerProt.coverage_response)
          |> marshal
    | ServerProt.DUMP_TYPES (fn, include_raw, strip_root) ->
        let types: ServerProt.dump_types_response =
          dump_types ~options ~workers ~env ~include_raw ~strip_root fn
        in
        marshal types
    | ServerProt.FIND_MODULE (moduleref, filename) ->
        (find_module ~options (moduleref, filename): filename option)
          |> marshal
    | ServerProt.FIND_REFS (fn, line, char) ->
        (find_refs ~options ~workers ~env (fn, line, char): ServerProt.find_refs_response)
          |> marshal
    | ServerProt.FORCE_RECHECK (files) ->
        Marshal.to_channel oc () [];
        flush oc;
        let updates = process_updates genv !env (SSet.of_list files) in
        env := recheck genv !env updates
    | ServerProt.GEN_FLOW_FILES files ->
        (gen_flow_files ~options !env files: ServerProt.gen_flow_file_response)
          |> marshal
    | ServerProt.GET_DEF (fn, line, char) ->
        let def: ServerProt.get_def_response =
          get_def ~options ~workers ~env client_logging_context (fn, line, char)
        in
        marshal def
    | ServerProt.GET_IMPORTS module_names ->
        get_imports ~options module_names
          |> marshal
    | ServerProt.INFER_TYPE (fn, line, char, verbose, include_raw) ->
        (infer_type
            ~options ~workers ~env
            client_logging_context
            (fn, line, char, verbose, include_raw) : ServerProt.infer_type_response)
          |> marshal
    | ServerProt.KILL ->
        (Ok () : ServerProt.stop_response) |> marshal;
        die_nicely ()
    | ServerProt.PORT (files) ->
        (port files: ServerProt.port_response)
          |> marshal
    | ServerProt.STATUS client_root ->
        let status: ServerProt.response = get_status genv !env client_root in
        marshal status;
        begin match status with
          | ServerProt.DIRECTORY_MISMATCH {ServerProt.server; ServerProt.client} ->
              Hh_logger.fatal "Status: Error";
              Hh_logger.fatal "server_dir=%s, client_dir=%s"
                (Path.to_string server)
                (Path.to_string client);
              Hh_logger.fatal "%s is not listening to the same directory. Exiting."
                name;
              FlowExitStatus.(exit Server_client_directory_mismatch)
          | _ -> ()
        end
    | ServerProt.SUGGEST (files) ->
        (suggest ~options ~workers ~env files: ServerProt.suggest_response)
          |> marshal
    | ServerProt.CONNECT ->
        let new_connections, new_client =
          Persistent_connection.add_client
            !env.connections
            client
            client_logging_context
        in
        (* See ideCommand.ml for a detailed explanation about why this is needed *)
        Persistent_connection.send_ready new_client;
        env := {!env with connections = new_connections}
    end;
    !env

  let respond_to_persistent_client genv env client msg =
    let env = ref env in
    let options = genv.ServerEnv.options in
    let workers = genv.ServerEnv.workers in
    match msg with
      | Persistent_connection_prot.Subscribe ->
          let errorl, _ = collate_errors !env.errors in
          let new_connections =
            Persistent_connection.subscribe_client !env.connections client errorl
          in
          { !env with connections = new_connections }
      | Persistent_connection_prot.Autocomplete (file_input, id) ->
          let client_logging_context = Persistent_connection.get_logging_context client in
          let results = autocomplete ~options ~workers ~env client_logging_context file_input in
          let wrapped = Persistent_connection_prot.AutocompleteResult (results, id) in
          Persistent_connection.send_message wrapped client;
          !env

  let should_close = function
    | { ServerProt.command = ServerProt.CONNECT; _ } -> false
    | _ -> true

  let handle_client genv env client =
    let command : ServerProt.command_with_context = Marshal.from_channel client.ic in
    let env = respond ~genv ~env ~client command in
    if should_close command then client.close ();
    env

  let handle_persistent_client genv env client =
    let msg, env =
      try
        Some (Persistent_connection.input_value client), env
      with
        | End_of_file ->
            print_endline "Lost connection to client";
            let new_connections = Persistent_connection.remove_client env.connections client in
            None, {env with connections = new_connections}
    in
    match msg with
      | Some msg -> respond_to_persistent_client genv env client msg
      | None -> env

end
