(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module TI = Type_inference_js
module Server = ServerFunctors

open Core_result
let try_with f =
  try f () with exn -> Error (Printexc.to_string exn)

module FlowProgram : Server.SERVER_PROGRAM = struct
  open Utils_js
  open Sys_utils
  open ServerEnv

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
    List.iter (fun (metric, value) ->
      Profiling_js.sample_memory
        ~metric:("init_done." ^ metric)
        ~value:(float_of_int value)
         profiling
    ) memory_metrics


  (* combine error maps into a single error set and a filtered warning map
   *
   * This can be a little expensive for large repositories and can take a couple of seconds.
   * Therefore there are a few things we want to do:
   *
   * 1. Memoize the result in env. This means subsequent calls to commands like `flow status` can
   *    be fast
   * 2. Eagerly calculate `collate_errors` after init or a recheck, so that the server still has
   *    the init or recheck lock. If we improve how clients can tell if a server is busy or stuck
   *    then we can probably relax this.
   * 3. Throw away the collated errors when lazy mode's typecheck_contents adds more dependents or
   *    dependencies to the checked set
   **)
  let regenerate_collated_errors =
    let open Errors in
    let open Error_suppressions in
    let add_unused_suppression_warnings checked suppressions warnings =
      (* For each unused suppression, create an warning *)
      Error_suppressions.unused suppressions
      |> List.fold_left
        (fun warnings loc ->
          let source_file = match Loc.source loc with Some x -> x | None -> File_key.SourceFile "-" in
          (* In lazy mode, dependencies are modules which we typecheck not because we care about
           * them, but because something important (a focused file or a focused file's dependent)
           * needs these dependencies. Therefore, we might not typecheck a dependencies' dependents.
           *
           * This means there might be an unused suppression comment warning in a dependency which
           * only shows up in lazy mode. To avoid this, we'll just avoid raising this kind of
           * warning in any dependency.*)
          if not (CheckedSet.dependencies checked |> FilenameSet.mem source_file)
          then begin
            let err =
              let msg = Flow_error.EUnusedSuppression loc in
              Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file msg in
            let file_warnings = FilenameMap.get source_file warnings
              |> Option.value ~default:ErrorSet.empty
              |> ErrorSet.add err in
            FilenameMap.add source_file file_warnings warnings
          end else
            warnings
        )
        warnings
    in
    let acc_fun severity_cover filename file_errs
        (errors, warnings, suppressed_errors, suppressions) =
      let file_errs, file_warns, file_suppressed_errors, suppressions =
        filter_suppressed_errors suppressions severity_cover file_errs in
      let errors = ErrorSet.union file_errs errors in
      let warnings = FilenameMap.add filename file_warns warnings in
      let suppressed_errors = List.rev_append file_suppressed_errors suppressed_errors in
      (errors, warnings, suppressed_errors, suppressions)
    in
    fun env ->
      let {
        ServerEnv.local_errors; merge_errors; suppressions; severity_cover_set;
      } = env.ServerEnv.errors in
      let suppressions = union_suppressions suppressions in

      (* union the errors from all files together, filtering suppressed errors *)
      let severity_cover = ExactCover.union_all severity_cover_set in
      let acc_fun = acc_fun severity_cover in
      let collated_errorset, warnings, collated_suppressed_errors, suppressions =
        (ErrorSet.empty, FilenameMap.empty, [], suppressions)
        |> FilenameMap.fold acc_fun local_errors
        |> FilenameMap.fold acc_fun merge_errors
      in

      let collated_warning_map =
        add_unused_suppression_warnings env.ServerEnv.checked_files suppressions warnings in
      { collated_errorset; collated_warning_map; collated_suppressed_errors }

  let get_collated_errors_separate_warnings env =
    let open ServerEnv in
    let collated_errors = match !(env.collated_errors) with
    | None ->
      let collated_errors = regenerate_collated_errors env in
      env.collated_errors := Some collated_errors;
      collated_errors
    | Some collated_errors ->
      collated_errors
    in
    let { collated_errorset; collated_warning_map; collated_suppressed_errors } = collated_errors in
    (collated_errorset, collated_warning_map, collated_suppressed_errors)

  (* combine error maps into a single error set and a single warning set *)
  let get_collated_errors env =
    let open Errors in
    let errors, warning_map, suppressed_errors = get_collated_errors_separate_warnings env in
    let warnings = FilenameMap.fold (fun _key -> ErrorSet.union) warning_map ErrorSet.empty in
    (errors, warnings, suppressed_errors)

  let init ~focus_targets genv =
    (* write binary path and version to server log *)
    Hh_logger.info "executable=%s" (Sys_utils.executable_path ());
    Hh_logger.info "version=%s" Flow_version.version;

    let workers = genv.ServerEnv.workers in
    let options = genv.ServerEnv.options in

    MonitorRPC.status_update ~event:ServerStatus.Init_start;

    let should_print_summary = Options.should_profile options in
    let env = Profiling_js.with_profiling ~should_print_summary begin fun profiling ->
      let parsed, libs, libs_ok, errors =
        Types_js.init ~profiling ~workers options in

      (* if any libs errored, we'll infer but not merge client code *)
      let should_merge = libs_ok in

      (* compute initial state *)
      let checked, errors =
        if Options.is_lazy_mode options then
          CheckedSet.empty, errors
        else
          let parsed = FilenameSet.elements parsed in
          Types_js.full_check ~profiling ~workers ~focus_targets ~options ~should_merge parsed errors
      in

      sample_init_memory profiling;

      SharedMem_js.init_done();

      (* Return an env that initializes invariants required and maintained by
         recheck, namely that `files` contains files that parsed successfully, and
         `errors` contains the current set of errors. *)
      let env = { ServerEnv.
        files = parsed;
        checked_files = checked;
        libs;
        errors;
        collated_errors = ref None;
        connections = Persistent_connection.empty;
      } in
      env.collated_errors := Some (regenerate_collated_errors env);
      env
    end in
    MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
    env


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
      let errors, warnings, _ = get_collated_errors env in
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

  let check_once _genv env =
    get_collated_errors env

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

  let suggest =
    let suggest_for_file ~options ~workers ~env result_map (file, region) =
      SMap.add file (try_with begin fun () ->
        Type_info_service.suggest ~options ~workers ~env
          (File_key.SourceFile file) region (cat file)
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
      (Context.file cx) loc moduleref in
    Module_js.get_file ~audit:Expensive.warn module_name

  let gen_flow_files ~options env files =
    let errors, warnings, _ = get_collated_errors env in
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
              let cx, _ = Merge_service.merge_strict_context ~options [file] in
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

  let find_refs ~options ~workers ~env (file_input, line, col, global) =
    FindRefs_js.find_refs options workers env file_input line col global

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
          let requires = File_sig.(fsig.module_sig.requires) in
          let mlocs = SMap.fold (fun mref r acc ->
            let m = SMap.find_unsafe mref resolved_modules in
            let loc = r.File_sig.loc in
            Modulename.Map.add m loc acc
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
      FlowExitStatus.(exit Flowconfig_changed)
    end;

    let is_incompatible filename_str =
      let filename = File_key.JsonFile filename_str in
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
        let file = File_key.LibFile filename in
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
  let recheck genv env ?(force_focus=false) updates =
    if FilenameSet.is_empty updates
    then env
    else begin
      MonitorRPC.status_update ~event:ServerStatus.Recheck_start;
      Persistent_connection.send_start_recheck env.connections;
      let options = genv.ServerEnv.options in
      let workers = genv.ServerEnv.workers in

      let env = Types_js.recheck ~options ~workers ~updates env ~force_focus in
      (* Unfortunately, regenerate_collated_errors is currently a little expensive. As the checked
       * repo grows, regenerate_collated_errors can easily take a couple of seconds. So we need to
       * run it before we release the recheck lock *)
      env.collated_errors := Some (regenerate_collated_errors env);

      MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
      Persistent_connection.send_end_recheck env.connections;
      let errors, warnings, _ = get_collated_errors_separate_warnings env in
      Persistent_connection.update_clients ~clients:env.connections ~errors ~warnings;
      env
    end

  let handle_command_unsafe
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
            ~options ~workers ~env (fn, line, char, global): ServerProt.Response.find_refs_response
        ) |> respond
    | ServerProt.Request.FORCE_RECHECK (files, force_focus) ->
        respond ServerProt.Response.FORCE_RECHECK;
        let updates = process_updates genv !env (SSet.of_list files) in
        env := recheck genv !env ~force_focus updates
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
              Hh_logger.fatal "%s is not listening to the same directory. Exiting."
                name;
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

  let handle_command genv env (request_id, command) =
    try handle_command_unsafe genv env (request_id, command)
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

  let handle_persistent_client genv env client_id msg =
    let env = ref env in
    let options = genv.ServerEnv.options in
    let workers = genv.ServerEnv.workers in
    Hh_logger.debug "Persistent request: %s" (Persistent_connection_prot.string_of_request msg);
    MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;
    let client = Persistent_connection.get_client !env.connections client_id in
    let env = match msg with
      | Persistent_connection_prot.Subscribe ->
          let current_errors, current_warnings, _ = get_collated_errors_separate_warnings !env in
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
              let file_options = Options.file_options options in
              let focused = Nel.fold_left
                (fun acc fn ->
                  FilenameSet.add (Files.filename_from_string ~options:file_options fn) acc)
                FilenameSet.empty
                filenames in
              let checked_files = CheckedSet.add ~focused env.checked_files in

              let new_focused_files = focused
                |> Fn.flip FilenameSet.diff (CheckedSet.focused env.checked_files)
                |> Fn.flip FilenameSet.diff (CheckedSet.dependents env.checked_files) in

              let env = { env with checked_files } in
              if not (FilenameSet.is_empty new_focused_files)
              then
                (* Rechecking will send errors to the clients *)
                recheck genv env new_focused_files
              else begin
                (* This open doesn't trigger a recheck, but we'll still send down the errors *)
                let errors, warnings, _ = get_collated_errors_separate_warnings env in
                Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings;
                env
              end
            | Some Options.LAZY_MODE_FILESYSTEM
            | None ->
              (* In filesystem lazy mode or in non-lazy mode, the only thing we need to do when
               * a new file is opened is to send the errors to the client *)
              let errors, warnings, _ = get_collated_errors_separate_warnings env in
              Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings;
              env
            end

      | Persistent_connection_prot.DidClose filenames ->
          Persistent_connection.send_message Persistent_connection_prot.DidCloseAck client;
          begin match Persistent_connection.client_did_close !env.connections client ~filenames with
          | None -> !env (* No new files were closed, so do nothing *)
          | Some (connections, client) ->
            let errors, warnings, _ = get_collated_errors_separate_warnings !env in
            Persistent_connection.send_errors_if_subscribed ~client ~errors ~warnings;
            {!env with connections}
          end
    in
    MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
    env
end
