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

  let init genv =
    (* Encapsulate merge_strict_context for dumper *)
    let merge_component options cx =
      let cache = new Context_cache.context_cache in
      Merge_service.merge_contents_context ~options cache cx in
    (* write binary path and version to server log *)
    Hh_logger.info "executable=%s" (Sys_utils.executable_path ());
    Hh_logger.info "version=%s" FlowConfig.version;
    (* start the server and pipe its result into the dumper *)
    Types_js.server_init genv
    |> Dumper.init merge_component genv

  let incorrect_hash oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_OUT_OF_DATE;
    FlowEventLogger.out_of_date ();
    Hh_logger.fatal     "Status: Error";
    Hh_logger.fatal     "%s is out of date. Exiting." name;
    FlowExitStatus.(exit Server_out_of_date)

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
  let filter_suppressed_errors suppressions errors =
    (* Filter out suppressed errors. also track which suppressions are used. *)
    let errors, suppressed_errors, suppressions = ErrorSet.fold
      (fun error (errors, suppressed_errors, supp_acc) ->
        let locs = Errors.locs_of_error error in
        let (suppressed, consumed_suppressions, supp_acc) = Error_suppressions.check locs supp_acc in
        let errors, suppressed_errors = if suppressed
          then errors, (error, consumed_suppressions)::suppressed_errors
          else ErrorSet.add error errors, suppressed_errors in
        errors, suppressed_errors, supp_acc
      ) errors (ErrorSet.empty, [], suppressions) in

    (* For each unused suppression, create an error *)
    let errors =
      Error_suppressions.unused suppressions
      |> List.fold_left
        (fun errset loc ->
          let err = Errors.mk_error [
            loc, ["Error suppressing comment"; "Unused suppression"]
          ] in
          ErrorSet.add err errset
        )
        errors in

      errors, suppressed_errors
  in
  fun { ServerEnv.local_errors; merge_errors; suppressions; } ->
    (* union suppressions from all files together *)
    let suppressions = FilenameMap.fold
      (fun _key -> Error_suppressions.union)
      suppressions
      Error_suppressions.empty in

    (* union the errors from all files together, filtering suppressed errors *)
    ErrorSet.empty
    |> collate local_errors
    |> collate merge_errors
    |> filter_suppressed_errors suppressions

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

  (* helper - print errors. used in check-and-die runs *)
  let print_errors ~profiling ~suppressed_errors options errors =
    let strip_root =
      if Options.should_strip_root options
      then Some (Options.root options)
      else None
    in

    let suppressed_errors = if Options.include_suppressed options
    then suppressed_errors
    else [] in

    match Options.json_mode options with
    | Some mode ->
      let profiling =
        if options.Options.opt_profile
        then Some profiling
        else None in
      Errors.Json_output.print_errors
        ~out_channel:stdout
        ~strip_root
        ~profiling
        ~pretty:(mode = Options.PrettyJSON)
        ~suppressed_errors
        errors
    | None ->
      let errors = List.fold_left
        (fun acc (error, _) -> Errors.ErrorSet.add error acc)
        errors
        suppressed_errors
      in
      Errors.Cli_output.print_errors
        ~out_channel:stdout
        ~flags:(Options.error_flags options)
        ~strip_root
        errors

  let run_once_and_exit ~profiling genv env =
    let errors, suppressed_errors = collate_errors env.ServerEnv.errors in
    print_errors ~profiling ~suppressed_errors genv.ServerEnv.options errors;
    if Errors.ErrorSet.is_empty errors
      then FlowExitStatus.(exit No_error)
      else FlowExitStatus.(exit Type_error)

  let die_nicely oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_DYING;
    FlowEventLogger.killed ();
    Hh_logger.fatal "Status: Error";
    Hh_logger.fatal "Sent KILL command by client. Dying.";
    (* when we exit, the dfind process will attempt to read from the broken
       pipe and then exit with SIGPIPE, so it is unnecessary to kill it
       explicitly *)
    die ()

  let autocomplete ~options command_context file_input =
    let path, content = match file_input with
      | ServerProt.FileName _ -> failwith "Not implemented"
      | ServerProt.FileContent (_, content) ->
          ServerProt.file_input_get_filename file_input, content
    in
    let state = Autocomplete_js.autocomplete_set_hooks () in
    let results =
      try
        let path = Loc.SourceFile path in
        let profiling, cx, parse_result =
          match Types_js.typecheck_contents ~options content path with
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
        Hh_logger.warn "Couldn't autocomplete%s" (Printexc.to_string exn);
        OK []
    in
    Autocomplete_js.autocomplete_unset_hooks ();
    results

  let check_file ~options ~force ~verbose file_input =
    let file = ServerProt.file_input_get_filename file_input in
    match file_input with
    | ServerProt.FileName _ -> failwith "Not implemented"
    | ServerProt.FileContent (_, content) ->
        let should_check =
          if force then
            true
          else
            let (_, docblock) =
              Parsing_service_js.get_docblock
                Docblock.max_tokens
                (Loc.SourceFile file)
                content
            in
            Docblock.is_flow docblock
        in
        if should_check then
          let file = Loc.SourceFile file in
          let checked = Types_js.typecheck_contents
            ~options ?verbose ~check_syntax:true content file in
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

  let infer_type ~options client_context (file_input, line, col, verbose, include_raw) =
    let file = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile file in
    let response = (try
      let content = ServerProt.file_input_get_content file_input in
      let profiling, cx = match Types_js.typecheck_contents ?verbose ~options content file with
      | profiling, Some cx, _, _ -> profiling, cx
      | _  -> failwith "Couldn't parse file" in
      let loc = mk_loc file line col in
      let result_str, data, loc, ground_t, possible_ts =
        let mk_data loc ty = [
          "loc", Reason.json_of_loc loc;
          "type", Debug_js.json_of_t ~depth:3 cx ty
        ] in
        Query_types.(match query_type cx loc with
        | FailureNoMatch ->
          "FAILURE_NO_MATCH", [],
          Loc.none, None, []
        | FailureUnparseable (loc, gt, possible_ts) ->
          "FAILURE_UNPARSEABLE", mk_data loc gt,
          loc, None, possible_ts
        | Success (loc, gt, possible_ts) ->
          "SUCCESS", mk_data loc gt,
          loc, Some gt, possible_ts
      ) in
      FlowEventLogger.type_at_pos_result
        ~client_context
        ~result_str
        ~json_data:(Hh_json.JSON_Object data)
        ~profiling;

      let ty, raw_type = match ground_t with
        | None -> None, None
        | Some t ->
            let ty = Some (Type_printer.string_of_t cx t) in
            let raw_type =
              if include_raw then
                Some (Debug_js.jstr_of_t ~size:50 ~depth:10 cx t)
              else
                None
            in
            ty, raw_type
      in
      let reasons =
        possible_ts
        |> List.map Type.reason_of_t
      in
      OK (loc, ty, raw_type, reasons)
    with exn ->
      let loc = mk_loc file line col in
      let err = (loc, spf "%s\n%s"
        (Printexc.to_string exn)
        (Printexc.get_backtrace ())) in
      Err err
    ) in
    response

  let dump_types ~options file_input include_raw strip_root =
    (* Print type using Flow type syntax *)
    let printer = Type_printer.string_of_t in
    (* Print raw representation of types as json; as it turns out, the
       json gets cut off at a specified depth, so pass the maximum
       possible depth to avoid that. *)
    let raw_printer c t =
      if include_raw
        then Some (Debug_js.jstr_of_t ~depth:max_int ~strip_root c t)
        else None
      in
    let file = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile file in
    let resp =
    (try
       let content = ServerProt.file_input_get_content file_input in
       let cx = match Types_js.typecheck_contents ~options content file with
       | _, Some cx, _, _ -> cx
       | _  -> failwith "Couldn't parse file" in
      OK (Query_types.dump_types printer raw_printer cx)
    with exn ->
      let loc = mk_loc file 0 0 in
      let err = (loc, Printexc.to_string exn) in
      Err err
    ) in
    resp

  let coverage ~options ~force file_input =
    let file = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile file in
    let resp =
    (try
      let content = ServerProt.file_input_get_content file_input in
      let should_check =
        if force then
          true
        else
          let (_, docblock) =
            Parsing_service_js.get_docblock Docblock.max_tokens file content in
          Docblock.is_flow docblock
      in
      let cx = match Types_js.typecheck_contents ~options content file with
      | _, Some cx, _, _ -> cx
      | _  -> failwith "Couldn't parse file" in
      let types = Query_types.covered_types cx in
      if should_check then
        OK types
      else
        OK (types |> List.map (fun (loc, _) -> (loc, false)))
    with exn ->
      let loc = mk_loc file 0 0 in
      let err = (loc, Printexc.to_string exn) in
      Err err
    ) in
    resp

  let parse_suggest_cmd file =
    let digits = "\\([0-9]+\\)" in
    let re = Printf.sprintf "\\(.*\\):%s:%s,%s:%s"
      digits digits digits digits in
    if Str.string_match (Str.regexp re) file 0
    then
      (Str.matched_group 1 file,
       List.map (fun i -> Str.matched_group i file) [2;3;4;5])
    else
      (file, [])

  (* NOTE: currently, not only returns list of annotations, but also rewrites
     file with annotations *)
  let suggest ~options =
    let suggest_for_file result_map file =
      (try
         let (file, region) = parse_suggest_cmd file in
         let file = Path.to_string (Path.make file) in
         let content = cat file in
         let file_loc = Loc.SourceFile file in
         let cx =
           match Types_js.typecheck_contents ~options content file_loc with
           | _, Some cx, _, _ -> cx
           | _  -> failwith "Couldn't parse file" in
         let lines = Str.split_delim (Str.regexp "\n") content in
         let insertions =
           Query_types.fill_types cx
           |> List.sort Pervasives.compare
           |> match region with
              | [] -> fun insertions -> insertions
              | [l1;c1;l2;c2] ->
                  let l1,c1,l2,c2 =
                    int_of_string l1,
                    int_of_string c1,
                    int_of_string l2,
                    int_of_string c2
                  in
                  List.filter (fun (l,c,_) ->
                    (l1,c1) <= (l,c) && (l,c) <= (l2,c2)
                  )
              | _ -> assert false
         in
         let new_content = Reason.do_patch lines insertions in
         let patch_content = Diff.diff_of_file_and_string file new_content in
         SMap.add file patch_content result_map
       with exn ->
         Hh_logger.warn
           "Could not fill types for %s\n%s"
           file
           (Printexc.to_string exn);
         result_map
      )

    in fun files ->
      List.fold_left suggest_for_file SMap.empty files

  (* NOTE: currently, not only returns list of annotations, but also writes a
     timestamped file with annotations *)
  let port = Port_service_js.port_files

  let find_module ~options (moduleref, filename) =
    let file = Loc.SourceFile filename in
    let metadata = Context.({ (metadata_of_options options) with
      checked = false;
    }) in
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
        let cache = new Context_cache.context_cache in
        let (flow_files, flow_file_cxs, non_flow_files, error) =
          List.fold_left (fun (flow_files, cxs, non_flow_files, error) file ->
            if error <> None then (flow_files, cxs, non_flow_files, error) else
            match file with
            | ServerProt.FileContent _ ->
              let error_msg = "This command only works with file paths." in
              let error =
                Some (ServerProt.GenFlowFile_UnexpectedError error_msg)
              in
              (flow_files, cxs, non_flow_files, error)
            | ServerProt.FileName file_path ->
              let src_file = Loc.SourceFile file_path in
              (* TODO: Use InfoHeap as the definitive way to detect @flow vs
               * non-@flow
               *)
              match cache#read_safe ~audit:Expensive.warn src_file with
              | None ->
                (flow_files, cxs, src_file::non_flow_files, error)
              | Some cx ->
                (src_file::flow_files, cx::cxs, non_flow_files, error)
          ) ([], [], [], None) files
        in
        begin match error with
        | Some e -> Utils_js.Err e
        | None ->
          try
            begin
              try List.iter (fun flow_file_cx ->
                Merge_service.merge_contents_context ~options cache flow_file_cx
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

            Utils_js.OK result_contents
          with exn -> Utils_js.Err (
            ServerProt.GenFlowFile_UnexpectedError (Printexc.to_string exn)
          )
        end
      end else
        Utils_js.Err (ServerProt.GenFlowFile_TypecheckError errors)
    in
    result

  let find_refs ~options (file_input, line, col) =
    let filename = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile filename in
    let loc = mk_loc file line col in
    let state = FindRefs_js.set_hooks loc in
    let result =
      try
        let content = ServerProt.file_input_get_content file_input in
        let cx = match Types_js.typecheck_contents ~options content file with
          | _, Some cx, _, _ -> cx
          | _  -> failwith "Couldn't parse file"
        in
        Some (FindRefs_js.result cx state)
      with exn ->
        Hh_logger.warn
          "Could not find refs for %s:%d:%d\n%s"
          filename line col
          (Printexc.to_string exn);
        None
    in
    FindRefs_js.unset_hooks ();
    result

  let get_def ~options command_context (file_input, line, col) =
    let filename = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile filename in
    let loc = mk_loc file line col in
    let state = GetDef_js.getdef_set_hooks loc in
    let result = try
      let content = ServerProt.file_input_get_content file_input in
      let profiling, cx = match Types_js.typecheck_contents ~options content file with
        | profiling, Some cx, _, _ -> profiling, cx
        | _  -> failwith "Couldn't parse file"
      in
      Some (GetDef_js.getdef_get_result
        profiling
        command_context
        ~options
        cx
        state
      )
    with exn ->
      Hh_logger.warn
        "Could not get definition for %s:%d:%d\n%s"
        filename line col
        (Printexc.to_string exn);
      None
    in
    GetDef_js.getdef_unset_hooks ();
    result

  let module_name_of_string ~options module_name_str =
    let path = Path.to_string (Path.make module_name_str) in
    if Files.is_flow_file ~options path
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

  let get_watch_paths options = Files.watched_paths options

  (* filter a set of updates coming from dfind and return
     a FilenameSet. updates may be coming in from
     the root, or an include path. *)
  let process_updates genv env updates =
    let options = genv.ServerEnv.options in
    let all_libs =
      let known_libs = env.ServerEnv.libs in
      let _, maybe_new_libs = Files.init options in
      SSet.union known_libs maybe_new_libs
    in
    let root = Options.root options in
    let config_path = Server_files_js.config_file root in
    let sroot = Path.to_string root in
    let want = Files.wanted ~options all_libs in

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
        Files.is_included options f)
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
      if Files.is_flow_file ~options f &&
        (* note: is_included may be expensive. check in-root match first. *)
        (String_utils.string_starts_with f sroot ||
          Files.is_included options f) &&
        (* removes excluded and lib files. the latter are already filtered *)
        want f
      then
        let filename = Files.filename_from_string ~options f in
        FilenameSet.add filename acc
      else acc
    ) updates FilenameSet.empty

  (* on notification, execute client commands or recheck files *)
  let recheck genv env updates =
    if FilenameSet.is_empty updates
    then env
    else begin
      let root = Options.root genv.ServerEnv.options in
      let tmp_dir = Options.temp_dir genv.ServerEnv.options in
      ignore(Lock.grab (Server_files_js.recheck_file ~tmp_dir root));
      let env = Types_js.recheck genv env ~updates in
      ignore(Lock.release (Server_files_js.recheck_file ~tmp_dir root));
      env
    end

  let respond genv env ~client ~msg =
    let env = ref env in
    let oc = client.oc in
    let marshal msg =
      Marshal.to_channel oc msg [];
      flush oc
    in
    let marshal_option = function
      | Some msg -> marshal msg
      | None -> ()
    in
    let options = genv.ServerEnv.options in
    let { ServerProt.client_logging_context; command; } = msg in
    begin match command with
    | ServerProt.AUTOCOMPLETE fn ->
        let results: ServerProt.autocomplete_response =
          autocomplete ~options client_logging_context fn
        in
        marshal results
    | ServerProt.CHECK_FILE (fn, verbose, graphml, force) ->
        let options = { options with Options.opt_output_graphml = graphml } in
        (check_file ~options ~force ~verbose fn: ServerProt.response)
          |> ServerProt.response_to_channel oc
    | ServerProt.COVERAGE (fn, force) ->
        (coverage ~options ~force fn: ServerProt.coverage_response)
          |> marshal
    | ServerProt.DUMP_TYPES (fn, format, strip_root) ->
        (dump_types ~options fn format strip_root: ServerProt.dump_types_response)
          |> marshal
    | ServerProt.ERROR_OUT_OF_DATE ->
        incorrect_hash oc
    | ServerProt.FIND_MODULE (moduleref, filename) ->
        (find_module ~options (moduleref, filename): filename option)
          |> marshal
    | ServerProt.FIND_REFS (fn, line, char) ->
        (find_refs ~options (fn, line, char): Loc.t list option)
          |> marshal_option
    | ServerProt.FORCE_RECHECK (files) ->
        Marshal.to_channel oc () [];
        flush oc;
        let updates = process_updates genv !env (Utils_js.set_of_list files) in
        env := recheck genv !env updates
    | ServerProt.GEN_FLOW_FILES files ->
        (gen_flow_files ~options !env files: ServerProt.gen_flow_file_response)
          |> marshal
    | ServerProt.GET_DEF (fn, line, char) ->
        (get_def ~options client_logging_context (fn, line, char): Loc.t option)
          |> marshal_option
    | ServerProt.GET_IMPORTS module_names ->
        get_imports ~options module_names
          |> marshal
    | ServerProt.INFER_TYPE (fn, line, char, verbose, include_raw) ->
        (infer_type ~options client_logging_context (fn, line, char, verbose, include_raw): ServerProt.infer_type_response)
          |> marshal
    | ServerProt.KILL ->
        die_nicely oc
    | ServerProt.PING ->
        ServerProt.response_to_channel oc ServerProt.PONG
    | ServerProt.PORT (files) ->
        (port files: ServerProt.port_response)
          |> marshal
    | ServerProt.STATUS client_root ->
        let status = get_status genv !env client_root in
        ServerProt.response_to_channel oc status;
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
        (suggest ~options files: string SMap.t)
          |> marshal
    | ServerProt.CONNECT ->
        let new_connections =
          Persistent_connection.add_client
            !env.connections
            client
            client_logging_context
        in
        env := {!env with connections = new_connections}
    end;
    !env

  let respond_to_persistent_client genv env client msg =
    let options = genv.ServerEnv.options in
    match msg with
      | Persistent_connection_prot.Subscribe ->
          let errorl, _ = collate_errors env.errors in
          let new_connections =
            Persistent_connection.subscribe_client env.connections client errorl
          in
          { env with connections = new_connections }
      | Persistent_connection_prot.Autocomplete file_input ->
          let client_logging_context = Persistent_connection.get_logging_context client in
          let results = autocomplete ~options client_logging_context file_input in
          let wrapped = Persistent_connection_prot.AutocompleteResult results in
          Persistent_connection.send_message wrapped client;
          env

  let should_close = function
    | { ServerProt.command = ServerProt.CONNECT; _ } -> false
    | _ -> true

  let handle_client genv env client =
    let msg = ServerProt.cmd_from_channel client.ic in
    let env = respond genv env ~client ~msg in
    if should_close msg then client.close ();
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
