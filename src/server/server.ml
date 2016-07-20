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

module FlowProgram : Server.SERVER_PROGRAM = struct
  open Utils_js
  open Sys_utils
  open ServerEnv
  open ServerUtils

  let name = "flow server"

  let preinit options =
    (* Do some initialization before creating workers, so that each worker is
     * forked with this information already available. *)
    ignore (Init_js.get_master_cx options)

  let init genv =
    (* write binary path and version to server log *)
    Flow_logger.log "executable=%s" (Sys_utils.executable_path ());
    Flow_logger.log "version=%s" FlowConfig.version;
    (* start the server *)
    Types_js.server_init genv

  let run_once_and_exit env =
    match env.ServerEnv.errorl with
      | [] -> FlowExitStatus.(exit No_error)
      | _ -> FlowExitStatus.(exit Type_error)

  let incorrect_hash oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_OUT_OF_DATE;
    FlowEventLogger.out_of_date ();
    Flow_logger.log     "Status: Error";
    Flow_logger.log     "%s is out of date. Exiting." name;
    FlowExitStatus.(exit Server_out_of_date)

  let status_log errors =
    if List.length errors = 0
    then Flow_logger.log "Status: OK"
    else Flow_logger.log "Status: Error";
    flush stdout

  let send_errorl el oc =
    if el = []
    then
      ServerProt.response_to_channel oc ServerProt.NO_ERRORS
    else begin
      ServerProt.response_to_channel oc (ServerProt.ERRORS el);
    end;
    flush oc

  let print_status genv env client_root oc =
    let server_root = Options.root genv.options in
    if server_root <> client_root
    then begin
      let msg = ServerProt.DIRECTORY_MISMATCH {
        ServerProt.server=server_root;
        ServerProt.client=client_root
      } in
      ServerProt.response_to_channel oc msg;
      Flow_logger.log "Status: Error";
      Flow_logger.log "server_dir=%s, client_dir=%s"
        (Path.to_string server_root)
        (Path.to_string client_root);
      Flow_logger.log "%s is not listening to the same directory. Exiting."
        name;
      FlowExitStatus.(exit Server_client_directory_mismatch)
    end;
    flush stdout;
    let errors = env.ServerEnv.errorl in
    (* TODO: check status.directory *)
    status_log errors;
    FlowEventLogger.status_response (Errors.json_of_errors errors);
    send_errorl errors oc

  let die_nicely genv oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_DYING;
    FlowEventLogger.killed ();
    Flow_logger.log "Status: Error";
    Flow_logger.log "Sent KILL command by client. Dying.";
    (match genv.ServerEnv.dfind with
    | Some handle -> Sys_utils.terminate_process (DfindLib.pid handle);
    | None -> ()
    );
    die ()

  let autocomplete ~options command_context file_input oc =
    let path, content = match file_input with
      | ServerProt.FileName _ -> failwith "Not implemented"
      | ServerProt.FileContent (_, content) ->
          ServerProt.file_input_get_filename file_input, content
    in
    let state = Autocomplete_js.autocomplete_set_hooks () in
    let results =
      try
        let path = Loc.SourceFile path in
        let timing, cx, parse_result =
          match Types_js.typecheck_contents ~options content path with
          | timing, Some cx, _, parse_result -> timing, cx, parse_result
          | _  -> failwith "Couldn't parse file"
        in
        AutocompleteService_js.autocomplete_get_results
          timing
          command_context
          cx
          state
          parse_result
      with exn ->
        Flow_logger.log "Couldn't autocomplete%s" (Printexc.to_string exn);
        OK []
    in
    Autocomplete_js.autocomplete_unset_hooks ();
    Marshal.to_channel oc (results : ServerProt.autocomplete_response) [];
    flush oc

  let check_file ~options file_input verbose respect_pragma oc =
    let file = ServerProt.file_input_get_filename file_input in
    let errors = match file_input with
    | ServerProt.FileName _ -> failwith "Not implemented"
    | ServerProt.FileContent (_, content) ->
        let should_check =
          if not respect_pragma then
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
        begin if should_check then
          let file = Loc.SourceFile file in
          (match Types_js.typecheck_contents ~options ?verbose content file with
          | _, _, errors, _ -> errors)
        else
          Errors.ErrorSet.empty
        end
    in
    send_errorl (Errors.to_list errors) oc

  let mk_loc file line col =
    {
      Loc.
      source = Some file;
      start = { Loc.line; column = col; offset = 0; };
      _end = { Loc.line; column = col + 1; offset = 0; };
    }

  let infer_type ~options (file_input, line, col, verbose, include_raw) oc =
    let file = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile file in
    let response = (try
      let content = ServerProt.file_input_get_content file_input in
      let cx = match Types_js.typecheck_contents ?verbose ~options content file with
      | _, Some cx, _, _ -> cx
      | _  -> failwith "Couldn't parse file" in
      let loc = mk_loc file line col in
      let (loc, ground_t, possible_ts) = Query_types.query_type cx loc in
      let ty, raw_type = match ground_t with
        | None -> None, None
        | Some t ->
            let ty = Some (Type_printer.string_of_t cx t) in
            let raw_type =
              if include_raw then
                Some (Debug_js.jstr_of_t ~depth:10 cx t)
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
    Marshal.to_channel oc (response: ServerProt.infer_type_response) [];
    flush oc

  let dump_types ~options file_input include_raw strip_root oc =
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
    Marshal.to_channel oc (resp : ServerProt.dump_types_response) [];
    flush oc

  let coverage ~options file_input oc =
    let file = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile file in
    let resp =
    (try
       let content = ServerProt.file_input_get_content file_input in
       let cx = match Types_js.typecheck_contents ~options content file with
       | _, Some cx, _, _ -> cx
       | _  -> failwith "Couldn't parse file" in
      OK (Query_types.covered_types cx)
    with exn ->
      let loc = mk_loc file 0 0 in
      let err = (loc, Printexc.to_string exn) in
      Err err
    ) in
    Marshal.to_channel oc (resp : ServerProt.coverage_response) [];
    flush oc

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
         Flow_logger.log
           "Could not fill types for %s\n%s"
           file
           (Printexc.to_string exn);
         result_map
      )

    in fun files oc ->
      let suggestions = List.fold_left suggest_for_file SMap.empty files in
      Marshal.to_channel oc suggestions [];
      flush oc

  (* NOTE: currently, not only returns list of annotations, but also writes a
     timestamped file with annotations *)
  let port files oc =
    let patches = Port_service_js.port_files files in
    Marshal.to_channel oc patches [];
    flush oc

  let find_module ~options (moduleref, filename) oc =
    let file = Loc.SourceFile filename in
    let metadata = Context.({ (metadata_of_options options) with
      checked = false;
    }) in
    let cx = Context.make metadata file (Modulename.Filename file) in
    let loc = {Loc.none with Loc.source = Some file;} in
    let module_name = Module_js.imported_module ~options cx loc moduleref in
    let response: filename option = Module_js.get_module_file module_name in
    Marshal.to_channel oc response [];
    flush oc

  let get_def ~options (file_input, line, col) oc =
    let filename = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile filename in
    let loc = mk_loc file line col in
    let state = GetDef_js.getdef_set_hooks loc in
    (try
      let content = ServerProt.file_input_get_content file_input in
      let cx = match Types_js.typecheck_contents ~options content file with
        | _, Some cx, _, _ -> cx
        | _  -> failwith "Couldn't parse file"
      in
      let result = GetDef_js.getdef_get_result ~options cx state in
      Marshal.to_channel oc result []
    with exn ->
      Flow_logger.log
        "Could not get definition for %s:%d:%d\n%s"
        filename line col
        (Printexc.to_string exn)
    );
    GetDef_js.getdef_unset_hooks ();
    flush oc

  let module_name_of_string ~options module_name_str =
    let path = Path.to_string (Path.make module_name_str) in
    if Files.is_flow_file ~options path
    then Modulename.Filename (Loc.SourceFile path)
    else Modulename.String module_name_str

  let get_importers ~options module_names oc =
    let add_to_results map module_name_str =
      let module_name = module_name_of_string ~options module_name_str in
      match Module_js.get_reverse_imports module_name with
      | Some references ->
          SMap.add module_name_str references map
      | None -> map
    in
    let results = List.fold_left add_to_results
                  SMap.empty module_names in
    Marshal.to_channel oc results [];
    flush oc

  let get_imports ~options module_names oc =
    let add_to_results (map, non_flow) module_name_str =
      let module_name = module_name_of_string ~options module_name_str in
      match Module_js.get_module_file module_name with
      | Some file ->
        (* We do not process all modules which are stored in our module
         * database. In case we do not process a module its requirements
         * are not kept track of. To avoid confusing results we notify the
         * client that these modules have not been processed.
         *)
        let { Module_js.required = requirements; require_loc = req_locs;
              checked; _ } = Module_js.get_module_info file in
        if checked
        then
          (SMap.add module_name_str (requirements, req_locs) map, non_flow)
        else
          (map, SSet.add module_name_str non_flow)
      | None ->
        (* We simply ignore non existent modules *)
        (map, non_flow)
    in let results = List.fold_left add_to_results
                     (SMap.empty, SSet.empty) module_names in
    (* Our result is a tuple. The first element is a map from module names to
     * modules imported by them and their locations of import. The second
     * element is a set of modules which are not marked for processing by
     * flow. *)
    Marshal.to_channel oc results [];
    flush oc

  let get_watch_paths options = Path_matcher.stems (Options.includes options)

  (* filter a set of updates coming from dfind and return
     a FilenameSet. updates may be coming in from
     the root, or an include path. *)
  let process_updates genv env updates =
    let all_libs = env.ServerEnv.libs in
    let options = genv.ServerEnv.options in
    let root = Options.root options in
    let config_path = Server_files_js.config_file root in
    let sroot = Path.to_string root in
    let want = Files.wanted ~options all_libs in

    (* Die if the .flowconfig changed *)
    if SSet.mem config_path updates then begin
      Flow_logger.log "Status: Error";
      Flow_logger.log
        "%s changed in an incompatible way. Exiting.\n%!"
        config_path;
      FlowExitStatus.(exit Server_out_of_date)
    end;

    (* Die if a package.json changed *)
    let modified_packages = SSet.filter (fun f ->
      (String_utils.string_starts_with f sroot ||
        Files.is_included options f)
      && (Filename.basename f) = "package.json" && want f
    ) updates in
    if not (SSet.is_empty modified_packages)
    then begin
      Flow_logger.log "Status: Error";
      SSet.iter (Flow_logger.log "Modified package: %s") modified_packages;
      Flow_logger.log
        "Packages changed in an incompatible way. Exiting.\n%!";
      FlowExitStatus.(exit Server_out_of_date)
    end;

    (* Die if a lib file changed *)
    let flow_typed_path = Path.to_string (Files.get_flowtyped_path root) in
    let libs = updates |> SSet.filter (fun x ->
      SSet.mem x all_libs || x = flow_typed_path
    ) in
    if not (SSet.is_empty libs)
    then begin
      Flow_logger.log "Status: Error";
      SSet.iter (Flow_logger.log "Modified lib file: %s") libs;
      Flow_logger.log
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
        let filename = Files.filename_from_string f in
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
      let env = Types_js.recheck genv env updates in
      ignore(Lock.release (Server_files_js.recheck_file ~tmp_dir root));
      env
    end

  let respond genv env ~client ~msg =
    let env = ref env in
    let oc = client.oc in
    let options = genv.ServerEnv.options in
    let { ServerProt.client_logging_context; command; } = msg in
    begin match command with
    | ServerProt.AUTOCOMPLETE fn ->
        autocomplete ~options client_logging_context fn oc
    | ServerProt.CHECK_FILE (fn, verbose, respect_pragma) ->
        check_file ~options fn verbose respect_pragma oc
    | ServerProt.COVERAGE (fn) ->
        coverage ~options fn oc
    | ServerProt.DUMP_TYPES (fn, format, strip_root) ->
        dump_types ~options fn format strip_root oc
    | ServerProt.ERROR_OUT_OF_DATE ->
        incorrect_hash oc
    | ServerProt.FIND_MODULE (moduleref, filename) ->
        find_module ~options (moduleref, filename) oc
    | ServerProt.FORCE_RECHECK (files) ->
        Marshal.to_channel oc () [];
        flush oc;
        let updates = process_updates genv !env (Utils_js.set_of_list files) in
        env := recheck genv !env updates
    | ServerProt.GET_DEF (fn, line, char) ->
        get_def ~options (fn, line, char) oc
    | ServerProt.GET_IMPORTERS module_names ->
        get_importers ~options module_names oc
    | ServerProt.GET_IMPORTS module_names ->
        get_imports ~options module_names oc
    | ServerProt.INFER_TYPE (fn, line, char, verbose, include_raw) ->
        infer_type ~options (fn, line, char, verbose, include_raw) oc
    | ServerProt.KILL ->
        die_nicely genv oc
    | ServerProt.PING ->
        ServerProt.response_to_channel oc ServerProt.PONG
    | ServerProt.PORT (files) ->
        port files oc
    | ServerProt.STATUS client_root ->
        print_status genv !env client_root oc
    | ServerProt.SUGGEST (files) ->
        suggest ~options files oc
    end;
    !env

  let handle_client genv env client =
    let msg = ServerProt.cmd_from_channel client.ic in
    let env = respond genv env ~client ~msg in
    client.close ();
    env

end
