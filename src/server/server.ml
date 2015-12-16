(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module type OPTION_PARSER = sig
  val parse : unit -> Options.options
end

module TI = Type_inference_js
module Server = ServerFunctors

module FlowProgram (OptionParser : OPTION_PARSER) : Server.SERVER_PROGRAM =
struct
  open Utils
  open Utils_js
  open Sys_utils
  open ServerEnv
  open ServerUtils

  let name = "flow server"

  let config_path root = Path.concat root ".flowconfig"

  (* This determines whether the current config file is compatible with the
   * config that this server was initialized with. Returning false means
   * that any change in .flowconfig results in a server restart. *)
  let validate_config _config = false

  let parse_options = OptionParser.parse

  let preinit flow_options =
    (* Do some initialization before creating workers, so that each worker is
     * forked with this information already available. Finding lib files is one
     * example *)
    Types_js.init_modes flow_options;
    ignore (Flow_js.master_cx ());
    Parsing_service_js.call_on_success SearchService_js.update

  let init genv env =
    (* write binary path and version to server log *)
    Flow_logger.log "executable=%s" (Sys_utils.executable_path ());
    Flow_logger.log "version=%s" FlowConfig.version;
    (* start the server *)
    let timing, env = Types_js.server_init genv env in
    let files =
      ServerEnv.PathMap.fold (fun fn _ acc ->
        ServerEnv.PathSet.add fn acc
      ) env.ServerEnv.files_info ServerEnv.PathSet.empty in
    SearchService_js.update_from_master files;
    timing, env

  let run_once_and_exit genv env =
    match env.ServerEnv.errorl with
      | [] -> FlowExitStatus.(exit Ok)
      | _ -> FlowExitStatus.(exit Type_error)

  let incorrect_hash oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_OUT_OF_DATE;
    FlowEventLogger.out_of_date ();
    Flow_logger.log     "Status: Error";
    Flow_logger.log     "%s is out of date. Exiting." name;
    FlowExitStatus.(exit Server_out_of_date)

  let status_log env =
    if List.length (Types_js.get_errors ()) = 0
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
    (* TODO: check status.directory *)
    status_log env;
    let errors = Types_js.get_errors () in
    FlowEventLogger.status_response (Errors_js.json_of_errors errors);
    send_errorl errors oc

  let die_nicely genv oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_DYING;
    FlowEventLogger.killed ();
    Flow_logger.log "Status: Error";
    Flow_logger.log "Sent KILL command by client. Dying.";
    (match genv.ServerEnv.dfind with
    | Some handle -> Unix.kill (DfindLib.pid handle) Sys.sigterm;
    | None -> ()
    );
    die ()

  let autocomplete command_context file_input oc =
    let path, content = match file_input with
      | ServerProt.FileName _ -> failwith "Not implemented"
      | ServerProt.FileContent (_, content) ->
          ServerProt.file_input_get_filename file_input, content
    in
    let state = Autocomplete_js.autocomplete_set_hooks () in
    let results =
      try
        let path = Loc.SourceFile path in
        let timing, cx, parse_result = (match Types_js.typecheck_contents content path with
          | timing, Some cx, _, parse_result -> timing, cx, parse_result
          | _  -> failwith "Couldn't parse file")
        in
        AutocompleteService_js.autocomplete_get_results
          timing
          command_context
          cx
          state
          parse_result
      with exn ->
        Flow_logger.log "Couldn't autocomplete%s" (Printexc.to_string exn);
        []
    in
    Autocomplete_js.autocomplete_unset_hooks ();
    Marshal.to_channel oc results [];
    flush oc

  let check_file file_input verbose oc =
    let file = ServerProt.file_input_get_filename file_input in
    let errors = match file_input with
    | ServerProt.FileName _ -> failwith "Not implemented"
    | ServerProt.FileContent (_, content) ->
        let file = Loc.SourceFile file in
        (match Types_js.typecheck_contents ?verbose content file with
        | _, _, errors, _ -> errors)
    in
    send_errorl (Errors_js.to_list errors) oc

  let mk_loc file line col =
    {
      Loc.
      source = Some file;
      start = { Loc.line; column = col; offset = 0; };
      _end = { Loc.line; column = col + 1; offset = 0; };
    }

  let infer_type (file_input, line, col, include_raw) oc =
    let file = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile file in
    let (err, resp) =
    (try
      let content = ServerProt.file_input_get_content file_input in
      let cx = match Types_js.typecheck_contents content file with
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
      (None, Some (loc, ty, raw_type, reasons))
    with exn ->
      let loc = mk_loc file line col in
      let err = (loc, Printexc.to_string exn) in
      (Some err, None)
    ); in
    Marshal.to_channel oc (err, resp) [];
    flush oc

  let dump_types file_input include_raw oc =
    (* Print type using Flow type syntax *)
    let printer = Type_printer.string_of_t in
    (* Print raw representation of types as json; as it turns out, the
       json gets cut off at a specified depth, so pass the maximum
       possible depth to avoid that. *)
    let raw_printer c t =
      if include_raw
        then Some (Debug_js.jstr_of_t ~depth:max_int c t)
        else None
      in
    let file = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile file in
    let (err, resp) =
    (try
       let content = ServerProt.file_input_get_content file_input in
       let cx = match Types_js.typecheck_contents content file with
       | _, Some cx, _, _ -> cx
       | _  -> failwith "Couldn't parse file" in
      (None, Some (Query_types.dump_types printer raw_printer cx))
    with exn ->
      let loc = mk_loc file 0 0 in
      let err = (loc, Printexc.to_string exn) in
      (Some err, None)
    ) in
    Marshal.to_channel oc (err, resp) [];
    flush oc

  let write_file file str =
    let oc = open_out file in
    output_string oc str;
    close_out oc

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

  let patch file new_content =
    let new_file = Filename.temp_file "" "" in
    write_file new_file new_content;
    let patch_file = Filename.temp_file "" "" in
    spf "diff -u --label old --label new %s %s > %s"
      file new_file patch_file
    |> Sys.command |> ignore;
    cat patch_file

  (* NOTE: currently, not only returns list of annotations, but also rewrites
     file with annotations *)
  let suggest =
    let suggest_for_file result_map file =
      (try
         let (file, region) = parse_suggest_cmd file in
         let file = Path.to_string (Path.make file) in
         let content = cat file in
         let file_loc = Loc.SourceFile file in
         let cx = match Types_js.typecheck_contents content file_loc with
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
         let new_content = Reason_js.do_patch lines insertions in
         let patch_content = patch file new_content in
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
  let port =
    let port_for_file result_map file =
      (try
        let file = Path.to_string (Path.make file) in
        let ast = Parsing_service_js.get_ast_unsafe (Loc.SourceFile file) in
        let content = cat file in
        let lines = Str.split_delim (Str.regexp "\n") content in
        let insertions = Comments_js.meta_program ast in
        let insertions = List.sort Pervasives.compare insertions in
        let new_content = Reason_js.do_patch lines insertions in
        let patch_content = patch file new_content in
        SMap.add file patch_content result_map
      with exn ->
        Flow_logger.log
          "Could not port docblock-style annotations for %s\n%s"
          file
          ((Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ()));
        result_map
      )

    in fun files oc ->
      let patches = List.fold_left port_for_file SMap.empty files in
      Marshal.to_channel oc patches [];
      flush oc

  let find_module (moduleref, filename) oc =
    let file = Loc.SourceFile filename in
    let module_name = Module_js.imported_module file moduleref in
    let response: filename option = Module_js.get_module_file module_name in
    Marshal.to_channel oc response [];
    flush oc

  let get_def (file_input, line, col) oc =
    let filename = ServerProt.file_input_get_filename file_input in
    let file = Loc.SourceFile filename in
    let loc = mk_loc file line col in
    let state = GetDef_js.getdef_set_hooks loc in
    (try
      let content = ServerProt.file_input_get_content file_input in
      let cx = match Types_js.typecheck_contents content file with
        | _, Some cx, _, _ -> cx
        | _  -> failwith "Couldn't parse file"
      in
      let result = GetDef_js.getdef_get_result cx state in
      Marshal.to_channel oc result []
    with exn ->
      Flow_logger.log
        "Could not get definition for %s:%d:%d\n%s"
        filename line col
        (Printexc.to_string exn)
    );
    GetDef_js.getdef_unset_hooks ();
    flush oc

  let module_name_of_string module_name_str =
    let path = Path.to_string (Path.make module_name_str) in
    if Files_js.is_flow_file path then Modulename.Filename (Loc.SourceFile path)
    else Modulename.String module_name_str

  let get_importers module_names oc =
    let add_to_results map module_name_str =
      let module_name = module_name_of_string module_name_str in
      match Module_js.get_reverse_imports module_name with
      | Some references ->
          SMap.add module_name_str references map
      | None -> map
    in
    let results = List.fold_left add_to_results
                  SMap.empty module_names in
    Marshal.to_channel oc results [];
    flush oc

  let get_imports module_names oc =
    let add_to_results (map, non_flow) module_name_str =
      let module_name = module_name_of_string module_name_str in
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

  let not_supported oc =
    output_string oc "Not supported\n";
    flush oc

  let search query oc =
    let results = SearchService_js.query query in
    Marshal.to_channel oc results [];
    flush oc

  let respond genv env ~client ~msg =
    let oc = client.oc in
    let { ServerProt.client_logging_context; command; } = msg in
    match command with
    | ServerProt.AUTOCOMPLETE fn ->
        autocomplete client_logging_context fn oc
    | ServerProt.CHECK_FILE (fn, verbose) ->
        check_file fn verbose oc
    | ServerProt.DUMP_TYPES (fn, format) ->
        dump_types fn format oc
    | ServerProt.ERROR_OUT_OF_DATE ->
        incorrect_hash oc
    | ServerProt.FIND_MODULE (moduleref, filename) ->
        find_module (moduleref, filename) oc
    | ServerProt.GET_DEF (fn, line, char) ->
        get_def (fn, line, char) oc
    | ServerProt.GET_IMPORTERS module_names ->
        get_importers module_names oc
    | ServerProt.GET_IMPORTS module_names ->
        get_imports module_names oc
    | ServerProt.INFER_TYPE (fn, line, char, include_raw) ->
        infer_type (fn, line, char, include_raw) oc
    | ServerProt.KILL ->
        die_nicely genv oc
    | ServerProt.PING ->
        ServerProt.response_to_channel oc ServerProt.PONG
    | ServerProt.PORT (files) ->
        port files oc
    | ServerProt.SEARCH query ->
        search query oc
    | ServerProt.STATUS client_root ->
        print_status genv env client_root oc
    | ServerProt.SUGGEST (files) ->
        suggest files oc

  let handle_client genv env client =
    let msg = ServerProt.cmd_from_channel client.ic in
    respond genv env ~client ~msg;
    client.close ()

  let get_watch_paths options =
    let config = FlowConfig.get (Options.root options) in
    config.FlowConfig.include_stems

  (* filter a set of updates coming from dfind and return
     a ServerEnv.PathSet. updates may be coming in from
     the root, or an include path. *)
  let process_updates genv env updates =
    let root = Options.root (genv.ServerEnv.options) in
    let config = FlowConfig.get root in
    let is_flow_file =
      let config_path = FlowConfig.fullpath root in
      fun f -> Files_js.is_flow_file f || f = config_path
    in
    let sroot = Path.to_string root in
    let want = Files_js.wanted config in
    let modified_packages = SSet.filter (fun f ->
      (str_starts_with f sroot || FlowConfig.is_included config f)
      && (Filename.basename f) = "package.json" && want f
    ) updates in
    if not (SSet.is_empty modified_packages)
    then begin
      Flow_logger.log "Status: Error";
      SSet.iter (Flow_logger.log "Modified package: %s") modified_packages;
      Flow_logger.log
        "%s is out of date. Exiting.\n%!"
        name;
      exit 4
    end;
    SSet.fold (fun f acc ->
      if is_flow_file f &&
        (* note: is_included may be expensive. check in-root match first. *)
        (str_starts_with f sroot || FlowConfig.is_included config f)
      then ServerEnv.PathSet.add (Path.make f) acc
      else acc
    ) updates ServerEnv.PathSet.empty

  (* XXX: can some of the logic in process_updates be moved here? *)
  let should_recheck _update = true

  (* on notification, execute client commands or recheck files *)
  let recheck genv env updates =
    let diff_js = updates in
    if ServerEnv.PathSet.is_empty diff_js
    then env
    else begin
      SearchService_js.clear updates;
      let all_libs = Files_js.get_lib_fileset () in
      let libs, files = ServerEnv.PathSet.fold (fun x (libs, files) ->
        let file = Path.to_string x in
        if SSet.mem file all_libs then SSet.add file libs, files
        else libs, FilenameSet.add (Loc.SourceFile file) files
      ) diff_js (SSet.empty, FilenameSet.empty) in
      (* TEMP: if library files change, stop the server *)
      if not (SSet.is_empty libs)
      then begin
        Flow_logger.log "Status: Error";
        SSet.iter (Flow_logger.log "Modified lib file: %s") libs;
        Flow_logger.log
          "%s is out of date. Exiting.\n%!"
          name;
        FlowExitStatus.(exit Server_out_of_date)
      end;
      let server_env = Types_js.recheck genv env files in
      SearchService_js.update_from_master updates;
      server_env
    end
end
