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

  let preinit () =
    (* Do some initialization before creating workers, so that each worker is
     * forked with this information already available. Finding lib files is one
     * example *)
    let flow_options = OptionParser.parse () in
    Types_js.init_modes flow_options;
    (* Force flowlib files to be extracted and their location saved before workers
     * fork, so everyone can know about the same flowlib path. *)
    ignore (Flowlib.get_flowlib_root ());
    ignore (Flow_js.master_cx ());
    ignore (Flow_js.builtins ());
    Parsing_service_js.call_on_success SearchService_js.update

  let init genv env =
    if not (Options.is_check_mode genv.ServerEnv.options) then (
      (* write binary path and version to server log *)
      Hh_logger.log "executable=%s" (Sys_utils.executable_path ());
      Hh_logger.log "version=%s" FlowConfig.version);
    (* start the server *)
    let env = Types_js.server_init genv env in
    let files =
      ServerEnv.PathMap.fold (fun fn _ acc ->
        ServerEnv.PathSet.add fn acc
      ) env.ServerEnv.files_info ServerEnv.PathSet.empty in
    SearchService_js.update_from_master files;
    env

  let run_once_and_exit genv env =
    match env.ServerEnv.errorl with
      | [] -> exit 0
      | _ -> exit 2

  let incorrect_hash oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_OUT_OF_DATE;
    FlowEventLogger.out_of_date ();
    Printf.printf     "Status: Error\n";
    Printf.printf     "%s is out of date. Exiting.\n" name;
    exit 4

  let status_log env =
    if List.length (Types_js.get_errors ()) = 0
    then Printf.printf "Status: OK\n"
    else Printf.printf "Status: Error\n";
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
      Printf.printf "Status: Error\n";
      Printf.printf "server_dir=%s, client_dir=%s\n"
        (Path.to_string server_root)
        (Path.to_string client_root);
      Printf.printf "%s is not listening to the same directory. Exiting.\n"
        name;
      exit 5
    end;
    flush stdout;
    (* TODO: check status.directory *)
    status_log env;
    let errors = Types_js.get_errors () in
    FlowEventLogger.check_response errors;
    send_errorl errors oc

  let die_nicely genv oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_DYING;
    FlowEventLogger.killed ();
    Printf.printf "Status: Error\n";
    Printf.printf "Sent KILL command by client. Dying.\n";
    (match genv.ServerEnv.dfind with
    | Some handle -> Unix.kill (DfindLib.pid handle) Sys.sigterm;
    | None -> ()
    );
    die ()

  let autocomplete file_input oc =
    let path, content = match file_input with
      | ServerProt.FileName _ -> failwith "Not implemented"
      | ServerProt.FileContent (_, content) ->
          ServerProt.file_input_get_filename file_input, content
    in
    let state = Autocomplete_js.autocomplete_set_hooks () in
    let results =
      try
        let cx = (match Types_js.typecheck_contents content path with
          | Some cx, _ -> cx
          | _, errors  -> failwith "Couldn't parse file")
        in
        AutocompleteService_js.autocomplete_get_results cx state
      with exn ->
        prerr_endlinef "Couldn't autocomplete\n%s" (Printexc.to_string exn);
        []
    in
    Autocomplete_js.autocomplete_unset_hooks ();
    Marshal.to_channel oc results [];
    flush oc

  let check_file file_input oc =
    let file = ServerProt.file_input_get_filename file_input in
    let errors = match file_input with
    | ServerProt.FileName _ -> failwith "Not implemented"
    | ServerProt.FileContent (_, content) ->
        (match Types_js.typecheck_contents content file with
        | _, errors -> errors)
    in
    send_errorl (Errors_js.to_list errors) oc

  let mk_loc file line col =
    {
      Loc.
      source = Some file;
      start = { Loc.line; column = col; offset = 0; };
      _end = { Loc.line; column = col + 1; offset = 0; };
    }

  let infer_type (file_input, line, col) oc =
    let file = ServerProt.file_input_get_filename file_input in
    let (err, resp) =
    (try
       let content = ServerProt.file_input_get_content file_input in
       let cx = match Types_js.typecheck_contents content file with
       | Some cx, _ -> cx
       | _, errors  -> failwith "Couldn't parse file" in
      let loc = mk_loc file line col in
      let (loc, ground_t, possible_ts) = TI.query_type cx loc in
      let ty = match ground_t with
        | None -> None
        | Some t -> Some (Constraint_js.string_of_t cx t)
      in
      let reasons =
        possible_ts
        |> List.map Constraint_js.reason_of_t
      in
      (None, Some (loc, ty, reasons))
    with exn ->
      let loc = mk_loc file line col in
      let err = (loc, Printexc.to_string exn) in
      (Some err, None)
    ); in
    Marshal.to_channel oc (err, resp) [];
    flush oc

  let dump_types file_input oc =
    let file = ServerProt.file_input_get_filename file_input in
    let (err, resp) =
    (try
      let content = ServerProt.file_input_get_content file_input in
      let cx = match Types_js.typecheck_contents content file with
      | Some cx, _ -> cx
      | _, errors  -> failwith "Couldn't parse file" in
      (None, Some (TI.dump_types cx))
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
    |> Unix.system |> ignore;
    cat patch_file

  (* NOTE: currently, not only returns list of annotations, but also rewrites
     file with annotations *)
  let suggest =
    let suggest_for_file result_map file =
      (try
         let (file, region) = parse_suggest_cmd file in
         let file = Path.to_string (Path.make file) in
         let cx = Types_js.merge_strict_file file in
         let content = cat file in
         let lines = Str.split_delim (Str.regexp "\n") content in
         let insertions =
           TI.fill_types cx
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
         prerr_endlinef
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
        let ast = Parsing_service_js.get_ast_unsafe file in
        let content = cat file in
        let lines = Str.split_delim (Str.regexp "\n") content in
        let insertions = Comments_js.meta_program ast in
        let insertions = List.sort Pervasives.compare insertions in
        let new_content = Reason_js.do_patch lines insertions in
        let patch_content = patch file new_content in
        SMap.add file patch_content result_map
      with exn ->
        prerr_endlinef
          "Could not port docblock-style annotations for %s\n%s"
          file
          ((Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ()));
        result_map
      )

    in fun files oc ->
      let patches = List.fold_left port_for_file SMap.empty files in
      Marshal.to_channel oc patches [];
      flush oc

  let find_modules module_names oc =
    let response =
      List.fold_left (fun module_map module_name ->
        match Module_js.get_module_file module_name with
        | Some file ->
            SMap.add module_name file module_map
        | None ->
            module_map
        ) SMap.empty module_names
    in
    Marshal.to_channel oc response [];
    flush oc

  let get_def (file_input, line, col) oc =
    let file = ServerProt.file_input_get_filename file_input in
    let loc = mk_loc file line col in
    let state = GetDef_js.getdef_set_hooks loc in
    (try
      let content = ServerProt.file_input_get_content file_input in
      let cx = match Types_js.typecheck_contents content file with
        | Some cx, _ -> cx
        | _, errors  -> failwith "Couldn't parse file"
      in
      let result = GetDef_js.getdef_get_result cx state in
      Marshal.to_channel oc result []
    with exn ->
      prerr_endlinef
        "Could not get definition for %s:%d:%d\n%s"
        file line col
        (Printexc.to_string exn)
    );
    GetDef_js.getdef_unset_hooks ();
    flush oc

  let get_importers module_names oc =
    let add_to_results map module_name =
      match Module_js.get_reverse_imports module_name with
      | Some references ->
          SMap.add module_name references map
      | None -> map
    in
    let results = List.fold_left add_to_results
                  SMap.empty module_names in
    Marshal.to_channel oc results [];
    flush oc

  let get_imports module_names oc =
    let add_to_results (map, non_flow) module_name =
      match (Module_js.get_module_file module_name) with
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
          (SMap.add module_name (requirements, req_locs) map, non_flow)
        else
          (map, SSet.add module_name non_flow)
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
    match msg with
    | ServerProt.AUTOCOMPLETE fn ->
        autocomplete fn oc
    | ServerProt.CHECK_FILE fn ->
        check_file fn oc
    | ServerProt.DUMP_TYPES fn ->
        dump_types fn oc
    | ServerProt.ERROR_OUT_OF_DATE ->
        incorrect_hash oc
    | ServerProt.FIND_MODULES module_names ->
        find_modules module_names oc
    | ServerProt.GET_DEF (fn, line, char) ->
        get_def (fn, line, char) oc
    | ServerProt.GET_IMPORTERS module_names ->
        get_importers module_names oc
    | ServerProt.GET_IMPORTS module_names ->
        get_imports module_names oc
    | ServerProt.INFER_TYPE (fn, line, char) ->
        infer_type (fn, line, char) oc
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
    let is_flow_file =
      let config_path = FlowConfig.fullpath root in
      fun f -> Files_js.is_flow_file f || f = config_path
    in
    let config = FlowConfig.get root in
    let sroot = Path.to_string root in
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
      let diff_js = ServerEnv.PathSet.fold (fun x a ->
        SSet.add (Path.to_string x) a) diff_js SSet.empty in
      (* TEMP: if library files change, stop the server *)
      let modified_lib_files = SSet.inter diff_js (Files_js.get_lib_files ()) in
      if not (SSet.is_empty modified_lib_files)
      then begin
        Printf.printf "Status: Error\n%!";
        SSet.iter (Printf.printf "Modified lib file: %s\n%!") modified_lib_files;
        Printf.printf
          "%s is out of date. Exiting.\n%!"
          name;
        exit 4
      end;
      let server_env = Types_js.recheck genv env diff_js in
      SearchService_js.update_from_master updates;
      server_env
    end

  let post_recheck_hook _genv _old_env _new_env _updates = ()

end
