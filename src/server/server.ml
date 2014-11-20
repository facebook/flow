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
  val parse : unit -> ServerArgs.options
  val get_flow_options : unit -> Types_js.options
end

module TI = Type_inference_js
module Server = ServerFunctors

module FlowProgram (OptionParser : OPTION_PARSER) : Server.SERVER_PROGRAM =
struct
  open Utils
  open ServerEnv

  let name = "flow server"

  let parse_options = OptionParser.parse

  let get_errors _ = Types_js.get_errors ()

  let preinit () =
    (* Force flowlib files to be extracted and their location saved before workers
     * fork, so everyone can know about the same flowlib path. *)
    ignore (Flowlib.get_flowlib_root ())

  let init genv env =
    let flow_options = OptionParser.get_flow_options () in
    let env = Types_js.server_init genv env flow_options in
    env

  let run_once_and_exit genv env =
    match env.ServerEnv.errorl with
      | [] -> exit 0
      | _ -> exit 2

  let marshal _ = ()

  let unmarshal _ = ()

  let incorrect_hash oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_OUT_OF_DATE;
    EventLogger.out_of_date ();
    Printf.printf     "Status: Error\n";
    Printf.printf     "%s is out of date. Exiting.\n" name;
    exit 4

  let status_log env =
    if List.length (get_errors env) = 0
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
    let server_root = ServerArgs.root genv.options in
    if not (Path.equal server_root client_root)
    then begin
      let msg = ServerProt.DIRECTORY_MISMATCH {
        ServerProt.server=server_root;
        ServerProt.client=client_root
      } in
      ServerProt.response_to_channel oc msg;
      Printf.printf "Status: Error\n";
      Printf.printf "server_dir=%s, client_dir=%s\n"
        (Path.string_of_path server_root)
        (Path.string_of_path client_root);
      Printf.printf "%s is not listening to the same directory. Exiting.\n"
        name;
      exit 5
    end;
    flush stdout;
    (* TODO: check status.directory *)
    status_log env;
    let errors = get_errors env in
    EventLogger.check_response errors;
    send_errorl errors oc

  let die_nicely oc =
    ServerProt.response_to_channel oc ServerProt.SERVER_DYING;
    EventLogger.killed ();
    Printf.printf "Status: Error\n";
    Printf.printf "Sent KILL command by client. Dying.\n";
    (match !ServerDfind.dfind_pid with
    | Some pid -> Unix.kill pid Sys.sigterm;
    | None -> failwith "Dfind died before we could kill it"
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
        let cx = (match Types_js.typecheck_contents
                                 content path true with
          | Some cx, _ -> cx
          | _, errors  -> failwith "Couldn't parse file")
        in
        AutocompleteService_js.autocomplete_get_results cx state
      with exn ->
        prerr_endline
          (spf "Couldn't autocomplete\n%s"
              (Printexc.to_string exn)
          );
        []
    in
    Autocomplete_js.autocomplete_unset_hooks ();
    Marshal.to_channel oc results [];
    flush oc

  (* our infer implementation uses a different type for file_input, we stub
     the (unused) public interface to avoid issues *)
  let infer (file_input, line, col) oc =
    ()

  let infer_type (file_input, line, col) oc =
    let file = ServerProt.file_input_get_filename file_input in
    let (pos, t, reasons) =
    (try
      let cx = match file_input with
        | ServerProt.FileName file ->
            Types_js.merge_strict_file file
        | ServerProt.FileContent (_, content) ->
            (match Types_js.typecheck_contents
                            content file false with
            | Some cx, _ -> cx
            | _, errors  -> failwith "Couldn't parse file") in
      let file = cx.Constraint_js.file in
      let pos = {
        Pos.
        pos_file = Relative_path.create Relative_path.Dummy file;
        pos_start = Reason_js.lexpos file line col;
        pos_end = Reason_js.lexpos file line (col+1);
      }
      in
      let (pos, ground_t, possible_ts) = TI.query_type cx pos in
      let ty = match ground_t with
        | None -> None
        | Some t -> Some (Constraint_js.string_of_t cx t)
      in
      let reasons =
        possible_ts
        |> List.map Constraint_js.reason_of_t
      in
      (pos, ty, reasons)
    with exn ->
      prerr_endline
        (spf "Could not show type for %s:%d:%d\n%s"
            file line col
            (Printexc.to_string exn)
        );
      (Pos.none, None, [])
    ); in
    Marshal.to_channel oc (pos, t, reasons) [];
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
         let file = Path.string_of_path (Path.mk_path file) in
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
         prerr_endline
           (spf "Could not fill types for %s\n%s"
              file
              (Printexc.to_string exn)
           );
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
        let file = Path.string_of_path (Path.mk_path file) in
        let ast = Parsing_service_js.get_ast_unsafe file in
        let content = cat file in
        let lines = Str.split_delim (Str.regexp "\n") content in
        let insertions = Comments_js.meta_program ast in
        let insertions = List.sort Pervasives.compare insertions in
        let new_content = Reason_js.do_patch lines insertions in
        let patch_content = patch file new_content in
        SMap.add file patch_content result_map
      with exn ->
        prerr_endline
          (spf "Could not port docblock-style annotations for %s\n%s"
            file
            (Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ())
          );
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
    let pos = {
      Pos.
      pos_file = Relative_path.create Relative_path.Dummy file;
      pos_start = Reason_js.lexpos file line col;
      pos_end = Reason_js.lexpos file line (col+1);
    } in
    let state = GetDef_js.getdef_set_hooks pos in
    (try
      let content = ServerProt.file_input_get_content file_input in
      let cx = match Types_js.typecheck_contents
                       content file true with
        | Some cx, _ -> cx
        | _, errors  -> failwith "Couldn't parse file"
      in
      let result = GetDef_js.getdef_get_result cx state in
      Marshal.to_channel oc result []
    with exn ->
      prerr_endline
        (spf "Could not get definition for %s:%d:%d\n%s"
            file line col
            (Printexc.to_string exn)
        )
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

  let respond genv env ~client ~msg =
    let _, oc = client in
    match msg with
    | ServerProt.AUTOCOMPLETE fn ->
        autocomplete fn oc
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
        die_nicely oc
    | ServerProt.PING ->
        ServerProt.response_to_channel oc ServerProt.PONG
    | ServerProt.PORT (files) ->
        port files oc
    | ServerProt.STATUS client_root ->
        print_status genv env client_root oc
    | ServerProt.SUGGEST (files) ->
        suggest files oc

  let handle_connection_ genv env socket =
    let cli, _ = Unix.accept socket in
    let ic = Unix.in_channel_of_descr cli in
    let oc = Unix.out_channel_of_descr cli in
    let client = ic, oc in
    let msg = ServerProt.cmd_from_channel ic in
    let finished, _, _ = Unix.select [cli] [] [] 0.0 in
    (if finished <> [] then () else begin
      ServerPeriodical.stamp_connection();
      respond genv env ~client ~msg;
      (try Unix.close cli with e ->
        Printf.fprintf stderr "Error: %s\n" (Printexc.to_string e);
        flush stderr);
    end)


  let handle_connection genv env socket =
    try handle_connection_ genv env socket
    with
    | Unix.Unix_error (e, _, _) ->
        flush stdout
    | e ->
        flush stdout

  let filter_update genv _env update =
    let flowconfig_path =
      FlowConfig.fullpath (ServerArgs.root genv.ServerEnv.options) in
    Files_js.is_flow_file (Relative_path.to_absolute update)
      || Relative_path.to_absolute update = flowconfig_path

  (* on notification, execute client commands or recheck files *)
  let recheck genv env updates =
    let diff_js = updates in
    if Relative_path.Set.is_empty diff_js
    then env
    else
      let diff_js = Relative_path.Set.fold (fun x a ->
        SSet.add (Relative_path.to_absolute x) a) diff_js SSet.empty in
      let root = ServerArgs.root genv.ServerEnv.options in
      if SSet.mem (FlowConfig.fullpath root) diff_js
      then begin
        (* TODO: Restart in a cross platform way *)
        Printf.printf     "Status: Error\n";
        Printf.printf     ".flowconfig modified. %s is out of date. Exiting.\n" name;
        exit 4
      end;
      let options = OptionParser.get_flow_options () in

      let n = SSet.cardinal diff_js in
      prerr_endline (spf "recheck %d files:" n);
      let _ = SSet.fold (fun f i ->
        prerr_endline (spf "%d/%d: %s" i n f); i + 1) diff_js 1 in

      Types_js.recheck genv env diff_js options
end
