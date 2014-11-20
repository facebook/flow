(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open ServerEnv

module Server = ServerFunctors

module Program : Server.SERVER_PROGRAM = struct
  let name = "hh_server"

  let get_errors env = env.errorl

  let infer = ServerInferType.go

  let suggest _files oc =
    output_string oc "Unimplemented\n";
    flush oc

  let incorrect_hash oc =
    ServerMsg.response_to_channel oc ServerMsg.SERVER_OUT_OF_DATE;
    EventLogger.out_of_date ();
    Printf.printf     "Status: Error\n";
    Printf.printf     "%s is out of date. Exiting.\n" name;
    exit 4

  let status_log env =
    if List.length (get_errors env) = 0
    then Printf.printf "Status: OK\n"
    else Printf.printf "Status: Error\n";
    flush stdout

  let print_status genv env client_root oc =
    let server_root = ServerArgs.root genv.options in
    if not (Path.equal server_root client_root)
    then begin
      let msg = ServerMsg.DIRECTORY_MISMATCH {ServerMsg.server=server_root; ServerMsg.client=client_root} in
      ServerMsg.response_to_channel oc msg;
      Printf.printf "Status: Error\n";
      Printf.printf "server_dir=%s, client_dir=%s\n"
        (Path.string_of_path server_root)
        (Path.string_of_path client_root);
      Printf.printf "%s is not listening to the same directory. Exiting.\n" name;
      exit 5
    end;
    flush stdout;
    (* TODO: check status.directory *)
    status_log env;
    let errors = get_errors env in
    EventLogger.check_response errors;
    ServerError.send_errorl errors oc

  let die_nicely oc =
    ServerMsg.response_to_channel oc ServerMsg.SERVER_DYING;
    EventLogger.killed ();
    Printf.printf "Status: Error\n";
    Printf.printf "Sent KILL command by client. Dying.\n";
    (match !ServerDfind.dfind_pid with
    | Some pid -> Unix.kill pid Sys.sigterm;
    | None -> failwith "Dfind died before we could kill it"
    );
    die ()

  let respond genv env ~client ~msg =
    let _, oc = client in
    match msg with
    | ServerMsg.ERROR_OUT_OF_DATE -> incorrect_hash oc
    | ServerMsg.PRINT_COVERAGE_LEVELS fn -> ServerColorFile.go fn oc
    | ServerMsg.INFER_TYPE (fn, line, char) ->
        infer (fn, line, char) oc
    | ServerMsg.SUGGEST (files) -> suggest files oc
    | ServerMsg.STATUS client_root -> print_status genv env client_root oc
    | ServerMsg.LIST_FILES    -> ServerEnv.list_files env oc
    | ServerMsg.AUTOCOMPLETE content ->
        ServerAutoComplete.auto_complete env content oc
    | ServerMsg.IDENTIFY_FUNCTION (content, line, char) ->
        ServerIdentifyFunction.go content line char oc
    | ServerMsg.OUTLINE content ->
        ServerFileOutline.go content oc
    | ServerMsg.METHOD_JUMP (class_, find_children) ->
        ServerMethodJumps.go class_ find_children env genv oc
    | ServerMsg.SAVE_STATE filename ->
        let dump = ServerSign.dump_state env genv in
        let status = ServerSign.save dump filename in
        output_string oc (status^"\n");
        flush oc
    | ServerMsg.SHOW name ->
        output_string oc "starting\n";
        output_string oc "class:\n";
        SharedMem.invalidate_caches();
        let qual_name = if name.[0] = '\\' then name
          else ("\\"^name) in
        let nenv = env.nenv in
        (match SMap.get (Naming.canon_key qual_name) (snd nenv.Naming.iclasses) with
          | None -> output_string oc "Missing from nenv\n"
          | Some canon ->
            let p, _ = SMap.find_unsafe canon (fst nenv.Naming.iclasses) in
            output_string oc ((Pos.string (Pos.to_absolute p))^"\n")
        );
        let class_ = Typing_env.Classes.get qual_name in
        (match class_ with
        | None -> output_string oc "Missing from Typing_env\n"
        | Some c ->
            let class_str = Typing_print.class_ c in
            output_string oc (class_str^"\n")
        );
        output_string oc "function:\n";
        (match SMap.get qual_name nenv.Naming.ifuns with
        | Some (p, _) -> output_string oc (Pos.string (Pos.to_absolute p)^"\n")
        | None -> output_string oc "Missing from nenv\n");
        let fun_ = Typing_env.Funs.get qual_name in
        (match fun_ with
        | None ->
            output_string oc "Missing from Typing_env\n"
        | Some f ->
            let fun_str = Typing_print.fun_ f in
            output_string oc (fun_str^"\n")
        );
        flush oc
    | ServerMsg.KILL -> die_nicely oc
    | ServerMsg.PING -> ServerMsg.response_to_channel oc ServerMsg.PONG
    | ServerMsg.BUILD build_opts ->
      let build_hook = BuildMain.go build_opts genv env oc in
      ServerTypeCheck.hook_after_parsing := begin fun genv env ->
        (* subtle: an exception there (such as writing on a closed pipe)
         * will not be catched by handle_connection() because
         * we have already returned from handle_connection(), hence
         * this additional try.
         *)
        (try
           build_hook genv env;
           close_out oc;
        with exn ->
          Printf.printf "Exn in build_hook: %s" (Printexc.to_string exn);
        );
        ServerTypeCheck.hook_after_parsing := (fun _ _ -> ())
      end
    | ServerMsg.FIND_REFS find_refs_action ->
        ServerFindRefs.go find_refs_action genv env oc
    | ServerMsg.REFACTOR refactor_action ->
        ServerRefactor.go refactor_action genv env oc
    | ServerMsg.ARGUMENT_INFO (contents, line, col) ->
        ServerArgumentInfo.go genv env oc contents line col
    | ServerMsg.PROLOG ->
      let path = PrologMain.go genv env oc in
      (* the rational for this PROLOG_READY: prefix is that the string
       * passed below corresponds to a command that will ultimately be
       * exec'ed by hh_client, and because we also (ab)use 'oc' to
       * communicate possible error message to the client, it's
       * safer to at least add a prefix.
       *)
      output_string oc ("PROLOG_READY:" ^path);
      flush oc
    | ServerMsg.SEARCH (query, type_) ->
        ServerSearch.go query type_ oc
    | ServerMsg.CALC_COVERAGE path ->
        ServerCoverageMetric.go path genv oc

  let handle_connection_ genv env socket =
    let cli, _ = Unix.accept socket in
    let ic = Unix.in_channel_of_descr cli in
    let oc = Unix.out_channel_of_descr cli in
    let client = ic, oc in
    let msg = ServerMsg.cmd_from_channel ic in
    let finished, _, _ = Unix.select [cli] [] [] 0.0 in
    if finished <> [] then () else begin
      ServerPeriodical.stamp_connection();
      match msg with
      | ServerMsg.BUILD _ ->
        (* The build step is special. It closes the socket itself. *)
        respond genv env ~client ~msg
      | _ ->
        respond genv env ~client ~msg;
        (try Unix.close cli with e ->
          Printf.fprintf stderr "Error: %s\n" (Printexc.to_string e);
          flush stderr);
    end

  let handle_connection genv env socket =
    try handle_connection_ genv env socket
    with
    | Unix.Unix_error (e, _, _) ->
        Printf.fprintf stderr "Unix error: %s\n" (Unix.error_message e);
        Printexc.print_backtrace stderr;
        flush stderr
    | e ->
        Printf.fprintf stderr "Error: %s\n" (Printexc.to_string e);
        Printexc.print_backtrace stderr;
        flush stderr

  let preinit () =
    HackSearchService.attach_hooks ();
    (* Force hhi files to be extracted and their location saved before workers
     * fork, so everyone can know about the same hhi path. *)
    ignore (Hhi.get_hhi_root());
    ignore (
      Sys.signal Sys.sigusr1 (Sys.Signal_handle Typing.debug_print_last_pos)
    )

  let make_next_files dir =
    let php_next_files = Find.make_next_files_php dir in
    let js_next_files = Find.make_next_files_js ~filter:(fun _ -> true) dir in
    fun () -> php_next_files () @ js_next_files ()

  let init genv env =
    let module RP = Relative_path in
    let root = ServerArgs.root genv.options in
    let next_files_hhi =
      match Hhi.get_hhi_root () with
      | Some hhi_root ->
          compose (rev_rev_map (RP.create RP.Hhi)) (make_next_files hhi_root)
      | None -> print_endline "Could not locate hhi files"; exit 1 in
    let next_files_root = compose
      (rev_rev_map (RP.create RP.Root)) (make_next_files root)
    in
    let next_files = fun () ->
      match next_files_hhi () with
      | [] -> next_files_root ()
      | x -> x in
    ServerInit.init genv env next_files

  let run_once_and_exit genv env =
    ServerError.print_errorl (ServerArgs.json_mode genv.options)
                             (List.map Errors.to_absolute env.errorl) stdout;
    match ServerArgs.convert genv.options with
    | None ->
        exit (if env.errorl = [] then 0 else 1)
    | Some dirname ->
        ServerConvert.go genv env dirname;
        exit 0

  (* We won't filter more rigorously until later so the hooks can stay up to
   * date on filesystem changes *)
  let filter_update _genv _env _update = true

  let filter_typecheck_update update =
    Find.is_php_path (Relative_path.suffix update)

  let recheck genv env updates =
    let php_diff = Relative_path.Set.filter filter_typecheck_update updates in
    BuildMain.incremental_update genv env updates;
    if Relative_path.Set.is_empty php_diff
    then
      begin
        BuildMain.incremental_update genv env updates;
        env
      end
    else
      let failed_parsing = Relative_path.Set.union php_diff env.failed_parsing in
      let check_env = { env with failed_parsing = failed_parsing } in
      let env = ServerTypeCheck.check genv check_env in
      BuildMain.incremental_update genv env updates;
      env

  let parse_options = ServerArgs.parse_options

  let marshal chan =
    Marshal.to_channel chan !Typing_deps.ifiles [];
    HackSearchService.SS.MasterApi.marshal chan

  let unmarshal chan =
    Typing_deps.ifiles := Marshal.from_channel chan;
    HackSearchService.SS.MasterApi.unmarshal chan
end
