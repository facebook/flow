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
open ServerUtils

module Server = ServerFunctors

module Program : Server.SERVER_PROGRAM = struct
  module EventLogger = EventLogger

  let name = "hh_server"

  let config_filename_ =
    Relative_path.concat Relative_path.Root ".hhconfig"

  let config_filename () = config_filename_

  let load_config () = ServerConfig.load config_filename_

  let validate_config genv =
    let new_config = load_config () in
    (* This comparison can eventually be made more complex; we may not always
     * need to restart hh_server, e.g. changing the path to the load script
     * is immaterial*)
    genv.config = new_config

  let get_errors env = env.errorl

  let infer = ServerInferType.go

  let suggest _files oc =
    output_string oc "Unimplemented\n";
    flush oc

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
    ServerError.send_errorl errors oc;
    (* check_response takes a while, so do it last. We don't want the client to
     * time out waiting for our response. *)
    EventLogger.check_response errors

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

  let respond (genv:ServerEnv.genv) env ~client ~msg =
    let oc = client.oc in
    match msg with
    | ServerMsg.PRINT_COVERAGE_LEVELS fn ->
      ServerColorFile.go env fn oc
    | ServerMsg.INFER_TYPE (fn, line, char) ->
        infer env (fn, line, char) oc
    | ServerMsg.SUGGEST (files) -> suggest files oc
    | ServerMsg.STATUS client_root -> print_status genv env client_root oc
    | ServerMsg.LIST_FILES -> ServerEnv.list_files env oc
    | ServerMsg.LIST_MODES ->
        Relative_path.Map.iter begin fun fn fileinfo ->
          match Relative_path.prefix fn with
          | Relative_path.Root ->
            let mode = match fileinfo.FileInfo.file_mode with
              | None -> "php"
              | Some FileInfo.Mdecl -> "decl"
              | Some FileInfo.Mpartial -> "partial"
              | Some FileInfo.Mstrict -> "strict" in
            Printf.fprintf oc "%s\t%s\n%!" mode (Relative_path.to_absolute fn)
          | _ -> ()
        end env.ServerEnv.files_info
    | ServerMsg.AUTOCOMPLETE content ->
        let result = ServerAutoComplete.auto_complete env.nenv content in
        print_endline "Auto-complete";
        Marshal.to_channel oc result [];
        flush oc
    | ServerMsg.IDENTIFY_FUNCTION (content, line, char) ->
        ServerIdentifyFunction.go content line char oc
    | ServerMsg.OUTLINE content ->
        ServerFileOutline.go content oc
    | ServerMsg.METHOD_JUMP (class_, find_children) ->
        ServerMethodJumps.go class_ find_children env genv oc
    | ServerMsg.SHOW name ->
        output_string oc "starting\n";
        SharedMem.invalidate_caches();
        let qual_name = if name.[0] = '\\' then name else ("\\"^name) in
        let nenv = env.nenv in
        output_string oc "class:\n";
        let class_name = (
          match SMap.get (Naming.canon_key qual_name) (snd nenv.Naming.iclasses) with
          | None ->
            let () = output_string oc "Missing from naming env\n" in qual_name
          | Some canon ->
            let p, _ = SMap.find_unsafe canon (fst nenv.Naming.iclasses) in
            let () = output_string oc ((Pos.string (Pos.to_absolute p))^"\n") in
            canon
        ) in
        let class_ = Typing_env.Classes.get class_name in
        (match class_ with
        | None -> output_string oc "Missing from typing env\n"
        | Some c ->
            let class_str = Typing_print.class_ c in
            output_string oc (class_str^"\n")
        );
        output_string oc "\nfunction:\n";
        let fun_name =
        (match SMap.get (Naming.canon_key qual_name) (snd nenv.Naming.ifuns) with
          | None ->
            let () = output_string oc "Missing from naming env\n" in qual_name
          | Some canon ->
            let p, _ = SMap.find_unsafe canon (fst nenv.Naming.ifuns) in
            let () = output_string oc ((Pos.string (Pos.to_absolute p))^"\n") in
            canon
        ) in
        let fun_ = Typing_env.Funs.get fun_name in
        (match fun_ with
        | None ->
            output_string oc "Missing from typing env\n"
        | Some f ->
            let fun_str = Typing_print.fun_ f in
            output_string oc (fun_str^"\n")
        );
        output_string oc "\nglobal const:\n";
        (match SMap.get qual_name nenv.Naming.iconsts with
        | Some (p, _) -> output_string oc (Pos.string (Pos.to_absolute p)^"\n")
        | None -> output_string oc "Missing from naming env\n");
        let gconst_ty = Typing_env.GConsts.get qual_name in
        (match gconst_ty with
        | None -> output_string oc "Missing from typing env\n"
        | Some gc ->
            let gconst_str = Typing_print.gconst gc in
            output_string oc ("ty: "^gconst_str^"\n")
        );
        output_string oc "typedef:\n";
        (match SMap.get qual_name nenv.Naming.itypedefs with
        | Some (p, _) -> output_string oc (Pos.string (Pos.to_absolute p)^"\n")
        | None -> output_string oc "Missing from naming env\n");
        let tdef = Typing_env.Typedefs.get qual_name in
        (match tdef with
        | None ->
            output_string oc "Missing from typing env\n"
        | Some td ->
            let td_str = Typing_print.typedef td in
            output_string oc (td_str^"\n")
        );
        flush oc
    | ServerMsg.KILL -> die_nicely oc
    | ServerMsg.PING -> ServerMsg.response_to_channel oc ServerMsg.PONG
    | ServerMsg.BUILD build_opts ->
      let build_hook = BuildMain.go build_opts genv env oc in
      (match build_hook with
      | None -> client.close ()
      | Some build_hook -> begin
        ServerTypeCheck.hook_after_parsing := (fun genv old_env env updates ->
          (* subtle: an exception there (such as writing on a closed pipe)
           * will not be caught by handle_connection() because
           * we have already returned from handle_connection(), hence
           * this additional try.
           *)
          (try
            with_context
              ~enter:(fun () -> ())
              ~exit:(fun () -> client.close ())
              ~do_:(fun () -> build_hook genv old_env env updates);
          with exn ->
            let msg = Printexc.to_string exn in
            Printf.printf "Exn in build_hook: %s" msg;
            EventLogger.master_exception msg;
          );
          ServerTypeCheck.hook_after_parsing := (fun _ _ _ _ -> ())
        )
      end)
    | ServerMsg.FIND_REFS find_refs_action ->
        ServerFindRefs.go find_refs_action genv env oc
    | ServerMsg.REFACTOR refactor_action ->
        ServerRefactor.go refactor_action genv env oc
    | ServerMsg.DUMP_SYMBOL_INFO file_list ->
        let results = (SymbolInfoService.find_fun_calls
          genv.ServerEnv.workers file_list env) in
        Marshal.to_channel oc results [];
        flush oc;
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
        ServerCoverageMetric.go path genv env oc
    | ServerMsg.LINT fnl ->
        ServerLint.go genv fnl oc
    | ServerMsg.LINT_ALL code ->
        ServerLint.lint_all genv code oc

  let handle_client (genv:ServerEnv.genv) (env:ServerEnv.env) client =
    let msg = ServerMsg.cmd_from_channel client.ic in
    match msg with
    | ServerMsg.BUILD _ ->
        (* The build step is special. It closes the socket itself. *)
        respond genv env ~client ~msg
    | _ ->
        respond genv env ~client ~msg;
        client.close ()

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

  let stamp_file = Tmp.get_dir() ^ "/stamp"
  let touch_stamp () = Sys_utils.with_umask 0o111 begin fun () ->
    (* Open and close the file to set its mtime. Don't use the Unix.utimes
     * function since that will fail if the stamp file doesn't exist. *)
    close_out (open_out stamp_file)
  end
  let touch_stamp_errors l1 l2 =
    (* We don't want to needlessly touch the stamp file if the error list is
     * the same and nothing has changed, but we also don't want to spend a ton
     * of time comparing huge lists of errors over and over (i.e., grind to a
     * halt in the cases when there are thousands of errors). So we cut off the
     * comparison at an arbitrary point. *)
    let rec length_greater_than n = function
      | [] -> false
      | _ when n = 0 -> true
      | _::l -> length_greater_than (n-1) l in
    if length_greater_than 5 l1 || length_greater_than 5 l2 || l1 <> l2
    then touch_stamp ()

  let init genv env =
    let module RP = Relative_path in
    let root = ServerArgs.root genv.options in
    let hhi_root = Path.mk_path (Hhi.get_hhi_root ()) in
    let next_files_hhi =
      compose (rev_rev_map (RP.create RP.Hhi)) (make_next_files hhi_root) in
    let next_files_root = compose
      (rev_rev_map (RP.create RP.Root)) (make_next_files root)
    in
    let next_files = fun () ->
      match next_files_hhi () with
      | [] -> next_files_root ()
      | x -> x in
    let env = ServerInit.init genv env next_files in
    touch_stamp ();
    env

  let run_once_and_exit genv env =
    ServerError.print_errorl (ServerArgs.json_mode genv.options)
                             (List.map Errors.to_absolute env.errorl) stdout;
    match ServerArgs.convert genv.options with
    | None ->
        exit (if env.errorl = [] then 0 else 1)
    | Some dirname ->
        ServerConvert.go genv env dirname;
        exit 0

  let process_updates _genv _env updates =
    Relative_path.relativize_set Relative_path.Root updates

  let should_recheck update =
    Find.is_php_path (Relative_path.suffix update)

  let recheck genv old_env typecheck_updates =
    if Relative_path.Set.is_empty typecheck_updates then old_env else begin
      let failed_parsing =
        Relative_path.Set.union typecheck_updates old_env.failed_parsing in
      let check_env = { old_env with failed_parsing = failed_parsing } in
      let new_env = ServerTypeCheck.check genv check_env in
      touch_stamp_errors old_env.errorl new_env.errorl;
      new_env
    end

  let post_recheck_hook = BuildMain.incremental_update

  let parse_options = ServerArgs.parse_options

  let get_watch_paths _options = []

  let marshal chan =
    Typing_deps.marshal chan;
    HackSearchService.SS.MasterApi.marshal chan

  let unmarshal chan =
    Typing_deps.unmarshal chan;
    HackSearchService.SS.MasterApi.unmarshal chan
end
