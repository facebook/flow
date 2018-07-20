(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js

let get_lazy_stats genv env =
  { ServerProt.Response.
    lazy_mode = Options.lazy_mode genv.options;
    checked_files = CheckedSet.all env.checked_files |> Utils_js.FilenameSet.cardinal;
    total_files = Utils_js.FilenameSet.cardinal env.files;
  }

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
    Persistent_connection.send_exit env.connections FlowExitStatus.Flowconfig_changed;
    FlowExitStatus.(exit Flowconfig_changed)
  end;

  (* WARNING! Be careful when adding new incompatibilities to this function. While dfind will
   * return any file which changes within the watched directories, watchman only watches for
   * specific extensions and files. Make sure to update the watchman_expression_terms in our
   * watchman file watcher! *)
  let is_incompatible filename_str =
    let filename = File_key.JsonFile filename_str in
    match Sys_utils.cat_or_failed filename_str with
    | None -> true (* Failed to read package.json *)
    | Some content ->
      try
        let ast = Parsing_service_js.parse_json_file ~fail:true content filename in
        Module_js.package_incompatible filename_str ast
      with _ -> true (* Failed to parse package.json *)
  in

  let is_incompatible_package_json f = (
      String_utils.string_starts_with f sroot ||
      Files.is_included file_options f
    )
    && (Filename.basename f) = "package.json"
    && want f
    && is_incompatible f
  in

  (* Die if a package.json changed in an incompatible way *)
  let incompatible_packages = SSet.filter (fun f ->
    is_incompatible_package_json f
  ) updates in

  if not (SSet.is_empty incompatible_packages)
  then begin
    Hh_logger.fatal "Status: Error";
    SSet.iter (Hh_logger.fatal "Modified package: %s") incompatible_packages;
    Hh_logger.fatal
      "Packages changed in an incompatible way. Exiting.\n%!";
    FlowExitStatus.(exit Server_out_of_date)
  end;

  Option.iter (Options.module_resolver options) ~f:(fun module_resolver ->
    let str_module_resolver = Path.to_string module_resolver in
    if SSet.mem str_module_resolver updates then begin
      let msg = Printf.sprintf "Module resolver %s changed in an incompatible way. Exiting.\n%!"
        str_module_resolver in
      Hh_logger.fatal "%s" msg;
      FlowExitStatus.(exit ~msg Server_out_of_date)
    end;
  );

  let flow_typed_path = Path.to_string (Files.get_flowtyped_path root) in
  let is_changed_lib filename =
    let is_lib = SSet.mem filename all_libs || filename = flow_typed_path in
    is_lib &&
      let file = File_key.LibFile filename in
      match Sys_utils.cat_or_failed filename with
      | None -> true (* Failed to read lib file *)
      | Some content ->
        (* Check if the lib file's hash has changed *)
        not (Parsing_service_js.does_content_match_file_hash file content)
  in

  (* Die if a lib file changed *)
  let libs = updates |> SSet.filter is_changed_lib in
  if not (SSet.is_empty libs)
  then begin
    Hh_logger.fatal "Status: Error";
    SSet.iter (Hh_logger.fatal "Modified lib file: %s") libs;
    Hh_logger.fatal
      "Lib files changed in an incompatible way. Exiting.\n%!";
    Persistent_connection.send_exit env.connections FlowExitStatus.Server_out_of_date;
    FlowExitStatus.(exit Server_out_of_date)
  end;

  let is_flow_file = Files.is_flow_file ~options:file_options in
  SSet.fold (fun f acc ->
    if is_flow_file f &&
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
let recheck genv env ?(files_to_focus=FilenameSet.empty) updates =
  (* Caller should have already checked this *)
  assert (not (FilenameSet.is_empty updates));

  MonitorRPC.status_update ~event:ServerStatus.Recheck_start;
  Persistent_connection.send_start_recheck env.connections;
  let options = genv.ServerEnv.options in
  let workers = genv.ServerEnv.workers in

  let%lwt profiling, summary, env =
    Types_js.recheck ~options ~workers ~updates env ~files_to_focus in

  let lazy_stats = get_lazy_stats genv env in
  Persistent_connection.send_end_recheck ~lazy_stats env.connections;
  (* We must send "end_recheck" prior to sending errors+warnings so the client *)
  (* knows that this set of errors+warnings are final ones, not incremental.   *)
  let calc_errors_and_warnings () =
    let errors, warnings, _ = ErrorCollator.get_with_separate_warnings env in
    errors, warnings
  in
  Persistent_connection.update_clients ~clients:env.connections ~calc_errors_and_warnings;

  MonitorRPC.status_update ~event:(ServerStatus.Finishing_up summary);
  Lwt.return (profiling, env)

(* Perform a single recheck. This will incorporate any pending changes from the file watcher.
 * If any file watcher notifications come in during the recheck, it will be canceled and restarted
 * to include the new changes *)
let rec recheck_single
    ?(files_to_recheck=Utils_js.FilenameSet.empty)
    ?(files_to_focus=Utils_js.FilenameSet.empty)
    genv env =
  let env = ServerMonitorListenerState.update_env env in
  let process_updates = process_updates genv env in
  let new_updates = ServerMonitorListenerState.get_and_clear_updates_for_recheck ~process_updates in
  let updates = Utils_js.FilenameSet.union files_to_recheck new_updates
    |> Utils_js.FilenameSet.union files_to_focus in
  if Utils_js.FilenameSet.is_empty updates
  then Lwt.return (Error env) (* Nothing to do *)
  else
    let recheck_thread = recheck genv env ~files_to_focus updates in
    let cancel_thread =
      let%lwt () = ServerMonitorListenerState.wait_for_updates_for_recheck ~process_updates in
      Hh_logger.info "Canceling recheck due to new file changes";
      Lwt.cancel recheck_thread;
      Lwt.return_unit
    in
    try%lwt
      let%lwt ret = recheck_thread in
      Lwt.cancel cancel_thread;
      Lwt.return (Ok ret)
    with Lwt.Canceled as exn ->
      Hh_logger.info ~exn
        "Recheck successfully canceled. Restarting the recheck to include new file changes";
      recheck_single ~files_to_recheck:updates ~files_to_focus genv env

let rec recheck_loop
    ?(files_to_recheck=Utils_js.FilenameSet.empty)
    ?(files_to_focus=Utils_js.FilenameSet.empty)
    genv env =
  match%lwt recheck_single ~files_to_recheck ~files_to_focus genv env with
  | Error env ->
    (* No more work to do for now *)
    Lwt.return env
  | Ok (_profiling, env) ->
    (* We just finished a recheck. Let's see if there's any more stuff to recheck *)
    recheck_loop genv env
