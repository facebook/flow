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
let recheck genv env ?(force_focus=false) updates =
  if FilenameSet.is_empty updates
  then Lwt.return (None, env)
  else begin
    MonitorRPC.status_update ~event:ServerStatus.Recheck_start;
    Persistent_connection.send_start_recheck env.connections;
    let options = genv.ServerEnv.options in
    let workers = genv.ServerEnv.workers in

    let%lwt profiling, env = Types_js.recheck ~options ~workers ~updates env ~force_focus in

    let lazy_stats = get_lazy_stats genv env in
    Persistent_connection.send_end_recheck ~lazy_stats env.connections;
    let calc_errors_and_warnings () =
      let errors, warnings, _ = ErrorCollator.get_with_separate_warnings env in
      errors, warnings
    in
    Persistent_connection.update_clients ~clients:env.connections ~calc_errors_and_warnings;

    MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
    Lwt.return (Some profiling, env)
  end
