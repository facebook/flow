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

  let handle_client (genv:ServerEnv.genv) (env:ServerEnv.env) client =
    ServerCommand.handle genv env client

  let preinit () =
    HackSearchService.attach_hooks ();
    (* Force hhi files to be extracted and their location saved before workers
     * fork, so everyone can know about the same hhi path. *)
    ignore (Hhi.get_hhi_root());
    ignore (
      Sys.signal Sys.sigusr1 (Sys.Signal_handle Typing.debug_print_last_pos)
    )

  let make_next_files dir =
    let php_next_files = Find.make_next_files FindUtils.is_php dir in
    let js_next_files = Find.make_next_files FindUtils.is_js dir in
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
    let hhi_root = Hhi.get_hhi_root () in
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
    FindUtils.is_php (Relative_path.suffix update)

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
