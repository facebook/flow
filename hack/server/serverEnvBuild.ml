(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Building the environment *)
(*****************************************************************************)
open ServerEnv

module SLC = ServerLocalConfig

let make_genv options config local_config =
  let root = ServerArgs.root options in
  let check_mode   = ServerArgs.check_mode options in
  let gc_control   = ServerConfig.gc_control config in
  Typing_deps.trace :=
    not check_mode || ServerArgs.convert options <> None ||
    ServerArgs.save_filename options <> None;
  let nbr_procs = GlobalConfig.nbr_procs in
  let workers =
    if Sys.win32 then
      None (* No parallelism on Windows yet, Work-in-progress *)
    else
      Some (Worker.make nbr_procs gc_control) in
  let watchman =
    if check_mode || not local_config.SLC.use_watchman
    then None
    else Watchman.init local_config.SLC.watchman_init_timeout root
  in
  if Option.is_some watchman then Hh_logger.log "Using watchman";
  let indexer, notifier, wait_until_ready =
    match watchman with
    | Some watchman ->
      let indexer filter =
        let files = Watchman.get_all_files watchman in
        Bucket.make ~max_size:1000 (List.filter filter files) in
      let notifier () = Watchman.get_changes watchman in
      HackEventLogger.set_use_watchman ();
      (* We don't have an easy way to wait for watchman's init crawl to
       * finish *)
      let wait_until_ready () = () in
      indexer, notifier, wait_until_ready
    | None ->
      let indexer filter = Find.make_next_files ~name:"root" ~filter root in
      let log_link = ServerFiles.dfind_log root in
      let log_file = Sys_utils.make_link_of_timestamped log_link in
      let dfind =
        DfindLib.init ~log_file (GlobalConfig.scuba_table_name, [root]) in
      let notifier () =
        begin try
          Timeout.with_timeout ~timeout:120
            ~on_timeout:(fun () -> Exit_status.(exit Dfind_unresponsive))
            ~do_:(fun t -> DfindLib.get_changes ~timeout:t dfind)
        with _ ->
          Exit_status.(exit Dfind_died)
        end
      in
      let ready = ref false in
      let wait_until_ready () =
        if !ready then ()
        else (DfindLib.wait_until_ready dfind; ready := true)
      in
      indexer, notifier, wait_until_ready
  in
  { options;
    config;
    local_config;
    workers;
    indexer;
    notifier;
    wait_until_ready;
  }

let make_env config =
  { tcopt          = ServerConfig.typechecker_options config;
    files_info     = Relative_path.Map.empty;
    errorl         = [];
    failed_parsing = Relative_path.Set.empty;
    failed_decl    = Relative_path.Set.empty;
    failed_check   = Relative_path.Set.empty;
  }
