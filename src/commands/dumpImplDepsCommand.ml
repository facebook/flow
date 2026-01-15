(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils
open Utils_js

let spec =
  {
    CommandSpec.name = "unstable-dump-impl-deps";
    doc = "Outputs the implementation dependency graph as JSON";
    usage =
      Printf.sprintf
        "Usage: %s dump-impl-deps [OPTION]... [ROOT]\n\nOutputs the implementation dependency graph as JSON to stdout.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n"
        exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> options_and_json_flags
        |> shm_flags
        |> ignore_version_flag
        |> from_flag
        |> no_cgroup_flag
        |> anon "root" (optional string)
      );
  }

let graph_to_json graph =
  let map = FilenameGraph.to_map graph in
  let json_pairs =
    FilenameMap.fold
      (fun file deps acc ->
        let file_str = File_key.to_string file in
        let deps_list = FilenameSet.fold (fun dep acc -> File_key.to_string dep :: acc) deps [] in
        let deps_sorted = List.sort String.compare deps_list in
        let deps_json =
          Hh_json.JSON_Array (List.map (fun s -> Hh_json.JSON_String s) deps_sorted)
        in
        (file_str, deps_json) :: acc)
      map
      []
  in
  Hh_json.JSON_Object json_pairs

let main base_flags options_flags _json _pretty shm_flags ignore_version path_opt () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = CommandUtils.guess_root flowconfig_name path_opt in
  let (flowconfig, flowconfig_hash) =
    let flowconfig_path = Server_files_js.config_file flowconfig_name root in
    read_config_and_hash_or_exit ~enforce_warnings:(not ignore_version) flowconfig_path
  in
  let options =
    let lazy_mode = Some FlowConfig.Non_lazy in
    let saved_state_options_flags =
      {
        CommandUtils.Saved_state_flags.saved_state_fetcher = Some Options.Dummy_fetcher;
        saved_state_force_recheck = false;
        saved_state_no_fallback = false;
        saved_state_skip_version_check = false;
        saved_state_verify = false;
      }
    in
    make_options
      ~flowconfig_name
      ~flowconfig_hash
      ~flowconfig
      ~lazy_mode
      ~root
      ~options_flags
      ~saved_state_options_flags
  in
  let init_id = Random_id.short_string () in
  (* initialize loggers before doing too much *)
  LoggingUtils.init_loggers ~options ~min_level:Hh_logger.Level.Error ();

  if not ignore_version then assert_version flowconfig;

  let shared_mem_config = shm_config shm_flags flowconfig in

  (* Disable logging to keep stdout clean for JSON output *)
  PidLog.disable ();
  MonitorRPC.disable ();

  let run () =
    let num_workers = Options.max_workers options in
    let handle =
      match SharedMem.init ~num_workers shared_mem_config with
      | Ok handle -> handle
      | Error () -> raise SharedMem.Out_of_shared_memory
    in
    let genv = ServerEnvBuild.make_genv ~options ~init_id handle in
    let workers = genv.ServerEnv.workers in

    let%lwt (libs_ok, env) =
      Profiling_js.with_profiling_lwt ~label:"Init" ~should_print_summary:false (fun profiling ->
          Types_js.init ~profiling ~workers options
      )
      |> Lwt.map snd
    in
    if not libs_ok then begin
      prerr_endline "Library initialization failed";
      Exit.(exit Could_not_find_flowconfig)
    end;

    (* Get the implementation dependency graph *)
    let impl_graph =
      Dependency_info.implementation_dependency_graph env.ServerEnv.dependency_info
    in
    let json = graph_to_json impl_graph in
    print_endline (Hh_json.json_to_string ~pretty:true json);
    Lwt.return_unit
  in
  LwtInit.run_lwt run

let command = CommandSpec.command spec main
