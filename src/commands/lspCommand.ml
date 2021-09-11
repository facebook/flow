(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils

(***********************************************************************)
(* flow lsp command *)
(***********************************************************************)

let spec =
  {
    CommandSpec.name = "lsp";
    doc = "Acts as a server for the Language Server Protocol over stdin/stdout [experimental]";
    usage =
      Printf.sprintf
        "Usage: %s lsp\n\nRuns a server for the Language Server Protocol\n"
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> temp_dir_flag
        |> shm_flags
        |> flag "--lazy" no_arg ~doc:"Deprecated, has no effect"
        |> flag "--lazy-mode" string ~doc:"Deprecated, has no effect"
        |> autostop_flag
        |> from_flag);
  }

let main
    base_flags
    (temp_dir : string option)
    (shm_flags : CommandUtils.shared_mem_params)
    (_lazy : bool)
    (_lazy_mode : string option)
    (autostop : bool)
    (() : unit) : unit =
  (* always set `quiet`, since the LSP doesn't want any log spew. this only applies to the
     `start` command and does not imply a quiet server, which will still write to its log
     file. *)
  let quiet = true in
  let connect_params =
    {
      retries = 0;
      timeout = None;
      no_auto_start = false;
      temp_dir;
      autostop;
      lazy_mode = None;
      shm_flags;
      ignore_version = false;
      quiet;
      on_mismatch = Choose_newest;
    }
  in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  FlowLsp.run ~flowconfig_name ~connect_params

let command = CommandSpec.command spec main
