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
        |> lazy_flags
        |> autostop_flag
        |> from_flag);
  }

let main
    base_flags
    (temp_dir : string option)
    (shm_flags : CommandUtils.shared_mem_params)
    (lazy_mode : Options.lazy_mode option)
    (autostop : bool)
    (() : unit) : unit =
  let connect_params =
    {
      retries = 0;
      retry_if_init = false;
      timeout = None;
      no_auto_start = false;
      temp_dir;
      autostop;
      lazy_mode;
      shm_flags;
      ignore_version = false;
      quiet = false;
      on_mismatch = Choose_newest;
    }
  in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  FlowLsp.run ~flowconfig_name ~connect_params

let command = CommandSpec.command spec main
