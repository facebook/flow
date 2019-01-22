(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils

let print_endlinef = Utils_js.print_endlinef
let prerr_endlinef = Utils_js.prerr_endlinef

let depgraph_subcommand =
  let spec = {
    CommandSpec.
    name = "dep-graph";
    doc = "Output .dot file for the dependency graph of a repository";
    usage = Printf.sprintf
      "Usage: %s graph dep-graph [OPTION]...\n\n\
        e.g. %s graph dep-graph --out path/to/output --root path/to/root\n\
        e.g. %s graph dep-graph --out path/to/output \n\
        or   %s graph dep-graph --strip-root --out path/to/output --root path/to/root\n\
        Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
        ROOT is assumed to be the current directory if unspecified.\n\
        If --strip-root is specified, the file paths in the output graph
        will be relative to ROOT.\n\
        The graph will be output in FILE.\n\n"
        Utils_js.exe_name
        Utils_js.exe_name
        Utils_js.exe_name
        Utils_js.exe_name;
    args = CommandSpec.ArgSpec.(
      empty
      |> base_flags
      |> connect_flags
      |> strip_root_flag
      |> wait_for_recheck_flag
      |> flag "--out" (required string) ~doc:"Location to print the output file"
      |> root_flag
    )
  } in

  let main base_flags option_values strip_root wait_for_recheck outfile path_opt () =
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    let root = CommandUtils.guess_root flowconfig_name path_opt in
    (* Create the outfile if it doesn't already exist *)
    let outpath = Files.imaginary_realpath outfile |> Path.make |> Path.to_string in
    (* connect to server *)
    let wait_for_recheck = if wait_for_recheck = None then Some true else wait_for_recheck in
    let request = ServerProt.Request.GRAPH_DEP_GRAPH {
      root = Path.to_string root;
      strip_root;
      outfile = outpath;
      wait_for_recheck;
    } in
    match connect_and_make_request flowconfig_name option_values root request with
    | ServerProt.Response.GRAPH_DEP_GRAPH (Error msg) ->
      FlowExitStatus.(exit ~msg Unknown_error)
    | ServerProt.Response.GRAPH_DEP_GRAPH (Ok _) -> ()
    | response -> failwith_bad_response ~request ~response in

  CommandSpec.command spec main

let cycle_subcommand =
  let spec = { CycleCommand.spec with
    CommandSpec.usage = Printf.sprintf
      "Usage: %s graph cycle [OPTION]...\n\n\
        e.g. %s graph cycle path/to/file.js \n"
        Utils_js.exe_name
        Utils_js.exe_name;
  } in
  CommandSpec.command spec CycleCommand.main

let command =
  let spec = {
    CommandSpec.
    name = "graph";
    doc = "Outputs dependency graphs of flow repositories";
    usage = Printf.sprintf
      "Usage: %s graph SUBCOMMAND [OPTIONS]...\n\
        Outputs dependency graphs of flow repositories\n\n\
        SUBCOMMANDS:\n\
          cycle: Produces a graph of the dependency cycle containing the input file\n\
          dep-graph: Produces the dependency graph of a repository\n"
        CommandUtils.exe_name;
    args = CommandSpec.ArgSpec.(
      empty
      |> anon "subcommand" (required (command [
           "cycle", cycle_subcommand;
           "dep-graph", depgraph_subcommand
         ]))
    )
  }
  in
  let main (cmd, argv) () =
    CommandUtils.run_command cmd argv
  in
  CommandSpec.command spec main
