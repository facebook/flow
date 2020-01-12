(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils

let print_endlinef = Utils_js.print_endlinef

let prerr_endlinef = Utils_js.prerr_endlinef

let spec =
  {
    CommandSpec.name = "cycle";
    doc = "Output .dot file for cycle containing the given file";
    usage =
      Printf.sprintf
        "Usage: %s cycle [OPTION]...\n\ne.g. %s cycle path/to/file.js \n"
        Utils_js.exe_name
        Utils_js.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_flags
        |> root_flag
        |> strip_root_flag
        |> flag "--types" no_arg ~doc:"Only consider type dependencies"
        |> anon "FILE..." (required string));
  }

let main base_flags option_values root strip_root types_only file () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let file = expand_path file in
  let root = guess_root flowconfig_name root in
  let strip_root f =
    if strip_root then
      Files.relative_path (Path.to_string root) f
    else
      f
  in
  (* connect to server *)
  let request = ServerProt.Request.CYCLE { filename = file; types_only } in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.CYCLE (Error msg) -> FlowExitStatus.(exit ~msg Unknown_error)
  | ServerProt.Response.CYCLE (Ok dep_graph) ->
    (* print .dot file to stdout *)
    LwtUtils.output_graph Lwt_io.stdout strip_root dep_graph |> Lwt_main.run
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
