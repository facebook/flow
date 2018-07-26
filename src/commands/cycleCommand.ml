(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils

let print_endlinef = Utils_js.print_endlinef
let prerr_endlinef = Utils_js.prerr_endlinef

let spec = {
  CommandSpec.
  name = "cycle";
  doc = "Output .dot file for cycle containing the given file";
  usage = Printf.sprintf
    "Usage: %s cycle [OPTION]...\n\n\
      e.g. %s cycle path/to/file.js \n"
      Utils_js.exe_name
      Utils_js.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> base_flags
    |> connect_flags
    |> root_flag
    |> strip_root_flag
    |> anon "FILE..." (required string)
  )
}

let main base_flags option_values root strip_root file () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let file = expand_path file in
  let root = guess_root flowconfig_name root in
  let strip_root f =
    if strip_root
    then Files.relative_path (Path.to_string root) f
    else f
  in
  (* connect to server *)
  let request = ServerProt.Request.CYCLE file in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.CYCLE (Error msg) ->
    prerr_endline msg
  | ServerProt.Response.CYCLE (Ok dep_graph) ->
    (* print .dot file to stdout *)
    print_endline "digraph {";
    List.iter (fun (f, dep_fs) ->
      List.iter (fun dep_f ->
        print_endlinef "  \"%s\" -> \"%s\""
          (strip_root f)
          (strip_root dep_f)
      ) dep_fs
    ) dep_graph;
    print_endline "}"
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
