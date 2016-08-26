(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open CommandUtils

let spf = Printf.sprintf

let name = "shadow-file"
let spec = {
  CommandSpec.
  name;
  doc = "Given a filename, generate a shadow (.js.flow) file.";
  usage = Printf.sprintf
    "Usage: %s %s [OPTIONS] [FILE] [FILE] [FILE]...\n\n\
      e.g. %s %s foo.js > foo.js.flow\n"
    CommandUtils.exe_name
    name
    CommandUtils.exe_name
    name
  ;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> error_flags
    |> strip_root_flag
    |> anon "file" (required string)
        ~doc:"The file for which a shadow file should be generated"
  )
}

let main option_values root error_flags strip_root file () = (
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> Some (expand_path file)
  ) in
  let filename = ServerProt.FileName (expand_path file) in
  let (in_chan, out_chan) = connect option_values root in
  ServerProt.cmd_to_channel out_chan (ServerProt.GEN_INTERFACES [filename]);
  let response =
    (Timeout.input_value in_chan: ServerProt.gen_interface_response)
  in

  match response with
  | response::[] -> (
    match response with
    | Utils_js.Err (ServerProt.GenIface_TypecheckError (_, errors)) ->
      let errors = Errors.to_list errors in
      Errors.print_error_summary ~out_channel:stderr ~flags:error_flags ~strip_root ~root errors;
      FlowExitStatus.exit
        ~msg:"\nThere must be no type errors in order to generate a shadow file!"
        FlowExitStatus.Type_error;
    | Utils_js.Err (ServerProt.GenIface_UnexpectedError (file_path, error)) ->
      FlowExitStatus.exit
        ~msg:(spf "Error: %s: %s" file_path error)
        FlowExitStatus.Unknown_error
    | Utils_js.OK (_file, interface) ->
      print_endline interface
  )
  | response ->
    let msg = spf (
      "Internal Error: Expected a single interface description from the " ^^
      "server, but received %d interfaces!"
    ) (List.length response) in
    prerr_endline msg
)

let command = CommandSpec.command spec main
