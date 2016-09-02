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

let name = "gen-flow-files"
let spec = {
  CommandSpec.
  name;

  (**
   * Still iterating on this command but wanted to leave the eventual
   * docs/usage info that we're targeting here in comments here to
   * foreshadow what's to come a bit.
   *)
  (*
  doc = "Generate minimal .js.flow files for publishing to npm.";
  usage = Printf.sprintf
    ("Usage: %s %s [OPTIONS] SRC_DIR OUT_DIR\n" ^^
     "         or\n" ^^
     "       %s %s [OPTIONS] INPUT_FILE\n" ^^
     "\n" ^^
     "e.g. %s %s ./src ./dist\n" ^^
     "or   %s %s ./src/lib/foo.js > ./dist/lib/foo.js.flow\n")

    CommandUtils.exe_name
    name
    CommandUtils.exe_name
    name
    CommandUtils.exe_name
    name
    CommandUtils.exe_name
    name
  ;
  *)
  doc = "EXPERIMENTAL: Generate minimal .js.flow files for publishing to npm.";
  usage = Printf.sprintf
    "Usage (EXPERIMENTAL): %s %s [OPTIONS] [FILE]\n\n\
      e.g. %s %s ./src/foo.js > ./dist/foo.js.flow\n"
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
    |> anon "src_dir" (required string)
        ~doc:"The source directory to scan and generate .js.flow files from"
    |> anon "out_dir" string
        ~doc:"The output directory to write generated .js.flow files into"
  )
}

let main option_values root error_flags strip_root src_dir out_dir () = (
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> Some (expand_path src_dir)
  ) in
  let filenames =
    (**
     * If there are 2 args specified, the command was invoked with the form:
     *
     *   flow gen-flow-files ./src ./dist
     *
     * If there is only 1 arg specified, the command was invoke with the form:
     *
     *   flow gen-flow-files ./src/foo.js > ./dist/foo.js.flow
     *)
    match out_dir with
    | Some _out_dir ->
      failwith (Printf.sprintf
        ("ERROR: This command is still \"in beta\" and only currently " ^^
         "supports the command form: `%s %s ./src/foo.js > ./dist/foo.js.flow`")
        CommandUtils.exe_name
        name
      )
    | None -> [ServerProt.FileName (expand_path src_dir)]
  in
  let (in_chan, out_chan) = connect option_values root in

  let open ServerProt in
  cmd_to_channel out_chan (GEN_FLOW_FILES filenames);
  match (Timeout.input_value in_chan: gen_flow_file_response) with
  | Utils_js.Err (GenFlowFile_TypecheckError errors) ->
    let errors = Errors.to_list errors in
    Errors.print_error_summary
      ~out_channel:stderr
      ~flags:error_flags
      ~strip_root
      ~root
      errors;
    let msg =
      "\nIn order to generate a shadow file there must be no type errors!"
    in
    FlowExitStatus.exit ~msg FlowExitStatus.Type_error;
  | Utils_js.Err (GenFlowFile_UnexpectedError error_msg) ->
    prerr_endline (spf "Error: %s" error_msg);
    FlowExitStatus.exit FlowExitStatus.Unknown_error
  | Utils_js.OK results ->
    (if List.length results <> 1 then failwith (
      "Error: This command is still experimental and is currently only able " ^
      "to generate .js.flow files for 1 file at a time. Handling multiple " ^
      "files will come soon!"
    ));
    let (_filepath, result) = List.hd results in
    match result with
    | GenFlowFile_FlowFile content ->
      print_endline content
    | GenFlowFile_NonFlowFile ->
      print_endline "// This file does not have an @flow at the top!"
)

let command = CommandSpec.command spec main
