(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open CommandUtils

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
    |> ignore_flag
    |> include_flag
    |> untyped_flag
    |> declaration_flag
    |> from_flag
    |> anon "src" (required string)
    |> flag "--out-dir" string
        ~doc:"The path to write the generated .js.flow files into"
  )
}

let write_file strip_root root content perm src_file_path dest_file_path =
  let fd = Unix.(openfile dest_file_path [O_CREAT; O_TRUNC; O_WRONLY;] perm) in
  let root_str = Path.to_string root in
  let printed_src_file_path =
    if strip_root
    then Files.relative_path root_str src_file_path
    else src_file_path
  in
  print_string (spf "%s -> " printed_src_file_path);
  flush stdout;
  (try (
    ignore (Unix.single_write fd content 0 (String.length content));
    let printed_dest_file_path =
      if strip_root
      then Files.relative_path root_str dest_file_path
      else dest_file_path
    in
    print_endline printed_dest_file_path
  ) with exn -> print_endline "ERROR!"; Unix.close fd; raise exn);
  Unix.close fd

let main option_values root error_flags strip_root ignore_flag
  include_flag untyped_flag declaration_flag from src out_dir () = (
  FlowEventLogger.set_from from;
  let src = expand_path src in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> Some src
  ) in

  (match out_dir with None -> () | Some out_dir ->
    if not (Sys.is_directory out_dir) then
      let msg = spf "%s: `--out-dir` must be a directory!" out_dir in
      FlowExitStatus.exit ~msg FlowExitStatus.Commandline_usage_error
  );

  let src_is_dir = Sys.is_directory src in
  let filenames =
    if not src_is_dir then [File_input.FileName src] else (
      (* If `src` is a directory, we require that an out_dir was specified *)
      (if out_dir = None then
        let msg =
          "When the `src` arg is a directory, the `--out-dir` flag is required."
        in
        FlowExitStatus.exit ~msg FlowExitStatus.Commandline_usage_error
      );

      let options = LsCommand.make_options ~root ~ignore_flag ~include_flag ~untyped_flag ~declaration_flag in
      let _, libs = Files.init options in
      let next_files =
        LsCommand.get_ls_files ~root ~all:false ~options ~libs ~imaginary:false (Some src)
      in
      let files = Files.get_all next_files in
      let num_files = SSet.cardinal files in
      print_endlinef "Found %d files, generating libdefs..." num_files;
      List.map (fun f -> File_input.FileName f) (SSet.elements files)
    )
  in

  let include_warnings = error_flags.Errors.Cli_output.include_warnings in
  let request = ServerProt.Request.GEN_FLOW_FILES (filenames, include_warnings) in
  let open ServerProt.Response in
  match connect_and_make_request option_values root request, out_dir with
  | GEN_FLOW_FILES (Error (GenFlowFiles_TypecheckError {errors; warnings})), _ ->
    let strip_root = if strip_root then Some root else None in
    Errors.Cli_output.print_errors
      ~out_channel:stderr
      ~flags:error_flags
      ~strip_root
      ~errors
      ~warnings
      ~lazy_msg:None
      ();
    let msg =
      "\nIn order to generate a shadow file there must be no type errors!"
    in
    FlowExitStatus.exit ~msg FlowExitStatus.Type_error;
  | GEN_FLOW_FILES (Error (GenFlowFiles_UnexpectedError error_msg)), _ ->
    let msg = spf "Error: %s" error_msg in
    FlowExitStatus.exit ~msg FlowExitStatus.Unknown_error
  | GEN_FLOW_FILES (Ok results), None ->
    (if List.length results <> 1 then (
      let msg =
        "Internal Error: Received multiple results for a single file!"
      in
      FlowExitStatus.exit ~msg FlowExitStatus.Unknown_error
    ));
    let (_filepath, result) = List.hd results in
    (match result with
      | GenFlowFiles_FlowFile content ->
        print_endline content
      | GenFlowFiles_NonFlowFile ->
        print_endline "// This file does not have an @flow at the top!"
    )
  | GEN_FLOW_FILES (Ok results), Some out_dir ->
    let out_dir = expand_path out_dir in
    let src_stat = Unix.stat src in
    results |> List.iter (fun (file_path, result) ->
      match result with
      | GenFlowFiles_FlowFile content when src_is_dir ->
        let dest_path = file_path
          (* File path relative to the src dir *)
          |> Files.relative_path src
          (* Make the path OS specific *)
          |> Str.split_delim (Str.regexp "/")
          |> String.concat Filename.dir_sep
          (* Concatenated with the output dir *)
          |> Filename.concat out_dir in

        (* Replace file extension .js -> .js.flow *)
        let dest_path = dest_path ^ ".flow" in

        let dest_dir = Filename.dirname dest_path in
        Files.mkdirp dest_dir src_stat.Unix.st_perm;

        let file_path_stat = Unix.stat file_path in
        (try write_file strip_root root content file_path_stat.Unix.st_perm file_path dest_path
        with exn -> prerr_endlinef "Error writing %s:" dest_path; raise exn)

      | GenFlowFiles_FlowFile content ->
        let file_name = Filename.basename file_path in
        let dest_path = Filename.concat out_dir file_name in
        let dest_path = dest_path ^ ".flow" in

        let file_path_stat = Unix.stat file_path in
        (try write_file strip_root root content file_path_stat.Unix.st_perm file_path dest_path
        with exn -> prerr_endlinef "Error writing %s:" dest_path; raise exn)

      | GenFlowFiles_NonFlowFile -> ()
    )
  | response, _ -> failwith_bad_response ~request ~response
)

let command = CommandSpec.command spec main
