(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow init command *)
(***********************************************************************)

let spec =
  {
    CommandSpec.name = "init";
    doc = "Initializes a directory to be used as a flow root directory";
    usage =
      Printf.sprintf
        "Usage: %s init [ROOT]\nInitializes a directory to be used as a flow root directory\n\ne.g. %s init /path/to/root\nor %s init\nor %s init --options \"optionA=123;optionB=456\"\nor %s init --lints \"lintA=on,lintB=off\"\n\nIf the root is not specified it is assumed to be the current working directory\n\nThis command will create and initialize /path/to/root/.flowconfig\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> CommandUtils.base_flags
        |> CommandUtils.from_flag
        |> CommandUtils.flowconfig_flags
        |> flag "--options" (optional string) ~doc:"Semicolon-delimited list of key=value pairs"
        |> anon "root" (optional string));
  }

let error (errs : (int * string) list) =
  let msg =
    errs
    |> Base.List.map ~f:(fun (ln, msg) -> Utils_js.spf ".flowconfig:%d %s" ln msg)
    |> String.concat "\n"
  in
  Exit.(exit ~msg Invalid_flowconfig)

let main base_flags flowconfig_flags options root () =
  let root =
    match root with
    | None -> Sys.getcwd () |> Path.make
    | Some root -> Path.make root
  in
  FlowEventLogger.set_root (Some (Path.to_string root));
  let options =
    match options with
    | None -> []
    | Some str -> Str.split (Str.regexp ";") str
  in
  let ignores = flowconfig_flags.CommandUtils.ignores in
  let untyped = flowconfig_flags.CommandUtils.untyped in
  let declarations = flowconfig_flags.CommandUtils.declarations in
  let includes = flowconfig_flags.CommandUtils.includes in
  let libs = flowconfig_flags.CommandUtils.libs in
  let lints = flowconfig_flags.CommandUtils.raw_lint_severities in
  let file = Server_files_js.config_file base_flags.CommandUtils.Base_flags.flowconfig_name root in
  (if Sys.file_exists file then
    let msg = Utils_js.spf "Error: \"%s\" already exists!\n%!" file in
    Exit.(exit ~msg Invalid_flowconfig));

  let config = FlowConfig.init ~ignores ~untyped ~declarations ~includes ~libs ~options ~lints in
  let config =
    match config with
    | Ok (config, []) -> config
    | Ok (_, warnings) -> error warnings (* TODO: write warnings to stderr instead of exiting *)
    | Error err -> error [err]
  in
  let out = Sys_utils.open_out_no_fail file in
  FlowConfig.write config out;
  Sys_utils.close_out_no_fail file out

let command = CommandSpec.command spec main
