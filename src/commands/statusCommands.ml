(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandInfo
open CommandUtils

(***********************************************************************)
(* flow status (report current error set) command impl *)
(***********************************************************************)

module type CONFIG = sig
  (* explicit == called with "flow status ..."
     rather than simply "flow ..." *)
  val explicit : bool
end

module Impl (CommandList : COMMAND_LIST) (Config : CONFIG) = struct

  let spec = if Config.explicit
  then
    {
      CommandSpec.
      name = "status";
      doc = "(default) Shows current Flow errors by asking the Flow server";
      usage = Printf.sprintf
        "Usage: %s status [OPTION]... [ROOT]\n\
          Shows current Flow errors by asking the Flow server.\n\n\
          Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
          ROOT is assumed to be the current directory if unspecified.\n\
          A server will be started if none is running over ROOT.\n\
          \n\
          Status command options:"
          exe_name;
      args = CommandSpec.ArgSpec.(
        empty
        |> server_and_json_flags
        |> json_version_flag
        |> error_flags
        |> strip_root_flag
        |> from_flag
        |> dummy false (* match --version below *)
        |> anon "root" (optional string)
      )
    }
  else
    let command_info = CommandList.commands
      |> List.map (fun (command) ->
        (CommandSpec.name command, CommandSpec.doc command)
      )
      |> List.filter (fun (cmd, doc) -> cmd <> "" && doc <> "")
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    in
    let col_width = List.fold_left
      (fun acc (cmd, _) -> max acc (String.length cmd)) 0 command_info in
    let cmd_usage = command_info
      |> List.map (fun (cmd, doc) ->
            Utils_js.spf "  %-*s  %s" col_width cmd doc
         )
      |> String.concat "\n"
    in
    {
      CommandSpec.
      name = "default";
      doc = "";
      usage = Printf.sprintf
        "Usage: %s [COMMAND] \n\n\
          Valid values for COMMAND:\n%s\n\n\
          Default values if unspecified:\n\
            \ \ COMMAND\
              \tstatus\n\
          \n\
          Status command options:"
          exe_name
          cmd_usage;
      args = CommandSpec.ArgSpec.(
        empty
        |> server_and_json_flags
        |> json_version_flag
        |> error_flags
        |> strip_root_flag
        |> from_flag
        |> flag "--version" no_arg
            ~doc:"(Deprecated, use `flow version` instead) Print version number and exit"
        |> anon "root" (optional string)
      )
    }

  type args = {
    root: Path.t;
    from: string;
    output_json: bool;
    output_json_version: Errors.Json_output.json_version option;
    pretty: bool;
    error_flags: Errors.Cli_output.error_flags;
    strip_root: bool;
  }

  let check_status (args:args) server_flags =
    let name = "flow" in

    let include_warnings = args.error_flags.Errors.Cli_output.include_warnings in
    let request = ServerProt.Request.STATUS (args.root, include_warnings) in
    let response, lazy_stats = match connect_and_make_request server_flags args.root request with
    | ServerProt.Response.STATUS {status_response; lazy_stats} -> status_response, lazy_stats
    | response -> failwith_bad_response ~request ~response
    in
    let strip_root = if args.strip_root then Some args.root else None in
    let print_json = Errors.Json_output.print_errors
      ~out_channel:stdout ~strip_root ~pretty:args.pretty ?version:args.output_json_version
      ~suppressed_errors:([])
    in
    let lazy_msg = match lazy_stats.ServerProt.Response.lazy_mode with
    | Some mode -> Some (
        Printf.sprintf
          ("The Flow server is currently in %s lazy mode and is only checking %d/%d files.\n" ^^
          "To learn more, visit flow.org/en/docs/lang/lazy-modes")
        Options.(match mode with | LAZY_MODE_FILESYSTEM -> "filesystem" | LAZY_MODE_IDE -> "IDE")
        lazy_stats.ServerProt.Response.checked_files
        lazy_stats.ServerProt.Response.total_files
      )
    | None -> None in
    match response with
    | ServerProt.Response.DIRECTORY_MISMATCH d ->
      let msg = Printf.sprintf
        ("%s is running on a different directory.\n" ^^
         "server_root: %s, client_root: %s")
        name
        (Path.to_string d.ServerProt.Response.server)
        (Path.to_string d.ServerProt.Response.client)
      in
      FlowExitStatus.(exit ~msg Server_client_directory_mismatch)
    | ServerProt.Response.ERRORS {errors; warnings} ->
      let error_flags = args.error_flags in
      begin if args.output_json then
        print_json ~errors ~warnings ()
      else if args.from = "vim" || args.from = "emacs" then
        Errors.Vim_emacs_output.print_errors ~strip_root
          stdout ~errors ~warnings ()
      else
        Errors.Cli_output.print_errors
          ~strip_root
          ~flags:error_flags
          ~out_channel:stdout
          ~errors
          ~warnings
          ~lazy_msg
          ()
      end;
      FlowExitStatus.exit (get_check_or_status_exit_code errors warnings error_flags.Errors.Cli_output.max_warnings)
    | ServerProt.Response.NO_ERRORS ->
      if args.output_json then
        print_json ~errors:Errors.ErrorSet.empty ~warnings:Errors.ErrorSet.empty ()
      else begin
        Printf.printf "No errors!\n%!";
        Option.iter lazy_msg ~f:(Printf.printf "\n%s\n%!")
      end;
      FlowExitStatus.(exit No_error)
    | ServerProt.Response.NOT_COVERED ->
      let msg = "Why on earth did the server respond with NOT_COVERED?" in
      FlowExitStatus.(exit ~msg Unknown_error)

  let main server_flags json pretty json_version error_flags strip_root from version root () =
    FlowEventLogger.set_from from;
    if version then (
      prerr_endline "Warning: \
        `flow --version` is deprecated in favor of `flow version`";
      print_version ();
      FlowExitStatus.(exit No_error)
    );

    let root = guess_root root in
    Memlog.set_root root;

    let json = json || Option.is_some json_version || pretty in

    let args = {
      root;
      from = server_flags.CommandUtils.from;
      output_json = json;
      output_json_version = json_version;
      pretty;
      error_flags;
      strip_root;
    } in
    check_status args server_flags
end

module Status(CommandList : COMMAND_LIST) = struct
  module Main = Impl (CommandList) (struct let explicit = true end)
  let command = CommandSpec.command Main.spec Main.main
end

module Default(CommandList : COMMAND_LIST) = struct
  module Main = Impl (CommandList) (struct let explicit = false end)
  let command = CommandSpec.command Main.spec Main.main
end
