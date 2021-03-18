(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  let spec =
    if Config.explicit then
      {
        CommandSpec.name = "status";
        doc = "(default) Shows current Flow errors by asking the Flow server";
        usage =
          Printf.sprintf
            "Usage: %s status [OPTION]... [ROOT]\nShows current Flow errors by asking the Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\nA server will be started if none is running over ROOT.\n\nStatus command options:"
            exe_name;
        args =
          CommandSpec.ArgSpec.(
            empty
            |> base_flags
            |> connect_and_json_flags
            |> json_version_flag
            |> offset_style_flag
            |> error_flags
            |> strip_root_flag
            |> from_flag
            |> dummy false (* match --version below *)
            |> anon "root" (optional string));
      }
    else
      let command_info =
        CommandList.commands
        |> Base.List.map ~f:(fun command -> (CommandSpec.name command, CommandSpec.doc command))
        |> List.filter (fun (cmd, doc) -> cmd <> "" && doc <> "")
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      let cmd_usage = CommandSpec.format_two_columns ~col_pad:1 command_info in
      {
        CommandSpec.name = "default";
        doc = "";
        usage =
          Printf.sprintf
            "Usage: %s [COMMAND] \n\nValid values for COMMAND:\n%s\n\nDefault values if unspecified:\n\ \ COMMAND\tstatus\n\nStatus command options:"
            exe_name
            cmd_usage;
        args =
          CommandSpec.ArgSpec.(
            empty
            |> base_flags
            |> connect_and_json_flags
            |> json_version_flag
            |> offset_style_flag
            |> error_flags
            |> strip_root_flag
            |> from_flag
            |> flag "--version" no_arg ~doc:"Print version number and exit"
            |> anon "root" (optional string));
      }

  type args = {
    root: Path.t;
    output_json: bool;
    output_json_version: Errors.Json_output.json_version option;
    offset_style: CommandUtils.offset_style option;
    pretty: bool;
    error_flags: Errors.Cli_output.error_flags;
    strip_root: bool;
  }

  let check_status flowconfig_name (args : args) connect_flags =
    let name = "flow" in
    let include_warnings = args.error_flags.Errors.Cli_output.include_warnings in
    let request = ServerProt.Request.STATUS { client_root = args.root; include_warnings } in
    let (response, lazy_stats) =
      match connect_and_make_request flowconfig_name connect_flags args.root request with
      | ServerProt.Response.STATUS { status_response; lazy_stats } -> (status_response, lazy_stats)
      | response -> failwith_bad_response ~request ~response
    in
    let strip_root =
      if args.strip_root then
        Some args.root
      else
        None
    in
    let offset_kind = CommandUtils.offset_kind_of_offset_style args.offset_style in
    let print_json =
      Errors.Json_output.print_errors
        ~out_channel:stdout
        ~strip_root
        ~pretty:args.pretty
        ?version:args.output_json_version
        ~offset_kind
    in
    let lazy_msg =
      match lazy_stats.ServerProt.Response.lazy_mode with
      | Options.NON_LAZY_MODE -> None
      | mode ->
        Some
          (Printf.sprintf
             ( "The Flow server is currently in %s lazy mode and is only checking %d/%d files.\n"
             ^^ "To learn more, visit flow.org/en/docs/lang/lazy-modes" )
             Options.(
               match mode with
               | LAZY_MODE_FILESYSTEM -> "filesystem"
               | LAZY_MODE_IDE -> "IDE"
               | LAZY_MODE_WATCHMAN -> "Watchman"
               | NON_LAZY_MODE -> assert false)
             lazy_stats.ServerProt.Response.checked_files
             lazy_stats.ServerProt.Response.total_files)
    in
    match response with
    | ServerProt.Response.DIRECTORY_MISMATCH d ->
      let msg =
        Printf.sprintf
          ("%s is running on a different directory.\n" ^^ "server_root: %s, client_root: %s")
          name
          (Path.to_string d.ServerProt.Response.server)
          (Path.to_string d.ServerProt.Response.client)
      in
      Exit.(exit ~msg Server_client_directory_mismatch)
    | ServerProt.Response.ERRORS { errors; warnings; suppressed_errors } ->
      let error_flags = args.error_flags in
      let from = FlowEventLogger.get_from_I_AM_A_CLOWN () in
      begin
        if args.output_json then
          print_json ~errors ~warnings ~suppressed_errors ()
        else if from = Some "vim" || from = Some "emacs" then
          Errors.Vim_emacs_output.print_errors ~strip_root stdout ~errors ~warnings ()
        else
          let errors =
            List.fold_left
              (fun acc (error, _) -> Errors.ConcreteLocPrintableErrorSet.add error acc)
              errors
              suppressed_errors
          in
          Errors.Cli_output.print_errors
            ~strip_root
            ~flags:error_flags
            ~out_channel:stdout
            ~errors
            ~warnings
            ~lazy_msg
            ()
      end;
      Exit.exit
        (get_check_or_status_exit_code errors warnings error_flags.Errors.Cli_output.max_warnings)
    | ServerProt.Response.NO_ERRORS ->
      if args.output_json then
        print_json
          ~errors:Errors.ConcreteLocPrintableErrorSet.empty
          ~warnings:Errors.ConcreteLocPrintableErrorSet.empty
          ~suppressed_errors:[]
          ()
      else (
        Printf.printf "No errors!\n%!";
        Base.Option.iter lazy_msg ~f:(Printf.printf "\n%s\n%!")
      );
      Exit.(exit No_error)
    | ServerProt.Response.NOT_COVERED ->
      let msg = "Why on earth did the server respond with NOT_COVERED?" in
      Exit.(exit ~msg Unknown_error)

  let main
      base_flags
      connect_flags
      json
      pretty
      json_version
      offset_style
      error_flags
      strip_root
      version
      root
      () =
    if version then (
      print_version ();
      Exit.(exit No_error)
    );

    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    let root = guess_root flowconfig_name root in
    let json = json || Base.Option.is_some json_version || pretty in
    let args =
      {
        root;
        output_json = json;
        output_json_version = json_version;
        offset_style;
        pretty;
        error_flags;
        strip_root;
      }
    in
    check_status flowconfig_name args connect_flags
end

module Status (CommandList : COMMAND_LIST) = struct
  module Main =
    Impl
      (CommandList)
      (struct
        let explicit = true
      end)

  let command = CommandSpec.command Main.spec Main.main
end

module Default (CommandList : COMMAND_LIST) = struct
  module Main =
    Impl
      (CommandList)
      (struct
        let explicit = false
      end)

  let command = CommandSpec.command Main.spec Main.main
end
