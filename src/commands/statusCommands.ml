(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandInfo
open CommandUtils

(***********************************************************************)
(* flow status (report current error set) command impl *)
(***********************************************************************)

type status_args = {
  root: File_path.t;
  output_json: bool;
  output_json_version: Flow_errors_utils.Json_output.json_version option;
  offset_style: CommandUtils.offset_style option;
  pretty: bool;
  error_flags: Flow_errors_utils.Cli_output.error_flags;
  strip_root: bool;
}

let check_status flowconfig_name (args : status_args) connect_flags =
  let include_warnings = args.error_flags.Flow_errors_utils.Cli_output.include_warnings in
  let request = ServerProt.Request.STATUS { include_warnings } in
  let (response, lazy_stats) =
    match CommandUtils.connect_and_make_request flowconfig_name connect_flags args.root request with
    | ServerProt.Response.STATUS { status_response; lazy_stats } -> (status_response, lazy_stats)
    | response -> CommandUtils.failwith_bad_response ~request ~response
  in
  let strip_root =
    if args.strip_root then
      Some args.root
    else
      None
  in
  let offset_kind = CommandUtils.offset_kind_of_offset_style args.offset_style in
  let print_json =
    Flow_errors_utils.Json_output.print_errors
      ~out_channel:stdout
      ~strip_root
      ~pretty:args.pretty
      ?version:args.output_json_version
      ~offset_kind
  in
  let lazy_msg =
    match lazy_stats.ServerProt.Response.lazy_mode with
    | false -> None
    | true ->
      let checked_source =
        lazy_stats.ServerProt.Response.checked_files
        - lazy_stats.ServerProt.Response.checked_libdef_files
      in
      let total_source =
        lazy_stats.ServerProt.Response.total_files
        - lazy_stats.ServerProt.Response.total_libdef_files
      in
      let libdef_msg =
        Printf.sprintf
          " (+ %d/%d libdefs)"
          lazy_stats.ServerProt.Response.checked_libdef_files
          lazy_stats.ServerProt.Response.total_libdef_files
      in
      Some
        (Printf.sprintf
           ("The Flow server is currently in lazy mode and is only checking %d/%d source files%s.\n"
           ^^ "To learn more, visit flow.org/en/docs/lang/lazy-modes"
           )
           checked_source
           total_source
           libdef_msg
        )
  in
  match response with
  | ServerProt.Response.ERRORS { errors; warnings; suppressed_errors } ->
    let error_flags = args.error_flags in
    let from = FlowEventLogger.get_from_I_AM_A_CLOWN () in
    begin
      if args.output_json then
        print_json ~errors ~warnings ~suppressed_errors ()
      else if from = Some "vim" || from = Some "emacs" then
        Flow_errors_utils.Vim_emacs_output.print_errors ~strip_root stdout ~errors ~warnings ()
      else
        let errors =
          List.fold_left
            (fun acc (error, _) -> Flow_errors_utils.ConcreteLocPrintableErrorSet.add error acc)
            errors
            suppressed_errors
        in
        Flow_errors_utils.Cli_output.print_errors
          ~strip_root
          ~flags:error_flags
          ~out_channel:stdout
          ~errors
          ~warnings
          ~lazy_msg
          ()
    end;
    Exit.exit
      (CommandUtils.get_check_or_status_exit_code
         errors
         warnings
         error_flags.Flow_errors_utils.Cli_output.max_warnings
      )
  | ServerProt.Response.NO_ERRORS ->
    if args.output_json then
      print_json
        ~errors:Flow_errors_utils.ConcreteLocPrintableErrorSet.empty
        ~warnings:Flow_errors_utils.ConcreteLocPrintableErrorSet.empty
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
        visibility = CommandSpec.Public;
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
            |> dummy false (* match --help-all below *)
            |> anon "root" (optional string)
          );
      }
    else
      let command_info =
        CommandList.commands
        |> Base.List.filter ~f:(fun command ->
               CommandSpec.name command <> "" && CommandSpec.visibility command = CommandSpec.Public
           )
        |> Base.List.map ~f:(fun command -> (CommandSpec.name command, CommandSpec.doc command))
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      let cmd_usage = CommandSpec.format_two_columns ~col_pad:1 command_info in
      {
        CommandSpec.name = "default";
        doc = "";
        visibility = CommandSpec.Internal;
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
            |> flag "--version" truthy ~doc:"Print version number and exit"
            |> flag
                 "--help-all"
                 truthy
                 ~doc:"Show all commands including internal and experimental ones"
            |> anon "root" (optional string)
          );
      }

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
      help_all
      root
      () =
    if version then (
      print_version ();
      Exit.(exit No_error)
    );

    if help_all then (
      let all_commands =
        CommandList.commands |> Base.List.filter ~f:(fun command -> CommandSpec.name command <> "")
      in
      let by_visibility v =
        all_commands
        |> Base.List.filter ~f:(fun command -> CommandSpec.visibility command = v)
        |> Base.List.map ~f:(fun command -> (CommandSpec.name command, CommandSpec.doc command))
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      let public = by_visibility CommandSpec.Public in
      let experimental = by_visibility CommandSpec.Experimental in
      let internal = by_visibility CommandSpec.Internal in
      let col_width =
        1
        + Base.List.fold_left
            ~f:(fun acc (a, _) -> max acc (String.length a))
            ~init:0
            (public @ experimental @ internal)
      in
      let fmt = CommandSpec.format_two_columns ~col_pad:1 ~col_width in
      let buf = Buffer.create 1024 in
      Printf.bprintf
        buf
        "Documentation: https://flow.org/en/docs/cli/\n\nUsage: %s [COMMAND] \n\nValid values for COMMAND:\n%s\n"
        exe_name
        (fmt public);
      if experimental <> [] then
        Printf.bprintf
          buf
          "\nExperimental commands (may change or be removed):\n%s\n"
          (fmt experimental);
      if internal <> [] then
        Printf.bprintf buf "\nInternal commands (not part of the public API):\n%s\n" (fmt internal);
      print_string (Buffer.contents buf);
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

module Check (CommandList : COMMAND_LIST) = struct
  module Main =
    Impl
      (CommandList)
      (struct
        let explicit = true
      end)

  let spec = { Main.spec with CommandSpec.name = "check" }

  let command = CommandSpec.command spec Main.main
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
