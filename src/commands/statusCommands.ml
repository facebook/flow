(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

(***********************************************************************)
(* flow status (report current error set) command impl *)
(***********************************************************************)

module Impl = struct
  (* explicit == called with "flow status ..."
     rather than simply "flow ..." *)
  let parse explicit =
    let option_values, options = CommandUtils.create_command_options true in
    let options = CommandUtils.sort_opts options in
    let usage = match explicit with
    | true ->
      Printf.sprintf
        "Usage: %s status [OPTION]... [ROOT]\n\
        Shows current Flow errors by asking the Flow server.\n\n\
        Flow will search upward for a .flowconfig file, beginning at ROOT.\n\
        ROOT is assumed to be the current directory if unspecified.\n\
        A server will be started if none is running over ROOT."
        Sys.argv.(0)
    | false ->
      (* NOTE: new commands must be added manually here, currently *)
      Printf.sprintf
        "Usage: %s [COMMAND] \n\n\
        Valid values for COMMAND:\n\
          \ \ autocomplete\
            \tQueries autocompletion information\n\
          \ \ check\
            \t\tDoes a full Flow check and prints the results\n\
          \ \ find-module\
            \tShows filenames for one or more modules\n\
          \ \ get-def\
            \tGets the definition location of a variable or property\n\
          \ \ get-importers\
            \tGets a list of all importers for one or more given modules\n\
          \ \ get-imports\
            \tGet names of all modules imported by one or more given\
            \ modules\n\
          \ \ init\
            \t\tInitializes a directory to be used as a flow root directory\n\
          \ \ port\
            \t\tShows ported type annotations for given files\n\
          \ \ server\
            \tRuns a Flow server (not normally invoked from the command line)\n\
          \ \ single\
            \tDoes a single-threaded check (testing)\n\
          \ \ start\
            \t\tStarts a Flow server\n\
          \ \ status\
            \t(default) Shows current Flow errors by asking the Flow server\n\
          \ \ stop\
            \t\tStops a Flow server\n\
          \ \ suggest\
            \tShows type annotation suggestions for given files\n\
          \ \ type-at-pos\
            \tShows the type at a given file and position\n\
        \n\
        Default values if unspecified:\n\
          \ \ COMMAND\
            \tstatus\n\
        \n\
        Status command options:"
        Sys.argv.(0)
    in
    let args = match explicit with
      | true ->
          ClientArgs.parse_without_command options usage "status"
      | false ->
          let args = ref [] in
          Arg.parse (Arg.align options) (fun x -> args := x::!args) usage;
          List.rev !args
    in
    let root = match args with
      | [] -> None
      | [x] -> Some x
      | _ ->
          Arg.usage options usage;
          exit 2
    in
    let root = CommandUtils.guess_root root in
    {
      ClientEnv.mode = ClientEnv.MODE_STATUS;
      ClientEnv.root = root;
      ClientEnv.from = !(option_values.CommandUtils.from);
      ClientEnv.output_json = !(option_values.CommandUtils.json);
      ClientEnv.retry_if_init = !(option_values.CommandUtils.retry_if_init);
      ClientEnv.retries = !(option_values.CommandUtils.retries);
      ClientEnv.timeout = None;
      ClientEnv.autostart = true;
      ClientEnv.server_options_cmd = None;
    }

  open ClientEnv

  type env = client_check_env

  let check_status connect (args:env) =
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle
      (fun _ -> raise ClientExceptions.Server_busy)
    );
    ignore(Unix.alarm 6);

    let name = "flow" in

    (* Check if a server is up *)
    if not (ClientUtils.server_exists args.root)
    then begin
      ignore (Unix.alarm 0);
      if args.autostart
      then
        (* fork the server and raise an exception *)
        CommandUtils.start_flow_server args.root;
      raise ClientExceptions.Server_missing
    end;

    let ic, oc = connect args in
    ServerProt.cmd_to_channel oc (ServerProt.STATUS args.root);
    let response = ServerProt.response_from_channel ic in
    ignore (Unix.alarm 0);
    match response with
    | ServerProt.DIRECTORY_MISMATCH d ->
      Printf.printf "%s is running on a different directory.\n" name;
      Printf.printf "server_root: %s, client_root: %s\n"
        (Path.string_of_path d.ServerProt.server)
        (Path.string_of_path d.ServerProt.client);
      flush stdout;
      raise ClientExceptions.Server_directory_mismatch
    | ServerProt.ERRORS e ->
      if args.output_json || args.from <> ""
      then Errors_js.print_errorl args.output_json e stdout
      else (
        List.iter Errors_js.print_error_color e;
        print_newline ();
        Printf.printf "Found %d errors\n" (List.length e);
        exit 2
      )
    | ServerProt.NO_ERRORS ->
      Errors_js.print_errorl args.output_json [] stdout;
      exit 0
    | ServerProt.PONG ->
        Printf.printf "Why on earth did the server respond with a pong?\n%!";
        exit 2
    | ServerProt.SERVER_DYING ->
      Printf.printf "Server has been killed for %s\n"
        (Path.string_of_path args.root);
      exit 2
    | ServerProt.SERVER_OUT_OF_DATE ->
      if args.autostart
      then Printf.printf "%s is outdated, going to launch a new one.\n" name
      else Printf.printf "%s is outdated, killing it.\n" name;
      flush stdout;
      raise ClientExceptions.Server_missing

  let rec main env =
    try
      (* TODO: This code should really use commandUtils's connect utilities to
         avoid code duplication *)
      check_status
        (fun env -> ClientUtils.connect env.root)
        env
    with
    | ClientExceptions.Server_initializing ->
        if env.ClientEnv.retry_if_init
        then (
          Printf.fprintf stderr "Flow server still initializing\n%!";
          Unix.sleep 1;
          main env
        ) else (
          prerr_endline "Flow server still initializing, giving up!";
          exit 2
        )
    | ClientExceptions.Server_cant_connect ->
        retry env 1 "Error: could not connect to flow server"
    | ClientExceptions.Server_busy ->
        retry env 1 "Error: flow server is busy"
    | ClientExceptions.Server_missing ->
        retry env 3 "The flow server will be ready in a moment"
    | _ ->
        Printf.fprintf stderr "Something went wrong :(\n%!";
        exit 2

  and retry env sleep msg =
    if env.retries > 0
    then begin
      Printf.fprintf stderr "%s\n%!" msg;
      Unix.sleep sleep;
      let retries = env.ClientEnv.retries - 1 in
      main { env with ClientEnv.retries }
    end else begin
      Printf.fprintf stderr "Out of retries, exiting!\n%!";
      exit 2
    end
end

module Status = struct
  open Impl
  let run () = main (parse true)
end

module Default = struct
  open Impl
  let run () = main (parse false)
end
