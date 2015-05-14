(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ClientEnv
open ClientExceptions
open Utils

module Cmd = ServerCommand
module Rpc = ServerRpc

let connect args =
  let ic, oc = ClientUtils.connect args.root in
  if not args.output_json && Tty.spinner_used() then
    Tty.print_clear_line stderr;
  (ic, oc)

let get_list_files (args:client_check_env): string list =
  let ic, oc = connect args in
  Cmd.(stream_request oc LIST_FILES);
  let res = ref [] in
  try
    while true do
      res := (input_line ic) :: !res
    done;
    assert false
  with End_of_file -> !res

let print_all ic =
  try
    while true do
      Printf.printf "%s\n" (input_line ic);
    done
  with End_of_file -> ()

let expand_path file =
  let path = Path.make file in
  if Path.file_exists path
  then Path.to_string path
  else
    let file = Filename.concat (Sys.getcwd()) file in
    let path = Path.make file in
    if Path.file_exists path
    then Path.to_string path
    else begin
      Printf.printf "File not found\n";
      exit 2
    end

let rec main args retries =
  let has_timed_out = match args.timeout with
    | None -> false
    | Some t -> Unix.time() > t
  in
  if has_timed_out then begin
    Printf.fprintf stderr "Error: hh_client hit timeout, giving up!\n%!";
    exit 7
  end else try
    match args.mode with
    | MODE_LIST_FILES ->
        let infol = get_list_files args in
        List.iter (Printf.printf "%s\n") infol
    | MODE_LIST_MODES ->
        let ic, oc = connect args in
        Cmd.(stream_request oc LIST_MODES);
        begin try
          while true do print_endline (input_line ic) done;
        with End_of_file -> () end
    | MODE_COLORING file ->
        let file_input = match file with
          | "-" ->
            let content = ClientUtils.read_stdin_to_string () in
            ServerUtils.FileContent content
          | _ ->
            let file = expand_path file in
            ServerUtils.FileName file
        in
        let conn = connect args in
        let pos_level_l = Cmd.rpc conn @@ Rpc.COVERAGE_LEVELS file_input in
        ClientColorFile.go file_input args.output_json pos_level_l;
        exit 0
    | MODE_COVERAGE file ->
        let conn = connect args in
        let counts_opt =
          Cmd.rpc conn @@ Rpc.COVERAGE_COUNTS (expand_path file) in
        ClientCoverageMetric.go ~json:args.output_json counts_opt;
        exit 0
    | MODE_FIND_CLASS_REFS name ->
        let conn = connect args in
        let results =
          Cmd.rpc conn @@ Rpc.FIND_REFS (ServerFindRefs.Class name) in
        ClientFindRefs.go results args.output_json;
        exit 0
    | MODE_FIND_REFS name ->
        let conn = connect args in
        let pieces = Str.split (Str.regexp "::") name in
        let action =
          try
            match pieces with
            | class_name :: method_name :: _ ->
                ServerFindRefs.Method (class_name, method_name)
            | method_name :: _ -> ServerFindRefs.Function method_name
            | _ -> raise Exit
          with _ -> Printf.fprintf stderr "Invalid input\n"; exit 1 in
        let results = Cmd.rpc conn @@ Rpc.FIND_REFS action in
        ClientFindRefs.go results args.output_json;
        exit 0
    | MODE_DUMP_SYMBOL_INFO files ->
        let ic, oc = connect args in
        ClientSymbolInfo.go files ic oc expand_path
    | MODE_REFACTOR ->
        ClientRefactor.go args;
        exit 0
    | MODE_IDENTIFY_FUNCTION arg ->
        let tpos = Str.split (Str.regexp ":") arg in
        let line, char =
          try
            match tpos with
            | [line; char] ->
                int_of_string line, int_of_string char
            | _ -> raise Exit
          with _ ->
            Printf.fprintf stderr "Invalid position\n"; exit 1
        in
        let conn = connect args in
        let content = ClientUtils.read_stdin_to_string () in
        let result =
          Cmd.rpc conn @@ Rpc.IDENTIFY_FUNCTION (content, line, char) in
        print_endline result
    | MODE_TYPE_AT_POS arg ->
        let tpos = Str.split (Str.regexp ":") arg in
        let fn, line, char =
          try
            match tpos with
            | [filename; line; char] ->
                let fn = expand_path filename in
                ServerUtils.FileName fn, int_of_string line, int_of_string char
            | [line; char] ->
                let content = ClientUtils.read_stdin_to_string () in
                ServerUtils.FileContent content,
                int_of_string line,
                int_of_string char
            | _ -> raise Exit
          with _ ->
            Printf.fprintf stderr "Invalid position\n"; exit 1
        in
        let conn = connect args in
        let pos, ty = Cmd.rpc conn @@ Rpc.INFER_TYPE (fn, line, char) in
        ClientTypeAtPos.go pos ty args.output_json;
        exit 0
    | MODE_ARGUMENT_INFO arg ->
        let tpos = Str.split (Str.regexp ":") arg in
        let line, char =
          try
            match tpos with
            | [line; char] ->
                int_of_string line, int_of_string char
            | _ -> raise Exit
          with _ ->
            Printf.fprintf stderr "Invalid position\n"; exit 1
        in
        let conn = connect args in
        let content = ClientUtils.read_stdin_to_string () in
        let results =
          Cmd.rpc conn @@ Rpc.ARGUMENT_INFO (content, line, char) in
        ClientArgumentInfo.go results args.output_json;
        exit 0
    | MODE_AUTO_COMPLETE ->
        let conn = connect args in
        let content = ClientUtils.read_stdin_to_string () in
        let results = Cmd.rpc conn @@ Rpc.AUTOCOMPLETE content in
        ClientAutocomplete.go results args.output_json;
        exit 0
    | MODE_OUTLINE ->
        let content = ClientUtils.read_stdin_to_string () in
        let conn = connect args in
        let results = Cmd.rpc conn @@ Rpc.OUTLINE content in
        ClientOutline.go results args.output_json;
        exit 0
    | MODE_METHOD_JUMP_CHILDREN class_ ->
        let conn = connect args in
        let results = Cmd.rpc conn @@ Rpc.METHOD_JUMP (class_, true) in
        ClientMethodJumps.go results true args.output_json;
        exit 0
    | MODE_METHOD_JUMP_ANCESTORS class_ ->
        let conn = connect args in
        let results = Cmd.rpc conn @@ Rpc.METHOD_JUMP (class_, false) in
        ClientMethodJumps.go results false args.output_json;
        exit 0
    | MODE_STATUS ->
        let conn = connect args in
        let error_list = Cmd.rpc conn Rpc.STATUS in
        if args.output_json || args.from <> "" || error_list = []
        then ServerError.print_errorl args.output_json error_list stdout
        else List.iter ClientCheckStatus.print_error_color error_list;
        exit (if error_list = [] then 0 else 2)
    | MODE_VERSION ->
        Printf.printf "%s\n" (Build_id.build_id_ohai);
    | MODE_SHOW classname ->
        let ic, oc = connect args in
        Cmd.(stream_request oc (SHOW classname));
        print_all ic
    | MODE_SEARCH (query, type_) ->
        let conn = connect args in
        let results = Cmd.rpc conn @@ Rpc.SEARCH (query, type_) in
        ClientSearch.go results args.output_json;
        exit 0
    | MODE_LINT fnl ->
        let conn = connect args in
        let fnl = List.fold_left begin fun acc fn ->
          match Sys_utils.realpath fn with
          | Some path -> path :: acc
          | None ->
              prerr_endlinef "Could not find file '%s'" fn;
              acc
        end [] fnl in
        let results = Cmd.rpc conn @@ Rpc.LINT fnl in
        ClientLint.go results args.output_json;
        exit 0
    | MODE_LINT_ALL code ->
        let conn = connect args in
        let results = Cmd.rpc conn @@ Rpc.LINT_ALL code in
        ClientLint.go results args.output_json;
        exit 0
    | MODE_UNSPECIFIED -> assert false
  with
  | Server_initializing ->
      let init_msg = "hh_server still initializing. If it was "^
                     "just started this can take some time." in
      if args.retry_if_init
      then begin
        Printf.fprintf stderr "%s Retrying... %s\r" init_msg (Tty.spinner());
        flush stderr;
        Unix.sleep(1);
        main args retries
      end else begin
        Printf.fprintf stderr "%s Try again...\n" init_msg;
        flush stderr;
      end
  | Server_cant_connect ->
      if retries > 1
      then begin
        Printf.fprintf stderr "Error: could not connect to hh_server, retrying... %s\r"
          (Tty.spinner());
        flush stderr;
        Unix.sleep(1);
        main args (retries-1)
      end else begin
        Printf.fprintf stderr "Error: could not connect to hh_server, giving up!\n";
        flush stderr;
        exit 3
      end
  | Server_busy ->
      if retries > 1
      then begin
        Printf.fprintf stderr "Error: hh_server is busy, retrying... %s\r"
          (Tty.spinner());
        flush stderr;
        Unix.sleep(1);
        main args (retries-1)
      end else begin
        Printf.fprintf stderr "Error: hh_server is busy, giving up!\n";
        flush stderr;
        exit 4;
      end
  | Server_missing ->
      ClientStart.start_server { ClientStart.
        root = args.root;
        wait = false;
        no_load = args.no_load;
      };
      if args.autostart
      then Printf.eprintf ("The server will be ready in a few seconds "^^
        "(a couple of minutes if your files are cold)!\n")
      else begin
        Printf.eprintf ("Error: no hh_server running. Either start hh_server"^^
          " yourself or run hh_client without --autostart-server false\n%!");
        exit 6;
      end;
      if retries > 1
      then
        (* No sleep in this one -- if the server is missing, the startup code
         * will wait to continue until at least the server has started
         * initalizing to continue. *)
        main args (retries-1)
  | Server_out_of_date ->
      let name = "hh_server" in
      if args.autostart
      then begin
        Printf.eprintf "%s is outdated, going to launch a new one.\n%!" name;
        (* Don't decrement retries -- the server is definitely not running, so
         * the next time round will hit Server_missing above, *but* before that
         * will actually start the server -- we need to make sure that happens.
         *)
        main args retries
      end else
        Printf.eprintf "%s is outdated, killing it.\n" name;
        exit 6
  | Server_directory_mismatch ->
      if retries > 1
      then begin
        Unix.sleep(3);
        main args (retries-1)
      end else begin
        Printf.fprintf stderr "The server will be ready in a few seconds (a couple of minutes if your files are cold)!\n";
        flush stderr;
        exit 6;
      end
  | _ ->
      if retries > 1
      then begin
        Printf.fprintf stderr "Error: hh_server disconnected or crashed, retrying... %s\r"
          (Tty.spinner());
        flush stderr;
        Unix.sleep(1);
        main args (retries-1)
      end else begin
        prerr_string
          ("Error: hh_server disconnected or crashed, giving up!\n"^
          "Server may have entered a bad state: Try `hh_client restart`\n");
        flush stderr;
        exit 5;
      end
