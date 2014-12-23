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

let connect args =
  let ic, oc = ClientUtils.connect args.root in
  if not args.output_json && Tty.spinner_used() then
    Tty.print_clear_line stderr;
  (ic, oc)

let get_list_files (args:client_check_env): string list =
  let ic, oc = connect args in
  ServerMsg.cmd_to_channel oc ServerMsg.LIST_FILES;
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
  let path = Path.mk_path file in
  if Path.file_exists path
  then Path.string_of_path path
  else
    let file = Filename.concat (Sys.getcwd()) file in
    let path = Path.mk_path file in
    if Path.file_exists path
    then Path.string_of_path path
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
    | MODE_COLORING file ->
        let ic, oc = connect args in
        let file_input = match file with
          | "-" ->
            let content = ClientUtils.read_stdin_to_string () in
            ServerMsg.FileContent content
          | _ ->
            let file = expand_path file in
            ServerMsg.FileName file
        in
        let command = ServerMsg.PRINT_COVERAGE_LEVELS file_input in
        ServerMsg.cmd_to_channel oc command;
        let pos_level_l : ServerColorFile.result = Marshal.from_channel ic in
        ClientColorFile.go file_input args.output_json pos_level_l;
        exit 0
    | MODE_COVERAGE file ->
        let ic, oc = connect args in
        let command = ServerMsg.CALC_COVERAGE (expand_path file) in
        ServerMsg.cmd_to_channel oc command;
        let counts_opt : ServerCoverageMetric.result =
          Marshal.from_channel ic in
        ClientCoverageMetric.go args.output_json counts_opt;
        exit 0
    | MODE_FIND_CLASS_REFS name ->
        let ic, oc = connect args in
        let command = ServerMsg.FIND_REFS (ServerMsg.Class name) in
        ServerMsg.cmd_to_channel oc command;
        let results : ServerFindRefs.result = Marshal.from_channel ic in
        ClientFindRefs.go results args.output_json;
        exit 0
    | MODE_FIND_REFS name ->
        let ic, oc = connect args in
        let pieces = Str.split (Str.regexp "::") name in
        let action =
          try
            match pieces with
            | class_name :: method_name :: _ ->
                ServerMsg.Method (class_name, method_name)
            | method_name :: _ -> ServerMsg.Function method_name
            | _ -> raise Exit
          with _ -> Printf.fprintf stderr "Invalid input\n"; exit 1 in
        let command = ServerMsg.FIND_REFS action in
        ServerMsg.cmd_to_channel oc command;
        let results : ServerFindRefs.result = Marshal.from_channel ic in
        ClientFindRefs.go results args.output_json;
        exit 0
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
      let ic, oc = connect args in
      let content = ClientUtils.read_stdin_to_string () in
      let command = ServerMsg.IDENTIFY_FUNCTION (content, line, char) in
      ServerMsg.cmd_to_channel oc command;
      print_all ic
    | MODE_TYPE_AT_POS arg ->
      let tpos = Str.split (Str.regexp ":") arg in
      let fn, line, char =
        try
          match tpos with
          | [filename; line; char] ->
              let fn = expand_path filename in
              ServerMsg.FileName fn, int_of_string line, int_of_string char
          | [line; char] ->
              let content = ClientUtils.read_stdin_to_string () in
              ServerMsg.FileContent content, int_of_string line, int_of_string char
          | _ -> raise Exit
        with _ ->
          Printf.fprintf stderr "Invalid position\n"; exit 1
      in
      let ic, oc = connect args in
      ServerMsg.cmd_to_channel oc (ServerMsg.INFER_TYPE (fn, line, char));
      let ((pos, ty) : ServerInferType.result) = Marshal.from_channel ic in
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
      let ic, oc = connect args in
      let content = ClientUtils.read_stdin_to_string () in
      ServerMsg.cmd_to_channel oc
          (ServerMsg.ARGUMENT_INFO (content, line, char));
      let results : ServerArgumentInfo.result = Marshal.from_channel ic in
      ClientArgumentInfo.go results args.output_json;
      exit 0
    | MODE_AUTO_COMPLETE ->
      let ic, oc = connect args in
      let content = ClientUtils.read_stdin_to_string () in
      let command = ServerMsg.AUTOCOMPLETE content in
      ServerMsg.cmd_to_channel oc command;
      let results : AutocompleteService.result = Marshal.from_channel ic in
      ClientAutocomplete.go results args.output_json;
      exit 0
    | MODE_OUTLINE ->
      let content = ClientUtils.read_stdin_to_string () in
      let ic, oc = connect args in
      let command = ServerMsg.OUTLINE content in
      ServerMsg.cmd_to_channel oc command;
      let results : ServerFileOutline.result = Marshal.from_channel ic in
      ClientOutline.go results args.output_json;
      exit 0
    | MODE_METHOD_JUMP_CHILDREN class_ ->
      let ic, oc = connect args in
      let command = ServerMsg.METHOD_JUMP (class_, true) in
      ServerMsg.cmd_to_channel oc command;
      let results : MethodJumps.result list = Marshal.from_channel ic in
      ClientMethodJumps.go results true args.output_json;
      exit 0
    | MODE_METHOD_JUMP_ANCESTORS class_ ->
      let ic, oc = connect args in
      let command = ServerMsg.METHOD_JUMP (class_, false) in
      ServerMsg.cmd_to_channel oc command;
      let results : MethodJumps.result list = Marshal.from_channel ic in
      ClientMethodJumps.go results false args.output_json;
      exit 0
    | MODE_STATUS -> ClientCheckStatus.check_status connect args
    | MODE_VERSION ->
      Printf.printf "%s\n" (Build_id.build_id_ohai);
    | MODE_SHOW classname ->
        let ic, oc = connect args in
        ServerMsg.cmd_to_channel oc (ServerMsg.SHOW classname);
        print_all ic
    | MODE_SEARCH (query, type_) ->
        let ic, oc = connect args in
        ServerMsg.cmd_to_channel oc (ServerMsg.SEARCH (query, type_));
        let results : ServerSearch.result = Marshal.from_channel ic in
        ClientSearch.go results args.output_json;
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
      if retries > 1
      then begin
        Unix.sleep(3);
        main args (retries-1)
      end else begin
        if args.autostart
        then Printf.fprintf stderr "The server will be ready in a few seconds (a couple of minutes if your files are cold)!\n"
        else Printf.fprintf stderr "Error: no hh_server running. Either start hh_server yourself or run hh_client without --autostart-server false\n%!";
        flush stderr;
        exit 6;
      end
  | Server_out_of_date ->
      if args.autostart
      then begin
        Unix.sleep(1);
        (* Don't decrement retries -- the server is definitely not running, so
         * the next time round will hit Server_missing above, *but* before that
         * will actually start the server -- we need to make sure that happens.
         *)
        main args retries
      end else
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
        Printf.fprintf stderr "Error: hh_server disconnected or crashed, giving up!\n";
        flush stderr;
        exit 5;
      end
