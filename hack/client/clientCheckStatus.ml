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
open Sys_utils

module C = Tty

let print_reason_color ~(first:bool) ~(code:int) ((p, s): Pos.absolute * string) =
  let line, start, end_ = Pos.info_pos p in
  let code_clr = C.Normal C.Yellow in
  let err_clr  = if first then C.Bold C.Red else C.Normal C.Green in
  let file_clr = if first then C.Bold C.Red else C.Normal C.Red in
  let line_clr = C.Normal C.Yellow in
  let col_clr  = C.Normal C.Cyan in

  let to_print_code = if not first then [] else [
    (C.Normal C.Default, " (");
    (code_clr,       Errors.error_code_to_string code);
    (C.Normal C.Default, ")");
  ] in
  let to_print = [
    (file_clr,           p.Pos.pos_file);
    (C.Normal C.Default, ":");
    (line_clr,           string_of_int line);
    (C.Normal C.Default, ":");
    (col_clr,            string_of_int start);
    (C.Normal C.Default, ",");
    (col_clr,            string_of_int end_);
    (C.Normal C.Default, ": ");
    (err_clr,            s);
  ] @ to_print_code @ [(C.Normal C.Default, "\n")] in

  if not first then Printf.printf "  " else ();
  if Unix.isatty Unix.stdout
  then
    C.print to_print
  else
    let strings = List.map (fun (_,x) -> x) to_print in
    List.iter (Printf.printf "%s") strings

let print_error_color e =
  let code = Errors.get_code e in
  let msg_list = Errors.to_list e in
  print_reason_color ~first:true ~code (List.hd msg_list);
  List.iter (print_reason_color ~first:false ~code) (List.tl msg_list)

let check_status connect (args:client_check_env) =
  let name = "hh_server" in
  let response = with_timeout 6
    ~on_timeout:(fun _ -> raise Server_busy)
    ~do_:(fun () ->
      let ic, oc = connect args in
      ServerMsg.cmd_to_channel oc (ServerMsg.STATUS args.root);
      ServerMsg.response_from_channel ic) in
  match response with
  | ServerMsg.NO_ERRORS ->
    ServerError.print_errorl args.output_json [] stdout;
    exit 0
  | ServerMsg.ERRORS error_list ->
    if args.output_json || args.from <> ""
    then ServerError.print_errorl args.output_json error_list stdout
    else List.iter print_error_color error_list;
    exit 2
  | ServerMsg.DIRECTORY_MISMATCH d ->
    Printf.printf "%s is running on a different directory.\n" name;
    Printf.printf "server_root: %s, client_root: %s\n"
      (Path.string_of_path d.ServerMsg.server)
      (Path.string_of_path d.ServerMsg.client);
    flush stdout;
    raise Server_directory_mismatch
  | ServerMsg.SERVER_DYING ->
    Printf.printf "Server has been killed for %s\n"
      (Path.string_of_path args.root);
    exit 2
  | ServerMsg.PONG ->
      Printf.printf "Why on earth did the server respond with a pong?\n%!";
      exit 2
