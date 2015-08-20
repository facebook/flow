(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

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
    let cwd = Sys.getcwd () ^ "/" in
    let file_path = [
      (file_clr, lstrip p.Pos.pos_file cwd);
      (C.Normal C.Default, ":");
    ] in
    C.print (file_path @ to_print)
  else
    let strings = List.map (fun (_,x) -> x) to_print in
    Printf.printf "%s:" p.Pos.pos_file;
    List.iter (Printf.printf "%s") strings

let print_error_color e =
  let code = Errors.get_code e in
  let msg_list = Errors.to_list e in
  print_reason_color ~first:true ~code (List.hd msg_list);
  List.iter (print_reason_color ~first:false ~code) (List.tl msg_list)
