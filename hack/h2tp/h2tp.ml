(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module A = Ast
module CE = Common_exns
module Opts = Converter_options
module Sys = Sys_ext
open Utils

let parse_options () =
  let src = ref None
  and dest = ref None
  and convert_collections = ref true
  and usage = Printf.sprintf
    "Usage: %s source destination\n"
    Sys.argv.(0) in

  let fail_with_usage error_str = begin
    prerr_endline error_str;
    prerr_endline usage;
    exit 1
  end in

  let parse_arg s = begin
    match !Arg.current with
      | 1 -> src := Some s;
      | 2 -> dest := Some s;
      | _ -> fail_with_usage "This program only accepts two arguments";
  end in

  let speclist = [
    ("--no-collections", Arg.Clear convert_collections,
      "Enables a mode which assumes there are no collections used");
  ] in

  Arg.parse speclist parse_arg usage;

  if !Arg.current < 3 then fail_with_usage
    "This program requires a source file and a destination file";

  let options = {
    Opts.convert_collections = !convert_collections
  } in
  (unsafe_opt !src, unsafe_opt !dest, options)

let _ =
  let (src, dest, options) = parse_options () in
  try
    SharedMem.init ();
    Opts.set options;
    Engine.go (Sys.chop_dirsymbol src) (Sys.chop_dirsymbol dest);
    print_string "The Conversion was successful\n"
  with
  | e -> Sys.die (CE.flatten_error e)
