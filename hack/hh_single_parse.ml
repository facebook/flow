(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let usage = Printf.sprintf "Usage: %s filename\n" Sys.argv.(0)

let print_error error = error
  |> Errors.to_absolute
  |> Errors.to_string
  |> output_string stderr

let parse_and_print filename =
  let file = Relative_path.create Relative_path.Dummy filename in
  let errorl, result =
    Errors.do_ begin fun () ->
      Parser_hack.from_file file
    end
  in
  if not (Errors.is_empty errorl) then begin
    Errors.iter_error_list print_error errorl;
    exit 1
  end;
  let str = Debug.dump_ast (Ast.AProgram result.Parser_hack.ast) in
  Printf.printf "%s" str

let main filename =
  EventLogger.init (Daemon.devnull ()) 0.0;
  SharedMem.(init default_config);
  if (String.length filename) = 0 then begin
    Printf.eprintf "%s" usage;
    exit 1
  end;
  Unix.handle_unix_error parse_and_print filename

let () =
  Arg.parse [] main usage;
