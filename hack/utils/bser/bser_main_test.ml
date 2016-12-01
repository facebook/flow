(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let usage = "Usage: bser_test [filename] [--to_json|--to_bser|--roundtrip]\n"

type mode =
  | To_bser
  | To_json
  | Roundtrip

let run_test mode path : unit =
  match mode with
  | To_json -> path
     |> Bser.json_of_bser_file
     |> Hh_json.json_to_output stdout;
     output_string stdout "\n"

  | To_bser -> path
     |> Hh_json.json_of_file ~strict:false
     |> Bser.json_to_channel stdout;
     output_string stdout "\n"

  | Roundtrip -> path
     |> failwith "Not implemented yet"



let () =
  if Array.length (Sys.argv) != 3 then begin
    output_string stderr usage;
    flush stderr;
    exit 2;
  end;
  let path = Sys.argv.(1) in
  if not (Sys.file_exists path) || (Sys.is_directory path) then begin
    let message = Printf.sprintf
      "Path `%s` not found or isn't a regular file\n"
      Sys.argv.(1)
    in
    output_string stderr (usage ^ message);
    flush stderr;
    exit 2
  end;
  let mode = match Sys.argv.(2) with
    | "--to_json" -> To_json
    | "--to_bser" -> To_bser
    | _ -> output_string stderr usage;
           flush stderr;
           exit 2
  in
  Unix.handle_unix_error run_test mode path
