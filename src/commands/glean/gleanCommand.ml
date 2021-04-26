(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils

let spec =
  let open CommandSpec in
  {
    name = "glean";
    doc = "";
    usage =
      Printf.sprintf "Usage: %s glean <path> --output-dir <dirname> --write-root <name>" exe_name;
    args =
      (let open ArgSpec in
      empty
      |> codemod_flags
      |> flag "--output-dir" (required string) ~doc:"Name of directory to output the JSON into"
      |> flag "--write-root" (required string) ~doc:"Prefix to attach to file names (e.g. www)");
  }

let main codemod_flags output_dir write_root =
  if (not (Sys.file_exists output_dir)) || not (Sys.is_directory output_dir) then
    failwith "Output directory doesn't exist. Create it."
  else if Array.length (Sys.readdir output_dir) <> 0 then
    failwith "Output directory is nonempty. Empty it."
  else
    CodemodCommand.main (GleanRunner.make ~output_dir ~write_root) codemod_flags

let command = CommandSpec.command spec main
