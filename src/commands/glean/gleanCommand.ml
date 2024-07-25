(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
      |> flag "--output-dir" (optional string) ~doc:"Name of directory to output the JSON into"
      |> flag "--write-root" (optional string) ~doc:"Prefix to attach to file names (e.g. www)"
      |> flag
           "--include-direct-deps"
           truthy
           ~doc:"Additionally index direct dependencies of input files"
      |> flag
           "--include-reachable-deps"
           truthy
           ~doc:"Additionally index reachable dependencies of input files"
      |> flag "--schema-version" truthy ~doc:"Show schema version used by the indexer"
      |> flag "--glean-log" truthy ~doc:"Log extra information from Glean run"
      |> flag
           "--glean-timeout"
           (required ~default:600 int)
           ~doc:"Maximum time to wait per file, in seconds"
      );
  }

let main
    codemod_flags
    output_dir_opt
    write_root_opt
    include_direct_deps
    include_reachable_deps
    show_schema_version
    glean_log
    glean_timeout
    () =
  if show_schema_version then
    print_endline (Int.to_string GleanRunner.all_schema_version)
  else (
    if glean_timeout < 0 then
      failwith
        (Utils_js.spf
           "--glean-timeout must be a positive integer, or 0 to disable. Got %d"
           glean_timeout
        );
    match (output_dir_opt, write_root_opt) with
    | (Some output_dir, Some write_root) ->
      if (not (Sys.file_exists output_dir)) || not (Sys.is_directory output_dir) then
        failwith "Output directory doesn't exist. Create it."
      else if Array.length (Sys.readdir output_dir) <> 0 then
        failwith "Output directory is nonempty. Empty it."
      else
        CodemodCommand.main
          (GleanRunner.make
             ~output_dir
             ~write_root
             ~include_direct_deps
             ~include_reachable_deps
             ~glean_log
             ~glean_timeout
          )
          codemod_flags
          ()
    | _ -> failwith "--output-dir and --write-root are required."
  )

let command = CommandSpec.command spec main
