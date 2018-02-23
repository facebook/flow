(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow port (transform docblock-style annotations) command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "port";
  doc = "Shows ported type annotations for given files";
  usage = Printf.sprintf
    "Usage: %s port [OPTION]... [FILE]...\n\n\
      Ports types in one or more files\n\n\
      Example usage:\n\
      \t%s port file1 file2\n"
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> from_flag
    |> anon "files" (required (list_of string))
        ~doc:"File(s) to port"
  )
}

let main option_values root from files () =
  FlowEventLogger.set_from from;
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> Some (List.hd files)
  ) in
  let files = List.map expand_path files in
  let request = ServerProt.Request.PORT files in
  let patch_map = match connect_and_make_request option_values root request with
  | ServerProt.Response.PORT patch_map -> patch_map
  | response -> failwith_bad_response ~request ~response
  in
  SMap.iter (fun file patches_or_err ->
    match patches_or_err with
    | Ok patches ->
      Printf.printf "%s\n%s" file patches
    | Error exn ->
      Printf.eprintf
        "Could not port docblock-style annotations for %s\n%s"
        file
        ((Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ()));
  ) patch_map;
  flush stderr;
  flush stdout

let command = CommandSpec.command spec main
