(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow force-recheck *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "force-recheck";
  doc = "Forces the server to recheck a given list of files";
  usage = Printf.sprintf
    "Usage: %s force-recheck [OPTION]... [FILES]\n\
      Forces the Flow server to recheck a given list of files.\n\n\
      FILES may be omitted if and only if --input-file is used.\n"
    exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> from_flag
    |> flag "--focus" no_arg
      ~doc:("If the server is running in lazy mode, force it to focus on these files")
    |> flag "--input-file" string
      ~doc:("File containing list of files to recheck, one per line. If -, list of files is "^
        "read from the standard input.")
    |> anon "files" (list_of string)
        ~doc:"Specify files to recheck"
  )
}

type args = {
  root: Path.t;
  files: string list;
  focus: bool;
}

let force_recheck (args:args) server_flags =
  let ic, oc = connect server_flags args.root in
  let files = List.map get_path_of_file args.files in
  send_command oc (ServerProt.FORCE_RECHECK (files, args.focus));
  let () = Timeout.input_value ic in
  FlowExitStatus.(exit No_error)

let rec find_parent_that_exists path =
  if Sys.file_exists path
  then path
  else begin
    let newpath = Filename.dirname path in
    (* dirname called repeatedly should eventually return ".", which should
     * always exist. But no harm in being overly cautious. Let's detect
     * infinite recursion *)
    if newpath = path
    then path
    else find_parent_that_exists newpath
  end

let main server_flags root from focus input_file files () =
  FlowEventLogger.set_from from;

  begin match input_file, files with
  | None, (None | Some []) ->
    CommandSpec.usage spec;
    let msg = "FILES may be omitted if and only if --input-file is used" in
    FlowExitStatus.(exit ~msg Commandline_usage_error)
  | _ -> ()
  end;

  let files = get_filenames_from_input ~allow_imaginary:true input_file files in

  let root = guess_root (
    match root, files with
    | Some root, _ -> Some root
    | None, file::_ -> (Some (find_parent_that_exists file))
    | None, [] -> None
  ) in
  let args = { root; files; focus } in
  force_recheck args server_flags

let command = CommandSpec.command spec main
