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

(*****************************************************************************)
(* Debugging sections.
 *
 * This is useful when we modify the formatter and we want to make sure
 * we didn't introduce errors.
 * You can run 'hh_format --debug directory_name' and it will output all
 * the errors.
 *
 * It takes all the files in a directory and verifies that:
 * -) The result parses
 * -) formatting is idempotent (for each file)
 *
 *)
(*****************************************************************************)
exception Format_error

let debug () fnl =
  let modes = [Some FileInfo.Mstrict; Some FileInfo.Mpartial] in
  List.fold_left begin fun () (filepath : Path.t) ->
    try
      let content = Sys_utils.cat (filepath :> string) in

      (* Checking that we can parse the output *)
      let parsing_errors1, parser_output1 = Errors.do_ begin fun () ->
        let rp =
          Relative_path.create Relative_path.Dummy (filepath :> string) in
        Parser_hack.program rp content
      end in
      if parser_output1.Parser_hack.file_mode = None || parsing_errors1 <> []
      then raise Exit;

      if parsing_errors1 <> []
      then begin
        Printf.eprintf
          "The file had a syntax error before we even started: %s\n%!"
          (filepath :> string);
      end;

      let content = Format_hack.program modes filepath content in
      let content =
        match content with
        | Format_hack.Success content -> content
        | Format_hack.Disabled_mode ->
            raise Exit
        | Format_hack.Parsing_error _ ->
            Printf.eprintf "Parsing: %s\n%!" (filepath :> string);
            ""
        | Format_hack.Internal_error ->
            Printf.eprintf "Internal: %s\n%!" (filepath :> string);
            ""
      in

      (* Checking for idempotence *)
      let content2 = Format_hack.program modes filepath content in
      let content2 =
        match content2 with
        | Format_hack.Success content2 -> content2
        | _ -> raise Format_error
      in
      if content <> content2
      then begin
        Printf.eprintf
          "Applying the formatter twice lead to different results: %s\n%!"
          (filepath :> string);
        let () = Random.self_init() in
        let nbr = string_of_int (Random.int 100000) in
        let tmp = "/tmp/xx_"^nbr in
        let file1 = tmp^"_1.php" in
        let file2 = tmp^"_2.php" in
        let oc = open_out file1 in
        output_string oc content;
        close_out oc;
        let oc = open_out file2 in
        output_string oc content2;
        close_out oc;
        let _ = Sys.command ("diff "^file1^" "^file2) in
        let _ = Sys.command ("rm "^file1^" "^file2) in
        flush stdout
      end;

      (* Checking that we can parse the output *)
      let parsing_errors2, _parser_output2 = Errors.do_ begin fun () ->
        let rp = Relative_path.(create Dummy (filepath :> string)) in
        Parser_hack.program rp content
      end in
      if parsing_errors2 <> []
      then begin
        Printf.eprintf
          "The output of the formatter could not be parsed: %s\n%!"
          (filepath :> string);
      end;

      ()
    with
    | Format_error ->
        Printf.eprintf "Format error: %s\n%!" (filepath :> string);
    | Exit ->
        ()
  end () fnl

let debug_directory dir =
  let path = Path.make dir in
  let next = compose
    (rev_rev_map Path.make)
    (Find.make_next_files FindUtils.is_php path) in
  let workers = Worker.make GlobalConfig.nbr_procs GlobalConfig.gc_control in
  MultiWorker.call
    (Some workers)
    ~job:debug
    ~neutral:()
    ~merge:(fun () () -> ())
    ~next

(*****************************************************************************)
(* Parsing the command line *)
(*****************************************************************************)

let parse_args() =
  let from = ref 0 in
  let to_ = ref max_int in
  let files = ref [] in
  let diff = ref false in
  let modes = ref [Some FileInfo.Mstrict; Some FileInfo.Mpartial] in
  let root = ref None in
  let debug = ref false in
  let test = ref false in
  let apply_mode = ref Format_mode.Print in
  let set_apply_mode mode () = match !apply_mode with
    | Format_mode.Patch -> () (* Patch implies In_place but not vice versa *)
    | Format_mode.In_place when mode = Format_mode.Patch -> apply_mode := mode
    | Format_mode.In_place -> ()
    | Format_mode.Print -> apply_mode := mode
  in
  Arg.parse
    [
      "--from", Arg.Set_int from,
      "[int] start after character position";

      "--to", Arg.Set_int to_,
      "[int] stop after character position";

      "-i", Arg.Unit (set_apply_mode Format_mode.In_place),
      "modify the files in place";

      "--in-place", Arg.Unit (set_apply_mode Format_mode.In_place),
      "modify the files in place";

      "-p", Arg.Unit (set_apply_mode Format_mode.Patch),
      "interactively choose hunks of patches to apply (implies --in-place)";

      "--patch", Arg.Unit (set_apply_mode Format_mode.Patch),
      "interactively choose hunks of patches to apply (implies --in-place)";

      "--diff", Arg.Set diff,
      "formats the changed lines in a diff "^
      "(example: git diff | hh_format --diff)";

      "--yolo", Arg.Unit (fun () ->
        modes := [Some FileInfo.Mdecl; None (* PHP *)]),
      "Formats *only* PHP and decl-mode files. Results may be unreliable; "^
      "you should *always* inspect the formatted output before committing it!";

      "--root", Arg.String (fun x -> root := Some x),
      "specifies a root directory (useful in diff mode)";

      "--debug", Arg.Set debug, "";
      "--test", Arg.Set test, "";
    ]
    (fun file -> files := file :: !files)
    (Printf.sprintf "Usage: %s (filename|directory)" Sys.argv.(0));
  !files, !from, !to_, !apply_mode, !debug, !diff, !modes, !root, !test

(*****************************************************************************)
(* Formats a file in place *)
(*****************************************************************************)

let format_in_place modes (filepath : Path.t) =
  let content = Sys_utils.cat (filepath :> string) in
  match Format_hack.program modes filepath content with
  | Format_hack.Success result ->
      let oc = open_out (filepath :> string) in
      output_string oc result;
      close_out oc;
      None
  | Format_hack.Internal_error ->
      Some "Internal error\n"
  | Format_hack.Parsing_error errorl ->
      Some (Errors.to_string (Errors.to_absolute (List.hd errorl)))
  | Format_hack.Disabled_mode ->
      None

(*****************************************************************************)
(* Formats all the hack files in a directory (in place) *)
(*****************************************************************************)

let job_in_place modes acc fnl =
  List.fold_left begin fun acc filename ->
    match format_in_place modes filename with
    | None -> acc
    | Some err -> err :: acc
  end acc fnl

let directory modes dir =
  let path = Path.make dir in
  let next = compose
    (rev_rev_map Path.make)
    (Find.make_next_files FindUtils.is_php path) in
  let workers = Worker.make GlobalConfig.nbr_procs GlobalConfig.gc_control in
  let messages =
    MultiWorker.call
      (Some workers)
      ~job:(job_in_place modes)
      ~neutral:[]
      ~merge:List.rev_append
      ~next
  in
  List.iter (Printf.fprintf stderr "%s\n") messages

(*****************************************************************************)
(* Applies the formatter directly to a string. *)
(*****************************************************************************)

let format_string modes file from to_ content =
  match Format_hack.region modes file from to_ content with
  | Format_hack.Success content ->
      output_string stdout content
  | Format_hack.Internal_error ->
      Printf.fprintf stderr "Internal error\n%!";
      exit 2
  | Format_hack.Parsing_error error ->
      Printf.fprintf stderr "Parsing error\n%s\n%!"
        (Errors.to_string (Errors.to_absolute (List.hd error)));
      exit 2
  | Format_hack.Disabled_mode ->
      exit 0

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let read_stdin () =
  let buf = Buffer.create 256 in
  try
    while true do
      Buffer.add_string buf (read_line());
      Buffer.add_char buf '\n';
    done;
    assert false
  with End_of_file ->
    Buffer.contents buf

let format_stdin modes from to_ =
  let content = read_stdin () in
  format_string modes Path.dummy_path from to_ content

(*****************************************************************************)
(* The main entry point. *)
(*****************************************************************************)

let () =
  SharedMem.(init default_config);
  PidLog.log_oc := Some (open_out "/dev/null");
  let files, from, to_, apply_mode, debug, diff, modes, root, test =
    parse_args() in
  if not test then FormatEventLogger.init (Unix.time());
  match files with
  | [] when diff ->
      let prefix =
        match root with
        | None ->
            Printf.eprintf "No root specified, trying to guess one\n";
            let root = ClientArgs.get_root None in
            Printf.eprintf "Guessed root: %a\n%!" Path.output root;
            root
        | Some root -> Path.make root
      in
      let diff = read_stdin () in
      let file_and_modified_lines = Format_diff.parse_diff prefix diff in
      Format_diff.apply modes apply_mode ~diff:file_and_modified_lines
  | _ when diff ->
      Printf.fprintf stderr "--diff mode expects no files\n";
      exit 2
  | [] when apply_mode <> Format_mode.Print ->
      Printf.fprintf stderr "Cannot modify stdin in-place\n";
      exit 2
  | [] -> format_stdin modes from to_
  | [dir] when Sys.is_directory dir ->
      if debug
      then debug_directory dir
      else directory modes dir
  | [filename] ->
      let filepath = Path.make filename in
      (match apply_mode with
      | Format_mode.Print ->
          format_string modes filepath from to_ (Path.cat filepath)
      | Format_mode.In_place -> begin
          match format_in_place modes filepath with
          | None -> ()
          | Some error ->
              Printf.eprintf "Error: %s\n" error;
              exit 2
        end
      | Format_mode.Patch ->
          Printf.eprintf "Error: --patch only supported in diff mode\n";
          exit 2);
  | _ ->
      Printf.eprintf "More than one file given\n";
      exit 2
