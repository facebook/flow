(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ClientCommand
open ClientEnv
open Utils

let rec guess_root config start recursion_limit : Path.t option =
  let fs_root = Path.make "/" in
  if start = fs_root then None
  else if Wwwroot.is_www_directory ~config start then Some start
  else if recursion_limit <= 0 then None
  else guess_root config (Path.parent start) (recursion_limit - 1)

let parse_command () =
  if Array.length Sys.argv < 2
  then CKNone
  else match String.lowercase Sys.argv.(1) with
  | "check" -> CKCheck
  | "start" -> CKStart
  | "stop" -> CKStop
  | "restart" -> CKRestart
  | "build" -> CKBuild
  | "prolog" -> CKProlog
  | _ -> CKNone

let parse_without_command options usage command =
  let args = ref [] in
  Arg.parse (Arg.align options) (fun x -> args := x::!args) usage;
  match List.rev !args with
  | x::rest when (String.lowercase x) = (String.lowercase command) -> rest
  | args -> args

let get_root ?(config=".hhconfig") path_opt =
  let start_str = match path_opt with
    | None -> "."
    | Some s -> s in
  let start_path = Path.make start_str in
  let root = match guess_root config start_path 50 with
    | None -> start_path
    | Some r -> r in
  Wwwroot.assert_www_directory ~config root;
  root

(* *** *** NB *** *** ***
 * Commonly-used options are documented in hphp/hack/man/hh_client.1 --
 * if you are making significant changes you need to update the manpage as
 * well. Experimental or otherwise volatile options need not be documented
 * there, but keep what's there up to date please. *)
let parse_check_args cmd =
  (* arg parse output refs *)
  let mode = ref MODE_UNSPECIFIED in
  let retries = ref 800 in
  let output_json = ref false in
  let retry_if_init = ref true in
  let no_load = ref false in
  let timeout = ref None in
  let autostart = ref true in
  let from = ref "" in
  let version = ref false in

  (* custom behaviors *)
  let set_from x () = from := x in
  let set_mode x () =
    if !mode <> MODE_UNSPECIFIED
    then raise (Arg.Bad "only a single mode should be specified")
    else mode := x
  in

  (* parse args *)
  let usage =
    match cmd with
    | CKCheck -> Printf.sprintf
      "Usage: %s check [OPTION]... [WWW-ROOT]\n\n\
      WWW-ROOT is assumed to be current directory if unspecified\n"
      Sys.argv.(0)
    | CKNone -> Printf.sprintf
      "Usage: %s [COMMAND] [OPTION]... [WWW-ROOT]\n\n\
      Valid values for COMMAND:\n\
        \tcheck\
          \t\tShows current Hack errors\n\
        \tstart\
          \t\tStarts a Hack server\n\
        \tstop\
          \t\tStops a Hack server\n\
        \trestart\
          \t\tRestarts a Hack server\n\
      \n\
      Default values if unspecified:\n\
        \tCOMMAND\
          \t\tcheck\n\
        \tWWW-ROOT\
          \tCurrent directory\n\
      \n\
      Check command options:\n"
      Sys.argv.(0)
    | _ -> failwith "No other keywords should make it here"
  in
  let options = [
    (* modes *)
    "--status", Arg.Unit (set_mode MODE_STATUS),
      " (mode) show a human readable list of errors (default)";
    "--type-at-pos", Arg.String (fun x -> set_mode (MODE_TYPE_AT_POS x) ()),
      " (mode) show type at a given position in file [line:character]";
    "--args-at-pos", Arg.String (fun x -> set_mode (MODE_ARGUMENT_INFO x) ()),
      "";
    "--list-files", Arg.Unit (set_mode MODE_LIST_FILES),
      " (mode) list files with errors";
    "--list-modes", Arg.Unit (set_mode MODE_LIST_MODES),
      " (mode) list all files with their associated hack modes";
    "--auto-complete", Arg.Unit (set_mode MODE_AUTO_COMPLETE),
      " (mode) auto-completes the text on stdin";
    "--color", Arg.String (fun x -> set_mode (MODE_COLORING x) ()),
      " (mode) pretty prints the file content showing what is checked (give '-' for stdin)";
    "--coverage", Arg.String (fun x -> set_mode (MODE_COVERAGE x) ()),
      " (mode) calculates the extent of typing of a given file or directory";
    "--find-refs", Arg.String (fun x -> set_mode (MODE_FIND_REFS x) ()),
      " (mode) finds references of the provided method name";
    "--find-class-refs", Arg.String (fun x -> set_mode (MODE_FIND_CLASS_REFS x) ()),
      " (mode) finds references of the provided class name";
    "--dump-symbol-info", Arg.String (fun files ->
        set_mode (MODE_DUMP_SYMBOL_INFO files) ()
        ),
      (*  Input format:
       *  The file list can either be "-" which accepts the input from stdin
       *  separated by newline(for long list) or directly from command line
       *  separated by semicolon.
       *  Output format:
       *    [
       *      "function_calls": list of fun_calls;
       *    ]
       *  Note: results list can be in any order *)
      "";
    "--identify-function", Arg.String (fun x -> set_mode (MODE_IDENTIFY_FUNCTION x) ()),
      " (mode) print the full function name at the position [line:character] of the text on stdin";
    "--refactor", Arg.Unit (set_mode MODE_REFACTOR),
      "";
    "--search", Arg.String (fun x -> set_mode (MODE_SEARCH (x, "")) ()),
      " (mode) fuzzy search symbol definitions";
    "--search-class",
      Arg.String (fun x -> set_mode
          (MODE_SEARCH (x, "class")) ()),
      " (mode) fuzzy search class definitions";
    "--search-function",
      Arg.String (fun x -> set_mode
          (MODE_SEARCH (x, "function")) ()),
      " (mode) fuzzy search function definitions";
    "--search-typedef",
      Arg.String (fun x -> set_mode
          (MODE_SEARCH (x, "typedef")) ()),
      " (mode) fuzzy search typedef definitions";
    "--search-constant",
      Arg.String (fun x -> set_mode
          (MODE_SEARCH (x, "constant")) ()),
      " (mode) fuzzy search constant definitions";
    "--outline", Arg.Unit (set_mode MODE_OUTLINE),
      " (mode) prints an outline of the text on stdin";
    "--inheritance-children", Arg.String (fun x -> set_mode (MODE_METHOD_JUMP_CHILDREN x) ()),
      " (mode) prints a list of all related classes or methods to the given class";
    "--inheritance-ancestors", Arg.String (fun x -> set_mode (MODE_METHOD_JUMP_ANCESTORS x) ()),
      " (mode) prints a list of all related classes or methods to the given class";
    "--show", Arg.String (fun x -> set_mode (MODE_SHOW x) ()),
      " (mode) show human-readable type info for the given name; output is not meant for machine parsing";
    "--lint", Arg.Rest begin fun fn ->
        mode := match !mode with
          | MODE_UNSPECIFIED -> MODE_LINT [fn]
          | MODE_LINT fnl -> MODE_LINT (fn :: fnl)
          | _ -> raise (Arg.Bad "only a single mode should be specified")
      end,
      " (mode) lint the given list of files";
    "--lint-all", Arg.Int (fun x -> set_mode (MODE_LINT_ALL x) ()),
      " (mode) find all occurrences of lint with the given error code";
    "--version", Arg.Set version,
      " (mode) show version and exit\n";
    (* Create a checkpoint which can be used to retrieve changed files later *)
    "--create-checkpoint", Arg.String (fun x -> set_mode (MODE_CREATE_CHECKPOINT x) ()),
      "";
    (* Retrieve changed files since input checkpoint.
     * Output is separated by newline.
     * Exit code will be non-zero if no checkpoint is found *)
    "--retrieve-checkpoint",
      Arg.String (fun x -> set_mode (MODE_RETRIEVE_CHECKPOINT x) ()),
      "";
    (* Delete an existing checkpoint.
     * Exitcode will be non-zero if no checkpoint is found *)
    "--delete-checkpoint",
      Arg.String (fun x -> set_mode (MODE_DELETE_CHECKPOINT x) ()),
      "";

    (* flags *)
    "--json", Arg.Set output_json,
      " output json for machine consumption. (default: false)";
    "--retries", Arg.Set_int retries,
      spf " set the number of retries. (default: %d)" !retries;
    "--retry-if-init", Arg.Bool (fun x -> retry_if_init := x),
      " retry if the server is initializing (default: true)";
    "--no-load", Arg.Set no_load,
      " start from a fresh state";
    "--from", Arg.Set_string from,
      " set this so we know who is calling hh_client";
    "--timeout",  Arg.Float (fun x -> timeout := Some (Unix.time() +. x)),
      " set the timeout in seconds (default: no timeout)";
    "--autostart-server", Arg.Bool (fun x -> autostart := x),
      " automatically start hh_server if it's not running (default: true)\n";

    (* deprecated *)
    "--from-vim", Arg.Unit (fun () -> from := "vim"; retries := 0; retry_if_init := false),
      " (deprecated) equivalent to --from vim --retries 0 --retry-if-init false";
    "--from-emacs", Arg.Unit (set_from "emacs"),
      " (deprecated) equivalent to --from emacs";
    "--from-arc-diff", Arg.Unit (set_from "arc_diff"),
      " (deprecated) equivalent to --from arc_diff";
    "--from-arc-land", Arg.Unit (set_from "arc_land"),
      " (deprecated) equivalent to --from arc_land";
    "--from-check-trunk", Arg.Unit (set_from "check_trunk"),
      " (deprecated) equivalent to --from check_trunk";
  ] in
  let args = parse_without_command options usage "check" in

  if !version then begin
    print_endline Build_id.build_id_ohai;
    exit 0;
  end;

  (* fixups *)
  if !mode == MODE_UNSPECIFIED then mode := MODE_STATUS;
  let root =
    match args with
    | [] -> get_root None
    | [x] -> get_root (Some x)
    | _ ->
        Printf.fprintf stderr "Error: please provide at most one www directory\n%!";
        exit 1;
  in
  let () = if (!from) = "emacs" then
      Printf.fprintf stdout "-*- mode: compilation -*-\n%!"
  in
  CCheck {
    mode = !mode;
    root = root;
    from = !from;
    output_json = !output_json;
    retry_if_init = !retry_if_init;
    retries = !retries;
    timeout = !timeout;
    autostart = !autostart;
    no_load = !no_load;
  }

let parse_start_env command =
  let usage =
    Printf.sprintf
      "Usage: %s %s [OPTION]... [WWW-ROOT]\n\
      %s a Hack server\n\n\
      WWW-ROOT is assumed to be current directory if unspecified\n"
      Sys.argv.(0) command (String.capitalize command) in
  let wait = ref false in
  let no_load = ref false in
  let options = [
    "--wait", Arg.Set wait,
    " wait for the server to finish initializing (default: false)";
    "--no-load", Arg.Set no_load,
    " start from a fresh state"
  ] in
  let args = parse_without_command options usage command in
  let root =
    match args with
    | [] -> get_root None
    | [x] -> get_root (Some x)
    | _ ->
        Printf.fprintf stderr
          "Error: please provide at most one www directory\n%!";
        exit 1 in
  { ClientStart.
    root = root;
    wait = !wait;
    no_load = !no_load;
  }

let parse_start_args () =
  CStart (parse_start_env "start")

let parse_restart_args () =
  CRestart (parse_start_env "restart")

let parse_stop_args () =
  let usage =
    Printf.sprintf
      "Usage: %s stop [OPTION]... [WWW-ROOT]\n\
      Stop a hack server\n\n\
      WWW-ROOT is assumed to be current directory if unspecified\n"
      Sys.argv.(0) in
  let options = [] in
  let args = parse_without_command options usage "stop" in
  let root =
    match args with
    | [] -> get_root None
    | [x] -> get_root (Some x)
    | _ ->
        Printf.fprintf stderr "Error: please provide at most one www directory\n%!";
        exit 1
  in CStop {ClientStop.root = root}

let parse_build_args () =
  let usage =
    Printf.sprintf
      "Usage: %s build [WWW-ROOT]\n\
      Generates build files\n"
      Sys.argv.(0) in
  let steps = ref None in
  let no_steps = ref None in
  let verbose = ref false in
  let serial = ref false in
  let test_dir = ref None in
  let grade = ref true in
  let check = ref false in
  let is_push = ref false in
  let clean = ref false in
  (* todo: for now better to default to true here, but this is temporary! *)
  let clean_before_build = ref true in
  let incremental = ref false in
  let run_scripts = ref true in
  let wait = ref false in
  let options = [
    "--steps", Arg.String (fun x ->
      steps := Some (Str.split (Str.regexp ",") x)),
    " comma-separated list of build steps to run";
    "--no-steps", Arg.String (fun x ->
      no_steps := Some (Str.split (Str.regexp ",") x)),
    " comma-separated list of build steps not to run";
    "--no-run-scripts", Arg.Clear run_scripts,
    " don't run unported arc build scripts";
    "--serial", Arg.Set serial,
    " run without parallel worker processes";
    "--test-dir", Arg.String (fun x -> test_dir := Some x),
    " <dir> generates into <dir> and compares with root";
    "--no-grade", Arg.Clear grade,
    " skip full comparison with root";
    "--check", Arg.Set check,
    " run some sanity checks on the server state";
    "--push", Arg.Set is_push,
    " run steps appropriate for push build";
    "--clean", Arg.Set clean,
    " erase all previously generated files";
    "--clean-before-build", Arg.Set clean_before_build,
    " erase previously generated files before building (default)";
    "--no-clean-before-build", Arg.Clear clean_before_build,
    " do not erase previously generated files before building";
    (* Don't document --incremental option for now *)
    "--incremental", Arg.Set incremental, "";
    "--wait", Arg.Set wait,
    " wait forever for hh_server intialization (default: false)";
    "--verbose", Arg.Set verbose,
    " guess what";
  ] in
  let args = parse_without_command options usage "build" in
  let root =
    match args with
    | [x] -> get_root (Some x)
    | _ -> Printf.printf "%s\n" usage; exit 2
  in
  CBuild { ClientBuild.
    root = root;
    wait = !wait;
    build_opts = { ServerBuild.
      steps = !steps;
      no_steps = !no_steps;
      run_scripts = !run_scripts;
      serial = !serial;
      test_dir = !test_dir;
      grade = !grade;
      is_push = !is_push;
      clean = !clean;
      clean_before_build = !clean_before_build;
      check = !check;
      incremental = !incremental;
      user = Sys_utils.logname ();
      verbose = !verbose;
    }
  }

let parse_prolog_args () =
  let usage =
    Printf.sprintf
      "Usage: %s prolog [WWW-ROOT]\n\
      run prolog interpreter on code database\n"
      Sys.argv.(0) in
  let options = [] in
  let args = parse_without_command options usage "prolog" in
  let root =
    match args with
    | [x] -> get_root (Some x)
    | _ -> Printf.printf "%s\n" usage; exit 2
  in
  CProlog { ClientProlog.root = root; }

let parse_args () =
  match parse_command () with
    | CKNone
    | CKCheck as cmd -> parse_check_args cmd
    | CKStart -> parse_start_args ()
    | CKStop -> parse_stop_args ()
    | CKRestart -> parse_restart_args ()
    | CKBuild -> parse_build_args ()
    | CKProlog -> parse_prolog_args ()
