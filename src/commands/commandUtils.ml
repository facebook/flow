(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let print_version () =
  print_endline "Flow, a static type checker for Javascript, version 0.1.1"

(* line split/transform utils *)
module Line : sig
  (* split string at nth line. if it exists, returns pre, line, post *)
  val split_nth : string -> int -> (string * string * string) option

  (* transform nth line, if it exists. returns reconstructed string *)
  val transform_nth : string -> int -> (string -> string) -> string

end = struct
  let breaks = "\r\n"

  let rec eol s x i =
    if i >= x then x else
    if String.contains breaks s.[i] then i else
    eol s x (i + 1)

  let rec line s x n i =
    if n <= 0 then i, eol s x (i + 1) else
    let i = eol s x i in
    if i >= x then x, x else
    line s x (n - 1) (i + 1)

  let split_nth s n =
    let x = String.length s in
    let i, j = line s x n 0 in
    if i = x then None else
    Some String.(sub s 0 i, sub s i (j - i), sub s j (x - j))

  let transform_nth s n f =
    match split_nth s n with
    | Some (pre, s, post) -> pre ^ (f s) ^ post
    | None -> s
end

let arg_set_unit r = Arg.Unit (fun () -> r := true)

let arg_set_enum list r = Arg.Symbol (list, (fun e -> r := e))

let arg_set_string r = Arg.String (fun s -> r := Some s)

let global_kill_time = ref None

let set_timeout max_wait_time_seconds =
  global_kill_time := Some (Sys.time() +. (float_of_int max_wait_time_seconds))

let timeout_go_boom () =
  print_endline "Timeout exceeded, exiting";
  exit 3

let check_timeout () =
  match !global_kill_time with
    | None -> ()
    | Some kill_time ->
        if Sys.time () > kill_time then timeout_go_boom ()

let sleep seconds =
  match !global_kill_time with
    | None -> Unix.sleep seconds
    | Some kill_time ->
        if int_of_float (ceil (kill_time -. Sys.time ())) <= seconds
        then timeout_go_boom ()
        else Unix.sleep seconds

let no_dashes opt =
  if opt.[0] != '-' then opt
  else if opt.[1] != '-' then String.sub opt 1 ((String.length opt) - 1)
  else String.sub opt 2 ((String.length opt) - 2)

let sort_opts opts =
  let cmp (a, _, _) (b, _, _) = String.compare (no_dashes a) (no_dashes b) in
  List.sort cmp opts

type command_params = {
  version : bool ref;
  json : bool ref;
  from : string ref;
  show_all_errors : bool ref;
  retries : int ref;
  retry_if_init : bool ref;
  timeout : int ref;
  no_auto_start : bool ref;
}

let create_command_options accepts_json =
  let command_values = {
    version = ref false;
    json = ref false;
    from = ref "";
    show_all_errors = ref false;
    retries = ref 3;
    retry_if_init = ref true;
    timeout = ref 0;
    no_auto_start = ref false;
  } in
  let command_list = [
    "--version", arg_set_unit command_values.version,
      " Print version number and exit";
    "--timeout", Arg.Set_int command_values.timeout,
      " Maximum time to wait, in seconds";
    "--from", Arg.Set_string command_values.from,
      " Specify client (for use by editor plugins)";
    "--show-all-errors", arg_set_unit command_values.show_all_errors,
      " Print all errors (the default is to truncate after 50 errors)";
    "--retries", Arg.Set_int command_values.retries,
      " Set the number of retries. (default: 3)";
    "--retry-if-init", Arg.Bool (fun x -> command_values.retry_if_init := x),
      " retry if the server is initializing (default: true)";
    "--no-auto-start", arg_set_unit command_values.no_auto_start,
      " If the server if it is not running, do not start it; just exit";
  ] in
  command_values,
  if accepts_json
  then ( "--json", arg_set_unit command_values.json,
          " Output results in JSON format";) :: command_list
  else command_list

let start_flow_server root =
  Printf.fprintf stderr "Flow server launched for %s\n%!"
    (Path.string_of_path root);
  let flow_server = Printf.sprintf "%s start %s 1>&2"
    (Sys.argv.(0))
    (Shell.escape_string_for_shell (Path.string_of_path root)) in
  match Unix.system flow_server with
    | Unix.WEXITED 0 -> ()
    | _ -> (Printf.fprintf stderr "Could not start flow server!\n"; exit 77)

let rec connect_helper autostart retries retry_if_init root =
  check_timeout ();
  try
    if not (ClientUtils.server_exists root)
    then raise ClientExceptions.Server_missing;
    ClientUtils.connect root
  with
  | ClientExceptions.Server_initializing ->
      let init_msg = "flow server still initializing. If it was " ^
                     "just started this can take some time." in
      if retry_if_init
      then (
        Printf.fprintf stderr "%s Retrying... %s\r%!" init_msg (Tty.spinner());
        sleep 1;
        connect_helper autostart retries retry_if_init root
      ) else (
        Printf.fprintf stderr "%s Try again...\n%!" init_msg;
        exit 2
      )
  | ClientExceptions.Server_cant_connect ->
      retry autostart retries retry_if_init root
        1 "Error: could not connect to flow server, retrying..."
  | ClientExceptions.Server_busy ->
      retry autostart retries retry_if_init root
        1 "Error: flow server is busy, retrying..."
  | ClientExceptions.Server_missing ->
    if autostart
    then (
      start_flow_server root;
      retry autostart retries retry_if_init root
        3 "The flow server will be ready in a moment."
    ) else (
      prerr_endline (Utils.spf
          "Error: There is no flow server running in '%s'."
          (Path.string_of_path root));
      exit 2
    )
  | _ -> Printf.fprintf stderr "Something went wrong :(\n%!"; exit 2

and retry autostart retries retry_if_init root delay message =
  check_timeout ();
  if retries > 0
  then (
    prerr_endline message;
    sleep delay;
    let retries = retries - 1 in
    connect_helper autostart retries retry_if_init root
  ) else (
    prerr_endline "Out of retries, exiting!";
    exit 2
  )

let connect_with_autostart command_values =
  connect_helper
    (not !(command_values.no_auto_start))
    !(command_values.retries)
    !(command_values.retry_if_init)

(* Given a file or directory, find a valid flow root directory *)
let guess_root dir_or_file =
  let dir_or_file = match dir_or_file with
  | Some dir_or_file -> dir_or_file
  | None -> "." in
  let dir = if Sys.is_directory dir_or_file
    then dir_or_file
    else Filename.dirname dir_or_file in
  match ClientArgs.guess_root ".flowconfig" (Path.mk_path dir) 50 with
  | Some root -> root
  | None -> Printf.fprintf stderr "Could not find a .flowconfig in %s or any \
      of its parent directories\nsee \"flow init --help\" for more info\n%!" dir; exit 2

(* convert 1,1 based line/column to 1,0 for internal use *)
let convert_input_pos (line, column) =
  let column =
    if column > 1
    then column - 1
    else 0 in
  (line, column)

(* copied (and adapted) from Hack's ClientCheck module *)
let get_path_of_file file =
  let path = Path.mk_path file in
  if Path.file_exists path
  then Path.string_of_path path
  else
    (* Filename.concat does not return a normalized path when the file does
       not exist. Thus, we do it on our own... *)
    let file = Files_js.normalize_path (Sys.getcwd()) file in
    let path = Path.mk_path file in
    Path.string_of_path path
