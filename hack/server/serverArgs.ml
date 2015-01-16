(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* File parsing the arguments on the command line *)
(*****************************************************************************)

open Utils

(*****************************************************************************)
(* The options from the command line *)
(*****************************************************************************)

type options = {
  check_mode       : bool;
  json_mode        : bool;
  root             : Path.path;
  should_detach    : bool;
  convert          : Path.path option;
  load_save_opt    : env_store_action option;
  (* Configures only the workers. Workers can have more relaxed GC configs as
   * they are short-lived processes *)
  gc_control       : Gc.control;
  assume_php       : bool;
}

and env_store_action =
  | Load of string
  | Save of string

(*****************************************************************************)
(* Usage code *)
(*****************************************************************************)
let usage = Printf.sprintf "Usage: %s [WWW DIRECTORY]\n" Sys.argv.(0)

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

module Messages = struct
  let debug         = " debugging mode"
  let check         = " check and exit"
  let json          = " output errors in json format (arc lint mode)"
  let daemon        = " detach process"
  let from_vim      = " passed from hh_client"
  let from_emacs    = " passed from hh_client"
  let from_hhclient = " passed from hh_client"
  let convert       = " adds type annotations automatically"
  let save          = " save server state to file"
  let no_load       = " don't load from a saved state"
end


(*****************************************************************************)
(* CAREFUL!!!!!!! *)
(*****************************************************************************)
(* --json is used for the linters. External tools are relying on the
   format -- don't change it in an incompatible way!
*)
(*****************************************************************************)

let make_gc_control config =
  let minor_heap_size = match SMap.get "gc_minor_heap_size" config with
    | Some s -> int_of_string s
    | None -> ServerConfig.gc_control.Gc.minor_heap_size in
  let space_overhead = match SMap.get "gc_space_overhead" config with
    | Some s -> int_of_string s
    | None -> ServerConfig.gc_control.Gc.space_overhead in
  { ServerConfig.gc_control with Gc.minor_heap_size; Gc.space_overhead; }

let config_assume_php config =
  match SMap.get "assume_php" config with
    | Some s -> bool_of_string s
    | None -> true

(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)

let parse_options () =
  let root          = ref "" in
  let from_vim      = ref false in
  let from_emacs    = ref false in
  let from_hhclient = ref false in
  let debug         = ref false in
  let check_mode    = ref false in
  let json_mode     = ref false in
  let should_detach = ref false in
  let convert_dir   = ref None  in
  let save          = ref "" in
  let no_load       = ref false in
  let version       = ref false in
  let cdir          = fun s -> convert_dir := Some s in
  let options =
    ["--debug"         , Arg.Set debug         , Messages.debug;
     "--check"         , Arg.Set check_mode    , Messages.check;
     "--json"          , Arg.Set json_mode     , Messages.json; (* CAREFUL!!! *)
     "--daemon"        , Arg.Set should_detach , Messages.daemon;
     "-d"              , Arg.Set should_detach , Messages.daemon;
     "--from-vim"      , Arg.Set from_vim      , Messages.from_vim;
     "--from-emacs"    , Arg.Set from_emacs    , Messages.from_emacs;
     "--from-hhclient" , Arg.Set from_hhclient , Messages.from_hhclient;
     "--convert"       , Arg.String cdir       , Messages.convert;
     "--save"          , Arg.Set_string save   , Messages.save;
     "--no-load"       , Arg.Set no_load       , Messages.no_load;
     "--version"       , Arg.Set version       , "";
    ] in
  let options = Arg.align options in
  Arg.parse options (fun s -> root := s) usage;
  if !version then begin
    print_string Build_id.build_id_ohai;
    exit 0
  end;
  (* json implies check *)
  let check_mode = !check_mode || !json_mode; in
  (* Conversion mode implies check *)
  let check_mode = check_mode || !convert_dir <> None in
  let convert = Utils.opt_map Path.mk_path (!convert_dir) in
  (match !root with
  | "" ->
      Printf.fprintf stderr "You must specify a root directory!\n";
      exit 2
  | _ -> ());
  let root_path = Path.mk_path !root in
  Wwwroot.assert_www_directory root_path;
  let hhconfig = Path.string_of_path (Path.concat root_path ".hhconfig") in
  let config = Config_file.parse hhconfig in
  let load_save_opt = match !save with
    | "" -> begin
      if !no_load then None
      else
        match SMap.get "load_script" config with
        | None -> None
        | Some cmd ->
            let cmd =
              if Filename.is_relative cmd then (!root)^"/"^cmd else cmd in
            Some (Load cmd)
      end
    | s -> Some (Save s) in
  { json_mode     = !json_mode;
    check_mode    = check_mode;
    root          = root_path;
    should_detach = !should_detach;
    convert       = convert;
    load_save_opt = load_save_opt;
    gc_control    = make_gc_control config;
    assume_php    = config_assume_php config;
  }

(* useful in testing code *)
let default_options ~root = {
  check_mode = false;
  json_mode = false;
  root = Path.mk_path root;
  should_detach = false;
  convert = None;
  load_save_opt = None;
  gc_control = ServerConfig.gc_control;
  assume_php = true;
}

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let check_mode options = options.check_mode
let json_mode options = options.json_mode
let root options = options.root
let should_detach options = options.should_detach
let convert options = options.convert
let load_save_opt options = options.load_save_opt
let gc_control options = options.gc_control
let assume_php options = options.assume_php
