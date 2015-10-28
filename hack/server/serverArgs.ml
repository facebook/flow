(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* The options from the command line *)
(*****************************************************************************)

type options = {
  ai_mode          : Ai_options.prepared option;
  check_mode       : bool;
  json_mode        : bool;
  root             : Path.t;
  should_detach    : bool;
  convert          : Path.t option;
  no_load          : bool;
  save_filename    : (state_kind * string) option;
  waiting_client   : Handle.handle option;
}

and state_kind =
  | Complete
  | Mini

(*****************************************************************************)
(* Usage code *)
(*****************************************************************************)
let usage = Printf.sprintf "Usage: %s [WWW DIRECTORY]\n" Sys.argv.(0)

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

module Messages = struct
  let debug         = " debugging mode"
  let ai            = " run ai with options"
  let check         = " check and exit"
  let json          = " output errors in json format (arc lint mode)"
  let daemon        = " detach process"
  let from_vim      = " passed from hh_client"
  let from_emacs    = " passed from hh_client"
  let from_hhclient = " passed from hh_client"
  let convert       = " adds type annotations automatically"
  let save          = " save server state to file"
  let save_mini     = " save mini server state to file"
  let no_load       = " don't load from a saved state"
  let waiting_client= " send message to fd/handle when server has begun \
                      \ starting and again when it's done starting"
end

(*****************************************************************************)
(* CAREFUL!!!!!!! *)
(*****************************************************************************)
(* --json is used for the linters. External tools are relying on the
   format -- don't change it in an incompatible way!
*)
(*****************************************************************************)

(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)

let parse_options () =
  let root          = ref "" in
  let from_vim      = ref false in
  let from_emacs    = ref false in
  let from_hhclient = ref false in
  let debug         = ref false in
  let ai_mode       = ref None in
  let check_mode    = ref false in
  let json_mode     = ref false in
  let should_detach = ref false in
  let convert_dir   = ref None  in
  let save          = ref None in
  let no_load       = ref false in
  let version       = ref false in
  let waiting_client= ref None in
  let cdir          = fun s -> convert_dir := Some s in
  let set_ai   = fun s -> ai_mode := Some (Ai_options.prepare ~server:true s) in
  let set_save      = fun s -> save := Some (Complete, s) in
  let set_save_mini = fun s -> save := Some (Mini, s) in
  let set_wait      = fun fd -> waiting_client := Some fd in
  let options =
    ["--debug"         , Arg.Set debug         , Messages.debug;
     "--ai"            , Arg.String set_ai     , Messages.ai;
     "--check"         , Arg.Set check_mode    , Messages.check;
     "--json"          , Arg.Set json_mode     , Messages.json; (* CAREFUL!!! *)
     "--daemon"        , Arg.Set should_detach , Messages.daemon;
     "-d"              , Arg.Set should_detach , Messages.daemon;
     "--from-vim"      , Arg.Set from_vim      , Messages.from_vim;
     "--from-emacs"    , Arg.Set from_emacs    , Messages.from_emacs;
     "--from-hhclient" , Arg.Set from_hhclient , Messages.from_hhclient;
     "--convert"       , Arg.String cdir       , Messages.convert;
     "--save"          , Arg.String set_save   , Messages.save;
     "--save-mini"     , Arg.String set_save_mini, Messages.save_mini;
     "--no-load"       , Arg.Set no_load       , Messages.no_load;
     "--version"       , Arg.Set version       , "";
     "--waiting-client", Arg.Int set_wait      , Messages.waiting_client;
    ] in
  let options = Arg.align options in
  Arg.parse options (fun s -> root := s) usage;
  if !version then begin
    print_string Build_id.build_id_ohai;
    exit 0
  end;
  (* --json and --save both imply check *)
  let check_mode = !check_mode || !json_mode || !save <> None; in
  (* Conversion mode implies check *)
  let check_mode = check_mode || !convert_dir <> None in
  let convert = Option.map ~f:Path.make !convert_dir in
  if check_mode && !waiting_client <> None then begin
    Printf.eprintf "--check is incompatible with wait modes!\n";
    Exit_status.(exit Input_error)
  end;
  (match !root with
  | "" ->
      Printf.eprintf "You must specify a root directory!\n";
      Exit_status.(exit Input_error)
  | _ -> ());
  let root_path = Path.make !root in
  Wwwroot.assert_www_directory root_path;
  {
    json_mode     = !json_mode;
    ai_mode       = !ai_mode;
    check_mode    = check_mode;
    root          = root_path;
    should_detach = !should_detach;
    convert       = convert;
    no_load       = !no_load;
    save_filename = !save;
    waiting_client= !waiting_client;
  }

(* useful in testing code *)
let default_options ~root = {
  ai_mode = None;
  check_mode = false;
  json_mode = false;
  root = Path.make root;
  should_detach = false;
  convert = None;
  no_load = true;
  save_filename = None;
  waiting_client = None;
}

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let ai_mode options = options.ai_mode
let check_mode options = options.check_mode
let json_mode options = options.json_mode
let root options = options.root
let should_detach options = options.should_detach
let convert options = options.convert
let no_load options = options.no_load
let save_filename options = options.save_filename
let waiting_client options = options.waiting_client
