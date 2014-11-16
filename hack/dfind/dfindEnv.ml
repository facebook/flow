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
(* The environment shared by everyone *)
(*****************************************************************************)

module SSet = Set.Make(String)
module SMap = Map.Make(String)

module Time = struct
  type t = int

  let counter = ref 0
  let get () = incr counter; !counter
  let compare = (-)

  (* The beginning of times *)
  let bot = 0

  let to_string x = string_of_int x
end

module TimeFiles = MonoidAvl.Make(struct
  (* Timestamp + filename *)
  type elt = Time.t * string
  let compare (_, x) (_, y) = String.compare x y

  type monoelt = Time.t
  let neutral = Time.bot
  let make = fst
  let compose = max
end)


type dir = string
type handle = string

(* The directory a client cares about + its handle + the channel where we
 * will output the answer.
*)
type client = dir * handle * Unix.file_descr

type output = {
    oclose       : bool;
    odescr       : Unix.file_descr;
    obuffer      : string;
    mutable opos : int;
  }

type t = {
    (* Work left to output *)
    mutable to_output : output list                           ;

    (* The fsnotify environment, we use this for interacting with fsnotify  *)
            fsnotify  : Fsnotify.env                          ;

    (* The set of files with their timestamp *)
    mutable files     : TimeFiles.t                           ;

    (* The set of new files (files created during an event) *)
    mutable new_files : SSet.t                                ;

    (* The directories (and the files they contain) *)
    mutable dirs      : SSet.t SMap.t                         ;

    (* The user handles *)
            chandles   : ((string * string), Time.t) Hashtbl.t ;

    (* The list of clients listening to changes *)
    mutable clientl   : client list                           ;

    (* The output channel where we want to log errors (/tmp/dfind.log) *)
            log       : out_channel                           ;

    (* Keeps track of when the last query came in, so we can exit after
       a period of inactivity. *)
    mutable last_query : float                                ;

    (* Time since the process started *)
            start_time : float                                ;
  }

(*****************************************************************************)
(* The environment variable containing what we want to skip *)
(*****************************************************************************)

let skip_var = "DFIND_SKIP"

(* The files we want to skip *)
let get_skip_var log =
  try
    let skip_pattern = Sys.getenv skip_var in
    match skip_pattern with
    | "NOTHING" -> None
    | _ ->
        (* It could be useful to log what we are skipping *)
        Printf.fprintf log "%s = \"%s\"\n" skip_var skip_pattern;
        flush log;
        Some (Str.regexp skip_pattern)
  with Not_found -> Some (Str.regexp ".*/wiki/images/.*")

(*****************************************************************************)
(* Functions used to update handles *)
(*****************************************************************************)

let get_handle env handle =
  let res =
    try
      Some (Hashtbl.find env.chandles handle)
    with Not_found ->
      None
  in
  res

let set_handle env handle value =
  Printf.fprintf env.log "Setting handle time (%s, %s): %s\n"
    (fst handle)
    (snd handle)
    (Time.to_string value);
  flush env.log;
  Hashtbl.replace env.chandles handle value;
  ()

(*****************************************************************************)
(* Functions used to update the client list *)
(*****************************************************************************)

let add_client env client =
  env.clientl <- client :: env.clientl;
  ()

let get_clients env = env.clientl

(*****************************************************************************)
(* Building the original environment, this call is called only once
 * by the server (cf server.ml)
 *)
(*****************************************************************************)

let make root =
  let user = Sys.getenv "USER" in
  let log = open_out ("/tmp/dfind_"^user^".log") in
  let fsnotify = Fsnotify.init root log in
  {
    to_output = []                ;
    fsnotify  = fsnotify          ;
    files     = TimeFiles.empty   ;
    new_files = SSet.empty        ;
    dirs      = SMap.empty        ;
    chandles  = Hashtbl.create 23 ;
    clientl   = []                ;
    log       = log               ;
    last_query = Unix.time()      ;
    start_time = Unix.time()      ;
  }

(*****************************************************************************)
(* Output to the clients *)
(*****************************************************************************)

let string_of_files files =
  let buffer = Buffer.create 256 in
  SSet.iter begin fun file ->
    Buffer.add_string buffer file;
    Buffer.add_string buffer "\n";
  end files;
  Buffer.contents buffer

let add_output env ~close descr files =
  let output = {
    oclose  = close;
    odescr  = descr;
    obuffer = string_of_files files;
    opos    = 0;
  } in
  env.to_output <- output :: env.to_output;
  ()

let get_output_descrl env =
  let fdl = List.map (fun o -> o.odescr) env.to_output in
  (* Selecting those that are still alive *)
  List.filter begin fun fd ->
    try
      ignore (Unix.select [] [fd] [] (0.0));
      true
    with _ -> false
  end fdl

let output env =
  let new_output = ref [] in
  List.iter begin fun out ->
    try
      let buffer_size = String.length out.obuffer in
      let size = min 4096 (buffer_size - out.opos) in
      let continue =
        out.opos < String.length out.obuffer &&
        let _, ready, _ = Unix.select [] [out.odescr] [] (0.0) in
        ready <> []
      in
      if continue then begin
        let written = Unix.write out.odescr out.obuffer out.opos size in
        out.opos <- written + out.opos
      end;
      if out.oclose && out.opos >= String.length out.obuffer
      then (try Unix.close out.odescr with _ -> ())
      else (new_output := out :: !new_output)
    with e ->
      (try Unix.close out.odescr with _ -> ())
  end env.to_output;
  env.to_output <- !new_output
