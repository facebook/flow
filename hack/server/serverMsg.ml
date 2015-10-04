(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Sys_utils

(**
 * Commands and responses hh_server deals with
 *
 * This code is responsible for serializing/deserializing messages.
 *
 * It doesn't really matter if the code is clean or not. The only
 * thing to keep in mind is that the code must remain backwards compatible since
 * old clients end up hitting new servers during updates.
 *)

type build_opts = {
  steps: string list option; (* steps for hack build to run.
                         None means 'all' *)
  no_steps: string list option; (* ...but don't run these steps *)
  run_scripts: bool; (* when true, run remaining arc build steps
                     that we haven't figured out how to port yet*)
  serial: bool; (* when true, don't use parallel workers *)
  test_dir: string option; (* test dir to generate into *)
  grade: bool; (* when true, diff test output against www and print
                  some stats *)
  check: bool; (* some sanity checking *)
  clean_before_build: bool; (* when true, do a clean build *)
  clean: bool; (* when true just clean all generated files *)
  is_push: bool; (* for push builds *)
  incremental: bool; (* for incremental build *)
  wait: bool; (* when true, wait forever for server initialization *)
  (* user the client is running as; if the server is running as a different user
   * we will probably have hard-to-debug permissions issues, so error out *)
  user: string;
  verbose: bool;
}


type find_refs_action =
| Class of string
| Method of string * string
| Function of string

type refactor_action =
| ClassRename of string * string (* old_name * new_name *)
| MethodRename of string * string * string (* class_name * old_name * new_name*)
| FunctionRename of string * string (* old_name * new_name *)

type file_input =
| FileName of string
| FileContent of string

type insert_patch = {
  pos: Pos.absolute;
  text: string;
}

type patch =
| Insert of insert_patch
| Remove of Pos.absolute
| Replace of insert_patch

type command =
| ERROR_OUT_OF_DATE
| STATUS of Path.path
| LIST_FILES
| LIST_MODES
| AUTOCOMPLETE of string
| SHOW of string
| KILL
| PING
| BUILD of build_opts
| PROLOG
| FIND_REFS of find_refs_action
| IDENTIFY_FUNCTION of string * int * int
| OUTLINE of string
| METHOD_JUMP of (string * bool)
| INFER_TYPE of file_input * int * int (* filename|content, line, char *)
| REFACTOR of refactor_action
| SEARCH of string * string
| SUGGEST of string list
| ARGUMENT_INFO of string * int * int
| CALC_COVERAGE of string
| PRINT_COVERAGE_LEVELS of file_input
| LINT of string list
| LINT_ALL of int

let cmd_to_channel (oc:out_channel) (cmd:command): unit =
  Printf.fprintf oc "%s\n" Build_id.build_id_ohai;
  Marshal.to_channel oc cmd [];
  flush oc

let cmd_from_channel (ic:in_channel): command =
  let s = input_line ic in
  if s <> Build_id.build_id_ohai
  then ERROR_OUT_OF_DATE
  else Marshal.from_channel ic

type directory_mismatch = {
  server: Path.path;
  client: Path.path;
}

type response =
| SERVER_OUT_OF_DATE
| DIRECTORY_MISMATCH of directory_mismatch
| NO_ERRORS
| ERRORS of Pos.absolute Errors.error_ list
| SERVER_DYING
| PONG

type build_progress =
| BUILD_PROGRESS of string
| BUILD_ERROR of string
| BUILD_FINISHED

let response_to_string = function
  | SERVER_OUT_OF_DATE -> "Server Out of Date"
  | DIRECTORY_MISMATCH _ -> "Directory Mismatch"
  | NO_ERRORS -> "No Errors"
  | ERRORS _ -> "Some Errors"
  | SERVER_DYING -> "Server Dying"
  | PONG -> "Pong"

let response_to_channel (oc:out_channel) (cmd:response): unit =
  (* flush immediately so that the client knows we're not hung; see
   * response_from_channel below *)
  Printf.fprintf oc "%s\n%!" Build_id.build_id_ohai;
  Marshal.to_channel oc cmd [];
  flush oc

let response_from_channel (ic:in_channel): response =
  let s = input_line ic in
  if s <> Build_id.build_id_ohai
  then SERVER_OUT_OF_DATE
  (* there may be a lot of data returned, so (un)marshalling may take a while;
   * suspend any active timeouts for now. Since we've already received the
   * build id, we know that the server has finished computing the data it wants
   * to send us (i.e. it has not hung) *)
  else with_timeout 0
    ~on_timeout:(fun _ -> ())
    ~do_:(fun () -> Marshal.from_channel ic)
