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
(* Prelude *)
(*****************************************************************************)
(*
 * For the initialization of the server we use the plain 'find' command
 * to find the list of files to analyze.
 *)

(*****************************************************************************)
(* The file extensions we are interested in *)
(*****************************************************************************)

let extensions = [
  ".php"  ; (* normal php file *)
  ".hh"   ; (* Hack extension some open source code is starting to use *)
  ".phpt" ; (* our php template files *)
  ".hhi"  ; (* interface files only visible to the type checker *)
]

let is_directory path = try Sys.is_directory path with Sys_error _ -> false

let is_dot_file path =
  let filename = Filename.basename path in
  String.length filename > 0 && filename.[0] = '.'

let is_php_path path =
  not (is_dot_file path) &&
  List.exists (Filename.check_suffix path) extensions &&
  not (is_directory path)

let is_js_path path =
  not (is_dot_file path) &&
  Filename.check_suffix path ".js" &&
  not (is_directory path)

let escape_spaces = Str.global_replace (Str.regexp " ") "\\ "

let paths_to_path_string paths =
  let stringed_paths = List.map Path.string_of_path paths in
  let escaped_paths =  List.map escape_spaces stringed_paths in
  String.concat " " escaped_paths

let find_with_name paths pattern =
  let paths = paths_to_path_string paths in
  let cmd = Utils.spf "find %s -name \"%s\"" paths pattern in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 16 in
  (try
    while true do
      Buffer.add_channel buf ic 1
    done
  with End_of_file -> ());
  (try ignore (Unix.close_process_in ic) with _ -> ());
  Str.split (Str.regexp "\n") (Buffer.contents buf)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let make_next_files_with_find filter ?(others=[]) root =
  let paths = paths_to_path_string (root::others) in
  let ic = Unix.open_process_in ("find "^paths) in
  let done_ = ref false in
  (* This is subtle, but to optimize latency, we open the process and
   * then return a closure immediately. That way 'find' gets started
   * in parallel and will be ready when we need to get the list of
   * files (although it will be stopped very soon as the pipe buffer
   * will get full very quickly).
   *)
  fun () ->
    if !done_
    (* see multiWorker.mli, this is the protocol for nextfunc *)
    then []
    else
      let result = ref [] in
      let i = ref 0 in
      try
        while !i < 1000 do
          let path = input_line ic in
          if filter path
          then begin
            result := path :: !result;
            incr i;
          end
        done;
        List.rev !result
      with End_of_file ->
        done_ := true;
        (try ignore (Unix.close_process_in ic) with _ -> ());
        !result

let make_next_files_php = make_next_files_with_find is_php_path
let make_next_files_js ~filter =
  make_next_files_with_find (fun x -> is_js_path x && filter x)
