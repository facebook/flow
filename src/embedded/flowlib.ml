(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

external get_embedded_flowlib_data : string -> string option =
  "get_embedded_flowlib_data"

let touch_root r =
  let filter = FindUtils.is_js in
  Find.iter_files ~filter [ r ] (Sys_utils.try_touch ~follow_symlinks:true)

(* There are several verify-use race conditions here (and in Hack's file
 * handling in general, really). Running the server as root is likely to be a
 * security risk. Be careful. *)
let extract path data =
  let oc = Unix.open_process_out ("tar xzC " ^ (Path.to_string path)) in
  output_string oc data;
  flush oc;
  ignore (Unix.close_process_out oc);
  touch_root path;
  true

let extract_win32_res tmpdir =
  match Hhi_win32res.read_index () with
  | None -> false
  | Some idx ->
    Hhi_win32res.dump_files tmpdir idx;
    touch_root tmpdir;
    true

let extract_flowlib dir =
  if Sys.win32 then
    extract_win32_res dir
  else
    match get_embedded_flowlib_data (Sys_utils.executable_path ()) with
    | Some data -> extract dir data
    | None ->
    (* Look for the flowlib.tar.gz in the place where it normally resides, so
     * that we support debugging binaries that don't have the section embedded,
     * such as bytecode builds. *)
      let exe_dir = Filename.dirname (Sys_utils.executable_path ()) in
      let path = exe_dir ^ "/flowlib.tar.gz" in
      if Sys.file_exists path then extract dir (Sys_utils.cat path) else false
