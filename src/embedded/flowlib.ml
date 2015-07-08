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

(* OCaml handles the value restriction much better than SML. <3 *)
let root = ref None

let touch_root r =
  let r = Path.to_string r in
  ignore (Unix.system ("find \"" ^ r ^ "\" -name \"*.js\" -exec touch '{}' ';'"))

(* There are several verify-use race conditions here (and in Hack's file
 * handling in general, really). Running the server as root is likely to be a
 * security risk. Be careful. *)
let extract data =
  let tmpdir = Tmp.temp_dir "flowlib" in
  let path = Path.make tmpdir in
  let oc = Unix.open_process_out ("tar xzC " ^ (Path.to_string path)) in
  output_string oc data;
  flush oc;
  ignore (Unix.close_process_out oc);
  touch_root path;
  Path.concat path "lib"

let extract_embedded () =
  Sys_utils.executable_path ()
  |> get_embedded_flowlib_data
  |> Utils.opt_map extract

(* Look for the flowlib.tar.gz in the place where it normally resides, so that we
 * support debugging binaries that don't have the section embedded, such as
 * bytecode builds. *)
let extract_external () =
  let path =
    (Filename.dirname (Sys_utils.executable_path ())) ^ "/flowlib.tar.gz" in
  if Sys.file_exists path then Some (extract (Sys_utils.cat path)) else None

let get_flowlib_root_impl () =
  match extract_embedded () with
  | Some path -> Some path
  | None -> extract_external ()

(* We want this to be idempotent so that later code can check if a given file
 * came from the flowlib unarchive directory or not, to provide better error
 * messages. *)
let get_flowlib_root () =
  match !root with
  | Some r -> r
  | None ->
      let r = get_flowlib_root_impl () in
      root := Some r;
      r
