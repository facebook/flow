(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

external get_embedded_hhi_data : string -> string option =
  "get_embedded_hhi_data"

(* OCaml handles the value restriction much better than SML. <3 *)
let root = ref None

let touch_root r =
  let filter file = Filename.check_suffix file ".hhi" in
  Find.iter_files ~filter [ r ] (Sys_utils.try_touch ~follow_symlinks:true)

let touch () =
  match !root with
  | Some r -> touch_root r
  | _ -> ()

(* There are several verify-use race conditions here (and in Hack's file
 * handling in general, really). Running the server as root is likely to be a
 * security risk. Be careful. *)
let extract data =
  let tmpdir = Path.make (Tmp.temp_dir GlobalConfig.tmp_dir "hhi") in
  let oc = Unix.open_process_out ("tar xzC " ^ (Path.to_string tmpdir)) in
  output_string oc data;
  flush oc;
  ignore (Unix.close_process_out oc);
  touch_root tmpdir;
  tmpdir

let extract_embedded () =
  Option.map (get_embedded_hhi_data Sys.executable_name) extract

(* Look for the hhi.tar.gz in the place where it normally resides, so that we
 * support debugging binaries that don't have the section embedded, such as
 * bytecode builds. *)
let extract_external () =
  let path =
    Path.concat (Path.dirname Path.executable_name) "/../hhi.tar.gz" in
  if Path.file_exists path then Some (extract (Path.cat path)) else None

let extract_win32_res () =
  match Hhi_win32res.read_index () with
  | None -> None
  | Some idx ->
    let tmpdir = Path.make (Tmp.temp_dir GlobalConfig.tmp_dir "hhi") in
    Hhi_win32res.dump_files tmpdir idx;
    touch_root tmpdir;
    Some tmpdir

let get_hhi_root_impl () =
  if Sys.win32 then
    extract_win32_res ()
  else
    match extract_embedded () with
    | Some path -> Some path
    | None -> extract_external ()

(* We want this to be idempotent so that later code can check if a given file
 * came from the hhi unarchive directory or not, to provide better error
 * messages. *)
let get_hhi_root () =
  match !root with
  | Some r -> r
  | None ->
      let r = get_hhi_root_impl () in
      match r with
      | None ->
          print_endline "Could not locate hhi files";
          Exit_status.(exit Missing_hhi)
      | Some r ->
          root := Some r;
          Relative_path.set_path_prefix Relative_path.Hhi r;
          r

let set_hhi_root_for_unit_test dir =
  (* no need to call realpath() on this; we never extract the hhi files for our
   * unit tests, so this is just a dummy value and does not need to be a real
   * path*)
  root := Some dir;
  Relative_path.set_path_prefix Relative_path.Hhi dir
