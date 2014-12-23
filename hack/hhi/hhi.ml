(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

external get_embedded_hhi_data : string -> string option =
  "get_embedded_hhi_data"

(* OCaml handles the value restriction much better than SML. <3 *)
let root = ref None

let touch_root r =
  let r = Filename.quote (Path.string_of_path r) in
  ignore (Unix.system ("find " ^ r ^ " -name *.hhi -exec touch '{}' ';'"))

let touch () =
  match !root with
  | Some (Some r) -> touch_root r
  | _ -> ()

(* There are several verify-use race conditions here (and in Hack's file
 * handling in general, really). Running the server as root is likely to be a
 * security risk. Be careful. *)
let extract data =
  let tmpdir = Tmp.temp_dir "hhi" in
  let path = Path.mk_path tmpdir in
  let oc = Unix.open_process_out ("tar xzC " ^ (Path.string_of_path path)) in
  output_string oc data;
  flush oc;
  ignore (Unix.close_process_out oc);
  touch_root path;
  path

let extract_embedded () =
  Utils.opt_map extract (get_embedded_hhi_data Sys.executable_name)

(* Look for the hhi.tar.gz in the place where it normally resides, so that we
 * support debugging binaries that don't have the section embedded, such as
 * bytecode builds. *)
let extract_external () =
  let path = (Filename.dirname Sys.executable_name) ^ "/../hhi.tar.gz" in
  if Sys.file_exists path then Some (extract (Utils.cat path)) else None

let get_hhi_root_impl () =
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
      root := Some r;
      (* TODO(jezng) refactor this ugliness *)
      Relative_path.set_path_prefix
        Relative_path.Hhi
        (Path.string_of_path (unsafe_opt r));
      r

let set_hhi_root_for_unit_test dir =
  root := Some (Some dir);
  Relative_path.set_path_prefix Relative_path.Hhi (Path.string_of_path dir)
