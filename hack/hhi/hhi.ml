(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* OCaml handles the value restriction much better than SML. <3 *)
let root = ref None

(* Compiler embeds the hhi contents directly into the source *)
let hhi_contents = [%hhi_contents]

let get_raw_hhi_contents () = hhi_contents

let write_hhi dir (filename, contents) =
  let file = Path.(concat dir filename |> to_string) in
  (* Make sure the subdirectory exists; this structure is nested *)
  Sys_utils.mkdir_p (Filename.dirname file);
  Sys_utils.write_file ~file contents

let extract_hhis () =
  let tmpdir = Path.make (Tmp.temp_dir GlobalConfig.tmp_dir "hhi") in
  Array.iter (write_hhi tmpdir) hhi_contents;
  tmpdir

(* Touch functionality for all hhis below root *)
let touch_root r =
  let filter file = Filename.check_suffix file ".hhi" in
  Find.iter_files ~filter [ r ] (Sys_utils.try_touch ~follow_symlinks:true)

let touch () =
  match !root with
  | Some r -> touch_root r
  | _ -> ()

(* Entry points to actually extract the files and set up the hhi path.
 *
 * We want this to be idempotent so that later code can check if a given file
 * came from the hhi unarchive directory or not, to provide better error
 * messages. *)
let get_hhi_root () =
  match !root with
  | Some r -> r
  | None -> begin
      let r = extract_hhis () in
      root := Some r;
      Relative_path.set_path_prefix Relative_path.Hhi r;
      r
  end

let set_hhi_root_for_unit_test dir =
  (* no need to call realpath() on this; we never extract the hhi files for our
   * unit tests, so this is just a dummy value and does not need to be a real
   * path*)
  root := Some dir;
  Relative_path.set_path_prefix Relative_path.Hhi dir
