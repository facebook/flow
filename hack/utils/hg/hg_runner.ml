(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Prints out the current HG revision, closest SVN ancestor, and the files
 * changed between that HG revision and the SVN ancestor.
 *
 * This tool is particularly useful to manually test the Hg module.
 *)

module Args = struct

  type t = {
    root : string;
  }

  let usage = Printf.sprintf
    "Usage: %s [REPO DIRECTORY]\n"
    Sys.argv.(0)

  let parse () =
    let root = ref None in
    let () = Arg.parse [] (fun s -> root := (Some s)) usage in
    match !root with
    | None ->
      Printf.eprintf "%s" usage;
      exit 1
    | Some root ->
      { root = root; }

  let root args = args.root

end;;

let () =
  Sys_utils.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    exit 0));
  let args = Args.parse () in
  let current_hg_rev = Hg.current_working_copy_hg_rev @@ Args.root args in
  let current_hg_rev, _ = Future.get current_hg_rev in
  Printf.eprintf "Current HG rev: %s\n" current_hg_rev;
  let svn_ancestor = Hg.get_closest_svn_ancestor
    current_hg_rev @@ Args.root args in
  let svn_ancestor = Future.get svn_ancestor in
  Printf.eprintf "SVN ancestor: %s\n" svn_ancestor;
  let changes = Hg.files_changed_since_svn_rev
    current_hg_rev svn_ancestor @@ Args.root args in
  let changes = Future.get changes in
  Printf.eprintf "Changes: %s\n" changes
