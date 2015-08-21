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
(* Adds a new file or directory to the environment *)
(*****************************************************************************)
open Core
open DfindEnv
open DfindMaybe
open Utils

(*****************************************************************************)
(* helpers *)
(*****************************************************************************)

(* Gives back a set of all the files in the directory.
 * A directory handle is of type Unix.dir_handle, it is the result of
 * a call to Unix.opendir. Not to be confused with dfind handles.
 * The path argument is useful because we want this function to give us
 * "full" paths. If I am in the directory "/tmp/bla" and I iterate
 * over the elements of the directory, the result I want is:
 * /tmp/bla/file1
 * /tmp/bla/file2
 * As opposed to:
 * file1
 * file2
*)

let get_files path dir_handle =
  let paths = ref SSet.empty in
  try
    while true do
      let file = Unix.readdir dir_handle in
      if file = "." || file = ".."
      then ()
      else
        let path =
          Path.to_string @@
          Path.concat (Path.expanduser path) file in
        paths := SSet.add path !paths;
    done;
    assert false
  with _ -> !paths

(* Gets rid of the '/' or '\' at the end of a directory name *)
let normalize path =
  let size = String.length path in
  if Char.escaped path.[size - 1] = Filename.dir_sep
  then String.sub path 0 (size - 1)
  else path

(*****************************************************************************)
(* The entry point
 * 1) We add a watch to the entry point + all the sub elements of the entry
 *    point when it is a directory
 *
 * 2) We add all the files conservatively to the TimeTree. That is, files
 *    are never removed. If you want them to be removed reboot the server.
 *    It is much more complicated to try to keep an accurate view of the state
 *    of the world. I leave that to smarter people than me.
 *
 * 3) All the operations are performed in the maybe monad, so that we never
 *    fail. Any operation could fail, because files could be removed while
 *    we are working on them.
 *
 *)
(*****************************************************************************)

module ISet = Set.Make (struct type t = int let compare = compare end)

(* This used to be an environment variable, but it is too complicated
 * for now. Hardcoding! Yay!
*)
let blacklist = List.map ~f:Str.regexp [
  ".*/wiki/images/.*";
  ".*/\\.git";
  ".*/\\.svn";
  ".*/\\.hg";
]

let is_blacklisted path =
  try
    List.iter blacklist begin fun re ->
      if Str.string_match re path 0
      then raise Exit
      else ()
    end;
    false
  with Exit -> true

let rec add_file links env path =
  let path = normalize path in
  match is_blacklisted path with
  | true -> return ()
  | false when not (SSet.mem path env.new_files) -> add_new_file links env path
  | _ -> return ()

and add_watch links env path =
  call (add_fsnotify_watch env) path >>= function
  | None -> return ()
  | Some watch -> add_file links env path

and add_fsnotify_watch env path =
  return (Fsnotify.add_watch env.fsnotify path)

and add_new_file links env path =
  let time = Time.get() in
  env.files <- TimeFiles.add (time, path) env.files;
  env.new_files <- SSet.add path env.new_files;
  call (wrap Unix.lstat) path >>= fun ({ Unix.st_kind = kind; _ } as st) ->
  if ISet.mem st.Unix.st_ino links then return () else
  let links = ISet.add st.Unix.st_ino links in
  match kind with
  | Unix.S_LNK when ISet.mem st.Unix.st_ino links ->
      return ()
  | Unix.S_LNK ->
      return ()
      (* TODO add an option to support symlinks *)
(*       call (wrap Unix.readlink) path >>= add_file links env *)
  | Unix.S_DIR ->
      call (add_watch links env) path >>= fun () ->
      call (wrap Unix.opendir) path >>= fun dir_handle ->
      let files = get_files path dir_handle in
      SSet.iter (fun x -> ignore (add_file links env x)) files;
      (try Unix.closedir dir_handle with _ -> ());
      let prev_files =
        try SMap.find_unsafe path env.dirs
        with Not_found -> SSet.empty in
      let prev_files = SSet.union files prev_files in
      let files = SSet.fold begin fun file all_files ->
        try
          let sub_dir = SMap.find_unsafe file env.dirs in
          SSet.union sub_dir all_files
        with Not_found ->
          SSet.add file all_files
      end files prev_files in
      env.dirs <- SMap.add path files env.dirs;
      return ()
  | _ -> return ()


(* This is the only thing we want to expose *)
let path env x = ignore (add_file ISet.empty env x)
