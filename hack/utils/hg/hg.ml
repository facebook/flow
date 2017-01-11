(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** Tools for shelling out to Mercurial. *)

open Core

exception Malformed_result

type hg_rev = string
type svn_rev = string


(** We shell out hg commands, so its computation is out-of-process, and
 * return a Future for the result. *)
module Future = struct
  type 'a promise =
    | Complete : 'a -> 'a promise
    | Incomplete of Process_types.t * (string -> 'a)
  type 'a t = 'a promise ref

  let make process transformer =
    ref (Incomplete (process, transformer))

  let get : 'a t -> 'a = fun promise -> match !promise with
    | Complete v -> v
    | Incomplete (process, transformer) ->
      let result = Process.read_and_close_pid process in
      let result = transformer result in
      promise := Complete result;
      result
end

(** Returns the closest SVN ancestor to the hg revision.
 *
 * hg log -r 'reverse(::<hg_rev>)' -T '{svnrev}\n' -l 150 --cwd <repo> *
 * Note: The output is a newline-separated list of corresponding SVN
 * revision numbers of the ancestors. For each ancestor that does NOT have
 * a corresponding SVN revision, a blankline is printed. So we use "-l 150"
 * to print up to 150 ancestors that might be newlines, filter away the blanks,
 * and return the first result.
 *)
let get_closest_svn_ancestor hg_rev repo =
  let process = Process.exec "hg" [
    "log";
    {|-r|};
    {|reverse(::|} ^ hg_rev ^ {|)|};
    {|-T|};
    {|{svnrev}\n|};
    {|-l|};
    {|150|};
    {|--cwd|};
    repo;
  ]
  in
  Future.make process @@ fun result ->
    let lines = Sys_utils.split_lines result in
    let nonempty str = String.length str > 0 in
    List.filter lines ~f:nonempty |> List.hd_exn

(** Get the hg revision hash of the current working copy in the repo dir.
 *
 * hg id -i --cwd <repo> *)
let current_working_copy_hg_rev repo =
  let process = Process.exec "hg" ["id"; "-i"; "--cwd"; repo; ] in
  Future.make process @@ fun result ->
    let result = String.trim result in
    if String.length result < 1 then
      raise Malformed_result
    else
      if result.[(String.length result) - 1] = '+' then
        (String.sub result 0 ((String.length result) - 1)), true
      else
        result, false

(** Returns the files changed between the hg_rev and the ancestor
 * SVN revision.
 *
 * hg status --rev r<svn_rev> --rev <hg_rev> --cwd <repo> *)
let files_changed_since_svn_rev hg_rev svn_rev repo =
  let process = Process.exec "hg" [
    "status";
    {|--rev|};
    ("r" ^ svn_rev);
    {|--rev|};
    hg_rev;
    {|--cwd|};
    repo;
  ] in
  Future.make process String.trim
