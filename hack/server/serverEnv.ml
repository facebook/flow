(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Utils

(*****************************************************************************)
(* The "static" environment, initialized first and then doesn't change *)
(*****************************************************************************)

type genv = {
    options          : ServerArgs.options;
    config           : ServerConfig.t;
    local_config     : ServerLocalConfig.t;
    workers          : Worker.t list option;
    (* Returns the list of files under .hhconfig, subject to a filter *)
    indexer          : (string -> bool) -> string MultiWorker.nextlist;
    (* Each time this is called, it should return the files that have changed
     * since the last invocation *)
    notifier         : unit -> SSet.t;
    (* If daemons are spawned as part of the init process, wait for them here *)
    wait_until_ready : unit -> unit;
  }

(*****************************************************************************)
(* The environment constantly maintained by the server *)
(*****************************************************************************)

(* In addition to this environment, many functions are storing and
 * updating ASTs, NASTs, and types in a shared space
 * (see respectively Parser_heap, Naming_heap, Typing_env).
 * The Ast.id are keys to index this shared space.
 *)
type env = {
    files_info     : FileInfo.t Relative_path.Map.t;
    tcopt          : TypecheckerOptions.t;
    errorl         : Errors.t;
    (* the strings in those sets represent filenames *)
    failed_parsing : Relative_path.Set.t;
    failed_decl    : Relative_path.Set.t;
    failed_check   : Relative_path.Set.t;
  }

let file_filter f =
  (FindUtils.is_php f && not (FilesToIgnore.should_ignore f))
  || FindUtils.is_js f

let list_files env oc =
  let acc = List.fold_right
    ~f:begin fun error acc ->
      let pos = Errors.get_pos error in
      Relative_path.Set.add (Pos.filename pos) acc
    end
    ~init:Relative_path.Set.empty
    env.errorl in
  Relative_path.Set.iter (fun s ->
    Printf.fprintf oc "%s\n" (Relative_path.to_absolute s)) acc;
  flush oc
