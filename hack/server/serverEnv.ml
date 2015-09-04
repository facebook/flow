(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

(*****************************************************************************)
(* The "static" environment, initialized first and then doesn't change *)
(*****************************************************************************)

type genv = {
    options          : ServerArgs.options;
    config           : ServerConfig.t;
    workers          : Worker.t list option;
    dfind            : DfindLib.t option;
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
    nenv           : Naming.env;
    errorl         : Errors.t;
    (* the strings in those sets represent filenames *)
    failed_parsing : Relative_path.Set.t;
    failed_decl    : Relative_path.Set.t;
    failed_check   : Relative_path.Set.t;
  }

let typechecker_options env = (Naming.typechecker_options env.nenv)

(*****************************************************************************)
(* Listing all the files present in the environment *)
(*****************************************************************************)

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
