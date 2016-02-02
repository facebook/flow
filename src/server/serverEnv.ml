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
(* The "static" environment, initialized first and then doesn't change *)
(*****************************************************************************)

type genv = {
    options          : Options.options;
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
    files: Utils_js.FilenameSet.t;
    libs: Utils.SSet.t; (* a subset of `files` *)
    errorl: Errors_js.error list;
  }

let async_queue : (unit -> unit) list ref = ref []

let async f = async_queue := f :: !async_queue

let invoke_async_queue () =
  let queue = !async_queue in
  (* we reset the queue before rather than after invoking the function as
   * those functions may themselves add more items to the queue *)
  async_queue := [];
  List.iter (fun f -> f ()) queue

(*****************************************************************************)
(* Killing the server  *)
(*****************************************************************************)

let die() =
  exit(0)
