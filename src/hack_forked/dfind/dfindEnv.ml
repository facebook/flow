(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The environment shared by everyone *)
(*****************************************************************************)

type t = {
  (* The fsnotify environment, we use this for interacting with fsnotify  *)
  fsnotify: Fsnotify.env;
  (* The set of new files (files created during an event) *)
  mutable new_files: SSet.t;
  (* The directories (and the files they contain) *)
  mutable dirs: SSet.t SMap.t;
}

(*****************************************************************************)
(* Building the original environment, this call is called only once
 * by the server (cf dfindServer.ml)
 *)
(*****************************************************************************)

let make roots =
  let fsnotify = Fsnotify.init roots in
  { fsnotify; new_files = SSet.empty; dirs = SMap.empty }
