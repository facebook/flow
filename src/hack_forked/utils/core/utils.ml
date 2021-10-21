(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Callstack is simply a typed way to indicate that a string is a callstack *)
type callstack = Callstack of string

let () = Random.self_init ()

let try_finally ~f ~(finally : unit -> unit) =
  let res =
    try f () with
    | e ->
      let e = Exception.wrap e in
      finally ();
      Exception.reraise e
  in
  finally ();
  res
