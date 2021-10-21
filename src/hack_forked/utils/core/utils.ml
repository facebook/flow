(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Callstack is simply a typed way to indicate that a string is a callstack *)
type callstack = Callstack of string

let () = Random.self_init ()

let spf = Printf.sprintf

let singleton_if cond x =
  if cond then
    [x]
  else
    []

(* Since OCaml usually runs w/o backtraces enabled, the note makes errors
 * easier to debug. *)
let unsafe_opt_note note = function
  | None -> raise (Invalid_argument note)
  | Some x -> x

let unsafe_opt x = unsafe_opt_note "unsafe_opt got None" x

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
