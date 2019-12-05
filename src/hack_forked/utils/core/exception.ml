(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  exn: exn;
  backtrace: Printexc.raw_backtrace;
}

(* In ocaml, backtraces (the path that the exception bubbled up after being thrown) are stored as
 * global state and NOT with the exception itself. This means the only safe place to ever read the
 * backtrace is immediately after the exception is caught in the `with` block of a `try...with`.
 *
 * Proper use of this module is something like
 *
 *  try
 *    ...
 *  with exn ->
 *    let e = Exception.wrap exn in (* DO THIS FIRST!!! *)
 *    my_fun e; (* If this code throws internally it will overwrite the global backtrace *)
 *    Exception.reraise e
 *)

let wrap exn =
  let backtrace = Printexc.get_raw_backtrace () in
  { exn; backtrace }

(* The inverse of `wrap`, returns the wrapped `exn`. You might use this to pattern
   match on the raw exception or print it, but should not reraise it since it
   will not include the correct backtrace; use `reraise` or `to_exn` instead. *)
let unwrap { exn; backtrace = _ } = exn

let reraise { exn; backtrace } = Printexc.raise_with_backtrace exn backtrace

(* Converts back to an `exn` with the right backtrace. Generally, avoid this in favor of
   the helpers in this module, like `to_string` and `get_backtrace_string`. *)
let to_exn t = (try reraise t with exn -> exn)

(* Like `wrap`, but for the unusual case where you want to create an `Exception`
   for an un-raised `exn`, capturing its stack trace. If you've caught an exception,
   you should use `wrap` instead, since it already has a stack trace. *)
let wrap_unraised ?(frames = 100) exn =
  let frames =
    if Printexc.backtrace_status () then
      frames
    else
      0
  in
  let backtrace = Printexc.get_callstack frames in
  { exn; backtrace }

let get_ctor_string { exn; backtrace = _ } = Printexc.to_string exn

let get_backtrace_string { exn = _; backtrace } = Printexc.raw_backtrace_to_string backtrace

let to_string t =
  let ctor = get_ctor_string t in
  let bt = get_backtrace_string t in
  if bt = "" then
    ctor
  else
    ctor ^ "\n" ^ bt

let get_current_callstack_string n = Printexc.get_callstack n |> Printexc.raw_backtrace_to_string

let record_backtrace = Printexc.record_backtrace
