(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  exn: exn;
  backtrace: Printexc.raw_backtrace
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

let reraise { exn; backtrace } =
  Printexc.raise_with_backtrace exn backtrace

let to_string { exn; backtrace } =
  Printf.sprintf "%s\n%s" (Printexc.to_string exn) (Printexc.raw_backtrace_to_string backtrace)
