(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(* bring in utils shared with Hack *)
include Utils

(* ok-or-error type *)
type ('a,'b) ok_or_err = OK of 'a | Err of 'b

let assert_false s =
  let callstack = Printexc.(get_callstack 10 |> raw_backtrace_to_string) in
  prerr_endline (spf "%s%s\n%s:\n%s%s%s"
    (* this clowny shit is to evade hg's conflict marker detection *)
    "<<<<" "<<<<" s callstack ">>>>" ">>>>"
  );
  failwith s

let __DEBUG__ ?(s="") f =
  try f () with _ -> assert_false s

(* Time logging utility. Computes the elapsed time when running some code, and
   if the elapsed time satisfies a given predicate (typically, is more than a
   threshold), prints a message. *)
let time pred msg f =
  let start = Unix.gettimeofday () in
  let ret = f () in
  let elap = (Unix.gettimeofday ()) -. start in
  if not (pred elap) then () else prerr_endline (msg elap);
  ret
