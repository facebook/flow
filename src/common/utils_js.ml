(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(**
 * There seems to be a bug in OCaml 4.01 that causes `include Utils` in this
 * file to generate an interface for this file that is merged with the interface
 * for Utils -- but is somehow not type compatible with it.
 *
 * The only workaround I could find was to require that downstream users of both
 * utils.ml and utils_js.ml explicitly refer to both :(
 *
 * If at some point we are able to deprecate support for 4.01, we should
 * consider re-instating the following line and using Utils_js as a direct
 * extension of utils.ml.
 *)
(* include Utils *)

open Utils

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

let call_succeeds try_function function_input =
  try
    try_function function_input;
    true
  with
  (* print failwith <msg> command's exception message *)
  | Failure msg -> prerr_endline msg;
                   false
  | _ -> false

(* quick exception format *)

let fmt_exc exc = Printexc.((to_string exc) ^ "\n" ^ (get_backtrace ()))

let fmt_file_exc file exc = file ^ ": " ^ (fmt_exc exc)
