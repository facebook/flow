(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Time logging utility. Computes the elapsed time when running some code, and
   if the elapsed time satisfies a given predicate (typically, is more than a
   threshold), prints a message. *)
let time pred msg f =
  let start = Unix.gettimeofday () in
  let ret = f () in
  let elap = (Unix.gettimeofday ()) -. start in
  if not (pred elap) then () else prerr_endline (msg elap);
  ret

let wraptime ~options pred msg f =
  if Options.should_profile options then time pred msg f
  else f()

let checktime ~options limit msg f =
  wraptime ~options (fun t -> t > limit) msg f

let logtime ~options msg f =
  wraptime ~options (fun _ -> true) msg f
