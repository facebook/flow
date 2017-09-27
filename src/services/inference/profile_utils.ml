(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Time logging utility. Computes the elapsed time when running some code, and
   if the elapsed time satisfies a given predicate (typically, is more than a
   threshold), prints a message. *)

let checktime, logtime =
  let time ~pred ~msg ~log ~f =
    let start = Unix.gettimeofday () in
    let ret = f () in
    let elap = (Unix.gettimeofday ()) -. start in
    (match log with None -> () | Some log -> log elap);
    if not (pred elap) then () else prerr_endline (msg elap);
    ret
  in

  let wraptime ~options ~pred ~msg ~log ~f =
    if Options.should_profile options then time ~pred ~msg ~log ~f
    else f()
  in

  let checktime ~options ~limit ~msg ~log ~f =
    wraptime ~options ~pred:(fun t -> t > limit) ~msg ~log:(Some log) ~f
  in

  let logtime ~options ~msg ~f =
    wraptime ~options ~pred:(fun _ -> true) ~msg ~log:None ~f
  in

  checktime, logtime
