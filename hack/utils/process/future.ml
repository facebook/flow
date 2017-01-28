(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** We shell out hg commands, so its computation is out-of-process, and
 * return a Future for the result. *)

type 'a promise =
  | Complete : 'a -> 'a promise
  | Incomplete of Process_types.t * (string -> 'a)
type 'a t = 'a promise ref

let make process transformer =
  ref (Incomplete (process, transformer))

let get : 'a t -> 'a = fun promise -> match !promise with
  | Complete v -> v
  | Incomplete (process, transformer) ->
    let status, result, err = Process.read_and_wait_pid process in
    match status with
    | Unix.WEXITED 0 ->
      let result = transformer result in
      let () = promise := Complete result in
      result
    | _ ->
      raise (Future_sig.Process_failure (status, err))

let is_ready promise = match !promise with
  | Complete _ -> true
  | Incomplete (process, _) ->
    Process.is_ready process
