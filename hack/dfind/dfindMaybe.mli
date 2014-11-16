(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* A modified maybe monad
 * Most of the time, I prefer to use exceptions, I like things to blow up
 * if something went wrong.
 * However, in the case of dfind, exceptions are painful. We don't want things
 * to blow-up, we want to carry-on whatever happens.
 * So this monad never fails, it logs very nasty errors, for example, it will
 * log the fact that a watch couldn't be created, when the file still exists.
*)
(*****************************************************************************)

type 'a t

(* Called at the initialization of the server (cf server.ml) *)
val set_log : out_channel -> unit

val (>>=)   : 'a t -> ('a -> 'b t) -> 'b t
val return  : 'a -> 'a t

(* Calls (f path), never fails, logs the nasty exceptions *)
val call    : (string -> 'a t) -> string -> 'a t
val wrap    : ('a -> 'b) -> ('a -> 'b t)
