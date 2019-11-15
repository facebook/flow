(*
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type 'a t

val create : unit -> 'a t

val add : 'a t -> 'a -> unit

val segments : 'a t -> 'a list
