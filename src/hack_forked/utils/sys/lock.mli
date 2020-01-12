(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val grab : string -> bool

val release : string -> bool

val blocking_grab_then_release : string -> unit

val fd_of : string -> int

val check : string -> bool
