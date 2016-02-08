(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
  | Return
  | Throw
  | Break of string option
  | Continue of string option

exception Exn of t

val swap: t -> bool -> bool
val set: t -> unit
val throw_control_flow_exception: t -> 'a
val check_control_flow_exception: t option -> unit
val catch_control_flow_exception: (unit -> unit) -> t option
val ignore_break_to_label: string option -> (unit -> 'a) -> t option
val ignore_break_or_continue_to_label: string option -> (unit -> 'a) -> t option
val string: t -> string
