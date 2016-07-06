(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t

val create: unit -> t
val start_timer: timer:string -> t -> t
val stop_timer: timer:string -> t -> t
val get_finished_timer: timer:string -> t -> (float * float) option
val to_json: t -> Hh_json.json
val to_string: t -> string
