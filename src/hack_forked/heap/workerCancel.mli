(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val stop_workers : unit -> unit

val resume_workers : unit -> unit

val check_should_exit : unit -> unit

val set_on_worker_cancelled : (unit -> unit) -> unit

val with_no_cancellations : (unit -> 'a) -> 'a

val with_worker_exit : (unit -> 'a) -> 'a
