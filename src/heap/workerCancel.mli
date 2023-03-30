(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Worker_should_cancel

val stop_workers : unit -> unit

val resume_workers : unit -> unit

val check_should_cancel : unit -> unit

val with_no_cancellations : (unit -> 'a) -> 'a
