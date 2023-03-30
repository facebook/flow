(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type request = Request of (serializer -> unit)

and serializer = { send: 'a. 'a -> unit }

type worker_mode =
  | Prespawned_long_lived
  | Prespawned_should_fork
  | Spawned

type job_status = Job_terminated of Unix.process_status

val worker_main :
  ('a -> worker_mode) -> 'a -> request Daemon.in_channel * 'c Daemon.out_channel -> 'd
