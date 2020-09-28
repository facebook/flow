(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type request = Request of (serializer -> unit)

and serializer = { send: 'a. 'a -> unit }

type job_status = Job_terminated of Unix.process_status

val win32_worker_main :
  ('a -> unit) ->
  'a * Unix.file_descr option ->
  request Daemon.in_channel * 'c Daemon.out_channel ->
  'd

val unix_worker_main :
  ('a -> unit) ->
  'a * Unix.file_descr option ->
  request Daemon.in_channel * 'c Daemon.out_channel ->
  'd
