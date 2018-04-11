(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type request = Request of (serializer -> unit)
and serializer = { send: 'a. 'a -> unit }

val win32_worker_main:
  ('a -> 'b) ->
  'a ->
  request Daemon.in_channel * 'c Daemon.out_channel
  -> 'd

val unix_worker_main:
  ('a -> 'b) ->
  'a ->
  request Daemon.in_channel * 'c Daemon.out_channel
  -> 'd
