(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val start :
  FlowServerMonitorOptions.t ->
  unit

val daemonize:
  wait:bool ->
  on_spawn:(int -> unit) ->
  FlowServerMonitorOptions.t ->
  unit
