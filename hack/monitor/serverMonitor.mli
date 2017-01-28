(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

module Make_monitor :
  functor (Informant : Informant_sig.S) ->
    sig
      val start_monitoring:
        waiting_client:Unix.file_descr option ->
        Informant.init_env ->
        ServerMonitorUtils.monitor_config ->
        ServerMonitorUtils.monitor_starter -> 'a
    end
