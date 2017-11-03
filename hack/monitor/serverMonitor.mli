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
  functor (SC : ServerMonitorUtils.Server_config) ->
  functor (Informant : Informant_sig.S) ->
    sig
      type t

      (** Start a monitor without running the check loop. Useful for testing. *)
      val start_monitor:
        waiting_client:Unix.file_descr option ->
        max_purgatory_clients:int ->
        SC.server_start_options ->
        Informant.init_env ->
        ServerMonitorUtils.monitor_config ->
        t

      (** Run the check loop once. Useful for testing. *)
      val check_and_run_loop_once : t -> t

      (** Start the monitor and repeatedly run the check and run loop.
       * Does not return. *)
      val start_monitoring:
        waiting_client:Unix.file_descr option ->
        max_purgatory_clients:int ->
        SC.server_start_options ->
        Informant.init_env ->
        ServerMonitorUtils.monitor_config ->
        'a
    end
