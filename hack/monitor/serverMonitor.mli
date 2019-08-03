(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
*)

module Make_monitor :
  functor (SC : ServerMonitorUtils.Server_config) ->
  functor (Informant : Informant_sig.S) ->
    sig
      type t

      (** Start a monitor without running the check loop. Useful for testing. *)
      val start_monitor:
        current_version:Config_file.version ->
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
        current_version:Config_file.version ->
        waiting_client:Unix.file_descr option ->
        max_purgatory_clients:int ->
        SC.server_start_options ->
        Informant.init_env ->
        ServerMonitorUtils.monitor_config ->
        'a
    end
