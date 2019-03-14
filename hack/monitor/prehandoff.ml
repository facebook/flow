(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type exit_status = {
  status : Unix.process_status;
  was_oom: bool;
}

type msg =
  (** Last of the prehandoff messages; includes finale_file_name of server. *)
  | Sentinel of string
  (** The monitor keeps a queue of connections that will need to be passed
   * onto the next server instance. This queue has a size limit that has been
   * reached. *)
  | Server_dormant_connections_limit_reached
  (** Monitor is running but has no server - i.e. dormant. Connection has been
   * placed on a queue to be sent to the next started server. *)
  | Server_not_alive_dormant of string
  (** Server process died. Connect another client to start another one. *)
  | Server_died of exit_status
  (** Server died from a config change, and the Monitor didn't automatically
   * start a new one because a version change in the config file. *)
  | Server_died_config_change
