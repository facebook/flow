(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
*)

type process_data =
  {
    (** Process ID. *)
    pid : int;
    finale_file : string;
    start_t : float;
    (** Get occasional updates about status/busyness from typechecker here. *)
    in_fd: Unix.file_descr;
    (** Send client's File Descriptors to the typechecker over this. *)
    out_fds : (string * Unix.file_descr) list;
    last_request_handoff : float ref;
  }

type server_process =
  | Not_yet_started
  | Alive of process_data
  | Informant_killed
  (** When the server crashes, we want to track that it has crashed and report
   * that crash info to the next hh_client that connects. We keep that info
   * here. *)
  | Died_unexpectedly of Unix.process_status * bool
  (**
   * The problem we need to solve is this: when the Monitor wants to start
   * a new Server instance, it might not be safe to do so because we might
   * end up running the a version of the Server not meant for this state of
   * the repo (as specified in the .hhconfig file).
   *
   * Monitor might want to start a Server because the last one died (crashed,
   * or exited due to hhconfig change), or the Informant has decided to start
   * a new instance on a better saved state. Because the .hhconfig file has
   * indicated a version change is occuring and the logic to parse this version
   * change and locate the binaries for that version are not inside server
   * code, we need to force that logic to be exercised on the user side, and
   * only start a new server after we've confirmed that it has been exercised
   * (which is confirmed by a new client connecting and succeeding the build_id
   * handshake; alternatively, failing that handshake and then this Monitor
   * correctly exits).
   *
   * So, whenever we want to start a new Server, the Monitor must check the
   * hhconfig file's version number. If it doesn't match the version at the
   * time of this Monitor's startup, we enter this state Died_config_changed.
   *
   * Only a new client connection after entering this state can transition
   * use away from this state.
   *
   * NB: Monitor could be mid-way handling a new client connection when it
   * processes the next Informant decision. In which case, we can't guarantee
   * that the client that connected did in fact exercise the version lookup
   * logic; so the only safe thing to do is enter the Died_config_changed
   * state until the *next* client that connects.
   *
   * These transitions are centralized in ServerMonitor.kill_and_maybe_restart_server
   * Don't do them elsewhere or you will screw it up.
   *
   * State transition looks sort of like:
   *
   *
   *                [ Died_config_changed ]
   *                       ^       \
   *                      /         \ new client connection
   *             maybe  /            \ connection triggers
   *     restart but  /                \ maybe_restart which actually
   *  config not mat/ching              \ does start one this time
   *              /                      \
   *            /                         \
   * [ Any server state ]-------------> [ new server instance ]
   *                        restart
   *                         and
   *                        config
   *                       matches
   *
   *
   * Why don't we just exit the Monitor automatically, instead of keeping it
   * around with a server in this Died_config_changed state? We want Nuclide
   * to know that things are dandy and it should retry connecting without
   * the user having to click anything.
   *)
  | Died_config_changed
