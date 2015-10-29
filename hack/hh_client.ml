(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(**
 * Hack for HipHop: type checker's client code.
 *
 * This code gets called in various different ways:
 * - from emacs, where the output is asynchronous
 * - from vim, where vim is blocked until this code exits
 * - from arc diff, our linter
 * - from arc land, our commit hook
 * - from check trunk, our irc bot which checks the state of trunk
 * - manually, from the command line
 *
 * Usage: hh_client [OPTION]... [WWW DIRECTORY] [FILE]...
 *
 * --from-emacs:
 * --from-arc-diff:
 *   => waits for server to initialize
 *   => retries upto 3x on error conditions
 *   => output debugging info
 * --from-vim:
 *   => does not wait for server to initialize
 *   => does not retry on error conditions
 *   => should minimize output to single lines
 * --from-arc-land:
 *   => waits but does not retry too many times?
 *   => minimal output
 *
 *  Use --help or see clientArgs.ml for more options
 *)

let () =
  (* Ignore SIGPIPE since we might get a server hangup and don't care (can
   * detect and handle better than a signal). Ignore SIGUSR1 since we sometimes
   * use that for the server to tell us when it's done initializing, but if we
   * aren't explicitly listening we don't care. *)
  Sys_utils.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys_utils.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    raise Exit_status.(Exit_with Interrupted)));
  let command = ClientArgs.parse_args () in
  let root = ClientArgs.root command in
  HackEventLogger.client_init root;
  let exit_status =
    try
      match command with
        | ClientCommand.CCheck check_env -> ClientCheck.main check_env
        | ClientCommand.CStart env -> ClientStart.main env
        | ClientCommand.CStop env -> ClientStop.main env
        | ClientCommand.CRestart env -> ClientRestart.main env
        | ClientCommand.CBuild env -> ClientBuild.main env
    with Exit_status.Exit_with es ->
      HackEventLogger.client_bad_exit es;
      es
  in
  Exit_status.exit exit_status
