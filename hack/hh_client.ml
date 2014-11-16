(**
 * Copyright (c) 2014, Facebook, Inc.
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
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let command = ClientArgs.parse_args () in
  let log_cmd = ClientLogCommandUtils.log_command_of_command command in
  EventLogger.client_startup log_cmd;
  begin match command with
    | ClientCommand.CCheck check_env ->
        ClientCheck.main check_env check_env.ClientEnv.retries;
    | ClientCommand.CStart env ->  ClientStart.main env
    | ClientCommand.CStop env -> HackClientStop.main env
    | ClientCommand.CRestart env -> ClientRestart.main env
    | ClientCommand.CStatus env -> ClientStatus.main env
    | ClientCommand.CBuild env -> ClientBuild.main env
    | ClientCommand.CProlog env -> ClientProlog.main env
  end;
  EventLogger.client_finish log_cmd;
  ()
