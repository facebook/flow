(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* These are all the little bits of information which the Flow server monitor needs in order to
 * function *)

type watchman_options = {
  debug: bool;  (** Turn on debugging messages for the file watcher *)
  defer_states: string list;  (** Defer watchman notifications while these states are asserted *)
  mergebase_with: string;  (** symbolic commit to find changes against *)
  survive_restarts: bool;  (** try to recover from watchman restarting *)
  sync_timeout: int option;
      (** How long to wait for the file watcher to synchronize, in milliseconds *)
}

type file_watcher =
  | NoFileWatcher
  | DFind
  | Watchman of watchman_options

type t = {
  (* Where the monitor logs will go by default *)
  log_file: string;
  (* If true then the monitor will exit when the last client exits. This is used by lsp. *)
  autostop: bool;
  (* If true then the monitor will always exit when a server exits, and will never try to create
   * a new server. This is currently only used for testing what causes servers to die *)
  no_restart: bool;
  (* Where the server logs will go *)
  server_log_file: string;
  (* The server's options *)
  server_options: Options.t;
  (* The shared memory config *)
  shared_mem_config: SharedMem.config;
  (* The argv of the process which created the server monitor *)
  argv: string array;
  (* What to use for file watching *)
  file_watcher: file_watcher;
  (* How long to wait for the file watcher to initialize, in seconds *)
  file_watcher_timeout: float option;
}

let string_of_file_watcher = function
  | NoFileWatcher -> "Dummy"
  | DFind -> "DFind"
  | Watchman _ -> "Watchman"
