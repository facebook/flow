(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* These are all the little bits of information which the Flow server monitor needs in order to
 * function *)

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
  shared_mem_config: SharedMem_js.config;
  (* The argv of the process which created the server monitor *)
  argv: string array;
  (* What to use for file watching *)
  file_watcher: Options.file_watcher;
  (* Turn on debugging messages for the file watcher *)
  file_watcher_debug: bool;
}
