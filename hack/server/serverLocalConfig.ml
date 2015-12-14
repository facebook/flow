(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Config_file.Getters

type t = {
  use_watchman: bool;
  watchman_init_timeout: int; (* in seconds *)
  use_mini_state: bool;
  load_mini_script_timeout: int; (* in seconds *)
  type_decl_bucket_size: int;
  io_priority: int;
  cpu_priority: int;
}

let default = {
  use_watchman = false;
  watchman_init_timeout = 10;
  use_mini_state = false;
  load_mini_script_timeout = 20;
  type_decl_bucket_size = 1000;
  io_priority = 7;
  cpu_priority = 10;
}

let path =
  let dir = try Sys.getenv "HH_LOCALCONF_PATH" with _ -> "/etc" in
  Filename.concat dir "hh.conf"

let load_ fn =
  (* Print out the contents in our logs so we know what settings this server
   * was started with *)
  let contents = Sys_utils.cat fn in
  Printf.eprintf "%s:\n%s\n" fn contents;
  let config = Config_file.parse_contents contents in
  let use_watchman = bool_ "use_watchman" ~default:false config in
  let use_mini_state = bool_ "use_mini_state" ~default:false config in
  let load_mini_script_timeout =
    int_ "load_mini_script_timeout" ~default:20 config in
  let type_decl_bucket_size =
    int_ "type_decl_bucket_size" ~default:1000 config in
  (* Buck and hgwatchman use a 10 second timeout too *)
  let watchman_init_timeout =
    int_ "watchman_init_timeout" ~default:10 config in
  let io_priority = int_ "io_priority" ~default:7 config in
  let cpu_priority = int_ "cpu_priority" ~default:10 config in
  {
    use_watchman;
    watchman_init_timeout;
    use_mini_state;
    load_mini_script_timeout;
    type_decl_bucket_size;
    io_priority;
    cpu_priority;
  }

let load () =
  try load_ path
  with _ ->
    Hh_logger.log "Could not load config at %s, using defaults" path;
    default
