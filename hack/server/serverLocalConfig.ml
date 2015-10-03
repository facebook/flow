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
open Utils

type t = {
  use_watchman: bool;
  load_mini_script: Path.t option;
}

let default = {
  use_watchman = false;
  load_mini_script = None;
}

let path =
  let dir = try Sys.getenv "HH_LOCALCONF_PATH" with _ -> "/etc" in
  Filename.concat dir "hh.conf"

let load_ fn =
  let config = Config_file.parse fn in
  let use_watchman = bool_ "use_watchman" ~default:false config in
  let load_mini_script =
    Option.map (SMap.get "load_mini_script" config) Path.make in
  {
    use_watchman;
    load_mini_script;
  }

let load () =
  try load_ path
  with _ ->
    Hh_logger.log "Could not load config at %s, using defaults" path;
    default
