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
  placeholder: unit;
}

let default = {
  use_watchman = false;
  placeholder = ();
}

let path = "/etc/hh.conf"

let load_ fn =
  let config = Config_file.parse fn in
  let use_watchman = bool_ "use_watchman" ~default:false config in
  {
    use_watchman;
    placeholder = ();
  }

let load () =
  try load_ path
  with _ ->
    Hh_logger.log "Could not load config at %s, using defaults" path;
    default
