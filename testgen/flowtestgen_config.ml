(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Config_utils = Flowtestgen_utils.Config
open Printf

(* Config module *)
type t = {
  (* the number of programs we generate *)
  num_prog: int;
  (* Whether we log to the console *)
  log_to_console: bool;
  (* Whether we want to type check using Flow *)
  type_check: bool;
  (* Whether we want to generate programs randomly *)
  random: bool;
  (* which engine to use *)
  engine: string;
}

(* Default config value *)
let default_config : t =
  { num_prog = 10; log_to_console = false; type_check = false; random = true; engine = "base" }

(* Default filename for the config file *)
let config_filename = "flowtestgen.json"

let string_of_config (conf : t) : string =
  let plist =
    Config_utils.
      [ ("num_prog", Int conf.num_prog);
        ("type_check", Bool conf.type_check);
        ("random", Bool conf.random);
        ("engine", Str conf.engine);
        ("log_to_console", Bool conf.log_to_console) ]
  in
  Config_utils.string_of_config plist

let load_config () : t =
  if not (Sys.file_exists config_filename) then (
    printf "No config file detected. Creating one with default values...\n";
    let out = open_out config_filename in
    let config_str = string_of_config default_config in
    fprintf out "%s\n" config_str;
    close_out out
  );
  let conf = Config_utils.load_json_config config_filename in
  let dc = default_config in
  {
    num_prog = Config_utils.get_int ~default:dc.num_prog conf "num_prog";
    engine = Config_utils.get_str ~default:dc.engine conf "engine";
    type_check = Config_utils.get_bool ~default:dc.type_check conf "type_check";
    random = Config_utils.get_bool ~default:dc.random conf "random";
    log_to_console = Config_utils.get_bool ~default:dc.log_to_console conf "log_to_console";
  }

let config = load_config ()
