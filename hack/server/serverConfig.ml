(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

type t = {
  load_script      : Path.t option;
  load_script_timeout : int; (* in seconds *)
  (* Configures only the workers. Workers can have more relaxed GC configs as
   * they are short-lived processes *)
  gc_control       : Gc.control;
  tc_options       : TypecheckerOptions.t;
}

let int_ key ~default config =
  Option.value_map (SMap.get key config) ~default ~f:int_of_string

let bool_ key ~default config =
  Option.value_map (SMap.get key config) ~default ~f:bool_of_string

let make_gc_control config =
  let {Gc.minor_heap_size; space_overhead; _} = GlobalConfig.gc_control in
  let minor_heap_size =
    int_ "gc_minor_heap_size" ~default:minor_heap_size config in
  let space_overhead =
    int_ "gc_space_overhead" ~default:space_overhead config in
  { GlobalConfig.gc_control with Gc.minor_heap_size; space_overhead; }


let config_list_regexp = (Str.regexp "[, \t]+")

let config_user_attributes config =
  match SMap.get "user_attributes" config with
    | None -> None
    | Some s ->
      let custom_attrs = Str.split config_list_regexp s in
      Some (List.fold_right SSet.add custom_attrs SSet.empty)

let load config_filename =
  let config = Config_file.parse (Relative_path.to_absolute config_filename) in
  let load_script = Option.map ~f:Path.make (SMap.get "load_script" config) in
  (* Since we use the unix alarm() for our timeouts, a timeout value of 0 means
   * to wait indefinitely *)
  let load_script_timeout = int_ "load_script_timeout" ~default:0 config in
  let tcopts = { TypecheckerOptions.
    tco_assume_php = bool_ "assume_php" ~default:true config;
    tco_unsafe_xhp = bool_ "unsafe_xhp" ~default:false config;
    tco_user_attrs = config_user_attributes config;
  } in
  {
    load_script = load_script;
    load_script_timeout = load_script_timeout;
    gc_control = make_gc_control config;
    tc_options = tcopts;
  }

(* useful in testing code *)
let default_config = {
  load_script = None;
  load_script_timeout = 0;
  gc_control = GlobalConfig.gc_control;
  tc_options = TypecheckerOptions.default;
}

let load_script config = config.load_script
let load_script_timeout config = config.load_script_timeout
let gc_control config = config.gc_control
let typechecker_options config = config.tc_options
