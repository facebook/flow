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
}

let make_gc_control config =
  let minor_heap_size = match SMap.get "gc_minor_heap_size" config with
    | Some s -> int_of_string s
    | None -> GlobalConfig.gc_control.Gc.minor_heap_size in
  let space_overhead = match SMap.get "gc_space_overhead" config with
    | Some s -> int_of_string s
    | None -> GlobalConfig.gc_control.Gc.space_overhead in
  { GlobalConfig.gc_control with Gc.minor_heap_size; Gc.space_overhead; }

let load config_filename =
  let config = Config_file.parse (Relative_path.to_absolute config_filename) in
  let load_script =
    Option.map ~f:Path.make (SMap.get "load_script" config) in
  (* Since we use the unix alarm() for our timeouts, a timeout value of 0 means
   * to wait indefinitely *)
  let load_script_timeout =
    Option.value_map (SMap.get "load_script_timeout" config)
    ~default:0 ~f:int_of_string in
  {
    load_script   = load_script;
    load_script_timeout = load_script_timeout;
    gc_control    = make_gc_control config;
  }

(* useful in testing code *)
let default_config = {
  load_script = None;
  load_script_timeout = 0;
  gc_control = GlobalConfig.gc_control;
}

let load_script config = config.load_script
let load_script_timeout config = config.load_script_timeout
let gc_control config = config.gc_control
