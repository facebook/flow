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
  load_script      : string option;
  load_script_timeout : int; (* in seconds *)
  (* Configures only the workers. Workers can have more relaxed GC configs as
   * they are short-lived processes *)
  gc_control       : Gc.control;
  tc_options       : TypecheckerOptions.t;
}

let make_gc_control config =
  let minor_heap_size = match SMap.get "gc_minor_heap_size" config with
    | Some s -> int_of_string s
    | None -> GlobalConfig.gc_control.Gc.minor_heap_size in
  let space_overhead = match SMap.get "gc_space_overhead" config with
    | Some s -> int_of_string s
    | None -> GlobalConfig.gc_control.Gc.space_overhead in
  { GlobalConfig.gc_control with Gc.minor_heap_size; Gc.space_overhead; }

let config_assume_php config =
  match SMap.get "assume_php" config with
    | Some s -> bool_of_string s
    | None -> true

let config_unsafe_xhp config =
  match SMap.get "unsafe_xhp" config with
    | Some s -> bool_of_string s
    | None -> false

let config_list_regexp = (Str.regexp "[, \t]+")

let config_user_attributes config =
  match SMap.get "user_attributes" config with
    | None -> None
    | Some s ->
      let custom_attrs = Str.split config_list_regexp s in
      Some (List.fold_right SSet.add custom_attrs SSet.empty)

let load config_filename =
  let config = Config_file.parse (Relative_path.to_absolute config_filename) in
  let load_script =
    match SMap.get "load_script" config with
    | None -> None
    | Some cmd ->
        let root = Relative_path.path_of_prefix Relative_path.Root in
        let cmd =
          if Filename.is_relative cmd then root^"/"^cmd else cmd in
        Some cmd in
  (* Since we use the unix alarm() for our timeouts, a timeout value of 0 means
   * to wait indefinitely *)
  let load_script_timeout =
    Option.value_map (SMap.get "load_script_timeout" config)
    ~default:0 ~f:int_of_string in
  let tcopts = {
    TypecheckerOptions.tco_assume_php = config_assume_php config;
    tco_unsafe_xhp = config_unsafe_xhp config;
    tco_user_attrs = config_user_attributes config;
  } in
  {
    load_script   = load_script;
    load_script_timeout = load_script_timeout;
    gc_control    = make_gc_control config;
    tc_options    = tcopts;
  }

(* useful in testing code *)
let default_config = {
  load_script = None;
  load_script_timeout = 0;
  gc_control = GlobalConfig.gc_control;
  tc_options = TypecheckerOptions.empty;
}

let load_script config = config.load_script
let load_script_timeout config = config.load_script_timeout
let gc_control config = config.gc_control
let typechecker_options config = config.tc_options
