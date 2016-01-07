(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Config_file.Getters
open Utils

type t = {
  load_script      : Path.t option;
  load_script_timeout : int; (* in seconds *)

  load_mini_script : Path.t option;

  (* Configures only the workers. Workers can have more relaxed GC configs as
   * they are short-lived processes *)
  gc_control       : Gc.control;
  sharedmem_config : SharedMem.config;
  tc_options       : TypecheckerOptions.t;
}

let filename = Relative_path.concat Relative_path.Root ".hhconfig"

let is_compatible c1 c2 =
  (* This comparison can eventually be made more complex; we may not always
   * need to restart hh_server, e.g. changing the path to the load script
   * is immaterial*)
  c1 = c2

let make_gc_control config =
  let {Gc.minor_heap_size; space_overhead; _} = GlobalConfig.gc_control in
  let minor_heap_size =
    int_ "gc_minor_heap_size" ~default:minor_heap_size config in
  let space_overhead =
    int_ "gc_space_overhead" ~default:space_overhead config in
  { GlobalConfig.gc_control with Gc.minor_heap_size; space_overhead; }

let make_sharedmem_config config options =
  let {SharedMem.global_size; heap_size} =
    SharedMem.default_config in
  let global_size = int_ "sharedmem_global_size" ~default:global_size config in
  let heap_size = int_ "sharedmem_heap_size" ~default:heap_size config in
  match ServerArgs.ai_mode options with
  | None -> {SharedMem.global_size; heap_size}
  | Some ai_options ->
    let global_size, heap_size =
      Ai.modify_shared_mem_sizes global_size heap_size ai_options in
    {SharedMem.global_size; heap_size}

let config_list_regexp = (Str.regexp "[, \t]+")

let config_user_attributes config =
  match SMap.get "user_attributes" config with
    | None -> None
    | Some s ->
      let custom_attrs = Str.split config_list_regexp s in
      Some (List.fold_right custom_attrs ~f:SSet.add ~init:SSet.empty)

let maybe_relative_path fn =
  (* Note: this is not the same as calling realpath; the cwd is not
   * necessarily the same as hh_server's root!!! *)
  Path.make begin
    if Filename.is_relative fn
    then Relative_path.(to_absolute (concat Root fn))
    else fn
  end

let load config_filename options =
  let config = Config_file.parse (Relative_path.to_absolute config_filename) in
  let load_script =
    Option.map (SMap.get "load_script" config) maybe_relative_path in
  (* Since we use the unix alarm() for our timeouts, a timeout value of 0 means
   * to wait indefinitely *)
  let load_script_timeout = int_ "load_script_timeout" ~default:0 config in
  let load_mini_script =
    Option.map (SMap.get "load_mini_script" config) maybe_relative_path in
  let tcopts = { TypecheckerOptions.
    tco_assume_php = bool_ "assume_php" ~default:true config;
    tco_unsafe_xhp = bool_ "unsafe_xhp" ~default:false config;
    tco_user_attrs = config_user_attributes config;
  } in
  {
    load_script = load_script;
    load_script_timeout = load_script_timeout;
    load_mini_script = load_mini_script;
    gc_control = make_gc_control config;
    sharedmem_config = make_sharedmem_config config options;
    tc_options = tcopts;
  }

(* useful in testing code *)
let default_config = {
  load_script = None;
  load_script_timeout = 0;
  load_mini_script = None;
  gc_control = GlobalConfig.gc_control;
  sharedmem_config = SharedMem.default_config;
  tc_options = TypecheckerOptions.default;
}

let load_script config = config.load_script
let load_script_timeout config = config.load_script_timeout
let load_mini_script config = config.load_mini_script
let gc_control config = config.gc_control
let sharedmem_config config = config.sharedmem_config
let typechecker_options config = config.tc_options
