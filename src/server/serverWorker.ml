(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type slave_state = Path.t option
let save () = Path.make (Relative_path.(path_of_prefix Root))
let restore saved_root =
  Relative_path.(set_path_prefix Root saved_root)

let builder =
  let save () =
    let master_cx = Flow_js.master_cx () in
    (Files_js.get_lib_files (), Files_js.get_lib_fileset ()),
    Flow_js.builtins master_cx,
    Flow_js.master_cx (),
    save (),
    FlowConfig.get_unsafe () in
  let restore (lf, b, cx, rp, fc) =
    Files_js.restore_lib_files lf;
    Flow_js.restore_builtins cx b;
    Flow_js.restore_master_cx cx;
    restore rp;
    FlowConfig.restore fc in
  Worker.register_entry_point ~save ~restore

let make options heap_handle =
  let gc_control   = GlobalConfig.gc_control in
  let nbr_procs    = GlobalConfig.nbr_procs in
  Worker.make builder nbr_procs gc_control heap_handle
