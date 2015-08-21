(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Configuration file *)
(*****************************************************************************)

external nproc: unit -> int = "nproc"
let nbr_procs = nproc ()

let freq_cache_capacity = 1000
let ordered_cache_capacity = 1000

(* Configures only the workers. Workers can have more relaxed GC configs as
 * they are short-lived processes *)
let gc_control = Gc.get ()

(* Where to write temp files *)
let tmp_dir =
  Path.to_string @@
  Path.concat Path.temp_dir_name "hh_server"
