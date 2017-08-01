(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val get_hhi_root : unit -> Path.t

val set_hhi_root_for_unit_test : Path.t -> unit

val get_raw_hhi_contents : unit -> (string * string) array

val touch : unit -> unit
