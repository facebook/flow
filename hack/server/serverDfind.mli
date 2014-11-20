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

val dfind_pid: int option ref

val dfind_init: 
  Path.path -> unit

(* new set of php and js files *)  
val get_updates: Path.path -> SSet.t

