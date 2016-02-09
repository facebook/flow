(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type 'a t = ('a, unit) Hashtbl.t

let create size = Hashtbl.create size
let clear set = Hashtbl.clear set
let copy set = Hashtbl.copy set
let add set x = Hashtbl.replace set x ()
let mem set x = Hashtbl.mem set x
let remove set x = Hashtbl.remove set x
let iter f set = Hashtbl.iter (fun k _ -> f k) set
let fold f set acc = Hashtbl.fold (fun k _ acc -> f k acc) set acc
let length set = Hashtbl.length set
