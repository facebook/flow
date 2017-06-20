(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = (Loc.t, Type.t) Hashtbl.t

let create () = Hashtbl.create 0
let set t loc value = Hashtbl.replace t loc value
let iter f t = Hashtbl.iter f t
let fold f t init = Hashtbl.fold f t init
let find_unsafe t k = Hashtbl.find t k
let reset t = Hashtbl.reset t
let copy t = Hashtbl.copy t
