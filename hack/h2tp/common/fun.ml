(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let memoized h k f =
  try Hashtbl.find h k
  with Not_found ->
    let v = f () in
    begin
      Hashtbl.add h k v;
      v
    end

let const x  = fun _ -> x
