(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

 type t = {
   id : int option;
 }

let empty = {
  id = None;
}

let of_id ~id = {
  id = Some id;
}

let is_empty ds =
  match ds.id with
  | Some _ -> false
  | None -> true

let get_id ds =
  match ds.id with
  | Some x -> x
  | None -> 0

let unsubscribe ds id =
  match ds.id with
  | Some x when x = id -> empty
  | _ -> ds
