(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = {
  id: int;
  amount: int;
  parent: t option;
}

let make ~id amount parent = {
  id;
  amount;
  parent;
}

let get_indent nesting nesting_set =
  let rec aux n =
    let amount =
      if ISet.mem n.id nesting_set then
        n.amount
      else
        0
    in
    match n.parent with
      | None -> amount
      | Some p -> amount + (aux p)
  in
  aux nesting
