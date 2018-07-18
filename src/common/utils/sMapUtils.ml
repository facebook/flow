(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Performs a map, but returns the original SMap.t if there is no change **)
let ident_map f map =
  let changed = ref false in
  let map' = SMap.map (fun elem ->
    let elem' = f elem in
    if elem == elem'
    then elem
    else begin
      changed := true;
      elem'
    end
  ) map in
  if !changed then map' else map
