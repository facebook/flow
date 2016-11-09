(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module type S = MyMap_sig.S
module Make(Ord: Map.OrderedType) : S with type key = Ord.t = struct
  include Map.Make(Ord)
  let get x t =
    try Some (find x t) with Not_found -> None

  let find_unsafe = find

  let union x y =
    fold add x y

  let compare x y = compare Pervasives.compare x y
  let equal x y = compare x y = 0

  let keys m = fold (fun k _ acc -> k :: acc) m []
  let values m = fold (fun _ v acc -> v :: acc) m []
  let elements m = fold (fun k v acc -> (k,v)::acc) m []

  let map_env f env m =
    fold (
      fun x y (env, acc) ->
        let env, y = f env y in
        env, add x y acc
    ) m (env, empty)

  let choose x =
    try Some (choose x) with Not_found -> None

  let from_keys keys f =
    List.fold_left begin fun acc key ->
      add key (f key) acc
    end empty keys

end
