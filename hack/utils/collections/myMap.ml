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

  let union ?combine x y =
    let combine = match combine with
      | None -> (fun _ fst _ -> Some fst)
      | Some f -> f
    in
    union combine x y

  let compare x y = compare Pervasives.compare x y
  let equal x y = compare x y = 0

  let keys m = fold (fun k _ acc -> k :: acc) m []
  let ordered_keys m = List.map fst (bindings m)

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

  let from_keys keys ~f =
    List.fold_left begin fun acc key ->
      add key (f key) acc
    end empty keys

  let add ?combine key new_value map =
    match combine with
    | None -> add key new_value map
    | Some combine -> begin
      match get key map with
      | None -> add key new_value map
      | Some old_value -> add key (combine old_value new_value) map
    end

  let ident_map f map =
    let map_, changed = fold (fun key item (map_, changed) ->
      let item_ = f item in
      add key item_ map_, changed || item_ != item
    ) map (empty, false) in
    if changed then map_ else map

  let ident_map_key ?combine f map =
    let map_, changed = fold (fun key item (map_, changed) ->
      let new_key = f key in
      add ?combine new_key item map_, changed || new_key != key
    ) map (empty, false) in
    if changed then map_ else map
end
