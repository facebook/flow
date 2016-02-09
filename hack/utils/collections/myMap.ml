(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module type S = sig
  type +'a t
  type key

  val empty: 'a t
  val singleton: key -> 'a -> 'a t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val exists: (key -> 'a -> bool) -> 'a t -> bool
  val for_all: (key -> 'a -> bool) -> 'a t -> bool
  val mem: key -> 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val get: key -> 'a t -> 'a option
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val remove: key -> 'a t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  val find_unsafe: key -> 'a t -> 'a
  val is_empty: 'a t -> bool
  val union: 'a t -> 'a t -> 'a t
  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val compare: 'a t -> 'a t -> int
  val equal: 'a t -> 'a t -> bool
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option)
    -> 'a t -> 'b t -> 'c t
  val choose : 'a t -> key * 'a
  val split: key -> 'a t -> 'a t * 'a option * 'a t
  val keys: 'a t -> key list
  val values: 'a t -> 'a list
  val min_binding : 'a t -> key * 'a

  val map_env: ('c -> 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t
  val elements: 'a t -> (key * 'a) list
end

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

end
