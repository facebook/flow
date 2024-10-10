(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open SharedMem

module type S = sig
  type key

  type value

  module KeySet : Flow_set.S with type elt = key

  (* Will allocate an entity associated with the key if not present, and store
   * the initial serialized value. If the key is already tracked, the existing
   * value will be advanced to the next version. In particular, two writes to
   * the same key may exhibit either of the two following behaviors:
   *
   * 1. The first write completes before the second write starts, and the second
   *    write's value will advance the entity allocated from the first write.
   * 2. The two writes allocate individual entities at the same key, and each
   *    write allocates the value's address at its own heap chunk. One
   *    of the writers will lose the race, and its data will be garbage collected.
   *
   * Note that due to `entity_advance` not being thread safe, it's a data race to
   * call `add` from two threads.
   *)
  val add : key -> value -> unit

  val get_committed : key -> value option

  val get_latest : key -> value option

  val remove : key -> unit

  val remove_batch : KeySet.t -> unit

  val mem : key -> bool
end

module Make (Key : Key) (Value : Value) :
  S with type key = Key.t and type value = Value.t and module KeySet = Flow_set.Make(Key) = struct
  module Tbl = HashtblSegment (Key)
  module KeySet = Flow_set.Make (Key)

  type key = Key.t

  type value = Value.t

  (** Returns address into the heap *)
  external hh_store : Value.t -> int -> _ addr = "hh_store_ocaml"

  external hh_deserialize : _ addr -> Value.t = "hh_deserialize"

  let add key value =
    let addr = hh_store value serialized_tag_val in
    let (size, write_entity) = NewAPI.prepare_write_entity in
    let prep = (size, (fun chunk -> write_entity chunk (Some addr))) in
    let entity_addr = NewAPI.alloc prep |> Tbl.add key in
    NewAPI.entity_advance entity_addr (Some addr)

  let mem = Tbl.mem

  let get_committed key =
    match Tbl.get key with
    | None -> None
    | Some addr -> NewAPI.entity_read_committed addr |> Option.map hh_deserialize

  let get_latest key =
    match Tbl.get key with
    | None -> None
    | Some addr -> NewAPI.entity_read_latest addr |> Option.map hh_deserialize

  let remove = Tbl.remove

  let remove_batch keys = KeySet.iter remove keys
end
