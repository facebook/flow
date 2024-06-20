open Core_hash_queue_intf

module type Key = Key

module type S_backend = S_backend

module Table = Core_hashtbl

module type Backend =
  S1 with type 'key create_arg := 'key Core_hashtbl.Hashable.t with type 'key create_key := 'key

module Backend : Backend = struct
  module Key_value = struct
    module T = struct
      type ('key, 'value) t = {
        key: 'key;
        value: 'value;
      }
    end

    include T

    let value t = t.value
  end

  open Key_value.T
  module Elt = Core_doubly_linked.Elt

  type ('key, 'data) t = {
    queue: ('key, 'data) Key_value.t Core_doubly_linked.t;
    table: ('key, ('key, 'data) Key_value.t Elt.t) Table.t;
  }

  let create ?(growth_allowed = true) ?(size = 16) hashable =
    {
      queue = Core_doubly_linked.create ();
      table = Table.create ~growth_allowed ~size (Table.Hashable.to_key hashable);
    }

  let clear t =
    Core_doubly_linked.clear t.queue;
    Table.clear t.table

  let enqueue_unchecked t back_or_front key value =
    let contents = { Key_value.key; value } in
    let elt =
      match back_or_front with
      | `back -> Core_doubly_linked.insert_last t.queue contents
      | `front -> Core_doubly_linked.insert_first t.queue contents
    in
    Table.set t.table ~key ~data:elt

  let enqueue t back_or_front key value =
    if Table.mem t.table key then
      `Key_already_present
    else (
      enqueue_unchecked t back_or_front key value;
      `Ok
    )

  let raise_enqueue_duplicate_key _t _key = failwith ""

  let enqueue_exn t back_or_front key value =
    match enqueue t back_or_front key value with
    | `Key_already_present -> raise_enqueue_duplicate_key t key
    | `Ok -> ()

  let enqueue_front_exn t = enqueue_exn t `front

  let lookup_and_move_to_front t key =
    match Table.find t.table key with
    | None -> None
    | Some elt ->
      Core_doubly_linked.move_to_front t.queue elt;
      Some (Key_value.value (Elt.value elt))

  let dequeue_with_key t back_or_front =
    let maybe_kv =
      match back_or_front with
      | `back -> Core_doubly_linked.remove_last t.queue
      | `front -> Core_doubly_linked.remove_first t.queue
    in
    match maybe_kv with
    | None -> None
    | Some kv ->
      Table.remove t.table kv.key;
      Some (kv.key, kv.value)

  let dequeue t back_or_front =
    match dequeue_with_key t back_or_front with
    | None -> None
    | Some (_, v) -> Some v

  let dequeue_back t = dequeue t `back
end

module type S = S0 with type ('key, 'data) hash_queue := ('key, 'data) Backend.t

module Make_with_hashable (T : sig
  module Key : Key

  val hashable : Key.t Core_hashtbl.Hashable.t
end) : S with type key = T.Key.t = struct
  include (Backend : Backend with type ('k, 'd) t := ('k, 'd) Backend.t)

  type key = T.Key.t

  type 'data t = (T.Key.t, 'data) Backend.t

  let hashable = T.hashable

  let create ?growth_allowed ?size () = create ?growth_allowed ?size hashable
end

module Make (Key : Key) : S with type key = Key.t = Make_with_hashable (struct
  module Key = Key

  let hashable = Table.Hashable.of_key (module Key)
end)

include Backend
