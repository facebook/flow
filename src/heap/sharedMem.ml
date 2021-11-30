(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Don't change the ordering of this record without updating hh_shared_init in
 * hh_shared.c, which indexes into config objects *)
type config = {
  heap_size: int;
  hash_table_pow: int;
  log_level: int;
}

type handle = Unix.file_descr

type table_stats = {
  nonempty_slots: int;
  used_slots: int;
  slots: int;
}

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* Phantom type parameter provides type-safety to callers of this API.
 * Internally, these are all just ints, so be careful! *)
type _ addr = int

type effort =
  [ `aggressive
  | `always_TEST
  ]

type serialized_tag = Serialized_resolved_requires

let heap_ref : buf option ref = ref None

exception Out_of_shared_memory

exception Hash_table_full

exception Heap_full

exception Failed_memfd_init of Unix.error

let () =
  Callback.register_exception "out_of_shared_memory" Out_of_shared_memory;
  Callback.register_exception "hash_table_full" Hash_table_full;
  Callback.register_exception "heap_full" Heap_full;
  Callback.register_exception "failed_memfd_init" (Failed_memfd_init Unix.EINVAL)

(*****************************************************************************)
(* Initializes the shared memory. Must be called before forking. *)
(*****************************************************************************)
external init : config -> num_workers:int -> buf * handle = "hh_shared_init"

let init config ~num_workers =
  try
    let (heap, handle) = init config ~num_workers in
    heap_ref := Some heap;
    Ok handle
  with
  | Failed_memfd_init _ -> Error ()

external connect : handle -> worker_id:int -> buf = "hh_connect"

let connect handle ~worker_id =
  let heap = connect handle ~worker_id in
  heap_ref := Some heap

(*****************************************************************************)
(* The current state of the incremental GC. *)
(*****************************************************************************)
type gc_phase =
  | Phase_idle
  | Phase_mark
  | Phase_sweep

external gc_phase : unit -> gc_phase = "hh_gc_phase"

(*****************************************************************************)
(* The size of the dynamically allocated shared memory section *)
(*****************************************************************************)
external heap_size : unit -> int = "hh_used_heap_size"

(*****************************************************************************)
(* The size of any new allocations since the previous full collection cycle *)
(*****************************************************************************)
external new_alloc_size : unit -> int = "hh_new_alloc_size"

(*****************************************************************************)
(* The size of all free space in shared memory *)
(*****************************************************************************)
external free_size : unit -> int = "hh_free_heap_size"

(*****************************************************************************)
(* Force a new GC cycle to start. Precondition: gc_phase must be Phase_idle *)
(*****************************************************************************)
external start_cycle : unit -> unit = "hh_start_cycle"

(*****************************************************************************)
(* Perform a fixed amount of marking work. The return value is the unused work
 * budget. If marking completed in the given budget, the returned value will be
 * greater than 0. Precondition: gc_phase must be Phase_mark. *)
(*****************************************************************************)
external mark_slice : int -> int = "hh_mark_slice"

(*****************************************************************************)
(* Perform a fixed amount of sweeping work. The return value is the unused work
 * budget. If weeping completed in the given budget, the returned value will be
 * greater than 0. Precondition: gc_phase must be Phase_sweep. *)
(*****************************************************************************)
external sweep_slice : int -> int = "hh_sweep_slice"

(*****************************************************************************)
(* Compact the heap, sliding objects "to the left" over any free objects
 * discovered during the previous full mark and sweep. Precondition: gc_phase
 * must be Phase_idle. *)
(*****************************************************************************)
external hh_compact : unit -> unit = "hh_compact"

(*****************************************************************************)
(* The logging level for shared memory statistics *)
(* 0 = nothing *)
(* 1 = log totals, averages, min, max bytes marshalled and unmarshalled *)
(*****************************************************************************)
external hh_log_level : unit -> int = "hh_log_level"

(*****************************************************************************)
(* The total number of slots in our hashtable *)
(*****************************************************************************)
external hash_stats : unit -> table_stats = "hh_hash_stats"

let on_compact = ref (fun _ _ -> ())

let compact_helper () =
  let k = !on_compact () in
  hh_compact ();
  k ()

(* GC will attempt to keep the overhead of garbage to no more than 20%. Before
 * we actually mark and sweep, however, we don't know how much garbage there is,
 * so we estimate.
 *
 * To estimate the amount of garbage, we consider all "new" allocations --
 * allocations since the previous mark+sweep -- to be garbage. We add that
 * number to the known free space. If that is at least 20% of the total space,
 * we will kick of a new mark and sweep pass. *)
let should_collect () =
  let estimated_garbage = free_size () + new_alloc_size () in
  estimated_garbage * 5 >= heap_size ()

(* After a full mark and sweep, we want to compact the heap if the amount of
 * free space is 20% of the scanned heap. *)
let should_compact () =
  let scanned_size = heap_size () - new_alloc_size () in
  free_size () * 5 >= scanned_size

(* Perform an incremental "slice" of GC work. The caller can control the amount
 * of work performed by passing in a smaller or larger "work" budget. This
 * function returns `true` when the GC phase was completed, and `false` if there
 * is still more work to do. *)
let collect_slice ?(force = false) work =
  let work = ref work in

  while !work > 0 do
    match gc_phase () with
    | Phase_idle ->
      if force || should_collect () then
        start_cycle ()
      else
        work := 0
    | Phase_mark -> work := mark_slice !work
    | Phase_sweep ->
      ignore (sweep_slice !work);
      work := 0
  done;

  let is_idle = gc_phase () = Phase_idle in

  (* The GC will be in idle phase under two conditions: (1) we started in idle
   * and did not start a new collect cycle, or (2) we just finished a sweep. In
   * condition (1) should_compact should return false, so we will only possibly
   * compact in condition (2), assuming 20% of the scanned heap is free. *)
  if is_idle && should_compact () then compact_helper ();

  is_idle

(* Perform a full GC pass, or complete an in-progress GC pass. This call
 * bypasses the `should_collect` heuristic and will instead always trigger a new
 * mark and sweep pass if the GC is currently idle. *)
let collect_full () =
  while not (collect_slice ~force:true max_int) do
    ()
  done

let finish_cycle () =
  while gc_phase () == Phase_mark do
    ignore (mark_slice max_int)
  done;
  while gc_phase () == Phase_sweep do
    ignore (sweep_slice max_int)
  done

(* Perform a full compaction of shared memory, such that no heap space is
 * wasted. We finish the current cycle, if one is in progress, then perform a
 * full mark and sweep pass before collecting. This ensures that any "floating
 * garbage" from a previous GC pass is also collected. *)
let compact () =
  finish_cycle ();
  start_cycle ();
  finish_cycle ();
  compact_helper ()

(* Compute size of values in the garbage-collected heap *)
let value_size r =
  let w = Obj.reachable_words r in
  w * (Sys.word_size / 8)

let debug_value_size = value_size

module type Key = sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int
end

module type Value = sig
  type t

  val description : string
end

module type SerializedTag = sig
  val value : serialized_tag
end

module type AddrValue = sig
  type t
end

(* The shared memory segment contains a single shared hash table. This functor
 * creates modules that give the illusion of multiple hash tables by adding a
 * prefix to each key. *)
module HashtblSegment (Key : Key) = struct
  type hash = string

  external hh_add : hash -> _ addr -> unit = "hh_add"

  external hh_mem : hash -> bool = "hh_mem"

  external hh_get : hash -> _ addr = "hh_get"

  external hh_remove : hash -> unit = "hh_remove"

  external hh_move : hash -> hash -> unit = "hh_move"

  let hh_add x y = WorkerCancel.with_worker_exit (fun () -> hh_add x y)

  let hh_mem x = WorkerCancel.with_worker_exit (fun () -> hh_mem x)

  let hh_get x = WorkerCancel.with_worker_exit (fun () -> hh_get x)

  (* The hash table supports a kind of versioning, where a key can be "oldified"
   * temporarily while a new value for the same key is written. The oldified key
   * can then be revived or removed. We ensure that the new and old entries
   * occupy distinct hash table slots by giving them distinct prefixes. *)

  let new_prefix = Prefix.make ()

  let old_prefix = Prefix.make ()

  let new_hash_of_key k = Digest.string (Prefix.make_key new_prefix (Key.to_string k))

  let old_hash_of_key k = Digest.string (Prefix.make_key old_prefix (Key.to_string k))

  let add k addr = hh_add (new_hash_of_key k) addr

  let mem k = hh_mem (new_hash_of_key k)

  let mem_old k = hh_mem (old_hash_of_key k)

  let get_hash hash =
    if hh_mem hash then
      Some (hh_get hash)
    else
      None

  let get k = get_hash (new_hash_of_key k)

  let get_old k = get_hash (old_hash_of_key k)

  let remove k =
    let new_hash = new_hash_of_key k in
    if hh_mem new_hash then hh_remove new_hash

  (* We oldify entries that might be changed by an operation, which involves
   * moving the address of the current heap value from the "new" key to the
   * "old" key. The operation might then write an updated value to the "new"
   * key.
   *
   * This function is strange, though. Why, if a new entry does not exist, do we
   * try to remove an extant old entry? Why should an old entry even exist at
   * this point? I was not able to find a clear justification for this behavior
   * from source control history... *)
  let oldify k =
    let new_hash = new_hash_of_key k in
    let old_hash = old_hash_of_key k in
    if hh_mem new_hash then
      hh_move new_hash old_hash
    else if hh_mem old_hash then
      hh_remove old_hash

  (* After an operation which first oldified some values, we might decide that
   * the original values are still good enough. In this case we can move the old
   * key back into the new slot.
   *
   * hh_move expects the destination slot to be empty, but the operation might
   * have written a new value before deciding to roll back, so we first check
   * and remove any new key first. *)
  let revive k =
    let new_hash = new_hash_of_key k in
    let old_hash = old_hash_of_key k in
    if hh_mem new_hash then hh_remove new_hash;
    if hh_mem old_hash then hh_move old_hash new_hash

  (* After an operation which first oldified some values, if we decide to commit
   * those changes, we can remove the old key. Generally the hash table entry
   * keeps the corresponding heap object alive, so removing this reference will
   * allow the GC to clean up the old value in the heap. *)
  let remove_old k =
    let old_hash = old_hash_of_key k in
    if hh_mem old_hash then hh_remove old_hash
end

(*****************************************************************************)
(* All the caches are functors returning a module of the following signature *)
(*****************************************************************************)

module type DebugCacheType = sig
  val get_size : unit -> int
end

module type CacheType = sig
  include DebugCacheType

  type key

  type value

  val add : key -> value -> unit

  val get : key -> value option

  val remove : key -> unit

  val clear : unit -> unit
end

(*****************************************************************************)
(* The signatures of what we are actually going to expose to the user *)
(*****************************************************************************)

module type NoCache = sig
  type key

  type value

  module KeySet : Flow_set.S with type elt = key

  val add : key -> value -> unit

  val get : key -> value option

  val get_old : key -> value option

  val remove_old_batch : KeySet.t -> unit

  val remove_batch : KeySet.t -> unit

  val mem : key -> bool

  val mem_old : key -> bool

  val oldify : key -> unit

  val oldify_batch : KeySet.t -> unit

  val revive_batch : KeySet.t -> unit
end

module type NoCacheTag = sig
  include NoCache

  val iter : (value -> unit) -> unit
end

module type LocalCache = sig
  module DebugL1 : DebugCacheType

  module DebugL2 : DebugCacheType

  type key

  type value

  val add : key -> value -> unit

  val get : key -> value option

  val remove : key -> unit

  val clear : unit -> unit
end

module type WithCache = sig
  include NoCache

  val write_around : key -> value -> unit

  val get_no_cache : key -> value option

  module DebugCache : LocalCache with type key = key and type value = value
end

(*****************************************************************************)
(* A functor returning an implementation of the S module without caching. *)
(*****************************************************************************)

module NoCacheInternal
    (Key : Key)
    (Value : Value) (Tag : sig
      val tag : int
    end) :
  NoCache with type key = Key.t and type value = Value.t and module KeySet = Flow_set.Make(Key) =
struct
  module Tbl = HashtblSegment (Key)
  module KeySet = Flow_set.Make (Key)

  type key = Key.t

  type value = Value.t

  (* Returns address into the heap, alloc size, and orig size *)
  external hh_store : Value.t -> int -> _ addr * int * int = "hh_store_ocaml"

  external hh_deserialize : _ addr -> Value.t = "hh_deserialize"

  external hh_get_size : _ addr -> int = "hh_get_size"

  let hh_store x = WorkerCancel.with_worker_exit (fun () -> hh_store x Tag.tag)

  let hh_deserialize x = WorkerCancel.with_worker_exit (fun () -> hh_deserialize x)

  let log_serialize compressed original =
    let compressed = float compressed in
    let original = float original in
    let saved = original -. compressed in
    let ratio = compressed /. original in
    Measure.sample (Value.description ^ " (bytes serialized into shared heap)") compressed;
    Measure.sample "ALL bytes serialized into shared heap" compressed;
    Measure.sample (Value.description ^ " (bytes saved in shared heap due to compression)") saved;
    Measure.sample "ALL bytes saved in shared heap due to compression" saved;
    Measure.sample (Value.description ^ " (shared heap compression ratio)") ratio;
    Measure.sample "ALL bytes shared heap compression ratio" ratio

  let log_deserialize l r =
    let sharedheap = float l in
    Measure.sample (Value.description ^ " (bytes deserialized from shared heap)") sharedheap;
    Measure.sample "ALL bytes deserialized from shared heap" sharedheap;

    if hh_log_level () > 1 then (
      (* value_size is a bit expensive to call this often, so only run with log levels >= 2 *)
      let localheap = float (value_size r) in
      Measure.sample (Value.description ^ " (bytes allocated for deserialized value)") localheap;
      Measure.sample "ALL bytes allocated for deserialized value" localheap
    )

  let add key value =
    let (addr, compressed_size, original_size) = hh_store value in
    Tbl.add key addr;
    if hh_log_level () > 0 && compressed_size > 0 then log_serialize compressed_size original_size

  let mem = Tbl.mem

  let mem_old = Tbl.mem_old

  let deserialize addr =
    let value = hh_deserialize addr in
    if hh_log_level () > 0 then log_deserialize (hh_get_size addr) (Obj.repr value);
    value

  let get key =
    match Tbl.get key with
    | None -> None
    | Some addr -> Some (deserialize addr)

  let get_old key =
    match Tbl.get_old key with
    | None -> None
    | Some addr -> Some (deserialize addr)

  let remove_batch keys = KeySet.iter Tbl.remove keys

  let oldify = Tbl.oldify

  let oldify_batch keys = KeySet.iter oldify keys

  let revive_batch keys = KeySet.iter Tbl.revive keys

  let remove_old_batch keys = KeySet.iter Tbl.remove_old keys
end

module NoCache (Key : Key) (Value : Value) =
  NoCacheInternal (Key) (Value)
    (struct
      let tag = 0
    end)

module NoCacheTag (Key : Key) (Value : Value) (Tag : SerializedTag) = struct
  let tag =
    (* Tag values here must match the corresponding values from NewAPI.tag *)
    match Tag.value with
    | Serialized_resolved_requires -> 1

  include
    NoCacheInternal (Key) (Value)
      (struct
        let tag = tag
      end)

  external hh_iter_serialized : ('a -> unit) -> int -> unit = "hh_iter_serialized"

  let iter f = hh_iter_serialized f tag
end

module NoCacheAddr (Key : Key) (Value : AddrValue) = struct
  module Tbl = HashtblSegment (Key)
  module KeySet = Flow_set.Make (Key)

  type key = Key.t

  type value = Value.t addr

  let add = Tbl.add

  let mem = Tbl.mem

  let mem_old = Tbl.mem_old

  let get = Tbl.get

  let get_old = Tbl.get_old

  let remove_batch keys = KeySet.iter Tbl.remove keys

  let oldify = Tbl.oldify

  let oldify_batch keys = KeySet.iter oldify keys

  let revive_batch keys = KeySet.iter Tbl.revive keys

  let remove_old_batch keys = KeySet.iter Tbl.remove_old keys
end

(*****************************************************************************)
(* All the cache are configured by a module of type CacheConfig *)
(*****************************************************************************)

module type CacheConfig = sig
  type key

  type value

  (* The capacity of the cache *)
  val capacity : int
end

(*****************************************************************************)
(* Cache keeping the objects the most frequently used. *)
(*****************************************************************************)

module FreqCache (Config : CacheConfig) :
  CacheType with type key := Config.key and type value := Config.value = struct
  (* The cache itself *)
  let (cache : (Config.key, int ref * Config.value) Hashtbl.t) = Hashtbl.create (2 * Config.capacity)

  let size = ref 0

  let get_size () = !size

  let clear () =
    Hashtbl.clear cache;
    size := 0

  (* The collection function is called when we reach twice original
   * capacity in size. When the collection is triggered, we only keep
   * the most frequently used objects.
   * So before collection: size = 2 * capacity
   * After collection: size = capacity (with the most frequently used objects)
   *)
  let collect () =
    if !size < 2 * Config.capacity then
      ()
    else
      let l = ref [] in
      Hashtbl.iter
        begin
          fun key (freq, v) ->
          l := (key, !freq, v) :: !l
        end
        cache;
      Hashtbl.clear cache;
      l := Base.List.sort ~compare:(fun (_, x, _) (_, y, _) -> y - x) !l;
      let i = ref 0 in
      while !i < Config.capacity do
        match !l with
        | [] -> i := Config.capacity
        | (k, _freq, v) :: rl ->
          Hashtbl.replace cache k (ref 0, v);
          l := rl;
          incr i
      done;
      size := Config.capacity;
      ()

  let add x y =
    collect ();
    try
      let (freq, y') = Hashtbl.find cache x in
      incr freq;
      if y' == y then
        ()
      else
        Hashtbl.replace cache x (freq, y)
    with
    | Not_found ->
      incr size;
      let elt = (ref 0, y) in
      Hashtbl.replace cache x elt;
      ()

  let find x =
    let (freq, value) = Hashtbl.find cache x in
    incr freq;
    value

  let get x =
    try Some (find x) with
    | Not_found -> None

  let remove x =
    if Hashtbl.mem cache x then decr size;
    Hashtbl.remove cache x
end

(*****************************************************************************)
(* An ordered cache keeps the most recently used objects *)
(*****************************************************************************)

module OrderedCache (Config : CacheConfig) :
  CacheType with type key := Config.key and type value := Config.value = struct
  let (cache : (Config.key, Config.value) Hashtbl.t) = Hashtbl.create Config.capacity

  let queue = Queue.create ()

  let size = ref 0

  let get_size () = !size

  let clear () =
    Hashtbl.clear cache;
    size := 0;
    Queue.clear queue;
    ()

  let add x y =
    ( if !size >= Config.capacity then
      (* Remove oldest element - if it's still around. *)
      let elt = Queue.pop queue in
      if Hashtbl.mem cache elt then (
        decr size;
        Hashtbl.remove cache elt
      )
    );

    (* Add the new element, but bump the size only if it's a new addition. *)
    Queue.push x queue;
    if not (Hashtbl.mem cache x) then incr size;
    Hashtbl.replace cache x y

  let find x = Hashtbl.find cache x

  let get x =
    try Some (find x) with
    | Not_found -> None

  let remove x =
    try
      if Hashtbl.mem cache x then decr size;
      Hashtbl.remove cache x
    with
    | Not_found -> ()
end

(*****************************************************************************)
(* Every time we create a new cache, a function that knows how to clear the
 * cache is registered in the "invalidate_callback_list" global.
 *)
(*****************************************************************************)

let invalidate_callback_list = ref []

module LocalCache (Config : CacheConfig) = struct
  type key = Config.key

  type value = Config.value

  (* Young values cache *)
  module L1 = OrderedCache (Config)

  (* Frequent values cache *)
  module L2 = FreqCache (Config)

  (* These are exposed only for tests *)
  module DebugL1 = L1
  module DebugL2 = L2

  let add x y =
    L1.add x y;
    L2.add x y

  let get x =
    match L1.get x with
    | None ->
      (match L2.get x with
      | None -> None
      | Some v as result ->
        L1.add x v;
        result)
    | Some v as result ->
      L2.add x v;
      result

  let remove x =
    L1.remove x;
    L2.remove x

  let clear () =
    L1.clear ();
    L2.clear ()
end

(*****************************************************************************)
(* A functor returning an implementation of the S module with caching.
 * We need to avoid constantly deserializing types, because it costs us too
 * much time. The caches keep a deserialized version of the types.
 *)
(*****************************************************************************)
module WithCache (Key : Key) (Value : Value) :
  WithCache with type key = Key.t and type value = Value.t and module KeySet = Flow_set.Make(Key) =
struct
  module Direct = NoCache (Key) (Value)

  type key = Direct.key

  type value = Direct.value

  module KeySet = Direct.KeySet

  module Cache = LocalCache (struct
    type nonrec key = key

    type nonrec value = value

    let capacity = 1000
  end)

  (* This is exposed for tests *)
  module DebugCache = Cache

  let add x y =
    Direct.add x y;
    Cache.add x y

  let get_no_cache = Direct.get

  let write_around x y =
    (* Note that we do not need to do any cache invalidation here because
     * Direct.add is a no-op if the key already exists. *)
    Direct.add x y

  let log_hit_rate ~hit =
    Measure.sample
      (Value.description ^ " (cache hit rate)")
      ( if hit then
        1.
      else
        0.
      );
    Measure.sample
      "(ALL cache hit rate)"
      ( if hit then
        1.
      else
        0.
      )

  let get x =
    match Cache.get x with
    | None ->
      let result =
        match Direct.get x with
        | None -> None
        | Some v as result ->
          Cache.add x v;
          result
      in
      if hh_log_level () > 0 then log_hit_rate ~hit:false;
      result
    | Some _ as result ->
      if hh_log_level () > 0 then log_hit_rate ~hit:true;
      result

  (* We don't cache old objects, they are not accessed often enough. *)
  let get_old = Direct.get_old

  let mem_old = Direct.mem_old

  let mem x =
    match get x with
    | None -> false
    | Some _ -> true

  let oldify key =
    Direct.oldify key;
    Cache.remove key

  let oldify_batch keys =
    Direct.oldify_batch keys;
    KeySet.iter Cache.remove keys

  let revive_batch keys =
    Direct.revive_batch keys;
    KeySet.iter Cache.remove keys

  let remove_batch xs =
    Direct.remove_batch xs;
    KeySet.iter Cache.remove xs

  let () =
    invalidate_callback_list :=
      begin
        fun () ->
        Cache.clear ()
      end
      :: !invalidate_callback_list

  let remove_old_batch = Direct.remove_old_batch
end

module NewAPI = struct
  type chunk = {
    heap: buf;
    mutable next_addr: int;
    mutable remaining_size: int;
  }

  type heap_string

  type 'a addr_tbl

  type 'a opt

  type docblock

  type aloc_table

  type type_sig

  type checked_file

  type size = int

  let bsize_wsize bsize = bsize * Sys.word_size / 8

  let addr_offset addr size = addr + bsize_wsize size

  let get_heap () =
    match !heap_ref with
    | None -> failwith "get_heap: not connected"
    | Some heap -> heap

  external alloc : size -> _ addr = "hh_ml_alloc"

  let alloc size f =
    let addr = alloc size in
    let chunk = { heap = get_heap (); next_addr = addr; remaining_size = size } in
    let x = f chunk in
    (* Ensure allocated space was initialized. *)
    assert (chunk.remaining_size = 0);
    assert (chunk.next_addr = addr_offset addr size);
    x

  (* Addresses are relative to the hashtbl pointer, so the null address actually
   * points to the hash field of the first hashtbl entry, which is never a
   * meaningful address, so we can use it to represent "missing" or similar.
   *
   * Naturally, we need to be careful not to dereference the null addr! Any
   * internal use of null should be hidden from callers of this module. *)
  let null_addr = 0

  (* header utils *)

  (* The integer values corresponding to these tags are encoded in the low byte
   * of object headers. Any changes made here must be kept in sync with
   * hh_shared.c -- e.g., the should_scan function. *)
  type tag =
    | Serialized_tag
    | Serialized_resolved_requires_tag
    (* tags defined above this point are serialized+compressed *)
    | String_tag (* 2 *)
    | Docblock_tag
    | ALoc_table_tag
    | Type_sig_tag
    (* tags defined below this point are scanned for pointers *)
    | Addr_tbl_tag (* 6 *)
    | Checked_file_tag

  (* avoid unused constructor warning *)
  let () = ignore Serialized_tag

  let () = ignore Serialized_resolved_requires_tag

  (* constant constructors are integers *)
  let tag_val : tag -> int = Obj.magic

  (* double-check integer value is consistent with hh_shared.c *)
  let () = assert (tag_val Addr_tbl_tag = 6)

  let mk_header tag size =
    (* lower byte of header is reserved for 6-bit tag and 2 GC bits, OCaml
     * representation of the header does not include the GC bits, which will be
     * set when converting to intnat before writing to shmem, see
     * unsafe_write_header. *)
    tag_val tag lor (size lsl 6)

  let obj_tag hd = hd land 0x3F

  let obj_size hd = hd lsr 6

  (* sizes *)

  let header_size = 1

  let addr_size = 1

  (* Obj used as an efficient way to get at the word size of an OCaml string
   * directly from the block header, since that's the size we need. *)
  let string_size s = Obj.size (Obj.repr s)

  let addr_tbl_size xs = addr_size * Array.length xs

  let docblock_size = string_size

  let aloc_table_size = string_size

  (* Because the heap is word aligned, we store the size of the type sig object
   * in words. The underlying data might not be word sized, so we use a trick
   * lifted from OCaml's representation of strings. The last byte of the block
   * stores a value which we can use to recover the byte size from the word
   * size. If the value is exactly word sized, we add another word to hold this
   * final byte. *)
  let type_sig_size bsize = (bsize + 8) lsr 3

  let checked_file_size = 3 * addr_size

  (* headers *)

  let string_header s =
    let size = string_size s in
    mk_header String_tag size

  let addr_tbl_header xs =
    let size = addr_tbl_size xs in
    mk_header Addr_tbl_tag size

  let docblock_header docblock =
    let size = docblock_size docblock in
    mk_header Docblock_tag size

  let aloc_table_header tbl =
    let size = aloc_table_size tbl in
    mk_header ALoc_table_tag size

  let checked_file_header = mk_header Checked_file_tag checked_file_size

  (* offsets *)

  let docblock_addr file = addr_offset file 1

  let aloc_table_addr file = addr_offset file 2

  let type_sig_addr file = addr_offset file 3

  (* read *)

  let assert_tag hd tag = assert (obj_tag hd = tag_val tag)

  (* Read a string from the heap at the specified address with the specified
   * size (in words). This read is not bounds checked or type checked; caller
   * must ensure that the given destination contains string data. *)
  external unsafe_read_string : _ addr -> int -> string = "hh_read_string"

  external unsafe_read_int8 : buf -> int -> int = "%caml_ba_unsafe_ref_1"

  (* Read int64 from given byte offset in the heap. This is bounds checked. *)
  external read_int64 : buf -> int -> int64 = "%caml_bigstring_get64"

  (* Read a header from the heap. The low 2 bits of the header are reserved for
   * GC and not used in OCaml. *)
  let read_header heap addr =
    let hd64 = read_int64 heap addr in
    (* Double-check that the data looks like a header. All reachable headers
     * will have the lsb set. *)
    assert (Int64.(logand hd64 1L = 1L));
    Int64.(to_int (shift_right_logical hd64 2))

  let read_header_checked heap tag addr =
    let hd = read_header heap addr in
    assert_tag hd tag;
    hd

  (* Read an address from the heap. *)
  let read_addr heap addr =
    let addr64 = read_int64 heap addr in
    (* double-check that the data looks like an address *)
    assert (Int64.logand addr64 1L = 0L);
    Int64.to_int addr64

  let read_string_generic tag addr offset =
    let hd = read_header_checked (get_heap ()) tag addr in
    let str_addr = addr_offset addr (header_size + offset) in
    let str_size = obj_size hd - offset in
    unsafe_read_string str_addr str_size

  let read_string addr = read_string_generic String_tag addr 0

  let read_addr_tbl_generic f addr init =
    if addr = null_addr then
      init 0 (fun _ -> failwith "empty")
    else
      let heap = get_heap () in
      let hd = read_header_checked heap Addr_tbl_tag addr in
      init (obj_size hd) (fun i -> f (read_addr heap (addr_offset addr (header_size + i))))

  let read_addr_tbl f addr = read_addr_tbl_generic f addr Array.init

  let read_opt f addr =
    if addr = null_addr then
      None
    else
      Some (f addr)

  let read_docblock addr = read_string_generic Docblock_tag addr 0

  let read_aloc_table addr = read_string_generic ALoc_table_tag addr 0

  let type_sig_buf addr =
    let heap = get_heap () in
    let hd = read_header_checked heap Type_sig_tag addr in
    let offset_index = bsize_wsize (obj_size hd) - 1 in
    let bsize =
      offset_index - unsafe_read_int8 heap (addr_offset addr header_size + offset_index)
    in
    Bigarray.Array1.sub heap (addr_offset addr header_size) bsize

  let read_type_sig addr f = f (type_sig_buf addr)

  (* getters *)

  let file_docblock file = read_addr (get_heap ()) (docblock_addr file)

  let file_aloc_table file = read_addr (get_heap ()) (aloc_table_addr file)

  let file_type_sig file = read_addr (get_heap ()) (type_sig_addr file)

  (* write *)

  external unsafe_write_int8 : buf -> int -> int -> unit = "%caml_ba_unsafe_set_1"

  (* Write int64 to given byte offset in the heap. This is not bounds checked. *)
  external unsafe_write_int64 : buf -> int -> int64 -> unit = "%caml_bigstring_set64u"

  (* Write a header at a specified address in the heap. This write is not
   * bounds checked; caller must ensure the given destination has already been
   * allocated. *)
  let unsafe_write_header_at heap dst hd =
    unsafe_write_int64 heap dst Int64.(logor 1L (shift_left (of_int hd) 2))

  (* Write an address at a specified address in the heap. This write is not
   * bounds checked; caller must ensure the given destination has already been
   * allocated. *)
  let unsafe_write_addr_at heap dst addr = unsafe_write_int64 heap dst (Int64.of_int addr)

  (* Write a string at the specified address in the heap. This write is not
   * bounds checked; caller must ensure the given destination has already been
   * allocated. *)
  external unsafe_write_string_at : _ addr -> string -> unit = "hh_write_string" [@@noalloc]

  (* Write a header into the given chunk and advance the chunk address. This
   * write is not bounds checked; caller must ensure the given destination has
   * already been allocated. *)
  let unsafe_write_header chunk hd =
    unsafe_write_header_at chunk.heap chunk.next_addr hd;
    chunk.next_addr <- addr_offset chunk.next_addr header_size

  (* Write an address into the given chunk and advance the chunk address. This
   * write is not bounds checked; caller must ensure the given destination has
   * already been allocated. *)
  let unsafe_write_addr chunk addr =
    unsafe_write_addr_at chunk.heap chunk.next_addr addr;
    chunk.next_addr <- addr_offset chunk.next_addr addr_size

  (* Write a string into the given chunk and advance the chunk address. This
   * write is not bounds checked; caller must ensure the given destination has
   * already been allocated. *)
  let unsafe_write_string chunk s =
    unsafe_write_string_at chunk.next_addr s;
    chunk.next_addr <- addr_offset chunk.next_addr (string_size s)

  (* Consume space in the chunk for the object described by the given header,
   * write header, advance chunk address, and return address to the header. This
   * function should be called before writing any heap object. *)
  let write_header chunk hd =
    let size = header_size + obj_size hd in
    chunk.remaining_size <- chunk.remaining_size - size;
    assert (chunk.remaining_size >= 0);
    let addr = chunk.next_addr in
    unsafe_write_header chunk hd;
    addr

  let write_string chunk s =
    let heap_string = write_header chunk (string_header s) in
    unsafe_write_string chunk s;
    heap_string

  let write_docblock chunk docblock =
    let addr = write_header chunk (docblock_header docblock) in
    unsafe_write_string chunk docblock;
    addr

  let write_aloc_table chunk tbl =
    let addr = write_header chunk (aloc_table_header tbl) in
    unsafe_write_string chunk tbl;
    addr

  let write_type_sig chunk bsize f =
    let size = type_sig_size bsize in
    let hd = mk_header Type_sig_tag size in
    let addr = write_header chunk hd in
    let offset_index = bsize_wsize size - 1 in
    unsafe_write_int8 chunk.heap (addr_offset addr header_size + offset_index) (offset_index - bsize);
    let buf = Bigarray.Array1.sub chunk.heap (addr_offset addr header_size) bsize in
    f buf;
    chunk.next_addr <- addr_offset chunk.next_addr size;
    addr

  let write_checked_file chunk docblock aloc_table type_sig =
    let checked_file = write_header chunk checked_file_header in
    unsafe_write_addr chunk docblock;
    unsafe_write_addr chunk aloc_table;
    unsafe_write_addr chunk type_sig;
    checked_file

  let write_addr_tbl f chunk xs =
    if Array.length xs = 0 then
      null_addr
    else
      let hd = addr_tbl_header xs in
      let map = write_header chunk hd in
      chunk.next_addr <- addr_offset chunk.next_addr (obj_size hd);
      Array.iteri
        (fun i x ->
          let addr = f chunk x in
          unsafe_write_addr_at chunk.heap (addr_offset map (header_size + i)) addr)
        xs;
      map

  let write_opt f chunk = function
    | None -> null_addr
    | Some x -> f chunk x
end
