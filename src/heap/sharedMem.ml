(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_core

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

type heap = (nativeint, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t

let heap_ref : heap option ref = ref None

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
external init : config -> num_workers:int -> heap * handle = "hh_shared_init"

let init config ~num_workers =
  try
    let (heap, handle) = init config ~num_workers in
    heap_ref := Some heap;
    handle
  with Failed_memfd_init _ ->
    if EventLogger.should_log () then EventLogger.sharedmem_failed_memfd_init ();
    Hh_logger.log "Failed to use anonymous memfd init";
    (* TODO: The server should exit, but we should exit with a different
     * message, since Out_of_shared_memory is also raised when memfd_reserve
     * fails. This error condition is specifically when the memfd could not be
     * initialized. *)
    raise Out_of_shared_memory

external connect : handle -> worker_id:int -> heap = "hh_connect"

let connect handle ~worker_id =
  let heap = connect handle ~worker_id in
  heap_ref := Some heap

(*****************************************************************************)
(* The shared memory garbage collector. It must be called every time we
 * free data (cf hh_shared.c for the underlying C implementation).
 *)
(*****************************************************************************)
external hh_collect : unit -> unit = "hh_collect"

(*****************************************************************************)
(* The size of the dynamically allocated shared memory section *)
(*****************************************************************************)
external heap_size : unit -> int = "hh_used_heap_size"

(*****************************************************************************)
(* Part of the heap not reachable from hashtable entries. *)
(*****************************************************************************)
external wasted_heap_size : unit -> int = "hh_wasted_heap_size"

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

(*****************************************************************************)
(* Must be called after the initialization of the hack server is over.
 * (cf serverInit.ml). *)
(*****************************************************************************)
let init_done () = EventLogger.sharedmem_init_done (heap_size ())

let should_collect (effort : [ `gentle | `aggressive | `always_TEST ]) =
  let overhead =
    match effort with
    | `always_TEST -> 1.0
    | `aggressive -> 1.2
    | `gentle -> 2.0
  in
  let used = heap_size () in
  let wasted = wasted_heap_size () in
  let reachable = used - wasted in
  used >= truncate (float reachable *. overhead)

let collect (effort : [ `gentle | `aggressive | `always_TEST ]) =
  let old_size = heap_size () in
  Stats.update_max_heap_size old_size;
  let start_t = Unix.gettimeofday () in
  (* The wrapper is used to run the function in a worker instead of master. *)
  if should_collect effort then hh_collect ();
  let new_size = heap_size () in
  let time_taken = Unix.gettimeofday () -. start_t in
  if old_size <> new_size then (
    Hh_logger.log
      "Sharedmem GC: %d bytes before; %d bytes after; in %f seconds"
      old_size
      new_size
      time_taken;
    EventLogger.sharedmem_gc_ran effort old_size new_size time_taken
  )

(*****************************************************************************)
(* Compute size of values in the garbage-collected heap *)
(*****************************************************************************)

let value_size r =
  let w = Obj.reachable_words r in
  w * (Sys.word_size / 8)

let debug_value_size = value_size

(*****************************************************************************)
(* Module returning the MD5 of the key. It's because the code in C land
 * expects this format. I prefer to make it an abstract type to make sure
 * we don't forget to give the MD5 instead of the key itself.
 *)
(*****************************************************************************)

module type Key = sig
  (* The type of keys that OCaml-land callers try to insert *)
  type userkey

  (* The type of keys that get stored in the C hashtable *)
  type t

  (* The type of old keys that get stored in the C hashtable *)
  type old

  (* The md5 of an old or a new key *)
  type md5

  (* Creation/conversion primitives *)
  val make : Prefix.t -> userkey -> t

  val make_old : Prefix.t -> userkey -> old

  val to_old : t -> old

  val new_from_old : old -> t

  (* Md5 primitives *)
  val md5 : t -> md5

  val md5_old : old -> md5

  val string_of_md5 : md5 -> string
end

module type Value = sig
  type t

  val prefix : Prefix.t

  val description : string
end

(*****************************************************************************)
(* The interface that all keys need to implement *)
(*****************************************************************************)

module type UserKeyType = sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int
end

module KeyFunctor (UserKeyType : UserKeyType) : Key with type userkey = UserKeyType.t = struct
  type userkey = UserKeyType.t

  type t = string

  type old = string

  type md5 = string

  (* The prefix we use for old keys. The prefix guarantees that we never
   * mix old and new data, because a key can never start with the prefix
   * "old_", it always starts with a number (cf Prefix.make()).
   *)
  let old_prefix = "old_"

  let make prefix x = Prefix.make_key prefix (UserKeyType.to_string x)

  let make_old prefix x = old_prefix ^ Prefix.make_key prefix (UserKeyType.to_string x)

  let to_old x = old_prefix ^ x

  let new_from_old x =
    let module S = String in
    S.sub x (S.length old_prefix) (S.length x - S.length old_prefix)

  let md5 = Digest.string

  let md5_old = Digest.string

  let string_of_md5 x = x
end

(*****************************************************************************)
(* Immediate access to shared memory (cf hh_shared.c for the underlying
 * representation).
 *)
(*****************************************************************************)
module Raw (Key : Key) (Value : Value) : sig
  val add : Key.md5 -> Value.t -> unit

  val mem : Key.md5 -> bool

  val get : Key.md5 -> Value.t

  val remove : Key.md5 -> unit

  val move : Key.md5 -> Key.md5 -> unit
end = struct
  type addr = int

  (* Returns address into the heap, alloc size, and orig size *)
  external hh_store : Value.t -> addr * int * int = "hh_store_ocaml"

  external hh_deserialize : addr -> Value.t = "hh_deserialize"

  external hh_add : Key.md5 -> addr -> unit = "hh_add"

  external hh_mem : Key.md5 -> bool = "hh_mem"

  external hh_get_size : addr -> int = "hh_get_size"

  external hh_get : Key.md5 -> addr = "hh_get"

  external hh_remove : Key.md5 -> unit = "hh_remove"

  external hh_move : Key.md5 -> Key.md5 -> unit = "hh_move"

  let hh_store x = WorkerCancel.with_worker_exit (fun () -> hh_store x)

  let hh_deserialize x = WorkerCancel.with_worker_exit (fun () -> hh_deserialize x)

  let hh_mem x = WorkerCancel.with_worker_exit (fun () -> hh_mem x)

  let hh_add x y = WorkerCancel.with_worker_exit (fun () -> hh_add x y)

  let hh_get x = WorkerCancel.with_worker_exit (fun () -> hh_get x)

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
    hh_add key addr;
    if hh_log_level () > 0 && compressed_size > 0 then log_serialize compressed_size original_size

  let mem key = hh_mem key

  let get key =
    let addr = hh_get key in
    let v = hh_deserialize addr in
    if hh_log_level () > 0 then log_deserialize (hh_get_size addr) (Obj.repr v);
    v

  let remove key = hh_remove key

  let move from_key to_key = hh_move from_key to_key
end

module New (Key : Key) (Value : Value) : sig
  (* Adds a binding to the table, the table is left unchanged if the
   * key was already bound.
   *)
  val add : Key.t -> Value.t -> unit

  val mem : Key.t -> bool

  val get : Key.t -> Value.t option

  val remove : Key.t -> unit

  (* Binds the key to the old one.
   * If 'mykey' is bound to 'myvalue', oldifying 'mykey' makes 'mykey'
   * accessible to the "Old" module, in other words: "Old.mem mykey" returns
   * true and "New.mem mykey" returns false after oldifying.
   *)
  val oldify : Key.t -> unit
end = struct
  module Raw = Raw (Key) (Value)

  let add key value = Raw.add (Key.md5 key) value

  let mem key = Raw.mem (Key.md5 key)

  let get key =
    let key = Key.md5 key in
    if Raw.mem key then
      Some (Raw.get key)
    else
      None

  let remove key =
    let key = Key.md5 key in
    if Raw.mem key then (
      Raw.remove key;
      assert (not (Raw.mem key))
    ) else
      ()

  let oldify key =
    if mem key then
      let old_key = Key.to_old key in
      Raw.move (Key.md5 key) (Key.md5_old old_key)
    else
      ()
end

(* Same as new, but for old values *)
module Old (Key : Key) (Value : Value) : sig
  val get : Key.old -> Value.t option

  val remove : Key.old -> unit

  val mem : Key.old -> bool

  (* Takes an old value and moves it back to a "new" one *)
  val revive : Key.old -> unit
end = struct
  module Raw = Raw (Key) (Value)

  let get key =
    let key = Key.md5_old key in
    if Raw.mem key then
      Some (Raw.get key)
    else
      None

  let mem key = Raw.mem (Key.md5_old key)

  let remove key = if mem key then Raw.remove (Key.md5_old key)

  let revive key =
    if mem key then (
      let new_key = Key.new_from_old key in
      let new_key = Key.md5 new_key in
      let old_key = Key.md5_old key in
      if Raw.mem new_key then Raw.remove new_key;
      Raw.move old_key new_key
    )
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

  module KeySet : Set.S with type elt = key

  val add : key -> value -> unit

  val get : key -> value option

  val get_old : key -> value option

  val remove_old_batch : KeySet.t -> unit

  val remove_batch : KeySet.t -> unit

  val mem : key -> bool

  val mem_old : key -> bool

  val oldify_batch : KeySet.t -> unit

  val revive_batch : KeySet.t -> unit
end

module type DebugLocalCache = sig
  module DebugL1 : DebugCacheType

  module DebugL2 : DebugCacheType
end

module type LocalCache = sig
  include DebugLocalCache

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

  module DebugCache : DebugLocalCache
end

(*****************************************************************************)
(* A functor returning an implementation of the S module without caching. *)
(*****************************************************************************)

module NoCache (UserKeyType : UserKeyType) (Value : Value) :
  NoCache
    with type key = UserKeyType.t
     and type value = Value.t
     and module KeySet = Set.Make(UserKeyType) = struct
  module Key = KeyFunctor (UserKeyType)
  module New = New (Key) (Value)
  module Old = Old (Key) (Value)
  module KeySet = Set.Make (UserKeyType)

  type key = UserKeyType.t

  type value = Value.t

  let add x y = New.add (Key.make Value.prefix x) y

  let get x = New.get (Key.make Value.prefix x)

  let get_old x =
    let key = Key.make_old Value.prefix x in
    Old.get key

  let remove_batch xs =
    KeySet.iter
      begin
        fun str_key ->
        let key = Key.make Value.prefix str_key in
        New.remove key
      end
      xs

  let oldify_batch xs =
    KeySet.iter
      begin
        fun str_key ->
        let key = Key.make Value.prefix str_key in
        if New.mem key then
          New.oldify key
        else
          let key = Key.make_old Value.prefix str_key in
          Old.remove key
      end
      xs

  let revive_batch xs =
    KeySet.iter
      begin
        fun str_key ->
        let old_key = Key.make_old Value.prefix str_key in
        if Old.mem old_key then
          Old.revive old_key
        else
          let key = Key.make Value.prefix str_key in
          New.remove key
      end
      xs

  let mem x = New.mem (Key.make Value.prefix x)

  let mem_old x = Old.mem (Key.make_old Value.prefix x)

  let remove_old_batch xs =
    KeySet.iter
      begin
        fun str_key ->
        let key = Key.make_old Value.prefix str_key in
        Old.remove key
      end
      xs
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
      l := List.sort ~compare:(fun (_, x, _) (_, y, _) -> y - x) !l;
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
    with Not_found ->
      incr size;
      let elt = (ref 0, y) in
      Hashtbl.replace cache x elt;
      ()

  let find x =
    let (freq, value) = Hashtbl.find cache x in
    incr freq;
    value

  let get x = (try Some (find x) with Not_found -> None)

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
      ) );

    (* Add the new element, but bump the size only if it's a new addition. *)
    Queue.push x queue;
    if not (Hashtbl.mem cache x) then incr size;
    Hashtbl.replace cache x y

  let find x = Hashtbl.find cache x

  let get x = (try Some (find x) with Not_found -> None)

  let remove x =
    try
      if Hashtbl.mem cache x then decr size;
      Hashtbl.remove cache x
    with Not_found -> ()
end

(*****************************************************************************)
(* Every time we create a new cache, a function that knows how to clear the
 * cache is registered in the "invalidate_callback_list" global.
 *)
(*****************************************************************************)

let invalidate_callback_list = ref []

module LocalCache (UserKeyType : UserKeyType) (Value : Value) :
  LocalCache with type key := UserKeyType.t and type value := Value.t = struct
  module Config = struct
    type key = UserKeyType.t

    type value = Value.t

    let capacity = 1000
  end

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

  let () =
    invalidate_callback_list :=
      begin
        fun () ->
        L1.clear ();
        L2.clear ()
      end
      :: !invalidate_callback_list
end

(*****************************************************************************)
(* A functor returning an implementation of the S module with caching.
 * We need to avoid constantly deserializing types, because it costs us too
 * much time. The caches keep a deserialized version of the types.
 *)
(*****************************************************************************)
module WithCache (UserKeyType : UserKeyType) (Value : Value) :
  WithCache
    with type key = UserKeyType.t
     and type value = Value.t
     and module KeySet = Set.Make(UserKeyType) = struct
  module Direct = NoCache (UserKeyType) (Value)

  type key = Direct.key

  type value = Direct.value

  module KeySet = Direct.KeySet
  module Cache = LocalCache (UserKeyType) (Value)

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
        0. );
    Measure.sample
      "(ALL cache hit rate)"
      ( if hit then
        1.
      else
        0. )

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
  open Bigarray

  type nonrec heap = heap

  (* Phantom type parameter provides type-safety to callers of this API.
   * Internally, these are all just ints, so be careful! *)
  type _ addr = int

  type chunk = {
    heap: heap;
    mutable next_addr: int;
    mutable remaining_size: int;
  }

  type heap_string

  type 'a addr_tbl

  type checked_file

  type exports

  type export_def

  type module_ref

  type remote_ref

  type local_def

  type pattern_def

  type pattern

  type size = int

  let get_heap () =
    match !heap_ref with
    | None -> failwith "get_heap: not connected"
    | Some heap -> heap

  external alloc : size -> 'a addr = "hh_ml_alloc"

  let alloc size f =
    let addr = alloc size in
    let chunk = { heap = get_heap (); next_addr = addr; remaining_size = size } in
    let x = f chunk in
    (* Ensure allocated space was initialized. *)
    assert (chunk.remaining_size = 0);
    assert (chunk.next_addr = addr + size);
    x

  (* Addresses are relative to the hashtbl pointer, so the null address actually
   * points to the hash field of the first hashtbl entry, which is never a
   * meaningful address, so we can use it to represent "missing" or similar.
   *
   * Naturally, we need to be careful not to dereference the null addr! Any
   * internal use of null should be hidden from callers of this module. *)
  let null_addr = 0

  (* header utils *)

  type tag =
    | Serialized_tag
    | String_tag
    | Addr_map_tag
    | Checked_file_tag
    | Exports_tag
    | Export_def_tag
    | Module_ref_tag
    | Remote_ref_tag
    | Local_def_tag
    | Pattern_def_tag
    | Pattern_tag

  (* avoid unused constructor warning *)
  let () = ignore Serialized_tag

  (* constant constructors are integers *)
  let tag_val : tag -> int = Obj.magic

  let mk_header tag size =
    (* lower byte of header is reserved for tag, lsb will be set when converting
     * to intnat before writing to shmem, see unsafe_write_header. *)
    (tag_val tag lsl 1) lor (size lsl 7)

  let obj_tag hd = (hd lsr 1) land 0x3F

  let obj_size hd = hd lsr 7

  (* sizes *)

  let header_size = 1

  let addr_size = 1

  (* Obj used as an efficient way to get at the word size of an OCaml string
   * directly from the block header, since that's the size we need. *)
  let string_size s = Obj.size (Obj.repr s)

  let addr_tbl_size xs = addr_size * Array.length xs

  let checked_file_size = 8 * addr_size

  let exports_size def = string_size def

  let export_def_size = function
    | None -> 0
    | Some def -> string_size def

  let module_ref_size ref = string_size ref

  let remote_ref_size ref = string_size ref

  let local_def_size def = string_size def

  let pattern_def_size def = string_size def

  let pattern_size pattern = string_size pattern

  (* headers *)

  let string_header s =
    let size = string_size s in
    mk_header String_tag size

  let addr_tbl_header xs =
    let size = addr_tbl_size xs in
    mk_header Addr_map_tag size

  let checked_file_header = mk_header Checked_file_tag checked_file_size

  let exports_header exports =
    let size = exports_size exports in
    mk_header Exports_tag size

  let export_def_header def =
    let size = export_def_size (Some def) in
    mk_header Export_def_tag size

  let module_ref_header ref =
    let size = module_ref_size ref in
    mk_header Module_ref_tag size

  let remote_ref_header ref =
    let size = remote_ref_size ref in
    mk_header Remote_ref_tag size

  let local_def_header def =
    let size = local_def_size def in
    mk_header Local_def_tag size

  let pattern_def_header def =
    let size = pattern_def_size def in
    mk_header Pattern_def_tag size

  let pattern_header pattern =
    let size = pattern_size pattern in
    mk_header Pattern_tag size

  (* offsets *)

  let filename_addr file = file + 1

  let exports_addr file = file + 2

  let export_def_addr file = file + 3

  let module_refs_addr file = file + 4

  let local_defs_addr file = file + 5

  let remote_refs_addr file = file + 6

  let pattern_defs_addr file = file + 7

  let patterns_addr file = file + 8

  (* read *)

  let assert_tag hd tag = assert (obj_tag hd = tag_val tag)

  (* Read a string from the heap at the specified address with the specified
   * size (in words). This read is not bounds checked or type checked; caller
   * must ensure that the given destination contains string data. *)
  external unsafe_read_string : _ addr -> int -> string = "hh_read_string"

  (* Read a header from the heap. The low bit of the header word is always set,
   * but when converting to an OCaml int we forfeit that bit to OCaml's own tag.
   *)
  let read_header heap addr =
    let hd_nat = Array1.get heap addr in
    (* double-check that the data looks like a header *)
    assert (Nativeint.(logand hd_nat 1n = 1n));
    Nativeint.(to_int (shift_right_logical hd_nat 1))

  let read_header_checked heap tag addr =
    let hd = read_header heap addr in
    assert_tag hd tag;
    hd

  (* Read an address from the heap. In the heap, we store word-aligned byte
   * offsets, but in OCaml we prefer word offsets. As with Val_addr when passing
   * back addresses from C, we perform the byte->word offset conversion here. *)
  let read_addr heap addr =
    let addr_nat = Array1.get heap addr in
    (* double-check that the data looks like an address *)
    assert (Nativeint.(logand addr_nat 1n = 0n));
    Nativeint.(to_int (shift_right_logical addr_nat 3))

  let read_string_generic tag addr =
    let hd = read_header_checked (get_heap ()) tag addr in
    let offset = addr + header_size in
    let length = obj_size hd in
    unsafe_read_string offset length

  let read_string addr = read_string_generic String_tag addr

  let read_addr_tbl_generic f addr init =
    let heap = get_heap () in
    let hd = read_header_checked heap Addr_map_tag addr in
    init (obj_size hd) (fun i -> f (read_addr heap (addr + header_size + i)))

  let read_addr_tbl f addr = read_addr_tbl_generic f addr Array.init

  let read_exports addr = read_string_generic Exports_tag addr

  let read_export_def addr =
    if addr = null_addr then
      None
    else
      Some (read_string_generic Export_def_tag addr)

  let read_module_ref addr = read_string_generic Module_ref_tag addr

  let read_remote_ref addr = read_string_generic Remote_ref_tag addr

  let read_local_def addr = read_string_generic Local_def_tag addr

  let read_pattern_def addr = read_string_generic Pattern_def_tag addr

  let read_pattern addr = read_string_generic Pattern_tag addr

  (* getters *)

  let file_name file = read_addr (get_heap ()) (filename_addr file)

  let file_exports file = read_addr (get_heap ()) (exports_addr file)

  let file_export_def file = read_addr (get_heap ()) (export_def_addr file)

  let file_module_refs file = read_addr (get_heap ()) (module_refs_addr file)

  let file_local_defs file = read_addr (get_heap ()) (local_defs_addr file)

  let file_remote_refs file = read_addr (get_heap ()) (remote_refs_addr file)

  let file_pattern_defs file = read_addr (get_heap ()) (pattern_defs_addr file)

  let file_patterns file = read_addr (get_heap ()) (patterns_addr file)

  (* write *)

  (* Write a header at a specified address in the heap. This write is not
   * bounds checked; caller must ensure the given destination has already been
   * allocated. *)
  let unsafe_write_header_at heap dst hd =
    Array1.unsafe_set heap dst Nativeint.(logor 1n (shift_left (of_int hd) 1))

  (* Write an address at a specified address in the heap. This write is not
   * bounds checked; caller must ensure the given destination has already been
   * allocated. *)
  let unsafe_write_addr_at heap dst addr =
    Array1.unsafe_set heap dst Nativeint.(shift_left (of_int addr) 1)

  (* Write a string at the specified address in the heap. This write is not
   * bounds checked; caller must ensure the given destination has already been
   * allocated. *)
  external unsafe_write_string_at : _ addr -> string -> unit = "hh_write_string" [@@noalloc]

  (* Write a header into the given chunk and advance the chunk address. This
   * write is not bounds checked; caller must ensure the given destination has
   * already been allocated. *)
  let unsafe_write_header chunk hd =
    unsafe_write_header_at chunk.heap chunk.next_addr hd;
    chunk.next_addr <- chunk.next_addr + header_size

  (* Write an address into the given chunk and advance the chunk address. This
   * write is not bounds checked; caller must ensure the given destination has
   * already been allocated. *)
  let unsafe_write_addr chunk addr =
    unsafe_write_addr_at chunk.heap chunk.next_addr addr;
    chunk.next_addr <- chunk.next_addr + addr_size

  (* Write a string into the given chunk and advance the chunk address. This
   * write is not bounds checked; caller must ensure the given destination has
   * already been allocated. *)
  let unsafe_write_string chunk s =
    unsafe_write_string_at chunk.next_addr s;
    chunk.next_addr <- chunk.next_addr + string_size s

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

  let write_checked_file
      chunk filename exports export_def module_refs local_defs remote_refs pattern_defs patterns =
    let checked_file = write_header chunk checked_file_header in
    unsafe_write_addr chunk filename;
    unsafe_write_addr chunk exports;
    unsafe_write_addr chunk export_def;
    unsafe_write_addr chunk module_refs;
    unsafe_write_addr chunk local_defs;
    unsafe_write_addr chunk remote_refs;
    unsafe_write_addr chunk pattern_defs;
    unsafe_write_addr chunk patterns;
    checked_file

  let write_exports chunk exports =
    let heap_exports = write_header chunk (exports_header exports) in
    unsafe_write_string chunk exports;
    heap_exports

  let write_export_def chunk = function
    | None -> null_addr
    | Some def ->
      let heap_def = write_header chunk (export_def_header def) in
      unsafe_write_string chunk def;
      heap_def

  let write_module_ref chunk ref =
    let heap_ref = write_header chunk (module_ref_header ref) in
    unsafe_write_string chunk ref;
    heap_ref

  let write_local_def chunk def =
    let heap_def = write_header chunk (local_def_header def) in
    unsafe_write_string chunk def;
    heap_def

  let write_remote_ref chunk ref =
    let heap_ref = write_header chunk (remote_ref_header ref) in
    unsafe_write_string chunk ref;
    heap_ref

  let write_pattern_def chunk def =
    let heap_resolved = write_header chunk (pattern_def_header def) in
    unsafe_write_string chunk def;
    heap_resolved

  let write_pattern chunk pattern =
    let heap_pattern = write_header chunk (pattern_header pattern) in
    unsafe_write_string chunk pattern;
    heap_pattern

  let write_addr_tbl f chunk xs =
    let hd = addr_tbl_header xs in
    let map = write_header chunk hd in
    chunk.next_addr <- chunk.next_addr + obj_size hd;
    Array.iteri
      (fun i x ->
        let addr = f chunk x in
        unsafe_write_addr_at chunk.heap (map + header_size + i) addr)
      xs;
    map
end
