(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_core

(* Don't change the ordering of this record without updating hh_shared_init in
 * hh_shared.c, which indexes into config objects *)
type config = {
  global_size      : int;
  heap_size        : int;
  dep_table_pow    : int;
  hash_table_pow   : int;
  shm_dirs         : string list;
  shm_min_avail    : int;
  log_level        : int;
}

(* Allocated in C only. *)
type handle = private {
  h_fd: Unix.file_descr;
  h_global_size: int;
  h_heap_size: int;
}

exception Out_of_shared_memory
exception Hash_table_full
exception Dep_table_full
exception Heap_full
exception Sql_assertion_failure of int
exception Failed_anonymous_memfd_init
exception Less_than_minimum_available of int
exception Failed_to_use_shm_dir of string
exception C_assertion_failure of string
let () =
  Callback.register_exception "out_of_shared_memory" Out_of_shared_memory;
  Callback.register_exception "hash_table_full" Hash_table_full;
  Callback.register_exception "dep_table_full" Dep_table_full;
  Callback.register_exception "heap_full" Heap_full;
  Callback.register_exception
    "sql_assertion_failure"
    (Sql_assertion_failure 0);
  Callback.register_exception
    "failed_anonymous_memfd_init"
    Failed_anonymous_memfd_init;
  Callback.register_exception
    "less_than_minimum_available"
    (Less_than_minimum_available 0);
  Callback.register_exception
    "c_assertion_failure" (C_assertion_failure "dummy string")

(*****************************************************************************)
(* Initializes the shared memory. Must be called before forking. *)
(*****************************************************************************)
external hh_shared_init :
  config:config -> shm_dir:string option -> handle = "hh_shared_init"

let anonymous_init config =
  hh_shared_init
    ~config
    ~shm_dir: None

let rec shm_dir_init config = function
| [] ->
    Hh_logger.log
      "We've run out of filesystems to use for shared memory";
    raise Out_of_shared_memory
| shm_dir::shm_dirs ->
    let shm_min_avail = config.shm_min_avail in
    begin try
      (* For some reason statvfs is segfaulting when the directory doesn't
       * exist, instead of returning -1 and an errno *)
      if not (Sys.file_exists shm_dir)
      then raise (Failed_to_use_shm_dir "shm_dir does not exist");
      hh_shared_init
        ~config
        ~shm_dir:(Some shm_dir)
    with
    | Less_than_minimum_available avail ->
        EventLogger.(log_if_initialized (fun () ->
          sharedmem_less_than_minimum_available
            ~shm_dir
            ~shm_min_avail
            ~avail
        ));
        if !Utils.debug
        then Hh_logger.log
          "Filesystem %s only has %d bytes available, \
            which is less than the minimum %d bytes"
          shm_dir
          avail
          config.shm_min_avail;
        shm_dir_init config shm_dirs
    | Unix.Unix_error (e, fn, arg) ->
        let fn_string =
          if fn = ""
          then ""
          else Utils.spf " thrown by %s(%s)" fn arg in
        let reason =
          Utils.spf "Unix error%s: %s" fn_string (Unix.error_message e) in
        EventLogger.(log_if_initialized (fun () ->
          sharedmem_failed_to_use_shm_dir ~shm_dir ~reason
        ));
        if !Utils.debug
        then Hh_logger.log
          "Failed to use shm dir `%s`: %s"
          shm_dir
          reason;
        shm_dir_init config shm_dirs
    | Failed_to_use_shm_dir reason ->
        EventLogger.(log_if_initialized (fun () ->
          sharedmem_failed_to_use_shm_dir ~shm_dir ~reason
        ));
        if !Utils.debug
        then Hh_logger.log
          "Failed to use shm dir `%s`: %s"
          shm_dir
          reason;
        shm_dir_init config shm_dirs
    end

let init config =
 try anonymous_init config
  with Failed_anonymous_memfd_init ->
    EventLogger.(log_if_initialized (fun () ->
      sharedmem_failed_anonymous_memfd_init ()
    ));
    if !Utils.debug
    then Hh_logger.log "Failed to use anonymous memfd init";
    shm_dir_init config config.shm_dirs

external connect : handle -> is_master:bool -> unit = "hh_connect"

(*****************************************************************************)
(* The shared memory garbage collector. It must be called every time we
 * free data (cf hh_shared.c for the underlying C implementation).
 *)
(*****************************************************************************)
external hh_collect: bool -> unit = "hh_collect"

(*****************************************************************************)
(* Serializes the dependency table and writes it to a file *)
(*****************************************************************************)
external save_dep_table_sqlite: string -> int = "hh_save_dep_table_sqlite"

(*****************************************************************************)
(* Loads the dependency table by reading from a file *)
(*****************************************************************************)
external load_dep_table_sqlite: string -> int = "hh_load_dep_table_sqlite"

(*****************************************************************************)
(* Cleans up the artifacts generated by SQL *)
(*****************************************************************************)
external cleanup_sqlite: unit -> unit = "hh_cleanup_sqlite"

(*****************************************************************************)
(* The size of the dynamically allocated shared memory section *)
(*****************************************************************************)
external heap_size: unit -> int = "hh_heap_size"

(*****************************************************************************)
(* The logging level for shared memory statistics *)
(* 0 = nothing *)
(* 1 = log totals, averages, min, max bytes marshalled and unmarshalled *)
(*****************************************************************************)
external hh_log_level : unit -> int = "hh_log_level"

(*****************************************************************************)
(* The number of used slots in our hashtable *)
(*****************************************************************************)
external hash_used_slots : unit -> int * int = "hh_hash_used_slots"

(*****************************************************************************)
(* The total number of slots in our hashtable *)
(*****************************************************************************)
external hash_slots : unit -> int = "hh_hash_slots"

(*****************************************************************************)
(* The number of used slots in our dependency table *)
(*****************************************************************************)
external dep_used_slots : unit -> int = "hh_dep_used_slots"

(*****************************************************************************)
(* The total number of slots in our dependency table *)
(*****************************************************************************)
external dep_slots : unit -> int = "hh_dep_slots"

(*****************************************************************************)
(* Must be called after the initialization of the hack server is over.
 * (cf serverInit.ml). *)
(*****************************************************************************)
external hh_init_done: unit -> unit = "hh_call_after_init"

external hh_check_heap_overflow: unit -> bool  = "hh_check_heap_overflow"

let init_done () =
  hh_init_done ();
  if hh_log_level() > 0 then Measure.print_stats ();
  EventLogger.sharedmem_init_done (heap_size ())

type table_stats = {
  nonempty_slots : int;
  used_slots : int;
  slots : int;
}

let dep_stats () =
  let used = dep_used_slots () in
  {
    nonempty_slots = used;
    used_slots = used;
    slots = dep_slots ();
  }

let hash_stats () =
  let used_slots, nonempty_slots = hash_used_slots () in
  {
    nonempty_slots;
    used_slots;
    slots = hash_slots ();
  }

let collect (effort : [ `gentle | `aggressive ]) =
  let old_size = heap_size () in
  Stats.update_max_heap_size old_size;
  let start_t = Unix.gettimeofday () in
  hh_collect (effort = `aggressive);
  let new_size = heap_size () in
  let time_taken = Unix.gettimeofday () -. start_t in
  if old_size <> new_size then begin
    Hh_logger.log
      "Sharedmem GC: %d bytes before; %d bytes after; in %f seconds"
      old_size new_size time_taken;
    EventLogger.sharedmem_gc_ran effort old_size new_size time_taken
  end

let is_heap_overflow () = hh_check_heap_overflow ()

(*****************************************************************************)
(* Compute size of values in the garbage-collected heap *)
(*****************************************************************************)
module HeapSize = struct

  let rec traverse ((visited:ISet.t), acc) r =
    if Obj.is_block r then begin
      let p:int = Obj.magic r in
      if ISet.mem p visited
      then (visited,acc)
      else begin
        let visited' = ISet.add p visited in
        let n = Obj.size r in
        let acc' = acc + 1 + n in
        if Obj.tag r < Obj.no_scan_tag
        then traverse_fields (visited', acc') r n
        else (visited', acc')
      end
    end else (visited, acc)

  and traverse_fields acc r i =
    let i = i - 1 in
    if i < 0 then acc
    else traverse_fields (traverse acc (Obj.field r i)) r i

  (* Return size in bytes that o occupies in GC heap *)
  let size r =
    let (_, w) = traverse (ISet.empty, 0) r in
    w * (Sys.word_size / 8)
end

let value_size = HeapSize.size

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
  val make     : Prefix.t -> userkey -> t
  val make_old : Prefix.t -> userkey -> old

  val to_old   : t -> old

  val new_from_old : old -> t

  (* Md5 primitives *)
  val md5     : t -> md5
  val md5_old : old -> md5

end

module KeyFunctor (UserKeyType : sig
  type t
  val to_string : t -> string
end) : Key with type userkey = UserKeyType.t = struct

  type userkey = UserKeyType.t
  type t       = string
  type old     = string
  type md5     = string

  (* The prefix we use for old keys. The prefix guarantees that we never
   * mix old and new data, because a key can never start with the prefix
   * "old_", it always starts with a number (cf Prefix.make()).
   *)
  let old_prefix = "old_"

  let make prefix x = Prefix.make_key prefix (UserKeyType.to_string x)
  let make_old prefix x =
    old_prefix^Prefix.make_key prefix (UserKeyType.to_string x)

  let to_old x = old_prefix^x

  let new_from_old x =
    let module S = String in
    S.sub x (S.length old_prefix) (S.length x - S.length old_prefix)

  let md5 = Digest.string
  let md5_old = Digest.string

end

(*****************************************************************************)
(* Raw interface to shared memory (cf hh_shared.c for the underlying
 * representation).
 *)
(*****************************************************************************)
module Raw (Key: Key) (Value:Value.Type): sig
  val add    : Key.md5 -> Value.t -> unit
  val mem    : Key.md5 -> bool
  val get    : Key.md5 -> Value.t
  val remove : Key.md5 -> unit
  val move   : Key.md5 -> Key.md5 -> unit

  module LocalChanges : sig
    val push_stack : unit -> unit
    val pop_stack : unit -> unit
    val revert : Key.md5 -> unit
    val commit : Key.md5 -> unit
    val revert_all : unit -> unit
    val commit_all : unit -> unit
  end
end = struct

  (* Returns the number of bytes allocated in the heap, or a negative number
   * if no new memory was allocated *)
  external hh_add    : Key.md5 -> Value.t -> int * int = "hh_add"
  external hh_mem         : Key.md5 -> bool            = "hh_mem"
  external hh_get_size    : Key.md5 -> int             = "hh_get_size"
  external hh_get_and_deserialize: Key.md5 -> Value.t = "hh_get_and_deserialize"
  external hh_remove      : Key.md5 -> unit            = "hh_remove"
  external hh_move        : Key.md5 -> Key.md5 -> unit = "hh_move"

  let log_serialize compressed original =
    let compressed = float compressed in
    let original = float original in
    let saved = original -. compressed in
    let ratio = compressed /. original in
    Measure.sample (Value.description
      ^ " (bytes serialized into shared heap)") compressed;
    Measure.sample ("ALL bytes serialized into shared heap") compressed;
    Measure.sample (Value.description
      ^ " (bytes saved in shared heap due to compression)") saved;
    Measure.sample ("ALL bytes saved in shared heap due to compression") saved;
    Measure.sample (Value.description
      ^ " (shared heap compression ratio)") ratio;
    Measure.sample ("ALL bytes shared heap compression ratio") ratio

  let log_deserialize l r =
    let sharedheap = float l in
    let localheap = float (value_size r) in
    begin
      Measure.sample (Value.description
        ^ " (bytes deserialized from shared heap)") sharedheap;
      Measure.sample ("ALL bytes deserialized from shared heap") sharedheap;
      Measure.sample (Value.description
        ^ " (bytes allocated for deserialized value)") localheap;
      Measure.sample ("ALL bytes allocated for deserialized value") localheap
    end

  (**
   * Represents a set of local changes to the view of the shared memory heap
   * WITHOUT materializing to the changes in the actual heap. This allows us to
   * make speculative changes to the view of the world that can be reverted
   * quickly and correctly.
   *
   * A LocalChanges maintains the same invariants as the shared heap. Except
   * add are allowed to overwrite filled keys. This is for convenience so we
   * do not need to remove filled keys upfront.
   *
   * LocalChanges can be committed. This will apply the changes to the previous
   * stack, or directly to shared memory if there are no other active stacks.
   * Since changes are kept local to the process, this is NOT compatible with
   * the parallelism provided by MultiWorker.ml
   *)
  module LocalChanges = struct

    type action =
      (* The value does not exist in the current stack. When committed this
       * action will invoke remove on the previous stack.
       *)
      | Remove
      (* The value is added to a previously empty slot. When committed this
       * action will invoke add on the previous stack.
       *)
      | Add of Value.t
      (* The value is replacing a value already associated with a key in the
       * previous stack. When committed this action will invoke remove then
       * add on the previous stack.
       *)
      | Replace of Value.t

    type t = {
      current : (Key.md5, action) Hashtbl.t;
      prev    : t option;
    }

    let stack: t option ref = ref None

    let rec mem stack_opt key =
      match stack_opt with
      | None -> hh_mem key
      | Some stack ->
        try Hashtbl.find stack.current key <> Remove
        with Not_found -> mem stack.prev key

    let rec get stack_opt key =
      match stack_opt with
      | None ->
        let v = hh_get_and_deserialize key in
        if hh_log_level() > 0
        then (log_deserialize (hh_get_size key) (Obj.repr v));
        v
      | Some stack ->
        try match Hashtbl.find stack.current key with
          | Remove -> failwith "Trying to get a non-existent value"
          | Replace value
          | Add value -> value
        with Not_found ->
          get stack.prev key

    (**
     * For remove/add it is best to think of them in terms of a state machine.
     * A key can be in the following states:
     *
     *  Remove:
     *    Local changeset removes a key from the previous stack
     *  Replace:
     *    Local changeset replaces value of a key in previous stack
     *  Add:
     *    Local changeset associates a value with a key. The key is not
     *    present in the previous stacks
     *  Empty:
     *    No local changes and key is not present in previous stack
     *  Filled:
     *    No local changes and key has an associated value in previous stack
     *  *Error*:
     *    This means an exception will occur
     **)
    (**
     * Transitions table:
     *   Remove  -> *Error*
     *   Replace -> Remove
     *   Add     -> Empty
     *   Empty   -> *Error*
     *   Filled  -> Remove
     *)
    let remove stack_opt key =
      match stack_opt with
      | None -> hh_remove key
      | Some stack ->
        try match Hashtbl.find stack.current key with
          | Remove -> failwith "Trying to remove a non-existent value"
          | Replace _ -> Hashtbl.replace stack.current key Remove
          | Add _ -> Hashtbl.remove stack.current key
        with Not_found ->
          if mem stack.prev key then
            Hashtbl.replace stack.current key Remove
          else
            failwith "Trying to remove a non-existent value"

    (**
     * Transitions table:
     *   Remove  -> Replace
     *   Replace -> Replace
     *   Add     -> Add
     *   Empty   -> Add
     *   Filled  -> Replace
     *)
    let add stack_opt key value =
      match stack_opt with
      | None ->
        let compressed_size, original_size = hh_add key value in
        if hh_log_level() > 0 && compressed_size > 0
        then log_serialize compressed_size original_size
      | Some stack ->
        try match Hashtbl.find stack.current key with
          | Remove
          | Replace _ -> Hashtbl.replace stack.current key (Replace value)
          | Add _ -> Hashtbl.replace stack.current key (Add value)
        with Not_found ->
          if mem stack.prev key then
            Hashtbl.replace stack.current key (Replace value)
          else
            Hashtbl.replace stack.current key (Add value)

    let move stack_opt from_key to_key =
      match stack_opt with
      | None -> hh_move from_key to_key
      | Some stack ->
        assert (mem stack_opt from_key);
        assert (not @@ mem stack_opt to_key);
        let value = get stack_opt from_key in
        remove stack_opt from_key;
        add stack_opt to_key value

    let commit_action changeset key elem =
      match elem with
      | Remove -> remove changeset key
      | Add value -> add changeset key value
      | Replace value ->
        remove changeset key;
        add changeset key value

    (** Public API **)
    let push_stack () =
      stack := Some ({ current = Hashtbl.create 128; prev = !stack; })

    let pop_stack () =
      match !stack with
      | None ->
        failwith "There are no active local change stacks. Nothing to pop!"
      | Some { prev; _ } -> stack := prev

    let revert key =
      match !stack with
      | None -> ()
      | Some changeset -> Hashtbl.remove changeset.current key

    let commit key =
      match !stack with
      | None -> ()
      | Some changeset ->
        try
          commit_action
            changeset.prev key @@ Hashtbl.find changeset.current key
        with Not_found -> ()

    let revert_all () =
      match !stack with
      | None -> ()
      | Some changeset -> Hashtbl.clear changeset.current

    let commit_all () =
      match !stack with
      | None -> ()
      | Some changeset ->
        Hashtbl.iter (commit_action changeset.prev) changeset.current
  end

  let add key value = LocalChanges.(add !stack key value)
  let mem key = LocalChanges.(mem !stack key)
  let get key = LocalChanges.(get !stack key)
  let remove key = LocalChanges.(remove !stack key)
  let move from_key to_key = LocalChanges.(move !stack from_key to_key)
end

(*****************************************************************************)
(* Module used to access "new" values (as opposed to old ones).
 * There are several cases where we need to compare the old and the new
 * representation of objects (to determine what has changed).
 * The "old" representation is the value that was bound to that key in the
 * last round of type-checking.
 * Despite the fact that the same storage is used under the hood, it's good
 * to separate the two interfaces to make sure we never mix old and new
 * values.
 *)
(*****************************************************************************)

module New : functor (Key : Key) -> functor(Value: Value.Type) -> sig

  (* Adds a binding to the table, the table is left unchanged if the
   * key was already bound.
   *)
  val add         : Key.t -> Value.t -> unit

  val get         : Key.t -> Value.t option
  val find_unsafe : Key.t -> Value.t
  val remove      : Key.t -> unit
  val mem         : Key.t -> bool

  (* Binds the key to the old one.
   * If 'mykey' is bound to 'myvalue', oldifying 'mykey' makes 'mykey'
   * accessible to the "Old" module, in other words: "Old.mem mykey" returns
   * true and "New.mem mykey" returns false after oldifying.
   *)
  val oldify      : Key.t -> unit

  module Raw: module type of Raw (Key) (Value)

end = functor (Key : Key) -> functor (Value : Value.Type) -> struct

  module Raw = Raw (Key) (Value)

  let add key value = Raw.add (Key.md5 key) value
  let mem key = Raw.mem (Key.md5 key)

  let get key =
    let key = Key.md5 key in
    if Raw.mem key
    then Some (Raw.get key)
    else None

  let find_unsafe key =
    match get key with
    | None -> raise Not_found
    | Some x -> x

  let remove key =
    let key = Key.md5 key in
    if Raw.mem key
    then begin
      Raw.remove key;
      assert (not (Raw.mem key));
    end
    else ()

  let oldify key =
    if mem key
    then
      let old_key = Key.to_old key in
      Raw.move (Key.md5 key) (Key.md5_old old_key)
    else ()
end

(* Same as new, but for old values *)
module Old : functor (Key : Key) -> functor (Value : Value.Type) ->
  functor (Raw : module type of Raw (Key) (Value)) -> sig

  val get         : Key.old -> Value.t option
  val remove      : Key.old -> unit
  val mem         : Key.old -> bool
  (* Takes an old value and moves it back to a "new" one *)
  val revive      : Key.old -> unit

end = functor (Key : Key) -> functor (Value: Value.Type) ->
  functor (Raw : module type of Raw (Key) (Value)) -> struct

  let get key =
    let key = Key.md5_old key in
    if Raw.mem key
    then Some (Raw.get key)
    else None

  let mem key = Raw.mem (Key.md5_old key)

  let remove key =
    if mem key
    then Raw.remove (Key.md5_old key)

  let revive key =
  if mem key
  then
    let new_key = Key.new_from_old key in
    let new_key = Key.md5 new_key in
    let old_key = Key.md5_old key in
    if Raw.mem new_key
    then Raw.remove new_key;
    Raw.move old_key new_key
end

(*****************************************************************************)
(* The signatures of what we are actually going to expose to the user *)
(*****************************************************************************)

module type NoCache = sig
  type key
  type t
  module KeySet : Set.S with type elt = key
  module KeyMap : MyMap.S with type key = key

  val add              : key -> t -> unit
  val get              : key -> t option
  val get_old          : key -> t option
  val get_old_batch    : KeySet.t -> t option KeyMap.t
  val remove_old_batch : KeySet.t -> unit
  val find_unsafe      : key -> t
  val get_batch        : KeySet.t -> t option KeyMap.t
  val remove_batch     : KeySet.t -> unit
  val mem              : key -> bool
  val oldify_batch     : KeySet.t -> unit
  val revive_batch     : KeySet.t -> unit

  module LocalChanges : sig
    val push_stack : unit -> unit
    val pop_stack : unit -> unit
    val revert_batch : KeySet.t -> unit
    val commit_batch : KeySet.t -> unit
    val revert_all : unit -> unit
    val commit_all : unit -> unit
  end
end

module type WithCache = sig
  include NoCache
  val write_through : key -> t -> unit
end

(*****************************************************************************)
(* The interface that all keys need to implement *)
(*****************************************************************************)

module type UserKeyType = sig
  type t
  val to_string : t -> string
  val compare : t -> t -> int
end

(*****************************************************************************)
(* A functor returning an implementation of the S module without caching. *)
(*****************************************************************************)

module NoCache (UserKeyType : UserKeyType) (Value : Value.Type) = struct

  module Key = KeyFunctor (UserKeyType)
  module New = New (Key) (Value)
  module Old = Old (Key) (Value) (New.Raw)
  module KeySet = Set.Make (UserKeyType)
  module KeyMap = MyMap.Make (UserKeyType)

  type key = UserKeyType.t
  type t = Value.t

  let add x y = New.add (Key.make Value.prefix x) y
  let find_unsafe x = New.find_unsafe (Key.make Value.prefix x)

  let get x =
    try Some (find_unsafe x) with Not_found -> None

  let get_old x =
    let key = Key.make_old Value.prefix x in
    Old.get key

  let get_old_batch xs =
    KeySet.fold begin fun str_key acc ->
      let key = Key.make_old Value.prefix str_key in
      KeyMap.add str_key (Old.get key) acc
    end xs KeyMap.empty

  let remove_batch xs =
    KeySet.iter begin fun str_key ->
      let key = Key.make Value.prefix str_key in
      New.remove key
    end xs

  let oldify_batch xs =
    KeySet.iter begin fun str_key ->
      let key = Key.make Value.prefix str_key in
      if New.mem key
      then
        New.oldify key
      else
        let key = Key.make_old Value.prefix str_key in
        Old.remove key
    end xs

  let revive_batch xs =
    KeySet.iter begin fun str_key ->
      let old_key = Key.make_old Value.prefix str_key in
      if Old.mem old_key
      then
        Old.revive old_key
      else
        let key = Key.make Value.prefix str_key in
        New.remove key
    end xs

  let get_batch xs =
    KeySet.fold begin fun str_key acc ->
      let key = Key.make Value.prefix str_key in
      match New.get key with
      | None -> KeyMap.add str_key None acc
      | Some data -> KeyMap.add str_key (Some data) acc
    end xs KeyMap.empty

  let mem x = New.mem (Key.make Value.prefix x)

  let remove_old_batch xs =
    KeySet.iter begin fun str_key ->
      let key = Key.make_old Value.prefix str_key in
      Old.remove key
    end xs

  module LocalChanges = struct
    include New.Raw.LocalChanges
    let revert_batch keys =
      KeySet.iter begin fun str_key ->
        let key = Key.make Value.prefix str_key in
        revert (Key.md5 key)
      end keys

    let commit_batch keys =
      KeySet.iter begin fun str_key ->
        let key = Key.make Value.prefix str_key in
        commit (Key.md5 key)
      end keys
  end
end

(*****************************************************************************)
(* All the cache are configured by a module of type ConfigType *)
(*****************************************************************************)

module type ConfigType = sig

(* The type of object we want to keep in cache *)
  type value

(* The capacity of the cache *)
  val capacity : int

end

(*****************************************************************************)
(* All the caches are functors returning a module of the following signature
 *)
(*****************************************************************************)

module type CacheType = sig
  type key
  type value

  val add: key -> value -> unit
  val get: key -> value option
  val remove: key -> unit
  val clear: unit -> unit
end

(*****************************************************************************)
(* Cache keeping the objects the most frequently used. *)
(*****************************************************************************)

module FreqCache (Key : sig type t end) (Config:ConfigType) :
  CacheType with type key := Key.t and type value := Config.value = struct

  type value = Config.value

(* The cache itself *)
  let (cache: (Key.t, int ref * Config.value) Hashtbl.t)
      = Hashtbl.create (2 * Config.capacity)
  let size = ref 0

  let clear() =
    Hashtbl.clear cache;
    size := 0

(* The collection function is called when we reach twice original capacity
 * in size. When the collection is triggered, we only keep the most recent
 * object.
 * So before collection: size = 2 * capacity
 * After collection: size = capacity (with the most recent objects)
 *)
  let collect() =
    if !size < 2 * Config.capacity then () else
    let l = ref [] in
    Hashtbl.iter begin fun key (freq, v) ->
      l := (key, !freq, v) :: !l
    end cache;
    Hashtbl.clear cache;
    l := List.sort (fun (_, x, _) (_, y, _) -> y - x) !l;
    let i = ref 0 in
    while !i < Config.capacity do
      match !l with
      | [] -> i := Config.capacity
      | (k, freq, v) :: rl ->
          Hashtbl.replace cache k (ref 0, v);
          l := rl;
          incr i;
    done;
    size := Config.capacity;
    ()

  let add x y =
    collect();
    try
      let freq, y' = Hashtbl.find cache x in
      incr freq;
      if y' == y
      then ()
      else Hashtbl.replace cache x (freq, y)
    with Not_found ->
      incr size;
      let elt = ref 0, y in
      Hashtbl.replace cache x elt;
      ()

  let find x =
    let freq, value = Hashtbl.find cache x in
    incr freq;
    value

  let get x = try Some (find x) with Not_found -> None

  let remove x =
    if Hashtbl.mem cache x
    then decr size;
    Hashtbl.remove cache x

end

(*****************************************************************************)
(* An ordered cache keeps the most recently used objects *)
(*****************************************************************************)

module OrderedCache (Key : sig type t end) (Config:ConfigType):
  CacheType with type key := Key.t and type value := Config.value = struct

  let (cache: (Key.t, Config.value) Hashtbl.t) =
    Hashtbl.create Config.capacity

  let queue = Queue.create()
  let size = ref 0

  let clear() =
    Hashtbl.clear cache;
    size := 0;
    Queue.clear queue;
    ()

  let add x y =
    if !size < Config.capacity
    then begin
      incr size;
      let () = Queue.push x queue in
      ()
    end
    else begin
      let elt = Queue.pop queue in
      Hashtbl.remove cache elt;
      Queue.push x queue;
      Hashtbl.replace cache x y
    end

  let find x = Hashtbl.find cache x
  let get x = try Some (find x) with Not_found -> None

  let remove x =
    try
      if Hashtbl.mem cache x
      then decr size;
      Hashtbl.remove cache x;
    with Not_found -> ()

end

(*****************************************************************************)
(* Every time we create a new cache, a function that knows how to clear the
 * cache is registered in the "invalidate_callback_list" global.
 *)
(*****************************************************************************)

let invalidate_callback_list = ref []
let invalidate_caches () =
  List.iter !invalidate_callback_list begin fun callback -> callback() end

module LocalCache (UserKeyType : UserKeyType) (Value : Value.Type) = struct

  type key = UserKeyType.t
  type value = Value.t

  module ConfValue = struct
    type value = Value.t
    let capacity = 1000
  end

  (* Young values cache *)
  module L1 = OrderedCache (UserKeyType) (ConfValue)
  (* Frequent values cache *)
  module L2 = FreqCache (UserKeyType) (ConfValue)

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
         result
      )
    | Some v as result ->
      L2.add x v;
      result

  let remove x =
    L1.remove x;
    L2.remove x

  let clear () =
    L1.clear();
    L2.clear()

  let () =
    invalidate_callback_list := begin fun () ->
      L1.clear();
      L2.clear()
    end :: !invalidate_callback_list

end

(*****************************************************************************)
(* A functor returning an implementation of the S module with caching.
 * We need to avoid constantly deserializing types, because it costs us too
 * much time. The caches keep a deserialized version of the types.
 *)
(*****************************************************************************)
module WithCache (UserKeyType : UserKeyType) (Value:Value.Type) = struct

  module Direct = NoCache (UserKeyType) (Value)

  type key = Direct.key
  type t = Direct.t

  module KeySet = Direct.KeySet
  module KeyMap = Direct.KeyMap

  module Cache = LocalCache (UserKeyType) (Value)

  let add x y =
    Direct.add x y;
    Cache.add x y

  let write_through x y =
    (* Note that we do not need to do any cache invalidation here because
     * Direct.add is a no-op if the key already exists. *)
    Direct.add x y

  let get x =
    match Cache.get x with
    | None ->
      (match Direct.get x with
       | None -> None
       | Some v as result ->
         Cache.add x v;
         result
      )
    | Some v as result ->
      result

  (* We don't cache old objects, they are not accessed often enough. *)
  let get_old = Direct.get_old
  let get_old_batch = Direct.get_old_batch

  let find_unsafe x =
    match get x with
    | None -> raise Not_found
    | Some x -> x

  let mem x =
    match get x with
    | None -> false
    | Some _ -> true

  let get_batch keys =
    KeySet.fold begin fun key acc ->
      KeyMap.add key (get key) acc
    end keys KeyMap.empty

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
    invalidate_callback_list := begin fun () ->
      Cache.clear()
    end :: !invalidate_callback_list

  let remove_old_batch = Direct.remove_old_batch

  module LocalChanges = struct

    let push_stack () =
      Direct.LocalChanges.push_stack ();
      Cache.clear ()

    let pop_stack () =
      Direct.LocalChanges.pop_stack ();
      Cache.clear ()

    let revert_batch keys =
      Direct.LocalChanges.revert_batch keys;
      KeySet.iter Cache.remove keys

    let commit_batch keys =
      Direct.LocalChanges.commit_batch keys;
      KeySet.iter Cache.remove keys

    let revert_all () =
      Direct.LocalChanges.revert_all ();
      Cache.clear ()

    let commit_all () =
      Direct.LocalChanges.commit_all ();
      Cache.clear ()
  end
end
