(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

type config = {
  global_size: int;
  heap_size : int;
}

let default_config =
  let gig = 1024 * 1024 * 1024 in
  {global_size = gig; heap_size = 20 * gig}

(*****************************************************************************)
(* Initializes the shared memory. Must be called before forking. *)
(*****************************************************************************)
external hh_shared_init: global_size:int -> heap_size:int -> unit
= "hh_shared_init"

let init config =
  hh_shared_init
    ~global_size:config.global_size
    ~heap_size:config.heap_size

(*****************************************************************************)
(* The shared memory garbage collector. It must be called every time we
 * free data (cf hh_shared.c for the underlying C implementation).
 *)
(*****************************************************************************)
external hh_collect: unit -> unit = "hh_collect"

(*****************************************************************************)
(* Serializes the shared memory and writes it to a file *)
(*****************************************************************************)
external save: string -> unit = "hh_save"

(*****************************************************************************)
(* Loads the shared memory by reading from a file *)
(*****************************************************************************)
external load: string -> unit = "hh_load"

(*****************************************************************************)
(* The size of the dynamically allocated shared memory section *)
(*****************************************************************************)
external heap_size: unit -> int = "hh_heap_size"

(*****************************************************************************)
(* The number of used slots in our hashtable *)
(*****************************************************************************)
external hash_used_slots : unit -> int = "hh_hash_used_slots"

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

let init_done () =
  hh_init_done ();
  EventLogger.sharedmem_init_done (heap_size ())

type table_stats = {
  used_slots : int;
  slots : int;
}

let dep_stats () = {
  used_slots = dep_used_slots ();
  slots = dep_slots ();
}

let hash_stats () = {
  used_slots = hash_used_slots ();
  slots = hash_slots ();
}

let collect () =
  let old_size = heap_size () in
  hh_collect ();
  let new_size = heap_size () in
  EventLogger.sharedmem_gc old_size new_size

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
  val to_new   : old -> t

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

  let to_new x = 
    let module S = String in
    S.sub x (S.length old_prefix) (S.length x - S.length old_prefix)

  let md5 = Digest.string
  let md5_old = Digest.string

end

(*****************************************************************************)
(* Functor for serialized values (to make sure we don't mix strings
 * with serialized strings).
 *)
(*****************************************************************************)

module Serial: functor(Value:Value.Type) -> sig
  type t

  val make : Value.t -> t
  val get  : t -> Value.t

end = functor(Value:Value.Type) -> struct
  type t = string

  let make x = Marshal.to_string x []
  let get x  = Marshal.from_string x 0
end


(*****************************************************************************)
(* Raw interface to shared memory (cf hh_shared.c for the underlying
 * representation).
 *)
(*****************************************************************************)
module Raw (Key: Key) (Value: sig type t end) = struct

  external hh_add         : Key.md5 -> Value.t -> unit = "hh_add"
  external hh_mem         : Key.md5 -> bool            = "hh_mem"
  external hh_get         : Key.md5 -> Value.t         = "hh_get"
  external hh_remove      : Key.md5 -> unit            = "hh_remove"
  external hh_move        : Key.md5 -> Key.md5 -> unit = "hh_move"
      
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

end = functor (Key : Key) -> functor (Value : Value.Type) -> struct

  module Data = Serial(Value)
  module Raw = Raw (Key) (Data)

  let add key value = Raw.hh_add (Key.md5 key) (Data.make value)
  let mem key = Raw.hh_mem (Key.md5 key)

  let get key =
    let key = Key.md5 key in
    if Raw.hh_mem key
    then Some (Data.get (Raw.hh_get key))
    else None

  let find_unsafe key = 
    match get key with 
    | None -> raise Not_found
    | Some x -> x

  let remove key =
    let key = Key.md5 key in
    if Raw.hh_mem key
    then begin 
      Raw.hh_remove key;
      assert (not (Raw.hh_mem key));
    end
    else ()

  let oldify key =
    if mem key
    then
      let old_key = Key.to_old key in
      Raw.hh_move (Key.md5 key) (Key.md5_old old_key)
    else ()
end

(* Same as new, but for old values *)
module Old : functor (Key : Key) -> functor (Value : Value.Type) -> sig

  val get         : Key.old -> Value.t option
  val remove      : Key.old -> unit
  val mem         : Key.old -> bool

  (* Takes an old value and moves it back to a "new" one
   * (useful for auto-complete).
   *)
  val revive      : Key.old -> unit

end = functor (Key : Key) -> functor (Value: Value.Type) -> struct

  module Data = Serial(Value)
  module Raw = Raw (Key) (Data)

  let get key =
    let key = Key.md5_old key in
    if Raw.hh_mem key
    then Some (Data.get (Raw.hh_get key))
    else None

  let mem key = Raw.hh_mem (Key.md5_old key)

  let remove key =
    if mem key
    then Raw.hh_remove (Key.md5_old key)

  let revive key =
    if mem key
    then
      let new_key = Key.to_new key in
      let new_key = Key.md5 new_key in
      let old_key = Key.md5_old key in
      if Raw.hh_mem new_key
      then Raw.hh_remove new_key;
      Raw.hh_move old_key new_key
end

(*****************************************************************************)
(* The signature of what we are actually going to expose to the user *)
(*****************************************************************************)

module type S = sig
  type key
  type t
  module KeySet : Set.S with type elt = key
  module KeyMap : MapSig with type key = key

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
  module Old = Old (Key) (Value)
  module KeySet = Set.Make (UserKeyType)
  module KeyMap = MyMap (UserKeyType)

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
      
module FreqCache (Key : Key) (Config:ConfigType) :
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

module OrderedCache (Key : Key) (Config:ConfigType):
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
  List.iter begin fun callback -> callback() end !invalidate_callback_list

(*****************************************************************************)
(* A functor returning an implementation of the S module with caching.
 * We need to avoid constantly deserializing types, because it costs us too
 * much time. The caches keep a deserialized version of the types.
 *)
(*****************************************************************************)        
module WithCache (UserKeyType : UserKeyType) (Value:Value.Type) = struct

  type key = UserKeyType.t
  type t = Value.t

  module ConfValue = struct
    type value = Value.t
    let capacity = 1000
  end

  module Key = KeyFunctor (UserKeyType)

  (* Young values cache *)
  module L1 = OrderedCache (Key) (ConfValue)
  (* Frequent values cache *)
  module L2 = FreqCache (Key) (ConfValue)

  module New = New (Key) (Value)
  module Old = Old (Key) (Value)
  module KeySet = Set.Make (UserKeyType)
  module KeyMap = MyMap (UserKeyType)

  module Direct = NoCache (UserKeyType) (Value)

  let add x y = 
    let x = Key.make Value.prefix x in
    L1.add x y;
    L2.add x y;
    New.add x y

  let get x = 
    let x = Key.make Value.prefix x in
    match L1.get x with
    | None ->
        (match L2.get x with
        | None ->
            (match New.get x with
            | None ->
                None
            | Some v as result ->
                L1.add x v;
                L2.add x v;
                result
            )
        | Some v as result ->
            L1.add x v;
            result
        )
    | Some v as result ->
        L2.add x v;
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
    
  let remove x = 
    let x = Key.make Value.prefix x in
    L1.remove x;
    L2.remove x;
    New.remove x

  let get_batch keys =
    KeySet.fold begin fun key acc ->
      KeyMap.add key (get key) acc
    end keys KeyMap.empty

  let oldify_batch keys =
    Direct.oldify_batch keys;
    KeySet.iter begin fun key ->
      let key = Key.make Value.prefix key in
      L1.remove key;
      L2.remove key;
    end keys

  let revive_batch keys =
    Direct.revive_batch keys;
    KeySet.iter begin fun x ->
      let x = Key.make Value.prefix x in
      L1.remove x;
      L2.remove x;
    end keys

  let remove_batch xs = KeySet.iter remove xs

  let () =
    invalidate_callback_list := begin fun () ->
      L1.clear();
      L2.clear();
    end :: !invalidate_callback_list

  let remove_old x = Old.remove (Key.make_old Value.prefix x)
  let remove_old_batch = KeySet.iter remove_old

end
