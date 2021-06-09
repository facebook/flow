(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The Hash_queue data structure contains a hash table for O(1) lookup as well
 * as a doubly linked list, which we use to keep track of recency of use. When
 * a file is created or accessed, we move it to the front of the list. When we
 * exceed the capacity of the cache, entries at the back of the list are
 * dropped. *)
module Cache = Core_kernel.Hash_queue.Make (struct
  type t = File_key.t

  let hash = Hashtbl.hash

  let sexp_of_t k = Core_kernel.Sexp.Atom (File_key.to_string k)

  let compare = File_key.compare
end)

(* Files which form a dependency cycle will share the same component context.
 * We will create a component context on demand for the first file in a given
 * component, then re-use it for any future files in the cycle.
 *
 * We keep track of a reference count, so we can forget about a cached
 * component context once every file referencing it has been removed. *)
type cached_ccx = {
  leader: File_key.t;
  ccx: Context.component_t;
  mutable refcount: int;
}

(* Each cached file holds a reference to its associated cached component
 * context so that we can decrement its reference count. *)
type cached_file = {
  file: Type_sig_merge.file;
  cached_ccx: cached_ccx;
}

type t = {
  files: cached_file Cache.t;
  ccxs: (File_key.t, cached_ccx) Hashtbl.t;
  mutable size: int;
  capacity: int;
}

let create ~capacity =
  let files = Cache.create () in
  let ccxs = Hashtbl.create 0 in
  { files; ccxs; size = 0; capacity }

(* When a file is dropped from the cache, we decrement the refcount on its
 * cached component context. Once no more files reference a given component
 * context, we remove it from the cache. *)
let release_ccx cache cached_ccx =
  let refcount = cached_ccx.refcount in
  if refcount = 1 then
    Hashtbl.remove cache.ccxs cached_ccx.leader
  else
    cached_ccx.refcount <- pred refcount

(* Files are added to the front of the cache and moved to the front when
 * accessed, so the least recently used file(s) are at the back. *)
let drop_least_recently_used cache =
  match Cache.dequeue_back cache.files with
  | None -> ()
  | Some { cached_ccx; _ } ->
    release_ccx cache cached_ccx;
    cache.size <- pred cache.size

(* Files in a cycle share the same component context, so if we are creating a
 * file in a cycle with an already cached file, its component context will
 * also be cached. *)
let find_or_create_ccx cache ~find_leader ~master_cx file_key =
  let leader = find_leader file_key in
  match Hashtbl.find_opt cache.ccxs leader with
  | Some cached_ccx ->
    cached_ccx.refcount <- succ cached_ccx.refcount;
    cached_ccx
  | None ->
    let ccx = Context.make_ccx master_cx in
    let cached_ccx = { leader; ccx; refcount = 1 } in
    Hashtbl.add cache.ccxs leader cached_ccx;
    cached_ccx

(* If a file for the given file key is already in the cache, we move it to the
 * front of the queue to indicate that it was recently used. Otherwise, we
 * add a newly created file at the front of the queue. *)
let find_or_create cache ~find_leader ~master_cx ~create_file file_key =
  match Cache.lookup_and_move_to_front cache.files file_key with
  | Some { file; _ } -> file
  | None ->
    let cached_ccx = find_or_create_ccx cache ~find_leader ~master_cx file_key in
    let file = create_file file_key cached_ccx.ccx in
    if cache.size = cache.capacity then drop_least_recently_used cache;
    Cache.enqueue_front_exn cache.files file_key { file; cached_ccx };
    cache.size <- succ cache.size;
    file

let remove cache file_key =
  match Cache.lookup cache.files file_key with
  | None -> ()
  | Some { cached_ccx; _ } ->
    Cache.remove_exn cache.files file_key;
    release_ccx cache cached_ccx;
    cache.size <- pred cache.size

(* Clearing the cache does not need to worry about the reference counts for
 * cached component contexts, since all cached files are also cleared. *)
let clear cache =
  Cache.clear cache.files;
  Hashtbl.clear cache.ccxs;
  cache.size <- 0
