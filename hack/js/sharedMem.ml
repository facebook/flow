(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* The local heap
 *)
(*****************************************************************************)
open Utils

type t = string list

(*****************************************************************************)
(* If there is no call to "make", it means that we are only dealing with
* one process, no need to create several data nodes, just use a local
* hashtable. (this is useful for hh_single_type_check for example).
*)
(*****************************************************************************)
let is_local = ref true
let (local_h: (string, string) Hashtbl.t) = Hashtbl.create 7

let find_unsafe x =
  Hashtbl.find local_h x

let get x = 
  try Some (find_unsafe x) with Not_found -> None
    
let mem x = get x <> None

let add x data = 
  Hashtbl.replace local_h x data

let remove x = 
  Hashtbl.remove local_h x

let remove_batch x =
  SSet.iter begin fun x ->
    Hashtbl.remove local_h x
  end x

(*****************************************************************************)
(* The signature of what we are actually going to expose to the user *)
(*****************************************************************************)        
module type S = sig
  type t
  type key
  module KeySet : Set.S
  module KeyMap : MapSig

  val add: key -> t -> unit
  val get: key -> t option
  val get_old: key -> t option
  val find_unsafe: key -> t
  val get_batch: KeySet.t -> t option KeyMap.t
  val remove: key -> unit
  val remove_batch: KeySet.t -> unit
  val mem: key -> bool
  val invalidate_cache: unit -> unit
  val oldify: KeySet.t -> unit
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
(* NoCache means no caching, read and write directly *)
(*****************************************************************************)        
module type NoCache_type =
  functor (UserKeyType : UserKeyType) ->
  functor (Value : Value.Type) ->
  S with type t = Value.t
    and type key = UserKeyType.t
    and module KeySet = Set.Make (UserKeyType)
    and module KeyMap = MyMap (UserKeyType)

module NoCache: NoCache_type =
  functor (UserKeyType : UserKeyType) ->
  functor (Value : Value.Type) -> struct

  module KeySet = Set.Make (UserKeyType)
  module KeyMap = MyMap (UserKeyType)

  type key = UserKeyType.t
  type t = Value.t

  let add x y =
    let x = Prefix.make_key Value.prefix (UserKeyType.to_string x) in
    add x (Obj.magic y)

  let find_unsafe x =
    let x = Prefix.make_key Value.prefix (UserKeyType.to_string x) in
    let data = find_unsafe x in
    (Obj.magic data)

  let get x =
    try Some (find_unsafe x) with Not_found -> None

  let oldify _ = raise Exit
  let get_old _ = None

  let remove x =
    let x = Prefix.make_key Value.prefix (UserKeyType.to_string x) in
    remove x

  let make_key_set xs =
    KeySet.fold begin fun x acc ->
      SSet.add (Prefix.make_key Value.prefix (UserKeyType.to_string x)) acc
    end xs SSet.empty
    
  let remove_batch xs = remove_batch (make_key_set xs)

  let get_batch xs =
    let acc = ref KeyMap.empty in
    KeySet.iter begin fun x ->
      let key = Prefix.make_key Value.prefix (UserKeyType.to_string x) in
      try acc := KeyMap.add x (Obj.magic (Hashtbl.find local_h key)) !acc
      with Not_found -> ()
    end xs;
    !acc

  let mem x =
    try ignore (find_unsafe x); true with Not_found -> false

  let invalidate_cache () =
    (* Since there is no cache, there is not much to do here ... *)
    ()

end

(*****************************************************************************)
(* Same thing but with 4 layers of cache ... Useful for type-checking        *)
(*****************************************************************************)        
module WithCache = NoCache
