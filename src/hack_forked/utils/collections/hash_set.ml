(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

external ( .!() ) : 'a array -> int -> 'a = "%array_unsafe_get"

external ( .!()<- ) : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

let rec power_2_above x n =
  if x >= n then
    x
  else if x * 2 > Sys.max_array_length then
    x
  else
    power_2_above (x * 2) n

module type S = sig
  type t

  type elt

  val create : int -> t

  val clear : t -> unit

  val reset : t -> unit

  val add : t -> elt -> elt
end

module Make (H : Hashtbl.HashedType) : S with type elt = H.t = struct
  type elt = H.t

  type t = {
    mutable length: int;
    mutable data: H.t list array;
    initial_size: int;
  }

  let create initial_size =
    let s = power_2_above 16 initial_size in
    { initial_size = s; length = 0; data = Array.make s [] }

  let clear h =
    h.length <- 0;
    let len = Array.length h.data in
    for i = 0 to len - 1 do
      h.data.!(i) <- []
    done

  let reset h =
    h.length <- 0;
    h.data <- Array.make h.initial_size []

  let key_index h key = H.hash key land (Array.length h.data - 1)

  let resize h =
    let odata = h.data in
    let osize = Array.length odata in
    let nsize = osize * 2 in
    if nsize < Sys.max_array_length then begin
      let ndata = Array.make nsize [] in
      h.data <- ndata;
      let rec insert_bucket = function
        | [] -> ()
        | key :: rest ->
          let nidx = key_index h key in
          ndata.!(nidx) <- key :: ndata.!(nidx);
          insert_bucket rest
      in
      for i = 0 to osize - 1 do
        insert_bucket odata.!(i)
      done
    end

  let add h key =
    let i = key_index h key in
    let bucket = h.data.!(i) in
    let rec find b =
      match b with
      | [] ->
        h.data.!(i) <- key :: bucket;
        h.length <- h.length + 1;
        if h.length > Array.length h.data lsl 1 then resize h;
        key
      | x :: l ->
        if H.equal x key then
          x
        else
          find l
    in
    find h.data.!(i)
end
