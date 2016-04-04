(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

type t
external hh_shared_list_create : int -> t = "hh_shared_list_create"
external hh_shared_list_append : t -> string -> unit = "hh_shared_list_append"
external hh_shared_list_reset : t -> unit = "hh_shared_list_reset"
external hh_shared_list_get : t -> string list = "hh_shared_list_get"

module Make(Config : sig
  type element
  val max_size : int (* in bytes *)
end) : sig
  type elem = Config.element
  val append : elem -> unit
  val get : unit -> elem list
  val reset : unit -> unit
end = struct
  type elem = Config.element

  let xs = hh_shared_list_create Config.max_size

  let append v = hh_shared_list_append xs (Marshal.to_string v [])

  let reset () = hh_shared_list_reset xs

  let get () =
    let slist = hh_shared_list_get xs in
    List.map slist (fun x -> Marshal.from_string x 0)
end
