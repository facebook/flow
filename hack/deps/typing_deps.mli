(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Dep :
  sig
    type variant =
      | GConst of string
      | GConstName of string
      | Const of string * string
      | Class of string
      | Fun of string
      | FunName of string
      | Prop of string * string
      | SProp of string * string
      | Method of string * string
      | SMethod of string * string
      | Cstr of string
      | Extends of string
    type t
    val make : variant -> t
    val is_class : t -> bool
    val extends_of_class : t -> t
    val compare : t -> t -> int
  end

module DepSet : module type of Set.Make (Dep)

val trace : bool ref
val add_idep : Dep.variant option -> Dep.variant -> unit
val get_ideps_from_hash : Dep.t -> DepSet.t
val get_ideps : Dep.variant -> DepSet.t
val get_bazooka : Dep.variant -> DepSet.t
val marshal : out_channel -> unit
val unmarshal : in_channel -> unit
val get_files : DepSet.t -> Relative_path.Set.t
val update_files : FileInfo.t Relative_path.Map.t -> unit
