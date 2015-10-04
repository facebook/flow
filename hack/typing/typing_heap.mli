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
open Typing_defs

module Class : sig type t = class_type val prefix : Prefix.t end
module Fun : sig type t = decl fun_type val prefix : Prefix.t end
module Typedef :
  sig
    type visibility = Public | Private
    type tdef =
        visibility * tparam list * decl ty option *
        decl ty * Pos.t
    type tdef_or_error = Error | Ok of tdef
    type t = tdef_or_error
    val prefix : Prefix.t
  end
module GConst : sig type t = decl ty val prefix : Prefix.t end

module Funs : module type of SharedMem.WithCache (String) (Fun)
module Classes : module type of SharedMem.WithCache (String) (Class)
module Typedefs : module type of SharedMem.WithCache (String) (Typedef)
module GConsts : module type of SharedMem.WithCache (String) (GConst)

module FuncTerminality : sig
  val get_fun : string -> Fun.t option
  val get_static_meth : string -> string -> Fun.t option
  val raise_exit_if_terminal : Fun.t option -> unit
end
