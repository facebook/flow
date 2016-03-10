(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Typing_defs

(* The following classes are used to make sure we make no typing
 * mistake when interacting with the database. The database knows
 * how to associate a string to a string. We need to deserialize
 * the string and make sure the type is correct. By using these
 * modules, the places where there could be a typing mistake are
 * very well isolated.
*)

(* Module used to represent serialized classes *)
module Class = struct
  type t = Typing_defs.class_type
  let prefix = Prefix.make()
end

(* a function type *)
module Fun = struct
  type t = decl Typing_defs.fun_type
  let prefix = Prefix.make()
end

module Typedef = struct
  type t = Typing_defs.typedef_type
  let prefix = Prefix.make()
end

module GConst = struct
  type t = decl ty
  let prefix = Prefix.make()
end

module Funs = SharedMem.WithCache (StringKey) (Fun)
module Classes = SharedMem.WithCache (StringKey) (Class)
module Typedefs = SharedMem.WithCache (StringKey) (Typedef)
module GConsts = SharedMem.WithCache (StringKey) (GConst)
