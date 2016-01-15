(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


open Utils

module ClassHeap = SharedMem.NoCache (String) (struct
  type t = Nast.class_
  let prefix = Prefix.make()
end)

module FunHeap = SharedMem.NoCache (String) (struct
  type t = Nast.fun_
  let prefix = Prefix.make()
end)

module TypedefHeap = SharedMem.NoCache (String) (struct
  type t = Nast.typedef
  let prefix = Prefix.make()
end)

module ConstHeap = SharedMem.NoCache (String) (struct
  type t = Nast.gconst
  let prefix = Prefix.make()
end)

(* Mapping the canonical name (lower case form) to the actual name *)
module type CanonHeap =
  SharedMem.S with type t = string
               and type key = string
               and module KeySet = Set.Make (String)

module ClassCanonHeap : CanonHeap = SharedMem.NoCache (String) (struct
  type t = string
  let prefix = Prefix.make()
end)

module FunCanonHeap : CanonHeap = SharedMem.NoCache (String) (struct
  type t = string
  let prefix = Prefix.make()
end)

(* We want to keep the positions of names that have been
 * replaced by identifiers.
 *)
module type IdHeap =
  SharedMem.S with type t = Pos.t * Ident.t
               and type key = string
               and module KeySet = Set.Make (String)

module ClassIdHeap : IdHeap = SharedMem.NoCache (String) (struct
  type t = Pos.t * Ident.t
  let prefix = Prefix.make()
end)

module FunIdHeap : IdHeap = SharedMem.NoCache (String) (struct
  type t = Pos.t * Ident.t
  let prefix = Prefix.make()
end)

module TypedefIdHeap : IdHeap = SharedMem.NoCache (String) (struct
  type t = Pos.t * Ident.t
  let prefix = Prefix.make()
end)

module ConstIdHeap : IdHeap = SharedMem.NoCache (String) (struct
  type t = Pos.t * Ident.t
  let prefix = Prefix.make()
end)
