(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(* ClassStatus is something that we want to be able to access very quickly.
 * Workers in the "declaration" phase need to be able to know if their parent
 * class has already been declared or not.
 * They will lookup the status of a class (declared, error, or todo) very
 * frequently.
 * That's why we are dissociating ClassStatus from ClassHeap.
 * If the status of the class was in ClassHeap, we would have to deserialize
 * a tree every time a parent class has been successfully declared.
 * It would be horribly slow.
 * Using a cache is not an option since the class status are updated
 * concurrently.
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
