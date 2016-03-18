(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Reordered_argument_map (S : MyMap.S) = struct
  include S
  let add m ~key ~data = add key data m
  let filter m ~f = filter f m
  let fold m ~init ~f = fold f m init
  let get m k = get k m
  let iter m ~f = iter f m
  let map m ~f = map f m
end

module Reordered_argument_set (S : Set.S) = struct
  include S
  let add s v = add v s
  let filter s ~f = filter f s
  let fold s ~init ~f = fold f s init
  let iter s ~f = iter f s
  let mem s v = mem v s
end

module SSet = Reordered_argument_set(SSet)
module SMap = Reordered_argument_map(SMap)
