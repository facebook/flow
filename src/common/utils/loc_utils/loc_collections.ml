(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocSet = Loc_sig.LocS.LSet
module LocMap = Loc_sig.LocS.LMap
module ALocSet = Loc_sig.ALocS.LSet
module ALocMap = Loc_sig.ALocS.LMap

module ALocIDS = struct
  type t = ALoc.id

  let compare (t1 : t) (t2 : t) = ALoc.quick_compare (t1 :> ALoc.t) (t2 :> ALoc.t)
end

module ALocIDSet = Set.Make (ALocIDS)
module ALocIDMap = Map.Make (ALocIDS)
