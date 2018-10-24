(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool

  module LMap: (MyMap.S with type key = t)
  module LSet: (Set.S with type elt = t)
end

module LocS = struct
  type t = Loc.t
  let compare = Loc.compare
  let equal = Loc.equal

  module LMap = Utils_js.LocMap
  module LSet = Utils_js.LocSet
end

module ALocS = struct
  type t = ALoc.t
  let compare = ALoc.compare
  let equal = ALoc.equal

  module LMap = Utils_js.ALocMap
  module LSet = Utils_js.ALocSet
end
