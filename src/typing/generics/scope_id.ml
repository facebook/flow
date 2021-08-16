(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ScopeInternal : sig
  type t

  val new_id : unit -> t

  module ScopeSet : Flow_set.S with type elt = t

  module ScopeMap : WrappedMap.S with type key = t
end = struct
  type t = int

  let new_id = Reason.mk_id

  module ScopeSet = Flow_set.Make (IntKey)
  module ScopeMap = WrappedMap.Make (IntKey)
end

include ScopeInternal
