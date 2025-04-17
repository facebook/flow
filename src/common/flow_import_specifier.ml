(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Key = struct
  type t = Userland of string [@@deriving show, ord]
end

module Map = WrappedMap.Make (Key)
module Set = Flow_set.Make (Key)
include Key
