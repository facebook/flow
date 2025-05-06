(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type userland = string [@@deriving show, ord]

let userland x = x

let unwrap_userland x = x

module Key = struct
  type t = Userland of userland [@@deriving show, ord]
end

let userland_specifier x = Key.Userland x

module Map = WrappedMap.Make (Key)
module Set = Flow_set.Make (Key)
include Key
