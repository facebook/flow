(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Loc_collections.LocMap

type names = string LocMap.t

type t = { names: names }

let default = { names = LocMap.empty }
