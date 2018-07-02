(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Utils_js.LocMap

type names = string LocMap.t
type t = {
  names: names
}

let default = {
  names = LocMap.empty
}
