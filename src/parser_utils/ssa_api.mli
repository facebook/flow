(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module LocMap = Utils_js.LocMap

type values = Loc.t list LocMap.t

val uninitialized: Loc.t (* TODO: replace this with something more robust *)
val values_of_loc: values -> Loc.t -> Loc.t list
val print_values: values -> string
