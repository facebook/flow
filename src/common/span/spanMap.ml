(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module SpanMap = MyMap.Make (struct
  type t = Loc.t
  let compare l0 l1 = Loc.span_compare l1 l0
end)

include SpanMap
