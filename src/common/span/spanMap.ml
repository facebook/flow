(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module SpanMap = MyMap.Make (struct
  type t = Loc.t

  let compare l0 l1 = Loc.span_compare l1 l0
end)

include SpanMap
