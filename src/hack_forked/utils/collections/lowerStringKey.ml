(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = string

let compare (x : t) (y : t) = String.compare (String.lowercase_ascii x) (String.lowercase_ascii y)
