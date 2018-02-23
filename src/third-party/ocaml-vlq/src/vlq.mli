(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  val encode: Buffer.t -> int -> unit
end

module Base64 : S
