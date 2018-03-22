(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Unexpected_eof
exception Invalid_base64 of char

module type S = sig
  val encode: Buffer.t -> int -> unit
  val decode: char Stream.t -> int
end

module Base64 : S
