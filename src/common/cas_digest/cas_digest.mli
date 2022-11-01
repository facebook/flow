(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  sha1: string;
  bytelen: int;
}

val sha1_of_sha1hex : string -> string

val sha1hex_of_sha1 : string -> string
