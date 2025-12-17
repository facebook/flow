(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = { buffer: Buffer.t }

val create : unit -> t

val add_string : string -> t -> t

val add_identifier : string -> t -> t

val add_newline : t -> t

val add_space : int -> t -> t

val contents : t -> string
