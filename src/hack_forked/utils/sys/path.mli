(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = private string

val dummy_path : t

val make : string -> t

val make_unsafe : string -> t

val to_string : t -> string

val file_exists : t -> bool

val is_directory : t -> bool

val is_ancestor : prefix:t -> t -> bool

val compare : t -> t -> int

val concat : t -> string -> t

val dirname : t -> t

val basename : t -> string

val output : out_channel -> t -> unit

val parent : t -> t

val cat : t -> string
