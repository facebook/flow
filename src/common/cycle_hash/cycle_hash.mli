(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type cx

type node

type read_hash = unit -> int64

type write_hash = int64 -> unit

type edge = node -> unit

type dep_edge = read_hash -> unit

type visit = edge -> dep_edge -> unit

val create_cx : unit -> cx

val create_node : visit -> read_hash -> write_hash -> node

val root : cx -> node -> unit

val read_hash : node -> int64
