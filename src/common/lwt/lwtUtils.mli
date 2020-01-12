(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val iter_all : unit Lwt.t list -> unit Lwt.t

val all : 'a Lwt.t list -> 'a list Lwt.t

val output_graph : Lwt_io.output_channel -> ('a -> string) -> ('a * 'a list) list -> unit Lwt.t
