(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val iter_all : unit Lwt.t list -> unit Lwt.t

val iter_all_result : (unit, 'e) result Lwt.t list -> (unit, 'e) result Lwt.t

val all : 'a Lwt.t list -> 'a list Lwt.t

val both : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

val both_result : ('a, 'e) result Lwt.t -> ('b, 'e) result Lwt.t -> ('a * 'b, 'e) result Lwt.t

val output_graph : Lwt_io.output_channel -> ('a -> string) -> ('a * 'a list) list -> unit Lwt.t

val fold_result_s :
  f:('acc -> 'b -> ('acc, 'c) result Lwt.t) -> init:'acc -> 'b list -> ('acc, 'c) result Lwt.t
