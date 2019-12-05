(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

type t = {
  mutable init_parsing_heap_size: int;
  mutable init_heap_size: int;
  mutable max_heap_size: int;
  gc_stat: Gc.stat;
}

val stats : t

val get_stats : unit -> t

val update_max_heap_size : int -> unit

val to_json : t -> Hh_json.json
