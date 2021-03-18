(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type libdir =
  | Flowlib of Path.t
  | Prelude of Path.t

val libdir : no_flowlib:bool -> Path.t -> libdir

val path_of_libdir : libdir -> Path.t

val extract : libdir -> unit
