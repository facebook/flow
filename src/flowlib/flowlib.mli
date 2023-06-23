(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val contents_list : no_flowlib:bool -> (string * string) list

type libdir =
  | Flowlib of File_path.t
  | Prelude of File_path.t

val libdir : no_flowlib:bool -> File_path.t -> libdir

val path_of_libdir : libdir -> File_path.t

val extract : libdir -> unit
