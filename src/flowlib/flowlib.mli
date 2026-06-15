(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val contents_list : no_flowlib:bool -> (string * string) list

type builtin_lib =
  | Builtin_flowlib
  | Builtin_prelude
  | Builtin_tslib

val builtin_lib_of_no_flowlib : no_flowlib:bool -> builtin_lib

type libdir =
  | Flowlib of File_path.t
  | Prelude of File_path.t
  | Tslib of File_path.t

val libdir : builtin_lib:builtin_lib -> File_path.t -> libdir

val path_of_libdir : libdir -> File_path.t

val extract : libdir -> unit

val extract_if_missing : libdir -> unit

val extract_if_missing_or_exit : Files.lib_dir option -> unit
