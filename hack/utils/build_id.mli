(**
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
*)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

external get_build_revision : unit -> string = "hh_get_build_revision"
external get_build_commit_time : unit -> int = "hh_get_build_commit_time"
external get_build_commit_time_string : unit -> string
  = "hh_get_build_commit_time_string"
external get_build_major : unit -> int = "hh_get_build_major"
external get_build_minor : unit -> int = "hh_get_build_minor"
val build_revision : string
val build_id_ohai : string
val build_commit_time : int
val build_major_version : int
val build_minor_version : int
val build_api_version : int
val build_mode : string
val is_build_optimized : bool
