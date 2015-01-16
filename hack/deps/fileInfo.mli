(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* This module defines the data structured used to describe the content of
 * a file.
 * The parser constructs FileInfo.t structs, that contain names and positions
 * plus some extra info required for the build.
 * After the names have been checked (Naming.make_env), we "simplify" the
 * struct and only keep the names defined in the files we know about.
 *)
(*****************************************************************************)

open Utils

(*****************************************************************************)
(* The record produced by the parsing phase. *)
(*****************************************************************************)

type id = Pos.t * string

type t = {
  funs : id list;
  classes : id list;
  typedefs : id list;
  consts : id list;
  comments : (Pos.t * string) list;
  consider_names_just_for_autoload: bool;
}

(*****************************************************************************)
(* The simplified record used after parsing. *)
(*****************************************************************************)

type names = {
  n_funs    : SSet.t;
  n_classes : SSet.t;
  n_types   : SSet.t;
  n_consts  : SSet.t;
}

type fast = names Relative_path.Map.t

val empty_names: names

(*****************************************************************************)
(* Functions simplifying the file information. *)
(*****************************************************************************)

val simplify: t -> names
val merge_names: names -> names -> names
val simplify_fast: t Relative_path.Map.t -> names Relative_path.Map.t
