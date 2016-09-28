(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = {time : float; content : string}

(* 1-based position is used here *)
type content_pos = {line : int; column : int}

type content_range = {
  st : content_pos;
  ed : content_pos;
}

type code_edit = {
  range : content_range option;
  text : string;
}

val of_content : content:string -> t

val get_content : t -> string

val edit_file : t -> code_edit list -> (t, string) Result.t

val edit_file_unsafe : t -> code_edit list -> t
