(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ide_api_types

type t = {time : float; content : string}

val of_content : content:string -> t

val get_content : t -> string

val edit_file : t -> text_edit list -> (t, string) Result.t

val edit_file_unsafe : t -> text_edit list -> t
