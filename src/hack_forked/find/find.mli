(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val make_next_files :
  ?name:string -> ?filter:(string -> bool) -> ?others:Path.t list -> Path.t -> unit -> string list

val find :
  ?max_depth:int -> ?filter:(string -> bool) -> ?file_only:bool -> Path.t list -> string list

val find_with_name : ?max_depth:int -> ?file_only:bool -> Path.t list -> string -> string list

val iter_files :
  ?max_depth:int ->
  ?filter:(string -> bool) ->
  ?file_only:bool ->
  Path.t list ->
  (string -> unit) ->
  unit
