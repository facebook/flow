(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 **)

open Typing_defs

val check_class_access:
  Pos.t -> Typing_env.env -> (Pos.t * visibility) -> Nast.class_id ->
  class_type -> unit

val check_obj_access:
  Pos.t -> Typing_env.env -> (Pos.t * visibility) -> unit

val is_visible:
  Typing_env.env -> visibility -> Nast.class_id option -> class_type -> bool

val min_vis_opt:
  (Pos.t * visibility) option -> (Pos.t * visibility) option ->
  (Pos.t * visibility) option
