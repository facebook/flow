(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val get_class_requirements : Typing_env.env -> Nast.class_ ->
  Typing_env.env * Typing_defs.requirement list * SSet.t

val check_class : Typing_env.env -> Typing_defs.class_type -> unit
