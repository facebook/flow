(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Typing_defs

(* Expands a type access to underlying type *)
val expand_with_env:
  Typing_phase.env ->
  Typing_env.env ->
  Typing_reason.t ->
  locl taccess_type ->
  Typing_env.env * ety

val expand:
  Typing_env.env ->
  Typing_reason.t ->
  locl taccess_type ->
  Typing_env.env * locl ty

val fill_type_hole:
  Typing_env.env ->
  Nast.class_id ->
  locl ty -> locl ty ->
  Typing_env.env * locl ty
