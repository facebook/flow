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
val expand: Typing_env.env -> taccess_type -> Typing_env.env * ty

val fill_with_class_id:
  Typing_env.env -> Nast.class_id -> ty -> ty -> Typing_env.env * ty

val fill_with_expr:
  Typing_env.env -> Nast.expr -> ty -> ty -> Typing_env.env * ty
