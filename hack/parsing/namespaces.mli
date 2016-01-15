(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val elaborate_id : Namespace_env.env -> Ast.ns_kind -> Ast.id -> Ast.id
val elaborate_defs : Ast.program -> Ast.program
