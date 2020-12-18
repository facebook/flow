(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val detect_errors :
  metadata:Context.metadata ->
  phase:Context.phase ->
  Context.t ->
  (Context.t * (ALoc.t, ALoc.t) Flow_ast.Program.t * 'a) Base.List.t ->
  unit
