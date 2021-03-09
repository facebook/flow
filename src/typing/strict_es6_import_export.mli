(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val detect_errors :
  'phase Context.t_ ->
  Context.metadata ->
  ('phase Context.t_ * (ALoc.t, ALoc.t) Flow_ast.Program.t * 'a) Base.List.t ->
  unit
