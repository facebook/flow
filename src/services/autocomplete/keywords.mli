(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val keywords_at_loc : (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> string list
