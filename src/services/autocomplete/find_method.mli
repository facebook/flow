(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find : Parsing_heaps.Reader.reader -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Class.Method.t Option.t
