(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ast = (Loc.t, Loc.t) Flow_ast.Program.t

val serialize_ast : ast -> string

val deserialize_ast : string -> File_key.t -> ast
