(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type module_ref = string

type require = module_ref * ALoc.t Nel.t * Modulename.t

type check_file =
  File_key.t ->
  require list ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  Loc.t Flow_ast.Comment.t list ->
  File_sig.With_ALoc.t ->
  Docblock.t ->
  ALoc.table Lazy.t ->
  Loc_collections.ALocIDSet.t ->
  Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

val mk_check_file :
  options:Options.t -> reader:Abstract_state_reader.t -> cache:Check_cache.t -> unit -> check_file
