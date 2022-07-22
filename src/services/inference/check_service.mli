(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type module_ref = string

type require = module_ref * ALoc.t Nel.t * Parsing_heaps.resolved_module

type check_file =
  File_key.t ->
  require list ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  Loc.t Flow_ast.Comment.t list ->
  File_sig.With_ALoc.t ->
  Docblock.t ->
  ALoc.table Lazy.t ->
  Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

module type READER = sig
  type provider

  type typed_parse

  val get_master_cx : unit -> Context.master_context

  val get_provider : Modulename.t -> provider option

  val get_file_key : provider -> File_key.t

  val get_typed_parse : provider -> typed_parse option

  val get_leader_key : typed_parse -> File_key.t

  val get_aloc_table : typed_parse -> ALoc.table

  val get_docblock : typed_parse -> Docblock.t

  val get_type_sig_buf : typed_parse -> Type_sig_bin.buf

  val get_resolved_modules : typed_parse -> Parsing_heaps.resolved_module SMap.t
end

val mk_heap_reader : Abstract_state_reader.t -> (module READER)

val mk_check_file :
  (module READER) -> options:Options.t -> cache:Check_cache.t -> unit -> check_file
