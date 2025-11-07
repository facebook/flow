(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val score_of_msg : Error_message.t -> int

val post_process_errors : Flow_error.ErrorSet.t -> Flow_error.ErrorSet.t

val make_intermediate_error :
  loc_of_aloc:('loc -> Loc.t) ->
  'loc Flow_error.t ->
  'loc Flow_intermediate_error_types.intermediate_error

val to_printable_error :
  loc_of_aloc:('loc -> Loc.t) ->
  get_ast:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  strip_root:File_path.t option ->
  'loc Flow_intermediate_error_types.intermediate_error ->
  Loc.t Flow_errors_utils.printable_error

val make_errors_printable :
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  strip_root:File_path.t option ->
  Flow_error.ErrorSet.t ->
  Flow_errors_utils.ConcreteLocPrintableErrorSet.t
