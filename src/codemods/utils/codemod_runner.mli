(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ('a, 'ctx) abstract_visitor = (Loc.t, Loc.t) Flow_ast.program -> 'ctx -> 'a

type 'a visitor =
  | Typed_visitor of ('a, Codemod_context.Typed.t) abstract_visitor
  | Untyped_visitor of ('a, Codemod_context.Untyped.t) abstract_visitor

val run_and_digest :
  genv:ServerEnv.genv ->
  should_print_summary:bool ->
  info:bool ->
  visitor:'a visitor ->
  reporter:'a Codemod_report.t ->
  Options.t ->
  SSet.t ->
  (File_key.t list * 'a) Lwt.t
