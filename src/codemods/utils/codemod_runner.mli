(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ('a, 'ctx) abstract_visitor = (Loc.t, Loc.t) Flow_ast.program -> 'ctx -> 'a

type 'a untyped_flow_init_runner_visitor = {
  (* Runner init function which is called after Types_js.init but before any of the
   * jobs. This is useful to setup/populate any shared memory for the jobs. *)
  init: reader:State_reader.t -> unit;
  visit: ('a, Codemod_context.UntypedFlowInit.t) abstract_visitor;
}

type 'a visitor =
  | Typed_visitor of ('a, Codemod_context.Typed.t) abstract_visitor
  | UntypedFlowInitRunner_visitor of 'a untyped_flow_init_runner_visitor
  | Untyped_visitor of ('a, Codemod_context.Untyped.t) abstract_visitor

val run :
  genv:ServerEnv.genv ->
  write:bool ->
  repeat:bool ->
  visitor:'a visitor ->
  reporter:'a Codemod_report.t ->
  Utils_js.FilenameSet.t ->
  unit Lwt.t
