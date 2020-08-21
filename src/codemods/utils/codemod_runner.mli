(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a unit_result = ('a, ALoc.t * Error_message.internal_error) result

type ('a, 'ctx) abstract_visitor = (Loc.t, Loc.t) Flow_ast.Program.t -> 'ctx -> 'a

(** TypedRunner - This runner does a full local typecheck and makes available the
    typed AST as well as the full context to each job. This runner is needed for
    any type based transforms. It runs a full local typecheck so can be slow on
    large codebases. *)
module type SIMPLE_TYPED_RUNNER_CONFIG = sig
  type accumulator

  val reporter : accumulator Codemod_report.t

  val visit : (accumulator, Codemod_context.Typed.t) abstract_visitor
end

module type UNTYPED_RUNNER_CONFIG = sig
  type accumulator

  val reporter : accumulator Codemod_report.t

  val visit : (accumulator, Codemod_context.Untyped.t) abstract_visitor
end

module type UNTYPED_FLOW_INIT_RUNNER_CONFIG = sig
  type accumulator

  val init : reader:State_reader.t -> unit

  val reporter : accumulator Codemod_report.t

  val visit : (accumulator, Codemod_context.UntypedFlowInit.t) abstract_visitor
end

module type RUNNABLE = sig
  val run : genv:ServerEnv.genv -> write:bool -> repeat:bool -> Utils_js.FilenameSet.t -> unit Lwt.t
end

module MakeSimpleTypedRunner (C : SIMPLE_TYPED_RUNNER_CONFIG) : RUNNABLE

module MakeUntypedFlowInitRunner (C : UNTYPED_FLOW_INIT_RUNNER_CONFIG) : RUNNABLE

module MakeUntypedRunner (C : UNTYPED_RUNNER_CONFIG) : RUNNABLE
