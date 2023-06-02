(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module defines the interface for the module that converts from the parameter representation
 * to types. This module is used by the Params module to reconstruct the typed AST.
 *
 * See component_declaration_config.ml for an example implementation.
 *)
module type Config = sig
  module Types : Component_sig_types.ParamConfig.S

  val eval_param : Context.t -> Types.param -> (ALoc.t * Type.t) Types.param_ast

  val eval_rest : Context.t -> Types.rest -> (ALoc.t * Type.t) Types.rest_ast

  val param_type_with_name : Types.param -> (ALoc.t * string * Type.t) option

  val rest_type : Types.rest -> Type.t
end
