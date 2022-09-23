(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type Config = sig
  module Types : Func_class_sig_types.Config.S

  val param_type : Types.param -> Type.fun_param

  val rest_type : Types.rest -> Type.fun_rest_param

  val this_type : Types.this_param -> Type.t

  val is_param_type_annotated : Types.param -> bool

  val is_rest_type_annotated : Types.rest -> bool

  val subst_param : Context.t -> Type.t Subst_name.Map.t -> Types.param -> Types.param

  val subst_rest : Context.t -> Type.t Subst_name.Map.t -> Types.rest -> Types.rest

  val subst_this : Context.t -> Type.t Subst_name.Map.t -> Types.this_param -> Types.this_param

  val eval_param : Context.t -> Types.param -> (ALoc.t * Type.t) Types.param_ast

  val eval_rest : Context.t -> Types.rest -> (ALoc.t * Type.t) Types.rest_ast

  val eval_this : Context.t -> Types.this_param -> (ALoc.t * Type.t) Types.this_ast
end

module type S = sig
  module Config_types : Func_class_sig_types.Config.S

  module Config : Config with module Types := Config_types

  module Types : Func_class_sig_types.Param.S with module Config := Config_types

  val empty : Types.reconstruct -> Types.t

  val add_param : Config_types.param -> Types.t -> Types.t

  val add_rest : Config_types.rest -> Types.t -> Types.t

  val add_this : Config_types.this_param -> Types.t -> Types.t

  val all_params_annotated : Types.t -> bool

  val value : Types.t -> Type.fun_param list

  val rest : Types.t -> Type.fun_rest_param option

  val this : Types.t -> Type.t option

  val eval : Context.t -> Types.t -> (ALoc.t * Type.t) Config_types.ast option
end
