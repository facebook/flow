(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type 'T ast

  type 'T param_ast

  type 'T rest_ast

  type t

  type param

  type rest

  type reconstruct =
    (ALoc.t * Type.t) param_ast list ->
    (ALoc.t * Type.t) rest_ast option ->
    (ALoc.t * Type.t) ast option

  val empty : reconstruct -> t

  val add_param : param -> t -> t

  val add_rest : rest -> t -> t

  val value : t -> Type.fun_param list

  val rest : t -> Type.fun_rest_param option

  val subst : Context.t -> Type.t SMap.t -> t -> t

  val eval : Context.t -> t -> (ALoc.t * Type.t) ast option
end

module type Config = sig
  type 'T ast

  type 'T param_ast

  type 'T rest_ast

  type param

  type rest

  val param_type : param -> Type.fun_param

  val rest_type : rest -> Type.fun_rest_param

  val subst_param : Context.t -> Type.t SMap.t -> param -> param

  val subst_rest : Context.t -> Type.t SMap.t -> rest -> rest

  val eval_param : Context.t -> param -> (ALoc.t * Type.t) param_ast

  val eval_rest : Context.t -> rest -> (ALoc.t * Type.t) rest_ast
end
