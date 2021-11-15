(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module type S = sig
  type 'T ast = (ALoc.t, 'T) Ast.Function.Params.t

  type 'T param_ast = (ALoc.t, 'T) Ast.Function.Param.t

  type 'T rest_ast = (ALoc.t, 'T) Ast.Function.RestParam.t

  type 'T this_ast = (ALoc.t, 'T) Ast.Function.ThisParam.t

  type pattern =
    | Id of (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t
    | Object of {
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation_or_hint;
        properties: (ALoc.t, ALoc.t) Ast.Pattern.Object.property list;
        comments: (ALoc.t, ALoc.t Ast.Comment.t list) Ast.Syntax.t option;
      }
    | Array of {
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation_or_hint;
        elements: (ALoc.t, ALoc.t) Ast.Pattern.Array.element list;
        comments: (ALoc.t, ALoc.t Ast.Comment.t list) Ast.Syntax.t option;
      }

  type param =
    | Param of {
        t: Type.t;
        loc: ALoc.t;
        ploc: ALoc.t;
        pattern: pattern;
        default: (ALoc.t, ALoc.t) Ast.Expression.t option;
        has_anno: bool;
      }

  type rest =
    | Rest of {
        t: Type.t;
        loc: ALoc.t;
        ploc: ALoc.t;
        id: (ALoc.t, ALoc.t * Type.t) Ast.Pattern.Identifier.t;
        has_anno: bool;
      }

  type this_param =
    | This of {
        t: Type.t;
        loc: ALoc.t;
        annot: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation;
      }

  val param_type : param -> string option * Type.t

  val rest_type : rest -> string option * ALoc.t * Type.t

  val this_type : this_param -> Type.t

  val subst_param : Context.t -> Type.t SMap.t -> param -> param

  val subst_rest : Context.t -> Type.t SMap.t -> rest -> rest

  val subst_this : Context.t -> Type.t SMap.t -> this_param -> this_param

  val bind : Context.t -> string -> Type.annotated_or_inferred -> ALoc.t -> unit

  val destruct :
    Context.t ->
    use_op:Type.use_op ->
    name_loc:ALoc.t ->
    has_anno:bool ->
    string ->
    Type.t Default.t Base.Option.t ->
    Type.t ->
    Type.t

  val eval_default :
    Context.t ->
    (ALoc.t, ALoc.t) Ast.Expression.t option ->
    (ALoc.t, ALoc.t * Type.t) Ast.Expression.t option

  val eval_param : Context.t -> param -> ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.Param.t'

  val eval_rest : Context.t -> rest -> ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.RestParam.t'

  val eval_this : 'a -> this_param -> ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.ThisParam.t'
end
