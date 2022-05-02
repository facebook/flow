(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Loc_collections

type t = {
  mutable annotations: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation ALocMap.t;
  mutable expressions: (ALoc.t, ALoc.t * Type.t) Ast.Expression.t ALocMap.t;
  mutable functions: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t) ALocMap.t;
  mutable aliases: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.TypeAlias.t) ALocMap.t;
  mutable opaques: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.OpaqueType.t) ALocMap.t;
  mutable interfaces: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Interface.t) ALocMap.t;
  mutable declared_classes:
    (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareClass.t) ALocMap.t;
  mutable classes: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Class.t) ALocMap.t;
  mutable class_sigs:
    (Func_class_sig_types.Class_stmt_sig_types.t * (Type.t -> (ALoc.t, ALoc.t * Type.t) Ast.Class.t))
    ALocMap.t;
}

let empty =
  {
    annotations = ALocMap.empty;
    expressions = ALocMap.empty;
    functions = ALocMap.empty;
    aliases = ALocMap.empty;
    opaques = ALocMap.empty;
    interfaces = ALocMap.empty;
    declared_classes = ALocMap.empty;
    classes = ALocMap.empty;
    class_sigs = ALocMap.empty;
  }

let set_annotation cache ((loc, _) as anno) =
  cache.annotations <- ALocMap.add loc anno cache.annotations

let set_expression cache (((loc, _), _) as exp) =
  cache.expressions <- ALocMap.add loc exp cache.expressions

let set_function cache loc fn = cache.functions <- ALocMap.add loc fn cache.functions

let set_alias cache loc alias = cache.aliases <- ALocMap.add loc alias cache.aliases

let set_opaque cache loc opaque = cache.opaques <- ALocMap.add loc opaque cache.opaques

let set_interface cache loc inter = cache.interfaces <- ALocMap.add loc inter cache.interfaces

let set_declared_class cache loc class_ =
  cache.declared_classes <- ALocMap.add loc class_ cache.declared_classes

let set_class cache loc class_ = cache.classes <- ALocMap.add loc class_ cache.classes

let set_class_sig cache loc class_ = cache.class_sigs <- ALocMap.add loc class_ cache.class_sigs

let get_annotation cache loc = ALocMap.find_opt loc cache.annotations

let get_expression cache loc = ALocMap.find_opt loc cache.expressions

let get_function cache loc = ALocMap.find_opt loc cache.functions

let get_alias cache loc = ALocMap.find_opt loc cache.aliases

let get_opaque cache loc = ALocMap.find_opt loc cache.opaques

let get_interface cache loc = ALocMap.find_opt loc cache.interfaces

let get_declared_class cache loc = ALocMap.find_opt loc cache.declared_classes

let get_class cache loc = ALocMap.find_opt loc cache.classes

let get_class_sig cache loc = ALocMap.find_opt loc cache.class_sigs
