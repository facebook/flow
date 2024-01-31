(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Loc_collections

type cache = {
  annotations: (ALoc.t, ALoc.t * Type.t) Ast.Type.annotation ALocMap.t;
  expressions: (ALoc.t, ALoc.t * Type.t) Ast.Expression.t ALocMap.t;
  statements: (ALoc.t, ALoc.t * Type.t) Ast.Statement.t ALocMap.t;
  jsx_body:
    (Type.unresolved_param list * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list)) ALocMap.t;
  functions: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t) ALocMap.t;
  function_sigs:
    ( Func_class_sig_types.Func_stmt_sig_types.t
    * ((ALoc.t, ALoc.t * Type.t) Ast.Function.Params.t ->
      (ALoc.t, ALoc.t * Type.t) Ast.Function.body ->
      Type.t ->
      (ALoc.t, ALoc.t * Type.t) Ast.Function.t
      )
    )
    ALocMap.t;
  aliases: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.TypeAlias.t) ALocMap.t;
  opaques: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.OpaqueType.t) ALocMap.t;
  interfaces: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Interface.t) ALocMap.t;
  declared_classes: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareClass.t) ALocMap.t;
  declared_components:
    (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareComponent.t) ALocMap.t;
  declared_namespaces:
    (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareNamespace.t) ALocMap.t;
  classes: (Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Class.t) ALocMap.t;
  class_sigs:
    ( Type.t
    * Type.t
    * Func_class_sig_types.Class_stmt_sig_types.t
    * (Type.t -> (ALoc.t, ALoc.t * Type.t) Ast.Class.t)
    )
    ALocMap.t;
  tparams: ((ALoc.t, ALoc.t * Type.t) Ast.Type.TypeParam.t * Type.typeparam * Type.t) ALocMap.t;
  component_sigs:
    ( Component_sig_types.Component_declaration_sig_types.t
    * ((ALoc.t, ALoc.t * Type.t) Ast.Statement.ComponentDeclaration.Params.t ->
      ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Block.t ->
      Type.t ->
      (ALoc.t, ALoc.t * Type.t) Ast.Statement.ComponentDeclaration.t
      )
    )
    ALocMap.t;
}

type t = cache ref

let mk_empty () =
  ref
    {
      annotations = ALocMap.empty;
      expressions = ALocMap.empty;
      statements = ALocMap.empty;
      jsx_body = ALocMap.empty;
      functions = ALocMap.empty;
      function_sigs = ALocMap.empty;
      aliases = ALocMap.empty;
      opaques = ALocMap.empty;
      interfaces = ALocMap.empty;
      declared_classes = ALocMap.empty;
      declared_components = ALocMap.empty;
      declared_namespaces = ALocMap.empty;
      classes = ALocMap.empty;
      class_sigs = ALocMap.empty;
      tparams = ALocMap.empty;
      component_sigs = ALocMap.empty;
    }

let set_annotation cache ((loc, _) as anno) =
  cache := { !cache with annotations = ALocMap.add loc anno !cache.annotations }

let set_expression cache (((loc, _), _) as exp) =
  cache := { !cache with expressions = ALocMap.add loc exp !cache.expressions }

let set_statement cache ((loc, _) as stmt) =
  cache := { !cache with statements = ALocMap.add loc stmt !cache.statements }

let set_jsx_children cache ((_, (loc, _)) as children) =
  cache := { !cache with jsx_body = ALocMap.add loc children !cache.jsx_body }

let set_function cache loc fn =
  cache := { !cache with functions = ALocMap.add loc fn !cache.functions }

let set_function_sig cache loc fn =
  cache := { !cache with function_sigs = ALocMap.add loc fn !cache.function_sigs }

let set_alias cache loc alias =
  cache := { !cache with aliases = ALocMap.add loc alias !cache.aliases }

let set_opaque cache loc opaque =
  cache := { !cache with opaques = ALocMap.add loc opaque !cache.opaques }

let set_interface cache loc inter =
  cache := { !cache with interfaces = ALocMap.add loc inter !cache.interfaces }

let set_declared_component cache loc component =
  cache :=
    { !cache with declared_components = ALocMap.add loc component !cache.declared_components }

let set_declared_class cache loc class_ =
  cache := { !cache with declared_classes = ALocMap.add loc class_ !cache.declared_classes }

let set_declared_namespace cache loc ns =
  cache := { !cache with declared_namespaces = ALocMap.add loc ns !cache.declared_namespaces }

let set_class cache loc class_ =
  cache := { !cache with classes = ALocMap.add loc class_ !cache.classes }

let set_class_sig cache loc class_ =
  cache := { !cache with class_sigs = ALocMap.add loc class_ !cache.class_sigs }

let set_tparam cache (((loc, _), _, _) as param) =
  cache := { !cache with tparams = ALocMap.add loc param !cache.tparams }

let set_component_sig cache loc c =
  cache := { !cache with component_sigs = ALocMap.add loc c !cache.component_sigs }

let get_annotation cache loc = ALocMap.find_opt loc !cache.annotations

let get_expression cache loc = ALocMap.find_opt loc !cache.expressions

let get_statement cache loc = ALocMap.find_opt loc !cache.statements

let get_jsx_children cache loc = ALocMap.find_opt loc !cache.jsx_body

let get_function_sig cache loc = ALocMap.find_opt loc !cache.function_sigs

let get_function cache loc = ALocMap.find_opt loc !cache.functions

let get_alias cache loc = ALocMap.find_opt loc !cache.aliases

let get_opaque cache loc = ALocMap.find_opt loc !cache.opaques

let get_interface cache loc = ALocMap.find_opt loc !cache.interfaces

let get_declared_class cache loc = ALocMap.find_opt loc !cache.declared_classes

let get_declared_component cache loc = ALocMap.find_opt loc !cache.declared_components

let get_declared_namespace cache loc = ALocMap.find_opt loc !cache.declared_namespaces

let get_class cache loc = ALocMap.find_opt loc !cache.classes

let get_class_sig cache loc = ALocMap.find_opt loc !cache.class_sigs

let get_tparam cache loc = ALocMap.find_opt loc !cache.tparams

let get_component_sig cache loc = ALocMap.find_opt loc !cache.component_sigs
