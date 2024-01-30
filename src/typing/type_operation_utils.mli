(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module contains a collection of functions that operate on types, with the LTI assumption
 * the inspected type will never receive any additional future bounds. *)

module Ast = Flow_ast
open Reason

module Import_export : sig
  val import_named_specifier_type :
    Context.t ->
    t ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    source_module_t:Type.t ->
    remote_name:string ->
    local_name:string ->
    ALoc.t option * Type.t

  val import_namespace_specifier_type :
    Context.t ->
    t ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    source_module_t:Type.t ->
    local_loc:ALoc.t ->
    Type.t

  val import_default_specifier_type :
    Context.t ->
    t ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    source_module_t:Type.t ->
    local_name:string ->
    ALoc.t option * Type.t
end
