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
  val concretize_module_type :
    Context.t -> Reason.t -> Type.t -> (Type.moduletype, Reason.reason * Type.any_source) result

  val get_module_t : Context.t -> ?perform_platform_validation:bool -> ALoc.t * string -> Type.t

  val import_named_specifier_type :
    Context.t ->
    reason ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    source_module_t:Type.t ->
    remote_name:string ->
    local_name:string ->
    ALoc.t option * Type.t

  val get_module_namespace_type : Context.t -> reason -> Type.t -> Type.t

  val import_namespace_specifier_type :
    Context.t ->
    reason ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    source_module_t:Type.t ->
    local_loc:ALoc.t ->
    Type.t

  val import_default_specifier_type :
    Context.t ->
    reason ->
    Ast.Statement.ImportDeclaration.import_kind ->
    module_name:string ->
    source_module_t:Type.t ->
    local_name:string ->
    ALoc.t option * Type.t

  val cjs_require_type : Context.t -> reason -> legacy_interop:bool -> Type.t -> Type.t
end
