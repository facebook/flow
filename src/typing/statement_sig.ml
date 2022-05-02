(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Func_class_sig_types

module type S = sig
  module Env : Env_sig.S

  module Abnormal : Abnormal_sig.S with module Env := Env

  module Import_export : module type of Import_export.Make (Env)

  module Toplevels : module type of Toplevels.DependencyToplevels (Env) (Abnormal)

  module Anno : Type_annotation_sig.S with module Env := Env

  module Func_stmt_config : Func_stmt_config_sig.S with module Types := Func_stmt_config_types.Types

  module Func_stmt_params :
    Func_params.S
      with module Config_types := Func_stmt_config_types.Types
       and module Config := Func_stmt_config
       and module Types = Func_stmt_params_types

  module Func_stmt_sig :
    Func_sig_intf.S
      with module Config_types := Func_stmt_config_types.Types
      with module Config := Func_stmt_config
       and module Param := Func_stmt_params
       and module Types = Func_stmt_sig_types

  module Class_stmt_sig :
    Class_sig_intf.S
      with module Config_types := Func_stmt_config_types.Types
       and module Config := Func_stmt_config
       and module Param := Func_stmt_params
       and module Func := Func_stmt_sig
       and module Types = Class_stmt_sig_types

  val expression :
    ?cond:Type.cond_context ->
    Context.t ->
    hint:Type_hint.hint ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

  val statement :
    Context.t -> (ALoc.t, ALoc.t) Ast.Statement.t -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t

  val toplevel_decls : Context.t -> (ALoc.t, ALoc.t) Ast.Statement.t list -> unit

  val for_of_elemt : Context.t -> Type.t -> Reason.reason -> bool -> Type.t

  val mk_function :
    Context.t ->
    hint:Type_hint.hint ->
    needs_this_param:bool ->
    general:Type.t ->
    Reason.reason ->
    (ALoc.t, ALoc.t) Ast.Function.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t

  val mk_func_sig :
    Context.t ->
    func_hint:Type_hint.hint ->
    needs_this_param:bool ->
    Type.t Subst_name.Map.t ->
    Reason.reason ->
    (ALoc.t, ALoc.t) Ast.Function.t ->
    Func_stmt_sig.Types.t
    * ((ALoc.t, ALoc.t * Type.t) Ast.Function.Params.t ->
      (ALoc.t, ALoc.t * Type.t) Ast.Function.body ->
      Type.t ->
      (ALoc.t, ALoc.t * Type.t) Ast.Function.t
      )

  val plus_assign :
    Context.t ->
    reason:Reason.reason ->
    lhs_reason:Reason.reason ->
    rhs_reason:Reason.reason ->
    Type.t ->
    Type.t ->
    Type.t

  val arith_assign : Context.t -> Reason.reason -> Type.t -> Type.t -> Type.t

  val mk_class :
    Context.t ->
    ALoc.t ->
    name_loc:ALoc.t ->
    general:Type.t ->
    Reason.t ->
    (ALoc.t, ALoc.t) Ast.Class.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Class.t

  val mk_class_sig :
    Context.t ->
    name_loc:ALoc.t ->
    class_loc:ALoc.t ->
    Reason.t ->
    Type.t ->
    (ALoc.t, ALoc.t) Ast.Class.t ->
    Class_stmt_sig.Types.t * (Type.t -> (ALoc.t, ALoc.t * Type.t) Ast.Class.t)

  val type_alias :
    Context.t ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.Statement.TypeAlias.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.TypeAlias.t

  val opaque_type :
    Context.t ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.Statement.OpaqueType.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.OpaqueType.t

  val import_named_specifier_type :
    Context.t ->
    Reason.t ->
    Ast.Statement.ImportDeclaration.import_kind ->
    source_loc:ALoc.t ->
    module_name:string ->
    remote_name_loc:ALoc.t ->
    remote_name:string ->
    local_name:string ->
    Type.t

  val import_namespace_specifier_type :
    Context.t ->
    Reason.t ->
    Ast.Statement.ImportDeclaration.import_kind ->
    source_loc:ALoc.t ->
    module_name:string ->
    local_loc:ALoc.t ->
    Type.t

  val import_default_specifier_type :
    Context.t ->
    Reason.t ->
    Ast.Statement.ImportDeclaration.import_kind ->
    source_loc:ALoc.t ->
    module_name:string ->
    local_loc:ALoc.t ->
    local_name:string ->
    Type.t

  val interface :
    Context.t ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.Statement.Interface.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Interface.t

  val declare_class :
    Context.t ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.Statement.DeclareClass.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareClass.t

  val mk_enum :
    Context.t ->
    enum_reason:Reason.t ->
    ALoc.t ->
    ALoc.t Ast.Statement.EnumDeclaration.body ->
    Type.enum_t
end
