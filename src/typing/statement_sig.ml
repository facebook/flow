(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module type S = sig
  module Env : Env_sig.S

  module Abnormal : Abnormal_sig.S with module Env := Env

  module Import_export : module type of Import_export.Make (Env)

  module Toplevels : module type of Toplevels.DependencyToplevels (Env) (Abnormal)

  module Anno : Type_annotation_sig.S with module Env := Env

  module Func_stmt_params : Func_params_intf.S

  module Func_stmt_sig : Func_sig_intf.S with type func_params := Func_stmt_params.t

  val expression :
    ?cond:Type.cond_context ->
    Context.t ->
    hint:Type.t option ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

  val statement :
    Context.t -> (ALoc.t, ALoc.t) Ast.Statement.t -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t

  val toplevel_decls : Context.t -> (ALoc.t, ALoc.t) Ast.Statement.t list -> unit

  val for_of_elemt : Context.t -> Type.t -> Reason.reason -> bool -> Type.t

  val mk_function :
    Context.t ->
    hint:Type.t option ->
    needs_this_param:bool ->
    general:Type.t ->
    Reason.reason ->
    (ALoc.t, ALoc.t) Ast.Function.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t

  val mk_func_sig :
    Context.t ->
    hint:Type.t option ->
    needs_this_param:bool ->
    Type.t Subst_name.Map.t ->
    Reason.reason ->
    (ALoc.t, ALoc.t) Ast.Function.t ->
    Func_stmt_sig.t
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

  val arith_assign : Context.t -> ALoc.t -> Type.t -> Type.t -> Type.t

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
