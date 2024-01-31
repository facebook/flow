(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Func_class_sig_types

module type S = sig
  module Anno : Type_annotation_sig.S

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

  module Component_declaration_config :
    Component_params_intf.Config with module Types := Component_sig_types.DeclarationParamConfig

  module Component_declaration_params :
    Component_params.S
      with module Config_types := Component_sig_types.DeclarationParamConfig
       and module Config := Component_declaration_config
       and module Types = Component_sig_types.Component_declaration_params_types

  module Component_declaration_body :
    Component_sig_intf.ComponentBody with module Config := Component_sig_types.DeclarationBodyConfig

  module Component_declaration_sig :
    Component_sig_intf.S
      with module Config_types := Component_sig_types.DeclarationParamConfig
      with module Config := Component_declaration_config
       and module Param := Component_declaration_params
       and module BodyConfig := Component_sig_types.DeclarationBodyConfig
       and module ComponentBody := Component_declaration_body
       and module Types = Component_sig_types.Component_declaration_sig_types

  module ObjectExpressionAcc : sig
    type t

    (* unit arg is required due to recursive modules *)
    val empty : unit -> t

    val add_prop : (Type.Properties.t -> Type.Properties.t) -> t -> t

    val add_spread : Type.t -> t -> t

    val mk_object_from_spread_acc :
      Context.t -> t -> Reason.t -> frozen:bool -> default_proto:Type.t -> Type.t
  end

  val convert_call_targs_opt' :
    Context.t -> (ALoc.t, ALoc.t) Ast.Expression.CallTypeArgs.t option -> Type.targ list option

  val expression :
    ?cond:Type.cond_context ->
    ?as_const:bool ->
    Context.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

  val expression_or_spread :
    Context.t ->
    (ALoc.t, ALoc.t) Ast.Expression.expression_or_spread ->
    Type.call_arg * (ALoc.t, ALoc.t * Type.t) Ast.Expression.expression_or_spread

  val jsx_mk_props :
    Context.t ->
    Reason.reason ->
    check_expression:
      (Context.t -> (ALoc.t, ALoc.t) Ast.Expression.t -> (ALoc.t, ALoc.t * Type.t) Ast.Expression.t) ->
    collapse_children:
      (Context.t ->
      ALoc.t * (ALoc.t, ALoc.t) Ast.JSX.child list ->
      Type.unresolved_param list * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list)
      ) ->
    string ->
    (ALoc.t, ALoc.t) Ast.JSX.Opening.attribute list ->
    ALoc.t * (ALoc.t, ALoc.t) Ast.JSX.child list ->
    Type.t
    * (ALoc.t, ALoc.t * Type.t) Ast.JSX.Opening.attribute list
    * Type.unresolved_param list
    * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list)

  val collapse_children :
    Context.t ->
    ALoc.t * (ALoc.t, ALoc.t) Ast.JSX.child list ->
    Type.unresolved_param list * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list)

  val statement :
    Context.t -> (ALoc.t, ALoc.t) Ast.Statement.t -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t

  val statement_list :
    Context.t ->
    (ALoc.t, ALoc.t) Ast.Statement.t list ->
    (ALoc.t, ALoc.t * Type.t) Ast.Statement.t list

  val for_of_elemt : Context.t -> Type.t -> Reason.reason -> bool -> Type.t

  val mk_function :
    Context.t ->
    needs_this_param:bool ->
    (* Type of the function in typed ast.
     * For overloaded functions, the type must be read from the environment. *)
    ?tast_fun_type:Type.t ->
    statics:Env_api.EnvKey.t SMap.t ->
    Reason.reason ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.Function.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t

  val mk_arrow :
    Context.t ->
    statics:Env_api.EnvKey.t SMap.t ->
    Reason.reason ->
    (ALoc.t, ALoc.t) Ast.Function.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Function.t

  val mk_func_sig :
    Context.t ->
    required_this_param_type:Type.t option ->
    require_return_annot:bool ->
    constructor:bool ->
    getset:bool ->
    statics:Env_api.EnvKey.t SMap.t ->
    Type.t Subst_name.Map.t ->
    Reason.reason ->
    (ALoc.t, ALoc.t) Ast.Function.t ->
    Func_stmt_sig.Types.t
    * ((ALoc.t, ALoc.t * Type.t) Ast.Function.Params.t ->
      (ALoc.t, ALoc.t * Type.t) Ast.Function.body ->
      Type.t ->
      (ALoc.t, ALoc.t * Type.t) Ast.Function.t
      )

  val mk_component_sig :
    Context.t ->
    Type.t Subst_name.Map.t ->
    Reason.reason ->
    (ALoc.t, ALoc.t) Ast.Statement.ComponentDeclaration.t ->
    Component_declaration_sig.Types.t
    * ((ALoc.t, ALoc.t * Type.t) Ast.Statement.ComponentDeclaration.Params.t ->
      ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.Block.t ->
      Type.t ->
      (ALoc.t, ALoc.t * Type.t) Ast.Statement.ComponentDeclaration.t
      )

  val assignment_lhs :
    Context.t -> (ALoc.t, ALoc.t) Ast.Pattern.t -> (ALoc.t, ALoc.t * Type.t) Ast.Pattern.t

  val arith_assign :
    Context.t ->
    reason:Reason.reason ->
    lhs_reason:Reason.reason ->
    rhs_reason:Reason.reason ->
    Type.t ->
    Type.t ->
    Type.ArithKind.t ->
    Type.t

  val mk_class :
    Context.t ->
    ALoc.t ->
    name_loc:ALoc.t ->
    ?tast_class_type:Type.t ->
    Reason.t ->
    (ALoc.t, ALoc.t) Ast.Class.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Class.t

  val mk_class_sig :
    Context.t ->
    name_loc:ALoc.t ->
    class_loc:ALoc.t ->
    Reason.t ->
    (ALoc.t, ALoc.t) Ast.Class.t ->
    Type.t * Type.t * Class_stmt_sig.Types.t * (Type.t -> (ALoc.t, ALoc.t * Type.t) Ast.Class.t)

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

  val declare_component :
    Context.t ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.Statement.DeclareComponent.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareComponent.t

  val declare_namespace :
    Context.t ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.Statement.DeclareNamespace.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.Statement.DeclareNamespace.t

  val mk_enum :
    Context.t ->
    enum_reason:Reason.t ->
    ALoc.t ->
    string ->
    ALoc.t Ast.Statement.EnumDeclaration.body ->
    Type.enum_t

  val optional_chain :
    cond:Type.cond_context Base.Option.t ->
    Context.t ->
    (ALoc.t, ALoc.t) Ast.Expression.t ->
    Type.t * Type.t list * (ALoc.t, ALoc.t * Type.t) Ast.Expression.t

  val string_literal : Context.t -> as_const:bool -> ALoc.t -> ALoc.t Ast.StringLiteral.t -> Type.t

  val boolean_literal : as_const:bool -> ALoc.t -> ALoc.t Ast.BooleanLiteral.t -> Type.t

  val null_literal : ALoc.t -> Type.t

  val number_literal : as_const:bool -> ALoc.t -> ALoc.t Ast.NumberLiteral.t -> Type.t

  val bigint_literal : as_const:bool -> ALoc.t -> ALoc.t Ast.BigIntLiteral.t -> Type.t

  val regexp_literal : Context.t -> ALoc.t -> Type.t

  val module_ref_literal :
    Context.t ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.ModuleRefLiteral.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Ast.ModuleRefLiteral.t

  val identifier : Context.t -> ALoc.t Ast.Identifier.t' -> ALoc.t -> Type.t

  val get_prop :
    cond:Type.cond_context option ->
    Context.t ->
    Reason.t ->
    use_op:Type.use_op ->
    Type.t ->
    Reason.t * string ->
    Type.t

  val empty_array : Context.t -> ALoc.t -> Reason.t * Type.t
end
