(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

module type ConsGen = sig
  val mk_instance :
    Context.t -> ?type_t_kind:Type.type_t_kind -> reason -> ?use_desc:bool -> Type.t -> Type.t

  val specialize :
    Context.t ->
    Type.t ->
    Type.use_op ->
    Reason.t ->
    Reason.t ->
    Type.t list Base.Option.t ->
    Type.t

  val get_prop :
    Context.t -> Type.use_op -> Reason.t -> ?op_reason:Reason.t -> Reason.name -> Type.t -> Type.t

  val qualify_type :
    Context.t -> Type.use_op -> Reason.t -> op_reason:Reason.t -> Reason.name -> Type.t -> Type.t

  val get_builtin_type : Context.t -> reason -> ?use_desc:bool -> string -> Type.t

  val obj_test_proto : Context.t -> Reason.t -> Type.t -> Type.t

  val mixin : Context.t -> Reason.t -> Type.t -> Type.t
end

(** services for producing types from annotations,
    called during AST traversal.
 *)

module type S = sig
  module Class_type_sig : Class_sig_intf.S

  val convert :
    Context.t ->
    Type.t Subst_name.Map.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t

  val convert_list :
    Context.t ->
    Type.t Subst_name.Map.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.t list ->
    Type.t list * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t list

  val convert_opt :
    Context.t ->
    Type.t Subst_name.Map.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.t option ->
    Type.t option * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t option

  val convert_qualification :
    ?lookup_mode:Type_env.LookupMode.t ->
    Context.t ->
    string ->
    (ALoc.t, ALoc.t) Flow_ast.Type.Generic.Identifier.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Generic.Identifier.t

  val convert_render_type :
    Context.t ->
    allow_generic_t:bool ->
    Type.t Subst_name.Map.t ->
    Env_api.With_ALoc.L.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.Renders.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Renders.t

  val convert_type_guard :
    Context.t ->
    Type.t Subst_name.Map.t ->
    Type.fun_param list ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Flow_ast.Identifier.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.t ->
    (ALoc.t, ALoc.t Flow_ast.Comment.t list) Flow_ast.Syntax.t option ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeGuard.t * Type.fun_predicate option

  val mk_super :
    Context.t ->
    Type.t Subst_name.Map.t ->
    ALoc.t ->
    Type.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.TypeArgs.t option ->
    (ALoc.t * Type.t * Type.t list option)
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeArgs.t option

  val mk_type_available_annotation :
    Context.t ->
    Type.t Subst_name.Map.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.annotation ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.annotation

  val mk_function_type_annotation :
    Context.t ->
    Type.t Subst_name.Map.t ->
    ALoc.t * (ALoc.t, ALoc.t) Flow_ast.Type.Function.t ->
    Type.t * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Function.t)

  val mk_nominal_type :
    Context.t ->
    Reason.t ->
    Type.t Subst_name.Map.t ->
    Type.t * (ALoc.t, ALoc.t) Flow_ast.Type.TypeArgs.t option ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeArgs.t option

  val mk_type_param :
    Context.t ->
    Type.t Subst_name.Map.t ->
    from_infer_type:bool ->
    (ALoc.t, ALoc.t) Flow_ast.Type.TypeParam.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeParam.t * Type.typeparam * Type.t

  val mk_type_param_declarations :
    Context.t ->
    ?tparams_map:Type.t Subst_name.Map.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.TypeParams.t option ->
    Type.typeparams
    * Type.t Subst_name.Map.t
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeParams.t option

  val mk_interface_sig :
    Context.t ->
    ALoc.t ->
    Reason.t ->
    (ALoc.t, ALoc.t) Flow_ast.Statement.Interface.t ->
    Type.t * Class_type_sig.Types.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.Interface.t

  val mk_declare_class_sig :
    Context.t ->
    ALoc.t ->
    string ->
    Reason.t ->
    (ALoc.t, ALoc.t) Flow_ast.Statement.DeclareClass.t ->
    Type.t * Class_type_sig.Types.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.DeclareClass.t

  val mk_declare_component_sig :
    Context.t ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Flow_ast.Statement.DeclareComponent.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.DeclareComponent.t

  val polarity : Context.t -> ALoc.t Flow_ast.Variance.t option -> Polarity.t

  val qualified_name : (ALoc.t, ALoc.t) Flow_ast.Type.Generic.Identifier.t -> string

  val error_type :
    Context.t ->
    ALoc.t ->
    Error_message.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t

  val error_on_unsupported_variance_annotation :
    Context.t -> kind:string -> (ALoc.t, ALoc.t) Flow_ast.Type.TypeParams.t option -> unit
end
