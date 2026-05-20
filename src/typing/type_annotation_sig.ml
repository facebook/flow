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

  (** Compute the [Func_class_sig_types.Class.class_like_binding_kind] of the
      leaf binding referenced by an [extends]/[implements] AST identifier
      (qualified or unqualified), reading untyped AST locs (so this can be
      called BEFORE [convert_qualification]). Used to attach per-typeapp
      binding-kind metadata to interface [extends] and class [implements]
      lists for [Class_sig.supertype] / [check_implements] dispatch. Cycle-
      safe — never forces any tvar. *)
  val binding_kind_of_generic_id_pre_convert :
    Context.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.Generic.Identifier.t ->
    Func_class_sig_types.Class.class_like_binding_kind

  (** Like [binding_kind_of_generic_id_pre_convert] but also handles cross-
      module class-like imports and namespace-member class-like references.
      For cross-module imports, recomputes the raw exporter-side type via
      [import_named_specifier_type_for_extends] (skipping
      [canonicalize_imported_type]'s [fix_this_instance] unwrap) and tags as
      [ClassLikeRaw{Mono,Poly}]. For namespace members ([NS.I]), the
      converted [c] is already raw and we structurally peek at it.
      Cycle-safe: only inspects [Name_def] entries (AST) for cross-module
      imports, and resolves the converted [c] which is never an in-progress
      in-file interface body (cross-module / qualified refs only). *)
  val binding_kind_of_generic_id_post_convert :
    Context.t ->
    id_pre_convert:(ALoc.t, ALoc.t) Flow_ast.Type.Generic.Identifier.t ->
    converted_t:Type.t ->
    Func_class_sig_types.Class.class_like_binding_kind

  val convert_render_type :
    Context.t ->
    Type.t Subst_name.Map.t ->
    Env_api.With_ALoc.L.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.Renders.t ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Renders.t

  val convert_type_guard :
    Context.t ->
    Type.t Subst_name.Map.t ->
    Type.fun_param list ->
    ALoc.t ->
    Flow_ast.Type.TypeGuard.kind ->
    (ALoc.t, ALoc.t) Flow_ast.Identifier.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.t ->
    (ALoc.t, ALoc.t Flow_ast.Comment.t list) Flow_ast.Syntax.t option ->
    Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeGuard.t * Type.type_guard option

  val mk_empty_interface_type : Context.t -> ALoc.t -> Type.t

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
    kind:Flow_ast_mapper.type_params_context ->
    (ALoc.t, ALoc.t) Flow_ast.Type.TypeParam.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeParam.t * Type.typeparam * Type.t

  val mk_type_param_declarations :
    Context.t ->
    kind:Flow_ast_mapper.type_params_context ->
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

  val convert_indexer :
    Context.t ->
    Type.t Subst_name.Map.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.Object.Indexer.t' ->
    Type.dicttype * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Object.Indexer.t'

  val polarity :
    Context.t -> on:[ `Property | `TypeParam ] -> ALoc.t Flow_ast.Variance.t option -> Polarity.t

  val qualified_name : (ALoc.t, ALoc.t) Flow_ast.Type.Generic.Identifier.t -> string

  val error_type :
    Context.t ->
    ALoc.t ->
    Error_message.t ->
    (ALoc.t, ALoc.t) Flow_ast.Type.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t

  val error_on_unsupported_variance_annotation :
    Context.t -> kind:string -> (ALoc.t, ALoc.t) Flow_ast.Type.TypeParams.t option -> unit

  type method_kind =
    | FunctionKind
    | MethodKind of { static: bool }
    | ConstructorKind
    | GetterKind
    | SetterKind

  val method_kind_to_string : method_kind -> string

  val convert_return_annotation :
    meth_kind:method_kind ->
    Context.t ->
    Type.t Subst_name.Map.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Function.Params.t ->
    Type.fun_param list ->
    (ALoc.t, ALoc.t) Flow_ast.Type.Function.return_annotation ->
    Type.t
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Function.return_annotation
    * Type.type_guard option

  (** Build the typed Function.Param AST node for a single function-type
      parameter from its raw param, the resolved type, and the converted
      annotation AST. Shared by all function-type conversion paths to keep
      typed-AST construction consistent. *)
  val typed_function_param_ast :
    (ALoc.t, ALoc.t) Flow_ast.Type.Function.Param.t' ->
    Type.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Function.Param.t'
end
