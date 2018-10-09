(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** services for producing types from annotations,
    called during AST traversal.
  *)

val convert: Context.t ->
  Type.t SMap.t ->
  (Loc.t, Loc.t) Flow_ast.Type.t ->
  (Loc.t, Loc.t * Type.t) Flow_ast.Type.t

val convert_list :
  Context.t ->
  Type.t SMap.t ->
  (Loc.t, Loc.t) Flow_ast.Type.t list ->
  Type.t list *
  (Loc.t, Loc.t * Type.t) Flow_ast.Type.t list

val convert_opt :
  Context.t ->
  Type.t SMap.t ->
  (Loc.t, Loc.t) Flow_ast.Type.t option ->
  Type.t option * (Loc.t, Loc.t * Type.t) Flow_ast.Type.t option

val convert_qualification: ?lookup_mode:Env.LookupMode.t ->
  Context.t ->
  string ->
  (Loc.t, Loc.t) Flow_ast.Type.Generic.Identifier.t ->
  Type.t * (Loc.t, Loc.t * Type.t) Flow_ast.Type.Generic.Identifier.t

val mk_super: Context.t ->
  Type.t SMap.t ->
  Loc.t ->
  Type.t ->
  (Loc.t, Loc.t) Flow_ast.Type.ParameterInstantiation.t option ->
  (Loc.t * Type.t * Type.t list option) * (Loc.t, Loc.t * Type.t) Flow_ast.Type.ParameterInstantiation.t option

val mk_type_annotation: Context.t ->
  Type.t SMap.t ->
  Reason.t ->
  (Loc.t, Loc.t) Flow_ast.Type.annotation_or_hint ->
  Type.t * (Loc.t, Loc.t * Type.t) Flow_ast.Type.annotation_or_hint

val mk_type_available_annotation: Context.t ->
  Type.t SMap.t ->
  (Loc.t, Loc.t) Flow_ast.Type.annotation ->
  Type.t * (Loc.t, Loc.t * Type.t) Flow_ast.Type.annotation

val mk_nominal_type:
  Context.t ->
  Reason.t ->
  Type.t SMap.t ->
  (Type.t * (Loc.t, Loc.t) Flow_ast.Type.ParameterInstantiation.t option) ->
  Type.t * (Loc.t, Loc.t * Type.t) Flow_ast.Type.ParameterInstantiation.t option

val mk_type_param_declarations: Context.t ->
  ?tparams_map:(Type.t SMap.t) ->
  (Loc.t, Loc.t) Flow_ast.Type.ParameterDeclaration.t option ->
  Type.typeparams *
  Type.t SMap.t *
  (Loc.t, Loc.t * Type.t) Flow_ast.Type.ParameterDeclaration.t option

val mk_interface_sig: Context.t ->
  Reason.t ->
  (Loc.t, Loc.t) Flow_ast.Statement.Interface.t ->
  Class_sig.t * Type.t * (Loc.t, Loc.t * Type.t) Flow_ast.Statement.Interface.t

val mk_declare_class_sig: Context.t ->
  Reason.t ->
  (Loc.t, Loc.t) Flow_ast.Statement.DeclareClass.t ->
  Class_sig.t * Type.t * (Loc.t, Loc.t * Type.t) Flow_ast.Statement.DeclareClass.t

val polarity: Loc.t Flow_ast.Variance.t option -> Type.polarity

val qualified_name: (Loc.t, Loc.t) Flow_ast.Type.Generic.Identifier.t -> string

val error_type:
  Context.t
  -> Loc.t
  -> Flow_error.error_message
  -> (Loc.t * Type.t) * (Loc.t, Loc.t * Typed_ast.T.t) Flow_ast.Type.t'
