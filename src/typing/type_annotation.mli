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
  (Loc.t, Loc.t) Ast.Type.t ->
  (Loc.t, Loc.t * Type.t) Ast.Type.t

val convert_list :
  Context.t ->
  Type.t SMap.t ->
  (Loc.t, Loc.t) Ast.Type.t list ->
  Type.t list *
  (Loc.t, Loc.t * Type.t) Ast.Type.t list

val convert_opt :
  Context.t ->
  Type.t SMap.t ->
  (Loc.t, Loc.t) Ast.Type.t option ->
  Type.t option * (Loc.t, Loc.t * Type.t) Ast.Type.t option

val convert_qualification: ?lookup_mode:Env.LookupMode.t ->
  Context.t ->
  string ->
  (Loc.t, Loc.t) Ast.Type.Generic.Identifier.t ->
  Type.t * (Loc.t, Loc.t * Type.t) Ast.Type.Generic.Identifier.t

val mk_interface_super: Context.t ->
  Type.t SMap.t ->
  Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t ->
  Type.t * (Loc.t * (Loc.t, Loc.t * Type.t) Ast.Type.Generic.t)

val mk_super: Context.t ->
  Type.t SMap.t ->
  Loc.t ->
  Type.t ->
  (Loc.t, Loc.t) Ast.Type.ParameterInstantiation.t option ->
  Type.t * (Loc.t, Loc.t * Type.t) Ast.Type.ParameterInstantiation.t option

val mk_type_annotation: Context.t ->
  Type.t SMap.t ->
  Reason.t ->
  (Loc.t, Loc.t) Ast.Type.annotation option ->
  Type.t * (Loc.t, Loc.t * Type.t) Ast.Type.annotation option

val mk_nominal_type:
  Context.t ->
  Reason.t ->
  Type.t SMap.t ->
  (Type.t * (Loc.t, Loc.t) Ast.Type.ParameterInstantiation.t option) ->
  Type.t * (Loc.t, Loc.t * Type.t) Ast.Type.ParameterInstantiation.t option

val mk_type_param_declarations: Context.t ->
  ?tparams_map:(Type.t SMap.t) ->
  (Loc.t, Loc.t) Ast.Type.ParameterDeclaration.t option ->
  Type.typeparam list *
  Type.t SMap.t *
  (Loc.t, Loc.t * Type.t) Ast.Type.ParameterDeclaration.t option

val mk_interface_sig: Context.t ->
  Reason.t ->
  (Loc.t, Loc.t) Ast.Statement.Interface.t ->
  Class_sig.t * Type.t * (Loc.t, Loc.t * Type.t) Ast.Statement.Interface.t

val mk_declare_class_sig: Context.t ->
  Reason.t ->
  (Loc.t, Loc.t) Ast.Statement.DeclareClass.t ->
  Class_sig.t * Type.t * (Loc.t, Loc.t * Type.t) Ast.Statement.DeclareClass.t

val polarity: Loc.t Ast.Variance.t option -> Type.polarity

val qualified_name: (Loc.t, Loc.t) Ast.Type.Generic.Identifier.t -> string
