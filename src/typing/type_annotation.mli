(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** services for producing types from annotations,
    called during AST traversal.
  *)

module Class_type_sig : Class_sig.S

val convert :
  Context.t ->
  Type.t SMap.t ->
  (ALoc.t, ALoc.t) Flow_ast.Type.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t

val convert_list :
  Context.t ->
  Type.t SMap.t ->
  (ALoc.t, ALoc.t) Flow_ast.Type.t list ->
  Type.t list * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t list

val convert_opt :
  Context.t ->
  Type.t SMap.t ->
  (ALoc.t, ALoc.t) Flow_ast.Type.t option ->
  Type.t option * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t option

val convert_qualification :
  ?lookup_mode:Env.LookupMode.t ->
  Context.t ->
  string ->
  (ALoc.t, ALoc.t) Flow_ast.Type.Generic.Identifier.t ->
  Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.Generic.Identifier.t

val mk_super :
  Context.t ->
  Type.t SMap.t ->
  ALoc.t ->
  Type.t ->
  (ALoc.t, ALoc.t) Flow_ast.Type.TypeArgs.t option ->
  (ALoc.t * Type.t * Type.t list option) * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeArgs.t option

val mk_type_annotation :
  Context.t ->
  Type.t SMap.t ->
  Reason.t ->
  (ALoc.t, ALoc.t) Flow_ast.Type.annotation_or_hint ->
  Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.annotation_or_hint

val mk_return_type_annotation :
  Context.t ->
  Type.t SMap.t ->
  Reason.t ->
  definitely_returns_void:bool ->
  (ALoc.t, ALoc.t) Flow_ast.Type.annotation_or_hint ->
  Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.annotation_or_hint

val mk_type_available_annotation :
  Context.t ->
  Type.t SMap.t ->
  (ALoc.t, ALoc.t) Flow_ast.Type.annotation ->
  Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.annotation

val mk_nominal_type :
  Context.t ->
  Reason.t ->
  Type.t SMap.t ->
  Type.t * (ALoc.t, ALoc.t) Flow_ast.Type.TypeArgs.t option ->
  Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeArgs.t option

val mk_type_param_declarations :
  Context.t ->
  ?tparams_map:Type.t SMap.t ->
  (ALoc.t, ALoc.t) Flow_ast.Type.TypeParams.t option ->
  Type.typeparams * Type.t SMap.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.TypeParams.t option

val mk_interface_sig :
  Context.t ->
  Reason.t ->
  (ALoc.t, ALoc.t) Flow_ast.Statement.Interface.t ->
  Class_type_sig.t * Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.Interface.t

val mk_declare_class_sig :
  Context.t ->
  Reason.t ->
  (ALoc.t, ALoc.t) Flow_ast.Statement.DeclareClass.t ->
  Class_type_sig.t * Type.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.DeclareClass.t

val polarity : 'a Flow_ast.Variance.t option -> Polarity.t

val qualified_name : (ALoc.t, ALoc.t) Flow_ast.Type.Generic.Identifier.t -> string

val error_type :
  Context.t ->
  ALoc.t ->
  Error_message.t ->
  (ALoc.t, ALoc.t) Flow_ast.Type.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.t
