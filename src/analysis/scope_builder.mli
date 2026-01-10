(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val remove_params_default :
  ('loc, 'loc) Flow_ast.Function.Params.t -> bool * ('loc, 'loc) Flow_ast.Function.Params.t

val pattern_with_toplevel_annot_removed :
  none:'loc ->
  ('loc, 'loc) Flow_ast.Pattern.t ->
  ('loc, 'loc) Flow_ast.Type.annotation_or_hint * ('loc, 'loc) Flow_ast.Pattern.t

val make_component_annot_collector_and_default_remover :
  none:'loc -> (('loc, 'loc) Flow_ast.Type.t list, 'loc) Flow_ast_visitor.visitor

val for_statement :
  enable_enums:bool ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> 'a) -> 'a) ->
  scoped:('loc -> ('loc, 'loc) Flow_ast.Statement.For.t -> 'a) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Statement.For.t ->
  'a

val for_in_statement :
  enable_enums:bool ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> 'a) -> 'a) ->
  scoped:('loc -> ('loc, 'loc) Flow_ast.Statement.ForIn.t -> 'a) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Statement.ForIn.t ->
  'a

val for_of_statement :
  enable_enums:bool ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> 'a) -> 'a) ->
  scoped:('loc -> ('loc, 'loc) Flow_ast.Statement.ForOf.t -> 'a) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Statement.ForOf.t ->
  'a

val catch_clause :
  enable_enums:bool ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> 'a) -> 'a) ->
  scoped:('loc -> ('loc, 'loc) Flow_ast.Statement.Try.CatchClause.t' -> 'a) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Statement.Try.CatchClause.t' ->
  'a

val scoped_type_params :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ?hoist_op:((unit -> unit) -> unit) ->
  in_tparam_scope:(unit -> unit) ->
  ('loc, 'loc) Flow_ast.Type.TypeParams.t option ->
  unit

val function_expression_without_name :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  this_binding_function_id_opt:
    (fun_loc:'loc -> has_this_annot:bool -> ('loc, 'loc) Flow_ast.Identifier.t option -> unit) ->
  lambda:
    (is_arrow:bool ->
    fun_loc:'loc ->
    generator_return_loc:'loc option ->
    ('loc, 'loc) Flow_ast.Function.Params.t ->
    ('loc, 'loc) Flow_ast.Function.ReturnAnnot.t ->
    ('loc, 'loc) Flow_ast.Type.Predicate.t option ->
    ('loc, 'loc) Flow_ast.Function.body ->
    unit
    ) ->
  is_arrow:bool ->
  'loc ->
  ('loc, 'loc) Flow_ast.Function.t ->
  ('loc, 'loc) Flow_ast.Function.t

val match_case :
  enable_enums:bool ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> 'a) -> 'a) ->
  on_super_match_case:(on_case_body:('b -> 'b) -> ('loc, 'loc, 'b) Flow_ast.Match.Case.t -> 'a) ->
  on_case_body:('b -> 'b) ->
  ('loc, 'loc, 'b) Flow_ast.Match.Case.t ->
  'a

val function_declaration :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  this_binding_function_id_opt:
    (fun_loc:'loc -> has_this_annot:bool -> ('loc, 'loc) Flow_ast.Identifier.t option -> unit) ->
  hoist_annotations:((unit -> unit) -> unit) ->
  lambda:
    (is_arrow:bool ->
    fun_loc:'loc ->
    generator_return_loc:'loc option ->
    ('loc, 'loc) Flow_ast.Function.Params.t ->
    ('loc, 'loc) Flow_ast.Function.ReturnAnnot.t ->
    ('loc, 'loc) Flow_ast.Type.Predicate.t option ->
    ('loc, 'loc) Flow_ast.Function.body ->
    unit
    ) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Function.t ->
  ('loc, 'loc) Flow_ast.Function.t

val type_alias :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Statement.TypeAlias.t ->
  ('loc, 'loc) Flow_ast.Statement.TypeAlias.t

val opaque_type :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Statement.OpaqueType.t ->
  ('loc, 'loc) Flow_ast.Statement.OpaqueType.t

val interface :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Statement.Interface.t ->
  ('loc, 'loc) Flow_ast.Statement.Interface.t

val function_type :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Type.Function.t ->
  ('loc, 'loc) Flow_ast.Type.Function.t

val component_declaration :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  hoist_annotations:((unit -> unit) -> unit) ->
  component_body_with_params:
    (component_loc:'loc ->
    'loc * ('loc, 'loc) Flow_ast.Statement.Block.t ->
    ('loc, 'loc) Flow_ast.Statement.ComponentDeclaration.Params.t ->
    unit
    ) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Statement.ComponentDeclaration.t ->
  ('loc, 'loc) Flow_ast.Statement.ComponentDeclaration.t

val declare_component :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  make_component_annot_collector_and_default_remover:
    (unit -> (('loc, 'loc) Flow_ast.Type.t list, 'loc) Flow_ast_visitor.visitor) ->
  hoist_annotations:((unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Statement.DeclareComponent.t ->
  ('loc, 'loc) Flow_ast.Statement.DeclareComponent.t

val component_type :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Type.Component.t ->
  ('loc, 'loc) Flow_ast.Type.Component.t

val object_mapped_type_property :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Type.Object.MappedType.t ->
  ('loc, 'loc) Flow_ast.Type.Object.MappedType.t

val component_body_with_params :
  enable_enums:bool ->
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  make_component_annot_collector_and_default_remover:
    (unit -> (('loc, 'loc) Flow_ast.Type.t list, 'loc) Flow_ast_visitor.visitor) ->
  with_bindings:('loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  'loc * ('loc, 'loc) Flow_ast.Statement.Block.t ->
  ('loc, 'loc) Flow_ast.Statement.ComponentDeclaration.Params.t ->
  unit

val import_named_specifier :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  import_kind:Flow_ast.Statement.ImportDeclaration.import_kind ->
  ('loc, 'loc) Flow_ast.Statement.ImportDeclaration.named_specifier ->
  ('loc, 'loc) Flow_ast.Statement.ImportDeclaration.named_specifier

val scoped_infer_type_params :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:('loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  binding_infer_type_identifier:
    (('loc, 'loc) Flow_ast.Identifier.t -> ('loc, 'loc) Flow_ast.Identifier.t) ->
  in_tparam_scope:(unit -> unit) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Type.TypeParam.t list ->
  unit

val conditional_type :
  'loc #Flow_ast_mapper.mapper ->
  extends_in_infer_type:(('loc, 'loc) Flow_ast.Type.t -> ('loc, 'loc) Flow_ast.Type.t) ->
  scoped_infer_type_params:
    (in_tparam_scope:(unit -> unit) -> 'loc -> ('loc, 'loc) Flow_ast.Type.TypeParam.t list -> unit) ->
  ('loc, 'loc) Flow_ast.Type.Conditional.t ->
  ('loc, 'loc) Flow_ast.Type.Conditional.t

val class_expression :
  with_bindings:('loc -> lexical:bool -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  on_cls:('loc -> ('loc, 'loc) Flow_ast.Class.t -> unit) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Class.t ->
  ('loc, 'loc) Flow_ast.Class.t

val declare_class :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Statement.DeclareClass.t ->
  ('loc, 'loc) Flow_ast.Statement.DeclareClass.t

val declare_function :
  'loc #Flow_ast_mapper.mapper ->
  super_declare_function:
    ('loc ->
    ('loc, 'loc) Flow_ast.Statement.DeclareFunction.t ->
    ('loc, 'loc) Flow_ast.Statement.DeclareFunction.t
    ) ->
  hoist_annotations:((unit -> unit) -> unit) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Statement.DeclareFunction.t ->
  ('loc, 'loc) Flow_ast.Statement.DeclareFunction.t

val lambda :
  enable_enums:bool ->
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  pattern_with_toplevel_annot_removed:
    (('loc, 'loc) Flow_ast.Pattern.t ->
    ('loc, 'loc) Flow_ast.Type.annotation_or_hint * ('loc, 'loc) Flow_ast.Pattern.t
    ) ->
  ('loc, 'loc) Flow_ast.Function.Params.t ->
  ('loc, 'loc) Flow_ast.Function.ReturnAnnot.t ->
  ('loc, 'loc) Flow_ast.Type.Predicate.t option ->
  ('loc, 'loc) Flow_ast.Function.body ->
  unit

val class_ :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  class_identifier_opt:(class_loc:'loc -> ('loc, 'loc) Flow_ast.Identifier.t option -> unit) ->
  'loc ->
  ('loc, 'loc) Flow_ast.Class.t ->
  ('loc, 'loc) Flow_ast.Class.t

val record_declaration :
  with_types:bool ->
  'loc #Flow_ast_mapper.mapper ->
  with_bindings:(?lexical:bool -> 'loc -> 'loc Bindings.t -> (unit -> unit) -> unit) ->
  ('loc, 'loc) Flow_ast.Statement.RecordDeclaration.t ->
  ('loc, 'loc) Flow_ast.Statement.RecordDeclaration.t

module Make (L : Loc_sig.S) (Api : Scope_api_sig.S with module L = L) :
  Scope_builder_sig.S with module L = L and module Api = Api

module With_Loc :
  Scope_builder_sig.S with module L = Loc_sig.LocS and module Api = Scope_api.With_Loc

module With_ALoc :
  Scope_builder_sig.S with module L = Loc_sig.ALocS and module Api = Scope_api.With_ALoc

module With_ILoc :
  Scope_builder_sig.S with module L = Loc_sig.ILocS and module Api = Scope_api.With_ILoc

include module type of With_Loc
