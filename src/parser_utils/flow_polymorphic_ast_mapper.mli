(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

class virtual ['M, 'T, 'N, 'U] mapper :
  object
    method array : ('M, 'T) Ast.Expression.Array.t -> ('N, 'U) Ast.Expression.Array.t

    method arrow_function : ('M, 'T) Ast.Function.t -> ('N, 'U) Ast.Function.t

    method assignment :
      ('M, 'T) Ast.Expression.Assignment.t -> ('N, 'U) Ast.Expression.Assignment.t

    method assignment_pattern : ('M, 'T) Flow_ast.Pattern.t -> ('N, 'U) Ast.Pattern.t

    method binary : ('M, 'T) Ast.Expression.Binary.t -> ('N, 'U) Ast.Expression.Binary.t

    method binding_pattern :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.t ->
      ('N, 'U) Ast.Pattern.t

    method block : ('M, 'T) Ast.Statement.Block.t -> ('N, 'U) Ast.Statement.Block.t

    method break : 'M Ast.Statement.Break.t -> 'N Ast.Statement.Break.t

    method call : 'T -> ('M, 'T) Ast.Expression.Call.t -> ('N, 'U) Ast.Expression.Call.t

    method catch_clause :
      ('M, 'T) Ast.Statement.Try.CatchClause.t' -> ('N, 'U) Ast.Statement.Try.CatchClause.t'

    method catch_clause_pattern : ('M, 'T) Flow_ast.Pattern.t -> ('N, 'U) Ast.Pattern.t

    method class_ : ('M, 'T) Ast.Class.t -> ('N, 'U) Ast.Class.t

    method class_body : ('M, 'T) Flow_ast.Class.Body.t -> ('N, 'U) Ast.Class.Body.t

    method class_decorator : ('M, 'T) Ast.Class.Decorator.t -> ('N, 'U) Ast.Class.Decorator.t

    method class_element : ('M, 'T) Ast.Class.Body.element -> ('N, 'U) Ast.Class.Body.element

    method class_extends : ('M, 'T) Ast.Class.Extends.t -> ('N, 'U) Ast.Class.Extends.t

    method class_identifier : ('M, 'T) Ast.Identifier.t -> ('N, 'U) Ast.Identifier.t

    method class_implements :
      ('M, 'T) Flow_ast.Class.Implements.t -> ('N, 'U) Ast.Class.Implements.t

    method class_method : ('M, 'T) Ast.Class.Method.t' -> ('N, 'U) Ast.Class.Method.t'

    method class_private_field :
      ('M, 'T) Ast.Class.PrivateField.t' -> ('N, 'U) Ast.Class.PrivateField.t'

    method class_property : ('M, 'T) Ast.Class.Property.t' -> ('N, 'U) Ast.Class.Property.t'

    method comment : 'M Ast.Comment.t -> 'N Ast.Comment.t

    method t_comment : 'T Ast.Comment.t -> 'U Ast.Comment.t

    method syntax : 'internal. ('M, 'internal) Ast.Syntax.t -> ('N, 'internal) Ast.Syntax.t

    method comprehension :
      ('M, 'T) Ast.Expression.Comprehension.t -> ('N, 'U) Ast.Expression.Comprehension.t

    method comprehension_block :
      ('M, 'T) Ast.Expression.Comprehension.Block.t ->
      ('N, 'U) Ast.Expression.Comprehension.Block.t

    method conditional :
      ('M, 'T) Ast.Expression.Conditional.t -> ('N, 'U) Ast.Expression.Conditional.t

    method continue : 'M Ast.Statement.Continue.t -> 'N Ast.Statement.Continue.t

    method debugger : unit -> unit

    method declare_class :
      ('M, 'T) Ast.Statement.DeclareClass.t -> ('N, 'U) Ast.Statement.DeclareClass.t

    method declare_export_declaration :
      'M ->
      ('M, 'T) Ast.Statement.DeclareExportDeclaration.t ->
      ('N, 'U) Ast.Statement.DeclareExportDeclaration.t

    method declare_export_declaration_decl :
      ('M, 'T) Ast.Statement.DeclareExportDeclaration.declaration ->
      ('N, 'U) Ast.Statement.DeclareExportDeclaration.declaration

    method declare_function :
      ('M, 'T) Ast.Statement.DeclareFunction.t -> ('N, 'U) Ast.Statement.DeclareFunction.t

    method declare_interface :
      ('M, 'T) Ast.Statement.Interface.t -> ('N, 'U) Ast.Statement.Interface.t

    method declare_module :
      'M -> ('M, 'T) Ast.Statement.DeclareModule.t -> ('N, 'U) Ast.Statement.DeclareModule.t

    method declare_module_exports :
      'M -> ('M, 'T) Flow_ast.Type.annotation -> ('N, 'U) Ast.Type.annotation

    method declare_opaque_type :
      ('M, 'T) Ast.Statement.OpaqueType.t -> ('N, 'U) Ast.Statement.OpaqueType.t

    method declare_type_alias :
      ('M, 'T) Ast.Statement.TypeAlias.t -> ('N, 'U) Ast.Statement.TypeAlias.t

    method declare_variable :
      ('M, 'T) Ast.Statement.DeclareVariable.t -> ('N, 'U) Ast.Statement.DeclareVariable.t

    method do_while : ('M, 'T) Ast.Statement.DoWhile.t -> ('N, 'U) Ast.Statement.DoWhile.t

    method empty : unit -> unit

    method enum_declaration :
      ('M, 'T) Ast.Statement.EnumDeclaration.t -> ('N, 'U) Ast.Statement.EnumDeclaration.t

    method enum_boolean_body :
      'M Ast.Statement.EnumDeclaration.BooleanBody.t ->
      'N Ast.Statement.EnumDeclaration.BooleanBody.t

    method enum_number_body :
      'M Ast.Statement.EnumDeclaration.NumberBody.t ->
      'N Ast.Statement.EnumDeclaration.NumberBody.t

    method enum_string_body :
      'M Ast.Statement.EnumDeclaration.StringBody.t ->
      'N Ast.Statement.EnumDeclaration.StringBody.t

    method enum_symbol_body :
      'M Ast.Statement.EnumDeclaration.SymbolBody.t ->
      'N Ast.Statement.EnumDeclaration.SymbolBody.t

    method enum_defaulted_member :
      'M Ast.Statement.EnumDeclaration.DefaultedMember.t ->
      'N Ast.Statement.EnumDeclaration.DefaultedMember.t

    method enum_boolean_member :
      (bool, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t ->
      (bool, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t

    method enum_number_member :
      (Ast.NumberLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t ->
      (Ast.NumberLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t

    method enum_string_member :
      (Ast.StringLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t ->
      (Ast.StringLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t

    method explicit_or_implicit :
      ('M, 'T) Ast.Expression.TypeParameterInstantiation.type_parameter_instantiation ->
      ('N, 'U) Ast.Expression.TypeParameterInstantiation.type_parameter_instantiation

    method export_default_declaration :
      'M ->
      ('M, 'T) Ast.Statement.ExportDefaultDeclaration.t ->
      ('N, 'U) Ast.Statement.ExportDefaultDeclaration.t

    method export_default_declaration_decl :
      ('M, 'T) Ast.Statement.ExportDefaultDeclaration.declaration ->
      ('N, 'U) Ast.Statement.ExportDefaultDeclaration.declaration

    method export_named_declaration :
      'M ->
      ('M, 'T) Ast.Statement.ExportNamedDeclaration.t ->
      ('N, 'U) Ast.Statement.ExportNamedDeclaration.t

    method export_named_specifier :
      'M Ast.Statement.ExportNamedDeclaration.specifier ->
      'N Ast.Statement.ExportNamedDeclaration.specifier

    method export_specifier :
      'M Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t ->
      'N Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t

    method expression : ('M, 'T) Ast.Expression.t -> ('N, 'U) Ast.Expression.t

    method expression_or_spread :
      ('M, 'T) Ast.Expression.expression_or_spread -> ('N, 'U) Ast.Expression.expression_or_spread

    method expression_statement :
      ('M, 'T) Ast.Statement.Expression.t -> ('N, 'U) Ast.Statement.Expression.t

    method for_in_assignment_pattern : ('M, 'T) Flow_ast.Pattern.t -> ('N, 'U) Ast.Pattern.t

    method for_in_statement : ('M, 'T) Ast.Statement.ForIn.t -> ('N, 'U) Ast.Statement.ForIn.t

    method for_in_statement_lhs :
      ('M, 'T) Ast.Statement.ForIn.left -> ('N, 'U) Ast.Statement.ForIn.left

    method for_of_assignment_pattern : ('M, 'T) Flow_ast.Pattern.t -> ('N, 'U) Ast.Pattern.t

    method for_of_statement : ('M, 'T) Ast.Statement.ForOf.t -> ('N, 'U) Ast.Statement.ForOf.t

    method for_of_statement_lhs :
      ('M, 'T) Ast.Statement.ForOf.left -> ('N, 'U) Ast.Statement.ForOf.left

    method for_statement : ('M, 'T) Ast.Statement.For.t -> ('N, 'U) Ast.Statement.For.t

    method for_statement_init : ('M, 'T) Ast.Statement.For.init -> ('N, 'U) Ast.Statement.For.init

    method function_ : ('M, 'T) Ast.Function.t -> ('N, 'U) Ast.Function.t

    method function_body : ('M, 'T) Ast.Function.body -> ('N, 'U) Ast.Function.body

    method function_declaration : ('M, 'T) Ast.Function.t -> ('N, 'U) Ast.Function.t

    method function_expression : ('M, 'T) Ast.Function.t -> ('N, 'U) Ast.Function.t

    method function_identifier : ('M, 'M) Flow_ast.Identifier.t -> ('N, 'N) Ast.Identifier.t

    method function_param : ('M, 'T) Flow_ast.Function.Param.t -> ('N, 'U) Ast.Function.Param.t

    method function_params : ('M, 'T) Flow_ast.Function.Params.t -> ('N, 'U) Ast.Function.Params.t

    method function_param_pattern : ('M, 'T) Flow_ast.Pattern.t -> ('N, 'U) Ast.Pattern.t

    method function_param_type :
      ('M, 'T) Ast.Type.Function.Param.t -> ('N, 'U) Ast.Type.Function.Param.t

    method function_rest_param :
      ('M, 'T) Flow_ast.Function.RestParam.t -> ('N, 'U) Ast.Function.RestParam.t

    method function_rest_param_type :
      ('M, 'T) Ast.Type.Function.RestParam.t -> ('N, 'U) Ast.Type.Function.RestParam.t

    method function_type : ('M, 'T) Ast.Type.Function.t -> ('N, 'U) Ast.Type.Function.t

    method generator : ('M, 'T) Ast.Expression.Generator.t -> ('N, 'U) Ast.Expression.Generator.t

    method generic_identifier_type :
      ('M, 'T) Ast.Type.Generic.Identifier.t -> ('N, 'U) Ast.Type.Generic.Identifier.t

    method generic_type : ('M, 'T) Ast.Type.Generic.t -> ('N, 'U) Ast.Type.Generic.t

    method identifier : ('M, 'M) Ast.Identifier.t -> ('N, 'N) Ast.Identifier.t

    method if_consequent_statement :
      has_else:bool -> ('M, 'T) Ast.Statement.t -> ('N, 'U) Ast.Statement.t

    method if_statement : ('M, 'T) Ast.Statement.If.t -> ('N, 'U) Ast.Statement.If.t

    method implicit : 'T -> 'U

    method import : 'T -> ('M, 'T) Ast.Expression.t -> ('N, 'U) Ast.Expression.t

    method import_declaration :
      'M ->
      ('M, 'T) Ast.Statement.ImportDeclaration.t ->
      ('N, 'U) Ast.Statement.ImportDeclaration.t

    method import_default_specifier : ('M, 'T) Ast.Identifier.t -> ('N, 'U) Ast.Identifier.t

    method import_named_specifier :
      ('M, 'T) Ast.Statement.ImportDeclaration.named_specifier ->
      ('N, 'U) Ast.Statement.ImportDeclaration.named_specifier

    method import_namespace_specifier : ('M, 'M) Ast.Identifier.t -> ('N, 'N) Ast.Identifier.t

    method import_specifier :
      ('M, 'T) Ast.Statement.ImportDeclaration.specifier ->
      ('N, 'U) Ast.Statement.ImportDeclaration.specifier

    method interface : ('M, 'T) Ast.Statement.Interface.t -> ('N, 'U) Ast.Statement.Interface.t

    method interface_declaration :
      ('M, 'T) Ast.Statement.Interface.t -> ('N, 'U) Ast.Statement.Interface.t

    method interface_type : ('M, 'T) Ast.Type.Interface.t -> ('N, 'U) Ast.Type.Interface.t

    method jsx_attribute : ('M, 'T) Flow_ast.JSX.Attribute.t -> ('N, 'U) Ast.JSX.Attribute.t

    method jsx_attribute_value :
      ('M, 'T) Ast.JSX.Attribute.value -> ('N, 'U) Ast.JSX.Attribute.value

    method jsx_children : 'M * ('M, 'T) Ast.JSX.child list -> 'N * ('N, 'U) Ast.JSX.child list

    method jsx_child : ('M, 'T) Ast.JSX.child -> ('N, 'U) Ast.JSX.child

    method jsx_closing_element : ('M, 'T) Ast.JSX.Closing.t -> ('N, 'U) Ast.JSX.Closing.t

    method jsx_element : ('M, 'T) Ast.JSX.element -> ('N, 'U) Ast.JSX.element

    method jsx_expression :
      ('M, 'T) Ast.JSX.ExpressionContainer.t -> ('N, 'U) Ast.JSX.ExpressionContainer.t

    method jsx_fragment : ('M, 'T) Ast.JSX.fragment -> ('N, 'U) Ast.JSX.fragment

    method jsx_identifier : 'T Flow_ast.JSX.Identifier.t -> 'U Ast.JSX.Identifier.t

    method jsx_member_expression :
      ('M, 'T) Ast.JSX.MemberExpression.t -> ('N, 'U) Ast.JSX.MemberExpression.t

    method jsx_member_expression_object :
      ('M, 'T) Ast.JSX.MemberExpression._object -> ('N, 'U) Ast.JSX.MemberExpression._object

    method jsx_name : ('M, 'T) Ast.JSX.name -> ('N, 'U) Ast.JSX.name

    method jsx_namespaced_name :
      ('M, 'T) Flow_ast.JSX.NamespacedName.t -> ('N, 'U) Ast.JSX.NamespacedName.t

    method jsx_opening_attribute :
      ('M, 'T) Ast.JSX.Opening.attribute -> ('N, 'U) Ast.JSX.Opening.attribute

    method jsx_opening_element : ('M, 'T) Ast.JSX.Opening.t -> ('N, 'U) Ast.JSX.Opening.t

    method jsx_spread_attribute :
      ('M, 'T) Ast.JSX.SpreadAttribute.t' -> ('N, 'U) Ast.JSX.SpreadAttribute.t'

    method label_identifier : ('M, 'M) Ast.Identifier.t -> ('N, 'N) Ast.Identifier.t

    method labeled_statement : ('M, 'T) Ast.Statement.Labeled.t -> ('N, 'U) Ast.Statement.Labeled.t

    method literal : 'M Ast.Literal.t -> 'N Ast.Literal.t

    method logical : ('M, 'T) Ast.Expression.Logical.t -> ('N, 'U) Ast.Expression.Logical.t

    method member : ('M, 'T) Ast.Expression.Member.t -> ('N, 'U) Ast.Expression.Member.t

    method member_private_name : 'M Flow_ast.PrivateName.t -> 'N Ast.PrivateName.t

    method member_property :
      ('M, 'T) Ast.Expression.Member.property -> ('N, 'U) Ast.Expression.Member.property

    method member_property_expression : ('M, 'T) Flow_ast.Expression.t -> ('N, 'U) Ast.Expression.t

    method member_property_identifier : ('M, 'T) Ast.Identifier.t -> ('N, 'U) Ast.Identifier.t

    method meta_property : 'M Ast.Expression.MetaProperty.t -> 'N Ast.Expression.MetaProperty.t

    method new_ : ('M, 'T) Ast.Expression.New.t -> ('N, 'U) Ast.Expression.New.t

    method object_ : ('M, 'T) Ast.Expression.Object.t -> ('N, 'U) Ast.Expression.Object.t

    method object_indexer_type :
      ('M, 'T) Ast.Type.Object.Indexer.t -> ('N, 'U) Ast.Type.Object.Indexer.t

    method object_internal_slot_type :
      ('M, 'T) Ast.Type.Object.InternalSlot.t -> ('N, 'U) Ast.Type.Object.InternalSlot.t

    method object_key :
      ('M, 'T) Ast.Expression.Object.Property.key -> ('N, 'U) Ast.Expression.Object.Property.key

    method object_key_identifier : ('M, 'T) Ast.Identifier.t -> ('N, 'U) Ast.Identifier.t

    method object_property :
      ('M, 'T) Ast.Expression.Object.Property.t -> ('N, 'U) Ast.Expression.Object.Property.t

    method object_property_or_spread_property :
      ('M, 'T) Ast.Expression.Object.property -> ('N, 'U) Ast.Expression.Object.property

    method object_property_type :
      ('M, 'T) Ast.Type.Object.Property.t -> ('N, 'U) Ast.Type.Object.Property.t

    method object_property_value_type :
      ('M, 'T) Ast.Type.Object.Property.value -> ('N, 'U) Ast.Type.Object.Property.value

    method object_type : ('M, 'T) Ast.Type.Object.t -> ('N, 'U) Ast.Type.Object.t

    method object_type_property :
      ('M, 'T) Ast.Type.Object.property -> ('N, 'U) Ast.Type.Object.property

    method virtual on_loc_annot : 'M -> 'N

    method virtual on_type_annot : 'T -> 'U

    method opaque_type : ('M, 'T) Ast.Statement.OpaqueType.t -> ('N, 'U) Ast.Statement.OpaqueType.t

    method optional_call :
      'T -> ('M, 'T) Ast.Expression.OptionalCall.t -> ('N, 'U) Ast.Expression.OptionalCall.t

    method optional_member :
      ('M, 'T) Ast.Expression.OptionalMember.t -> ('N, 'U) Ast.Expression.OptionalMember.t

    method pattern :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Flow_ast.Pattern.t ->
      ('N, 'U) Ast.Pattern.t

    method pattern_array_e :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.Array.element ->
      ('N, 'U) Ast.Pattern.Array.element

    method pattern_array_element :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.Array.Element.t' ->
      ('N, 'U) Ast.Pattern.Array.Element.t'

    method pattern_array_element_pattern :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Flow_ast.Pattern.t ->
      ('N, 'U) Ast.Pattern.t

    method pattern_array_rest_element :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.Array.RestElement.t' ->
      ('N, 'U) Ast.Pattern.Array.RestElement.t'

    method pattern_array_rest_element_pattern :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Flow_ast.Pattern.t ->
      ('N, 'U) Ast.Pattern.t

    method pattern_assignment_pattern :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.t ->
      ('N, 'U) Ast.Pattern.t

    method pattern_expression : ('M, 'T) Flow_ast.Expression.t -> ('N, 'U) Ast.Expression.t

    method pattern_identifier :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'M) Ast.Identifier.t ->
      ('N, 'N) Ast.Identifier.t

    method pattern_literal :
      ?kind:Ast.Statement.VariableDeclaration.kind -> 'M Ast.Literal.t -> 'N Ast.Literal.t

    method pattern_object_p :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.Object.property ->
      ('N, 'U) Ast.Pattern.Object.property

    method pattern_object_property :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.Object.Property.t' ->
      ('N, 'U) Ast.Pattern.Object.Property.t'

    method pattern_object_property_computed_key :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Flow_ast.Expression.t ->
      ('N, 'U) Ast.Expression.t

    method pattern_object_property_identifier_key :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Identifier.t ->
      ('N, 'U) Ast.Identifier.t

    method pattern_object_property_key :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.Object.Property.key ->
      ('N, 'U) Ast.Pattern.Object.Property.key

    method pattern_object_property_literal_key :
      ?kind:Ast.Statement.VariableDeclaration.kind -> 'M Ast.Literal.t -> 'N Ast.Literal.t

    method pattern_object_property_pattern :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Flow_ast.Pattern.t ->
      ('N, 'U) Ast.Pattern.t

    method pattern_object_rest_property :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.Object.RestProperty.t' ->
      ('N, 'U) Ast.Pattern.Object.RestProperty.t'

    method pattern_object_rest_property_pattern :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Flow_ast.Pattern.t ->
      ('N, 'U) Ast.Pattern.t

    method predicate_expression : ('M, 'T) Flow_ast.Expression.t -> ('N, 'U) Ast.Expression.t

    method private_name : 'M Flow_ast.PrivateName.t -> 'N Ast.PrivateName.t

    method program : ('M, 'T) Ast.program -> ('N, 'U) Ast.program

    method return : ('M, 'T) Ast.Statement.Return.t -> ('N, 'U) Ast.Statement.Return.t

    method sequence : ('M, 'T) Ast.Expression.Sequence.t -> ('N, 'U) Ast.Expression.Sequence.t

    method spread_element :
      ('M, 'T) Ast.Expression.SpreadElement.t -> ('N, 'U) Ast.Expression.SpreadElement.t

    method spread_property :
      ('M, 'T) Ast.Expression.Object.SpreadProperty.t ->
      ('N, 'U) Ast.Expression.Object.SpreadProperty.t

    method statement : ('M, 'T) Ast.Statement.t -> ('N, 'U) Ast.Statement.t

    method statement_list : ('M, 'T) Flow_ast.Statement.t list -> ('N, 'U) Ast.Statement.t list

    method switch : ('M, 'T) Ast.Statement.Switch.t -> ('N, 'U) Ast.Statement.Switch.t

    method switch_case :
      ('M, 'T) Ast.Statement.Switch.Case.t' -> ('N, 'U) Ast.Statement.Switch.Case.t'

    method t_function_identifier : ('M, 'T) Ast.Identifier.t -> ('N, 'U) Ast.Identifier.t

    method t_identifier : ('M, 'T) Ast.Identifier.t -> ('N, 'U) Ast.Identifier.t

    method t_pattern_identifier :
      ?kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Identifier.t ->
      ('N, 'U) Ast.Identifier.t

    method tagged_template :
      ('M, 'T) Ast.Expression.TaggedTemplate.t -> ('N, 'U) Ast.Expression.TaggedTemplate.t

    method template_literal :
      ('M, 'T) Ast.Expression.TemplateLiteral.t -> ('N, 'U) Ast.Expression.TemplateLiteral.t

    method template_literal_element :
      'M Ast.Expression.TemplateLiteral.Element.t -> 'N Ast.Expression.TemplateLiteral.Element.t

    method throw : ('M, 'T) Ast.Statement.Throw.t -> ('N, 'U) Ast.Statement.Throw.t

    method toplevel_statement_list : ('M, 'T) Ast.Statement.t list -> ('N, 'U) Ast.Statement.t list

    method try_catch : ('M, 'T) Ast.Statement.Try.t -> ('N, 'U) Ast.Statement.Try.t

    method type_ : ('M, 'T) Flow_ast.Type.t -> ('N, 'U) Ast.Type.t

    method type_alias : ('M, 'T) Ast.Statement.TypeAlias.t -> ('N, 'U) Ast.Statement.TypeAlias.t

    method type_annotation : ('M, 'T) Flow_ast.Type.annotation -> 'N * ('N, 'U) Flow_ast.Type.t

    method type_annotation_hint :
      ('M, 'T) Ast.Type.annotation_or_hint -> ('N, 'U) Ast.Type.annotation_or_hint

    method type_cast : ('M, 'T) Ast.Expression.TypeCast.t -> ('N, 'U) Ast.Expression.TypeCast.t

    method type_parameter_declaration_opt :
      ('M, 'T) Ast.Type.ParameterDeclaration.t option ->
      (('N, 'U) Ast.Type.ParameterDeclaration.t option -> 'a) ->
      'a

    method type_parameter_declaration_type_param :
      ('M, 'T) Ast.Type.ParameterDeclaration.TypeParam.t ->
      ('N, 'U) Ast.Type.ParameterDeclaration.TypeParam.t

    method type_parameter_instantiation :
      ('M, 'T) Flow_ast.Type.ParameterInstantiation.t -> ('N, 'U) Ast.Type.ParameterInstantiation.t

    method type_parameter_instantiation_with_implicit :
      ('M, 'T) Flow_ast.Expression.TypeParameterInstantiation.t ->
      ('N, 'U) Ast.Expression.TypeParameterInstantiation.t

    method type_predicate : ('M, 'T) Flow_ast.Type.Predicate.t -> ('N, 'U) Ast.Type.Predicate.t

    method unary_expression : ('M, 'T) Ast.Expression.Unary.t -> ('N, 'U) Ast.Expression.Unary.t

    method update_expression : ('M, 'T) Ast.Expression.Update.t -> ('N, 'U) Ast.Expression.Update.t

    method variable_declaration :
      ('M, 'T) Ast.Statement.VariableDeclaration.t -> ('N, 'U) Ast.Statement.VariableDeclaration.t

    method variable_declarator :
      kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Statement.VariableDeclaration.Declarator.t ->
      ('N, 'U) Ast.Statement.VariableDeclaration.Declarator.t

    method variable_declarator_pattern :
      kind:Ast.Statement.VariableDeclaration.kind ->
      ('M, 'T) Ast.Pattern.t ->
      ('N, 'U) Ast.Pattern.t

    method while_ : ('M, 'T) Ast.Statement.While.t -> ('N, 'U) Ast.Statement.While.t

    method with_ : ('M, 'T) Ast.Statement.With.t -> ('N, 'U) Ast.Statement.With.t

    method yield : ('M, 'T) Ast.Expression.Yield.t -> ('N, 'U) Ast.Expression.Yield.t
  end
