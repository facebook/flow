(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type schema_node = {
  kind_name : string;
  type_name : string;
  description : string;
  fields : string list
}

type transformation =
{
  pattern : string;
  func : schema_node -> string
}

type template_file =
{
  filename : string;
  template : string;
  transformations : transformation list
}

let from_list l =
  match l with
  | kind_name :: type_name :: description :: fields ->
    { kind_name; type_name; description; fields }
  | _ -> failwith "bad schema"

let schema = List.map from_list [
  [ "ScriptHeader";
    "script_header";
    "header";
    "header_less_than";
    "header_question";
    "header_language" ];
  [ "Script";
    "script";
    "script";
    "script_header";
    "script_declarations" ];
  [ "SimpleTypeSpecifier";
    "simple_type_specifier";
    "simple_type_specifier";
    "simple_type_specifier" ];
  [ "LiteralExpression";
    "literal_expression";
    "literal";
    "literal_expression" ];
  [ "VariableExpression";
    "variable_expression";
    "variable";
    "variable_expression" ];
  [ "QualifiedNameExpression";
    "qualified_name_expression";
    "qualified_name";
    "qualified_name_expression" ];
  [ "PipeVariableExpression";
    "pipe_variable_expression";
    "pipe_variable";
    "pipe_variable_expression" ];
  [ "EnumDeclaration";
    "enum_declaration";
    "enum_declaration";
    "enum_keyword";
    "enum_name";
    "enum_colon";
    "enum_base";
    "enum_type";
    "enum_left_brace";
    "enum_enumerators";
    "enum_right_brace" ];
  [ "Enumerator";
    "enumerator";
    "enumerator";
    "enumerator_name";
    "enumerator_equal";
    "enumerator_value";
    "enumerator_semicolon" ];
  [ "AliasDeclaration";
    "alias_declaration";
    "alias_declaration";
    "alias_attribute_spec";
    "alias_keyword";
    "alias_name";
    "alias_generic_parameter";
    "alias_constraint";
    "alias_equal";
    "alias_type";
    "alias_semicolon" ];
  [ "PropertyDeclaration";
    "property_declaration";
    "property_declaration";
    "property_modifiers";
    "property_type";
    "property_declarators";
    "property_semicolon" ];
  [ "PropertyDeclarator";
    "property_declarator";
    "property_declarator";
    "property_name";
    "property_initializer" ];
  [ "NamespaceDeclaration";
    "namespace_declaration";
    "namespace_declaration";
    "namespace_keyword";
    "namespace_name";
    "namespace_body" ];
  [ "NamespaceBody";
    "namespace_body";
    "namespace_body";
    "namespace_left_brace";
    "namespace_declarations";
    "namespace_right_brace" ];
  [ "NamespaceUseDeclaration";
    "namespace_use_declaration";
    "namespace_use_declaration";
    "namespace_use_keyword";
    "namespace_use_kind";
    "namespace_use_clauses";
    "namespace_use_semicolon"];
  [ "NamespaceGroupUseDeclaration";
    "namespace_group_use_declaration";
    "namespace_group_use_declaration";
    "namespace_group_use_keyword";
    "namespace_group_use_kind";
    "namespace_group_use_prefix";
    "namespace_group_use_left_brace";
    "namespace_group_use_clauses";
    "namespace_group_use_right_brace";
    "namespace_group_use_semicolon" ];
  [ "NamespaceUseClause";
    "namespace_use_clause";
    "namespace_use_clause";
    "namespace_use_clause_kind";
    "namespace_use_name";
    "namespace_use_as";
    "namespace_use_alias" ];
  [ "FunctionDeclaration";
    "function_declaration";
    "function_declaration";
    "function_attribute_spec";
    "function_declaration_header";
    "function_body" ];
  [ "FunctionDeclarationHeader";
    "function_declaration_header";
    "function_declaration_header";
    "function_async";
    "function_keyword";
    "function_name";
    "function_type_parameter_list";
    "function_left_paren";
    "function_parameter_list";
    "function_right_paren";
    "function_colon";
    "function_type" ];
  [ "MethodishDeclaration";
    "methodish_declaration";
    "methodish_declaration";
    "methodish_attribute";
    "methodish_modifiers";
    "methodish_function_decl_header";
    "methodish_function_body";
    "methodish_semicolon" ];
  [ "ClassishDeclaration";
    "classish_declaration";
    "classish_declaration";
    "classish_attribute";
    "classish_modifiers";
    "classish_keyword";
    "classish_name";
    "classish_type_parameter_list";
    "classish_extends";
    "classish_extends_list";
    "classish_implements";
    "classish_implements_list";
    "classish_body" ];
  [ "ClassishBody";
    "classish_body";
    "classish_body";
    "classish_body_left_brace";
    "classish_body_elements";
    "classish_body_right_brace" ];
  [ "TraitUse";
    "trait_use";
    "trait_use";
    "trait_use_keyword";
    "trait_use_name_list";
    "trait_use_semicolon" ];
  [ "RequireClause";
    "require_clause";
    "require_clause";
    "require_keyword";
    "require_kind";
    "require_name";
    "require_semicolon" ];
  [ "ConstDeclaration";
    "const_declaration";
    "const_declaration";
    "const_abstract";
    "const_keyword";
    "const_type_specifier";
    "const_declarator_list";
    "const_semicolon" ];
  [ "ConstantDeclarator";
    "constant_declarator";
    "constant_declarator";
    "constant_declarator_name";
    "constant_declarator_initializer" ];
  [ "TypeConstDeclaration";
    "type_const_declaration";
    "type_const_declaration";
    "type_const_abstract";
    "type_const_keyword";
    "type_const_type_keyword";
    "type_const_name";
    "type_const_type_constraint";
    "type_const_equal";
    "type_const_type_specifier";
    "type_const_semicolon" ];
  [ "DecoratedExpression";
    "decorated_expression";
    "decorated_expression";
    "decorated_expression_decorator";
    "decorated_expression_expression" ];
  [ "ParameterDeclaration";
    "parameter_declaration";
    "parameter_declaration";
    "param_attribute";
    "param_visibility";
    "param_type";
    "param_name";
    "param_default" ];
  [ "AttributeSpecification";
    "attribute_specification";
    "attribute_specification";
    "attribute_spec_left_double_angle";
    "attribute_spec_attribute_list";
    "attribute_spec_right_double_angle" ];
  [ "Attribute";
    "attribute";
    "attribute";
    "attribute_name";
    "attribute_left_paren";
    "attribute_values";
    "attribute_right_paren" ];
  [ "InclusionExpression";
    "inclusion_expression";
    "inclusion_expression";
    "inclusion_require";
    "inclusion_filename" ];
  [ "InclusionDirective";
    "inclusion_directive";
    "inclusion_directive";
    "inclusion_expression";
    "inclusion_semicolon" ];
  [ "CompoundStatement";
    "compound_statement";
    "compound_statement";
    "compound_left_brace";
    "compound_statements";
    "compound_right_brace" ];
  [ "ExpressionStatement";
    "expression_statement";
    "expression_statement";
    "expr_statement_expr";
    "expr_statement_semicolon" ];
  [ "WhileStatement";
    "while_statement";
    "while_statement";
    "while_keyword";
    "while_left_paren";
    "while_condition";
    "while_right_paren";
    "while_body" ];
  [ "IfStatement";
    "if_statement";
    "if_statement";
    "if_keyword";
    "if_left_paren";
    "if_condition";
    "if_right_paren";
    "if_statement";
    "if_elseif_clauses";
    "if_else_clause" ];
  [ "ElseifClause";
    "elseif_clause";
    "elseif_clause";
    "elseif_keyword";
    "elseif_left_paren";
    "elseif_condition";
    "elseif_right_paren";
    "elseif_statement" ];
  [ "ElseClause";
    "else_clause";
    "else_clause";
    "else_keyword";
    "else_statement" ];
  [ "TryStatement";
    "try_statement";
    "try_statement";
    "try_keyword";
    "try_compound_statement";
    "catch_clauses";
    "finally_clause" ];
  [ "CatchClause";
    "catch_clause";
    "catch_clause";
    "catch_keyword";
    "catch_left_paren";
    "catch_type";
    "catch_variable";
    "catch_right_paren";
    "catch_compound_statement" ];
  [ "FinallyClause";
    "finally_clause";
    "finally_clause";
    "finally_keyword";
    "finally_compound_statement" ];
  [ "DoStatement";
    "do_statement";
    "do_statement";
    "do_keyword";
    "do_statement";
    "do_while_keyword";
    "do_left_paren";
    "do_condition_expr";
    "do_right_paren";
    "do_semicolon" ];
  [ "ForStatement";
    "for_statement";
    "for_statement";
    "for_keyword";
    "for_left_paren";
    "for_initializer_expr";
    "for_first_semicolon";
    "for_control_expr";
    "for_second_semicolon";
    "for_end_of_loop_expr";
    "for_right_paren";
    "for_statement" ];
  [ "ForeachStatement";
    "foreach_statement";
    "foreach_statement";
    "foreach_keyword";
    "foreach_left_paren";
    "foreach_collection_name";
    "foreach_await_opt";
    "foreach_as";
    "foreach_key_opt";
    "foreach_key_arrow_opt";
    "foreach_value";
    "foreach_right_paren";
    "foreach_statement" ];
  [ "SwitchStatement";
    "switch_statement";
    "switch_statement";
    "switch_keyword";
    "switch_left_paren";
    "switch_expr";
    "switch_right_paren";
    "switch_compound_statement" ];
  [ "CaseStatement";
    "case_statement";
    "case_statement";
    "case_keyword";
    "case_expr";
    "case_colon";
    "case_stmt" ];
  [ "DefaultStatement";
    "default_statement";
    "default_statement";
    "default_keyword";
    "default_colon";
    "default_stmt" ];
  [ "ReturnStatement";
    "return_statement";
    "return_statement";
    "return_keyword";
    "return_expr";
    "return_semicolon" ];
  [ "ThrowStatement";
    "throw_statement";
    "throw_statement";
    "throw_keyword";
    "throw_expr";
    "throw_semicolon" ];
  [ "BreakStatement";
    "break_statement";
    "break_statement";
    "break_keyword";
    "break_level";
    "break_semicolon" ];
  [ "ContinueStatement";
    "continue_statement";
    "continue_statement";
    "continue_keyword";
    "continue_level";
    "continue_semicolon" ];
  [ "FunctionStaticStatement";
    "function_static_statement";
    "function_static_statement";
    "static_static";
    "static_declarations";
    "static_semicolon" ];
  [ "StaticDeclarator";
    "static_declarator";
    "static_declarator";
    "static_name";
    "static_initializer" ];
  [ "EchoStatement";
    "echo_statement";
    "echo_statement";
    "echo_keyword";
    "echo_expression_list";
    "echo_semicolon" ];
  [ "SimpleInitializer";
    "simple_initializer";
    "simple_initializer";
    "simple_initializer_equal";
    "simple_initializer_value" ];
  [ "AnonymousFunction";
    "anonymous_function";
    "anonymous_function";
    "anonymous_async";
    "anonymous_function";
    "anonymous_left_paren";
    "anonymous_parameter_list";
    "anonymous_right_paren";
    "anonymous_colon";
    "anonymous_type";
    "anonymous_use";
    "anonymous_body" ];
  [ "AnonymousFunctionUseClause";
    "anonymous_function_use_clause";
    "anonymous_function_use_clause";
    "anonymous_use_keyword";
    "anonymous_use_left_paren";
    "anonymous_use_variables";
    "anonymous_use_right_paren" ];
  [ "LambdaExpression";
    "lambda_expression";
    "lambda_expression";
    "lambda_async";
    "lambda_signature";
    "lambda_arrow";
    "lambda_body" ];
  [ "LambdaSignature";
    "lambda_signature";
    "lambda_signature";
    "lambda_left_paren";
    "lambda_parameter_list";
    "lambda_right_paren";
    "lambda_colon";
    "lambda_type" ];
  [ "CastExpression";
    "cast_expression";
    "cast_expression";
    "cast_left_paren";
    "cast_type";
    "cast_right_paren";
    "cast_operand" ];
  [ "ScopeResolutionExpression";
    "scope_resolution_expression";
    "scope_resolution_expression";
    "scope_resolution_qualifier";
    "scope_resolution_operator";
    "scope_resolution_name" ];
  [ "MemberSelectionExpression";
    "member_selection_expression";
    "member_selection_expression";
    "member_object";
    "member_operator";
    "member_name" ];
  [ "SafeMemberSelectionExpression";
    "safe_member_selection_expression";
    "safe_member_selection_expression";
    "safe_member_object";
    "safe_member_operator";
    "safe_member_name" ];
  [ "YieldExpression";
    "yield_expression";
    "yield_expression";
    "yield_keyword";
    "yield_operand" ];
  [ "PrintExpression";
    "print_expression";
    "print_expression";
    "print_keyword";
    "print_expr" ];
  [ "PrefixUnaryExpression";
    "prefix_unary_expression";
    "prefix_unary_expression";
    "prefix_unary_operator";
    "prefix_unary_operand" ];
  [ "PostfixUnaryExpression";
    "postfix_unary_expression";
    "postfix_unary_expression";
    "postfix_unary_operand";
    "postfix_unary_operator" ];
  [ "BinaryExpression";
    "binary_expression";
    "binary_expression";
    "binary_left_operand";
    "binary_operator";
    "binary_right_operand" ];
  [ "InstanceofExpression";
    "instanceof_expression";
    "instanceof_expression";
    "instanceof_left_operand";
    "instanceof_operator";
    "instanceof_right_operand" ];
  [ "ConditionalExpression";
    "conditional_expression";
    "conditional_expression";
    "conditional_test";
    "conditional_question";
    "conditional_consequence";
    "conditional_colon";
    "conditional_alternative" ];
  [ "FunctionCallExpression";
    "function_call_expression";
    "function_call_expression";
    "function_call_receiver";
    "function_call_lparen";
    "function_call_arguments";
    "function_call_rparen" ];
  [ "ParenthesizedExpression";
    "parenthesized_expression";
    "parenthesized_expression";
    "paren_expr_left_paren";
    "paren_expr";
    "paren_expr_right_paren" ];
  [ "BracedExpression";
    "braced_expression";
    "braced_expression";
    "braced_expr_left_brace";
    "braced_expr";
    "braced_expr_right_brace" ];
  [ "ListExpression";
    "list_expression";
    "list_expression";
    "list_keyword";
    "list_left_paren";
    "list_members";
    "list_right_paren" ];
  [ "CollectionLiteralExpression";
    "collection_literal_expression";
    "collection_literal_expression";
    "collection_literal_name";
    "collection_literal_left_brace";
    "collection_literal_initialization_list";
    "collection_literal_right_brace" ];
  [ "ObjectCreationExpression";
    "object_creation_expression";
    "object_creation_expression";
    "object_creation_new";
    "object_creation_class";
    "object_creation_lparen";
    "object_creation_arguments";
    "object_creation_rparen" ];
  [ "ArrayCreationExpression";
    "array_creation_expression";
    "array_creation_expression";
    "array_creation_left_bracket";
    "array_creation_members";
    "array_creation_right_bracket" ];
  [ "ArrayIntrinsicExpression";
    "array_intrinsic_expression";
    "array_intrinsic_expression";
    "array_intrinsic_keyword";
    "array_intrinsic_left_paren";
    "array_intrinsic_members";
    "array_intrinsic_right_paren" ];
  [ "ElementInitializer";
    "element_initializer";
    "element_initializer";
    "element_key";
    "element_arrow";
    "element_value" ];
  [ "SubscriptExpression";
    "subscript_expression";
    "subscript_expression";
    "subscript_receiver";
    "subscript_left";
    "subscript_index";
    "subscript_right" ];
  [ "AwaitableCreationExpression";
    "awaitable_creation_expression";
    "awaitable_creation_expression";
    "awaitable_async";
    "awaitable_compound_statement" ];
  [ "XHPChildrenDeclaration";
    "xhp_children_declaration";
    "xhp_children_declaration";
    "xhp_children";
    "xhp_children_expression";
    "xhp_children_semicolon" ];
  [ "XHPCategoryDeclaration";
    "xhp_category_declaration";
    "xhp_category_declaration";
    "xhp_category";
    "xhp_category_list";
    "xhp_category_semicolon" ];
  [ "XHPEnumType";
    "xhp_enum_type";
    "xhp_enum_type";
    "xhp_enum_keyword";
    "xhp_enum_left_brace";
    "xhp_enum_values";
    "xhp_enum_right_brace" ];
  [ "XHPRequired";
    "xhp_required";
    "xhp_required";
    "xhp_required_at";
    "xhp_required" ];
  [ "XHPClassAttributeDeclaration";
    "xhp_class_attribute_declaration";
    "xhp_class_attribute_declaration";
    "xhp_attribute_keyword";
    "xhp_attribute_list";
    "xhp_attribute_semicolon" ];
  [ "XHPClassAttribute";
    "xhp_class_attribute";
    "xhp_class_attribute";
    "xhp_attribute_decl_type";
    "xhp_attribute_decl_name";
    "xhp_attribute_decl_initializer";
    "xhp_attribute_decl_required" ];
  [ "XHPAttribute";
    "xhp_attribute";
    "xhp_attribute";
    "xhp_attribute_name";
    "xhp_attribute_equal";
    "xhp_attribute_expr" ];
  [ "XHPOpen";
    "xhp_open";
    "xhp_open";
    "xhp_open_name";
    "xhp_open_attributes";
    "xhp_open_right_angle" ];
  [ "XHPExpression";
    "xhp_expression";
    "xhp_expression";
    "xhp_open";
    "xhp_body";
    "xhp_close" ];
  [ "XHPClose";
    "xhp_close";
    "xhp_close";
    "xhp_close_left_angle";
    "xhp_close_name";
    "xhp_close_right_angle" ];
  [ "TypeConstant";
    "type_constant";
    "type_constant";
    "type_constant_left_type";
    "type_constant_separator";
    "type_constant_right_type" ];
  [ "VectorTypeSpecifier";
    "vector_type_specifier";
    "vector_type_specifier";
    "vector_array";
    "vector_left_angle";
    "vector_type";
    "vector_right_angle" ];
  [ "TypeParameter";
    "type_parameter";
    "type_parameter";
    "type_variance_opt";
    "type_name";
    "type_constraint_list_opt" ];
  [ "TypeConstraint";
    "type_constraint";
    "type_constraint";
    "constraint_keyword";
    "constraint_type" ];
  [ "MapTypeSpecifier";
    "map_type_specifier";
    "map_type_specifier";
    "map_array";
    "map_left_angle";
    "map_key";
    "map_comma";
    "map_value";
    "map_right_angle" ];
  [ "ClosureTypeSpecifier";
    "closure_type_specifier";
    "closure_type_specifier";
    "closure_outer_left_paren";
    "closure_function";
    "closure_inner_left_paren";
    "closure_parameter_types";
    "closure_inner_right_paren";
    "closure_colon";
    "closure_return_type";
    "closure_outer_right_paren" ];
  [ "ClassnameTypeSpecifier";
    "classname_type_specifier";
    "classname_type_specifier";
    "classname_classname";
    "classname_left_angle";
    "classname_type";
    "classname_right_angle" ];
  [ "FieldSpecifier";
    "field_specifier";
    "field_specifier";
    "field_name";
    "field_arrow";
    "field_type" ];
  [ "FieldInitializer";
    "field_initializer";
    "field_initializer";
    "field_initializer_name";
    "field_initializer_arrow";
    "field_initializer_value" ];
  [ "ShapeTypeSpecifier";
    "shape_type_specifier";
    "shape_type_specifier";
    "shape_type_keyword";
    "shape_type_left_paren";
    "shape_type_fields";
    "shape_type_right_paren" ];
  [ "ShapeExpression";
    "shape_expression";
    "shape_expression";
    "shape_expression_keyword";
    "shape_expression_left_paren";
    "shape_expression_fields";
    "shape_expression_right_paren" ];
  [ "GenericTypeSpecifier";
    "generic_type_specifier";
    "generic_type_specifier";
    "generic_class_type";
    "generic_arguments" ];
  [ "NullableTypeSpecifier";
    "nullable_type_specifier";
    "nullable_type_specifier";
    "nullable_question";
    "nullable_type" ];
  [ "SoftTypeSpecifier";
    "soft_type_specifier";
    "soft_type_specifier";
    "soft_at";
    "soft_type" ];
  [ "TypeArguments";
    "type_arguments";
    "type_arguments";
    "type_arguments_left_angle";
    "type_arguments";
    "type_arguments_right_angle" ];
  [ "TypeParameters";
    "type_parameters";
    "type_parameters";
    "type_parameters_left_angle";
    "type_parameters";
    "type_parameters_right_angle" ];
  [ "TupleTypeSpecifier";
    "tuple_type_specifier";
    "tuple_type_specifier";
    "tuple_left_paren";
    "tuple_types";
    "tuple_right_paren" ];
  [ "Error";
    "error";
    "error";
    "error" ];
  [ "ListItem";
    "list_item";
    "list_item";
    "list_item";
    "list_separator" ]]

let map_and_concat f items =
  String.concat "" (List.map f items)

let transform_schema f =
  map_and_concat f schema

let replace pattern new_text source =
  Str.replace_first (Str.regexp pattern) new_text source

let generate_string template =
  let folder s x = replace x.pattern (transform_schema x.func) s in
  List.fold_left folder template.template template.transformations

let generate_file template =
  let file = open_out template.filename in
  let s = generate_string template in
  Printf.fprintf file "%s" s;
  close_out file

module GenerateFFSyntax = struct

  let to_parse_tree x =
    let mapper f = Printf.sprintf "      %s: t;\n" f in
    let fields = map_and_concat mapper x.fields in
    Printf.sprintf "    and %s = {\n%s    }\n"
      x.type_name fields

  let to_syntax x =
    Printf.sprintf "    | %s of %s\n"
      x.kind_name x.type_name

  let to_to_kind x =
    Printf.sprintf "      | %s _ ->\n        SyntaxKind.%s\n"
      x.kind_name x.kind_name

  let to_type_tests x =
    Printf.sprintf "    let is_%s node =\n      kind node = SyntaxKind.%s\n"
      x.type_name x.kind_name

  let to_children x =
    let mapper f = Printf.sprintf "        %s;\n" f in
    let fields = map_and_concat mapper x.fields in
    Printf.sprintf "      | %s {\n%s      } -> [\n%s      ]\n"
      x.kind_name fields fields

  let to_children_names x =
    let mapper1 f = Printf.sprintf "        %s;\n" f in
    let mapper2 f = Printf.sprintf "        \"%s\";\n" f in
    let fields1 = map_and_concat mapper1 x.fields in
    let fields2 = map_and_concat mapper2 x.fields in
    Printf.sprintf "      | %s {\n%s      } -> [\n%s      ]\n"
      x.kind_name fields1 fields2

  let to_syntax_from_children x =
    let mapper f = Printf.sprintf "          %s;\n" f in
    let fields = map_and_concat mapper x.fields in
    Printf.sprintf "      | (SyntaxKind.%s, [
%s        ]) ->
        %s {
%s        }
"
      x.kind_name fields x.kind_name fields

  let to_constructor_methods x =
    let mapper1 f = Printf.sprintf "      %s\n" f in
    let mapper2 f = Printf.sprintf "        %s;\n" f in
    let fields1 = map_and_concat mapper1 x.fields in
    let fields2 = map_and_concat mapper2 x.fields in
    Printf.sprintf "    let make_%s
%s    =
      from_children SyntaxKind.%s [
%s      ]

"
      x.type_name fields1 x.kind_name fields2

  let full_fidelity_syntax_template =
"(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the \"hack\" directory of this source tree. An additional
 * grant of patent rights can be found in the PATENTS file in the same
 * directory.
 *
 *)
(* THIS FILE IS GENERATED; DO NOT EDIT IT *)
(**
  To regenerate this file build hphp/hack/src:generate_full_fidelity and run
  the binary.
  buck build hphp/hack/src:generate_full_fidelity
  buck-out/bin/hphp/hack/src/generate_full_fidelity/generate_full_fidelity.opt
*)
(**
 * This module contains the code describing the structure of a syntax tree.
 *
 * The relationships between the various functors and signatures here needs
 * some explanation.
 *
 * First off, the structure of the syntax tree is described by the collection
 * of recursive types that makes up the bulk of this file. The type \"t\" is
 * the type of a node in the syntax tree; each node has associated with it
 * an arbitrary value of type SyntaxValue.t, and syntax node proper, which
 * has structure given by the \"syntax\" type.
 *
 * Note that every child in the syntax tree is of type t, except for the
 * \"Token\" type. This should be the *only* child of a type other than t.
 * We are explicitly NOT attempting to impose a type structure on the parse
 * tree beyond what is already implied by the types here. For example,
 * we are not attempting to put into the type system here the restriction that
 * the children of a binary operator must be expressions. The reason for this
 * is because we are potentially parsing code as it is being typed, and we
 * do not want to restrict our ability to make good error recovery by imposing
 * a restriction that will only be valid in correct program text.
 *
 * That said, it would of course be ideal if the only children of a compound
 * statement were statements, and so on. But those invariants should be
 * imposed by the design of the parser, not by the type system of the syntax
 * tree code.
 *
 * We want to be able to use different kinds of tokens, with different
 * performance characteristics. Therefore this module is first functorized by
 * token type.
 *
 * We wish to associate arbitrary values with the syntax nodes so that we can
 * construct syntax trees with various properties -- trees that only know
 * their widths and are thereby cheap to serialize, trees that have full
 * position data for each node, trees where the tokens know their text and
 * can therefore be edited, trees that have name annotations or type
 * annotations, and so on.
 *
 * Therefore this module is functorized again with the value type to be
 * associated with the node.
 *
 * We also wish to provide factory methods, so that nodes can be built up
 * from their child nodes. A factory method must not just know all the
 * children and the kind of node it is constructing; it also must know how
 * to construct the value that this node is going to be tagged with. For
 * that reason, a third, optional functor is provided. This functor requires
 * that methods be provided to construct the values associated with a token
 * or with any arbitrary node, given its children. If this functor is used
 * then the resulting module contains factory methods.
 *
 * This module also provides some useful helper functions, like an iterator,
 * a rewriting visitor, and so on.
 *
 *)

module SyntaxKind = Full_fidelity_syntax_kind
module Operator = Full_fidelity_operator

module type TokenType = sig
  type t
  val kind: t -> Full_fidelity_token_kind.t
  val to_json: t -> Hh_json.json
end

module type SyntaxValueType = sig
  type t
end

(* These functors describe the shape of a parse tree that has a particular
   kind of token in the leaves, and a particular kind of value associated
   with each node. *)
module WithToken(Token: TokenType) = struct
  module WithSyntaxValue(SyntaxValue: SyntaxValueType) = struct

    type t = {
      syntax : syntax ;
      value : SyntaxValue.t
    }
PARSE_TREE
    and syntax =
    | Token of Token.t
    | Missing
    | SyntaxList of t list
SYNTAX

    let make syntax value =
      { syntax; value }

    let syntax node =
      node.syntax

    let value node =
      node.value

    let to_kind syntax =
      match syntax with
      | Missing -> SyntaxKind.Missing
      | Token _  -> SyntaxKind.Token
      | SyntaxList _ -> SyntaxKind.SyntaxList
TO_KIND

    let kind node =
      to_kind (syntax node)

    let is_missing node =
      kind node = SyntaxKind.Missing

    let is_list node =
      kind node = SyntaxKind.SyntaxList

TYPE_TESTS

    let is_loop_statement node =
      is_for_statement node ||
      is_foreach_statement node ||
      is_while_statement node ||
      is_do_statement node

    let is_separable_prefix node =
      match syntax node with
      | Token t -> begin
        Full_fidelity_token_kind.(match Token.kind t with
        | PlusPlus | MinusMinus -> false
        | _ -> true) end
      | _ -> true

    let is_specific_token kind node =
      match syntax node with
      | Token t -> Token.kind t = kind
      | _ -> false


    let is_semicolon = is_specific_token Full_fidelity_token_kind.Semicolon
    let is_name = is_specific_token Full_fidelity_token_kind.Name
    let is_construct = is_specific_token Full_fidelity_token_kind.Construct
    let is_destruct = is_specific_token Full_fidelity_token_kind.Destruct
    let is_static = is_specific_token Full_fidelity_token_kind.Static
    let is_private = is_specific_token Full_fidelity_token_kind.Private
    let is_public = is_specific_token Full_fidelity_token_kind.Public
    let is_protected = is_specific_token Full_fidelity_token_kind.Protected
    let is_abstract = is_specific_token Full_fidelity_token_kind.Abstract
    let is_final = is_specific_token Full_fidelity_token_kind.Final
    let is_void = is_specific_token Full_fidelity_token_kind.Void
    let is_left_brace = is_specific_token Full_fidelity_token_kind.LeftBrace
    let is_ellipsis = is_specific_token Full_fidelity_token_kind.DotDotDot
    let is_comma = is_specific_token Full_fidelity_token_kind.Comma
    let is_array = is_specific_token Full_fidelity_token_kind.Array

    let children node =
      match node.syntax with
      | Missing -> []
      | Token _ -> []
      | SyntaxList x -> x
CHILDREN

    let children_names node =
      match node.syntax with
      | Missing -> []
      | Token _ -> []
      | SyntaxList _ -> []
CHILDREN_NAMES

    let rec to_json node =
      let open Hh_json in
      let ch = match node.syntax with
      | Token t -> [ \"token\", Token.to_json t ]
      | SyntaxList x -> [ (\"elements\", JSON_Array (List.map to_json x)) ]
      | _ ->
        let rec aux acc c n =
          match c, n with
          | ([], []) -> acc
          | ((hc :: tc), (hn :: tn)) ->
            aux ((hn, to_json hc) :: acc) tc tn
          | _ -> failwith \"mismatch between children and names\" in
        List.rev (aux [] (children node) (children_names node)) in
      let k = (\"kind\", JSON_String (SyntaxKind.to_string (kind node))) in
      JSON_Object (k :: ch)

    let binary_operator_kind b =
      match syntax b.binary_operator with
      | Token token ->
        let kind = Token.kind token in
        if Operator.is_trailing_operator_token kind then
          Some (Operator.trailing_from_token kind)
        else
          None
      | _ -> None

    let get_token node =
      match (syntax node) with
      | Token token -> Some token
      | _ -> None

    let leading_token node =
      let rec aux nodes =
        match nodes with
        | [] -> None
        | h :: t ->
          let token = get_token h in
          if token = None then
            let result = aux (children h) in
            if result = None then aux t else result
          else
            token in
      aux [node]

    let trailing_token node =
      let rec aux nodes =
        match nodes with
        | [] -> None
        | h :: t ->
          let token = get_token h in
          if token = None then
            let result = aux (List.rev (children h)) in
            if result = None then aux t else result
          else
            token in
      aux [node]

    let syntax_from_children kind ts =
      match kind, ts with
SYNTAX_FROM_CHILDREN      | (SyntaxKind.Missing, []) -> Missing
      | (SyntaxKind.SyntaxList, items) -> SyntaxList items
      | _ -> failwith
        \"syntax_from_children called with wrong number of children\"

    let all_tokens node =
      let rec aux acc nodes =
        match nodes with
        | [] -> acc
        | h :: t ->
          begin
            match syntax h with
            | Token token -> aux (token :: acc) t
            | _ -> aux (aux acc (children h)) t
          end in
      List.rev (aux [] [node])

    module type ValueBuilderType = sig
      val value_from_children:
        Full_fidelity_syntax_kind.t -> t list -> SyntaxValue.t
      val value_from_token: Token.t -> SyntaxValue.t
    end

    module WithValueBuilder(ValueBuilder: ValueBuilderType) = struct
      let from_children kind ts =
        let syntax = syntax_from_children kind ts in
        let value = ValueBuilder.value_from_children kind ts in
        make syntax value

      let make_token token =
        let syntax = Token token in
        let value = ValueBuilder.value_from_token token in
        make syntax value

      let make_missing () =
        from_children SyntaxKind.Missing []

      let make_list items =
        match items with
        | [] -> make_missing()
        | h :: [] -> h
        | _ -> from_children SyntaxKind.SyntaxList items

CONSTRUCTOR_METHODS

    end (* WithValueBuilder *)
  end (* WithSyntaxValue *)
end (* WithToken *)
"

  let full_fidelity_syntax =
  {
    filename = "hphp/hack/src/full_fidelity/full_fidelity_syntax.ml";
    template = full_fidelity_syntax_template;
    transformations = [
      { pattern = "PARSE_TREE"; func = to_parse_tree };
      { pattern = "SYNTAX"; func = to_syntax };
      { pattern = "TO_KIND"; func = to_to_kind };
      { pattern = "TYPE_TESTS"; func = to_type_tests };
      { pattern = "CHILDREN"; func = to_children };
      { pattern = "CHILDREN_NAMES"; func = to_children_names };
      { pattern = "SYNTAX_FROM_CHILDREN"; func = to_syntax_from_children };
      { pattern = "CONSTRUCTOR_METHODS"; func = to_constructor_methods };
    ]
  }

end (* GenerateFFSyntax *)

module GenerateFFSyntaxKind = struct

  let to_tokens x =
    Printf.sprintf "| %s\n" x.kind_name

  let to_to_string x =
    Printf.sprintf "  | %s -> \"%s\"\n" x.kind_name x.description

  let full_fidelity_syntax_kind_template =
"(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the \"hack\" directory of this source tree. An additional
 * grant of patent rights can be found in the PATENTS file in the same
 * directory.
 *
 *)
(* THIS FILE IS GENERATED; DO NOT EDIT IT *)
(**
  To regenerate this file build hphp/hack/src:generate_full_fidelity and run
  the binary.
  buck build hphp/hack/src:generate_full_fidelity
  buck-out/bin/hphp/hack/src/generate_full_fidelity/generate_full_fidelity.opt
*)
type t =
| Token
| Missing
| SyntaxList
TOKENS

let to_string kind =
  match kind with
  | Missing -> \"missing\"
  | Token -> \"token\"
  | SyntaxList -> \"list\"
TO_STRING"

let full_fidelity_syntax_kind =
{
  filename = "hphp/hack/src/full_fidelity/full_fidelity_syntax_kind.ml";
  template = full_fidelity_syntax_kind_template;
  transformations = [
    { pattern = "TOKENS"; func = to_tokens };
    { pattern = "TO_STRING"; func = to_to_string };
  ]
}

end (* GenerateFFSyntaxKind *)

let () =
  generate_file GenerateFFSyntax.full_fidelity_syntax;
  generate_file GenerateFFSyntaxKind.full_fidelity_syntax_kind
