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
  prefix : string;
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
  | kind_name :: type_name :: description :: prefix :: fields ->
    { kind_name; type_name; description; prefix; fields }
  | _ -> failwith "bad schema"

let schema = List.map from_list [
  [ "ScriptHeader";
    "script_header";
    "header";
    "header";
    "less_than";
    "question";
    "language" ];
  [ "Script";
    "script";
    "script";
    "script";
    "header";
    "declarations" ];
  [ "SimpleTypeSpecifier";
    "simple_type_specifier";
    "simple_type_specifier";
    "simple_type";
    "specifier" ];
  [ "LiteralExpression";
    "literal_expression";
    "literal";
    "literal";
    "expression" ];
  [ "VariableExpression";
    "variable_expression";
    "variable";
    "variable";
    "expression" ];
  [ "QualifiedNameExpression";
    "qualified_name_expression";
    "qualified_name";
    "qualified_name";
    "expression" ];
  [ "PipeVariableExpression";
    "pipe_variable_expression";
    "pipe_variable";
    "pipe_variable";
    "expression" ];
  [ "EnumDeclaration";
    "enum_declaration";
    "enum_declaration";
    "enum";
    "keyword";
    "name";
    "colon";
    "base";
    "type";
    "left_brace";
    "enumerators";
    "right_brace" ];
  [ "Enumerator";
    "enumerator";
    "enumerator";
    "enumerator";
    "name";
    "equal";
    "value";
    "semicolon" ];
  [ "AliasDeclaration";
    "alias_declaration";
    "alias_declaration";
    "alias";
    "attribute_spec";
    "keyword";
    "name";
    "generic_parameter";
    "constraint";
    "equal";
    "type";
    "semicolon" ];
  [ "PropertyDeclaration";
    "property_declaration";
    "property_declaration";
    "property";
    "modifiers";
    "type";
    "declarators";
    "semicolon" ];
  [ "PropertyDeclarator";
    "property_declarator";
    "property_declarator";
    "property";
    "name";
    "initializer" ];
  [ "NamespaceDeclaration";
    "namespace_declaration";
    "namespace_declaration";
    "namespace";
    "keyword";
    "name";
    "body" ];
  [ "NamespaceBody";
    "namespace_body";
    "namespace_body";
    "namespace";
    "left_brace";
    "declarations";
    "right_brace" ];
  [ "NamespaceUseDeclaration";
    "namespace_use_declaration";
    "namespace_use_declaration";
    "namespace_use";
    "keyword";
    "kind";
    "clauses";
    "semicolon"];
  [ "NamespaceGroupUseDeclaration";
    "namespace_group_use_declaration";
    "namespace_group_use_declaration";
    "namespace_group_use";
    "keyword";
    "kind";
    "prefix";
    "left_brace";
    "clauses";
    "right_brace";
    "semicolon" ];
  [ "NamespaceUseClause";
    "namespace_use_clause";
    "namespace_use_clause";
    "namespace_use";
    "clause_kind";
    "name";
    "as";
    "alias" ];
  [ "FunctionDeclaration";
    "function_declaration";
    "function_declaration";
    "function";
    "attribute_spec";
    "declaration_header";
    "body" ];
  [ "FunctionDeclarationHeader";
    "function_declaration_header";
    "function_declaration_header";
    "function";
    "async";
    "keyword";
    "name";
    "type_parameter_list";
    "left_paren";
    "parameter_list";
    "right_paren";
    "colon";
    "type" ];
  [ "MethodishDeclaration";
    "methodish_declaration";
    "methodish_declaration";
    "methodish";
    "attribute";
    "modifiers";
    "function_decl_header";
    "function_body";
    "semicolon" ];
  [ "ClassishDeclaration";
    "classish_declaration";
    "classish_declaration";
    "classish";
    "attribute";
    "modifiers";
    "keyword";
    "name";
    "type_parameters";
    "extends_keyword";
    "extends_list";
    "implements_keyword";
    "implements_list";
    "body" ];
  [ "ClassishBody";
    "classish_body";
    "classish_body";
    "classish_body";
    "left_brace";
    "elements";
    "right_brace" ];
  [ "TraitUse";
    "trait_use";
    "trait_use";
    "trait_use";
    "keyword";
    "names";
    "semicolon" ];
  [ "RequireClause";
    "require_clause";
    "require_clause";
    "require";
    "keyword";
    "kind";
    "name";
    "semicolon" ];
  [ "ConstDeclaration";
    "const_declaration";
    "const_declaration";
    "const";
    "abstract";
    "keyword";
    "type_specifier";
    "declarators";
    "semicolon" ];
  [ "ConstantDeclarator";
    "constant_declarator";
    "constant_declarator";
    "constant_declarator";
    "name";
    "initializer" ];
  [ "TypeConstDeclaration";
    "type_const_declaration";
    "type_const_declaration";
    "type_const";
    "abstract";
    "keyword";
    "type_keyword";
    "name";
    "type_constraint";
    "equal";
    "type_specifier";
    "semicolon" ];
  [ "DecoratedExpression";
    "decorated_expression";
    "decorated_expression";
    "decorated_expression";
    "decorator";
    "expression" ];
  [ "ParameterDeclaration";
    "parameter_declaration";
    "parameter_declaration";
    "parameter";
    "attribute";
    "visibility";
    "type";
    "name";
    "default_value" ];
  [ "AttributeSpecification";
    "attribute_specification";
    "attribute_specification";
    "attribute_specification";
    "left_double_angle";
    "attributes";
    "right_double_angle" ];
  [ "Attribute";
    "attribute";
    "attribute";
    "attribute";
    "name";
    "left_paren";
    "values";
    "right_paren" ];
  [ "InclusionExpression";
    "inclusion_expression";
    "inclusion_expression";
    "inclusion";
    "require";
    "filename" ];
  [ "InclusionDirective";
    "inclusion_directive";
    "inclusion_directive";
    "inclusion";
    "expression";
    "semicolon" ];
  [ "CompoundStatement";
    "compound_statement";
    "compound_statement";
    "compound";
    "left_brace";
    "statements";
    "right_brace" ];
  [ "ExpressionStatement";
    "expression_statement";
    "expression_statement";
    "expression_statement";
    "expression";
    "semicolon" ];
  [ "WhileStatement";
    "while_statement";
    "while_statement";
    "while";
    "keyword";
    "left_paren";
    "condition";
    "right_paren";
    "body" ];
  [ "IfStatement";
    "if_statement";
    "if_statement";
    "if";
    "keyword";
    "left_paren";
    "condition";
    "right_paren";
    "statement";
    "elseif_clauses";
    "else_clause" ];
  [ "ElseifClause";
    "elseif_clause";
    "elseif_clause";
    "elseif";
    "keyword";
    "left_paren";
    "condition";
    "right_paren";
    "statement" ];
  [ "ElseClause";
    "else_clause";
    "else_clause";
    "else";
    "keyword";
    "statement" ];
  [ "TryStatement";
    "try_statement";
    "try_statement";
    "try";
    "keyword";
    "compound_statement";
    "catch_clauses";
    "finally_clause" ];
  [ "CatchClause";
    "catch_clause";
    "catch_clause";
    "catch";
    "keyword";
    "left_paren";
    "type";
    "variable";
    "right_paren";
    "body" ];
  [ "FinallyClause";
    "finally_clause";
    "finally_clause";
    "finally";
    "keyword";
    "body" ];
  [ "DoStatement";
    "do_statement";
    "do_statement";
    "do";
    "keyword";
    "body";
    "while_keyword";
    "left_paren";
    "condition";
    "right_paren";
    "semicolon" ];
  [ "ForStatement";
    "for_statement";
    "for_statement";
    "for";
    "keyword";
    "left_paren";
    "initializer";
    "first_semicolon";
    "control";
    "second_semicolon";
    "end_of_loop";
    "right_paren";
    "body" ];
  [ "ForeachStatement";
    "foreach_statement";
    "foreach_statement";
    "foreach";
    "keyword";
    "left_paren";
    "collection";
    "await";
    "as";
    "key";
    "arrow";
    "value";
    "right_paren";
    "body" ];
  [ "SwitchStatement";
    "switch_statement";
    "switch_statement";
    "switch";
    "keyword";
    "left_paren";
    "expression";
    "right_paren";
    "body" ];
  [ "CaseStatement";
    "case_statement";
    "case_statement";
    "case";
    "keyword";
    "expression";
    "colon";
    "statement" ];
  [ "DefaultStatement";
    "default_statement";
    "default_statement";
    "default";
    "keyword";
    "colon";
    "statement" ];
  [ "ReturnStatement";
    "return_statement";
    "return_statement";
    "return";
    "keyword";
    "expression";
    "semicolon" ];
  [ "ThrowStatement";
    "throw_statement";
    "throw_statement";
    "throw";
    "keyword";
    "expression";
    "semicolon" ];
  [ "BreakStatement";
    "break_statement";
    "break_statement";
    "break";
    "keyword";
    "level";
    "semicolon" ];
  [ "ContinueStatement";
    "continue_statement";
    "continue_statement";
    "continue";
    "keyword";
    "level";
    "semicolon" ];
  [ "FunctionStaticStatement";
    "function_static_statement";
    "function_static_statement";
    "static";
    "static_keyword";
    "declarations";
    "semicolon" ];
  [ "StaticDeclarator";
    "static_declarator";
    "static_declarator";
    "static";
    "name";
    "initializer" ];
  [ "EchoStatement";
    "echo_statement";
    "echo_statement";
    "echo";
    "keyword";
    "expressions";
    "semicolon" ];
  [ "SimpleInitializer";
    "simple_initializer";
    "simple_initializer";
    "simple_initializer";
    "equal";
    "value" ];
  [ "AnonymousFunction";
    "anonymous_function";
    "anonymous_function";
    "anonymous";
    "async_keyword";
    "function_keyword";
    "left_paren";
    "parameters";
    "right_paren";
    "colon";
    "type";
    "use";
    "body" ];
  [ "AnonymousFunctionUseClause";
    "anonymous_function_use_clause";
    "anonymous_function_use_clause";
    "anonymous_use";
    "keyword";
    "left_paren";
    "variables";
    "right_paren" ];
  [ "LambdaExpression";
    "lambda_expression";
    "lambda_expression";
    "lambda";
    "async";
    "signature";
    "arrow";
    "body" ];
  [ "LambdaSignature";
    "lambda_signature";
    "lambda_signature";
    "lambda";
    "left_paren";
    "parameters";
    "right_paren";
    "colon";
    "type" ];
  [ "CastExpression";
    "cast_expression";
    "cast_expression";
    "cast";
    "left_paren";
    "type";
    "right_paren";
    "operand" ];
  [ "ScopeResolutionExpression";
    "scope_resolution_expression";
    "scope_resolution_expression";
    "scope_resolution";
    "qualifier";
    "operator";
    "name" ];
  [ "MemberSelectionExpression";
    "member_selection_expression";
    "member_selection_expression";
    "member";
    "object";
    "operator";
    "name" ];
  [ "SafeMemberSelectionExpression";
    "safe_member_selection_expression";
    "safe_member_selection_expression";
    "safe_member";
    "object";
    "operator";
    "name" ];
  [ "YieldExpression";
    "yield_expression";
    "yield_expression";
    "yield";
    "keyword";
    "operand" ];
  [ "PrintExpression";
    "print_expression";
    "print_expression";
    "print";
    "keyword";
    "expression" ];
  [ "PrefixUnaryExpression";
    "prefix_unary_expression";
    "prefix_unary_expression";
    "prefix_unary";
    "operator";
    "operand" ];
  [ "PostfixUnaryExpression";
    "postfix_unary_expression";
    "postfix_unary_expression";
    "postfix_unary";
    "operand";
    "operator" ];
  [ "BinaryExpression";
    "binary_expression";
    "binary_expression";
    "binary";
    "left_operand";
    "operator";
    "right_operand" ];
  [ "InstanceofExpression";
    "instanceof_expression";
    "instanceof_expression";
    "instanceof";
    "left_operand";
    "operator";
    "right_operand" ];
  [ "ConditionalExpression";
    "conditional_expression";
    "conditional_expression";
    "conditional";
    "test";
    "question";
    "consequence";
    "colon";
    "alternative" ];
  [ "FunctionCallExpression";
    "function_call_expression";
    "function_call_expression";
    "function_call";
    "receiver";
    "left_paren";
    "argument_list";
    "right_paren" ];
  [ "ParenthesizedExpression";
    "parenthesized_expression";
    "parenthesized_expression";
    "parenthesized_expression";
    "left_paren";
    "expression";
    "right_paren" ];
  [ "BracedExpression";
    "braced_expression";
    "braced_expression";
    "braced_expression";
    "left_brace";
    "expression";
    "right_brace" ];
  [ "ListExpression";
    "list_expression";
    "list_expression";
    "list";
    "keyword";
    "left_paren";
    "members";
    "right_paren" ];
  [ "CollectionLiteralExpression";
    "collection_literal_expression";
    "collection_literal_expression";
    "collection_literal";
    "name";
    "left_brace";
    "initializers";
    "right_brace" ];
  [ "ObjectCreationExpression";
    "object_creation_expression";
    "object_creation_expression";
    "object_creation";
    "new_keyword";
    "type";
    "left_paren";
    "argument_list";
    "right_paren" ];
  [ "ArrayCreationExpression";
    "array_creation_expression";
    "array_creation_expression";
    "array_creation";
    "left_bracket";
    "members";
    "right_bracket" ];
  [ "ArrayIntrinsicExpression";
    "array_intrinsic_expression";
    "array_intrinsic_expression";
    "array_intrinsic";
    "keyword";
    "left_paren";
    "members";
    "right_paren" ];
  [ "ElementInitializer";
    "element_initializer";
    "element_initializer";
    "element";
    "key";
    "arrow";
    "value" ];
  [ "SubscriptExpression";
    "subscript_expression";
    "subscript_expression";
    "subscript";
    "receiver";
    "left_bracket";
    "index";
    "right_bracket" ];
  [ "AwaitableCreationExpression";
    "awaitable_creation_expression";
    "awaitable_creation_expression";
    "awaitable";
    "async";
    "compound_statement" ];
  [ "XHPChildrenDeclaration";
    "xhp_children_declaration";
    "xhp_children_declaration";
    "xhp_children";
    "keyword";
    "expression";
    "semicolon" ];
  [ "XHPCategoryDeclaration";
    "xhp_category_declaration";
    "xhp_category_declaration";
    "xhp_category";
    "keyword";
    "categories";
    "semicolon" ];
  [ "XHPEnumType";
    "xhp_enum_type";
    "xhp_enum_type";
    "xhp_enum";
    "keyword";
    "left_brace";
    "values";
    "right_brace" ];
  [ "XHPRequired";
    "xhp_required";
    "xhp_required";
    "xhp_required";
    "at";
    "keyword" ];
  [ "XHPClassAttributeDeclaration";
    "xhp_class_attribute_declaration";
    "xhp_class_attribute_declaration";
    "xhp_attribute";
    "keyword";
    "attributes";
    "semicolon" ];
  [ "XHPClassAttribute";
    "xhp_class_attribute";
    "xhp_class_attribute";
    "xhp_attribute_decl";
    "type";
    "name";
    "initializer";
    "required" ];
  [ "XHPAttribute";
    "xhp_attribute";
    "xhp_attribute";
    "xhp_attribute";
    "name";
    "equal";
    "expression" ];
  [ "XHPOpen";
    "xhp_open";
    "xhp_open";
    "xhp_open";
    "name";
    "attributes";
    "right_angle" ];
  [ "XHPExpression";
    "xhp_expression";
    "xhp_expression";
    "xhp";
    "open";
    "body";
    "close" ];
  [ "XHPClose";
    "xhp_close";
    "xhp_close";
    "xhp_close";
    "left_angle";
    "name";
    "right_angle" ];
  [ "TypeConstant";
    "type_constant";
    "type_constant";
    "type_constant";
    "left_type";
    "separator";
    "right_type" ];
  [ "VectorTypeSpecifier";
    "vector_type_specifier";
    "vector_type_specifier";
    "vector";
    "array";
    "left_angle";
    "type";
    "right_angle" ];
  [ "TypeParameter";
    "type_parameter";
    "type_parameter";
    "type";
    "variance";
    "name";
    "constraints" ];
  [ "TypeConstraint";
    "type_constraint";
    "type_constraint";
    "constraint";
    "keyword";
    "type" ];
  [ "MapTypeSpecifier";
    "map_type_specifier";
    "map_type_specifier";
    "map";
    "array";
    "left_angle";
    "key";
    "comma";
    "value";
    "right_angle" ];
  [ "ClosureTypeSpecifier";
    "closure_type_specifier";
    "closure_type_specifier";
    "closure";
    "outer_left_paren";
    "function_keyword";
    "inner_left_paren";
    "parameter_types";
    "inner_right_paren";
    "colon";
    "return_type";
    "outer_right_paren" ];
  [ "ClassnameTypeSpecifier";
    "classname_type_specifier";
    "classname_type_specifier";
    "classname";
    "keyword";
    "left_angle";
    "type";
    "right_angle" ];
  [ "FieldSpecifier";
    "field_specifier";
    "field_specifier";
    "field";
    "name";
    "arrow";
    "type" ];
  [ "FieldInitializer";
    "field_initializer";
    "field_initializer";
    "field_initializer";
    "name";
    "arrow";
    "value" ];
  [ "ShapeTypeSpecifier";
    "shape_type_specifier";
    "shape_type_specifier";
    "shape_type";
    "keyword";
    "left_paren";
    "fields";
    "right_paren" ];
  [ "ShapeExpression";
    "shape_expression";
    "shape_expression";
    "shape_expression";
    "keyword";
    "left_paren";
    "fields";
    "right_paren" ];
  [ "GenericTypeSpecifier";
    "generic_type_specifier";
    "generic_type_specifier";
    "generic";
    "class_type";
    "argument_list" ];
  [ "NullableTypeSpecifier";
    "nullable_type_specifier";
    "nullable_type_specifier";
    "nullable";
    "question";
    "type" ];
  [ "SoftTypeSpecifier";
    "soft_type_specifier";
    "soft_type_specifier";
    "soft";
    "at";
    "type" ];
  [ "TypeArguments";
    "type_arguments";
    "type_arguments";
    "type_arguments";
    "left_angle";
    "types";
    "right_angle" ];
  [ "TypeParameters";
    "type_parameters";
    "type_parameters";
    "type_parameters";
    "left_angle";
    "parameters";
    "right_angle" ];
  [ "TupleTypeSpecifier";
    "tuple_type_specifier";
    "tuple_type_specifier";
    "tuple";
    "left_paren";
    "types";
    "right_paren" ];
  [ "ErrorSyntax";
    "error";
    "error";
    "error";
    "error" ];
  [ "ListItem";
    "list_item";
    "list_item";
    "list";
    "item";
    "separator" ]]

let variable_text_tokens = [
  [ "ErrorToken"; "error_token" ];
  [ "Name"; "name" ];
  [ "QualifiedName"; "qualified_name" ];
  [ "Variable"; "variable" ];
  [ "NamespacePrefix"; "namespace_prefix" ];
  [ "DecimalLiteral"; "decimal_literal" ];
  [ "OctalLiteral"; "octal_literal" ];
  [ "HexadecimalLiteral"; "hexadecimal_literal" ];
  [ "BinaryLiteral"; "binary_literal" ];
  [ "FloatingLiteral"; "floating_literal" ];
  [ "SingleQuotedStringLiteral"; "single_quoted_string_literal" ];
  [ "DoubleQuotedStringLiteral"; "double_quoted_string_literal" ];
  [ "HeredocStringLiteral"; "heredoc_string_literal" ];
  [ "NowdocStringLiteral"; "nowdoc_string_literal" ];
  [ "BooleanLiteral"; "boolean_literal" ];
  [ "XHPCategoryName"; "XHP_category_name" ];
  [ "XHPElementName"; "XHP_element_name" ];
  [ "XHPClassName"; "XHP_class_name" ];
  [ "XHPStringLiteral"; "XHP_string_literal" ];
  [ "XHPBody"; "XHP_body" ];
  [ "XHPComment"; "XHP_comment" ]]

let no_text_tokens = [
  [ "EndOfFile"; "end_of_file" ]]

let given_text_tokens = [
  [ "Abstract"; "abstract" ];
  [ "Array"; "array" ];
  [ "Arraykey"; "arraykey" ];
  [ "As"; "as" ];
  [ "Async"; "async" ];
  [ "Attribute"; "attribute" ];
  [ "Await"; "await" ];
  [ "Bool"; "bool" ];
  [ "Break"; "break" ];
  [ "Case"; "case" ];
  [ "Catch"; "catch" ];
  [ "Category"; "category" ];
  [ "Children"; "children" ];
  [ "Class"; "class" ];
  [ "Classname"; "classname" ];
  [ "Clone"; "clone" ];
  [ "Const"; "const" ];
  [ "Construct"; "__construct" ];
  [ "Continue"; "continue" ];
  [ "Default"; "default" ];
  [ "Destruct"; "__destruct" ];
  [ "Do"; "do" ];
  [ "Double"; "double" ];
  [ "Echo"; "echo" ];
  [ "Else"; "else" ];
  [ "Elseif"; "elseif" ];
  [ "Empty"; "empty" ];
  [ "Enum"; "enum" ];
  [ "Extends"; "extends" ];
  [ "Float"; "float" ];
  [ "Final"; "final" ];
  [ "Finally"; "finally" ];
  [ "For"; "for" ];
  [ "Foreach"; "foreach" ];
  [ "Function"; "function" ];
  [ "If"; "if" ];
  [ "Implements"; "implements" ];
  [ "Include"; "include" ];
  [ "Include_once"; "include_once" ];
  [ "Instanceof"; "instanceof" ];
  [ "Insteadof"; "insteadof" ];
  [ "Int"; "int" ];
  [ "Interface"; "interface" ];
  [ "List"; "list" ];
  [ "Mixed"; "mixed" ];
  [ "Namespace"; "namespace" ];
  [ "New"; "new" ];
  [ "Newtype"; "newtype" ];
  [ "Noreturn"; "noreturn" ];
  [ "Num"; "num" ];
  [ "Object"; "object" ];
  [ "Parent"; "parent" ];
  [ "Print"; "print" ];
  [ "Private"; "private" ];
  [ "Protected"; "protected" ];
  [ "Public"; "public" ];
  [ "Require"; "require" ];
  [ "Require_once"; "require_once" ];
  [ "Required"; "required" ];
  [ "Resource"; "resource" ];
  [ "Return"; "return" ];
  [ "Self"; "self" ];
  [ "Shape"; "shape" ];
  [ "Static"; "static" ];
  [ "String"; "string" ];
  [ "Super"; "super" ];
  [ "Switch"; "switch" ];
  [ "This"; "this" ];
  [ "Throw"; "throw" ];
  [ "Trait"; "trait" ];
  [ "Try"; "try" ];
  [ "Tuple"; "tuple" ];
  [ "Type"; "type" ];
  [ "Unset"; "unset" ];
  [ "Use"; "use" ];
  [ "Var"; "var" ];
  [ "Void"; "void" ];
  [ "While"; "while" ];
  [ "Yield"; "yield" ];
  [ "LeftBracket"; "[" ];
  [ "RightBracket"; "]" ];
  [ "LeftParen"; "(" ];
  [ "RightParen"; ")" ];
  [ "LeftBrace"; "{" ];
  [ "RightBrace"; "}" ];
  [ "Dot"; "." ];
  [ "MinusGreaterThan"; "->" ];
  [ "PlusPlus"; "++" ];
  [ "MinusMinus"; "--" ];
  [ "StarStar"; "**" ];
  [ "Star"; "*" ];
  [ "Plus"; "+" ];
  [ "Minus"; "-" ];
  [ "Tilde"; "~" ];
  [ "Exclamation"; "!" ];
  [ "Dollar"; "$" ];
  [ "Slash"; "/" ];
  [ "Percent"; "%" ];
  [ "LessThanLessThan"; "<<" ];
  [ "GreaterThanGreaterThan"; ">>" ];
  [ "LessThan"; "<" ];
  [ "GreaterThan"; ">" ];
  [ "LessThanEqual"; "<=" ];
  [ "GreaterThanEqual"; ">=" ];
  [ "EqualEqual"; "==" ];
  [ "EqualEqualEqual"; "===" ];
  [ "ExclamationEqual"; "!=" ];
  [ "ExclamationEqualEqual"; "!==" ];
  [ "Carat"; "^" ];
  [ "Bar"; "|" ];
  [ "Ampersand"; "&" ];
  [ "AmpersandAmpersand"; "&&" ];
  [ "BarBar"; "||" ];
  [ "Question"; "?" ];
  [ "QuestionQuestion"; "??" ];
  [ "Colon"; ":" ];
  [ "Semicolon"; ";" ];
  [ "Equal"; "=" ];
  [ "StarStarEqual"; "**=" ];
  [ "StarEqual"; "*=" ];
  [ "SlashEqual"; "/=" ];
  [ "PercentEqual"; "%=" ];
  [ "PlusEqual"; "+=" ];
  [ "MinusEqual"; "-=" ];
  [ "DotEqual"; ".=" ];
  [ "LessThanLessThanEqual"; "<<=" ];
  [ "GreaterThanGreaterThanEqual"; ">>=" ];
  [ "AmpersandEqual"; "&=" ];
  [ "CaratEqual"; "^=" ];
  [ "BarEqual"; "|=" ];
  [ "Comma"; "," ];
  [ "At"; "@" ];
  [ "ColonColon"; "::" ];
  [ "EqualGreaterThan"; "=>" ];
  [ "EqualEqualGreaterThan"; "==>" ];
  [ "QuestionMinusGreaterThan"; "?->" ];
  [ "DotDotDot"; "..." ];
  [ "DollarDollar"; "$$" ];
  [ "BarGreaterThan"; "|>" ];
  [ "NullLiteral"; "null" ];
  [ "SlashGreaterThan"; "/>" ];
  [ "LessThanSlash"; "</" ]]

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
    let mapper f = Printf.sprintf "      %s_%s: t;\n" x.prefix f in
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
    let mapper f = Printf.sprintf "        %s_%s;\n" x.prefix f in
    let fields = map_and_concat mapper x.fields in
    Printf.sprintf "      | %s {\n%s      } -> [\n%s      ]\n"
      x.kind_name fields fields

  let to_children_names x =
    let mapper1 f = Printf.sprintf "        %s_%s;\n" x.prefix f in
    let mapper2 f = Printf.sprintf "        \"%s_%s\";\n" x.prefix f in
    let fields1 = map_and_concat mapper1 x.fields in
    let fields2 = map_and_concat mapper2 x.fields in
    Printf.sprintf "      | %s {\n%s      } -> [\n%s      ]\n"
      x.kind_name fields1 fields2

  let to_syntax_from_children x =
    let mapper f = Printf.sprintf "          %s_%s;\n" x.prefix f in
    let fields = map_and_concat mapper x.fields in
    Printf.sprintf "      | (SyntaxKind.%s, [
%s        ]) ->
        %s {
%s        }
"
      x.kind_name fields x.kind_name fields

  let to_constructor_methods x =
    let mapper1 f = Printf.sprintf "      %s_%s\n" x.prefix f in
    let mapper2 f = Printf.sprintf "        %s_%s;\n" x.prefix f in
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
