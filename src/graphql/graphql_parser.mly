%{
open Parsing
open Graphql_ast

let symbol_loc () =
  let source = Graphql_parsing.curr_source () in
  Loc.from_lb_p source (symbol_start_pos ()) (symbol_end_pos ())
%}

%token <string> NAME
%token LBRACE
%token RBRACE
%token LCURLY
%token RCURLY
%token LBRACKET
%token RBRACKET
%token COLON
%token EQUAL
%token BANG
%token PIPE
%token ELLIPSIS
%token DOLLAR
%token AT
%token <string> INT
%token <string> FLOAT
%token <string> STRING

%token ENUM
%token FALSE
%token FRAGMENT
%token IMPLEMENTS
%token INPUT
%token INTERFACE
%token MUTATION
%token NULL
%token ON
%token QUERY
%token SCALAR
%token SCHEMA
%token SUBSCRIPTION
%token TRUE
%token TYPE
%token UNION

%token EOF

%start doc
%type <Graphql_ast.Document.t> doc

%%

doc:
  | definitions {
      {
        Document.definitions = $1;
      }
    }

definitions:
  | EOF { [] }
  | definition definitions { $1 :: $2 }

definition:
  | selection_set {
      Definition.Operation { OperationDef.
        operation = OperationType.Query;
        name = None;
        directives = None;
        selectionSet = $1;
        variableDefs = None;
      }
    }
  | operation_type name_opt var_defs directives_opt selection_set {
      Definition.Operation { OperationDef.
        operation = $1;
        name = $2;
        directives = $4;
        selectionSet = $5;
        variableDefs = $3;
      }
    }
  | FRAGMENT fragment_name_opt ON name directives_opt selection_set {
      Definition.Fragment { FragmentDef.
        name = $2;
        typeCondition = $4;
        selectionSet = $6;
        directives = $5;
      }
    }
  | SCALAR name directives_opt {
      Definition.ScalarType {
        ScalarTypeDef.name = $2;
        directives = $3;
        loc = symbol_loc ();
      }
    }
  | TYPE name interfaces directives_opt LCURLY field_defs RCURLY {
      Definition.ObjectType {
        ObjectTypeDef.name = $2;
        fields = $6;
        interfaces = $3;
        directives = $4;
      }
    }
  | INTERFACE name directives_opt LCURLY field_defs RCURLY {
      Definition.InterfaceType {
        InterfaceTypeDef.name = $2;
        fields = $5;
        directives = $3;
      }
    }
  | UNION name directives_opt EQUAL union_types {
      Definition.UnionType {
        UnionTypeDef.name = $2;
        types = $5;
        directives = $3;
      }
    }
  | ENUM name directives_opt LCURLY enum_values RCURLY {
      Definition.EnumType {
        EnumTypeDef.name = $2;
        values = $5;
        directives = $3;
      }
    }
  | INPUT name directives_opt LCURLY input_value_defs RCURLY {
      Definition.InputObjectType {
        InputObjectTypeDef.name = $2;
        fields = $5;
        directives = $3;
      }
    }
  | SCHEMA directives_opt LCURLY operation_types RCURLY {
      Definition.Schema { SchemaDef.
        operationTypes = $4;
        directives = $2;
      }
    }

var_defs:
  | { None }
  | LBRACE var_defs_ RBRACE { Some $2 }

var_defs_:
  | var_def { [$1] }
  | var_def var_defs_ { $1 :: $2 }

var_def:
  | var COLON type_ref default_value_opt {
      { VariableDef.
        variable = $1;
        type_ = $3;
        defaultValue = $4;
        loc = symbol_loc ();
      }
    }

selection_set:
  | LCURLY selections RCURLY {
      {
        SelectionSet.selections = $2;
      }
    }

selection_set_opt:
  | { None }
  | selection_set { Some $1 }

selections:
  | selection { [$1] }
  | selection selections { $1 :: $2 }

selection:
  | field_name arguments_opt directives_opt selection_set_opt {
      let (alias, name) = $1 in
      Selection.Field {
        Field.alias = alias;
        name = name;
        args = $2;
        selectionSet = $4;
        directives = $3;
      }
    }
  | ELLIPSIS fragment_name directives_opt {
      Selection.FragmentSpread {
        FragmentSpread.name = $2;
        directives = $3;
      }
    }
  | ELLIPSIS type_condition_opt directives_opt selection_set {
      Selection.InlineFragment {
        InlineFragment.typeCondition = $2;
        selectionSet = $4;
        directives = $3;
      }
    }

field_name:
  | name { (None, $1) }
  | name COLON name { (Some $1, $3) }

type_condition_opt:
  | { None }
  | ON name { Some $2 }

union_types:
  | name { [$1] }
  | name PIPE union_types { $1 :: $3 }

enum_values:
  | enum_value { [$1] }
  | enum_value enum_values { $1 :: $2 }

enum_value:
  | name directives_opt {
      { EnumValueDef.
        name = $1;
        directives = $2;
        loc = symbol_loc ();
      }
    }

field_defs:
  | { [] }
  | field_def field_defs { $1 :: $2 }

field_def:
  | name arg_defs COLON type_ref directives_opt {
      {
        FieldDef.name = $1;
        type_ = $4;
        args = $2;
        directives = $5;
      }
    }

arg_defs:
  | { [] }
  | LBRACE arg_defs_ RBRACE { $2 }

arg_defs_:
  | input_value_def { [$1] }
  | input_value_def arg_defs_ { $1 :: $2 }

input_value_defs:
  | { [] }
  | input_value_def input_value_defs { $1 :: $2 }

input_value_def:
  | name COLON type_ref default_value_opt directives_opt {
      { InputValueDef.
        name = $1;
        type_ = $3;
        defaultValue = $4;
        directives = $5;
        loc = symbol_loc ();
      }
    }

default_value_opt:
  | { None }
  | EQUAL value { Some $2 }

type_ref:
  | name { Type.Named $1 }
  | LBRACKET type_ref RBRACKET { Type.List (symbol_loc (), $2) }
  | type_ref BANG { Type.NonNull (symbol_loc (), $1) }

interfaces:
  | { [] }
  | IMPLEMENTS interfaces_list { $2 }

interfaces_list:
  | name { [$1] }
  | name interfaces_list { $1 :: $2 }

name: name_ { (symbol_loc (), $1) }

name_:
  | fragment_name_ { $1 }
  | ON { "on" }

enum_value_str:
  | NAME { $1 }
  | ENUM { "enum" }
  | FRAGMENT { "fragment" }
  | IMPLEMENTS { "implements" }
  | INPUT { "input" }
  | INTERFACE { "interface" }
  | MUTATION { "mutation" }
  | ON { "on" }
  | QUERY { "query" }
  | SCALAR { "scalar" }
  | SCHEMA { "schema" }
  | SUBSCRIPTION { "subscription" }
  | TYPE { "type" }
  | UNION { "union" }

fragment_name: fragment_name_ { (symbol_loc (), $1) }

fragment_name_:
  | NAME { $1 }
  | ENUM { "enum" }
  | FALSE { "false" }
  | FRAGMENT { "fragment" }
  | IMPLEMENTS { "implements" }
  | INPUT { "input" }
  | INTERFACE { "interface" }
  | MUTATION { "mutation" }
  | NULL { "null" }
  | QUERY { "query" }
  | SCALAR { "scalar" }
  | SCHEMA { "schema" }
  | SUBSCRIPTION { "subscription" }
  | TRUE { "true" }
  | TYPE { "type" }
  | UNION { "union" }

fragment_name_opt:
  | { None }
  | fragment_name { Some $1 }

name_opt:
  | { None }
  | name { Some $1 }

var:
  | DOLLAR name { (symbol_loc (), $2) }

value:
  | var { Value.Variable $1 }
  | INT { Value.IntValue (symbol_loc(), $1) }
  | FLOAT { Value.FloatValue (symbol_loc(), $1) }
  | STRING { Value.StringValue (symbol_loc(), $1) }
  | TRUE { Value.BooleanValue (symbol_loc(), true) }
  | FALSE { Value.BooleanValue (symbol_loc(), false) }
  | enum_value_str { Value.EnumValue (symbol_loc (), $1) }
  | LBRACKET list_values RBRACKET { Value.ListValue (symbol_loc (), $2) }
  | LCURLY object_fields RCURLY {
      let obj = { ObjectValue.
        fields = $2;
        loc = symbol_loc ();
      } in
      Value.ObjectValue obj
    }

list_values:
  | { [] }
  | value list_values { $1 :: $2 }

object_fields:
  | { [] }
  | object_field object_fields {
      $1 :: $2
    }

object_field:
  | name COLON value {
      { ObjectField.name = $1; value = $3; loc = symbol_loc () }
    }

arguments:
  | LBRACE arguments_ RBRACE { $2 }

arguments_opt:
  | { None }
  | arguments { Some $1 }

arguments_:
  | argument { [$1] }
  | argument arguments_ { $1 :: $2 }

argument:
  | name COLON value {
      { Argument.
        name = $1;
        value = $3;
        loc = symbol_loc ();
      }
    }

directives:
  | directive { [$1] }
  | directive directives { $1 :: $2 }

directives_opt:
  | { None }
  | directives { Some $1 }

directive:
  | AT name arguments_opt {
      { Directive.
        name = $2;
        arguments = $3;
        loc = symbol_loc ();
      }
    }

operation_types:
  | { [] }
  | operation_type COLON name operation_types {
      let node = {
        OperationType.operation = $1;
        type_ = $3;
      } in
      node :: $4
    }

operation_type:
  | QUERY { OperationType.Query }
  | MUTATION { OperationType.Mutation }
  | SUBSCRIPTION { OperationType.Subscription }
