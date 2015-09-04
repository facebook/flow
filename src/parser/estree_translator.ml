(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast

module type Translator = sig
  type t
  val string: string -> t
  val bool: bool -> t
  val obj: (string * t) array -> t
  val array: t array -> t
  val number: float -> t
  val null: t
  val regexp: Loc.t -> string -> string -> t
end

module Translate (Impl : Translator) : (sig
  type t
  val program:
    Loc.t * Ast.Statement.t list * (Loc.t * Ast.Comment.t') list ->
    t
  val expression: Ast.Expression.t -> t
  val errors: (Loc.t * Parse_error.t) list -> t
end with type t = Impl.t) = struct
  type t = Impl.t

  open Ast
  open Impl

  let array_of_list fn list = array (Array.of_list (List.map fn list))
  let int x = number (float x)
  let option f = function
    | Some v -> f v
    | None -> null

  let position p =
    obj [|
      "line", int p.Loc.line;
      "column", int p.Loc.column;
    |]

  let loc location =
    obj [|
      "source", option (fun x -> string x) location.Loc.source;
      "start", position location.Loc.start;
      "end", position location.Loc._end;
    |]

  let range location = Loc.(
    array [|
      int location.start.offset;
      int location._end.offset;
    |]
  )

  let node _type location props =
    obj (Array.append [|
      "type", string _type;
      "loc", loc location;
      "range", range location;
    |] props)

  let errors l =
    let error (location, e) =
      obj [|
        "loc", loc location;
        "message", string (Parse_error.PP.error e);
      |]
    in array_of_list error l

  let rec program (loc, statements, comments) =
    node "Program" loc [|
      "body", statement_list statements;
      "comments", comment_list comments;
    |]

  and statement_list statements = array_of_list statement statements
  and statement = Statement.(function
  | loc, Empty -> node "EmptyStatement" loc [||]
  | loc, Block b -> block (loc, b)
  | loc, Expression expr ->
      node "ExpressionStatement" loc [|
        "expression", expression expr.Expression.expression;
      |]
  | loc, If _if -> If.(
      node "IfStatement" loc [|
        "test", expression _if.test;
        "consequent", statement _if.consequent;
        "alternate", option statement _if.alternate;
      |]
    )
  | loc, Labeled labeled -> Labeled.(
      node "LabeledStatement" loc [|
        "label", identifier labeled.label;
        "body", statement labeled.body;
      |]
    )
  | loc, Break break ->
      node "BreakStatement" loc [|
        "label", option identifier break.Break.label;
      |]
  | loc, Continue continue ->
      node "ContinueStatement" loc [|
        "label", option identifier continue.Continue.label;
      |]
  | loc, With _with -> With.(
      node "WithStatement" loc [|
        "object", expression _with._object;
        "body", statement _with.body;
      |]
    )
  | loc, TypeAlias alias -> TypeAlias.(
      node "TypeAlias" loc [|
        "id", identifier alias.id;
        "typeParameters", option type_parameter_declaration alias.typeParameters;
        "right", _type alias.right;
      |]
    )
  | loc, Switch switch -> Switch.(
      node "SwitchStatement" loc [|
        "discriminant", expression switch.discriminant;
        "cases", array_of_list case switch.cases;
        "lexical", bool switch.lexical;
      |]
    )
  | loc, Return return ->
      node "ReturnStatement" loc [|
        "argument", option expression return.Return.argument;
      |]
  | loc, Throw throw ->
      node "ThrowStatement" loc [|
        "argument", expression throw.Throw.argument;
      |]
  | loc, Try _try -> Try.(
      node "TryStatement" loc [|
        "block", block _try.block;
        "handler", option catch _try.handler;
        "guardedHandlers", array_of_list catch _try.guardedHandlers;
        "finalizer", option block _try.finalizer;
      |]
    )
  | loc, While _while -> While.(
      node "WhileStatement" loc [|
        "test", expression _while.test;
        "body", statement _while.body;
      |]
    )
  | loc, DoWhile dowhile -> DoWhile.(
      node "DoWhileStatement" loc [|
        "body", statement dowhile.body;
        "test", expression dowhile.test;
      |]
    )
  | loc, For _for -> For.(
      let init = function
      | InitDeclaration init -> variable_declaration init
      | InitExpression expr -> expression expr
      in
      node "ForStatement" loc [|
        "init", option init _for.init;
        "test", option expression _for.test;
        "update", option expression _for.update;
        "body", statement _for.body;
      |]
    )
  | loc, ForIn forin -> ForIn.(
      let left = (match forin.left with
      | LeftDeclaration left -> variable_declaration left
      | LeftExpression left -> expression left) in
      node "ForInStatement" loc [|
        "left", left;
        "right", expression forin.right;
        "body", statement forin.body;
        "each", bool forin.each;
      |]
    )
  | loc, ForOf forof -> ForOf.(
      let left = (match forof.left with
      | LeftDeclaration left -> variable_declaration left
      | LeftExpression left -> expression left) in
      node "ForOfStatement" loc [|
        "left", left;
        "right", expression forof.right;
        "body", statement forof.body;
      |]
    )
  | loc, Let _let -> Let.(
      node "LetStatement" loc [|
        "head", array_of_list let_assignment _let.head;
        "body", statement _let.body;
      |]
    )
  | loc, Debugger -> node "DebuggerStatement" loc [||]
  | loc, ClassDeclaration c -> class_declaration (loc, c)
  | loc, InterfaceDeclaration i -> interface_declaration (loc, i)
  | loc, VariableDeclaration var -> variable_declaration (loc, var)
  | loc, FunctionDeclaration fn ->  Statement.FunctionDeclaration.(
      (* esprima/estree hasn't come around to the idea that function decls can
       * have optional ids :( *)
      let (node_type, node_value) = (
        match fn.id with
        | Some(id) -> "FunctionDeclaration", identifier id
        | None -> "FunctionExpression", null
      ) in

      let body =  (match fn.body with
      | BodyBlock b -> block b
      | BodyExpression b -> expression b) in

      node node_type loc [|
        "id", node_value;
        "params", array_of_list pattern fn.params;
        "defaults", array_of_list (option expression) fn.defaults;
        "rest", option identifier fn.rest;
        "body", body;
        "async", bool fn.async;
        "generator", bool fn.generator;
        "expression", bool fn.expression;
        "returnType", option type_annotation fn.returnType;
        "typeParameters", option type_parameter_declaration fn.typeParameters;
      |]
    )
    | loc, DeclareVariable d -> DeclareVariable.(
        node "DeclareVariable" loc [|
          "id", identifier d.id;
        |]
    )
    | loc, DeclareFunction d -> DeclareFunction.(
        node "DeclareFunction" loc [|
          "id", identifier d.id;
        |]
    )
    | loc, DeclareTypeAlias alias -> TypeAlias.(
        node "DeclareTypeAlias" loc [|
          "id", identifier alias.id;
          "typeParameters", option type_parameter_declaration alias.typeParameters;
          "right", _type alias.right;
        |]
    )
    | loc, DeclareClass d -> Statement.Interface.(
        node "DeclareClass" loc [|
          "id", identifier d.id;
          "typeParameters", option type_parameter_declaration d.typeParameters;
          "body", object_type d.body;
          "extends", array_of_list interface_extends d.extends;
        |]
      )
    | loc, DeclareModule m -> DeclareModule.(
        let id = match m.id with
        | Literal lit -> literal lit
        | Identifier id -> identifier id
        in
        node "DeclareModule" loc [|
          "id", id;
          "body", block m.body;
        |]
      )
    | loc, ExportDeclaration export -> ExportDeclaration.(
        let declaration = match export.declaration with
        | Some (Declaration stmt) -> statement stmt
        | Some (ExportDeclaration.Expression expr) -> expression expr
        | None -> null
        in
        let specifiers = match export.specifiers with
        | Some (ExportSpecifiers specifiers) ->
            array_of_list export_specifier specifiers
        | Some (ExportBatchSpecifier loc) ->
            array [| node "ExportBatchSpecifier" loc [||] |]
        | None ->
            array [||]
        in
        let exportKind = (
          match export.exportKind with
          | ExportType -> "type"
          | ExportValue -> "value"
        ) in
        node "ExportDeclaration" loc [|
          "default", bool export.default;
          "declaration", declaration;
          "specifiers", specifiers;
          "source", option literal export.source;
          "exportKind", string exportKind;
        |]
      )
    | loc, ImportDeclaration import -> ImportDeclaration.(
        let specifiers = (match import.default with
        | None -> []
        | Some default -> [import_default_specifier default]) in
        let specifiers = (match import.specifier with
        | Some (NameSpace ns) -> import_namespace_specifier ns :: specifiers
        | Some (Named (_, sl)) -> (List.rev (List.map import_specifier sl)) @ specifiers
        | None -> specifiers) in
        let import_kind = match import.importKind with
        | ImportType -> "type"
        | ImportTypeof -> "typeof"
        | ImportValue -> "value"
        in
        node "ImportDeclaration" loc [|
          "specifiers", array (Array.of_list (List.rev specifiers));
          "source", literal import.source;
          "importKind", string (import_kind);
        |]
    )
  )

  and expression = Expression.(function
    | loc, This -> node "ThisExpression" loc [||]
    | loc, Array arr ->
        node "ArrayExpression" loc [|
          "elements", array_of_list (option expression_or_spread) arr.Array.elements;
        |]
    | loc, Object _object ->
        node "ObjectExpression" loc [|
          "properties", array_of_list object_property _object.Object.properties;
        |]
    | loc, Function _function -> function_expression (loc, _function)
    | loc, ArrowFunction arrow -> ArrowFunction.(
        let body = (match arrow.body with
        | Statement.FunctionDeclaration.BodyBlock b -> block b
        | Statement.FunctionDeclaration.BodyExpression expr -> expression expr)
        in
        node "ArrowFunctionExpression" loc [|
          "id", option identifier arrow.id;
          "params", array_of_list pattern arrow.params;
          "defaults", array_of_list (option expression) arrow.defaults;
          "rest", option identifier arrow.rest;
          "body", body;
          "async", bool arrow.async;
          "generator", bool arrow.generator;
          "expression", bool arrow.expression;
          "returnType", option type_annotation arrow.returnType;
          "typeParameters", option type_parameter_declaration arrow.typeParameters;
        |]
      )
    | loc, Sequence sequence ->
        node "SequenceExpression" loc [|
          "expressions", array_of_list expression sequence.Sequence.expressions;
        |]
    | loc, Unary unary -> Unary.(
        match unary.operator with
        | Await ->
          (* await is defined as a separate expression in ast-types
           *
           * TODO
           * 1) Send a PR to ast-types
           *    (https://github.com/benjamn/ast-types/issues/113)
           * 2) Output a UnaryExpression
           * 3) Modify the esprima test runner to compare AwaitExpression and
           *    our UnaryExpression
           * *)
          node "AwaitExpression" loc [|
            "argument", expression unary.argument;
          |]
        | _ -> begin
          let operator = match unary.operator with
          | Minus -> "-"
          | Plus -> "+"
          | Not -> "!"
          | BitNot -> "~"
          | Typeof -> "typeof"
          | Void -> "void"
          | Delete -> "delete"
          | Await -> failwith "matched above"
          in
          node "UnaryExpression" loc [|
            "operator", string operator;
            "prefix", bool unary.prefix;
            "argument", expression unary.argument;
          |]
        end
      )
    | loc, Binary binary -> Binary.(
        let operator = match binary.operator with
        | Equal -> "=="
        | NotEqual -> "!="
        | StrictEqual -> "==="
        | StrictNotEqual -> "!=="
        | LessThan -> "<"
        | LessThanEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanEqual -> ">="
        | LShift -> "<<"
        | RShift -> ">>"
        | RShift3 -> ">>>"
        | Plus -> "+"
        | Minus -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Mod -> "%"
        | BitOr -> "|"
        | Xor -> "^"
        | BitAnd -> "&"
        | In -> "in"
        | Instanceof -> "instanceof"
        in
        node "BinaryExpression" loc [|
          "operator", string operator;
          "left", expression binary.left;
          "right", expression binary.right;
        |]
      )
    | loc, TypeCast typecast -> TypeCast.(
        node "TypeCastExpression" loc [|
          "expression", expression typecast.expression;
          "typeAnnotation", type_annotation typecast.typeAnnotation;
        |]
      )
    | loc, Assignment assignment -> Assignment.(
        let operator = match assignment.operator with
        | Assign -> "="
        | PlusAssign -> "+="
        | MinusAssign -> "-="
        | MultAssign -> "*="
        | DivAssign -> "/="
        | ModAssign -> "%="
        | LShiftAssign -> "<<="
        | RShiftAssign -> ">>="
        | RShift3Assign -> ">>>="
        | BitOrAssign -> "|="
        | BitXorAssign -> "^="
        | BitAndAssign -> "&="
        in
        node "AssignmentExpression" loc [|
          "operator", string operator;
          "left", pattern assignment.left;
          "right", expression assignment.right;
        |]
      )
    | loc, Update update -> Update.(
        let operator = match update.operator with
        | Increment -> "++"
        | Decrement -> "--"
        in
        node "UpdateExpression" loc [|
          "operator", string operator;
          "argument", expression update.argument;
          "prefix", bool update.prefix;
        |]
      )
    | loc, Logical logical -> Logical.(
        let operator = match logical.operator with
        | Or -> "||"
        | And -> "&&"
        in
        node "LogicalExpression" loc [|
          "operator", string operator;
          "left", expression logical.left;
          "right", expression logical.right;
        |]
      )
    | loc, Conditional conditional -> Conditional.(
        node "ConditionalExpression" loc [|
          "test", expression conditional.test;
          "consequent", expression conditional.consequent;
          "alternate", expression conditional.alternate;
        |]
      )
    | loc, New _new -> New.(
        node "NewExpression" loc [|
          "callee", expression _new.callee;
          "arguments", array_of_list expression_or_spread _new.arguments;
        |]
      )
    | loc, Call call -> Call.(
        node "CallExpression" loc [|
          "callee", expression call.callee;
          "arguments", array_of_list expression_or_spread call.arguments;
        |]
      )
    | loc, Member member -> Member.(
        let property = match member.property with
        | PropertyIdentifier id -> identifier id
        | PropertyExpression expr -> expression expr
        in
        node "MemberExpression" loc [|
          "object", expression member._object;
          "property", property;
          "computed", bool member.computed;
        |]
      )
    | loc, Yield yield -> Yield.(
        node "YieldExpression" loc [|
          "argument", expression yield.argument;
          "delegate", bool yield.delegate;
        |]
      )
    | loc, Comprehension comp -> Comprehension.(
        node "ComprehensionExpression" loc [|
          "blocks", array_of_list comprehension_block comp.blocks;
          "filter", option expression comp.filter;
        |]
      )
    | loc, Generator gen -> Generator.(
        node "GeneratorExpression" loc [|
          "blocks", array_of_list comprehension_block gen.blocks;
          "filter", option expression gen.filter;
        |]
      )
    | loc, Let _let -> Let.(
        node "LetExpression" loc [|
          "head", array_of_list let_assignment _let.head;
          "body", expression _let.body;
        |]
      )
    | _loc, Identifier id -> identifier id
    | loc, Literal lit -> literal (loc, lit)
    | loc, TemplateLiteral lit -> template_literal (loc, lit)
    | loc, TaggedTemplate tagged -> tagged_template (loc, tagged)
    | loc, Class c -> class_expression (loc, c)
    | loc, JSXElement element -> jsx_element (loc, element))

  and function_expression (loc, _function) = Expression.Function.(
    let body = match _function.body with
    | Statement.FunctionDeclaration.BodyBlock b -> block b
    | Statement.FunctionDeclaration.BodyExpression expr -> expression expr
    in
    node "FunctionExpression" loc [|
      "id", option identifier _function.id;
      "params", array_of_list pattern _function.params;
      "defaults", array_of_list (option expression) _function.defaults;
      "rest", option identifier _function.rest;
      "body", body;
      "async", bool _function.async;
      "generator", bool _function.generator;
      "expression", bool _function.expression;
      "returnType", option type_annotation _function.returnType;
      "typeParameters", option type_parameter_declaration _function.typeParameters;
    |]
  )

  and identifier (loc, id) = Identifier.(
    node "Identifier" loc [|
      "name", string id.name;
      "typeAnnotation", option type_annotation id.typeAnnotation;
      "optional", bool id.optional;
    |]
  )

  and case (loc, c) = Statement.Switch.Case.(
    node "SwitchCase" loc [|
      "test", option expression c.test;
      "consequent", array_of_list statement c.consequent;
    |]
  )

  and catch (loc, c) = Statement.Try.CatchClause.(
    node "CatchClause" loc [|
      "param", pattern c.param;
      "guard", option expression c.guard;
      "body", block c.body;
    |]
  )

  and block (loc, b) =
    node "BlockStatement" loc [|
      "body", statement_list b.Statement.Block.body;
    |]

  and let_assignment assignment = Statement.Let.(
    obj [|
      "id", pattern assignment.id;
      "init", option expression assignment.init;
    |]
  )

  and class_declaration (loc, c) = Class.(
    (* esprima/estree hasn't come around to the idea that class decls can have
     * optional ids :( *)
    let (node_type, node_value) = (
      match c.id with
      | Some(id) -> "ClassDeclaration", identifier id
      | None -> "ClassExpression", null
    ) in

    node node_type loc [|
      "id", node_value;
      "body", class_body c.body;
      "superClass", option expression c.superClass;
      "typeParameters", option type_parameter_declaration c.typeParameters;
      "superTypeParameters", option type_parameter_instantiation c.superTypeParameters;
      "implements", array_of_list class_implements c.implements;
    |]
  )

  and class_expression (loc, c) = Class.(
    node "ClassExpression" loc [|
      "id", option identifier c.id;
      "body", class_body c.body;
      "superClass", option expression c.superClass;
      "typeParameters", option type_parameter_declaration c.typeParameters;
      "superTypeParameters", option type_parameter_instantiation c.superTypeParameters;
      "implements", array_of_list class_implements c.implements;
    |]
  )

  and class_implements (loc, implements) = Class.Implements.(
    node "ClassImplements" loc [|
      "id", identifier implements.id;
      "typeParameters", option type_parameter_instantiation implements.typeParameters;
    |]
  )

  and class_body (loc, body) = Class.Body.(
    node "ClassBody" loc [|
      "body", array_of_list class_element body.body;
    |]
  )

  and class_element = Class.Body.(function
    | Method m -> class_method m
    | Property p -> class_property p)

  and class_method (loc, method_) =
    let { Class.Method.key; value; kind; static; decorators; } = method_ in
    let key, computed = Expression.Object.Property.(match key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true) in
    let kind = Class.Method.(match kind with
      | Constructor -> "constructor"
      | Method -> "method"
      | Get -> "get"
      | Set -> "set") in
    node "MethodDefinition" loc [|
      "key", key;
      "value", function_expression value;
      "kind", string kind;
      "static", bool static;
      "computed", bool computed;
      "decorators", array_of_list expression decorators;
    |]

  and class_property (loc, prop) = Class.Property.(
    let key, computed = (match prop.key with
    | Expression.Object.Property.Literal lit -> literal lit, false
    | Expression.Object.Property.Identifier id -> identifier id, false
    | Expression.Object.Property.Computed expr -> expression expr, true) in
    node "ClassProperty" loc [|
      "key", key;
      "value", option expression prop.value;
      "typeAnnotation", option type_annotation prop.typeAnnotation;
      "computed", bool computed;
      "static", bool prop.static;
    |]
  )

  and interface_declaration (loc, i) = Statement.Interface.(
    node "InterfaceDeclaration" loc [|
      "id", identifier i.id;
      "typeParameters", option type_parameter_declaration i.typeParameters;
      "body", object_type i.body;
      "extends", array_of_list interface_extends i.extends;
    |]
  )

  and interface_extends (loc, g) = Type.Generic.(
    let id = match g.id with
    | Identifier.Unqualified id -> identifier id
    | Identifier.Qualified q -> generic_type_qualified_identifier q
    in
    node "InterfaceExtends" loc [|
      "id", id;
      "typeParameters", option type_parameter_instantiation g.typeParameters;
    |]
  )

  and pattern = Pattern.(function
    | loc, Object obj ->
        node "ObjectPattern" loc [|
          "properties", array_of_list object_pattern_property obj.Object.properties;
          "typeAnnotation", option type_annotation obj.Object.typeAnnotation;
        |]
    | loc, Array arr ->
        node "ArrayPattern" loc [|
          "elements", array_of_list (option array_pattern_element) arr.Array.elements;
          "typeAnnotation", option type_annotation arr.Array.typeAnnotation;
        |]
    | _loc, Identifier id -> identifier id
    | _loc, Expression expr -> expression expr)

  and array_pattern_element = Pattern.Array.(function
    | Element p -> pattern p
    | Spread (loc, { SpreadElement.argument; }) ->
        node "SpreadElementPattern" loc [|
          "argument", pattern argument;
        |]
  )

  and object_property = Expression.Object.(function
    | Property (loc, prop) -> Property.(
      (* This is a slight deviation from the Mozilla spec. In the spec, an object
        * property is not a proper node, and lacks a location and a "type" field.
        * Esprima promotes it to a proper node and that is useful, so I'm
        * following their example *)
      let key, computed = (match prop.key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true)  in
      let kind = match prop.kind with
      | Init -> "init"
      | Get -> "get"
      | Set -> "set"
      in
      node "Property" loc [|
        "key", key;
        "value", expression prop.value;
        "kind", string kind;
        "method", bool prop._method;
        "shorthand", bool prop.shorthand;
        "computed", bool computed;
      |]
    )
  | SpreadProperty(loc, prop) -> SpreadProperty.(
    node "SpreadProperty" loc [|
      "argument", expression prop.argument;
    |]
  ))

  and object_pattern_property = Pattern.Object.(function
    | Property (loc, prop) -> Property.(
      let key, computed = (match prop.key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true) in
      node "PropertyPattern" loc [|
        "key", key;
        "pattern", pattern prop.pattern;
        "computed", bool computed;
      |]
    )
    | SpreadProperty (loc, prop) -> SpreadProperty.(
      node "SpreadPropertyPattern" loc [|
        "argument", pattern prop.argument;
      |]
    )
  )

  and expression_or_spread = Expression.(function
    | Expression expr -> expression expr
    | Spread (loc, { SpreadElement.argument; }) ->
        node "SpreadElement" loc [|
          "argument", expression argument;
        |]
  )

  and comprehension_block (loc, b) = Expression.Comprehension.Block.(
    node "ComprehensionBlock" loc [|
      "left", pattern b.left;
      "right", expression b.right;
      "each", bool b.each;
    |]
  )

  and literal (loc, lit) = Literal.(
    let { value; raw; } = lit in
    let value_ = match value with
    | String str -> string str
    | Boolean b -> bool b
    | Null -> null
    | Number f -> number f
    | RegExp { RegExp.pattern; flags; } -> regexp loc pattern flags
    in
    let props = match value with
    | RegExp { RegExp.pattern; flags; } ->
        let regex = obj [|
          "pattern", string pattern;
          "flags", string flags;
        |] in
        [| "value", value_; "raw", string raw; "regex", regex |]
    | _ ->
        [| "value", value_; "raw", string raw; |]
    in
    node "Literal" loc props
  )

  and template_literal (loc, value) = Expression.TemplateLiteral.(
    node "TemplateLiteral" loc [|
      "quasis", array_of_list template_element value.quasis;
      "expressions", array_of_list expression value.expressions;
    |]
  )

  and template_element (loc, element) = Expression.TemplateLiteral.Element.(
    let value = obj [|
      "raw", string element.value.raw;
      "cooked", string element.value.cooked;
    |] in
    node "TemplateElement" loc [|
      "value", value;
      "tail", bool element.tail;
    |]
  )

  and tagged_template (loc, tagged) = Expression.TaggedTemplate.(
    node "TaggedTemplateExpression" loc [|
      "tag", expression tagged.tag;
      "quasi", template_literal tagged.quasi;
    |]
  )

  and variable_declaration (loc, var) = Statement.VariableDeclaration.(
    let kind = match var.kind with
    | Var -> "var"
    | Let -> "let"
    | Const -> "const"
    in
    node "VariableDeclaration" loc [|
      "declarations", array_of_list variable_declarator var.declarations;
      "kind", string kind;
    |]
  )

  and variable_declarator (loc, declarator) =
    Statement.VariableDeclaration.Declarator.(
      node "VariableDeclarator" loc [|
        "id", pattern declarator.id;
        "init", option expression declarator.init;
      |]
    )

  and _type (loc, t) = Type.(
    match t with
    | Any -> any_type loc
    | Void -> void_type loc
    | Number -> number_type loc
    | String -> string_type loc
    | Boolean -> boolean_type loc
    | Nullable t -> nullable_type loc t
    | Function fn -> function_type (loc, fn)
    | Object o -> object_type (loc, o)
    | Array t -> array_type loc t
    | Generic g -> generic_type (loc, g)
    | Union u -> union_type (loc, u)
    | Intersection i -> intersection_type (loc, i)
    | Typeof t -> typeof_type (loc, t)
    | Tuple t -> tuple_type (loc, t)
    | StringLiteral s -> string_literal_type (loc, s)
    | NumberLiteral n -> number_literal_type (loc, n)
    | BooleanLiteral b -> boolean_literal_type (loc, b)
    | Exists -> exists_type loc
  )

  and any_type loc = node "AnyTypeAnnotation" loc [||]

  and void_type loc = node "VoidTypeAnnotation" loc [||]

  and number_type loc = node "NumberTypeAnnotation" loc [||]

  and string_type loc = node "StringTypeAnnotation" loc [||]

  and boolean_type loc = node "BooleanTypeAnnotation" loc [||]

  and nullable_type loc t =
    node "NullableTypeAnnotation" loc [|
      "typeAnnotation", _type t;
    |]

  and function_type (loc, fn) = Type.Function.(
    node "FunctionTypeAnnotation" loc [|
      "params", array_of_list function_type_param fn.params;
      "returnType", _type fn.returnType;
      "rest", option function_type_param fn.rest;
      "typeParameters", option type_parameter_declaration fn.typeParameters;
    |]
  )

  and function_type_param (loc, param) = Type.Function.Param.(
    node "FunctionTypeParam" loc [|
      "name", identifier param.name;
      "typeAnnotation", _type param.typeAnnotation;
      "optional", bool param.optional;
    |]
  )

  and object_type (loc, o) = Type.Object.(
    node "ObjectTypeAnnotation" loc [|
      "properties", array_of_list object_type_property o.properties;
      "indexers", array_of_list object_type_indexer o.indexers;
      "callProperties", array_of_list object_type_call_property o.callProperties;
    |]
  )

  and object_type_property (loc, prop) = Type.Object.Property.(
    let key = match prop.key with
    | Expression.Object.Property.Literal lit -> literal lit
    | Expression.Object.Property.Identifier id -> identifier id
    | Expression.Object.Property.Computed _ ->
      failwith "There should not be computed object type property keys"
    in
    node "ObjectTypeProperty" loc [|
      "key", key;
      "value", _type prop.value;
      "optional", bool prop.optional;
      "static", bool prop.static;
    |]
  )

  and object_type_indexer (loc, indexer) = Type.Object.Indexer.(
    node "ObjectTypeIndexer" loc [|
      "id", identifier indexer.id;
      "key", _type indexer.key;
      "value", _type indexer.value;
      "static", bool indexer.static;
    |]
  )

  and object_type_call_property (loc, callProperty) = Type.Object.CallProperty.(
    node "ObjectTypeCallProperty" loc [|
      "value", function_type callProperty.value;
      "static", bool callProperty.static;
    |]
  )

  and array_type loc t =
    node "ArrayTypeAnnotation" loc [|
      "elementType", (_type t);
    |]

  and generic_type_qualified_identifier (loc, q) = Type.Generic.Identifier.(
    let qualification = match q.qualification with
    | Unqualified id -> identifier id
    | Qualified q -> generic_type_qualified_identifier q
    in
    node "QualifiedTypeIdentifier" loc [|
      "qualification", qualification;
      "id", identifier q.id;
    |]
  )

  and generic_type (loc, g) = Type.Generic.(
    let id = match g.id with
    | Identifier.Unqualified id -> identifier id
    | Identifier.Qualified q -> generic_type_qualified_identifier q
    in
    node "GenericTypeAnnotation" loc [|
      "id", id;
      "typeParameters", option type_parameter_instantiation g.typeParameters;
    |]
  )

  and union_type (loc, tl) =
    node "UnionTypeAnnotation" loc [|
      "types", array_of_list _type tl;
    |]

  and intersection_type (loc, tl) =
    node "IntersectionTypeAnnotation" loc [|
      "types", array_of_list _type tl;
    |]

  and typeof_type (loc, t) =
    node "TypeofTypeAnnotation" loc [|
      "argument", _type t;
    |]

  and tuple_type (loc, tl) =
    node "TupleTypeAnnotation" loc [|
      "types", array_of_list _type tl;
    |]

  and string_literal_type (loc, s) = Type.StringLiteral.(
    node "StringLiteralTypeAnnotation" loc [|
      "value", string s.value;
      "raw", string s.raw;
    |]
  )

  and number_literal_type (loc, s) = Type.NumberLiteral.(
    node "NumberLiteralTypeAnnotation" loc [|
      "value", number s.value;
      "raw", string s.raw;
    |]
  )

  and boolean_literal_type (loc, s) = Type.BooleanLiteral.(
    node "BooleanLiteralTypeAnnotation" loc [|
      "value", bool s.value;
      "raw", string s.raw;
    |]
  )

  and exists_type loc = node "ExistsTypeAnnotation" loc [||]

  and type_annotation (loc, ty) =
    node "TypeAnnotation" loc [|
      "typeAnnotation", _type ty;
    |]

  and type_parameter_declaration (loc, params) = Type.ParameterDeclaration.(
    node "TypeParameterDeclaration" loc [|
      "params", array_of_list identifier params.params;
    |]
  )

  and type_parameter_instantiation (loc, params) = Type.ParameterInstantiation.(
    node "TypeParameterInstantiation" loc [|
      "params", array_of_list _type params.params;
    |]
  )

  and jsx_element (loc, (element: JSX.element)) = JSX.(
    node "JSXElement" loc [|
      "openingElement", jsx_opening element.openingElement;
      "closingElement", option jsx_closing element.closingElement;
      "children", array_of_list jsx_child element.children;
    |]
  )

  and jsx_opening (loc, opening) = JSX.Opening.(
    node "JSXOpeningElement" loc [|
      "name", jsx_name opening.name;
      "attributes", array_of_list jsx_opening_attribute opening.attributes;
      "selfClosing", bool opening.selfClosing;
    |]
  )

  and jsx_opening_attribute = JSX.Opening.(function
    | Attribute attribute -> jsx_attribute attribute
    | SpreadAttribute attribute -> jsx_spread_attribute attribute
  )

  and jsx_closing (loc, closing) = JSX.Closing.(
    node "JSXClosingElement" loc [|
      "name", jsx_name closing.name;
    |]
  )

  and jsx_child = JSX.(function
    | loc, Element element -> jsx_element (loc, element)
    | loc, ExpressionContainer expr -> jsx_expression_container (loc, expr)
    | loc, Text str -> jsx_text (loc, str)
  )

  and jsx_name = JSX.(function
    | Identifier id -> jsx_identifier id
    | NamespacedName namespaced_name -> jsx_namespaced_name namespaced_name
    | MemberExpression member -> jsx_member_expression member
  )

  and jsx_attribute (loc, attribute) = JSX.Attribute.(
    let name = match attribute.name with
    | Identifier id -> jsx_identifier id
    | NamespacedName namespaced_name -> jsx_namespaced_name namespaced_name
    in
    node "JSXAttribute" loc [|
      "name", name;
      "value", option jsx_attribute_value attribute.value;
    |]
  )

  and jsx_attribute_value = JSX.Attribute.(function
    | Literal (loc, value) -> literal (loc, value)
    | ExpressionContainer (loc, expr) -> jsx_expression_container (loc, expr)
  )

  and jsx_spread_attribute (loc, attribute) = JSX.SpreadAttribute.(
    node "JSXSpreadAttribute" loc [|
      "argument", expression attribute.argument;
    |]
  )

  and jsx_expression_container (loc, expr) = JSX.ExpressionContainer.(
    let expression = match expr.expression with
    | Some expr -> expression expr
    | None ->
        (* I think the JSXEmptyExpression is a little stupid...I think null
         * would be a better choice but oh well. Anyway, the location for an
         * empty expression doesn't really make sense, so this is as good a
         * choice as any *)
        node "JSXEmptyExpression" loc [||]
    in
    node "JSXExpressionContainer" loc [|
      "expression", expression;
    |]
  )

  and jsx_text (loc, text) = JSX.Text.(
    let { value; raw; } = text in
    node "JSXText" loc [|
      "value", string value;
      "raw", string raw;
    |]
  )

  and jsx_member_expression (loc, member_expression) = JSX.MemberExpression.(
    let _object = match member_expression._object with
    | Identifier id -> jsx_identifier id
    | MemberExpression member -> jsx_member_expression member in
    node "JSXMemberExpression" loc [|
      "object", _object;
      "property", jsx_identifier member_expression.property;
    |]
  )

  and jsx_namespaced_name (loc, namespaced_name) = JSX.NamespacedName.(
    node "JSXNamespacedName" loc [|
      "namespace", jsx_identifier namespaced_name.namespace;
      "name", jsx_identifier namespaced_name.name;
    |]
  )

  and jsx_identifier (loc, id) = JSX.Identifier.(
    node "JSXIdentifier" loc [|
      "name", string id.name;
    |]
  )

  and export_specifier (loc, specifier) = Statement.ExportDeclaration.Specifier.(
    node "ExportSpecifier" loc [|
      "id", identifier specifier.id;
      "name", option identifier specifier.name;
    |]
  )

  and import_default_specifier id =
    node "ImportDefaultSpecifier" (fst id) [|
      "id", identifier id;
    |]

  and import_namespace_specifier (loc, id) =
    node "ImportNamespaceSpecifier" loc [|
      "id", identifier id;
    |]

  and import_specifier (loc, specifier) = Statement.ImportDeclaration.NamedSpecifier.(
    node "ImportSpecifier" loc [|
      "id", identifier specifier.id;
      "name", option identifier specifier.name;
    |]
  )

  and comment_list comments = array_of_list comment comments

  and comment (loc, c) = Comment.(
    let _type, value = match c with
      | Line s -> "Line", s
      | Block s -> "Block", s in
    node _type loc [|
      "value", string value;
    |]
  )
end
