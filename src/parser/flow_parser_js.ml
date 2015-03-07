(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Translate = struct
  module Ast = Spider_monkey_ast
  open Ast

  let string = Js.string
  let int x = Js.number_of_float (float x)
  let bool = Js.bool
  let array fn list = Js.array (Array.of_list (List.map fn list))
  let new_obj () = Js.Unsafe.obj [||]
  let translation_errors = ref []

  let option f = function
    | Some v -> f v
    | None -> Js.null

  let position p =
    let ret = new_obj () in
    Js.Unsafe.set ret "line" (int p.Loc.line);
    Js.Unsafe.set ret "column" (int p.Loc.column);
    ret

  let loc location =
    let ret = new_obj () in
    Js.Unsafe.set ret "source" (option (fun x -> Js.some (string x)) location.Loc.source);
    Js.Unsafe.set ret "start" (position location.Loc.start);
    Js.Unsafe.set ret "end" (position location.Loc._end);
    ret

  let range location = Loc.(
    Js.array (Array.of_list [location.start.offset; location._end.offset;])
  )

  let node _type location =
    let ret = Js.Unsafe.obj [||] in
    Js.Unsafe.set ret "type" (string _type);
    Js.Unsafe.set ret "loc" (loc location);
    Js.Unsafe.set ret "range" (range location);
    ret

  let errors l =
    let error (location, e) =
      let ret = new_obj () in
      Js.Unsafe.set ret "loc" (loc location);
      Js.Unsafe.set ret "message" (string (Parse_error.PP.error e));
      ret
    in array error l

  let rec program (loc, statements, comments) =
    translation_errors := [];
    let ret =  node "Program" loc in
    Js.Unsafe.set ret "body" (statement_list statements);
    Js.Unsafe.set ret "comments" (comment_list comments);
    ret, !translation_errors

  and statement_list statements = array statement statements
  and statement = Statement.(function
  | loc, Empty -> node "EmptyStatement" loc
  | loc, Block b -> block (loc, b)
  | loc, Expression expr ->
      let ret = node "ExpressionStatement" loc in
      Js.Unsafe.set ret "expression" (expression expr.Expression.expression);
      ret
  | loc, If _if -> If.(
      let ret = node "IfStatement" loc in
      Js.Unsafe.set ret "test" (expression _if.test);
      Js.Unsafe.set ret "consequent" (statement _if.consequent);
      Js.Unsafe.set ret "alternate" (option statement _if.alternate);
      ret
    )
  | loc, Labeled labeled -> Labeled.(
      let ret = node "LabeledStatement" loc in
      Js.Unsafe.set ret "label" (identifier labeled.label);
      Js.Unsafe.set ret "body" (statement labeled.body);
      ret
    )
  | loc, Break break ->
      let ret = node "BreakStatement" loc in
      Js.Unsafe.set ret "label" (option identifier break.Break.label);
      ret
  | loc, Continue continue ->
      let ret = node "ContinueStatement" loc in
      Js.Unsafe.set ret "label" (option identifier continue.Continue.label);
      ret
  | loc, With _with -> With.(
      let ret = node "WithStatement" loc in
      Js.Unsafe.set ret "object" (expression _with._object);
      Js.Unsafe.set ret "body" (statement _with.body);
      ret
    )
  | loc, TypeAlias alias -> TypeAlias.(
      let ret = node "TypeAlias" loc in
      Js.Unsafe.set ret "id" (identifier alias.id);
      Js.Unsafe.set ret "typeParameters" (option type_parameter_declaration alias.typeParameters);
      Js.Unsafe.set ret "right" (_type alias.right);
      ret
  )
  | loc, Switch switch -> Switch.(
      let ret = node "SwitchStatement" loc in
      Js.Unsafe.set ret "discriminant" (expression switch.discriminant);
      Js.Unsafe.set ret "cases" (array case switch.cases);
      Js.Unsafe.set ret "lexical" (bool switch.lexical);
      ret
    )
  | loc, Return return ->
      let ret = node "ReturnStatement" loc in
      Js.Unsafe.set ret "argument" (option expression return.Return.argument);
      ret
  | loc, Throw throw ->
      let ret = node "ThrowStatement" loc in
      Js.Unsafe.set ret "argument" (expression throw.Throw.argument);
      ret
  | loc, Try _try -> Try.(
      let ret = node "TryStatement" loc in
      Js.Unsafe.set ret "block" (block _try.block);
      Js.Unsafe.set ret "handler" (option catch _try.handler);
      Js.Unsafe.set ret "guardedHandlers" (array catch _try.guardedHandlers);
      Js.Unsafe.set ret "finalizer" (option block _try.finalizer);
      ret
    )
  | loc, While _while -> While.(
      let ret = node "WhileStatement" loc in
      Js.Unsafe.set ret "test" (expression _while.test);
      Js.Unsafe.set ret "body" (statement _while.body);
      ret
    )
  | loc, DoWhile dowhile -> DoWhile.(
      let ret = node "DoWhileStatement" loc in
      Js.Unsafe.set ret "body" (statement dowhile.body);
      Js.Unsafe.set ret "test" (expression dowhile.test);
      ret
    )
  | loc, For _for -> For.(
      let ret = node "ForStatement" loc in
      Js.Unsafe.set ret "init" (option (function
        | InitDeclaration init -> variable_declaration init
        | InitExpression expr -> expression expr)
        _for.init);
      Js.Unsafe.set ret "test" (option expression _for.test);
      Js.Unsafe.set ret "update" (option expression _for.update);
      Js.Unsafe.set ret "body" (statement _for.body);
      ret
    )
  | loc, ForIn forin -> ForIn.(
      let ret = node "ForInStatement" loc in
      let left = (match forin.left with
      | LeftDeclaration left -> variable_declaration left
      | LeftExpression left -> expression left) in
      Js.Unsafe.set ret "left" (left);
      Js.Unsafe.set ret "right" (expression forin.right);
      Js.Unsafe.set ret "body" (statement forin.body);
      Js.Unsafe.set ret "each" (bool forin.each);
      ret
    )
  | loc, ForOf forof -> ForOf.(
      let ret = node "ForOfStatement" loc in
      let left = (match forof.left with
      | LeftDeclaration left -> variable_declaration left
      | LeftExpression left -> expression left) in
      Js.Unsafe.set ret "left" (left);
      Js.Unsafe.set ret "right" (expression forof.right);
      Js.Unsafe.set ret "body" (statement forof.body);
      ret
    )
  | loc, Let _let -> Let.(
      let ret = node "LetStatement" loc in
      Js.Unsafe.set ret "head" (array let_assignment _let.head);
      Js.Unsafe.set ret "body" (statement _let.body);
      ret
    )
  | loc, Debugger -> node "DebuggerStatement" loc
  | loc, ClassDeclaration c -> class_declaration (loc, c)
  | loc, InterfaceDeclaration i -> interface_declaration (loc, i)
  | loc, VariableDeclaration var ->  variable_declaration (loc, var)
  | loc, FunctionDeclaration fn ->  Statement.FunctionDeclaration.(
      let ret = node "FunctionDeclaration" loc in
      Js.Unsafe.set ret "id" (identifier fn.id);
      Js.Unsafe.set ret "params" (array pattern fn.params);
      Js.Unsafe.set ret "defaults" (array (option expression) fn.defaults);
      Js.Unsafe.set ret "rest" (option identifier fn.rest);
      let body =  (match fn.body with
      | BodyBlock b -> block b
      | BodyExpression b -> expression b) in
      Js.Unsafe.set ret "body" (body);
      Js.Unsafe.set ret "generator" (bool fn.generator);
      Js.Unsafe.set ret "expression" (bool fn.expression);
      Js.Unsafe.set ret "returnType" (option type_annotation fn.returnType);
      Js.Unsafe.set ret "typeParameters" (option type_parameter_declaration fn.typeParameters);
      ret
    )
    | loc, DeclareVariable d -> DeclareVariable.(
        let ret = node "DeclareVariable" loc in
        Js.Unsafe.set ret "id" (identifier d.id);
        ret
    )
    | loc, DeclareFunction d -> DeclareFunction.(
        let ret = node "DeclareFunction" loc in
        Js.Unsafe.set ret "id" (identifier d.id);
        ret
    )
    | loc, DeclareClass d ->
        let ret = interface_declaration (loc, d) in
        Js.Unsafe.set ret "type" (string "DeclareClass");
        ret
    | loc, DeclareModule m -> DeclareModule.(
        let ret = node "DeclareModule" loc in
        Js.Unsafe.set ret "id" (match m.id with
        | Literal lit -> literal lit
        | Identifier id -> identifier id);
        Js.Unsafe.set ret "body" (block m.body);
        ret
      )
    | loc, ExportDeclaration export -> ExportDeclaration.(
        let ret = node "ExportDeclaration" loc in
        Js.Unsafe.set ret "default" (bool export.default);
        Js.Unsafe.set ret "declaration" (match export.declaration with
        | Some (Declaration stmt) -> statement stmt
        | Some (ExportDeclaration.Expression expr) -> expression expr
        | None -> Js.null);
        Js.Unsafe.set ret "specifiers" (match export.specifiers with
        | Some (ExportSpecifiers specifiers) ->
            array export_specifier specifiers
        | Some (ExportBatchSpecifier loc) ->
            Js.array (Array.of_list [node "ExportBatchSpecifier" loc])
        | None ->
            Js.array (Array.of_list []));
        Js.Unsafe.set ret "source" (option module_specifier export.source);
        ret
      )
    | loc, ImportDeclaration import -> ImportDeclaration.(
        let ret = node "ImportDeclaration" loc in
        let specifiers = (match import.default with
        | None -> []
        | Some default -> [import_default_specifier default]) in
        let specifiers = (match import.specifier with
        | Some (NameSpace ns) -> import_namespace_specifier ns :: specifiers
        | Some (Named (_, sl)) -> (List.rev (List.map import_specifier sl)) @ specifiers
        | None -> specifiers) in
        Js.Unsafe.set ret "specifiers" (Js.array (Array.of_list (List.rev specifiers)));
        Js.Unsafe.set ret "source" (option module_specifier import.source);
        Js.Unsafe.set ret "isType" (bool import.isType);
        ret
    )
  )

  and expression = Expression.(function
    | loc, This -> node "ThisExpression" loc
    | loc, Array arr ->
        let ret = node "ArrayExpression" loc in
        Js.Unsafe.set ret "elements" (array (option expression_or_spread) arr.Array.elements);
        ret
    | loc, Object _object ->
        let ret = node "ObjectExpression" loc in
        Js.Unsafe.set ret "properties" (array object_property _object.Object.properties);
        ret
    | loc, Function _function -> function_expression (loc, _function)
    | loc, ArrowFunction arrow -> ArrowFunction.(
        let ret = node "ArrowFunctionExpression" loc in
        Js.Unsafe.set ret "id" (option identifier arrow.id);
        Js.Unsafe.set ret "params" (array pattern arrow.params);
        Js.Unsafe.set ret "defaults" (array (option expression) arrow.defaults);
        Js.Unsafe.set ret "rest" (option identifier arrow.rest);
        let body = (match arrow.body with
        | Statement.FunctionDeclaration.BodyBlock b -> block b
        | Statement.FunctionDeclaration.BodyExpression expr -> expression expr) in
        Js.Unsafe.set ret "body" (body);
        Js.Unsafe.set ret "generator" (bool arrow.generator);
        Js.Unsafe.set ret "expression" (bool arrow.expression);
        ret
      )
    | loc, Sequence sequence ->
        let ret = node "SequenceExpression" loc in
        Js.Unsafe.set ret "expressions" (array expression sequence.Sequence.expressions);
        ret
    | loc, Unary unary -> Unary.(
        let ret = node "UnaryExpression" loc in
        let operator = string (match unary.operator with
        | Minus -> "-"
        | Plus -> "+"
        | Not -> "!"
        | BitNot -> "~"
        | Typeof -> "typeof"
        | Void -> "void"
        | Delete -> "delete") in
        Js.Unsafe.set ret "operator" (operator);
        Js.Unsafe.set ret "prefix" (bool unary.prefix);
        Js.Unsafe.set ret "argument" (expression unary.argument);
        ret
      )
    | loc, Binary binary -> Binary.(
        let ret = node "BinaryExpression" loc in
        Js.Unsafe.set ret "operator" (string (match binary.operator with
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
        | Instanceof -> "instanceof"));
        Js.Unsafe.set ret "left" (expression binary.left);
        Js.Unsafe.set ret "right" (expression binary.right);
        ret
      )
    | loc, TypeCast typecast -> TypeCast.(
        let ret = node "TypeCastExpression" loc in
        Js.Unsafe.set ret "expression" (expression typecast.expression);
        Js.Unsafe.set ret "typeAnnotation"
          (type_annotation typecast.typeAnnotation);
        ret
      )
    | loc, Assignment assignment -> Assignment.(
        let ret = node "AssignmentExpression" loc in
        Js.Unsafe.set ret "operator" (string (match assignment.operator with
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
        | BitAndAssign -> "&="));
        Js.Unsafe.set ret "left" (pattern assignment.left);
        Js.Unsafe.set ret "right" (expression assignment.right);
        ret
      )
    | loc, Update update -> Update.(
        let ret = node "UpdateExpression" loc in
        Js.Unsafe.set ret "operator" (string (match update.operator with
        | Increment -> "++"
        | Decrement -> "--"));
        Js.Unsafe.set ret "argument" (expression update.argument);
        Js.Unsafe.set ret "prefix" (bool update.prefix);
        ret
      )
    | loc, Logical logical -> Logical.(
        let ret = node "LogicalExpression" loc in
        Js.Unsafe.set ret "operator" (string (match logical.operator with
        | Or -> "||"
        | And -> "&&"));
        Js.Unsafe.set ret "left" (expression logical.left);
        Js.Unsafe.set ret "right" (expression logical.right);
        ret
      )
    | loc, Conditional conditional -> Conditional.(
        let ret = node "ConditionalExpression" loc in
        Js.Unsafe.set ret "test" (expression conditional.test);
        Js.Unsafe.set ret "consequent" (expression conditional.consequent);
        Js.Unsafe.set ret "alternate" (expression conditional.alternate);
        ret
      )
    | loc, New _new -> New.(
        let ret = node "NewExpression" loc in
        Js.Unsafe.set ret "callee" (expression _new.callee);
        Js.Unsafe.set ret "arguments" (array expression_or_spread _new.arguments);
        ret
      )
    | loc, Call call -> Call.(
        let ret = node "CallExpression" loc in
        Js.Unsafe.set ret "callee" (expression call.callee);
        Js.Unsafe.set ret "arguments" (array expression_or_spread call.arguments);
        ret
      )
    | loc, Member member -> Member.(
        let ret = node "MemberExpression" loc in
        Js.Unsafe.set ret "object" (expression member._object);
        Js.Unsafe.set ret "property" ((match member.property with
        | PropertyIdentifier id -> identifier id
        | PropertyExpression expr -> expression expr));
        Js.Unsafe.set ret "computed" (bool member.computed);
        ret
      )
    | loc, Yield yield -> Yield.(
        let ret = node "YieldExpression" loc in
        Js.Unsafe.set ret "argument" (expression yield.argument);
        Js.Unsafe.set ret "delegate" (bool yield.delegate);
        ret
      )
    | loc, Comprehension comp -> Comprehension.(
        let ret = node "ComprehensionExpression" loc in
        Js.Unsafe.set ret "blocks" (array comprehension_block comp.blocks);
        Js.Unsafe.set ret "filter" (option expression comp.filter);
        ret
      )
    | loc, Generator gen -> Generator.(
        let ret = node "GeneratorExpression" loc in
        Js.Unsafe.set ret "blocks" (array comprehension_block gen.blocks);
        Js.Unsafe.set ret "filter" (option expression gen.filter);
        ret
      )
    | loc, Let _let -> Let.(
        let ret = node "LetExpression" loc in
        Js.Unsafe.set ret "head" (array let_assignment _let.head);
        Js.Unsafe.set ret "body" (expression _let.body);
        ret
      )
    | _loc, Identifier id -> identifier id
    | loc, Literal lit -> literal (loc, lit)
    | loc, TemplateLiteral lit -> template_literal (loc, lit)
    | loc, TaggedTemplate tagged -> tagged_template (loc, tagged)
    | loc, Class c -> class_expression (loc, c)
    | loc, JSXElement element -> jsx_element (loc, element))

  and function_expression (loc, _function) = Expression.Function.(
    let ret = node "FunctionExpression" loc in
    Js.Unsafe.set ret "id" (option identifier _function.id);
    Js.Unsafe.set ret "params" (array pattern _function.params);
    Js.Unsafe.set ret "defaults" (array (option expression) _function.defaults);
    Js.Unsafe.set ret "rest" (option identifier _function.rest);
    Js.Unsafe.set ret "body" (match _function.body with
    | Statement.FunctionDeclaration.BodyBlock b -> block b
    | Statement.FunctionDeclaration.BodyExpression expr -> expression expr);
    Js.Unsafe.set ret "generator" (bool _function.generator);
    Js.Unsafe.set ret "expression" (bool _function.expression);
    Js.Unsafe.set ret "returnType" (option type_annotation _function.returnType);
    Js.Unsafe.set ret "typeParameters" (option type_parameter_declaration _function.typeParameters);
    ret
  )

  and identifier (loc, id) = Identifier.(
    let ret = node "Identifier" loc in
    Js.Unsafe.set ret "name" (string id.name);
    Js.Unsafe.set ret "typeAnnotation" (option type_annotation id.typeAnnotation);
    Js.Unsafe.set ret "optional" (bool id.optional);
    ret
  )

  and case (loc, c) = Statement.Switch.Case.(
    let ret = node "SwitchCase" loc in
    Js.Unsafe.set ret "test" (option expression c.test);
    Js.Unsafe.set ret "consequent" (array statement c.consequent);
    ret
  )

  and catch (loc, c) = Statement.Try.CatchClause.(
    let ret = node "CatchClause" loc in
    Js.Unsafe.set ret "param" (pattern c.param);
    Js.Unsafe.set ret "guard" (option expression c.guard);
    Js.Unsafe.set ret "body" (block c.body);
    ret
  )

  and block (loc, b) =
    let ret = node "BlockStatement" loc in
    Js.Unsafe.set ret "body" (statement_list b.Statement.Block.body);
    ret

  and let_assignment assignment = Statement.Let.(
    let ret = new_obj () in
    Js.Unsafe.set ret "id" (pattern assignment.id);
    Js.Unsafe.set ret "init" (option expression assignment.init);
    ret
  )

  and class_declaration (loc, c) = Statement.Class.(
    let ret = node "ClassDeclaration" loc in
    Js.Unsafe.set ret "id" (identifier c.id);
    Js.Unsafe.set ret "body" (class_body c.body);
    Js.Unsafe.set ret "superClass" (option expression c.superClass);
    Js.Unsafe.set ret "typeParameters" (option type_parameter_declaration c.typeParameters);
    Js.Unsafe.set ret "superTypeParameters" (option type_parameter_instantiation c.superTypeParameters);
    Js.Unsafe.set ret "implements" (array class_implements c.implements);
    ret
  )

  and class_expression (loc, c) = Expression.Class.(
    let ret = node "ClassExpression" loc in
    Js.Unsafe.set ret "id" (option identifier c.id);
    Js.Unsafe.set ret "body" (class_body c.body);
    Js.Unsafe.set ret "superClass" (option expression c.superClass);
    Js.Unsafe.set ret "typeParameters" (option type_parameter_declaration c.typeParameters);
    Js.Unsafe.set ret "superTypeParameters" (option type_parameter_instantiation c.superTypeParameters);
    Js.Unsafe.set ret "implements" (array class_implements c.implements);
    ret
  )

  and class_implements (loc, implements) = Statement.Class.Implements.(
    let ret = node "ClassImplements" loc in
    Js.Unsafe.set ret "id" (identifier implements.id);
    Js.Unsafe.set ret "typeParameters" (option type_parameter_instantiation implements.typeParameters);
    ret
  )

  and class_body (loc, body) = Statement.Class.Body.(
    let ret = node "ClassBody" loc in
    Js.Unsafe.set ret "body" (array class_element body.body);
    ret
  )

  and class_element = Statement.Class.Body.(function
    | Method m -> class_method m
    | Property p -> class_property p)

  and class_method (loc, method_) =
    let { Statement.Class.Method.key; value; kind; static; } = method_ in
    Expression.Object.Property.(
      let ret = node "MethodDefinition" loc in
      let key, computed = (match key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true) in
      Js.Unsafe.set ret "key" key;
      Js.Unsafe.set ret "value" (function_expression value);
      Js.Unsafe.set ret "kind" (string (match kind with
      | Init -> "init"
      | Get -> "get"
      | Set -> "set"));
      Js.Unsafe.set ret "static" (bool static);
      Js.Unsafe.set ret "computed" (bool computed);
      ret
    )

  and class_property (loc, prop) = Statement.Class.Property.(
    let ret = node "ClassProperty" loc in
    let key, computed = (match prop.key with
    | Expression.Object.Property.Literal lit -> literal lit, false
    | Expression.Object.Property.Identifier id -> identifier id, false
    | Expression.Object.Property.Computed expr -> expression expr, true) in
    Js.Unsafe.set ret "key" key;
    Js.Unsafe.set ret "typeAnnotation" (type_annotation prop.typeAnnotation);
    Js.Unsafe.set ret "computed" (bool computed);
    Js.Unsafe.set ret "static" (bool prop.static);
    ret
  )

  and interface_declaration (loc, i) = Statement.Interface.(
    let ret = node "InterfaceDeclaration" loc in
    Js.Unsafe.set ret "id" (identifier i.id);
    Js.Unsafe.set ret "typeParameters" (option type_parameter_declaration i.typeParameters);
    Js.Unsafe.set ret "body" (object_type i.body);
    Js.Unsafe.set ret "extends" (array interface_extends i.extends);
    ret
  )

  and interface_extends (loc, extends) = Statement.Interface.Extends.(
    let ret = node "InterfaceExtends" loc in
    Js.Unsafe.set ret "id" (identifier extends.id);
    Js.Unsafe.set ret "typeParameters" (option type_parameter_instantiation extends.typeParameters);
    ret
  )

  and pattern = Pattern.(function
    | loc, Object obj ->
        let ret = node "ObjectPattern" loc in
        Js.Unsafe.set ret "properties" (array object_pattern_property obj.Object.properties);
        Js.Unsafe.set ret "typeAnnotation" (option type_annotation obj.Object.typeAnnotation);
        ret
    | loc, Array arr ->
        let ret = node "ArrayPattern" loc in
        Js.Unsafe.set ret "elements" (array (option array_pattern_element) arr.Array.elements);
        Js.Unsafe.set ret "typeAnnotation" (option type_annotation arr.Array.typeAnnotation);
        ret
    | _loc, Identifier id -> identifier id
    | _loc, Expression expr -> expression expr)

  and array_pattern_element = Pattern.Array.(function
    | Element p -> pattern p
    | Spread (loc, { SpreadElement.argument; }) ->
        let ret = node "SpreadElementPattern" loc in
        Js.Unsafe.set ret "argument" (pattern argument);
        ret
  )

  and object_property = Expression.Object.(function
    | Property (loc, prop) -> Property.(
      (* This is a slight deviation from the Mozilla spec. In the spec, an object
        * property is not a proper node, and lacks a location and a "type" field.
        * Esprima promotes it to a proper node and that is useful, so I'm
        * following their example *)
      let ret = node "Property" loc in
      let key, computed = (match prop.key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true)  in
      Js.Unsafe.set ret "key" key;
      Js.Unsafe.set ret "value" (expression prop.value);
      Js.Unsafe.set ret "kind" (string (match prop.kind with
      | Init -> "init"
      | Get -> "get"
      | Set -> "set"));
      Js.Unsafe.set ret "method" (bool prop._method);
      Js.Unsafe.set ret "shorthand" (bool prop.shorthand);
      Js.Unsafe.set ret "computed" (bool computed);
      ret
    )
  | SpreadProperty(loc, prop) -> SpreadProperty.(
    let ret = node "SpreadProperty" loc in
    Js.Unsafe.set ret "argument" (expression prop.argument);
    ret
  ))

  and object_pattern_property = Pattern.Object.(function
    | Property (loc, prop) -> Property.(
      let ret = node "PropertyPattern"  loc in
      let key, computed =  (match prop.key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true) in
      Js.Unsafe.set ret "key" key;
      Js.Unsafe.set ret "pattern" (pattern prop.pattern);
      Js.Unsafe.set ret "computed" (bool computed);
      ret
    )
    | SpreadProperty (loc, prop) -> SpreadProperty.(
      let ret = node "SpreadPropertyPattern" loc in
      Js.Unsafe.set ret "argument" (pattern prop.argument);
      ret
    )
  )

  and expression_or_spread = Expression.(function
    | Expression expr -> expression expr
    | Spread (loc, { SpreadElement.argument; }) ->
        let ret = node "SpreadElement" loc in
        Js.Unsafe.set ret "argument" (expression argument);
        ret
  )

  and comprehension_block (loc, b) = Expression.Comprehension.Block.(
    let ret = node "ComprehensionBlock" loc in
    Js.Unsafe.set ret "left" (pattern b.left);
    Js.Unsafe.set ret "right" (expression b.right);
    Js.Unsafe.set ret "each" (bool b.each);
    ret
  )

  and literal (loc, lit) = Literal.(
    let { value; raw; } = lit in
    let ret = node "Literal" loc in
    (match value with
      | String str ->
          Js.Unsafe.set ret "value" (string str)
      | Boolean b ->
          Js.Unsafe.set ret "value" (bool b)
      | Null ->
          Js.Unsafe.set ret "value" (Js.null)
      | Number f ->
          Js.Unsafe.set ret "value" (Js.number_of_float f)
      | RegExp { RegExp.pattern; flags; } ->
          let regexp = try Js.Unsafe.new_obj (Js.Unsafe.variable "RegExp") [|
            Js.Unsafe.inject (string pattern);
            Js.Unsafe.inject (string flags);
          |]
          with _ ->
            translation_errors := (loc, Parse_error.InvalidRegExp)::!translation_errors;
            (* Invalid RegExp. We already validated the flags, but we've been
             * too lazy to write a JS regexp parser in Ocaml, so we didn't know
             * the pattern was invalid. We'll recover with an empty pattern.
             *)
            Js.Unsafe.new_obj (Js.Unsafe.variable "RegExp") [|
              Js.Unsafe.inject ("");
              Js.Unsafe.inject (string flags);
            |]
          in
          Js.Unsafe.set ret "value" regexp
    );
    Js.Unsafe.set ret "raw" (string raw);
    ret
  )

  and template_literal (loc, value) = Expression.TemplateLiteral.(
    let ret = node "TemplateLiteral" loc in
    Js.Unsafe.set ret "quasis" (array template_element value.quasis);
    Js.Unsafe.set ret "expressions" (array expression value.expressions);
    ret
  )

  and template_element (loc, element) = Expression.TemplateLiteral.Element.(
    let ret = node "TemplateElement" loc in
    let value = new_obj () in
    Js.Unsafe.set value "raw" (string element.value.raw);
    Js.Unsafe.set value "cooked" (string element.value.cooked);
    Js.Unsafe.set ret "value" (value);
    Js.Unsafe.set ret "tail" (bool element.tail);
    ret
  )

  and tagged_template (loc, tagged) = Expression.TaggedTemplate.(
    let ret = node "TaggedTemplateExpression" loc in
    Js.Unsafe.set ret "tag" (expression tagged.tag);
    Js.Unsafe.set ret "quasi" (template_literal tagged.quasi);
    ret
  )

  and variable_declaration (loc, var) = Statement.VariableDeclaration.(
    let ret = node "VariableDeclaration" loc in
    Js.Unsafe.set ret "declarations" (array variable_declarator var.declarations);
    Js.Unsafe.set ret "kind" (string (match var.kind with
    | Var -> "var"
    | Let -> "let"
    | Const -> "const"));
    ret
  )

  and variable_declarator (loc, declarator) =
    Statement.VariableDeclaration.Declarator.(
      let ret = node "VariableDeclarator" loc in
      Js.Unsafe.set ret "id" (pattern declarator.id);
      Js.Unsafe.set ret "init" (option expression declarator.init);
      ret
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
  )

  and any_type loc = node "AnyTypeAnnotation" loc

  and void_type loc = node "VoidTypeAnnotation" loc

  and number_type loc = node "NumberTypeAnnotation" loc

  and string_type loc = node "StringTypeAnnotation" loc

  and boolean_type loc = node "BooleanTypeAnnotation" loc

  and nullable_type loc t =
    let ret = node "NullableTypeAnnotation" loc in
    Js.Unsafe.set ret "typeAnnotation" (_type t);
    ret

  and function_type (loc, fn) = Type.Function.(
    let ret = node "FunctionTypeAnnotation" loc in
    Js.Unsafe.set ret "params" (array function_type_param fn.params);
    Js.Unsafe.set ret "returnType" (_type fn.returnType);
    Js.Unsafe.set ret "rest" (option function_type_param fn.rest);
    Js.Unsafe.set ret "typeParameters" (option type_parameter_declaration fn.typeParameters);
    ret
  )

  and function_type_param (loc, param) = Type.Function.Param.(
    let ret = node "FunctionTypeParam" loc in
    Js.Unsafe.set ret "name" (identifier param.name);
    Js.Unsafe.set ret "typeAnnotation" (_type param.typeAnnotation);
    Js.Unsafe.set ret "optional" (bool param.optional);
    ret
  )

  and object_type (loc, o) = Type.Object.(
    let ret = node "ObjectTypeAnnotation" loc in
    Js.Unsafe.set ret "properties" (array object_type_property o.properties);
    Js.Unsafe.set ret "indexers" (array object_type_indexer o.indexers);
    Js.Unsafe.set ret "callProperties" (array object_type_call_property o.callProperties);
    ret
  )

  and object_type_property (loc, prop) = Type.Object.Property.(
    let ret = node "ObjectTypeProperty" loc in
    Js.Unsafe.set ret "key" (match prop.key with
    | Expression.Object.Property.Literal lit -> literal lit
    | Expression.Object.Property.Identifier id -> identifier id
    | Expression.Object.Property.Computed _ ->
      failwith "There should not be computed object type property keys"
    );
    Js.Unsafe.set ret "value" (_type prop.value);
    Js.Unsafe.set ret "optional" (bool prop.optional);
    Js.Unsafe.set ret "static" (bool prop.static);
    ret
  )

  and object_type_indexer (loc, indexer) = Type.Object.Indexer.(
    let ret = node "ObjectTypeIndexer" loc in
    Js.Unsafe.set ret "id" (identifier indexer.id);
    Js.Unsafe.set ret "key" (_type indexer.key);
    Js.Unsafe.set ret "value" (_type indexer.value);
    Js.Unsafe.set ret "static" (bool indexer.static);
    ret
  )

  and object_type_call_property (loc, callProperty) = Type.Object.CallProperty.(
    let ret = node "ObjectTypeCallProperty" loc in
    Js.Unsafe.set ret "value" (function_type callProperty.value);
    Js.Unsafe.set ret "static" (bool callProperty.static);
    ret
  )

  and array_type loc t =
    let ret = node "ArrayTypeAnnotation" loc in
    Js.Unsafe.set ret "elementType" (_type t);
    ret

  and generic_type_qualified_identifier (loc, q) = Type.Generic.Identifier.(
    let ret = node "QualifiedTypeIdentifier" loc in
    Js.Unsafe.set ret "qualification" (match q.qualification with
    | Unqualified id -> identifier id
    | Qualified q -> generic_type_qualified_identifier q);
    Js.Unsafe.set ret "id" (identifier q.id);
    ret
  )

  and generic_type (loc, g) = Type.Generic.(
    let ret = node "GenericTypeAnnotation" loc in
    Js.Unsafe.set ret "id" (match g.id with
    | Identifier.Unqualified id -> identifier id
    | Identifier.Qualified q -> generic_type_qualified_identifier q);
    Js.Unsafe.set ret "typeParameters" (option type_parameter_instantiation g.typeParameters);
    ret
  )

  and union_type (loc, tl) =
    let ret = node "UnionTypeAnnotation" loc in
    Js.Unsafe.set ret "types" (array _type tl);
    ret

  and intersection_type (loc, tl) =
    let ret = node "IntersectionTypeAnnotation" loc in
    Js.Unsafe.set ret "types" (array _type tl);
    ret

  and typeof_type (loc, t) =
    let ret = node "TypeofTypeAnnotation" loc in
    Js.Unsafe.set ret "argument" (_type t);
    ret

  and tuple_type (loc, tl) =
    let ret = node "TupleTypeAnnotation" loc in
    Js.Unsafe.set ret "types" (array _type tl);
    ret

  and string_literal_type (loc, s) = Type.StringLiteral.(
    let ret = node "StringLiteralTypeAnnotation" loc in
    Js.Unsafe.set ret "value" (string s.value);
    Js.Unsafe.set ret "raw" (string s.raw);
    ret
  )

  and type_annotation (loc, ty) =
    let ret = node "TypeAnnotation" loc in
    Js.Unsafe.set ret "typeAnnotation" (_type ty);
    ret

  and type_parameter_declaration (loc, params) = Type.ParameterDeclaration.(
    let ret = node "TypeParameterDeclaration" loc in
    Js.Unsafe.set ret "params" (array identifier params.params);
    ret
  )

  and type_parameter_instantiation (loc, params) = Type.ParameterInstantiation.(
    let ret = node "TypeParameterInstantiation" loc in
    Js.Unsafe.set ret "params" (array _type params.params);
    ret
  )

  and jsx_element (loc, (element: JSX.element)) = JSX.(
    let ret = node "JSXElement" loc in
    Js.Unsafe.set ret "openingElement" (jsx_opening element.openingElement);
    Js.Unsafe.set ret "closingElement" (option jsx_closing element.closingElement);
    Js.Unsafe.set ret "children" (array jsx_child element.children);
    ret
  )

  and jsx_opening (loc, opening) = JSX.Opening.(
    let ret = node "JSXOpeningElement" loc in
    Js.Unsafe.set ret "name" (jsx_name opening.name);
    Js.Unsafe.set ret "attributes" (array jsx_opening_attribute opening.attributes);
    Js.Unsafe.set ret "selfClosing" (bool opening.selfClosing);
    ret
  )

  and jsx_opening_attribute = JSX.Opening.(function
    | Attribute attribute -> jsx_attribute attribute
    | SpreadAttribute attribute -> jsx_spread_attribute attribute
  )

  and jsx_closing (loc, closing) = JSX.Closing.(
    let ret = node "JSXClosingElement" loc in
    Js.Unsafe.set ret "name" (jsx_name closing.name);
    ret
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
    let ret = node "JSXAttribute" loc in
    Js.Unsafe.set ret "name" ((match attribute.name with
    | Identifier id -> jsx_identifier id
    | NamespacedName namespaced_name -> jsx_namespaced_name namespaced_name));
    Js.Unsafe.set ret "value" (option jsx_attribute_value attribute.value);
    ret
  )

  and jsx_attribute_value = JSX.Attribute.(function
    | Literal (loc, value) -> literal (loc, value)
    | ExpressionContainer (loc, expr) -> jsx_expression_container (loc, expr)
  )

  and jsx_spread_attribute (loc, attribute) = JSX.SpreadAttribute.(
    let ret = node "JSXSpreadAttribute" loc in
    Js.Unsafe.set ret "argument" (expression attribute.argument);
    ret
  )

  and jsx_expression_container (loc, expr) = JSX.ExpressionContainer.(
    let ret = node "JSXExpressionContainer" loc in
    Js.Unsafe.set ret "expression" ((match expr.expression with
      | Some expr -> expression expr
      | None ->
          (* I think the JSXEmptyExpression is a little stupid...I think null
           * would be a better choice but oh well. Anyway, the location for an
           * empty expression doesn't really make sense, so this is as good a
           * choice as any *)
          node "JSXEmptyExpression" loc));
    ret
  )

  and jsx_text (loc, text) = JSX.Text.(
    let { value; raw; } = text in
    let ret = node "JSXText" loc in
    Js.Unsafe.set ret "value" (string value);
    Js.Unsafe.set ret "raw" (string raw);
    ret
  )

  and jsx_member_expression (loc, member_expression) = JSX.MemberExpression.(
    let ret = node "JSXMemberExpression" loc in
    let _object = match member_expression._object with
    | Identifier id -> jsx_identifier id
    | MemberExpression member -> jsx_member_expression member in
    Js.Unsafe.set ret "object" _object;
    Js.Unsafe.set ret "property" (jsx_identifier member_expression.property);
    ret
  )

  and jsx_namespaced_name (loc, namespaced_name) = JSX.NamespacedName.(
    let ret = node "JSXNamespacedName" loc in
    Js.Unsafe.set ret "namespace" (jsx_identifier namespaced_name.namespace);
    Js.Unsafe.set ret "name" (jsx_identifier namespaced_name.name);
    ret
  )

  and jsx_identifier (loc, id) = JSX.Identifier.(
    let ret = node "JSXIdentifier" loc in
    Js.Unsafe.set ret "name" (string id.name);
    ret
  )

  and module_specifier lit =
    let ret = literal lit in
    Js.Unsafe.set ret "type" (string "ModuleSpecifier");
    ret

  and export_specifier (loc, specifier) = Statement.ExportDeclaration.Specifier.(
    let ret = node "ExportSpecifier" loc in
    Js.Unsafe.set ret "id" (identifier specifier.id);
    Js.Unsafe.set ret "name" (option identifier specifier.name);
    ret
  )

  and import_default_specifier id =
    let ret = node "ImportDefaultSpecifier" (fst id) in
    Js.Unsafe.set ret "id" (identifier id);
    ret

  and import_namespace_specifier (loc, id) =
    let ret = node "ImportNamespaceSpecifier" loc in
    Js.Unsafe.set ret "id" (identifier id);
    ret

  and import_specifier (loc, specifier) = Statement.ImportDeclaration.NamedSpecifier.(
    let ret = node "ImportSpecifier" loc in
    Js.Unsafe.set ret "id" (identifier specifier.id);
    Js.Unsafe.set ret "name" (option identifier specifier.name);
    ret
  )

  and comment_list comments = array comment comments

  and comment (loc, c) = Comment.(
    let _type, value = match c with
      | Line s -> "Line", s
      | Block s -> "Block", s in
    let ret = node _type loc in
    Js.Unsafe.set ret "value" (string value);
    ret
  )
end

let throw e =
  let fn = (Js.Unsafe.new_obj (Js.Unsafe.variable "Function") [|
    Js.Unsafe.inject (Js.string "e");
    Js.Unsafe.inject (Js.string "throw e;");
  |]) in
  Js.Unsafe.call fn fn [| e|]

let parse content =
  let content = Js.to_string content in
  try
    let (ocaml_ast, errors) = Parser_flow.program ~fail:false content in
    let (ret, translation_errors) = Translate.program ocaml_ast in
    Js.Unsafe.set ret "errors" (Translate.errors (errors @ translation_errors));
    ret
  with Parse_error.Error l ->
    let e = Js.Unsafe.new_obj (Js.Unsafe.variable "Error") [|
      Js.Unsafe.inject (Js.string ((string_of_int (List.length l))^" errors"));
    |] in
    Js.Unsafe.set e "name" ((Js.string "Parse Error"));
    ignore (throw e);
    Js.Unsafe.obj [||]

let exports =
  if (Js.typeof (Js.Unsafe.variable "exports") != Js.string "undefined")
  then Js.Unsafe.variable "exports"
  else begin
    let exports = Js.Unsafe.obj [||] in
    Js.Unsafe.set Js.Unsafe.global "flow" exports;
    exports
  end
let () = Js.Unsafe.set exports "parse" (Js.wrap_callback parse)
