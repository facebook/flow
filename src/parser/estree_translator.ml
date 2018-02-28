(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


module type Translator = sig
  type t
  val string: string -> t
  val bool: bool -> t
  val obj: (string * t) list -> t
  val array: t list -> t
  val number: float -> t
  val null: t
  val regexp: Loc.t -> string -> string -> t
end

module type Config = sig
  val include_locs: bool
  val include_comments: bool
end

module Translate (Impl : Translator) (Config : Config) : (sig
  type t
  val program:
    Loc.t * Loc.t Ast.Statement.t list * (Loc.t * Ast.Comment.t') list ->
    t
  val expression: Loc.t Ast.Expression.t -> t
  val errors: (Loc.t * Parse_error.t) list -> t
end with type t = Impl.t) = struct
  type t = Impl.t

  open Ast
  open Impl

  let array_of_list fn list = array (List.map fn list)
  let int x = number (float x)
  let option f = function
    | Some v -> f v
    | None -> null

  let position p =
    obj [
      "line", int p.Loc.line;
      "column", int p.Loc.column;
    ]

  let loc location =
    let source = match Loc.source location with
    | Some File_key.LibFile src
    | Some File_key.SourceFile src
    | Some File_key.JsonFile src
    | Some File_key.ResourceFile src -> string src
    | Some File_key.Builtins -> string "(global)"
    | None -> null
    in
    obj [
      "source", source;
      "start", position location.Loc.start;
      "end", position location.Loc._end;
    ]

  let range location = Loc.(
    array [
      int location.start.offset;
      int location._end.offset;
    ]
  )

  let node _type location props =
    let prefix =
      if Config.include_locs then
        (* sorted backwards due to the rev_append below *)
        [ "range", range location;
          "loc", loc location;
          "type", string _type; ]
      else
        [ "type", string _type; ]
    in
    obj (List.rev_append prefix props)

  let errors l =
    let error (location, e) =
      obj [
        "loc", loc location;
        "message", string (Parse_error.PP.error e);
      ]
    in array_of_list error l

  let rec program (loc, statements, comments) =
    let body = statement_list statements in
    let props =
      if Config.include_comments then [ "body", body; "comments", comment_list comments; ]
      else [ "body", body; ]
    in
    node "Program" loc props

  and statement_list statements = array_of_list statement statements
  and statement = Statement.(function
  | loc, Empty -> node "EmptyStatement" loc []
  | loc, Block b -> block (loc, b)
  | loc, Expression expr ->
      node "ExpressionStatement" loc [
        "expression", expression expr.Expression.expression;
        "directive", option string expr.Expression.directive;
      ]
  | loc, If _if -> If.(
      node "IfStatement" loc [
        "test", expression _if.test;
        "consequent", statement _if.consequent;
        "alternate", option statement _if.alternate;
      ]
    )
  | loc, Labeled labeled -> Labeled.(
      node "LabeledStatement" loc [
        "label", identifier labeled.label;
        "body", statement labeled.body;
      ]
    )
  | loc, Break break ->
      node "BreakStatement" loc [
        "label", option identifier break.Break.label;
      ]
  | loc, Continue continue ->
      node "ContinueStatement" loc [
        "label", option identifier continue.Continue.label;
      ]
  | loc, With _with -> With.(
      node "WithStatement" loc [
        "object", expression _with._object;
        "body", statement _with.body;
      ]
    )
  | loc, TypeAlias alias -> type_alias (loc, alias)
  | loc, OpaqueType opaque_t -> opaque_type ~declare:false (loc, opaque_t)
  | loc, Switch switch -> Switch.(
      node "SwitchStatement" loc [
        "discriminant", expression switch.discriminant;
        "cases", array_of_list case switch.cases;
      ]
    )
  | loc, Return return ->
      node "ReturnStatement" loc [
        "argument", option expression return.Return.argument;
      ]
  | loc, Throw throw ->
      node "ThrowStatement" loc [
        "argument", expression throw.Throw.argument;
      ]
  | loc, Try _try -> Try.(
      node "TryStatement" loc [
        "block", block _try.block;
        "handler", option catch _try.handler;
        "finalizer", option block _try.finalizer;
      ]
    )
  | loc, While _while -> While.(
      node "WhileStatement" loc [
        "test", expression _while.test;
        "body", statement _while.body;
      ]
    )
  | loc, DoWhile dowhile -> DoWhile.(
      node "DoWhileStatement" loc [
        "body", statement dowhile.body;
        "test", expression dowhile.test;
      ]
    )
  | loc, For _for -> For.(
      let init = function
      | InitDeclaration init -> variable_declaration init
      | InitExpression expr -> expression expr
      in
      node "ForStatement" loc [
        "init", option init _for.init;
        "test", option expression _for.test;
        "update", option expression _for.update;
        "body", statement _for.body;
      ]
    )
  | loc, ForIn forin -> ForIn.(
      let left = match forin.left with
      | LeftDeclaration left -> variable_declaration left
      | LeftPattern left -> pattern left
      in
      node "ForInStatement" loc [
        "left", left;
        "right", expression forin.right;
        "body", statement forin.body;
        "each", bool forin.each;
      ]
    )
  | loc, ForOf forof -> ForOf.(
      let type_ =
        if forof.async
        then "ForAwaitStatement"
        else "ForOfStatement"
      in
      let left = match forof.left with
      | LeftDeclaration left -> variable_declaration left
      | LeftPattern left -> pattern left
      in
      node type_ loc [
        "left", left;
        "right", expression forof.right;
        "body", statement forof.body;
      ]
    )
  | loc, Debugger -> node "DebuggerStatement" loc []
  | loc, ClassDeclaration c -> class_declaration (loc, c)
  | loc, InterfaceDeclaration i -> interface_declaration (loc, i)
  | loc, VariableDeclaration var -> variable_declaration (loc, var)
  | loc, FunctionDeclaration fn -> function_declaration (loc, fn)
  | loc, DeclareVariable d -> declare_variable (loc, d)
  | loc, DeclareFunction d -> declare_function (loc, d)
  | loc, DeclareClass d -> declare_class (loc, d)
  | loc, DeclareInterface i -> declare_interface (loc, i)
  | loc, DeclareTypeAlias a -> declare_type_alias (loc, a)
  | loc, DeclareOpaqueType t -> opaque_type ~declare:true (loc, t)
  | loc, DeclareModule m -> DeclareModule.(
      let id = match m.id with
      | Literal lit -> string_literal lit
      | Identifier id -> identifier id
      in
      node "DeclareModule" loc [
        "id", id;
        "body", block m.body;
        "kind", (
          match m.kind with
          | DeclareModule.CommonJS _ -> string "CommonJS"
          | DeclareModule.ES _ -> string "ES"
        )
      ]
    )
    | loc, DeclareExportDeclaration export -> DeclareExportDeclaration.(
        match export.specifiers with
        | Some (ExportNamedDeclaration.ExportBatchSpecifier (_, None)) ->
          node "DeclareExportAllDeclaration" loc [
            "source", option string_literal export.source;
          ]
        | _ ->
          let declaration = match export.declaration with
          | Some (Variable v) -> declare_variable v
          | Some (Function f) -> declare_function f
          | Some (Class c) -> declare_class c
          | Some (DefaultType t) -> _type t
          | Some (NamedType t) -> type_alias t
          | Some (NamedOpaqueType t) -> opaque_type ~declare:true t
          | Some (Interface i) -> interface_declaration i
          | None -> null
          in
          node "DeclareExportDeclaration" loc [
            "default", bool (
              match export.default with
              | Some _ -> true
              | None -> false);
            "declaration", declaration;
            "specifiers", export_specifiers export.specifiers;
            "source", option string_literal export.source;
          ]
      )
    | loc, DeclareModuleExports annot ->
        node "DeclareModuleExports" loc [
          "typeAnnotation", type_annotation annot
        ]
    | loc, ExportNamedDeclaration export -> ExportNamedDeclaration.(
        match export.specifiers with
        | Some (ExportBatchSpecifier (_, None)) ->
          node "ExportAllDeclaration" loc [
            "source", option string_literal export.source;
            "exportKind", string (export_kind export.exportKind);
          ]
        | _ ->
          node "ExportNamedDeclaration" loc [
            "declaration", option statement export.declaration;
            "specifiers", export_specifiers export.specifiers;
            "source", option string_literal export.source;
            "exportKind", string (export_kind export.exportKind);
          ]
      )
    | loc, ExportDefaultDeclaration export -> ExportDefaultDeclaration.(
        let declaration = match export.declaration with
        | Declaration stmt -> statement stmt
        | ExportDefaultDeclaration.Expression expr -> expression expr
        in
        node "ExportDefaultDeclaration" loc [
          "declaration", declaration;
          "exportKind", string (export_kind Statement.ExportValue);
        ]
      )
    | loc, ImportDeclaration import -> ImportDeclaration.(
        let specifiers = match import.specifiers with
          | Some (ImportNamedSpecifiers specifiers) ->
              List.map (fun {local; remote; kind;} ->
                import_named_specifier local remote kind
              ) specifiers
          | Some (ImportNamespaceSpecifier id) ->
              [import_namespace_specifier id]
          | None ->
              []
        in

        let specifiers = match import.default with
          | Some default -> (import_default_specifier default)::specifiers
          | None -> specifiers
        in

        let import_kind = match import.importKind with
        | ImportType -> "type"
        | ImportTypeof -> "typeof"
        | ImportValue -> "value"
        in

        node "ImportDeclaration" loc [
          "specifiers", array specifiers;
          "source", string_literal import.source;
          "importKind", string (import_kind);
        ]
    )
  )

  and expression = Expression.(function
    | loc, This -> node "ThisExpression" loc []
    | loc, Super -> node "Super" loc []
    | loc, Array arr ->
        node "ArrayExpression" loc [
          "elements", array_of_list (option expression_or_spread) arr.Array.elements;
        ]
    | loc, Object _object ->
        node "ObjectExpression" loc [
          "properties", array_of_list object_property _object.Object.properties;
        ]
    | loc, Function _function -> function_expression (loc, _function)
    | loc, ArrowFunction arrow -> Function.(
        let body = (match arrow.body with
        | BodyBlock b -> block b
        | BodyExpression expr -> expression expr)
        in
        node "ArrowFunctionExpression" loc [
          "id", option identifier arrow.id;
          "params", function_params arrow.params;
          "body", body;
          "async", bool arrow.async;
          "generator", bool arrow.generator;
          "predicate", option predicate arrow.predicate;
          "expression", bool arrow.expression;
          "returnType", option type_annotation arrow.returnType;
          "typeParameters", option type_parameter_declaration arrow.typeParameters;
        ]
      )
    | loc, Sequence sequence ->
        node "SequenceExpression" loc [
          "expressions", array_of_list expression sequence.Sequence.expressions;
        ]
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
          node "AwaitExpression" loc [
            "argument", expression unary.argument;
          ]
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
          node "UnaryExpression" loc [
            "operator", string operator;
            "prefix", bool unary.prefix;
            "argument", expression unary.argument;
          ]
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
        | Exp -> "**"
        | Div -> "/"
        | Mod -> "%"
        | BitOr -> "|"
        | Xor -> "^"
        | BitAnd -> "&"
        | In -> "in"
        | Instanceof -> "instanceof"
        in
        node "BinaryExpression" loc [
          "operator", string operator;
          "left", expression binary.left;
          "right", expression binary.right;
        ]
      )
    | loc, TypeCast typecast -> TypeCast.(
        node "TypeCastExpression" loc [
          "expression", expression typecast.expression;
          "typeAnnotation", type_annotation typecast.typeAnnotation;
        ]
      )
    | loc, Assignment assignment -> Assignment.(
        let operator = match assignment.operator with
        | Assign -> "="
        | PlusAssign -> "+="
        | MinusAssign -> "-="
        | MultAssign -> "*="
        | ExpAssign -> "**="
        | DivAssign -> "/="
        | ModAssign -> "%="
        | LShiftAssign -> "<<="
        | RShiftAssign -> ">>="
        | RShift3Assign -> ">>>="
        | BitOrAssign -> "|="
        | BitXorAssign -> "^="
        | BitAndAssign -> "&="
        in
        node "AssignmentExpression" loc [
          "operator", string operator;
          "left", pattern assignment.left;
          "right", expression assignment.right;
        ]
      )
    | loc, Update update -> Update.(
        let operator = match update.operator with
        | Increment -> "++"
        | Decrement -> "--"
        in
        node "UpdateExpression" loc [
          "operator", string operator;
          "argument", expression update.argument;
          "prefix", bool update.prefix;
        ]
      )
    | loc, Logical logical -> Logical.(
        let operator = match logical.operator with
        | Or -> "||"
        | And -> "&&"
        in
        node "LogicalExpression" loc [
          "operator", string operator;
          "left", expression logical.left;
          "right", expression logical.right;
        ]
      )
    | loc, Conditional conditional -> Conditional.(
        node "ConditionalExpression" loc [
          "test", expression conditional.test;
          "consequent", expression conditional.consequent;
          "alternate", expression conditional.alternate;
        ]
      )
    | loc, New _new -> New.(
        node "NewExpression" loc [
          "callee", expression _new.callee;
          "arguments", array_of_list expression_or_spread _new.arguments;
        ]
      )
    | loc, Call call -> Call.(
        node "CallExpression" loc [
          "callee", expression call.callee;
          "arguments", array_of_list expression_or_spread call.arguments;
          "optional", bool call.optional;
        ]
      )
    | loc, Member member -> Member.(
        let property = match member.property with
        | PropertyIdentifier id -> identifier id
        | PropertyPrivateName name -> private_name name
        | PropertyExpression expr -> expression expr
        in
        node "MemberExpression" loc [
          "object", expression member._object;
          "property", property;
          "computed", bool member.computed;
          "optional", bool member.optional;
        ]
      )
    | loc, Yield yield -> Yield.(
        node "YieldExpression" loc [
          "argument", option expression yield.argument;
          "delegate", bool yield.delegate;
        ]
      )
    | loc, Comprehension comp -> Comprehension.(
        node "ComprehensionExpression" loc [
          "blocks", array_of_list comprehension_block comp.blocks;
          "filter", option expression comp.filter;
        ]
      )
    | loc, Generator gen -> Generator.(
        node "GeneratorExpression" loc [
          "blocks", array_of_list comprehension_block gen.blocks;
          "filter", option expression gen.filter;
        ]
      )
    | _loc, Identifier id -> identifier id
    | loc, Literal lit -> literal (loc, lit)
    | loc, TemplateLiteral lit -> template_literal (loc, lit)
    | loc, TaggedTemplate tagged -> tagged_template (loc, tagged)
    | loc, Class c -> class_expression (loc, c)
    | loc, JSXElement element -> jsx_element (loc, element)
    | loc, JSXFragment fragment -> jsx_fragment (loc, fragment)
    | loc, MetaProperty meta_prop -> MetaProperty.(
        node "MetaProperty" loc [
          "meta", identifier meta_prop.meta;
          "property", identifier meta_prop.property;
        ]
      )
    | loc, Import arg -> node "CallExpression" loc [
        "callee", node "Import" (Loc.btwn loc (fst arg)) [];
        "arguments", array_of_list expression [arg];
      ]
  )

  and function_declaration (loc, fn) = Function.(
    let body = match fn.body with
    | BodyBlock b -> block b
    | BodyExpression b -> expression b in

    node "FunctionDeclaration" loc [
      (* estree hasn't come around to the idea that function decls can have
         optional ids, but acorn, babel, espree and esprima all have, so let's
         do it too. see https://github.com/estree/estree/issues/98 *)
      "id", option identifier fn.id;
      "params", function_params fn.params;
      "body", body;
      "async", bool fn.async;
      "generator", bool fn.generator;
      "predicate", option predicate fn.predicate;
      "expression", bool fn.expression;
      "returnType", option type_annotation fn.returnType;
      "typeParameters", option type_parameter_declaration fn.typeParameters;
    ]
  )

  and function_expression (loc, _function) = Function.(
    let body = match _function.body with
    | BodyBlock b -> block b
    | BodyExpression expr -> expression expr
    in
    node "FunctionExpression" loc [
      "id", option identifier _function.id;
      "params", function_params _function.params;
      "body", body;
      "async", bool _function.async;
      "generator", bool _function.generator;
      "predicate", option predicate _function.predicate;
      "expression", bool _function.expression;
      "returnType", option type_annotation _function.returnType;
      "typeParameters", option type_parameter_declaration _function.typeParameters;
    ]
  )

  and identifier (loc, name) =
    node "Identifier" loc [
      "name", string name;
      "typeAnnotation", null;
      "optional", bool false;
    ]

  and private_name (loc, name) =
    node "PrivateName" loc [
      "id", identifier name;
    ]

  and pattern_identifier loc {
    Pattern.Identifier.name; typeAnnotation; optional;
  } =
    node "Identifier" loc [
      "name", string (snd name);
      "typeAnnotation", option type_annotation typeAnnotation;
      "optional", bool optional;
    ]

  and case (loc, c) = Statement.Switch.Case.(
    node "SwitchCase" loc [
      "test", option expression c.test;
      "consequent", array_of_list statement c.consequent;
    ]
  )

  and catch (loc, c) = Statement.Try.CatchClause.(
    node "CatchClause" loc [
      "param", pattern c.param;
      "body", block c.body;
    ]
  )

  and block (loc, b) =
    node "BlockStatement" loc [
      "body", statement_list b.Statement.Block.body;
    ]

  and declare_variable (loc, d) = Statement.DeclareVariable.(
    let id_loc = Loc.btwn (fst d.id) (match d.typeAnnotation with
      | Some typeAnnotation -> fst typeAnnotation
      | None -> fst d.id) in
    node "DeclareVariable" loc [
      "id", pattern_identifier id_loc {
        Pattern.Identifier.name = d.id;
                           typeAnnotation = d.typeAnnotation;
                           optional = false;
      };
    ]
  )

  and declare_function (loc, d) = Statement.DeclareFunction.(
    let id_loc = Loc.btwn (fst d.id) (fst d.typeAnnotation) in
    node "DeclareFunction" loc [
      "id", pattern_identifier id_loc {
        Pattern.Identifier.name = d.id;
                           typeAnnotation = Some d.typeAnnotation;
                           optional = false;
      };
      "predicate", option predicate d.predicate
    ]
  )

  and declare_class (loc, d) = Statement.DeclareClass.(
    (* TODO: extends shouldn't return an array *)
    let extends = match d.extends with
    | Some extends -> array [interface_extends extends]
    | None -> array []
    in
    node "DeclareClass" loc [
      "id", identifier d.id;
      "typeParameters", option type_parameter_declaration d.typeParameters;
      "body", object_type d.body;
      "extends", extends;
    ]
  )

  and declare_interface (loc, { Statement.Interface.
    id;
    typeParameters;
    body;
    extends;
  }) =
    node "DeclareInterface" loc [
      "id", identifier id;
      "typeParameters", option type_parameter_declaration typeParameters;
      "body", object_type body;
      "extends", array_of_list interface_extends extends;
    ]

  and export_kind = function
    | Statement.ExportType -> "type"
    | Statement.ExportValue -> "value"

  and export_specifiers = Statement.ExportNamedDeclaration.(function
    | Some (ExportSpecifiers specifiers) ->
        array_of_list export_specifier specifiers
    | Some (ExportBatchSpecifier (loc, Some name)) ->
        array [
          node "ExportNamespaceSpecifier" loc [
            "exported", identifier name
          ]
        ]
    | Some (ExportBatchSpecifier (_, None)) ->
        (* this should've been handled by callers, since this represents an
           ExportAllDeclaration, not a specifier. *)
        array []
    | None ->
        array []
  )

  and declare_type_alias (loc, { Statement.TypeAlias.
    id;
    typeParameters;
    right;
  }) =
    node "DeclareTypeAlias" loc [
      "id", identifier id;
      "typeParameters", option type_parameter_declaration typeParameters;
      "right", _type right;
    ]

  and type_alias (loc, alias) =  Statement.TypeAlias.(
    node "TypeAlias" loc [
      "id", identifier alias.id;
      "typeParameters", option type_parameter_declaration alias.typeParameters;
      "right", _type alias.right;
    ]
  )
  and opaque_type ~declare (loc, opaque_t) =  Statement.OpaqueType.(
    let name = if declare then "DeclareOpaqueType" else "OpaqueType" in
    node name loc [
      "id", identifier opaque_t.id;
      "typeParameters", option type_parameter_declaration opaque_t.typeParameters;
      "impltype", option _type opaque_t.impltype;
      "supertype", option _type opaque_t.supertype;
    ]
  )

  and class_declaration (loc, c) = Class.(
    node "ClassDeclaration" loc [
      (* estree hasn't come around to the idea that class decls can have
         optional ids, but acorn, babel, espree and esprima all have, so let's
         do it too. see https://github.com/estree/estree/issues/98 *)
      "id", option identifier c.id;
      "body", class_body c.body;
      "superClass", option expression c.superClass;
      "typeParameters", option type_parameter_declaration c.typeParameters;
      "superTypeParameters", option type_parameter_instantiation c.superTypeParameters;
      "implements", array_of_list class_implements c.implements;
      "decorators", array_of_list expression c.classDecorators;
    ]
  )

  and class_expression (loc, c) = Class.(
    node "ClassExpression" loc [
      "id", option identifier c.id;
      "body", class_body c.body;
      "superClass", option expression c.superClass;
      "typeParameters", option type_parameter_declaration c.typeParameters;
      "superTypeParameters", option type_parameter_instantiation c.superTypeParameters;
      "implements", array_of_list class_implements c.implements;
      "decorators", array_of_list expression c.classDecorators;
    ]
  )

  and class_implements (loc, implements) = Class.Implements.(
    node "ClassImplements" loc [
      "id", identifier implements.id;
      "typeParameters", option type_parameter_instantiation implements.typeParameters;
    ]
  )

  and class_body (loc, body) = Class.Body.(
    node "ClassBody" loc [
      "body", array_of_list class_element body.body;
    ]
  )

  and class_element = Class.Body.(function
    | Method m -> class_method m
    | PrivateField p -> class_private_field p
    | Property p -> class_property p)

  and class_method (loc, method_) =
    let { Class.Method.key; value; kind; static; decorators; } = method_ in
    let key, computed = Expression.Object.Property.(match key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | PrivateName name -> private_name name, false
      | Computed expr -> expression expr, true) in
    let kind = Class.Method.(match kind with
      | Constructor -> "constructor"
      | Method -> "method"
      | Get -> "get"
      | Set -> "set") in
    node "MethodDefinition" loc [
      "key", key;
      "value", function_expression value;
      "kind", string kind;
      "static", bool static;
      "computed", bool computed;
      "decorators", array_of_list expression decorators;
    ]

  and class_private_field (loc, prop) = Class.PrivateField.(
    let (_, key) = prop.key in
    node "ClassPrivateProperty" loc [
      "key", identifier key;
      "value", option expression prop.value;
      "typeAnnotation", option type_annotation prop.typeAnnotation;
      "static", bool prop.static;
      "variance", option variance prop.variance;
    ]
  )
  and class_property (loc, prop) = Class.Property.(
    let key, computed = (match prop.key with
    | Expression.Object.Property.Literal lit -> literal lit, false
    | Expression.Object.Property.Identifier id -> identifier id, false
    | Expression.Object.Property.PrivateName _ ->
        failwith "Internal Error: Private name found in class prop"
    | Expression.Object.Property.Computed expr -> expression expr, true) in
    node "ClassProperty" loc [
      "key", key;
      "value", option expression prop.value;
      "typeAnnotation", option type_annotation prop.typeAnnotation;
      "computed", bool computed;
      "static", bool prop.static;
      "variance", option variance prop.variance;
    ]
  )

  and interface_declaration (loc, i) = Statement.Interface.(
    node "InterfaceDeclaration" loc [
      "id", identifier i.id;
      "typeParameters", option type_parameter_declaration i.typeParameters;
      "body", object_type i.body;
      "extends", array_of_list interface_extends i.extends;
    ]
  )

  and interface_extends (loc, g) = Type.Generic.(
    let id = match g.id with
    | Identifier.Unqualified id -> identifier id
    | Identifier.Qualified q -> generic_type_qualified_identifier q
    in
    node "InterfaceExtends" loc [
      "id", id;
      "typeParameters", option type_parameter_instantiation g.typeParameters;
    ]
  )

  and pattern = Pattern.(function
    | loc, Object obj ->
        node "ObjectPattern" loc [
          "properties", array_of_list object_pattern_property obj.Object.properties;
          "typeAnnotation", option type_annotation obj.Object.typeAnnotation;
        ]
    | loc, Array arr ->
        node "ArrayPattern" loc [
          "elements", array_of_list (option array_pattern_element) arr.Array.elements;
          "typeAnnotation", option type_annotation arr.Array.typeAnnotation;
        ]
    | loc, Assignment { Assignment.left; right } ->
        node "AssignmentPattern" loc [
          "left", pattern left;
          "right", expression right
        ]
    | loc, Identifier pattern_id ->
        pattern_identifier loc pattern_id
    | _loc, Expression expr -> expression expr)

  and function_params = Ast.Function.Params.(function
    | _, { params; rest = Some (rest_loc, { Function.RestElement.argument }) } ->
      let rest = node "RestElement" rest_loc [
        "argument", pattern argument;
      ] in
      let rev_params = params |> List.map pattern |> List.rev in
      let params = List.rev (rest::rev_params) in
      array params
    | _, { params; rest = None } ->
      array_of_list pattern params
  )

  and array_pattern_element = Pattern.Array.(function
    | Element p -> pattern p
    | RestElement (loc, { RestElement.argument; }) ->
        node "RestElement" loc [
          "argument", pattern argument;
        ]
  )

  and object_property = Expression.Object.(function
    | Property (loc, prop) -> Property.(
      let key, value, kind, method_, shorthand = match prop with
      | Init { key; value; shorthand } ->
        key, expression value, "init", false, shorthand
      | Method { key; value = (loc, func) } ->
        key, function_expression (loc, func), "init", true, false
      | Get { key; value = (loc, func) } ->
        key, function_expression (loc, func), "get", false, false
      | Set { key; value = (loc, func) } ->
        key, function_expression (loc, func), "set", false, false
      in
      let key, computed = match key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | PrivateName _ -> failwith "Internal Error: Found private field in object props"
      | Computed expr -> expression expr, true
      in
      node "Property" loc [
        "key", key;
        "value", value;
        "kind", string kind;
        "method", bool method_;
        "shorthand", bool shorthand;
        "computed", bool computed;
      ]
    )
  | SpreadProperty(loc, prop) -> SpreadProperty.(
    node "SpreadProperty" loc [
      "argument", expression prop.argument;
    ]
  ))

  and object_pattern_property = Pattern.Object.(function
    | Property (loc, prop) -> Property.(
      let key, computed = (match prop.key with
      | Literal lit -> literal lit, false
      | Identifier id -> identifier id, false
      | Computed expr -> expression expr, true) in
      node "Property" loc [
        "key", key;
        "value", pattern prop.pattern;
        "kind", string "init";
        "method", bool false;
        "shorthand", bool prop.shorthand;
        "computed", bool computed;
      ]
    )
    | RestProperty (loc, prop) -> RestProperty.(
      node "RestProperty" loc [
        "argument", pattern prop.argument;
      ]
    )
  )

  and expression_or_spread = Expression.(function
    | Expression expr -> expression expr
    | Spread (loc, { SpreadElement.argument; }) ->
        node "SpreadElement" loc [
          "argument", expression argument;
        ]
  )

  and comprehension_block (loc, b) = Expression.Comprehension.Block.(
    node "ComprehensionBlock" loc [
      "left", pattern b.left;
      "right", expression b.right;
      "each", bool b.each;
    ]
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
        let regex = obj [
          "pattern", string pattern;
          "flags", string flags;
        ] in
        [ "value", value_; "raw", string raw; "regex", regex ]
    | _ ->
        [ "value", value_; "raw", string raw; ]
    in
    node "Literal" loc props
  )

  and string_literal (loc, lit) = StringLiteral.(
    node "Literal" loc [
      "value", string lit.value;
      "raw", string lit.raw;
    ]
  )

  and template_literal (loc, value) = Expression.TemplateLiteral.(
    node "TemplateLiteral" loc [
      "quasis", array_of_list template_element value.quasis;
      "expressions", array_of_list expression value.expressions;
    ]
  )

  and template_element (loc, element) = Expression.TemplateLiteral.Element.(
    let value = obj [
      "raw", string element.value.raw;
      "cooked", string element.value.cooked;
    ] in
    node "TemplateElement" loc [
      "value", value;
      "tail", bool element.tail;
    ]
  )

  and tagged_template (loc, tagged) = Expression.TaggedTemplate.(
    node "TaggedTemplateExpression" loc [
      "tag", expression tagged.tag;
      "quasi", template_literal tagged.quasi;
    ]
  )

  and variable_declaration (loc, var) = Statement.VariableDeclaration.(
    let kind = match var.kind with
    | Var -> "var"
    | Let -> "let"
    | Const -> "const"
    in
    node "VariableDeclaration" loc [
      "declarations", array_of_list variable_declarator var.declarations;
      "kind", string kind;
    ]
  )

  and variable_declarator (loc, declarator) =
    Statement.VariableDeclaration.Declarator.(
      node "VariableDeclarator" loc [
        "id", pattern declarator.id;
        "init", option expression declarator.init;
      ]
    )

  and variance (loc, sigil) =
    let kind = Variance.(match sigil with
    | Plus -> string "plus"
    | Minus -> string "minus"
    ) in
    node "Variance" loc [ "kind", kind ]

  and _type (loc, t) = Type.(
    match t with
    | Any -> any_type loc
    | Mixed -> mixed_type loc
    | Empty -> empty_type loc
    | Void -> void_type loc
    | Null -> null_type loc
    | Number -> number_type loc
    | String -> string_type loc
    | Boolean -> boolean_type loc
    | Nullable t -> nullable_type loc t
    | Function fn -> function_type (loc, fn)
    | Object o -> object_type (loc, o)
    | Array t -> array_type loc t
    | Generic g -> generic_type (loc, g)
    | Union (t0, t1, ts) -> union_type (loc, t0::t1::ts)
    | Intersection (t0, t1, ts) -> intersection_type (loc, t0::t1::ts)
    | Typeof t -> typeof_type (loc, t)
    | Tuple t -> tuple_type (loc, t)
    | StringLiteral s -> string_literal_type (loc, s)
    | NumberLiteral n -> number_literal_type (loc, n)
    | BooleanLiteral b -> boolean_literal_type (loc, b)
    | Exists -> exists_type loc
  )

  and any_type loc = node "AnyTypeAnnotation" loc []

  and mixed_type loc = node "MixedTypeAnnotation" loc []

  and empty_type loc = node "EmptyTypeAnnotation" loc []

  and void_type loc = node "VoidTypeAnnotation" loc []

  and null_type loc = node "NullLiteralTypeAnnotation" loc []

  and number_type loc = node "NumberTypeAnnotation" loc []

  and string_type loc = node "StringTypeAnnotation" loc []

  and boolean_type loc = node "BooleanTypeAnnotation" loc []

  and nullable_type loc t =
    node "NullableTypeAnnotation" loc [
      "typeAnnotation", _type t;
    ]

  and function_type (loc, fn) = Type.Function.(
    let (_, { Params.params; rest }) = fn.params in
    node "FunctionTypeAnnotation" loc [
      "params", array_of_list function_type_param params;
      "returnType", _type fn.returnType;
      "rest", option function_type_rest rest;
      "typeParameters", option type_parameter_declaration fn.typeParameters;
    ]
  )

  and function_type_param (loc, param) = Type.Function.Param.(
    node "FunctionTypeParam" loc [
      "name", option identifier param.name;
      "typeAnnotation", _type param.typeAnnotation;
      "optional", bool param.optional;
    ]
  )

  and function_type_rest (_loc, { Type.Function.RestParam.argument }) =
    (* TODO: add a node for the rest param itself, including the `...`,
       like we do with RestElement on normal functions. This should be
       coordinated with Babel, ast-types, etc. so keeping the status quo for
       now. Here's an example: *)
    (* node "FunctionTypeRestParam" loc [
      "argument", function_type_param argument;
    ] *)
    function_type_param argument

  and object_type (loc, o) = Type.Object.(
    let props, ixs, calls = List.fold_left (fun (props, ixs, calls) -> function
      | Property p ->
        let prop = object_type_property p in
        prop::props, ixs, calls
      | SpreadProperty p ->
        let prop = object_type_spread_property p in
        prop::props, ixs, calls
      | Indexer i ->
        let ix = object_type_indexer i in
        props, ix::ixs, calls
      | CallProperty c ->
        let call = object_type_call_property c in
        props, ixs, call::calls
    ) ([], [], []) o.properties in
    node "ObjectTypeAnnotation" loc [
      "exact", bool o.exact;
      "properties", array (List.rev props);
      "indexers", array (List.rev ixs);
      "callProperties", array (List.rev calls);
    ]
  )

  and object_type_property (loc, { Type.Object.Property.
    key; value; optional; static; variance = variance_; _method;
  }) =
    let key = match key with
    | Expression.Object.Property.Literal lit -> literal lit
    | Expression.Object.Property.Identifier id -> identifier id
    | Expression.Object.Property.PrivateName _ ->
      failwith "Internal Error: Found private field in object props"
    | Expression.Object.Property.Computed _ ->
      failwith "There should not be computed object type property keys"
    in
    let value, kind = match value with
    | Type.Object.Property.Init value -> _type value, "init"
    | Type.Object.Property.Get (loc, f) -> function_type (loc, f), "get"
    | Type.Object.Property.Set (loc, f) -> function_type (loc, f), "set"
    in
    node "ObjectTypeProperty" loc [
      "key", key;
      "value", value;
      "method", bool _method;
      "optional", bool optional;
      "static", bool static;
      "variance", option variance variance_;
      "kind", string kind;
    ]

  and object_type_spread_property (loc, prop) = Type.Object.SpreadProperty.(
    node "ObjectTypeSpreadProperty" loc [
      "argument", _type prop.argument;
    ]
  )

  and object_type_indexer (loc, indexer) = Type.Object.Indexer.(
    node "ObjectTypeIndexer" loc [
      "id", option identifier indexer.id;
      "key", _type indexer.key;
      "value", _type indexer.value;
      "static", bool indexer.static;
      "variance", option variance indexer.variance;
    ]
  )

  and object_type_call_property (loc, callProperty) = Type.Object.CallProperty.(
    node "ObjectTypeCallProperty" loc [
      "value", function_type callProperty.value;
      "static", bool callProperty.static;
    ]
  )

  and array_type loc t =
    node "ArrayTypeAnnotation" loc [
      "elementType", (_type t);
    ]

  and generic_type_qualified_identifier (loc, q) = Type.Generic.Identifier.(
    let qualification = match q.qualification with
    | Unqualified id -> identifier id
    | Qualified q -> generic_type_qualified_identifier q
    in
    node "QualifiedTypeIdentifier" loc [
      "qualification", qualification;
      "id", identifier q.id;
    ]
  )

  and generic_type (loc, g) = Type.Generic.(
    let id = match g.id with
    | Identifier.Unqualified id -> identifier id
    | Identifier.Qualified q -> generic_type_qualified_identifier q
    in
    node "GenericTypeAnnotation" loc [
      "id", id;
      "typeParameters", option type_parameter_instantiation g.typeParameters;
    ]
  )

  and union_type (loc, ts) =
    node "UnionTypeAnnotation" loc [
      "types", array_of_list _type ts;
    ]

  and intersection_type (loc, ts) =
    node "IntersectionTypeAnnotation" loc [
      "types", array_of_list _type ts;
    ]

  and typeof_type (loc, t) =
    node "TypeofTypeAnnotation" loc [
      "argument", _type t;
    ]

  and tuple_type (loc, tl) =
    node "TupleTypeAnnotation" loc [
      "types", array_of_list _type tl;
    ]

  and string_literal_type (loc, s) = Ast.StringLiteral.(
    node "StringLiteralTypeAnnotation" loc [
      "value", string s.value;
      "raw", string s.raw;
    ]
  )

  and number_literal_type (loc, s) = Ast.NumberLiteral.(
    node "NumberLiteralTypeAnnotation" loc [
      "value", number s.value;
      "raw", string s.raw;
    ]
  )

  and boolean_literal_type (loc, value) =
    node "BooleanLiteralTypeAnnotation" loc [
      "value", bool value;
      "raw", string (if value then "true" else "false");
    ]

  and exists_type loc = node "ExistsTypeAnnotation" loc []

  and type_annotation (loc, ty) =
    node "TypeAnnotation" loc [
      "typeAnnotation", _type ty;
    ]

  and type_parameter_declaration (loc, params) = Type.ParameterDeclaration.(
    node "TypeParameterDeclaration" loc [
      "params", array_of_list type_param params.params;
    ]
  )

  and type_param (loc, { Type.ParameterDeclaration.TypeParam.
    name = (_, name);
    bound;
    variance = tp_var;
    default;
  }) =
    node "TypeParameter" loc [
      (* we track the location of the name, but don't expose it here for
         backwards-compatibility. TODO: change this? *)
      "name", string name;
      "bound", option type_annotation bound;
      "variance", option variance tp_var;
      "default", option _type default;
    ]

  and type_parameter_instantiation (loc, params) = Type.ParameterInstantiation.(
    node "TypeParameterInstantiation" loc [
      "params", array_of_list _type params.params;
    ]
  )

  and jsx_element (loc, (element: Loc.t JSX.element)) = JSX.(
    node "JSXElement" loc [
      "openingElement", jsx_opening element.openingElement;
      "closingElement", option jsx_closing element.closingElement;
      "children", array_of_list jsx_child element.children;
    ]
  )

  and jsx_fragment (loc, (fragment: Loc.t JSX.fragment)) = JSX.(
    node "JSXFragment" loc [
      "openingFragment", jsx_opening_fragment fragment.frag_openingElement;
      "children", array_of_list jsx_child fragment.frag_children;
      "closingFragment", option jsx_closing_fragment fragment.frag_closingElement
    ]
  )

  and jsx_opening (loc, opening) = JSX.Opening.(
    node "JSXOpeningElement" loc [
      "name", jsx_name opening.name;
      "attributes", array_of_list jsx_opening_attribute opening.attributes;
      "selfClosing", bool opening.selfClosing;
    ]
  )

  and jsx_opening_fragment loc =
    node "JSXOpeningFragment" loc []

  and jsx_opening_attribute = JSX.Opening.(function
    | Attribute attribute -> jsx_attribute attribute
    | SpreadAttribute attribute -> jsx_spread_attribute attribute
  )

  and jsx_closing (loc, closing) = JSX.Closing.(
    node "JSXClosingElement" loc [
      "name", jsx_name closing.name;
    ]
  )

  and jsx_closing_fragment loc =
    node "JSXClosingFragment" loc []

  and jsx_child = JSX.(function
    | loc, Element element -> jsx_element (loc, element)
    | loc, Fragment fragment -> jsx_fragment (loc, fragment)
    | loc, ExpressionContainer expr -> jsx_expression_container (loc, expr)
    | loc, SpreadChild expr ->
        node "JSXSpreadChild" loc [
          "expression", expression expr;
        ]
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
    node "JSXAttribute" loc [
      "name", name;
      "value", option jsx_attribute_value attribute.value;
    ]
  )

  and jsx_attribute_value = JSX.Attribute.(function
    | Literal (loc, value) -> literal (loc, value)
    | ExpressionContainer (loc, expr) -> jsx_expression_container (loc, expr)
  )

  and jsx_spread_attribute (loc, attribute) = JSX.SpreadAttribute.(
    node "JSXSpreadAttribute" loc [
      "argument", expression attribute.argument;
    ]
  )

  and jsx_expression_container (loc, expr) = JSX.ExpressionContainer.(
    let expression = match expr.expression with
    | Expression expr -> expression expr
    | EmptyExpression empty_loc ->
        node "JSXEmptyExpression" empty_loc []
    in
    node "JSXExpressionContainer" loc [
      "expression", expression;
    ]
  )

  and jsx_text (loc, text) = JSX.Text.(
    let { value; raw; } = text in
    node "JSXText" loc [
      "value", string value;
      "raw", string raw;
    ]
  )

  and jsx_member_expression (loc, member_expression) = JSX.MemberExpression.(
    let _object = match member_expression._object with
    | Identifier id -> jsx_identifier id
    | MemberExpression member -> jsx_member_expression member in
    node "JSXMemberExpression" loc [
      "object", _object;
      "property", jsx_identifier member_expression.property;
    ]
  )

  and jsx_namespaced_name (loc, namespaced_name) = JSX.NamespacedName.(
    node "JSXNamespacedName" loc [
      "namespace", jsx_identifier namespaced_name.namespace;
      "name", jsx_identifier namespaced_name.name;
    ]
  )

  and jsx_identifier (loc, id) = JSX.Identifier.(
    node "JSXIdentifier" loc [
      "name", string id.name;
    ]
  )

  and export_specifier (loc, specifier) =
    let open Statement.ExportNamedDeclaration.ExportSpecifier in
    let exported = match specifier.exported with
    | Some exported -> identifier exported
    | None -> identifier specifier.local
    in
    node "ExportSpecifier" loc [
      "local", identifier specifier.local;
      "exported", exported;
    ]

  and import_default_specifier id =
    node "ImportDefaultSpecifier" (fst id) [
      "local", identifier id;
    ]

  and import_namespace_specifier (loc, id) =
    node "ImportNamespaceSpecifier" loc [
      "local", identifier id;
    ]

  and import_named_specifier local_id remote_id kind =
    let span_loc =
      match local_id with
      | Some local_id -> Loc.btwn (fst remote_id) (fst local_id)
      | None -> fst remote_id
    in
    let local_id = match local_id with
    | Some id -> id
    | None -> remote_id
    in
    node "ImportSpecifier" span_loc [
      "imported", identifier remote_id;
      "local", identifier local_id;
      "importKind", (
        match kind with
        | Some Statement.ImportDeclaration.ImportType -> string "type"
        | Some Statement.ImportDeclaration.ImportTypeof -> string "typeof"
        | Some Statement.ImportDeclaration.ImportValue | None -> null
      );
    ]

  and comment_list comments = array_of_list comment comments

  and comment (loc, c) = Comment.(
    let _type, value = match c with
      | Line s -> "Line", s
      | Block s -> "Block", s in
    node _type loc [
      "value", string value;
    ]
  )

  and predicate (loc, p) = Ast.Type.Predicate.(
    let _type, value = match p with
      | Declared e -> "DeclaredPredicate", ["value", expression e]
      | Inferred -> "InferredPredicate", []
    in
    node _type loc value
  )
end
