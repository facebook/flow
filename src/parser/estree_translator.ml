(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module type Config = sig
  val include_locs : bool

  val include_comments : bool

  (* FIXME(festevezga, T39098154) Temporary flag while we're migrating from one approach to another *)
  val include_interned_comments : bool
end

module Translate (Impl : Translator_intf.S) (Config : Config) : sig
  type t

  val program :
    Offset_utils.t option ->
    Loc.t * (Loc.t, Loc.t) Ast.Statement.t list * (Loc.t * Ast.Comment.t') list ->
    t

  val expression : Offset_utils.t option -> (Loc.t, Loc.t) Ast.Expression.t -> t

  val errors : (Loc.t * Parse_error.t) list -> t
end
with type t = Impl.t = struct
  type t = Impl.t

  type functions = {
    program: Loc.t * (Loc.t, Loc.t) Ast.Statement.t list * (Loc.t * Ast.Comment.t') list -> t;
    expression: (Loc.t, Loc.t) Ast.Expression.t -> t;
  }

  open Ast
  open Impl

  let array_of_list fn list = array (List.rev_map fn list |> List.rev)

  let option f = function
    | Some v -> f v
    | None -> null

  let hint f = function
    | Ast.Type.Available v -> f v
    | Ast.Type.Missing _ -> null

  let position p = obj [("line", int p.Loc.line); ("column", int p.Loc.column)]

  let loc location =
    let source =
      match Loc.source location with
      | Some (File_key.LibFile src)
      | Some (File_key.SourceFile src)
      | Some (File_key.JsonFile src)
      | Some (File_key.ResourceFile src) ->
        string src
      | Some File_key.Builtins -> string "(global)"
      | None -> null
    in
    obj
      [
        ("source", source);
        ("start", position location.Loc.start);
        ("end", position location.Loc._end);
      ]

  let errors l =
    let error (location, e) =
      obj [("loc", loc location); ("message", string (Parse_error.PP.error e))]
    in
    array_of_list error l

  (* This is basically a lightweight class. We close over some state and then return more than one
   * function that can access that state. We don't need most class features though, so let's avoid
   * the dynamic dispatch and the disruptive change. *)
  let make_functions offset_table =
    let range offset_table location =
      Loc.(
        array
          [
            int (Offset_utils.offset offset_table location.start);
            int (Offset_utils.offset offset_table location._end);
          ])
    in
    let rec node _type location ?comments props =
      let locs =
        if Config.include_locs then
          (* sorted backwards due to the rev_append below *)
          let range =
            match offset_table with
            | Some table -> [("range", range table location)]
            | None -> []
          in
          range @ [("loc", loc location)]
        else
          []
      in
      let comments =
        let open Ast.Syntax in
        match (Config.include_interned_comments, comments) with
        | (true, Some c) ->
          (match c with
          | { leading = _ :: _ as l; trailing = _ :: _ as t; _ } ->
            [("leadingComments", comment_list l); ("trailingComments", comment_list t)]
          | { leading = _ :: _ as l; trailing = []; _ } -> [("leadingComments", comment_list l)]
          | { leading = []; trailing = _ :: _ as t; _ } -> [("trailingComments", comment_list t)]
          | _ -> [])
        | (_, _) -> []
      in
      let prefix = locs @ comments @ [("type", string _type)] in
      obj (List.rev_append prefix props)
    and program (loc, statements, comments) =
      let body = statement_list statements in
      let props =
        if Config.include_comments then
          [("body", body); ("comments", comment_list comments)]
        else
          [("body", body)]
      in
      node "Program" loc props
    and statement_list statements = array_of_list statement statements
    and statement =
      let open Statement in
      function
      | (loc, Empty { Empty.comments }) -> node ?comments "EmptyStatement" loc []
      | (loc, Block b) -> block (loc, b)
      | (loc, Expression { Expression.expression = expr; directive; comments }) ->
        node
          ?comments
          "ExpressionStatement"
          loc
          [("expression", expression expr); ("directive", option string directive)]
      | (loc, If { If.test; consequent; alternate; comments }) ->
        node
          ?comments
          "IfStatement"
          loc
          [
            ("test", expression test);
            ("consequent", statement consequent);
            ("alternate", option statement alternate);
          ]
      | (loc, Labeled { Labeled.label; body; comments }) ->
        node
          ?comments
          "LabeledStatement"
          loc
          [("label", identifier label); ("body", statement body)]
      | (loc, Break { Break.label; comments }) ->
        node ?comments "BreakStatement" loc [("label", option identifier label)]
      | (loc, Continue { Continue.label; comments }) ->
        node ?comments "ContinueStatement" loc [("label", option identifier label)]
      | (loc, With { With._object; body; comments }) ->
        node
          ?comments
          "WithStatement"
          loc
          [("object", expression _object); ("body", statement body)]
      | (loc, TypeAlias alias) -> type_alias (loc, alias)
      | (loc, OpaqueType opaque_t) -> opaque_type ~declare:false (loc, opaque_t)
      | (loc, Switch { Switch.discriminant; cases; comments }) ->
        node
          ?comments
          "SwitchStatement"
          loc
          [("discriminant", expression discriminant); ("cases", array_of_list case cases)]
      | (loc, Return { Return.argument; comments }) ->
        node ?comments "ReturnStatement" loc [("argument", option expression argument)]
      | (loc, Throw { Throw.argument; comments }) ->
        node ?comments "ThrowStatement" loc [("argument", expression argument)]
      | (loc, Try { Try.block = block_; handler; finalizer; comments }) ->
        node
          ?comments
          "TryStatement"
          loc
          [
            ("block", block block_);
            ("handler", option catch handler);
            ("finalizer", option block finalizer);
          ]
      | (loc, While { While.test; body; comments }) ->
        node ?comments "WhileStatement" loc [("test", expression test); ("body", statement body)]
      | (loc, DoWhile { DoWhile.body; test; comments }) ->
        node ?comments "DoWhileStatement" loc [("body", statement body); ("test", expression test)]
      | (loc, For { For.init = init_; test; update; body }) ->
        let init = function
          | For.InitDeclaration init -> variable_declaration init
          | For.InitExpression expr -> expression expr
        in
        node
          "ForStatement"
          loc
          [
            ("init", option init init_);
            ("test", option expression test);
            ("update", option expression update);
            ("body", statement body);
          ]
      | (loc, ForIn { ForIn.left; right; body; each }) ->
        let left =
          match left with
          | ForIn.LeftDeclaration left -> variable_declaration left
          | ForIn.LeftPattern left -> pattern left
        in
        node
          "ForInStatement"
          loc
          [
            ("left", left);
            ("right", expression right);
            ("body", statement body);
            ("each", bool each);
          ]
      | (loc, ForOf { ForOf.await; left; right; body }) ->
        let left =
          match left with
          | ForOf.LeftDeclaration left -> variable_declaration left
          | ForOf.LeftPattern left -> pattern left
        in
        node
          "ForOfStatement"
          loc
          [
            ("left", left);
            ("right", expression right);
            ("body", statement body);
            ("await", bool await);
          ]
      | (loc, EnumDeclaration enum) -> enum_declaration (loc, enum)
      | (loc, Debugger { Debugger.comments }) -> node ?comments "DebuggerStatement" loc []
      | (loc, ClassDeclaration c) -> class_declaration (loc, c)
      | (loc, InterfaceDeclaration i) -> interface_declaration (loc, i)
      | (loc, VariableDeclaration var) -> variable_declaration (loc, var)
      | (loc, FunctionDeclaration fn) -> function_declaration (loc, fn)
      | (loc, DeclareVariable d) -> declare_variable (loc, d)
      | (loc, DeclareFunction d) -> declare_function (loc, d)
      | (loc, DeclareClass d) -> declare_class (loc, d)
      | (loc, DeclareInterface i) -> declare_interface (loc, i)
      | (loc, DeclareTypeAlias a) -> declare_type_alias (loc, a)
      | (loc, DeclareOpaqueType t) -> opaque_type ~declare:true (loc, t)
      | (loc, DeclareModule { DeclareModule.id; body; kind }) ->
        let id =
          match id with
          | DeclareModule.Literal lit -> string_literal lit
          | DeclareModule.Identifier id -> identifier id
        in
        node
          "DeclareModule"
          loc
          [
            ("id", id);
            ("body", block body);
            ( "kind",
              match kind with
              | DeclareModule.CommonJS _ -> string "CommonJS"
              | DeclareModule.ES _ -> string "ES" );
          ]
      | ( loc,
          DeclareExportDeclaration
            { DeclareExportDeclaration.specifiers; declaration; default; source } ) ->
        begin
          match specifiers with
          | Some (ExportNamedDeclaration.ExportBatchSpecifier (_, None)) ->
            node "DeclareExportAllDeclaration" loc [("source", option string_literal source)]
          | _ ->
            let declaration =
              match declaration with
              | Some (DeclareExportDeclaration.Variable v) -> declare_variable v
              | Some (DeclareExportDeclaration.Function f) -> declare_function f
              | Some (DeclareExportDeclaration.Class c) -> declare_class c
              | Some (DeclareExportDeclaration.DefaultType t) -> _type t
              | Some (DeclareExportDeclaration.NamedType t) -> type_alias t
              | Some (DeclareExportDeclaration.NamedOpaqueType t) -> opaque_type ~declare:true t
              | Some (DeclareExportDeclaration.Interface i) -> interface_declaration i
              | None -> null
            in
            node
              "DeclareExportDeclaration"
              loc
              [
                ( "default",
                  bool
                    (match default with
                    | Some _ -> true
                    | None -> false) );
                ("declaration", declaration);
                ("specifiers", export_specifiers specifiers);
                ("source", option string_literal source);
              ]
        end
      | (loc, DeclareModuleExports annot) ->
        node "DeclareModuleExports" loc [("typeAnnotation", type_annotation annot)]
      | ( loc,
          ExportNamedDeclaration
            { ExportNamedDeclaration.specifiers; declaration; source; exportKind; comments } ) ->
        begin
          match specifiers with
          | Some (ExportNamedDeclaration.ExportBatchSpecifier (_, None)) ->
            node
              ?comments
              "ExportAllDeclaration"
              loc
              [
                ("source", option string_literal source);
                ("exportKind", string (export_kind exportKind));
              ]
          | _ ->
            node
              ?comments
              "ExportNamedDeclaration"
              loc
              [
                ("declaration", option statement declaration);
                ("specifiers", export_specifiers specifiers);
                ("source", option string_literal source);
                ("exportKind", string (export_kind exportKind));
              ]
        end
      | ( loc,
          ExportDefaultDeclaration
            {
              ExportDefaultDeclaration.declaration;
              default = _ (* TODO: confirm we shouldn't use this *);
              comments;
            } ) ->
        let declaration =
          match declaration with
          | ExportDefaultDeclaration.Declaration stmt -> statement stmt
          | ExportDefaultDeclaration.Expression expr -> expression expr
        in
        node
          ?comments
          "ExportDefaultDeclaration"
          loc
          [("declaration", declaration); ("exportKind", string (export_kind Statement.ExportValue))]
      | ( loc,
          ImportDeclaration { ImportDeclaration.specifiers; default; importKind; source; comments }
        ) ->
        let specifiers =
          match specifiers with
          | Some (ImportDeclaration.ImportNamedSpecifiers specifiers) ->
            List.map
              (fun { ImportDeclaration.local; remote; kind } ->
                import_named_specifier local remote kind)
              specifiers
          | Some (ImportDeclaration.ImportNamespaceSpecifier id) -> [import_namespace_specifier id]
          | None -> []
        in
        let specifiers =
          match default with
          | Some default -> import_default_specifier default :: specifiers
          | None -> specifiers
        in
        let import_kind =
          match importKind with
          | ImportDeclaration.ImportType -> "type"
          | ImportDeclaration.ImportTypeof -> "typeof"
          | ImportDeclaration.ImportValue -> "value"
        in
        node
          ?comments
          "ImportDeclaration"
          loc
          [
            ("specifiers", array specifiers);
            ("source", string_literal source);
            ("importKind", string import_kind);
          ]
    and expression =
      let open Expression in
      function
      | (loc, This { This.comments }) -> node ?comments "ThisExpression" loc []
      | (loc, Super { Super.comments }) -> node ?comments "Super" loc []
      | (loc, Array { Array.elements; comments }) ->
        node
          ?comments
          "ArrayExpression"
          loc
          [("elements", array_of_list (option expression_or_spread) elements)]
      | (loc, Object { Object.properties; comments }) ->
        node
          ?comments
          "ObjectExpression"
          loc
          [("properties", array_of_list object_property properties)]
      | (loc, Function _function) -> function_expression (loc, _function)
      | ( loc,
          ArrowFunction
            {
              Function.params = (_, { Function.Params.comments = params_comments; _ }) as params;
              async;
              predicate = predicate_;
              tparams;
              return;
              body;
              comments = func_comments;
              sig_loc = _;
              (* TODO: arrows shouldn't have these: *)
              id = _;
              generator = _;
            } ) ->
        let (body, expression) =
          match body with
          | Function.BodyBlock b -> (block b, false)
          | Function.BodyExpression expr -> (expression expr, true)
        in
        let return =
          match return with
          | Ast.Type.Missing _ -> None
          | Ast.Type.Available t -> Some t
        in
        let comments = Flow_ast_utils.merge_comments ~outer:func_comments ~inner:params_comments in
        node
          ?comments
          "ArrowFunctionExpression"
          loc
          [
            ("id", null);
            ("params", function_params params);
            ("body", body);
            ("async", bool async);
            ("generator", bool false);
            ("predicate", option predicate predicate_);
            ("expression", bool expression);
            ("returnType", option type_annotation return);
            ("typeParameters", option type_parameter_declaration tparams);
          ]
      | (loc, Sequence { Sequence.expressions; comments }) ->
        node
          ?comments
          "SequenceExpression"
          loc
          [("expressions", array_of_list expression expressions)]
      | (loc, Unary { Unary.operator; argument; comments }) ->
        Unary.(
          (match operator with
          | Await -> node ?comments "AwaitExpression" loc [("argument", expression argument)]
          | _ ->
            let operator =
              match operator with
              | Minus -> "-"
              | Plus -> "+"
              | Not -> "!"
              | BitNot -> "~"
              | Typeof -> "typeof"
              | Void -> "void"
              | Delete -> "delete"
              | Await -> failwith "matched above"
            in
            node
              ?comments
              "UnaryExpression"
              loc
              [
                ("operator", string operator);
                ("prefix", bool true);
                ("argument", expression argument);
              ]))
      | (loc, Binary { Binary.left; operator; right; comments }) ->
        node
          ?comments
          "BinaryExpression"
          loc
          [
            ("operator", string (Flow_ast_utils.string_of_binary_operator operator));
            ("left", expression left);
            ("right", expression right);
          ]
      | (loc, TypeCast { TypeCast.expression = expr; annot; comments }) ->
        node
          ?comments
          "TypeCastExpression"
          loc
          [("expression", expression expr); ("typeAnnotation", type_annotation annot)]
      | (loc, Assignment { Assignment.left; operator; right; comments }) ->
        let operator =
          match operator with
          | None -> "="
          | Some op -> Flow_ast_utils.string_of_assignment_operator op
        in
        node
          ?comments
          "AssignmentExpression"
          loc
          [("operator", string operator); ("left", pattern left); ("right", expression right)]
      | (loc, Update { Update.operator; argument; prefix; comments }) ->
        let operator =
          match operator with
          | Update.Increment -> "++"
          | Update.Decrement -> "--"
        in
        node
          ?comments
          "UpdateExpression"
          loc
          [
            ("operator", string operator); ("argument", expression argument); ("prefix", bool prefix);
          ]
      | (loc, Logical { Logical.left; operator; right; comments }) ->
        let operator =
          match operator with
          | Logical.Or -> "||"
          | Logical.And -> "&&"
          | Logical.NullishCoalesce -> "??"
        in
        node
          ?comments
          "LogicalExpression"
          loc
          [("operator", string operator); ("left", expression left); ("right", expression right)]
      | (loc, Conditional { Conditional.test; consequent; alternate; comments }) ->
        node
          ?comments
          "ConditionalExpression"
          loc
          [
            ("test", expression test);
            ("consequent", expression consequent);
            ("alternate", expression alternate);
          ]
      | (loc, New { New.callee; targs; arguments; comments }) ->
        let arguments =
          match arguments with
          | Some arguments -> arg_list arguments
          | None -> array []
        in
        node
          ?comments
          "NewExpression"
          loc
          [
            ("callee", expression callee);
            ("typeArguments", option call_type_args targs);
            ("arguments", arguments);
          ]
      | (loc, Call ({ Call.comments; _ } as call)) ->
        node ?comments "CallExpression" loc (call_node_properties call)
      | (loc, OptionalCall { OptionalCall.call = { Call.comments; _ } as call; optional }) ->
        node
          ?comments
          "OptionalCallExpression"
          loc
          (call_node_properties call @ [("optional", bool optional)])
      | (loc, Member ({ Member.comments; _ } as member)) ->
        node ?comments "MemberExpression" loc (member_node_properties member)
      | (loc, OptionalMember { OptionalMember.member = { Member.comments; _ } as member; optional })
        ->
        node
          ?comments
          "OptionalMemberExpression"
          loc
          (member_node_properties member @ [("optional", bool optional)])
      | (loc, Yield { Yield.argument; delegate; comments }) ->
        node
          ?comments
          "YieldExpression"
          loc
          [("argument", option expression argument); ("delegate", bool delegate)]
      | (loc, Comprehension { Comprehension.blocks; filter }) ->
        node
          "ComprehensionExpression"
          loc
          [
            ("blocks", array_of_list comprehension_block blocks);
            ("filter", option expression filter);
          ]
      | (loc, Generator { Generator.blocks; filter }) ->
        node
          "GeneratorExpression"
          loc
          [
            ("blocks", array_of_list comprehension_block blocks);
            ("filter", option expression filter);
          ]
      | (_loc, Identifier id) -> identifier id
      | (loc, Literal ({ Literal.value = Ast.Literal.BigInt _; _ } as lit)) ->
        bigint_literal (loc, lit)
      | (loc, Literal lit) -> literal (loc, lit)
      | (loc, TemplateLiteral lit) -> template_literal (loc, lit)
      | (loc, TaggedTemplate tagged) -> tagged_template (loc, tagged)
      | (loc, Class c) -> class_expression (loc, c)
      | (loc, JSXElement element) -> jsx_element (loc, element)
      | (loc, JSXFragment fragment) -> jsx_fragment (loc, fragment)
      | (loc, MetaProperty { MetaProperty.meta; property; comments }) ->
        node
          ?comments
          "MetaProperty"
          loc
          [("meta", identifier meta); ("property", identifier property)]
      | (loc, Import { Import.argument; comments }) ->
        node
          ?comments
          "CallExpression"
          loc
          [
            ("callee", node "Import" (Loc.btwn loc (fst argument)) []);
            ("arguments", array_of_list expression [argument]);
          ]
    and function_declaration
        ( loc,
          {
            Function.id;
            params = (_, { Function.Params.comments = params_comments; _ }) as params;
            async;
            generator;
            predicate = predicate_;
            tparams;
            return;
            body;
            comments = func_comments;
            sig_loc = _;
          } ) =
      let body =
        match body with
        | Function.BodyBlock b -> b
        | Function.BodyExpression _ -> failwith "Unexpected FunctionDeclaration with BodyExpression"
      in
      let return =
        match return with
        | Ast.Type.Missing _ -> None
        | Ast.Type.Available t -> Some t
      in
      let comments = Flow_ast_utils.merge_comments ~outer:func_comments ~inner:params_comments in
      node
        ?comments
        "FunctionDeclaration"
        loc
        [
          (* estree hasn't come around to the idea that function decls can have
           optional ids, but acorn, babel, espree and esprima all have, so let's
           do it too. see https://github.com/estree/estree/issues/98 *)
          ("id", option identifier id);
          ("params", function_params params);
          ("body", block body);
          ("async", bool async);
          ("generator", bool generator);
          ("predicate", option predicate predicate_);
          ("expression", bool false);
          ("returnType", option type_annotation return);
          ("typeParameters", option type_parameter_declaration tparams);
        ]
    and function_expression
        ( loc,
          {
            Function.id;
            params = (_, { Function.Params.comments = params_comments; _ }) as params;
            async;
            generator;
            predicate = predicate_;
            tparams;
            return;
            body;
            comments = func_comments;
            sig_loc = _;
          } ) =
      let body =
        match body with
        | Function.BodyBlock b -> b
        | Function.BodyExpression _ -> failwith "Unexpected FunctionExpression with BodyExpression"
      in
      let return =
        match return with
        | Ast.Type.Missing _ -> None
        | Ast.Type.Available t -> Some t
      in
      let comments = Flow_ast_utils.merge_comments ~outer:func_comments ~inner:params_comments in
      node
        ?comments
        "FunctionExpression"
        loc
        [
          ("id", option identifier id);
          ("params", function_params params);
          ("body", block body);
          ("async", bool async);
          ("generator", bool generator);
          ("predicate", option predicate predicate_);
          ("expression", bool false);
          ("returnType", option type_annotation return);
          ("typeParameters", option type_parameter_declaration tparams);
        ]
    and identifier (loc, { Identifier.name; comments }) =
      node
        "Identifier"
        ?comments
        loc
        [("name", string name); ("typeAnnotation", null); ("optional", bool false)]
    and private_name (loc, { PrivateName.id; comments }) =
      node ?comments "PrivateName" loc [("id", identifier id)]
    and pattern_identifier
        loc { Pattern.Identifier.name = (_, { Identifier.name; comments }); annot; optional } =
      node
        ?comments
        "Identifier"
        loc
        [
          ("name", string name);
          ("typeAnnotation", hint type_annotation annot);
          ("optional", bool optional);
        ]
    and arg_list (_loc, arguments) =
      (* ESTree does not have a unique node for argument lists, so there's nowhere to
         include the loc. *)
      array_of_list expression_or_spread arguments
    and case (loc, { Statement.Switch.Case.test; consequent; comments }) =
      node
        ?comments
        "SwitchCase"
        loc
        [("test", option expression test); ("consequent", array_of_list statement consequent)]
    and catch (loc, { Statement.Try.CatchClause.param; body; comments }) =
      node ?comments "CatchClause" loc [("param", option pattern param); ("body", block body)]
    and block (loc, { Statement.Block.body; comments }) =
      node ?comments "BlockStatement" loc [("body", statement_list body)]
    and declare_variable (loc, { Statement.DeclareVariable.id; annot; comments }) =
      let id_loc =
        Loc.btwn
          (fst id)
          (match annot with
          | Ast.Type.Available annot -> fst annot
          | Ast.Type.Missing _ -> fst id)
      in
      node
        ?comments
        "DeclareVariable"
        loc
        [
          ("id", pattern_identifier id_loc { Pattern.Identifier.name = id; annot; optional = false });
        ]
    and declare_function
        (loc, { Statement.DeclareFunction.id; annot; predicate = predicate_; comments }) =
      let id_loc = Loc.btwn (fst id) (fst annot) in
      node
        ?comments
        "DeclareFunction"
        loc
        [
          ( "id",
            pattern_identifier
              id_loc
              { Pattern.Identifier.name = id; annot = Ast.Type.Available annot; optional = false }
          );
          ("predicate", option predicate predicate_);
        ]
    and declare_class
        (loc, { Statement.DeclareClass.id; tparams; body; extends; implements; mixins; comments }) =
      (* TODO: extends shouldn't return an array *)
      let extends =
        match extends with
        | Some extends -> array [interface_extends extends]
        | None -> array []
      in
      let implements =
        match implements with
        | Some (_, { Class.Implements.interfaces; comments = _ }) ->
          array_of_list class_implements interfaces
        | None -> array []
      in
      node
        ?comments
        "DeclareClass"
        loc
        [
          ("id", identifier id);
          ("typeParameters", option type_parameter_declaration tparams);
          ("body", object_type ~include_inexact:false body);
          ("extends", extends);
          ("implements", implements);
          ("mixins", array_of_list interface_extends mixins);
        ]
    and declare_interface (loc, { Statement.Interface.id; tparams; body; extends; comments }) =
      node
        ?comments
        "DeclareInterface"
        loc
        [
          ("id", identifier id);
          ("typeParameters", option type_parameter_declaration tparams);
          ("body", object_type ~include_inexact:false body);
          ("extends", array_of_list interface_extends extends);
        ]
    and export_kind = function
      | Statement.ExportType -> "type"
      | Statement.ExportValue -> "value"
    and export_specifiers =
      let open Statement.ExportNamedDeclaration in
      function
      | Some (ExportSpecifiers specifiers) -> array_of_list export_specifier specifiers
      | Some (ExportBatchSpecifier (loc, Some name)) ->
        array [node "ExportNamespaceSpecifier" loc [("exported", identifier name)]]
      | Some (ExportBatchSpecifier (_, None)) ->
        (* this should've been handled by callers, since this represents an
             ExportAllDeclaration, not a specifier. *)
        array []
      | None -> array []
    and declare_type_alias (loc, { Statement.TypeAlias.id; tparams; right; comments }) =
      node
        ?comments
        "DeclareTypeAlias"
        loc
        [
          ("id", identifier id);
          ("typeParameters", option type_parameter_declaration tparams);
          ("right", _type right);
        ]
    and type_alias (loc, { Statement.TypeAlias.id; tparams; right; comments }) =
      node
        ?comments
        "TypeAlias"
        loc
        [
          ("id", identifier id);
          ("typeParameters", option type_parameter_declaration tparams);
          ("right", _type right);
        ]
    and opaque_type
        ~declare (loc, { Statement.OpaqueType.id; tparams; impltype; supertype; comments }) =
      let name =
        if declare then
          "DeclareOpaqueType"
        else
          "OpaqueType"
      in
      node
        ?comments
        name
        loc
        [
          ("id", identifier id);
          ("typeParameters", option type_parameter_declaration tparams);
          ("impltype", option _type impltype);
          ("supertype", option _type supertype);
        ]
    and class_declaration ast = class_helper "ClassDeclaration" ast
    and class_expression ast = class_helper "ClassExpression" ast
    and class_helper
        node_type (loc, { Class.id; extends; body; tparams; implements; classDecorators; comments })
        =
      let (super, super_targs, comments) =
        match extends with
        | Some (_, { Class.Extends.expr; targs; comments = extends_comments }) ->
          (Some expr, targs, Flow_ast_utils.merge_comments ~outer:comments ~inner:extends_comments)
        | None -> (None, None, comments)
      in
      let (implements, comments) =
        match implements with
        | Some (_, { Class.Implements.interfaces; comments = implements_comments }) ->
          ( array_of_list class_implements interfaces,
            Flow_ast_utils.merge_comments ~outer:comments ~inner:implements_comments )
        | None -> (array [], comments)
      in
      node
        ?comments
        node_type
        loc
        [
          (* estree hasn't come around to the idea that class decls can have
           optional ids, but acorn, babel, espree and esprima all have, so let's
           do it too. see https://github.com/estree/estree/issues/98 *)
          ("id", option identifier id);
          ("body", class_body body);
          ("typeParameters", option type_parameter_declaration tparams);
          ("superClass", option expression super);
          ("superTypeParameters", option type_args super_targs);
          ("implements", implements);
          ("decorators", array_of_list class_decorator classDecorators);
        ]
    and class_decorator (loc, { Class.Decorator.expression = expr; comments }) =
      node ?comments "Decorator" loc [("expression", expression expr)]
    and class_implements (loc, { Class.Implements.Interface.id; targs }) =
      node "ClassImplements" loc [("id", identifier id); ("typeParameters", option type_args targs)]
    and class_body (loc, { Class.Body.body; comments }) =
      node ?comments "ClassBody" loc [("body", array_of_list class_element body)]
    and class_element =
      Class.Body.(
        function
        | Method m -> class_method m
        | PrivateField p -> class_private_field p
        | Property p -> class_property p)
    and class_method (loc, { Class.Method.key; value; kind; static; decorators; comments }) =
      let (key, computed, comments) =
        let open Expression.Object.Property in
        match key with
        | Literal lit -> (literal lit, false, comments)
        | Identifier id -> (identifier id, false, comments)
        | PrivateName name -> (private_name name, false, comments)
        | Computed (_, { ComputedKey.expression = expr; comments = computed_comments }) ->
          ( expression expr,
            true,
            Flow_ast_utils.merge_comments ~outer:comments ~inner:computed_comments )
      in
      let kind =
        Class.Method.(
          match kind with
          | Constructor -> "constructor"
          | Method -> "method"
          | Get -> "get"
          | Set -> "set")
      in
      node
        ?comments
        "MethodDefinition"
        loc
        [
          ("key", key);
          ("value", function_expression value);
          ("kind", string kind);
          ("static", bool static);
          ("computed", bool computed);
          ("decorators", array_of_list class_decorator decorators);
        ]
    and class_private_field
        ( loc,
          {
            Class.PrivateField.key = (_, { PrivateName.id; comments = key_comments });
            value;
            annot;
            static;
            variance = variance_;
            comments;
          } ) =
      let (value, declare) =
        match value with
        | Class.Property.Declared -> (None, true)
        | Class.Property.Uninitialized -> (None, false)
        | Class.Property.Initialized x -> (Some x, false)
      in
      let comments = Flow_ast_utils.merge_comments ~outer:comments ~inner:key_comments in
      let props =
        [
          ("key", identifier id);
          ("value", option expression value);
          ("typeAnnotation", hint type_annotation annot);
          ("static", bool static);
          ("variance", option variance variance_);
        ]
        @
        if declare then
          [("declare", bool declare)]
        else
          []
      in
      node ?comments "ClassPrivateProperty" loc props
    and class_property
        (loc, { Class.Property.key; value; annot; static; variance = variance_; comments }) =
      let (key, computed, comments) =
        match key with
        | Expression.Object.Property.Literal lit -> (literal lit, false, comments)
        | Expression.Object.Property.Identifier id -> (identifier id, false, comments)
        | Expression.Object.Property.PrivateName _ ->
          failwith "Internal Error: Private name found in class prop"
        | Expression.Object.Property.Computed
            (_, { ComputedKey.expression = expr; comments = key_comments }) ->
          (expression expr, true, Flow_ast_utils.merge_comments ~outer:comments ~inner:key_comments)
      in
      let (value, declare) =
        match value with
        | Class.Property.Declared -> (None, true)
        | Class.Property.Uninitialized -> (None, false)
        | Class.Property.Initialized x -> (Some x, false)
      in
      let props =
        [
          ("key", key);
          ("value", option expression value);
          ("typeAnnotation", hint type_annotation annot);
          ("computed", bool computed);
          ("static", bool static);
          ("variance", option variance variance_);
        ]
        @
        if declare then
          [("declare", bool declare)]
        else
          []
      in
      node ?comments "ClassProperty" loc props
    and enum_declaration (loc, { Statement.EnumDeclaration.id; body }) =
      let open Statement.EnumDeclaration in
      let enum_body =
        match body with
        | (loc, BooleanBody { BooleanBody.members; explicitType }) ->
          node
            "EnumBooleanBody"
            loc
            [
              ( "members",
                array_of_list
                  (fun ( loc,
                         {
                           InitializedMember.id;
                           init = (_, { BooleanLiteral.value; comments = _ });
                         } ) ->
                    node "EnumBooleanMember" loc [("id", identifier id); ("init", bool value)])
                  members );
              ("explicitType", bool explicitType);
            ]
        | (loc, NumberBody { NumberBody.members; explicitType }) ->
          node
            "EnumNumberBody"
            loc
            [
              ( "members",
                array_of_list
                  (fun (loc, { InitializedMember.id; init }) ->
                    node
                      "EnumNumberMember"
                      loc
                      [("id", identifier id); ("init", number_literal init)])
                  members );
              ("explicitType", bool explicitType);
            ]
        | (loc, StringBody { StringBody.members; explicitType }) ->
          let members =
            match members with
            | StringBody.Defaulted defaulted_members ->
              List.map
                (fun (loc, { DefaultedMember.id }) ->
                  node "EnumDefaultedMember" loc [("id", identifier id)])
                defaulted_members
            | StringBody.Initialized initialized_members ->
              List.map
                (fun (loc, { InitializedMember.id; init }) ->
                  node "EnumStringMember" loc [("id", identifier id); ("init", string_literal init)])
                initialized_members
          in
          node
            "EnumStringBody"
            loc
            [("members", array members); ("explicitType", bool explicitType)]
        | (loc, SymbolBody { SymbolBody.members }) ->
          node
            "EnumSymbolBody"
            loc
            [
              ( "members",
                array_of_list
                  (fun (loc, { DefaultedMember.id }) ->
                    node "EnumDefaultedMember" loc [("id", identifier id)])
                  members );
            ]
      in
      node "EnumDeclaration" loc [("id", identifier id); ("body", enum_body)]
    and interface_declaration (loc, { Statement.Interface.id; tparams; body; extends; comments }) =
      node
        ?comments
        "InterfaceDeclaration"
        loc
        [
          ("id", identifier id);
          ("typeParameters", option type_parameter_declaration tparams);
          ("body", object_type ~include_inexact:false body);
          ("extends", array_of_list interface_extends extends);
        ]
    and interface_extends (loc, { Type.Generic.id; targs; comments }) =
      let id =
        match id with
        | Type.Generic.Identifier.Unqualified id -> identifier id
        | Type.Generic.Identifier.Qualified q -> generic_type_qualified_identifier q
      in
      node ?comments "InterfaceExtends" loc [("id", id); ("typeParameters", option type_args targs)]
    and pattern =
      Pattern.(
        function
        | (loc, Object { Object.properties; annot }) ->
          node
            "ObjectPattern"
            loc
            [
              ("properties", array_of_list object_pattern_property properties);
              ("typeAnnotation", hint type_annotation annot);
            ]
        | (loc, Array { Array.elements; annot; comments }) ->
          node
            ?comments
            "ArrayPattern"
            loc
            [
              ("elements", array_of_list (option array_pattern_element) elements);
              ("typeAnnotation", hint type_annotation annot);
            ]
        | (loc, Identifier pattern_id) -> pattern_identifier loc pattern_id
        | (_loc, Expression expr) -> expression expr)
    and function_param (loc, { Ast.Function.Param.argument; default }) =
      match default with
      | Some default ->
        node "AssignmentPattern" loc [("left", pattern argument); ("right", expression default)]
      | None -> pattern argument
    and function_params =
      let open Ast.Function.Params in
      function
      | ( _,
          {
            params;
            rest = Some (rest_loc, { Function.RestParam.argument; comments });
            comments = _;
          } ) ->
        let rest = node ?comments "RestElement" rest_loc [("argument", pattern argument)] in
        let rev_params = List.rev_map function_param params in
        let params = List.rev (rest :: rev_params) in
        array params
      | (_, { params; rest = None; comments = _ }) -> array_of_list function_param params
    and rest_element loc { Pattern.RestElement.argument; comments } =
      node ?comments "RestElement" loc [("argument", pattern argument)]
    and array_pattern_element =
      let open Pattern.Array in
      function
      | Element (loc, { Element.argument; default = Some default }) ->
        node "AssignmentPattern" loc [("left", pattern argument); ("right", expression default)]
      | Element (_loc, { Element.argument; default = None }) -> pattern argument
      | RestElement (loc, el) -> rest_element loc el
    and object_property =
      let open Expression.Object in
      function
      | Property (loc, prop) ->
        Property.(
          let (key, value, kind, method_, shorthand, comments) =
            match prop with
            | Init { key; value; shorthand } ->
              (key, expression value, "init", false, shorthand, None)
            | Method { key; value = (loc, func) } ->
              (key, function_expression (loc, func), "init", true, false, None)
            | Get { key; value = (loc, func); comments } ->
              (key, function_expression (loc, func), "get", false, false, comments)
            | Set { key; value = (loc, func); comments } ->
              (key, function_expression (loc, func), "set", false, false, comments)
          in
          let (key, computed, comments) =
            match key with
            | Literal lit -> (literal lit, false, comments)
            | Identifier id -> (identifier id, false, comments)
            | PrivateName _ -> failwith "Internal Error: Found private field in object props"
            | Computed (_, { ComputedKey.expression = expr; comments = key_comments }) ->
              ( expression expr,
                true,
                Flow_ast_utils.merge_comments ~outer:comments ~inner:key_comments )
          in
          node
            ?comments
            "Property"
            loc
            [
              ("key", key);
              ("value", value);
              ("kind", string kind);
              ("method", bool method_);
              ("shorthand", bool shorthand);
              ("computed", bool computed);
            ])
      | SpreadProperty (loc, { SpreadProperty.argument; comments }) ->
        node ?comments "SpreadProperty" loc [("argument", expression argument)]
    and object_pattern_property =
      let open Pattern.Object in
      function
      | Property (loc, { Property.key; pattern = patt; default; shorthand }) ->
        let (key, computed, comments) =
          match key with
          | Property.Literal lit -> (literal lit, false, None)
          | Property.Identifier id -> (identifier id, false, None)
          | Property.Computed (_, { ComputedKey.expression = expr; comments }) ->
            (expression expr, true, comments)
        in
        let value =
          match default with
          | Some default ->
            let loc = Loc.btwn (fst patt) (fst default) in
            node "AssignmentPattern" loc [("left", pattern patt); ("right", expression default)]
          | None -> pattern patt
        in
        node
          ?comments
          "Property"
          loc
          [
            ("key", key);
            ("value", value);
            ("kind", string "init");
            ("method", bool false);
            ("shorthand", bool shorthand);
            ("computed", bool computed);
          ]
      | RestElement (loc, el) -> rest_element loc el
    and expression_or_spread =
      let open Expression in
      function
      | Expression expr -> expression expr
      | Spread (loc, { SpreadElement.argument; comments }) ->
        node ?comments "SpreadElement" loc [("argument", expression argument)]
    and comprehension_block (loc, { Expression.Comprehension.Block.left; right; each }) =
      node
        "ComprehensionBlock"
        loc
        [("left", pattern left); ("right", expression right); ("each", bool each)]
    and literal (loc, { Literal.value; raw; comments }) =
      let value_ =
        match value with
        | Literal.String str -> string str
        | Literal.Boolean b -> bool b
        | Literal.Null -> null
        | Literal.Number f -> number f
        | Literal.BigInt _ -> failwith "We should not create Literal nodes for bigints"
        | Literal.RegExp { Literal.RegExp.pattern; flags } -> regexp loc pattern flags
      in
      let props =
        match value with
        | Literal.RegExp { Literal.RegExp.pattern; flags } ->
          let regex = obj [("pattern", string pattern); ("flags", string flags)] in
          [("value", value_); ("raw", string raw); ("regex", regex)]
        | _ -> [("value", value_); ("raw", string raw)]
      in
      node ?comments "Literal" loc props
    and number_literal (loc, { NumberLiteral.value; raw; comments }) =
      node ?comments "Literal" loc [("value", number value); ("raw", string raw)]
    and bigint_literal (loc, { Literal.raw; comments; _ }) =
      node ?comments "BigIntLiteral" loc [("value", null); ("bigint", string raw)]
    and string_literal (loc, { StringLiteral.value; raw; comments }) =
      node ?comments "Literal" loc [("value", string value); ("raw", string raw)]
    and template_literal (loc, { Expression.TemplateLiteral.quasis; expressions; comments }) =
      node
        ?comments
        "TemplateLiteral"
        loc
        [
          ("quasis", array_of_list template_element quasis);
          ("expressions", array_of_list expression expressions);
        ]
    and template_element
        ( loc,
          {
            Expression.TemplateLiteral.Element.value =
              { Expression.TemplateLiteral.Element.raw; cooked };
            tail;
          } ) =
      let value = obj [("raw", string raw); ("cooked", string cooked)] in
      node "TemplateElement" loc [("value", value); ("tail", bool tail)]
    and tagged_template (loc, { Expression.TaggedTemplate.tag; quasi; comments }) =
      node
        ?comments
        "TaggedTemplateExpression"
        loc
        [("tag", expression tag); ("quasi", template_literal quasi)]
    and variable_declaration (loc, { Statement.VariableDeclaration.kind; declarations; comments }) =
      let kind =
        match kind with
        | Statement.VariableDeclaration.Var -> "var"
        | Statement.VariableDeclaration.Let -> "let"
        | Statement.VariableDeclaration.Const -> "const"
      in
      node
        ?comments
        "VariableDeclaration"
        loc
        [("declarations", array_of_list variable_declarator declarations); ("kind", string kind)]
    and variable_declarator (loc, { Statement.VariableDeclaration.Declarator.id; init }) =
      node "VariableDeclarator" loc [("id", pattern id); ("init", option expression init)]
    and variance (loc, { Variance.kind; comments }) =
      let kind =
        Variance.(
          match kind with
          | Plus -> string "plus"
          | Minus -> string "minus")
      in
      node ?comments "Variance" loc [("kind", kind)]
    and _type (loc, t) =
      Type.(
        match t with
        | Any comments -> any_type loc comments
        | Mixed comments -> mixed_type loc comments
        | Empty comments -> empty_type loc comments
        | Void comments -> void_type loc comments
        | Null comments -> null_type loc comments
        | Symbol comments -> symbol_type loc comments
        | Number comments -> number_type loc comments
        | BigInt comments -> bigint_type loc comments
        | String comments -> string_type loc comments
        | Boolean comments -> boolean_type loc comments
        | Nullable t -> nullable_type loc t
        | Function fn -> function_type (loc, fn)
        | Object o -> object_type ~include_inexact:true (loc, o)
        | Interface i -> interface_type (loc, i)
        | Array t -> array_type loc t
        | Generic g -> generic_type (loc, g)
        | Union t -> union_type (loc, t)
        | Intersection t -> intersection_type (loc, t)
        | Typeof t -> typeof_type (loc, t)
        | Tuple t -> tuple_type (loc, t)
        | StringLiteral s -> string_literal_type (loc, s)
        | NumberLiteral n -> number_literal_type (loc, n)
        | BigIntLiteral n -> bigint_literal_type (loc, n)
        | BooleanLiteral b -> boolean_literal_type (loc, b)
        | Exists comments -> exists_type loc comments)
    and any_type loc comments = node ?comments "AnyTypeAnnotation" loc []
    and mixed_type loc comments = node ?comments "MixedTypeAnnotation" loc []
    and empty_type loc comments = node ?comments "EmptyTypeAnnotation" loc []
    and void_type loc comments = node ?comments "VoidTypeAnnotation" loc []
    and null_type loc comments = node ?comments "NullLiteralTypeAnnotation" loc []
    and symbol_type loc comments = node ?comments "SymbolTypeAnnotation" loc []
    and number_type loc comments = node ?comments "NumberTypeAnnotation" loc []
    and bigint_type loc comments = node ?comments "BigIntTypeAnnotation" loc []
    and string_type loc comments = node ?comments "StringTypeAnnotation" loc []
    and boolean_type loc comments = node ?comments "BooleanTypeAnnotation" loc []
    and nullable_type loc { Type.Nullable.argument; comments } =
      node ?comments "NullableTypeAnnotation" loc [("typeAnnotation", _type argument)]
    and function_type
        ( loc,
          {
            Type.Function.params =
              (_, { Type.Function.Params.params; rest; comments = params_comments });
            return;
            tparams;
            comments = func_comments;
          } ) =
      let comments = Flow_ast_utils.merge_comments ~inner:params_comments ~outer:func_comments in
      node
        ?comments
        "FunctionTypeAnnotation"
        loc
        [
          ("params", array_of_list function_type_param params);
          ("returnType", _type return);
          ("rest", option function_type_rest rest);
          ("typeParameters", option type_parameter_declaration tparams);
        ]
    and function_type_param ?comments (loc, { Type.Function.Param.name; annot; optional }) =
      node
        ?comments
        "FunctionTypeParam"
        loc
        [
          ("name", option identifier name);
          ("typeAnnotation", _type annot);
          ("optional", bool optional);
        ]
    and function_type_rest (_loc, { Type.Function.RestParam.argument; comments }) =
      (* TODO: add a node for the rest param itself, including the `...`,
         like we do with RestElement on normal functions. This should be
         coordinated with Babel, ast-types, etc. so keeping the status quo for
         now. Here's an example: *)
      (* node "FunctionTypeRestParam" loc [
        "argument", function_type_param argument;
      ] *)
      function_type_param ?comments argument
    and object_type ~include_inexact (loc, { Type.Object.properties; exact; inexact; comments }) =
      Type.Object.(
        let (props, ixs, calls, slots) =
          List.fold_left
            (fun (props, ixs, calls, slots) -> function
              | Property p ->
                let prop = object_type_property p in
                (prop :: props, ixs, calls, slots)
              | SpreadProperty p ->
                let prop = object_type_spread_property p in
                (prop :: props, ixs, calls, slots)
              | Indexer i ->
                let ix = object_type_indexer i in
                (props, ix :: ixs, calls, slots)
              | CallProperty c ->
                let call = object_type_call_property c in
                (props, ixs, call :: calls, slots)
              | InternalSlot s ->
                let slot = object_type_internal_slot s in
                (props, ixs, calls, slot :: slots))
            ([], [], [], [])
            properties
        in
        let fields =
          [
            ("exact", bool exact);
            ("properties", array (List.rev props));
            ("indexers", array (List.rev ixs));
            ("callProperties", array (List.rev calls));
            ("internalSlots", array (List.rev slots));
          ]
        in
        let fields =
          if include_inexact then
            ("inexact", bool inexact) :: fields
          else
            fields
        in
        node ?comments "ObjectTypeAnnotation" loc fields)
    and object_type_property
        ( loc,
          {
            Type.Object.Property.key;
            value;
            optional;
            static;
            proto;
            variance = variance_;
            _method;
            comments;
          } ) =
      let key =
        match key with
        | Expression.Object.Property.Literal lit -> literal lit
        | Expression.Object.Property.Identifier id -> identifier id
        | Expression.Object.Property.PrivateName _ ->
          failwith "Internal Error: Found private field in object props"
        | Expression.Object.Property.Computed _ ->
          failwith "There should not be computed object type property keys"
      in
      let (value, kind) =
        match value with
        | Type.Object.Property.Init value -> (_type value, "init")
        | Type.Object.Property.Get (loc, f) -> (function_type (loc, f), "get")
        | Type.Object.Property.Set (loc, f) -> (function_type (loc, f), "set")
      in
      node
        ?comments
        "ObjectTypeProperty"
        loc
        [
          ("key", key);
          ("value", value);
          ("method", bool _method);
          ("optional", bool optional);
          ("static", bool static);
          ("proto", bool proto);
          ("variance", option variance variance_);
          ("kind", string kind);
        ]
    and object_type_spread_property (loc, { Type.Object.SpreadProperty.argument; comments }) =
      node ?comments "ObjectTypeSpreadProperty" loc [("argument", _type argument)]
    and object_type_indexer
        (loc, { Type.Object.Indexer.id; key; value; static; variance = variance_; comments }) =
      node
        ?comments
        "ObjectTypeIndexer"
        loc
        [
          ("id", option identifier id);
          ("key", _type key);
          ("value", _type value);
          ("static", bool static);
          ("variance", option variance variance_);
        ]
    and object_type_call_property (loc, { Type.Object.CallProperty.value; static; comments }) =
      node
        ?comments
        "ObjectTypeCallProperty"
        loc
        [("value", function_type value); ("static", bool static)]
    and object_type_internal_slot
        (loc, { Type.Object.InternalSlot.id; optional; static; _method; value; comments }) =
      node
        ?comments
        "ObjectTypeInternalSlot"
        loc
        [
          ("id", identifier id);
          ("optional", bool optional);
          ("static", bool static);
          ("method", bool _method);
          ("value", _type value);
        ]
    and interface_type (loc, { Type.Interface.extends; body; comments }) =
      node
        ?comments
        "InterfaceTypeAnnotation"
        loc
        [
          ("extends", array_of_list interface_extends extends);
          ("body", object_type ~include_inexact:false body);
        ]
    and array_type loc { Type.Array.argument; comments } =
      node ?comments "ArrayTypeAnnotation" loc [("elementType", _type argument)]
    and generic_type_qualified_identifier (loc, { Type.Generic.Identifier.id; qualification }) =
      let qualification =
        match qualification with
        | Type.Generic.Identifier.Unqualified id -> identifier id
        | Type.Generic.Identifier.Qualified q -> generic_type_qualified_identifier q
      in
      node "QualifiedTypeIdentifier" loc [("qualification", qualification); ("id", identifier id)]
    and generic_type (loc, { Type.Generic.id; targs; comments }) =
      let id =
        match id with
        | Type.Generic.Identifier.Unqualified id -> identifier id
        | Type.Generic.Identifier.Qualified q -> generic_type_qualified_identifier q
      in
      node
        ?comments
        "GenericTypeAnnotation"
        loc
        [("id", id); ("typeParameters", option type_args targs)]
    and union_type (loc, { Type.Union.types = (t0, t1, ts); comments }) =
      node ?comments "UnionTypeAnnotation" loc [("types", array_of_list _type (t0 :: t1 :: ts))]
    and intersection_type (loc, { Type.Intersection.types = (t0, t1, ts); comments }) =
      node
        ?comments
        "IntersectionTypeAnnotation"
        loc
        [("types", array_of_list _type (t0 :: t1 :: ts))]
    and typeof_type (loc, { Type.Typeof.argument; internal = _; comments }) =
      node ?comments "TypeofTypeAnnotation" loc [("argument", _type argument)]
    and tuple_type (loc, { Type.Tuple.types; comments }) =
      node ?comments "TupleTypeAnnotation" loc [("types", array_of_list _type types)]
    and string_literal_type (loc, { Ast.StringLiteral.value; raw; comments }) =
      node
        ?comments
        "StringLiteralTypeAnnotation"
        loc
        [("value", string value); ("raw", string raw)]
    and number_literal_type (loc, { Ast.NumberLiteral.value; raw; comments }) =
      node
        ?comments
        "NumberLiteralTypeAnnotation"
        loc
        [("value", number value); ("raw", string raw)]
    and bigint_literal_type (loc, { Ast.BigIntLiteral.bigint; comments; _ }) =
      let raw = bigint in
      node ?comments "BigIntLiteralTypeAnnotation" loc [("value", null); ("raw", string raw)]
    and boolean_literal_type (loc, { Ast.BooleanLiteral.value; comments }) =
      node
        ?comments
        "BooleanLiteralTypeAnnotation"
        loc
        [
          ("value", bool value);
          ( "raw",
            string
              ( if value then
                "true"
              else
                "false" ) );
        ]
    and exists_type loc comments = node ?comments "ExistsTypeAnnotation" loc []
    and type_annotation (loc, ty) = node "TypeAnnotation" loc [("typeAnnotation", _type ty)]
    and type_parameter_declaration (loc, { Type.TypeParams.params; comments }) =
      node ?comments "TypeParameterDeclaration" loc [("params", array_of_list type_param params)]
    and type_param
        ( loc,
          {
            Type.TypeParam.name = (_, { Identifier.name; comments });
            bound;
            variance = tp_var;
            default;
          } ) =
      node
        ?comments
        "TypeParameter"
        loc
        [
          (* we track the location of the name, but don't expose it here for
           backwards-compatibility. TODO: change this? *)
          ("name", string name);
          ("bound", hint type_annotation bound);
          ("variance", option variance tp_var);
          ("default", option _type default);
        ]
    and type_args (loc, { Type.TypeArgs.arguments; comments }) =
      node ?comments "TypeParameterInstantiation" loc [("params", array_of_list _type arguments)]
    and call_type_args (loc, targs) =
      node "TypeParameterInstantiation" loc [("params", array_of_list call_type_arg targs)]
    and call_type_arg x =
      match x with
      | Expression.CallTypeArg.Explicit t -> _type t
      | Expression.CallTypeArg.Implicit (loc, { Expression.CallTypeArg.Implicit.comments }) ->
        generic_type
          ( loc,
            {
              Type.Generic.id =
                Type.Generic.Identifier.Unqualified (Flow_ast_utils.ident_of_source (loc, "_"));
              targs = None;
              comments;
            } )
    and jsx_element
        (loc, { JSX.openingElement; closingElement; children = (_loc, children); comments }) =
      node
        ?comments
        "JSXElement"
        loc
        [
          ("openingElement", jsx_opening openingElement);
          ("closingElement", option jsx_closing closingElement);
          ("children", array_of_list jsx_child children);
        ]
    and jsx_fragment
        ( loc,
          {
            JSX.frag_openingElement;
            frag_closingElement;
            frag_children = (_loc, frag_children);
            frag_comments;
          } ) =
      node
        ?comments:frag_comments
        "JSXFragment"
        loc
        [
          ("openingFragment", jsx_opening_fragment frag_openingElement);
          ("children", array_of_list jsx_child frag_children);
          ("closingFragment", jsx_closing_fragment frag_closingElement);
        ]
    and jsx_opening (loc, { JSX.Opening.name; attributes; selfClosing }) =
      node
        "JSXOpeningElement"
        loc
        [
          ("name", jsx_name name);
          ("attributes", array_of_list jsx_opening_attribute attributes);
          ("selfClosing", bool selfClosing);
        ]
    and jsx_opening_fragment loc = node "JSXOpeningFragment" loc []
    and jsx_opening_attribute =
      JSX.Opening.(
        function
        | Attribute attribute -> jsx_attribute attribute
        | SpreadAttribute attribute -> jsx_spread_attribute attribute)
    and jsx_closing (loc, { JSX.Closing.name }) =
      node "JSXClosingElement" loc [("name", jsx_name name)]
    and jsx_closing_fragment loc = node "JSXClosingFragment" loc []
    and jsx_child =
      JSX.(
        function
        | (loc, Element element) -> jsx_element (loc, element)
        | (loc, Fragment fragment) -> jsx_fragment (loc, fragment)
        | (loc, ExpressionContainer expr) -> jsx_expression_container (loc, expr)
        | (loc, SpreadChild spread) -> jsx_spread_child (loc, spread)
        | (loc, Text str) -> jsx_text (loc, str))
    and jsx_name =
      JSX.(
        function
        | Identifier id -> jsx_identifier id
        | NamespacedName namespaced_name -> jsx_namespaced_name namespaced_name
        | MemberExpression member -> jsx_member_expression member)
    and jsx_attribute (loc, { JSX.Attribute.name; value }) =
      let name =
        match name with
        | JSX.Attribute.Identifier id -> jsx_identifier id
        | JSX.Attribute.NamespacedName namespaced_name -> jsx_namespaced_name namespaced_name
      in
      node "JSXAttribute" loc [("name", name); ("value", option jsx_attribute_value value)]
    and jsx_attribute_value =
      JSX.Attribute.(
        function
        | Literal (loc, value) -> literal (loc, value)
        | ExpressionContainer (loc, expr) -> jsx_expression_container (loc, expr))
    and jsx_spread_attribute (loc, { JSX.SpreadAttribute.argument; comments }) =
      node ?comments "JSXSpreadAttribute" loc [("argument", expression argument)]
    and jsx_expression_container (loc, { JSX.ExpressionContainer.expression = expr; comments }) =
      let expression =
        match expr with
        | JSX.ExpressionContainer.Expression expr -> expression expr
        | JSX.ExpressionContainer.EmptyExpression ->
          let empty_loc =
            Loc.
              {
                loc with
                start = { loc.start with column = loc.start.column + 1 };
                _end = { loc._end with column = loc._end.column - 1 };
              }
          in
          node ?comments "JSXEmptyExpression" empty_loc []
      in
      node ?comments "JSXExpressionContainer" loc [("expression", expression)]
    and jsx_spread_child (loc, { JSX.SpreadChild.expression = expr; comments }) =
      node ?comments "JSXSpreadChild" loc [("expression", expression expr)]
    and jsx_text (loc, { JSX.Text.value; raw }) =
      node "JSXText" loc [("value", string value); ("raw", string raw)]
    and jsx_member_expression (loc, { JSX.MemberExpression._object; property }) =
      let _object =
        match _object with
        | JSX.MemberExpression.Identifier id -> jsx_identifier id
        | JSX.MemberExpression.MemberExpression member -> jsx_member_expression member
      in
      node "JSXMemberExpression" loc [("object", _object); ("property", jsx_identifier property)]
    and jsx_namespaced_name (loc, { JSX.NamespacedName.namespace; name }) =
      node
        "JSXNamespacedName"
        loc
        [("namespace", jsx_identifier namespace); ("name", jsx_identifier name)]
    and jsx_identifier (loc, { JSX.Identifier.name; comments }) =
      node ?comments "JSXIdentifier" loc [("name", string name)]
    and export_specifier (loc, { Statement.ExportNamedDeclaration.ExportSpecifier.exported; local })
        =
      let exported =
        match exported with
        | Some exported -> identifier exported
        | None -> identifier local
      in
      node "ExportSpecifier" loc [("local", identifier local); ("exported", exported)]
    and import_default_specifier id =
      node "ImportDefaultSpecifier" (fst id) [("local", identifier id)]
    and import_namespace_specifier (loc, id) =
      node "ImportNamespaceSpecifier" loc [("local", identifier id)]
    and import_named_specifier local_id remote_id kind =
      let span_loc =
        match local_id with
        | Some local_id -> Loc.btwn (fst remote_id) (fst local_id)
        | None -> fst remote_id
      in
      let local_id =
        match local_id with
        | Some id -> id
        | None -> remote_id
      in
      node
        "ImportSpecifier"
        span_loc
        [
          ("imported", identifier remote_id);
          ("local", identifier local_id);
          ( "importKind",
            match kind with
            | Some Statement.ImportDeclaration.ImportType -> string "type"
            | Some Statement.ImportDeclaration.ImportTypeof -> string "typeof"
            | Some Statement.ImportDeclaration.ImportValue
            | None ->
              null );
        ]
    and comment_list comments = array_of_list comment comments
    and comment (loc, c) =
      Comment.(
        let (_type, value) =
          match c with
          | Line s -> ("Line", s)
          | Block s -> ("Block", s)
        in
        node _type loc [("value", string value)])
    and predicate (loc, { Ast.Type.Predicate.kind; comments }) =
      let open Ast.Type.Predicate in
      let (_type, value) =
        match kind with
        | Declared e -> ("DeclaredPredicate", [("value", expression e)])
        | Inferred -> ("InferredPredicate", [])
      in
      node ?comments _type loc value
    and call_node_properties { Expression.Call.callee; targs; arguments; comments = _ } =
      [
        ("callee", expression callee);
        ("typeArguments", option call_type_args targs);
        ("arguments", arg_list arguments);
      ]
    and member_node_properties { Expression.Member._object; property; comments = _ } =
      let (property, computed) =
        match property with
        | Expression.Member.PropertyIdentifier id -> (identifier id, false)
        | Expression.Member.PropertyPrivateName name -> (private_name name, false)
        | Expression.Member.PropertyExpression expr -> (expression expr, true)
      in
      [("object", expression _object); ("property", property); ("computed", bool computed)]
    in
    { program; expression }

  let program offset_table = (make_functions offset_table).program

  let expression offset_table = (make_functions offset_table).expression
end
