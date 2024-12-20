(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module type Config = sig
  val include_locs : bool
end

module Translate (Impl : Translator_intf.S) (Config : Config) : sig
  type t

  val program : Offset_utils.t option -> (Loc.t, Loc.t) Ast.Program.t -> t

  val expression : Offset_utils.t option -> (Loc.t, Loc.t) Ast.Expression.t -> t

  val errors : (Loc.t * Parse_error.t) list -> t
end
with type t = Impl.t = struct
  type t = Impl.t

  type functions = {
    program: (Loc.t, Loc.t) Ast.Program.t -> t;
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

  let format_internal_comments = function
    | None -> None
    | Some { Ast.Syntax.leading; trailing; internal } ->
      Flow_ast_utils.mk_comments_opt ~leading ~trailing:(internal @ trailing) ()

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
          ]
      )
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
        match comments with
        | Some c ->
          (match c with
          | { leading = _ :: _ as l; trailing = _ :: _ as t; _ } ->
            [("leadingComments", comment_list l); ("trailingComments", comment_list t)]
          | { leading = _ :: _ as l; trailing = []; _ } -> [("leadingComments", comment_list l)]
          | { leading = []; trailing = _ :: _ as t; _ } -> [("trailingComments", comment_list t)]
          | _ -> [])
        | None -> []
      in
      let prefix = locs @ comments @ [("type", string _type)] in
      obj (List.rev_append prefix props)
    and program (loc, { Ast.Program.statements; interpreter; comments; all_comments }) =
      let body = statement_list statements in
      let props = [("body", body); ("comments", comment_list all_comments)] in
      let props =
        match interpreter with
        | Some (loc, value) ->
          let directive = node "InterpreterDirective" loc [("value", string value)] in
          props @ [("interpreter", directive)]
        | None -> props
      in
      node ?comments "Program" loc props
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
        let alternate =
          match alternate with
          | None -> null
          | Some (_, { If.Alternate.body; comments = alternate_comments }) ->
            statement (Comment_attachment.statement_add_comments body alternate_comments)
        in
        node
          ?comments
          "IfStatement"
          loc
          [
            ("test", expression test); ("consequent", statement consequent); ("alternate", alternate);
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
      | (loc, Match { Match.arg; cases; comments }) ->
        node
          ?comments
          "MatchStatement"
          loc
          [("argument", expression arg); ("cases", array_of_list match_statement_case cases)]
      | (loc, Switch { Switch.discriminant; cases; comments; exhaustive_out = _ }) ->
        node
          ?comments
          "SwitchStatement"
          loc
          [("discriminant", expression discriminant); ("cases", array_of_list case cases)]
      | (loc, Return { Return.argument; comments; return_out = _ }) ->
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
      | (loc, For { For.init = init_; test; update; body; comments }) ->
        let init = function
          | For.InitDeclaration init -> variable_declaration init
          | For.InitExpression expr -> expression expr
        in
        node
          ?comments
          "ForStatement"
          loc
          [
            ("init", option init init_);
            ("test", option expression test);
            ("update", option expression update);
            ("body", statement body);
          ]
      | (loc, ForIn { ForIn.left; right; body; each; comments }) ->
        let left =
          match left with
          | ForIn.LeftDeclaration left -> variable_declaration left
          | ForIn.LeftPattern left -> pattern left
        in
        node
          ?comments
          "ForInStatement"
          loc
          [
            ("left", left);
            ("right", expression right);
            ("body", statement body);
            ("each", bool each);
          ]
      | (loc, ForOf { ForOf.await; left; right; body; comments }) ->
        let left =
          match left with
          | ForOf.LeftDeclaration left -> variable_declaration left
          | ForOf.LeftPattern left -> pattern left
        in
        node
          ?comments
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
      | (loc, ComponentDeclaration c) -> component_declaration (loc, c)
      | (loc, DeclareVariable d) -> declare_variable (loc, d)
      | (loc, DeclareFunction d) -> declare_function (loc, d)
      | (loc, DeclareClass d) -> declare_class (loc, d)
      | (loc, DeclareComponent d) -> declare_component (loc, d)
      | (loc, DeclareEnum enum) -> declare_enum (loc, enum)
      | (loc, DeclareInterface i) -> declare_interface (loc, i)
      | (loc, DeclareTypeAlias a) -> declare_type_alias (loc, a)
      | (loc, DeclareOpaqueType t) -> opaque_type ~declare:true (loc, t)
      | (loc, DeclareModule { DeclareModule.id; body; comments }) ->
        let id =
          match id with
          | DeclareModule.Literal lit -> string_literal lit
          | DeclareModule.Identifier id -> identifier id
        in
        node ?comments "DeclareModule" loc [("id", id); ("body", block body)]
      | (loc, DeclareNamespace { DeclareNamespace.id; body; comments }) ->
        let id = identifier id in
        node ?comments "DeclareNamespace" loc [("id", id); ("body", block body)]
      | ( loc,
          DeclareExportDeclaration
            { DeclareExportDeclaration.specifiers; declaration; default; source; comments }
        ) -> begin
        match specifiers with
        | Some (ExportNamedDeclaration.ExportBatchSpecifier (_, None)) ->
          node
            ?comments
            "DeclareExportAllDeclaration"
            loc
            [("source", option string_literal source)]
        | _ ->
          let declaration =
            match declaration with
            | Some (DeclareExportDeclaration.Variable v) -> declare_variable v
            | Some (DeclareExportDeclaration.Function f) -> declare_function f
            | Some (DeclareExportDeclaration.Class c) -> declare_class c
            | Some (DeclareExportDeclaration.Component c) -> declare_component c
            | Some (DeclareExportDeclaration.DefaultType t) -> _type t
            | Some (DeclareExportDeclaration.NamedType t) -> type_alias t
            | Some (DeclareExportDeclaration.NamedOpaqueType t) -> opaque_type ~declare:true t
            | Some (DeclareExportDeclaration.Interface i) -> interface_declaration i
            | Some (DeclareExportDeclaration.Enum enum) -> declare_enum enum
            | None -> null
          in
          node
            ?comments
            "DeclareExportDeclaration"
            loc
            [
              ( "default",
                bool
                  (match default with
                  | Some _ -> true
                  | None -> false)
              );
              ("declaration", declaration);
              ("specifiers", export_specifiers specifiers);
              ("source", option string_literal source);
            ]
      end
      | (loc, DeclareModuleExports { DeclareModuleExports.annot; comments }) ->
        node ?comments "DeclareModuleExports" loc [("typeAnnotation", type_annotation annot)]
      | ( loc,
          ExportNamedDeclaration
            { ExportNamedDeclaration.specifiers; declaration; source; export_kind; comments }
        ) -> begin
        match specifiers with
        | Some (ExportNamedDeclaration.ExportBatchSpecifier (_, exported)) ->
          node
            ?comments
            "ExportAllDeclaration"
            loc
            [
              ("source", option string_literal source);
              ("exported", option identifier exported);
              ("exportKind", string (string_of_export_kind export_kind));
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
              ("exportKind", string (string_of_export_kind export_kind));
            ]
      end
      | ( loc,
          ExportDefaultDeclaration
            {
              ExportDefaultDeclaration.declaration;
              default = _ (* TODO: confirm we shouldn't use this *);
              comments;
            }
        ) ->
        let declaration =
          match declaration with
          | ExportDefaultDeclaration.Declaration stmt -> statement stmt
          | ExportDefaultDeclaration.Expression expr -> expression expr
        in
        node
          ?comments
          "ExportDefaultDeclaration"
          loc
          [
            ("declaration", declaration);
            ("exportKind", string (string_of_export_kind Statement.ExportValue));
          ]
      | ( loc,
          ImportDeclaration { ImportDeclaration.specifiers; default; import_kind; source; comments }
        ) ->
        let specifiers =
          match specifiers with
          | Some (ImportDeclaration.ImportNamedSpecifiers specifiers) ->
            List.map
              (fun { ImportDeclaration.local; remote; remote_name_def_loc = _; kind } ->
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
          match import_kind with
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
          ?comments:(format_internal_comments comments)
          "ArrayExpression"
          loc
          [("elements", array_of_list array_element elements)]
      | (loc, Object { Object.properties; comments }) ->
        node
          ?comments:(format_internal_comments comments)
          "ObjectExpression"
          loc
          [("properties", array_of_list object_property properties)]
      | (loc, Function _function) -> function_expression (loc, _function)
      | ( loc,
          ArrowFunction
            {
              Function.params = (_, { Function.Params.comments = params_comments; _ }) as params;
              async;
              effect = _;
              predicate = predicate_;
              tparams;
              return;
              body;
              comments = func_comments;
              sig_loc = _;
              (* TODO: arrows shouldn't have these: *)
              id = _;
              generator = _;
            }
        ) ->
        let (body, expression) =
          match body with
          | Function.BodyBlock b -> (block b, false)
          | Function.BodyExpression expr -> (expression expr, true)
        in
        let comments =
          Flow_ast_utils.merge_comments
            ~outer:func_comments
            ~inner:(format_internal_comments params_comments)
        in
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
            ("returnType", function_return_type return);
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
              ])
        )
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
      | (loc, AsExpression { AsExpression.expression = expr; annot = (_, annot); comments }) ->
        node
          ?comments
          "AsExpression"
          loc
          [("expression", expression expr); ("typeAnnotation", _type annot)]
      | (loc, TSSatisfies { TSSatisfies.expression = expr; annot = (_, annot); comments }) ->
        node
          ?comments
          "SatisfiesExpression"
          loc
          [("expression", expression expr); ("typeAnnotation", _type annot)]
      | (loc, AsConstExpression { AsConstExpression.expression = expr; comments }) ->
        node ?comments "AsConstExpression" loc [("expression", expression expr)]
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
        let (arguments, comments) =
          match arguments with
          | Some ((_, { ArgList.comments = args_comments; _ }) as arguments) ->
            ( arg_list arguments,
              Flow_ast_utils.merge_comments
                ~inner:(format_internal_comments args_comments)
                ~outer:comments
            )
          | None -> (array [], comments)
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
      | ( loc,
          Call
            ({ Call.comments; arguments = (_, { ArgList.comments = args_comments; _ }); _ } as call)
        ) ->
        let comments =
          Flow_ast_utils.merge_comments
            ~inner:(format_internal_comments args_comments)
            ~outer:comments
        in
        node ?comments "CallExpression" loc (call_node_properties call)
      | ( loc,
          OptionalCall
            {
              OptionalCall.call =
                { Call.comments; arguments = (_, { ArgList.comments = args_comments; _ }); _ } as
                call;
              optional;
              filtered_out = _;
            }
        ) ->
        let comments =
          Flow_ast_utils.merge_comments
            ~inner:(format_internal_comments args_comments)
            ~outer:comments
        in
        node
          ?comments
          "OptionalCallExpression"
          loc
          (call_node_properties call @ [("optional", bool optional)])
      | (loc, Member ({ Member.comments; _ } as member)) ->
        node ?comments "MemberExpression" loc (member_node_properties member)
      | ( loc,
          OptionalMember
            { OptionalMember.member = { Member.comments; _ } as member; optional; filtered_out = _ }
        ) ->
        node
          ?comments
          "OptionalMemberExpression"
          loc
          (member_node_properties member @ [("optional", bool optional)])
      | (loc, Yield { Yield.argument; delegate; comments; result_out = _ }) ->
        node
          ?comments
          "YieldExpression"
          loc
          [("argument", option expression argument); ("delegate", bool delegate)]
      | (_loc, Identifier id) -> identifier id
      | (loc, StringLiteral lit) -> string_literal (loc, lit)
      | (loc, BooleanLiteral lit) -> boolean_literal (loc, lit)
      | (loc, NullLiteral lit) -> null_literal (loc, lit)
      | (loc, NumberLiteral lit) -> number_literal (loc, lit)
      | (loc, BigIntLiteral lit) -> bigint_literal (loc, lit)
      | (loc, RegExpLiteral lit) -> regexp_literal (loc, lit)
      | (loc, ModuleRefLiteral lit) -> module_ref_literal (loc, lit)
      | (loc, TemplateLiteral lit) -> template_literal (loc, lit)
      | (loc, TaggedTemplate tagged) -> tagged_template (loc, tagged)
      | (loc, Class c) -> class_expression (loc, c)
      | (loc, JSXElement element) -> jsx_element (loc, element)
      | (loc, JSXFragment fragment) -> jsx_fragment (loc, fragment)
      | (loc, Match { Match.arg; cases; comments; arg_internal = _; match_keyword_loc = _ }) ->
        node
          ?comments
          "MatchExpression"
          loc
          [("argument", expression arg); ("cases", array_of_list match_expression_case cases)]
      | (loc, MetaProperty { MetaProperty.meta; property; comments }) ->
        node
          ?comments
          "MetaProperty"
          loc
          [("meta", identifier meta); ("property", identifier property)]
      | (loc, Import { Import.argument; comments }) ->
        node ?comments "ImportExpression" loc [("source", expression argument)]
    and match_expression_case (loc, { Expression.Match.Case.pattern; body; guard; comments }) =
      node
        ?comments
        "MatchExpressionCase"
        loc
        [
          ("pattern", match_pattern pattern);
          ("body", expression body);
          ("guard", option expression guard);
        ]
    and match_statement_case (loc, { Statement.Match.Case.pattern; body; guard; comments }) =
      node
        ?comments
        "MatchStatementCase"
        loc
        [
          ("pattern", match_pattern pattern);
          ("body", block body);
          ("guard", option expression guard);
        ]
    and match_pattern (loc, pattern) =
      let open MatchPattern in
      let literal x = node "MatchLiteralPattern" loc [("literal", x)] in
      match pattern with
      | WildcardPattern comments -> node ?comments "MatchWildcardPattern" loc []
      | StringPattern lit -> literal (string_literal (loc, lit))
      | BooleanPattern lit -> literal (boolean_literal (loc, lit))
      | NullPattern comments -> literal (null_literal (loc, comments))
      | NumberPattern lit -> literal (number_literal (loc, lit))
      | BigIntPattern lit -> literal (bigint_literal (loc, lit))
      | UnaryPattern { UnaryPattern.operator; argument; comments } ->
        let operator =
          match operator with
          | UnaryPattern.Minus -> "-"
          | UnaryPattern.Plus -> "+"
        in
        let argument =
          match argument with
          | (loc, UnaryPattern.NumberLiteral lit) -> number_literal (loc, lit)
          | (loc, UnaryPattern.BigIntLiteral lit) -> bigint_literal (loc, lit)
        in
        node
          ?comments
          "MatchUnaryPattern"
          loc
          [("operator", string operator); ("argument", argument)]
      | BindingPattern binding -> match_binding_pattern (loc, binding)
      | IdentifierPattern id -> match_identifier_pattern id
      | MemberPattern mem ->
        let rec member (loc, { MemberPattern.base; property; comments }) =
          let member_base = function
            | MemberPattern.BaseIdentifier id -> match_identifier_pattern id
            | MemberPattern.BaseMember mem -> member mem
          in
          let member_property = function
            | MemberPattern.PropertyString lit -> string_literal lit
            | MemberPattern.PropertyNumber lit -> number_literal lit
            | MemberPattern.PropertyIdentifier id -> identifier id
          in
          node
            ?comments
            "MatchMemberPattern"
            loc
            [("base", member_base base); ("property", member_property property)]
        in
        member mem
      | ObjectPattern { ObjectPattern.properties; rest; comments } ->
        let property_key key =
          match key with
          | ObjectPattern.Property.StringLiteral lit -> string_literal lit
          | ObjectPattern.Property.NumberLiteral lit -> number_literal lit
          | ObjectPattern.Property.Identifier id -> identifier id
        in
        let property (loc, { ObjectPattern.Property.key; pattern; shorthand; comments }) =
          node
            ?comments
            "MatchObjectPatternProperty"
            loc
            [
              ("key", property_key key);
              ("pattern", match_pattern pattern);
              ("shorthand", bool shorthand);
            ]
        in
        node
          ?comments:(format_internal_comments comments)
          "MatchObjectPattern"
          loc
          [
            ("properties", array_of_list property properties);
            ("rest", option match_rest_pattern rest);
          ]
      | ArrayPattern { ArrayPattern.elements; rest; comments } ->
        node
          ?comments:(format_internal_comments comments)
          "MatchArrayPattern"
          loc
          [
            ( "elements",
              array_of_list
                (fun { ArrayPattern.Element.pattern; _ } -> match_pattern pattern)
                elements
            );
            ("rest", option match_rest_pattern rest);
          ]
      | OrPattern { OrPattern.patterns; comments } ->
        node ?comments "MatchOrPattern" loc [("patterns", array_of_list match_pattern patterns)]
      | AsPattern { AsPattern.pattern; target; comments } ->
        let target =
          match target with
          | AsPattern.Binding (loc, binding) -> match_binding_pattern (loc, binding)
          | AsPattern.Identifier id -> identifier id
        in
        node ?comments "MatchAsPattern" loc [("pattern", match_pattern pattern); ("target", target)]
    and match_identifier_pattern id =
      let (loc, _) = id in
      node "MatchIdentifierPattern" loc [("id", identifier id)]
    and match_binding_pattern (loc, { MatchPattern.BindingPattern.kind; id; comments }) =
      let kind = Flow_ast_utils.string_of_variable_kind kind in
      node ?comments "MatchBindingPattern" loc [("id", identifier id); ("kind", string kind)]
    and match_rest_pattern (loc, { MatchPattern.RestPattern.argument; comments }) =
      node ?comments "MatchRestPattern" loc [("argument", option match_binding_pattern argument)]
    and function_declaration
        ( loc,
          {
            Function.id;
            params = (_, { Function.Params.comments = params_comments; _ }) as params;
            async;
            generator;
            effect;
            predicate = predicate_;
            tparams;
            return;
            body;
            comments = func_comments;
            sig_loc = _;
          }
        ) =
      let body =
        match body with
        | Function.BodyBlock b -> b
        | Function.BodyExpression _ -> failwith "Unexpected FunctionDeclaration with BodyExpression"
      in
      let comments =
        Flow_ast_utils.merge_comments
          ~outer:func_comments
          ~inner:(format_internal_comments params_comments)
      in
      let (node_name, nonhook_attrs) =
        if effect = Function.Hook then
          ("HookDeclaration", [])
        else
          ( "FunctionDeclaration",
            [
              ("async", bool async);
              ("generator", bool generator);
              ("predicate", option predicate predicate_);
              ("expression", bool false);
            ]
          )
      in
      node
        ?comments
        node_name
        loc
        ([
           (* estree hasn't come around to the idea that function decls can have
              optional ids, but acorn, babel, espree and esprima all have, so let's
              do it too. see https://github.com/estree/estree/issues/98 *)
           ("id", option identifier id);
           ("params", function_params params);
           ("body", block body);
           ("returnType", function_return_type return);
           ("typeParameters", option type_parameter_declaration tparams);
         ]
        @ nonhook_attrs
        )
    and function_expression
        ( loc,
          {
            Function.id;
            params = (_, { Function.Params.comments = params_comments; _ }) as params;
            async;
            generator;
            effect = _;
            predicate = predicate_;
            tparams;
            return;
            body;
            comments = func_comments;
            sig_loc = _;
          }
        ) =
      let body =
        match body with
        | Function.BodyBlock b -> b
        | Function.BodyExpression _ -> failwith "Unexpected FunctionExpression with BodyExpression"
      in
      let comments =
        Flow_ast_utils.merge_comments
          ~outer:func_comments
          ~inner:(format_internal_comments params_comments)
      in
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
          ("returnType", function_return_type return);
          ("typeParameters", option type_parameter_declaration tparams);
        ]
    and identifier (loc, { Identifier.name; comments }) =
      node
        "Identifier"
        ?comments
        loc
        [("name", string name); ("typeAnnotation", null); ("optional", bool false)]
    and private_identifier (loc, { PrivateName.name; comments }) =
      node
        ?comments
        "PrivateIdentifier"
        loc
        [("name", string name); ("typeAnnotation", null); ("optional", bool false)]
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
    and arg_list (_loc, { Expression.ArgList.arguments; comments = _ }) =
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
      node
        ?comments:(format_internal_comments comments)
        "BlockStatement"
        loc
        [("body", statement_list body)]
    and declare_variable (loc, { Statement.DeclareVariable.id; annot; kind; comments }) =
      let id_loc = Loc.btwn (fst id) (fst annot) in
      let kind = Flow_ast_utils.string_of_variable_kind kind in
      node
        ?comments
        "DeclareVariable"
        loc
        [
          ( "id",
            pattern_identifier
              id_loc
              { Pattern.Identifier.name = id; annot = Ast.Type.Available annot; optional = false }
          );
          ("kind", string kind);
        ]
    and declare_function
        (loc, { Statement.DeclareFunction.id; annot; predicate = predicate_; comments }) =
      let id_loc = Loc.btwn (fst id) (fst annot) in
      let (name, predicate) =
        match annot with
        | (_, (_, Type.Function { Type.Function.effect = Function.Hook; _ })) -> ("DeclareHook", [])
        | _ -> ("DeclareFunction", [("predicate", option predicate predicate_)])
      in
      node
        ?comments
        name
        loc
        ([
           ( "id",
             pattern_identifier
               id_loc
               { Pattern.Identifier.name = id; annot = Ast.Type.Available annot; optional = false }
           );
         ]
        @ predicate
        )
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
    and declare_component (loc, component) =
      let {
        Statement.DeclareComponent.id;
        tparams;
        params = (_, { Type.Component.Params.comments = params_comments; _ }) as params;
        renders;
        comments = component_comments;
      } =
        component
      in
      let comments =
        Flow_ast_utils.merge_comments
          ~outer:component_comments
          ~inner:(format_internal_comments params_comments)
      in
      let (_, { Type.Component.Params.params = param_list; rest; comments = _ }) = params in
      node
        ?comments
        "DeclareComponent"
        loc
        [
          ("id", identifier id);
          ("params", component_type_params param_list);
          ("rest", option component_type_rest_param rest);
          ("params", component_type_params param_list);
          ("rendersType", renders_annotation renders);
          ("typeParameters", option type_parameter_declaration tparams);
        ]
    and component_type (loc, component) =
      let {
        Type.Component.tparams;
        params = (_, { Type.Component.Params.comments = params_comments; _ }) as params;
        renders;
        comments = component_comments;
      } =
        component
      in
      let comments =
        Flow_ast_utils.merge_comments
          ~outer:component_comments
          ~inner:(format_internal_comments params_comments)
      in
      let (_, { Type.Component.Params.params = param_list; rest; comments = _ }) = params in
      node
        ?comments
        "ComponentTypeAnnotation"
        loc
        [
          ("params", component_type_params param_list);
          ("rest", option component_type_rest_param rest);
          ("rendersType", renders_annotation renders);
          ("typeParameters", option type_parameter_declaration tparams);
        ]
    and component_type_params params =
      let open Type.Component in
      let params =
        List.map
          (fun (loc, { Param.name; annot; optional }) ->
            let (_, annot') = annot in
            component_type_param ~optional loc (Some name) annot')
          params
      in
      array params
    and component_type_rest_param rest =
      let open Type.Component in
      let (loc, { RestParam.argument; annot; optional; comments }) = rest in
      component_type_param
        ?comments
        ~optional
        loc
        (Option.map (fun i -> Statement.ComponentDeclaration.Param.Identifier i) argument)
        annot
    and component_type_param ?comments ~optional loc name annot =
      let name' =
        match name with
        | Some (Statement.ComponentDeclaration.Param.Identifier id) -> option identifier (Some id)
        | Some (Statement.ComponentDeclaration.Param.StringLiteral id) ->
          option string_literal (Some id)
        | None -> option identifier None
      in
      node
        ?comments
        "ComponentTypeParameter"
        loc
        [("name", name'); ("typeAnnotation", _type annot); ("optional", bool optional)]
    and declare_enum (loc, { Statement.EnumDeclaration.id; body; comments }) =
      node ?comments "DeclareEnum" loc [("id", identifier id); ("body", enum_body body)]
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
    and string_of_export_kind = function
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
        node_type (loc, { Class.id; extends; body; tparams; implements; class_decorators; comments })
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
            Flow_ast_utils.merge_comments ~outer:comments ~inner:implements_comments
          )
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
          ("decorators", array_of_list class_decorator class_decorators);
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
        | Property p -> class_property p
      )
    and class_method (loc, { Class.Method.key; value; kind; static; decorators; comments }) =
      let (key, computed, comments) =
        let open Expression.Object.Property in
        match key with
        | StringLiteral lit -> (string_literal lit, false, comments)
        | NumberLiteral lit -> (number_literal lit, false, comments)
        | BigIntLiteral lit -> (bigint_literal lit, false, comments)
        | Identifier id -> (identifier id, false, comments)
        | PrivateName name -> (private_identifier name, false, comments)
        | Computed (_, { ComputedKey.expression = expr; comments = computed_comments }) ->
          ( expression expr,
            true,
            Flow_ast_utils.merge_comments ~outer:comments ~inner:computed_comments
          )
      in
      let kind =
        Class.Method.(
          match kind with
          | Constructor -> "constructor"
          | Method -> "method"
          | Get -> "get"
          | Set -> "set"
        )
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
            Class.PrivateField.key;
            value;
            annot;
            static;
            variance = variance_;
            decorators;
            comments;
          }
        ) =
      let (value, declare) =
        match value with
        | Class.Property.Declared -> (None, true)
        | Class.Property.Uninitialized -> (None, false)
        | Class.Property.Initialized x -> (Some x, false)
      in
      let props =
        [
          ("key", private_identifier key);
          ("value", option expression value);
          ("typeAnnotation", hint type_annotation annot);
          ("computed", bool false);
          ("static", bool static);
          ("variance", option variance variance_);
        ]
        @ ( if decorators = [] then
            []
          else
            [("decorators", array_of_list class_decorator decorators)]
          )
        @
        if declare then
          [("declare", bool declare)]
        else
          []
      in
      node ?comments "PropertyDefinition" loc props
    and class_property
        ( loc,
          { Class.Property.key; value; annot; static; variance = variance_; decorators; comments }
        ) =
      let (key, computed, comments) =
        match key with
        | Expression.Object.Property.StringLiteral lit -> (string_literal lit, false, comments)
        | Expression.Object.Property.NumberLiteral lit -> (number_literal lit, false, comments)
        | Expression.Object.Property.BigIntLiteral lit -> (bigint_literal lit, false, comments)
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
        @ ( if decorators = [] then
            []
          else
            [("decorators", array_of_list class_decorator decorators)]
          )
        @
        if declare then
          [("declare", bool declare)]
        else
          []
      in
      node ?comments "PropertyDefinition" loc props
    and component_declaration (loc, component) =
      let open Statement.ComponentDeclaration in
      let {
        id;
        tparams;
        params = (_, { Params.comments = params_comments; _ }) as params;
        body;
        renders;
        comments = component_comments;
        sig_loc = _;
      } =
        component
      in
      let comments =
        Flow_ast_utils.merge_comments
          ~outer:component_comments
          ~inner:(format_internal_comments params_comments)
      in
      node
        ?comments
        "ComponentDeclaration"
        loc
        [
          ("body", block body);
          ("id", identifier id);
          ("params", component_params params);
          ("rendersType", renders_annotation renders);
          ("typeParameters", option type_parameter_declaration tparams);
        ]
    and component_params =
      let open Statement.ComponentDeclaration.Params in
      function
      | ( _,
          {
            params;
            rest = Some (rest_loc, { Statement.ComponentDeclaration.RestParam.argument; comments });
            comments = _;
          }
        ) ->
        let rest = node ?comments "RestElement" rest_loc [("argument", pattern argument)] in
        let rev_params = List.rev_map component_param params in
        let params = List.rev (rest :: rev_params) in
        array params
      | (_, { params; rest = None; comments = _ }) ->
        let params = List.map component_param params in
        array params
    and component_param param =
      let open Statement.ComponentDeclaration.Param in
      let (loc, { name; local; default; shorthand }) = param in
      let name' =
        match name with
        | Identifier id -> identifier id
        | StringLiteral id -> string_literal id
      in
      let local' =
        match default with
        | Some default ->
          node "AssignmentPattern" loc [("left", pattern local); ("right", expression default)]
        | None -> pattern local
      in
      node
        "ComponentParameter"
        loc
        [("name", name'); ("local", local'); ("shorthand", bool shorthand)]
    and enum_body body =
      let open Statement.EnumDeclaration in
      match body with
      | (loc, BooleanBody { BooleanBody.members; explicit_type; has_unknown_members; comments }) ->
        node
          ?comments:(format_internal_comments comments)
          "EnumBooleanBody"
          loc
          [
            ( "members",
              array_of_list
                (fun (loc, { InitializedMember.id; init }) ->
                  node
                    "EnumBooleanMember"
                    loc
                    [("id", identifier id); ("init", boolean_literal init)])
                members
            );
            ("explicitType", bool explicit_type);
            ("hasUnknownMembers", bool has_unknown_members);
          ]
      | (loc, NumberBody { NumberBody.members; explicit_type; has_unknown_members; comments }) ->
        node
          ?comments:(format_internal_comments comments)
          "EnumNumberBody"
          loc
          [
            ( "members",
              array_of_list
                (fun (loc, { InitializedMember.id; init }) ->
                  node "EnumNumberMember" loc [("id", identifier id); ("init", number_literal init)])
                members
            );
            ("explicitType", bool explicit_type);
            ("hasUnknownMembers", bool has_unknown_members);
          ]
      | (loc, StringBody { StringBody.members; explicit_type; has_unknown_members; comments }) ->
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
          ?comments:(format_internal_comments comments)
          "EnumStringBody"
          loc
          [
            ("members", array members);
            ("explicitType", bool explicit_type);
            ("hasUnknownMembers", bool has_unknown_members);
          ]
      | (loc, SymbolBody { SymbolBody.members; has_unknown_members; comments }) ->
        node
          ?comments:(format_internal_comments comments)
          "EnumSymbolBody"
          loc
          [
            ( "members",
              array_of_list
                (fun (loc, { DefaultedMember.id }) ->
                  node "EnumDefaultedMember" loc [("id", identifier id)])
                members
            );
            ("hasUnknownMembers", bool has_unknown_members);
          ]
      | (loc, BigIntBody { BigIntBody.members; explicit_type; has_unknown_members; comments }) ->
        node
          ?comments:(format_internal_comments comments)
          "EnumBigIntBody"
          loc
          [
            ( "members",
              array_of_list
                (fun (loc, { InitializedMember.id; init }) ->
                  node "EnumBigIntMember" loc [("id", identifier id); ("init", bigint_literal init)])
                members
            );
            ("explicitType", bool explicit_type);
            ("hasUnknownMembers", bool has_unknown_members);
          ]
    and enum_declaration (loc, { Statement.EnumDeclaration.id; body; comments }) =
      node ?comments "EnumDeclaration" loc [("id", identifier id); ("body", enum_body body)]
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
        | (loc, Object { Object.properties; annot; comments }) ->
          node
            ?comments:(format_internal_comments comments)
            "ObjectPattern"
            loc
            [
              ("properties", array_of_list object_pattern_property properties);
              ("typeAnnotation", hint type_annotation annot);
            ]
        | (loc, Array { Array.elements; annot; comments }) ->
          node
            ?comments:(format_internal_comments comments)
            "ArrayPattern"
            loc
            [
              ("elements", array_of_list array_pattern_element elements);
              ("typeAnnotation", hint type_annotation annot);
            ]
        | (loc, Identifier pattern_id) -> pattern_identifier loc pattern_id
        | (_loc, Expression expr) -> expression expr
      )
    and function_param (loc, { Ast.Function.Param.argument; default }) =
      match default with
      | Some default ->
        node "AssignmentPattern" loc [("left", pattern argument); ("right", expression default)]
      | None -> pattern argument
    and this_param (loc, { Function.ThisParam.annot; comments }) =
      node
        ?comments
        "Identifier"
        loc
        [("name", string "this"); ("typeAnnotation", type_annotation annot)]
    and function_params =
      let open Ast.Function.Params in
      function
      | ( _,
          {
            params;
            rest = Some (rest_loc, { Function.RestParam.argument; comments });
            comments = _;
            this_;
          }
        ) ->
        let rest = node ?comments "RestElement" rest_loc [("argument", pattern argument)] in
        let rev_params = List.rev_map function_param params in
        let params = List.rev (rest :: rev_params) in
        let params =
          match this_ with
          | Some this -> this_param this :: params
          | None -> params
        in
        array params
      | (_, { params; rest = None; this_; comments = _ }) ->
        let params = List.map function_param params in
        let params =
          match this_ with
          | Some this -> this_param this :: params
          | None -> params
        in
        array params
    and rest_element loc { Pattern.RestElement.argument; comments } =
      node ?comments "RestElement" loc [("argument", pattern argument)]
    and array_pattern_element =
      let open Pattern.Array in
      function
      | Hole _ -> null
      | Element (loc, { Element.argument; default = Some default }) ->
        node "AssignmentPattern" loc [("left", pattern argument); ("right", expression default)]
      | Element (_loc, { Element.argument; default = None }) -> pattern argument
      | RestElement (loc, el) -> rest_element loc el
    and function_return_type = function
      | Ast.Function.ReturnAnnot.Missing _ -> null
      | Ast.Function.ReturnAnnot.TypeGuard (loc, g) -> type_guard_annotation (loc, g)
      | Ast.Function.ReturnAnnot.Available t -> type_annotation t
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
            | StringLiteral lit -> (string_literal lit, false, comments)
            | NumberLiteral lit -> (number_literal lit, false, comments)
            | BigIntLiteral lit -> (bigint_literal lit, false, comments)
            | Identifier id -> (identifier id, false, comments)
            | PrivateName _ -> failwith "Internal Error: Found private field in object props"
            | Computed (_, { ComputedKey.expression = expr; comments = key_comments }) ->
              ( expression expr,
                true,
                Flow_ast_utils.merge_comments ~outer:comments ~inner:key_comments
              )
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
            ]
        )
      | SpreadProperty (loc, { SpreadProperty.argument; comments }) ->
        node ?comments "SpreadElement" loc [("argument", expression argument)]
    and object_pattern_property =
      let open Pattern.Object in
      function
      | Property (loc, { Property.key; pattern = patt; default; shorthand }) ->
        let (key, computed, comments) =
          match key with
          | Property.StringLiteral lit -> (string_literal lit, false, None)
          | Property.NumberLiteral lit -> (number_literal lit, false, None)
          | Property.BigIntLiteral lit -> (bigint_literal lit, false, None)
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
    and spread_element (loc, { Expression.SpreadElement.argument; comments }) =
      node ?comments "SpreadElement" loc [("argument", expression argument)]
    and expression_or_spread =
      let open Expression in
      function
      | Expression expr -> expression expr
      | Spread spread -> spread_element spread
    and array_element =
      let open Expression.Array in
      function
      | Hole _ -> null
      | Expression expr -> expression expr
      | Spread spread -> spread_element spread
    and number_literal (loc, { NumberLiteral.value; raw; comments }) =
      node ?comments "Literal" loc [("value", number value); ("raw", string raw)]
    and bigint_literal (loc, { BigIntLiteral.value; raw; comments }) =
      (* https://github.com/estree/estree/blob/master/es2020.md#bigintliteral
       * `bigint` property is the string representation of the `BigInt` value.
       * It must contain only decimal digits and not include numeric separators `_` or the suffix `n`.
       *)
      let bigint =
        match value with
        | Some value -> Int64.to_string value
        | None ->
          String.sub raw 0 (String.length raw - 1) |> String.split_on_char '_' |> String.concat ""
      in
      node ?comments "Literal" loc [("value", null); ("bigint", string bigint); ("raw", string raw)]
    and string_literal (loc, { StringLiteral.value; raw; comments }) =
      node ?comments "Literal" loc [("value", string value); ("raw", string raw)]
    and boolean_literal (loc, { BooleanLiteral.value; comments }) =
      let raw =
        if value then
          "true"
        else
          "false"
      in
      node ?comments "Literal" loc [("value", bool value); ("raw", string raw)]
    and regexp_literal (loc, { RegExpLiteral.pattern; flags; raw; comments; _ }) =
      let value = regexp loc pattern flags in
      let regex = obj [("pattern", string pattern); ("flags", string flags)] in
      node ?comments "Literal" loc [("value", value); ("raw", string raw); ("regex", regex)]
    and null_literal (loc, comments) =
      node ?comments "Literal" loc [("value", null); ("raw", string "null")]
    and module_ref_literal (loc, { ModuleRefLiteral.value; raw; comments; _ }) =
      string_literal (loc, { StringLiteral.value; raw; comments })
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
          }
        ) =
      let value = obj [("raw", string raw); ("cooked", string cooked)] in
      node "TemplateElement" loc [("value", value); ("tail", bool tail)]
    and tagged_template (loc, { Expression.TaggedTemplate.tag; quasi; comments }) =
      node
        ?comments
        "TaggedTemplateExpression"
        loc
        [("tag", expression tag); ("quasi", template_literal quasi)]
    and variable_declaration (loc, { Statement.VariableDeclaration.kind; declarations; comments }) =
      let kind = Flow_ast_utils.string_of_variable_kind kind in
      node
        ?comments
        "VariableDeclaration"
        loc
        [("declarations", array_of_list variable_declarator declarations); ("kind", string kind)]
    and variable_declarator (loc, { Statement.VariableDeclaration.Declarator.id; init }) =
      node "VariableDeclarator" loc [("id", pattern id); ("init", option expression init)]
    and variance (loc, { Variance.kind; comments }) =
      let open Variance in
      let kind_str =
        match kind with
        | Plus -> "plus"
        | Minus -> "minus"
        | Readonly -> "readonly"
        | In -> "in"
        | Out -> "out"
        | InOut -> "in-out"
      in
      node ?comments "Variance" loc [("kind", string kind_str)]
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
        | Boolean { raw = _; comments } -> boolean_type loc comments
        | Nullable t -> nullable_type loc t
        | Function fn -> function_type (loc, fn)
        | Component c -> component_type (loc, c)
        | Object o -> object_type ~include_inexact:true (loc, o)
        | Interface i -> interface_type (loc, i)
        | Array t -> array_type loc t
        | Conditional t -> conditional_type loc t
        | Infer t -> infer_type loc t
        | Generic g -> generic_type (loc, g)
        | IndexedAccess ia -> indexed_access (loc, ia)
        | OptionalIndexedAccess ia -> optional_indexed_access (loc, ia)
        | Union t -> union_type (loc, t)
        | Intersection t -> intersection_type (loc, t)
        | Typeof t -> typeof_type (loc, t)
        | Keyof t -> keyof_type (loc, t)
        | Renders renders -> render_type loc renders
        | ReadOnly t -> read_only_type (loc, t)
        | Tuple t -> tuple_type (loc, t)
        | StringLiteral s -> string_literal_type (loc, s)
        | NumberLiteral n -> number_literal_type (loc, n)
        | BigIntLiteral n -> bigint_literal_type (loc, n)
        | BooleanLiteral b -> boolean_literal_type (loc, b)
        | Exists comments -> exists_type loc comments
        | Unknown comments -> unknown_type loc comments
        | Never comments -> never_type loc comments
        | Undefined comments -> undefined_type loc comments
      )
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
    and unknown_type loc comments = node ?comments "UnknownTypeAnnotation" loc []
    and never_type loc comments = node ?comments "NeverTypeAnnotation" loc []
    and undefined_type loc comments = node ?comments "UndefinedTypeAnnotation" loc []
    and return_annotation = function
      | Ast.Type.Function.TypeAnnotation t -> _type t
      | Ast.Type.Function.TypeGuard g -> type_guard g
    and type_guard (loc, { Ast.Type.TypeGuard.kind; guard = (x, t); comments }) =
      let kind =
        let open Ast.Type.TypeGuard in
        match kind with
        | Default -> null
        | Asserts -> string "asserts"
        | Implies -> string "implies"
      in
      node
        ?comments:(format_internal_comments comments)
        "TypePredicate"
        loc
        [("parameterName", identifier x); ("typeAnnotation", option _type t); ("kind", kind)]
    and function_type
        ( loc,
          {
            Type.Function.params =
              (_, { Type.Function.Params.this_; params; rest; comments = params_comments });
            return;
            tparams;
            effect;
            comments = func_comments;
          }
        ) =
      let comments =
        Flow_ast_utils.merge_comments
          ~inner:(format_internal_comments params_comments)
          ~outer:func_comments
      in
      let name =
        if effect = Function.Hook then
          "HookTypeAnnotation"
        else
          "FunctionTypeAnnotation"
      in
      node
        ?comments
        name
        loc
        ([
           ("params", array_of_list function_type_param params);
           ("returnType", return_annotation return);
           ("rest", option function_type_rest rest);
           ("typeParameters", option type_parameter_declaration tparams);
         ]
        @
        if effect = Function.Hook then
          []
        else
          [("this", option function_type_this_constraint this_)]
        )
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
    and function_type_this_constraint (loc, { Type.Function.ThisParam.annot = (_, annot); comments })
        =
      node
        ?comments
        "FunctionTypeParam"
        loc
        [
          ("name", option identifier None); ("typeAnnotation", _type annot); ("optional", bool false);
        ]
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
                (props, ixs, calls, slot :: slots)
              | MappedType m ->
                let mapped_type = object_type_mapped_type m in
                (mapped_type :: props, ixs, calls, slots))
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
        node ?comments:(format_internal_comments comments) "ObjectTypeAnnotation" loc fields
      )
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
          }
        ) =
      let key =
        match key with
        | Expression.Object.Property.StringLiteral lit -> string_literal lit
        | Expression.Object.Property.NumberLiteral lit -> number_literal lit
        | Expression.Object.Property.BigIntLiteral lit -> bigint_literal lit
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
    and object_type_mapped_type
        ( mt_loc,
          {
            Type.Object.MappedType.key_tparam;
            prop_type;
            source_type;
            variance = variance_;
            comments;
            optional;
          }
        ) =
      let optional_flag flag =
        Type.Object.MappedType.(
          match flag with
          | PlusOptional -> string "PlusOptional"
          | MinusOptional -> string "MinusOptional"
          | Optional -> string "Optional"
          | NoOptionalFlag -> null
        )
      in
      node
        ?comments
        "ObjectTypeMappedTypeProperty"
        mt_loc
        [
          ("keyTparam", type_param key_tparam);
          ("propType", _type prop_type);
          ("sourceType", _type source_type);
          ("variance", option variance variance_);
          ("optional", optional_flag optional);
        ]
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
    and conditional_type
        loc { Type.Conditional.check_type; extends_type; true_type; false_type; comments } =
      node
        ?comments
        "ConditionalTypeAnnotation"
        loc
        [
          ("checkType", _type check_type);
          ("extendsType", _type extends_type);
          ("trueType", _type true_type);
          ("falseType", _type false_type);
        ]
    and infer_type loc { Type.Infer.tparam; comments } =
      node ?comments "InferTypeAnnotation" loc [("typeParameter", type_param tparam)]
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
    and indexed_access_properties { Type.IndexedAccess._object; index; comments = _ } =
      [("objectType", _type _object); ("indexType", _type index)]
    and indexed_access (loc, ({ Type.IndexedAccess.comments; _ } as ia)) =
      node ?comments "IndexedAccessType" loc (indexed_access_properties ia)
    and optional_indexed_access
        ( loc,
          {
            Type.OptionalIndexedAccess.indexed_access =
              { Type.IndexedAccess.comments; _ } as indexed_access;
            optional;
          }
        ) =
      node
        ?comments
        "OptionalIndexedAccessType"
        loc
        (indexed_access_properties indexed_access @ [("optional", bool optional)])
    and union_type (loc, { Type.Union.types = (t0, t1, ts); comments }) =
      node ?comments "UnionTypeAnnotation" loc [("types", array_of_list _type (t0 :: t1 :: ts))]
    and intersection_type (loc, { Type.Intersection.types = (t0, t1, ts); comments }) =
      node
        ?comments
        "IntersectionTypeAnnotation"
        loc
        [("types", array_of_list _type (t0 :: t1 :: ts))]
    and typeof_type (loc, { Type.Typeof.argument; targs; comments }) =
      let targs_field =
        match targs with
        | None -> []
        | Some targs -> [("typeArguments", type_args targs)]
      in
      node ?comments "TypeofTypeAnnotation" loc (("argument", typeof_expr argument) :: targs_field)
    and typeof_expr id =
      match id with
      | Type.Typeof.Target.Unqualified id -> identifier id
      | Type.Typeof.Target.Qualified q -> typeof_qualifier q
    and typeof_qualifier (loc, { Type.Typeof.Target.id; qualification }) =
      let qualification = typeof_expr qualification in
      node "QualifiedTypeofIdentifier" loc [("qualification", qualification); ("id", identifier id)]
    and keyof_type (loc, { Type.Keyof.argument; comments }) =
      node ?comments "KeyofTypeAnnotation" loc [("argument", _type argument)]
    and renders_annotation = function
      | Ast.Type.AvailableRenders (loc, v) -> render_type loc v
      | Ast.Type.MissingRenders _ -> null
    and render_type loc { Type.Renders.operator_loc = _; comments; variant; argument } =
      let operator =
        match variant with
        | Type.Renders.Normal -> "renders"
        | Type.Renders.Maybe -> "renders?"
        | Type.Renders.Star -> "renders*"
      in
      flow_type_operator loc comments operator argument
    and flow_type_operator loc comments operator operand =
      node
        ?comments
        "TypeOperator"
        loc
        [("operator", string operator); ("typeAnnotation", _type operand)]
    and read_only_type (loc, { Type.ReadOnly.argument; comments }) =
      flow_type_operator loc comments "readonly" argument
    and tuple_type (loc, { Type.Tuple.elements; inexact; comments }) =
      node
        ?comments
        "TupleTypeAnnotation"
        loc
        [
          ( "elementTypes",
            array_of_list
              (function
                | (_, Type.Tuple.UnlabeledElement annot) -> _type annot
                | (loc, Type.Tuple.LabeledElement e) -> tuple_labeled_element loc e
                | (loc, Type.Tuple.SpreadElement e) -> tuple_spread_element loc e)
              elements
          );
          ("inexact", bool inexact);
        ]
    and tuple_labeled_element
        ?comments loc { Type.Tuple.LabeledElement.name; annot; variance = variance_; optional } =
      node
        ?comments
        "TupleTypeLabeledElement"
        loc
        [
          ("label", identifier name);
          ("elementType", _type annot);
          ("variance", option variance variance_);
          ("optional", bool optional);
        ]
    and tuple_spread_element ?comments loc { Type.Tuple.SpreadElement.name; annot } =
      node
        ?comments
        "TupleTypeSpreadElement"
        loc
        [("label", option identifier name); ("typeAnnotation", _type annot)]
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
    and bigint_literal_type (loc, { Ast.BigIntLiteral.raw; comments; _ }) =
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
                "false"
              )
          );
        ]
    and exists_type loc comments = node ?comments "ExistsTypeAnnotation" loc []
    and type_annotation (loc, ty) = node "TypeAnnotation" loc [("typeAnnotation", _type ty)]
    and type_guard_annotation (loc, (loc1, guard)) =
      node "TypeAnnotation" loc [("typeAnnotation", type_guard (loc1, guard))]
    and type_parameter_declaration (loc, { Type.TypeParams.params; comments }) =
      node
        ?comments:(format_internal_comments comments)
        "TypeParameterDeclaration"
        loc
        [("params", array_of_list type_param params)]
    and type_param
        ( loc,
          {
            Type.TypeParam.name = (_, { Identifier.name; comments });
            bound;
            bound_kind;
            variance = tp_var;
            default;
            const;
          }
        ) =
      node
        ?comments
        "TypeParameter"
        loc
        ([
           (* we track the location of the name, but don't expose it here for
              backwards-compatibility. TODO: change this? *)
           ("name", string name);
           ("bound", hint type_annotation bound);
           ("const", bool (Option.is_some const));
           ("variance", option variance tp_var);
           ("default", option _type default);
         ]
        @
        match bound_kind with
        | Type.TypeParam.Colon -> []
        | Type.TypeParam.Extends -> [("usesExtendsBound", bool true)]
        )
    and type_args (loc, { Type.TypeArgs.arguments; comments }) =
      node
        ?comments:(format_internal_comments comments)
        "TypeParameterInstantiation"
        loc
        [("params", array_of_list _type arguments)]
    and call_type_args (loc, { Expression.CallTypeArgs.arguments; comments }) =
      node
        ?comments:(format_internal_comments comments)
        "TypeParameterInstantiation"
        loc
        [("params", array_of_list call_type_arg arguments)]
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
            }
          )
    and jsx_element
        (loc, { JSX.opening_element; closing_element; children = (_loc, children); comments }) =
      node
        ?comments
        "JSXElement"
        loc
        [
          ("openingElement", jsx_opening opening_element);
          ("closingElement", option jsx_closing closing_element);
          ("children", array_of_list jsx_child children);
        ]
    and jsx_fragment
        ( loc,
          {
            JSX.frag_opening_element;
            frag_closing_element;
            frag_children = (_loc, frag_children);
            frag_comments;
          }
        ) =
      node
        ?comments:frag_comments
        "JSXFragment"
        loc
        [
          ("openingFragment", jsx_opening_fragment frag_opening_element);
          ("children", array_of_list jsx_child frag_children);
          ("closingFragment", jsx_closing_fragment frag_closing_element);
        ]
    and jsx_opening (loc, { JSX.Opening.name; targs; attributes; self_closing }) =
      node
        "JSXOpeningElement"
        loc
        ([
           ("name", jsx_name name);
           ("attributes", array_of_list jsx_opening_attribute attributes);
           ("selfClosing", bool self_closing);
         ]
        @
        match targs with
        | Some targs -> [("typeArguments", call_type_args targs)]
        | None -> []
        )
    and jsx_opening_fragment loc = node "JSXOpeningFragment" loc []
    and jsx_opening_attribute =
      JSX.Opening.(
        function
        | Attribute attribute -> jsx_attribute attribute
        | SpreadAttribute attribute -> jsx_spread_attribute attribute
      )
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
        | (loc, Text str) -> jsx_text (loc, str)
      )
    and jsx_name =
      JSX.(
        function
        | Identifier id -> jsx_identifier id
        | NamespacedName namespaced_name -> jsx_namespaced_name namespaced_name
        | MemberExpression member -> jsx_member_expression member
      )
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
        | StringLiteral (loc, value) -> string_literal (loc, value)
        | ExpressionContainer (loc, expr) -> jsx_expression_container (loc, expr)
      )
    and jsx_spread_attribute (loc, { JSX.SpreadAttribute.argument; comments }) =
      node ?comments "JSXSpreadAttribute" loc [("argument", expression argument)]
    and jsx_expression_container (loc, { JSX.ExpressionContainer.expression = expr; comments }) =
      let expression =
        match expr with
        | JSX.ExpressionContainer.Expression expr -> expression expr
        | JSX.ExpressionContainer.EmptyExpression ->
          let empty_loc =
            let open Loc in
            {
              loc with
              start = { loc.start with column = loc.start.column + 1 };
              _end = { loc._end with column = loc._end.column - 1 };
            }
          in

          node "JSXEmptyExpression" empty_loc []
      in
      node
        ?comments:(format_internal_comments comments)
        "JSXExpressionContainer"
        loc
        [("expression", expression)]
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
    and import_default_specifier
        { Statement.ImportDeclaration.identifier = id; remote_default_name_def_loc = _ } =
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
              null
          );
        ]
    and comment_list comments = array_of_list comment comments
    and comment (loc, c) =
      Comment.(
        let (_type, value) =
          match c with
          | { kind = Line; text = s; _ } -> ("Line", s)
          | { kind = Block; text = s; _ } -> ("Block", s)
        in
        node _type loc [("value", string value)]
      )
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
        | Expression.Member.PropertyPrivateName name -> (private_identifier name, false)
        | Expression.Member.PropertyExpression expr -> (expression expr, true)
      in
      [("object", expression _object); ("property", property); ("computed", bool computed)]
    in
    { program; expression }

  let program offset_table = (make_functions offset_table).program

  let expression offset_table = (make_functions offset_table).expression
end
