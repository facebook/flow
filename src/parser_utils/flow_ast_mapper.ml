(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let map_opt: 'node. ('node -> 'node) -> 'node option -> 'node option =
  fun map opt ->
    match opt with
    | Some item ->
      let item' = map item in
      if item == item' then opt else Some item'
    | None -> opt

let map_list: 'node. ('node -> 'node) -> 'node list -> 'node list =
  fun map list ->
    let rev_list, changed = List.fold_left (fun (list', changed) item ->
      let item' = map item in
      item'::list', changed || item' != item
    ) ([], false) list in
    if changed then List.rev rev_list else list

let id: 'node 'a. ('node -> 'node) -> 'node -> 'a -> ('node -> 'a) -> 'a =
  fun map item same diff ->
    let item' = map item in
    if item == item' then same else diff item'

class mapper = object(this)
  method program (program: Ast.program) =
    let (loc, statements, comments) = program in
    let statements' = this#statement_list statements in
    let comments' = map_list (this#comment) comments in
    if statements == statements' && comments == comments' then program
    else loc, statements', comments'

  method statement (stmt: Ast.Statement.t) =
    let open Ast.Statement in
    match stmt with
    | (loc, Block block) ->
      id this#block block stmt (fun block -> loc, Block block)

    | (loc, Break break) ->
      id this#break break stmt (fun break -> loc, Break break)

    | (loc, ClassDeclaration cls) ->
      id this#class_ cls stmt (fun cls -> loc, ClassDeclaration cls)

    | (loc, Continue cont) ->
      id this#continue cont stmt (fun cont -> loc, Continue cont)

    | (loc, DeclareExportDeclaration decl) ->
      id this#declare_export_declaration decl stmt (fun decl -> loc, DeclareExportDeclaration decl)

    | (loc, DoWhile stuff) ->
      id this#do_while stuff stmt (fun stuff -> loc, DoWhile stuff)

    | (_loc, Empty) ->
      stmt

    | (loc, ExportDefaultDeclaration decl) ->
      id this#export_default_declaration decl stmt (fun decl -> loc, ExportDefaultDeclaration decl)

    | (loc, ExportNamedDeclaration decl) ->
      id this#export_named_declaration decl stmt (fun decl -> loc, ExportNamedDeclaration decl)

    | (loc, Expression expr) ->
      id this#expression_statement expr stmt (fun expr -> loc, Expression expr)

    | (loc, For for_stmt) ->
      id this#for_statement for_stmt stmt (fun for_stmt -> loc, For for_stmt)

    | (loc, ForIn stuff) ->
      id this#for_in_statement stuff stmt (fun stuff -> loc, ForIn stuff)

    | (loc, ForOf stuff) ->
      id this#for_of_statement stuff stmt (fun stuff -> loc, ForOf stuff)

    | (loc, FunctionDeclaration func) ->
      id this#function_declaration func stmt (fun func -> loc, FunctionDeclaration func)

    | (loc, If if_stmt) ->
      id this#if_statement if_stmt stmt (fun if_stmt -> loc, If if_stmt)

    | (loc, ImportDeclaration decl) ->
      id this#import_declaration decl stmt (fun decl -> loc, ImportDeclaration decl)

    | (loc, Labeled label) ->
      id this#labeled_statement label stmt (fun label -> loc, Labeled label)

    | (loc, Return ret) ->
      id this#return ret stmt (fun ret -> loc, Return ret)

    | (loc, Switch switch) ->
      id this#switch switch stmt (fun switch -> loc, Switch switch)

    | (loc, Throw throw) ->
      id this#throw throw stmt (fun throw -> loc, Throw throw)

    | (loc, Try try_stmt) ->
      id this#try_catch try_stmt stmt (fun try_stmt -> loc, Try try_stmt)

    | (loc, VariableDeclaration decl) ->
      id this#variable_declaration decl stmt (fun decl -> loc, VariableDeclaration decl)

    | (loc, While stuff) ->
      id this#while_ stuff stmt (fun stuff -> loc, While stuff)

    | (loc, With stuff) ->
      id this#with_ stuff stmt (fun stuff -> loc, With stuff)

    (* TODO: ES6 or Flow specific stuff *)
    | (_loc, Debugger) -> stmt
    | (_loc, DeclareClass _) -> stmt
    | (_loc, DeclareFunction _) -> stmt
    | (_loc, DeclareModule _) -> stmt
    | (_loc, DeclareModuleExports _) -> stmt
    | (_loc, DeclareVariable _) -> stmt
    | (_loc, InterfaceDeclaration _) -> stmt
    | (_loc, TypeAlias _) -> stmt
    | (_loc, OpaqueType _) -> stmt

  method comment (c: Ast.Comment.t) = c

  method expression (expr: Ast.Expression.t) =
    let open Ast.Expression in
    match expr with
    | _, This -> expr
    | _, Super -> expr
    | loc, Array x -> id this#array x expr (fun x -> loc, Array x)
    | loc, ArrowFunction x -> id this#arrow_function x expr (fun x -> loc, ArrowFunction x)
    | loc, Assignment x -> id this#assignment x expr (fun x -> loc, Assignment x)
    | loc, Binary x -> id this#binary x expr (fun x -> loc, Binary x)
    | loc, Call x -> id this#call x expr (fun x -> loc, Call x)
    | loc, Class x -> id this#class_ x expr (fun x -> loc, Class x)
    | loc, Comprehension x -> id this#comprehension x expr (fun x -> loc, Comprehension x)
    | loc, Conditional x -> id this#conditional x expr (fun x -> loc, Conditional x)
    | loc, Function x -> id this#function_ x expr (fun x -> loc, Function x)
    | loc, Generator x -> id this#generator x expr (fun x -> loc, Generator x)
    | loc, Identifier x -> id this#identifier x expr (fun x -> loc, Identifier x)
    | loc, Import x -> id this#import x expr (fun x -> loc, Import x)
    | loc, JSXElement x -> id this#jsx_element x expr (fun x -> loc, JSXElement x)
    | loc, Literal x -> id this#literal x expr (fun x -> loc, Literal x)
    | loc, Logical x -> id this#logical x expr (fun x -> loc, Logical x)
    | loc, Member x -> id this#member x expr (fun x -> loc, Member x)
    | loc, MetaProperty x -> id this#meta_property x expr (fun x -> loc, MetaProperty x)
    | loc, New x -> id this#new_ x expr (fun x -> loc, New x)
    | loc, Object x -> id this#object_ x expr (fun x -> loc, Object x)
    | loc, Sequence x -> id this#sequence x expr (fun x -> loc, Sequence x)
    | loc, TaggedTemplate x -> id this#tagged_template x expr (fun x -> loc, TaggedTemplate x)
    | loc, TemplateLiteral x -> id this#template_literal x expr (fun x -> loc, TemplateLiteral x)
    | loc, TypeCast x -> id this#type_cast x expr (fun x -> loc, TypeCast x)
    | loc, Unary x -> id this#unary_expression x expr (fun x -> loc, Unary x)
    | loc, Update x -> id this#update_expression x expr (fun x -> loc, Update x)
    | loc, Yield x -> id this#yield x expr (fun x -> loc, Yield x)

  method array (expr: Ast.Expression.Array.t) =
    let open Ast.Expression in
    let { Array.elements } = expr in
    let elements' = map_list (map_opt this#expression_or_spread) elements in
    if elements == elements' then expr
    else { Array.elements = elements' }

  method arrow_function (expr: Ast.Function.t) =
    this#function_ expr

  method assignment (expr: Ast.Expression.Assignment.t) =
    let open Ast.Expression.Assignment in
    let { operator = _; left; right } = expr in
    let left' = this#assignment_pattern left in
    let right' = this#expression right in
    if left == left' && right == right' then expr
    else { expr with left = left'; right = right' }

  method binary (expr: Ast.Expression.Binary.t) =
    let open Ast.Expression.Binary in
    let { operator = _; left; right } = expr in
    let left' = this#expression left in
    let right' = this#expression right in
    if left == left' && right == right' then expr
    else { expr with left = left'; right = right' }

  method block (stmt: Ast.Statement.Block.t) =
    let open Ast.Statement.Block in
    let { body } = stmt in
    let body' = this#statement_list body in
    if body == body' then stmt else { body = body' }

  method break (break: Ast.Statement.Break.t) =
    let open Ast.Statement.Break in
    let { label } = break in
    let label' = map_opt this#identifier label in
    if label == label' then break else { label = label' }

  method call (expr: Ast.Expression.Call.t) =
    let open Ast.Expression.Call in
    let { callee; arguments } = expr in
    let callee' = this#expression callee in
    let arguments' = map_list this#expression_or_spread arguments in
    if callee == callee' && arguments == arguments' then expr
    else { callee = callee'; arguments = arguments' }

  method catch_clause (clause: Ast.Statement.Try.CatchClause.t') =
    let open Ast.Statement.Try.CatchClause in
    let { param; body } = clause in
    let param' = this#catch_clause_pattern param in
    let body' =
      let (body_loc, block) = body in
      id this#block block body (fun block -> body_loc, block)
    in
    if param == param' && body == body' then clause
    else { param = param'; body = body' }

  method class_ (cls: Ast.Class.t) =
    let open Ast.Class in
    let {
      id; body; superClass;
      typeParameters = _; superTypeParameters = _; implements = _; classDecorators = _;
    } = cls in
    let id' = map_opt this#identifier id in
    let body' = this#class_body body in
    let superClass' = map_opt this#expression superClass in
    if id == id' && body == body' && superClass' == superClass then cls
    else { cls with id = id'; body = body'; superClass = superClass' }

  method class_body (cls_body: Ast.Class.Body.t) =
    let open Ast.Class.Body in
    let loc, { body } = cls_body in
    let body' = map_list this#class_element body in
    if body == body' then cls_body
    else loc, { body = body' }

  method class_element (elem: Ast.Class.Body.element) =
    let open Ast.Class.Body in
    match elem with
    | Method (loc, meth) -> id this#class_method meth elem (fun meth -> Method (loc, meth))
    | Property (loc, prop) -> id this#class_property prop elem (fun prop -> Property (loc, prop))

  method class_method (meth: Ast.Class.Method.t') =
    let open Ast.Class.Method in
    let { kind = _; key; value; static = _; decorators = _; } = meth in
    let key' = this#object_key key in
    let value' =
      let loc, fn = value in
      id this#function_ fn value (fun fn -> loc, fn) in
    if key == key' && value == value' then meth
    else { meth with key = key'; value = value' }

  method class_property (prop: Ast.Class.Property.t') =
    let open Ast.Class.Property in
    let { key; value; typeAnnotation; static = _; variance = _; } = prop in
    let key' = this#object_key key in
    let value' = map_opt this#expression value in
    let typeAnnotation' = map_opt this#type_annotation typeAnnotation in
    if key == key' && value == value' && typeAnnotation' == typeAnnotation then prop
    else { prop with key = key'; value = value'; typeAnnotation = typeAnnotation' }

  (* TODO *)
  method comprehension (expr: Ast.Expression.Comprehension.t) = expr

  method conditional (expr: Ast.Expression.Conditional.t) =
    let open Ast.Expression.Conditional in
    let { test; consequent; alternate } = expr in
    let test' = this#expression test in
    let consequent' = this#expression consequent in
    let alternate' = this#expression alternate in
    if test == test' && consequent == consequent' && alternate == alternate'
    then expr
    else { test = test'; consequent = consequent'; alternate = alternate' }

  method continue (cont: Ast.Statement.Continue.t) =
    let open Ast.Statement.Continue in
    let { label } = cont in
    let label' = map_opt this#identifier label in
    if label == label' then cont else { label = label' }

  method declare_export_declaration (decl: Ast.Statement.DeclareExportDeclaration.t) =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default; source; specifiers; declaration } = decl in
    let specifiers' = map_opt this#export_named_specifier specifiers in
    let declaration' = map_opt this#declare_export_declaration_decl declaration in
    if specifiers == specifiers' && declaration == declaration' then decl
    else { default; source; specifiers = specifiers'; declaration = declaration' }

  (* TODO *)
  method declare_export_declaration_decl (decl: Ast.Statement.DeclareExportDeclaration.declaration) =
    decl

  method do_while (stuff: Ast.Statement.DoWhile.t) =
    let open Ast.Statement.DoWhile in
    let { body; test } = stuff in
    let body' = this#statement body in
    let test' = this#expression test in
    if body == body' && test == test' then stuff
    else { body = body'; test = test' }

  method export_default_declaration (decl: Ast.Statement.ExportDefaultDeclaration.t) =
    let open Ast.Statement.ExportDefaultDeclaration in
    let { exportKind; declaration } = decl in
    let declaration' = this#export_default_declaration_decl declaration in
    if declaration == declaration' then decl
    else { exportKind; declaration = declaration' }

  method export_default_declaration_decl (decl: Ast.Statement.ExportDefaultDeclaration.declaration) =
    let open Ast.Statement.ExportDefaultDeclaration in
    match decl with
    | Declaration stmt -> id this#statement stmt decl (fun stmt -> Declaration stmt)
    | Expression expr -> id this#expression expr decl (fun expr -> Expression expr)

  method export_named_declaration (decl: Ast.Statement.ExportNamedDeclaration.t) =
    let open Ast.Statement.ExportNamedDeclaration in
    let { exportKind; source; specifiers; declaration } = decl in
    let specifiers' = map_opt this#export_named_specifier specifiers in
    let declaration' = map_opt this#statement declaration in
    if specifiers == specifiers' && declaration == declaration' then decl
    else { exportKind; source; specifiers = specifiers'; declaration = declaration' }

  (* TODO *)
  method export_named_specifier (spec: Ast.Statement.ExportNamedDeclaration.specifier) =
    spec

  method expression_statement (stmt: Ast.Statement.Expression.t) =
    let open Ast.Statement.Expression in
    let { expression = expr; directive = _ } = stmt in
    id this#expression expr stmt (fun expression -> { stmt with expression })

  method expression_or_spread expr_or_spread =
    let open Ast.Expression in
    match expr_or_spread with
    | Expression expr ->
      id this#expression expr expr_or_spread (fun expr -> Expression expr)
    | Spread spread ->
      id this#spread_element spread expr_or_spread (fun spread -> Spread spread)

  method for_in_statement (stmt: Ast.Statement.ForIn.t) =
    let open Ast.Statement.ForIn in
    let { left; right; body; each } = stmt in
    let left' = this#for_in_statement_lhs left in
    let right' = this#expression right in
    let body' = this#statement body in
    if left == left' && right == right' && body == body' then stmt
    else { left = left'; right = right'; body = body'; each }

  method for_in_statement_lhs (left: Ast.Statement.ForIn.left) =
    let open Ast.Statement.ForIn in
    match left with
    | LeftDeclaration (loc, decl) ->
      id this#variable_declaration decl left (fun decl -> LeftDeclaration (loc, decl))
    | LeftExpression expr ->
      id this#expression expr left (fun expr -> LeftExpression expr)

  method for_of_statement (stuff: Ast.Statement.ForOf.t) =
    let open Ast.Statement.ForOf in
    let { left; right; body; async } = stuff in
    let left' = this#for_of_statement_lhs left in
    let right' = this#expression right in
    let body' = this#statement body in
    if left == left' && right == right' && body == body' then stuff
    else { left = left'; right = right'; body = body'; async }

  method for_of_statement_lhs (left: Ast.Statement.ForOf.left) =
    let open Ast.Statement.ForOf in
    match left with
    | LeftDeclaration (loc, decl) ->
      id this#variable_declaration decl left (fun decl -> LeftDeclaration (loc, decl))
    | LeftExpression expr ->
      id this#expression expr left (fun expr -> LeftExpression expr)

  method for_statement (stmt: Ast.Statement.For.t) =
    let open Ast.Statement.For in
    let { init; test; update; body } = stmt in
    let init' = map_opt this#for_statement_init init in
    let test' = map_opt this#expression test in
    let update' = map_opt this#expression update in
    let body' = this#statement body in
    if init == init' &&
       test == test' &&
       update == update' &&
       body == body'
      then stmt
      else { init = init'; test = test'; update = update'; body = body' }

  method for_statement_init (init: Ast.Statement.For.init) =
    let open Ast.Statement.For in
    match init with
    | InitDeclaration (loc, decl) ->
      id this#variable_declaration decl init
        (fun decl -> InitDeclaration (loc, decl))
    | InitExpression expr ->
      id this#expression expr init (fun expr -> InitExpression expr)

  method function_param_type (fpt: Ast.Type.Function.Param.t) =
    let open Ast.Type.Function.Param in
    let loc, { typeAnnotation; name; optional; } = fpt in
    let typeAnnotation' = this#type_ typeAnnotation in
    if typeAnnotation' == typeAnnotation then fpt
    else loc, { typeAnnotation = typeAnnotation'; name; optional }

  method function_rest_param_type (frpt: Ast.Type.Function.RestParam.t) =
    let open Ast.Type.Function.RestParam in
    let loc, { argument } = frpt in
    let argument' = this#function_param_type argument in
    if argument' == argument then frpt
    else loc, { argument = argument' }

  method function_type (ft: Ast.Type.Function.t) =
    let open Ast.Type.Function in
    let { params = (ps, rpo) ; returnType; typeParameters; } = ft in
    let ps' = map_list this#function_param_type ps in
    let rpo' = map_opt this#function_rest_param_type rpo in
    let returnType' = this#type_ returnType in
    if ps' == ps && rpo' == rpo && returnType' == returnType then ft
    else { params = (ps', rpo'); returnType = returnType'; typeParameters }

  method object_property_value_type (opvt: Ast.Type.Object.Property.value) =
    let open Ast.Type.Object.Property in
    match opvt with
    | Init t -> id this#type_ t opvt (fun t -> Init t)
    | Get (loc, ft) -> id this#function_type ft opvt (fun ft -> Get (loc, ft))
    | Set (loc, ft) -> id this#function_type ft opvt (fun ft -> Set (loc, ft))

  method object_property_type (opt: Ast.Type.Object.Property.t) =
    let open Ast.Type.Object.Property in
    let loc, { key; value; optional; static; _method; variance; } = opt in
    let value' = this#object_property_value_type value in
    if value' == value then opt
    else loc, { key; value = value'; optional; static; _method; variance }

  method object_type (ot: Ast.Type.Object.t) =
    let open Ast.Type.Object in
    let { properties ; exact; } = ot in
    let properties' = map_list (fun p -> match p with
      | Property p' -> id this#object_property_type p' p (fun p' -> Property p')
      | _ -> p (* TODO *)
    ) properties in
    if properties' == properties then ot
    else { properties = properties'; exact }

  method generic_identifier_type (git: Ast.Type.Generic.Identifier.t) =
    let open Ast.Type.Generic.Identifier in
    match git with
    | Unqualified i -> id this#identifier i git (fun i -> Unqualified i)
    | _ -> git (* TODO *)

  method generic_type (gt: Ast.Type.Generic.t) =
    let open Ast.Type.Generic in
    let { id; typeParameters; } = gt in
    let id' = this#generic_identifier_type id in
    if id' == id then gt
    else { id = id'; typeParameters }

  method type_ (t: Ast.Type.t) =
    let open Ast.Type in
    match t with
    | _, Any
    | _, Mixed
    | _, Empty
    | _, Void
    | _, Null
    | _, Number
    | _, String
    | _, Boolean
    | _, StringLiteral _
    | _, NumberLiteral _
    | _, BooleanLiteral _
    | _, Exists
      -> t
    | loc, Nullable t' -> id this#type_ t' t (fun t' -> loc, Nullable t')
    | loc, Array t' -> id this#type_ t' t (fun t' -> loc, Array t')
    | loc, Typeof t' -> id this#type_ t' t (fun t' -> loc, Typeof t')
    | loc, Function ft -> id this#function_type ft t (fun ft -> loc, Function ft)
    | loc, Object ot -> id this#object_type ot t (fun ot -> loc, Object ot)
    | loc, Generic gt -> id this#generic_type gt t (fun gt -> loc, Generic gt)
    | loc, Union (t0, t1, ts) ->
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = map_list this#type_ ts in
      if t0' == t0 && t1' == t1 && ts' == ts then t
      else loc, Union (t0', t1', ts')
    | loc, Intersection (t0, t1, ts) ->
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = map_list this#type_ ts in
      if t0' == t0 && t1' == t1 && ts' == ts then t
      else loc, Intersection (t0', t1', ts')
    | loc, Tuple ts ->
      let ts' = map_list this#type_ ts in
      if ts' == ts then t
      else loc, Tuple ts'

  method type_annotation (annot: Ast.Type.annotation) =
    let loc, t = annot in
    id this#type_ t annot (fun t -> loc, t)

  method function_ (expr: Ast.Function.t) =
    let open Ast.Function in
    let {
      id = ident; params; body; async; generator; expression;
      predicate; returnType; typeParameters;
    } = expr in
    let ident' = map_opt this#identifier ident in
    let params' =
      let (param_list, rest) = params in
      let param_list' = map_list this#function_param_pattern param_list in
      let rest' = map_opt this#function_rest_element rest in
      if param_list == param_list' && rest == rest' then params
      else (param_list', rest')
    in
    let returnType' = map_opt this#type_annotation returnType in
    let body' = match body with
      | BodyBlock (loc, block) ->
        id this#block block body (fun block -> BodyBlock (loc, block))
      | BodyExpression expr ->
        id this#expression expr body (fun expr -> BodyExpression expr)
    in
    (* TODO: walk predicate *)
    (* TODO: walk typeParameters *)
    if ident == ident' && params == params' && body == body' && returnType == returnType' then expr
    else {
      id = ident'; params = params'; returnType = returnType'; body = body';
      async; generator; expression; predicate; typeParameters;
    }

  method function_declaration (stmt: Ast.Function.t) =
    this#function_ stmt

  (* TODO *)
  method generator (expr: Ast.Expression.Generator.t) = expr

  method identifier (expr: Ast.Identifier.t) = expr

  method import (expr: Ast.Expression.t) = expr

  method if_consequent_statement ~has_else (stmt: Ast.Statement.t) =
    ignore has_else;
    this#statement stmt

  method if_statement (stmt: Ast.Statement.If.t) =
    let open Ast.Statement.If in
    let { test; consequent; alternate } = stmt in
    let test' = this#expression test in
    let consequent' =
      this#if_consequent_statement ~has_else:(alternate <> None) consequent in
    let alternate' = map_opt this#statement alternate in
    if test == test' && consequent == consequent' && alternate == alternate'
    then stmt
    else { test = test'; consequent = consequent'; alternate = alternate' }

  method import_declaration (decl: Ast.Statement.ImportDeclaration.t) =
    let open Ast.Statement.ImportDeclaration in
    let { importKind; source; specifiers } = decl in
    match importKind with
    | ImportValue ->
      let specifiers' = map_list this#import_specifier specifiers in
      if specifiers == specifiers' then decl
      else { importKind; source; specifiers = specifiers'; }
    | ImportType | ImportTypeof -> decl (* TODO *)

  method import_specifier (specifier: Ast.Statement.ImportDeclaration.specifier) =
    let open Ast.Statement.ImportDeclaration in
    match specifier with
    | ImportNamedSpecifier { kind; local; remote } ->
      begin match kind with
      | None ->
        let ident = match local with
          | None -> remote
          | Some ident -> ident in
        id (this#import_named_specifier ~ident) local specifier
          (fun local -> ImportNamedSpecifier { kind = None; local; remote })
      | Some _importKind -> specifier (* TODO *)
      end
    | ImportDefaultSpecifier ident ->
      id this#import_default_specifier ident specifier
        (fun ident -> ImportDefaultSpecifier ident)
    | ImportNamespaceSpecifier (loc, ident) ->
      id this#import_namespace_specifier ident specifier
        (fun ident -> ImportNamespaceSpecifier (loc, ident))

  method import_named_specifier ~ident (local: Ast.Identifier.t option) =
    id this#identifier ident local (fun ident -> Some ident)

  method import_default_specifier (id: Ast.Identifier.t) =
    this#identifier id

  method import_namespace_specifier (id: Ast.Identifier.t) =
    this#identifier id

  method jsx_element (expr: Ast.JSX.element) =
    let open Ast.JSX in
    let { openingElement; closingElement = _; children } = expr in
    let openingElement' = this#jsx_opening_element openingElement in
    let children' = map_list this#jsx_child children in
    if openingElement == openingElement' && children == children' then expr
    else { expr with openingElement = openingElement'; children = children' }

  method jsx_opening_element (elem: Ast.JSX.Opening.t) =
    let open Ast.JSX.Opening in
    let loc, { name; selfClosing; attributes } = elem in
    let attributes' = map_list this#jsx_opening_attribute attributes in
    if attributes == attributes' then elem
    else loc, { name; selfClosing; attributes = attributes' }

  method jsx_opening_attribute (jsx_attr: Ast.JSX.Opening.attribute) =
    let open Ast.JSX.Opening in
    match jsx_attr with
    | Attribute attr ->
      id this#jsx_attribute attr jsx_attr (fun attr -> Attribute attr)
    | SpreadAttribute (loc, attr) ->
      id this#jsx_spread_attribute attr jsx_attr (fun attr -> SpreadAttribute (loc, attr))

  method jsx_spread_attribute (attr: Ast.JSX.SpreadAttribute.t') =
    let open Ast.JSX.SpreadAttribute in
    let { argument } = attr in
    id this#expression argument attr (fun argument -> { argument })

  method jsx_attribute (attr: Ast.JSX.Attribute.t) =
    let open Ast.JSX.Attribute in
    let loc, { name; value } = attr in
    let value' = map_opt this#jsx_attribute_value value in
    if value == value' then attr
    else loc, { name; value }

  method jsx_attribute_value (value: Ast.JSX.Attribute.value) =
    let open Ast.JSX.Attribute in
    match value with
    | Literal _ -> value
    | ExpressionContainer (expr_loc, expr) ->
      id this#jsx_expression expr value (fun expr -> ExpressionContainer (expr_loc, expr))

  method jsx_child (child: Ast.JSX.child) =
    let open Ast.JSX in
    let loc, child' = child in
    match child' with
    | Element elem ->
      id this#jsx_element elem child (fun elem -> loc, Element elem)
    | ExpressionContainer expr ->
      id this#jsx_expression expr child (fun expr -> loc, ExpressionContainer expr)
    | Text _ -> child

  method jsx_expression (jsx_expr: Ast.JSX.ExpressionContainer.t) =
    let open Ast.JSX.ExpressionContainer in
    let { expression } = jsx_expr in
    match expression with
    | Expression expr ->
      id this#expression expr jsx_expr (fun expr -> { expression = Expression expr})
    | EmptyExpression _ -> jsx_expr

  method labeled_statement (stmt: Ast.Statement.Labeled.t) =
    let open Ast.Statement.Labeled in
    let { label; body } = stmt in
    let label' = this#identifier label in
    let body' = this#statement body in
    if label == label' && body == body' then stmt
    else { label = label'; body = body' }

  method literal (expr: Ast.Literal.t) = expr

  method logical (expr: Ast.Expression.Logical.t) =
    let open Ast.Expression.Logical in
    let { operator = _; left; right } = expr in
    let left' = this#expression left in
    let right' = this#expression right in
    if left == left' && right == right' then expr
    else { expr with left = left'; right = right' }

  method member (expr: Ast.Expression.Member.t) =
    let open Ast.Expression.Member in
    let { _object; property; computed = _; } = expr in
    let _object' = this#expression _object in
    let property' = this#member_property property in
    if _object == _object' && property == property' then expr
    else { expr with _object = _object'; property = property' }

  method member_property (expr: Ast.Expression.Member.property) =
    let open Ast.Expression.Member in
    match expr with
    | PropertyIdentifier ident ->
      id this#member_property_identifier ident expr
        (fun ident -> PropertyIdentifier ident)
    | PropertyExpression e ->
      id this#member_property_expression e expr (fun e -> PropertyExpression e)

  method member_property_identifier (ident: Ast.Identifier.t) =
    this#identifier ident

  method member_property_expression (expr: Ast.Expression.t) =
    this#expression expr

  (* TODO *)
  method meta_property (expr: Ast.Expression.MetaProperty.t) = expr

  method new_ (expr: Ast.Expression.New.t) =
    let open Ast.Expression.New in
    let { callee; arguments } = expr in
    let callee' = this#expression callee in
    let arguments' = map_list this#expression_or_spread arguments in
    if callee == callee' && arguments == arguments' then expr
    else { callee = callee'; arguments = arguments' }

  method object_ (expr: Ast.Expression.Object.t) =
    let open Ast.Expression.Object in
    let { properties } = expr in
    let properties' = map_list (fun prop ->
      match prop with
      | Property p ->
        let p' = this#object_property p in
        if p == p' then prop else Property p'
      | SpreadProperty s ->
        let s' = this#spread_property s in
        if s == s' then prop else SpreadProperty s'
    ) properties in
    if properties == properties' then expr
    else { properties = properties' }

  method object_property (prop: Ast.Expression.Object.Property.t) =
    let open Ast.Expression.Object.Property in
    let (loc, { key; value; _method; shorthand }) = prop in
    let key' = this#object_key key in
    let value' = match value with
      | Init expr ->
        let expr' = this#expression expr in
        if expr == expr' then value else Init expr'
      | Get (loc, fn) ->
        let fn' = this#function_ fn in
        if fn == fn' then value else Get (loc, fn')
      | Set (loc, fn) ->
        let fn' = this#function_ fn in
        if fn == fn' then value else Set (loc, fn')
    in
    if key == key' && value == value' then prop
    else (loc, { key = key'; value = value'; _method; shorthand })

  method object_key (key: Ast.Expression.Object.Property.key) =
    let open Ast.Expression.Object.Property in
    match key with
    | Literal (loc, lit) ->
      id this#literal lit key (fun lit -> Literal (loc, lit))
    | Identifier ident ->
      id this#object_key_identifier ident key (fun ident -> Identifier ident)
    | Computed expr ->
      id this#expression expr key (fun expr -> Computed expr)

  method object_key_identifier (ident: Ast.Identifier.t) =
    this#identifier ident

  method function_param_pattern (expr: Ast.Pattern.t) =
    this#binding_pattern expr

  method variable_declarator_pattern ~kind (expr: Ast.Pattern.t) =
    this#binding_pattern ~kind expr

  method catch_clause_pattern (expr: Ast.Pattern.t) =
    this#binding_pattern expr

  method binding_pattern ?(kind=Ast.Statement.VariableDeclaration.Var) (expr: Ast.Pattern.t) =
    this#pattern ~kind expr

  method assignment_pattern (expr: Ast.Pattern.t) =
    this#pattern expr

  (* NOTE: Patterns are highly overloaded. A pattern can be a binding pattern,
     which has a kind (Var/Let/Const, with Var being the default for all pre-ES5
     bindings), or an assignment pattern, which has no kind. Subterms that are
     patterns inherit the kind (or lack thereof). *)
  method pattern ?kind (expr: Ast.Pattern.t) =
    let open Ast.Pattern in
    let (loc, patt) = expr in
    let patt' = match patt with
      | Object { Object.properties; typeAnnotation } ->
        let properties' = map_list (this#pattern_object_p ?kind) properties in
        let typeAnnotation' = map_opt this#type_annotation typeAnnotation in
        if properties' == properties && typeAnnotation' == typeAnnotation then patt
        else Object { Object.properties = properties'; typeAnnotation = typeAnnotation' }
      | Array { Array.elements; typeAnnotation } ->
        let elements' = map_list (map_opt (this#pattern_array_e ?kind)) elements in
        let typeAnnotation' = map_opt this#type_annotation typeAnnotation in
        if elements' == elements && typeAnnotation' == typeAnnotation then patt
        else Array { Array.elements = elements'; typeAnnotation = typeAnnotation' }
      | Assignment { Assignment.left; right } ->
        let left' = this#pattern_assignment_pattern ?kind left in
        let right' = this#expression right in
        if left == left' && right == right' then patt
        else Assignment { Assignment.left = left'; right = right' }
      | Identifier { Identifier.name; typeAnnotation; optional } ->
        let name' = this#identifier name in
        let typeAnnotation' = map_opt this#type_annotation typeAnnotation in
        if name == name' && typeAnnotation == typeAnnotation' then patt
        else Identifier { Identifier.name = name'; typeAnnotation = typeAnnotation'; optional }
      | Expression e ->
        (* TODO: wtf is this? *)
        id this#pattern_expression e patt (fun e -> Expression e)
    in
    if patt == patt' then expr else (loc, patt')

  method pattern_object_p ?kind (p: Ast.Pattern.Object.property) =
    let open Ast.Pattern.Object in
    match p with
    | Property (loc, prop) ->
      id (this#pattern_object_property ?kind) prop p (fun prop -> Property (loc, prop))
    | RestProperty (loc, prop) ->
      id (this#pattern_object_rest_property ?kind) prop p (fun prop -> RestProperty (loc, prop))

  method pattern_object_property ?kind (prop: Ast.Pattern.Object.Property.t') =
    let open Ast.Pattern.Object.Property in
    let { key; pattern; shorthand = _ } = prop in
    let pattern' = this#pattern_object_property_pattern ?kind pattern in
    if pattern' == pattern then prop
    else { key; pattern = pattern'; shorthand = false }

  method pattern_object_rest_property ?kind (prop: Ast.Pattern.Object.RestProperty.t') =
    let open Ast.Pattern.Object.RestProperty in
    let { argument } = prop in
    let argument' = this#pattern_object_rest_property_pattern ?kind argument in
    if argument' == argument then prop
    else { argument = argument' }

  method pattern_object_property_pattern ?kind (expr: Ast.Pattern.t) =
    this#pattern ?kind expr

  method pattern_object_rest_property_pattern ?kind (expr: Ast.Pattern.t) =
    this#pattern ?kind expr

  method pattern_array_e ?kind (e: Ast.Pattern.Array.element) =
    let open Ast.Pattern.Array in
    match e with
    | Element elem ->
      id (this#pattern_array_element_pattern ?kind) elem e (fun elem -> Element elem)
    | RestElement (loc, elem) ->
      id (this#pattern_array_rest_element ?kind) elem e (fun elem -> RestElement (loc, elem))

  method pattern_array_element_pattern ?kind (expr: Ast.Pattern.t) =
    this#pattern ?kind expr

  method pattern_array_rest_element ?kind (elem: Ast.Pattern.Array.RestElement.t') =
    let open Ast.Pattern.Array.RestElement in
    let { argument } = elem in
    let argument' = this#pattern_array_rest_element_pattern ?kind argument in
    if argument' == argument then elem
    else { argument = argument' }

  method pattern_array_rest_element_pattern ?kind (expr: Ast.Pattern.t) =
    this#pattern ?kind expr

  method pattern_assignment_pattern ?kind (expr: Ast.Pattern.t) =
    this#pattern ?kind expr

  method pattern_expression (expr: Ast.Expression.t) =
    this#expression expr

  (* TODO *)
  method function_rest_element (expr: Ast.Function.RestElement.t) = expr

  method return (stmt: Ast.Statement.Return.t) =
    let open Ast.Statement.Return in
    let { argument } = stmt in
    let argument' = map_opt this#expression argument in
    if argument == argument' then stmt else { argument = argument' }

  method sequence (expr: Ast.Expression.Sequence.t) =
    let open Ast.Expression.Sequence in
    let { expressions } = expr in
    let expressions' = map_list this#expression expressions in
    if expressions == expressions' then expr else { expressions = expressions' }

  method statement_list (stmts: Ast.Statement.t list) =
    map_list this#statement stmts

  method spread_element (expr: Ast.Expression.SpreadElement.t) =
    let open Ast.Expression.SpreadElement in
    let loc, { argument } = expr in
    id this#expression argument expr (fun argument -> loc, { argument })

  method spread_property (expr: Ast.Expression.Object.SpreadProperty.t) =
    let open Ast.Expression.Object.SpreadProperty in
    let (loc, { argument }) = expr in
    id this#expression argument expr (fun argument -> loc, { argument })

  method switch (switch: Ast.Statement.Switch.t) =
    let open Ast.Statement.Switch in
    let { discriminant; cases } = switch in
    let discriminant' = this#expression discriminant in
    let cases' = map_list (fun stuff ->
      let (loc, case) = stuff in
      id this#switch_case case stuff (fun case -> loc, case)
    ) cases in
    if discriminant == discriminant' && cases == cases' then switch
    else { discriminant = discriminant'; cases = cases' }

  method switch_case (case: Ast.Statement.Switch.Case.t') =
    let open Ast.Statement.Switch.Case in
    let { test; consequent } = case in
    let test' = map_opt this#expression test in
    let consequent' = this#statement_list consequent in
    if test == test' && consequent == consequent' then case
    else { test = test'; consequent = consequent' }

  method tagged_template (expr: Ast.Expression.TaggedTemplate.t) =
    let open Ast.Expression.TaggedTemplate in
    let { tag; quasi } = expr in
    let tag' = this#expression tag in
    let quasi' =
      let loc, templ = quasi in
      id this#template_literal templ quasi (fun templ -> loc, templ) in
    if tag == tag' && quasi == quasi' then expr
    else { tag = tag'; quasi = quasi' }

  method template_literal (expr: Ast.Expression.TemplateLiteral.t) =
    let open Ast.Expression.TemplateLiteral in
    let { quasis; expressions } = expr in
    let quasis' = map_list this#template_literal_element quasis in
    let expressions' = map_list this#expression expressions in
    if quasis == quasis' && expressions == expressions' then expr
    else { quasis = quasis'; expressions = expressions' }

  (* TODO *)
  method template_literal_element (elem: Ast.Expression.TemplateLiteral.Element.t) =
    elem

  method throw (stmt: Ast.Statement.Throw.t) =
    let open Ast.Statement.Throw in
    let { argument } = stmt in
    id this#expression argument stmt (fun argument -> { argument })

  method try_catch (stmt: Ast.Statement.Try.t) =
    let open Ast.Statement.Try in
    let { block = (block_loc, block); handler; finalizer } = stmt in
    let block' = this#block block in
    let handler' = match handler with
    | Some (loc, clause) ->
      id this#catch_clause clause handler (fun clause -> Some (loc, clause))
    | None -> handler
    in
    let finalizer' = match finalizer with
    | Some (finalizer_loc, block) ->
      id this#block block finalizer (fun block -> Some (finalizer_loc, block))
    | None -> finalizer
    in
    if block == block' && handler == handler' && finalizer == finalizer'
    then stmt
    else {
      block = (block_loc, block');
      handler = handler';
      finalizer = finalizer'
    }

  method type_cast (expr: Ast.Expression.TypeCast.t) =
    let open Ast.Expression.TypeCast in
    let { expression; typeAnnotation; } = expr in
    let expression' = this#expression expression in
    let typeAnnotation' = this#type_annotation typeAnnotation in
    if expression' == expression && typeAnnotation' == typeAnnotation then expr
    else { expression = expression'; typeAnnotation = typeAnnotation' }

  method unary_expression (expr: Ast.Expression.Unary.t) =
    let open Ast.Expression in
    let { Unary.argument; operator = _; prefix = _ } = expr in
    id this#expression argument expr
      (fun argument -> { expr with Unary.argument })

  method update_expression (expr: Ast.Expression.Update.t) =
    let open Ast.Expression.Update in
    let { argument; operator = _; prefix = _ } = expr in
    id this#expression argument expr (fun argument -> { expr with argument })

  method variable_declaration (decl: Ast.Statement.VariableDeclaration.t) =
    let open Ast.Statement.VariableDeclaration in
    let { declarations; kind } = decl in
    let decls' = map_list (this#variable_declarator ~kind) declarations in
    if declarations == decls' then decl
    else { declarations = decls'; kind }

  method variable_declarator ~kind (decl: Ast.Statement.VariableDeclaration.Declarator.t) =
    let open Ast.Statement.VariableDeclaration.Declarator in
    let (loc, { id; init }) = decl in
    let id' = this#variable_declarator_pattern ~kind id in
    let init' = map_opt this#expression init in
    if id == id' && init == init' then decl
    else (loc, { id = id'; init = init' })

  method while_ (stuff: Ast.Statement.While.t) =
    let open Ast.Statement.While in
    let { test; body } = stuff in
    let test' = this#expression test in
    let body' = this#statement body in
    if test == test' && body == body' then stuff
    else { test = test'; body = body' }

  method with_ (stuff: Ast.Statement.With.t) =
    let open Ast.Statement.With in
    let { _object; body } = stuff in
    let _object' = this#expression _object in
    let body' = this#statement body in
    if _object == _object' && body == body' then stuff
    else { _object = _object'; body = body' }

  (* TODO *)
  method yield (expr: Ast.Expression.Yield.t) = expr

end

let fold_program mappers ast =
  List.fold_left (fun ast (m: mapper) -> m#program ast) ast mappers
