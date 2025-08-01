(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let map_opt : 'node. ('node -> 'node) -> 'node option -> 'node option =
 fun map opt ->
  match opt with
  | Some item ->
    let item' = map item in
    if item == item' then
      opt
    else
      Some item'
  | None -> opt

let id_loc : 'node 'a. ('loc -> 'node -> 'node) -> 'loc -> 'node -> 'a -> ('node -> 'a) -> 'a =
 fun map loc item same diff ->
  let item' = map loc item in
  if item == item' then
    same
  else
    diff item'

let id : 'node 'a. ('node -> 'node) -> 'node -> 'a -> ('node -> 'a) -> 'a =
 fun map item same diff ->
  let item' = map item in
  if item == item' then
    same
  else
    diff item'

let map_loc : 'node. ('loc -> 'node -> 'node) -> 'loc * 'node -> 'loc * 'node =
 fun map same ->
  let (loc, item) = same in
  id_loc map loc item same (fun diff -> (loc, diff))

let map_loc_opt : 'node. ('loc -> 'node -> 'node) -> ('loc * 'node) option -> ('loc * 'node) option
    =
 fun map same ->
  map_opt
    (fun same ->
      let (loc, item) = same in
      id_loc map loc item same (fun diff -> (loc, diff)))
    same

let map_list map lst =
  let (rev_lst, changed) =
    List.fold_left
      (fun (lst', changed) item ->
        let item' = map item in
        (item' :: lst', changed || item' != item))
      ([], false)
      lst
  in
  if changed then
    List.rev rev_lst
  else
    lst

let map_list_multiple map lst =
  let (rev_lst, changed) =
    List.fold_left
      (fun (lst', changed) item ->
        match map item with
        | [] -> (lst', true)
        | [item'] -> (item' :: lst', changed || item != item')
        | items' -> (List.rev_append items' lst', true))
      ([], false)
      lst
  in
  if changed then
    List.rev rev_lst
  else
    lst

type type_params_context =
  | ClassTP
  | FunctionTP
  | DeclareFunctionTP
  | DeclareClassTP
  | DeclareComponentTP
  | TypeAliasTP
  | InterfaceTP
  | OpaqueTypeTP
  | ComponentDeclarationTP
  | ComponentTypeTP
  | FunctionTypeTP
  | InferTP
  | ObjectMappedTypeTP

class ['loc] mapper =
  object (this)
    method program (program : ('loc, 'loc) Ast.Program.t) =
      let open Ast.Program in
      let (loc, { statements; interpreter; comments; all_comments }) = program in
      let statements' = this#toplevel_statement_list statements in
      let comments' = this#syntax_opt comments in
      let all_comments' = map_list this#comment all_comments in
      if statements == statements' && comments == comments' && all_comments == all_comments' then
        program
      else
        ( loc,
          {
            statements = statements';
            interpreter;
            comments = comments';
            all_comments = all_comments';
          }
        )

    method statement (stmt : ('loc, 'loc) Ast.Statement.t) =
      let open Ast.Statement in
      match stmt with
      | (loc, Block block) -> id_loc this#block loc block stmt (fun block -> (loc, Block block))
      | (loc, Break break) -> id_loc this#break loc break stmt (fun break -> (loc, Break break))
      | (loc, ClassDeclaration cls) ->
        id_loc this#class_declaration loc cls stmt (fun cls -> (loc, ClassDeclaration cls))
      | (loc, ComponentDeclaration component) ->
        id_loc this#component_declaration loc component stmt (fun component ->
            (loc, ComponentDeclaration component)
        )
      | (loc, Continue cont) -> id_loc this#continue loc cont stmt (fun cont -> (loc, Continue cont))
      | (loc, Debugger dbg) -> id_loc this#debugger loc dbg stmt (fun dbg -> (loc, Debugger dbg))
      | (loc, DeclareClass stuff) ->
        id_loc this#declare_class loc stuff stmt (fun stuff -> (loc, DeclareClass stuff))
      | (loc, DeclareComponent stuff) ->
        id_loc this#declare_component loc stuff stmt (fun stuff -> (loc, DeclareComponent stuff))
      | (loc, DeclareEnum enum) ->
        id_loc this#declare_enum loc enum stmt (fun enum -> (loc, DeclareEnum enum))
      | (loc, DeclareExportDeclaration decl) ->
        id_loc this#declare_export_declaration loc decl stmt (fun decl ->
            (loc, DeclareExportDeclaration decl)
        )
      | (loc, DeclareFunction stuff) ->
        id_loc this#declare_function loc stuff stmt (fun stuff -> (loc, DeclareFunction stuff))
      | (loc, DeclareInterface stuff) ->
        id_loc this#declare_interface loc stuff stmt (fun stuff -> (loc, DeclareInterface stuff))
      | (loc, DeclareModule m) ->
        id_loc this#declare_module loc m stmt (fun m -> (loc, DeclareModule m))
      | (loc, DeclareModuleExports annot) ->
        id_loc this#declare_module_exports loc annot stmt (fun annot ->
            (loc, DeclareModuleExports annot)
        )
      | (loc, DeclareNamespace n) ->
        id_loc this#declare_namespace loc n stmt (fun n -> (loc, DeclareNamespace n))
      | (loc, DeclareOpaqueType otype) ->
        id_loc this#opaque_type loc otype stmt (fun otype -> (loc, DeclareOpaqueType otype))
      | (loc, DeclareTypeAlias stuff) ->
        id_loc this#declare_type_alias loc stuff stmt (fun stuff -> (loc, DeclareTypeAlias stuff))
      | (loc, DeclareVariable stuff) ->
        id_loc this#declare_variable loc stuff stmt (fun stuff -> (loc, DeclareVariable stuff))
      | (loc, DoWhile stuff) ->
        id_loc this#do_while loc stuff stmt (fun stuff -> (loc, DoWhile stuff))
      | (loc, Empty empty) -> id_loc this#empty loc empty stmt (fun empty -> (loc, Empty empty))
      | (loc, EnumDeclaration enum) ->
        id_loc this#enum_declaration loc enum stmt (fun enum -> (loc, EnumDeclaration enum))
      | (loc, ExportDefaultDeclaration decl) ->
        id_loc this#export_default_declaration loc decl stmt (fun decl ->
            (loc, ExportDefaultDeclaration decl)
        )
      | (loc, ExportNamedDeclaration decl) ->
        id_loc this#export_named_declaration loc decl stmt (fun decl ->
            (loc, ExportNamedDeclaration decl)
        )
      | (loc, Expression expr) ->
        id_loc this#expression_statement loc expr stmt (fun expr -> (loc, Expression expr))
      | (loc, For for_stmt) ->
        id_loc this#for_statement loc for_stmt stmt (fun for_stmt -> (loc, For for_stmt))
      | (loc, ForIn stuff) ->
        id_loc this#for_in_statement loc stuff stmt (fun stuff -> (loc, ForIn stuff))
      | (loc, ForOf stuff) ->
        id_loc this#for_of_statement loc stuff stmt (fun stuff -> (loc, ForOf stuff))
      | (loc, FunctionDeclaration func) ->
        id_loc this#function_declaration loc func stmt (fun func -> (loc, FunctionDeclaration func))
      | (loc, If if_stmt) ->
        id_loc this#if_statement loc if_stmt stmt (fun if_stmt -> (loc, If if_stmt))
      | (loc, ImportDeclaration decl) ->
        id_loc this#import_declaration loc decl stmt (fun decl -> (loc, ImportDeclaration decl))
      | (loc, InterfaceDeclaration stuff) ->
        id_loc this#interface_declaration loc stuff stmt (fun stuff ->
            (loc, InterfaceDeclaration stuff)
        )
      | (loc, Labeled label) ->
        id_loc this#labeled_statement loc label stmt (fun label -> (loc, Labeled label))
      | (loc, Match x) -> id_loc this#match_statement loc x stmt (fun x -> (loc, Match x))
      | (loc, OpaqueType otype) ->
        id_loc this#opaque_type loc otype stmt (fun otype -> (loc, OpaqueType otype))
      | (loc, Return ret) -> id_loc this#return loc ret stmt (fun ret -> (loc, Return ret))
      | (loc, Switch switch) ->
        id_loc this#switch loc switch stmt (fun switch -> (loc, Switch switch))
      | (loc, Throw throw) -> id_loc this#throw loc throw stmt (fun throw -> (loc, Throw throw))
      | (loc, Try try_stmt) ->
        id_loc this#try_catch loc try_stmt stmt (fun try_stmt -> (loc, Try try_stmt))
      | (loc, VariableDeclaration decl) ->
        id_loc this#variable_declaration loc decl stmt (fun decl -> (loc, VariableDeclaration decl))
      | (loc, While stuff) -> id_loc this#while_ loc stuff stmt (fun stuff -> (loc, While stuff))
      | (loc, With stuff) -> id_loc this#with_ loc stuff stmt (fun stuff -> (loc, With stuff))
      | (loc, TypeAlias stuff) ->
        id_loc this#type_alias loc stuff stmt (fun stuff -> (loc, TypeAlias stuff))

    method comment (c : 'loc Ast.Comment.t) = c

    method syntax_opt
        : 'internal. ('loc, 'internal) Ast.Syntax.t option -> ('loc, 'internal) Ast.Syntax.t option
        =
      map_opt this#syntax

    method syntax : 'internal. ('loc, 'internal) Ast.Syntax.t -> ('loc, 'internal) Ast.Syntax.t =
      fun attached ->
        let open Ast.Syntax in
        let { leading; trailing; internal } = attached in
        let leading' = map_list this#comment leading in
        let trailing' = map_list this#comment trailing in
        if leading == leading' && trailing == trailing' then
          attached
        else
          { leading = leading'; trailing = trailing'; internal }

    method expression (expr : ('loc, 'loc) Ast.Expression.t) =
      let open Ast.Expression in
      match expr with
      | (loc, Array x) -> id_loc this#array loc x expr (fun x -> (loc, Array x))
      | (loc, ArrowFunction x) ->
        id_loc this#arrow_function loc x expr (fun x -> (loc, ArrowFunction x))
      | (loc, AsConstExpression x) ->
        id_loc this#as_const_expression loc x expr (fun x -> (loc, AsConstExpression x))
      | (loc, AsExpression x) ->
        id_loc this#as_expression loc x expr (fun x -> (loc, AsExpression x))
      | (loc, Assignment x) -> id_loc this#assignment loc x expr (fun x -> (loc, Assignment x))
      | (loc, Binary x) -> id_loc this#binary loc x expr (fun x -> (loc, Binary x))
      | (loc, Call x) -> id_loc this#call loc x expr (fun x -> (loc, Call x))
      | (loc, Class x) -> id_loc this#class_expression loc x expr (fun x -> (loc, Class x))
      | (loc, Conditional x) -> id_loc this#conditional loc x expr (fun x -> (loc, Conditional x))
      | (loc, Function x) -> id_loc this#function_expression loc x expr (fun x -> (loc, Function x))
      | (loc, Identifier x) -> id this#identifier x expr (fun x -> (loc, Identifier x))
      | (loc, Import x) -> id (this#import loc) x expr (fun x -> (loc, Import x))
      | (loc, JSXElement x) -> id_loc this#jsx_element loc x expr (fun x -> (loc, JSXElement x))
      | (loc, JSXFragment x) -> id_loc this#jsx_fragment loc x expr (fun x -> (loc, JSXFragment x))
      | (loc, StringLiteral x) ->
        id_loc this#string_literal loc x expr (fun x -> (loc, StringLiteral x))
      | (loc, BooleanLiteral x) ->
        id_loc this#boolean_literal loc x expr (fun x -> (loc, BooleanLiteral x))
      | (loc, NullLiteral x) -> id_loc this#null_literal loc x expr (fun x -> (loc, NullLiteral x))
      | (loc, NumberLiteral x) ->
        id_loc this#number_literal loc x expr (fun x -> (loc, NumberLiteral x))
      | (loc, BigIntLiteral x) ->
        id_loc this#bigint_literal loc x expr (fun x -> (loc, BigIntLiteral x))
      | (loc, RegExpLiteral x) ->
        id_loc this#regexp_literal loc x expr (fun x -> (loc, RegExpLiteral x))
      | (loc, ModuleRefLiteral x) ->
        id_loc this#module_ref_literal loc x expr (fun x -> (loc, ModuleRefLiteral x))
      | (loc, Logical x) -> id_loc this#logical loc x expr (fun x -> (loc, Logical x))
      | (loc, Match x) -> id_loc this#match_expression loc x expr (fun x -> (loc, Match x))
      | (loc, Member x) -> id_loc this#member loc x expr (fun x -> (loc, Member x))
      | (loc, MetaProperty x) ->
        id_loc this#meta_property loc x expr (fun x -> (loc, MetaProperty x))
      | (loc, New x) -> id_loc this#new_ loc x expr (fun x -> (loc, New x))
      | (loc, Object x) -> id_loc this#object_ loc x expr (fun x -> (loc, Object x))
      | (loc, OptionalCall x) -> id (this#optional_call loc) x expr (fun x -> (loc, OptionalCall x))
      | (loc, OptionalMember x) ->
        id_loc this#optional_member loc x expr (fun x -> (loc, OptionalMember x))
      | (loc, Sequence x) -> id_loc this#sequence loc x expr (fun x -> (loc, Sequence x))
      | (loc, Super x) -> id_loc this#super_expression loc x expr (fun x -> (loc, Super x))
      | (loc, TaggedTemplate x) ->
        id_loc this#tagged_template loc x expr (fun x -> (loc, TaggedTemplate x))
      | (loc, TemplateLiteral x) ->
        id_loc this#template_literal loc x expr (fun x -> (loc, TemplateLiteral x))
      | (loc, This x) -> id_loc this#this_expression loc x expr (fun x -> (loc, This x))
      | (loc, TypeCast x) -> id_loc this#type_cast loc x expr (fun x -> (loc, TypeCast x))
      | (loc, TSSatisfies x) -> id_loc this#ts_satisfies loc x expr (fun x -> (loc, TSSatisfies x))
      | (loc, Unary x) -> id_loc this#unary_expression loc x expr (fun x -> (loc, Unary x))
      | (loc, Update x) -> id_loc this#update_expression loc x expr (fun x -> (loc, Update x))
      | (loc, Yield x) -> id_loc this#yield loc x expr (fun x -> (loc, Yield x))

    method array _loc (expr : ('loc, 'loc) Ast.Expression.Array.t) =
      let open Ast.Expression in
      let { Array.elements; comments } = expr in
      let elements' = map_list this#array_element elements in
      let comments' = this#syntax_opt comments in
      if elements == elements' && comments == comments' then
        expr
      else
        { Array.elements = elements'; comments = comments' }

    method array_element element =
      let open Ast.Expression.Array in
      match element with
      | Expression expr -> id this#expression expr element (fun expr -> Expression expr)
      | Spread spread -> id this#spread_element spread element (fun spread -> Spread spread)
      | Hole _ -> element

    method arrow_function loc (expr : ('loc, 'loc) Ast.Function.t) = this#function_ loc expr

    method as_const_expression _loc (expr : ('loc, 'loc) Ast.Expression.AsConstExpression.t) =
      let open Ast.Expression.AsConstExpression in
      let { expression; comments } = expr in
      let expression' = this#expression expression in
      let comments' = this#syntax_opt comments in
      if expression' == expression && comments' == comments then
        expr
      else
        { expression = expression'; comments = comments' }

    method as_expression _loc (expr : ('loc, 'loc) Ast.Expression.AsExpression.t) =
      let open Ast.Expression.AsExpression in
      let { expression; annot; comments } = expr in
      let expression' = this#expression expression in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      if expression' == expression && annot' == annot && comments' == comments then
        expr
      else
        { expression = expression'; annot = annot'; comments = comments' }

    method assignment _loc (expr : ('loc, 'loc) Ast.Expression.Assignment.t) =
      let open Ast.Expression.Assignment in
      let { operator = _; left; right; comments } = expr in
      let left' = this#assignment_pattern left in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if left == left' && right == right' && comments == comments' then
        expr
      else
        { expr with left = left'; right = right'; comments = comments' }

    method binary _loc (expr : ('loc, 'loc) Ast.Expression.Binary.t) =
      let open Ast.Expression.Binary in
      let { operator = _; left; right; comments } = expr in
      let left' = this#expression left in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if left == left' && right == right' && comments == comments' then
        expr
      else
        { expr with left = left'; right = right'; comments = comments' }

    method block _loc (stmt : ('loc, 'loc) Ast.Statement.Block.t) =
      let open Ast.Statement.Block in
      let { body; comments } = stmt in
      let body' = this#statement_list body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        stmt
      else
        { body = body'; comments = comments' }

    method break _loc (break : 'loc Ast.Statement.Break.t) =
      let open Ast.Statement.Break in
      let { label; comments } = break in
      let label' = map_opt this#label_identifier label in
      let comments' = this#syntax_opt comments in
      if label == label' && comments == comments' then
        break
      else
        { label = label'; comments = comments' }

    method call _loc (expr : ('loc, 'loc) Ast.Expression.Call.t) =
      let open Ast.Expression.Call in
      let { callee; targs; arguments; comments } = expr in
      let callee' = this#expression callee in
      let targs' = map_opt this#call_type_args targs in
      let arguments' = this#arg_list arguments in
      let comments' = this#syntax_opt comments in
      if callee == callee' && targs == targs' && arguments == arguments' && comments == comments'
      then
        expr
      else
        { callee = callee'; targs = targs'; arguments = arguments'; comments = comments' }

    method arg_list (arg_list : ('loc, 'loc) Ast.Expression.ArgList.t) =
      let open Ast.Expression.ArgList in
      let (loc, { arguments; comments }) = arg_list in
      let arguments' = map_list this#expression_or_spread arguments in
      let comments' = this#syntax_opt comments in
      if arguments == arguments' && comments == comments' then
        arg_list
      else
        (loc, { arguments = arguments'; comments = comments' })

    method optional_call loc (expr : ('loc, 'loc) Ast.Expression.OptionalCall.t) =
      let open Ast.Expression.OptionalCall in
      let { call; optional = _; filtered_out = _ } = expr in
      let call' = this#call loc call in
      if call == call' then
        expr
      else
        { expr with call = call' }

    method call_type_args (targs : ('loc, 'loc) Ast.Expression.CallTypeArgs.t) =
      let open Ast.Expression.CallTypeArgs in
      let (loc, { arguments; comments }) = targs in
      let arguments' = map_list this#call_type_arg arguments in
      let comments' = this#syntax_opt comments in
      if arguments == arguments' && comments == comments' then
        targs
      else
        (loc, { arguments = arguments'; comments = comments' })

    method call_type_arg t =
      let open Ast.Expression.CallTypeArg in
      match t with
      | Explicit x ->
        let x' = this#type_ x in
        if x' == x then
          t
        else
          Explicit x'
      | Implicit (loc, { Implicit.comments }) ->
        let comments' = this#syntax_opt comments in
        if comments == comments' then
          t
        else
          Implicit (loc, { Implicit.comments = comments' })

    method catch_body (body : 'loc * ('loc, 'loc) Ast.Statement.Block.t) = map_loc this#block body

    method catch_clause _loc (clause : ('loc, 'loc) Ast.Statement.Try.CatchClause.t') =
      let open Ast.Statement.Try.CatchClause in
      let { param; body; comments } = clause in
      let param' = map_opt this#catch_clause_pattern param in
      let body' = this#catch_body body in
      let comments' = this#syntax_opt comments in
      if param == param' && body == body' && comments == comments' then
        clause
      else
        { param = param'; body = body'; comments = comments' }

    method class_declaration loc (cls : ('loc, 'loc) Ast.Class.t) = this#class_ loc cls

    method class_expression loc (cls : ('loc, 'loc) Ast.Class.t) = this#class_ loc cls

    method class_ _loc (cls : ('loc, 'loc) Ast.Class.t) =
      let open Ast.Class in
      let { id; body; tparams; extends; implements; class_decorators; comments } = cls in
      let id' = map_opt this#class_identifier id in
      let tparams' = map_opt (this#type_params ~kind:ClassTP) tparams in
      let body' = this#class_body body in
      let extends' = map_opt (map_loc this#class_extends) extends in
      let implements' = map_opt this#class_implements implements in
      let class_decorators' = map_list this#class_decorator class_decorators in
      let comments' = this#syntax_opt comments in
      if
        id == id'
        && body == body'
        && extends == extends'
        && implements == implements'
        && class_decorators == class_decorators'
        && comments == comments'
        && tparams == tparams'
      then
        cls
      else
        {
          id = id';
          body = body';
          extends = extends';
          implements = implements';
          class_decorators = class_decorators';
          comments = comments';
          tparams = tparams';
        }

    method class_extends _loc (extends : ('loc, 'loc) Ast.Class.Extends.t') =
      let open Ast.Class.Extends in
      let { expr; targs; comments } = extends in
      let expr' = this#expression expr in
      let targs' = map_opt this#type_args targs in
      let comments' = this#syntax_opt comments in
      if expr == expr' && targs == targs' && comments == comments' then
        extends
      else
        { expr = expr'; targs = targs'; comments = comments' }

    method class_identifier (ident : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ~kind:Ast.Variable.Let ident

    method class_body (cls_body : ('loc, 'loc) Ast.Class.Body.t) =
      let open Ast.Class.Body in
      let (loc, { body; comments }) = cls_body in
      let body' = map_list this#class_element body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        cls_body
      else
        (loc, { body = body'; comments = comments' })

    method class_decorator (dec : ('loc, 'loc) Ast.Class.Decorator.t) =
      let open Ast.Class.Decorator in
      let (loc, { expression; comments }) = dec in
      let expression' = this#expression expression in
      let comments' = this#syntax_opt comments in
      if expression == expression' && comments == comments' then
        dec
      else
        (loc, { expression = expression'; comments = comments' })

    method class_element (elem : ('loc, 'loc) Ast.Class.Body.element) =
      let open Ast.Class.Body in
      match elem with
      | Method (loc, meth) -> id_loc this#class_method loc meth elem (fun meth -> Method (loc, meth))
      | Property (loc, prop) ->
        id_loc this#class_property loc prop elem (fun prop -> Property (loc, prop))
      | PrivateField (loc, field) ->
        id_loc this#class_private_field loc field elem (fun field -> PrivateField (loc, field))
      | StaticBlock (loc, block) ->
        id_loc this#class_static_block loc block elem (fun block -> StaticBlock (loc, block))

    method class_implements (implements : ('loc, 'loc) Ast.Class.Implements.t) =
      let open Ast.Class.Implements in
      let (loc, { interfaces; comments }) = implements in
      let interfaces' = map_list this#class_implements_interface interfaces in
      let comments' = this#syntax_opt comments in
      if interfaces == interfaces' && comments == comments' then
        implements
      else
        (loc, { interfaces = interfaces'; comments = comments' })

    method class_implements_interface (interface : ('loc, 'loc) Ast.Class.Implements.Interface.t) =
      let open Ast.Class.Implements.Interface in
      let (loc, { id; targs }) = interface in
      let id' = this#type_identifier_reference id in
      let targs' = map_opt this#type_args targs in
      if id == id' && targs == targs' then
        interface
      else
        (loc, { id = id'; targs = targs' })

    method class_method _loc (meth : ('loc, 'loc) Ast.Class.Method.t') =
      let open Ast.Class.Method in
      let { kind = _; key; value; static = _; decorators; comments } = meth in
      let key' = this#object_key key in
      let value' = map_loc this#function_expression_or_method value in
      let decorators' = map_list this#class_decorator decorators in
      let comments' = this#syntax_opt comments in
      if key == key' && value == value' && decorators == decorators' && comments == comments' then
        meth
      else
        { meth with key = key'; value = value'; decorators = decorators'; comments = comments' }

    method class_property _loc (prop : ('loc, 'loc) Ast.Class.Property.t') =
      let open Ast.Class.Property in
      let { key; value; annot; static = _; variance; decorators; comments } = prop in
      let key' = this#object_key key in
      let value' = this#class_property_value value in
      let annot' = this#type_annotation_hint annot in
      let variance' = this#variance_opt variance in
      let decorators' = map_list this#class_decorator decorators in
      let comments' = this#syntax_opt comments in
      if
        key == key'
        && value == value'
        && annot' == annot
        && variance' == variance
        && decorators' == decorators
        && comments' == comments
      then
        prop
      else
        {
          prop with
          key = key';
          value = value';
          annot = annot';
          variance = variance';
          decorators = decorators';
          comments = comments';
        }

    method class_property_value (value : ('loc, 'loc) Ast.Class.Property.value) =
      let open Ast.Class.Property in
      match value with
      | Declared -> value
      | Uninitialized -> value
      | Initialized x ->
        let x' = this#expression x in
        if x == x' then
          value
        else
          Initialized x'

    method class_private_field _loc (prop : ('loc, 'loc) Ast.Class.PrivateField.t') =
      let open Ast.Class.PrivateField in
      let { key; value; annot; static = _; variance; decorators; comments } = prop in
      let key' = this#private_name key in
      let value' = this#class_property_value value in
      let annot' = this#type_annotation_hint annot in
      let variance' = this#variance_opt variance in
      let decorators' = map_list this#class_decorator decorators in
      let comments' = this#syntax_opt comments in
      if
        key == key'
        && value == value'
        && annot' == annot
        && variance' == variance
        && decorators' == decorators
        && comments' == comments
      then
        prop
      else
        {
          prop with
          key = key';
          value = value';
          annot = annot';
          variance = variance';
          decorators = decorators';
          comments = comments';
        }

    method class_static_block _loc (block : ('loc, 'loc) Ast.Class.StaticBlock.t') =
      let open Ast.Class.StaticBlock in
      let { body; comments } = block in
      let body' = this#statement_list body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        block
      else
        { body = body'; comments = comments' }

    method default_opt (default : ('loc, 'loc) Ast.Expression.t option) =
      map_opt this#expression default

    method component_declaration _loc (component : ('loc, 'loc) Ast.Statement.ComponentDeclaration.t)
        =
      let open Ast.Statement.ComponentDeclaration in
      let { id = ident; tparams; params; body; renders; comments; sig_loc } = component in
      let ident' = this#component_identifier ident in
      let tparams' = map_opt (this#type_params ~kind:ComponentDeclarationTP) tparams in
      let params' = this#component_params params in
      let body' = this#component_body body in
      let renders' = this#component_renders_annotation renders in
      let comments' = this#syntax_opt comments in
      if
        ident == ident'
        && tparams == tparams'
        && params == params'
        && body == body'
        && renders == renders'
        && comments == comments'
      then
        component
      else
        {
          id = ident';
          tparams = tparams';
          params = params';
          body = body';
          renders = renders';
          comments = comments';
          sig_loc;
        }

    method component_identifier (ident : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ~kind:Ast.Variable.Var ident

    method component_params (params : ('loc, 'loc) Ast.Statement.ComponentDeclaration.Params.t) =
      let open Ast.Statement.ComponentDeclaration in
      let (loc, { Params.params = params_list; rest; comments }) = params in
      let params_list' = map_list this#component_param params_list in
      let rest' = map_opt this#component_rest_param rest in
      let comments' = this#syntax_opt comments in
      if params_list == params_list' && rest == rest' && comments == comments' then
        params
      else
        (loc, { Params.params = params_list'; rest = rest'; comments = comments' })

    method component_param (param : ('loc, 'loc) Ast.Statement.ComponentDeclaration.Param.t) =
      let open Ast.Statement.ComponentDeclaration.Param in
      let (loc, { name; local; default; shorthand }) = param in
      let name' = this#component_param_name name in
      let local' = this#component_param_pattern local in
      let default' = this#default_opt default in
      if name == name' && local == local' && default == default' then
        param
      else
        (loc, { name = name'; local = local'; default = default'; shorthand })

    method component_param_name
        (param_name : ('loc, 'loc) Ast.Statement.ComponentDeclaration.Param.param_name) =
      let open Ast.Statement.ComponentDeclaration.Param in
      match param_name with
      | Identifier ident -> id this#identifier ident param_name (fun x -> Identifier x)
      | StringLiteral (loc, str) ->
        id_loc this#string_literal loc str param_name (fun x -> StringLiteral (loc, x))

    method component_param_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#binding_pattern ~kind:Ast.Variable.Let expr

    method component_rest_param (expr : ('loc, 'loc) Ast.Statement.ComponentDeclaration.RestParam.t)
        =
      let open Ast.Statement.ComponentDeclaration.RestParam in
      let (loc, { argument; comments }) = expr in
      let argument' = this#component_param_pattern argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        expr
      else
        (loc, { argument = argument'; comments = comments' })

    method component_body (body : 'loc * ('loc, 'loc) Ast.Statement.Block.t) =
      let (loc, block) = body in
      id_loc this#block loc block body (fun block -> (loc, block))

    method conditional _loc (expr : ('loc, 'loc) Ast.Expression.Conditional.t) =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate; comments } = expr in
      let test' = this#predicate_expression test in
      let consequent' = this#expression consequent in
      let alternate' = this#expression alternate in
      let comments' = this#syntax_opt comments in
      if
        test == test'
        && consequent == consequent'
        && alternate == alternate'
        && comments == comments'
      then
        expr
      else
        { test = test'; consequent = consequent'; alternate = alternate'; comments = comments' }

    method continue _loc (cont : 'loc Ast.Statement.Continue.t) =
      let open Ast.Statement.Continue in
      let { label; comments } = cont in
      let label' = map_opt this#label_identifier label in
      let comments' = this#syntax_opt comments in
      if label == label' && comments == comments' then
        cont
      else
        { label = label'; comments = comments' }

    method debugger _loc (dbg : 'loc Ast.Statement.Debugger.t) =
      let open Ast.Statement.Debugger in
      let { comments } = dbg in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        dbg
      else
        { comments = comments' }

    method declare_class _loc (decl : ('loc, 'loc) Ast.Statement.DeclareClass.t) =
      let open Ast.Statement.DeclareClass in
      let { id = ident; tparams; body; extends; mixins; implements; comments } = decl in
      let id' = this#class_identifier ident in
      let tparams' = map_opt (this#type_params ~kind:DeclareClassTP) tparams in
      let body' = map_loc this#object_type body in
      let extends' = map_opt (map_loc this#generic_type) extends in
      let mixins' = map_list (map_loc this#generic_type) mixins in
      let implements' = map_opt this#class_implements implements in
      let comments' = this#syntax_opt comments in
      if
        id' == ident
        && tparams' == tparams
        && body' == body
        && extends' == extends
        && mixins' == mixins
        && implements' == implements
        && comments' == comments
      then
        decl
      else
        {
          id = id';
          tparams = tparams';
          body = body';
          extends = extends';
          mixins = mixins';
          implements = implements';
          comments = comments';
        }

    method declare_component _loc (decl : ('loc, 'loc) Ast.Statement.DeclareComponent.t) =
      let open Ast.Statement.DeclareComponent in
      let { id = ident; tparams; params; renders; comments } = decl in
      let ident' = this#component_identifier ident in
      let tparams' = map_opt (this#type_params ~kind:DeclareComponentTP) tparams in
      let params' = this#component_type_params params in
      let renders' = this#component_renders_annotation renders in
      let comments' = this#syntax_opt comments in
      if
        ident == ident'
        && tparams == tparams'
        && params == params'
        && renders == renders'
        && comments == comments'
      then
        decl
      else
        {
          id = ident';
          tparams = tparams';
          params = params';
          renders = renders';
          comments = comments';
        }

    method component_type _loc (t : ('loc, 'loc) Ast.Type.Component.t) =
      let open Ast.Type.Component in
      let { tparams; params; renders; comments } = t in
      let tparams' = map_opt (this#type_params ~kind:ComponentTypeTP) tparams in
      let params' = this#component_type_params params in
      let renders' = this#component_renders_annotation renders in
      let comments' = this#syntax_opt comments in
      if tparams == tparams' && params == params' && renders == renders' && comments == comments'
      then
        t
      else
        { tparams = tparams'; params = params'; renders = renders'; comments = comments' }

    method component_type_params (params : ('loc, 'loc) Ast.Type.Component.Params.t) =
      let open Ast.Type.Component in
      let (loc, { Params.params = params_list; rest; comments }) = params in
      let params_list' = map_list this#component_type_param params_list in
      let rest' = map_opt this#component_type_rest_param rest in
      let comments' = this#syntax_opt comments in
      if params_list == params_list' && rest == rest' && comments == comments' then
        params
      else
        (loc, { Params.params = params_list'; rest = rest'; comments = comments' })

    method component_type_param (param : ('loc, 'loc) Ast.Type.Component.Param.t) =
      let open Ast.Type.Component.Param in
      let (loc, { name; annot; optional }) = param in
      let name' = this#component_param_name name in
      let annot' = this#type_annotation annot in
      if name == name' && annot == annot' then
        param
      else
        (loc, { name = name'; annot = annot'; optional })

    method component_type_rest_param (expr : ('loc, 'loc) Ast.Type.Component.RestParam.t) =
      let open Ast.Type.Component.RestParam in
      let (loc, { argument; annot; optional; comments }) = expr in
      let argument' = map_opt this#identifier argument in
      let annot' = this#type_ annot in
      let comments' = this#syntax_opt comments in
      if argument == argument' && annot == annot' && comments == comments' then
        expr
      else
        (loc, { argument = argument'; annot = annot'; comments = comments'; optional })

    method declare_enum loc (enum : ('loc, 'loc) Ast.Statement.EnumDeclaration.t) =
      this#enum_declaration loc enum

    method declare_export_declaration
        _loc (decl : ('loc, 'loc) Ast.Statement.DeclareExportDeclaration.t) =
      let open Ast.Statement.DeclareExportDeclaration in
      let { default; source; specifiers; declaration; comments } = decl in
      let source' = map_loc_opt this#export_source source in
      let specifiers' = map_opt this#export_named_specifier specifiers in
      let declaration' = map_opt this#declare_export_declaration_decl declaration in
      let comments' = this#syntax_opt comments in
      if
        source == source'
        && specifiers == specifiers'
        && declaration == declaration'
        && comments == comments'
      then
        decl
      else
        {
          default;
          source = source';
          specifiers = specifiers';
          declaration = declaration';
          comments = comments';
        }

    method declare_export_declaration_decl
        (decl : ('loc, 'loc) Ast.Statement.DeclareExportDeclaration.declaration) =
      let open Ast.Statement.DeclareExportDeclaration in
      match decl with
      | Variable (loc, dv) ->
        let dv' = this#declare_variable loc dv in
        if dv' == dv then
          decl
        else
          Variable (loc, dv')
      | Function (loc, df) ->
        let df' = this#declare_function loc df in
        if df' == df then
          decl
        else
          Function (loc, df')
      | Class (loc, dc) ->
        let dc' = this#declare_class loc dc in
        if dc' == dc then
          decl
        else
          Class (loc, dc')
      | Component (loc, dc) ->
        let dc' = this#declare_component loc dc in
        if dc' == dc then
          decl
        else
          Component (loc, dc')
      | DefaultType t ->
        let t' = this#type_ t in
        if t' == t then
          decl
        else
          DefaultType t'
      | NamedType (loc, ta) ->
        let ta' = this#type_alias loc ta in
        if ta' == ta then
          decl
        else
          NamedType (loc, ta')
      | NamedOpaqueType (loc, ot) ->
        let ot' = this#opaque_type loc ot in
        if ot' == ot then
          decl
        else
          NamedOpaqueType (loc, ot')
      | Interface (loc, i) ->
        let i' = this#interface loc i in
        if i' == i then
          decl
        else
          Interface (loc, i')
      | Enum (loc, enum) ->
        let enum' = this#enum_declaration loc enum in
        if enum' == enum then
          decl
        else
          Enum (loc, enum')

    method declare_function _loc (decl : ('loc, 'loc) Ast.Statement.DeclareFunction.t) =
      let open Ast.Statement.DeclareFunction in
      let { id = ident; annot; predicate; comments } = decl in
      let id' = this#function_identifier ident in
      let annot' = this#type_annotation annot in
      let predicate' = map_opt this#predicate predicate in
      let comments' = this#syntax_opt comments in
      if id' == ident && annot' == annot && predicate' == predicate && comments' == comments then
        decl
      else
        { id = id'; annot = annot'; predicate = predicate'; comments = comments' }

    method declare_interface loc (decl : ('loc, 'loc) Ast.Statement.Interface.t) =
      this#interface loc decl

    method declare_module _loc (m : ('loc, 'loc) Ast.Statement.DeclareModule.t) =
      let open Ast.Statement.DeclareModule in
      let { id; body; comments } = m in
      let body' = map_loc this#block body in
      let comments' = this#syntax_opt comments in
      if body' == body && comments == comments' then
        m
      else
        { id; body = body'; comments = comments' }

    method declare_module_exports _loc (exports : ('loc, 'loc) Ast.Statement.DeclareModuleExports.t)
        =
      let open Ast.Statement.DeclareModuleExports in
      let { annot; comments } = exports in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      if annot == annot' && comments == comments' then
        exports
      else
        { annot = annot'; comments = comments' }

    method declare_namespace _loc (m : ('loc, 'loc) Ast.Statement.DeclareNamespace.t) =
      let open Ast.Statement.DeclareNamespace in
      let { id; body; comments } = m in
      let id' =
        match id with
        | Global g_id ->
          let g_id' = this#identifier g_id in
          if g_id == g_id' then
            id
          else
            Global g_id'
        | Local p_id ->
          let p_id' = this#pattern_identifier ~kind:Ast.Variable.Const p_id in
          if p_id == p_id' then
            id
          else
            Local p_id'
      in
      let body' = map_loc this#block body in
      let comments' = this#syntax_opt comments in
      if id' == id && body' == body && comments == comments' then
        m
      else
        { id = id'; body = body'; comments = comments' }

    method declare_type_alias loc (decl : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      this#type_alias loc decl

    method declare_variable _loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      let { id = ident; annot; kind; comments } = decl in
      let id' = this#pattern_identifier ~kind ident in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      if id' == ident && annot' == annot && comments' == comments then
        decl
      else
        { id = id'; annot = annot'; kind; comments = comments' }

    method do_while _loc (stuff : ('loc, 'loc) Ast.Statement.DoWhile.t) =
      let open Ast.Statement.DoWhile in
      let { body; test; comments } = stuff in
      let body' = this#statement body in
      let test' = this#predicate_expression test in
      let comments' = this#syntax_opt comments in
      if body == body' && test == test' && comments == comments' then
        stuff
      else
        { body = body'; test = test'; comments = comments' }

    method empty _loc empty =
      let open Ast.Statement.Empty in
      let { comments } = empty in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        empty
      else
        { comments = comments' }

    method enum_declaration _loc (enum : ('loc, 'loc) Ast.Statement.EnumDeclaration.t) =
      let open Ast.Statement.EnumDeclaration in
      let { id = ident; body; comments } = enum in
      let id' = this#pattern_identifier ~kind:Ast.Variable.Const ident in
      let body' = this#enum_body body in
      let comments' = this#syntax_opt comments in
      if ident == id' && body == body' && comments == comments' then
        enum
      else
        { id = id'; body = body'; comments = comments' }

    method enum_body (body : 'loc Ast.Statement.EnumDeclaration.body) =
      let open Ast.Statement.EnumDeclaration in
      match body with
      | (loc, BooleanBody boolean_body) ->
        id this#enum_boolean_body boolean_body body (fun body -> (loc, BooleanBody body))
      | (loc, NumberBody number_body) ->
        id this#enum_number_body number_body body (fun body -> (loc, NumberBody body))
      | (loc, StringBody string_body) ->
        id this#enum_string_body string_body body (fun body -> (loc, StringBody body))
      | (loc, SymbolBody symbol_body) ->
        id this#enum_symbol_body symbol_body body (fun body -> (loc, SymbolBody body))
      | (loc, BigIntBody bigint_body) ->
        id this#enum_bigint_body bigint_body body (fun body -> (loc, BigIntBody body))

    method enum_boolean_body (body : 'loc Ast.Statement.EnumDeclaration.BooleanBody.t) =
      let open Ast.Statement.EnumDeclaration.BooleanBody in
      let { members; explicit_type = _; has_unknown_members = _; comments } = body in
      let members' = map_list this#enum_boolean_member members in
      let comments' = this#syntax_opt comments in
      if members == members' && comments == comments' then
        body
      else
        { body with members = members'; comments = comments' }

    method enum_number_body (body : 'loc Ast.Statement.EnumDeclaration.NumberBody.t) =
      let open Ast.Statement.EnumDeclaration.NumberBody in
      let { members; explicit_type = _; has_unknown_members = _; comments } = body in
      let members' = map_list this#enum_number_member members in
      let comments' = this#syntax_opt comments in
      if members == members' && comments == comments' then
        body
      else
        { body with members = members'; comments = comments' }

    method enum_string_body (body : 'loc Ast.Statement.EnumDeclaration.StringBody.t) =
      let open Ast.Statement.EnumDeclaration.StringBody in
      let { members; explicit_type = _; has_unknown_members = _; comments } = body in
      let members' =
        match members with
        | Defaulted m -> id (map_list this#enum_defaulted_member) m members (fun m -> Defaulted m)
        | Initialized m -> id (map_list this#enum_string_member) m members (fun m -> Initialized m)
      in
      let comments' = this#syntax_opt comments in
      if members == members' && comments == comments' then
        body
      else
        { body with members = members'; comments = comments' }

    method enum_symbol_body (body : 'loc Ast.Statement.EnumDeclaration.SymbolBody.t) =
      let open Ast.Statement.EnumDeclaration.SymbolBody in
      let { members; has_unknown_members = _; comments } = body in
      let members' = map_list this#enum_defaulted_member members in
      let comments' = this#syntax_opt comments in
      if members == members' && comments == comments' then
        body
      else
        { body with members = members'; comments = comments' }

    method enum_bigint_body (body : 'loc Ast.Statement.EnumDeclaration.BigIntBody.t) =
      let open Ast.Statement.EnumDeclaration.BigIntBody in
      let { members; explicit_type = _; has_unknown_members = _; comments } = body in
      let members' = map_list this#enum_bigint_member members in
      let comments' = this#syntax_opt comments in
      if members == members' && comments == comments' then
        body
      else
        { body with members = members'; comments = comments' }

    method enum_defaulted_member (member : 'loc Ast.Statement.EnumDeclaration.DefaultedMember.t) =
      let open Ast.Statement.EnumDeclaration.DefaultedMember in
      let (loc, { id = ident }) = member in
      let id' = this#enum_member_identifier ident in
      if ident == id' then
        member
      else
        (loc, { id = id' })

    method enum_boolean_member
        (member :
          ('loc Ast.BooleanLiteral.t, 'loc) Ast.Statement.EnumDeclaration.InitializedMember.t
          ) =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (loc, { id = ident; init }) = member in
      let id' = this#enum_member_identifier ident in
      if ident == id' then
        member
      else
        (loc, { id = id'; init })

    method enum_number_member
        (member : ('loc Ast.NumberLiteral.t, 'loc) Ast.Statement.EnumDeclaration.InitializedMember.t)
        =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (loc, { id = ident; init }) = member in
      let id' = this#enum_member_identifier ident in
      if ident == id' then
        member
      else
        (loc, { id = id'; init })

    method enum_string_member
        (member : ('loc Ast.StringLiteral.t, 'loc) Ast.Statement.EnumDeclaration.InitializedMember.t)
        =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (loc, { id = ident; init }) = member in
      let id' = this#enum_member_identifier ident in
      if ident == id' then
        member
      else
        (loc, { id = id'; init })

    method enum_bigint_member
        (member : ('loc Ast.BigIntLiteral.t, 'loc) Ast.Statement.EnumDeclaration.InitializedMember.t)
        =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (loc, { id = ident; init }) = member in
      let id' = this#enum_member_identifier ident in
      if ident == id' then
        member
      else
        (loc, { id = id'; init })

    method enum_member_identifier (id : ('loc, 'loc) Ast.Identifier.t) = this#identifier id

    method export_default_declaration
        _loc (decl : ('loc, 'loc) Ast.Statement.ExportDefaultDeclaration.t) =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default; declaration; comments } = decl in
      let declaration' = this#export_default_declaration_decl declaration in
      let comments' = this#syntax_opt comments in
      if declaration' == declaration && comments' == comments then
        decl
      else
        { default; declaration = declaration'; comments = comments' }

    method export_default_declaration_decl
        (decl : ('loc, 'loc) Ast.Statement.ExportDefaultDeclaration.declaration) =
      let open Ast.Statement.ExportDefaultDeclaration in
      match decl with
      | Declaration stmt -> id this#statement stmt decl (fun stmt -> Declaration stmt)
      | Expression expr -> id this#expression expr decl (fun expr -> Expression expr)

    method export_named_declaration _loc (decl : ('loc, 'loc) Ast.Statement.ExportNamedDeclaration.t)
        =
      let open Ast.Statement.ExportNamedDeclaration in
      let { export_kind; source; specifiers; declaration; comments } = decl in
      let source' = map_loc_opt this#export_source source in
      let specifiers' = map_opt this#export_named_specifier specifiers in
      let declaration' = map_opt this#statement declaration in
      let comments' = this#syntax_opt comments in
      if
        source == source'
        && specifiers == specifiers'
        && declaration == declaration'
        && comments == comments'
      then
        decl
      else
        {
          export_kind;
          source = source';
          specifiers = specifiers';
          declaration = declaration';
          comments = comments';
        }

    method export_named_declaration_specifier
        (spec : ('loc, 'loc) Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t) =
      let open Ast.Statement.ExportNamedDeclaration.ExportSpecifier in
      let (loc, { local; exported; from_remote; imported_name_def_loc }) = spec in
      let local' = this#identifier local in
      let exported' = map_opt this#identifier exported in
      if local == local' && exported == exported' then
        spec
      else
        (loc, { local = local'; exported = exported'; from_remote; imported_name_def_loc })

    method export_batch_specifier
        (spec : ('loc, 'loc) Ast.Statement.ExportNamedDeclaration.ExportBatchSpecifier.t) =
      let (loc, id_opt) = spec in
      let id_opt' = map_opt this#identifier id_opt in
      if id_opt == id_opt' then
        spec
      else
        (loc, id_opt')

    method export_named_specifier
        (spec : ('loc, 'loc) Ast.Statement.ExportNamedDeclaration.specifier) =
      let open Ast.Statement.ExportNamedDeclaration in
      match spec with
      | ExportSpecifiers spec_list ->
        let spec_list' = map_list this#export_named_declaration_specifier spec_list in
        if spec_list == spec_list' then
          spec
        else
          ExportSpecifiers spec_list'
      | ExportBatchSpecifier batch ->
        let batch' = this#export_batch_specifier batch in
        if batch == batch' then
          spec
        else
          ExportBatchSpecifier batch'

    method export_source _loc (source : 'loc Ast.StringLiteral.t) =
      let open Ast.StringLiteral in
      let { value; raw; comments } = source in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        source
      else
        { value; raw; comments = comments' }

    method expression_statement _loc (stmt : ('loc, 'loc) Ast.Statement.Expression.t) =
      let open Ast.Statement.Expression in
      let { expression = expr; directive; comments } = stmt in
      let expr' = this#expression expr in
      let comments' = this#syntax_opt comments in
      if expr == expr' && comments == comments' then
        stmt
      else
        { expression = expr'; directive; comments = comments' }

    method expression_or_spread expr_or_spread =
      let open Ast.Expression in
      match expr_or_spread with
      | Expression expr -> id this#expression expr expr_or_spread (fun expr -> Expression expr)
      | Spread spread -> id this#spread_element spread expr_or_spread (fun spread -> Spread spread)

    method for_in_statement _loc (stmt : ('loc, 'loc) Ast.Statement.ForIn.t) =
      let open Ast.Statement.ForIn in
      let { left; right; body; each; comments } = stmt in
      let left' = this#for_in_statement_lhs left in
      let right' = this#expression right in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      if left == left' && right == right' && body == body' && comments == comments' then
        stmt
      else
        { left = left'; right = right'; body = body'; each; comments = comments' }

    method for_in_statement_lhs (left : ('loc, 'loc) Ast.Statement.ForIn.left) =
      let open Ast.Statement.ForIn in
      match left with
      | LeftDeclaration decl ->
        id this#for_in_left_declaration decl left (fun decl -> LeftDeclaration decl)
      | LeftPattern patt ->
        id this#for_in_assignment_pattern patt left (fun patt -> LeftPattern patt)

    method for_in_left_declaration left =
      let (loc, decl) = left in
      id_loc this#variable_declaration loc decl left (fun decl -> (loc, decl))

    method for_of_statement _loc (stuff : ('loc, 'loc) Ast.Statement.ForOf.t) =
      let open Ast.Statement.ForOf in
      let { left; right; body; await; comments } = stuff in
      let left' = this#for_of_statement_lhs left in
      let right' = this#expression right in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      if left == left' && right == right' && body == body' && comments == comments' then
        stuff
      else
        { left = left'; right = right'; body = body'; await; comments = comments' }

    method for_of_statement_lhs (left : ('loc, 'loc) Ast.Statement.ForOf.left) =
      let open Ast.Statement.ForOf in
      match left with
      | LeftDeclaration decl ->
        id this#for_of_left_declaration decl left (fun decl -> LeftDeclaration decl)
      | LeftPattern patt ->
        id this#for_of_assignment_pattern patt left (fun patt -> LeftPattern patt)

    method for_of_left_declaration left =
      let (loc, decl) = left in
      id_loc this#variable_declaration loc decl left (fun decl -> (loc, decl))

    method for_statement _loc (stmt : ('loc, 'loc) Ast.Statement.For.t) =
      let open Ast.Statement.For in
      let { init; test; update; body; comments } = stmt in
      let init' = map_opt this#for_statement_init init in
      let test' = map_opt this#predicate_expression test in
      let update' = map_opt this#expression update in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      if
        init == init'
        && test == test'
        && update == update'
        && body == body'
        && comments == comments'
      then
        stmt
      else
        { init = init'; test = test'; update = update'; body = body'; comments = comments' }

    method for_statement_init (init : ('loc, 'loc) Ast.Statement.For.init) =
      let open Ast.Statement.For in
      match init with
      | InitDeclaration decl ->
        id this#for_init_declaration decl init (fun decl -> InitDeclaration decl)
      | InitExpression expr -> id this#expression expr init (fun expr -> InitExpression expr)

    method for_init_declaration init =
      let (loc, decl) = init in
      id_loc this#variable_declaration loc decl init (fun decl -> (loc, decl))

    method function_param_type (fpt : ('loc, 'loc) Ast.Type.Function.Param.t) =
      let open Ast.Type.Function.Param in
      let (loc, { annot; name; optional }) = fpt in
      let annot' = this#type_ annot in
      let name' = map_opt this#identifier name in
      if annot' == annot && name' == name then
        fpt
      else
        (loc, { annot = annot'; name = name'; optional })

    method function_rest_param_type (frpt : ('loc, 'loc) Ast.Type.Function.RestParam.t) =
      let open Ast.Type.Function.RestParam in
      let (loc, { argument; comments }) = frpt in
      let argument' = this#function_param_type argument in
      let comments' = this#syntax_opt comments in
      if argument' == argument && comments' == comments then
        frpt
      else
        (loc, { argument = argument'; comments = comments' })

    method function_this_param_type (this_param : ('loc, 'loc) Ast.Type.Function.ThisParam.t) =
      let open Ast.Type.Function.ThisParam in
      let (loc, { annot; comments }) = this_param in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      if annot' == annot && comments' == comments then
        this_param
      else
        (loc, { annot = annot'; comments = comments' })

    method function_type_return_annotation
        (return : ('loc, 'loc) Ast.Type.Function.return_annotation) =
      let open Ast.Type.Function in
      match return with
      | TypeAnnotation t -> id this#type_ t return (fun rt -> TypeAnnotation rt)
      | TypeGuard g -> id this#type_guard g return (fun tg -> TypeGuard tg)

    method function_type _loc (ft : ('loc, 'loc) Ast.Type.Function.t) =
      let open Ast.Type.Function in
      let {
        params = (params_loc, { Params.this_; params = ps; rest = rpo; comments = params_comments });
        return;
        tparams;
        comments = func_comments;
        effect_;
      } =
        ft
      in
      let tparams' = map_opt (this#type_params ~kind:FunctionTypeTP) tparams in
      let this_' = map_opt this#function_this_param_type this_ in
      let ps' = map_list this#function_param_type ps in
      let rpo' = map_opt this#function_rest_param_type rpo in
      let return' = this#function_type_return_annotation return in
      let func_comments' = this#syntax_opt func_comments in
      let params_comments' = this#syntax_opt params_comments in
      if
        ps' == ps
        && rpo' == rpo
        && return' == return
        && tparams' == tparams
        && func_comments' == func_comments
        && params_comments' == params_comments
        && this_' == this_
      then
        ft
      else
        {
          params =
            ( params_loc,
              { Params.this_ = this_'; params = ps'; rest = rpo'; comments = params_comments' }
            );
          return = return';
          tparams = tparams';
          comments = func_comments';
          effect_;
        }

    method label_identifier (ident : ('loc, 'loc) Ast.Identifier.t) = this#identifier ident

    method object_property_value_type (opvt : ('loc, 'loc) Ast.Type.Object.Property.value) =
      let open Ast.Type.Object.Property in
      match opvt with
      | Init t -> id this#type_ t opvt (fun t -> Init t)
      | Get t -> id this#object_type_property_getter t opvt (fun t -> Get t)
      | Set t -> id this#object_type_property_setter t opvt (fun t -> Set t)

    method object_type_property_getter getter =
      let (loc, ft) = getter in
      id_loc this#function_type loc ft getter (fun ft -> (loc, ft))

    method object_type_property_setter setter =
      let (loc, ft) = setter in
      id_loc this#function_type loc ft setter (fun ft -> (loc, ft))

    method object_property_type (opt : ('loc, 'loc) Ast.Type.Object.Property.t) =
      let open Ast.Type.Object.Property in
      let (loc, { key; value; optional; static; proto; _method; variance; comments }) = opt in
      let key' = this#object_key key in
      let value' = this#object_property_value_type value in
      let variance' = this#variance_opt variance in
      let comments' = this#syntax_opt comments in
      if key' == key && value' == value && variance' == variance && comments' == comments then
        opt
      else
        ( loc,
          {
            key = key';
            value = value';
            optional;
            static;
            proto;
            _method;
            variance = variance';
            comments = comments';
          }
        )

    method object_spread_property_type (opt : ('loc, 'loc) Ast.Type.Object.SpreadProperty.t) =
      let open Ast.Type.Object.SpreadProperty in
      let (loc, { argument; comments }) = opt in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      if argument' == argument && comments == comments' then
        opt
      else
        (loc, { argument = argument'; comments = comments' })

    method object_indexer_property_type (opt : ('loc, 'loc) Ast.Type.Object.Indexer.t) =
      let open Ast.Type.Object.Indexer in
      let (loc, { id; key; value; static; variance; comments }) = opt in
      let key' = this#type_ key in
      let value' = this#type_ value in
      let variance' = this#variance_opt variance in
      let comments' = this#syntax_opt comments in
      if key' == key && value' == value && variance' == variance && comments' == comments then
        opt
      else
        (loc, { id; key = key'; value = value'; static; variance = variance'; comments = comments' })

    method object_internal_slot_property_type (slot : ('loc, 'loc) Ast.Type.Object.InternalSlot.t) =
      let open Ast.Type.Object.InternalSlot in
      let (loc, { id; value; optional; static; _method; comments }) = slot in
      let id' = this#identifier id in
      let value' = this#type_ value in
      let comments' = this#syntax_opt comments in
      if id == id' && value == value' && comments == comments' then
        slot
      else
        (loc, { id = id'; value = value'; optional; static; _method; comments = comments' })

    method object_call_property_type (call : ('loc, 'loc) Ast.Type.Object.CallProperty.t) =
      let open Ast.Type.Object.CallProperty in
      let (loc, { value = (value_loc, value); static; comments }) = call in
      let value' = this#function_type value_loc value in
      let comments' = this#syntax_opt comments in
      if value == value' && comments == comments' then
        call
      else
        (loc, { value = (value_loc, value'); static; comments = comments' })

    method object_mapped_type_property (mt : ('loc, 'loc) Ast.Type.Object.MappedType.t) =
      let open Ast.Type.Object.MappedType in
      let (loc, { key_tparam; prop_type; source_type; variance; comments; optional }) = mt in
      let key_tparam' = this#type_param ~kind:ObjectMappedTypeTP key_tparam in
      let prop_type' = this#type_ prop_type in
      let source_type' = this#type_ source_type in
      let variance' = this#variance_opt variance in
      let comments' = this#syntax_opt comments in
      if
        key_tparam' == key_tparam
        && prop_type' == prop_type
        && source_type' == source_type
        && variance' == variance
        && comments' == comments
      then
        mt
      else
        ( loc,
          {
            key_tparam = key_tparam';
            prop_type = prop_type';
            source_type = source_type';
            variance = variance';
            comments = comments';
            optional;
          }
        )

    method object_type _loc (ot : ('loc, 'loc) Ast.Type.Object.t) =
      let open Ast.Type.Object in
      let { properties; exact; inexact; comments } = ot in
      let properties' = map_list this#object_type_property properties in
      let comments' = this#syntax_opt comments in
      if properties' == properties && comments == comments' then
        ot
      else
        { properties = properties'; exact; inexact; comments = comments' }

    method object_type_property (p : ('loc, 'loc) Ast.Type.Object.property) =
      let open Ast.Type.Object in
      match p with
      | Property p' -> id this#object_property_type p' p (fun p' -> Property p')
      | SpreadProperty p' -> id this#object_spread_property_type p' p (fun p' -> SpreadProperty p')
      | Indexer p' -> id this#object_indexer_property_type p' p (fun p' -> Indexer p')
      | InternalSlot p' ->
        id this#object_internal_slot_property_type p' p (fun p' -> InternalSlot p')
      | CallProperty p' -> id this#object_call_property_type p' p (fun p' -> CallProperty p')
      | MappedType p' -> id this#object_mapped_type_property p' p (fun p' -> MappedType p')

    method interface_type _loc (i : ('loc, 'loc) Ast.Type.Interface.t) =
      let open Ast.Type.Interface in
      let { extends; body; comments } = i in
      let extends' = map_list (map_loc this#generic_type) extends in
      let body' = map_loc this#object_type body in
      let comments' = this#syntax_opt comments in
      if extends' == extends && body' == body && comments == comments' then
        i
      else
        { extends = extends'; body = body'; comments = comments' }

    method generic_identifier_type (git : ('loc, 'loc) Ast.Type.Generic.Identifier.t) =
      let open Ast.Type.Generic.Identifier in
      match git with
      | Unqualified i -> id this#type_identifier_reference i git (fun i -> Unqualified i)
      | Qualified i -> id this#generic_qualified_identifier_type i git (fun i -> Qualified i)

    method generic_qualified_identifier_type qual =
      let open Ast.Type.Generic.Identifier in
      let (loc, { qualification; id }) = qual in
      let qualification' = this#generic_identifier_type qualification in
      let id' = this#member_type_identifier id in
      if qualification' == qualification && id' == id then
        qual
      else
        (loc, { qualification = qualification'; id = id' })

    method member_type_identifier id = this#identifier id

    method variance (variance : 'loc Ast.Variance.t) =
      let (loc, { Ast.Variance.kind; comments }) = variance in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        variance
      else
        (loc, { Ast.Variance.kind; comments = comments' })

    method variance_opt (opt : 'loc Ast.Variance.t option) = map_opt this#variance opt

    method tparam_const_modifier (c : 'loc Ast.Type.TypeParam.ConstModifier.t) =
      let (loc, comments) = c in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        c
      else
        (loc, comments')

    method type_args (targs : ('loc, 'loc) Ast.Type.TypeArgs.t) =
      let open Ast.Type.TypeArgs in
      let (loc, { arguments; comments }) = targs in
      let arguments' = map_list this#type_ arguments in
      let comments' = this#syntax_opt comments in
      if arguments == arguments' && comments == comments' then
        targs
      else
        (loc, { arguments = arguments'; comments = comments' })

    method type_params ~kind (tparams : ('loc, 'loc) Ast.Type.TypeParams.t) =
      let open Ast.Type.TypeParams in
      let (loc, { params = tps; comments }) = tparams in
      let tps' = map_list (this#type_param ~kind) tps in
      let comments' = this#syntax_opt comments in
      if tps' == tps && comments' == comments then
        tparams
      else
        (loc, { params = tps'; comments = comments' })

    method type_param ~kind:_ (tparam : ('loc, 'loc) Ast.Type.TypeParam.t) =
      let open Ast.Type.TypeParam in
      let (loc, { name; bound; bound_kind; variance; default; const }) = tparam in
      let bound' = this#type_annotation_hint bound in
      let variance' = this#variance_opt variance in
      let default' = map_opt this#type_ default in
      let const' = map_opt this#tparam_const_modifier const in
      let name' = this#binding_type_identifier name in
      if
        name' == name
        && bound' == bound
        && variance' == variance
        && default' == default
        && const' == const
      then
        tparam
      else
        ( loc,
          {
            name = name';
            bound = bound';
            bound_kind;
            variance = variance';
            default = default';
            const = const';
          }
        )

    method generic_type _loc (gt : ('loc, 'loc) Ast.Type.Generic.t) =
      let open Ast.Type.Generic in
      let { id; targs; comments } = gt in
      let id' = this#generic_identifier_type id in
      let targs' = map_opt this#type_args targs in
      let comments' = this#syntax_opt comments in
      if id' == id && targs' == targs && comments' == comments then
        gt
      else
        { id = id'; targs = targs'; comments = comments' }

    method indexed_access_type _loc (ia : ('loc, 'loc) Ast.Type.IndexedAccess.t) =
      let open Ast.Type.IndexedAccess in
      let { _object; index; comments } = ia in
      let _object' = this#type_ _object in
      let index' = this#type_ index in
      let comments' = this#syntax_opt comments in
      if _object' == _object && index' == index && comments' == comments then
        ia
      else
        { _object = _object'; index = index'; comments = comments' }

    method optional_indexed_access_type loc (ia : ('loc, 'loc) Ast.Type.OptionalIndexedAccess.t) =
      let open Ast.Type.OptionalIndexedAccess in
      let { indexed_access; optional } = ia in
      let indexed_access' = this#indexed_access_type loc indexed_access in
      if indexed_access' == indexed_access then
        ia
      else
        { indexed_access = indexed_access'; optional }

    method string_literal _loc (lit : 'loc Ast.StringLiteral.t) =
      let open Ast.StringLiteral in
      let { value; raw; comments } = lit in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        lit
      else
        { value; raw; comments = comments' }

    method number_literal _loc (lit : 'loc Ast.NumberLiteral.t) =
      let open Ast.NumberLiteral in
      let { value; raw; comments } = lit in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        lit
      else
        { value; raw; comments = comments' }

    method bigint_literal _loc (lit : 'loc Ast.BigIntLiteral.t) =
      let open Ast.BigIntLiteral in
      let { value; raw; comments } = lit in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        lit
      else
        { value; raw; comments = comments' }

    method boolean_literal _loc (lit : 'loc Ast.BooleanLiteral.t) =
      let open Ast.BooleanLiteral in
      let { value; comments } = lit in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        lit
      else
        { value; comments = comments' }

    method null_literal _loc comments = this#syntax_opt comments

    method regexp_literal _loc (lit : 'loc Ast.RegExpLiteral.t) =
      let open Ast.RegExpLiteral in
      let { pattern; flags; raw; comments } = lit in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        lit
      else
        { pattern; flags; raw; comments = comments' }

    method module_ref_literal _loc (lit : ('loc, 'loc) Ast.ModuleRefLiteral.t) =
      let open Ast.ModuleRefLiteral in
      let { value; require_loc; def_loc_opt; prefix_len; raw; comments } = lit in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        lit
      else
        { value; require_loc; def_loc_opt; prefix_len; raw; comments }

    method nullable_type (t : ('loc, 'loc) Ast.Type.Nullable.t) =
      let open Ast.Type.Nullable in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        t
      else
        { argument = argument'; comments = comments' }

    method conditional_type (t : ('loc, 'loc) Ast.Type.Conditional.t) =
      let open Ast.Type.Conditional in
      let { check_type; extends_type; true_type; false_type; comments } = t in
      let check_type' = this#type_ check_type in
      let extends_type' = this#type_ extends_type in
      let true_type' = this#type_ true_type in
      let false_type' = this#type_ false_type in
      let comments' = this#syntax_opt comments in
      if
        check_type == check_type'
        && extends_type == extends_type'
        && true_type == true_type'
        && false_type == false_type'
        && comments == comments'
      then
        t
      else
        {
          check_type = check_type';
          extends_type = extends_type';
          true_type = true_type';
          false_type = false_type';
          comments = comments';
        }

    method infer_type (t : ('loc, 'loc) Ast.Type.Infer.t) =
      let open Ast.Type.Infer in
      let { tparam; comments } = t in
      let tparam' = this#type_param ~kind:InferTP tparam in
      let comments' = this#syntax_opt comments in
      if tparam == tparam' && comments == comments' then
        t
      else
        { tparam = tparam'; comments = comments' }

    method typeof_type (t : ('loc, 'loc) Ast.Type.Typeof.t) =
      let open Ast.Type.Typeof in
      let { argument; targs; comments } = t in
      let argument' = this#typeof_expression argument in
      let targs' = map_opt this#type_args targs in
      let comments' = this#syntax_opt comments in
      if argument == argument' && targs = targs' && comments == comments' then
        t
      else
        { argument = argument'; targs = targs'; comments = comments' }

    method typeof_expression (git : ('loc, 'loc) Ast.Type.Typeof.Target.t) =
      let open Ast.Type.Typeof.Target in
      match git with
      | Unqualified i -> id this#typeof_identifier i git (fun i -> Unqualified i)
      | Qualified i -> id this#typeof_qualified_identifier i git (fun i -> Qualified i)

    method typeof_identifier id = this#identifier id

    method typeof_member_identifier id = this#identifier id

    method typeof_qualified_identifier qual =
      let open Ast.Type.Typeof.Target in
      let (loc, { qualification; id }) = qual in
      let qualification' = this#typeof_expression qualification in
      let id' = this#typeof_member_identifier id in
      if qualification' == qualification && id' == id then
        qual
      else
        (loc, { qualification = qualification'; id = id' })

    method keyof_type (t : ('loc, 'loc) Ast.Type.Keyof.t) =
      let open Ast.Type.Keyof in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        t
      else
        { argument = argument'; comments = comments' }

    method render_type (t : ('loc, 'loc) Ast.Type.Renders.t) =
      let open Ast.Type.Renders in
      let { operator_loc; argument; variant; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        t
      else
        { operator_loc; argument = argument'; comments = comments'; variant }

    method readonly_type (t : ('loc, 'loc) Ast.Type.ReadOnly.t) =
      let open Ast.Type.ReadOnly in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        t
      else
        { argument = argument'; comments = comments' }

    method tuple_type (t : ('loc, 'loc) Ast.Type.Tuple.t) =
      let open Ast.Type.Tuple in
      let { elements; inexact; comments } = t in
      let elements' = map_list this#tuple_element elements in
      let comments' = this#syntax_opt comments in
      if elements == elements' && comments == comments' then
        t
      else
        { elements = elements'; inexact; comments = comments' }

    method tuple_element (el : ('loc, 'loc) Ast.Type.Tuple.element) =
      let open Ast.Type.Tuple in
      match el with
      | (loc, UnlabeledElement t) -> id this#type_ t el (fun t -> (loc, UnlabeledElement t))
      | (loc, LabeledElement e) ->
        id this#tuple_labeled_element e el (fun e -> (loc, LabeledElement e))
      | (loc, SpreadElement e) -> id this#tuple_spread_element e el (fun e -> (loc, SpreadElement e))

    method tuple_labeled_element (t : ('loc, 'loc) Ast.Type.Tuple.LabeledElement.t) =
      let open Ast.Type.Tuple.LabeledElement in
      (* Tuple element labels are not bindings so don't map over `name`. *)
      let { annot; name; variance; optional } = t in
      let annot' = this#type_ annot in
      let variance' = this#variance_opt variance in
      if annot' == annot && variance' == variance then
        t
      else
        { annot = annot'; name; variance = variance'; optional }

    method tuple_spread_element (t : ('loc, 'loc) Ast.Type.Tuple.SpreadElement.t) =
      let open Ast.Type.Tuple.SpreadElement in
      let { annot; name } = t in
      let annot' = this#type_ annot in
      if annot' == annot then
        t
      else
        { annot = annot'; name }

    method array_type (t : ('loc, 'loc) Ast.Type.Array.t) =
      let open Ast.Type.Array in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        t
      else
        { argument = argument'; comments = comments' }

    method union_type _loc (t : ('loc, 'loc) Ast.Type.Union.t) =
      let open Ast.Type.Union in
      let { types = (t0, t1, ts); comments } = t in
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = map_list this#type_ ts in
      let comments' = this#syntax_opt comments in
      if t0' == t0 && t1' == t1 && ts' == ts && comments' == comments then
        t
      else
        { types = (t0', t1', ts'); comments = comments' }

    method intersection_type _loc (t : ('loc, 'loc) Ast.Type.Intersection.t) =
      let open Ast.Type.Intersection in
      let { types = (t0, t1, ts); comments } = t in
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = map_list this#type_ ts in
      let comments' = this#syntax_opt comments in
      if t0' == t0 && t1' == t1 && ts' == ts && comments' == comments then
        t
      else
        { types = (t0', t1', ts'); comments = comments' }

    method type_ (t : ('loc, 'loc) Ast.Type.t) =
      let open Ast.Type in
      match t with
      | (loc, Any comments) -> id this#syntax_opt comments t (fun comments -> (loc, Any comments))
      | (loc, Mixed comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, Mixed comments))
      | (loc, Empty comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, Empty comments))
      | (loc, Void comments) -> id this#syntax_opt comments t (fun comments -> (loc, Void comments))
      | (loc, Null comments) -> id this#syntax_opt comments t (fun comments -> (loc, Null comments))
      | (loc, Symbol comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, Symbol comments))
      | (loc, Number comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, Number comments))
      | (loc, BigInt comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, BigInt comments))
      | (loc, String comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, String comments))
      | (loc, Boolean { raw; comments }) ->
        id this#syntax_opt comments t (fun comments -> (loc, Boolean { raw; comments }))
      | (loc, Exists comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, Exists comments))
      | (loc, Unknown comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, Unknown comments))
      | (loc, Never comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, Never comments))
      | (loc, Undefined comments) ->
        id this#syntax_opt comments t (fun comments -> (loc, Undefined comments))
      | (loc, Nullable t') -> id this#nullable_type t' t (fun t' -> (loc, Nullable t'))
      | (loc, Array t') -> id this#array_type t' t (fun t' -> (loc, Array t'))
      | (loc, Conditional t') -> id this#conditional_type t' t (fun t' -> (loc, Conditional t'))
      | (loc, Infer t') -> id this#infer_type t' t (fun t' -> (loc, Infer t'))
      | (loc, Typeof t') -> id this#typeof_type t' t (fun t' -> (loc, Typeof t'))
      | (loc, Keyof t') -> id this#keyof_type t' t (fun t' -> (loc, Keyof t'))
      | (loc, Renders t') -> id this#render_type t' t (fun t' -> (loc, Renders t'))
      | (loc, ReadOnly t') -> id this#readonly_type t' t (fun t' -> (loc, ReadOnly t'))
      | (loc, Function ft) -> id_loc this#function_type loc ft t (fun ft -> (loc, Function ft))
      | (loc, Component ct) -> id_loc this#component_type loc ct t (fun ct -> (loc, Component ct))
      | (loc, Object ot) -> id_loc this#object_type loc ot t (fun ot -> (loc, Object ot))
      | (loc, Interface i) -> id_loc this#interface_type loc i t (fun i -> (loc, Interface i))
      | (loc, Generic gt) -> id_loc this#generic_type loc gt t (fun gt -> (loc, Generic gt))
      | (loc, IndexedAccess ia) ->
        id_loc this#indexed_access_type loc ia t (fun ia -> (loc, IndexedAccess ia))
      | (loc, OptionalIndexedAccess ia) ->
        id_loc this#optional_indexed_access_type loc ia t (fun ia -> (loc, OptionalIndexedAccess ia))
      | (loc, StringLiteral lit) ->
        id_loc this#string_literal loc lit t (fun lit -> (loc, StringLiteral lit))
      | (loc, NumberLiteral lit) ->
        id_loc this#number_literal loc lit t (fun lit -> (loc, NumberLiteral lit))
      | (loc, BigIntLiteral lit) ->
        id_loc this#bigint_literal loc lit t (fun lit -> (loc, BigIntLiteral lit))
      | (loc, BooleanLiteral lit) ->
        id_loc this#boolean_literal loc lit t (fun lit -> (loc, BooleanLiteral lit))
      | (loc, Union t') -> id_loc this#union_type loc t' t (fun t' -> (loc, Union t'))
      | (loc, Intersection t') ->
        id_loc this#intersection_type loc t' t (fun t' -> (loc, Intersection t'))
      | (loc, Tuple t') -> id this#tuple_type t' t (fun t' -> (loc, Tuple t'))

    method type_annotation (annot : ('loc, 'loc) Ast.Type.annotation) =
      let (loc, a) = annot in
      id this#type_ a annot (fun a -> (loc, a))

    method type_annotation_hint (return : ('M, 'T) Ast.Type.annotation_or_hint) =
      let open Ast.Type in
      match return with
      | Available annot -> id this#type_annotation annot return (fun a -> Available a)
      | Missing _loc -> return

    method component_renders_annotation (renders : ('M, 'T) Ast.Type.component_renders_annotation) =
      let open Ast.Type in
      match renders with
      | AvailableRenders (loc, render_type) ->
        let render_type' = this#render_type render_type in
        if render_type' == render_type then
          renders
        else
          AvailableRenders (loc, render_type')
      | MissingRenders _loc -> renders

    method function_declaration loc (stmt : ('loc, 'loc) Ast.Function.t) = this#function_ loc stmt

    method function_expression loc (stmt : ('loc, 'loc) Ast.Function.t) =
      this#function_expression_or_method loc stmt

    (** previously, we conflated [function_expression] and [class_method]. callers should be
        updated to override those individually.

        DEPRECATED: use either function_expression or class_method *)
    method function_expression_or_method loc (stmt : ('loc, 'loc) Ast.Function.t) =
      this#function_ loc stmt

    (* Internal helper for function declarations, function expressions and arrow functions *)
    method function_ _loc (expr : ('loc, 'loc) Ast.Function.t) =
      let open Ast.Function in
      let {
        id = ident;
        params;
        body;
        async;
        generator;
        effect_;
        predicate;
        return;
        tparams;
        sig_loc;
        comments;
      } =
        expr
      in
      let ident' = map_opt this#function_identifier ident in
      let tparams' = map_opt (this#type_params ~kind:FunctionTP) tparams in
      let params' = this#function_params params in
      let return' = this#function_return_annotation return in
      let body' = this#function_body_any body in
      let predicate' = map_opt this#predicate predicate in
      let comments' = this#syntax_opt comments in
      if
        ident == ident'
        && params == params'
        && body == body'
        && predicate == predicate'
        && return == return'
        && tparams == tparams'
        && comments == comments'
      then
        expr
      else
        {
          id = ident';
          params = params';
          return = return';
          body = body';
          async;
          generator;
          effect_;
          predicate = predicate';
          tparams = tparams';
          sig_loc;
          comments = comments';
        }

    method function_params (params : ('loc, 'loc) Ast.Function.Params.t) =
      let open Ast.Function in
      let (loc, { Params.params = params_list; rest; comments; this_ }) = params in
      let params_list' = map_list this#function_param params_list in
      let rest' = map_opt this#function_rest_param rest in
      let this_' = map_opt this#function_this_param this_ in
      let comments' = this#syntax_opt comments in
      if params_list == params_list' && rest == rest' && comments == comments' && this_ == this_'
      then
        params
      else
        (loc, { Params.params = params_list'; rest = rest'; comments = comments'; this_ = this_' })

    method function_this_param (this_param : ('loc, 'loc) Ast.Function.ThisParam.t) =
      let open Ast.Function.ThisParam in
      let (loc, { annot; comments }) = this_param in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      if annot' == annot && comments' == comments then
        this_param
      else
        (loc, { annot = annot'; comments = comments' })

    method function_param (param : ('loc, 'loc) Ast.Function.Param.t) =
      let open Ast.Function.Param in
      let (loc, { argument; default }) = param in
      let argument' = this#function_param_pattern argument in
      let default' = this#default_opt default in
      if argument == argument' && default == default' then
        param
      else
        (loc, { argument = argument'; default = default' })

    method function_return_annotation (return : ('loc, 'loc) Ast.Function.ReturnAnnot.t) =
      let open Ast.Function.ReturnAnnot in
      match return with
      | Missing _loc -> return
      | Available t -> id this#type_annotation t return (fun rt -> Available rt)
      | TypeGuard g -> id this#type_guard_annotation g return (fun tg -> TypeGuard tg)

    method function_body_any (body : ('loc, 'loc) Ast.Function.body) =
      match body with
      | Ast.Function.BodyBlock block ->
        id this#function_body block body (fun block -> Ast.Function.BodyBlock block)
      | Ast.Function.BodyExpression expr ->
        id this#body_expression expr body (fun expr -> Ast.Function.BodyExpression expr)

    method function_body (body : 'loc * ('loc, 'loc) Ast.Statement.Block.t) =
      let (loc, block) = body in
      id_loc this#block loc block body (fun block -> (loc, block))

    method body_expression (expr : ('loc, 'loc) Ast.Expression.t) = this#expression expr

    method function_identifier (ident : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ~kind:Ast.Variable.Var ident

    method identifier (id : ('loc, 'loc) Ast.Identifier.t) =
      let open Ast.Identifier in
      let (loc, { name; comments }) = id in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        id
      else
        (loc, { name; comments = comments' })

    method type_identifier (id : ('loc, 'loc) Ast.Identifier.t) = this#identifier id

    method type_identifier_reference (id : ('loc, 'loc) Ast.Identifier.t) = this#type_identifier id

    method binding_type_identifier (id : ('loc, 'loc) Ast.Identifier.t) = this#type_identifier id

    method interface _loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      let { id = ident; tparams; extends; body; comments } = interface in
      let id' = this#binding_type_identifier ident in
      let tparams' = map_opt (this#type_params ~kind:InterfaceTP) tparams in
      let extends' = map_list (map_loc this#generic_type) extends in
      let body' = map_loc this#object_type body in
      let comments' = this#syntax_opt comments in
      if
        id' == ident
        && tparams' == tparams
        && extends' == extends
        && body' == body
        && comments' == comments
      then
        interface
      else
        { id = id'; tparams = tparams'; extends = extends'; body = body'; comments = comments' }

    method interface_declaration loc (decl : ('loc, 'loc) Ast.Statement.Interface.t) =
      this#interface loc decl

    method private_name (id : 'loc Ast.PrivateName.t) =
      let open Ast.PrivateName in
      let (loc, { name; comments }) = id in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        id
      else
        (loc, { name; comments = comments' })

    method computed_key (key : ('loc, 'loc) Ast.ComputedKey.t) =
      let open Ast.ComputedKey in
      let (loc, { expression; comments }) = key in
      let expression' = this#expression expression in
      let comments' = this#syntax_opt comments in
      if expression == expression' && comments == comments' then
        key
      else
        (loc, { expression = expression'; comments = comments' })

    method import _loc (expr : ('loc, 'loc) Ast.Expression.Import.t) =
      let open Ast.Expression.Import in
      let { argument; comments } = expr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        expr
      else
        { argument = argument'; comments = comments' }

    method if_consequent_statement ~has_else (stmt : ('loc, 'loc) Ast.Statement.t) =
      ignore has_else;
      this#statement stmt

    method if_alternate_statement _loc (altern : ('loc, 'loc) Ast.Statement.If.Alternate.t') =
      let open Ast.Statement.If.Alternate in
      let { body; comments } = altern in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        altern
      else
        { body = body'; comments = comments' }

    method if_statement _loc (stmt : ('loc, 'loc) Ast.Statement.If.t) =
      let open Ast.Statement.If in
      let { test; consequent; alternate; comments } = stmt in
      let test' = this#predicate_expression test in
      let consequent' = this#if_consequent_statement ~has_else:(alternate <> None) consequent in
      let alternate' = map_opt (map_loc this#if_alternate_statement) alternate in
      let comments' = this#syntax_opt comments in
      if
        test == test'
        && consequent == consequent'
        && alternate == alternate'
        && comments == comments'
      then
        stmt
      else
        { test = test'; consequent = consequent'; alternate = alternate'; comments = comments' }

    method import_declaration _loc (decl : ('loc, 'loc) Ast.Statement.ImportDeclaration.t) =
      let open Ast.Statement.ImportDeclaration in
      let { import_kind; source; specifiers; default; comments } = decl in
      let source' = map_loc this#import_source source in
      let specifiers' = map_opt (this#import_specifier ~import_kind) specifiers in
      let default' =
        map_opt
          (fun ({ identifier; remote_default_name_def_loc } as id) ->
            let identifier' = this#import_default_specifier ~import_kind identifier in
            if identifier' == identifier then
              id
            else
              { identifier = identifier'; remote_default_name_def_loc })
          default
      in
      let comments' = this#syntax_opt comments in
      if
        source == source'
        && specifiers == specifiers'
        && default == default'
        && comments == comments'
      then
        decl
      else
        {
          import_kind;
          source = source';
          specifiers = specifiers';
          default = default';
          comments = comments';
        }

    method import_source _loc (source : 'loc Ast.StringLiteral.t) =
      let open Ast.StringLiteral in
      let { value; raw; comments } = source in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        source
      else
        { value; raw; comments = comments' }

    method import_specifier
        ~import_kind (specifier : ('loc, 'loc) Ast.Statement.ImportDeclaration.specifier) =
      let open Ast.Statement.ImportDeclaration in
      match specifier with
      | ImportNamedSpecifiers named_specifiers ->
        let named_specifiers' =
          map_list (this#import_named_specifier ~import_kind) named_specifiers
        in
        if named_specifiers == named_specifiers' then
          specifier
        else
          ImportNamedSpecifiers named_specifiers'
      | ImportNamespaceSpecifier (loc, ident) ->
        id_loc (this#import_namespace_specifier ~import_kind) loc ident specifier (fun ident ->
            ImportNamespaceSpecifier (loc, ident)
        )

    method remote_identifier id = this#identifier id

    method import_named_specifier
        ~(import_kind : Ast.Statement.ImportDeclaration.import_kind)
        (specifier : ('loc, 'loc) Ast.Statement.ImportDeclaration.named_specifier) =
      let open Ast.Statement.ImportDeclaration in
      let { kind; local; remote; remote_name_def_loc } = specifier in
      let (is_type_remote, is_type_local) =
        match (import_kind, kind) with
        | (ImportType, _)
        | (_, Some ImportType) ->
          (true, true)
        | (ImportTypeof, _)
        | (_, Some ImportTypeof) ->
          (false, true)
        | _ -> (false, false)
      in
      let remote' =
        match local with
        | None ->
          if is_type_remote then
            this#binding_type_identifier remote
          else
            this#pattern_identifier ~kind:Ast.Variable.Let remote
        | Some _ -> this#remote_identifier remote
      in
      let local' =
        match local with
        | None -> None
        | Some ident ->
          let local_visitor =
            if is_type_local then
              this#binding_type_identifier
            else
              this#pattern_identifier ~kind:Ast.Variable.Let
          in
          id local_visitor ident local (fun ident -> Some ident)
      in
      if local == local' && remote == remote' then
        specifier
      else
        { kind; local = local'; remote = remote'; remote_name_def_loc }

    method import_default_specifier ~import_kind (id : ('loc, 'loc) Ast.Identifier.t) =
      let open Ast.Statement.ImportDeclaration in
      let local_visitor =
        match import_kind with
        | ImportType
        | ImportTypeof ->
          this#binding_type_identifier
        | _ -> this#pattern_identifier ~kind:Ast.Variable.Let
      in
      local_visitor id

    method import_namespace_specifier ~import_kind _loc (id : ('loc, 'loc) Ast.Identifier.t) =
      let open Ast.Statement.ImportDeclaration in
      let local_visitor =
        match import_kind with
        | ImportType
        | ImportTypeof ->
          this#binding_type_identifier
        | _ -> this#pattern_identifier ~kind:Ast.Variable.Let
      in
      local_visitor id

    method jsx_element _loc (expr : ('loc, 'loc) Ast.JSX.element) =
      let open Ast.JSX in
      let { opening_element; closing_element; children; comments } = expr in
      let opening_element' = this#jsx_opening_element opening_element in
      let closing_element' = map_opt this#jsx_closing_element closing_element in
      let children' = this#jsx_children children in
      let comments' = this#syntax_opt comments in
      if
        opening_element == opening_element'
        && closing_element == closing_element'
        && children == children'
        && comments == comments'
      then
        expr
      else
        {
          opening_element = opening_element';
          closing_element = closing_element';
          children = children';
          comments = comments';
        }

    method jsx_fragment _loc (expr : ('loc, 'loc) Ast.JSX.fragment) =
      let open Ast.JSX in
      let { frag_children; frag_comments; _ } = expr in
      let children' = this#jsx_children frag_children in
      let frag_comments' = this#syntax_opt frag_comments in
      if frag_children == children' && frag_comments == frag_comments' then
        expr
      else
        { expr with frag_children = children'; frag_comments = frag_comments' }

    method jsx_opening_element (elem : ('loc, 'loc) Ast.JSX.Opening.t) =
      let open Ast.JSX.Opening in
      let (loc, { name; targs; self_closing; attributes }) = elem in
      let name' = this#jsx_element_name name in
      let targs' = map_opt this#call_type_args targs in
      let attributes' = map_list this#jsx_opening_attribute attributes in
      if name == name' && targs == targs' && attributes == attributes' then
        elem
      else
        (loc, { name = name'; targs = targs'; self_closing; attributes = attributes' })

    method jsx_closing_element (elem : ('loc, 'loc) Ast.JSX.Closing.t) =
      let open Ast.JSX.Closing in
      let (loc, { name }) = elem in
      let name' = this#jsx_element_name name in
      if name == name' then
        elem
      else
        (loc, { name = name' })

    method jsx_opening_attribute (jsx_attr : ('loc, 'loc) Ast.JSX.Opening.attribute) =
      let open Ast.JSX.Opening in
      match jsx_attr with
      | Attribute attr -> id this#jsx_attribute attr jsx_attr (fun attr -> Attribute attr)
      | SpreadAttribute (loc, attr) ->
        id_loc this#jsx_spread_attribute loc attr jsx_attr (fun attr -> SpreadAttribute (loc, attr))

    method jsx_spread_attribute _loc (attr : ('loc, 'loc) Ast.JSX.SpreadAttribute.t') =
      let open Ast.JSX.SpreadAttribute in
      let { argument; comments } = attr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        attr
      else
        { argument = argument'; comments = comments' }

    method jsx_attribute (attr : ('loc, 'loc) Ast.JSX.Attribute.t) =
      let open Ast.JSX.Attribute in
      let (loc, { name; value }) = attr in
      let name' = this#jsx_attribute_name name in
      let value' = map_opt this#jsx_attribute_value value in
      if name == name' && value == value' then
        attr
      else
        (loc, { name = name'; value = value' })

    method jsx_attribute_name (name : ('loc, 'loc) Ast.JSX.Attribute.name) =
      let open Ast.JSX.Attribute in
      match name with
      | Identifier ident ->
        id this#jsx_attribute_name_identifier ident name (fun ident -> Identifier ident)
      | NamespacedName ns ->
        id this#jsx_attribute_name_namespaced ns name (fun ns -> NamespacedName ns)

    method jsx_attribute_name_identifier ident = this#jsx_identifier ident

    method jsx_attribute_name_namespaced ns = this#jsx_namespaced_name ns

    method jsx_attribute_value (value : ('loc, 'loc) Ast.JSX.Attribute.value) =
      let open Ast.JSX.Attribute in
      match value with
      | StringLiteral (loc, lit) ->
        id_loc this#jsx_attribute_value_literal loc lit value (fun lit -> StringLiteral (loc, lit))
      | ExpressionContainer (loc, expr) ->
        id_loc this#jsx_attribute_value_expression loc expr value (fun expr ->
            ExpressionContainer (loc, expr)
        )

    method jsx_attribute_value_expression loc (jsx_expr : ('loc, 'loc) Ast.JSX.ExpressionContainer.t)
        =
      this#jsx_expression loc jsx_expr

    method jsx_attribute_value_literal loc (lit : 'loc Ast.StringLiteral.t) =
      this#string_literal loc lit

    method jsx_children ((loc, children) as orig : 'loc * ('loc, 'loc) Ast.JSX.child list) =
      let children' = map_list this#jsx_child children in
      if children == children' then
        orig
      else
        (loc, children')

    method jsx_child (child : ('loc, 'loc) Ast.JSX.child) =
      let open Ast.JSX in
      match child with
      | (loc, Element elem) ->
        id_loc this#jsx_element loc elem child (fun elem -> (loc, Element elem))
      | (loc, Fragment frag) ->
        id_loc this#jsx_fragment loc frag child (fun frag -> (loc, Fragment frag))
      | (loc, ExpressionContainer expr) ->
        id_loc this#jsx_expression loc expr child (fun expr -> (loc, ExpressionContainer expr))
      | (loc, SpreadChild spread) ->
        id this#jsx_spread_child spread child (fun spread -> (loc, SpreadChild spread))
      | (_loc, Text _) -> child

    method jsx_expression _loc (jsx_expr : ('loc, 'loc) Ast.JSX.ExpressionContainer.t) =
      let open Ast.JSX.ExpressionContainer in
      let { expression; comments } = jsx_expr in
      let comments' = this#syntax_opt comments in
      match expression with
      | Expression expr ->
        let expr' = this#expression expr in
        if expr == expr' && comments == comments' then
          jsx_expr
        else
          { expression = Expression expr'; comments = comments' }
      | EmptyExpression ->
        if comments == comments' then
          jsx_expr
        else
          { expression = EmptyExpression; comments = comments' }

    method jsx_spread_child (jsx_spread_child : ('loc, 'loc) Ast.JSX.SpreadChild.t) =
      let open Ast.JSX.SpreadChild in
      let { expression; comments } = jsx_spread_child in
      let expression' = this#expression expression in
      let comments' = this#syntax_opt comments in
      if expression == expression' && comments == comments' then
        jsx_spread_child
      else
        { expression = expression'; comments = comments' }

    method jsx_element_name (name : ('loc, 'loc) Ast.JSX.name) =
      let open Ast.JSX in
      match name with
      | Identifier ident ->
        id this#jsx_element_name_identifier ident name (fun ident -> Identifier ident)
      | NamespacedName ns ->
        id this#jsx_element_name_namespaced ns name (fun ns -> NamespacedName ns)
      | MemberExpression expr ->
        id this#jsx_element_name_member_expression expr name (fun expr -> MemberExpression expr)

    method jsx_element_name_identifier ident = this#jsx_identifier ident

    method jsx_element_name_namespaced ns = this#jsx_namespaced_name ns

    method jsx_element_name_member_expression expr = this#jsx_member_expression expr

    method jsx_namespaced_name (namespaced_name : ('loc, 'loc) Ast.JSX.NamespacedName.t) =
      let open Ast.JSX in
      NamespacedName.(
        let (loc, { namespace; name }) = namespaced_name in
        let namespace' = this#jsx_identifier namespace in
        let name' = this#jsx_identifier name in
        if namespace == namespace' && name == name' then
          namespaced_name
        else
          (loc, { namespace = namespace'; name = name' })
      )

    method jsx_member_expression (member_exp : ('loc, 'loc) Ast.JSX.MemberExpression.t) =
      let open Ast.JSX in
      let (loc, { MemberExpression._object; MemberExpression.property }) = member_exp in
      let _object' = this#jsx_member_expression_object _object in
      let property' = this#jsx_identifier property in
      if _object == _object' && property == property' then
        member_exp
      else
        (loc, MemberExpression.{ _object = _object'; property = property' })

    method jsx_member_expression_object (_object : ('loc, 'loc) Ast.JSX.MemberExpression._object) =
      let open Ast.JSX.MemberExpression in
      match _object with
      | Identifier ident ->
        id this#jsx_member_expression_identifier ident _object (fun ident -> Identifier ident)
      | MemberExpression nested_exp ->
        id this#jsx_member_expression nested_exp _object (fun exp -> MemberExpression exp)

    method jsx_member_expression_identifier ident = this#jsx_element_name_identifier ident

    method jsx_identifier (id : ('loc, 'loc) Ast.JSX.Identifier.t) =
      let open Ast.JSX.Identifier in
      let (loc, { name; comments }) = id in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        id
      else
        (loc, { name; comments = comments' })

    method labeled_statement _loc (stmt : ('loc, 'loc) Ast.Statement.Labeled.t) =
      let open Ast.Statement.Labeled in
      let { label; body; comments } = stmt in
      let label' = this#label_identifier label in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      if label == label' && body == body' && comments == comments' then
        stmt
      else
        { label = label'; body = body'; comments = comments' }

    method logical _loc (expr : ('loc, 'loc) Ast.Expression.Logical.t) =
      let open Ast.Expression.Logical in
      let { operator = _; left; right; comments } = expr in
      let left' = this#expression left in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if left == left' && right == right' && comments == comments' then
        expr
      else
        { expr with left = left'; right = right'; comments = comments' }

    method match_
        : 'B.
          'loc ->
          on_case_body:('B -> 'B) ->
          ('loc, 'loc, 'B) Ast.Match.t ->
          ('loc, 'loc, 'B) Ast.Match.t =
      fun _loc ~on_case_body x ->
        let open Ast.Match in
        let { arg; cases; match_keyword_loc; comments } = x in
        let arg' = this#expression arg in
        let cases' = map_list (this#match_case ~on_case_body) cases in
        let comments' = this#syntax_opt comments in
        if arg == arg' && cases == cases' && comments == comments' then
          x
        else
          { arg = arg'; cases = cases'; match_keyword_loc; comments = comments' }

    method match_case
        : 'B.
          on_case_body:('B -> 'B) ->
          ('loc, 'loc, 'B) Ast.Match.Case.t ->
          ('loc, 'loc, 'B) Ast.Match.Case.t =
      fun ~on_case_body case ->
        let open Ast.Match.Case in
        let (loc, { pattern; body; guard; comments; invalid_syntax }) = case in
        let pattern' = this#match_pattern pattern in
        let body' = on_case_body body in
        let guard' = map_opt this#expression guard in
        let comments' = this#syntax_opt comments in
        if pattern == pattern' && body == body' && guard == guard' && comments == comments' then
          case
        else
          ( loc,
            {
              pattern = pattern';
              body = body';
              guard = guard';
              comments = comments';
              invalid_syntax;
            }
          )

    method match_expression loc (x : ('loc, 'loc) Ast.Expression.match_expression) =
      this#match_ loc ~on_case_body:this#expression x

    method match_statement loc (x : ('loc, 'loc) Ast.Statement.match_statement) =
      this#match_ loc ~on_case_body:this#statement x

    method match_pattern (pattern : ('loc, 'loc) Ast.MatchPattern.t) =
      let open Ast.MatchPattern in
      match pattern with
      | (loc, WildcardPattern x) ->
        id this#match_wildcard_pattern x pattern (fun x -> (loc, WildcardPattern x))
      | (loc, StringPattern x) ->
        id_loc this#string_literal loc x pattern (fun x -> (loc, StringPattern x))
      | (loc, BooleanPattern x) ->
        id_loc this#boolean_literal loc x pattern (fun x -> (loc, BooleanPattern x))
      | (loc, NullPattern x) -> id this#syntax_opt x pattern (fun x -> (loc, NullPattern x))
      | (loc, NumberPattern x) ->
        id_loc this#number_literal loc x pattern (fun x -> (loc, NumberPattern x))
      | (loc, BigIntPattern x) ->
        id_loc this#bigint_literal loc x pattern (fun x -> (loc, BigIntPattern x))
      | (loc, UnaryPattern x) ->
        id this#match_unary_pattern x pattern (fun x -> (loc, UnaryPattern x))
      | (loc, IdentifierPattern x) ->
        id this#identifier x pattern (fun x -> (loc, IdentifierPattern x))
      | (loc, MemberPattern x) ->
        id this#match_member_pattern x pattern (fun x -> (loc, MemberPattern x))
      | (loc, BindingPattern x) ->
        id_loc this#match_binding_pattern loc x pattern (fun x -> (loc, BindingPattern x))
      | (loc, ObjectPattern x) ->
        id this#match_object_pattern x pattern (fun x -> (loc, ObjectPattern x))
      | (loc, ArrayPattern x) ->
        id this#match_array_pattern x pattern (fun x -> (loc, ArrayPattern x))
      | (loc, OrPattern x) -> id this#match_or_pattern x pattern (fun x -> (loc, OrPattern x))
      | (loc, AsPattern x) -> id this#match_as_pattern x pattern (fun x -> (loc, AsPattern x))

    method match_unary_pattern (unary_pattern : 'loc Ast.MatchPattern.UnaryPattern.t) =
      let open Ast.MatchPattern.UnaryPattern in
      let { operator; argument; comments } = unary_pattern in
      let (arg_loc, arg) = argument in
      let argument' =
        id_loc this#match_unary_pattern_argument arg_loc arg argument (fun arg -> (arg_loc, arg))
      in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        unary_pattern
      else
        { operator; argument = argument'; comments = comments' }

    method match_unary_pattern_argument loc (argument : 'loc Ast.MatchPattern.UnaryPattern.argument)
        =
      let open Ast.MatchPattern.UnaryPattern in
      match argument with
      | NumberLiteral lit ->
        id_loc this#number_literal loc lit argument (fun lit -> NumberLiteral lit)
      | BigIntLiteral lit ->
        id_loc this#bigint_literal loc lit argument (fun lit -> BigIntLiteral lit)

    method match_member_pattern (member_pattern : ('loc, 'loc) Ast.MatchPattern.MemberPattern.t) =
      let open Ast.MatchPattern.MemberPattern in
      let (loc, { base; property; comments }) = member_pattern in
      let base' = this#match_member_pattern_base base in
      let property' = this#match_member_pattern_property property in
      let comments' = this#syntax_opt comments in
      if base == base' && property == property' && comments == comments' then
        member_pattern
      else
        (loc, { base = base'; property = property'; comments = comments' })

    method match_member_pattern_base (base : ('loc, 'loc) Ast.MatchPattern.MemberPattern.base) =
      let open Ast.MatchPattern.MemberPattern in
      match base with
      | BaseIdentifier x -> id this#identifier x base (fun x -> BaseIdentifier x)
      | BaseMember x -> id this#match_member_pattern x base (fun x -> BaseMember x)

    method match_member_pattern_property
        (prop : ('loc, 'loc) Ast.MatchPattern.MemberPattern.property) =
      let open Ast.MatchPattern.MemberPattern in
      match prop with
      | PropertyString (loc, lit) ->
        id_loc this#string_literal loc lit prop (fun lit -> PropertyString (loc, lit))
      | PropertyNumber (loc, lit) ->
        id_loc this#number_literal loc lit prop (fun lit -> PropertyNumber (loc, lit))
      | PropertyBigInt (loc, lit) ->
        id_loc this#bigint_literal loc lit prop (fun lit -> PropertyBigInt (loc, lit))
      | PropertyIdentifier ident ->
        id this#identifier ident prop (fun ident -> PropertyIdentifier ident)

    method match_binding_pattern
        _loc (binding_pattern : ('loc, 'loc) Ast.MatchPattern.BindingPattern.t) =
      let open Ast.MatchPattern.BindingPattern in
      let { id; kind; comments } = binding_pattern in
      let id' = this#pattern_identifier ~kind id in
      let comments' = this#syntax_opt comments in
      if id == id' && comments == comments' then
        binding_pattern
      else
        { id = id'; kind; comments = comments' }

    method match_object_pattern (object_pattern : ('loc, 'loc) Ast.MatchPattern.ObjectPattern.t) =
      let open Ast.MatchPattern.ObjectPattern in
      let { properties; rest; comments } = object_pattern in
      let properties' = map_list this#match_object_pattern_property properties in
      let rest' = map_loc_opt this#match_rest_pattern rest in
      let comments' = this#syntax_opt comments in
      if properties == properties' && rest == rest' && comments == comments' then
        object_pattern
      else
        { properties = properties'; rest = rest'; comments = comments' }

    method match_object_pattern_property
        (prop : ('loc, 'loc) Ast.MatchPattern.ObjectPattern.Property.t) =
      let open Ast.MatchPattern.ObjectPattern.Property in
      match prop with
      | (loc, Valid { key; pattern; shorthand; comments }) ->
        let key' = this#match_object_pattern_property_key key in
        let pattern' = this#match_pattern pattern in
        let comments' = this#syntax_opt comments in
        if key == key' && pattern == pattern' && comments == comments' then
          prop
        else
          (loc, Valid { key = key'; pattern = pattern'; shorthand; comments = comments' })
      | (loc, InvalidShorthand id) ->
        let id' = this#identifier id in
        if id == id' then
          prop
        else
          (loc, InvalidShorthand id')

    method match_object_pattern_property_key
        (key : ('loc, 'loc) Ast.MatchPattern.ObjectPattern.Property.key) =
      let open Ast.MatchPattern.ObjectPattern.Property in
      match key with
      | StringLiteral (loc, lit) ->
        id_loc this#string_literal loc lit key (fun lit -> StringLiteral (loc, lit))
      | NumberLiteral (loc, lit) ->
        id_loc this#number_literal loc lit key (fun lit -> NumberLiteral (loc, lit))
      | BigIntLiteral (loc, lit) ->
        id_loc this#bigint_literal loc lit key (fun lit -> BigIntLiteral (loc, lit))
      | Identifier ident -> id this#identifier ident key (fun ident -> Identifier ident)

    method match_array_pattern (array_pattern : ('loc, 'loc) Ast.MatchPattern.ArrayPattern.t) =
      let open Ast.MatchPattern.ArrayPattern in
      let { elements; rest; comments } = array_pattern in
      let elements' = map_list this#match_pattern_array_element elements in
      let rest' = map_loc_opt this#match_rest_pattern rest in
      let comments' = this#syntax_opt comments in
      if elements == elements' && rest == rest' && comments == comments' then
        array_pattern
      else
        { elements = elements'; rest = rest'; comments = comments' }

    method match_pattern_array_element
        (element : ('loc, 'loc) Ast.MatchPattern.ArrayPattern.Element.t) =
      let open Ast.MatchPattern.ArrayPattern.Element in
      let { pattern; index } = element in
      let pattern' = this#match_pattern pattern in
      if pattern == pattern' then
        element
      else
        { pattern = pattern'; index }

    method match_rest_pattern _loc (rest : ('loc, 'loc) Ast.MatchPattern.RestPattern.t') =
      let open Ast.MatchPattern.RestPattern in
      let { argument; comments } = rest in
      let argument' = map_loc_opt this#match_binding_pattern argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        rest
      else
        { argument = argument'; comments = comments' }

    method match_or_pattern (or_pattern : ('loc, 'loc) Ast.MatchPattern.OrPattern.t) =
      let open Ast.MatchPattern.OrPattern in
      let { patterns; comments } = or_pattern in
      let patterns' = map_list this#match_pattern patterns in
      let comments' = this#syntax_opt comments in
      if patterns == patterns' && comments == comments' then
        or_pattern
      else
        { patterns = patterns'; comments = comments' }

    method match_as_pattern (as_pattern : ('loc, 'loc) Ast.MatchPattern.AsPattern.t) =
      let open Ast.MatchPattern.AsPattern in
      let { pattern; target; comments } = as_pattern in
      let pattern' = this#match_pattern pattern in
      let target' = this#match_as_pattern_target target in
      let comments' = this#syntax_opt comments in
      if pattern == pattern' && target == target' && comments == comments' then
        as_pattern
      else
        { pattern = pattern'; target = target'; comments = comments' }

    method match_as_pattern_target (target : ('loc, 'loc) Ast.MatchPattern.AsPattern.target) =
      let open Ast.MatchPattern.AsPattern in
      match target with
      | Binding (loc, binding) ->
        id_loc this#match_binding_pattern loc binding target (fun x -> Binding (loc, x))
      | Identifier ident ->
        id (this#pattern_identifier ~kind:Ast.Variable.Const) ident target (fun x -> Identifier x)

    method match_wildcard_pattern (wildcard_pattern : 'loc Ast.MatchPattern.WildcardPattern.t) =
      let open Ast.MatchPattern.WildcardPattern in
      let { comments; invalid_syntax_default_keyword } = wildcard_pattern in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        wildcard_pattern
      else
        { comments = comments'; invalid_syntax_default_keyword }

    method member _loc (expr : ('loc, 'loc) Ast.Expression.Member.t) =
      let open Ast.Expression.Member in
      let { _object; property; comments } = expr in
      let _object' = this#expression _object in
      let property' = this#member_property property in
      let comments' = this#syntax_opt comments in
      if _object == _object' && property == property' && comments == comments' then
        expr
      else
        { _object = _object'; property = property'; comments = comments' }

    method optional_member loc (expr : ('loc, 'loc) Ast.Expression.OptionalMember.t) =
      let open Ast.Expression.OptionalMember in
      let { member; optional = _; filtered_out = _ } = expr in
      let member' = this#member loc member in
      if member == member' then
        expr
      else
        { expr with member = member' }

    method member_property (expr : ('loc, 'loc) Ast.Expression.Member.property) =
      let open Ast.Expression.Member in
      match expr with
      | PropertyIdentifier ident ->
        id this#member_property_identifier ident expr (fun ident -> PropertyIdentifier ident)
      | PropertyPrivateName ident ->
        id this#member_private_name ident expr (fun ident -> PropertyPrivateName ident)
      | PropertyExpression e ->
        id this#member_property_expression e expr (fun e -> PropertyExpression e)

    method member_property_identifier (ident : ('loc, 'loc) Ast.Identifier.t) =
      this#identifier ident

    method member_private_name (name : 'loc Ast.PrivateName.t) = this#private_name name

    method member_property_expression (expr : ('loc, 'loc) Ast.Expression.t) = this#expression expr

    method meta_property _loc (expr : 'loc Ast.Expression.MetaProperty.t) =
      let open Ast.Expression.MetaProperty in
      let { meta; property; comments } = expr in
      let meta' = this#identifier meta in
      let property' = this#identifier property in
      let comments' = this#syntax_opt comments in
      if meta == meta' && property == property' && comments == comments' then
        expr
      else
        { meta = meta'; property = property'; comments = comments' }

    method new_ _loc (expr : ('loc, 'loc) Ast.Expression.New.t) =
      let open Ast.Expression.New in
      let { callee; targs; arguments; comments } = expr in
      let callee' = this#expression callee in
      let targs' = map_opt this#call_type_args targs in
      let arguments' = map_opt this#arg_list arguments in
      let comments' = this#syntax_opt comments in
      if callee == callee' && targs == targs' && arguments == arguments' && comments == comments'
      then
        expr
      else
        { callee = callee'; targs = targs'; arguments = arguments'; comments = comments' }

    method object_ _loc (expr : ('loc, 'loc) Ast.Expression.Object.t) =
      let open Ast.Expression.Object in
      let { properties; comments } = expr in
      let properties' =
        map_list
          (fun prop ->
            match prop with
            | Property p ->
              let p' = this#object_property p in
              if p == p' then
                prop
              else
                Property p'
            | SpreadProperty s ->
              let s' = this#spread_property s in
              if s == s' then
                prop
              else
                SpreadProperty s')
          properties
      in
      let comments' = this#syntax_opt comments in
      if properties == properties' && comments == comments' then
        expr
      else
        { properties = properties'; comments = comments' }

    method object_property (prop : ('loc, 'loc) Ast.Expression.Object.Property.t) =
      let open Ast.Expression.Object.Property in
      match prop with
      | (loc, Init { key; value; shorthand }) ->
        let key' = this#object_key key in
        let value' = this#expression value in
        let shorthand' =
          (* Try to figure out if shorthand should still be true--if
             key and value change differently, it should become false *)
          shorthand
          &&
          match (key', value') with
          | ( Identifier (_, { Ast.Identifier.name = key_name; _ }),
              (_, Ast.Expression.Identifier (_, { Ast.Identifier.name = value_name; _ }))
            ) ->
            String.equal key_name value_name
          | _ -> key == key' && value == value'
        in
        if key == key' && value == value' && shorthand == shorthand' then
          prop
        else
          (loc, Init { key = key'; value = value'; shorthand = shorthand' })
      | (loc, Method { key; value = fn }) ->
        let key' = this#object_key key in
        let fn' = map_loc this#function_expression_or_method fn in
        if key == key' && fn == fn' then
          prop
        else
          (loc, Method { key = key'; value = fn' })
      | (loc, Get { key; value = fn; comments }) ->
        let key' = this#object_key key in
        let fn' = map_loc this#function_expression_or_method fn in
        let comments' = this#syntax_opt comments in
        if key == key' && fn == fn' && comments == comments' then
          prop
        else
          (loc, Get { key = key'; value = fn'; comments = comments' })
      | (loc, Set { key; value = fn; comments }) ->
        let key' = this#object_key key in
        let fn' = map_loc this#function_expression_or_method fn in
        let comments' = this#syntax_opt comments in
        if key == key' && fn == fn' && comments == comments' then
          prop
        else
          (loc, Set { key = key'; value = fn'; comments = comments' })

    method object_key (key : ('loc, 'loc) Ast.Expression.Object.Property.key) =
      let open Ast.Expression.Object.Property in
      match key with
      | StringLiteral lit -> id this#object_key_string_literal lit key (fun lit -> StringLiteral lit)
      | NumberLiteral lit -> id this#object_key_number_literal lit key (fun lit -> NumberLiteral lit)
      | BigIntLiteral lit -> id this#object_key_bigint_literal lit key (fun lit -> BigIntLiteral lit)
      | Identifier ident -> id this#object_key_identifier ident key (fun ident -> Identifier ident)
      | PrivateName ident -> id this#private_name ident key (fun ident -> PrivateName ident)
      | Computed computed -> id this#object_key_computed computed key (fun expr -> Computed expr)

    method object_key_string_literal (literal : 'loc * 'loc Ast.StringLiteral.t) =
      let (loc, lit) = literal in
      id_loc this#string_literal loc lit literal (fun lit -> (loc, lit))

    method object_key_number_literal (literal : 'loc * 'loc Ast.NumberLiteral.t) =
      let (loc, lit) = literal in
      id_loc this#number_literal loc lit literal (fun lit -> (loc, lit))

    method object_key_bigint_literal (literal : 'loc * 'loc Ast.BigIntLiteral.t) =
      let (loc, lit) = literal in
      id_loc this#bigint_literal loc lit literal (fun lit -> (loc, lit))

    method object_key_identifier (ident : ('loc, 'loc) Ast.Identifier.t) = this#identifier ident

    method object_key_computed (key : ('loc, 'loc) Ast.ComputedKey.t) = this#computed_key key

    method opaque_type _loc (otype : ('loc, 'loc) Ast.Statement.OpaqueType.t) =
      let open Ast.Statement.OpaqueType in
      let { id; tparams; impl_type; lower_bound; upper_bound; legacy_upper_bound; comments } =
        otype
      in
      let id' = this#binding_type_identifier id in
      let tparams' = map_opt (this#type_params ~kind:OpaqueTypeTP) tparams in
      let impl_type' = map_opt this#type_ impl_type in
      let legacy_upper_bound' = map_opt this#type_ legacy_upper_bound in
      let lower_bound' = map_opt this#type_ lower_bound in
      let upper_bound' = map_opt this#type_ upper_bound in
      let comments' = this#syntax_opt comments in
      if
        id == id'
        && impl_type == impl_type'
        && tparams == tparams'
        && lower_bound == lower_bound'
        && upper_bound == upper_bound'
        && legacy_upper_bound == legacy_upper_bound'
        && comments == comments'
      then
        otype
      else
        {
          id = id';
          tparams = tparams';
          impl_type = impl_type';
          lower_bound = lower_bound';
          upper_bound = upper_bound';
          legacy_upper_bound = legacy_upper_bound';
          comments = comments';
        }

    method function_param_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#binding_pattern ~kind:Ast.Variable.Let expr

    method variable_declarator_pattern ~kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#binding_pattern ~kind expr

    method catch_clause_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#binding_pattern ~kind:Ast.Variable.Let expr

    method for_in_assignment_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#assignment_pattern expr

    method for_of_assignment_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#assignment_pattern expr

    method binding_pattern ?(kind = Ast.Variable.Var) (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ~kind expr

    method assignment_pattern (expr : ('loc, 'loc) Ast.Pattern.t) = this#pattern expr

    (* NOTE: Patterns are highly overloaded. A pattern can be a binding pattern,
       which has a kind (Var/Let/Const, with Var being the default for all pre-ES5
       bindings), or an assignment pattern, which has no kind. Subterms that are
       patterns inherit the kind (or lack thereof). *)
    method pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      let open Ast.Pattern in
      let (loc, patt) = expr in
      let patt' =
        match patt with
        | Object { Object.properties; annot; comments } ->
          let properties' = map_list (this#pattern_object_p ?kind) properties in
          let annot' = this#type_annotation_hint annot in
          let comments' = this#syntax_opt comments in
          if properties' == properties && annot' == annot && comments' == comments then
            patt
          else
            Object { Object.properties = properties'; annot = annot'; comments = comments' }
        | Array { Array.elements; annot; comments } ->
          let elements' = map_list (this#pattern_array_e ?kind) elements in
          let annot' = this#type_annotation_hint annot in
          let comments' = this#syntax_opt comments in
          if comments == comments' && elements' == elements && annot' == annot then
            patt
          else
            Array { Array.elements = elements'; annot = annot'; comments = comments' }
        | Identifier { Identifier.name; annot; optional } ->
          let name' = this#pattern_identifier ?kind name in
          let annot' = this#type_annotation_hint annot in
          if name == name' && annot == annot' then
            patt
          else
            Identifier { Identifier.name = name'; annot = annot'; optional }
        | Expression e -> id this#pattern_expression e patt (fun e -> Expression e)
      in
      if patt == patt' then
        expr
      else
        (loc, patt')

    method pattern_identifier ?kind (ident : ('loc, 'loc) Ast.Identifier.t) =
      ignore kind;
      this#identifier ident

    method pattern_string_literal ?kind loc (expr : 'loc Ast.StringLiteral.t) =
      ignore kind;
      this#string_literal loc expr

    method pattern_number_literal ?kind loc (expr : 'loc Ast.NumberLiteral.t) =
      ignore kind;
      this#number_literal loc expr

    method pattern_bigint_literal ?kind loc (expr : 'loc Ast.BigIntLiteral.t) =
      ignore kind;
      this#bigint_literal loc expr

    method pattern_object_p ?kind (p : ('loc, 'loc) Ast.Pattern.Object.property) =
      let open Ast.Pattern.Object in
      match p with
      | Property prop -> id (this#pattern_object_property ?kind) prop p (fun prop -> Property prop)
      | RestElement prop ->
        id (this#pattern_object_rest_property ?kind) prop p (fun prop -> RestElement prop)

    method pattern_object_property ?kind (prop : ('loc, 'loc) Ast.Pattern.Object.Property.t) =
      let open Ast.Pattern.Object.Property in
      let (loc, { key; pattern; default; shorthand }) = prop in
      let key' = this#pattern_object_property_key ?kind key in
      let pattern' = this#pattern_object_property_pattern ?kind pattern in
      let default' = this#default_opt default in
      let shorthand' =
        (* Try to figure out if shorthand should still be true--if
            key and value change differently, it should become false *)
        shorthand
        &&
        match (key', pattern') with
        | ( Identifier (_, { Ast.Identifier.name = key_name; _ }),
            ( _,
              Ast.Pattern.Identifier
                { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name = value_name; _ }); _ }
            )
          ) ->
          String.equal key_name value_name
        | _ -> key == key' && pattern == pattern'
      in
      if key' == key && pattern' == pattern && default' == default && shorthand == shorthand' then
        prop
      else
        (loc, { key = key'; pattern = pattern'; default = default'; shorthand = shorthand' })

    method pattern_object_property_key ?kind (key : ('loc, 'loc) Ast.Pattern.Object.Property.key) =
      let open Ast.Pattern.Object.Property in
      match key with
      | StringLiteral lit ->
        id (this#pattern_object_property_string_literal_key ?kind) lit key (fun lit' ->
            StringLiteral lit'
        )
      | NumberLiteral lit ->
        id (this#pattern_object_property_number_literal_key ?kind) lit key (fun lit' ->
            NumberLiteral lit'
        )
      | BigIntLiteral lit ->
        id (this#pattern_object_property_bigint_literal_key ?kind) lit key (fun lit' ->
            BigIntLiteral lit'
        )
      | Identifier identifier ->
        id (this#pattern_object_property_identifier_key ?kind) identifier key (fun id' ->
            Identifier id'
        )
      | Computed expr ->
        id (this#pattern_object_property_computed_key ?kind) expr key (fun expr' -> Computed expr')

    method pattern_object_property_string_literal_key
        ?kind (literal : 'loc * 'loc Ast.StringLiteral.t) =
      let (loc, key) = literal in
      id_loc (this#pattern_string_literal ?kind) loc key literal (fun key' -> (loc, key'))

    method pattern_object_property_number_literal_key
        ?kind (literal : 'loc * 'loc Ast.NumberLiteral.t) =
      let (loc, key) = literal in
      id_loc (this#pattern_number_literal ?kind) loc key literal (fun key' -> (loc, key'))

    method pattern_object_property_bigint_literal_key
        ?kind (literal : 'loc * 'loc Ast.BigIntLiteral.t) =
      let (loc, key) = literal in
      id_loc (this#pattern_bigint_literal ?kind) loc key literal (fun key' -> (loc, key'))

    method pattern_object_property_identifier_key ?kind (key : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ?kind key

    method pattern_object_property_computed_key ?kind (key : ('loc, 'loc) Ast.ComputedKey.t) =
      ignore kind;
      this#computed_key key

    method pattern_object_rest_property ?kind (prop : ('loc, 'loc) Ast.Pattern.RestElement.t) =
      let open Ast.Pattern.RestElement in
      let (loc, { argument; comments }) = prop in
      let argument' = this#pattern_object_rest_property_pattern ?kind argument in
      let comments' = this#syntax_opt comments in
      if argument' == argument && comments == comments' then
        prop
      else
        (loc, { argument = argument'; comments = comments' })

    method pattern_object_property_pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind expr

    method pattern_object_rest_property_pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind expr

    method pattern_array_e ?kind (e : ('loc, 'loc) Ast.Pattern.Array.element) =
      let open Ast.Pattern.Array in
      match e with
      | Hole _ -> e
      | Element elem -> id (this#pattern_array_element ?kind) elem e (fun elem -> Element elem)
      | RestElement elem ->
        id (this#pattern_array_rest_element ?kind) elem e (fun elem -> RestElement elem)

    method pattern_array_element ?kind (elem : ('loc, 'loc) Ast.Pattern.Array.Element.t) =
      let open Ast.Pattern.Array.Element in
      let (loc, { argument; default }) = elem in
      let argument' = this#pattern_array_element_pattern ?kind argument in
      let default' = this#default_opt default in
      if argument == argument' && default == default' then
        elem
      else
        (loc, { argument = argument'; default = default' })

    method pattern_array_element_pattern ?kind (patt : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind patt

    method pattern_array_rest_element ?kind (elem : ('loc, 'loc) Ast.Pattern.RestElement.t) =
      let open Ast.Pattern.RestElement in
      let (loc, { argument; comments }) = elem in
      let argument' = this#pattern_array_rest_element_pattern ?kind argument in
      let comments' = this#syntax_opt comments in
      if argument' == argument && comments == comments' then
        elem
      else
        (loc, { argument = argument'; comments = comments' })

    method pattern_array_rest_element_pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind expr

    method pattern_expression (expr : ('loc, 'loc) Ast.Expression.t) = this#expression expr

    method predicate (pred : ('loc, 'loc) Ast.Type.Predicate.t) =
      let open Ast.Type.Predicate in
      let (loc, { kind; comments }) = pred in
      let kind' =
        match kind with
        | Inferred -> kind
        | Declared expr -> id this#expression expr kind (fun expr' -> Declared expr')
      in
      let comments' = this#syntax_opt comments in
      if kind == kind' && comments == comments' then
        pred
      else
        (loc, { kind = kind'; comments = comments' })

    method predicate_expression (expr : ('loc, 'loc) Ast.Expression.t) = this#expression expr

    method type_guard_annotation
        (type_guard_annotation : ('loc, 'loc) Ast.Type.type_guard_annotation) =
      let (loc, type_guard) = type_guard_annotation in
      let type_guard' = this#type_guard type_guard in
      if type_guard' = type_guard then
        type_guard_annotation
      else
        (loc, type_guard')

    method type_guard (guard : ('loc, 'loc) Ast.Type.TypeGuard.t) =
      let open Ast.Type.TypeGuard in
      let (loc, { kind; guard = (x, t); comments }) = guard in
      let x' = this#identifier x in
      let t' = map_opt this#type_ t in
      let comments' = this#syntax_opt comments in
      if x' == x && t' == t && comments' == comments then
        guard
      else
        (loc, { kind; guard = (x', t'); comments = comments' })

    method function_rest_param (expr : ('loc, 'loc) Ast.Function.RestParam.t) =
      let open Ast.Function.RestParam in
      let (loc, { argument; comments }) = expr in
      let argument' = this#function_param_pattern argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        expr
      else
        (loc, { argument = argument'; comments = comments' })

    method return _loc (stmt : ('loc, 'loc) Ast.Statement.Return.t) =
      let open Ast.Statement.Return in
      let { argument; comments; return_out } = stmt in
      let argument' = map_opt this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        stmt
      else
        { argument = argument'; comments = comments'; return_out }

    method sequence _loc (expr : ('loc, 'loc) Ast.Expression.Sequence.t) =
      let open Ast.Expression.Sequence in
      let { expressions; comments } = expr in
      let expressions' = map_list this#expression expressions in
      let comments' = this#syntax_opt comments in
      if expressions == expressions' && comments == comments' then
        expr
      else
        { expressions = expressions'; comments = comments' }

    method toplevel_statement_list (stmts : ('loc, 'loc) Ast.Statement.t list) =
      this#statement_list stmts

    method statement_list (stmts : ('loc, 'loc) Ast.Statement.t list) =
      map_list_multiple this#statement_fork_point stmts

    method statement_fork_point (stmt : ('loc, 'loc) Ast.Statement.t) = [this#statement stmt]

    method spread_element (expr : ('loc, 'loc) Ast.Expression.SpreadElement.t) =
      let open Ast.Expression.SpreadElement in
      let (loc, { argument; comments }) = expr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        expr
      else
        (loc, { argument = argument'; comments = comments' })

    method spread_property (expr : ('loc, 'loc) Ast.Expression.Object.SpreadProperty.t) =
      let open Ast.Expression.Object.SpreadProperty in
      let (loc, { argument; comments }) = expr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        expr
      else
        (loc, { argument = argument'; comments = comments' })

    method super_expression _loc (expr : 'loc Ast.Expression.Super.t) =
      let open Ast.Expression.Super in
      let { comments } = expr in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        expr
      else
        { comments = comments' }

    method switch _loc (switch : ('loc, 'loc) Ast.Statement.Switch.t) =
      let open Ast.Statement.Switch in
      let { discriminant; cases; comments; exhaustive_out } = switch in
      let discriminant' = this#expression discriminant in
      let cases' = map_list this#switch_case cases in
      let comments' = this#syntax_opt comments in
      if discriminant == discriminant' && cases == cases' && comments == comments' then
        switch
      else
        { discriminant = discriminant'; cases = cases'; comments = comments'; exhaustive_out }

    method switch_case (case : ('loc, 'loc) Ast.Statement.Switch.Case.t) =
      let open Ast.Statement.Switch.Case in
      let (loc, { test; case_test_loc; consequent; comments }) = case in
      let test' = map_opt this#expression test in
      let consequent' = this#statement_list consequent in
      let comments' = this#syntax_opt comments in
      if test == test' && consequent == consequent' && comments == comments' then
        case
      else
        (loc, { test = test'; case_test_loc; consequent = consequent'; comments = comments' })

    method tagged_template _loc (expr : ('loc, 'loc) Ast.Expression.TaggedTemplate.t) =
      let open Ast.Expression.TaggedTemplate in
      let { tag; quasi; comments } = expr in
      let tag' = this#expression tag in
      let quasi' = map_loc this#template_literal quasi in
      let comments' = this#syntax_opt comments in
      if tag == tag' && quasi == quasi' && comments == comments' then
        expr
      else
        { tag = tag'; quasi = quasi'; comments = comments' }

    method template_literal _loc (expr : ('loc, 'loc) Ast.Expression.TemplateLiteral.t) =
      let open Ast.Expression.TemplateLiteral in
      let { quasis; expressions; comments } = expr in
      let quasis' = map_list this#template_literal_element quasis in
      let expressions' = map_list this#expression expressions in
      let comments' = this#syntax_opt comments in
      if quasis == quasis' && expressions == expressions' && comments == comments' then
        expr
      else
        { quasis = quasis'; expressions = expressions'; comments = comments' }

    (* TODO *)
    method template_literal_element (elem : 'loc Ast.Expression.TemplateLiteral.Element.t) = elem

    method this_expression _loc (expr : 'loc Ast.Expression.This.t) =
      let open Ast.Expression.This in
      let { comments } = expr in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        expr
      else
        { comments = comments' }

    method throw _loc (stmt : ('loc, 'loc) Ast.Statement.Throw.t) =
      let open Ast.Statement.Throw in
      let { argument; comments } = stmt in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        stmt
      else
        { argument = argument'; comments = comments' }

    method try_catch _loc (stmt : ('loc, 'loc) Ast.Statement.Try.t) =
      let open Ast.Statement.Try in
      let { block; handler; finalizer; comments } = stmt in
      let block' = map_loc this#block block in
      let handler' =
        match handler with
        | Some (loc, clause) ->
          id_loc this#catch_clause loc clause handler (fun clause -> Some (loc, clause))
        | None -> handler
      in
      let finalizer' =
        match finalizer with
        | Some (finalizer_loc, block) ->
          id_loc this#block finalizer_loc block finalizer (fun block -> Some (finalizer_loc, block))
        | None -> finalizer
      in
      let comments' = this#syntax_opt comments in
      if block == block' && handler == handler' && finalizer == finalizer' && comments == comments'
      then
        stmt
      else
        { block = block'; handler = handler'; finalizer = finalizer'; comments = comments' }

    method type_cast _loc (expr : ('loc, 'loc) Ast.Expression.TypeCast.t) =
      let open Ast.Expression.TypeCast in
      let { expression; annot; comments } = expr in
      let expression' = this#expression expression in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      if expression' == expression && annot' == annot && comments' == comments then
        expr
      else
        { expression = expression'; annot = annot'; comments = comments' }

    method ts_satisfies _loc (expr : ('loc, 'loc) Ast.Expression.TSSatisfies.t) =
      let open Ast.Expression.TSSatisfies in
      let { expression; annot; comments } = expr in
      let expression' = this#expression expression in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      if expression' == expression && annot' = annot && comments' == comments then
        expr
      else
        { expression = expression'; annot = annot'; comments = comments' }

    method unary_expression _loc (expr : ('loc, 'loc) Flow_ast.Expression.Unary.t) =
      let open Flow_ast.Expression.Unary in
      let { argument; operator = _; comments } = expr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        expr
      else
        { expr with argument = argument'; comments = comments' }

    method update_expression _loc (expr : ('loc, 'loc) Ast.Expression.Update.t) =
      let open Ast.Expression.Update in
      let { argument; operator = _; prefix = _; comments } = expr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        expr
      else
        { expr with argument = argument'; comments = comments' }

    method variable_declaration _loc (decl : ('loc, 'loc) Ast.Statement.VariableDeclaration.t) =
      let open Ast.Statement.VariableDeclaration in
      let { declarations; kind; comments } = decl in
      let decls' = map_list (this#variable_declarator ~kind) declarations in
      let comments' = this#syntax_opt comments in
      if declarations == decls' && comments == comments' then
        decl
      else
        { declarations = decls'; kind; comments = comments' }

    method variable_declarator
        ~kind (decl : ('loc, 'loc) Ast.Statement.VariableDeclaration.Declarator.t) =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (loc, { id; init }) = decl in
      let id' = this#variable_declarator_pattern ~kind id in
      let init' = map_opt this#expression init in
      if id == id' && init == init' then
        decl
      else
        (loc, { id = id'; init = init' })

    method while_ _loc (stuff : ('loc, 'loc) Ast.Statement.While.t) =
      let open Ast.Statement.While in
      let { test; body; comments } = stuff in
      let test' = this#predicate_expression test in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      if test == test' && body == body' && comments == comments' then
        stuff
      else
        { test = test'; body = body'; comments = comments' }

    method with_ _loc (stuff : ('loc, 'loc) Ast.Statement.With.t) =
      let open Ast.Statement.With in
      let { _object; body; comments } = stuff in
      let _object' = this#expression _object in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      if _object == _object' && body == body' && comments == comments' then
        stuff
      else
        { _object = _object'; body = body'; comments = comments' }

    method type_alias _loc (stuff : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      let open Ast.Statement.TypeAlias in
      let { id; tparams; right; comments } = stuff in
      let id' = this#binding_type_identifier id in
      let tparams' = map_opt (this#type_params ~kind:TypeAliasTP) tparams in
      let right' = this#type_ right in
      let comments' = this#syntax_opt comments in
      if id == id' && right == right' && tparams == tparams' && comments == comments' then
        stuff
      else
        { id = id'; tparams = tparams'; right = right'; comments = comments' }

    method yield _loc (expr : ('loc, 'loc) Ast.Expression.Yield.t) =
      let open Ast.Expression.Yield in
      let { argument; delegate; comments; result_out } = expr in
      let argument' = map_opt this#expression argument in
      let comments' = this#syntax_opt comments in
      if comments == comments' && argument == argument' then
        expr
      else
        { argument = argument'; delegate; comments = comments'; result_out }
  end

let fold_program (mappers : 'a mapper list) ast =
  List.fold_left (fun ast (m : 'a mapper) -> m#program ast) ast mappers
