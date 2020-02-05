(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

class ['loc] mapper =
  object (this)
    method program (program : ('loc, 'loc) Ast.program) =
      let (loc, statements, comments) = program in
      let statements' = this#toplevel_statement_list statements in
      let comments' = ListUtils.ident_map this#comment comments in
      if statements == statements' && comments == comments' then
        program
      else
        (loc, statements', comments')

    method statement (stmt : ('loc, 'loc) Ast.Statement.t) =
      let open Ast.Statement in
      match stmt with
      | (loc, Block block) -> id_loc this#block loc block stmt (fun block -> (loc, Block block))
      | (loc, Break break) -> id_loc this#break loc break stmt (fun break -> (loc, Break break))
      | (loc, ClassDeclaration cls) ->
        id_loc this#class_ loc cls stmt (fun cls -> (loc, ClassDeclaration cls))
      | (loc, Continue cont) ->
        id_loc this#continue loc cont stmt (fun cont -> (loc, Continue cont))
      | (loc, Debugger) ->
        this#debugger loc;
        stmt
      | (loc, DeclareClass stuff) ->
        id_loc this#declare_class loc stuff stmt (fun stuff -> (loc, DeclareClass stuff))
      | (loc, DeclareExportDeclaration decl) ->
        id_loc this#declare_export_declaration loc decl stmt (fun decl ->
            (loc, DeclareExportDeclaration decl))
      | (loc, DeclareFunction stuff) ->
        id_loc this#declare_function loc stuff stmt (fun stuff -> (loc, DeclareFunction stuff))
      | (loc, DeclareInterface stuff) ->
        id_loc this#declare_interface loc stuff stmt (fun stuff -> (loc, DeclareInterface stuff))
      | (loc, DeclareModule m) ->
        id_loc this#declare_module loc m stmt (fun m -> (loc, DeclareModule m))
      | (loc, DeclareTypeAlias stuff) ->
        id_loc this#declare_type_alias loc stuff stmt (fun stuff -> (loc, DeclareTypeAlias stuff))
      | (loc, DeclareVariable stuff) ->
        id_loc this#declare_variable loc stuff stmt (fun stuff -> (loc, DeclareVariable stuff))
      | (loc, DeclareModuleExports annot) ->
        id_loc this#declare_module_exports loc annot stmt (fun annot ->
            (loc, DeclareModuleExports annot))
      | (loc, DoWhile stuff) ->
        id_loc this#do_while loc stuff stmt (fun stuff -> (loc, DoWhile stuff))
      | (loc, Empty) ->
        this#empty loc;
        stmt
      | (loc, EnumDeclaration enum) ->
        id_loc this#enum_declaration loc enum stmt (fun enum -> (loc, EnumDeclaration enum))
      | (loc, ExportDefaultDeclaration decl) ->
        id_loc this#export_default_declaration loc decl stmt (fun decl ->
            (loc, ExportDefaultDeclaration decl))
      | (loc, ExportNamedDeclaration decl) ->
        id_loc this#export_named_declaration loc decl stmt (fun decl ->
            (loc, ExportNamedDeclaration decl))
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
            (loc, InterfaceDeclaration stuff))
      | (loc, Labeled label) ->
        id_loc this#labeled_statement loc label stmt (fun label -> (loc, Labeled label))
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
      (* TODO: Flow specific stuff *)
      | (_loc, DeclareOpaqueType _) -> stmt

    method comment (c : 'loc Ast.Comment.t) = c

    method syntax_opt
        : 'internal. ('loc, 'internal) Ast.Syntax.t option -> ('loc, 'internal) Ast.Syntax.t option
        =
      map_opt this#syntax

    method syntax : 'internal. ('loc, 'internal) Ast.Syntax.t -> ('loc, 'internal) Ast.Syntax.t =
      fun attached ->
        let open Ast.Syntax in
        let { leading; trailing; internal } = attached in
        let leading' = ListUtils.ident_map this#comment leading in
        let trailing' = ListUtils.ident_map this#comment trailing in
        if leading == leading' && trailing == trailing' then
          attached
        else
          { leading = leading'; trailing = trailing'; internal }

    method expression (expr : ('loc, 'loc) Ast.Expression.t) =
      let open Ast.Expression in
      match expr with
      | (_, This) -> expr
      | (_, Super) -> expr
      | (loc, Array x) -> id_loc this#array loc x expr (fun x -> (loc, Array x))
      | (loc, ArrowFunction x) ->
        id_loc this#arrow_function loc x expr (fun x -> (loc, ArrowFunction x))
      | (loc, Assignment x) -> id_loc this#assignment loc x expr (fun x -> (loc, Assignment x))
      | (loc, Binary x) -> id_loc this#binary loc x expr (fun x -> (loc, Binary x))
      | (loc, Call x) -> id_loc this#call loc x expr (fun x -> (loc, Call x))
      | (loc, Class x) -> id_loc this#class_ loc x expr (fun x -> (loc, Class x))
      | (loc, Comprehension x) ->
        id_loc this#comprehension loc x expr (fun x -> (loc, Comprehension x))
      | (loc, Conditional x) -> id_loc this#conditional loc x expr (fun x -> (loc, Conditional x))
      | (loc, Function x) -> id_loc this#function_expression loc x expr (fun x -> (loc, Function x))
      | (loc, Generator x) -> id_loc this#generator loc x expr (fun x -> (loc, Generator x))
      | (loc, Identifier x) -> id this#identifier x expr (fun x -> (loc, Identifier x))
      | (loc, Import x) -> id (this#import loc) x expr (fun x -> (loc, Import x))
      | (loc, JSXElement x) -> id_loc this#jsx_element loc x expr (fun x -> (loc, JSXElement x))
      | (loc, JSXFragment x) -> id_loc this#jsx_fragment loc x expr (fun x -> (loc, JSXFragment x))
      | (loc, Literal x) -> id_loc this#literal loc x expr (fun x -> (loc, Literal x))
      | (loc, Logical x) -> id_loc this#logical loc x expr (fun x -> (loc, Logical x))
      | (loc, Member x) -> id_loc this#member loc x expr (fun x -> (loc, Member x))
      | (loc, MetaProperty x) ->
        id_loc this#meta_property loc x expr (fun x -> (loc, MetaProperty x))
      | (loc, New x) -> id_loc this#new_ loc x expr (fun x -> (loc, New x))
      | (loc, Object x) -> id_loc this#object_ loc x expr (fun x -> (loc, Object x))
      | (loc, OptionalCall x) -> id (this#optional_call loc) x expr (fun x -> (loc, OptionalCall x))
      | (loc, OptionalMember x) ->
        id_loc this#optional_member loc x expr (fun x -> (loc, OptionalMember x))
      | (loc, Sequence x) -> id_loc this#sequence loc x expr (fun x -> (loc, Sequence x))
      | (loc, TaggedTemplate x) ->
        id_loc this#tagged_template loc x expr (fun x -> (loc, TaggedTemplate x))
      | (loc, TemplateLiteral x) ->
        id_loc this#template_literal loc x expr (fun x -> (loc, TemplateLiteral x))
      | (loc, TypeCast x) -> id_loc this#type_cast loc x expr (fun x -> (loc, TypeCast x))
      | (loc, Unary x) -> id_loc this#unary_expression loc x expr (fun x -> (loc, Unary x))
      | (loc, Update x) -> id_loc this#update_expression loc x expr (fun x -> (loc, Update x))
      | (loc, Yield x) -> id_loc this#yield loc x expr (fun x -> (loc, Yield x))

    method array _loc (expr : ('loc, 'loc) Ast.Expression.Array.t) =
      let open Ast.Expression in
      let { Array.elements; comments } = expr in
      let elements' = ListUtils.ident_map (map_opt this#expression_or_spread) elements in
      let comments' = this#syntax_opt comments in
      if elements == elements' && comments == comments' then
        expr
      else
        { Array.elements = elements'; comments = comments' }

    method arrow_function loc (expr : ('loc, 'loc) Ast.Function.t) = this#function_ loc expr

    method assignment _loc (expr : ('loc, 'loc) Ast.Expression.Assignment.t) =
      let open Ast.Expression.Assignment in
      let { operator = _; left; right } = expr in
      let left' = this#assignment_pattern left in
      let right' = this#expression right in
      if left == left' && right == right' then
        expr
      else
        { expr with left = left'; right = right' }

    method binary _loc (expr : ('loc, 'loc) Ast.Expression.Binary.t) =
      let open Ast.Expression.Binary in
      let { operator = _; left; right } = expr in
      let left' = this#expression left in
      let right' = this#expression right in
      if left == left' && right == right' then
        expr
      else
        { expr with left = left'; right = right' }

    method block _loc (stmt : ('loc, 'loc) Ast.Statement.Block.t) =
      let open Ast.Statement.Block in
      let { body } = stmt in
      let body' = this#statement_list body in
      if body == body' then
        stmt
      else
        { body = body' }

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
      let { callee; targs; arguments } = expr in
      let callee' = this#expression callee in
      let targs' = map_opt this#call_type_args targs in
      let arguments' = this#call_arguments arguments in
      if callee == callee' && targs == targs' && arguments == arguments' then
        expr
      else
        { callee = callee'; targs = targs'; arguments = arguments' }

    method call_arguments (arg_list : ('loc, 'loc) Ast.Expression.ArgList.t) =
      let (loc, arguments) = arg_list in
      let arguments' = ListUtils.ident_map this#expression_or_spread arguments in
      if arguments == arguments' then
        arg_list
      else
        (loc, arguments')

    method optional_call loc (expr : ('loc, 'loc) Ast.Expression.OptionalCall.t) =
      let open Ast.Expression.OptionalCall in
      let { call; optional = _ } = expr in
      let call' = this#call loc call in
      if call == call' then
        expr
      else
        { expr with call = call' }

    method call_type_args (targs : ('loc, 'loc) Ast.Expression.CallTypeArgs.t) =
      let (loc, ts) = targs in
      let ts' = ListUtils.ident_map this#call_type_arg ts in
      if ts' == ts then
        targs
      else
        (loc, ts')

    method call_type_arg t =
      let open Ast.Expression.CallTypeArg in
      match t with
      | Explicit x ->
        let x' = this#type_ x in
        if x' == x then
          t
        else
          Explicit x'
      | Implicit _ -> t

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

    method class_ _loc (cls : ('loc, 'loc) Ast.Class.t) =
      let open Ast.Class in
      let { id; body; tparams = _; extends; implements = _; classDecorators = _; comments } = cls in
      let id' = map_opt this#class_identifier id in
      let body' = this#class_body body in
      let extends' = map_opt (map_loc this#class_extends) extends in
      let comments' = this#syntax_opt comments in
      if id == id' && body == body' && extends == extends' && comments = comments' then
        cls
      else
        { cls with id = id'; body = body'; extends = extends'; comments = comments' }

    method class_extends _loc (extends : ('loc, 'loc) Ast.Class.Extends.t') =
      let open Ast.Class.Extends in
      let { expr; targs } = extends in
      let expr' = this#expression expr in
      let targs' = map_opt this#type_args targs in
      if expr == expr' && targs == targs' then
        extends
      else
        { expr = expr'; targs = targs' }

    method class_identifier (ident : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let ident

    method class_body (cls_body : ('loc, 'loc) Ast.Class.Body.t) =
      let open Ast.Class.Body in
      let (loc, { body }) = cls_body in
      let body' = ListUtils.ident_map this#class_element body in
      if body == body' then
        cls_body
      else
        (loc, { body = body' })

    method class_element (elem : ('loc, 'loc) Ast.Class.Body.element) =
      let open Ast.Class.Body in
      match elem with
      | Method (loc, meth) ->
        id_loc this#class_method loc meth elem (fun meth -> Method (loc, meth))
      | Property (loc, prop) ->
        id_loc this#class_property loc prop elem (fun prop -> Property (loc, prop))
      | PrivateField (loc, field) ->
        id_loc this#class_private_field loc field elem (fun field -> PrivateField (loc, field))

    method class_method _loc (meth : ('loc, 'loc) Ast.Class.Method.t') =
      let open Ast.Class.Method in
      let { kind = _; key; value; static = _; decorators = _ } = meth in
      let key' = this#object_key key in
      let value' = map_loc this#function_expression value in
      if key == key' && value == value' then
        meth
      else
        { meth with key = key'; value = value' }

    method class_property _loc (prop : ('loc, 'loc) Ast.Class.Property.t') =
      let open Ast.Class.Property in
      let { key; value; annot; static = _; variance = _ } = prop in
      let key' = this#object_key key in
      let value' = map_opt this#expression value in
      let annot' = this#type_annotation_hint annot in
      if key == key' && value == value' && annot' == annot then
        prop
      else
        { prop with key = key'; value = value'; annot = annot' }

    method class_private_field _loc (prop : ('loc, 'loc) Ast.Class.PrivateField.t') =
      let open Ast.Class.PrivateField in
      let { key; value; annot; static = _; variance = _ } = prop in
      let key' = this#private_name key in
      let value' = map_opt this#expression value in
      let annot' = this#type_annotation_hint annot in
      if key == key' && value == value' && annot' == annot then
        prop
      else
        { prop with key = key'; value = value'; annot = annot' }

    (* TODO *)
    method comprehension _loc (expr : ('loc, 'loc) Ast.Expression.Comprehension.t) = expr

    method conditional _loc (expr : ('loc, 'loc) Ast.Expression.Conditional.t) =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate } = expr in
      let test' = this#predicate_expression test in
      let consequent' = this#expression consequent in
      let alternate' = this#expression alternate in
      if test == test' && consequent == consequent' && alternate == alternate' then
        expr
      else
        { test = test'; consequent = consequent'; alternate = alternate' }

    method continue _loc (cont : 'loc Ast.Statement.Continue.t) =
      let open Ast.Statement.Continue in
      let { label; comments } = cont in
      let label' = map_opt this#label_identifier label in
      let comments' = this#syntax_opt comments in
      if label == label' then
        cont
      else
        { label = label'; comments = comments' }

    method debugger _loc = ()

    method declare_class _loc (decl : ('loc, 'loc) Ast.Statement.DeclareClass.t) =
      let open Ast.Statement.DeclareClass in
      let { id = ident; tparams; body; extends; mixins; implements } = decl in
      let id' = this#class_identifier ident in
      let tparams' = map_opt this#type_params tparams in
      let body' = map_loc this#object_type body in
      let extends' = map_opt (map_loc this#generic_type) extends in
      let mixins' = ListUtils.ident_map (map_loc this#generic_type) mixins in
      if
        id' == ident
        && tparams' == tparams
        && body' == body
        && extends' == extends
        && mixins' == mixins
      then
        decl
      else
        {
          id = id';
          tparams = tparams';
          body = body';
          extends = extends';
          mixins = mixins';
          implements;
        }

    method declare_export_declaration
        _loc (decl : ('loc, 'loc) Ast.Statement.DeclareExportDeclaration.t) =
      let open Ast.Statement.DeclareExportDeclaration in
      let { default; source; specifiers; declaration } = decl in
      let specifiers' = map_opt this#export_named_specifier specifiers in
      let declaration' = map_opt this#declare_export_declaration_decl declaration in
      if specifiers == specifiers' && declaration == declaration' then
        decl
      else
        { default; source; specifiers = specifiers'; declaration = declaration' }

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

    method declare_function _loc (decl : ('loc, 'loc) Ast.Statement.DeclareFunction.t) =
      let open Ast.Statement.DeclareFunction in
      let { id = ident; annot; predicate } = decl in
      let id' = this#function_identifier ident in
      let annot' = this#type_annotation annot in
      (* TODO: walk predicate *)
      if id' == ident && annot' == annot then
        decl
      else
        { id = id'; annot = annot'; predicate }

    method declare_interface loc (decl : ('loc, 'loc) Ast.Statement.Interface.t) =
      this#interface loc decl

    method declare_module _loc (m : ('loc, 'loc) Ast.Statement.DeclareModule.t) =
      let open Ast.Statement.DeclareModule in
      let { id; body; kind } = m in
      let body' = map_loc this#block body in
      if body' == body then
        m
      else
        { id; body = body'; kind }

    (* TODO *)
    method declare_module_exports _loc (annot : ('loc, 'loc) Ast.Type.annotation) = annot

    method declare_type_alias loc (decl : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      this#type_alias loc decl

    method declare_variable _loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      let { id = ident; annot } = decl in
      let id' = this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Var ident in
      let annot' = this#type_annotation_hint annot in
      if id' == ident && annot' == annot then
        decl
      else
        { id = id'; annot = annot' }

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

    method empty _loc = ()

    method enum_declaration _loc (enum : ('loc, 'loc) Ast.Statement.EnumDeclaration.t) =
      let open Ast.Statement.EnumDeclaration in
      let { id = ident; body } = enum in
      let id' = this#identifier ident in
      let body' =
        match body with
        | (loc, BooleanBody boolean_body) ->
          id this#enum_boolean_body boolean_body body (fun body -> (loc, BooleanBody body))
        | (loc, NumberBody number_body) ->
          id this#enum_number_body number_body body (fun body -> (loc, NumberBody body))
        | (loc, StringBody string_body) ->
          id this#enum_string_body string_body body (fun body -> (loc, StringBody body))
        | (loc, SymbolBody symbol_body) ->
          id this#enum_symbol_body symbol_body body (fun body -> (loc, SymbolBody body))
      in
      if ident == id' && body == body' then
        enum
      else
        { id = id'; body = body' }

    method enum_boolean_body (body : 'loc Ast.Statement.EnumDeclaration.BooleanBody.t) =
      let open Ast.Statement.EnumDeclaration.BooleanBody in
      let { members; explicitType = _ } = body in
      let members' = ListUtils.ident_map this#enum_boolean_member members in
      if members == members' then
        body
      else
        { body with members = members' }

    method enum_number_body (body : 'loc Ast.Statement.EnumDeclaration.NumberBody.t) =
      let open Ast.Statement.EnumDeclaration.NumberBody in
      let { members; explicitType = _ } = body in
      let members' = ListUtils.ident_map this#enum_number_member members in
      if members == members' then
        body
      else
        { body with members = members' }

    method enum_string_body (body : 'loc Ast.Statement.EnumDeclaration.StringBody.t) =
      let open Ast.Statement.EnumDeclaration.StringBody in
      let { members; explicitType = _ } = body in
      let members' =
        match members with
        | Defaulted members -> Defaulted (ListUtils.ident_map this#enum_defaulted_member members)
        | Initialized members -> Initialized (ListUtils.ident_map this#enum_string_member members)
      in
      if members == members' then
        body
      else
        { body with members = members' }

    method enum_symbol_body (body : 'loc Ast.Statement.EnumDeclaration.SymbolBody.t) =
      let open Ast.Statement.EnumDeclaration.SymbolBody in
      let { members } = body in
      let members' = ListUtils.ident_map this#enum_defaulted_member members in
      if members == members' then
        body
      else
        { members = members' }

    method enum_defaulted_member (member : 'loc Ast.Statement.EnumDeclaration.DefaultedMember.t) =
      let open Ast.Statement.EnumDeclaration.DefaultedMember in
      let (loc, { id = ident }) = member in
      let id' = this#identifier ident in
      if ident = id' then
        member
      else
        (loc, { id = id' })

    method enum_boolean_member
        (member : (bool, 'loc) Ast.Statement.EnumDeclaration.InitializedMember.t) =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (loc, { id = ident; init }) = member in
      let id' = this#identifier ident in
      if ident = id' then
        member
      else
        (loc, { id = id'; init })

    method enum_number_member
        (member : (Ast.NumberLiteral.t, 'loc) Ast.Statement.EnumDeclaration.InitializedMember.t) =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (loc, { id = ident; init }) = member in
      let id' = this#identifier ident in
      if ident = id' then
        member
      else
        (loc, { id = id'; init })

    method enum_string_member
        (member : (Ast.StringLiteral.t, 'loc) Ast.Statement.EnumDeclaration.InitializedMember.t) =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (loc, { id = ident; init }) = member in
      let id' = this#identifier ident in
      if ident = id' then
        member
      else
        (loc, { id = id'; init })

    method export_default_declaration
        _loc (decl : ('loc, 'loc) Ast.Statement.ExportDefaultDeclaration.t) =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default; declaration } = decl in
      let declaration' = this#export_default_declaration_decl declaration in
      if declaration' = declaration then
        decl
      else
        { default; declaration = declaration' }

    method export_default_declaration_decl
        (decl : ('loc, 'loc) Ast.Statement.ExportDefaultDeclaration.declaration) =
      let open Ast.Statement.ExportDefaultDeclaration in
      match decl with
      | Declaration stmt -> id this#statement stmt decl (fun stmt -> Declaration stmt)
      | Expression expr -> id this#expression expr decl (fun expr -> Expression expr)

    method export_named_declaration
        _loc (decl : ('loc, 'loc) Ast.Statement.ExportNamedDeclaration.t) =
      let open Ast.Statement.ExportNamedDeclaration in
      let { exportKind; source; specifiers; declaration } = decl in
      let specifiers' = map_opt this#export_named_specifier specifiers in
      let declaration' = map_opt this#statement declaration in
      if specifiers == specifiers' && declaration == declaration' then
        decl
      else
        { exportKind; source; specifiers = specifiers'; declaration = declaration' }

    method export_named_declaration_specifier
        (spec : 'loc Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t) =
      let open Ast.Statement.ExportNamedDeclaration.ExportSpecifier in
      let (loc, { local; exported }) = spec in
      let local' = this#identifier local in
      let exported' = map_opt this#identifier exported in
      if local == local' && exported == exported' then
        spec
      else
        (loc, { local = local'; exported = exported' })

    method export_named_specifier (spec : 'loc Ast.Statement.ExportNamedDeclaration.specifier) =
      let open Ast.Statement.ExportNamedDeclaration in
      match spec with
      | ExportSpecifiers spec_list ->
        let spec_list' = ListUtils.ident_map this#export_named_declaration_specifier spec_list in
        if spec_list == spec_list' then
          spec
        else
          ExportSpecifiers spec_list'
      | ExportBatchSpecifier (loc, id_opt) ->
        let id_opt' = map_opt this#identifier id_opt in
        if id_opt == id_opt' then
          spec
        else
          ExportBatchSpecifier (loc, id_opt')

    method expression_statement _loc (stmt : ('loc, 'loc) Ast.Statement.Expression.t) =
      let open Ast.Statement.Expression in
      let { expression = expr; directive = _ } = stmt in
      id this#expression expr stmt (fun expression -> { stmt with expression })

    method expression_or_spread expr_or_spread =
      let open Ast.Expression in
      match expr_or_spread with
      | Expression expr -> id this#expression expr expr_or_spread (fun expr -> Expression expr)
      | Spread spread -> id this#spread_element spread expr_or_spread (fun spread -> Spread spread)

    method for_in_statement _loc (stmt : ('loc, 'loc) Ast.Statement.ForIn.t) =
      let open Ast.Statement.ForIn in
      let { left; right; body; each } = stmt in
      let left' = this#for_in_statement_lhs left in
      let right' = this#expression right in
      let body' = this#statement body in
      if left == left' && right == right' && body == body' then
        stmt
      else
        { left = left'; right = right'; body = body'; each }

    method for_in_statement_lhs (left : ('loc, 'loc) Ast.Statement.ForIn.left) =
      let open Ast.Statement.ForIn in
      match left with
      | LeftDeclaration (loc, decl) ->
        id_loc this#variable_declaration loc decl left (fun decl -> LeftDeclaration (loc, decl))
      | LeftPattern patt ->
        id this#for_in_assignment_pattern patt left (fun patt -> LeftPattern patt)

    method for_of_statement _loc (stuff : ('loc, 'loc) Ast.Statement.ForOf.t) =
      let open Ast.Statement.ForOf in
      let { left; right; body; async } = stuff in
      let left' = this#for_of_statement_lhs left in
      let right' = this#expression right in
      let body' = this#statement body in
      if left == left' && right == right' && body == body' then
        stuff
      else
        { left = left'; right = right'; body = body'; async }

    method for_of_statement_lhs (left : ('loc, 'loc) Ast.Statement.ForOf.left) =
      let open Ast.Statement.ForOf in
      match left with
      | LeftDeclaration (loc, decl) ->
        id_loc this#variable_declaration loc decl left (fun decl -> LeftDeclaration (loc, decl))
      | LeftPattern patt ->
        id this#for_of_assignment_pattern patt left (fun patt -> LeftPattern patt)

    method for_statement _loc (stmt : ('loc, 'loc) Ast.Statement.For.t) =
      let open Ast.Statement.For in
      let { init; test; update; body } = stmt in
      let init' = map_opt this#for_statement_init init in
      let test' = map_opt this#predicate_expression test in
      let update' = map_opt this#expression update in
      let body' = this#statement body in
      if init == init' && test == test' && update == update' && body == body' then
        stmt
      else
        { init = init'; test = test'; update = update'; body = body' }

    method for_statement_init (init : ('loc, 'loc) Ast.Statement.For.init) =
      let open Ast.Statement.For in
      match init with
      | InitDeclaration (loc, decl) ->
        id_loc this#variable_declaration loc decl init (fun decl -> InitDeclaration (loc, decl))
      | InitExpression expr -> id this#expression expr init (fun expr -> InitExpression expr)

    method function_param_type (fpt : ('loc, 'loc) Ast.Type.Function.Param.t) =
      let open Ast.Type.Function.Param in
      let (loc, { annot; name; optional }) = fpt in
      let annot' = this#type_ annot in
      if annot' == annot then
        fpt
      else
        (loc, { annot = annot'; name; optional })

    method function_rest_param_type (frpt : ('loc, 'loc) Ast.Type.Function.RestParam.t) =
      let open Ast.Type.Function.RestParam in
      let (loc, { argument }) = frpt in
      let argument' = this#function_param_type argument in
      if argument' == argument then
        frpt
      else
        (loc, { argument = argument' })

    method function_type _loc (ft : ('loc, 'loc) Ast.Type.Function.t) =
      let open Ast.Type.Function in
      let { params = (params_loc, { Params.params = ps; rest = rpo }); return; tparams } = ft in
      let ps' = ListUtils.ident_map this#function_param_type ps in
      let rpo' = map_opt this#function_rest_param_type rpo in
      let return' = this#type_ return in
      let tparams' = map_opt this#type_params tparams in
      if ps' == ps && rpo' == rpo && return' == return && tparams' == tparams then
        ft
      else
        {
          params = (params_loc, { Params.params = ps'; rest = rpo' });
          return = return';
          tparams = tparams';
        }

    method label_identifier (ident : ('loc, 'loc) Ast.Identifier.t) = this#identifier ident

    method object_property_value_type (opvt : ('loc, 'loc) Ast.Type.Object.Property.value) =
      let open Ast.Type.Object.Property in
      match opvt with
      | Init t -> id this#type_ t opvt (fun t -> Init t)
      | Get (loc, ft) -> id_loc this#function_type loc ft opvt (fun ft -> Get (loc, ft))
      | Set (loc, ft) -> id_loc this#function_type loc ft opvt (fun ft -> Set (loc, ft))

    method object_property_type (opt : ('loc, 'loc) Ast.Type.Object.Property.t) =
      let open Ast.Type.Object.Property in
      let (loc, { key; value; optional; static; proto; _method; variance }) = opt in
      let value' = this#object_property_value_type value in
      if value' == value then
        opt
      else
        (loc, { key; value = value'; optional; static; proto; _method; variance })

    method object_spread_property_type (opt : ('loc, 'loc) Ast.Type.Object.SpreadProperty.t) =
      let open Ast.Type.Object.SpreadProperty in
      let (loc, { argument }) = opt in
      let argument' = this#type_ argument in
      if argument' == argument then
        opt
      else
        (loc, { argument = argument' })

    method object_indexer_property_type (opt : ('loc, 'loc) Ast.Type.Object.Indexer.t) =
      let open Ast.Type.Object.Indexer in
      let (loc, { id; key; value; static; variance }) = opt in
      let key' = this#type_ key in
      let value' = this#type_ value in
      if key' == key && value' == value then
        opt
      else
        (loc, { id; key = key'; value = value'; static; variance })

    method object_type _loc (ot : ('loc, 'loc) Ast.Type.Object.t) =
      let open Ast.Type.Object in
      let { properties; exact; inexact } = ot in
      let properties' =
        ListUtils.ident_map
          (fun p ->
            match p with
            | Property p' -> id this#object_property_type p' p (fun p' -> Property p')
            | SpreadProperty p' ->
              id this#object_spread_property_type p' p (fun p' -> SpreadProperty p')
            | Indexer p' -> id this#object_indexer_property_type p' p (fun p' -> Indexer p')
            | CallProperty _
            | InternalSlot _ ->
              p) (* TODO *)
          properties
      in
      if properties' == properties then
        ot
      else
        { properties = properties'; exact; inexact }

    method interface_type _loc (i : ('loc, 'loc) Ast.Type.Interface.t) =
      let open Ast.Type.Interface in
      let { extends; body } = i in
      let extends' = ListUtils.ident_map (map_loc this#generic_type) extends in
      let body' = map_loc this#object_type body in
      if extends' == extends && body' == body then
        i
      else
        { extends = extends'; body = body' }

    method generic_identifier_type (git : ('loc, 'loc) Ast.Type.Generic.Identifier.t) =
      let open Ast.Type.Generic.Identifier in
      match git with
      | Unqualified i -> id this#identifier i git (fun i -> Unqualified i)
      | Qualified (loc, { qualification; id }) ->
        let qualification' = this#generic_identifier_type qualification in
        let id' = this#identifier id in
        if qualification' == qualification && id' == id then
          git
        else
          Qualified (loc, { qualification = qualification'; id = id' })

    method variance (variance : 'loc Ast.Variance.t option) = variance

    method type_args (targs : ('loc, 'loc) Ast.Type.TypeArgs.t) =
      let (loc, ts) = targs in
      let ts' = ListUtils.ident_map this#type_ ts in
      if ts' == ts then
        targs
      else
        (loc, ts')

    method type_params (tparams : ('loc, 'loc) Ast.Type.TypeParams.t) =
      let (loc, tps) = tparams in
      let tps' = ListUtils.ident_map this#type_param tps in
      if tps' == tps then
        tparams
      else
        (loc, tps')

    method type_param (tparam : ('loc, 'loc) Ast.Type.TypeParam.t) =
      let open Ast.Type.TypeParam in
      let (loc, { name; bound; variance; default }) = tparam in
      let name' = this#identifier name in
      let bound' = this#type_annotation_hint bound in
      let variance' = this#variance variance in
      let default' = map_opt this#type_ default in
      if name' == name && bound' == bound && variance' == variance && default' == default then
        tparam
      else
        (loc, { name = name'; bound = bound'; variance = variance'; default = default' })

    method generic_type _loc (gt : ('loc, 'loc) Ast.Type.Generic.t) =
      let open Ast.Type.Generic in
      let { id; targs } = gt in
      let id' = this#generic_identifier_type id in
      let targs' = map_opt this#type_args targs in
      if id' == id && targs' == targs then
        gt
      else
        { id = id'; targs = targs' }

    method string_literal_type _loc (lit : Ast.StringLiteral.t) = lit

    method type_ (t : ('loc, 'loc) Ast.Type.t) =
      let open Ast.Type in
      match t with
      | (_, Any)
      | (_, Mixed)
      | (_, Empty)
      | (_, Void)
      | (_, Null)
      | (_, Symbol)
      | (_, Number)
      | (_, BigInt)
      | (_, String)
      | (_, Boolean)
      | (_, NumberLiteral _)
      | (_, BigIntLiteral _)
      | (_, BooleanLiteral _)
      | (_, Exists) ->
        t
      | (loc, Nullable t') -> id this#type_ t' t (fun t' -> (loc, Nullable t'))
      | (loc, Array t') -> id this#type_ t' t (fun t' -> (loc, Array t'))
      | (loc, Typeof t') -> id this#type_ t' t (fun t' -> (loc, Typeof t'))
      | (loc, Function ft) -> id_loc this#function_type loc ft t (fun ft -> (loc, Function ft))
      | (loc, Object ot) -> id_loc this#object_type loc ot t (fun ot -> (loc, Object ot))
      | (loc, Interface i) -> id_loc this#interface_type loc i t (fun i -> (loc, Interface i))
      | (loc, Generic gt) -> id_loc this#generic_type loc gt t (fun gt -> (loc, Generic gt))
      | (loc, StringLiteral lit) ->
        id_loc this#string_literal_type loc lit t (fun lit -> (loc, StringLiteral lit))
      | (loc, Union (t0, t1, ts)) ->
        let t0' = this#type_ t0 in
        let t1' = this#type_ t1 in
        let ts' = ListUtils.ident_map this#type_ ts in
        if t0' == t0 && t1' == t1 && ts' == ts then
          t
        else
          (loc, Union (t0', t1', ts'))
      | (loc, Intersection (t0, t1, ts)) ->
        let t0' = this#type_ t0 in
        let t1' = this#type_ t1 in
        let ts' = ListUtils.ident_map this#type_ ts in
        if t0' == t0 && t1' == t1 && ts' == ts then
          t
        else
          (loc, Intersection (t0', t1', ts'))
      | (loc, Tuple ts) ->
        let ts' = ListUtils.ident_map this#type_ ts in
        if ts' == ts then
          t
        else
          (loc, Tuple ts')

    method type_annotation (annot : ('loc, 'loc) Ast.Type.annotation) =
      let (loc, a) = annot in
      id this#type_ a annot (fun a -> (loc, a))

    method type_annotation_hint (return : ('M, 'T) Ast.Type.annotation_or_hint) =
      let open Ast.Type in
      match return with
      | Available annot ->
        let annot' = this#type_annotation annot in
        if annot' == annot then
          return
        else
          Available annot'
      | Missing _loc -> return

    method function_declaration loc (stmt : ('loc, 'loc) Ast.Function.t) = this#function_ loc stmt

    method function_expression loc (stmt : ('loc, 'loc) Ast.Function.t) = this#function_ loc stmt

    (* Internal helper for function declarations, function expressions and arrow functions *)
    method function_ _loc (expr : ('loc, 'loc) Ast.Function.t) =
      let open Ast.Function in
      let { id = ident; params; body; async; generator; predicate; return; tparams; sig_loc } =
        expr
      in
      let ident' = map_opt this#function_identifier ident in
      let params' = this#function_params params in
      let return' = this#type_annotation_hint return in
      let body' = this#function_body_any body in
      (* TODO: walk predicate *)
      let tparams' = map_opt this#type_params tparams in
      if
        ident == ident'
        && params == params'
        && body == body'
        && return == return'
        && tparams == tparams'
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
          predicate;
          tparams = tparams';
          sig_loc;
        }

    method function_params (params : ('loc, 'loc) Ast.Function.Params.t) =
      let open Ast.Function in
      let (loc, { Params.params = params_list; rest }) = params in
      let params_list' = ListUtils.ident_map this#function_param params_list in
      let rest' = map_opt this#function_rest_param rest in
      if params_list == params_list' && rest == rest' then
        params
      else
        (loc, { Params.params = params_list'; rest = rest' })

    method function_param (param : ('loc, 'loc) Ast.Function.Param.t) =
      let open Ast.Function.Param in
      let (loc, { argument; default }) = param in
      let argument' = this#function_param_pattern argument in
      let default' = map_opt this#expression default in
      if argument == argument' && default == default' then
        param
      else
        (loc, { argument = argument'; default = default' })

    method function_body_any (body : ('loc, 'loc) Ast.Function.body) =
      match body with
      | Ast.Function.BodyBlock (loc, block) ->
        id_loc this#function_body loc block body (fun block -> Ast.Function.BodyBlock (loc, block))
      | Ast.Function.BodyExpression expr ->
        id this#expression expr body (fun expr -> Ast.Function.BodyExpression expr)

    method function_body loc (block : ('loc, 'loc) Ast.Statement.Block.t) = this#block loc block

    method function_identifier (ident : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Var ident

    (* TODO *)
    method generator _loc (expr : ('loc, 'loc) Ast.Expression.Generator.t) = expr

    method identifier (id : ('loc, 'loc) Ast.Identifier.t) =
      let open Ast.Identifier in
      let (loc, { name; comments }) = id in
      let comments' = this#syntax_opt comments in
      if comments == comments' then
        id
      else
        (loc, { name; comments = comments' })

    method interface _loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      let { id = ident; tparams; extends; body } = interface in
      let id' = this#class_identifier ident in
      let tparams' = map_opt this#type_params tparams in
      let extends' = ListUtils.ident_map (map_loc this#generic_type) extends in
      let body' = map_loc this#object_type body in
      if id' == ident && tparams' == tparams && extends' == extends && body' == body then
        interface
      else
        { id = id'; tparams = tparams'; extends = extends'; body = body' }

    method interface_declaration loc (decl : ('loc, 'loc) Ast.Statement.Interface.t) =
      this#interface loc decl

    method private_name (expr : 'loc Ast.PrivateName.t) = expr

    method import _loc (expr : ('loc, 'loc) Ast.Expression.t) = expr

    method if_consequent_statement ~has_else (stmt : ('loc, 'loc) Ast.Statement.t) =
      ignore has_else;
      this#statement stmt

    method if_alternate_statement (stmt : ('loc, 'loc) Ast.Statement.t) = this#statement stmt

    method if_statement _loc (stmt : ('loc, 'loc) Ast.Statement.If.t) =
      let open Ast.Statement.If in
      let { test; consequent; alternate; comments } = stmt in
      let test' = this#predicate_expression test in
      let consequent' = this#if_consequent_statement ~has_else:(alternate <> None) consequent in
      let alternate' = map_opt this#if_alternate_statement alternate in
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
      let { importKind; source; specifiers; default } = decl in
      let specifiers' = map_opt this#import_specifier specifiers in
      let default' = map_opt this#import_default_specifier default in
      if specifiers == specifiers' && default == default' then
        decl
      else
        { importKind; source; specifiers = specifiers'; default = default' }

    method import_specifier (specifier : ('loc, 'loc) Ast.Statement.ImportDeclaration.specifier) =
      let open Ast.Statement.ImportDeclaration in
      match specifier with
      | ImportNamedSpecifiers named_specifiers ->
        let named_specifiers' = ListUtils.ident_map this#import_named_specifier named_specifiers in
        if named_specifiers == named_specifiers' then
          specifier
        else
          ImportNamedSpecifiers named_specifiers'
      | ImportNamespaceSpecifier (loc, ident) ->
        id_loc this#import_namespace_specifier loc ident specifier (fun ident ->
            ImportNamespaceSpecifier (loc, ident))

    method import_named_specifier
        (specifier : ('loc, 'loc) Ast.Statement.ImportDeclaration.named_specifier) =
      let open Ast.Statement.ImportDeclaration in
      let { kind; local; remote } = specifier in
      let remote' = this#identifier remote in
      let local' =
        match local with
        | None -> None
        | Some ident ->
          id
            (this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let)
            ident
            local
            (fun ident -> Some ident)
      in
      if local == local' && remote == remote' then
        specifier
      else
        { kind; local = local'; remote = remote' }

    method import_default_specifier (id : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let id

    method import_namespace_specifier _loc (id : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let id

    method jsx_element _loc (expr : ('loc, 'loc) Ast.JSX.element) =
      let open Ast.JSX in
      let { openingElement; closingElement; children } = expr in
      let openingElement' = this#jsx_opening_element openingElement in
      let closingElement' = map_opt this#jsx_closing_element closingElement in
      let children' = this#jsx_children children in
      if
        openingElement == openingElement'
        && closingElement == closingElement'
        && children == children'
      then
        expr
      else
        { openingElement = openingElement'; closingElement = closingElement'; children = children' }

    method jsx_fragment _loc (expr : ('loc, 'loc) Ast.JSX.fragment) =
      let open Ast.JSX in
      let { frag_children; _ } = expr in
      let children' = this#jsx_children frag_children in
      if frag_children == children' then
        expr
      else
        { expr with frag_children = children' }

    method jsx_opening_element (elem : ('loc, 'loc) Ast.JSX.Opening.t) =
      let open Ast.JSX.Opening in
      let (loc, { name; selfClosing; attributes }) = elem in
      let name' = this#jsx_name name in
      let attributes' = ListUtils.ident_map this#jsx_opening_attribute attributes in
      if name == name' && attributes == attributes' then
        elem
      else
        (loc, { name = name'; selfClosing; attributes = attributes' })

    method jsx_closing_element (elem : ('loc, 'loc) Ast.JSX.Closing.t) =
      let open Ast.JSX.Closing in
      let (loc, { name }) = elem in
      let name' = this#jsx_name name in
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
      let { argument } = attr in
      id this#expression argument attr (fun argument -> { argument })

    method jsx_attribute (attr : ('loc, 'loc) Ast.JSX.Attribute.t) =
      let open Ast.JSX.Attribute in
      let (loc, { name; value }) = attr in
      let value' = map_opt this#jsx_attribute_value value in
      if value == value' then
        attr
      else
        (loc, { name; value = value' })

    method jsx_attribute_value (value : ('loc, 'loc) Ast.JSX.Attribute.value) =
      let open Ast.JSX.Attribute in
      match value with
      | Literal _ -> value
      | ExpressionContainer (loc, expr) ->
        id_loc this#jsx_expression loc expr value (fun expr -> ExpressionContainer (loc, expr))

    method jsx_children ((loc, children) as orig : 'loc * ('loc, 'loc) Ast.JSX.child list) =
      let children' = ListUtils.ident_map this#jsx_child children in
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
      | (loc, SpreadChild expr) ->
        id this#expression expr child (fun expr -> (loc, SpreadChild expr))
      | (_loc, Text _) -> child

    method jsx_expression _loc (jsx_expr : ('loc, 'loc) Ast.JSX.ExpressionContainer.t) =
      let open Ast.JSX.ExpressionContainer in
      let { expression } = jsx_expr in
      match expression with
      | Expression expr ->
        id this#expression expr jsx_expr (fun expr -> { expression = Expression expr })
      | EmptyExpression -> jsx_expr

    method jsx_name (name : ('loc, 'loc) Ast.JSX.name) =
      let open Ast.JSX in
      let name' =
        match name with
        | Identifier id -> Identifier (this#jsx_identifier id)
        | NamespacedName namespaced_name ->
          NamespacedName (this#jsx_namespaced_name namespaced_name)
        | MemberExpression member_exp -> MemberExpression (this#jsx_member_expression member_exp)
      in
      (* structural equality since it's easier than checking equality in each branch of the match
       * above *)
      if name = name' then
        name
      else
        name'

    method jsx_namespaced_name (namespaced_name : ('loc, 'loc) Ast.JSX.NamespacedName.t) =
      let open Ast.JSX in
      NamespacedName.(
        let (loc, { namespace; name }) = namespaced_name in
        let namespace' = this#jsx_identifier namespace in
        let name' = this#jsx_identifier name in
        if namespace == namespace' && name == name' then
          namespaced_name
        else
          (loc, { namespace = namespace'; name = name' }))

    method jsx_member_expression (member_exp : ('loc, 'loc) Ast.JSX.MemberExpression.t) =
      let open Ast.JSX in
      let (loc, { MemberExpression._object; MemberExpression.property }) = member_exp in
      let _object' =
        match _object with
        | MemberExpression.Identifier id ->
          let id' = this#jsx_identifier id in
          if id' == id then
            _object
          else
            MemberExpression.Identifier id'
        | MemberExpression.MemberExpression nested_exp ->
          let nested_exp' = this#jsx_member_expression nested_exp in
          if nested_exp' == nested_exp then
            _object
          else
            MemberExpression.MemberExpression nested_exp'
      in
      let property' = this#jsx_identifier property in
      if _object == _object' && property == property' then
        member_exp
      else
        (loc, MemberExpression.{ _object = _object'; property = property' })

    method jsx_identifier (id : 'loc Ast.JSX.Identifier.t) = id

    method labeled_statement _loc (stmt : ('loc, 'loc) Ast.Statement.Labeled.t) =
      let open Ast.Statement.Labeled in
      let { label; body } = stmt in
      let label' = this#label_identifier label in
      let body' = this#statement body in
      if label == label' && body == body' then
        stmt
      else
        { label = label'; body = body' }

    method literal _loc (expr : 'loc Ast.Literal.t) = expr

    method logical _loc (expr : ('loc, 'loc) Ast.Expression.Logical.t) =
      let open Ast.Expression.Logical in
      let { operator = _; left; right } = expr in
      let left' = this#expression left in
      let right' = this#expression right in
      if left == left' && right == right' then
        expr
      else
        { expr with left = left'; right = right' }

    method member _loc (expr : ('loc, 'loc) Ast.Expression.Member.t) =
      let open Ast.Expression.Member in
      let { _object; property } = expr in
      let _object' = this#expression _object in
      let property' = this#member_property property in
      if _object == _object' && property == property' then
        expr
      else
        { _object = _object'; property = property' }

    method optional_member loc (expr : ('loc, 'loc) Ast.Expression.OptionalMember.t) =
      let open Ast.Expression.OptionalMember in
      let { member; optional = _ } = expr in
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

    (* TODO *)
    method meta_property _loc (expr : 'loc Ast.Expression.MetaProperty.t) = expr

    method new_ _loc (expr : ('loc, 'loc) Ast.Expression.New.t) =
      let open Ast.Expression.New in
      let { callee; targs; arguments; comments } = expr in
      let callee' = this#expression callee in
      let targs' = map_opt this#call_type_args targs in
      let arguments' = map_opt this#call_arguments arguments in
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
        ListUtils.ident_map
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
        if key == key' && value == value' then
          prop
        else
          (loc, Init { key = key'; value = value'; shorthand })
      | (loc, Method { key; value = fn }) ->
        let key' = this#object_key key in
        let fn' = map_loc this#function_expression fn in
        if key == key' && fn == fn' then
          prop
        else
          (loc, Method { key = key'; value = fn' })
      | (loc, Get { key; value = fn }) ->
        let key' = this#object_key key in
        let fn' = map_loc this#function_expression fn in
        if key == key' && fn == fn' then
          prop
        else
          (loc, Get { key = key'; value = fn' })
      | (loc, Set { key; value = fn }) ->
        let key' = this#object_key key in
        let fn' = map_loc this#function_expression fn in
        if key == key' && fn == fn' then
          prop
        else
          (loc, Set { key = key'; value = fn' })

    method object_key (key : ('loc, 'loc) Ast.Expression.Object.Property.key) =
      let open Ast.Expression.Object.Property in
      match key with
      | Literal (loc, lit) -> id_loc this#literal loc lit key (fun lit -> Literal (loc, lit))
      | Identifier ident -> id this#object_key_identifier ident key (fun ident -> Identifier ident)
      | PrivateName ident -> id this#private_name ident key (fun ident -> PrivateName ident)
      | Computed expr -> id this#expression expr key (fun expr -> Computed expr)

    method object_key_identifier (ident : ('loc, 'loc) Ast.Identifier.t) = this#identifier ident

    method opaque_type _loc (otype : ('loc, 'loc) Ast.Statement.OpaqueType.t) =
      let open Ast.Statement.OpaqueType in
      let { id; tparams; impltype; supertype } = otype in
      let id' = this#identifier id in
      let tparams' = map_opt this#type_params tparams in
      let impltype' = map_opt this#type_ impltype in
      let supertype' = map_opt this#type_ supertype in
      if
        id == id'
        && impltype == impltype'
        && tparams == tparams'
        && impltype == impltype'
        && supertype == supertype'
      then
        otype
      else
        { id = id'; tparams = tparams'; impltype = impltype'; supertype = supertype' }

    method function_param_pattern (expr : ('loc, 'loc) Ast.Pattern.t) = this#binding_pattern expr

    method variable_declarator_pattern ~kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#binding_pattern ~kind expr

    method catch_clause_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#binding_pattern ~kind:Ast.Statement.VariableDeclaration.Let expr

    method for_in_assignment_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#assignment_pattern expr

    method for_of_assignment_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#assignment_pattern expr

    method binding_pattern
        ?(kind = Ast.Statement.VariableDeclaration.Var) (expr : ('loc, 'loc) Ast.Pattern.t) =
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
        | Object { Object.properties; annot } ->
          let properties' = ListUtils.ident_map (this#pattern_object_p ?kind) properties in
          let annot' = this#type_annotation_hint annot in
          if properties' == properties && annot' == annot then
            patt
          else
            Object { Object.properties = properties'; annot = annot' }
        | Array { Array.elements; annot; comments } ->
          let elements' = ListUtils.ident_map (map_opt (this#pattern_array_e ?kind)) elements in
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

    method pattern_literal ?kind loc (expr : 'loc Ast.Literal.t) =
      ignore kind;
      this#literal loc expr

    method pattern_object_p ?kind (p : ('loc, 'loc) Ast.Pattern.Object.property) =
      let open Ast.Pattern.Object in
      match p with
      | Property (loc, prop) ->
        id (this#pattern_object_property ?kind) prop p (fun prop -> Property (loc, prop))
      | RestProperty (loc, prop) ->
        id (this#pattern_object_rest_property ?kind) prop p (fun prop -> RestProperty (loc, prop))

    method pattern_object_property ?kind (prop : ('loc, 'loc) Ast.Pattern.Object.Property.t') =
      let open Ast.Pattern.Object.Property in
      let { key; pattern; default; shorthand = _ } = prop in
      let key' = this#pattern_object_property_key ?kind key in
      let pattern' = this#pattern_object_property_pattern ?kind pattern in
      let default' = map_opt this#expression default in
      if key' == key && pattern' == pattern && default' == default then
        prop
      else
        { key = key'; pattern = pattern'; default = default'; shorthand = false }

    method pattern_object_property_key ?kind (key : ('loc, 'loc) Ast.Pattern.Object.Property.key) =
      let open Ast.Pattern.Object.Property in
      match key with
      | Literal (loc, lit) ->
        id_loc (this#pattern_object_property_literal_key ?kind) loc lit key (fun lit' ->
            Literal (loc, lit'))
      | Identifier identifier ->
        id (this#pattern_object_property_identifier_key ?kind) identifier key (fun id' ->
            Identifier id')
      | Computed expr ->
        id (this#pattern_object_property_computed_key ?kind) expr key (fun expr' -> Computed expr')

    method pattern_object_property_literal_key ?kind loc (key : 'loc Ast.Literal.t) =
      this#pattern_literal ?kind loc key

    method pattern_object_property_identifier_key ?kind (key : ('loc, 'loc) Ast.Identifier.t) =
      this#pattern_identifier ?kind key

    method pattern_object_property_computed_key ?kind (key : ('loc, 'loc) Ast.Expression.t) =
      ignore kind;
      this#pattern_expression key

    method pattern_object_rest_property
        ?kind (prop : ('loc, 'loc) Ast.Pattern.Object.RestProperty.t') =
      let open Ast.Pattern.Object.RestProperty in
      let { argument } = prop in
      let argument' = this#pattern_object_rest_property_pattern ?kind argument in
      if argument' == argument then
        prop
      else
        { argument = argument' }

    method pattern_object_property_pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind expr

    method pattern_object_rest_property_pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind expr

    method pattern_array_e ?kind (e : ('loc, 'loc) Ast.Pattern.Array.element) =
      let open Ast.Pattern.Array in
      match e with
      | Element (loc, elem) ->
        id (this#pattern_array_element ?kind) elem e (fun elem -> Element (loc, elem))
      | RestElement (loc, elem) ->
        id (this#pattern_array_rest_element ?kind) elem e (fun elem -> RestElement (loc, elem))

    method pattern_array_element ?kind (elem : ('loc, 'loc) Ast.Pattern.Array.Element.t') =
      let open Ast.Pattern.Array.Element in
      let { argument; default } = elem in
      let argument' = this#pattern_array_element_pattern ?kind argument in
      let default' = map_opt this#expression default in
      if argument == argument' && default == default' then
        elem
      else
        { argument = argument'; default = default' }

    method pattern_array_element_pattern ?kind (patt : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind patt

    method pattern_array_rest_element ?kind (elem : ('loc, 'loc) Ast.Pattern.Array.RestElement.t') =
      let open Ast.Pattern.Array.RestElement in
      let { argument } = elem in
      let argument' = this#pattern_array_rest_element_pattern ?kind argument in
      if argument' == argument then
        elem
      else
        { argument = argument' }

    method pattern_array_rest_element_pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind expr

    method pattern_assignment_pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      this#pattern ?kind expr

    method pattern_expression (expr : ('loc, 'loc) Ast.Expression.t) = this#expression expr

    method predicate_expression (expr : ('loc, 'loc) Ast.Expression.t) = this#expression expr

    method function_rest_param (expr : ('loc, 'loc) Ast.Function.RestParam.t) =
      let open Ast.Function.RestParam in
      let (loc, { argument }) = expr in
      id this#binding_pattern argument expr (fun argument -> (loc, { argument }))

    method return _loc (stmt : ('loc, 'loc) Ast.Statement.Return.t) =
      let open Ast.Statement.Return in
      let { argument; comments } = stmt in
      let argument' = map_opt this#expression argument in
      let comments' = this#syntax_opt comments in
      if argument == argument' && comments == comments' then
        stmt
      else
        { argument = argument'; comments = comments' }

    method sequence _loc (expr : ('loc, 'loc) Ast.Expression.Sequence.t) =
      let open Ast.Expression.Sequence in
      let { expressions } = expr in
      let expressions' = ListUtils.ident_map this#expression expressions in
      if expressions == expressions' then
        expr
      else
        { expressions = expressions' }

    method toplevel_statement_list (stmts : ('loc, 'loc) Ast.Statement.t list) =
      this#statement_list stmts

    method statement_list (stmts : ('loc, 'loc) Ast.Statement.t list) =
      ListUtils.ident_map_multiple this#statement_fork_point stmts

    method statement_fork_point (stmt : ('loc, 'loc) Ast.Statement.t) = [this#statement stmt]

    method spread_element (expr : ('loc, 'loc) Ast.Expression.SpreadElement.t) =
      let open Ast.Expression.SpreadElement in
      let (loc, { argument }) = expr in
      id this#expression argument expr (fun argument -> (loc, { argument }))

    method spread_property (expr : ('loc, 'loc) Ast.Expression.Object.SpreadProperty.t) =
      let open Ast.Expression.Object.SpreadProperty in
      let (loc, { argument }) = expr in
      id this#expression argument expr (fun argument -> (loc, { argument }))

    method switch _loc (switch : ('loc, 'loc) Ast.Statement.Switch.t) =
      let open Ast.Statement.Switch in
      let { discriminant; cases } = switch in
      let discriminant' = this#expression discriminant in
      let cases' = ListUtils.ident_map (map_loc this#switch_case) cases in
      if discriminant == discriminant' && cases == cases' then
        switch
      else
        { discriminant = discriminant'; cases = cases' }

    method switch_case _loc (case : ('loc, 'loc) Ast.Statement.Switch.Case.t') =
      let open Ast.Statement.Switch.Case in
      let { test; consequent } = case in
      let test' = map_opt this#expression test in
      let consequent' = this#statement_list consequent in
      if test == test' && consequent == consequent' then
        case
      else
        { test = test'; consequent = consequent' }

    method tagged_template _loc (expr : ('loc, 'loc) Ast.Expression.TaggedTemplate.t) =
      let open Ast.Expression.TaggedTemplate in
      let { tag; quasi } = expr in
      let tag' = this#expression tag in
      let quasi' = map_loc this#template_literal quasi in
      if tag == tag' && quasi == quasi' then
        expr
      else
        { tag = tag'; quasi = quasi' }

    method template_literal _loc (expr : ('loc, 'loc) Ast.Expression.TemplateLiteral.t) =
      let open Ast.Expression.TemplateLiteral in
      let { quasis; expressions } = expr in
      let quasis' = ListUtils.ident_map this#template_literal_element quasis in
      let expressions' = ListUtils.ident_map this#expression expressions in
      if quasis == quasis' && expressions == expressions' then
        expr
      else
        { quasis = quasis'; expressions = expressions' }

    (* TODO *)
    method template_literal_element (elem : 'loc Ast.Expression.TemplateLiteral.Element.t) = elem

    method throw _loc (stmt : ('loc, 'loc) Ast.Statement.Throw.t) =
      let open Ast.Statement.Throw in
      let { argument } = stmt in
      id this#expression argument stmt (fun argument -> { argument })

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
      let { expression; annot } = expr in
      let expression' = this#expression expression in
      let annot' = this#type_annotation annot in
      if expression' == expression && annot' == annot then
        expr
      else
        { expression = expression'; annot = annot' }

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
      let { argument; operator = _; prefix = _ } = expr in
      id this#expression argument expr (fun argument -> { expr with argument })

    method variable_declaration _loc (decl : ('loc, 'loc) Ast.Statement.VariableDeclaration.t) =
      let open Ast.Statement.VariableDeclaration in
      let { declarations; kind } = decl in
      let decls' = ListUtils.ident_map (this#variable_declarator ~kind) declarations in
      if declarations == decls' then
        decl
      else
        { declarations = decls'; kind }

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
      let { test; body } = stuff in
      let test' = this#predicate_expression test in
      let body' = this#statement body in
      if test == test' && body == body' then
        stuff
      else
        { test = test'; body = body' }

    method with_ _loc (stuff : ('loc, 'loc) Ast.Statement.With.t) =
      let open Ast.Statement.With in
      let { _object; body } = stuff in
      let _object' = this#expression _object in
      let body' = this#statement body in
      if _object == _object' && body == body' then
        stuff
      else
        { _object = _object'; body = body' }

    method type_alias _loc (stuff : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      let open Ast.Statement.TypeAlias in
      let { id; tparams; right } = stuff in
      let id' = this#identifier id in
      let tparams' = map_opt this#type_params tparams in
      let right' = this#type_ right in
      if id == id' && right == right' && tparams == tparams' then
        stuff
      else
        { id = id'; tparams = tparams'; right = right' }

    method yield _loc (expr : ('loc, 'loc) Ast.Expression.Yield.t) =
      let open Ast.Expression.Yield in
      let { argument; delegate; comments } = expr in
      let argument' = map_opt this#expression argument in
      let comments' = this#syntax_opt comments in
      if comments = comments' && argument == argument' then
        expr
      else
        { argument = argument'; delegate; comments = comments' }
  end

let fold_program (mappers : 'a mapper list) ast =
  List.fold_left (fun ast (m : 'a mapper) -> m#program ast) ast mappers
