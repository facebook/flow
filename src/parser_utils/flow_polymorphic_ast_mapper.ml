(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Ast = Flow_ast

let ( * ) : 'a 'b 'c 'd. ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd =
 (fun f g (x, y) -> (f x, g y))

class virtual ['M, 'T, 'N, 'U] mapper =
  object (this)
    method virtual on_loc_annot : 'M -> 'N

    method virtual on_type_annot : 'T -> 'U

    method program (program : ('M, 'T) Ast.Program.t) : ('N, 'U) Ast.Program.t =
      let open Ast.Program in
      let (annot, { statements; interpreter; comments; all_comments }) = program in
      let annot' = this#on_loc_annot annot in
      let statements' = this#toplevel_statement_list statements in
      let interpreter' = Option.map ~f:this#interpreter_directive interpreter in
      let comments' = this#syntax_opt comments in
      let all_comments' = List.map ~f:this#comment all_comments in
      ( annot',
        {
          statements = statements';
          interpreter = interpreter';
          comments = comments';
          all_comments = all_comments';
        }
      )

    method interpreter_directive ((annot, value) : 'M * string) : 'N * string =
      (this#on_loc_annot annot, value)

    method statement ((annot, stmt) : ('M, 'T) Ast.Statement.t) : ('N, 'U) Ast.Statement.t =
      let open Ast.Statement in
      ( this#on_loc_annot annot,
        match stmt with
        | Block block -> Block (this#block block)
        | Break break -> Break (this#break break)
        | ClassDeclaration cls -> ClassDeclaration (this#class_declaration cls)
        | ComponentDeclaration component ->
          ComponentDeclaration (this#component_declaration component)
        | Continue cont -> Continue (this#continue cont)
        | Debugger dbg -> Debugger (this#debugger dbg)
        | DeclareClass stuff -> DeclareClass (this#declare_class stuff)
        | DeclareComponent stuff -> DeclareComponent (this#declare_component stuff)
        | DeclareEnum enum -> DeclareEnum (this#declare_enum enum)
        | DeclareExportDeclaration decl ->
          DeclareExportDeclaration (this#declare_export_declaration annot decl)
        | DeclareFunction stuff -> DeclareFunction (this#declare_function stuff)
        | DeclareInterface stuff -> DeclareInterface (this#declare_interface annot stuff)
        | DeclareModule m -> DeclareModule (this#declare_module annot m)
        | DeclareNamespace n -> DeclareNamespace (this#declare_namespace annot n)
        | DeclareTypeAlias stuff -> DeclareTypeAlias (this#declare_type_alias annot stuff)
        | DeclareVariable stuff -> DeclareVariable (this#declare_variable stuff)
        | DeclareModuleExports exports -> DeclareModuleExports (this#declare_module_exports exports)
        | DoWhile stuff -> DoWhile (this#do_while stuff)
        | Empty comments -> Empty (this#empty comments)
        | EnumDeclaration enum -> EnumDeclaration (this#enum_declaration enum)
        | ExportDefaultDeclaration decl ->
          ExportDefaultDeclaration (this#export_default_declaration annot decl)
        | ExportNamedDeclaration decl ->
          ExportNamedDeclaration (this#export_named_declaration annot decl)
        | Expression expr -> Expression (this#expression_statement expr)
        | For for_stmt -> For (this#for_statement for_stmt)
        | ForIn stuff -> ForIn (this#for_in_statement stuff)
        | ForOf stuff -> ForOf (this#for_of_statement stuff)
        | FunctionDeclaration func -> FunctionDeclaration (this#function_declaration func)
        | If if_stmt -> If (this#if_statement if_stmt)
        | ImportDeclaration decl -> ImportDeclaration (this#import_declaration annot decl)
        | InterfaceDeclaration stuff -> InterfaceDeclaration (this#interface_declaration annot stuff)
        | Labeled label -> Labeled (this#labeled_statement label)
        | Match x -> Match (this#match_statement x)
        | OpaqueType otype -> OpaqueType (this#opaque_type annot otype)
        | Return ret -> Return (this#return ret)
        | Switch switch -> Switch (this#switch switch)
        | Throw throw -> Throw (this#throw throw)
        | Try try_stmt -> Try (this#try_catch try_stmt)
        | VariableDeclaration decl -> VariableDeclaration (this#variable_declaration decl)
        | While stuff -> While (this#while_ stuff)
        | With stuff -> With (this#with_ stuff)
        | TypeAlias stuff -> TypeAlias (this#type_alias annot stuff)
        | DeclareOpaqueType otype -> DeclareOpaqueType (this#declare_opaque_type annot otype)
      )

    method comment ((annot, c) : 'M Ast.Comment.t) : 'N Ast.Comment.t = (this#on_loc_annot annot, c)

    method syntax : 'internal. ('M, 'internal) Ast.Syntax.t -> ('N, 'internal) Ast.Syntax.t =
      fun attached ->
        let open Ast.Syntax in
        let { leading; trailing; internal } = attached in
        let leading' = List.map ~f:this#comment leading in
        let trailing' = List.map ~f:this#comment trailing in
        { leading = leading'; trailing = trailing'; internal }

    method syntax_opt
        : 'internal. ('M, 'internal) Ast.Syntax.t option -> ('N, 'internal) Ast.Syntax.t option =
      Option.map ~f:this#syntax

    method syntax_with_internal (comments : ('M, 'M Ast.Comment.t list) Ast.Syntax.t)
        : ('N, 'N Ast.Comment.t list) Ast.Syntax.t =
      let open Ast.Syntax in
      let { leading; trailing; internal } = comments in
      let leading' = List.map ~f:this#comment leading in
      let trailing' = List.map ~f:this#comment trailing in
      let internal' = List.map ~f:this#comment internal in
      { leading = leading'; trailing = trailing'; internal = internal' }

    method syntax_with_internal_opt
        : ('M, 'M Ast.Comment.t list) Ast.Syntax.t option ->
          ('N, 'N Ast.Comment.t list) Ast.Syntax.t option =
      Option.map ~f:this#syntax_with_internal

    method expression ((annot, expr') : ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
      let open Ast.Expression in
      ( this#on_type_annot annot,
        match expr' with
        | Array x -> Array (this#array x)
        | ArrowFunction x -> ArrowFunction (this#arrow_function x)
        | AsExpression x -> AsExpression (this#as_expression x)
        | AsConstExpression x -> AsConstExpression (this#as_const_expression x)
        | Assignment x -> Assignment (this#assignment x)
        | Binary x -> Binary (this#binary x)
        | Call x -> Call (this#call annot x)
        | Class x -> Class (this#class_expression x)
        | Conditional x -> Conditional (this#conditional x)
        | Function x -> Function (this#function_expression x)
        | Identifier x -> Identifier (this#t_identifier x)
        | Import x -> Import (this#import annot x)
        | JSXElement x -> JSXElement (this#jsx_element annot x)
        | JSXFragment x -> JSXFragment (this#jsx_fragment x)
        | StringLiteral x -> StringLiteral (this#string_literal x)
        | BooleanLiteral x -> BooleanLiteral (this#boolean_literal x)
        | NullLiteral x -> NullLiteral (this#syntax_opt x)
        | NumberLiteral x -> NumberLiteral (this#number_literal x)
        | BigIntLiteral x -> BigIntLiteral (this#bigint_literal x)
        | RegExpLiteral x -> RegExpLiteral (this#regexp_literal x)
        | ModuleRefLiteral x -> ModuleRefLiteral (this#module_ref_literal x)
        | Logical x -> Logical (this#logical x)
        | Match x -> Match (this#match_expression x)
        | Member x -> Member (this#member annot x)
        | MetaProperty x -> MetaProperty (this#meta_property x)
        | New x -> New (this#new_ annot x)
        | Object x -> Object (this#object_ x)
        | OptionalCall x -> OptionalCall (this#optional_call annot x)
        | OptionalMember x -> OptionalMember (this#optional_member annot x)
        | Sequence x -> Sequence (this#sequence x)
        | Super x -> Super (this#super_expression x)
        | TaggedTemplate x -> TaggedTemplate (this#tagged_template x)
        | TemplateLiteral x -> TemplateLiteral (this#template_literal x)
        | This x -> This (this#this_expression x)
        | TypeCast x -> TypeCast (this#type_cast x)
        | TSSatisfies x -> TSSatisfies (this#ts_satisfies x)
        | Unary x -> Unary (this#unary_expression x)
        | Update x -> Update (this#update_expression x)
        | Yield x -> Yield (this#yield x)
      )

    method arg_list ((annot, args) : ('M, 'T) Ast.Expression.ArgList.t)
        : ('N, 'U) Ast.Expression.ArgList.t =
      let open Ast.Expression.ArgList in
      let { arguments; comments } = args in
      let annot' = this#on_loc_annot annot in
      let arguments' = List.map ~f:this#expression_or_spread arguments in
      let comments' = this#syntax_with_internal_opt comments in
      (annot', { arguments = arguments'; comments = comments' })

    method array (expr : ('M, 'T) Ast.Expression.Array.t) : ('N, 'U) Ast.Expression.Array.t =
      let open Ast.Expression in
      let { Array.elements; Array.comments } = expr in
      let elements' = List.map ~f:this#array_element elements in
      let comments' = this#syntax_with_internal_opt comments in
      { Array.elements = elements'; comments = comments' }

    method array_element (element : ('M, 'T) Ast.Expression.Array.element)
        : ('N, 'U) Ast.Expression.Array.element =
      let open Ast.Expression.Array in
      match element with
      | Hole loc -> Hole (this#on_loc_annot loc)
      | Expression expr -> Expression (this#expression expr)
      | Spread spread -> Spread (this#spread_element spread)

    method arrow_function (expr : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
      this#function_ expr

    method as_const_expression (expr : ('M, 'T) Ast.Expression.AsConstExpression.t)
        : ('N, 'U) Ast.Expression.AsConstExpression.t =
      let open Ast.Expression.AsConstExpression in
      let { expression; comments } = expr in
      let expression' = this#expression expression in
      let comments' = this#syntax_opt comments in
      { expression = expression'; comments = comments' }

    method as_expression (expr : ('M, 'T) Ast.Expression.AsExpression.t)
        : ('N, 'U) Ast.Expression.AsExpression.t =
      let open Ast.Expression.AsExpression in
      let { expression; annot; comments } = expr in
      let expression' = this#expression expression in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      { expression = expression'; annot = annot'; comments = comments' }

    method assignment (expr : ('M, 'T) Ast.Expression.Assignment.t)
        : ('N, 'U) Ast.Expression.Assignment.t =
      let open Ast.Expression.Assignment in
      let { operator; left; right; comments } = expr in
      let left' = this#assignment_pattern left in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      { operator; left = left'; right = right'; comments = comments' }

    method binary (expr : ('M, 'T) Ast.Expression.Binary.t) : ('N, 'U) Ast.Expression.Binary.t =
      let open Ast.Expression.Binary in
      let { operator; left; right; comments } = expr in
      let left' = this#expression left in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      { operator; left = left'; right = right'; comments = comments' }

    method block (stmt : ('M, 'T) Ast.Statement.Block.t) : ('N, 'U) Ast.Statement.Block.t =
      let open Ast.Statement.Block in
      let { body; comments } = stmt in
      let body' = this#statement_list body in
      let comments' = this#syntax_with_internal_opt comments in
      { body = body'; comments = comments' }

    method break (break : 'M Ast.Statement.Break.t) : 'N Ast.Statement.Break.t =
      let open Ast.Statement.Break in
      let { label; comments } = break in
      let label' = Option.map ~f:this#label_identifier label in
      let comments' = this#syntax_opt comments in
      { label = label'; comments = comments' }

    method call _annot (expr : ('M, 'T) Ast.Expression.Call.t) : ('N, 'U) Ast.Expression.Call.t =
      let open Ast.Expression.Call in
      let { callee; targs; arguments; comments } = expr in
      let callee' = this#expression callee in
      let targs' = Option.map ~f:this#call_type_args targs in
      let arguments' = this#arg_list arguments in
      let comments' = this#syntax_opt comments in
      { callee = callee'; targs = targs'; arguments = arguments'; comments = comments' }

    method optional_call annot (expr : ('M, 'T) Ast.Expression.OptionalCall.t)
        : ('N, 'U) Ast.Expression.OptionalCall.t =
      let open Ast.Expression.OptionalCall in
      let { call; optional; filtered_out } = expr in
      let call' = this#call annot call in
      let filtered_out' = this#on_type_annot filtered_out in
      { call = call'; optional; filtered_out = filtered_out' }

    method call_type_args (pi : ('M, 'T) Ast.Expression.CallTypeArgs.t)
        : ('N, 'U) Ast.Expression.CallTypeArgs.t =
      let open Ast.Expression.CallTypeArgs in
      let (annot, { arguments; comments }) = pi in
      let annot' = this#on_loc_annot annot in
      let arguments' = List.map ~f:this#call_type_arg arguments in
      let comments' = this#syntax_with_internal_opt comments in
      (annot', { arguments = arguments'; comments = comments' })

    method call_type_arg (x : ('M, 'T) Ast.Expression.CallTypeArg.t)
        : ('N, 'U) Ast.Expression.CallTypeArg.t =
      let open Ast.Expression.CallTypeArg in
      match x with
      | Explicit t -> Explicit (this#type_ t)
      | Implicit t -> Implicit (this#implicit t)

    method catch_body (body : ('M, 'T) Ast.Statement.Block.t) : ('N, 'U) Ast.Statement.Block.t =
      this#block body

    method catch_clause (clause : ('M, 'T) Ast.Statement.Try.CatchClause.t')
        : ('N, 'U) Ast.Statement.Try.CatchClause.t' =
      let open Ast.Statement.Try.CatchClause in
      let { param; body; comments } = clause in
      let param' = Option.map ~f:this#catch_clause_pattern param in
      let body' =
        let (annot, body) = body in
        (this#on_loc_annot annot, this#catch_body body)
      in
      let comments' = this#syntax_opt comments in
      { param = param'; body = body'; comments = comments' }

    method default_opt (default : ('M, 'T) Ast.Expression.t option)
        : ('N, 'U) Ast.Expression.t option =
      Option.map ~f:this#expression default

    method component_declaration (component : ('M, 'T) Ast.Statement.ComponentDeclaration.t)
        : ('N, 'U) Ast.Statement.ComponentDeclaration.t =
      let open Ast.Statement.ComponentDeclaration in
      let { id = ident; params; body; renders; tparams; comments; sig_loc } = component in
      let ident' = this#component_identifier ident in
      this#type_params_opt tparams (fun tparams' ->
          let params' = this#component_params params in
          let renders' = this#component_renders_annotation renders in
          let body' = this#component_body body in
          let comments' = this#syntax_opt comments in
          let sig_loc' = this#on_loc_annot sig_loc in
          {
            id = ident';
            params = params';
            renders = renders';
            body = body';
            tparams = tparams';
            comments = comments';
            sig_loc = sig_loc';
          }
      )

    method component_identifier (ident : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#pattern_identifier ~kind:Ast.Variable.Var ident

    method component_params (params : ('M, 'T) Ast.Statement.ComponentDeclaration.Params.t)
        : ('N, 'U) Ast.Statement.ComponentDeclaration.Params.t =
      let (annot, { Ast.Statement.ComponentDeclaration.Params.params = params_list; rest; comments })
          =
        params
      in
      let params_list' = List.map ~f:this#component_param params_list in
      let rest' = Option.map ~f:this#component_rest_param rest in
      let comments' = this#syntax_with_internal_opt comments in
      ( this#on_loc_annot annot,
        {
          Ast.Statement.ComponentDeclaration.Params.params = params_list';
          rest = rest';
          comments = comments';
        }
      )

    method component_param (param : ('M, 'T) Ast.Statement.ComponentDeclaration.Param.t)
        : ('N, 'U) Ast.Statement.ComponentDeclaration.Param.t =
      let open Ast.Statement.ComponentDeclaration.Param in
      let (annot, { name; local; default; shorthand }) = param in
      let annot' = this#on_loc_annot annot in
      let name' = this#component_param_name name in
      let local' = this#component_param_pattern local in
      let default' = this#default_opt default in
      (annot', { name = name'; local = local'; default = default'; shorthand })

    method component_param_name
        (name : ('M, 'T) Ast.Statement.ComponentDeclaration.Param.param_name)
        : ('N, 'U) Ast.Statement.ComponentDeclaration.Param.param_name =
      let open Ast.Statement.ComponentDeclaration.Param in
      match name with
      | Identifier ident -> Identifier (this#t_identifier ident)
      | StringLiteral (annot, str) ->
        StringLiteral (this#on_loc_annot annot, this#string_literal str)

    method component_param_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#binding_pattern expr

    method component_rest_param (expr : ('M, 'T) Ast.Statement.ComponentDeclaration.RestParam.t)
        : ('N, 'U) Ast.Statement.ComponentDeclaration.RestParam.t =
      let open Ast.Statement.ComponentDeclaration.RestParam in
      let (annot, { argument; comments }) = expr in
      let annot' = this#on_loc_annot annot in
      let argument' = this#component_param_pattern argument in
      let comments' = this#syntax_opt comments in
      (annot', { argument = argument'; comments = comments' })

    method component_body (body : 'M * ('M, 'T) Ast.Statement.Block.t)
        : 'N * ('N, 'U) Ast.Statement.Block.t =
      (this#on_loc_annot * this#block) body

    method class_declaration cls = this#class_ cls

    method class_expression cls = this#class_ cls

    method class_ (cls : ('M, 'T) Ast.Class.t) : ('N, 'U) Ast.Class.t =
      let open Ast.Class in
      let { id; body; tparams; extends; implements; class_decorators; comments } = cls in
      let id' = Option.map ~f:this#class_identifier id in
      let comments' = this#syntax_opt comments in
      this#type_params_opt tparams (fun tparams' ->
          let extends' = Option.map ~f:this#class_extends extends in
          let body' = this#class_body body in
          let implements' = Option.map ~f:this#class_implements implements in
          let class_decorators' = List.map ~f:this#class_decorator class_decorators in
          {
            id = id';
            body = body';
            tparams = tparams';
            extends = extends';
            implements = implements';
            class_decorators = class_decorators';
            comments = comments';
          }
      )

    method class_extends (extends : ('M, 'T) Ast.Class.Extends.t) : ('N, 'U) Ast.Class.Extends.t =
      let open Ast.Class.Extends in
      let (annot, { expr; targs; comments }) = extends in
      let annot' = this#on_loc_annot annot in
      let expr' = this#expression expr in
      let targs' = Option.map ~f:this#type_args targs in
      let comments' = this#syntax_opt comments in
      (annot', { expr = expr'; targs = targs'; comments = comments' })

    method class_decorator (dec : ('M, 'T) Ast.Class.Decorator.t) : ('N, 'U) Ast.Class.Decorator.t =
      let open Ast.Class.Decorator in
      let (annot, { expression; comments }) = dec in
      let annot' = this#on_loc_annot annot in
      let expression' = this#expression expression in
      let comments' = this#syntax_opt comments in
      (annot', { expression = expression'; comments = comments' })

    method class_identifier (ident : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#pattern_identifier ~kind:Ast.Variable.Let ident

    method class_body (cls_body : ('M, 'T) Ast.Class.Body.t) : ('N, 'U) Ast.Class.Body.t =
      let open Ast.Class.Body in
      let (annot, { body; comments }) = cls_body in
      let annot' = this#on_loc_annot annot in
      let body' = List.map ~f:this#class_element body in
      let comments' = this#syntax_opt comments in
      (annot', { body = body'; comments = comments' })

    method class_element (elem : ('M, 'T) Ast.Class.Body.element) : ('N, 'U) Ast.Class.Body.element
        =
      let open Ast.Class.Body in
      match elem with
      | Method (annot, meth) -> Method (this#on_type_annot annot, this#class_method meth)
      | Property (annot, prop) -> Property (this#on_type_annot annot, this#class_property prop)
      | PrivateField (annot, field) ->
        PrivateField (this#on_type_annot annot, this#class_private_field field)

    method class_key key = this#object_key key

    method class_method (meth : ('M, 'T) Ast.Class.Method.t') : ('N, 'U) Ast.Class.Method.t' =
      let open Ast.Class.Method in
      let { kind; key; value; static; decorators; comments } = meth in
      let key' = this#class_method_key key in
      let value' = (this#on_loc_annot * this#function_expression) value in
      let decorators' = List.map ~f:this#class_decorator decorators in
      let comments' = this#syntax_opt comments in
      { kind; key = key'; value = value'; static; decorators = decorators'; comments = comments' }

    method class_method_key key = this#class_key key

    method class_property (prop : ('M, 'T) Ast.Class.Property.t') : ('N, 'U) Ast.Class.Property.t' =
      let open Ast.Class.Property in
      let { key; value; annot; static; variance; decorators; comments } = prop in
      let key' = this#class_property_key key in
      let value' = this#class_property_value value in
      let annot' = this#type_annotation_hint annot in
      let decorators' = List.map ~f:this#class_decorator decorators in
      let variance' = this#variance_opt variance in
      let comments' = this#syntax_opt comments in
      {
        key = key';
        value = value';
        annot = annot';
        static;
        variance = variance';
        decorators = decorators';
        comments = comments';
      }

    method class_property_key key = this#class_key key

    method class_property_value (value : ('M, 'T) Ast.Class.Property.value)
        : ('N, 'U) Ast.Class.Property.value =
      let open Ast.Class.Property in
      match value with
      | Declared -> Declared
      | Uninitialized -> Uninitialized
      | Initialized expr -> Initialized (this#expression expr)

    method class_private_field (prop : ('M, 'T) Ast.Class.PrivateField.t') =
      let open Ast.Class.PrivateField in
      let { key; value; annot; static; variance; decorators; comments } = prop in
      let key' = this#private_name key in
      let value' = this#class_property_value value in
      let annot' = this#type_annotation_hint annot in
      let decorators' = List.map ~f:this#class_decorator decorators in
      let variance' = this#variance_opt variance in
      let comments' = this#syntax_opt comments in
      {
        key = key';
        value = value';
        annot = annot';
        static;
        variance = variance';
        decorators = decorators';
        comments = comments';
      }

    method conditional (expr : ('M, 'T) Ast.Expression.Conditional.t)
        : ('N, 'U) Ast.Expression.Conditional.t =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate; comments } = expr in
      let test' = this#predicate_expression test in
      let consequent' = this#expression consequent in
      let alternate' = this#expression alternate in
      let comments' = this#syntax_opt comments in
      { test = test'; consequent = consequent'; alternate = alternate'; comments = comments' }

    method continue (cont : 'M Ast.Statement.Continue.t) : 'N Ast.Statement.Continue.t =
      let open Ast.Statement.Continue in
      let { label; comments } = cont in
      let label' = Option.map ~f:this#label_identifier label in
      let comments' = this#syntax_opt comments in
      { label = label'; comments = comments' }

    method debugger (dbg : 'M Ast.Statement.Debugger.t) : 'N Ast.Statement.Debugger.t =
      let open Ast.Statement.Debugger in
      let { comments } = dbg in
      let comments' = this#syntax_opt comments in
      { comments = comments' }

    method declare_class (decl : ('M, 'T) Ast.Statement.DeclareClass.t)
        : ('N, 'U) Ast.Statement.DeclareClass.t =
      let open Ast.Statement.DeclareClass in
      let { id = ident; tparams; body; extends; mixins; implements; comments } = decl in
      let id' = this#class_identifier ident in
      this#type_params_opt tparams (fun tparams' ->
          let body' =
            let (a, b) = body in
            (this#on_loc_annot a, this#object_type b)
          in
          let extends' = Option.map ~f:(this#on_loc_annot * this#generic_type) extends in
          let mixins' = List.map ~f:(this#on_loc_annot * this#generic_type) mixins in
          let implements' = Option.map ~f:this#class_implements implements in
          let comments' = this#syntax_opt comments in
          {
            id = id';
            tparams = tparams';
            body = body';
            extends = extends';
            mixins = mixins';
            implements = implements';
            comments = comments';
          }
      )

    method class_implements (implements : ('M, 'T) Ast.Class.Implements.t)
        : ('N, 'U) Ast.Class.Implements.t =
      let open Ast.Class.Implements in
      let (annot, { interfaces; comments }) = implements in
      let annot' = this#on_loc_annot annot in
      let interfaces' = List.map ~f:this#class_implements_interface interfaces in
      let comments' = this#syntax_opt comments in
      (annot', { interfaces = interfaces'; comments = comments' })

    method class_implements_interface (interface : ('M, 'T) Ast.Class.Implements.Interface.t)
        : ('N, 'U) Ast.Class.Implements.Interface.t =
      let open Ast.Class.Implements.Interface in
      let (annot, { id = id_; targs }) = interface in
      let annot' = this#on_loc_annot annot in
      let id' = this#type_identifier_reference id_ in
      let targs' = Option.map ~f:this#type_args targs in
      (annot', { id = id'; targs = targs' })

    method declare_component (decl : ('M, 'T) Ast.Statement.DeclareComponent.t)
        : ('N, 'U) Ast.Statement.DeclareComponent.t =
      let open Ast.Statement.DeclareComponent in
      let { id = ident; params; renders; tparams; comments } = decl in
      let ident' = this#t_identifier ident in
      this#type_params_opt tparams (fun tparams' ->
          let params' = this#component_type_params params in
          let renders' = this#component_renders_annotation renders in
          let comments' = this#syntax_opt comments in
          {
            id = ident';
            params = params';
            renders = renders';
            tparams = tparams';
            comments = comments';
          }
      )

    method component_type (t : ('M, 'T) Ast.Type.Component.t) : ('N, 'U) Ast.Type.Component.t =
      let open Ast.Type.Component in
      let { params; renders; tparams; comments } = t in
      this#type_params_opt tparams (fun tparams' ->
          let params' = this#component_type_params params in
          let renders' = this#component_renders_annotation renders in
          let comments' = this#syntax_opt comments in
          { params = params'; renders = renders'; tparams = tparams'; comments = comments' }
      )

    method component_type_params (params : ('M, 'T) Ast.Type.Component.Params.t)
        : ('N, 'U) Ast.Type.Component.Params.t =
      let open Ast.Type.Component.Params in
      let (annot, { params = params_list; rest; comments }) = params in
      let params_list' = List.map ~f:this#component_type_param params_list in
      let rest' = Option.map ~f:this#component_type_rest_param rest in
      let comments' = this#syntax_with_internal_opt comments in
      (this#on_loc_annot annot, { params = params_list'; rest = rest'; comments = comments' })

    method component_type_param (param : ('M, 'T) Ast.Type.Component.Param.t)
        : ('N, 'U) Ast.Type.Component.Param.t =
      let open Ast.Type.Component.Param in
      let (loc_annot, { name; annot; optional }) = param in
      let name' = this#component_param_name name in
      let annot' = this#type_annotation annot in
      (this#on_loc_annot loc_annot, { name = name'; annot = annot'; optional })

    method component_type_rest_param (expr : ('M, 'T) Ast.Type.Component.RestParam.t)
        : ('N, 'U) Ast.Type.Component.RestParam.t =
      let open Ast.Type.Component.RestParam in
      let (loc_annot, { argument; annot; optional; comments }) = expr in
      let argument' = Option.map ~f:this#t_identifier argument in
      let annot' = this#type_ annot in
      let comments' = this#syntax_opt comments in
      ( this#on_loc_annot loc_annot,
        { argument = argument'; annot = annot'; comments = comments'; optional }
      )

    method declare_enum (enum : ('M, 'T) Ast.Statement.EnumDeclaration.t)
        : ('N, 'U) Ast.Statement.EnumDeclaration.t =
      this#enum_declaration enum

    method declare_export_declaration
        _annot (decl : ('M, 'T) Ast.Statement.DeclareExportDeclaration.t)
        : ('N, 'U) Ast.Statement.DeclareExportDeclaration.t =
      let open Ast.Statement.DeclareExportDeclaration in
      let { default; source; specifiers; declaration; comments } = decl in
      let default' = Option.map ~f:this#on_loc_annot default in
      let source' =
        match source with
        | None -> None
        | Some (loc, lit) -> Some (this#on_type_annot loc, this#export_source loc lit)
      in
      let specifiers' = Option.map ~f:this#export_named_specifier specifiers in
      let declaration' = Option.map ~f:this#declare_export_declaration_decl declaration in
      let comments' = this#syntax_opt comments in
      {
        default = default';
        source = source';
        specifiers = specifiers';
        declaration = declaration';
        comments = comments';
      }

    method declare_export_declaration_decl
        (decl : ('M, 'T) Ast.Statement.DeclareExportDeclaration.declaration)
        : ('N, 'U) Ast.Statement.DeclareExportDeclaration.declaration =
      let open Ast.Statement.DeclareExportDeclaration in
      match decl with
      | Variable (annot, decl_var) ->
        Variable (this#on_loc_annot annot, this#declare_variable decl_var)
      | Function (annot, decl_func) ->
        Function (this#on_loc_annot annot, this#declare_function decl_func)
      | Class (annot, decl_class) -> Class (this#on_loc_annot annot, this#declare_class decl_class)
      | Component (annot, decl_component) ->
        Component (this#on_loc_annot annot, this#declare_component decl_component)
      | DefaultType t -> DefaultType (this#type_ t)
      | NamedType (annot, alias) -> NamedType (this#on_loc_annot annot, this#type_alias annot alias)
      | NamedOpaqueType (annot, ot) ->
        NamedOpaqueType (this#on_loc_annot annot, this#opaque_type annot ot)
      | Interface (annot, iface) -> Interface (this#on_loc_annot annot, this#interface annot iface)
      | Enum (annot, enum) -> Enum (this#on_loc_annot annot, this#enum_declaration enum)

    method declare_function (decl : ('M, 'T) Ast.Statement.DeclareFunction.t)
        : ('N, 'U) Ast.Statement.DeclareFunction.t =
      let open Ast.Statement.DeclareFunction in
      let { id = ident; annot; predicate; comments } = decl in
      let id' = this#function_identifier ident in
      let annot' = this#type_annotation annot in
      let predicate' = Option.map ~f:this#predicate predicate in
      let comments' = this#syntax_opt comments in
      { id = id'; annot = annot'; predicate = predicate'; comments = comments' }

    method declare_interface annot (decl : ('M, 'T) Ast.Statement.Interface.t)
        : ('N, 'U) Ast.Statement.Interface.t =
      this#interface annot decl

    method declare_module _annot (m : ('M, 'T) Ast.Statement.DeclareModule.t)
        : ('N, 'U) Ast.Statement.DeclareModule.t =
      let open Ast.Statement.DeclareModule in
      let { id; body; comments } = m in
      let id' =
        match id with
        | Identifier id -> Identifier (this#t_identifier id)
        | Literal (annot, name) -> Literal (this#on_type_annot annot, this#string_literal name)
      in
      let body' = (this#on_loc_annot * this#block) body in
      let comments' = this#syntax_opt comments in
      { id = id'; body = body'; comments = comments' }

    method declare_namespace _annot (n : ('M, 'T) Ast.Statement.DeclareNamespace.t)
        : ('N, 'U) Ast.Statement.DeclareNamespace.t =
      let open Ast.Statement.DeclareNamespace in
      let { id; body; comments } = n in
      let id' = this#t_identifier id in
      let body' = (this#on_loc_annot * this#block) body in
      let comments' = this#syntax_opt comments in
      { id = id'; body = body'; comments = comments' }

    method declare_module_exports (exports : ('M, 'T) Ast.Statement.DeclareModuleExports.t)
        : ('N, 'U) Ast.Statement.DeclareModuleExports.t =
      let open Ast.Statement.DeclareModuleExports in
      let { annot; comments } = exports in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      { annot = annot'; comments = comments' }

    method declare_type_alias annot (decl : ('M, 'T) Ast.Statement.TypeAlias.t)
        : ('N, 'U) Ast.Statement.TypeAlias.t =
      this#type_alias annot decl

    method declare_variable (decl : ('M, 'T) Ast.Statement.DeclareVariable.t)
        : ('N, 'U) Ast.Statement.DeclareVariable.t =
      let open Ast.Statement.DeclareVariable in
      let { id = ident; annot; kind; comments } = decl in
      let id' = this#pattern_identifier ~kind ident in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      { id = id'; annot = annot'; kind; comments = comments' }

    method do_while (stuff : ('M, 'T) Ast.Statement.DoWhile.t) : ('N, 'U) Ast.Statement.DoWhile.t =
      let open Ast.Statement.DoWhile in
      let { body; test; comments } = stuff in
      let body' = this#statement body in
      let test' = this#predicate_expression test in
      let comments' = this#syntax_opt comments in
      { body = body'; test = test'; comments = comments' }

    method empty (empty : 'M Ast.Statement.Empty.t) : 'N Ast.Statement.Empty.t =
      let open Ast.Statement.Empty in
      let { comments } = empty in
      let comments' = this#syntax_opt comments in
      { comments = comments' }

    method enum_declaration (enum : ('M, 'T) Ast.Statement.EnumDeclaration.t)
        : ('N, 'U) Ast.Statement.EnumDeclaration.t =
      let open Ast.Statement.EnumDeclaration in
      let { id; body; comments } = enum in
      let body' = this#enum_body body in
      let comments' = this#syntax_opt comments in
      { id = this#t_identifier id; body = body'; comments = comments' }

    method enum_body (body : 'M Ast.Statement.EnumDeclaration.body)
        : 'N Ast.Statement.EnumDeclaration.body =
      let open Ast.Statement.EnumDeclaration in
      match body with
      | (annot, BooleanBody boolean_body) ->
        (this#on_loc_annot annot, BooleanBody (this#enum_boolean_body boolean_body))
      | (annot, NumberBody number_body) ->
        (this#on_loc_annot annot, NumberBody (this#enum_number_body number_body))
      | (annot, StringBody string_body) ->
        (this#on_loc_annot annot, StringBody (this#enum_string_body string_body))
      | (annot, SymbolBody symbol_body) ->
        (this#on_loc_annot annot, SymbolBody (this#enum_symbol_body symbol_body))
      | (annot, BigIntBody bigint_body) ->
        (this#on_loc_annot annot, BigIntBody (this#enum_bigint_body bigint_body))

    method enum_boolean_body (body : 'M Ast.Statement.EnumDeclaration.BooleanBody.t)
        : 'N Ast.Statement.EnumDeclaration.BooleanBody.t =
      let open Ast.Statement.EnumDeclaration.BooleanBody in
      let { members; explicit_type; has_unknown_members; comments } = body in
      let members' = List.map ~f:this#enum_boolean_member members in
      let comments' = this#syntax_with_internal_opt comments in
      { members = members'; explicit_type; has_unknown_members; comments = comments' }

    method enum_number_body (body : 'M Ast.Statement.EnumDeclaration.NumberBody.t)
        : 'N Ast.Statement.EnumDeclaration.NumberBody.t =
      let open Ast.Statement.EnumDeclaration.NumberBody in
      let { members; explicit_type; has_unknown_members; comments } = body in
      let members' = List.map ~f:this#enum_number_member members in
      let comments' = this#syntax_with_internal_opt comments in
      { members = members'; explicit_type; has_unknown_members; comments = comments' }

    method enum_string_body (body : 'M Ast.Statement.EnumDeclaration.StringBody.t)
        : 'N Ast.Statement.EnumDeclaration.StringBody.t =
      let open Ast.Statement.EnumDeclaration.StringBody in
      let { members; explicit_type; has_unknown_members; comments } = body in
      let members' =
        match members with
        | Defaulted members -> Defaulted (List.map ~f:this#enum_defaulted_member members)
        | Initialized members -> Initialized (List.map ~f:this#enum_string_member members)
      in
      let comments' = this#syntax_with_internal_opt comments in
      { members = members'; explicit_type; has_unknown_members; comments = comments' }

    method enum_symbol_body (body : 'M Ast.Statement.EnumDeclaration.SymbolBody.t)
        : 'N Ast.Statement.EnumDeclaration.SymbolBody.t =
      let open Ast.Statement.EnumDeclaration.SymbolBody in
      let { members; has_unknown_members; comments } = body in
      let members' = List.map ~f:this#enum_defaulted_member members in
      let comments' = this#syntax_with_internal_opt comments in
      { members = members'; has_unknown_members; comments = comments' }

    method enum_bigint_body (body : 'M Ast.Statement.EnumDeclaration.BigIntBody.t)
        : 'N Ast.Statement.EnumDeclaration.BigIntBody.t =
      let open Ast.Statement.EnumDeclaration.BigIntBody in
      let { members; explicit_type; has_unknown_members; comments } = body in
      let members' = List.map ~f:this#enum_bigint_member members in
      let comments' = this#syntax_with_internal_opt comments in
      { members = members'; explicit_type; has_unknown_members; comments = comments' }

    method enum_defaulted_member (member : 'M Ast.Statement.EnumDeclaration.DefaultedMember.t)
        : 'N Ast.Statement.EnumDeclaration.DefaultedMember.t =
      let open Ast.Statement.EnumDeclaration.DefaultedMember in
      let (annot, { id }) = member in
      (this#on_loc_annot annot, { id = this#enum_member_identifier id })

    method enum_boolean_member
        (member : ('M Ast.BooleanLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t)
        : ('N Ast.BooleanLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (annot, { id; init = (init_annot, init_val) }) = member in
      let init' = (this#on_loc_annot init_annot, this#boolean_literal init_val) in
      (this#on_loc_annot annot, { id = this#enum_member_identifier id; init = init' })

    method enum_number_member
        (member : ('M Ast.NumberLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t)
        : ('N Ast.NumberLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (annot, { id; init = (init_annot, init_val) }) = member in
      let init' = (this#on_loc_annot init_annot, this#number_literal init_val) in
      (this#on_loc_annot annot, { id = this#enum_member_identifier id; init = init' })

    method enum_string_member
        (member : ('M Ast.StringLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t)
        : ('N Ast.StringLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (annot, { id; init = (init_annot, init_val) }) = member in
      let init' = (this#on_loc_annot init_annot, this#string_literal init_val) in
      (this#on_loc_annot annot, { id = this#enum_member_identifier id; init = init' })

    method enum_bigint_member
        (member : ('M Ast.BigIntLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t)
        : ('N Ast.BigIntLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (annot, { id; init = (init_annot, init_val) }) = member in
      let init' = (this#on_loc_annot init_annot, this#bigint_literal init_val) in
      (this#on_loc_annot annot, { id = this#enum_member_identifier id; init = init' })

    method enum_member_identifier (ident : ('M, 'M) Ast.Identifier.t) : ('N, 'N) Ast.Identifier.t =
      this#identifier ident

    method export_default_declaration
        _loc (decl : ('M, 'T) Ast.Statement.ExportDefaultDeclaration.t)
        : ('N, 'U) Ast.Statement.ExportDefaultDeclaration.t =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default; declaration; comments } = decl in
      let default' = this#on_type_annot default in
      let declaration' = this#export_default_declaration_decl declaration in
      let comments' = this#syntax_opt comments in
      { default = default'; declaration = declaration'; comments = comments' }

    method export_default_declaration_decl
        (decl : ('M, 'T) Ast.Statement.ExportDefaultDeclaration.declaration)
        : ('N, 'U) Ast.Statement.ExportDefaultDeclaration.declaration =
      let open Ast.Statement.ExportDefaultDeclaration in
      match decl with
      | Declaration stmt -> Declaration (this#statement stmt)
      | Expression expr -> Expression (this#expression expr)

    method export_named_declaration _loc (decl : ('M, 'T) Ast.Statement.ExportNamedDeclaration.t)
        : ('N, 'U) Ast.Statement.ExportNamedDeclaration.t =
      let open Ast.Statement.ExportNamedDeclaration in
      let { export_kind; source; specifiers; declaration; comments } = decl in
      let source' =
        match source with
        | None -> None
        | Some (loc, lit) -> Some (this#on_type_annot loc, this#export_source loc lit)
      in
      let specifiers' = Option.map ~f:this#export_named_specifier specifiers in
      let declaration' = Option.map ~f:this#statement declaration in
      let comments' = this#syntax_opt comments in
      {
        export_kind;
        source = source';
        specifiers = specifiers';
        declaration = declaration';
        comments = comments';
      }

    method export_named_specifier (spec : ('M, 'T) Ast.Statement.ExportNamedDeclaration.specifier)
        : ('N, 'U) Ast.Statement.ExportNamedDeclaration.specifier =
      let open Ast.Statement.ExportNamedDeclaration in
      match spec with
      | ExportSpecifiers specs ->
        ExportSpecifiers (List.map ~f:this#export_named_declaration_specifier specs)
      | ExportBatchSpecifier batch -> ExportBatchSpecifier (this#export_batch_specifier batch)

    method export_source _loc lit = this#string_literal lit

    method export_named_declaration_specifier
        (spec : ('M, 'T) Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t)
        : ('N, 'U) Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t =
      let open Ast.Statement.ExportNamedDeclaration.ExportSpecifier in
      let (annot, { local; exported }) = spec in
      let local' = this#t_identifier local in
      let exported' = Option.map ~f:this#t_identifier exported in
      (this#on_loc_annot annot, { local = local'; exported = exported' })

    method export_batch_specifier
        (spec : ('M, 'T) Ast.Statement.ExportNamedDeclaration.ExportBatchSpecifier.t)
        : ('N, 'U) Ast.Statement.ExportNamedDeclaration.ExportBatchSpecifier.t =
      let (annot, name) = spec in
      let annot' = this#on_loc_annot annot in
      let name' = Option.map ~f:this#t_identifier name in
      (annot', name')

    method expression_statement (stmt : ('M, 'T) Ast.Statement.Expression.t)
        : ('N, 'U) Ast.Statement.Expression.t =
      let open Ast.Statement.Expression in
      let { expression = expr; directive; comments } = stmt in
      let expression' = this#expression expr in
      let comments' = this#syntax_opt comments in
      { expression = expression'; directive; comments = comments' }

    method expression_or_spread (expr_or_spread : ('M, 'T) Ast.Expression.expression_or_spread)
        : ('N, 'U) Ast.Expression.expression_or_spread =
      let open Ast.Expression in
      match expr_or_spread with
      | Expression expr -> Expression (this#expression expr)
      | Spread spread -> Spread (this#spread_element spread)

    method for_in_statement (stmt : ('M, 'T) Ast.Statement.ForIn.t) : ('N, 'U) Ast.Statement.ForIn.t
        =
      let open Ast.Statement.ForIn in
      let { left; right; body; each; comments } = stmt in
      let left' = this#for_in_statement_lhs left in
      let right' = this#expression right in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      { left = left'; right = right'; body = body'; each; comments = comments' }

    method for_in_statement_lhs (left : ('M, 'T) Ast.Statement.ForIn.left)
        : ('N, 'U) Ast.Statement.ForIn.left =
      let open Ast.Statement.ForIn in
      match left with
      | LeftDeclaration decl -> LeftDeclaration (this#for_in_left_declaration decl)
      | LeftPattern patt -> LeftPattern (this#for_in_assignment_pattern patt)

    method for_in_left_declaration
        ((annot, decl) : 'M * ('M, 'T) Ast.Statement.VariableDeclaration.t)
        : 'N * ('N, 'U) Ast.Statement.VariableDeclaration.t =
      (this#on_loc_annot annot, this#variable_declaration decl)

    method for_of_statement (stuff : ('M, 'T) Ast.Statement.ForOf.t)
        : ('N, 'U) Ast.Statement.ForOf.t =
      let open Ast.Statement.ForOf in
      let { left; right; body; await; comments } = stuff in
      let left' = this#for_of_statement_lhs left in
      let right' = this#expression right in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      { left = left'; right = right'; body = body'; await; comments = comments' }

    method for_of_statement_lhs (left : ('M, 'T) Ast.Statement.ForOf.left) =
      let open Ast.Statement.ForOf in
      match left with
      | LeftDeclaration decl -> LeftDeclaration (this#for_of_left_declaration decl)
      | LeftPattern patt -> LeftPattern (this#for_of_assignment_pattern patt)

    method for_of_left_declaration
        ((annot, decl) : 'M * ('M, 'T) Ast.Statement.VariableDeclaration.t)
        : 'N * ('N, 'U) Ast.Statement.VariableDeclaration.t =
      (this#on_loc_annot annot, this#variable_declaration decl)

    method for_statement (stmt : ('M, 'T) Ast.Statement.For.t) : ('N, 'U) Ast.Statement.For.t =
      let open Ast.Statement.For in
      let { init; test; update; body; comments } = stmt in
      let init' = Option.map ~f:this#for_statement_init init in
      let test' = Option.map ~f:this#predicate_expression test in
      let update' = Option.map ~f:this#expression update in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      { init = init'; test = test'; update = update'; body = body'; comments = comments' }

    method for_statement_init (init : ('M, 'T) Ast.Statement.For.init)
        : ('N, 'U) Ast.Statement.For.init =
      let open Ast.Statement.For in
      match init with
      | InitDeclaration decl -> InitDeclaration (this#for_init_declaration decl)
      | InitExpression expr -> InitExpression (this#expression expr)

    method for_init_declaration ((annot, decl) : 'M * ('M, 'T) Ast.Statement.VariableDeclaration.t)
        : 'N * ('N, 'U) Ast.Statement.VariableDeclaration.t =
      (this#on_loc_annot annot, this#variable_declaration decl)

    method function_param_type (fpt : ('M, 'T) Ast.Type.Function.Param.t)
        : ('N, 'U) Ast.Type.Function.Param.t =
      let open Ast.Type.Function.Param in
      let (annot, { annot = t_annot; name; optional }) = fpt in
      let t_annot' = this#type_ t_annot in
      let name' = Option.map ~f:this#t_identifier name in
      (this#on_loc_annot annot, { annot = t_annot'; name = name'; optional })

    method function_rest_param_type (frpt : ('M, 'T) Ast.Type.Function.RestParam.t)
        : ('N, 'U) Ast.Type.Function.RestParam.t =
      let open Ast.Type.Function.RestParam in
      let (annot, { argument; comments }) = frpt in
      let annot' = this#on_loc_annot annot in
      let argument' = this#function_param_type argument in
      let comments' = this#syntax_opt comments in
      (annot', { argument = argument'; comments = comments' })

    method function_this_param_type (frpt : ('M, 'T) Ast.Type.Function.ThisParam.t)
        : ('N, 'U) Ast.Type.Function.ThisParam.t =
      let open Ast.Type.Function.ThisParam in
      let (loc, { annot; comments }) = frpt in
      let loc' = this#on_loc_annot loc in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      (loc', { annot = annot'; comments = comments' })

    method function_type_return_annotation
        (ret_annot : ('M, 'T) Ast.Type.Function.return_annotation)
        : ('N, 'U) Ast.Type.Function.return_annotation =
      let open Ast.Type.Function in
      match ret_annot with
      | TypeAnnotation t -> TypeAnnotation (this#type_ t)
      | TypeGuard g -> TypeGuard (this#type_guard g)

    method function_type (ft : ('M, 'T) Ast.Type.Function.t) : ('N, 'U) Ast.Type.Function.t =
      let open Ast.Type.Function in
      let {
        params =
          (params_annot, { Params.this_; params = ps; rest = rpo; comments = params_comments });
        return;
        tparams;
        comments = func_comments;
        effect;
      } =
        ft
      in
      this#type_params_opt tparams (fun tparams' ->
          let this_' = Option.map ~f:this#function_this_param_type this_ in
          let ps' = List.map ~f:this#function_param_type ps in
          let rpo' = Option.map ~f:this#function_rest_param_type rpo in
          let return' = this#function_type_return_annotation return in
          let func_comments' = this#syntax_opt func_comments in
          let params_comments' = this#syntax_with_internal_opt params_comments in
          {
            params =
              ( this#on_loc_annot params_annot,
                { Params.this_ = this_'; params = ps'; rest = rpo'; comments = params_comments' }
              );
            return = return';
            tparams = tparams';
            effect;
            comments = func_comments';
          }
      )

    method label_identifier (ident : ('M, 'M) Ast.Identifier.t) : ('N, 'N) Ast.Identifier.t =
      this#identifier ident

    method object_property_value_type (opvt : ('M, 'T) Ast.Type.Object.Property.value)
        : ('N, 'U) Ast.Type.Object.Property.value =
      let open Ast.Type.Object.Property in
      match opvt with
      | Init t -> Init (this#type_ t)
      | Get t -> Get (this#object_type_property_getter t)
      | Set t -> Set (this#object_type_property_setter t)

    method object_type_property_getter getter =
      let (annot, ft) = getter in
      let annot' = this#on_loc_annot annot in
      let ft' = this#function_type ft in
      (annot', ft')

    method object_type_property_setter setter =
      let (annot, ft) = setter in
      let annot' = this#on_loc_annot annot in
      let ft' = this#function_type ft in
      (annot', ft')

    method object_property_type (opt : ('M, 'T) Ast.Type.Object.Property.t)
        : ('N, 'U) Ast.Type.Object.Property.t =
      let open Ast.Type.Object.Property in
      let (annot, { key; value; optional; static; proto; _method; variance; comments }) = opt in
      let key' = this#object_key key in
      let value' = this#object_property_value_type value in
      let variance' = this#variance_opt variance in
      let comments' = this#syntax_opt comments in
      ( this#on_loc_annot annot,
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

    method object_indexer_property_type (oit : ('M, 'T) Ast.Type.Object.Indexer.t)
        : ('N, 'U) Ast.Type.Object.Indexer.t =
      let open Ast.Type.Object.Indexer in
      let (annot, { id = id_; key; value; static; variance; comments }) = oit in
      let id' = Option.map ~f:this#identifier id_ in
      let key' = this#type_ key in
      let value' = this#type_ value in
      let variance' = this#variance_opt variance in
      let comments' = this#syntax_opt comments in
      ( this#on_loc_annot annot,
        { id = id'; key = key'; value = value'; static; variance = variance'; comments = comments' }
      )

    method object_mapped_type (omt : ('M, 'T) Ast.Type.Object.MappedType.t)
        : ('N, 'U) Ast.Type.Object.MappedType.t =
      let open Ast.Type.Object.MappedType in
      let (annot, { key_tparam; prop_type; source_type; variance; optional; comments }) = omt in
      (* Source type does not have the tparams in scope, so we visit it first *)
      let source_type' = this#type_ source_type in
      (* We visit this with type_params_opt intentionally because this method is relied upon in
       * subclasses to be called at every location that binds tparams. This is unfortunate and
       * forces this method to write some pretty hacky code. *)
      let fake_tparams_opt =
        Some (fst key_tparam, { Ast.Type.TypeParams.params = [key_tparam]; comments = None })
      in
      this#type_params_opt fake_tparams_opt (fun fake_tparams_opt' ->
          let fake_tparams = Base.Option.value_exn fake_tparams_opt' in
          let key_tparam' =
            match (snd fake_tparams).Ast.Type.TypeParams.params with
            | [key] -> key
            | _ -> failwith "Illegal mapped type"
          in
          let prop_type' = this#type_ prop_type in
          let variance' = Option.map ~f:this#variance variance in
          let comments' = Option.map ~f:this#syntax comments in
          ( this#on_loc_annot annot,
            {
              key_tparam = key_tparam';
              source_type = source_type';
              prop_type = prop_type';
              variance = variance';
              optional;
              comments = comments';
            }
          )
      )

    method object_internal_slot_property_type (islot : ('M, 'T) Ast.Type.Object.InternalSlot.t)
        : ('N, 'U) Ast.Type.Object.InternalSlot.t =
      let open Ast.Type.Object.InternalSlot in
      let (annot, { id = id_; value; optional; static; _method; comments }) = islot in
      let id' = this#identifier id_ in
      let value' = this#type_ value in
      let comments' = this#syntax_opt comments in
      ( this#on_loc_annot annot,
        { id = id'; value = value'; optional; static; _method; comments = comments' }
      )

    method object_type (ot : ('M, 'T) Ast.Type.Object.t) : ('N, 'U) Ast.Type.Object.t =
      let open Ast.Type.Object in
      let { properties; exact; inexact; comments } = ot in
      let properties' = List.map ~f:this#object_type_property properties in
      let comments' = this#syntax_with_internal_opt comments in
      { properties = properties'; exact; inexact; comments = comments' }

    method object_type_property (prop : ('M, 'T) Ast.Type.Object.property)
        : ('N, 'U) Ast.Type.Object.property =
      let open Ast.Type.Object in
      match prop with
      | Property prop -> Property (this#object_property_type prop)
      | SpreadProperty prop -> SpreadProperty (this#object_spread_property_type prop)
      | Indexer indexer -> Indexer (this#object_indexer_property_type indexer)
      | CallProperty prop -> CallProperty (this#object_call_property_type prop)
      | MappedType mapped_type -> MappedType (this#object_mapped_type mapped_type)
      | InternalSlot islot -> InternalSlot (this#object_internal_slot_property_type islot)

    method object_spread_property_type (opt : ('M, 'T) Ast.Type.Object.SpreadProperty.t)
        : ('N, 'U) Ast.Type.Object.SpreadProperty.t =
      let open Ast.Type.Object.SpreadProperty in
      let (annot, { argument; comments }) = opt in
      let annot' = this#on_loc_annot annot in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      (annot', { argument = argument'; comments = comments' })

    method object_call_property_type (call : ('M, 'T) Ast.Type.Object.CallProperty.t)
        : ('N, 'U) Ast.Type.Object.CallProperty.t =
      let open Ast.Type.Object.CallProperty in
      let (annot, { value; static; comments }) = call in
      let annot' = this#on_loc_annot annot in
      let value' = (this#on_loc_annot * this#function_type) value in
      let comments' = this#syntax_opt comments in
      (annot', { value = value'; static; comments = comments' })

    method interface_type (i : ('M, 'T) Ast.Type.Interface.t) : ('N, 'U) Ast.Type.Interface.t =
      let open Ast.Type.Interface in
      let { extends; body; comments } = i in
      let extends' = List.map ~f:(this#on_loc_annot * this#generic_type) extends in
      let body' = (this#on_loc_annot * this#object_type) body in
      let comments' = this#syntax_opt comments in
      { extends = extends'; body = body'; comments = comments' }

    method generic_identifier_type (git : ('M, 'T) Ast.Type.Generic.Identifier.t)
        : ('N, 'U) Ast.Type.Generic.Identifier.t =
      let open Ast.Type.Generic.Identifier in
      match git with
      | Unqualified i -> Unqualified (this#type_identifier_reference i)
      | Qualified qual -> Qualified (this#generic_qualified_identifier_type qual)

    method generic_qualified_identifier_type (qual : ('M, 'T) Ast.Type.Generic.Identifier.qualified)
        : ('N, 'U) Ast.Type.Generic.Identifier.qualified =
      let open Ast.Type.Generic.Identifier in
      let (annot, { qualification; id = id_ }) = qual in
      let qualification' = this#generic_identifier_type qualification in
      let id' = this#member_type_identifier id_ in
      (this#on_loc_annot annot, { qualification = qualification'; id = id' })

    method member_type_identifier (id : ('M, 'T) Flow_ast.Identifier.t)
        : ('N, 'U) Flow_ast.Identifier.t =
      this#t_identifier id

    method type_args (targs : ('M, 'T) Ast.Type.TypeArgs.t) : ('N, 'U) Ast.Type.TypeArgs.t =
      let open Ast.Type.TypeArgs in
      let (annot, { arguments; comments }) = targs in
      let annot' = this#on_loc_annot annot in
      let arguments' = List.map ~f:this#type_ arguments in
      let comments' = this#syntax_with_internal_opt comments in
      (annot', { arguments = arguments'; comments = comments' })

    method type_params_opt
        : 'a.
          ('M, 'T) Ast.Type.TypeParams.t option ->
          (('N, 'U) Ast.Type.TypeParams.t option -> 'a) ->
          'a =
      fun tparams f ->
        let tparams' = Option.map ~f:this#type_params tparams in
        f tparams'

    method type_params (tparams : ('M, 'T) Ast.Type.TypeParams.t) : ('N, 'U) Ast.Type.TypeParams.t =
      let open Ast.Type.TypeParams in
      let (annot, { params = tps; comments }) = tparams in
      let annot' = this#on_loc_annot annot in
      let tps' = List.map ~f:this#type_param tps in
      let comments' = this#syntax_with_internal_opt comments in
      (annot', { params = tps'; comments = comments' })

    method type_param (tparam : ('M, 'T) Ast.Type.TypeParam.t) : ('N, 'U) Ast.Type.TypeParam.t =
      let open Ast.Type.TypeParam in
      let (annot, { name; bound; bound_kind; variance; default; const }) = tparam in
      let name' = this#type_param_identifier name in
      let bound' = this#type_annotation_hint bound in
      let variance' = this#variance_opt variance in
      let default' = Option.map ~f:this#type_ default in
      let const' = Option.map ~f:this#tparam_const_modifier const in
      ( this#on_loc_annot annot,
        {
          name = name';
          bound = bound';
          bound_kind;
          variance = variance';
          default = default';
          const = const';
        }
      )

    method type_param_identifier (id : ('M, 'M) Ast.Identifier.t) : ('N, 'N) Ast.Identifier.t =
      this#identifier id

    method generic_type (gt : ('M, 'T) Ast.Type.Generic.t) : ('N, 'U) Ast.Type.Generic.t =
      let open Ast.Type.Generic in
      let { id; targs; comments } = gt in
      let id' = this#generic_identifier_type id in
      let targs' = Option.map ~f:this#type_args targs in
      let comments' = this#syntax_opt comments in
      { id = id'; targs = targs'; comments = comments' }

    method indexed_access_type (ia : ('M, 'T) Ast.Type.IndexedAccess.t)
        : ('N, 'U) Ast.Type.IndexedAccess.t =
      let open Ast.Type.IndexedAccess in
      let { _object; index; comments } = ia in
      let _object' = this#type_ _object in
      let index' = this#type_ index in
      let comments' = this#syntax_opt comments in
      { _object = _object'; index = index'; comments = comments' }

    method optional_indexed_access_type (ia : ('M, 'T) Ast.Type.OptionalIndexedAccess.t)
        : ('N, 'U) Ast.Type.OptionalIndexedAccess.t =
      let open Ast.Type.OptionalIndexedAccess in
      let { indexed_access; optional } = ia in
      let indexed_access' = this#indexed_access_type indexed_access in
      { indexed_access = indexed_access'; optional }

    method predicate ((annot, pred) : ('M, 'T) Ast.Type.Predicate.t) : ('N, 'U) Ast.Type.Predicate.t
        =
      let open Ast.Type.Predicate in
      let { kind; comments } = pred in
      let annot' = this#on_loc_annot annot in
      let kind' =
        match kind with
        | Declared e -> Declared (this#expression e)
        | Inferred -> Inferred
      in
      let comments' = this#syntax_opt comments in
      (annot', { kind = kind'; comments = comments' })

    method nullable_type (t : ('M, 'T) Ast.Type.Nullable.t) : ('N, 'U) Ast.Type.Nullable.t =
      let open Ast.Type.Nullable in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method typeof_type (t : ('M, 'T) Ast.Type.Typeof.t) : ('N, 'U) Ast.Type.Typeof.t =
      let open Ast.Type.Typeof in
      let { argument; targs; comments } = t in
      let argument' = this#typeof_expression argument in
      let targs' = Option.map ~f:this#type_args targs in
      let comments' = this#syntax_opt comments in
      { argument = argument'; targs = targs'; comments = comments' }

    method typeof_expression (git : ('M, 'T) Ast.Type.Typeof.Target.t) =
      let open Ast.Type.Typeof.Target in
      match git with
      | Unqualified i -> Unqualified (this#typeof_identifier i)
      | Qualified i -> Qualified (this#typeof_qualified_identifier i)

    method typeof_identifier id = this#t_identifier id

    method typeof_member_identifier id = this#t_identifier id

    method typeof_qualified_identifier (qual : ('M, 'T) Ast.Type.Typeof.Target.qualified)
        : ('N, 'U) Ast.Type.Typeof.Target.qualified =
      let open Ast.Type.Typeof.Target in
      let (annot, { qualification; id = id_ }) = qual in
      let qualification' = this#typeof_expression qualification in
      let id' = this#typeof_member_identifier id_ in
      (this#on_type_annot annot, { qualification = qualification'; id = id' })

    method keyof_type (t : ('M, 'T) Ast.Type.Keyof.t) : ('N, 'U) Ast.Type.Keyof.t =
      let open Ast.Type.Keyof in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method component_renders_annotation (renders : ('M, 'T) Ast.Type.component_renders_annotation) =
      let open Ast.Type in
      match renders with
      | AvailableRenders (loc, renders) ->
        AvailableRenders (this#on_loc_annot loc, this#render_type renders)
      | MissingRenders loc -> MissingRenders (this#on_type_annot loc)

    method render_type (t : ('M, 'T) Ast.Type.Renders.t) : ('N, 'U) Ast.Type.Renders.t =
      let open Ast.Type.Renders in
      let { operator_loc; argument; comments; variant } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      {
        operator_loc = this#on_loc_annot operator_loc;
        argument = argument';
        comments = comments';
        variant;
      }

    method readonly_type (t : ('M, 'T) Ast.Type.ReadOnly.t) : ('N, 'U) Ast.Type.ReadOnly.t =
      let open Ast.Type.ReadOnly in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method tuple_element (element : ('M, 'T) Ast.Type.Tuple.element)
        : ('N, 'U) Ast.Type.Tuple.element =
      let open Ast.Type.Tuple in
      match element with
      | (annot, UnlabeledElement t_annot) ->
        (this#on_loc_annot annot, UnlabeledElement (this#type_ t_annot))
      | (annot, LabeledElement e) ->
        (this#on_loc_annot annot, LabeledElement (this#tuple_labeled_element e))
      | (annot, SpreadElement e) ->
        (this#on_loc_annot annot, SpreadElement (this#tuple_spread_element e))

    method tuple_labeled_element (t : ('M, 'T) Ast.Type.Tuple.LabeledElement.t)
        : ('N, 'U) Ast.Type.Tuple.LabeledElement.t =
      let open Ast.Type.Tuple.LabeledElement in
      let { annot = t_annot; name; variance; optional } = t in
      let t_annot' = this#type_ t_annot in
      let name' = this#t_identifier name in
      let variance' = this#variance_opt variance in
      { annot = t_annot'; name = name'; variance = variance'; optional }

    method tuple_spread_element (t : ('M, 'T) Ast.Type.Tuple.SpreadElement.t)
        : ('N, 'U) Ast.Type.Tuple.SpreadElement.t =
      let open Ast.Type.Tuple.SpreadElement in
      let { annot = t_annot; name } = t in
      let t_annot' = this#type_ t_annot in
      let name' = Option.map ~f:this#t_identifier name in
      { annot = t_annot'; name = name' }

    method tuple_type (t : ('M, 'T) Ast.Type.Tuple.t) : ('N, 'U) Ast.Type.Tuple.t =
      let open Ast.Type.Tuple in
      let { elements; inexact; comments } = t in
      let elements' = List.map ~f:this#tuple_element elements in
      let comments' = this#syntax_opt comments in
      { elements = elements'; inexact; comments = comments' }

    method array_type (t : ('M, 'T) Ast.Type.Array.t) : ('N, 'U) Ast.Type.Array.t =
      let open Ast.Type.Array in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method conditional_type (t : ('M, 'T) Ast.Type.Conditional.t) : ('N, 'U) Ast.Type.Conditional.t
        =
      let open Ast.Type.Conditional in
      let { check_type; extends_type; true_type; false_type; comments } = t in
      let check_type' = this#type_ check_type in
      let extends_type' = this#type_ extends_type in
      let true_type' = this#type_ true_type in
      let false_type' = this#type_ false_type in
      let comments' = Option.map ~f:this#syntax comments in
      {
        check_type = check_type';
        extends_type = extends_type';
        true_type = true_type';
        false_type = false_type';
        comments = comments';
      }

    method infer_type (t : ('M, 'T) Ast.Type.Infer.t) : ('N, 'U) Ast.Type.Infer.t =
      let open Ast.Type.Infer in
      let { tparam; comments } = t in
      let tparam' = this#type_param tparam in
      let comments' = Option.map ~f:this#syntax comments in
      { tparam = tparam'; comments = comments' }

    method union_type (t : ('M, 'T) Ast.Type.Union.t) : ('N, 'U) Ast.Type.Union.t =
      let open Ast.Type.Union in
      let { types = (t0, t1, ts); comments } = t in
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = List.map ~f:this#type_ ts in
      let comments' = this#syntax_opt comments in
      { types = (t0', t1', ts'); comments = comments' }

    method intersection_type (t : ('M, 'T) Ast.Type.Intersection.t)
        : ('N, 'U) Ast.Type.Intersection.t =
      let open Ast.Type.Intersection in
      let { types = (t0, t1, ts); comments } = t in
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = List.map ~f:this#type_ ts in
      let comments' = this#syntax_opt comments in
      { types = (t0', t1', ts'); comments = comments' }

    method string_literal (t : 'M Ast.StringLiteral.t) : 'N Ast.StringLiteral.t =
      let open Ast.StringLiteral in
      let { value; raw; comments } = t in
      let comments' = this#syntax_opt comments in
      { value; raw; comments = comments' }

    method number_literal (t : 'M Ast.NumberLiteral.t) : 'N Ast.NumberLiteral.t =
      let open Ast.NumberLiteral in
      let { value; raw; comments } = t in
      let comments' = this#syntax_opt comments in
      { value; raw; comments = comments' }

    method bigint_literal (t : 'M Ast.BigIntLiteral.t) : 'N Ast.BigIntLiteral.t =
      let open Ast.BigIntLiteral in
      let { value; raw; comments } = t in
      let comments' = this#syntax_opt comments in
      { value; raw; comments = comments' }

    method boolean_literal (t : 'M Ast.BooleanLiteral.t) : 'N Ast.BooleanLiteral.t =
      let open Ast.BooleanLiteral in
      let { value; comments } = t in
      let comments' = this#syntax_opt comments in
      { value; comments = comments' }

    method regexp_literal (t : 'M Ast.RegExpLiteral.t) : 'N Ast.RegExpLiteral.t =
      let open Ast.RegExpLiteral in
      let { pattern; flags; raw; comments } = t in
      let comments' = this#syntax_opt comments in
      { pattern; flags; raw; comments = comments' }

    method module_ref_literal (mref : ('M, 'T) Ast.ModuleRefLiteral.t)
        : ('N, 'U) Ast.ModuleRefLiteral.t =
      let open Ast.ModuleRefLiteral in
      let { value; require_loc; def_loc_opt; prefix_len; legacy_interop; raw; comments } = mref in
      let require_loc' = this#on_loc_annot require_loc in
      let def_loc_opt' = Base.Option.map ~f:this#on_loc_annot def_loc_opt in
      let comments' = this#syntax_opt comments in
      {
        value;
        require_loc = require_loc';
        def_loc_opt = def_loc_opt';
        prefix_len;
        legacy_interop;
        raw;
        comments = comments';
      }

    method type_ ((annot, t) : ('M, 'T) Ast.Type.t) : ('N, 'U) Ast.Type.t =
      let open Ast.Type in
      ( this#on_type_annot annot,
        match t with
        | Any comments -> Any (this#syntax_opt comments)
        | Mixed comments -> Mixed (this#syntax_opt comments)
        | Empty comments -> Empty (this#syntax_opt comments)
        | Void comments -> Void (this#syntax_opt comments)
        | Null comments -> Null (this#syntax_opt comments)
        | Symbol comments -> Symbol (this#syntax_opt comments)
        | Number comments -> Number (this#syntax_opt comments)
        | BigInt comments -> BigInt (this#syntax_opt comments)
        | String comments -> String (this#syntax_opt comments)
        | Boolean { raw; comments } -> Boolean { raw; comments = this#syntax_opt comments }
        | Exists comments -> Exists (this#syntax_opt comments)
        | Unknown comments -> Unknown (this#syntax_opt comments)
        | Never comments -> Never (this#syntax_opt comments)
        | Undefined comments -> Undefined (this#syntax_opt comments)
        | Nullable t' -> Nullable (this#nullable_type t')
        | Array t' -> Array (this#array_type t')
        | Conditional t' -> Conditional (this#conditional_type t')
        | Infer t' -> Infer (this#infer_type t')
        | Typeof t' -> Typeof (this#typeof_type t')
        | Keyof t' -> Keyof (this#keyof_type t')
        | Renders t' -> Renders (this#render_type t')
        | ReadOnly t' -> ReadOnly (this#readonly_type t')
        | Function ft -> Function (this#function_type ft)
        | Component c -> Component (this#component_type c)
        | Object ot -> Object (this#object_type ot)
        | Interface i -> Interface (this#interface_type i)
        | Generic gt -> Generic (this#generic_type gt)
        | IndexedAccess ia -> IndexedAccess (this#indexed_access_type ia)
        | OptionalIndexedAccess ia -> OptionalIndexedAccess (this#optional_indexed_access_type ia)
        | Union t' -> Union (this#union_type t')
        | Intersection t' -> Intersection (this#intersection_type t')
        | Tuple t' -> Tuple (this#tuple_type t')
        | StringLiteral t' -> StringLiteral (this#string_literal t')
        | NumberLiteral t' -> NumberLiteral (this#number_literal t')
        | BigIntLiteral t' -> BigIntLiteral (this#bigint_literal t')
        | BooleanLiteral t' -> BooleanLiteral (this#boolean_literal t')
      )

    method implicit (t : ('M, 'T) Ast.Expression.CallTypeArg.Implicit.t)
        : ('N, 'U) Ast.Expression.CallTypeArg.Implicit.t =
      let open Ast.Expression.CallTypeArg.Implicit in
      let (annot, { comments }) = t in
      let annot' = this#on_type_annot annot in
      let comments' = this#syntax_opt comments in
      (annot', { comments = comments' })

    method type_annotation ((annot, t_annot) : ('M, 'T) Ast.Type.annotation) =
      (this#on_loc_annot annot, this#type_ t_annot)

    method type_annotation_hint (return : ('M, 'T) Ast.Type.annotation_or_hint)
        : ('N, 'U) Ast.Type.annotation_or_hint =
      let open Ast.Type in
      match return with
      | Available annot -> Available (this#type_annotation annot)
      | Missing loc -> Missing (this#on_type_annot loc)

    method type_guard (guard : ('M, 'T) Ast.Type.TypeGuard.t) : ('N, 'U) Ast.Type.TypeGuard.t =
      let open Ast.Type.TypeGuard in
      let (annot, { kind; guard = (x, t); comments }) = guard in
      let annot' = this#on_loc_annot annot in
      let x' = this#identifier x in
      let t' = Option.map ~f:this#type_ t in
      let comments' = this#syntax_with_internal_opt comments in
      (annot', { kind; guard = (x', t'); comments = comments' })

    method type_guard_annotation ((annot, type_guard) : ('M, 'T) Ast.Type.type_guard_annotation)
        : ('N, 'U) Ast.Type.type_guard_annotation =
      let annot' = this#on_loc_annot annot in
      let type_guard' = this#type_guard type_guard in
      (annot', type_guard')

    method function_declaration (stmt : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
      this#function_ stmt

    method function_expression (expr : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
      this#function_ expr

    (** previously, we conflated [function_expression] and [class_method]. callers should be
        updated to override those individually.

        DEPRECATED: use either function_expression or class_method *)
    method function_expression_or_method (expr : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t
        =
      this#function_ expr

    method function_return_annotation (return : ('M, 'T) Ast.Function.ReturnAnnot.t)
        : ('N, 'U) Ast.Function.ReturnAnnot.t =
      let open Ast.Function.ReturnAnnot in
      match return with
      | Missing loc -> Missing (this#on_type_annot loc)
      | Available annot -> Available (this#type_annotation annot)
      | TypeGuard guard -> TypeGuard (this#type_guard_annotation guard)

    (* Internal helper for function declarations, function expressions and arrow functions *)
    method function_ (expr : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
      let open Ast.Function in
      let {
        id = ident;
        params;
        body;
        async;
        generator;
        effect;
        predicate;
        return;
        tparams;
        sig_loc;
        comments;
      } =
        expr
      in
      let ident' = Option.map ~f:this#function_identifier ident in
      this#type_params_opt tparams (fun tparams' ->
          let params' = this#function_params params in
          let return' = this#function_return_annotation return in
          let body' = this#function_body_any body in
          let predicate' = Option.map ~f:this#predicate predicate in
          let sig_loc' = this#on_loc_annot sig_loc in
          let comments' = this#syntax_opt comments in
          {
            id = ident';
            params = params';
            return = return';
            body = body';
            async;
            generator;
            effect;
            predicate = predicate';
            tparams = tparams';
            sig_loc = sig_loc';
            comments = comments';
          }
      )

    method function_params (params : ('M, 'T) Ast.Function.Params.t)
        : ('N, 'U) Ast.Function.Params.t =
      let (annot, { Ast.Function.Params.params = params_list; rest; comments; this_ }) = params in
      let params_list' = List.map ~f:this#function_param params_list in
      let rest' = Option.map ~f:this#function_rest_param rest in
      let this_' = Option.map ~f:this#function_this_param this_ in
      let comments' = this#syntax_with_internal_opt comments in
      ( this#on_loc_annot annot,
        {
          Ast.Function.Params.params = params_list';
          rest = rest';
          comments = comments';
          this_ = this_';
        }
      )

    method function_param (param : ('M, 'T) Ast.Function.Param.t) : ('N, 'U) Ast.Function.Param.t =
      let open Ast.Function.Param in
      let (annot, { argument; default }) = param in
      let annot' = this#on_loc_annot annot in
      let argument' = this#function_param_pattern argument in
      let default' = this#default_opt default in
      (annot', { argument = argument'; default = default' })

    method function_rest_param (expr : ('M, 'T) Ast.Function.RestParam.t)
        : ('N, 'U) Ast.Function.RestParam.t =
      let open Ast.Function.RestParam in
      let (annot, { argument; comments }) = expr in
      let annot' = this#on_loc_annot annot in
      let argument' = this#function_param_pattern argument in
      let comments' = this#syntax_opt comments in
      (annot', { argument = argument'; comments = comments' })

    method function_this_param (this_param : ('M, 'T) Ast.Function.ThisParam.t)
        : ('N, 'U) Ast.Function.ThisParam.t =
      let open Ast.Function.ThisParam in
      let (loc, { annot; comments }) = this_param in
      let loc' = this#on_loc_annot loc in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      (loc', { annot = annot'; comments = comments' })

    method function_body_any (body : ('M, 'T) Ast.Function.body) : ('N, 'U) Ast.Function.body =
      let open Ast.Function in
      match body with
      | BodyBlock body -> BodyBlock (this#function_body body)
      | BodyExpression expr -> BodyExpression (this#body_expression expr)

    method function_body (body : 'M * ('M, 'T) Ast.Statement.Block.t)
        : 'N * ('N, 'U) Ast.Statement.Block.t =
      (this#on_loc_annot * this#block) body

    method body_expression (expr : ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
      this#expression expr

    method function_identifier (ident : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#pattern_identifier ~kind:Ast.Variable.Var ident

    method identifier ((annot, { Ast.Identifier.name; comments }) : ('M, 'M) Ast.Identifier.t)
        : ('N, 'N) Ast.Identifier.t =
      let annot = this#on_loc_annot annot in
      let comments = this#syntax_opt comments in
      (annot, { Ast.Identifier.name; comments })

    method type_identifier id = this#t_identifier id

    method type_identifier_reference id = this#type_identifier id

    method binding_type_identifier id = this#type_identifier id

    method t_identifier ((annot, { Ast.Identifier.name; comments }) : ('M, 'T) Ast.Identifier.t)
        : ('N, 'U) Ast.Identifier.t =
      let annot = this#on_type_annot annot in
      let comments = this#syntax_opt comments in
      (annot, { Ast.Identifier.name; comments })

    method interface _annot (interface : ('M, 'T) Ast.Statement.Interface.t)
        : ('N, 'U) Ast.Statement.Interface.t =
      let open Ast.Statement.Interface in
      let { id = ident; tparams; extends; body; comments } = interface in
      let id' = this#binding_type_identifier ident in
      this#type_params_opt tparams (fun tparams' ->
          let extends' = List.map ~f:(this#on_loc_annot * this#generic_type) extends in
          let body' = (this#on_loc_annot * this#object_type) body in
          let comments' = this#syntax_opt comments in
          { id = id'; tparams = tparams'; extends = extends'; body = body'; comments = comments' }
      )

    method interface_declaration annot (decl : ('M, 'T) Ast.Statement.Interface.t)
        : ('N, 'U) Ast.Statement.Interface.t =
      this#interface annot decl

    method private_name ((annot, ident) : 'M Ast.PrivateName.t) : 'N Ast.PrivateName.t =
      let open Ast.PrivateName in
      let { name; comments } = ident in
      let annot' = this#on_loc_annot annot in
      let comments' = this#syntax_opt comments in
      (annot', { name; comments = comments' })

    method computed_key ((annot, key) : ('M, 'T) Ast.ComputedKey.t) : ('N, 'U) Ast.ComputedKey.t =
      let open Ast.ComputedKey in
      let { expression; comments } = key in
      let annot' = this#on_loc_annot annot in
      let expression' = this#expression expression in
      let comments' = this#syntax_opt comments in
      (annot', { expression = expression'; comments = comments' })

    method import _annot (expr : ('M, 'T) Ast.Expression.Import.t)
        : ('N, 'U) Ast.Expression.Import.t =
      let open Ast.Expression.Import in
      let { argument; comments } = expr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method if_consequent_statement ~has_else (stmt : ('M, 'T) Ast.Statement.t)
        : ('N, 'U) Ast.Statement.t =
      ignore has_else;
      this#statement stmt

    method if_alternate_statement (altern : ('M, 'T) Ast.Statement.If.Alternate.t)
        : ('N, 'U) Ast.Statement.If.Alternate.t =
      let open Ast.Statement.If.Alternate in
      let (annot, { body; comments }) = altern in
      let annot' = this#on_loc_annot annot in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      (annot', { body = body'; comments = comments' })

    method if_statement (stmt : ('M, 'T) Ast.Statement.If.t) : ('N, 'U) Ast.Statement.If.t =
      let open Ast.Statement.If in
      let { test; consequent; alternate; comments } = stmt in
      let test' = this#predicate_expression test in
      let consequent' =
        this#if_consequent_statement ~has_else:(Option.is_some alternate) consequent
      in
      let alternate' = Option.map ~f:this#if_alternate_statement alternate in
      let comments' = this#syntax_opt comments in
      { test = test'; consequent = consequent'; alternate = alternate'; comments = comments' }

    method import_declaration _loc (decl : ('M, 'T) Ast.Statement.ImportDeclaration.t)
        : ('N, 'U) Ast.Statement.ImportDeclaration.t =
      let open Ast.Statement.ImportDeclaration in
      let { import_kind; source; specifiers; default; comments } = decl in
      let source' =
        let (annot, lit) = source in
        (this#on_type_annot annot, this#import_source annot lit)
      in
      let specifiers' = Option.map ~f:(this#import_specifier ~import_kind) specifiers in
      let default' =
        Option.map
          ~f:(fun { identifier; remote_default_name_def_loc } ->
            {
              identifier = this#import_default_specifier ~import_kind identifier;
              remote_default_name_def_loc =
                Option.map ~f:this#on_loc_annot remote_default_name_def_loc;
            })
          default
      in
      let comments' = this#syntax_opt comments in
      {
        import_kind;
        source = source';
        specifiers = specifiers';
        default = default';
        comments = comments';
      }

    method import_source _loc source = this#string_literal source

    method import_specifier
        ~import_kind (specifier : ('M, 'T) Ast.Statement.ImportDeclaration.specifier)
        : ('N, 'U) Ast.Statement.ImportDeclaration.specifier =
      let open Ast.Statement.ImportDeclaration in
      match specifier with
      | ImportNamedSpecifiers named_specifiers ->
        let named_specifiers' =
          List.map ~f:(this#import_named_specifier ~import_kind) named_specifiers
        in
        ImportNamedSpecifiers named_specifiers'
      | ImportNamespaceSpecifier (annot, ident) ->
        let ident' = this#import_namespace_specifier ~import_kind annot ident in
        ImportNamespaceSpecifier (this#on_loc_annot annot, ident')

    method remote_identifier id = this#t_identifier id

    method import_named_specifier
        ~(import_kind : Ast.Statement.ImportDeclaration.import_kind)
        (specifier : ('M, 'T) Ast.Statement.ImportDeclaration.named_specifier)
        : ('N, 'U) Ast.Statement.ImportDeclaration.named_specifier =
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
          Some (local_visitor ident)
      in
      let remote_name_def_loc' = Option.map ~f:this#on_loc_annot remote_name_def_loc in
      { kind; local = local'; remote = remote'; remote_name_def_loc = remote_name_def_loc' }

    method import_default_specifier
        ~(import_kind : Ast.Statement.ImportDeclaration.import_kind) (id : ('M, 'T) Ast.Identifier.t)
        : ('N, 'U) Ast.Identifier.t =
      ignore import_kind;
      this#pattern_identifier ~kind:Ast.Variable.Let id

    method import_namespace_specifier
        ~(import_kind : Ast.Statement.ImportDeclaration.import_kind)
        _loc
        (id : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      ignore import_kind;
      this#pattern_identifier ~kind:Ast.Variable.Let id

    method jsx_element (_annot : 'T) (expr : ('M, 'T) Ast.JSX.element) =
      let open Ast.JSX in
      let { opening_element; closing_element; children; comments } = expr in
      let opening_element' = this#jsx_opening_element opening_element in
      let closing_element' = Option.map ~f:this#jsx_closing_element closing_element in
      let children' = this#jsx_children children in
      let comments' = this#syntax_opt comments in
      {
        opening_element = opening_element';
        closing_element = closing_element';
        children = children';
        comments = comments';
      }

    method jsx_fragment (expr : ('M, 'T) Ast.JSX.fragment) : ('N, 'U) Ast.JSX.fragment =
      let open Ast.JSX in
      let { frag_opening_element; frag_closing_element; frag_children; frag_comments } = expr in
      let opening' = this#on_loc_annot frag_opening_element in
      let closing' = this#on_loc_annot frag_closing_element in
      let children' = this#jsx_children frag_children in
      let frag_comments' = this#syntax_opt frag_comments in
      {
        frag_opening_element = opening';
        frag_closing_element = closing';
        frag_children = children';
        frag_comments = frag_comments';
      }

    method jsx_opening_element (elem : ('M, 'T) Ast.JSX.Opening.t) : ('N, 'U) Ast.JSX.Opening.t =
      let open Ast.JSX.Opening in
      let (annot, { name; targs; self_closing; attributes }) = elem in
      let name' = this#jsx_element_name name in
      let targs' = Option.map ~f:this#call_type_args targs in
      let attributes' = List.map ~f:this#jsx_opening_attribute attributes in
      ( this#on_loc_annot annot,
        { name = name'; targs = targs'; self_closing; attributes = attributes' }
      )

    method jsx_closing_element (elem : ('M, 'T) Ast.JSX.Closing.t) : ('N, 'U) Ast.JSX.Closing.t =
      let open Ast.JSX.Closing in
      let (annot, { name }) = elem in
      let name' = this#jsx_element_name name in
      (this#on_loc_annot annot, { name = name' })

    method jsx_opening_attribute (jsx_attr : ('M, 'T) Ast.JSX.Opening.attribute)
        : ('N, 'U) Ast.JSX.Opening.attribute =
      let open Ast.JSX.Opening in
      match jsx_attr with
      | Attribute attr -> Attribute (this#jsx_attribute attr)
      | SpreadAttribute (annot, attr) ->
        SpreadAttribute (this#on_loc_annot annot, this#jsx_spread_attribute attr)

    method jsx_spread_attribute (attr : ('M, 'T) Ast.JSX.SpreadAttribute.t')
        : ('N, 'U) Ast.JSX.SpreadAttribute.t' =
      let open Ast.JSX.SpreadAttribute in
      let { argument; comments } = attr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method jsx_attribute (attr : ('M, 'T) Ast.JSX.Attribute.t) : ('N, 'U) Ast.JSX.Attribute.t =
      let open Ast.JSX.Attribute in
      let (annot, { name; value }) = attr in
      let name' = this#jsx_attribute_name name in
      let value' = Option.map ~f:this#jsx_attribute_value value in
      (this#on_loc_annot annot, { name = name'; value = value' })

    method jsx_attribute_name (name : ('M, 'T) Ast.JSX.Attribute.name) =
      let open Ast.JSX.Attribute in
      match name with
      | Identifier id -> Identifier (this#jsx_attribute_name_identifier id)
      | NamespacedName nname -> NamespacedName (this#jsx_attribute_name_namespaced nname)

    method jsx_attribute_name_identifier id = this#jsx_identifier id

    method jsx_attribute_name_namespaced nname = this#jsx_namespaced_name nname

    method jsx_attribute_value (value : ('M, 'T) Ast.JSX.Attribute.value)
        : ('N, 'U) Ast.JSX.Attribute.value =
      let open Ast.JSX.Attribute in
      match value with
      | StringLiteral lit -> StringLiteral (this#jsx_attribute_value_literal lit)
      | ExpressionContainer expr -> ExpressionContainer (this#jsx_attribute_value_expression expr)

    method jsx_attribute_value_literal lit =
      let (annot, lit) = lit in
      (this#on_type_annot annot, this#string_literal lit)

    method jsx_attribute_value_expression expr =
      let (annot, expr) = expr in
      (this#on_type_annot annot, this#jsx_expression expr)

    method jsx_children (children : 'M * ('M, 'T) Ast.JSX.child list)
        : 'N * ('N, 'U) Ast.JSX.child list =
      let (annot, children') = children in
      (this#on_loc_annot annot, List.map ~f:this#jsx_child children')

    method jsx_child (child : ('M, 'T) Ast.JSX.child) : ('N, 'U) Ast.JSX.child =
      let open Ast.JSX in
      let (annot, child') = child in
      ( this#on_type_annot annot,
        match child' with
        | Element elem -> Element (this#jsx_element annot elem)
        | Fragment frag -> Fragment (this#jsx_fragment frag)
        | ExpressionContainer expr -> ExpressionContainer (this#jsx_expression expr)
        | SpreadChild spread -> SpreadChild (this#jsx_spread_child spread)
        | Text _ as child' -> child'
      )

    method jsx_expression (jsx_expr : ('M, 'T) Ast.JSX.ExpressionContainer.t)
        : ('N, 'U) Ast.JSX.ExpressionContainer.t =
      let open Ast.JSX.ExpressionContainer in
      let { expression; comments } = jsx_expr in
      let expression' =
        match expression with
        | Expression expr -> Expression (this#expression expr)
        | EmptyExpression -> EmptyExpression
      in
      let comments' = this#syntax_with_internal_opt comments in
      { expression = expression'; comments = comments' }

    method jsx_spread_child (jsx_spread_child : ('M, 'T) Ast.JSX.SpreadChild.t)
        : ('N, 'U) Ast.JSX.SpreadChild.t =
      let open Ast.JSX.SpreadChild in
      let { expression; comments } = jsx_spread_child in
      let expression' = this#expression expression in
      let comments' = this#syntax_opt comments in
      { expression = expression'; comments = comments' }

    method jsx_element_name (name : ('M, 'T) Ast.JSX.name) : ('N, 'U) Ast.JSX.name =
      let open Ast.JSX in
      match name with
      | Identifier id -> Identifier (this#jsx_element_name_identifier id)
      | NamespacedName namespaced_name ->
        NamespacedName (this#jsx_element_name_namespaced namespaced_name)
      | MemberExpression member_exp ->
        MemberExpression (this#jsx_element_name_member_expression member_exp)

    method jsx_element_name_identifier ident = this#jsx_identifier ident

    method jsx_element_name_namespaced ns = this#jsx_namespaced_name ns

    method jsx_element_name_member_expression expr = this#jsx_member_expression expr

    method jsx_namespaced_name (namespaced_name : ('M, 'T) Ast.JSX.NamespacedName.t)
        : ('N, 'U) Ast.JSX.NamespacedName.t =
      let open Ast.JSX in
      NamespacedName.(
        let (annot, { namespace; name }) = namespaced_name in
        let namespace' = this#jsx_identifier namespace in
        let name' = this#jsx_identifier name in
        (this#on_loc_annot annot, { namespace = namespace'; name = name' })
      )

    method jsx_member_expression (member_exp : ('M, 'T) Ast.JSX.MemberExpression.t)
        : ('N, 'U) Ast.JSX.MemberExpression.t =
      let open Ast.JSX in
      let (annot, { MemberExpression._object; MemberExpression.property }) = member_exp in
      let _object' = this#jsx_member_expression_object _object in
      let property' = this#jsx_identifier property in
      (this#on_loc_annot annot, MemberExpression.{ _object = _object'; property = property' })

    method jsx_member_expression_object (_object : ('M, 'T) Ast.JSX.MemberExpression._object)
        : ('N, 'U) Ast.JSX.MemberExpression._object =
      let open Ast.JSX.MemberExpression in
      match _object with
      | Identifier id ->
        let id' = this#jsx_member_expression_identifier id in
        Identifier id'
      | MemberExpression nested_exp ->
        let nested_exp' = this#jsx_member_expression nested_exp in
        MemberExpression nested_exp'

    method jsx_member_expression_identifier id = this#jsx_element_name_identifier id

    method jsx_identifier ((annot, id) : ('M, 'T) Ast.JSX.Identifier.t)
        : ('N, 'U) Ast.JSX.Identifier.t =
      let open Ast.JSX.Identifier in
      let { name; comments } = id in
      let annot' = this#on_type_annot annot in
      let comments' = this#syntax_opt comments in
      (annot', { name; comments = comments' })

    method labeled_statement (stmt : ('M, 'T) Ast.Statement.Labeled.t)
        : ('N, 'U) Ast.Statement.Labeled.t =
      let open Ast.Statement.Labeled in
      let { label; body; comments } = stmt in
      let label' = this#label_identifier label in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      { label = label'; body = body'; comments = comments' }

    method logical (expr : ('M, 'T) Ast.Expression.Logical.t) : ('N, 'U) Ast.Expression.Logical.t =
      let open Ast.Expression.Logical in
      let { operator; left; right; comments } = expr in
      let left' = this#expression left in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      { operator; left = left'; right = right'; comments = comments' }

    method match_expression (x : ('M, 'T) Ast.Expression.Match.t) : ('N, 'U) Ast.Expression.Match.t
        =
      let open Ast.Expression.Match in
      let { arg; cases; arg_internal; match_keyword_loc; comments } = x in
      let arg' = this#expression arg in
      let cases' = List.map ~f:(this#on_loc_annot * this#match_expression_case) cases in
      let arg_internal' = this#on_loc_annot arg_internal in
      let match_keyword_loc' = this#on_type_annot match_keyword_loc in
      let comments' = this#syntax_opt comments in
      {
        arg = arg';
        cases = cases';
        arg_internal = arg_internal';
        match_keyword_loc = match_keyword_loc';
        comments = comments';
      }

    method match_expression_case (case : ('M, 'T) Ast.Expression.Match.Case.t')
        : ('N, 'U) Ast.Expression.Match.Case.t' =
      let open Ast.Expression.Match.Case in
      let { pattern; body; guard; comments } = case in
      let pattern' = this#match_pattern pattern in
      let guard' = Option.map ~f:this#expression guard in
      let body' = this#expression body in
      let comments' = this#syntax_opt comments in
      { pattern = pattern'; body = body'; guard = guard'; comments = comments' }

    method match_statement (x : ('M, 'T) Ast.Statement.Match.t) : ('N, 'U) Ast.Statement.Match.t =
      let open Ast.Statement.Match in
      let { arg; cases; comments } = x in
      let arg' = this#expression arg in
      let cases' = List.map ~f:(this#on_loc_annot * this#match_statement_case) cases in
      let comments' = this#syntax_opt comments in
      { arg = arg'; cases = cases'; comments = comments' }

    method match_statement_case (case : ('M, 'T) Ast.Statement.Match.Case.t') =
      let open Ast.Statement.Match.Case in
      let { pattern; body; guard; comments } = case in
      let pattern' = this#match_pattern pattern in
      let guard' = Option.map ~f:this#expression guard in
      let body' = (this#on_loc_annot * this#block) body in
      let comments' = this#syntax_opt comments in
      { pattern = pattern'; body = body'; guard = guard'; comments = comments' }

    method match_pattern (pattern : ('M, 'T) Ast.MatchPattern.t) : ('N, 'U) Ast.MatchPattern.t =
      let open Ast.MatchPattern in
      let (annot, patt) = pattern in
      ( this#on_loc_annot annot,
        match patt with
        | WildcardPattern x -> WildcardPattern (this#syntax_opt x)
        | StringPattern x -> StringPattern (this#string_literal x)
        | BooleanPattern x -> BooleanPattern (this#boolean_literal x)
        | NullPattern x -> NullPattern (this#syntax_opt x)
        | NumberPattern x -> NumberPattern (this#number_literal x)
        | BigIntPattern x -> BigIntPattern (this#bigint_literal x)
        | UnaryPattern x -> UnaryPattern (this#match_unary_pattern x)
        | IdentifierPattern x -> IdentifierPattern (this#t_identifier x)
        | MemberPattern x -> MemberPattern (this#match_member_pattern x)
        | BindingPattern x -> BindingPattern (this#match_binding_pattern x)
        | ObjectPattern x -> ObjectPattern (this#match_object_pattern x)
        | ArrayPattern x -> ArrayPattern (this#match_array_pattern x)
        | OrPattern x -> OrPattern (this#match_or_pattern x)
        | AsPattern x -> AsPattern (this#match_as_pattern x)
      )

    method match_unary_pattern (unary_pattern : 'M Ast.MatchPattern.UnaryPattern.t)
        : 'N Ast.MatchPattern.UnaryPattern.t =
      let open Ast.MatchPattern.UnaryPattern in
      let { operator; argument; comments } = unary_pattern in
      let (arg_loc, arg) = argument in
      let argument' = (this#on_loc_annot arg_loc, this#match_unary_pattern_argument arg) in
      let comments' = this#syntax_opt comments in
      { operator; argument = argument'; comments = comments' }

    method match_member_pattern (member_pattern : ('M, 'T) Ast.MatchPattern.MemberPattern.t)
        : ('N, 'U) Ast.MatchPattern.MemberPattern.t =
      let open Ast.MatchPattern.MemberPattern in
      let (loc, { base; property; comments }) = member_pattern in
      let loc' = this#on_type_annot loc in
      let base' = this#match_member_pattern_base base in
      let property' = this#match_member_pattern_property property in
      let comments' = this#syntax_opt comments in
      (loc', { base = base'; property = property'; comments = comments' })

    method match_member_pattern_base (base : ('M, 'T) Ast.MatchPattern.MemberPattern.base)
        : ('N, 'U) Ast.MatchPattern.MemberPattern.base =
      let open Ast.MatchPattern.MemberPattern in
      match base with
      | BaseIdentifier ident -> BaseIdentifier (this#t_identifier ident)
      | BaseMember mem -> BaseMember (this#match_member_pattern mem)

    method match_member_pattern_property (prop : ('M, 'T) Ast.MatchPattern.MemberPattern.property)
        : ('N, 'U) Ast.MatchPattern.MemberPattern.property =
      let open Ast.MatchPattern.MemberPattern in
      match prop with
      | PropertyString (loc, lit) -> PropertyString (this#on_loc_annot loc, this#string_literal lit)
      | PropertyNumber (loc, lit) -> PropertyNumber (this#on_loc_annot loc, this#number_literal lit)
      | PropertyIdentifier ident -> PropertyIdentifier (this#t_identifier ident)

    method match_unary_pattern_argument (argument : 'M Ast.MatchPattern.UnaryPattern.argument)
        : 'N Ast.MatchPattern.UnaryPattern.argument =
      let open Ast.MatchPattern.UnaryPattern in
      match argument with
      | NumberLiteral lit -> NumberLiteral (this#number_literal lit)
      | BigIntLiteral lit -> BigIntLiteral (this#bigint_literal lit)

    method match_binding_pattern (binding_pattern : ('M, 'T) Ast.MatchPattern.BindingPattern.t)
        : ('N, 'U) Ast.MatchPattern.BindingPattern.t =
      let open Ast.MatchPattern.BindingPattern in
      let { id; kind; comments } = binding_pattern in
      let id' = this#pattern_identifier ~kind id in
      let comments' = this#syntax_opt comments in
      { id = id'; kind; comments = comments' }

    method match_object_pattern (object_pattern : ('M, 'T) Ast.MatchPattern.ObjectPattern.t)
        : ('N, 'U) Ast.MatchPattern.ObjectPattern.t =
      let open Ast.MatchPattern.ObjectPattern in
      let { properties; rest; comments } = object_pattern in
      let properties' = List.map ~f:this#match_object_pattern_property properties in
      let rest' = Option.map rest ~f:(this#on_loc_annot * this#match_rest_pattern) in
      let comments' = this#syntax_with_internal_opt comments in
      { properties = properties'; rest = rest'; comments = comments' }

    method match_object_pattern_property (prop : ('M, 'T) Ast.MatchPattern.ObjectPattern.Property.t)
        : ('N, 'U) Ast.MatchPattern.ObjectPattern.Property.t =
      let open Ast.MatchPattern.ObjectPattern.Property in
      let (loc, { key; pattern; shorthand; comments }) = prop in
      let key' = this#match_object_pattern_property_key key in
      let pattern' = this#match_pattern pattern in
      let comments' = this#syntax_opt comments in
      (this#on_loc_annot loc, { key = key'; pattern = pattern'; shorthand; comments = comments' })

    method match_object_pattern_property_key
        (key : ('M, 'T) Ast.MatchPattern.ObjectPattern.Property.key)
        : ('N, 'U) Ast.MatchPattern.ObjectPattern.Property.key =
      let open Ast.MatchPattern.ObjectPattern.Property in
      match key with
      | StringLiteral (annot, lit) ->
        StringLiteral (this#on_loc_annot annot, this#string_literal lit)
      | NumberLiteral (annot, lit) ->
        NumberLiteral (this#on_loc_annot annot, this#number_literal lit)
      | Identifier ident -> Identifier (this#t_identifier ident)

    method match_array_pattern (array_pattern : ('M, 'T) Ast.MatchPattern.ArrayPattern.t)
        : ('N, 'U) Ast.MatchPattern.ArrayPattern.t =
      let open Ast.MatchPattern.ArrayPattern in
      let { elements; rest; comments } = array_pattern in
      let elements' = List.map ~f:this#match_pattern_array_element elements in
      let rest' = Option.map rest ~f:(this#on_loc_annot * this#match_rest_pattern) in
      let comments' = this#syntax_with_internal_opt comments in
      { elements = elements'; rest = rest'; comments = comments' }

    method match_pattern_array_element (element : ('M, 'T) Ast.MatchPattern.ArrayPattern.Element.t)
        =
      let open Ast.MatchPattern.ArrayPattern.Element in
      let { pattern; index } = element in
      let pattern' = this#match_pattern pattern in
      let index' = this#on_loc_annot index in
      { pattern = pattern'; index = index' }

    method match_rest_pattern (rest : ('M, 'T) Ast.MatchPattern.RestPattern.t')
        : ('N, 'U) Ast.MatchPattern.RestPattern.t' =
      let open Ast.MatchPattern.RestPattern in
      let { argument; comments } = rest in
      let argument' = Option.map argument ~f:(this#on_loc_annot * this#match_binding_pattern) in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method match_or_pattern (or_pattern : ('M, 'T) Ast.MatchPattern.OrPattern.t)
        : ('N, 'U) Ast.MatchPattern.OrPattern.t =
      let open Ast.MatchPattern.OrPattern in
      let { patterns; comments } = or_pattern in
      let patterns' = List.map ~f:this#match_pattern patterns in
      let comments' = this#syntax_opt comments in
      { patterns = patterns'; comments = comments' }

    method match_as_pattern (as_pattern : ('M, 'T) Ast.MatchPattern.AsPattern.t)
        : ('N, 'U) Ast.MatchPattern.AsPattern.t =
      let open Ast.MatchPattern.AsPattern in
      let { pattern; target; comments } = as_pattern in
      let pattern' = this#match_pattern pattern in
      let target' =
        match target with
        | Binding (loc, binding) ->
          Binding (this#on_loc_annot loc, this#match_binding_pattern binding)
        | Identifier id -> Identifier (this#pattern_identifier ~kind:Ast.Variable.Const id)
      in
      let comments' = this#syntax_opt comments in
      { pattern = pattern'; target = target'; comments = comments' }

    method member (_annot : 'T) (expr : ('M, 'T) Ast.Expression.Member.t)
        : ('N, 'U) Ast.Expression.Member.t =
      let open Ast.Expression.Member in
      let { _object; property; comments } = expr in
      let _object' = this#expression _object in
      let property' = this#member_property property in
      let comments' = this#syntax_opt comments in
      { _object = _object'; property = property'; comments = comments' }

    method optional_member (annot : 'T) (expr : ('M, 'T) Ast.Expression.OptionalMember.t)
        : ('N, 'U) Ast.Expression.OptionalMember.t =
      let open Ast.Expression.OptionalMember in
      let { member; optional; filtered_out } = expr in
      let member' = this#member annot member in
      let filtered_out' = this#on_type_annot filtered_out in
      { member = member'; optional; filtered_out = filtered_out' }

    method member_property (expr : ('M, 'T) Ast.Expression.Member.property)
        : ('N, 'U) Ast.Expression.Member.property =
      let open Ast.Expression.Member in
      match expr with
      | PropertyIdentifier ident -> PropertyIdentifier (this#member_property_identifier ident)
      | PropertyPrivateName ident -> PropertyPrivateName (this#member_private_name ident)
      | PropertyExpression e -> PropertyExpression (this#member_property_expression e)

    method member_property_identifier (ident : ('M, 'T) Ast.Identifier.t)
        : ('N, 'U) Ast.Identifier.t =
      this#t_identifier ident

    method member_private_name (name : 'M Ast.PrivateName.t) : 'N Ast.PrivateName.t =
      this#private_name name

    method member_property_expression (expr : ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t
        =
      this#expression expr

    method meta_property (expr : 'M Ast.Expression.MetaProperty.t)
        : 'N Ast.Expression.MetaProperty.t =
      let open Ast.Expression.MetaProperty in
      let { meta; property; comments } = expr in
      {
        meta = this#identifier meta;
        property = this#identifier property;
        comments = this#syntax_opt comments;
      }

    method new_ _annot (expr : ('M, 'T) Ast.Expression.New.t) : ('N, 'U) Ast.Expression.New.t =
      let open Ast.Expression.New in
      let { callee; targs; arguments; comments } = expr in
      let callee' = this#expression callee in
      let targs' = Option.map ~f:this#call_type_args targs in
      let arguments' = Option.map ~f:this#arg_list arguments in
      let comments' = this#syntax_opt comments in
      { callee = callee'; targs = targs'; arguments = arguments'; comments = comments' }

    method object_ (expr : ('M, 'T) Ast.Expression.Object.t) : ('N, 'U) Ast.Expression.Object.t =
      let open Ast.Expression.Object in
      let { properties; comments } = expr in
      let comments' = this#syntax_with_internal_opt comments in
      let properties' = List.map ~f:this#object_property_or_spread_property properties in
      { properties = properties'; comments = comments' }

    method object_property_or_spread_property (prop : ('M, 'T) Ast.Expression.Object.property)
        : ('N, 'U) Ast.Expression.Object.property =
      let open Ast.Expression.Object in
      match prop with
      | Property p -> Property (this#object_property p)
      | SpreadProperty s -> SpreadProperty (this#spread_property s)

    method object_property (prop : ('M, 'T) Ast.Expression.Object.Property.t)
        : ('N, 'U) Ast.Expression.Object.Property.t =
      let open Ast.Expression.Object.Property in
      let (annot, prop') = prop in
      ( this#on_loc_annot annot,
        match prop' with
        | Init { key; value; shorthand } ->
          let key' = this#object_key key in
          let value' = this#expression value in
          Init { key = key'; value = value'; shorthand }
        | Method { key; value = (fn_annot, fn) } ->
          let key' = this#object_key key in
          let fn' = this#function_expression fn in
          Method { key = key'; value = (this#on_loc_annot fn_annot, fn') }
        | Get { key; value = (fn_annot, fn); comments } ->
          let key' = this#object_key key in
          let fn' = this#function_expression fn in
          let comments' = this#syntax_opt comments in
          Get { key = key'; value = (this#on_loc_annot fn_annot, fn'); comments = comments' }
        | Set { key; value = (fn_annot, fn); comments } ->
          let key' = this#object_key key in
          let fn' = this#function_expression fn in
          let comments' = this#syntax_opt comments in
          Set { key = key'; value = (this#on_loc_annot fn_annot, fn'); comments = comments' }
      )

    method object_key (key : ('M, 'T) Ast.Expression.Object.Property.key)
        : ('N, 'U) Ast.Expression.Object.Property.key =
      let open Ast.Expression.Object.Property in
      match key with
      | StringLiteral literal -> StringLiteral (this#object_key_string_literal literal)
      | NumberLiteral literal -> NumberLiteral (this#object_key_number_literal literal)
      | BigIntLiteral literal -> BigIntLiteral (this#object_key_bigint_literal literal)
      | Identifier ident -> Identifier (this#object_key_identifier ident)
      | PrivateName ident -> PrivateName (this#object_key_private_name ident)
      | Computed computed -> Computed (this#object_key_computed computed)

    method object_key_string_literal literal =
      let (annot, lit) = literal in
      (this#on_type_annot annot, this#string_literal lit)

    method object_key_number_literal literal =
      let (annot, lit) = literal in
      (this#on_type_annot annot, this#number_literal lit)

    method object_key_bigint_literal literal =
      let (annot, lit) = literal in
      (this#on_type_annot annot, this#bigint_literal lit)

    method object_key_identifier (ident : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#t_identifier ident

    method object_key_private_name (key : 'M Ast.PrivateName.t) : 'N Ast.PrivateName.t =
      this#private_name key

    method object_key_computed (key : ('M, 'T) Ast.ComputedKey.t) : ('N, 'U) Ast.ComputedKey.t =
      this#computed_key key

    method opaque_type _annot (otype : ('M, 'T) Ast.Statement.OpaqueType.t)
        : ('N, 'U) Ast.Statement.OpaqueType.t =
      let open Ast.Statement.OpaqueType in
      let { id; tparams; impltype; supertype; comments } = otype in
      let id' = this#binding_type_identifier id in
      this#type_params_opt tparams (fun tparams' ->
          let impltype' = Option.map ~f:this#type_ impltype in
          let supertype' = Option.map ~f:this#type_ supertype in
          let comments' = this#syntax_opt comments in
          {
            id = id';
            tparams = tparams';
            impltype = impltype';
            supertype = supertype';
            comments = comments';
          }
      )

    method declare_opaque_type annot (otype : ('M, 'T) Ast.Statement.OpaqueType.t)
        : ('N, 'U) Ast.Statement.OpaqueType.t =
      this#opaque_type annot otype

    method function_param_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#binding_pattern expr

    method variable_declarator_pattern ~kind (expr : ('M, 'T) Ast.Pattern.t)
        : ('N, 'U) Ast.Pattern.t =
      this#binding_pattern ~kind expr

    method catch_clause_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#binding_pattern ~kind:Ast.Variable.Let expr

    method for_in_assignment_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#assignment_pattern expr

    method for_of_assignment_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#assignment_pattern expr

    method binding_pattern ?(kind = Ast.Variable.Var) (expr : ('M, 'T) Ast.Pattern.t)
        : ('N, 'U) Ast.Pattern.t =
      this#pattern ~kind expr

    method assignment_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#pattern expr

    (* NOTE: Patterns are highly overloaded. A pattern can be a binding pattern,
       which has a kind (Var/Let/Const, with Var being the default for all pre-ES5
       bindings), or an assignment pattern, which has no kind. Subterms that are
       patterns inherit the kind (or lack thereof). *)
    method pattern ?kind (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      let open Ast.Pattern in
      let (annot, patt) = expr in
      ( this#on_type_annot annot,
        match patt with
        | Object { Object.properties; annot; comments } ->
          let properties' = List.map ~f:(this#pattern_object_p ?kind) properties in
          let annot' = this#type_annotation_hint annot in
          let comments' = this#syntax_with_internal_opt comments in
          Object { Object.properties = properties'; annot = annot'; comments = comments' }
        | Array { Array.elements; annot; comments } ->
          let elements' = List.map ~f:(this#pattern_array_e ?kind) elements in
          let annot' = this#type_annotation_hint annot in
          let comments' = this#syntax_with_internal_opt comments in
          Array { Array.elements = elements'; annot = annot'; comments = comments' }
        | Identifier { Identifier.name; annot; optional } ->
          let name' = this#pattern_identifier ?kind name in
          let annot' = this#type_annotation_hint annot in
          Identifier { Identifier.name = name'; annot = annot'; optional }
        | Expression e -> Expression (this#pattern_expression e)
      )

    method pattern_identifier ?kind (ident : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t
        =
      ignore kind;
      this#t_identifier ident

    method pattern_string_literal ?kind (expr : 'M Ast.StringLiteral.t) : 'N Ast.StringLiteral.t =
      ignore kind;
      this#string_literal expr

    method pattern_number_literal ?kind (expr : 'M Ast.NumberLiteral.t) : 'N Ast.NumberLiteral.t =
      ignore kind;
      this#number_literal expr

    method pattern_bigint_literal ?kind (expr : 'M Ast.BigIntLiteral.t) : 'N Ast.BigIntLiteral.t =
      ignore kind;
      this#bigint_literal expr

    method pattern_object_p ?kind (p : ('M, 'T) Ast.Pattern.Object.property) =
      let open Ast.Pattern.Object in
      match p with
      | Property (annot, prop) ->
        Property (this#on_loc_annot annot, this#pattern_object_property ?kind prop)
      | RestElement (annot, prop) ->
        RestElement (this#on_loc_annot annot, this#pattern_object_rest_property ?kind prop)

    method pattern_object_property ?kind (prop : ('M, 'T) Ast.Pattern.Object.Property.t')
        : ('N, 'U) Ast.Pattern.Object.Property.t' =
      let open Ast.Pattern.Object.Property in
      let { key; pattern; default; shorthand } = prop in
      let key' = this#pattern_object_property_key ?kind key in
      let pattern' = this#pattern_object_property_pattern ?kind pattern in
      let default' = this#default_opt default in
      { key = key'; pattern = pattern'; default = default'; shorthand }

    method pattern_object_property_key ?kind (key : ('M, 'T) Ast.Pattern.Object.Property.key) =
      let open Ast.Pattern.Object.Property in
      match key with
      | StringLiteral (annot, lit) ->
        StringLiteral
          (this#on_loc_annot annot, this#pattern_object_property_string_literal_key ?kind lit)
      | NumberLiteral (annot, lit) ->
        NumberLiteral
          (this#on_loc_annot annot, this#pattern_object_property_number_literal_key ?kind lit)
      | BigIntLiteral (annot, lit) ->
        BigIntLiteral
          (this#on_loc_annot annot, this#pattern_object_property_bigint_literal_key ?kind lit)
      | Identifier identifier ->
        Identifier (this#pattern_object_property_identifier_key ?kind identifier)
      | Computed expr -> Computed (this#pattern_object_property_computed_key ?kind expr)

    method pattern_object_property_string_literal_key ?kind (key : 'M Ast.StringLiteral.t)
        : 'N Ast.StringLiteral.t =
      this#pattern_string_literal ?kind key

    method pattern_object_property_number_literal_key ?kind (key : 'M Ast.NumberLiteral.t)
        : 'N Ast.NumberLiteral.t =
      this#pattern_number_literal ?kind key

    method pattern_object_property_bigint_literal_key ?kind (key : 'M Ast.BigIntLiteral.t)
        : 'N Ast.BigIntLiteral.t =
      this#pattern_bigint_literal ?kind key

    method pattern_object_property_identifier_key ?kind (key : ('M, 'T) Ast.Identifier.t)
        : ('N, 'U) Ast.Identifier.t =
      this#pattern_identifier ?kind key

    method pattern_object_property_computed_key ?kind (key : ('M, 'T) Ast.ComputedKey.t)
        : ('N, 'U) Ast.ComputedKey.t =
      ignore kind;
      this#computed_key key

    method pattern_object_rest_property ?kind (prop : ('M, 'T) Ast.Pattern.RestElement.t')
        : ('N, 'U) Ast.Pattern.RestElement.t' =
      let open Ast.Pattern.RestElement in
      let { argument; comments } = prop in
      let argument' = this#pattern_object_rest_property_pattern ?kind argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method pattern_object_property_pattern ?kind (expr : ('M, 'T) Ast.Pattern.t)
        : ('N, 'U) Ast.Pattern.t =
      this#pattern ?kind expr

    method pattern_object_rest_property_pattern ?kind (expr : ('M, 'T) Ast.Pattern.t)
        : ('N, 'U) Ast.Pattern.t =
      this#pattern ?kind expr

    method pattern_array_e ?kind (e : ('M, 'T) Ast.Pattern.Array.element)
        : ('N, 'U) Ast.Pattern.Array.element =
      let open Ast.Pattern.Array in
      match e with
      | Hole loc -> Hole (this#on_loc_annot loc)
      | Element (annot, elem) ->
        Element (this#on_loc_annot annot, this#pattern_array_element ?kind elem)
      | RestElement (annot, elem) ->
        RestElement (this#on_loc_annot annot, this#pattern_array_rest_element ?kind elem)

    method pattern_array_element ?kind (elem : ('M, 'T) Ast.Pattern.Array.Element.t')
        : ('N, 'U) Ast.Pattern.Array.Element.t' =
      let open Ast.Pattern.Array.Element in
      let { argument; default } = elem in
      let argument' = this#pattern_array_element_pattern ?kind argument in
      let default' = this#default_opt default in
      { argument = argument'; default = default' }

    method pattern_array_element_pattern ?kind (expr : ('M, 'T) Ast.Pattern.t)
        : ('N, 'U) Ast.Pattern.t =
      this#pattern ?kind expr

    method pattern_array_rest_element ?kind (elem : ('M, 'T) Ast.Pattern.RestElement.t') =
      let open Ast.Pattern.RestElement in
      let { argument; comments } = elem in
      let argument' = this#pattern_array_rest_element_pattern ?kind argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method pattern_array_rest_element_pattern ?kind (expr : ('M, 'T) Ast.Pattern.t)
        : ('N, 'U) Ast.Pattern.t =
      this#pattern ?kind expr

    method pattern_assignment_pattern ?kind (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t
        =
      this#pattern ?kind expr

    method pattern_expression (expr : ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
      this#expression expr

    method predicate_expression (expr : ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
      this#expression expr

    method return (stmt : ('M, 'T) Ast.Statement.Return.t) : ('N, 'U) Ast.Statement.Return.t =
      let open Ast.Statement.Return in
      let { argument; comments; return_out } = stmt in
      let return_out' = this#on_type_annot return_out in
      let argument' = Base.Option.map ~f:this#expression argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments'; return_out = return_out' }

    method sequence (expr : ('M, 'T) Ast.Expression.Sequence.t) : ('N, 'U) Ast.Expression.Sequence.t
        =
      let open Ast.Expression.Sequence in
      let { expressions; comments } = expr in
      let expressions' = List.map ~f:this#expression expressions in
      let comments' = this#syntax_opt comments in
      { expressions = expressions'; comments = comments' }

    method toplevel_statement_list (stmts : ('M, 'T) Ast.Statement.t list)
        : ('N, 'U) Ast.Statement.t list =
      this#statement_list stmts

    method statement_list (stmts : ('M, 'T) Ast.Statement.t list) : ('N, 'U) Ast.Statement.t list =
      List.bind ~f:this#statement_fork_point stmts

    method statement_fork_point (stmt : ('M, 'T) Ast.Statement.t) : ('N, 'U) Ast.Statement.t list =
      [this#statement stmt]

    method spread_element (expr : ('M, 'T) Ast.Expression.SpreadElement.t)
        : ('N, 'U) Ast.Expression.SpreadElement.t =
      let open Ast.Expression.SpreadElement in
      let (annot, { argument; comments }) = expr in
      let annot' = this#on_loc_annot annot in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      (annot', { argument = argument'; comments = comments' })

    method spread_property (expr : ('M, 'T) Ast.Expression.Object.SpreadProperty.t)
        : ('N, 'U) Ast.Expression.Object.SpreadProperty.t =
      let open Ast.Expression.Object.SpreadProperty in
      let (annot, { argument; comments }) = expr in
      let annot' = this#on_loc_annot annot in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      (annot', { argument = argument'; comments = comments' })

    method super_expression (expr : 'M Ast.Expression.Super.t) : 'N Ast.Expression.Super.t =
      let open Ast.Expression.Super in
      let { comments } = expr in
      let comments' = this#syntax_opt comments in
      { comments = comments' }

    method switch (switch : ('M, 'T) Ast.Statement.Switch.t) : ('N, 'U) Ast.Statement.Switch.t =
      let open Ast.Statement.Switch in
      let { discriminant; cases; comments; exhaustive_out } = switch in
      let exhaustive_out' = this#on_type_annot exhaustive_out in
      let discriminant' = this#expression discriminant in
      let cases' = List.map ~f:(this#on_loc_annot * this#switch_case) cases in
      let comments' = this#syntax_opt comments in
      {
        discriminant = discriminant';
        cases = cases';
        comments = comments';
        exhaustive_out = exhaustive_out';
      }

    method switch_case (case : ('M, 'T) Ast.Statement.Switch.Case.t') =
      let open Ast.Statement.Switch.Case in
      let { test; consequent; comments } = case in
      let test' = Option.map ~f:this#expression test in
      let consequent' = this#statement_list consequent in
      let comments' = this#syntax_opt comments in
      { test = test'; consequent = consequent'; comments = comments' }

    method tagged_template (expr : ('M, 'T) Ast.Expression.TaggedTemplate.t)
        : ('N, 'U) Ast.Expression.TaggedTemplate.t =
      let open Ast.Expression.TaggedTemplate in
      let { tag; quasi; comments } = expr in
      let tag' = this#expression tag in
      let quasi' = (this#on_loc_annot * this#template_literal) quasi in
      let comments' = this#syntax_opt comments in
      { tag = tag'; quasi = quasi'; comments = comments' }

    method template_literal (expr : ('M, 'T) Ast.Expression.TemplateLiteral.t)
        : ('N, 'U) Ast.Expression.TemplateLiteral.t =
      let open Ast.Expression.TemplateLiteral in
      let { quasis; expressions; comments } = expr in
      let quasis' = List.map ~f:this#template_literal_element quasis in
      let expressions' = List.map ~f:this#expression expressions in
      let comments' = this#syntax_opt comments in
      { quasis = quasis'; expressions = expressions'; comments = comments' }

    method template_literal_element ((annot, elem) : 'M Ast.Expression.TemplateLiteral.Element.t)
        : 'N Ast.Expression.TemplateLiteral.Element.t =
      (this#on_loc_annot annot, elem)

    method this_expression (expr : 'M Ast.Expression.This.t) : 'N Ast.Expression.This.t =
      let open Ast.Expression.This in
      let { comments } = expr in
      let comments' = this#syntax_opt comments in
      { comments = comments' }

    method throw (stmt : ('M, 'T) Ast.Statement.Throw.t) : ('N, 'U) Ast.Statement.Throw.t =
      let open Ast.Statement.Throw in
      let { argument; comments } = stmt in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; comments = comments' }

    method try_catch (stmt : ('M, 'T) Ast.Statement.Try.t) : ('N, 'U) Ast.Statement.Try.t =
      let open Ast.Statement.Try in
      let { block; handler; finalizer; comments } = stmt in
      let block' = (this#on_loc_annot * this#block) block in
      let handler' = Option.map ~f:(this#on_loc_annot * this#catch_clause) handler in
      let finalizer' = Option.map ~f:(this#on_loc_annot * this#block) finalizer in
      let comments' = this#syntax_opt comments in
      { block = block'; handler = handler'; finalizer = finalizer'; comments = comments' }

    method type_cast (expr : ('M, 'T) Ast.Expression.TypeCast.t)
        : ('N, 'U) Ast.Expression.TypeCast.t =
      let open Ast.Expression.TypeCast in
      let { expression; annot; comments } = expr in
      let expression' = this#expression expression in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      { expression = expression'; annot = annot'; comments = comments' }

    method ts_satisfies (expr : ('M, 'T) Ast.Expression.TSSatisfies.t)
        : ('N, 'U) Ast.Expression.TSSatisfies.t =
      let open Ast.Expression.TSSatisfies in
      let { expression; annot; comments } = expr in
      let expression' = this#expression expression in
      let annot' = this#type_annotation annot in
      let comments' = this#syntax_opt comments in
      { expression = expression'; annot = annot'; comments = comments' }

    method unary_expression (expr : ('M, 'T) Ast.Expression.Unary.t)
        : ('N, 'U) Ast.Expression.Unary.t =
      let open Ast.Expression.Unary in
      let { argument; operator; comments } = expr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; operator; comments = comments' }

    method update_expression (expr : ('M, 'T) Ast.Expression.Update.t)
        : ('N, 'U) Ast.Expression.Update.t =
      let open Ast.Expression.Update in
      let { argument; operator; prefix; comments } = expr in
      let argument' = this#expression argument in
      let comments' = this#syntax_opt comments in
      { argument = argument'; operator; prefix; comments = comments' }

    method variable_declaration (decl : ('M, 'T) Ast.Statement.VariableDeclaration.t)
        : ('N, 'U) Ast.Statement.VariableDeclaration.t =
      let open Ast.Statement.VariableDeclaration in
      let { declarations; kind; comments } = decl in
      let decls' = List.map ~f:(this#variable_declarator ~kind) declarations in
      let comments' = this#syntax_opt comments in
      { declarations = decls'; kind; comments = comments' }

    method variable_declarator
        ~kind (decl : ('M, 'T) Ast.Statement.VariableDeclaration.Declarator.t)
        : ('N, 'U) Ast.Statement.VariableDeclaration.Declarator.t =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (annot, { id; init }) = decl in
      let id' = this#variable_declarator_pattern ~kind id in
      let init' = Option.map ~f:this#expression init in
      (this#on_loc_annot annot, { id = id'; init = init' })

    method variance (variance : 'M Ast.Variance.t) : 'N Ast.Variance.t =
      let open Ast.Variance in
      let (annot, { kind; comments }) = variance in
      let annot' = this#on_loc_annot annot in
      let comments' = this#syntax_opt comments in
      (annot', { kind; comments = comments' })

    method variance_opt (opt : 'M Ast.Variance.t option) : 'N Ast.Variance.t option =
      Option.map ~f:this#variance opt

    method tparam_const_modifier (c : 'M Ast.Type.TypeParam.ConstModifier.t)
        : 'N Ast.Type.TypeParam.ConstModifier.t =
      let (loc, comments) = c in
      let loc' = this#on_loc_annot loc in
      let comments' = this#syntax_opt comments in
      (loc', comments')

    method while_ (stuff : ('M, 'T) Ast.Statement.While.t) : ('N, 'U) Ast.Statement.While.t =
      let open Ast.Statement.While in
      let { test; body; comments } = stuff in
      let test' = this#predicate_expression test in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      { test = test'; body = body'; comments = comments' }

    method with_ (stuff : ('M, 'T) Ast.Statement.With.t) : ('N, 'U) Ast.Statement.With.t =
      let open Ast.Statement.With in
      let { _object; body; comments } = stuff in
      let _object' = this#expression _object in
      let body' = this#statement body in
      let comments' = this#syntax_opt comments in
      { _object = _object'; body = body'; comments = comments' }

    method type_alias _annot (stuff : ('M, 'T) Ast.Statement.TypeAlias.t)
        : ('N, 'U) Ast.Statement.TypeAlias.t =
      let open Ast.Statement.TypeAlias in
      let { id; tparams; right; comments } = stuff in
      let id' = this#binding_type_identifier id in
      this#type_params_opt tparams (fun tparams' ->
          let right' = this#type_ right in
          let comments' = this#syntax_opt comments in
          { id = id'; tparams = tparams'; right = right'; comments = comments' }
      )

    method type_alias_identifier (id : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#t_identifier id

    method yield (expr : ('M, 'T) Ast.Expression.Yield.t) : ('N, 'U) Ast.Expression.Yield.t =
      let open Ast.Expression.Yield in
      let { argument; delegate; comments; result_out } = expr in
      let argument' = Base.Option.map ~f:this#expression argument in
      let comments' = this#syntax_opt comments in
      let result_out' = this#on_type_annot result_out in
      { argument = argument'; delegate; comments = comments'; result_out = result_out' }
  end
