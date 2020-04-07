(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let ( * ) : 'a 'b 'c 'd. ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd =
 (fun f g (x, y) -> (f x, g y))

class virtual ['M, 'T, 'N, 'U] mapper =
  object (this)
    method virtual on_loc_annot : 'M -> 'N

    method virtual on_type_annot : 'T -> 'U

    method program (program : ('M, 'T) Ast.program) : ('N, 'U) Ast.program =
      let (annot, statements, comments) = program in
      let annot' = this#on_loc_annot annot in
      let statements' = this#toplevel_statement_list statements in
      let comments' = Base.List.map ~f:this#comment comments in
      (annot', statements', comments')

    method statement ((annot, stmt) : ('M, 'T) Ast.Statement.t) : ('N, 'U) Ast.Statement.t =
      Ast.Statement.
        ( this#on_loc_annot annot,
          match stmt with
          | Block block -> Block (this#block block)
          | Break break -> Break (this#break break)
          | ClassDeclaration cls -> ClassDeclaration (this#class_ cls)
          | Continue cont -> Continue (this#continue cont)
          | Debugger dbg -> Debugger (this#debugger dbg)
          | DeclareClass stuff -> DeclareClass (this#declare_class stuff)
          | DeclareExportDeclaration decl ->
            DeclareExportDeclaration (this#declare_export_declaration annot decl)
          | DeclareFunction stuff -> DeclareFunction (this#declare_function stuff)
          | DeclareInterface stuff -> DeclareInterface (this#declare_interface stuff)
          | DeclareModule m -> DeclareModule (this#declare_module annot m)
          | DeclareTypeAlias stuff -> DeclareTypeAlias (this#declare_type_alias stuff)
          | DeclareVariable stuff -> DeclareVariable (this#declare_variable stuff)
          | DeclareModuleExports t_annot ->
            DeclareModuleExports (this#declare_module_exports annot t_annot)
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
          | InterfaceDeclaration stuff -> InterfaceDeclaration (this#interface_declaration stuff)
          | Labeled label -> Labeled (this#labeled_statement label)
          | OpaqueType otype -> OpaqueType (this#opaque_type otype)
          | Return ret -> Return (this#return ret)
          | Switch switch -> Switch (this#switch switch)
          | Throw throw -> Throw (this#throw throw)
          | Try try_stmt -> Try (this#try_catch try_stmt)
          | VariableDeclaration decl -> VariableDeclaration (this#variable_declaration decl)
          | While stuff -> While (this#while_ stuff)
          | With stuff -> With (this#with_ stuff)
          | TypeAlias stuff -> TypeAlias (this#type_alias stuff)
          | DeclareOpaqueType otype -> DeclareOpaqueType (this#declare_opaque_type otype) )

    method comment ((annot, c) : 'M Ast.Comment.t) : 'N Ast.Comment.t = (this#on_loc_annot annot, c)

    method t_comment ((annot, c) : 'T Ast.Comment.t) : 'U Ast.Comment.t =
      (this#on_type_annot annot, c)

    method syntax : 'internal. ('M, 'internal) Ast.Syntax.t -> ('N, 'internal) Ast.Syntax.t =
      fun attached ->
        let open Ast.Syntax in
        let { leading; trailing; internal } = attached in
        let leading' = List.map this#comment leading in
        let trailing' = List.map this#comment trailing in
        { leading = leading'; trailing = trailing'; internal }

    method expression ((annot, expr') : ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
      Ast.Expression.
        ( this#on_type_annot annot,
          match expr' with
          | Array x -> Array (this#array x)
          | ArrowFunction x -> ArrowFunction (this#arrow_function x)
          | Assignment x -> Assignment (this#assignment x)
          | Binary x -> Binary (this#binary x)
          | Call x -> Call (this#call annot x)
          | Class x -> Class (this#class_ x)
          | Comprehension x -> Comprehension (this#comprehension x)
          | Conditional x -> Conditional (this#conditional x)
          | Function x -> Function (this#function_expression x)
          | Generator x -> Generator (this#generator x)
          | Identifier x -> Identifier (this#t_identifier x)
          | Import x -> Import (this#import annot x)
          | JSXElement x -> JSXElement (this#jsx_element x)
          | JSXFragment x -> JSXFragment (this#jsx_fragment x)
          | Literal x -> Literal (this#literal x)
          | Logical x -> Logical (this#logical x)
          | Member x -> Member (this#member x)
          | MetaProperty x -> MetaProperty (this#meta_property x)
          | New x -> New (this#new_ x)
          | Object x -> Object (this#object_ x)
          | OptionalCall x -> OptionalCall (this#optional_call annot x)
          | OptionalMember x -> OptionalMember (this#optional_member x)
          | Sequence x -> Sequence (this#sequence x)
          | Super x -> Super (this#super_expression x)
          | TaggedTemplate x -> TaggedTemplate (this#tagged_template x)
          | TemplateLiteral x -> TemplateLiteral (this#template_literal x)
          | This x -> This (this#this_expression x)
          | TypeCast x -> TypeCast (this#type_cast x)
          | Unary x -> Unary (this#unary_expression x)
          | Update x -> Update (this#update_expression x)
          | Yield x -> Yield (this#yield x) )

    method arg_list ((annot, args) : ('M, 'T) Ast.Expression.ArgList.t)
        : ('N, 'U) Ast.Expression.ArgList.t =
      (this#on_loc_annot annot, Base.List.map ~f:this#expression_or_spread args)

    method array (expr : ('M, 'T) Ast.Expression.Array.t) : ('N, 'U) Ast.Expression.Array.t =
      let open Ast.Expression in
      let { Array.elements; Array.comments } = expr in
      let elements' = Base.List.map ~f:(Base.Option.map ~f:this#expression_or_spread) elements in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { Array.elements = elements'; comments = comments' }

    method arrow_function (expr : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
      this#function_ expr

    method assignment (expr : ('M, 'T) Ast.Expression.Assignment.t)
        : ('N, 'U) Ast.Expression.Assignment.t =
      let open Ast.Expression.Assignment in
      let { operator; left; right; comments } = expr in
      let left' = this#assignment_pattern left in
      let right' = this#expression right in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { operator; left = left'; right = right'; comments = comments' }

    method binary (expr : ('M, 'T) Ast.Expression.Binary.t) : ('N, 'U) Ast.Expression.Binary.t =
      let open Ast.Expression.Binary in
      let { operator; left; right; comments } = expr in
      let left' = this#expression left in
      let right' = this#expression right in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { operator; left = left'; right = right'; comments = comments' }

    method block (stmt : ('M, 'T) Ast.Statement.Block.t) : ('N, 'U) Ast.Statement.Block.t =
      let open Ast.Statement.Block in
      let { body; comments } = stmt in
      let body' = this#statement_list body in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { body = body'; comments = comments' }

    method break (break : 'M Ast.Statement.Break.t) : 'N Ast.Statement.Break.t =
      let open Ast.Statement.Break in
      let { label; comments } = break in
      let label' = Base.Option.map ~f:this#label_identifier label in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { label = label'; comments = comments' }

    method call _annot (expr : ('M, 'T) Ast.Expression.Call.t) : ('N, 'U) Ast.Expression.Call.t =
      let open Ast.Expression.Call in
      let { callee; targs; arguments; comments } = expr in
      let callee' = this#expression callee in
      let targs' = Base.Option.map ~f:this#call_type_args targs in
      let arguments' = this#arg_list arguments in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { callee = callee'; targs = targs'; arguments = arguments'; comments = comments' }

    method optional_call annot (expr : ('M, 'T) Ast.Expression.OptionalCall.t)
        : ('N, 'U) Ast.Expression.OptionalCall.t =
      let open Ast.Expression.OptionalCall in
      let { call; optional } = expr in
      let call' = this#call annot call in
      { call = call'; optional }

    method call_type_args (pi : ('M, 'T) Ast.Expression.CallTypeArgs.t)
        : ('N, 'U) Ast.Expression.CallTypeArgs.t =
      let (annot, targs) = pi in
      let targs' = Base.List.map ~f:this#call_type_arg targs in
      (this#on_loc_annot annot, targs')

    method call_type_arg (x : ('M, 'T) Ast.Expression.CallTypeArg.t)
        : ('N, 'U) Ast.Expression.CallTypeArg.t =
      let open Ast.Expression.CallTypeArg in
      match x with
      | Explicit t -> Explicit (this#type_ t)
      | Implicit t -> Implicit (this#implicit t)

    method catch_clause (clause : ('M, 'T) Ast.Statement.Try.CatchClause.t')
        : ('N, 'U) Ast.Statement.Try.CatchClause.t' =
      let open Ast.Statement.Try.CatchClause in
      let { param; body; comments } = clause in
      let param' = Base.Option.map ~f:this#catch_clause_pattern param in
      let body' = (this#on_loc_annot * this#block) body in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { param = param'; body = body'; comments = comments' }

    method class_ (cls : ('M, 'T) Ast.Class.t) : ('N, 'U) Ast.Class.t =
      let open Ast.Class in
      let { id; body; tparams; extends; implements; classDecorators; comments } = cls in
      let id' = Base.Option.map ~f:this#class_identifier id in
      let comments' = Base.Option.map ~f:this#syntax comments in
      this#type_params_opt tparams (fun tparams' ->
          let extends' = Base.Option.map ~f:this#class_extends extends in
          let body' = this#class_body body in
          let implements' = Base.Option.map ~f:this#class_implements implements in
          let classDecorators' = Base.List.map ~f:this#class_decorator classDecorators in
          {
            id = id';
            body = body';
            tparams = tparams';
            extends = extends';
            implements = implements';
            classDecorators = classDecorators';
            comments = comments';
          })

    method class_extends (extends : ('M, 'T) Ast.Class.Extends.t) : ('N, 'U) Ast.Class.Extends.t =
      let open Ast.Class.Extends in
      let (annot, { expr; targs; comments }) = extends in
      let annot' = this#on_loc_annot annot in
      let expr' = this#expression expr in
      let targs' = Base.Option.map ~f:this#type_args targs in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { expr = expr'; targs = targs'; comments = comments' })

    method class_decorator (dec : ('M, 'T) Ast.Class.Decorator.t) : ('N, 'U) Ast.Class.Decorator.t =
      let open Ast.Class.Decorator in
      let (annot, { expression; comments }) = dec in
      let annot' = this#on_loc_annot annot in
      let expression' = this#expression expression in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { expression = expression'; comments = comments' })

    method class_identifier (ident : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#t_pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let ident

    method class_body (cls_body : ('M, 'T) Ast.Class.Body.t) : ('N, 'U) Ast.Class.Body.t =
      let open Ast.Class.Body in
      let (annot, { body; comments }) = cls_body in
      let annot' = this#on_loc_annot annot in
      let body' = Base.List.map ~f:this#class_element body in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { body = body'; comments = comments' })

    method class_element (elem : ('M, 'T) Ast.Class.Body.element) : ('N, 'U) Ast.Class.Body.element
        =
      let open Ast.Class.Body in
      match elem with
      | Method (annot, meth) -> Method (this#on_type_annot annot, this#class_method meth)
      | Property (annot, prop) -> Property (this#on_type_annot annot, this#class_property prop)
      | PrivateField (annot, field) ->
        PrivateField (this#on_type_annot annot, this#class_private_field field)

    method class_method (meth : ('M, 'T) Ast.Class.Method.t') : ('N, 'U) Ast.Class.Method.t' =
      let open Ast.Class.Method in
      let { kind; key; value; static; decorators; comments } = meth in
      let key' = this#object_key key in
      let value' = (this#on_loc_annot * this#function_expression) value in
      let decorators' = Base.List.map ~f:this#class_decorator decorators in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { kind; key = key'; value = value'; static; decorators = decorators'; comments = comments' }

    method class_property (prop : ('M, 'T) Ast.Class.Property.t') : ('N, 'U) Ast.Class.Property.t' =
      let open Ast.Class.Property in
      let { key; value; annot; static; variance; comments } = prop in
      let key' = this#object_key key in
      let value' = this#class_property_value value in
      let annot' = this#type_annotation_hint annot in
      let variance' = Base.Option.map ~f:this#variance variance in
      let comments' = Base.Option.map ~f:this#syntax comments in
      {
        key = key';
        value = value';
        annot = annot';
        static;
        variance = variance';
        comments = comments';
      }

    method class_property_value (value : ('M, 'T) Ast.Class.Property.value)
        : ('N, 'U) Ast.Class.Property.value =
      let open Ast.Class.Property in
      match value with
      | Declared -> Declared
      | Uninitialized -> Uninitialized
      | Initialized expr -> Initialized (this#expression expr)

    method class_private_field (prop : ('M, 'T) Ast.Class.PrivateField.t') =
      let open Ast.Class.PrivateField in
      let { key; value; annot; static; variance; comments } = prop in
      let key' = this#private_name key in
      let value' = this#class_property_value value in
      let annot' = this#type_annotation_hint annot in
      let variance' = Base.Option.map ~f:this#variance variance in
      let comments' = Base.Option.map ~f:this#syntax comments in
      {
        key = key';
        value = value';
        annot = annot';
        static;
        variance = variance';
        comments = comments';
      }

    method comprehension (expr : ('M, 'T) Ast.Expression.Comprehension.t)
        : ('N, 'U) Ast.Expression.Comprehension.t =
      let open Ast.Expression.Comprehension in
      let { blocks; filter } = expr in
      let blocks' = Base.List.map ~f:this#comprehension_block blocks in
      let filter' = Base.Option.map ~f:this#expression filter in
      { blocks = blocks'; filter = filter' }

    method comprehension_block (block : ('M, 'T) Ast.Expression.Comprehension.Block.t)
        : ('N, 'U) Ast.Expression.Comprehension.Block.t =
      let open Ast.Expression.Comprehension.Block in
      let (annot, { left; right; each }) = block in
      let left' = this#pattern left in
      let right' = this#expression right in
      (this#on_loc_annot annot, { left = left'; right = right'; each })

    method conditional (expr : ('M, 'T) Ast.Expression.Conditional.t)
        : ('N, 'U) Ast.Expression.Conditional.t =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate; comments } = expr in
      let test' = this#predicate_expression test in
      let consequent' = this#expression consequent in
      let alternate' = this#expression alternate in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { test = test'; consequent = consequent'; alternate = alternate'; comments = comments' }

    method continue (cont : 'M Ast.Statement.Continue.t) : 'N Ast.Statement.Continue.t =
      let open Ast.Statement.Continue in
      let { label; comments } = cont in
      let label' = Base.Option.map ~f:this#label_identifier label in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { label = label'; comments = comments' }

    method debugger (dbg : 'M Ast.Statement.Debugger.t) : 'N Ast.Statement.Debugger.t =
      let open Ast.Statement.Debugger in
      let { comments } = dbg in
      let comments' = Base.Option.map ~f:this#syntax comments in
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
          let extends' = Base.Option.map ~f:(this#on_loc_annot * this#generic_type) extends in
          let mixins' = Base.List.map ~f:(this#on_loc_annot * this#generic_type) mixins in
          let implements' = Base.Option.map ~f:this#class_implements implements in
          let comments' = Base.Option.map ~f:this#syntax comments in
          {
            id = id';
            tparams = tparams';
            body = body';
            extends = extends';
            mixins = mixins';
            implements = implements';
            comments = comments';
          })

    method class_implements (implements : ('M, 'T) Ast.Class.Implements.t)
        : ('N, 'U) Ast.Class.Implements.t =
      let open Ast.Class.Implements in
      let (annot, { interfaces; comments }) = implements in
      let annot' = this#on_loc_annot annot in
      let interfaces' = Base.List.map ~f:this#class_implements_interface interfaces in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { interfaces = interfaces'; comments = comments' })

    method class_implements_interface (interface : ('M, 'T) Ast.Class.Implements.Interface.t)
        : ('N, 'U) Ast.Class.Implements.Interface.t =
      let open Ast.Class.Implements.Interface in
      let (annot, { id = id_; targs }) = interface in
      let annot' = this#on_loc_annot annot in
      let id' = this#t_identifier id_ in
      let targs' = Base.Option.map ~f:this#type_args targs in
      (annot', { id = id'; targs = targs' })

    method declare_export_declaration
        _annot (decl : ('M, 'T) Ast.Statement.DeclareExportDeclaration.t)
        : ('N, 'U) Ast.Statement.DeclareExportDeclaration.t =
      let open Ast.Statement.DeclareExportDeclaration in
      let { default; source; specifiers; declaration } = decl in
      let default' = Base.Option.map ~f:this#on_loc_annot default in
      let source' = Base.Option.map ~f:(this#on_loc_annot * this#string_literal) source in
      let specifiers' = Base.Option.map ~f:this#export_named_specifier specifiers in
      let declaration' = Base.Option.map ~f:this#declare_export_declaration_decl declaration in
      { default = default'; source = source'; specifiers = specifiers'; declaration = declaration' }

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
      | DefaultType t -> DefaultType (this#type_ t)
      | NamedType (annot, alias) -> NamedType (this#on_loc_annot annot, this#type_alias alias)
      | NamedOpaqueType (annot, ot) -> NamedOpaqueType (this#on_loc_annot annot, this#opaque_type ot)
      | Interface (annot, iface) -> Interface (this#on_loc_annot annot, this#interface iface)

    method declare_function (decl : ('M, 'T) Ast.Statement.DeclareFunction.t)
        : ('N, 'U) Ast.Statement.DeclareFunction.t =
      let open Ast.Statement.DeclareFunction in
      let { id = ident; annot; predicate; comments } = decl in
      let id' = this#t_function_identifier ident in
      let annot' = this#type_annotation annot in
      let predicate' = Base.Option.map ~f:this#type_predicate predicate in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { id = id'; annot = annot'; predicate = predicate'; comments = comments' }

    method declare_interface (decl : ('M, 'T) Ast.Statement.Interface.t)
        : ('N, 'U) Ast.Statement.Interface.t =
      this#interface decl

    method declare_module _annot (m : ('M, 'T) Ast.Statement.DeclareModule.t)
        : ('N, 'U) Ast.Statement.DeclareModule.t =
      let open Ast.Statement.DeclareModule in
      let { id; body; kind; comments } = m in
      let id' =
        match id with
        | Identifier id -> Identifier (this#t_identifier id)
        | Literal (annot, name) -> Literal (this#on_type_annot annot, this#string_literal name)
      in
      let kind' =
        match kind with
        | CommonJS annot -> CommonJS (this#on_loc_annot annot)
        | ES annot -> ES (this#on_loc_annot annot)
      in
      let body' = (this#on_loc_annot * this#block) body in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { id = id'; body = body'; kind = kind'; comments = comments' }

    method declare_module_exports _annot (t_annot : ('M, 'T) Ast.Type.annotation)
        : ('N, 'U) Ast.Type.annotation =
      this#type_annotation t_annot

    method declare_type_alias (decl : ('M, 'T) Ast.Statement.TypeAlias.t)
        : ('N, 'U) Ast.Statement.TypeAlias.t =
      this#type_alias decl

    method declare_variable (decl : ('M, 'T) Ast.Statement.DeclareVariable.t)
        : ('N, 'U) Ast.Statement.DeclareVariable.t =
      let open Ast.Statement.DeclareVariable in
      let { id = ident; annot; comments } = decl in
      let id' = this#t_pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Var ident in
      let annot' = this#type_annotation_hint annot in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { id = id'; annot = annot'; comments = comments' }

    method do_while (stuff : ('M, 'T) Ast.Statement.DoWhile.t) : ('N, 'U) Ast.Statement.DoWhile.t =
      let open Ast.Statement.DoWhile in
      let { body; test; comments } = stuff in
      let body' = this#statement body in
      let test' = this#predicate_expression test in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { body = body'; test = test'; comments = comments' }

    method empty (empty : 'M Ast.Statement.Empty.t) : 'N Ast.Statement.Empty.t =
      let open Ast.Statement.Empty in
      let { comments } = empty in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { comments = comments' }

    method enum_declaration (enum : ('M, 'T) Ast.Statement.EnumDeclaration.t)
        : ('N, 'U) Ast.Statement.EnumDeclaration.t =
      let open Ast.Statement.EnumDeclaration in
      let { id; body } = enum in
      let body' =
        match body with
        | (annot, BooleanBody boolean_body) ->
          (this#on_loc_annot annot, BooleanBody (this#enum_boolean_body boolean_body))
        | (annot, NumberBody number_body) ->
          (this#on_loc_annot annot, NumberBody (this#enum_number_body number_body))
        | (annot, StringBody string_body) ->
          (this#on_loc_annot annot, StringBody (this#enum_string_body string_body))
        | (annot, SymbolBody symbol_body) ->
          (this#on_loc_annot annot, SymbolBody (this#enum_symbol_body symbol_body))
      in
      { id = this#t_identifier id; body = body' }

    method enum_boolean_body (body : 'M Ast.Statement.EnumDeclaration.BooleanBody.t)
        : 'N Ast.Statement.EnumDeclaration.BooleanBody.t =
      let open Ast.Statement.EnumDeclaration.BooleanBody in
      let { members; explicitType } = body in
      { members = Base.List.map ~f:this#enum_boolean_member members; explicitType }

    method enum_number_body (body : 'M Ast.Statement.EnumDeclaration.NumberBody.t)
        : 'N Ast.Statement.EnumDeclaration.NumberBody.t =
      let open Ast.Statement.EnumDeclaration.NumberBody in
      let { members; explicitType } = body in
      { members = Base.List.map ~f:this#enum_number_member members; explicitType }

    method enum_string_body (body : 'M Ast.Statement.EnumDeclaration.StringBody.t)
        : 'N Ast.Statement.EnumDeclaration.StringBody.t =
      let open Ast.Statement.EnumDeclaration.StringBody in
      let { members; explicitType } = body in
      let members' =
        match members with
        | Defaulted members -> Defaulted (Base.List.map ~f:this#enum_defaulted_member members)
        | Initialized members -> Initialized (Base.List.map ~f:this#enum_string_member members)
      in
      { members = members'; explicitType }

    method enum_symbol_body (body : 'M Ast.Statement.EnumDeclaration.SymbolBody.t)
        : 'N Ast.Statement.EnumDeclaration.SymbolBody.t =
      let open Ast.Statement.EnumDeclaration.SymbolBody in
      let { members } = body in
      { members = Base.List.map ~f:this#enum_defaulted_member members }

    method enum_defaulted_member (member : 'M Ast.Statement.EnumDeclaration.DefaultedMember.t)
        : 'N Ast.Statement.EnumDeclaration.DefaultedMember.t =
      let open Ast.Statement.EnumDeclaration.DefaultedMember in
      let (annot, { id }) = member in
      (this#on_loc_annot annot, { id = this#identifier id })

    method enum_boolean_member
        (member : ('M Ast.BooleanLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t)
        : ('N Ast.BooleanLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (annot, { id; init = (init_annot, init_val) }) = member in
      let init' = (this#on_loc_annot init_annot, this#boolean_literal init_val) in
      (this#on_loc_annot annot, { id = this#identifier id; init = init' })

    method enum_number_member
        (member : ('M Ast.NumberLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t)
        : ('N Ast.NumberLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (annot, { id; init = (init_annot, init_val) }) = member in
      let init' = (this#on_loc_annot init_annot, this#number_literal init_val) in
      (this#on_loc_annot annot, { id = this#identifier id; init = init' })

    method enum_string_member
        (member : ('M Ast.StringLiteral.t, 'M) Ast.Statement.EnumDeclaration.InitializedMember.t)
        : ('N Ast.StringLiteral.t, 'N) Ast.Statement.EnumDeclaration.InitializedMember.t =
      let open Ast.Statement.EnumDeclaration.InitializedMember in
      let (annot, { id; init = (init_annot, init_val) }) = member in
      let init' = (this#on_loc_annot init_annot, this#string_literal init_val) in
      (this#on_loc_annot annot, { id = this#identifier id; init = init' })

    method export_default_declaration
        _loc (decl : ('M, 'T) Ast.Statement.ExportDefaultDeclaration.t)
        : ('N, 'U) Ast.Statement.ExportDefaultDeclaration.t =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default; declaration; comments } = decl in
      let default' = this#on_loc_annot default in
      let declaration' = this#export_default_declaration_decl declaration in
      let comments' = Base.Option.map ~f:this#syntax comments in
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
      let { exportKind; source; specifiers; declaration; comments } = decl in
      let source' = Base.Option.map ~f:(this#on_loc_annot * this#string_literal) source in
      let specifiers' = Base.Option.map ~f:this#export_named_specifier specifiers in
      let declaration' = Base.Option.map ~f:this#statement declaration in
      let comments' = Base.Option.map ~f:this#syntax comments in
      {
        exportKind;
        source = source';
        specifiers = specifiers';
        declaration = declaration';
        comments = comments';
      }

    method export_named_specifier (spec : 'M Ast.Statement.ExportNamedDeclaration.specifier)
        : 'N Ast.Statement.ExportNamedDeclaration.specifier =
      let open Ast.Statement.ExportNamedDeclaration in
      match spec with
      | ExportSpecifiers specs -> ExportSpecifiers (Base.List.map ~f:this#export_specifier specs)
      | ExportBatchSpecifier (annot, name) ->
        let annot' = this#on_loc_annot annot in
        let name' = Base.Option.map ~f:this#identifier name in
        ExportBatchSpecifier (annot', name')

    method export_specifier (spec : 'M Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t)
        : 'N Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t =
      let open Ast.Statement.ExportNamedDeclaration.ExportSpecifier in
      let (annot, { local; exported }) = spec in
      let local' = this#identifier local in
      let exported' = Base.Option.map ~f:this#identifier exported in
      (this#on_loc_annot annot, { local = local'; exported = exported' })

    method expression_statement (stmt : ('M, 'T) Ast.Statement.Expression.t)
        : ('N, 'U) Ast.Statement.Expression.t =
      let open Ast.Statement.Expression in
      let { expression = expr; directive; comments } = stmt in
      let expression' = this#expression expr in
      let comments' = Base.Option.map ~f:this#syntax comments in
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
      let { left; right; body; each } = stmt in
      let left' = this#for_in_statement_lhs left in
      let right' = this#expression right in
      let body' = this#statement body in
      { left = left'; right = right'; body = body'; each }

    method for_in_statement_lhs (left : ('M, 'T) Ast.Statement.ForIn.left)
        : ('N, 'U) Ast.Statement.ForIn.left =
      let open Ast.Statement.ForIn in
      match left with
      | LeftDeclaration (annot, decl) ->
        LeftDeclaration (this#on_loc_annot annot, this#variable_declaration decl)
      | LeftPattern patt -> LeftPattern (this#for_in_assignment_pattern patt)

    method for_of_statement (stuff : ('M, 'T) Ast.Statement.ForOf.t)
        : ('N, 'U) Ast.Statement.ForOf.t =
      let open Ast.Statement.ForOf in
      let { left; right; body; await } = stuff in
      let left' = this#for_of_statement_lhs left in
      let right' = this#expression right in
      let body' = this#statement body in
      { left = left'; right = right'; body = body'; await }

    method for_of_statement_lhs (left : ('M, 'T) Ast.Statement.ForOf.left) =
      let open Ast.Statement.ForOf in
      match left with
      | LeftDeclaration (annot, decl) ->
        LeftDeclaration (this#on_loc_annot annot, this#variable_declaration decl)
      | LeftPattern patt -> LeftPattern (this#for_of_assignment_pattern patt)

    method for_statement (stmt : ('M, 'T) Ast.Statement.For.t) : ('N, 'U) Ast.Statement.For.t =
      let open Ast.Statement.For in
      let { init; test; update; body } = stmt in
      let init' = Base.Option.map ~f:this#for_statement_init init in
      let test' = Base.Option.map ~f:this#predicate_expression test in
      let update' = Base.Option.map ~f:this#expression update in
      let body' = this#statement body in
      { init = init'; test = test'; update = update'; body = body' }

    method for_statement_init (init : ('M, 'T) Ast.Statement.For.init)
        : ('N, 'U) Ast.Statement.For.init =
      let open Ast.Statement.For in
      match init with
      | InitDeclaration (annot, decl) ->
        InitDeclaration (this#on_loc_annot annot, this#variable_declaration decl)
      | InitExpression expr -> InitExpression (this#expression expr)

    method function_param_type (fpt : ('M, 'T) Ast.Type.Function.Param.t)
        : ('N, 'U) Ast.Type.Function.Param.t =
      let open Ast.Type.Function.Param in
      let (annot, { annot = t_annot; name; optional }) = fpt in
      let t_annot' = this#type_ t_annot in
      let name' = Base.Option.map ~f:this#t_identifier name in
      (this#on_loc_annot annot, { annot = t_annot'; name = name'; optional })

    method function_rest_param_type (frpt : ('M, 'T) Ast.Type.Function.RestParam.t)
        : ('N, 'U) Ast.Type.Function.RestParam.t =
      let open Ast.Type.Function.RestParam in
      let (annot, { argument; comments }) = frpt in
      let annot' = this#on_loc_annot annot in
      let argument' = this#function_param_type argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { argument = argument'; comments = comments' })

    method function_type (ft : ('M, 'T) Ast.Type.Function.t) : ('N, 'U) Ast.Type.Function.t =
      let open Ast.Type.Function in
      let {
        params = (params_annot, { Params.params = ps; rest = rpo; comments = params_comments });
        return;
        tparams;
        comments = func_comments;
      } =
        ft
      in
      this#type_params_opt tparams (fun tparams' ->
          let ps' = Base.List.map ~f:this#function_param_type ps in
          let rpo' = Base.Option.map ~f:this#function_rest_param_type rpo in
          let return' = this#type_ return in
          let func_comments' = Base.Option.map ~f:this#syntax func_comments in
          let params_comments' = Base.Option.map ~f:this#syntax params_comments in
          {
            params =
              ( this#on_loc_annot params_annot,
                { Params.params = ps'; rest = rpo'; comments = params_comments' } );
            return = return';
            tparams = tparams';
            comments = func_comments';
          })

    method label_identifier (ident : ('M, 'M) Ast.Identifier.t) : ('N, 'N) Ast.Identifier.t =
      this#identifier ident

    method object_property_value_type (opvt : ('M, 'T) Ast.Type.Object.Property.value)
        : ('N, 'U) Ast.Type.Object.Property.value =
      let open Ast.Type.Object.Property in
      match opvt with
      | Init t -> Init (this#type_ t)
      | Get (annot, ft) -> Get (this#on_loc_annot annot, this#function_type ft)
      | Set (annot, ft) -> Set (this#on_loc_annot annot, this#function_type ft)

    method object_property_type (opt : ('M, 'T) Ast.Type.Object.Property.t)
        : ('N, 'U) Ast.Type.Object.Property.t =
      let open Ast.Type.Object.Property in
      let (annot, { key; value; optional; static; proto; _method; variance; comments }) = opt in
      let key' = this#object_key key in
      let value' = this#object_property_value_type value in
      let variance' = Base.Option.map ~f:this#variance variance in
      let comments' = Base.Option.map ~f:this#syntax comments in
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
        } )

    method object_indexer_type (oit : ('M, 'T) Ast.Type.Object.Indexer.t)
        : ('N, 'U) Ast.Type.Object.Indexer.t =
      let open Ast.Type.Object.Indexer in
      let (annot, { id = id_; key; value; static; variance; comments }) = oit in
      let id' = Base.Option.map ~f:this#identifier id_ in
      let key' = this#type_ key in
      let value' = this#type_ value in
      let variance' = Base.Option.map ~f:this#variance variance in
      let comments' = Base.Option.map ~f:this#syntax comments in
      ( this#on_loc_annot annot,
        { id = id'; key = key'; value = value'; static; variance = variance'; comments = comments' }
      )

    method object_internal_slot_type (islot : ('M, 'T) Ast.Type.Object.InternalSlot.t)
        : ('N, 'U) Ast.Type.Object.InternalSlot.t =
      let open Ast.Type.Object.InternalSlot in
      let (annot, { id = id_; value; optional; static; _method; comments }) = islot in
      let id' = this#identifier id_ in
      let value' = this#type_ value in
      let comments' = Base.Option.map ~f:this#syntax comments in
      ( this#on_loc_annot annot,
        { id = id'; value = value'; optional; static; _method; comments = comments' } )

    method object_type (ot : ('M, 'T) Ast.Type.Object.t) : ('N, 'U) Ast.Type.Object.t =
      let open Ast.Type.Object in
      let { properties; exact; inexact; comments } = ot in
      let properties' = Base.List.map ~f:this#object_type_property properties in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { properties = properties'; exact; inexact; comments = comments' }

    method object_type_property (prop : ('M, 'T) Ast.Type.Object.property)
        : ('N, 'U) Ast.Type.Object.property =
      let open Ast.Type.Object in
      match prop with
      | Property prop -> Property (this#object_property_type prop)
      | SpreadProperty (annot, { SpreadProperty.argument; comments }) ->
        let annot' = this#on_loc_annot annot in
        let argument' = this#type_ argument in
        let comments' = Base.Option.map ~f:this#syntax comments in
        SpreadProperty (annot', { SpreadProperty.argument = argument'; comments = comments' })
      | Indexer indexer -> Indexer (this#object_indexer_type indexer)
      | CallProperty (annot, { CallProperty.value; static; comments }) ->
        let value' = (this#on_loc_annot * this#function_type) value in
        let comments' = Base.Option.map ~f:this#syntax comments in
        CallProperty
          (this#on_loc_annot annot, { CallProperty.value = value'; static; comments = comments' })
      | InternalSlot islot -> InternalSlot (this#object_internal_slot_type islot)

    method interface_type (i : ('M, 'T) Ast.Type.Interface.t) : ('N, 'U) Ast.Type.Interface.t =
      let open Ast.Type.Interface in
      let { extends; body; comments } = i in
      let extends' = Base.List.map ~f:(this#on_loc_annot * this#generic_type) extends in
      let body' = (this#on_loc_annot * this#object_type) body in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { extends = extends'; body = body'; comments = comments' }

    method generic_identifier_type (git : ('M, 'T) Ast.Type.Generic.Identifier.t)
        : ('N, 'U) Ast.Type.Generic.Identifier.t =
      let open Ast.Type.Generic.Identifier in
      match git with
      | Unqualified i -> Unqualified (this#t_identifier i)
      | Qualified (annot, { qualification; id = id_ }) ->
        let qualification' = this#generic_identifier_type qualification in
        let id' = this#t_identifier id_ in
        Qualified (this#on_loc_annot annot, { qualification = qualification'; id = id' })

    method type_args (targs : ('M, 'T) Ast.Type.TypeArgs.t) : ('N, 'U) Ast.Type.TypeArgs.t =
      let open Ast.Type.TypeArgs in
      let (annot, { arguments; comments }) = targs in
      let annot' = this#on_loc_annot annot in
      let arguments' = Base.List.map ~f:this#type_ arguments in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { arguments = arguments'; comments = comments' })

    method type_params_opt
        : 'a. ('M, 'T) Ast.Type.TypeParams.t option ->
          (('N, 'U) Ast.Type.TypeParams.t option -> 'a) -> 'a =
      fun tparams f ->
        let tparams' =
          Base.Option.map
            ~f:(fun tparams ->
              let open Ast.Type.TypeParams in
              let (annot, { params = tps; comments }) = tparams in
              let annot' = this#on_loc_annot annot in
              let tps' = Base.List.map ~f:this#type_param tps in
              let comments' = Base.Option.map ~f:this#syntax comments in
              (annot', { params = tps'; comments = comments' }))
            tparams
        in
        f tparams'

    method type_param (tparam : ('M, 'T) Ast.Type.TypeParam.t) : ('N, 'U) Ast.Type.TypeParam.t =
      let open Ast.Type.TypeParam in
      let (annot, { name; bound; variance; default }) = tparam in
      let name' = this#type_param_identifier name in
      let bound' = this#type_annotation_hint bound in
      let variance' = Base.Option.map ~f:this#variance variance in
      let default' = Base.Option.map ~f:this#type_ default in
      ( this#on_loc_annot annot,
        { name = name'; bound = bound'; variance = variance'; default = default' } )

    method type_param_identifier (id : ('M, 'M) Ast.Identifier.t) : ('N, 'N) Ast.Identifier.t =
      this#identifier id

    method generic_type (gt : ('M, 'T) Ast.Type.Generic.t) : ('N, 'U) Ast.Type.Generic.t =
      let open Ast.Type.Generic in
      let { id; targs; comments } = gt in
      let id' = this#generic_identifier_type id in
      let targs' = Base.Option.map ~f:this#type_args targs in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { id = id'; targs = targs'; comments = comments' }

    method type_predicate ((annot, pred) : ('M, 'T) Ast.Type.Predicate.t)
        : ('N, 'U) Ast.Type.Predicate.t =
      let open Ast.Type.Predicate in
      let { kind; comments } = pred in
      let annot' = this#on_loc_annot annot in
      let kind' =
        match kind with
        | Declared e -> Declared (this#expression e)
        | Inferred -> Inferred
      in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { kind = kind'; comments = comments' })

    method nullable_type (t : ('M, 'T) Ast.Type.Nullable.t) : ('N, 'U) Ast.Type.Nullable.t =
      let open Ast.Type.Nullable in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; comments = comments' }

    method typeof_type (t : ('M, 'T) Ast.Type.Typeof.t) : ('N, 'U) Ast.Type.Typeof.t =
      let open Ast.Type.Typeof in
      let { argument; internal; comments } = t in
      let argument' = this#type_ argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; internal; comments = comments' }

    method tuple_type (t : ('M, 'T) Ast.Type.Tuple.t) : ('N, 'U) Ast.Type.Tuple.t =
      let open Ast.Type.Tuple in
      let { types; comments } = t in
      let types' = Base.List.map ~f:this#type_ types in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { types = types'; comments = comments' }

    method array_type (t : ('M, 'T) Ast.Type.Array.t) : ('N, 'U) Ast.Type.Array.t =
      let open Ast.Type.Array in
      let { argument; comments } = t in
      let argument' = this#type_ argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; comments = comments' }

    method union_type (t : ('M, 'T) Ast.Type.Union.t) : ('N, 'U) Ast.Type.Union.t =
      let open Ast.Type.Union in
      let { types = (t0, t1, ts); comments } = t in
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = Base.List.map ~f:this#type_ ts in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { types = (t0', t1', ts'); comments = comments' }

    method intersection_type (t : ('M, 'T) Ast.Type.Intersection.t)
        : ('N, 'U) Ast.Type.Intersection.t =
      let open Ast.Type.Intersection in
      let { types = (t0, t1, ts); comments } = t in
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = Base.List.map ~f:this#type_ ts in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { types = (t0', t1', ts'); comments = comments' }

    method string_literal (t : 'M Ast.StringLiteral.t) : 'N Ast.StringLiteral.t =
      let open Ast.StringLiteral in
      let { value; raw; comments } = t in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { value; raw; comments = comments' }

    method number_literal (t : 'M Ast.NumberLiteral.t) : 'N Ast.NumberLiteral.t =
      let open Ast.NumberLiteral in
      let { value; raw; comments } = t in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { value; raw; comments = comments' }

    method bigint_literal (t : 'M Ast.BigIntLiteral.t) : 'N Ast.BigIntLiteral.t =
      let open Ast.BigIntLiteral in
      let { approx_value; bigint; comments } = t in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { approx_value; bigint; comments = comments' }

    method boolean_literal (t : 'M Ast.BooleanLiteral.t) : 'N Ast.BooleanLiteral.t =
      let open Ast.BooleanLiteral in
      let { value; comments } = t in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { value; comments = comments' }

    method type_ ((annot, t) : ('M, 'T) Ast.Type.t) : ('N, 'U) Ast.Type.t =
      Ast.Type.
        ( this#on_type_annot annot,
          match t with
          | Any comments -> Any (Base.Option.map ~f:this#syntax comments)
          | Mixed comments -> Mixed (Base.Option.map ~f:this#syntax comments)
          | Empty comments -> Empty (Base.Option.map ~f:this#syntax comments)
          | Void comments -> Void (Base.Option.map ~f:this#syntax comments)
          | Null comments -> Null (Base.Option.map ~f:this#syntax comments)
          | Symbol comments -> Symbol (Base.Option.map ~f:this#syntax comments)
          | Number comments -> Number (Base.Option.map ~f:this#syntax comments)
          | BigInt comments -> BigInt (Base.Option.map ~f:this#syntax comments)
          | String comments -> String (Base.Option.map ~f:this#syntax comments)
          | Boolean comments -> Boolean (Base.Option.map ~f:this#syntax comments)
          | Exists comments -> Exists (Base.Option.map ~f:this#syntax comments)
          | Nullable t' -> Nullable (this#nullable_type t')
          | Array t' -> Array (this#array_type t')
          | Typeof t' -> Typeof (this#typeof_type t')
          | Function ft -> Function (this#function_type ft)
          | Object ot -> Object (this#object_type ot)
          | Interface i -> Interface (this#interface_type i)
          | Generic gt -> Generic (this#generic_type gt)
          | Union t' -> Union (this#union_type t')
          | Intersection t' -> Intersection (this#intersection_type t')
          | Tuple t' -> Tuple (this#tuple_type t')
          | StringLiteral t' -> StringLiteral (this#string_literal t')
          | NumberLiteral t' -> NumberLiteral (this#number_literal t')
          | BigIntLiteral t' -> BigIntLiteral (this#bigint_literal t')
          | BooleanLiteral t' -> BooleanLiteral (this#boolean_literal t') )

    method implicit (t : ('M, 'T) Ast.Expression.CallTypeArg.Implicit.t)
        : ('N, 'U) Ast.Expression.CallTypeArg.Implicit.t =
      let open Ast.Expression.CallTypeArg.Implicit in
      let (annot, { comments }) = t in
      let annot' = this#on_type_annot annot in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { comments = comments' })

    method type_annotation ((annot, t_annot) : ('M, 'T) Ast.Type.annotation) =
      (this#on_loc_annot annot, this#type_ t_annot)

    method type_annotation_hint (return : ('M, 'T) Ast.Type.annotation_or_hint)
        : ('N, 'U) Ast.Type.annotation_or_hint =
      let open Ast.Type in
      match return with
      | Available annot -> Available (this#type_annotation annot)
      | Missing loc -> Missing (this#on_type_annot loc)

    method function_declaration (stmt : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
      this#function_ stmt

    method function_expression (expr : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
      this#function_ expr

    (* Internal helper for function declarations, function expressions and arrow functions *)
    method function_ (expr : ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
      let open Ast.Function in
      let {
        id = ident;
        params;
        body;
        async;
        generator;
        predicate;
        return;
        tparams;
        sig_loc;
        comments;
      } =
        expr
      in
      let ident' = Base.Option.map ~f:this#t_function_identifier ident in
      this#type_params_opt tparams (fun tparams' ->
          let params' = this#function_params params in
          let return' = this#type_annotation_hint return in
          let body' = this#function_body body in
          let predicate' = Base.Option.map ~f:this#type_predicate predicate in
          let sig_loc' = this#on_loc_annot sig_loc in
          let comments' = Base.Option.map ~f:this#syntax comments in
          {
            id = ident';
            params = params';
            return = return';
            body = body';
            async;
            generator;
            predicate = predicate';
            tparams = tparams';
            sig_loc = sig_loc';
            comments = comments';
          })

    method function_params (params : ('M, 'T) Ast.Function.Params.t)
        : ('N, 'U) Ast.Function.Params.t =
      let (annot, { Ast.Function.Params.params = params_list; rest; comments }) = params in
      let params_list' = Base.List.map ~f:this#function_param params_list in
      let rest' = Base.Option.map ~f:this#function_rest_param rest in
      let comments' = Base.Option.map ~f:this#syntax comments in
      ( this#on_loc_annot annot,
        { Ast.Function.Params.params = params_list'; rest = rest'; comments = comments' } )

    method function_param (param : ('M, 'T) Ast.Function.Param.t) : ('N, 'U) Ast.Function.Param.t =
      let open Ast.Function.Param in
      let (annot, { argument; default }) = param in
      let annot' = this#on_loc_annot annot in
      let argument' = this#function_param_pattern argument in
      let default' = Base.Option.map ~f:this#expression default in
      (annot', { argument = argument'; default = default' })

    method function_rest_param (expr : ('M, 'T) Ast.Function.RestParam.t)
        : ('N, 'U) Ast.Function.RestParam.t =
      let open Ast.Function.RestParam in
      let (annot, { argument; comments }) = expr in
      let annot' = this#on_loc_annot annot in
      let argument' = this#function_param_pattern argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { argument = argument'; comments = comments' })

    method function_body body =
      let open Ast.Function in
      match body with
      | BodyBlock body -> BodyBlock ((this#on_loc_annot * this#block) body)
      | BodyExpression expr -> BodyExpression (this#expression expr)

    method t_function_identifier (ident : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#t_pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Var ident

    method generator (expr : ('M, 'T) Ast.Expression.Generator.t)
        : ('N, 'U) Ast.Expression.Generator.t =
      let open Ast.Expression.Generator in
      let { blocks; filter } = expr in
      let blocks' = Base.List.map ~f:this#comprehension_block blocks in
      let filter' = Base.Option.map ~f:this#expression filter in
      { blocks = blocks'; filter = filter' }

    method identifier ((annot, { Ast.Identifier.name; comments }) : ('M, 'M) Ast.Identifier.t)
        : ('N, 'N) Ast.Identifier.t =
      let annot = this#on_loc_annot annot in
      let comments = Base.Option.map ~f:this#syntax comments in
      (annot, { Ast.Identifier.name; comments })

    method t_identifier ((annot, { Ast.Identifier.name; comments }) : ('M, 'T) Ast.Identifier.t)
        : ('N, 'U) Ast.Identifier.t =
      let annot = this#on_type_annot annot in
      let comments = Base.Option.map ~f:this#syntax comments in
      (annot, { Ast.Identifier.name; comments })

    method interface (interface : ('M, 'T) Ast.Statement.Interface.t)
        : ('N, 'U) Ast.Statement.Interface.t =
      let open Ast.Statement.Interface in
      let { id = ident; tparams; extends; body; comments } = interface in
      let id' = this#class_identifier ident in
      this#type_params_opt tparams (fun tparams' ->
          let extends' = Base.List.map ~f:(this#on_loc_annot * this#generic_type) extends in
          let body' = (this#on_loc_annot * this#object_type) body in
          let comments' = Base.Option.map ~f:this#syntax comments in
          { id = id'; tparams = tparams'; extends = extends'; body = body'; comments = comments' })

    method interface_declaration (decl : ('M, 'T) Ast.Statement.Interface.t)
        : ('N, 'U) Ast.Statement.Interface.t =
      this#interface decl

    method private_name ((annot, ident) : 'M Ast.PrivateName.t) : 'N Ast.PrivateName.t =
      let open Ast.PrivateName in
      let { id; comments } = ident in
      let annot' = this#on_loc_annot annot in
      let id' = this#identifier id in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { id = id'; comments = comments' })

    method computed_key ((annot, key) : ('M, 'T) Ast.ComputedKey.t) : ('N, 'U) Ast.ComputedKey.t =
      let open Ast.ComputedKey in
      let { expression; comments } = key in
      let annot' = this#on_loc_annot annot in
      let expression' = this#expression expression in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { expression = expression'; comments = comments' })

    method import _annot (expr : ('M, 'T) Ast.Expression.Import.t)
        : ('N, 'U) Ast.Expression.Import.t =
      let open Ast.Expression.Import in
      let { argument; comments } = expr in
      let argument' = this#expression argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; comments = comments' }

    method if_consequent_statement ~has_else (stmt : ('M, 'T) Ast.Statement.t)
        : ('N, 'U) Ast.Statement.t =
      ignore has_else;
      this#statement stmt

    method if_statement (stmt : ('M, 'T) Ast.Statement.If.t) : ('N, 'U) Ast.Statement.If.t =
      let open Ast.Statement.If in
      let { test; consequent; alternate; comments } = stmt in
      let test' = this#predicate_expression test in
      let consequent' = this#if_consequent_statement ~has_else:(alternate <> None) consequent in
      let alternate' = Base.Option.map ~f:this#statement alternate in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { test = test'; consequent = consequent'; alternate = alternate'; comments = comments' }

    method import_declaration _loc (decl : ('M, 'T) Ast.Statement.ImportDeclaration.t)
        : ('N, 'U) Ast.Statement.ImportDeclaration.t =
      let open Ast.Statement.ImportDeclaration in
      let { importKind; source; specifiers; default; comments } = decl in
      let specifiers' = Base.Option.map ~f:this#import_specifier specifiers in
      let default' = Base.Option.map ~f:this#import_default_specifier default in
      let source' = (this#on_loc_annot * this#string_literal) source in
      let comments' = Base.Option.map ~f:this#syntax comments in
      {
        importKind;
        source = source';
        specifiers = specifiers';
        default = default';
        comments = comments';
      }

    method import_specifier (specifier : ('M, 'T) Ast.Statement.ImportDeclaration.specifier)
        : ('N, 'U) Ast.Statement.ImportDeclaration.specifier =
      let open Ast.Statement.ImportDeclaration in
      match specifier with
      | ImportNamedSpecifiers named_specifiers ->
        let named_specifiers' = Base.List.map ~f:this#import_named_specifier named_specifiers in
        ImportNamedSpecifiers named_specifiers'
      | ImportNamespaceSpecifier (annot, ident) ->
        let ident' = this#import_namespace_specifier ident in
        ImportNamespaceSpecifier (this#on_loc_annot annot, ident')

    method import_named_specifier
        (specifier : ('M, 'T) Ast.Statement.ImportDeclaration.named_specifier)
        : ('N, 'U) Ast.Statement.ImportDeclaration.named_specifier =
      let open Ast.Statement.ImportDeclaration in
      let { kind; local; remote } = specifier in
      let local' = Base.Option.map ~f:this#t_pattern_identifier local in
      let remote' = this#t_pattern_identifier remote in
      { kind; local = local'; remote = remote' }

    method import_default_specifier (id : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#t_pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let id

    method import_namespace_specifier (id : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#t_pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let id

    method jsx_element (expr : ('M, 'T) Ast.JSX.element) =
      let open Ast.JSX in
      let { openingElement; closingElement; children; comments } = expr in
      let openingElement' = this#jsx_opening_element openingElement in
      let closingElement' = Base.Option.map ~f:this#jsx_closing_element closingElement in
      let children' = this#jsx_children children in
      let comments' = Base.Option.map ~f:this#syntax comments in
      {
        openingElement = openingElement';
        closingElement = closingElement';
        children = children';
        comments = comments';
      }

    method jsx_fragment (expr : ('M, 'T) Ast.JSX.fragment) : ('N, 'U) Ast.JSX.fragment =
      let open Ast.JSX in
      let { frag_openingElement; frag_closingElement; frag_children; frag_comments } = expr in
      let opening' = this#on_loc_annot frag_openingElement in
      let closing' = this#on_loc_annot frag_closingElement in
      let children' = this#jsx_children frag_children in
      let frag_comments' = Base.Option.map ~f:this#syntax frag_comments in
      {
        frag_openingElement = opening';
        frag_closingElement = closing';
        frag_children = children';
        frag_comments = frag_comments';
      }

    method jsx_opening_element (elem : ('M, 'T) Ast.JSX.Opening.t) : ('N, 'U) Ast.JSX.Opening.t =
      let open Ast.JSX.Opening in
      let (annot, { name; selfClosing; attributes }) = elem in
      let name' = this#jsx_name name in
      let attributes' = Base.List.map ~f:this#jsx_opening_attribute attributes in
      (this#on_loc_annot annot, { name = name'; selfClosing; attributes = attributes' })

    method jsx_closing_element (elem : ('M, 'T) Ast.JSX.Closing.t) : ('N, 'U) Ast.JSX.Closing.t =
      let open Ast.JSX.Closing in
      let (annot, { name }) = elem in
      let name' = this#jsx_name name in
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
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; comments = comments' }

    method jsx_attribute (attr : ('M, 'T) Ast.JSX.Attribute.t) : ('N, 'U) Ast.JSX.Attribute.t =
      let open Ast.JSX.Attribute in
      let (annot, { name; value }) = attr in
      let name' =
        match name with
        | Identifier id -> Identifier (this#jsx_identifier id)
        | NamespacedName nname -> NamespacedName (this#jsx_namespaced_name nname)
      in
      let value' = Base.Option.map ~f:this#jsx_attribute_value value in
      (this#on_loc_annot annot, { name = name'; value = value' })

    method jsx_attribute_value (value : ('M, 'T) Ast.JSX.Attribute.value)
        : ('N, 'U) Ast.JSX.Attribute.value =
      let open Ast.JSX.Attribute in
      match value with
      | Literal (annot, lit) -> Literal (this#on_type_annot annot, this#literal lit)
      | ExpressionContainer (annot, expr) ->
        ExpressionContainer (this#on_type_annot annot, this#jsx_expression expr)

    method jsx_children (children : 'M * ('M, 'T) Ast.JSX.child list)
        : 'N * ('N, 'U) Ast.JSX.child list =
      let (annot, children') = children in
      (this#on_loc_annot annot, Base.List.map ~f:this#jsx_child children')

    method jsx_child (child : ('M, 'T) Ast.JSX.child) : ('N, 'U) Ast.JSX.child =
      let open Ast.JSX in
      let (annot, child') = child in
      ( this#on_loc_annot annot,
        match child' with
        | Element elem -> Element (this#jsx_element elem)
        | Fragment frag -> Fragment (this#jsx_fragment frag)
        | ExpressionContainer expr -> ExpressionContainer (this#jsx_expression expr)
        | SpreadChild spread -> SpreadChild (this#jsx_spread_child spread)
        | Text _ as child' -> child' )

    method jsx_expression (jsx_expr : ('M, 'T) Ast.JSX.ExpressionContainer.t)
        : ('N, 'U) Ast.JSX.ExpressionContainer.t =
      let open Ast.JSX.ExpressionContainer in
      let { expression; comments } = jsx_expr in
      let expression' =
        match expression with
        | Expression expr -> Expression (this#expression expr)
        | EmptyExpression -> EmptyExpression
      in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { expression = expression'; comments = comments' }

    method jsx_spread_child (jsx_spread_child : ('M, 'T) Ast.JSX.SpreadChild.t)
        : ('N, 'U) Ast.JSX.SpreadChild.t =
      let open Ast.JSX.SpreadChild in
      let { expression; comments } = jsx_spread_child in
      let expression' = this#expression expression in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { expression = expression'; comments = comments' }

    method jsx_name (name : ('M, 'T) Ast.JSX.name) : ('N, 'U) Ast.JSX.name =
      let open Ast.JSX in
      match name with
      | Identifier id -> Identifier (this#jsx_identifier id)
      | NamespacedName namespaced_name -> NamespacedName (this#jsx_namespaced_name namespaced_name)
      | MemberExpression member_exp -> MemberExpression (this#jsx_member_expression member_exp)

    method jsx_namespaced_name (namespaced_name : ('M, 'T) Ast.JSX.NamespacedName.t)
        : ('N, 'U) Ast.JSX.NamespacedName.t =
      let open Ast.JSX in
      NamespacedName.(
        let (annot, { namespace; name }) = namespaced_name in
        let namespace' = this#jsx_identifier namespace in
        let name' = this#jsx_identifier name in
        (this#on_loc_annot annot, { namespace = namespace'; name = name' }))

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
        let id' = this#jsx_identifier id in
        Identifier id'
      | MemberExpression nested_exp ->
        let nested_exp' = this#jsx_member_expression nested_exp in
        MemberExpression nested_exp'

    method jsx_identifier ((annot, id) : ('M, 'T) Ast.JSX.Identifier.t)
        : ('N, 'U) Ast.JSX.Identifier.t =
      let open Ast.JSX.Identifier in
      let { name; comments } = id in
      let annot' = this#on_type_annot annot in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { name; comments = comments' })

    method labeled_statement (stmt : ('M, 'T) Ast.Statement.Labeled.t)
        : ('N, 'U) Ast.Statement.Labeled.t =
      let open Ast.Statement.Labeled in
      let { label; body; comments } = stmt in
      let label' = this#label_identifier label in
      let body' = this#statement body in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { label = label'; body = body'; comments = comments' }

    method literal (expr : 'M Ast.Literal.t) : 'N Ast.Literal.t =
      let open Ast.Literal in
      let { comments; _ } = expr in
      { expr with comments = Base.Option.map ~f:this#syntax comments }

    method logical (expr : ('M, 'T) Ast.Expression.Logical.t) : ('N, 'U) Ast.Expression.Logical.t =
      let open Ast.Expression.Logical in
      let { operator; left; right; comments } = expr in
      let left' = this#expression left in
      let right' = this#expression right in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { operator; left = left'; right = right'; comments = comments' }

    method member (expr : ('M, 'T) Ast.Expression.Member.t) : ('N, 'U) Ast.Expression.Member.t =
      let open Ast.Expression.Member in
      let { _object; property; comments } = expr in
      let _object' = this#expression _object in
      let property' = this#member_property property in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { _object = _object'; property = property'; comments = comments' }

    method optional_member (expr : ('M, 'T) Ast.Expression.OptionalMember.t)
        : ('N, 'U) Ast.Expression.OptionalMember.t =
      let open Ast.Expression.OptionalMember in
      let { member; optional } = expr in
      let member' = this#member member in
      { member = member'; optional }

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
        comments = Base.Option.map ~f:this#syntax comments;
      }

    method new_ (expr : ('M, 'T) Ast.Expression.New.t) : ('N, 'U) Ast.Expression.New.t =
      let open Ast.Expression.New in
      let { callee; targs; arguments; comments } = expr in
      let callee' = this#expression callee in
      let targs' = Base.Option.map ~f:this#call_type_args targs in
      let arguments' = Base.Option.map ~f:this#arg_list arguments in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { callee = callee'; targs = targs'; arguments = arguments'; comments = comments' }

    method object_ (expr : ('M, 'T) Ast.Expression.Object.t) : ('N, 'U) Ast.Expression.Object.t =
      let open Ast.Expression.Object in
      let { properties; comments } = expr in
      let comments' = Base.Option.map ~f:this#syntax comments in
      let properties' = List.map this#object_property_or_spread_property properties in
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
          let comments' = Base.Option.map ~f:this#syntax comments in
          Get { key = key'; value = (this#on_loc_annot fn_annot, fn'); comments = comments' }
        | Set { key; value = (fn_annot, fn); comments } ->
          let key' = this#object_key key in
          let fn' = this#function_expression fn in
          let comments' = Base.Option.map ~f:this#syntax comments in
          Set { key = key'; value = (this#on_loc_annot fn_annot, fn'); comments = comments' } )

    method object_key (key : ('M, 'T) Ast.Expression.Object.Property.key)
        : ('N, 'U) Ast.Expression.Object.Property.key =
      let open Ast.Expression.Object.Property in
      match key with
      | Literal (annot, lit) -> Literal (this#on_type_annot annot, this#literal lit)
      | Identifier ident -> Identifier (this#object_key_identifier ident)
      | PrivateName ident -> PrivateName (this#private_name ident)
      | Computed computed -> Computed (this#object_key_computed computed)

    method object_key_identifier (ident : ('M, 'T) Ast.Identifier.t) : ('N, 'U) Ast.Identifier.t =
      this#t_identifier ident

    method object_key_computed (key : ('M, 'T) Ast.ComputedKey.t) : ('N, 'U) Ast.ComputedKey.t =
      this#computed_key key

    method opaque_type (otype : ('M, 'T) Ast.Statement.OpaqueType.t)
        : ('N, 'U) Ast.Statement.OpaqueType.t =
      let open Ast.Statement.OpaqueType in
      let { id; tparams; impltype; supertype; comments } = otype in
      let id' = this#t_identifier id in
      this#type_params_opt tparams (fun tparams' ->
          let impltype' = Base.Option.map ~f:this#type_ impltype in
          let supertype' = Base.Option.map ~f:this#type_ supertype in
          let comments' = Base.Option.map ~f:this#syntax comments in
          {
            id = id';
            tparams = tparams';
            impltype = impltype';
            supertype = supertype';
            comments = comments';
          })

    method declare_opaque_type (otype : ('M, 'T) Ast.Statement.OpaqueType.t)
        : ('N, 'U) Ast.Statement.OpaqueType.t =
      this#opaque_type otype

    method function_param_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#binding_pattern expr

    method variable_declarator_pattern ~kind (expr : ('M, 'T) Ast.Pattern.t)
        : ('N, 'U) Ast.Pattern.t =
      this#binding_pattern ~kind expr

    method catch_clause_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#binding_pattern ~kind:Ast.Statement.VariableDeclaration.Let expr

    method for_in_assignment_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#assignment_pattern expr

    method for_of_assignment_pattern (expr : ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
      this#assignment_pattern expr

    method binding_pattern
        ?(kind = Ast.Statement.VariableDeclaration.Var) (expr : ('M, 'T) Ast.Pattern.t)
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
        | Object { Object.properties; annot } ->
          let properties' = Base.List.map ~f:(this#pattern_object_p ?kind) properties in
          let annot' = this#type_annotation_hint annot in
          Object { Object.properties = properties'; annot = annot' }
        | Array { Array.elements; annot; comments } ->
          let elements' =
            Base.List.map ~f:(Base.Option.map ~f:(this#pattern_array_e ?kind)) elements
          in
          let annot' = this#type_annotation_hint annot in
          let comments' = Base.Option.map ~f:this#syntax comments in
          Array { Array.elements = elements'; annot = annot'; comments = comments' }
        | Identifier { Identifier.name; annot; optional } ->
          let name' = this#t_pattern_identifier ?kind name in
          let annot' = this#type_annotation_hint annot in
          Identifier { Identifier.name = name'; annot = annot'; optional }
        | Expression e -> Expression (this#pattern_expression e) )

    method t_pattern_identifier ?kind (ident : ('M, 'T) Ast.Identifier.t)
        : ('N, 'U) Ast.Identifier.t =
      ignore kind;
      this#t_identifier ident

    method pattern_literal ?kind (expr : 'M Ast.Literal.t) : 'N Ast.Literal.t =
      ignore kind;
      this#literal expr

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
      let default' = Base.Option.map ~f:this#expression default in
      { key = key'; pattern = pattern'; default = default'; shorthand }

    method pattern_object_property_key ?kind (key : ('M, 'T) Ast.Pattern.Object.Property.key) =
      let open Ast.Pattern.Object.Property in
      match key with
      | Literal (annot, lit) ->
        Literal (this#on_loc_annot annot, this#pattern_object_property_literal_key ?kind lit)
      | Identifier identifier ->
        Identifier (this#pattern_object_property_identifier_key ?kind identifier)
      | Computed expr -> Computed (this#pattern_object_property_computed_key ?kind expr)

    method pattern_object_property_literal_key ?kind (key : 'M Ast.Literal.t) : 'N Ast.Literal.t =
      this#pattern_literal ?kind key

    method pattern_object_property_identifier_key ?kind (key : ('M, 'T) Ast.Identifier.t)
        : ('N, 'U) Ast.Identifier.t =
      this#t_pattern_identifier ?kind key

    method pattern_object_property_computed_key ?kind (key : ('M, 'T) Ast.ComputedKey.t)
        : ('N, 'U) Ast.ComputedKey.t =
      ignore kind;
      this#computed_key key

    method pattern_object_rest_property ?kind (prop : ('M, 'T) Ast.Pattern.RestElement.t')
        : ('N, 'U) Ast.Pattern.RestElement.t' =
      let open Ast.Pattern.RestElement in
      let { argument; comments } = prop in
      let argument' = this#pattern_object_rest_property_pattern ?kind argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
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
      | Element (annot, elem) ->
        Element (this#on_loc_annot annot, this#pattern_array_element ?kind elem)
      | RestElement (annot, elem) ->
        RestElement (this#on_loc_annot annot, this#pattern_array_rest_element ?kind elem)

    method pattern_array_element ?kind (elem : ('M, 'T) Ast.Pattern.Array.Element.t')
        : ('N, 'U) Ast.Pattern.Array.Element.t' =
      let open Ast.Pattern.Array.Element in
      let { argument; default } = elem in
      let argument' = this#pattern_array_element_pattern ?kind argument in
      let default' = Base.Option.map ~f:this#expression default in
      { argument = argument'; default = default' }

    method pattern_array_element_pattern ?kind (expr : ('M, 'T) Ast.Pattern.t)
        : ('N, 'U) Ast.Pattern.t =
      this#pattern ?kind expr

    method pattern_array_rest_element ?kind (elem : ('M, 'T) Ast.Pattern.RestElement.t') =
      let open Ast.Pattern.RestElement in
      let { argument; comments } = elem in
      let argument' = this#pattern_array_rest_element_pattern ?kind argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
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
      let { argument; comments } = stmt in
      let argument' = Base.Option.map ~f:this#expression argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; comments = comments' }

    method sequence (expr : ('M, 'T) Ast.Expression.Sequence.t) : ('N, 'U) Ast.Expression.Sequence.t
        =
      let open Ast.Expression.Sequence in
      let { expressions; comments } = expr in
      let expressions' = Base.List.map ~f:this#expression expressions in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { expressions = expressions'; comments = comments' }

    method toplevel_statement_list (stmts : ('M, 'T) Ast.Statement.t list)
        : ('N, 'U) Ast.Statement.t list =
      this#statement_list stmts

    method statement_list (stmts : ('M, 'T) Ast.Statement.t list) : ('N, 'U) Ast.Statement.t list =
      Base.List.map ~f:this#statement stmts

    method spread_element (expr : ('M, 'T) Ast.Expression.SpreadElement.t)
        : ('N, 'U) Ast.Expression.SpreadElement.t =
      let open Ast.Expression.SpreadElement in
      let (annot, { argument; comments }) = expr in
      let annot' = this#on_loc_annot annot in
      let argument' = this#expression argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { argument = argument'; comments = comments' })

    method spread_property (expr : ('M, 'T) Ast.Expression.Object.SpreadProperty.t)
        : ('N, 'U) Ast.Expression.Object.SpreadProperty.t =
      let open Ast.Expression.Object.SpreadProperty in
      let (annot, { argument; comments }) = expr in
      let annot' = this#on_loc_annot annot in
      let argument' = this#expression argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { argument = argument'; comments = comments' })

    method super_expression (expr : 'M Ast.Expression.Super.t) : 'N Ast.Expression.Super.t =
      let open Ast.Expression.Super in
      let { comments } = expr in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { comments = comments' }

    method switch (switch : ('M, 'T) Ast.Statement.Switch.t) : ('N, 'U) Ast.Statement.Switch.t =
      let open Ast.Statement.Switch in
      let { discriminant; cases; comments } = switch in
      let discriminant' = this#expression discriminant in
      let cases' = Base.List.map ~f:(this#on_loc_annot * this#switch_case) cases in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { discriminant = discriminant'; cases = cases'; comments = comments' }

    method switch_case (case : ('M, 'T) Ast.Statement.Switch.Case.t') =
      let open Ast.Statement.Switch.Case in
      let { test; consequent; comments } = case in
      let test' = Base.Option.map ~f:this#expression test in
      let consequent' = this#statement_list consequent in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { test = test'; consequent = consequent'; comments = comments' }

    method tagged_template (expr : ('M, 'T) Ast.Expression.TaggedTemplate.t)
        : ('N, 'U) Ast.Expression.TaggedTemplate.t =
      let open Ast.Expression.TaggedTemplate in
      let { tag; quasi; comments } = expr in
      let tag' = this#expression tag in
      let quasi' = (this#on_loc_annot * this#template_literal) quasi in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { tag = tag'; quasi = quasi'; comments = comments' }

    method template_literal (expr : ('M, 'T) Ast.Expression.TemplateLiteral.t)
        : ('N, 'U) Ast.Expression.TemplateLiteral.t =
      let open Ast.Expression.TemplateLiteral in
      let { quasis; expressions; comments } = expr in
      let quasis' = Base.List.map ~f:this#template_literal_element quasis in
      let expressions' = Base.List.map ~f:this#expression expressions in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { quasis = quasis'; expressions = expressions'; comments = comments' }

    method template_literal_element ((annot, elem) : 'M Ast.Expression.TemplateLiteral.Element.t)
        : 'N Ast.Expression.TemplateLiteral.Element.t =
      (this#on_loc_annot annot, elem)

    method this_expression (expr : 'M Ast.Expression.This.t) : 'N Ast.Expression.This.t =
      let open Ast.Expression.This in
      let { comments } = expr in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { comments = comments' }

    method throw (stmt : ('M, 'T) Ast.Statement.Throw.t) : ('N, 'U) Ast.Statement.Throw.t =
      let open Ast.Statement.Throw in
      let { argument; comments } = stmt in
      let argument' = this#expression argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; comments = comments' }

    method try_catch (stmt : ('M, 'T) Ast.Statement.Try.t) : ('N, 'U) Ast.Statement.Try.t =
      let open Ast.Statement.Try in
      let { block; handler; finalizer; comments } = stmt in
      let block' = (this#on_loc_annot * this#block) block in
      let handler' = Base.Option.map ~f:(this#on_loc_annot * this#catch_clause) handler in
      let finalizer' = Base.Option.map ~f:(this#on_loc_annot * this#block) finalizer in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { block = block'; handler = handler'; finalizer = finalizer'; comments = comments' }

    method type_cast (expr : ('M, 'T) Ast.Expression.TypeCast.t)
        : ('N, 'U) Ast.Expression.TypeCast.t =
      let open Ast.Expression.TypeCast in
      let { expression; annot; comments } = expr in
      let expression' = this#expression expression in
      let annot' = this#type_annotation annot in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { expression = expression'; annot = annot'; comments = comments' }

    method unary_expression (expr : ('M, 'T) Ast.Expression.Unary.t)
        : ('N, 'U) Ast.Expression.Unary.t =
      let open Ast.Expression.Unary in
      let { argument; operator; comments } = expr in
      let argument' = this#expression argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; operator; comments = comments' }

    method update_expression (expr : ('M, 'T) Ast.Expression.Update.t)
        : ('N, 'U) Ast.Expression.Update.t =
      let open Ast.Expression.Update in
      let { argument; operator; prefix; comments } = expr in
      let argument' = this#expression argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; operator; prefix; comments = comments' }

    method variable_declaration (decl : ('M, 'T) Ast.Statement.VariableDeclaration.t)
        : ('N, 'U) Ast.Statement.VariableDeclaration.t =
      let open Ast.Statement.VariableDeclaration in
      let { declarations; kind; comments } = decl in
      let decls' = Base.List.map ~f:(this#variable_declarator ~kind) declarations in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { declarations = decls'; kind; comments = comments' }

    method variable_declarator
        ~kind (decl : ('M, 'T) Ast.Statement.VariableDeclaration.Declarator.t)
        : ('N, 'U) Ast.Statement.VariableDeclaration.Declarator.t =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (annot, { id; init }) = decl in
      let id' = this#variable_declarator_pattern ~kind id in
      let init' = Base.Option.map ~f:this#expression init in
      (this#on_loc_annot annot, { id = id'; init = init' })

    method variance (variance : 'M Ast.Variance.t) : 'N Ast.Variance.t =
      let open Ast.Variance in
      let (annot, { kind; comments }) = variance in
      let annot' = this#on_loc_annot annot in
      let comments' = Base.Option.map ~f:this#syntax comments in
      (annot', { kind; comments = comments' })

    method while_ (stuff : ('M, 'T) Ast.Statement.While.t) : ('N, 'U) Ast.Statement.While.t =
      let open Ast.Statement.While in
      let { test; body; comments } = stuff in
      let test' = this#predicate_expression test in
      let body' = this#statement body in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { test = test'; body = body'; comments = comments' }

    method with_ (stuff : ('M, 'T) Ast.Statement.With.t) : ('N, 'U) Ast.Statement.With.t =
      let open Ast.Statement.With in
      let { _object; body; comments } = stuff in
      let _object' = this#expression _object in
      let body' = this#statement body in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { _object = _object'; body = body'; comments = comments' }

    method type_alias (stuff : ('M, 'T) Ast.Statement.TypeAlias.t)
        : ('N, 'U) Ast.Statement.TypeAlias.t =
      let open Ast.Statement.TypeAlias in
      let { id; tparams; right; comments } = stuff in
      let id' = this#t_identifier id in
      this#type_params_opt tparams (fun tparams' ->
          let right' = this#type_ right in
          let comments' = Base.Option.map ~f:this#syntax comments in
          { id = id'; tparams = tparams'; right = right'; comments = comments' })

    method yield (expr : ('M, 'T) Ast.Expression.Yield.t) : ('N, 'U) Ast.Expression.Yield.t =
      let open Ast.Expression.Yield in
      let { argument; delegate; comments } = expr in
      let argument' = Base.Option.map ~f:this#expression argument in
      let comments' = Base.Option.map ~f:this#syntax comments in
      { argument = argument'; delegate; comments = comments' }
  end
