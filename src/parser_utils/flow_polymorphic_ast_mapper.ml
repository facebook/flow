(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let ( * ) f g (x, y) = (f x, g y)
let id x = x

type m = []
type t = []
type n = []
type u = []

class ['M, 'T, 'N, 'U] mapper (f : 'M -> 'N) (g : 'T -> 'U) = object(this)

  (* OCaml's typechecker has a bug which allows 'M and 'N to unify. Uncommenting
    this method uses this bug to monomorphize 'M and 'N to different types,
    which makes it easier to avoid unifying 'M and 'N with each other when
    developing this class. Same for 'T and 'U.
  method uncomment_when_developing
    ((_ : 'M) : m) ((_ : 'T) : t) ((_ : 'N) : n) ((_ : 'U) : u) = ()
  *)

  method program (program: ('M, 'T) Ast.program) : ('N, 'U) Ast.program =
    let (annot, statements, comments) = program in
    let annot' = f annot in
    let statements' = this#toplevel_statement_list statements in
    let comments' = List.map (this#comment) comments in
    annot', statements', comments'

  method statement ((annot, stmt): ('M, 'T) Ast.Statement.t) : ('N, 'U) Ast.Statement.t =
    let open Ast.Statement in
    f annot,
    match stmt with
    | Block block -> Block (this#block block)

    | Break break -> Break (this#break break)

    | ClassDeclaration cls -> ClassDeclaration (this#class_ cls)

    | Continue cont -> Continue (this#continue cont)

    | Debugger -> Debugger

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

    | Empty ->
      this#empty ();
      Empty

    | ExportDefaultDeclaration decl ->
      ExportDefaultDeclaration (this#export_default_declaration annot decl)

    | ExportNamedDeclaration decl ->
      ExportNamedDeclaration (this#export_named_declaration annot decl)

    | Expression expr -> Expression (this#expression_statement expr)

    | For for_stmt -> For (this#for_statement for_stmt)

    | ForIn stuff -> ForIn (this#for_in_statement stuff)

    | ForOf stuff -> ForOf (this#for_of_statement stuff)

    | FunctionDeclaration func ->
      FunctionDeclaration (this#function_declaration func)

    | If if_stmt -> If (this#if_statement if_stmt)

    | ImportDeclaration decl ->
      ImportDeclaration (this#import_declaration annot decl)

    | InterfaceDeclaration stuff ->
      InterfaceDeclaration (this#interface_declaration stuff)

    | Labeled label -> Labeled (this#labeled_statement label)

    | OpaqueType otype -> OpaqueType (this#opaque_type otype)

    | Return ret -> Return (this#return ret)

    | Switch switch -> Switch (this#switch switch)

    | Throw throw -> Throw (this#throw throw)

    | Try try_stmt -> Try (this#try_catch try_stmt)

    | VariableDeclaration decl ->
      VariableDeclaration (this#variable_declaration decl)

    | While stuff -> While (this#while_ stuff)

    | With stuff -> With (this#with_ stuff)

    | TypeAlias stuff -> TypeAlias (this#type_alias stuff)

    | DeclareOpaqueType otype ->
      DeclareOpaqueType (this#declare_opaque_type otype)

  method comment ((annot, c): 'M Ast.Comment.t) : 'N Ast.Comment.t =
    f annot, c

  method expression ((annot, expr'): ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
    let open Ast.Expression in
    g annot,
    match expr' with
    | This -> This
    | Super -> Super
    | Array x -> Array (this#array x)
    | ArrowFunction x -> ArrowFunction (this#arrow_function x)
    | Assignment x -> Assignment (this#assignment x)
    | Binary x -> Binary (this#binary x)
    | Call x -> Call (this#call annot x)
    | Class x -> Class (this#class_ x)
    | Comprehension x -> Comprehension (this#comprehension x)
    | Conditional x -> Conditional (this#conditional x)
    | Function x -> Function (this#function_ x)
    | Generator x -> Generator (this#generator x)
    | Identifier x -> Identifier (this#identifier x)
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
    | TaggedTemplate x -> TaggedTemplate (this#tagged_template x)
    | TemplateLiteral x -> TemplateLiteral (this#template_literal x)
    | TypeCast x -> TypeCast (this#type_cast x)
    | Unary x -> Unary (this#unary_expression x)
    | Update x -> Update (this#update_expression x)
    | Yield x -> Yield (this#yield x)

  method array (expr: ('M, 'T) Ast.Expression.Array.t) : ('N, 'U) Ast.Expression.Array.t =
    let open Ast.Expression in
    let { Array.elements } = expr in
    let elements' = List.map (Option.map ~f:this#expression_or_spread) elements in
    { Array.elements = elements' }

  method arrow_function (expr: ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
    this#function_ expr

  method assignment (expr: ('M, 'T) Ast.Expression.Assignment.t)
                        : ('N, 'U) Ast.Expression.Assignment.t =
    let open Ast.Expression.Assignment in
    let { operator; left; right } = expr in
    let left' = this#assignment_pattern left in
    let right' = this#expression right in
    { operator; left = left'; right = right' }

  method binary (expr: ('M, 'T) Ast.Expression.Binary.t) : ('N, 'U) Ast.Expression.Binary.t =
    let open Ast.Expression.Binary in
    let { operator; left; right } = expr in
    let left' = this#expression left in
    let right' = this#expression right in
    { operator; left = left'; right = right' }

  method block (stmt: ('M, 'T) Ast.Statement.Block.t) : ('N, 'U) Ast.Statement.Block.t =
    let open Ast.Statement.Block in
    let { body } = stmt in
    let body' = this#statement_list body in
    { body = body' }

  method break (break: 'M Ast.Statement.Break.t) : 'N Ast.Statement.Break.t =
    let open Ast.Statement.Break in
    let { label } = break in
    let label' = Option.map ~f:this#label_identifier label in
    { label = label' }

  method call _annot (expr: ('M, 'T) Ast.Expression.Call.t) : ('N, 'U) Ast.Expression.Call.t =
    let open Ast.Expression.Call in
    let { callee; targs; arguments } = expr in
    let callee' = this#expression callee in
    let targs' = Option.map ~f:this#type_parameter_instantiation targs in
    let arguments' = List.map this#expression_or_spread arguments in
    { callee = callee'; targs = targs'; arguments = arguments' }

  method optional_call annot (expr: ('M, 'T) Ast.Expression.OptionalCall.t)
                                : ('N, 'U) Ast.Expression.OptionalCall.t =
    let open Ast.Expression.OptionalCall in
    let { call; optional; } = expr in
    let call' = this#call annot call in
    { call = call'; optional }

  method catch_clause (clause: ('M, 'T) Ast.Statement.Try.CatchClause.t')
                            : ('N, 'U) Ast.Statement.Try.CatchClause.t' =
    let open Ast.Statement.Try.CatchClause in
    let { param; body } = clause in
    let param' = Option.map ~f:this#catch_clause_pattern param in
    let body' = (f * this#block) body in
    { param = param'; body = body' }

  method class_ (cls: ('M, 'T) Ast.Class.t) : ('N, 'U) Ast.Class.t =
    let open Ast.Class in
    let { id; body; tparams; super; super_targs; implements; classDecorators; } = cls in
    let id' = Option.map ~f:this#class_identifier id in
    let body' = this#class_body body in
    let tparams' = Option.map ~f:this#type_parameter_declaration tparams in
    let super' = Option.map ~f:this#expression super in
    let super_targs' = Option.map ~f:this#type_parameter_instantiation super_targs in
    let implements' = List.map this#class_implements implements in
    let classDecorators' = List.map this#class_decorator classDecorators in
    {
      id = id';
      body = body';
      tparams = tparams';
      super = super';
      super_targs = super_targs';
      implements = implements';
      classDecorators = classDecorators';
    }

  method class_decorator (dec : ('M, 'T) Ast.Class.Decorator.t) : ('N, 'U) Ast.Class.Decorator.t =
    let open Ast.Class.Decorator in
    let annot, { expression } = dec in
    let expression' = this#expression expression in
    f annot, { expression = expression' }

  method class_identifier (ident: 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let ident

  method class_body (cls_body: ('M, 'T) Ast.Class.Body.t) : ('N, 'U) Ast.Class.Body.t =
    let open Ast.Class.Body in
    let annot, { body } = cls_body in
    let body' = List.map this#class_element body in
    f annot, { body = body' }

  method class_element (elem: ('M, 'T) Ast.Class.Body.element) : ('N, 'U) Ast.Class.Body.element =
    let open Ast.Class.Body in
    match elem with
    | Method (annot, meth) ->
      Method (f annot, this#class_method meth)
    | Property (annot, prop) ->
      Property (f annot, this#class_property prop)
    | PrivateField (annot, field) ->
      PrivateField (f annot, this#class_private_field field)

  method class_method (meth: ('M, 'T) Ast.Class.Method.t') : ('N, 'U) Ast.Class.Method.t' =
    let open Ast.Class.Method in
    let { kind; key; value; static; decorators; } = meth in
    let key' = this#object_key key in
    let value' = (f * this#function_) value in
    let decorators' = List.map this#class_decorator decorators in
    { kind; key = key'; value = value'; static; decorators = decorators' }

  method class_property (prop: ('M, 'T) Ast.Class.Property.t') : ('N, 'U) Ast.Class.Property.t' =
    let open Ast.Class.Property in
    let { key; value; annot; static; variance; } = prop in
    let key' = this#object_key key in
    let value' = Option.map ~f:this#expression value in
    let annot' = Option.map ~f:this#type_annotation annot in
    let variance' = Option.map ~f:(f * id) variance in
    { key = key'; value = value'; annot = annot'; static; variance = variance'; }

  method class_private_field (prop: ('M, 'T) Ast.Class.PrivateField.t') =
    let open Ast.Class.PrivateField in
    let { key; value; annot; static; variance; } = prop in
    let key' = this#private_name key in
    let value' = Option.map ~f:this#expression value in
    let annot' = Option.map ~f:this#type_annotation annot in
    let variance' = Option.map ~f:(f * id) variance in
    { key = key'; value = value'; annot = annot'; static; variance = variance' }

  method comprehension (expr: ('M, 'T) Ast.Expression.Comprehension.t)
                            : ('N, 'U) Ast.Expression.Comprehension.t =
    let open Ast.Expression.Comprehension in
    let { blocks; filter } = expr in
    let blocks' = List.map this#comprehension_block blocks in
    let filter' = Option.map ~f:this#expression filter in
    { blocks = blocks'; filter = filter' }

  method comprehension_block (block : ('M, 'T) Ast.Expression.Comprehension.Block.t)
                                    : ('N, 'U) Ast.Expression.Comprehension.Block.t =
    let open Ast.Expression.Comprehension.Block in
    let annot, { left; right; each } = block in
    let left' = this#pattern left in
    let right' = this#expression right in
    f annot, { left = left'; right = right'; each }

  method conditional (expr: ('M, 'T) Ast.Expression.Conditional.t)
                          : ('N, 'U) Ast.Expression.Conditional.t =
    let open Ast.Expression.Conditional in
    let { test; consequent; alternate } = expr in
    let test' = this#predicate_expression test in
    let consequent' = this#expression consequent in
    let alternate' = this#expression alternate in
    { test = test'; consequent = consequent'; alternate = alternate' }

  method continue (cont: 'M Ast.Statement.Continue.t) : 'N Ast.Statement.Continue.t =
    let open Ast.Statement.Continue in
    let { label } = cont in
    let label' = Option.map ~f:this#label_identifier label in
    { label = label' }

  method debugger () =
    ()

  method declare_class (decl: ('M, 'T) Ast.Statement.DeclareClass.t)
                            : ('N, 'U) Ast.Statement.DeclareClass.t =
    let open Ast.Statement.DeclareClass in
    let { id = ident; tparams; body; extends; mixins; implements } = decl in
    let id' = this#class_identifier ident in
    let tparams' = Option.map ~f:this#type_parameter_declaration tparams in
    let body' = (f *this#object_type) body in
    let extends' = Option.map ~f:(f * this#generic_type) extends in
    let mixins' = List.map (f * this#generic_type) mixins in
    let implements' = List.map this#class_implements implements in
    {
      id = id';
      tparams = tparams';
      body = body';
      extends = extends';
      mixins = mixins';
      implements = implements';
    }

  method class_implements (implements: ('M, 'T) Ast.Class.Implements.t)
                                    : ('N, 'U) Ast.Class.Implements.t =
    let open Ast.Class.Implements in
    let annot, { id = id_; targs } = implements in
    let id' = (f * id) id_ in
    let targs' = Option.map ~f:this#type_parameter_instantiation targs in
    f annot, { id = id'; targs = targs' }

  method declare_export_declaration _annot
    (decl: ('M, 'T) Ast.Statement.DeclareExportDeclaration.t)
         : ('N, 'U) Ast.Statement.DeclareExportDeclaration.t =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default; source; specifiers; declaration } = decl in
    let default' = Option.map ~f default in
    let source' = Option.map ~f:(f * id) source in
    let specifiers' = Option.map ~f:this#export_named_specifier specifiers in
    let declaration' = Option.map ~f:this#declare_export_declaration_decl declaration in
    {
      default = default';
      source = source';
      specifiers = specifiers';
      declaration = declaration'
    }

  method declare_export_declaration_decl
    (decl: ('M, 'T) Ast.Statement.DeclareExportDeclaration.declaration)
        : ('N, 'U) Ast.Statement.DeclareExportDeclaration.declaration =
    let open Ast.Statement.DeclareExportDeclaration in
    match decl with
    | Variable (annot, decl_var) ->
      Variable (f annot, this#declare_variable decl_var)
    | Function (annot, decl_func) ->
      Function (f annot, this#declare_function decl_func)
    | Class (annot, decl_class) ->
      Class (f annot, this#declare_class decl_class)
    | DefaultType t ->
      DefaultType (this#type_ t)
    | NamedType (annot, alias) ->
      NamedType (f annot, this#type_alias alias)
    | NamedOpaqueType (annot, ot) ->
      NamedOpaqueType (f annot, this#opaque_type ot)
    | Interface (annot, iface) ->
      Interface (f annot, this#interface iface)

  method declare_function (decl: ('M, 'T) Ast.Statement.DeclareFunction.t)
                              : ('N, 'U) Ast.Statement.DeclareFunction.t =
    let open Ast.Statement.DeclareFunction in
    let { id = ident; annot; predicate } = decl in
    let id' = this#function_identifier ident in
    let annot' = this#type_annotation annot in
    let predicate' = Option.map ~f:this#type_predicate predicate in
    { id = id'; annot = annot'; predicate = predicate' }

  method declare_interface (decl: ('M, 'T) Ast.Statement.Interface.t)
                                : ('N, 'U) Ast.Statement.Interface.t =
    this#interface decl

  method declare_module _annot (m: ('M, 'T) Ast.Statement.DeclareModule.t)
                              : ('N, 'U) Ast.Statement.DeclareModule.t =
    let open Ast.Statement.DeclareModule in
    let { id; body; kind } = m in
    let id' = match id with
      | Identifier (annot, name) -> Identifier (f annot, name)
      | Literal (annot, name) -> Literal (f annot, name)
    in
    let kind' = match kind with
      | CommonJS annot -> CommonJS (f annot)
      | ES annot -> ES (f annot)
    in
    let body' = (f * this#block) body in
    { id = id'; body = body'; kind = kind' }

  method declare_module_exports _annot (t_annot: ('M, 'T) Ast.Type.annotation)
                                              : ('N, 'U) Ast.Type.annotation =
    this#type_annotation t_annot

  method declare_type_alias (decl: ('M, 'T) Ast.Statement.TypeAlias.t)
                                : ('N, 'U) Ast.Statement.TypeAlias.t =
    this#type_alias decl

  method declare_variable (decl: ('M, 'T) Ast.Statement.DeclareVariable.t)
                              : ('N, 'U) Ast.Statement.DeclareVariable.t =
    let open Ast.Statement.DeclareVariable in
    let { id = ident; annot } = decl in
    let id' = this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Var ident in
    let annot' = Option.map ~f:this#type_annotation annot in
    { id = id'; annot = annot' }

  method do_while (stuff: ('M, 'T) Ast.Statement.DoWhile.t) : ('N, 'U) Ast.Statement.DoWhile.t =
    let open Ast.Statement.DoWhile in
    let { body; test } = stuff in
    let body' = this#statement body in
    let test' = this#predicate_expression test in
    { body = body'; test = test' }

  method empty () =
    ()

  method export_default_declaration _loc (decl: ('M, 'T) Ast.Statement.ExportDefaultDeclaration.t)
                                              : ('N, 'U) Ast.Statement.ExportDefaultDeclaration.t =
    let open Ast.Statement.ExportDefaultDeclaration in
    let { default; declaration } = decl in
    let default' = f default in
    let declaration' = this#export_default_declaration_decl declaration in
    { default = default'; declaration = declaration' }

  method export_default_declaration_decl
      (decl: ('M, 'T) Ast.Statement.ExportDefaultDeclaration.declaration)
          : ('N, 'U) Ast.Statement.ExportDefaultDeclaration.declaration =
    let open Ast.Statement.ExportDefaultDeclaration in
    match decl with
    | Declaration stmt -> Declaration (this#statement stmt)
    | Expression expr -> Expression (this#expression expr)

  method export_named_declaration _loc (decl: ('M, 'T) Ast.Statement.ExportNamedDeclaration.t)
                                            : ('N, 'U) Ast.Statement.ExportNamedDeclaration.t =
    let open Ast.Statement.ExportNamedDeclaration in
    let { exportKind; source; specifiers; declaration } = decl in
    let source' = Option.map ~f:(f * id) source in
    let specifiers' = Option.map ~f:this#export_named_specifier specifiers in
    let declaration' = Option.map ~f:this#statement declaration in
    { exportKind; source = source'; specifiers = specifiers'; declaration = declaration' }

  method export_named_specifier (spec: 'M Ast.Statement.ExportNamedDeclaration.specifier)
                                    : 'N Ast.Statement.ExportNamedDeclaration.specifier =
    let open Ast.Statement.ExportNamedDeclaration in
    match spec with
    | ExportSpecifiers specs -> ExportSpecifiers (List.map this#export_specifier specs)
    | ExportBatchSpecifier (annot, name) ->
      let annot' = f annot in
      let name' = Option.map ~f:(f * id) name in
      ExportBatchSpecifier (annot', name')

  method export_specifier (spec : 'M Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t)
                                : 'N Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t =
    let open Ast.Statement.ExportNamedDeclaration.ExportSpecifier in
    let annot, { local; exported } = spec in
    let local' = (f * id) local in
    let exported' = Option.map ~f:(f * id) exported in
    f annot, { local = local'; exported = exported' }

  method expression_statement (stmt: ('M, 'T) Ast.Statement.Expression.t)
                                  : ('N, 'U) Ast.Statement.Expression.t =
    let open Ast.Statement.Expression in
    let { expression = expr; directive; } = stmt in
    { expression = this#expression expr; directive; }

  method expression_or_spread expr_or_spread =
    let open Ast.Expression in
    match expr_or_spread with
    | Expression expr ->
      Expression (this#expression expr)
    | Spread spread ->
      Spread (this#spread_element spread)

  method for_in_statement (stmt: ('M, 'T) Ast.Statement.ForIn.t) : ('N, 'U) Ast.Statement.ForIn.t =
    let open Ast.Statement.ForIn in
    let { left; right; body; each } = stmt in
    let left' = this#for_in_statement_lhs left in
    let right' = this#expression right in
    let body' = this#statement body in
    { left = left'; right = right'; body = body'; each }

  method for_in_statement_lhs (left: ('M, 'T) Ast.Statement.ForIn.left)
                                  : ('N, 'U) Ast.Statement.ForIn.left =
    let open Ast.Statement.ForIn in
    match left with
    | LeftDeclaration (annot, decl) ->
      LeftDeclaration (f annot, this#variable_declaration decl)
    | LeftPattern patt -> LeftPattern (this#for_in_assignment_pattern patt)

  method for_of_statement (stuff: ('M, 'T) Ast.Statement.ForOf.t) : ('N, 'U) Ast.Statement.ForOf.t =
    let open Ast.Statement.ForOf in
    let { left; right; body; async } = stuff in
    let left' = this#for_of_statement_lhs left in
    let right' = this#expression right in
    let body' = this#statement body in
    { left = left'; right = right'; body = body'; async }

  method for_of_statement_lhs (left: ('M, 'T) Ast.Statement.ForOf.left) =
    let open Ast.Statement.ForOf in
    match left with
    | LeftDeclaration (annot, decl) ->
      LeftDeclaration (f annot, this#variable_declaration decl)
    | LeftPattern patt ->
      LeftPattern (this#for_of_assignment_pattern patt)

  method for_statement (stmt: ('M, 'T) Ast.Statement.For.t) : ('N, 'U) Ast.Statement.For.t =
    let open Ast.Statement.For in
    let { init; test; update; body } = stmt in
    let init' = Option.map ~f:this#for_statement_init init in
    let test' = Option.map ~f:this#predicate_expression test in
    let update' = Option.map ~f:this#expression update in
    let body' = this#statement body in
    { init = init'; test = test'; update = update'; body = body' }

  method for_statement_init (init: ('M, 'T) Ast.Statement.For.init)
                                : ('N, 'U) Ast.Statement.For.init =
    let open Ast.Statement.For in
    match init with
    | InitDeclaration (annot, decl) ->
      InitDeclaration (f annot, this#variable_declaration decl)
    | InitExpression expr ->
      InitExpression (this#expression expr)

  method function_param_type (fpt: ('M, 'T) Ast.Type.Function.Param.t)
                                : ('N, 'U) Ast.Type.Function.Param.t =
    let open Ast.Type.Function.Param in
    let annot, { annot = t_annot; name; optional; } = fpt in
    let t_annot' = this#type_ t_annot in
    let name' = Option.map ~f:(f * id) name in
    f annot, { annot = t_annot'; name = name'; optional }

  method function_rest_param_type (frpt: ('M, 'T) Ast.Type.Function.RestParam.t)
                                      : ('N, 'U) Ast.Type.Function.RestParam.t =
    let open Ast.Type.Function.RestParam in
    let annot, { argument } = frpt in
    let argument' = this#function_param_type argument in
    f annot, { argument = argument' }

  method function_type (ft: ('M, 'T) Ast.Type.Function.t) : ('N, 'U) Ast.Type.Function.t =
    let open Ast.Type.Function in
    let {
      params = (params_annot, { Params.params = ps; rest = rpo });
      return;
      tparams;
    } = ft in
    let ps' = List.map this#function_param_type ps in
    let rpo' = Option.map ~f:this#function_rest_param_type rpo in
    let return' = this#type_ return in
    let tparams' = Option.map ~f:this#type_parameter_declaration tparams in
    {
      params = (f params_annot, { Params.params = ps'; rest = rpo' });
      return = return';
      tparams = tparams';
    }

  method label_identifier (ident: 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    this#identifier ident

  method object_property_value_type (opvt: ('M, 'T) Ast.Type.Object.Property.value)
                                        : ('N, 'U) Ast.Type.Object.Property.value =
    let open Ast.Type.Object.Property in
    match opvt with
    | Init t -> Init (this#type_ t)
    | Get (annot, ft) ->
      Get (f annot, this#function_type ft)
    | Set (annot, ft) ->
      Set (f annot, this#function_type ft)

  method object_property_type (opt: ('M, 'T) Ast.Type.Object.Property.t)
                                  : ('N, 'U) Ast.Type.Object.Property.t =
    let open Ast.Type.Object.Property in
    let annot, { key; value; optional; static; proto; _method; variance; } = opt in
    let key' = this#object_key key in
    let value' = this#object_property_value_type value in
    let variance' = Option.map ~f:(f * id) variance in
    f annot, { key = key'; value = value'; optional; static; proto; _method; variance = variance' }

  method object_type (ot: ('M, 'T) Ast.Type.Object.t) : ('N, 'U) Ast.Type.Object.t =
    let open Ast.Type.Object in
    let { properties ; exact; } = ot in
    let properties' = List.map this#object_type_property properties in
    { properties = properties'; exact }

  method object_type_property (prop : ('M, 'T) Ast.Type.Object.property)
                                    : ('N, 'U) Ast.Type.Object.property =
    let open Ast.Type.Object in
    match prop with
    | Property prop -> Property (this#object_property_type prop)
    | SpreadProperty (annot, { SpreadProperty.argument }) ->
      let argument' = this#type_ argument in
      SpreadProperty (f annot, { SpreadProperty.argument = argument' })
    | Indexer (annot, indexer) ->
      let open Indexer in
      let { id = id_; key; value; static; variance } = indexer in
      let id' = Option.map ~f:(f * id) id_ in
      let key' = this#type_ key in
      let value' = this#type_ value in
      let variance' = Option.map ~f:(f * id) variance in
      Indexer (f annot, { id = id'; key = key'; value = value'; static; variance = variance' })
    | CallProperty (annot, { CallProperty.value; static }) ->
      let open CallProperty in
      let value' = (f * this#function_type) value in
      CallProperty (f annot, { value = value'; static })
    | InternalSlot (annot, islot) ->
      let open InternalSlot in
      let { id = id_; value; _ } = islot in
      let id' = (f * id) id_ in
      let value' = this#type_ value in
      InternalSlot (f annot, { islot with id = id'; value = value' })

  method interface_type (i: ('M, 'T) Ast.Type.Interface.t) : ('N, 'U) Ast.Type.Interface.t =
    let open Ast.Type.Interface in
    let { extends; body } = i in
    let extends' = List.map (f * this#generic_type) extends in
    let body' = (f * this#object_type) body in
    { extends = extends'; body = body' }

  method generic_identifier_type (git: 'M Ast.Type.Generic.Identifier.t)
                                    : 'N Ast.Type.Generic.Identifier.t =
    let open Ast.Type.Generic.Identifier in
    match git with
    | Unqualified i -> Unqualified (this#identifier i)
    | Qualified (annot, { qualification; id = id_ }) ->
      let qualification' = this#generic_identifier_type qualification in
      let id' = (f * id) id_ in
      Qualified (f annot, { qualification = qualification'; id = id' })

  method type_parameter_instantiation (pi: ('M, 'T) Ast.Type.ParameterInstantiation.t)
                                        : ('N, 'U) Ast.Type.ParameterInstantiation.t =
    let annot, targs = pi in
    let targs' = List.map this#type_ targs in
    f annot, targs'

  method type_parameter_declaration (pd: ('M, 'T) Ast.Type.ParameterDeclaration.t)
                                      : ('N, 'U) Ast.Type.ParameterDeclaration.t =
    let annot, type_params = pd in
    let type_params' = List.map this#type_parameter_declaration_type_param type_params in
    f annot, type_params'

  method type_parameter_declaration_type_param
    (type_param: ('M, 'T) Ast.Type.ParameterDeclaration.TypeParam.t)
              : ('N, 'U) Ast.Type.ParameterDeclaration.TypeParam.t =
    let open Ast.Type.ParameterDeclaration.TypeParam in
    let annot, { name; bound; variance; default; } = type_param in
    let name' = (f * id) name in
    let bound' = Option.map ~f:this#type_annotation bound in
    let variance' = Option.map ~f:(f * id) variance in
    let default' = Option.map ~f:this#type_ default in
    f annot, { name = name'; bound = bound'; variance = variance'; default = default'; }

  method generic_type (gt: ('M, 'T) Ast.Type.Generic.t) : ('N, 'U) Ast.Type.Generic.t =
    let open Ast.Type.Generic in
    let { id; targs; } = gt in
    let id' = this#generic_identifier_type id in
    let targs' = Option.map ~f:this#type_parameter_instantiation targs in
    { id = id'; targs = targs' }

  method type_predicate ((annot, pred) : ('M, 'T) Ast.Type.Predicate.t)
                                      : ('N, 'U) Ast.Type.Predicate.t =
    let open Ast.Type.Predicate in
    f annot,
    match pred with
    | Declared e -> Declared (this#expression e)
    | Inferred -> Inferred

  method type_ ((annot, t): ('M, 'T) Ast.Type.t) : ('N, 'U) Ast.Type.t =
    let open Ast.Type in
    g annot,
    match t with
    ( Any
    | Mixed
    | Empty
    | Void
    | Null
    | Number
    | String
    | Boolean
    | StringLiteral _
    | NumberLiteral _
    | BooleanLiteral _
    | Exists
    ) as t -> t
    | Nullable t' -> Nullable (this#type_ t')
    | Array t' -> Array (this#type_ t')
    | Typeof t' -> Typeof (this#type_ t')
    | Function ft -> Function (this#function_type ft)
    | Object ot -> Object (this#object_type ot)
    | Interface i -> Interface (this#interface_type i)
    | Generic gt -> Generic (this#generic_type gt)
    | Union (t0, t1, ts) ->
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = List.map this#type_ ts in
      Union (t0', t1', ts')
    | Intersection (t0, t1, ts) ->
      let t0' = this#type_ t0 in
      let t1' = this#type_ t1 in
      let ts' = List.map this#type_ ts in
      Intersection (t0', t1', ts')
    | Tuple ts ->
      let ts' = List.map this#type_ ts in
      Tuple ts'

  method type_annotation ((annot, t_annot): ('M, 'T) Ast.Type.annotation) =
    f annot, this#type_ t_annot

  method function_ (expr: ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
    let open Ast.Function in
    let {
      id = ident; params; body; async; generator; expression;
      predicate; return; tparams;
    } = expr in
    let ident' = Option.map ~f:this#function_identifier ident in
    let params' =
      let annot, { Params.params = params_list; rest } = params in
      let params_list' = List.map this#function_param_pattern params_list in
      let rest' = Option.map ~f:this#function_rest_element rest in
      f annot, { Params.params = params_list'; rest = rest' }
    in
    let return' = Option.map ~f:this#type_annotation return in
    let body' = match body with
      | BodyBlock (annot, block) ->
        BodyBlock (f annot, this#function_body block)
      | BodyExpression expr ->
        BodyExpression (this#expression expr)
    in
    let tparams' = Option.map ~f:this#type_parameter_declaration tparams in
    let predicate' = Option.map ~f:this#type_predicate predicate in
    {
      id = ident'; params = params'; return = return'; body = body';
      async; generator; expression; predicate = predicate'; tparams = tparams';
    }

  method function_body (block: ('M, 'T) Ast.Statement.Block.t) : ('N, 'U) Ast.Statement.Block.t =
    this#block block

  method function_identifier (ident: 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Var ident

  method function_declaration (stmt: ('M, 'T) Ast.Function.t) : ('N, 'U) Ast.Function.t =
    this#function_ stmt

  method generator (expr: ('M, 'T) Ast.Expression.Generator.t)
                        : ('N, 'U) Ast.Expression.Generator.t =
    let open Ast.Expression.Generator in
    let { blocks; filter } = expr in
    let blocks' = List.map this#comprehension_block blocks in
    let filter' = Option.map ~f:this#expression filter in
    { blocks = blocks'; filter = filter' }

  method identifier ((annot, name): 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    f annot, name

  method interface (interface: ('M, 'T) Ast.Statement.Interface.t)
                            : ('N, 'U) Ast.Statement.Interface.t =
    let open Ast.Statement.Interface in
    let { id = ident; tparams; extends; body } = interface in
    let id' = this#class_identifier ident in
    let tparams' = Option.map ~f:this#type_parameter_declaration tparams in
    let extends' = List.map (f * this#generic_type) extends in
    let body' = (f * this#object_type) body in
    { id = id'; tparams = tparams'; extends = extends'; body = body' }

  method interface_declaration (decl: ('M, 'T) Ast.Statement.Interface.t)
                                    : ('N, 'U) Ast.Statement.Interface.t =
    this#interface decl

  method private_name ((annot, ident): 'M Ast.PrivateName.t) : 'N Ast.PrivateName.t =
    f annot, this#identifier ident

  method import _annot (expr: ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
    this#expression expr

  method if_consequent_statement ~has_else (stmt: ('M, 'T) Ast.Statement.t)
                                                : ('N, 'U) Ast.Statement.t =
    ignore has_else;
    this#statement stmt

  method if_statement (stmt: ('M, 'T) Ast.Statement.If.t) : ('N, 'U) Ast.Statement.If.t =
    let open Ast.Statement.If in
    let { test; consequent; alternate } = stmt in
    let test' = this#predicate_expression test in
    let consequent' =
      this#if_consequent_statement ~has_else:(alternate <> None) consequent in
    let alternate' = Option.map ~f:this#statement alternate in
    { test = test'; consequent = consequent'; alternate = alternate' }

  method import_declaration _loc (decl: 'M Ast.Statement.ImportDeclaration.t)
                                      : 'N Ast.Statement.ImportDeclaration.t =
    let open Ast.Statement.ImportDeclaration in
    let { importKind; source; specifiers; default } = decl in
    let specifiers' = Option.map ~f:this#import_specifier specifiers in
    let default' = Option.map ~f:this#import_default_specifier default in
    let source' = (f * id) source in
    { importKind; source = source'; specifiers = specifiers'; default = default' }

  method import_specifier (specifier: 'M Ast.Statement.ImportDeclaration.specifier)
                                    : 'N Ast.Statement.ImportDeclaration.specifier =
    let open Ast.Statement.ImportDeclaration in
    match specifier with
    | ImportNamedSpecifiers named_specifiers ->
      let named_specifiers' = List.map this#import_named_specifier named_specifiers in
      ImportNamedSpecifiers named_specifiers'
    | ImportNamespaceSpecifier (annot, ident) ->
      let ident' = this#import_namespace_specifier ident in
      ImportNamespaceSpecifier (f annot, ident')

  method import_named_specifier (specifier: 'M Ast.Statement.ImportDeclaration.named_specifier)
                                          : 'N Ast.Statement.ImportDeclaration.named_specifier =
    let open Ast.Statement.ImportDeclaration in
    let { kind; local; remote } = specifier in
    let local' = Option.map ~f:this#pattern_identifier local in
    let remote' = this#pattern_identifier remote in
    { kind; local = local'; remote = remote' }

  method import_default_specifier (id: 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let id

  method import_namespace_specifier (id: 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let id

  method jsx_element (expr: ('M, 'T) Ast.JSX.element) =
    let open Ast.JSX in
    let { openingElement; closingElement; children } = expr in
    let openingElement' = this#jsx_opening_element openingElement in
    let closingElement' = Option.map ~f:this#jsx_closing_element closingElement in
    let children' = List.map this#jsx_child children in
    { openingElement = openingElement'; closingElement = closingElement'; children = children' }

  method jsx_fragment (expr: ('M, 'T) Ast.JSX.fragment) : ('N, 'U) Ast.JSX.fragment =
    let open Ast.JSX in
    let { frag_openingElement; frag_closingElement; frag_children } = expr in
    let opening' = f frag_openingElement in
    let closing' = Option.map ~f frag_closingElement in
    let children' = List.map this#jsx_child frag_children in
    { frag_openingElement = opening'; frag_closingElement = closing'; frag_children = children' }

  method jsx_opening_element (elem: ('M, 'T) Ast.JSX.Opening.t) : ('N, 'U) Ast.JSX.Opening.t =
    let open Ast.JSX.Opening in
    let annot, { name; selfClosing; attributes } = elem in
    let name' = this#jsx_name name in
    let attributes' = List.map this#jsx_opening_attribute attributes in
    f annot, { name = name'; selfClosing; attributes = attributes' }

  method jsx_closing_element (elem: 'M Ast.JSX.Closing.t) : 'N Ast.JSX.Closing.t =
    let open Ast.JSX.Closing in
    let annot, {name} = elem in
    let name' = this#jsx_name name in
    f annot, {name=name'}

  method jsx_opening_attribute (jsx_attr: ('M, 'T) Ast.JSX.Opening.attribute)
                                        : ('N, 'U) Ast.JSX.Opening.attribute =
    let open Ast.JSX.Opening in
    match jsx_attr with
    | Attribute attr ->
      Attribute (this#jsx_attribute attr)
    | SpreadAttribute (annot, attr) ->
      SpreadAttribute (f annot, this#jsx_spread_attribute attr)

  method jsx_spread_attribute (attr: ('M, 'T) Ast.JSX.SpreadAttribute.t')
                                  : ('N, 'U) Ast.JSX.SpreadAttribute.t' =
    let open Ast.JSX.SpreadAttribute in
    let { argument } = attr in
    { argument = this#expression argument }

  method jsx_attribute (attr: ('M, 'T) Ast.JSX.Attribute.t) : ('N, 'U) Ast.JSX.Attribute.t =
    let open Ast.JSX.Attribute in
    let annot, { name; value } = attr in
    let name' = match name with
      | Identifier (annot, name) -> Identifier (f annot, name)
      | NamespacedName nname -> NamespacedName (this#jsx_namespaced_name nname)
    in
    let value' = Option.map ~f:this#jsx_attribute_value value in
    f annot, { name = name'; value = value' }

  method jsx_attribute_value (value: ('M, 'T) Ast.JSX.Attribute.value)
                                  : ('N, 'U) Ast.JSX.Attribute.value =
    let open Ast.JSX.Attribute in
    match value with
    | Literal (annot, lit) -> Literal (f annot, lit)
    | ExpressionContainer (annot, expr) ->
      ExpressionContainer (f annot, this#jsx_expression expr)

  method jsx_child (child: ('M, 'T) Ast.JSX.child) : ('N, 'U) Ast.JSX.child =
    let open Ast.JSX in
    let annot, child' = child in
    f annot,
    match child' with
    | Element elem ->
      Element (this#jsx_element elem)
    | Fragment frag ->
      Fragment (this#jsx_fragment frag)
    | ExpressionContainer expr ->
      ExpressionContainer (this#jsx_expression expr)
    | SpreadChild expr ->
      SpreadChild (this#expression expr)
    | Text _ as child' -> child'

  method jsx_expression (jsx_expr: ('M, 'T) Ast.JSX.ExpressionContainer.t)
                                : ('N, 'U) Ast.JSX.ExpressionContainer.t =
    let open Ast.JSX.ExpressionContainer in
    let { expression } = jsx_expr in
    let expression' = match expression with
      | Expression expr -> Expression (this#expression expr)
      | EmptyExpression annot -> EmptyExpression (f annot)
    in
    { expression = expression' }

  method jsx_name (name: 'M Ast.JSX.name) =
    let open Ast.JSX in
    match name with
    | Identifier id -> Identifier (this#jsx_identifier id)
    | NamespacedName namespaced_name ->
      NamespacedName (this#jsx_namespaced_name namespaced_name)
    | MemberExpression member_exp ->
      MemberExpression (this#jsx_member_expression member_exp)

  method jsx_namespaced_name (namespaced_name: 'M Ast.JSX.NamespacedName.t)
                                            : 'N Ast.JSX.NamespacedName.t =
    let open Ast.JSX in
    let open NamespacedName in
    let annot, {namespace; name} = namespaced_name in
    let namespace' = this#jsx_identifier namespace in
    let name' = this#jsx_identifier name in
    f annot, {namespace=namespace'; name=name'}

  method jsx_member_expression (member_exp: 'M Ast.JSX.MemberExpression.t)
                                          : 'N Ast.JSX.MemberExpression.t =
    let open Ast.JSX in
    let annot, {MemberExpression._object; MemberExpression.property} = member_exp in
    let _object' = match _object with
      | MemberExpression.Identifier id ->
        let id' = this#jsx_identifier id in
        MemberExpression.Identifier id'
      | MemberExpression.MemberExpression nested_exp ->
        let nested_exp' = this#jsx_member_expression nested_exp in
        MemberExpression.MemberExpression nested_exp'
    in
    let property' = this#jsx_identifier property in
    f annot, MemberExpression.({_object=_object'; property=property'})

  method jsx_identifier ((annot, name): 'M Ast.JSX.Identifier.t) : 'N Ast.JSX.Identifier.t =
    f annot, name

  method labeled_statement (stmt: ('M, 'T) Ast.Statement.Labeled.t)
                                : ('N, 'U) Ast.Statement.Labeled.t =
    let open Ast.Statement.Labeled in
    let { label; body } = stmt in
    let label' = this#label_identifier label in
    let body' = this#statement body in
    { label = label'; body = body' }

  method literal (expr: Ast.Literal.t) : Ast.Literal.t = expr

  method logical (expr: ('M, 'T) Ast.Expression.Logical.t) : ('N, 'U) Ast.Expression.Logical.t =
    let open Ast.Expression.Logical in
    let { operator; left; right } = expr in
    let left' = this#expression left in
    let right' = this#expression right in
    { operator; left = left'; right = right' }

  method member (expr: ('M, 'T) Ast.Expression.Member.t) : ('N, 'U) Ast.Expression.Member.t =
    let open Ast.Expression.Member in
    let { _object; property; computed } = expr in
    let _object' = this#expression _object in
    let property' = this#member_property property in
    { _object = _object'; property = property'; computed }

  method optional_member (expr: ('M, 'T) Ast.Expression.OptionalMember.t)
                              : ('N, 'U) Ast.Expression.OptionalMember.t =
    let open Ast.Expression.OptionalMember in
    let { member; optional } = expr in
    let member' = this#member member in
    { member = member'; optional }

  method member_property (expr: ('M, 'T) Ast.Expression.Member.property)
                              : ('N, 'U) Ast.Expression.Member.property =
    let open Ast.Expression.Member in
    match expr with
    | PropertyIdentifier ident ->
      PropertyIdentifier (this#member_property_identifier ident)
    | PropertyPrivateName ident ->
      PropertyPrivateName (this#member_private_name ident)
    | PropertyExpression e ->
      PropertyExpression (this#member_property_expression e)

  method member_property_identifier (ident: 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    this#identifier ident

  method member_private_name (name: 'M Ast.PrivateName.t) : 'N Ast.PrivateName.t =
    this#private_name name

  method member_property_expression (expr: ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
    this#expression expr

  method meta_property (expr: 'M Ast.Expression.MetaProperty.t)
                            : 'N Ast.Expression.MetaProperty.t =
    let open Ast.Expression.MetaProperty in
    let { meta; property } = expr in
    { meta = (f * id) meta; property = (f * id) property }

  method new_ (expr: ('M, 'T) Ast.Expression.New.t) : ('N, 'U) Ast.Expression.New.t =
    let open Ast.Expression.New in
    let { callee; targs; arguments } = expr in
    let callee' = this#expression callee in
    let targs' = Option.map ~f:this#type_parameter_instantiation targs in
    let arguments' = List.map this#expression_or_spread arguments in
    { callee = callee'; targs = targs'; arguments = arguments' }

  method object_ (expr: ('M, 'T) Ast.Expression.Object.t) : ('N, 'U) Ast.Expression.Object.t =
    let open Ast.Expression.Object in
    let { properties } = expr in
    let properties' = List.map (fun prop ->
      match prop with
      | Property p ->
        let p' = this#object_property p in
        Property p'
      | SpreadProperty s ->
        let s' = this#spread_property s in
        SpreadProperty s'
    ) properties in
    { properties = properties' }

  method object_property (prop: ('M, 'T) Ast.Expression.Object.Property.t)
                              : ('N, 'U) Ast.Expression.Object.Property.t =
    let open Ast.Expression.Object.Property in
    let annot, prop' = prop in
    f annot,
    match prop' with
    | Init { key; value; shorthand } ->
      let key' = this#object_key key in
      let value' = this#expression value in
      Init { key = key'; value = value'; shorthand }

    | Method { key; value = fn_annot, fn } ->
      let key' = this#object_key key in
      let fn' = this#function_ fn in
      Method { key = key'; value = f fn_annot, fn' }

    | Get { key; value = fn_annot, fn } ->
      let key' = this#object_key key in
      let fn' = this#function_ fn in
      Get { key = key'; value = f fn_annot, fn' }

    | Set { key; value = fn_annot, fn } ->
      let key' = this#object_key key in
      let fn' = this#function_ fn in
      Set { key = key'; value = f fn_annot, fn' }

  method object_key (key: ('M, 'T) Ast.Expression.Object.Property.key) =
    let open Ast.Expression.Object.Property in
    match key with
    | Literal (annot, lit) ->
      Literal (f annot, this#literal lit)
    | Identifier ident ->
      Identifier (this#object_key_identifier ident)
    | PrivateName ident ->
      PrivateName (this#private_name ident)
    | Computed expr ->
      Computed (this#expression expr)

  method object_key_identifier (ident: 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    this#identifier ident

  method opaque_type (otype: ('M, 'T) Ast.Statement.OpaqueType.t)
                          : ('N, 'U) Ast.Statement.OpaqueType.t =
    let open Ast.Statement.OpaqueType in
    let { id; tparams; impltype; supertype } = otype in
    let id' = this#identifier id in
    let tparams' = Option.map ~f:this#type_parameter_declaration tparams in
    let impltype' = Option.map ~f:this#type_ impltype in
    let supertype' = Option.map ~f:this#type_ supertype  in
    {
      id = id';
      tparams = tparams';
      impltype = impltype';
      supertype = supertype'
    }

  method declare_opaque_type (otype: ('M, 'T) Ast.Statement.OpaqueType.t)
                                  : ('N, 'U) Ast.Statement.OpaqueType.t =
    this#opaque_type otype

  method function_param_pattern (expr: ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
    this#binding_pattern expr

  method variable_declarator_pattern ~kind (expr: ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
    this#binding_pattern ~kind expr

  method catch_clause_pattern (expr: ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
    this#binding_pattern ~kind:Ast.Statement.VariableDeclaration.Let expr

  method for_in_assignment_pattern (expr: ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
    this#assignment_pattern expr

  method for_of_assignment_pattern (expr: ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
    this#assignment_pattern expr

  method binding_pattern
    ?(kind=Ast.Statement.VariableDeclaration.Var)
    (expr: ('M, 'T) Ast.Pattern.t)
    : ('N, 'U) Ast.Pattern.t =
    this#pattern ~kind expr

  method assignment_pattern (expr: ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
    this#pattern expr

  (* NOTE: Patterns are highly overloaded. A pattern can be a binding pattern,
     which has a kind (Var/Let/Const, with Var being the default for all pre-ES5
     bindings), or an assignment pattern, which has no kind. Subterms that are
     patterns inherit the kind (or lack thereof). *)
  method pattern ?kind (expr: ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
    let open Ast.Pattern in
    let annot, patt = expr in
    f annot,
    match patt with
    | Object { Object.properties; annot } ->
      let properties' = List.map (this#pattern_object_p ?kind) properties in
      let annot' = Option.map ~f:this#type_annotation annot in
      Object { Object.properties = properties'; annot = annot' }
    | Array { Array.elements; annot } ->
      let elements' = List.map (Option.map ~f:(this#pattern_array_e ?kind)) elements in
      let annot' = Option.map ~f:this#type_annotation annot in
      Array { Array.elements = elements'; annot = annot' }
    | Assignment { Assignment.left; right } ->
      let left' = this#pattern_assignment_pattern ?kind left in
      let right' = this#expression right in
      Assignment { Assignment.left = left'; right = right' }
    | Identifier { Identifier.name; annot; optional } ->
      let name' = this#pattern_identifier ?kind name in
      let annot' = Option.map ~f:this#type_annotation annot in
      Identifier { Identifier.name = name'; annot = annot'; optional }
    | Expression e ->
      Expression (this#pattern_expression e)

  method pattern_identifier ?kind (ident: 'M Ast.Identifier.t) : 'N Ast.Identifier.t =
    ignore kind;
    this#identifier ident

  method pattern_literal ?kind (expr: Ast.Literal.t) : Ast.Literal.t =
    ignore kind;
    this#literal expr

  method pattern_object_p ?kind (p: ('M, 'T) Ast.Pattern.Object.property) =
    let open Ast.Pattern.Object in
    match p with
    | Property (annot, prop) ->
      Property (f annot, this#pattern_object_property ?kind prop)
    | RestProperty (annot, prop) ->
      RestProperty (f annot, this#pattern_object_rest_property ?kind prop)

  method pattern_object_property ?kind (prop: ('M, 'T) Ast.Pattern.Object.Property.t')
                                            : ('N, 'U) Ast.Pattern.Object.Property.t' =
    let open Ast.Pattern.Object.Property in
    let { key; pattern; shorthand = _ } = prop in
    let key' = this#pattern_object_property_key ?kind key in
    let pattern' = this#pattern_object_property_pattern ?kind pattern in
    { key = key'; pattern = pattern'; shorthand = false }

  method pattern_object_property_key ?kind (key: ('M, 'T) Ast.Pattern.Object.Property.key) =
    let open Ast.Pattern.Object.Property in
    match key with
    | Literal (annot, lit) ->
      Literal (f annot, this#pattern_object_property_literal_key ?kind lit)
    | Identifier identifier ->
      Identifier (this#pattern_object_property_identifier_key ?kind identifier)
    | Computed expr ->
      Computed (this#pattern_object_property_computed_key ?kind expr)

  method pattern_object_property_literal_key ?kind (key: Ast.Literal.t) : Ast.Literal.t =
    this#pattern_literal ?kind key

  method pattern_object_property_identifier_key ?kind (key: 'M Ast.Identifier.t)
                                                          : 'N Ast.Identifier.t =
    this#pattern_identifier ?kind key

  method pattern_object_property_computed_key ?kind (key: ('M, 'T) Ast.Expression.t)
                                                        : ('N, 'U) Ast.Expression.t =
    ignore kind;
    this#pattern_expression key

  method pattern_object_rest_property ?kind (prop: ('M, 'T) Ast.Pattern.Object.RestProperty.t')
                                                : ('N, 'U) Ast.Pattern.Object.RestProperty.t' =
    let open Ast.Pattern.Object.RestProperty in
    let { argument } = prop in
    let argument' = this#pattern_object_rest_property_pattern ?kind argument in
    { argument = argument' }

  method pattern_object_property_pattern ?kind (expr: ('M, 'T) Ast.Pattern.t)
                                                    : ('N, 'U) Ast.Pattern.t =
    this#pattern ?kind expr

  method pattern_object_rest_property_pattern ?kind (expr: ('M, 'T) Ast.Pattern.t)
                                                        : ('N, 'U) Ast.Pattern.t =
    this#pattern ?kind expr

  method pattern_array_e ?kind (e: ('M, 'T) Ast.Pattern.Array.element)
                                : ('N, 'U) Ast.Pattern.Array.element =
    let open Ast.Pattern.Array in
    match e with
    | Element elem ->
      Element (this#pattern_array_element_pattern ?kind elem)
    | RestElement (annot, elem) ->
      RestElement (f annot, this#pattern_array_rest_element ?kind elem)

  method pattern_array_element_pattern ?kind (expr: ('M, 'T) Ast.Pattern.t)
                                                  : ('N, 'U) Ast.Pattern.t =
    this#pattern ?kind expr

  method pattern_array_rest_element ?kind (elem: ('M, 'T) Ast.Pattern.Array.RestElement.t') =
    let open Ast.Pattern.Array.RestElement in
    let { argument } = elem in
    let argument' = this#pattern_array_rest_element_pattern ?kind argument in
    { argument = argument' }

  method pattern_array_rest_element_pattern ?kind (expr: ('M, 'T) Ast.Pattern.t)
                                                      : ('N, 'U) Ast.Pattern.t =
    this#pattern ?kind expr

  method pattern_assignment_pattern ?kind (expr: ('M, 'T) Ast.Pattern.t) : ('N, 'U) Ast.Pattern.t =
    this#pattern ?kind expr

  method pattern_expression (expr: ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
    this#expression expr

  method predicate_expression (expr: ('M, 'T) Ast.Expression.t) : ('N, 'U) Ast.Expression.t =
    this#expression expr

  method function_rest_element (expr: ('M, 'T) Ast.Function.RestElement.t)
                                    : ('N, 'U) Ast.Function.RestElement.t =
    let open Ast.Function.RestElement in
    let annot, { argument } = expr in
    f annot, { argument = this#pattern argument }

  method return (stmt: ('M, 'T) Ast.Statement.Return.t) : ('N, 'U) Ast.Statement.Return.t =
    let open Ast.Statement.Return in
    let { argument } = stmt in
    let argument' = Option.map ~f:this#expression argument in
    { argument = argument' }

  method sequence (expr: ('M, 'T) Ast.Expression.Sequence.t) : ('N, 'U) Ast.Expression.Sequence.t =
    let open Ast.Expression.Sequence in
    let { expressions } = expr in
    let expressions' = List.map this#expression expressions in
    { expressions = expressions' }

  method toplevel_statement_list (stmts: ('M, 'T) Ast.Statement.t list)
                                      : ('N, 'U) Ast.Statement.t list =
    this#statement_list stmts

  method statement_list (stmts: ('M, 'T) Ast.Statement.t list) : ('N, 'U) Ast.Statement.t list =
    List.map this#statement stmts

  method spread_element (expr: ('M, 'T) Ast.Expression.SpreadElement.t)
                            : ('N, 'U) Ast.Expression.SpreadElement.t =
    let open Ast.Expression.SpreadElement in
    let annot, { argument } = expr in
    f annot, { argument = this#expression argument }

  method spread_property (expr: ('M, 'T) Ast.Expression.Object.SpreadProperty.t)
                              : ('N, 'U) Ast.Expression.Object.SpreadProperty.t =
    let open Ast.Expression.Object.SpreadProperty in
    let annot, { argument } = expr in
    f annot, { argument = this#expression argument }

  method switch (switch: ('M, 'T) Ast.Statement.Switch.t) : ('N, 'U) Ast.Statement.Switch.t =
    let open Ast.Statement.Switch in
    let { discriminant; cases } = switch in
    let discriminant' = this#expression discriminant in
    let cases' = List.map (f * this#switch_case) cases in
    { discriminant = discriminant'; cases = cases' }

  method switch_case (case: ('M, 'T) Ast.Statement.Switch.Case.t') =
    let open Ast.Statement.Switch.Case in
    let { test; consequent } = case in
    let test' = Option.map ~f:this#expression test in
    let consequent' = this#statement_list consequent in
    { test = test'; consequent = consequent' }

  method tagged_template (expr: ('M, 'T) Ast.Expression.TaggedTemplate.t)
                              : ('N, 'U) Ast.Expression.TaggedTemplate.t =
    let open Ast.Expression.TaggedTemplate in
    let { tag; quasi } = expr in
    let tag' = this#expression tag in
    let quasi' = (f * this#template_literal) quasi in
    { tag = tag'; quasi = quasi' }

  method template_literal (expr: ('M, 'T) Ast.Expression.TemplateLiteral.t)
                              : ('N, 'U) Ast.Expression.TemplateLiteral.t =
    let open Ast.Expression.TemplateLiteral in
    let { quasis; expressions } = expr in
    let quasis' = List.map this#template_literal_element quasis in
    let expressions' = List.map this#expression expressions in
    { quasis = quasis'; expressions = expressions' }

  method template_literal_element ((annot, elem): 'M Ast.Expression.TemplateLiteral.Element.t)
                                                : 'N Ast.Expression.TemplateLiteral.Element.t =
    f annot, elem

  method throw (stmt: ('M, 'T) Ast.Statement.Throw.t) : ('N, 'U) Ast.Statement.Throw.t =
    let open Ast.Statement.Throw in
    let { argument } = stmt in
    { argument = this#expression argument }

  method try_catch (stmt: ('M, 'T) Ast.Statement.Try.t) : ('N, 'U) Ast.Statement.Try.t =
    let open Ast.Statement.Try in
    let { block; handler; finalizer } = stmt in
    let block' = (f * this#block) block in
    let handler' = Option.map ~f:(f * this#catch_clause) handler in
    let finalizer' = Option.map ~f:(f * this#block) finalizer in
    {
      block = block';
      handler = handler';
      finalizer = finalizer'
    }

  method type_cast (expr: ('M, 'T) Ast.Expression.TypeCast.t) : ('N, 'U) Ast.Expression.TypeCast.t =
    let open Ast.Expression.TypeCast in
    let { expression; annot; } = expr in
    let expression' = this#expression expression in
    let annot' = this#type_annotation annot in
    { expression = expression'; annot = annot' }

  method unary_expression (expr: ('M, 'T) Ast.Expression.Unary.t)
                              : ('N, 'U) Ast.Expression.Unary.t =
    let open Ast.Expression.Unary in
    let { argument; operator; prefix; } = expr in
    let argument' = this#expression argument in
    { argument = argument'; operator; prefix }

  method update_expression (expr: ('M, 'T) Ast.Expression.Update.t)
                                : ('N, 'U) Ast.Expression.Update.t =
    let open Ast.Expression.Update in
    let { argument; operator; prefix; } = expr in
    let argument' = this#expression argument in
    { argument = argument'; operator; prefix }

  method variable_declaration (decl: ('M, 'T) Ast.Statement.VariableDeclaration.t)
                                  : ('N, 'U) Ast.Statement.VariableDeclaration.t =
    let open Ast.Statement.VariableDeclaration in
    let { declarations; kind } = decl in
    let decls' = List.map (this#variable_declarator ~kind) declarations in
    { declarations = decls'; kind }

  method variable_declarator ~kind (decl: ('M, 'T) Ast.Statement.VariableDeclaration.Declarator.t)
                                        : ('N, 'U) Ast.Statement.VariableDeclaration.Declarator.t =
    let open Ast.Statement.VariableDeclaration.Declarator in
    let annot, { id; init } = decl in
    let id' = this#variable_declarator_pattern ~kind id in
    let init' = Option.map ~f:this#expression init in
    f annot, { id = id'; init = init' }

  method while_ (stuff: ('M, 'T) Ast.Statement.While.t) : ('N, 'U) Ast.Statement.While.t =
    let open Ast.Statement.While in
    let { test; body } = stuff in
    let test' = this#predicate_expression test in
    let body' = this#statement body in
    { test = test'; body = body' }

  method with_ (stuff: ('M, 'T) Ast.Statement.With.t) : ('N, 'U) Ast.Statement.With.t =
    let open Ast.Statement.With in
    let { _object; body } = stuff in
    let _object' = this#expression _object in
    let body' = this#statement body in
    { _object = _object'; body = body' }

  method type_alias (stuff: ('M, 'T) Ast.Statement.TypeAlias.t)
                          : ('N, 'U) Ast.Statement.TypeAlias.t =
    let open Ast.Statement.TypeAlias in
    let { id; tparams; right } = stuff in
    let id' = this#identifier id in
    let tparams' = Option.map ~f:this#type_parameter_declaration tparams in
    let right' = this#type_ right in
    { id = id'; tparams = tparams'; right = right' }

  method yield (expr: ('M, 'T) Ast.Expression.Yield.t) : ('N, 'U) Ast.Expression.Yield.t =
    let open Ast.Expression.Yield in
    let { argument; delegate } = expr in
    let argument' = Option.map ~f:this#expression argument in
    { argument = argument'; delegate }

end

let fold_program mappers ast =
  List.fold_left (fun ast m -> m#program ast) ast mappers
