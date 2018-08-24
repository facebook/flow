(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module LocMap = Utils_js.LocMap

module Kind = Signature_builder_kind
module Entry = Signature_builder_entry

module Deps = Signature_builder_deps
module Error = Deps.Error
module Dep = Deps.Dep

(* A signature of a module is described by exported expressions / definitions, but what we're really
   interested in is their types. In particular, we are interested in computing these types early, so
   that we can check the code inside a module against the signature in a separate pass. So the
   question is: what information is necessary to compute these types?

   Assuming we know how to map various kinds of type constructors (and destructors) to their
   meanings, all that remains to verify is that the types are well-formed: any identifiers appearing
   inside them should be defined in the top-level local scope, or imported, or global; and their
   "sort" of use (as a type or as a value) must match up with their definition.

   We break up the verification of well-formedness by computing a set of "dependencies" found by
   walking the structure of types, definitions, and expressions. The dependencies are simply the
   identifiers that are reached in this walk, coupled with their sort of use. Elsewhere, we
   recursively expand these dependencies by looking up the definitions of such identifiers, possibly
   uncovering further dependencies, and so on.

   A couple of important things to note at this point.

   1. The verification of well-formedness (and computation of types) is complete only up to the
   top-level local scope: any identifiers that are imported or global need to be resolved in a
   separate phase that builds things up in module-dependency order. To reflect this arrangement,
   verification returns not only a set of immediate errors but a set of conditions on imported and
   global identifiers that must be enforced by that separate phase.

   2. There is a fine line between errors found during verification and errors found during the
   computation of types (since both kinds of errors are static errors). Still, one might argue that
   the verification step should ensure that the computation step never fails. In that regard, the
   checks we have so far are not enough. In particular:

   (a) While classes are intended to be the only values that can be used as types, we also allow
   variables to be used as types, to account for the fact that a variable could be bound to a
   top-level local, imported, or global class. Ideally we would verify that these expectation is
   met, but we don't yet.

   (b) While destructuring only makes sense on types of the corresponding kinds (e.g., object
   destructuring would only work on object types), currently we allow destructuring on all
   types. Again, ideally we would discharge verification conditions for these and ensure that they
   are satisfied.

   (c) Parts of the module system are still under design. For example, can types be defined locally
   in anything other than the top-level scope? Do (or under what circumstances do) `require` and
   `import *` bring exported types in scope? These considerations will affect the computation step
   and ideally would be verified as well, but we're punting on them right now.
*)
module Eval = struct

  let rec type_ tps t =
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
      | _, BooleanLiteral _ -> Deps.bot
      | _, Nullable t -> type_ tps t
      | _, Function ft -> function_type tps ft
      | _, Object ot -> object_type tps ot
      | loc, Generic tr -> type_ref tps (loc, tr)
      | _, Typeof v ->
        begin match v with
          | loc, Ast.Type.Generic vr -> value_ref tps (loc, vr)
          | _ -> Deps.unreachable
        end
      | _, Interface it -> interface_type tps it
      | _, Array at -> array_type tps at
      | _, Union (t1, t2, ts) ->
        let deps = type_ tps t1 in
        let deps = Deps.join (deps, type_ tps t2) in
        List.fold_left (Deps.reduce_join (type_ tps)) deps ts
      | _, Intersection (t1, t2, ts) ->
        let deps = type_ tps t1 in
        let deps = Deps.join (deps, type_ tps t2) in
        List.fold_left (Deps.reduce_join (type_ tps)) deps ts
      | _, Tuple ts ->
        List.fold_left (Deps.reduce_join (type_ tps)) Deps.bot ts
      | _, Exists -> Deps.unreachable

  and function_type =
    let function_type_param tps param =
      let open Ast.Type.Function.Param in
      let _, { annot; _ } = param in
      type_ tps annot

    in fun tps ft ->
      let open Ast.Type.Function in
      let { params; return; _ } = ft in
      let _, { Params.params; rest; } = params in
      let deps = List.fold_left (Deps.reduce_join (function_type_param tps)) Deps.bot params in
      let deps = match rest with
      | None -> deps
      | Some (_, { RestParam.argument }) -> Deps.join (deps, function_type_param tps argument)
      in
      Deps.join (deps, type_ tps return)

  and object_type =
    let object_type_prop tps prop =
      let open Ast.Type.Object.Property in
      let _, { value; _ } = prop in
      match value with
        | Init t -> type_ tps t
        | Get (_, ft)
        | Set (_, ft)
          -> function_type tps ft
    in
    let object_type_spread_prop tps prop =
      let open Ast.Type.Object.SpreadProperty in
      let _, { argument } = prop in
      type_ tps argument
    in
    let object_type_indexer tps prop =
      let open Ast.Type.Object.Indexer in
      let _, { key; value; _ } = prop in
      Deps.join (type_ tps key, type_ tps value)
    in
    let object_type_call_prop tps prop =
      let open Ast.Type.Object.CallProperty in
      let _, { value = (_, ft); _ } = prop in
      function_type tps ft
    in
    let object_type_internal_slot tps prop =
      let open Ast.Type.Object.InternalSlot in
      let _, { value; _ } = prop in
      type_ tps value

    in fun tps ot ->
      let open Ast.Type.Object in
      let { properties; _ } = ot in
      List.fold_left (fun deps -> function
        | Property prop -> Deps.join (deps, object_type_prop tps prop)
        | SpreadProperty prop -> Deps.join (deps, object_type_spread_prop tps prop)
        | Indexer prop -> Deps.join (deps, object_type_indexer tps prop)
        | CallProperty prop -> Deps.join (deps, object_type_call_prop tps prop)
        | InternalSlot prop -> Deps.join (deps, object_type_internal_slot tps prop)
      ) Deps.bot properties

  and interface_type tps it =
      let open Ast.Type.Interface in
      let { body = (_, ot); _ } = it in
      object_type tps ot

  and array_type tps at =
    type_ tps at

  and type_ref =
    let open Ast.Type.Generic in
    let rec qualified_type_ref tps qualification =
      let open Identifier in
      match qualification with
        | Unqualified (_, name) ->
          if SSet.mem name tps then Deps.bot else Deps.type_ name
        | Qualified (_, { qualification; _ }) -> qualified_type_ref tps qualification
    in
    fun tps (_, r) ->
      let { id; targs } = r in
      let deps = qualified_type_ref tps id in
      Deps.join (deps, type_args tps targs)

  and value_ref =
    let open Ast.Type.Generic in
    let rec qualified_value_ref tps qualification =
      let open Identifier in
      match qualification with
        | Unqualified (loc, name) ->
          if SSet.mem name tps then Deps.top (Error.InvalidTypeParamUse loc) else Deps.value name
        | Qualified (_, { qualification; _ }) -> qualified_value_ref tps qualification
    in
    fun tps (_, r) ->
      let { id; targs } = r in
      let deps = qualified_value_ref tps id in
      Deps.join (deps, type_args tps targs)

  and type_args tps = function
    | None -> Deps.bot
    | Some (_, ts) -> List.fold_left (Deps.reduce_join (type_ tps)) Deps.bot ts

  let opaque_type tps impltype supertype =
    match impltype, supertype with
      | None, None -> Deps.bot
      | None, Some t | Some t, None -> type_ tps t
      | Some t1, Some t2 -> Deps.join (type_ tps t1, type_ tps t2)

  let type_params =
    let type_param tps tparam =
      let open Ast.Type.ParameterDeclaration.TypeParam in
      let _, { name = (_, x); bound; default; _ } = tparam in
      let deps = match bound with
        | None -> Deps.bot
        | Some (_, t) -> type_ tps t in
      let deps = match default with
        | None -> deps
        | Some t -> Deps.join (deps, type_ tps t) in
      x, deps
    in fun tps ->
      let init = tps, Deps.bot in
      function
      | None -> init
      | Some (_, tparams) ->
        List.fold_left (fun (tps, deps) tparam ->
          let tp, deps' = type_param tps tparam in
          SSet.add tp tps, Deps.join (deps, deps')
        ) init tparams

  let rec annot_path tps = function
    | Kind.Annot_path.Annot (_, t) -> type_ tps t
    | Kind.Annot_path.Object (path, _) -> annot_path tps path
    | Kind.Annot_path.Array (path, _) -> annot_path tps path

  let rec annotation ?init tps (loc, annot) =
    match annot with
      | Some path -> annot_path tps path
      | None ->
        begin match init with
          | Some expr -> literal_expr tps expr
          | None -> Deps.top (Error.ExpectedAnnotation loc)
        end

  and pattern tps patt =
    let open Ast.Pattern in
    match patt with
      | loc, Identifier { Identifier.annot; _ } -> annotation tps (loc, Kind.Annot_path.mk_annot annot)
      | loc, Object { Object.annot; _ } -> annotation tps (loc, Kind.Annot_path.mk_annot annot)
      | loc, Array { Array.annot; _ } -> annotation tps (loc, Kind.Annot_path.mk_annot annot)
      | _, Assignment { Assignment.left; _ } -> pattern tps left
      | loc, Expression _ -> Deps.todo loc "Expression"

  and literal_expr tps =
    let open Ast.Expression in
    function
      | _, Literal _ -> Deps.bot
      | _, TemplateLiteral _ -> Deps.bot
      | _, Identifier (_, name) -> Deps.value name
      | _, Class stuff ->
        let open Ast.Class in
        let { id; body; tparams; extends; implements; _ } = stuff in
        begin match id with
          | None ->
            begin
              let super, super_targs = match extends with
                | None -> None, None
                | Some (_, { Extends.expr; targs; }) -> Some expr, targs in
              class_ tparams body super super_targs implements
            end
          | Some (_, name) -> Deps.value name
        end
      | loc, Function stuff
      | loc, ArrowFunction stuff
        ->
        let open Ast.Function in
        let { id; tparams; params; return; _ } = stuff in
        begin match id with
          | None -> function_ tps tparams params (loc, return)
          | Some (_, name) -> Deps.value name
        end
      | _, Object stuff ->
        let open Ast.Expression.Object in
        let { properties } = stuff in
        object_ tps properties
      | _, Array stuff ->
        let open Ast.Expression.Array in
        let { elements } = stuff in
        array_ tps elements
      | _, TypeCast stuff ->
        let open Ast.Expression.TypeCast in
        let { annot; _ } = stuff in
        let _, t = annot in
        type_ tps t
      | loc, Member stuff ->
        let open Ast.Expression.Member in
        let { _object; property; _ } = stuff in
        let deps = literal_expr tps _object in
        begin match property with
          | PropertyIdentifier _
          | PropertyPrivateName _ -> deps
          | PropertyExpression _ -> Deps.top (Error.UnexpectedObjectKey loc)
        end
      | loc, Import _ -> Deps.dynamic_import loc
      | loc, Call stuff
          when begin
            let { Ast.Expression.Call.callee; _ } = stuff in
            match callee with
              | _, Identifier (_, "require") -> true
              | _ -> false
          end -> Deps.dynamic_require loc
      | loc, Unary stuff ->
        let open Ast.Expression.Unary in
        let { operator; argument; _ } = stuff in
        arith_unary tps operator loc argument
      | loc, Binary stuff ->
        let open Ast.Expression.Binary in
        let { operator; left; right } = stuff in
        arith_binary tps operator loc left right
      | loc, Sequence stuff ->
        let open Ast.Expression.Sequence in
        let { expressions } = stuff in
        begin match List.rev expressions with
          | expr::_ -> literal_expr tps expr
          | [] -> Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Sequence))
        end
      | loc, Assignment stuff ->
        let open Ast.Expression.Assignment in
        let { operator; left = _; right } = stuff in
        begin match operator with
          | Assign -> literal_expr tps right
          | _ -> Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Assignment))
        end
      | loc, Call _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Call))
      | loc, Comprehension _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Comprehension))
      | loc, Conditional _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Conditional))
      | loc, Generator _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Generator))
      | loc, JSXElement _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.JSXElement))
      | loc, JSXFragment _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.JSXFragment))
      | loc, Logical _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Logical))
      | loc, MetaProperty _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.MetaProperty))
      | loc, New _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.New))
      | loc, OptionalCall _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.OptionalCall))
      | loc, OptionalMember _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.OptionalMember))
      | loc, Super ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Super))
      | loc, TaggedTemplate _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.TaggedTemplate))
      | loc, This ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.This))
      | loc, Update _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Update))
      | loc, Yield _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Yield))

  and arith_unary tps operator loc argument =
    let open Ast.Expression.Unary in
    match operator with
      | Minus
      | Plus
      | Not
      | BitNot
      | Typeof
      | Void
      | Delete
        ->
        (* These operations have simple result types. *)
        ignore tps; ignore argument; Deps.bot
      | Await ->
        (* The result type of this operation depends in a complicated way on the argument type. *)
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Unary))

  and arith_binary tps operator _loc left right =
    let open Ast.Expression.Binary in
    match operator with
      | Plus ->
        let deps = literal_expr tps left in
        Deps.join (deps, literal_expr tps right)
      | Equal
      | NotEqual
      | StrictEqual
      | StrictNotEqual
      | LessThan
      | LessThanEqual
      | GreaterThan
      | GreaterThanEqual
      | LShift
      | RShift
      | RShift3
      | Minus
      | Mult
      | Exp
      | Div
      | Mod
      | BitOr
      | Xor
      | BitAnd
      | In
      | Instanceof
        ->
        (* These operations have simple result types. *)
        ignore left; ignore right; Deps.bot

  and function_ =
    let function_params tps params =
      let open Ast.Function in
      let _, { Params.params; rest; } = params in
      let deps = List.fold_left (Deps.reduce_join (pattern tps)) Deps.bot params in
      match rest with
        | None -> deps
        | Some (_, { RestElement.argument }) -> Deps.join (deps, pattern tps argument)

    in fun tps tparams params (loc, return) ->
      let tps, deps = type_params tps tparams in
      let deps = Deps.join (deps, function_params tps params) in
      Deps.join (deps, annotation tps (loc, Kind.Annot_path.mk_annot return))

  and class_ =
    let class_element tps element =
      let open Ast.Class in
      match element with
        | Body.Method (_, { Method.value; _ }) ->
          let loc, { Ast.Function.tparams; params; return; _ } = value in
          function_ tps tparams params (loc, return)
        | Body.Property (loc, { Property.annot; _ }) -> annotation tps (loc, Kind.Annot_path.mk_annot annot)
        | Body.PrivateField (loc, { PrivateField.annot; _ }) -> annotation tps (loc, Kind.Annot_path.mk_annot annot)

    in fun tparams body super super_targs implements ->
      let open Ast.Class in
      let _, { Body.body } = body in
      let tps, deps = type_params SSet.empty tparams in
      let deps = List.fold_left (Deps.reduce_join (class_element tps)) deps body in
      let deps = match super with
        | None -> deps
        | Some expr -> Deps.join (deps, literal_expr tps expr) in
      let deps = Deps.join (deps, type_args tps super_targs) in
      List.fold_left (Deps.reduce_join (implement tps)) deps implements

  and array_ =
    let spread_element tps spread_element =
      let open Ast.Expression.SpreadElement in
      let _, { argument } = spread_element in
      literal_expr tps argument
    in
    let array_element tps expr_or_spread_opt =
      let open Ast.Expression in
      match expr_or_spread_opt with
        | None -> Deps.bot
        | Some (Expression expr) -> literal_expr tps expr
        | Some (Spread spread) -> spread_element tps spread
    in
    fun tps elements ->
      List.fold_left (Deps.reduce_join (array_element tps)) Deps.bot elements

  and implement tps implement =
    let open Ast.Class.Implements in
    let _, { id = (_, name); targs } = implement in
    let deps = if SSet.mem name tps then Deps.bot else Deps.type_ name in
    Deps.join (deps, type_args tps targs)

  and object_ =
    let object_property tps =
      let open Ast.Expression.Object.Property in
      let object_key (loc, key) =
        let open Ast.Expression.Object.Property in
        match key with
          | Literal _
          | Identifier _
          | PrivateName _ -> Deps.bot
          | Computed _ -> Deps.top (Error.UnexpectedObjectKey loc)
      in function
        | loc, Init { key; value; _ } ->
          let deps = object_key (loc, key) in
          Deps.join (deps, literal_expr tps value)
        | loc, Method { key; value = (fn_loc, fn) }
        | loc, Get { key; value = (fn_loc, fn) }
        | loc, Set { key; value = (fn_loc, fn) }
          ->
          let deps = object_key (loc, key) in
          let open Ast.Function in
          let { tparams; params; return; _ } = fn in
          Deps.join (deps, function_ tps tparams params (fn_loc, return))
    in
    let object_spread_property tps p =
      let open Ast.Expression.Object.SpreadProperty in
      let _, { argument } = p in
      literal_expr tps argument
    in
    fun tps properties ->
      let open Ast.Expression.Object in
      List.fold_left (fun deps prop ->
        match prop with
          | Property p -> Deps.join (deps, object_property tps p)
          | SpreadProperty p -> Deps.join (deps, object_spread_property tps p)
      ) Deps.bot properties

end

  let eval (loc, kind) =
    match kind with
      | Kind.VariableDef { annot; init } ->
        Eval.annotation ?init SSet.empty (loc, annot)
      | Kind.FunctionDef { tparams; params; return; } ->
        Eval.function_ SSet.empty tparams params (loc, return)
      | Kind.DeclareFunctionDef { annot = (_, t) } ->
        Eval.type_ SSet.empty t
      | Kind.ClassDef { tparams; body; super; super_targs; implements } ->
        Eval.class_ tparams body super super_targs implements
      | Kind.DeclareClassDef { tparams; body; extends; mixins; implements } ->
        let tps, deps = Eval.type_params SSet.empty tparams in
        let deps = Deps.join (deps, Eval.object_type tps body) in
        let deps = match extends with
          | None -> deps
          | Some r -> Deps.join (deps, Eval.value_ref tps r) in
        let deps = List.fold_left (Deps.reduce_join (Eval.value_ref tps)) deps mixins in
        List.fold_left (Deps.reduce_join (Eval.implement tps)) deps implements
      | Kind.TypeDef { tparams; right } ->
        let tps, deps = Eval.type_params SSet.empty tparams in
        Deps.join (deps, Eval.type_ tps right)
      | Kind.OpaqueTypeDef { tparams; impltype; supertype } ->
        let tps, deps = Eval.type_params SSet.empty tparams in
        Deps.join (deps, Eval.opaque_type tps impltype supertype)
      | Kind.InterfaceDef { tparams; body; extends } ->
        let tps, deps = Eval.type_params SSet.empty tparams in
        let deps = Deps.join (deps, Eval.object_type tps body) in
        List.fold_left (Deps.reduce_join (Eval.type_ref tps)) deps extends
      | Kind.ImportNamedDef { kind; source; name } ->
        Deps.import_named (Kind.Sort.of_import_kind kind) source name
      | Kind.ImportStarDef { kind; source } ->
        Deps.import_star (Kind.Sort.of_import_kind kind) source
      | Kind.RequireDef { source } ->
        Deps.require source

  let cjs_exports =
    let tps = SSet.empty in
    function
    | File_sig.SetModuleExportsDef expr -> Eval.literal_expr tps expr
    | File_sig.AddModuleExportsDef (_id, expr) -> Eval.literal_expr tps expr
    | File_sig.DeclareModuleExportsDef (_loc, t) -> Eval.type_ tps t

  let eval_entry ((loc, _), kind) =
    eval (loc, kind)

  let eval_declare_variable declare_variable =
    eval_entry (Entry.declare_variable declare_variable)

  let eval_declare_function declare_function =
    eval_entry (Entry.declare_function declare_function)

  let eval_declare_class declare_class =
    eval_entry (Entry.declare_class declare_class)

  let eval_type_alias type_alias =
    eval_entry (Entry.type_alias type_alias)

  let eval_opaque_type opaque_type =
    eval_entry (Entry.opaque_type opaque_type)

  let eval_interface interface =
    eval_entry (Entry.interface interface)

  let eval_function_declaration loc function_declaration =
    let _, kind = Entry.function_declaration function_declaration in
    eval (loc, kind)

  let eval_class loc class_ =
    let _, kind = Entry.class_ class_ in
    eval (loc, kind)

  let eval_variable_declaration variable_declaration =
    List.fold_left (Deps.reduce_join eval_entry) Deps.bot @@
      Entry.variable_declaration variable_declaration

  let eval_stmt = Ast.Statement.(function
    | _, VariableDeclaration variable_declaration -> eval_variable_declaration variable_declaration
    | _, DeclareVariable declare_variable -> eval_declare_variable declare_variable
    | loc, FunctionDeclaration function_declaration -> eval_function_declaration loc function_declaration
    | _, DeclareFunction declare_function -> eval_declare_function declare_function
    | loc, ClassDeclaration class_ -> eval_class loc class_
    | _, DeclareClass declare_class -> eval_declare_class declare_class
    | _, TypeAlias type_alias -> eval_type_alias type_alias
    | _, DeclareTypeAlias type_alias -> eval_type_alias type_alias
    | _, OpaqueType opaque_type -> eval_opaque_type opaque_type
    | _, DeclareOpaqueType opaque_type -> eval_opaque_type opaque_type
    | _, InterfaceDeclaration interface -> eval_interface interface
    | _, DeclareInterface interface -> eval_interface interface

    | _, Expression _
    | _, DeclareExportDeclaration _
    | _, ExportDefaultDeclaration _
    | _, ExportNamedDeclaration _
    | _, ImportDeclaration _
    | _, Block _
    | _, Break _
    | _, Continue _
    | _, Debugger
    | _, DeclareModule _
    | _, DeclareModuleExports _
    | _, DoWhile _
    | _, Empty
    | _, For _
    | _, ForIn _
    | _, ForOf _
    | _, If _
    | _, Labeled _
    | _, Return _
    | _, Switch _
    | _, Throw _
    | _, Try _
    | _, While _
    | _, With _
      -> assert false
  )

  let eval_declare_export_declaration = Ast.Statement.DeclareExportDeclaration.(function
    | Variable (_, declare_variable) -> eval_declare_variable declare_variable
    | Function (_, declare_function) -> eval_declare_function declare_function
    | Class (_, declare_class) -> eval_declare_class declare_class
    | NamedType (_, type_alias) -> eval_type_alias type_alias
    | NamedOpaqueType (_, opaque_type) -> eval_opaque_type opaque_type
    | Interface (_, interface) -> eval_interface interface
    | DefaultType t -> Eval.type_ SSet.empty t
  )

  let eval_export_default_declaration = Ast.Statement.ExportDefaultDeclaration.(function
    | Declaration (loc, Ast.Statement.FunctionDeclaration
        ({ Ast.Function.id = Some _; _ } as function_declaration)
      ) ->
      eval_function_declaration loc function_declaration
    | Declaration (loc, Ast.Statement.ClassDeclaration ({ Ast.Class.id = Some _; _ } as class_)) ->
      eval_class loc class_
    | Declaration stmt -> eval_stmt stmt
    | Expression (loc, Ast.Expression.Function ({ Ast.Function.id = Some _; _ } as function_)) ->
      eval_function_declaration loc function_
    | Expression expr -> Eval.literal_expr SSet.empty expr
  )

  let eval_export_value_bindings named named_infos =
    let open File_sig in
    SMap.fold (fun n export deps ->
      Deps.join (
        deps,
      let export_def = SMap.get n named_infos in
      match export, export_def with
        | ExportDefault _, Some (DeclareExportDef decl) ->
          eval_declare_export_declaration decl
        | ExportNamed { kind = NamedDeclaration; _ }, Some (DeclareExportDef decl) ->
          eval_declare_export_declaration decl
        | ExportDefault _, Some (ExportDefaultDef decl) ->
          eval_export_default_declaration decl
        | ExportNamed { kind = NamedDeclaration; _ }, Some (ExportNamedDef stmt) ->
          eval_stmt stmt
        | ExportNamed { kind = NamedSpecifier { local; source }; _ }, None ->
          begin match source with
            | None -> Deps.value (snd local)
            | Some source ->
              Deps.import_named Kind.Sort.Value source local
          end
        | ExportNs { source; _ }, None ->
          Deps.import_star Kind.Sort.Value source
        | _ -> assert false
      )
    ) named Deps.bot

  let eval_export_type_bindings type_named type_named_infos =
    let open File_sig in
    SMap.fold (fun n export deps ->
      Deps.join (
        deps,
      let export_def = SMap.get n type_named_infos in
      match export, export_def with
        | TypeExportNamed { kind = NamedDeclaration; _ }, Some (DeclareExportDef decl) ->
          eval_declare_export_declaration decl
        | TypeExportNamed { kind = NamedDeclaration; _ }, Some (ExportNamedDef stmt) ->
          eval_stmt stmt
        | TypeExportNamed { kind = NamedSpecifier { local; source }; _ }, None ->
          begin match source with
            | None -> Deps.type_ (snd local)
            | Some source ->
              Deps.import_named Kind.Sort.Type source local
          end
        | _ -> assert false
      )
    ) type_named Deps.bot

  let exports file_sig =
    let open File_sig in
    let module_sig = file_sig.module_sig in
    let {
      info = exports_info;
      module_kind;
      type_exports_named;
      _
    } = module_sig in
    let { module_kind_info; type_exports_named_info } = exports_info in
    let deps = match module_kind, module_kind_info with
      | CommonJS _, CommonJSInfo cjs_exports_defs ->
        List.fold_left (Deps.reduce_join cjs_exports) Deps.bot cjs_exports_defs
      | ES { named; _ }, ESInfo named_infos ->
        eval_export_value_bindings named named_infos
      | _ -> assert false
    in
    Deps.join (
      deps,
      eval_export_type_bindings type_exports_named type_exports_named_info
    )

  let dynamic_validator (dynamic_imports, dynamic_requires) = function
    | Dep.DynamicImport loc ->
      begin match LocMap.get loc dynamic_imports with
        | None -> Deps.top (Deps.Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Import))
        | Some source -> Deps.import_star Kind.Sort.Value source
      end
    | Dep.DynamicRequire loc ->
      begin match LocMap.get loc dynamic_requires with
        | None -> Deps.top (Deps.Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Call))
        | Some source -> Deps.require source
      end

  let validate_and_eval env dynamic_sources dep =
    match dep with
      | Dep.Local local ->
        let sort, x = local in
        begin match SMap.get x env with
          | Some entries ->
            let validate = Kind.validator sort in
            Utils_js.LocMap.fold (fun loc kind deps ->
              Deps.join (
                deps,
                if validate kind then eval (loc, kind)
                else Deps.top (Dep.expectation sort x loc)
              )
            ) entries Deps.bot
          | None -> Deps.global local
        end
      | Dep.Remote _ -> Deps.unit dep
      | Dep.Dynamic dynamic -> dynamic_validator dynamic_sources dynamic

  let rec check cache env dynamic_sources deps =
    Deps.recurse (check_dep cache env dynamic_sources) deps

  and check_dep cache env dynamic_sources dep =
    if Deps.DepSet.mem dep !cache then Deps.ErrorSet.empty
    else begin
      cache := Deps.DepSet.add dep !cache;
      check cache env dynamic_sources (validate_and_eval env dynamic_sources dep)
    end

  let check env file_sig deps =
    let cache = ref Deps.DepSet.empty in
    let dynamic_sources =
      let open File_sig in
      let requires = file_sig.module_sig.requires in
      let dynamic_imports = ref LocMap.empty in
      let dynamic_requires = ref LocMap.empty in
      List.iter (function
        | ImportDynamic { source; import_loc } ->
          dynamic_imports := LocMap.add import_loc source !dynamic_imports
        | Require { source; require_loc; bindings = None } ->
          dynamic_requires := LocMap.add require_loc source !dynamic_requires
        | _ -> ()
      ) requires;
      !dynamic_imports, !dynamic_requires in
    let errors = check cache env dynamic_sources deps in
    let remote_dependencies = Deps.DepSet.filter Dep.remote !cache in
    errors, remote_dependencies
