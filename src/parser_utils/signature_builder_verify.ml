(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils

module Ast = Flow_ast

module LocMap = Utils_js.LocMap

module Kind = Signature_builder_kind
module Entry = Signature_builder_entry

module Deps = Signature_builder_deps.With_Loc
module File_sig = File_sig.With_Loc
module Error = Deps.Error
module Dep = Deps.Dep
module EASort = Signature_builder_deps.ExpectedAnnotationSort

module type EvalEnv = sig
  val prevent_munge: bool
  val ignore_static_propTypes: bool
  val facebook_fbt: string option
end

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
module Eval(Env: EvalEnv) = struct

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
      let { tparams; params; return } = ft in
      let tps, deps = type_params tps tparams in
      let _, { Params.params; rest; } = params in
      let deps = List.fold_left (Deps.reduce_join (function_type_param tps)) deps params in
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

    in fun tps ot ->
      let open Ast.Type.Object in
      let { properties; _ } = ot in
      List.fold_left (fun deps -> function
        | Property prop -> Deps.join (deps, object_type_prop tps prop)
        | Indexer prop -> Deps.join (deps, object_type_indexer tps prop)
        | CallProperty prop -> Deps.join (deps, object_type_call_prop tps prop)
        | SpreadProperty prop -> Deps.join (deps, object_type_spread_prop tps prop)
        | InternalSlot _prop -> Deps.unreachable
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
        | Unqualified (_, { Ast.Identifier.name; comments= _ }) ->
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
        | Unqualified (loc, { Ast.Identifier.name; comments= _ }) ->
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

  and type_params =
    let type_param tps tparam =
      let open Ast.Type.ParameterDeclaration.TypeParam in
      let _, { name = (_, { Ast.Identifier.name= x; comments= _ }); bound; default; _ } = tparam in
      let deps = match bound with
        | Ast.Type.Missing _ -> Deps.bot
        | Ast.Type.Available (_, t) -> type_ tps t in
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

  let opaque_type tps supertype =
    match supertype with
      | None -> Deps.bot
      | Some t -> type_ tps t

  let rec annot_path tps = function
    | Kind.Annot_path.Annot (_, t) -> type_ tps t
    | Kind.Annot_path.Object (path, _) -> annot_path tps path

  let rec init_path tps = function
    | Kind.Init_path.Init expr -> literal_expr tps expr
    | Kind.Init_path.Object (_, (path, _)) -> init_path tps path

  and annotation ~sort ?init tps (loc, annot) =
    match annot with
      | Some path -> annot_path tps path
      | None ->
        begin match init with
          | Some path -> init_path tps path
          | None -> Deps.top (Error.ExpectedAnnotation (loc, sort))
        end

  and annotated_type ~sort tps loc = function
    | Ast.Type.Missing _ -> Deps.top (Error.ExpectedAnnotation (loc, sort))
    | Ast.Type.Available (_, t) -> type_ tps t

  and pattern tps patt =
    let open Ast.Pattern in
    match patt with
      | loc, Identifier { Identifier.annot; _ }
      | loc, Object { Object.annot; _ }
      | loc, Array { Array.annot; _ }
        -> annotated_type ~sort:EASort.ArrayPattern tps loc annot
      | loc, Expression _ -> Deps.todo loc "Expression"

  and literal_expr tps =
    let open Ast.Expression in
    function
      | loc, Literal { Ast.Literal.value; raw = _; comments = _ } ->
        begin match value with
          | Ast.Literal.String _
          | Ast.Literal.Number _
          | Ast.Literal.Boolean _
          | Ast.Literal.Null
            -> Deps.bot
          | _ -> Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Literal))
        end
      | _, TemplateLiteral _ -> Deps.bot
      | _, Identifier stuff -> identifier stuff
      | _, Class stuff ->
        let open Ast.Class in
        let { id; body; tparams; extends; implements; _ } = stuff in
        let super, super_targs = match extends with
          | None -> None, None
          | Some (_, { Extends.expr; targs; }) -> Some expr, targs in
        let deps = class_ tparams body super super_targs implements in
        begin match id with
          | None -> deps
          | Some x -> Deps.replace_local_with_dynamic_class (Flow_ast_utils.source_of_ident x) deps
        end
      | _, Function stuff
      | _, ArrowFunction stuff
        ->
        let open Ast.Function in
        let { id = _; generator; tparams; params; return; body; _ } = stuff in
        function_ tps generator tparams params return body
      | loc, Object stuff ->
        let open Ast.Expression.Object in
        let { properties; comments= _ } = stuff in
        if properties = [] then Deps.top (Error.EmptyObject loc)
        else object_ tps loc properties
      | loc, Array stuff ->
        let open Ast.Expression.Array in
        let { elements } = stuff in
        begin match elements with
          | [] -> Deps.top (Error.EmptyArray loc)
          | e::es -> array_ tps loc (e, es)
        end
      | _, TypeCast stuff ->
        let open Ast.Expression.TypeCast in
        let { annot; _ } = stuff in
        let _, t = annot in
        type_ tps t
      | loc, Member stuff -> member loc stuff
      | loc, Import _ -> Deps.dynamic_import loc
      | loc, Call {
          Ast.Expression.Call.
          callee = (_, Identifier (_, { Ast.Identifier.name= "require"; comments= _ }));
          _
        } -> Deps.dynamic_require loc
      | _, Call {
          Ast.Expression.Call.
          callee = (_, Member {
            Ast.Expression.Member._object = (_, Identifier (_, { Ast.Identifier.name= "Object"; comments= _ }));
            property = Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name= "freeze"; comments= _ });
          });
          targs = None;
          arguments = [Expression ((_, Object _) as expr)]
        } -> literal_expr tps expr
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
          | None -> literal_expr tps right
          | Some _ -> Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Assignment))
        end
      | _, Update stuff ->
        let open Ast.Expression.Update in
        (* This operation has a simple result type. *)
        let { argument = _; _ } = stuff in
        Deps.bot

      | loc, JSXElement e ->
        let open Ast.JSX in
        let { openingElement; closingElement = _; children = _ } = e in
        let _loc, { Opening.name; selfClosing = _; attributes = _ } = openingElement in
        begin match name, Env.facebook_fbt with
          | Ast.JSX.Identifier (_loc_id, { Identifier.name = "fbt"}), Some _ -> Deps.bot
          | _ -> Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.JSXElement))
        end

      | loc, Call _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Call))
      | loc, Comprehension _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Comprehension))
      | loc, Conditional _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Conditional))
      | loc, Generator _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Generator))
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
      | loc, Yield _ ->
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Yield))

  and identifier stuff =
    let _, { Ast.Identifier.name; comments= _ } = stuff in
    Deps.value name

  and member loc stuff =
    let open Ast.Expression.Member in
    let { _object; property; _ } = stuff in
    let deps = match _object with
      | _, Ast.Expression.Identifier stuff -> identifier stuff
      | _, Ast.Expression.Member stuff -> member loc stuff
      | _ -> Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Member))
    in
    match property with
      | PropertyIdentifier _
      | PropertyPrivateName _ -> deps
      | PropertyExpression (key_loc, _) -> Deps.top (Error.UnexpectedObjectKey (loc, key_loc))

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

  and arith_binary tps operator loc left right =
    let open Ast.Expression.Binary in
    match operator with
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
        ignore tps; ignore left; ignore right; Deps.bot
      | Plus ->
        (* The result type of this operation depends in a complicated way on the left/right types. *)
        Deps.top (Error.UnexpectedExpression (loc, Ast_utils.ExpressionSort.Binary))

  and function_param tps (_, { Ast.Function.Param.argument; default = _ }) =
    pattern tps argument

  and function_rest_param tps (_, { Ast.Function.RestParam.argument }) =
    pattern tps argument

  and function_params tps params =
    let open Ast.Function in
    let _, { Params.params; rest; } = params in
    let deps = List.fold_left (Deps.reduce_join (function_param tps)) Deps.bot params in
    match rest with
      | None -> deps
      | Some param -> Deps.join (deps, function_rest_param tps param)

  and function_return tps ~is_missing_ok return =
    match return with
    | Ast.Type.Missing loc ->
      if is_missing_ok () then Deps.bot
      else Deps.top (Error.ExpectedAnnotation (loc, EASort.FunctionReturn))
    | Ast.Type.Available (_, t) -> type_ tps t

  and function_ tps generator tparams params return body =
    let tps, deps = type_params tps tparams in
    let deps = Deps.join (deps, function_params tps params) in
    let deps =
      let is_missing_ok () = not generator && Signature_utils.Procedure_decider.is body in
      Deps.join (deps, function_return tps ~is_missing_ok return)
    in
    deps

  and class_ =
    let class_element tps element =
      let open Ast.Class in
      match element with
        (* special cases *)
        | Body.Method (_, { Method.key = (Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name; comments= _ })); _ })
        | Body.Property (_, { Property.key = (Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name; comments= _ })); _ })
            when not Env.prevent_munge && Signature_utils.is_munged_property_name name ->
          Deps.bot
        | Body.Property (_, {
            Property.key = (Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name= "propTypes"; comments= _ }));
            static = true; _
          }) when Env.ignore_static_propTypes ->
          Deps.bot

        (* general cases *)
        | Body.Method (_, { Method.value; _ }) ->
          let _, { Ast.Function.generator; tparams; params; return; body; _ } = value in
          function_ tps generator tparams params return body
        | Body.Property (loc, { Property.annot; key; _ }) ->
          annotated_type ~sort:(EASort.Property key) tps loc annot
        | Body.PrivateField (loc, { PrivateField.key; annot; _ }) ->
          annotated_type ~sort:(EASort.PrivateField key) tps loc annot

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
    let array_element tps loc expr_or_spread_opt =
      let open Ast.Expression in
      match expr_or_spread_opt with
        | None -> Deps.top (Error.UnexpectedArrayHole loc)
        | Some (Expression expr) -> literal_expr tps expr
        | Some (Spread (spread_loc, _spread)) -> Deps.top (Error.UnexpectedArraySpread (loc, spread_loc))
    in
    fun tps loc elements ->
      Nel.fold_left (Deps.reduce_join (array_element tps loc)) Deps.bot elements

  and implement tps implement =
    let open Ast.Class.Implements in
    let _, { id = (_, { Ast.Identifier.name; comments= _ }); targs } = implement in
    let deps = if SSet.mem name tps then Deps.bot else Deps.type_ name in
    Deps.join (deps, type_args tps targs)

  and object_ =
    let object_property tps loc =
      let open Ast.Expression.Object.Property in
      let object_key (key_loc, key) =
        let open Ast.Expression.Object.Property in
        match key with
          | Literal _
          | Identifier _
          | PrivateName _ -> Deps.bot
          | Computed _ -> Deps.top (Error.UnexpectedObjectKey (loc, key_loc))
      in function
        | loc, Init { key; value; _ } ->
          let deps = object_key (loc, key) in
          Deps.join (deps, literal_expr tps value)
        | loc, Method { key; value = (_, fn) }
        | loc, Get { key; value = (_, fn) }
        | loc, Set { key; value = (_, fn) }
          ->
          let deps = object_key (loc, key) in
          let { Ast.Function.generator; tparams; params; return; body; _ } = fn in
          Deps.join (deps, function_ tps generator tparams params return body)
    in
    fun tps loc properties ->
      let open Ast.Expression.Object in
      List.fold_left (fun deps prop ->
        match prop with
          | Property p -> Deps.join (deps, object_property tps loc p)
          | SpreadProperty (spread_loc, _p) -> Deps.top (Error.UnexpectedObjectSpread (loc, spread_loc))
      ) Deps.bot properties

end

module Verifier(Env: EvalEnv) = struct

  module Eval = Eval(Env)

  let eval id_loc (loc, kind) =
    match kind with
      | Kind.VariableDef { id; annot; init } ->
        Eval.annotation ~sort:(EASort.VariableDefinition id) ?init
          SSet.empty (id_loc, annot)
      | Kind.FunctionDef { generator; tparams; params; return; body; } ->
        Eval.function_ SSet.empty generator tparams params return body
      | Kind.DeclareFunctionDef { annot = (_, t) } ->
        Eval.type_ SSet.empty t
      | Kind.ClassDef { tparams; body; super; super_targs; implements } ->
        Eval.class_ tparams body super super_targs implements
      | Kind.DeclareClassDef { tparams; body = (_, body); extends; mixins; implements } ->
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
      | Kind.OpaqueTypeDef { tparams; supertype } ->
        let tps, deps = Eval.type_params SSet.empty tparams in
        Deps.join (deps, Eval.opaque_type tps supertype)
      | Kind.InterfaceDef { tparams; body = (_, body); extends } ->
        let tps, deps = Eval.type_params SSet.empty tparams in
        let deps = Deps.join (deps, Eval.object_type tps body) in
        List.fold_left (Deps.reduce_join (Eval.type_ref tps)) deps extends
      | Kind.ImportNamedDef { kind; source; name } ->
        Deps.import_named (Kind.Sort.of_import_kind kind) source name
      | Kind.ImportStarDef { kind; source } ->
        Deps.import_star (Kind.Sort.of_import_kind kind) source
      | Kind.RequireDef { source; name } ->
        Deps.require ?name source
      | Kind.SketchyToplevelDef ->
        Deps.top (Deps.Error.SketchyToplevelDef loc)

  let cjs_exports =
    let tps = SSet.empty in
    function
    | File_sig.SetModuleExportsDef expr -> Eval.literal_expr tps expr
    | File_sig.AddModuleExportsDef (_id, expr) -> Eval.literal_expr tps expr
    | File_sig.DeclareModuleExportsDef (_loc, t) -> Eval.type_ tps t

  let eval_entry (id, kind) =
    let loc, _ = id in
    eval loc kind

  let eval_declare_variable loc declare_variable =
    eval_entry (Entry.declare_variable loc declare_variable)

  let eval_declare_function loc declare_function =
    eval_entry (Entry.declare_function loc declare_function)

  let eval_declare_class loc declare_class =
    eval_entry (Entry.declare_class loc declare_class)

  let eval_type_alias loc type_alias =
    eval_entry (Entry.type_alias loc type_alias)

  let eval_opaque_type loc opaque_type =
    eval_entry (Entry.opaque_type loc opaque_type)

  let eval_interface loc interface =
    eval_entry (Entry.interface loc interface)

  let eval_function_declaration loc function_declaration =
    eval_entry (Entry.function_declaration loc function_declaration)

  let eval_function_expression loc function_expression =
    eval_entry (Entry.function_expression loc function_expression)

  let eval_class loc class_ =
    eval_entry (Entry.class_ loc class_)

  let eval_declare_export_declaration = Ast.Statement.DeclareExportDeclaration.(function
    | Variable (loc, declare_variable) -> eval_declare_variable loc declare_variable
    | Function (loc, declare_function) -> eval_declare_function loc declare_function
    | Class (loc, declare_class) -> eval_declare_class loc declare_class
    | NamedType (loc, type_alias) -> eval_type_alias loc type_alias
    | NamedOpaqueType (loc, opaque_type) -> eval_opaque_type loc opaque_type
    | Interface (loc, interface) -> eval_interface loc interface
    | DefaultType t -> Eval.type_ SSet.empty t
  )

  let eval_export_default_declaration = Ast.Statement.ExportDefaultDeclaration.(function
    | Declaration (loc, Ast.Statement.FunctionDeclaration
        ({ Ast.Function.id = Some _; _ } as function_declaration)
      ) ->
      eval_function_declaration loc function_declaration
    | Declaration (_, Ast.Statement.FunctionDeclaration ({ Ast.Function.
        id = None; generator; tparams; params; return; body; _
      })) ->
      Eval.function_ SSet.empty generator tparams params return body
    | Declaration (loc, Ast.Statement.ClassDeclaration ({ Ast.Class.id = Some _; _ } as class_)) ->
      eval_class loc class_
    | Declaration (_, Ast.Statement.ClassDeclaration
        ({ Ast.Class.id = None; tparams; body; extends; implements; _ })
      ) ->
      let super, super_targs = match extends with
        | None -> None, None
        | Some (_, { Ast.Class.Extends.expr; targs; }) -> Some expr, targs in
      Eval.class_ tparams body super super_targs implements
    | Declaration _stmt -> Deps.unreachable
    | Expression (loc, Ast.Expression.Function ({ Ast.Function.id = Some _; _ } as function_)) ->
      eval_function_expression loc function_
    | Expression expr -> Eval.literal_expr SSet.empty expr
  )

  let eval_export_value_bindings named named_infos =
    let open File_sig in
    let named, ns = List.partition (function
      | _, (_, ExportNamed { kind = NamedSpecifier _; _ })
      | _, (_, ExportNs _)
        -> false
      | _, (_, _) -> true
    ) named in
    let deps = List.fold_left2 (fun deps (n, (_, export)) export_def ->
      Deps.join (deps,
      match export, export_def with
        | ExportDefault { local; _ }, DeclareExportDef decl ->
          begin match local with
            | Some id -> Deps.value (snd id)
            | None -> eval_declare_export_declaration decl
          end
        | ExportNamed { kind = NamedDeclaration; _ }, DeclareExportDef _decl ->
          Deps.value n
        | ExportDefault { local; _ }, ExportDefaultDef decl ->
          begin match local with
            | Some id -> Deps.value (snd id)
            | None -> eval_export_default_declaration decl
          end
        | ExportNamed { kind = NamedDeclaration; _ }, ExportNamedDef _stmt ->
          Deps.value n
        | _ -> assert false
      )
    ) Deps.bot named named_infos in
    List.fold_left (fun deps (_, (_, export)) ->
      Deps.join (deps,
      match export with
        | ExportNamed { kind = NamedSpecifier { local; source }; _ } ->
          begin match source with
            | None -> Deps.value (snd local)
            | Some source ->
              Deps.import_named Kind.Sort.Value source local
          end
        | ExportNs { source; _ } ->
          Deps.import_star Kind.Sort.Value source
        | _ -> assert false
      )
    ) deps ns

  let eval_export_type_bindings type_named type_named_infos =
    let open File_sig in
    let type_named, type_ns = List.partition (function
      | _, (_, TypeExportNamed { kind = NamedSpecifier _; _ }) -> false
      | _, (_, _) -> true
    ) type_named in
    let deps = List.fold_left2 (fun deps (n, (_, export)) export_def ->
      Deps.join (deps,
      match export, export_def with
        | TypeExportNamed { kind = NamedDeclaration; _ }, DeclareExportDef _decl ->
          Deps.type_ n
        | TypeExportNamed { kind = NamedDeclaration; _ }, ExportNamedDef _stmt ->
          Deps.type_ n
        | _ -> assert false
      )
    ) Deps.bot type_named type_named_infos in
    List.fold_left (fun deps (_, (_, export)) ->
      Deps.join (deps,
      match export with
        | TypeExportNamed { kind = NamedSpecifier { local; source }; _ } ->
          begin match source with
            | None -> Deps.type_ (snd local)
            | Some source ->
              Deps.import_named Kind.Sort.Type source local
          end
        | _ -> assert false
      )
    ) deps type_ns

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

  let dynamic_validator env (dynamic_imports, dynamic_requires) = function
    | Dep.Class (loc, x) ->
      if SMap.mem x env
      then Deps.top (Deps.Error.SketchyToplevelDef loc)
      else Deps.bot
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
                if validate (snd kind) then eval loc kind
                else Deps.top (Dep.expectation sort x loc)
              )
            ) entries Deps.bot
          | None -> Deps.global local
        end
      | Dep.Remote _ -> Deps.unit dep
      | Dep.Dynamic dynamic -> dynamic_validator env dynamic_sources dynamic

  let rec check cache env dynamic_sources deps =
    Deps.recurse (check_dep cache env dynamic_sources) deps

  and check_dep cache env dynamic_sources dep =
    if Deps.DepSet.mem dep !cache then Deps.PrintableErrorSet.empty
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
    let env =
      let local_uses = Deps.DepSet.fold Dep.local_uses !cache SSet.empty in
      SMap.filter (fun n _ -> SSet.mem n local_uses) env in
    errors, remote_dependencies, env

end
