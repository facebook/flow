(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module is the entry point of the typechecker. It sets up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

open Utils
open Utils_js

module Ast = Spider_monkey_ast

open Reason_js
open Type

open Env_js.LookupMode

(*************)
(* Utilities *)
(*************)

(* type exemplar set - reasons are not considered in compare *)
module TypeExSet = Set.Make(struct
  include Type
  let compare = reasonless_compare
end)

let mk_object cx reason =
  Flow_js.mk_object_with_proto cx reason (MixedT reason)

let summarize cx t = match t with
  | OpenT _ ->
      let reason = reason_of_t t in
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        Flow_js.flow cx (t, SummarizeT (reason, tvar))
      )
  (* These remaining cases simulate SummarizeT semantics, and are a slight
     optimization in that they avoid creation of fresh type
     variables. Semantically, we could have the above case fire unconditionally,
     since the fresh type variable is unified in all cases. *)
  | StrT (reason, AnyLiteral) -> t
  | StrT (reason, _) -> StrT.why reason
  | NumT (reason, AnyLiteral) -> t
  | NumT (reason, _) -> NumT.why reason
  | _ -> t

let mk_module_t cx reason = ModuleT(
  reason,
  {
    exports_tmap = Flow_js.mk_propmap cx SMap.empty;
    cjs_export = None;
  }
)

(**
 * When CommonJS modules set their export type, we do two things:
 *
 * (1) Set the type in the cjs_export slot of the ModuleT container
 *
 * (2) If the type is an object, mark it's properties as named exports, via
 *     CJSExtractNamedExportsT. (this is for convenience as part of our
 *     ES <-> CJS module interop semantics)
 *)
let mk_commonjs_module_t cx reason_exports_module reason export_t =
  let module_t = ModuleT (reason_exports_module, {
    exports_tmap = Flow_js.mk_propmap cx SMap.empty;
    cjs_export = Some export_t;
  }) in
  Flow_js.mk_tvar_where cx reason (fun t ->
    Flow_js.flow cx (
      export_t,
      CJSExtractNamedExportsT(reason, module_t, t)
    )
  )

(* given a module name, return associated tvar if already
 * present in module map, or create and add *)
let get_module_t cx m reason =
  match SMap.get m (Context.module_map cx) with
  | Some t -> t
  | None ->
      Flow_js.mk_tvar_where cx reason (fun t -> Context.add_module cx m t)

let require cx m_name loc =
  Context.add_require cx m_name loc;
  Type_inference_hooks_js.dispatch_require_hook cx m_name loc;
  let reason = mk_reason (spf "CommonJS exports of \"%s\"" m_name) loc in
  Flow_js.mk_tvar_where cx reason (fun t ->
    Flow_js.flow cx (
      get_module_t cx m_name (mk_reason m_name loc),
      CJSRequireT(reason, t)
    )
  )

let import cx m_name loc =
  Context.add_require cx m_name loc;
  Type_inference_hooks_js.dispatch_import_hook cx m_name loc;
  get_module_t cx m_name (mk_reason m_name loc)

let import_ns cx reason module_name loc =
  Context.add_require cx module_name loc;
  Type_inference_hooks_js.dispatch_import_hook cx module_name loc;
  Flow_js.mk_tvar_where cx reason (fun t ->
    Flow_js.flow cx (
      get_module_t cx module_name (mk_reason module_name loc),
      ImportModuleNsT(reason, t)
    )
  )

let exports cx =
  let m = Modulename.to_string (Context.module_name cx) in
  let loc = Loc.({ none with source = Some (Context.file cx) }) in
  get_module_t cx m (Reason_js.mk_reason "exports" loc)

let set_module_t cx reason f =
  let module_name = Modulename.to_string (Context.module_name cx) in
  Context.add_module cx module_name (Flow_js.mk_tvar_where cx reason f)

(**
 * Before running inference, we assume that we're dealing with a CommonJS
 * module that has a built-in, initialized `exports` object (i.e. it is not an
 * ES module).
 *
 * During inference, if we encounter an assignment to module.exports then we
 * use this as an indicator that the module is definitely a CommonJS module --
 * but that the bult-in `exports` value is no longer the exported variable.
 * Instead, whatever was assigned to `module.exports` is now that CJS exported
 * value.
 *
 * On the other hand, if we encounter an ES `export` statement during inference,
 * we use this as an indicator that the module is an ES module. The one
 * exception to this rule is that we do not use `export type` as an indicator of
 * an ES module (since we want CommonJS modules to be able to use `export type`
 * as well).
 *
 * At the end of inference, we make use of this information to decide which
 * types to store as the expors of the module (i.e. Do we use the built-in
 * `exports` value? Do we use the type that clobbered `module.exports`? Or do we
 * use neither because the module only has direct ES exports?).
 *)
let mark_exports_type cx reason new_exports_type = Context.(
  (match (Context.module_exports_type cx, new_exports_type) with
  | (ESModule, CommonJSModule(Some _))
  | (CommonJSModule(Some _), ESModule)
    ->
      let msg =
        "Unable to determine module type (CommonJS vs ES) if both an export " ^
        "statement and module.exports are used in the same module!"
      in
      Flow_js.add_warning cx [(reason, msg)]
  | _ -> ()
  );
  Context.set_module_exports_type cx new_exports_type
)

(**
 * Given an exported default declaration, identify nameless declarations and
 * name them with a special internal name that can be used to reference them
 * when assigning the export value.
 *)
let nameify_default_export_decl decl = Ast.Statement.(
  match decl with
  | loc, FunctionDeclaration(func_decl) ->
    if func_decl.FunctionDeclaration.id <> None then decl else
      loc, FunctionDeclaration(FunctionDeclaration.({
        func_decl with
          id = Some(Ast.Identifier.(loc, {
            name = internal_name "*default*";
            typeAnnotation = None;
            optional = false;
          }));
      }))

  | loc, ClassDeclaration(class_decl) ->
    if class_decl.Ast.Class.id <> None then decl else
      loc, ClassDeclaration(Ast.Class.({
        class_decl with
          id = Some(Ast.Identifier.(loc, {
            name = internal_name "*default*";
            typeAnnotation = None;
            optional = false;
          }));
      }))

  | _ -> decl
)

(**
 * Given a LHS destructuring pattern, extract a list of (loc, identifier-name)
 * tuples from the pattern that represent new bindings. This is primarily useful
 * for exporting a destructuring variable declaration.
 *)
let rec extract_destructured_bindings accum pattern = Ast.Pattern.(
  match pattern with
  | Identifier(loc, n) ->
    let name = n.Ast.Identifier.name in
    (loc, name)::accum

  | Object(n) ->
    let props = n.Object.properties in
    List.fold_left extract_obj_prop_pattern_bindings accum props

  | Array(n) ->
    let elems = n.Array.elements in
    List.fold_left extract_arr_elem_pattern_bindings accum elems

  | Expression(_) ->
    failwith "Parser Error: Expression patterns don't exist in JS."
) and extract_obj_prop_pattern_bindings accum = Ast.Pattern.(function
  | Object.Property(_, prop) ->
    let (_, rhs_pattern) = prop.Object.Property.pattern in
    extract_destructured_bindings accum rhs_pattern

  | Object.SpreadProperty(_) ->
    failwith "Unsupported: Destructuring object spread properties"
) and extract_arr_elem_pattern_bindings accum = Ast.Pattern.(function
  | Some(Array.Element(_, pattern)) ->
    extract_destructured_bindings accum pattern

  | Some(Array.Spread(_, {Array.SpreadElement.argument = (_, pattern)})) ->
    extract_destructured_bindings accum pattern

  | None -> accum
)

(* Destructuring visitor for tree-shaped patterns, parameteric over an action f
   to perform at the leaves. A type for the pattern is passed, which is taken
   apart as the visitor goes deeper. *)

let rec destructuring ?(parent_pattern_t=None) cx curr_t expr f = Ast.Pattern.(function
  | loc, Array { Array.elements; _; } -> Array.(
      elements |> List.iteri (fun i -> function
        | Some (Element ((loc, _) as p)) ->
            let key = NumT (
              mk_reason "number" loc,
              Literal (float i, string_of_int i)
            ) in
            let reason = mk_reason (spf "element %d" i) loc in
            let expr = Option.map expr (fun expr ->
              loc, Ast.Expression.(Member Member.({
                _object = expr;
                property = PropertyExpression (loc, Literal Ast.Literal.({
                  value = Number (float i);
                  raw = string_of_int i;
                }));
                computed = true;
              }))
            ) in
            let refinement = Option.bind expr (fun expr ->
              Refinement.get cx expr reason
            ) in
            let (parent_pattern_t, tvar) = (match refinement with
            | Some refined_t -> (refined_t, refined_t)
            | None ->
              (curr_t, Flow_js.mk_tvar_where cx reason (fun tvar ->
                Flow_js.flow cx (curr_t, GetElemT(reason, key, tvar))
              ))
            ) in
            destructuring ~parent_pattern_t:(Some parent_pattern_t) cx tvar expr f p
        | Some (Spread (loc, { SpreadElement.argument })) ->
            let reason = mk_reason "rest of array pattern" loc in
            let tvar = Flow_js.mk_tvar cx reason in
            Flow_js.flow cx (curr_t, ArrRestT(reason, i, tvar));
            destructuring ~parent_pattern_t:(Some curr_t) cx tvar expr f argument
        | None ->
            ()
      )
    )

  | loc, Object { Object.properties; _; } -> Object.(
      let xs = ref [] in
      properties |> List.iter (function
        | Property (loc, prop) -> Property.(
            match prop with
            | { key = Identifier (loc, id); pattern = p; } ->
                let x = id.Ast.Identifier.name in
                let reason = mk_reason (spf "property `%s`" x) loc in
                xs := x :: !xs;
                let expr = Option.map expr (fun expr ->
                  loc, Ast.Expression.(Member Member.({
                    _object = expr;
                    property = PropertyIdentifier (loc, id);
                    computed = false;
                  }))
                ) in
                let refinement = Option.bind expr (fun expr ->
                  Refinement.get cx expr reason
                ) in
                let (parent_pattern_t, tvar) = (match refinement with
                | Some refined_t -> (refined_t, refined_t)
                | None ->
                  (* use the same reason for the prop name and the lookup.
                     given `var {foo} = ...`, `foo` is both. compare to `a.foo`
                     where `foo` is the name and `a.foo` is the lookup. *)
                  (curr_t, Flow_js.mk_tvar_where cx reason (fun tvar ->
                    Flow_js.flow cx (curr_t, GetPropT(reason, (reason, x), tvar))
                  ))
                ) in
                destructuring ~parent_pattern_t:(Some parent_pattern_t) cx tvar expr f p
            | _ ->
              error_destructuring cx loc
          )
        | SpreadProperty (loc, { SpreadProperty.argument }) ->
            let reason = mk_reason "object pattern spread property" loc in
            let tvar = Flow_js.mk_tvar cx reason in
            Flow_js.flow cx (curr_t, ObjRestT(reason,!xs,tvar));
            destructuring ~parent_pattern_t:(Some curr_t) cx tvar expr f argument
      )
    )

  | loc, Identifier (_, { Ast.Identifier.name; _ }) ->
      Type_inference_hooks_js.dispatch_lval_hook cx name loc (
        match (parent_pattern_t, expr) with
        (**
         * If there was a parent_pattern, we must be within a destructuring
         * pattern and a `get-def` on this identifier should point at the "def"
         * of the original property. To accompish this, we emit the type of the
         * parent pattern so that get-def can dive in to that type and extract
         * the location of the "def" of this property.
         *)
        | (Some rhs_t, _) -> Type_inference_hooks_js.RHSType rhs_t

        (**
         * If there was no parent_pattern, we must not be within a destructuring
         * pattern and a `get-def` on this identifier should point at the
         * location of the RHS of the assignment.
         *)
        | (None, Some (loc, _)) -> Type_inference_hooks_js.RHSLoc loc

        (**
         * If there was no parent_pattern and no RHS expression (i.e. `var a;`,
         * function parameters, etc), there's nothing useful we can do for a
         * `get-def` on this identifier.
         *)
        | (None, None) -> Type_inference_hooks_js.NoRHS
      );
      f cx loc name curr_t

  | loc, _ -> error_destructuring cx loc
)

and error_destructuring cx loc =
  let msg = "unsupported destructuring" in
  Flow_js.add_error cx [mk_reason "" loc, msg]

and type_of_pattern = Ast.Pattern.(function
  | loc, Array { Array.typeAnnotation; _; } -> typeAnnotation

  | loc, Object { Object.typeAnnotation; _; } -> typeAnnotation

  | loc, Identifier (_, { Ast.Identifier.typeAnnotation; _; }) -> typeAnnotation

  | loc, _ -> None
)

(* instantiate pattern visitor for assignments *)
let destructuring_assignment cx rhs_t expr =
  destructuring cx rhs_t (Some expr) (fun cx loc name t ->
    let reason = mk_reason (spf "assignment of identifier `%s`" name) loc in
    Env_js.set_var cx name t reason
  )

(* AST helpers *)

(* translate AST async/generator flags into Scope.function_kind *)
let function_kind ~async ~generator =
  match async, generator with
  | true, true -> assert_false "async && generator"
  | true, false -> Scope.Async
  | false, true -> Scope.Generator
  | false, false -> Scope.Ordinary

let error_type cx loc msg =
  let reason = mk_reason "" loc in
  Flow_js.add_error cx [reason, msg];
  AnyT reason

let check_type_param_arity cx loc params n f =
  if List.length params = n
  then f ()
  else
    let msg = spf "Incorrect number of type parameters (expected %n)" n in
    error_type cx loc msg

let is_suppress_type type_name = FlowConfig.(Opts.(
  let config = get_unsafe () in
  SSet.mem type_name config.options.suppress_types
))

let are_getters_and_setters_enabled () = FlowConfig.(Opts.(
  let config = get_unsafe () in
  config.options.enable_unsafe_getters_and_setters
))

let warn_or_ignore_decorators cx decorators_list = FlowConfig.(Opts.(
  if decorators_list = [] then () else
  match (get_unsafe ()).options.esproposal_decorators with
  | ESPROPOSAL_ENABLE -> failwith "Decorators cannot be enabled!"
  | ESPROPOSAL_IGNORE -> ()
  | ESPROPOSAL_WARN ->
      let first_loc = fst (List.hd decorators_list) in
      let last_loc =
        fst (List.nth decorators_list ((List.length decorators_list) - 1))
      in
      let loc = Loc.btwn first_loc last_loc in
      let reason = mk_reason "Experimental decorator usage" loc in
      Flow_js.add_warning cx [
        reason,
        "Decorators are an early stage proposal that may change. " ^
        "Additionally, Flow does not account for the type implications of " ^
        "decorators at this time."
      ]
))

(**********************************)
(* Transform annotations to types *)
(**********************************)

(* converter *)
let rec convert cx type_params_map = Ast.Type.(function

  | loc, Any -> AnyT.at loc

  | loc, Void -> void_ loc

  | loc, Null -> NullT.at loc

  | loc, Number -> NumT.at loc

  | loc, String -> StrT.at loc

  | loc, Boolean -> BoolT.at loc

  | loc, Nullable t -> MaybeT (convert cx type_params_map t)

  | loc, Union ts ->
      let ts = List.map (convert cx type_params_map) ts in
      (* "Flatten" out any unions in this union, like
       * var a: number | (string | bool) *)
      let ts = List.map (function
        | UnionT (r, ts) -> ts
        | t -> [t]) ts in
      let ts = List.flatten ts in
      UnionT (mk_reason "union type" loc, ts)

  | loc, Intersection ts ->
      let ts = List.map (convert cx type_params_map) ts in
      IntersectionT (mk_reason "intersection type" loc, ts)

  | loc, Typeof x ->
      (match x with
      | (loc, Generic {
          Generic.id = qualification;
          typeParameters = None
        }) ->
          let valtype = convert_qualification ~lookup_mode:ForTypeof cx
            "typeof-annotation" qualification in
          Flow_js.mk_typeof_annotation cx valtype
      | _ ->
        error_type cx loc "Unexpected typeof expression")

  | loc, Tuple ts ->
      let elts = List.map (convert cx type_params_map) ts in
      let reason = mk_reason "tuple type" loc in
      let element_reason = mk_reason "tuple element" loc in
      let tx =
        if ts = []
        then Flow_js.mk_tvar cx element_reason
        else UnionT (element_reason, elts) in
      ArrT (reason, tx, elts)

  | loc, Array t ->
      let r = mk_reason "array type" loc in
      let t = convert cx type_params_map t in
      ArrT (r, t, [])

  | loc, StringLiteral { StringLiteral.value; _ }  ->
      let reason = mk_reason "string literal type" loc in
      mk_singleton_string reason value

  | loc, NumberLiteral { NumberLiteral.value; raw; _ }  ->
      let reason = mk_reason "number literal type" loc in
      mk_singleton_number reason value raw

  | loc, BooleanLiteral { BooleanLiteral.value; _ }  ->
      let reason = mk_reason "boolean literal type" loc in
      mk_singleton_boolean reason value

  (* TODO *)
  | loc, Generic { Generic.id = Generic.Identifier.Qualified (_,
         { Generic.Identifier.qualification; id; }); typeParameters } ->

    let m = convert_qualification cx "type-annotation" qualification in
    let _, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason name loc in
    let t = Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (m, GetPropT (reason, (reason, name), t));
    ) in
    let typeParameters = extract_type_param_instantiations typeParameters in
    let params = if typeParameters = []
      then None else Some typeParameters in
    mk_nominal_type cx reason type_params_map (t, params)

  (* type applications: name < params > *)
  | loc, Generic {
      Generic.id = Generic.Identifier.Unqualified (id);
      typeParameters
    } ->
    let _, { Ast.Identifier.name; _ } = id in
    let typeParameters = extract_type_param_instantiations typeParameters in

    (match name with

      (* TODO Type.Mixed *)
      | "mixed" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          MixedT.at loc
        )

      (* Array<T> *)
      | "Array" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          let t = convert cx type_params_map (List.hd typeParameters) in
          ArrT (mk_reason "array type" loc, t, [])
        )

      (* $Either<...T> is the union of types ...T *)
      | "$Either" ->
        let ts = List.map (convert cx type_params_map) typeParameters in
        UnionT (mk_reason "union type" loc, ts)

      (* $All<...T> is the intersection of types ...T *)
      | "$All" ->
        let ts = List.map (convert cx type_params_map) typeParameters in
        IntersectionT (mk_reason "intersection type" loc, ts)

      (* $Tuple<...T> is the tuple of types ...T *)
      | "$Tuple" ->
        let ts = List.map (convert cx type_params_map) typeParameters in
        ArrT (mk_reason "tuple type" loc, AnyT.t, ts)

      (* $Supertype<T> acts as any over supertypes of T *)
      | "$Supertype" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          UpperBoundT (convert cx type_params_map (List.hd typeParameters))
        )

      (* $Subtype<T> acts as any over subtypes of T *)
      | "$Subtype" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          LowerBoundT (convert cx type_params_map (List.hd typeParameters))
        )

      (* $Shape<T> matches the shape of T *)
      | "$Shape" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          ShapeT (convert cx type_params_map (List.hd typeParameters))
        )

      (* $Diff<T,S> *)
      | "$Diff" ->
        check_type_param_arity cx loc typeParameters 2 (fun () ->
          let t1 = typeParameters |> List.hd |> convert cx type_params_map in
          let t2 = typeParameters |> List.tl |> List.hd |> convert cx type_params_map in
          DiffT (t1, t2)
        )

      (* $Keys<T> is the set of keys of T *)
      (** TODO: remove $Enum **)
      | "$Keys" | "$Enum"->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          let t = convert cx type_params_map (List.hd typeParameters) in
          KeysT (mk_reason "key set" loc, t)
        )

      (* $Exports<'M'> is the type of the exports of module 'M' *)
      (** TODO: use `import typeof` instead when that lands **)
      | "$Exports" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          match List.hd typeParameters with
          | _, StringLiteral { StringLiteral.value; _ } ->
              let reason = (mk_reason (spf "exports of module `%s`" value) loc) in
              let remote_module_t =
                Env_js.get_var_declared_type cx (internal_module_name value) reason
              in
              Flow_js.mk_tvar_where cx reason (fun t ->
                Flow_js.flow cx (remote_module_t, CJSRequireT(reason, t))
              )
          | _ -> assert false
        )

      | "$Abstract" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          AbstractT(convert cx type_params_map (List.hd typeParameters))
        )

      (* Class<T> is the type of the class whose instances are of type T *)
      | "Class" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          ClassT(convert cx type_params_map (List.hd typeParameters))
        )

      | "Function" | "function" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          let reason = mk_reason "function type" loc in
          AnyFunT reason
        )

      | "Object" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          let reason = mk_reason "object type" loc in
          AnyObjT reason
        )

      | "Function$Prototype$Apply" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          let reason = mk_reason "function type" loc in
          FunProtoApplyT reason
        )

      | "Function$Prototype$Bind" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          let reason = mk_reason "function type" loc in
          FunProtoBindT reason
        )

      | "Function$Prototype$Call" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          let reason = mk_reason "function type" loc in
          FunProtoCallT reason
        )

      | "$Tainted" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          let t = convert cx type_params_map (List.hd typeParameters) in
          let reason = Reason_js.repos_reason loc (reason_of_t t) in
          UnionT (reason, [t; TaintT (mk_reason "taint" loc)])
        )


      (* You can specify in the .flowconfig the names of types that should be
       * treated like any<actualType>. So if you have
       * suppress_type=$FlowFixMe
       *
       * Then you can do
       *
       * var x: $FlowFixMe<number> = 123;
       *)
      (* TODO move these to type aliases once optional type args
         work properly in type aliases: #7007731 *)
      | type_name when is_suppress_type type_name ->
        (* Optional type params are info-only, validated then forgotten. *)
        List.iter (fun p -> ignore (convert cx type_params_map p)) typeParameters;
        AnyT.at loc

      (* TODO: presumably some existing uses of AnyT can benefit from AnyObjT
         as well: e.g., where AnyT is used to model prototypes and statics we
         don't care about; but then again, some of these uses may be internal,
         so while using AnyObjT may offer some sanity checking it might not
         reveal user-facing errors. *)

      (* in-scope type vars *)
      | _ when SMap.mem name type_params_map ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          SMap.find_unsafe name type_params_map
        )

      (* other applications with id as head expr *)
      | _ ->
        let reason = mk_reason name loc in
        let params = if typeParameters = []
          then None else Some typeParameters in
        let c = identifier ~lookup_mode:ForType cx name loc in
        mk_nominal_type cx reason type_params_map (c, params)
    )

  | loc, Function { Function.params; returnType; rest; typeParameters } ->
    let typeparams, type_params_map =
      mk_type_param_declarations cx type_params_map typeParameters in

    let rev_params_tlist, rev_params_names =
      (let rev_tlist, rev_pnames =
        List.fold_left (fun (tlist, pnames) param ->
        match param with
        | _, { Function.Param.name;
               Function.Param.typeAnnotation; optional = false; _ } ->
            (convert cx type_params_map typeAnnotation) :: tlist,
            ((snd name).Ast.Identifier.name) :: pnames
        | _, { Function.Param.name;
               Function.Param.typeAnnotation; optional = true; _ } ->
            (OptionalT (convert cx type_params_map typeAnnotation)) :: tlist,
            ((snd name).Ast.Identifier.name) :: pnames
      ) ([], []) params in
      match rest with
        | Some (_, { Function.Param.name;
                     Function.Param.typeAnnotation; _ }) ->
            let rest = mk_rest cx (convert cx type_params_map typeAnnotation) in
            rest :: rev_tlist,
            ((snd name).Ast.Identifier.name) :: rev_pnames
        | None -> rev_tlist, rev_pnames
      ) in
    let reason = mk_reason "function type" loc in
    let ft =
      FunT (
        reason,
        Flow_js.dummy_static reason,
        Flow_js.mk_tvar cx (mk_reason "prototype" loc),
        {
          this_t = Flow_js.mk_tvar cx (mk_reason "this" loc);
          params_tlist = (List.rev rev_params_tlist);
          params_names = Some (List.rev rev_params_names);
          return_t = convert cx type_params_map returnType;
          closure_t = 0;
          changeset = Changeset.empty
        })
    in
    if (typeparams = []) then ft else PolyT(typeparams, ft)

  | loc, Object { Object.properties; indexers; callProperties; } ->
    let props_map = List.fold_left (fun props_map ->
      Object.Property.(fun (loc, { key; value; optional; _ }) ->
        (match key with
          | Ast.Expression.Object.Property.Literal
              (_, { Ast.Literal.value = Ast.Literal.String name; _ })
          | Ast.Expression.Object.Property.Identifier
              (_, { Ast.Identifier.name; _ }) ->
              let t = convert cx type_params_map value in
              if optional
              then
                (* wrap types of optional properties, just like we do for
                   optional parameters *)
                SMap.add name (OptionalT t) props_map
              else
                SMap.add name t props_map
          | _ ->
            let msg = "Unsupported key in object type" in
            Flow_js.add_error cx [mk_reason "" loc, msg];
            props_map
        )
      )
    ) SMap.empty properties
    in
    let props_map = match callProperties with
      | [] -> props_map
      | [loc, { Object.CallProperty.value = (_, ft); _; }] ->
          SMap.add "$call" (convert cx type_params_map (loc, Ast.Type.Function ft)) props_map
      | fts ->
          let fts = List.map
            (fun (loc, { Object.CallProperty.value = (_, ft); _; }) ->
                convert cx type_params_map (loc, Ast.Type.Function ft))
            fts in
          let callable_reason = mk_reason "callable object type" loc in
          SMap.add "$call" (IntersectionT (callable_reason, fts)) props_map
    in
    (* Seal an object type unless it specifies an indexer. *)
    let sealed, dict = Object.Indexer.(
      match indexers with
      | [(_, { id = (_, { Ast.Identifier.name; _ }); key; value; _; })] ->
          let keyt = convert cx type_params_map key in
          let valuet = convert cx type_params_map value in
          false,
          Some { Type.
            dict_name = Some name;
            key = keyt;
            value = valuet
          }
      | [] ->
          true,
          None
      (* TODO *)
      | _ -> failwith "Unimplemented: multiple indexers"
    ) in
    let pmap = Flow_js.mk_propmap cx props_map in
    let proto = MixedT (reason_of_string "Object") in
    let flags = {
      sealed = if sealed then Sealed else UnsealedInFile (Loc.source loc);
      exact = not sealed;
      frozen = false;
    } in
    ObjT (mk_reason "object type" loc,
      Flow_js.mk_objecttype ~flags dict pmap proto)

  | loc, Exists ->
    (* Do not evaluate existential type variables when map is non-empty. This
       ensures that existential type variables under a polymorphic type remain
       unevaluated until the polymorphic type is applied. *)
    let force = SMap.is_empty type_params_map in
    let reason = mk_reason "existential" loc in
    if force then Flow_js.mk_tvar cx reason
    else ExistsT reason
  )

and convert_qualification ?(lookup_mode=ForType) cx reason_prefix
  = Ast.Type.Generic.Identifier.(function
  | Qualified (loc, { qualification; id; }) ->
    let m = convert_qualification ~lookup_mode cx reason_prefix qualification in
    let _, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason (spf "%s '<<object>>.%s')" reason_prefix name) loc in
    Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (m, GetPropT (reason, (reason, name), t));
    )

  | Unqualified (id) ->
    let loc, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason (spf "%s `%s`" reason_prefix name) loc in
    Env_js.get_var ~lookup_mode cx name reason
)

and mk_rest cx = function
  | ArrT (_, t, []) -> RestT t
  | AnyT _ as t -> RestT t
  | ArrT (r, t, _) ->
      let msg = "rest parameters should be an array type, got a tuple type instead" in
      Flow_js.add_warning cx [r,msg];
      RestT t
  | t ->
      (* unify t with Array<e>, return (RestT e) *)
      let reason = prefix_reason "element of " (reason_of_t t) in
      let tvar = Flow_js.mk_tvar cx reason in
      let arrt = ArrT(reason, tvar, []) in
      Flow_js.unify cx t arrt;
      RestT tvar

and mk_type cx type_params_map reason = function
  | None ->
      let t =
        if Context.is_weak cx
        then AnyT.why reason
        else Flow_js.mk_tvar cx reason
      in
      Hashtbl.replace (Context.annot_table cx) (loc_of_reason reason) t;
      t

  | Some annot ->
      convert cx type_params_map annot

and mk_type_annotation cx type_params_map reason = function
  | None -> mk_type cx type_params_map reason None
  | Some (loc, typeAnnotation) -> mk_type cx type_params_map reason (Some typeAnnotation)

(* Model a set of keys as the union of their singleton types. *)
and mk_keys_type reason keys =
  match keys with
  | [key] -> mk_singleton_string reason key
  | _ -> UnionT (reason, List.map (mk_singleton_string reason) keys)

and mk_singleton_string reason key =
  let reason = replace_reason (spf "string literal `%s`" key) reason in
  SingletonStrT (reason, key)

and mk_singleton_number reason num raw =
  let reason = replace_reason (spf "number literal `%.16g`" num) reason in
  SingletonNumT (reason, (num, raw))

and mk_singleton_boolean reason b =
  let reason = replace_reason (spf "boolean literal `%b`" b) reason in
  SingletonBoolT (reason, b)

(* Given the type of expression C and type arguments T1...Tn, return the type of
   values described by C<T1,...,Tn>, or C when there are no type arguments. *)
(** See comment on Flow_js.mk_instance for what the for_type flag means. **)
and mk_nominal_type ?(for_type=true) cx reason type_params_map (c, targs) =
  match targs with
  | Some ts ->
      let tparams = List.map (convert cx type_params_map) ts in
      TypeAppT (c, tparams)
  | None ->
      Flow_js.mk_instance cx reason ~for_type c

and void_ loc =
  VoidT.at loc

and null_ loc =
  NullT.at loc

(* take a list of AST type param declarations,
   do semantic checking and create types for them. *)
(* note: polarities arg is temporary -
   full support will put them in the typeParameter AST *)
and mk_type_param_declarations cx type_params_map
  ?(polarities=[]) typeParameters
  =
  let add_type_param (typeparams, smap) (loc, t) polarity =
    let name = t.Ast.Identifier.name in
    let reason = mk_reason name loc in
    let bound = match t.Ast.Identifier.typeAnnotation with
      | None -> MixedT reason
      | Some (_, u) -> mk_type cx (SMap.union smap type_params_map) reason (Some u)
    in
    (* leaving in this deliberately cumbersome backdoor until
       we have proper annotations, in case of emergency :) *)
    let polarity =
      if polarity != Neutral then polarity
      else if str_starts_with name "$Covariant$" then Positive
      else if str_starts_with name "$Contravariant$" then Negative
      else Neutral
    in
    let typeparam = { reason; name; bound; polarity } in
    (typeparam :: typeparams,
     SMap.add name (BoundT typeparam) smap)
  in
  let (types:Ast.Identifier.t list) =
    extract_type_param_declarations typeParameters
  in
  let polarities = if polarities != [] then polarities
    else make_list (fun () -> Neutral) (List.length types)
  in
  let typeparams, smap =
    List.fold_left2 add_type_param ([], SMap.empty) types polarities
  in
  List.rev typeparams, SMap.union smap type_params_map

and identifier ?(lookup_mode=ForValue) cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then AnyT.at loc
  else (
    if name = "undefined"
    then void_ loc
    else (
      let reason = mk_reason (spf "identifier `%s`" name) loc in
      let t = Env_js.var_ref ~lookup_mode cx name reason in
      t
    )
  )

and extract_type_param_declarations = function
  | None -> []
  | Some (_, typeParameters) -> typeParameters.Ast.Type.ParameterDeclaration.params

and extract_type_param_instantiations = function
  | None -> []
  | Some (_, typeParameters) -> typeParameters.Ast.Type.ParameterInstantiation.params

(* Function parameters get passed around and manipulated all over the
   place. Instead of passing around the raw data in several pieces,
   accumulate it into a single (abstract) type *)
module FuncParams : sig
  type t

  (* build up a params value *)
  val empty: t
  val add: Context.t -> (Type.t SMap.t) -> t -> Ast.Pattern.t -> Ast.Expression.t option -> t
  val add_rest: Context.t -> (Type.t SMap.t) -> t -> Ast.Identifier.t -> t

  (* name of each param, in order *)
  (* destructured params will be "_" *)
  val names: t -> string list

  (* type of each param in the param list *)
  val tlist: t -> Type.t list

  (* iterates over all bindings, traversing through any destructued
     bindings as well, in source order of declaration *)
  val iter: (string * Type.t * Loc.t -> unit) -> t -> unit

  (* if there is a default for this binding, run provided function *)
  val with_default: string -> (Ast.Expression.t -> unit) -> t -> unit

  val subst: Context.t -> (Type.t SMap.t) -> t -> t
end = struct
  type binding = string * Type.t * Loc.t
  type param =
    | Simple of Type.t * binding
    | Complex of Type.t * binding list
    | Rest of Type.t * binding
  type t = {
    list: param list;
    defaults: Ast.Expression.t SMap.t;
  }

  let empty = {
    list = [];
    defaults = SMap.empty
  }

  let add cx type_params_map params pattern default =
    Ast.Pattern.(match pattern with
    | loc, Identifier (_, { Ast.Identifier.name; typeAnnotation; optional }) ->
      let reason = mk_reason (spf "parameter `%s`" name) loc in
      let t = mk_type_annotation cx type_params_map reason typeAnnotation in
      (match default with
      | None ->
        let t =
          if optional
          then OptionalT t
          else t
        in
        Hashtbl.replace (Context.type_table cx) loc t;
        let binding = name, t, loc in
        let list = Simple (t, binding) :: params.list in
        { params with list }
      | Some expr ->
        (* TODO: assert (not optional) *)
        let binding = name, t, loc in
        { list = Simple (OptionalT t, binding) :: params.list;
          defaults = SMap.add name expr params.defaults })
    | loc, _ ->
      let reason = mk_reason "destructuring" loc in
      let t = type_of_pattern pattern
        |> mk_type_annotation cx type_params_map reason in
      let bindings = ref [] in
      pattern |> destructuring cx t None (fun _ loc name t ->
        Hashtbl.replace (Context.type_table cx) loc t;
        bindings := (name, t, loc) :: !bindings
      );
      { params with list = Complex (t, !bindings) :: params.list })

  let add_rest cx type_params_map params =
    function loc, { Ast.Identifier.name; typeAnnotation; _ } ->
      let reason = mk_reason (spf "rest parameter `%s`" name) loc in
      let t = mk_type_annotation cx type_params_map reason typeAnnotation in
      { params with list = Rest (mk_rest cx t, (name, t, loc)) :: params.list }

  let names params =
    params.list |> List.rev |> List.map (function
      | Simple (_, (name, _, _))
      | Rest (_, (name, _, _)) -> name
    | Complex _ -> "_")

  let tlist params =
    params.list |> List.rev |> List.map (function
      | Simple (t, _)
      | Complex (t, _)
      | Rest (t, _) -> t)

  let iter f params =
    params.list |> List.rev |> List.iter (function
      | Simple (_, b)
      | Rest (_, b) -> f b
      | Complex (t, bs) -> List.iter f bs)

  let with_default name f params =
    match SMap.get name params.defaults with
    | Some t -> f t
    | None -> ()

  let subst_binding cx map (name, t, loc) = (name, Flow_js.subst cx map t, loc)

  let subst cx map params =
    let list = params.list |> List.map (function
      | Simple (t, b) -> Simple (Flow_js.subst cx map t, subst_binding cx map b)
      | Complex (t, bs) -> Complex (Flow_js.subst cx map t, List.map (subst_binding cx map) bs)
      | Rest (t, b) -> Rest (Flow_js.subst cx map t, subst_binding cx map b)) in
    { params with list }
end

type class_method_signature = {
  meth_reason: reason;
  meth_tparams: Type.typeparam list;
  meth_tparams_map: Type.t SMap.t;
  meth_params: FuncParams.t;
  meth_return_type: Type.t;
}

type class_signature = {
  sig_fields: Type.t SMap.t;
  sig_methods: class_method_signature SMap.t;
  sig_getters: class_method_signature SMap.t;
  sig_setters: class_method_signature SMap.t;
}

(************)
(* Visitors *)
(************)

(********************************************************************
 * local inference preliminary pass: traverse AST, collecting
 * declarations and populating variable environment (scope stack)
 * in prep for main pass
 ********************************************************************)

let rec variable_decl cx type_params_map loc entry = Ast.Statement.(
  let value_kind, bind = match entry.VariableDeclaration.kind with
    | VariableDeclaration.Const -> Scope.Entry.Const, Env_js.bind_const
    | VariableDeclaration.Let -> Scope.Entry.Let None, Env_js.bind_let
    | VariableDeclaration.Var -> Scope.Entry.Var, Env_js.bind_var
  in

  let str_of_kind = Scope.Entry.string_of_value_kind value_kind in

  let declarator loc = Ast.(function
    | (loc, Pattern.Identifier (_, { Identifier.name; typeAnnotation; _ })) ->
      let r = mk_reason (spf "%s `%s`" str_of_kind name) loc in
      let t = mk_type_annotation cx type_params_map r typeAnnotation in
      Hashtbl.replace (Context.type_table cx) loc t;
      bind cx name t r
    | p ->
      let r = mk_reason (spf "%s _" str_of_kind) loc in
      let t = type_of_pattern p |> mk_type_annotation cx type_params_map r in
      p |> destructuring cx t None (fun cx loc name t ->
        Hashtbl.replace (Context.type_table cx) loc t;
        bind cx name t r
      )
  ) in

  VariableDeclaration.(entry.declarations |> List.iter (function
    | (loc, { Declarator.id; _; }) -> declarator loc id
  ));
)

and toplevel_decls cx type_params_map =
  List.iter (statement_decl cx type_params_map)

(* TODO: detect structural misuses abnormal control flow constructs *)
and statement_decl cx type_params_map = Ast.Statement.(

  let block_body cx { Block.body } =
    Env_js.in_lex_scope cx (fun () ->
      toplevel_decls cx type_params_map body
    )
  in

  let catch_clause cx { Try.CatchClause.body = (_, b); _ } =
    block_body cx b
  in

  function

  | (loc, Empty) -> ()

  | (loc, Block b) ->
      block_body cx b

  | (loc, Expression _) -> ()

  | (loc, If { If.test; consequent; alternate }) ->
      statement_decl cx type_params_map consequent;
      (match alternate with
        | None -> ()
        | Some st -> statement_decl cx type_params_map st
      )

  | (loc, Labeled { Labeled.label; body }) ->
      statement_decl cx type_params_map body

  | (loc, Break _) -> ()

  | (loc, Continue _) -> ()

  | (loc, With _) ->
      (* TODO disallow or push vars into env? *)
      ()
  | (loc, TypeAlias { TypeAlias.id; typeParameters; right; } ) ->
      let name_loc, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "type `%s`" name) name_loc in
      let tvar = Flow_js.mk_tvar cx r in
      Env_js.bind_type cx name tvar r

  | (loc, Switch { Switch.discriminant; cases; lexical }) ->
      (* TODO: ensure that default is last *)
      Env_js.in_lex_scope cx (fun () ->
        List.iter (fun (loc, { Switch.Case.test; consequent }) ->
          toplevel_decls cx type_params_map consequent
        ) cases
      )

  | (loc, Return _) -> ()

  | (loc, Throw _) -> ()

  | (loc, Try { Try.block = (_, b); handler; guardedHandlers; finalizer }) ->
      block_body cx b;

      (match handler with
        | None -> ()
        | Some (loc, h) -> catch_clause cx h
      );

      List.iter (fun (loc, h) ->
        catch_clause cx h
      ) guardedHandlers;

      (match finalizer with
        | None -> ()
        | Some (_, b) -> block_body cx b
      )

  | (loc, While { While.test; body }) ->
      statement_decl cx type_params_map body

  | (loc, DoWhile { DoWhile.body; test }) ->
      statement_decl cx type_params_map body

  | (loc, For { For.init; test; update; body }) ->
      Env_js.in_lex_scope cx (fun () ->
        (match init with
          | Some (For.InitDeclaration (loc, decl)) ->
              variable_decl cx type_params_map loc decl
          | _ -> ()
        );
        statement_decl cx type_params_map body
      )

  | (loc, ForIn { ForIn.left; right; body; each }) ->
      Env_js.in_lex_scope cx (fun () ->
        (match left with
          | ForIn.LeftDeclaration (loc, decl) ->
              variable_decl cx type_params_map loc decl
          | _ -> ()
        );
        statement_decl cx type_params_map body
      )

  | (loc, ForOf { ForOf.left; right; body; }) ->
      Env_js.in_lex_scope cx (fun () ->
        (match left with
          | ForOf.LeftDeclaration (loc, decl) ->
              variable_decl cx type_params_map loc decl
          | _ -> ()
        );
        statement_decl cx type_params_map body
      )

  | (loc, Let _) ->
      (* TODO *)
      ()

  | (loc, Debugger) -> ()

  | (loc, FunctionDeclaration { FunctionDeclaration.id; async; _ }) -> (
      match id with
      | Some id ->
        let _, { Ast.Identifier.name; _ } = id in
        let r = mk_reason (spf "%sfunction %s"
          (if async then "async " else "") name) loc in
        let tvar = Flow_js.mk_tvar cx r in
        Env_js.bind_fun cx name tvar r
      | None -> failwith (
          "Flow Error: Nameless function declarations should always be given " ^
          "an implicit name before they get hoisted!"
        )
    )

  | (loc, DeclareVariable { DeclareVariable.id; }) ->
      let _, { Ast.Identifier.name; typeAnnotation; _; } = id in
      let r = mk_reason (spf "declare %s" name) loc in
      let t = mk_type_annotation cx type_params_map r typeAnnotation in
      Hashtbl.replace (Context.type_table cx) loc t;
      Env_js.bind_declare_var cx name t r

  | (loc, DeclareFunction { DeclareFunction.id; }) ->
      let _, { Ast.Identifier.name; typeAnnotation; _; } = id in
      let r = mk_reason (spf "declare %s" name) loc in
      let t = mk_type_annotation cx type_params_map r typeAnnotation in
      Hashtbl.replace (Context.type_table cx) loc t;
      Env_js.bind_declare_fun cx name t r

  | (loc, VariableDeclaration decl) ->
      variable_decl cx type_params_map loc decl

  | (loc, ClassDeclaration { Ast.Class.id; _ }) -> (
      match id with
      | Some id ->
        let name_loc, { Ast.Identifier.name; _ } = id in
        let r = mk_reason (spf "class `%s`" name) name_loc in
        let tvar = Flow_js.mk_tvar cx r in
        Env_js.bind_implicit_let Scope.Entry.ClassNameBinding cx name tvar r
      | None -> ()
    )

  | (loc, DeclareClass { Interface.id; _ })
  | (loc, InterfaceDeclaration { Interface.id; _ }) as stmt ->
      let is_interface = match stmt with
      | (_, InterfaceDeclaration _) -> true
      | _ -> false in
      let _, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "class `%s`" name) loc in
      let tvar = Flow_js.mk_tvar cx r in
      (* interface is a type alias, declare class is a var *)
      if is_interface
      then Env_js.bind_type cx name tvar r
      else Env_js.bind_declare_var cx name tvar r

  | (loc, DeclareModule { DeclareModule.id; _ }) ->
      let name = match id with
      | DeclareModule.Identifier (_, id) -> id.Ast.Identifier.name
      | DeclareModule.Literal (_, { Ast.Literal.value = Ast.Literal.String str; _; }) ->
          str
      | _ ->
          (* The only literals that we should see as module names are strings *)
          assert false in
      let r = mk_reason (spf "module `%s`" name) loc in
      let t = Flow_js.mk_tvar cx r in
      Hashtbl.replace (Context.type_table cx) loc t;
      Env_js.bind_declare_var cx (internal_module_name name) t r

  | (loc, DeclareExportDeclaration {
      DeclareExportDeclaration.default;
      DeclareExportDeclaration.declaration;
      DeclareExportDeclaration.specifiers;
      DeclareExportDeclaration.source;
      }) ->
        DeclareExportDeclaration.(match declaration with
        | Some (Variable (loc, v)) ->
            statement_decl cx type_params_map (loc, DeclareVariable v)
        | Some (Function (loc, f)) ->
            statement_decl cx type_params_map (loc, DeclareFunction f)
        | Some (Class (loc, c)) ->
            statement_decl cx type_params_map (loc, DeclareClass c)
        | Some (DefaultType t) -> ()
        | None ->
            if not default
            then ()
            else failwith (
              "Parser Error: declare export default must always have an " ^
              "associated declaration or type!"
            )
        )

  | (_, ExportDeclaration {
      ExportDeclaration.default;
      ExportDeclaration.declaration;
      ExportDeclaration.specifiers;
      ExportDeclaration.source;
      ExportDeclaration.exportKind=_;
    }) -> (
      match declaration with
      | Some(ExportDeclaration.Declaration(stmt)) ->
        let stmt = if default then nameify_default_export_decl stmt else stmt in
        statement_decl cx type_params_map stmt
      | Some(ExportDeclaration.Expression(_)) -> ()
      | None -> if not default then () else failwith (
          "Parser Error: Default exports must always have an associated " ^
          "declaration or expression!"
        )
    )
  | (loc, ImportDeclaration {
      ImportDeclaration.default;
      ImportDeclaration.importKind;
      ImportDeclaration.source;
      ImportDeclaration.specifier;
    }) ->
      let (source_loc, source_literal) = source in
      let module_name = (
        match source_literal.Ast.Literal.value with
        | Ast.Literal.String(value) -> value
        | _ -> failwith  (
            "Parser error: Invalid source type! Must be a string literal."
          )
      ) in
      let (import_str, isType) = (
        match importKind with
        | ImportDeclaration.ImportType -> "import type", true
        | ImportDeclaration.ImportTypeof -> "import typeof", true
        | ImportDeclaration.ImportValue -> "import", false
      ) in

      (match default with
        | Some(_, local_ident) ->
          let local_name = local_ident.Ast.Identifier.name in
          let reason_str =
            (spf "%s %s from \"%s\"" import_str local_name module_name)
          in
          let reason = mk_reason reason_str loc in
          let tvar = Flow_js.mk_tvar cx reason in
          let state = Scope.State.Initialized in
          if isType
          then Env_js.bind_type ~state cx local_name tvar reason
          else Env_js.bind_var ~state cx local_name tvar reason
        | None -> ()
      );

      (match specifier with
        | Some (ImportDeclaration.Named (_, named_specifiers)) ->
          let init_specifier (specifier_loc, specifier) = (
            let loc, remote_ident =
              specifier.ImportDeclaration.NamedSpecifier.id
            in
            let remote_name = remote_ident.Ast.Identifier.name in
            let local_name, reason = (
              match specifier.ImportDeclaration.NamedSpecifier.name with
              | Some (_, { Ast.Identifier.name = local_name; _ }) ->
                let reason_str =
                  spf "%s { %s as %s }" import_str remote_name local_name
                in
                local_name, (mk_reason reason_str loc)
              | None ->
                let reason_str = spf "%s { %s }" import_str remote_name in
                remote_name, (mk_reason reason_str loc)
            ) in
            let tvar = Flow_js.mk_tvar cx reason in
            let spec_reason = repos_reason specifier_loc reason in
            let state = Scope.State.Initialized in
            if isType
            then Env_js.bind_type ~state cx local_name tvar spec_reason
            else Env_js.bind_var ~state cx local_name tvar spec_reason
          ) in

          List.iter init_specifier named_specifiers

        | Some (ImportDeclaration.NameSpace (_, (loc, local_ident))) ->
          let local_name = local_ident.Ast.Identifier.name in
          let reason =
            mk_reason (spf "%s * as %s" import_str local_name) loc
          in
          let tvar = Flow_js.mk_tvar cx reason in
          let state = Scope.State.Initialized in
          if isType
          then Env_js.bind_type ~state cx local_name tvar reason
          else Env_js.bind_var ~state cx local_name tvar reason

        | None -> ()
      )
)

(***************************************************************
 * local inference main pass: visit AST statement list, calling
 * flow to check types/create graphs for merge-time checking
 ***************************************************************)

and toplevels cx type_params_map stmts =
  let stmts = List.filter Ast.Statement.(function
    | (loc, Empty) -> false
    | _ -> true
  ) stmts
  in
  let n = ref 0 in
  match Abnormal.catch_control_flow_exception (fun () ->
    stmts |> List.iter (fun stmt ->
      statement cx type_params_map stmt;
      incr n (* n is bumped whenever stmt doesn't exit abnormally *)
    )
  ) with
  | Some exn ->
    (* control flow exit out of a flat list:
       check for unreachable code and rethrow *)
    (* !n is the index of the statement that exits abnormally, so !n+1 is the
       index of possibly unreachable code. *)
    let uc = !n+1 in
    if uc < List.length stmts
    then (
      let msg = "unreachable code" in
      let warn_unreachable loc = Flow_js.add_warning cx [mk_reason "" loc, msg] in
      let rec drop n lst = match (n, lst) with
        | (_, []) -> []
        | (0, l) -> l
        | (x, _ :: t) -> drop (pred x) t
      in
      let trailing = drop uc stmts in
      trailing |> List.iter Ast.Statement.(function
        (* function declarations are hoisted, so not unreachable *)
        | (_, FunctionDeclaration _ ) -> ()
        (* variable declarations are hoisted, but associated assignments are not,
           so skip variable declarations with no assignments.
           Note: this does not seem like a practice anyone would use *)
        | (_, VariableDeclaration d) -> VariableDeclaration.(d.declarations |>
            List.iter Declarator.(function
            | (_, { init = Some (loc, _); _ } ) -> warn_unreachable loc
            | _ -> ()
          ))
        | (loc, _) -> warn_unreachable loc
      )
    );
    Abnormal.throw_control_flow_exception exn
  | None -> ()

and statement cx type_params_map = Ast.Statement.(

  let variables cx loc { VariableDeclaration.declarations; kind } =
    List.iter (variable cx type_params_map kind) declarations
  in

  let interface cx loc structural {
    Interface.id;
    typeParameters;
    body = (_, { Ast.Type.Object.properties; indexers; callProperties });
    extends;
    mixins;
  } =
    let _, { Ast.Identifier.name = iname; _ } = id in
    let reason = mk_reason iname loc in

    (* TODO excise the Promise/PromisePolyfill special-case ASAP *)
    let typeparams, map =
      if (iname = "Promise" || iname = "PromisePolyfill") &&
        List.length (extract_type_param_declarations typeParameters) = 1
      then mk_type_param_declarations cx type_params_map typeParameters
        ~polarities:[Positive]
      else if (iname = "Generator") &&
        List.length (extract_type_param_declarations typeParameters) = 3
      then mk_type_param_declarations cx type_params_map typeParameters
        ~polarities:[Positive; Positive; Negative]
      else mk_type_param_declarations cx type_params_map typeParameters in

    let sfmap, smmap, fmap, mmap = List.fold_left Ast.Type.Object.Property.(
      fun (sfmap_, smmap_, fmap_, mmap_)
        (loc, { key; value; static; _method; optional }) ->
        if optional && _method
        then begin
          let msg = "optional methods are not supported" in
          Flow_js.add_error cx [Reason_js.mk_reason "" loc, msg]
        end;
        Ast.Expression.Object.Property.(match key with
        | Literal (loc, _)
        | Computed (loc, _) ->
          let msg = "illegal name" in
            Flow_js.add_error cx [Reason_js.mk_reason "" loc, msg];
            (sfmap_, smmap_, fmap_, mmap_)

        | Identifier (loc, { Ast.Identifier.name; _ }) ->
          let t = convert cx map value in
          let t = if optional then OptionalT t else t in
          (* check for overloads in static and instance method maps *)
          let map_ = if static then smmap_ else mmap_ in
          let t = match SMap.get name map_ with
            | None -> t
            | Some (IntersectionT (reason, seen_ts)) ->
                  IntersectionT (reason, seen_ts @ [t])
            | Some seen_t ->
                  IntersectionT (Reason_js.mk_reason iname loc, [seen_t; t])
          in
            (* TODO: additionally check that the four maps are disjoint *)
            (match static, _method with
            | true, true ->  (sfmap_, SMap.add name t smmap_, fmap_, mmap_)
            | true, false -> (SMap.add name t sfmap_, smmap_, fmap_, mmap_)
            | false, true -> (sfmap_, smmap_, fmap_, SMap.add name t mmap_)
            | false, false -> (sfmap_, smmap_, SMap.add name t fmap_, mmap_)
            )
        )
    ) (SMap.empty, SMap.empty, SMap.empty, SMap.empty) properties
    in
    let fmap = Ast.Type.Object.Indexer.(match indexers with
      | [] -> fmap
      | [(_, { key; value; _; })] ->
        let keyt = convert cx map key in
        let valuet = convert cx map value in
        fmap |> SMap.add "$key" keyt |> SMap.add "$value" valuet
    (* TODO *)
      | _ -> failwith "Unimplemented: multiple indexers")
    in
    let calls = callProperties |> List.map (function
      | loc, { Ast.Type.Object.CallProperty.value = (_, ft); static; } ->
        (static, convert cx map (loc, Ast.Type.Function ft))
    ) in
    let scalls, calls = List.partition fst calls in
    let smmap = match scalls with
      | [] -> smmap
      | [_,t] -> SMap.add "$call" t smmap
      | _ -> let scalls = List.map snd scalls in
             SMap.add "$call" (IntersectionT (mk_reason iname loc,
                                              scalls)) smmap
    in
    let mmap = match calls with
      | [] -> mmap
      | [_,t] -> SMap.add "$call" t mmap
      | _ ->
        let calls = List.map snd calls in
        SMap.add "$call" (IntersectionT (mk_reason iname loc,
                                              calls)) mmap
    in
    let mmap = match SMap.get "constructor" mmap with
      | None ->
        let constructor_funtype =
          Flow_js.mk_functiontype [] ~params_names:[] VoidT.t in
        let reason = Reason_js.mk_reason "constructor" loc in
        let funt = FunT (
          reason,
          Flow_js.dummy_static reason,
          Flow_js.dummy_prototype,
          constructor_funtype
        ) in
        SMap.add "constructor" funt mmap
      | Some _ ->
        mmap
    in
    let i = mk_interface cx reason typeparams map
      (sfmap, smmap, fmap, mmap) extends mixins structural in
    Hashtbl.replace (Context.type_table cx) loc i;
    (* interface is a type alias, declare class is a var *)
    Env_js.(if structural then init_type else init_var ~has_anno:false)
      cx iname i reason
  in

  let catch_clause cx { Try.CatchClause.param; guard; body = (_, b) } =
    Ast.Pattern.(match param with
      | loc, Identifier (name_loc, {
          Ast.Identifier.name; typeAnnotation = None; _
        }) ->
          let r = mk_reason "catch" loc in
          let t = Flow_js.mk_tvar cx r in

          (match Env_js.in_lex_scope cx (fun () ->
            Scope.(Env_js.bind_implicit_let
              ~state:State.Initialized Entry.CatchParamBinding cx name t r);

            Abnormal.catch_control_flow_exception (fun () ->
              toplevel_decls cx type_params_map b.Block.body;
              toplevels cx type_params_map b.Block.body
            )
          ) with
          | Some exn -> Abnormal.throw_control_flow_exception exn
          | None -> ()
          )

      | loc, Identifier (_, { Ast.Identifier.name; _ }) ->
          let msg = "type annotations for catch params not yet supported" in
          Flow_js.add_error cx [mk_reason "" loc, msg]

      | loc, _ ->
          let msg = "unsupported catch parameter declaration" in
          Flow_js.add_error cx [mk_reason "" loc, msg]
    )
  in

  function

  | (loc, Empty) -> ()

  | (loc, Block { Block.body }) ->
      Env_js.in_lex_scope cx (fun () ->
        toplevel_decls cx type_params_map body;
        toplevels cx type_params_map body
      )

  | (loc, Expression { Expression.expression = e }) ->
      ignore (expression cx type_params_map e)

  (* Refinements for `if` are derived by the following Hoare logic rule:

     [Pre & c] S1 [Post1]
     [Pre & ~c] S2 [Post2]
     Post = Post1 | Post2
     ----------------------------
     [Pre] if c S1 else S2 [Post]
  *)
  | (loc, If { If.test; consequent; alternate }) ->
      let reason = mk_reason "if" loc in

      let _, preds, not_preds, xts =
        predicates_of_condition cx type_params_map test in

      (* grab a reference to the incoming env -
         we'll restore it and merge branched envs later *)
      let start_env =  Env_js.peek_env () in
      let oldset = Changeset.clear () in

      (* swap in a refined clone of initial env for then *)
      Env_js.(
        update_env cx reason (clone_env start_env);
        refine_with_preds cx reason preds xts;
      );

      let exception_then = Abnormal.catch_control_flow_exception
        (fun () -> statement cx type_params_map consequent)
      in

      (* grab a reference to env after then branch *)
      let then_env = Env_js.peek_env () in

      (* then swap in a refined clone of initial env for else *)
      Env_js.(
        update_env cx reason (clone_env start_env);
        refine_with_preds cx reason not_preds xts;
      );

      let exception_else = match alternate with
        | None -> None
        | Some st ->
          Abnormal.catch_control_flow_exception
            (fun () -> statement cx type_params_map st)
      in

      (* grab a reference to env after else branch *)
      let else_env = Env_js.peek_env () in

      (* snapshot if-else changes and merge old changes back into state *)
      let newset = Changeset.merge oldset in

      (* adjust post-if environment. if we've returned from one arm,
         swap in the env generated by the other, otherwise merge *)
      let end_env = match exception_then, exception_else with
      | Some Abnormal.Return, None
      | Some Abnormal.Throw, None ->
        else_env

      | None, Some Abnormal.Return
      | None, Some Abnormal.Throw ->
        then_env

      | _ ->
        Env_js.merge_env cx reason (start_env, then_env, else_env) newset;
        start_env
      in
      Env_js.update_env cx reason end_env;

      (* handle control flow in cases where we've thrown from both sides *)
      Abnormal.(match exception_then, exception_else with
      | Some Throw, Some Return
      | Some Return, Some Throw ->
        throw_control_flow_exception Return;

      | Some then_exn, Some else_exn when then_exn = else_exn ->
        throw_control_flow_exception then_exn

      | _ -> ()
      )

  | (loc, Labeled { Labeled.label = _, { Ast.Identifier.name; _ }; body }) ->
      (match body with
      | (loc, While _)
      | (loc, DoWhile _)
      | (loc, For _)
      | (loc, ForIn _)
        ->
        let reason = mk_reason "label" loc in
        let oldset = Changeset.clear () in
        let label = Some name in
        let save_break_exn = Abnormal.(swap (Break label) false) in
        let save_continue_exn = Abnormal.(swap (Continue label) false) in

        let env = Env_js.peek_env () in
        Env_js.widen_env cx reason;

        let loop_env = Env_js.clone_env env in
        Env_js.update_env cx reason loop_env;

        Abnormal.(
          check_control_flow_exception (
            ignore_break_or_continue_to_label label (
              fun () -> statement cx type_params_map body)));

        let newset = Changeset.merge oldset in
        if Abnormal.(swap (Continue label) save_continue_exn)
        then Env_js.havoc_vars newset;
        Env_js.copy_env cx reason (env,loop_env) newset;

        if Abnormal.(swap (Break label) save_break_exn)
        then Env_js.havoc_vars newset

      | _ ->
        let oldset = Changeset.clear () in
        let label = Some name in
        let save_break_exn = Abnormal.(swap (Break label) false) in

        Abnormal.(
          check_control_flow_exception (
            ignore_break_to_label label (
              fun () -> statement cx type_params_map body)));

        let newset = Changeset.merge oldset in
        if Abnormal.(swap (Break label) save_break_exn)
        then Env_js.havoc_vars newset
      )

  | (loc, Break { Break.label }) ->
      let label_opt = match label with
        | None -> None
        | Some (_, { Ast.Identifier.name; _ }) -> Some name
      in
      Env_js.reset_current_activation (mk_reason "break" loc);
      Abnormal.(set (Break label_opt))

  | (loc, Continue { Continue.label }) ->
      let label_opt = match label with
        | None -> None
        | Some (_, { Ast.Identifier.name; _ }) -> Some name
      in
      Env_js.reset_current_activation (mk_reason "continue" loc);
      Abnormal.(set (Continue label_opt))

  | (loc, With _) ->
      (* TODO or disallow? *)
      ()

  | (loc, TypeAlias { TypeAlias.id; typeParameters; right; } ) ->
      let name_loc, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "type `%s`" name) name_loc in
      let typeparams, type_params_map =
        mk_type_param_declarations cx type_params_map typeParameters in
      let t = convert cx type_params_map right in
      let type_ =
        if typeparams = []
        then TypeT (r, t)
        else PolyT(typeparams, TypeT (r, t))
      in
      Hashtbl.replace (Context.type_table cx) loc type_;
      Env_js.init_type cx name type_ r

  | (loc, Switch { Switch.discriminant; cases; lexical }) ->
      ignore (expression cx type_params_map discriminant);

      Env_js.in_lex_scope cx (fun () ->

        (* initialize switch scope bindings in a single pass *)
        List.iter (fun (loc, { Switch.Case.test; consequent }) ->
          toplevel_decls cx type_params_map consequent
        ) cases;

        let save_break_exn = Abnormal.(swap (Break None) false) in
        let default = ref false in
        let env =  Env_js.peek_env () in
        let last_env = ref None in
        let fallthrough_env = ref env in
        let oldset = Changeset.clear () in

        let merge_with_last_env cx reason env changeset =
          match !last_env with
          | Some x ->
            Env_js.merge_env cx reason (env, env, x) changeset
          | None -> ()
        in

        let exceptions = List.rev_map (
          fun (loc, {Switch.Case.test; consequent}) ->
            if !default then None (* TODO: error when case follows default *)
            else (
              let reason = mk_reason "case" loc in
              let _, preds, not_preds, xtypes = match test with
              | None ->
                  default := true;
                  EmptyT.at loc, Scope.KeyMap.empty, Scope.KeyMap.empty,
                    Scope.KeyMap.empty
              | Some expr ->
                  let fake_ast = loc, Ast.Expression.(Binary {
                    Binary.operator = Binary.StrictEqual;
                    left = discriminant;
                    right = expr;
                  }) in
                  predicates_of_condition cx type_params_map fake_ast
              in

              (* env of new case inherits background accumulation... *)
              let case_env = Env_js.clone_env !fallthrough_env in
              Env_js.update_env cx reason case_env;
              (* ...and adds test refinement... *)
              Env_js.refine_with_preds cx reason preds xtypes;
              (* ...and merges with previous case, which will have been
                 cleared if it exited the switch *)
              merge_with_last_env cx reason case_env (Changeset.peek ());

              let case_exception = Abnormal.catch_control_flow_exception (
                fun () -> toplevels cx type_params_map consequent
              ) in

              (* swap in background env and add negatives of this case's preds *)
              Env_js.update_env cx reason !fallthrough_env;
              Env_js.refine_with_preds cx reason not_preds xtypes;

              last_env := Some case_env;

              case_exception
            )
          ) cases
        in

        let newset = Changeset.merge oldset in

        (* if there's no default clause, pretend there is and it's empty *)
        if not !default
        then (
          let default_env = Env_js.clone_env !fallthrough_env in
          let reason = mk_reason "default" loc in
          Env_js.update_env cx reason default_env;
          merge_with_last_env cx reason default_env newset
        );

        let did_break = Abnormal.(swap (Break None) save_break_exn) in
        if did_break then Env_js.havoc_vars newset;

        (* if the default clause exits abnormally and other cases either fall
           through or exit abnormally the same way, then the switch exits
           abnormally that way *)
        if !default
        then (
          match List.hd exceptions with
          | Some (Abnormal.Break None) | None -> ()
          | Some exn ->
              if List.tl exceptions |> List.for_all (function
                | None
                | Some Abnormal.Throw
                | Some Abnormal.Return -> true
                | _ -> false
              )
              then Abnormal.throw_control_flow_exception exn
        );

        (* in the case where we did have a default but never encountered a break,
           the changeset accumulated from all of the cases is lost in the
           Changeset.merge call and the variables are never havoc'd,
           so we need to carry these changes forward *)
        if !default && not did_break
        then (
          let reason = mk_reason "switch fallthrough" loc in
          let last_env = match !last_env with
            | Some x -> x
            | None -> !fallthrough_env
          in
          Env_js.copy_env cx reason (env,last_env) newset
        )
        else ()
      )

  | (loc, Return { Return.argument }) ->
      let reason = mk_reason "return" loc in
      let ret =
        Env_js.get_var cx (internal_name "return") reason
      in
      let t = match argument with
        | None -> void_ loc
        | Some expr -> expression cx type_params_map expr
      in
      let t =
        if Env_js.in_async_scope () then
          (* Convert the return expression's type T to Promise<T>. If the
           * expression type is itself a Promise<T>, ensure we still return
           * a Promise<T> via Promise.resolve. *)
          let reason = mk_reason "async return" loc in
          let promise = Flow_js.get_builtin cx "Promise" reason in
          Flow_js.mk_tvar_where cx reason (fun tvar ->
            let call = Flow_js.mk_methodtype promise [t] tvar in
            Flow_js.flow cx
              (promise, MethodT (reason, (reason, "resolve"), call))
          )
        else if Env_js.in_generator_scope () then
          (* Convert the return expression's type R to Generator<Y,R,N>, where
           * Y and R are internals, installed earlier. *)
          let reason = mk_reason "generator return" loc in
          Flow_js.get_builtin_typeapp cx reason "Generator" [
            Env_js.get_var cx (internal_name "yield") reason;
            t;
            Env_js.get_var cx (internal_name "next") reason
          ]
        else t
      in
      Flow_js.flow cx (t, ret);
      Env_js.reset_current_activation reason;
      Abnormal.(set Return)

  | (loc, Throw { Throw.argument }) ->
      let reason = mk_reason "throw" loc in
      ignore (expression cx type_params_map argument);
      Env_js.reset_current_activation reason;
      Abnormal.(set Throw)

  (***************************************************************************)
  (* Try-catch-finally statements have a lot of control flow possibilities. (To
     simplify matters, a missing catch block is considered to to be a catch block
     that throws, and a missing finally block is considered to be an empty block.)

     A try block may either

     * exit normally: in this case, it proceeds to the finally block.

     * exit abnormally: in this case, it proceeds to the catch block.

     A catch block may either:

     * exit normally: in this case, it proceeds to the finally block.

     * exit abnormally: in this case, it proceeds to the finally block and
     throws at the end of the finally block.

     A finally block may either:

     * exit normally: in this case, the try-catch-finally statement exits
     normally. (Note that to be in this case, either the try block exited
     normally, or it didn't and the catch block exited normally.)

     * exit abnormally: in this case, the try-catch-finally statement exits
     abnormally.

     Based on these possibilities, approximations for the local state at various
     points in a try-catch-finally statement can be derived.

     * The start of a catch block is reachable via anywhere in the try
     block. Thus, the local state must be conservative.

     * The start of a finally block is reachable via the end of the try block,
     or anywhere in the catch block. Thus, the local state must be conservative.

     * The end of a try-catch-finally statement is reachable via the end of the
     finally block. However, in this case we can assume that either

     ** the try block exited normally, in which case the local state at the
     start of the finally block is the same as the local state at the end of the
     try block.

     ** the catch block exited normally, in which case the local state at the
     start of the finally block is the same as the local state at the end of
     the catch block.

     Thus, a finally block should be analyzed twice, with each of the following
     assumptions for the local state at its start: (1) conservative (to model
     abnormal exits in the try or catch blocks); (2) whatever is at the end of
     the try block merged with whatever is at the end of the catch block (for
     normal exits in the try and catch blocks).

     Important to understand: since (1) is conservative, it should produce
     errors whenever (2) does, so that's not why we do them separately.
     But since (2) models exactly the states from which subsequent code is
     reachable, we can use its tighter approximation as the basis for
     subsequent analysis without loss of soundness.
   *)
  (***************************************************************************)
  | (loc, Try { Try.block = (_, b); handler; guardedHandlers; finalizer }) ->
      let reason = mk_reason "try" loc in
      let oldset = Changeset.clear () in

      (* save ref to initial env and swap in a clone *)
      let start_env = Env_js.peek_env () in
      Env_js.(update_env cx reason (clone_env start_env));

      let exception_try = Env_js.in_lex_scope cx (fun () ->
        Abnormal.catch_control_flow_exception (fun () ->
          toplevel_decls cx type_params_map b.Block.body;
          toplevels cx type_params_map b.Block.body
        )
      ) in

      (* save ref to env at end of try *)
      let try_env = Env_js.peek_env () in

      (* traverse catch block, save exceptions *)
      let exception_catch = match handler with
      | None ->
        (* a missing catch is equivalent to a catch that always throws *)
        Some Abnormal.Throw

      | Some (_, h) ->
        (* if try throws to here, we need an env that's conservative
           over everything that happened from start_env to try_env *)
        Env_js.(
          let e = clone_env start_env in
          merge_env cx reason (e, e, try_env) (Changeset.peek ());
          update_env cx reason e
        );

        Abnormal.catch_control_flow_exception
          (fun () -> catch_clause cx h)
      in

      (* save ref to env at end of catch *)
      let catch_env = Env_js.peek_env () in

      (* build initial env for non-throwing finally *)
      let nonthrow_finally_env = Env_js.(match exception_catch with
      | None ->
        (* if catch ends normally, then non-throwing finally can be
           reached via it or a non-throwing try. merge terminal states *)
        let e = clone_env try_env in
        merge_env cx reason (e, e, catch_env) (Changeset.peek ());
        e
      | Some _ ->
        (* if catch throws, then the only way into non-throwing finally
           is via non-throwing try *)
        try_env
      ) in

      assert (guardedHandlers = []); (* remove from AST *)

      (* traverse finally block, save exceptions,
         and leave in place the terminal env of the non-throwing case
         (in which subsequent code is reachable) *)
      let exception_finally = match finalizer with
      | None ->
        Env_js.update_env cx reason nonthrow_finally_env;
        None

      | Some (_, { Block.body }) ->
        (* analyze twice, with different start states *)

        (* 1. throwing-finally case. *)
        (* env may be in any state from start of try through end of catch *)
        Env_js.(
          let e = clone_env start_env in
          merge_env cx reason (e, e, catch_env) (Changeset.peek ());
          update_env cx reason e
        );

        let result = Env_js.in_lex_scope cx (fun () ->
          Abnormal.catch_control_flow_exception (fun () ->
            toplevel_decls cx type_params_map body;
            toplevels cx type_params_map body
          )
        ) in

        (* 2. non-throwing finally case. *)
        Env_js.update_env cx reason nonthrow_finally_env;

        (* (exceptions will be the same in both cases) *)
        let _ = Env_js.in_lex_scope cx (fun () ->
          Abnormal.catch_control_flow_exception (fun () ->
            toplevel_decls cx type_params_map body;
            toplevels cx type_params_map body
          )
        ) in

        result
      in

      let newset = Changeset.merge oldset in
      ignore newset;

      (* if finally has abnormal control flow, we throw here *)
      Abnormal.check_control_flow_exception exception_finally;

      (* other ways we throw due to try/catch abends *)
      Abnormal.(match exception_try, exception_catch with
      | Some (Throw as try_exn), Some Throw
      | Some (Return as try_exn), Some _ ->
          throw_control_flow_exception try_exn

      | Some Throw, Some (Return as catch_exn) ->
          throw_control_flow_exception catch_exn

      | _ -> ())


  (***************************************************************************)
  (* Refinements for `while` are derived by the following Hoare logic rule:

     [Pre' & c] S [Post']
     Pre' = Pre | Post'
     Post = Pre' & ~c
     ----------------------
     [Pre] while c S [Post]
  *)
  (***************************************************************************)
  | (loc, While { While.test; body }) ->

      let reason = mk_reason "while" loc in
      let save_break_exn = Abnormal.(swap (Break None) false) in
      let save_continue_exn = Abnormal.(swap (Continue None) false) in

      (* generate loop test preds and their complements *)
      let _, preds, not_preds, orig_types =
        predicates_of_condition cx type_params_map test in

      (* save current changeset and install an empty one *)
      let oldset = Changeset.clear () in

      (* widen_env wraps specifics in tvars, anticipating widening inflows *)
      Env_js.widen_env cx reason;

      (* start_env is Pre above: env as of loop top *)
      let start_env = Env_js.peek_env () in

      (* swap in Pre & c *)
      Env_js.(
        update_env cx reason (clone_env start_env);
        refine_with_preds cx reason preds orig_types
      );

      (* traverse loop body - after this, body_env = Post' *)
      ignore (Abnormal.catch_control_flow_exception
        (fun () -> statement cx type_params_map body));

      (* save ref to env after loop body *)
      let body_env = Env_js.peek_env () in

      (* save loop body changeset to newset, install merged changes *)
      let newset = Changeset.merge oldset in

      (* if we continued out of the loop, havoc vars changed by loop body *)
      if Abnormal.(swap (Continue None) save_continue_exn)
      then Env_js.havoc_vars newset;

      (* widen start_env with new specifics from body_env
         (turning Pre into Pre' = Pre | Post')
         then reinstall and add ~c to make Post *)
      Env_js.(
        copy_env cx reason (start_env, body_env) newset;
        update_env cx reason start_env;
        refine_with_preds cx reason not_preds orig_types
      );

      (* if we broke out of the loop, havoc vars changed by loop body *)
      if Abnormal.(swap (Break None) save_break_exn)
      then Env_js.havoc_vars newset

  (***************************************************************************)
  (* Refinements for `do-while` are derived by the following Hoare logic rule:

     [Pre'] S [Post']
     Pre' = Pre | (Post' & c)
     Post = Post' & ~c
     -------------------------
     [Pre] do S while c [Post]
  *)
  (***************************************************************************)
  | (loc, DoWhile { DoWhile.body; test }) ->
      let reason = mk_reason "do-while" loc in
      let save_break_exn = Abnormal.(swap (Break None) false) in
      let save_continue_exn = Abnormal.(swap (Continue None) false) in
      let env =  Env_js.peek_env () in
      let oldset = Changeset.clear () in
      (* env = Pre *)
      (* ENV = [env] *)

      Env_js.widen_env cx reason;
      (* env = Pre', Pre' > Pre *)

      let body_env = Env_js.clone_env env in
      Env_js.update_env cx reason body_env;
      (* body_env = Pre' *)
      (* ENV = [body_env] *)

      let exception_ = Abnormal.(
        ignore_break_or_continue_to_label None (
          fun () -> statement cx type_params_map body)
      ) in

      if Abnormal.(swap (Continue None) save_continue_exn)
      then Env_js.havoc_vars (Changeset.peek ());

      let _, preds, not_preds, xtypes =
        predicates_of_condition cx type_params_map test in
      (* body_env = Post' *)

      let done_env = Env_js.clone_env body_env in
      (* done_env = Post' *)

      Env_js.refine_with_preds cx reason preds xtypes;
      (* body_env = Post' & c *)

      let newset = Changeset.merge oldset in
      Env_js.copy_env cx reason (env, body_env) newset;
      (* Pre' > Post' & c *)

      Env_js.update_env cx reason done_env;
      Env_js.refine_with_preds cx reason not_preds xtypes;
      if Abnormal.(swap (Break None) save_break_exn)
      then Env_js.havoc_vars newset;
      (* ENV = [done_env] *)
      (* done_env = Post' & ~c *)

      Abnormal.check_control_flow_exception exception_

  (***************************************************************************)
  (* Refinements for `for` are derived by the following Hoare logic rule:

     [Pre] i [Init]
     [Pre' & c] S;u [Post']
     Pre' = Init | Post'
     Post = Pre' & ~c
     --------------------------
     [Pre] for (i;c;u) S [Post]

     NOTE: This rule is similar to that for `while`.
  *)
  (***************************************************************************)
  | (loc, For { For.init; test; update; body }) ->
      Env_js.in_lex_scope cx (fun () ->
        let reason = mk_reason "for" loc in
        let save_break_exn = Abnormal.(swap (Break None) false) in
        let save_continue_exn = Abnormal.(swap (Continue None) false) in
        (match init with
          | None -> ()
          | Some (For.InitDeclaration (loc, decl)) ->
              variable_decl cx type_params_map loc decl;
              variables cx loc decl
          | Some (For.InitExpression expr) ->
              ignore (expression cx type_params_map expr)
        );

        let env =  Env_js.peek_env () in
        let oldset = Changeset.clear () in
        Env_js.widen_env cx reason;

        let do_env = Env_js.clone_env env in
        Env_js.update_env cx reason do_env;

        let _, preds, not_preds, xtypes = match test with
          | None ->
              EmptyT.at loc, Scope.KeyMap.empty, Scope.KeyMap.empty,
              Scope.KeyMap.empty (* TODO: prune the "not" case *)
          | Some expr ->
              predicates_of_condition cx type_params_map expr
        in

        let body_env = Env_js.clone_env do_env in
        Env_js.update_env cx reason body_env;
        Env_js.refine_with_preds cx reason preds xtypes;

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx type_params_map body));

        if Abnormal.(swap (Continue None) save_continue_exn)
        then Env_js.havoc_vars (Changeset.peek ());

        (match update with
          | None -> ()
          | Some expr ->
              ignore (expression cx type_params_map expr)
        );

        let newset = Changeset.merge oldset in
        Env_js.copy_env cx reason (env, body_env) newset;

        Env_js.update_env cx reason do_env;
        Env_js.refine_with_preds cx reason not_preds xtypes;
        if Abnormal.(swap (Break None) save_break_exn)
        then Env_js.havoc_vars newset
      )

  (***************************************************************************)
  (* Refinements for `for-in` are derived by the following Hoare logic rule:

     [Pre] o [Init]
     [Pre'] S [Post']
     Pre' = Init | Post'
     Post = Pre'
     --------------------------
     [Pre] for (i in o) S [Post]
  *)
  (***************************************************************************)
  | (loc, ForIn { ForIn.left; right; body; each }) ->
      let reason = mk_reason "for-in" loc in
      let save_break_exn = Abnormal.(swap (Break None) false) in
      let save_continue_exn = Abnormal.(swap (Continue None) false) in
      let t = expression cx type_params_map right in
      let o = mk_object cx (mk_reason "iteration expected on object" loc) in
      Flow_js.flow cx (t, MaybeT o); (* null/undefined are allowed *)

      Env_js.in_lex_scope cx (fun () ->

        let env =  Env_js.peek_env () in
        let oldset = Changeset.clear () in
        Env_js.widen_env cx reason;

        let body_env = Env_js.clone_env env in
        Env_js.update_env cx reason body_env;

        let _, preds, _, xtypes =
          predicates_of_condition cx type_params_map right in
        Env_js.refine_with_preds cx reason preds xtypes;

        (match left with
          | ForIn.LeftDeclaration (loc, ({ VariableDeclaration.
              kind; declarations = [vdecl]
            } as decl)) ->
              variable_decl cx type_params_map loc decl;
              variable cx type_params_map kind ~if_uninitialized:StrT.at vdecl

          | ForIn.LeftExpression (loc, Ast.Expression.Identifier (_, id)) ->
              let name = id.Ast.Identifier.name in
              let reason = mk_reason (spf "for..in `%s`" name) loc in
              Env_js.set_var cx name (StrT.at loc) reason

          | _ ->
              let msg = "unexpected LHS in for...in" in
              Flow_js.add_error cx [mk_reason "" loc, msg]
        );

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx type_params_map body));

        let newset = Changeset.merge oldset in

        if Abnormal.(swap (Continue None) save_continue_exn)
        then Env_js.havoc_vars newset;
        Env_js.copy_env cx reason (env,body_env) newset;

        Env_js.update_env cx reason env;
        if Abnormal.(swap (Break None) save_break_exn)
        then Env_js.havoc_vars newset
      )

  | (loc, ForOf { ForOf.left; right; body; }) ->
      let reason = mk_reason "for-of" loc in
      let save_break_exn = Abnormal.(swap (Break None) false) in
      let save_continue_exn = Abnormal.(swap (Continue None) false) in
      let t = expression cx type_params_map right in

      let element_tvar = Flow_js.mk_tvar cx reason in
      let o = Flow_js.get_builtin_typeapp
        cx
        (mk_reason "iteration expected on Iterable" loc)
        "$Iterable"
        [element_tvar; AnyT.at loc; AnyT.at loc] in

      Flow_js.flow cx (t, o); (* null/undefined are NOT allowed *)

      Env_js.in_lex_scope cx (fun () ->

        let env =  Env_js.peek_env () in
        let oldset = Changeset.clear () in
        Env_js.widen_env cx reason;

        let body_env = Env_js.clone_env env in
        Env_js.update_env cx reason body_env;

        let _, preds, _, xtypes =
          predicates_of_condition cx type_params_map right in
        Env_js.refine_with_preds cx reason preds xtypes;

        (match left with
          | ForOf.LeftDeclaration (loc, ({ VariableDeclaration.
              kind; declarations = [vdecl]
            } as decl)) ->
              let repos_tvar loc =
                Flow_js.reposition cx (repos_reason loc reason) element_tvar
              in
              variable_decl cx type_params_map loc decl;
              variable cx type_params_map kind ~if_uninitialized:repos_tvar vdecl

          | ForOf.LeftExpression (loc, Ast.Expression.Identifier (_, id)) ->
              let name = id.Ast.Identifier.name in
              let reason = mk_reason (spf "for..of `%s`" name) loc in
              Env_js.set_var cx name element_tvar reason

          | _ ->
              let msg = "unexpected LHS in for...of" in
              Flow_js.add_error cx [mk_reason "" loc, msg]
        );

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx type_params_map body));

        let newset = Changeset.merge oldset in

        if Abnormal.(swap (Continue None) save_continue_exn)
        then Env_js.havoc_vars newset;
        Env_js.copy_env cx reason (env,body_env) newset;

        Env_js.update_env cx reason env;
        if Abnormal.(swap (Break None) save_break_exn)
        then Env_js.havoc_vars newset
      )

  | (loc, Let _) ->
      (* TODO *)
      ()

  | (loc, Debugger) ->
      ()

  | (loc, FunctionDeclaration {
      FunctionDeclaration.id;
      params; defaults; rest;
      body;
      generator;
      returnType;
      typeParameters;
      async;
      _
    }) ->
      let kind = function_kind ~async ~generator in
      let reason = mk_reason "function" loc in
      let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
      let fn_type = mk_function None cx type_params_map reason ~kind
        typeParameters (params, defaults, rest) returnType body this
      in

      (**
       * Use the loc for the function name in the types table. When the function
       * has no name (i.e. for `export default function() ...`), generate a loc
       * that will span the `function` keyword as a next-best-thing location.
       *)
      let type_table_loc =
        match id with
        | Some (loc, _) -> loc
        | None -> Loc.({
            source = loc.source;
            start = loc.start;
            _end = {
              line = loc.start.line;

              (* len('function') is 8 *)
              column = loc.start.column + 8;
              offset = loc.start.offset + 8;
            };
          })
      in
      Hashtbl.replace (Context.type_table cx) type_table_loc fn_type;
      (match id with
      | Some(_, {Ast.Identifier.name; _ }) ->
        Env_js.init_fun cx name fn_type reason
      | None -> ())

  | (loc, DeclareVariable { DeclareVariable.id; })
  | (loc, DeclareFunction { DeclareFunction.id; }) -> ()

  | (loc, VariableDeclaration decl) ->
      variables cx loc decl

  | (class_loc, ClassDeclaration c) ->
      let (name_loc, name) = extract_class_name class_loc c in
      let reason = mk_reason name name_loc in
      Env_js.declare_implicit_let Scope.Entry.ClassNameBinding cx name reason;
      let cls_type = mk_class cx type_params_map class_loc reason c in
      Hashtbl.replace (Context.type_table cx) class_loc cls_type;
      Env_js.init_implicit_let
        Scope.Entry.ClassNameBinding
        cx
        name
        ~has_anno:false
        cls_type
        reason

  | (loc, DeclareClass decl) ->
    interface cx loc false decl

  | (loc, InterfaceDeclaration decl) ->
    interface cx loc true decl

  | (loc, DeclareModule { DeclareModule.id; body; }) ->
    let name = match id with
    | DeclareModule.Identifier (_, id) -> id.Ast.Identifier.name
    | DeclareModule.Literal (_, { Ast.Literal.value = Ast.Literal.String str; _; }) ->
        str
    | _ ->
        (* The only literals that we should see as module names are strings *)
        assert false in
    let _, { Ast.Statement.Block.body = elements } = body in

    let reason = mk_reason (spf "module `%s`" name) loc in
    let t = Env_js.get_var_declared_type cx (internal_module_name name) reason in

    let module_scope = Scope.fresh () in
    Env_js.push_var_scope cx module_scope;

    toplevel_decls cx type_params_map elements;
    toplevels cx type_params_map elements;

    Env_js.pop_var_scope ();

    let for_types, exports_ = Scope.(Entry.(
      match get_entry "exports" module_scope with
      | Some (Value { specific = exports; _ }) ->
        (* TODO: what happens when value bindings other than `exports` are also declared? *)
        let for_types = SMap.filter (fun _ ->
          function
          | Value _ -> false
          | Type _ -> true
        ) module_scope.entries in

        for_types, exports

      | Some _ ->
        assert_false (
          spf "non-var exports entry in declared module `%s`" name)

      | None ->
        let for_types, nonfor_types = SMap.partition (
          fun _ entry -> match entry with
          | Value _ -> false
          | Type _ -> true
        ) module_scope.entries in

        let map = SMap.map (
          function
          | Value { specific; _ } -> specific
          | Type _ -> assert_false "type entry in nonfor_types"
        ) nonfor_types in

        for_types,
        Flow_js.mk_object_with_map_proto cx reason map (MixedT reason)
      )) in
    let module_t = mk_commonjs_module_t cx reason reason exports_ in
    let module_t = Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (
        module_t,
        SetNamedExportsT(
          reason,
          SMap.map Scope.Entry.(
            function
            | Type { _type; _ } -> _type
            | _ -> assert_false "non-type entry in for_types"
          ) for_types,
          t
        )
      )
    ) in
    Flow_js.unify cx module_t t;

  | (loc, DeclareExportDeclaration {
      DeclareExportDeclaration.default;
      DeclareExportDeclaration.declaration;
      DeclareExportDeclaration.specifiers;
      DeclareExportDeclaration.source;
    }) ->
      let open DeclareExportDeclaration in
      let open Ast.Identifier in
      let export_info = match declaration with
      | Some (Variable (loc, v)) ->
          let { DeclareVariable.id = (_, { name; _; }) } = v in
          statement cx type_params_map (loc, DeclareVariable v);
          [(spf "var %s" name, loc, name, None)]
      | Some (Function (loc, f)) ->
          let { DeclareFunction.id = (_, { name; _; }) } = f in
          statement cx type_params_map (loc, DeclareFunction f);
          [(spf "function %s() {}" name, loc, name, None)]
      | Some (Class (loc, c)) ->
          let { Interface.id = (name_loc, { name; _; }); _; } = c in
          statement cx type_params_map (loc, DeclareClass c);
          [(spf "class %s {}" name, name_loc, name, None)]
      | Some (DefaultType (loc, t)) ->
          let _type = convert cx type_params_map (loc, t) in
          [( "<<type>>", loc, "default", Some _type)]
      | None ->
          [] in

      export_statement cx type_params_map loc
        default export_info specifiers source
        Ast.Statement.ExportDeclaration.ExportValue

  | (loc, ExportDeclaration {
      ExportDeclaration.default;
      ExportDeclaration.declaration;
      ExportDeclaration.specifiers;
      ExportDeclaration.source;
      ExportDeclaration.exportKind;
    }) ->
      let open ExportDeclaration in
      let export_info = match declaration with
      | Some (Declaration decl) ->
          let decl = if default then nameify_default_export_decl decl else decl in
          statement cx type_params_map decl;
          (match decl with
          | loc, FunctionDeclaration({FunctionDeclaration.id=None; _;}) ->
            if default then
              [("function() {}", loc, internal_name "*default*", None)]
            else failwith (
              "Parser Error: Immediate exports of nameless functions can " ^
              "only exist for default exports!"
            )
          | loc, FunctionDeclaration({FunctionDeclaration.id=Some(_, id); _;}) ->
            let name = id.Ast.Identifier.name in
            [(spf "function %s() {}" name, loc, name, None)]
          | loc, ClassDeclaration({Ast.Class.id=None; _;}) ->
            if default then
              [("class {}", loc, internal_name "*default*", None)]
            else failwith (
              "Parser Error: Immediate exports of nameless classes can " ^
              "only exist for default exports"
            )
          | loc, ClassDeclaration({Ast.Class.id=Some(name_loc, id); _;}) ->
            let name = id.Ast.Identifier.name in
            [(spf "class %s {}" name, name_loc, name, None)]
          | _, VariableDeclaration({VariableDeclaration.declarations; _; }) ->
            let decl_to_bindings accum (loc, decl) =
              let id = snd decl.VariableDeclaration.Declarator.id in
              List.rev (extract_destructured_bindings accum id)
            in
            let bound_names = List.fold_left decl_to_bindings [] declarations in
            bound_names |> List.map (fun (loc, name) ->
              (spf "var %s" name, loc, name, None)
            )
          | _, TypeAlias({TypeAlias.id=(_, id); _;}) ->
            let name = id.Ast.Identifier.name in
            [(spf "type %s = ..." name, loc, name, None)]
          | _, InterfaceDeclaration({Interface.id=(_, id); _;}) ->
            let name = id.Ast.Identifier.name in
            [(spf "interface %s = ..." name, loc, name, None)]
          | _ -> failwith "Parser Error: Invalid export-declaration type!")

      | Some (Expression expr) ->
          if not default then failwith (
            "Parser Error: Exporting an expression is only possible for " ^
            "`export default`!"
          );

          let expr_t = expression cx type_params_map expr in
          [( "<<expression>>", fst expr, "default", Some expr_t)]
      | None -> [] in

      export_statement cx type_params_map loc
        default export_info specifiers source exportKind
  | (import_loc, ImportDeclaration({
      ImportDeclaration.default;
      ImportDeclaration.importKind;
      ImportDeclaration.source;
      ImportDeclaration.specifier;
    })) ->
      let (source_loc, source_literal) = source in
      let module_name = (
        match source_literal.Ast.Literal.value with
        | Ast.Literal.String(value) -> value
        | _ -> failwith  (
            "Parser error: Invalid source type! Must be a string literal."
          )
      ) in
      let (import_str, isType) = (
        match importKind with
        | ImportDeclaration.ImportType -> "import type", true
        | ImportDeclaration.ImportTypeof -> "import typeof", true
        | ImportDeclaration.ImportValue -> "import", false
      ) in

      let import_reason = mk_reason
        (spf "exports of \"%s\"" module_name)
        source_loc
      in

      let module_t = import cx module_name source_loc in

      let get_imported_t get_reason set_reason remote_export_name local_name =
        let imported_t = Flow_js.mk_tvar_where cx get_reason (fun t ->
          let import_type =
            if remote_export_name = "default"
            then ImportDefaultT(get_reason, (local_name, module_name), t)
            else ImportNamedT(get_reason, remote_export_name, t)
          in
          Flow_js.flow cx (module_t, import_type)
        ) in
        (match importKind with
          | ImportDeclaration.ImportType ->
              Flow_js.mk_tvar_where cx set_reason (fun t ->
                Flow_js.flow cx (imported_t, ImportTypeT(set_reason, t))
              )
          | ImportDeclaration.ImportTypeof ->
              Flow_js.mk_tvar_where cx set_reason (fun t ->
                Flow_js.flow cx (imported_t, ImportTypeofT(set_reason, t))
              )
          | ImportDeclaration.ImportValue -> imported_t
        )
      in

      let set_imported_binding reason local_name t =
        let t_generic =
          let lookup_mode = if isType then ForType else ForValue in
          Env_js.get_var_declared_type ~lookup_mode cx local_name reason
        in
        Flow_js.unify cx t t_generic
      in

      (match default with
        | Some(local_ident_loc, local_ident) ->
          let local_name = local_ident.Ast.Identifier.name in

          let reason = mk_reason
            (spf "Default import from `%s`" module_name)
            local_ident_loc
          in
          let imported_t = get_imported_t reason reason "default" local_name in

          let reason = mk_reason
            (spf "%s %s from \"%s\"" import_str local_name module_name)
            import_loc
          in
          set_imported_binding reason local_name imported_t
        | None -> ()
      );

      (match specifier with
        | Some(ImportDeclaration.Named(_, named_specifiers)) ->
          let import_specifier (specifier_loc, specifier) = (
            let (remote_ident_loc, remote_ident) =
              specifier.ImportDeclaration.NamedSpecifier.id
            in
            let remote_name = remote_ident.Ast.Identifier.name in

            let get_reason_str =
              spf "Named import from module `%s`" module_name
            in

            let (local_name, get_reason, set_reason) = (
              match specifier.ImportDeclaration.NamedSpecifier.name with
              | Some(local_ident_loc, { Ast.Identifier.name = local_name; _; }) ->
                let get_reason = mk_reason get_reason_str remote_ident_loc in
                let set_reason = mk_reason
                  (spf "%s { %s as %s }" import_str remote_name local_name)
                  specifier_loc
                in
                (local_name, get_reason, set_reason)
              | None ->
                let get_reason = mk_reason get_reason_str specifier_loc in
                let set_reason = mk_reason
                  (spf "%s { %s }" import_str remote_name)
                  specifier_loc
                in
                (remote_name, get_reason, set_reason)
            ) in
            let imported_t = get_imported_t get_reason set_reason remote_name local_name in

            set_imported_binding get_reason local_name imported_t
          ) in
          List.iter import_specifier named_specifiers
        | Some(ImportDeclaration.NameSpace(_, (ident_loc, local_ident))) ->
          let local_name = local_ident.Ast.Identifier.name in
          let reason = mk_reason
            (spf "%s * as %s" import_str local_name)
            ident_loc
          in
          (match importKind with
            | ImportDeclaration.ImportType ->
              let msg = spf (
                "This is invalid syntax. Maybe you meant: " ^^
                "`import type %s from \"%s\"`?"
              ) local_name module_name in
              let reason = repos_reason import_loc reason in
              Flow_js.add_error cx [(reason, msg)]
            | ImportDeclaration.ImportTypeof ->
              let set_reason = repos_reason import_loc reason in
              let module_ns_tvar = import_ns cx import_reason module_name source_loc in
              let module_ns_typeof = Flow_js.mk_tvar_where cx set_reason (fun t ->
                Flow_js.flow cx (module_ns_tvar, ImportTypeofT(reason, t))
              ) in
              let get_reason =
                mk_reason
                  (spf "import typeof * as %s from %S" local_name module_name)
                  import_loc
              in
              set_imported_binding get_reason local_name module_ns_typeof
            | ImportDeclaration.ImportValue ->
              let module_ns_tvar = import_ns cx import_reason module_name source_loc in
              set_imported_binding reason local_name module_ns_tvar
          )
        | None -> ()
      )
)


and export_statement cx type_params_map loc
  default declaration_export_info specifiers source exportKind =

  let open Ast.Statement in
  let open ExportDeclaration in
  let (lookup_mode, export_kind_start) = (
    match exportKind with
    | ExportValue -> (ForValue, "export")
    | ExportType -> (ForType, "export type")
  ) in

  let export_reason_start = spf "%s%s" export_kind_start (
    if default then " default" else ""
  ) in

  let export_from_local (export_reason, loc, local_name, local_tvar) = (
    let reason =
      mk_reason (spf "%s %s" export_reason_start export_reason) loc
    in
    let local_tvar = match local_tvar with
    | None -> Env_js.var_ref ~lookup_mode cx local_name reason
    | Some t -> t in

    (**
      * NOTE: We do not use type-only exports as an indicator of an
      *       ES module in order to allow CommonJS modules to export types.
      *
      *       Note that this means that modules that consist only of
      *       type-only exports will be internally considered a CommonJS
      *       module, but this should have minimal observable effect to the
      *       user given CommonJS<->ESModule interop.
      *)
    (if lookup_mode != ForType then
      mark_exports_type cx reason Context.ESModule);

    let local_name = if default then "default" else local_name in
    set_module_t cx reason (fun t ->
      Flow_js.flow cx (
        exports cx,
        SetNamedExportsT(reason, SMap.singleton local_name local_tvar, t)
      )
    )
  ) in

  (match (declaration_export_info, specifiers) with
    (* [declare] export [type] {foo, bar} [from ...]; *)
    | ([], Some(ExportSpecifiers(specifiers))) ->
      let export_specifier specifier = Specifier.(
        let (reason, local_name, remote_name) = (
          match specifier with
          | loc, {id = (_, {Ast.Identifier.name=id; _;}); name=None;} ->
            let reason = mk_reason (spf "export {%s}" id) loc in
            (reason, id, id)
          | loc, {id=(_, {Ast.Identifier.name=id; _;});
                  name=Some(_, {Ast.Identifier.name; _;})} ->
            let reason =
              mk_reason (spf "export {%s as %s}" id name) loc
            in
            (reason, id, name)
        ) in

        (**
          * Determine if we're dealing with the `export {} from` form
          * (and if so, retrieve the ModuleNamespaceObject tvar for the
          *  source module)
          *)
        let source_module_tvar = (
          match source with
          | Some(src_loc, {
              Ast.Literal.value = Ast.Literal.String(module_name);
              _;
            }) ->
              let reason =
                mk_reason "ModuleNamespace for export {} from" src_loc
              in
              Some(import_ns cx reason module_name src_loc)
          | Some(_) -> failwith (
              "Parser Error: `export ... from` must specify a string " ^
              "literal for the source module name!"
            )
          | None -> None
        ) in

        let local_tvar = (
          match source_module_tvar with
          | Some(tvar) ->
            Flow_js.mk_tvar_where cx reason (fun t ->
              Flow_js.flow cx (tvar, GetPropT(reason, (reason, local_name), t))
            )
          | None ->
            Env_js.var_ref ~lookup_mode cx local_name reason
        ) in

        (**
          * NOTE: We do not use type-only exports as an indicator of an
          *       ES module in order to allow CommonJS modules to export
          *       types.
          *
          *       Note that this means that modules that consist only of
          *       type-only exports will be internally considered a
          *       CommonJS module, but this should have minimal observable
          *       effect to the user given CommonJS<->ESModule interop.
          *)
        (if lookup_mode != ForType
        then mark_exports_type cx reason Context.ESModule);

        set_module_t cx reason (fun t ->
          Flow_js.flow cx (
            exports cx,
            SetNamedExportsT(reason, SMap.singleton remote_name local_tvar, t)
          )
        )
      ) in
      List.iter export_specifier specifiers

    (* [declare] export [type] * from "source"; *)
    | ([], Some(ExportBatchSpecifier(_))) ->
      let source_module_name = (
        match source with
        | Some(src_loc, {
            Ast.Literal.value = Ast.Literal.String(module_name);
            _;
          }) ->
            Context.add_require cx module_name loc;
            Type_inference_hooks_js.dispatch_import_hook cx module_name loc;
            module_name
        | _
          -> failwith (
            "Parser Error: `export * from` must specify a string " ^
            "literal for the source module name!"
          )
      ) in


      let reason = mk_reason (spf "export * from \"%s\"" source_module_name) loc in
      mark_exports_type cx reason Context.ESModule;
      set_module_t cx reason (fun t ->
        Flow_js.flow cx (
          get_module_t cx source_module_name reason,
          SetStarExportsT(reason, exports cx, t)
        )
      )

    | ([], None) -> failwith (
        "Parser Error: Export statement missing one of: Declaration, " ^
        "Expression, or Specifier list!"
      )
    | (_, Some _) -> failwith (
        "Parser Error: Export statement with a declaration/expression " ^
        "cannot also include a list of specifiers!"
      )

    (* [declare] export [type] [default] <<declaration>>; *)
    | (export_info, None) ->
      (**
        * Export each declared binding. Some declarations export multiple
        * bindings, like a multi-declarator variable declaration.
        *)
      List.iter export_from_local export_info
  )

and object_prop cx type_params_map map = Ast.Expression.Object.(function
  (* name = function expr *)
  | Property (loc, { Property.kind = Property.Init;
                     key = Property.Identifier (_, {
                       Ast.Identifier.name; typeAnnotation; _ });
                     value = (vloc, Ast.Expression.Function func);
                     _ }) ->
      Ast.Expression.Function.(
        let { params; defaults; rest; body;
              returnType; typeParameters; id; async; generator; _ } = func
        in
        let kind = function_kind ~async ~generator in
        let reason = mk_reason "function" vloc in
        let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
        let ft = mk_function id cx type_params_map ~kind reason typeParameters
          (params, defaults, rest) returnType body this
        in
        Hashtbl.replace (Context.type_table cx) vloc ft;
        SMap.add name ft map
      )

  (* name = non-function expr *)
  | Property (loc, { Property.kind = Property.Init;
      key =
        Property.Identifier (_, { Ast.Identifier.name; _ }) |
        Property.Literal (_, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        });
                   value = v;
                   _ }) ->
    let t = expression cx type_params_map v in
    SMap.add name t map

  (* literal LHS *)
  | Property (loc, { Property.key = Property.Literal _; _ }) ->
    let msg = "non-string literal property keys not supported" in
    Flow_js.add_error cx [mk_reason "" loc, msg];
    map


  (* With the enable_unsafe_getters_and_setters option set, we enable some
   * unsafe support for getters and setters. The main unsafe bit is that we
   * don't properly havok refinements when getter and setter methods are called.
   * When used in objects, they're a little strange. Technically the getter's
   * return type and the setter's param type don't have to be the same.
   *
   * To properly model this, we should keep track of which properties have
   * getters and setters. However, for now we'll be a little overly strict
   * and just enforce that any property that has had a getter and a setter
   * should just let the setter's param type flow to the getter's return type.
   *)

  (* unsafe getter property *)
  | Property (loc, {
      Property.kind = Property.Get;
      key = Property.Identifier (_, { Ast.Identifier.name; typeAnnotation; _ });
      value = (vloc, Ast.Expression.Function func);
      _ })
    when are_getters_and_setters_enabled () ->
    Ast.Expression.Function.(
      let { body; returnType; _ } = func in
      let reason = mk_reason "getter function" vloc in
      let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
      let function_type = mk_function None cx type_params_map reason None
        ([], [], None) returnType body this
      in
      let return_t = extract_getter_type function_type in
      let map, prop_t = (match SMap.get name map with
      | Some prop_t -> map, prop_t
      | _ ->
        let prop_t =
          Flow_js.mk_tvar cx (mk_reason "getter/setter property" vloc) in
        SMap.add name prop_t map, prop_t) in
      Flow_js.unify cx prop_t return_t;
      map
    )

  (* unsafe setter property *)
  | Property (loc, {
    Property.kind = Property.Set;
      key = Property.Identifier (_, { Ast.Identifier.name; typeAnnotation; _ });
      value = (vloc, Ast.Expression.Function func);
      _ })
    when are_getters_and_setters_enabled () ->
    Ast.Expression.Function.(
      let { params; defaults; body; returnType; _ } = func in
      let reason = mk_reason "setter function" vloc in
      let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
      let function_type = mk_function None cx type_params_map reason None
        (params, defaults, None) returnType body this
      in
      let param_t = extract_setter_type function_type in
      let map, prop_t = (match SMap.get name map with
      | Some prop_t -> map, prop_t
      | _ ->
        let prop_t =
          Flow_js.mk_tvar cx (mk_reason "getter/setter property" vloc) in
        SMap.add name prop_t map, prop_t) in
      Flow_js.unify cx prop_t param_t;
      map
    )

  | Property (loc, { Property.kind = Property.Get | Property.Set; _ }) ->
    let msg = "get/set properties not yet supported" in
    Flow_js.add_error cx [mk_reason "" loc, msg];
    map

  (* computed LHS *)
  | Property (loc, { Property.key = Property.Computed _; _ }) ->
    map

  (* spread prop *)
  | SpreadProperty (loc, { SpreadProperty.argument }) ->
    map
)

and prop_map_of_object cx type_params_map props =
  List.fold_left (object_prop cx type_params_map) SMap.empty props

and object_ cx type_params_map reason ?(allow_sealed=true) props =
  Ast.Expression.Object.(
  (* Return an object with specified sealing. *)
  let mk_object ?(sealed=false) map =
    Flow_js.mk_object_with_map_proto cx reason ~sealed map (MixedT reason)
  in
  (* Copy properties from from_obj to to_obj. We should ensure that to_obj is
     not sealed. *)
  let mk_spread from_obj to_obj =
    Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (to_obj, ObjAssignT(reason, from_obj, t, [], true));
    )
  in
  (* When there's no result, return a new object with specified sealing. When
     there's result, copy a new object into it, sealing the result when
     necessary.

     When building an object incrementally, only the final call to this function
     may be with sealed=true, so we will always have an unsealed object to copy
     properties to. *)
  let eval_object ?(sealed=false) (map, result) =
    match result with
    | None -> mk_object ~sealed map
    | Some result ->
      let result =
        if not (SMap.is_empty map)
        then mk_spread (mk_object map) result
        else result
      in
      if not sealed then result else
        Flow_js.mk_tvar_where cx reason (fun t ->
          Flow_js.flow cx (result, ObjSealT (reason, t))
        )
  in

  let sealed, map, result = List.fold_left (fun (sealed, map, result) t ->
    match t with
    | SpreadProperty (loc, { SpreadProperty.argument }) ->
        let spread = expression cx type_params_map argument in
        let obj = eval_object (map, result) in
        let result = mk_spread spread obj in
        false, SMap.empty, Some result
    | Property (loc, { Property.key = Property.Computed k; value = v; _ }) ->
        let k = expression cx type_params_map k in
        let v = expression cx type_params_map v in
        let obj = eval_object (map, result) in
        Flow_js.flow cx (obj, SetElemT (reason, k, v));
        (* TODO: vulnerable to race conditions? *)
        let result = obj in
        sealed, SMap.empty, Some result
    | t ->
        sealed, object_prop cx type_params_map map t, result
  ) (allow_sealed, SMap.empty, None) props in

  let sealed = match result with
    | Some result -> sealed
    | None -> sealed && not (SMap.is_empty map)
  in
  eval_object ~sealed (map, result)
)

and variable cx type_params_map kind
  ?if_uninitialized (loc, vdecl) = Ast.Statement.(
  let value_kind, init_var, declare_var = match kind with
    | VariableDeclaration.Const ->
      Scope.Entry.Const, Env_js.init_const, Env_js.declare_const
    | VariableDeclaration.Let ->
      Scope.Entry.Let None, Env_js.init_let, Env_js.declare_let
    | VariableDeclaration.Var ->
      Scope.Entry.Var, Env_js.init_var, (fun _ _ _ -> ())
  in
  let str_of_kind = Scope.Entry.string_of_value_kind value_kind in
  let { VariableDeclaration.Declarator.id; init } = vdecl in
  match id with
    | (loc, Ast.Pattern.Identifier (_, { Ast.Identifier.
          name; typeAnnotation; optional
        })) ->
        (* simple lvalue *)
        let reason = mk_reason (spf "%s %s" str_of_kind name) loc in
        let has_anno = not (typeAnnotation = None) in
        (match init with
          | Some ((rhs_loc, _) as expr) ->
            declare_var cx name reason; (* prepare for self-refs *)
            let rhs_reason = mk_reason (spf "assignment of var `%s`" name) rhs_loc in
            let rhs = expression cx type_params_map expr in
            let rhs = Flow_js.reposition cx rhs_reason rhs in
            init_var cx name ~has_anno rhs reason
          | None ->
            match if_uninitialized with
            | Some f ->
              if not optional
              then init_var cx name ~has_anno (f loc) reason
            | None ->
              if has_anno
              then Env_js.pseudo_init_declared_type cx name reason
              else declare_var cx name reason;
        )
    | loc, _ ->
        (* compound lvalue *)
        let reason = mk_reason (spf "%s _" str_of_kind) loc in
        let typeAnnotation = type_of_pattern id in
        let has_anno = not (typeAnnotation = None) in
        let t_ = typeAnnotation
          |> mk_type_annotation cx type_params_map reason in
        let t = match init with
          | Some expr -> expression cx type_params_map expr
          | None -> (
            match if_uninitialized with
            | Some f -> f loc
            | None -> void_ loc
          )
        in
        Flow_js.flow cx (t, t_);
        destructuring cx t init (fun cx loc name t ->
          let reason = mk_reason (spf "%s %s" str_of_kind name) loc in
          init_var cx name ~has_anno t reason
        ) id
)

and array_element cx type_params_map undef_loc el = Ast.Expression.(
  match el with
  | Some (Expression e) -> expression cx type_params_map e
  | Some (Spread (_, { SpreadElement.argument })) ->
      array_element_spread cx type_params_map argument
  | None -> EmptyT.at undef_loc
)

and expression_or_spread cx type_params_map = Ast.Expression.(function
  | Expression e -> expression cx type_params_map e
  | Spread (_, { SpreadElement.argument }) -> spread cx type_params_map argument
)

and array_element_spread cx type_params_map (loc, e) =
  let arr = expression cx type_params_map (loc, e) in
  let reason = mk_reason "spread operand" loc in
  Flow_js.mk_tvar_where cx reason (fun tvar ->
    Flow_js.flow cx (arr, ArrT (reason, tvar, []));
  )

and spread cx type_params_map (loc, e) =
  RestT (array_element_spread cx type_params_map (loc, e))

(* NOTE: the is_cond flag is only used when checking the type of conditions in
   `predicates_of_condition`: see comments on function `condition`. *)
and expression ?(is_cond=false) cx type_params_map (loc, e) =
  let t = expression_ ~is_cond cx type_params_map loc e in
  Hashtbl.replace (Context.type_table cx) loc t;
  t

and this_ cx r = Ast.Expression.(
  match Refinement.get cx (loc_of_reason r, This) r with
  | Some t -> t
  | None -> Env_js.get_var cx (internal_name "this") r
)

and super_ cx reason =
  Env_js.get_var cx (internal_name "super") reason

(* Module exports are treated differently than `exports`. The latter is a
   variable that is implicitly set to the empty object at the top of a
   module. As such, properties can be added to it throughout the module,
   corresponding to the pattern exports.foo = ... for exporting foo. As it turns
   out, module.exports is the same object. A different pattern is to reset
   module.exports at the end of the module, like module.exports = { foo: ... }
   to export foo. This makes any properties added to the exports object
   redundant. Both these patterns are modeled by storing module.exports as an
   internal variable, initially set to the empty object, doing inference on a
   module, and then flowing module.exports to exports, so that whatever its
   final value is (initial object or otherwise) is checked against the type
   declared for exports or any other use of exports. *)
and get_module_exports cx reason =
  Env_js.get_var cx (internal_name "exports") reason

and set_module_exports cx reason t =
  Env_js.set_var cx (internal_name "exports") t reason

and expression_ ~is_cond cx type_params_map loc e = Ast.Expression.(match e with

  | Literal lit ->
      literal cx loc lit

  | Identifier (_, { Ast.Identifier.name; _ }) -> identifier cx name loc

  | This ->
      this_ cx (mk_reason "this" loc)

  | Unary u ->
      unary cx type_params_map loc u

  | Update u ->
      update cx type_params_map loc u

  | Binary b ->
      binary cx type_params_map loc b

  | Logical l ->
      logical cx type_params_map loc l

  | TypeCast {
        TypeCast.expression = e;
        typeAnnotation } ->
      let r = mk_reason "typecast" loc in
      let t = mk_type_annotation cx type_params_map r (Some typeAnnotation) in
      Hashtbl.replace (Context.type_table cx) loc t;
      let infer_t = expression cx type_params_map e in
      Flow_js.flow cx (infer_t, t);
      t

  | Member {
      Member._object;
      property = Member.PropertyExpression index;
      _
    } ->
      let reason = mk_reason "access of computed property/element" loc in
      (match Refinement.get cx (loc, e) reason with
      | Some t -> t
      | None ->
        let tobj = expression cx type_params_map _object in
        let tind = expression cx type_params_map index in
        Flow_js.mk_tvar_where cx reason (fun t ->
          Flow_js.flow cx (tobj, GetElemT(reason, tind, t))
        )
      )

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "module"; _ });
      property = Member.PropertyIdentifier (_,
        { Ast.Identifier.name = "exports"; _ });
      _
    } ->
      let reason = mk_reason "module.exports" loc in
      get_module_exports cx reason

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "ReactGraphQL" | "ReactGraphQLLegacy"; _ });
      property = Member.PropertyIdentifier (_,
        { Ast.Identifier.name = "Mixin"; _ });
      _
    } ->
      let reason = mk_reason "ReactGraphQLMixin" loc in
      Flow_js.get_builtin cx "ReactGraphQLMixin" reason

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "super"; _ });
      property = Member.PropertyIdentifier (ploc,
        { Ast.Identifier.name; _ });
      _
    } ->
      let expr_reason = mk_reason (spf "property `%s`" name) loc in
      (match Refinement.get cx (loc, e) expr_reason with
      | Some t -> t
      | None ->
        let prop_reason = mk_reason (spf "property `%s`" name) ploc in

        (* TODO: shouldn't this be `mk_reason "super" super_loc`? *)
        let super = super_ cx expr_reason in

        if Type_inference_hooks_js.dispatch_member_hook cx name ploc super
        then AnyT.at ploc
        else (
          Flow_js.mk_tvar_where cx expr_reason (fun tvar ->
            Flow_js.flow cx (
              super, GetPropT(expr_reason, (prop_reason, name), tvar)
            )
          )
        )
      )

  | Member {
      Member._object;
      property = Member.PropertyIdentifier (ploc, { Ast.Identifier.name; _ });
      _
    } -> (
      let expr_reason = mk_reason (spf "property `%s`" name) loc in
      match Refinement.get cx (loc, e) expr_reason with
      | Some t -> t
      | None ->
        let prop_reason = mk_reason (spf "property `%s`" name) ploc in
        let tobj = expression cx type_params_map _object in
        if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
        then AnyT.at ploc
        else get_prop ~is_cond cx expr_reason tobj (prop_reason, name)
    )

  | Object { Object.properties } ->
    let reason = mk_reason "object literal" loc in
    object_ cx type_params_map reason properties

  | Array { Array.elements } -> (
    let reason = mk_reason "array literal" loc in
    let element_reason = mk_reason "array element" loc in
    match elements with
    | [] ->
        (* empty array, analogous to object with implicit properties *)
        let elemt = Flow_js.mk_tvar cx element_reason in
        ArrT (prefix_reason "empty " reason, elemt, [])
    | elems ->
        (* tup is true if no spreads *)
        (* tset is set of distinct (mod reason) elem types *)
        (* tlist is reverse list of element types if tup, else [] *)
        let tup, tset, tlist = List.fold_left (fun (tup, tset, tlist) elem ->
          let elemt = array_element cx type_params_map loc elem in

          let tup = match elem with Some (Spread _) -> false | _ -> tup in
          let elemt = if tup then elemt else summarize cx elemt in
          tup,
          TypeExSet.add elemt tset,
          if tup then elemt :: tlist else []
        ) (true, TypeExSet.empty, []) elems
        in
        (* composite elem type is union *)
        let elemt = match TypeExSet.elements tset with
        | [t] -> t
        | list -> UnionT (element_reason, list)
        in
        ArrT (reason, elemt, List.rev tlist)
    )

  | Call {
      Call.callee = _, Identifier (_, {
        Ast.Identifier.name = "require";
        _
      });
      arguments
    } when not (Env_js.local_scope_entry_exists cx "require") -> (
      match arguments with
      | [ Expression (_, Literal {
          Ast.Literal.value = Ast.Literal.String module_name; _;
        }) ] ->
        require cx module_name loc
      | _ ->
        let ignore_non_literals =
          FlowConfig.(Opts.((get_unsafe ()).options.ignore_non_literal_requires))
        in
        if not ignore_non_literals then (
          let msg =
            "The parameter passed to require() must be a literal string."
          in
          Flow_js.add_error cx [mk_reason "" loc, msg];
        );
        AnyT.at loc
    )

  | Call {
      Call.callee = _, Identifier (_, {
        Ast.Identifier.name = "requireLazy";
        _
      });
      arguments
    } -> (
      match arguments with
      | [Expression(_, Array({Array.elements;})); Expression(callback_expr);] ->
        (**
         * From a static perspective (and as long as side-effects aren't
         * considered in Flow), a requireLazy call can be viewed as an immediate
         * call to require() for each of the modules, and then an immediate call
         * to the requireLazy() callback with the results of each of the prior
         * calls to require().
         *
         * TODO: requireLazy() is FB-specific. Let's find a way to either
         *       generalize or toggle this only for the FB environment.
         *)

        let element_to_module_tvar tvars = (function
          | Some(Expression(_, Literal({
              Ast.Literal.value = Ast.Literal.String module_name;
              _;
            }))) ->
              let module_tvar = require cx module_name loc in
              module_tvar::tvars
          | _ ->
              let msg =
                "The first arg to requireLazy() must be a literal array of " ^
                "string literals!"
              in
              Flow_js.add_error cx [mk_reason "" loc, msg];
              tvars
        ) in
        let module_tvars = List.fold_left element_to_module_tvar [] elements in
        let module_tvars = List.rev module_tvars in

        let callback_expr_t = expression cx type_params_map callback_expr in
        let reason = mk_reason "requireLazy() callback" loc in
        let _ = func_call cx reason callback_expr_t module_tvars in

        null_ loc

      | _ ->
        let msg =
          "The first arg to requireLazy() must be a literal array of " ^
          "string literals!"
        in
        Flow_js.add_error cx [mk_reason "" loc, msg];

        AnyT.at loc
    )

  | New {
      New.callee = _, Identifier (_, { Ast.Identifier.name = "Function"; _ });
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      List.iter (fun t ->
        Flow_js.flow cx (t, StrT.at loc)
      ) argts;
      let reason = mk_reason "new Function(..)" loc in
      FunT (
        reason,
        Flow_js.dummy_static reason,
        Flow_js.dummy_prototype,
        Flow_js.mk_functiontype [] ~params_names:[] (MixedT reason)
      )
    )

  | New {
      New.callee = _, Identifier (_, { Ast.Identifier.name = "Array"; _ });
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      (match argts with
      | [argt] ->
        let reason = mk_reason "new Array(..)" loc in
        Flow_js.flow cx
          (argt, NumT (replace_reason "array length" reason, AnyLiteral));
        let element_reason = replace_reason "array element" reason in
        let t = Flow_js.mk_tvar cx element_reason in
        ArrT (reason, t, [])
      | _ ->
        let msg = "Use array literal instead of new Array(..)" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        EmptyT.at loc
      )
    )

  | New { New.callee; arguments } ->
      let class_ = expression cx type_params_map callee in
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      new_call cx loc class_ argts

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Ast.Identifier.name = "Object"; _ });
        property = Member.PropertyIdentifier (prop_loc,
          { Ast.Identifier.name; _ });
        _
      };
      arguments
    } ->
      static_method_call_Object cx type_params_map loc prop_loc name arguments

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Ast.Identifier.name = "React"; _ });
        property = Member.PropertyIdentifier (_,
          { Ast.Identifier.name = "createClass"; _ });
        _
      };
      arguments = [ Expression (_, Object { Object.properties = class_props }) ]
    } ->
      react_create_class cx type_params_map loc class_props

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (super_loc,
          { Ast.Identifier.name = "super"; _ });
        property = Member.PropertyIdentifier (ploc,
          { Ast.Identifier.name; _ });
        _
      };
      arguments
    } ->
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      let reason = mk_reason (spf "super.%s(...)" name) loc in
      let super = super_ cx (mk_reason "super" super_loc) in
      let reason_prop = mk_reason (spf "property `%s`" name) ploc in
      Type_inference_hooks_js.dispatch_call_hook cx name ploc super;
      Flow_js.mk_tvar_where cx reason (fun t ->
        let funtype = Flow_js.mk_methodtype super argts t in
        Flow_js.flow cx (super, MethodT (reason, (reason_prop, name), funtype))
      )

  | Call {
      Call.callee = (_, Member {
        Member._object;
        property = Member.PropertyIdentifier (ploc,
          { Ast.Identifier.name; _ });
        _
      }) as callee;
      arguments
    } ->
      (* method call *)
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      let reason = mk_reason (spf "call of method `%s`" name) loc in
      let ot = expression cx type_params_map _object in
      Type_inference_hooks_js.dispatch_call_hook cx name ploc ot;
      (match Refinement.get cx callee reason with
      | Some f ->
          (* note: the current state of affairs is that we understand
             member expressions as having refined types, rather than
             understanding receiver objects as carrying refined properties.
             generalizing this properly is a todo, and will deliver goodness.
             meanwhile, here we must hijack the property selection normally
             performed by the flow algorithm itself. *)
          Env_js.havoc_heap_refinements ();
          Flow_js.mk_tvar_where cx reason (fun t ->
            let frame = Env_js.peek_frame () in
            let app = Flow_js.mk_methodtype2 ot argts t frame in
            Flow_js.flow cx (f, CallT (reason, app));
          )
      | None ->
          Env_js.havoc_heap_refinements ();
          Flow_js.mk_tvar_where cx reason (fun t ->
            let frame = Env_js.peek_frame () in
            let reason_prop = mk_reason (spf "property `%s`" name) ploc in
            let app = Flow_js.mk_methodtype2 ot argts t frame in
            Flow_js.flow cx (ot, MethodT(reason, (reason_prop, name), app))
          )
      )

  | Call {
      Call.callee = _, Identifier (ploc, { Ast.Identifier.name = "super"; _ });
      arguments
    } ->
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      let reason = mk_reason "super(...)" loc in
      let super_reason = mk_reason "super" ploc in

      (* switch back env entries for this and super from undefined *)
      define_internal cx reason "this";
      define_internal cx reason "super";

      let super = super_ cx super_reason in
      Flow_js.mk_tvar_where cx reason (fun t ->
        let funtype = Flow_js.mk_methodtype super argts t in
        Flow_js.flow cx (super,
          MethodT(reason, (super_reason, "constructor"), funtype))
      )

  (******************************************)
  (* See ~/www/static_upstream/core/ *)

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "invariant"; _ });
      arguments
    } ->
      (* TODO: require *)
      let reason = mk_reason "invariant" loc in
      (match arguments with
      | (Expression (_, Literal {
          Ast.Literal.value = Ast.Literal.Boolean false; _;
        }))::arguments ->
        (* invariant(false, ...) is treated like a throw *)
        ignore (List.map (expression_or_spread cx type_params_map) arguments);
        Env_js.reset_current_activation reason;
        Abnormal.(set Throw)
      | (Expression cond)::arguments ->
        ignore (List.map (expression_or_spread cx type_params_map) arguments);
        let _, preds, not_preds, xtypes = predicates_of_condition cx type_params_map cond in
        Env_js.refine_with_preds cx reason preds xtypes
      | _ ->
        let msg = "unsupported arguments in call to invariant()" in
        Flow_js.add_error cx [mk_reason "" loc, msg]
      );
      void_ loc

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "copyProperties"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      let reason = mk_reason "object extension" loc in
      chain_objects cx reason (List.hd argts) (List.tl argts)

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "mergeInto"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      let reason = mk_reason "object extension" loc in
      ignore (chain_objects cx reason (List.hd argts) (List.tl argts));
      void_ loc

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "mergeDeepInto"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      ignore argts; (* TODO *)
      void_ loc

  | Call {
      Call.callee = _, Identifier (_, { Ast.Identifier.name = "merge"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      let reason = mk_reason "object" loc in
      spread_objects cx reason argts

  | Call {
      Call.callee = _, Identifier (_, { Ast.Identifier.name = "mixin"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      let reason = mk_reason "object" loc in
      ClassT (spread_objects cx reason argts)

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "classWithMixins"; _ });
      arguments
    } ->
    (* TODO *)
    AnyT.t

  | Call { Call.callee; arguments } ->
      let f = expression cx type_params_map callee in
      let reason = mk_reason "function call" loc in
      let argts =
        List.map (expression_or_spread cx type_params_map) arguments in
      func_call cx reason f argts

  | Conditional { Conditional.test; consequent; alternate } ->
      let reason = mk_reason "conditional" loc in
      let _, preds, not_preds, xtypes =
        predicates_of_condition cx type_params_map test in
      let env =  Env_js.peek_env () in
      let oldset = Changeset.clear () in

      let then_env = Env_js.clone_env env in
      Env_js.update_env cx reason then_env;
      Env_js.refine_with_preds cx reason preds xtypes;
      let t1 = expression cx type_params_map consequent in

      let else_env = Env_js.clone_env env in
      Env_js.update_env cx reason else_env;
      Env_js.refine_with_preds cx reason not_preds xtypes;
      let t2 = expression cx type_params_map alternate in

      let newset = Changeset.merge oldset in
      Env_js.merge_env cx reason (env, then_env, else_env) newset;
      Env_js.update_env cx reason env;
      (* TODO call loc_of_predicate on some pred?
         t1 is wrong but hopefully close *)
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (t1, t);
        Flow_js.flow cx (t2, t);
      )

  | Assignment { Assignment.operator; left; right } ->
      assignment cx type_params_map loc (left, operator, right)

  | Sequence { Sequence.expressions } ->
      List.fold_left (fun t e ->
        expression cx type_params_map e) (void_ loc
      ) expressions

  | Function {
      Function.id;
      params; defaults; rest;
      body;
      async;
      generator;
      returnType;
      typeParameters;
      _
    } ->
      let kind = function_kind ~async ~generator in
      let desc = Scope.(match kind with
      | Ordinary -> "function"
      | Async -> "async function"
      | Generator -> "generator function"
      | Module -> assert_false "module scope as function activation"
      | Global -> assert_false "global scope as function activation"
      ) in
      let reason = mk_reason desc loc in
      let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
      mk_function id cx type_params_map reason ~kind
        typeParameters (params, defaults, rest) returnType body this

  | ArrowFunction {
      ArrowFunction.id;
      params; defaults; rest;
      body;
      async;
      returnType;
      typeParameters;
      _
    } ->
      let kind = function_kind ~async ~generator:false in
      let desc = Scope.(match kind with
      | Ordinary -> "arrow function"
      | Async -> "async arrow function"
      | Generator -> assert_false "generator arrow function"
      | Module -> assert_false "module scope as arrow function activation"
      | Global -> assert_false "global scope as arrow function activation"
      ) in
      let reason = mk_reason desc loc in
      let this = this_ cx reason in
      let super = super_ cx reason in
      mk_arrow id cx type_params_map reason ~kind
        typeParameters (params, defaults, rest) returnType body this super

  | TaggedTemplate {
      TaggedTemplate.tag = _, Identifier (_,
        { Ast.Identifier.name = "query"; _ });
      quasi = _, { TemplateLiteral.quasis; expressions }
    } ->
    List.iter (fun e -> ignore (expression cx type_params_map e)) expressions;
    (*parse_graphql cx encaps;*)
    void_ loc

  | TaggedTemplate {
      TaggedTemplate.tag;
      quasi = _, { TemplateLiteral.quasis; expressions }
    } ->
      List.iter (fun e -> ignore (expression cx type_params_map e)) expressions;
      let t = expression cx type_params_map tag in
      let reason = mk_reason "encaps tag" loc in
      let reason_array = replace_reason "array" reason in
      let ret = Flow_js.mk_tvar cx reason in
      let ft = Flow_js.mk_functiontype
        [ ArrT (reason_array, StrT.why reason, []);
          RestT (AnyT.why reason) ]
        ret
      in
      Flow_js.flow cx (t, CallT (reason, ft));
      ret

  | TemplateLiteral {
      TemplateLiteral.quasis;
      expressions
    } ->
      List.iter (fun e -> ignore (expression cx type_params_map e)) expressions;
      StrT.at loc

  | JSXElement e ->
      jsx cx type_params_map e

  | Class c ->
      let (name_loc, name) = extract_class_name loc c in
      let reason = mk_reason (spf "class expr `%s`" name) loc in
      (match c.Ast.Class.id with
      | Some id ->
          let tvar = Flow_js.mk_tvar cx reason in
          let scope = Scope.fresh () in
          Scope.(
            let implicit = Entry.ClassNameBinding in
            let entry = Entry.(
              new_let tvar ~loc:name_loc ~state:State.Declared ~implicit
            ) in
            add_entry name entry scope
          );
          Env_js.push_var_scope cx scope;
          let class_t = mk_class cx type_params_map loc reason c in
          Env_js.pop_var_scope ();
          Flow_js.flow cx (class_t, tvar);
          class_t;
      | None -> mk_class cx type_params_map loc reason c)

  | Yield { Yield.argument; delegate = false } ->
      let reason = mk_reason "yield" loc in
      let yield = Env_js.get_var cx (internal_name "yield") reason in
      let t = match argument with
      | Some expr -> expression cx type_params_map expr
      | None -> VoidT.at loc in
      Flow_js.flow cx (t, yield);
      let next = Env_js.get_var cx (internal_name "next") reason in
      OptionalT next

  | Yield { Yield.argument; delegate = true } ->
      let reason = mk_reason "yield* delegate" loc in
      let next = Env_js.get_var cx
        (internal_name "next")
        (prefix_reason "next of parent generator in " reason) in
      let yield = Env_js.get_var cx
        (internal_name "yield")
        (prefix_reason "yield of parent generator in " reason) in
      let t = match argument with
      | Some expr -> expression cx type_params_map expr
      | None -> assert_false "delegate yield without argument" in

      let ret = Flow_js.mk_tvar cx
        (prefix_reason "return of child generator in " reason) in

      (* widen yield with the element type of the delegated-to iterable *)
      let iterable = Flow_js.get_builtin_typeapp cx
        (mk_reason "iteration expected on Iterable" loc)
        "$Iterable"
        [yield; ret; next] in
      Flow_js.flow cx (t, iterable);

      ret

  (* TODO *)
  | Comprehension _
  | Generator _
  | Let _ ->
    Flow_js.add_error cx [mk_reason "" loc, "not (sup)ported"];
    EmptyT.at loc
)

(* We assume that constructor functions return void
   and constructions return objects.
   TODO: This assumption does not always hold.
   If construction functions return non-void values (e.g., functions),
   then those values are returned by constructions.
*)
and new_call cx tok class_ argts =
  let reason = mk_reason "constructor call" tok in
  Flow_js.mk_tvar_where cx reason (fun t ->
    Flow_js.flow cx (class_, ConstructorT (reason, argts, t));
  )

and func_call cx reason func_t argts =
  Env_js.havoc_heap_refinements ();
  Flow_js.mk_tvar_where cx reason (fun t ->
    let frame = Env_js.peek_frame () in
    let app = Flow_js.mk_functiontype2 argts t frame in
    Flow_js.flow cx (func_t, CallT(reason, app))
  )

(* traverse a literal expression, return result type *)
and literal cx loc lit = Ast.Literal.(match lit.value with
  | String s ->
      StrT (mk_reason "string" loc, Literal s)

  | Boolean b ->
      BoolT (mk_reason "boolean" loc, Some b)

  | Null ->
      null_ loc

  | Number f ->
      NumT (mk_reason "number" loc, Literal (f, lit.raw))

  | RegExp rx ->
      Flow_js.get_builtin_type cx (mk_reason "regexp" loc) "RegExp"
)

(* traverse a unary expression, return result type *)
and unary cx type_params_map loc = Ast.Expression.Unary.(function
  | { operator = Not; argument; _ } ->
      let arg = expression cx type_params_map argument in
      let reason = mk_reason "not operator" loc in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (arg, NotT (reason, t));
      )

  | { operator = Plus; argument; _ } ->
      ignore (expression cx type_params_map argument);
      NumT.at loc

  | { operator = Minus; argument; _ } ->
      let arg = expression cx type_params_map argument in
      let reason = mk_reason "unary minus operator" loc in
      Flow_js.mk_tvar_derivable_where cx reason (fun t ->
        Flow_js.flow cx (arg, UnaryMinusT (reason, t));
      )

  | { operator = BitNot; argument; _ } ->
      let t = NumT.at loc in
      Flow_js.flow cx (expression cx type_params_map argument, t);
      t

  | { operator = Typeof; argument; _ } ->
      ignore (expression cx type_params_map argument);
      StrT.at loc

  | { operator = Void; argument; _ } ->
      ignore (expression cx type_params_map argument);
      void_ loc

  | { operator = Delete; argument; _ } ->
      ignore (expression cx type_params_map argument);
      BoolT.at loc

  | { operator = Await; argument; _ } ->
    (** TODO: await should look up Promise in the environment instead of going
        directly to the core definition. Otherwise, the following won't work
        with a polyfilled Promise! **)
    (* see declaration of $await in core.js:
       if argument is a Promise<T>, then (await argument) returns T.
       otherwise it just returns the argument type.
       TODO update this comment when recursive unwrapping of
       Promise is done.
     *)
    let reason = mk_reason "await" loc in
    let await = Flow_js.get_builtin cx "$await" reason in
    let arg = expression cx type_params_map argument in
    func_call cx reason await [arg]
)

(* numeric pre/post inc/dec *)
and update cx type_params_map loc expr = Ast.Expression.Update.(
  let reason = mk_reason "update" loc in
  let result_t = NumT.why reason in
  (match expr.argument with
  | _, Ast.Expression.Identifier (id_loc, { Ast.Identifier.name; _ }) ->
    let lhs_t = identifier cx name id_loc in
    Flow_js.flow cx (lhs_t, result_t);
     (* enforce state-based guards for binding update, e.g., const *)
     let id_reason = mk_reason name id_loc in
     Env_js.set_var cx name result_t id_reason
  | expr ->
    let lhs_t = expression cx type_params_map expr in
    Flow_js.flow cx (lhs_t, result_t)
  );
  result_t
)

(* traverse a binary expression, return result type *)
and binary cx type_params_map loc = Ast.Expression.Binary.(function
  | { operator = Equal; left; right }
  | { operator = NotEqual; left; right } ->
      let reason = mk_reason "non-strict equality comparison" loc in
      let t1 = expression cx type_params_map left in
      let t2 = expression cx type_params_map right in
      Flow_js.flow cx (t1, EqT (reason,t2));
      BoolT.at loc

  | { operator = In; left = (loc1, _) as left; right = (loc2, _) as right } ->
      let t1 = expression cx type_params_map left in
      let t2 = expression cx type_params_map right in
      let reason1 = mk_reason "LHS of `in` operator" loc1 in
      let reason2 = mk_reason "RHS of `in` operator" loc2 in
      let lhs = UnionT (reason1, [StrT.why reason1; NumT.why reason1]) in
      let rhs =
        let elemt = Flow_js.mk_tvar cx reason2 in
        UnionT (reason2, [AnyObjT reason2; ArrT (reason2, elemt, [])])
      in
      Flow_js.flow cx (t1, lhs);
      Flow_js.flow cx (t2, rhs);
      BoolT.at loc

  | { operator = StrictEqual; left; right }
  | { operator = StrictNotEqual; left; right }
  | { operator = Instanceof; left; right } ->
      ignore (expression cx type_params_map left);
      ignore (expression cx type_params_map right);
      BoolT.at loc

  | { operator = LessThan; left; right }
  | { operator = LessThanEqual; left; right }
  | { operator = GreaterThan; left; right }
  | { operator = GreaterThanEqual; left; right } ->
      let reason = mk_reason "relational comparison" loc in
      let t1 = expression cx type_params_map left in
      let t2 = expression cx type_params_map right in
      Flow_js.flow cx (t1, ComparatorT (reason,t2));
      BoolT.at loc

  | { operator = LShift; left; right }
  | { operator = RShift; left; right }
  | { operator = RShift3; left; right }
  | { operator = Minus; left; right }
  | { operator = Mult; left; right }
  | { operator = Div; left; right }
  | { operator = Mod; left; right }
  | { operator = BitOr; left; right }
  | { operator = Xor; left; right }
  | { operator = BitAnd; left; right } ->
      let t = NumT.at loc in
      Flow_js.flow cx (expression cx type_params_map left, t);
      Flow_js.flow cx (expression cx type_params_map right, t);
      t

  | { operator = Plus; left; right } ->
      let reason = mk_reason "+" loc in
      let t1 = expression cx type_params_map left in
      let t2 = expression cx type_params_map right in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (t1, AdderT (reason, t2, t));
      )
)

and logical cx type_params_map loc = Ast.Expression.Logical.(function
  | { operator = Or; left; right } ->
      let t1, _, not_map, xtypes = predicates_of_condition cx type_params_map left in
      let reason = mk_reason "||" loc in
      let t2 = Env_js.in_refined_env cx reason not_map xtypes
        (fun () -> expression cx type_params_map right)
      in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (t1, OrT (reason, t2, t));
      )

  | { operator = And; left; right } ->
      let t1, map, _, xtypes = predicates_of_condition cx type_params_map left in
      let reason = mk_reason "&&" loc in
      let t2 = Env_js.in_refined_env cx reason map xtypes
        (fun () -> expression cx type_params_map right)
      in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (t1, AndT (reason, t2, t));
      )
)

and assignment_lhs cx type_params_map = Ast.Pattern.(function
  | loc, Object _
  | loc, Array _ ->
      error_destructuring cx loc;
      AnyT.at loc

  | _, Identifier (loc, { Ast.Identifier.name; _ }) ->
      identifier cx name loc

  | _, Expression ((_, Ast.Expression.Member _) as m) ->
      expression cx type_params_map m

  (* parser will error before we get here *)
  | _ -> assert false
)

(* traverse assignment expressions *)
and assignment cx type_params_map loc = Ast.Expression.(function

  (* r = e *)
  | (r, Assignment.Assign, e) ->

      (* compute the type of the RHS. this is what we return *)
      let t = expression cx type_params_map e in

      (* update env, add constraints arising from LHS structure,
         handle special cases, etc. *)
      (match r with

        (* module.exports = e *)
        | lhs_loc, Ast.Pattern.Expression (_, Member {
            Member._object = _, Ast.Expression.Identifier (_,
              { Ast.Identifier.name = "module"; _ });
            property = Member.PropertyIdentifier (_,
              { Ast.Identifier.name = "exports"; _ });
            _
          }) ->
            let reason = mk_reason "assignment of module.exports" lhs_loc in
            mark_exports_type cx reason (Context.CommonJSModule(Some(lhs_loc)));
            set_module_exports cx reason t

        (* super.name = e *)
        | lhs_loc, Ast.Pattern.Expression (_, Member {
            Member._object = _, Identifier (_,
              { Ast.Identifier.name = "super"; _ });
            property = Member.PropertyIdentifier (ploc,
              { Ast.Identifier.name; _ });
            _
          }) ->
            let reason =
              mk_reason (spf "assignment of property `%s`" name) lhs_loc in
            let prop_reason = mk_reason (spf "property `%s`" name) ploc in
            let super = super_ cx reason in
            Flow_js.flow cx (super, SetPropT(reason, (prop_reason, name), t))

        (* _object.name = e *)
        | lhs_loc, Ast.Pattern.Expression ((_, Member {
            Member._object;
            property = Member.PropertyIdentifier (ploc,
              { Ast.Identifier.name; _ });
            _
          }) as expr) ->
            let o = expression cx type_params_map _object in
            (* if we fire this hook, it means the assignment is a sham. *)
            if not (Type_inference_hooks_js.dispatch_member_hook cx name ploc o)
            then (
              let reason = mk_reason
                (spf "assignment of property `%s`" name) lhs_loc in
              let prop_reason = mk_reason (spf "property `%s`" name) ploc in

              (* flow type to object property itself *)
              Flow_js.flow cx (o, SetPropT (reason, (prop_reason, name), t));

              (* types involved in the assignment are computed
                 in pre-havoc environment. it's the assignment itself
                 which clears refis *)
              (* TODO: havoc refinements for this prop name only *)
              Env_js.havoc_heap_refinements_with_propname name;

              (* add type refinement if LHS is a pattern we handle *)
              match Refinement.key expr with
              | Some key ->
                (* NOTE: currently, we allow property refinements to propagate
                   even if they may turn out to be invalid w.r.t. the
                   underlying object type. If invalid, of course, they produce
                   errors, but in the future we may want to prevent the
                   invalid types from flowing downstream as well.
                   Doing so would require that we defer any subsequent flow
                   calls that are sensitive to the refined type until the
                   object and refinement types - `o` and `t` here - are
                   fully resolved.
                 *)
                Env_js.set_expr cx key reason t t
              | None ->
                ()
            )

        (* _object[index] = e *)
        | lhs_loc, Ast.Pattern.Expression (_, Member {
            Member._object;
            property = Member.PropertyExpression index;
            _
          }) ->
            let reason =
              mk_reason "assignment of computed property/element" lhs_loc in
            let a = expression cx type_params_map _object in
            let i = expression cx type_params_map index in
            Flow_js.flow cx (a, SetElemT (reason, i, t));

            (* types involved in the assignment itself are computed
               in pre-havoc environment. it's the assignment itself
               which clears refis *)
            Env_js.havoc_heap_refinements ();

        (* other r structures are handled as destructuring assignments *)
        | _ ->
            destructuring_assignment cx t e r
      );
      t

  | (lhs, Assignment.PlusAssign, rhs) ->
      (* lhs += rhs *)
      let reason = mk_reason "+=" loc in
      let lhs_t = assignment_lhs cx type_params_map lhs in
      let rhs_t = expression cx type_params_map rhs in
      let result_t = Flow_js.mk_tvar cx reason in
      (* lhs = lhs + rhs *)
      Flow_js.flow cx (lhs_t, AdderT (reason, rhs_t, result_t));
      Flow_js.flow cx (rhs_t, AdderT (reason, lhs_t, result_t));
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier (id_loc, { Ast.Identifier.name; _ }) ->
        let id_reason = mk_reason name id_loc in
        Env_js.set_var cx name result_t id_reason
      | _ -> ()
      );
      lhs_t

  | (lhs, Assignment.MinusAssign, rhs)
  | (lhs, Assignment.MultAssign, rhs)
  | (lhs, Assignment.DivAssign, rhs)
  | (lhs, Assignment.ModAssign, rhs)
  | (lhs, Assignment.LShiftAssign, rhs)
  | (lhs, Assignment.RShiftAssign, rhs)
  | (lhs, Assignment.RShift3Assign, rhs)
  | (lhs, Assignment.BitOrAssign, rhs)
  | (lhs, Assignment.BitXorAssign, rhs)
  | (lhs, Assignment.BitAndAssign, rhs)
    ->
      (* lhs (numop)= rhs *)
      let reason = mk_reason "(numop)=" loc in
      let result_t = NumT.why reason in
      let lhs_t = assignment_lhs cx type_params_map lhs in
      let rhs_t = expression cx type_params_map rhs in
      (* lhs = lhs (numop) rhs *)
      Flow_js.flow cx (lhs_t, result_t);
      Flow_js.flow cx (rhs_t, result_t);
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier (id_loc, { Ast.Identifier.name; _ }) ->
        let id_reason = mk_reason name id_loc in
        Env_js.set_var cx name result_t id_reason
      | _ -> ()
      );
      lhs_t
)

(* Object assignment patterns. In the `copyProperties` model (chain_objects), an
   existing object receives properties from other objects. This pattern suffers
   from "races" in the type checker, since the object supposed to receive
   properties is available even when the other objects supplying the properties
   are not yet available. In the `mergeProperties` model (spread_objects), a new
   object receives properties from other objects and is returned, but the new
   object is made available only when the properties have actually been
   received. Similarly, clone_object makes the receiving object available only
   when the properties have actually been received. These patterns are useful
   when merging properties across modules, e.g., and should eventually replace
   other patterns wherever they are potentially racy. *)

and spread_objects cx reason those =
  chain_objects cx reason (mk_object cx reason) those

and clone_object_with_excludes cx reason this that excludes =
  Flow_js.mk_tvar_where cx reason (fun tvar ->
    Flow_js.flow cx (
      this,
      ObjAssignT(reason, that, ObjRestT(reason, excludes, tvar), [], true)
    )
  )

and clone_object cx reason this that =
  clone_object_with_excludes cx reason this that []

and chain_objects cx reason this those =
  List.fold_left (fun result that ->
    Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (result, ObjAssignT(reason, that, t, [], true));
    )
  ) this those

and react_ignored_attributes = [ "key"; "ref"; ]

and react_ignore_attribute aname =
  List.mem aname react_ignored_attributes

and jsx cx type_params_map = Ast.JSX.(function { openingElement; closingElement; children } ->
  jsx_title cx type_params_map openingElement (List.map (jsx_body cx type_params_map) children)
)

and jsx_title cx type_params_map openingElement children = Ast.JSX.(
  let eloc, { Opening.name; attributes; _ } = openingElement in
  match name with

  | Identifier (_, { Identifier.name }) when name = String.capitalize name ->
      let reason = mk_reason (spf "React element `%s`" name) eloc in
      let c = Env_js.get_var cx name reason in
      let map = ref SMap.empty in
      let spread = ref None in
      attributes |> List.iter (function
        | Opening.Attribute (aloc, { Attribute.
              name = Attribute.Identifier (_, { Identifier.name = aname });
              value
            }) ->
            let atype = (match value with
              | Some (Attribute.Literal (loc, lit)) ->
                  literal cx loc lit
              | Some (Attribute.ExpressionContainer (_, {
                  ExpressionContainer.expression = Some (loc, e)
                })) ->
                  expression cx type_params_map (loc, e)
              | _ ->
                  (* empty or nonexistent attribute values *)
                  EmptyT.at aloc
            ) in

            if not (react_ignore_attribute aname)
            then map := !map |> SMap.add aname atype

        | Opening.Attribute _ ->
            () (* TODO: attributes with namespaced names *)

        | Opening.SpreadAttribute (aloc, { SpreadAttribute.argument }) ->
            let ex_t = expression cx type_params_map argument in
            spread := Some (ex_t)
      );

      let reason_props = prefix_reason "props of " reason in
      let o = Flow_js.mk_object_with_map_proto cx reason_props
        !map (MixedT reason_props)
      in
      let o = match !spread with
        | None -> o
        | Some ex_t ->
            let reason_prop = prefix_reason "spread of " (reason_of_t ex_t) in
            clone_object_with_excludes cx reason_prop o ex_t react_ignored_attributes
      in
      (* TODO: children *)
      let react = require cx "react" eloc in
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        let reason_createElement = mk_reason "property `createElement`" eloc in
        Flow_js.flow cx (react, MethodT(
          reason,
          (reason_createElement, "createElement"),
          Flow_js.mk_methodtype react [c;o] tvar
        ))
      )

  | Identifier (_, { Identifier.name }) ->
      attributes |> List.iter (function
        | Opening.Attribute (aloc, { Attribute.
              name = Attribute.Identifier (_, { Identifier.name = aname });
              value
            }) ->
            ignore (match value with
              | Some (Attribute.Literal (loc, lit)) ->
                  literal cx loc lit
              | Some (Attribute.ExpressionContainer (_, {
                  ExpressionContainer.expression = Some (loc, e)
                })) ->
                  expression cx type_params_map (loc, e)
              | _ ->
                  (* empty or nonexistent attribute values *)
                  EmptyT.at aloc
            )

        | Opening.Attribute _ ->
            () (* TODO: attributes with namespaced names *)

        | Opening.SpreadAttribute (aloc, { SpreadAttribute.argument }) ->
            () (* TODO: spread attributes *)
      );

      AnyT.t

  | _ ->
      (* TODO? covers namespaced names, member expressions as element names *)
      AnyT.at eloc
)

and jsx_body cx type_params_map = Ast.JSX.(function
  | loc, Element e -> jsx cx type_params_map e
  | loc, ExpressionContainer ec -> (
      let { ExpressionContainer.expression = ex } = ec in
      match ex with
        | Some (loc, e) -> expression cx type_params_map (loc, e)
        | None -> EmptyT (mk_reason "empty jsx body" loc)
    )
  | loc, Text s -> StrT.at loc
)

(* Native support for React.PropTypes validation functions, which are
   interpreted as type annotations for React props. This strategy is reasonable
   because the validation functions enforce types at run time (during
   development), and we can always insist on them because they are turned off in
   production. *)

and mk_proptype cx type_params_map = Ast.Expression.(function
  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "isRequired"; _ });
      _object = e;
      _
    } ->
      mk_proptype cx type_params_map e

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "number"; _ });
      _
    } ->
      NumT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "string"; _ });
      _
    } ->
      StrT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "bool"; _ });
      _
    } ->
      BoolT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "array"; _ });
      _
    } ->
      ArrT (mk_reason "array" vloc, AnyT.at vloc, [])

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "func"; _ });
      _
    } ->
      AnyFunT (mk_reason "func" vloc)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "object"; _ });
      _
    } ->
      AnyObjT (mk_reason "object" vloc)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "node"; _ });
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "element"; _ });
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "arrayOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      ArrT (mk_reason "arrayOf" vloc, mk_proptype cx type_params_map e, [])

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "instanceOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      Flow_js.mk_instance cx (mk_reason "instanceOf" vloc)
        (expression cx type_params_map e)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "objectOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      let flags = {
        frozen = false;
        sealed = UnsealedInFile (Loc.source vloc);
        exact = true
      } in
      let dict = Some {
        dict_name = None;
        key = AnyT.t;
        value = mk_proptype cx type_params_map e
      } in
      let pmap = Flow_js.mk_propmap cx SMap.empty in
      let proto = MixedT (reason_of_string "Object") in
      ObjT (mk_reason "objectOf" vloc, Flow_js.mk_objecttype ~flags dict pmap proto)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "oneOf"; _ });
         _
      };
      arguments = [Expression (_, Array { Array.elements })]
    } ->
      let rec string_literals lits es = match (es) with
        | Some (Expression (loc, Literal { Ast.Literal.
            value = Ast.Literal.String lit; _
          })) :: tl ->
            string_literals (lit :: lits) tl
        | [] -> Some lits
        | _  -> None in
      (match string_literals [] elements with
        | Some lits ->
            let reason = mk_reason "oneOf" vloc in
            mk_keys_type reason lits
        | None -> AnyT.at vloc)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "oneOfType"; _ });
         _
      };
      arguments = [Expression (_, Array { Array.elements })]
    } ->
      let rec proptype_elements ts es = match es with
        | Some (Expression e) :: tl ->
            proptype_elements (mk_proptype cx type_params_map e :: ts) tl
        | [] -> Some ts
        | _ -> None in
      let reason = mk_reason "oneOfType" vloc in
      (match proptype_elements [] elements with
        | Some ts -> UnionT (reason, ts)
        | None -> AnyT.at vloc)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "shape"; _ });
         _
      };
      arguments = [Expression (_, Object { Object.properties })];
    } ->
      let reason = mk_reason "shape" vloc in
      let amap, omap, dict = mk_proptypes cx type_params_map properties in
      let map = SMap.union amap (SMap.map (fun t -> OptionalT t) omap) in
      Flow_js.mk_object_with_map_proto cx reason ?dict map (MixedT reason)

  (* Support for FB-specific ReactPropTypes validators. *)
  (** TODO: instead, route to custom lib defs, somehow...details of which have
      not been set up or even worked out yet. **)
  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "Fbt"; _ });
      _
    } ->
      (* We assume that there is a Fbt type defined in the global scope. *)
      Flow_js.get_builtin_type cx (mk_reason "Fbt" vloc) "Fbt"

  | vloc, _ -> AnyT.at vloc
)

and mk_proptypes cx type_params_map props = Ast.Expression.Object.(
  List.fold_left (fun (amap, omap, dict) -> function

    (* required prop *)
    | Property (loc, { Property.
        kind = Property.Init;
        key = Property.Identifier (_, {
          Ast.Identifier.name; _ });
        value = (vloc, Ast.Expression.Member {
          Ast.Expression.Member.
          property = Ast.Expression.Member.PropertyIdentifier (_, {
            Ast.Identifier.name = "isRequired"; _ });
          _object = e;
          _
        });
        _ }) ->
        let tvar = mk_proptype cx type_params_map e in
        SMap.add name tvar amap,
        omap,
        dict

    (* other prop *)
    | Property (loc, { Property.kind = Property.Init;
        key =
          Property.Identifier (_, { Ast.Identifier.name; _ }) |
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name;
            _;
          });
        value = v;
        _ }) ->
        let tvar = mk_proptype cx type_params_map v in
        amap,
        SMap.add name tvar omap,
        dict

    (* spread prop *)
    | SpreadProperty (loc, { SpreadProperty.argument }) ->
      (* Instead of modeling the spread precisely, we instead make the propTypes
         extensible. This has the effect of loosening the check for properties
         added by the spread, while leaving the checking of other properties as
         is. FWIW we use a similar approximation for mixins. It would be nice to
         be more precise here, but given that this only affects legacy React
         classes, and that reconstructing props is already fairly delicate in
         that world, it may not be worth it to spend time on this right now. *)
      amap, omap, Some { dict_name=None; key=StrT.t; value=AnyT.t; }

    (* literal LHS *)
    | Property (loc, { Property.key = Property.Literal _; _ }) ->
      let msg =
        "non-string literal property keys not supported for React propTypes" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      amap, omap, dict

    (* get/set kind *)
    | Property (loc, { Property.kind = Property.Get | Property.Set; _ }) ->
      let msg = "get/set properties not supported for React propTypes" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      amap, omap, dict

    (* computed LHS *)
    | Property (loc, { Property.key = Property.Computed _; _ }) ->
      let msg = "computed property keys not supported for React propTypes" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      amap, omap, dict

  ) (SMap.empty, SMap.empty, None) props
)

(* Legacy: generate React class from specification object. *)
and react_create_class cx type_params_map loc class_props = Ast.Expression.(
  let reason_class = mk_reason "React class" loc in
  let reason_component = mk_reason "React component" loc in
  let this = Flow_js.mk_tvar cx reason_component in
  let mixins = ref [] in
  let static_reason = prefix_reason "statics of " reason_class in
  let static = ref (mk_object cx static_reason) in
  let default_reason = prefix_reason "default props of " reason_component in
  let default = ref (mk_object cx default_reason) in
  let reason_state = prefix_reason "state of " reason_component in
  let state = ref (mk_object cx reason_state) in

  let props_reason = prefix_reason "props of " reason_component in
  let props = ref (mk_object cx props_reason) in

  let (fmap, mmap) =
    List.fold_left Ast.Expression.Object.(fun (fmap, mmap) -> function

      (* mixins *)
      | Property (loc, { Property.kind = Property.Init;
          key =
            Property.Identifier (_, { Ast.Identifier.name = "mixins"; _ });
          value = aloc, Array { Array.elements };
          _ }) ->
        mixins := List.map (array_element cx type_params_map aloc) elements;
        fmap, mmap

      (* statics *)
      | Property (loc, { Property.kind = Property.Init;
            key = Property.Identifier (nloc, {
            Ast.Identifier.name = "statics"; _ });
          value = _, Object { Object.properties };
          _ }) ->
        let reason = mk_reason "statics" nloc in
        static := object_ cx type_params_map reason ~allow_sealed:false properties;
        fmap, mmap

      (* propTypes *)
      | Property (loc, { Property.kind = Property.Init;
          key = Property.Identifier (nloc, {
            Ast.Identifier.name = "propTypes"; _ });
          value = _, Object { Object.properties } as value;
          _ }) ->
        ignore (expression cx type_params_map value);
        let reason = mk_reason "propTypes" nloc in
        let amap, omap, dict = mk_proptypes cx type_params_map properties in
        let map = SMap.fold (fun k v map ->
          SMap.add k (OptionalT v) map
        ) omap amap in
        props :=
          Flow_js.mk_object_with_map_proto cx reason ?dict map (MixedT reason);
        fmap, mmap

      (* getDefaultProps *)
      | Property (loc, { Property.kind = Property.Init;
          key = Property.Identifier (_, {
            Ast.Identifier.name = "getDefaultProps"; _ });
          value = (vloc, Ast.Expression.Function func);
          _ }) ->
        Ast.Expression.Function.(
          let { params; defaults; rest; body;
            returnType; typeParameters; _ } = func
          in
          let reason = mk_reason "defaultProps" vloc in
          let t = mk_method cx type_params_map reason (params, defaults, rest)
            returnType body this (MixedT reason)
          in
          let ret_loc = match returnType with
            | Some (_, (loc, _)) -> loc
            | None -> Ast.Statement.FunctionDeclaration.(match body with
              | BodyBlock (loc, _) -> loc
              | BodyExpression (loc, _) -> loc
            ) in
          let ret_reason = repos_reason ret_loc reason in
          let default_tvar = Flow_js.mk_tvar cx (derivable_reason ret_reason) in
          let override_default = BecomeT(ret_reason, default_tvar) in
          default := default_tvar;
          Flow_js.flow cx (t,
            CallT (reason,
              Flow_js.mk_functiontype [] override_default));
          fmap, mmap
        )

      (* getInitialState *)
      | Property (loc, { Property.kind = Property.Init;
          key = Property.Identifier (_, {
            Ast.Identifier.name = "getInitialState"; _ });
          value = (vloc, Ast.Expression.Function func);
          _ }) ->
        Ast.Expression.Function.(
          let { params; defaults; rest; body;
            returnType; typeParameters; _ } = func
          in
          let reason = mk_reason "initial state of React component" vloc in
          let t = mk_method cx type_params_map reason (params, defaults, rest)
            returnType body this (MixedT reason)
          in
          (* since the call to getInitialState happens internally, we need to
             fake a location to pretend the call happened. using the position
             of the return type makes it act like an IIFE. *)
          let ret_loc = match returnType with
            | Some (_, (loc, _)) -> loc
            | None -> Ast.Statement.FunctionDeclaration.(match body with
              | BodyBlock (loc, _) -> loc
              | BodyExpression (loc, _) -> loc
            ) in
          let ret_reason = repos_reason ret_loc reason in
          let state_tvar = Flow_js.mk_tvar cx (derivable_reason ret_reason) in
          let override_state = BecomeT (ret_reason, state_tvar) in
          state := state_tvar;
          Flow_js.flow cx (t,
            CallT (ret_reason,
              Flow_js.mk_functiontype [] override_state));
          fmap, mmap
        )

      (* name = function expr *)
      | Property (loc, { Property.kind = Property.Init;
          key = Property.Identifier (_, {
            Ast.Identifier.name; _ });
          value = (vloc, Ast.Expression.Function func);
          _ }) ->
        Ast.Expression.Function.(
          let { params; defaults; rest; body;
            returnType; typeParameters; async; generator; _ } = func
          in
          let kind = function_kind ~async ~generator in
          let reason = mk_reason "function" vloc in
          let t = mk_method cx type_params_map reason ~kind (params, defaults, rest)
            returnType body this (MixedT reason)
          in
          fmap, SMap.add name t mmap
        )

      (* name = non-function expr *)
      | Property (loc, { Property.kind = Property.Init;
          key =
            Property.Identifier (_, { Ast.Identifier.name; _ }) |
            Property.Literal (_, {
              Ast.Literal.value = Ast.Literal.String name; _;
            });
          value = v;
          _ }) ->
        let t = expression cx type_params_map v in
        SMap.add name t fmap, mmap

      | _ ->
        let msg = "unsupported property specification in createClass" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        fmap, mmap

    ) (SMap.empty, SMap.empty) class_props in

  let type_args = [!default; !props; !state] in
  let super_reason = prefix_reason "super of " reason_component in
  let super =
    Flow_js.get_builtin_typeapp cx super_reason
      "ReactComponent" type_args
  in

  let extract_map (from_map,to_map) name =
    match SMap.get name from_map with
    | Some t -> SMap.remove name from_map, SMap.add name t to_map
    | None -> from_map, to_map
  in
  let fmap = SMap.add "state" !state fmap in
  let fmap, smap =
    List.fold_left extract_map (fmap, SMap.empty)
      ["contextTypes";"childContextTypes";"displayName"]
  in
  let override_statics =
    Flow_js.mk_object_with_map_proto cx
      static_reason smap (MixedT static_reason)
  in
  let super_static = Flow_js.mk_tvar_where cx static_reason (fun t ->
    Flow_js.flow cx (super, GetPropT(static_reason, (static_reason, "statics"), t));
  ) in
  Flow_js.flow cx (super_static, override_statics);
  static := clone_object cx static_reason !static super_static;

  let id = Flow_js.mk_nominal cx in
  let itype = {
    class_id = id;
    type_args = SMap.empty;
    arg_polarities = SMap.empty;
    fields_tmap = Flow_js.mk_propmap cx fmap;
    methods_tmap = Flow_js.mk_propmap cx mmap;
    mixins = !mixins <> [];
    structural = false;
  } in
  Flow_js.flow cx (super, SuperT (super_reason, itype));

  (* TODO: Mixins are handled quite superficially. *)
  (* mixins' statics are copied into static to inherit static properties *)
  (* mixins must be consistent with instance properties *)
  !mixins |> List.iter (fun mixin ->
    static := clone_object cx static_reason !static mixin;
    Flow_js.flow cx (mixin, SuperT (super_reason, itype))
  );

  let instance = InstanceT (reason_component,!static,super,itype) in
  Flow_js.flow cx (instance, this);

  ClassT(instance)
)

(* given an expression found in a test position, notices certain
   type refinements which follow from the test's success or failure,
   and returns a quad:
   - result type of the test (not always bool)
   - map (lookup key -> type) of refinements which hold if
   the test is true
   - map of refinements which hold if the test is false
   - map of unrefined types for lvalues found in refinement maps
 *)
and predicates_of_condition cx type_params_map e = Ast.(Expression.(

  (* refinement key if expr is eligible, along with unrefined type *)
  let refinable_lvalue e =
    Refinement.key e, condition cx type_params_map e
  in

  (* package result quad from test type, refi key, unrefined type,
     predicate, and predicate's truth sense *)
  let result test_t key unrefined_t pred sense =
    let p, notp = if sense
      then pred, NotP pred
      else NotP pred, pred
    in
    (test_t,
      Scope.KeyMap.singleton key p,
      Scope.KeyMap.singleton key notp,
      Scope.KeyMap.singleton key unrefined_t)
  in

  (* package empty result (no refinements derived) from test type *)
  let empty_result test_t =
    Scope.(test_t, KeyMap.empty, KeyMap.empty, KeyMap.empty)
  in

  (* inspect a null equality test *)
  let null_test loc op e =
    let refinement = match refinable_lvalue e with
    | None, t -> None
    | Some name, t ->
        match op with
        | Binary.Equal | Binary.NotEqual ->
            Some (name, t, MaybeP, op = Binary.Equal)
        | Binary.StrictEqual | Binary.StrictNotEqual ->
            Some (name, t, NullP, op = Binary.StrictEqual)
        | _ -> None
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  (* inspect an undefined equality test *)
  let undef_test loc op e =
    let refinement = match refinable_lvalue e with
    | None, t -> None
    | Some name, t ->
        match op with
        | Binary.Equal | Binary.NotEqual ->
            Some (name, t, MaybeP, op = Binary.Equal)
        | Binary.StrictEqual | Binary.StrictNotEqual ->
            Some (name, t, VoidP, op = Binary.StrictEqual)
        | _ -> None
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  let bool_test loc op e right =
    let refinement = match refinable_lvalue e with
    | None, t -> None
    | Some name, t ->
        match op with
        (* TODO support == *)
        | Binary.StrictEqual | Binary.StrictNotEqual ->
            let pred = if right then TrueP else FalseP in
            Some (name, t, pred, op = Binary.StrictEqual)
        | _ -> None
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  (* inspect a sentinel property test *)
  let sentinel_prop_test loc op e key value =
    let val_t = expression cx type_params_map value in
    let refinement = match refinable_lvalue e with
    | None, t -> None
    | Some name, t ->
        match op with
        | Binary.StrictEqual | Binary.StrictNotEqual ->
            let pred = LeftP (SentinelProp key, val_t) in
            Some (name, t, pred, op = Binary.StrictEqual)
        | _ -> None
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  (* inspect a typeof equality test *)
  let typeof_test loc sense arg typename str_loc =
    match refinable_lvalue arg with
    | Some name, t ->
        let pred = match typename with
        | "boolean" -> Some BoolP
        | "function" -> Some FunP
        | "number" -> Some NumP
        | "object" -> Some ObjP
        | "string" -> Some StrP
        | "undefined" -> Some VoidP
        | _ -> None
        in
        begin match pred with
        | Some pred -> result BoolT.t name t pred sense
        | None ->
          let reason = mk_reason (spf "string literal `%s`" typename) str_loc in
          let err = "This value is not a valid `typeof` return value" in
          Flow_js.add_warning cx [reason, err];
          empty_result (BoolT.at loc)
        end
    | None, t -> empty_result (BoolT.at loc)
  in

  let mk_and map1 map2 = Scope.KeyMap.merge
    (fun x -> fun p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some p, None)
      | (None, Some p) -> Some p
      | (Some p1, Some p2) -> Some (AndP(p1,p2))
    )
    map1 map2
  in

  let mk_or map1 map2 = Scope.KeyMap.merge
    (fun x -> fun p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some p, None)
      | (None, Some p) -> None
      | (Some p1, Some p2) -> Some (OrP(p1,p2))
    )
    map1 map2
  in

  (* main *)
  match e with

  (* ids, member expressions *)
  | _, This
  | _, Identifier _
  | _, Member _ -> (
      match refinable_lvalue e with
      | Some name, t -> result t name t ExistsP true
      | None, t -> empty_result t
    )

  (* assignments *)
  | _, Assignment { Assignment.left = loc, Ast.Pattern.Identifier id; _ } -> (
      let expr = expression cx type_params_map e in
      match refinable_lvalue (loc, Ast.Expression.Identifier id) with
      | Some name, _ -> result expr name expr ExistsP true
      | None, _ -> empty_result expr
    )

  (* expr instanceof t *)
  | _, Binary { Binary.operator = Binary.Instanceof; left; right } -> (
      match refinable_lvalue left with
      | Some name, t ->
          let pred = LeftP (Instanceof, expression cx type_params_map right) in
          result BoolT.t name t pred true
      | None, t ->
          empty_result BoolT.t
    )

  (* expr op null *)
  | loc, Binary { Binary.
      operator = (Binary.Equal | Binary.StrictEqual |
                  Binary.NotEqual | Binary.StrictNotEqual) as op;
      left;
      right = _, Literal { Literal.value = Literal.Null; _ }
    } ->
      null_test loc op left

  (* null op expr *)
  | loc, Binary { Binary.
      operator = (Binary.Equal | Binary.StrictEqual |
                  Binary.NotEqual | Binary.StrictNotEqual) as op;
      left = _, Literal { Literal.value = Literal.Null; _ };
      right
    } ->
      null_test loc op right

  (* expr op undefined *)
  | loc, Binary { Binary.
      operator = (Binary.Equal | Binary.StrictEqual |
                  Binary.NotEqual | Binary.StrictNotEqual) as op;
      left;
      right = _, Identifier (_, { Identifier.name = "undefined"; _ })
    } ->
      undef_test loc op left

  (* undefined op expr *)
  | loc, Binary { Binary.
      operator = (Binary.Equal | Binary.StrictEqual |
                  Binary.NotEqual | Binary.StrictNotEqual) as op;
      left = _, Identifier (_, { Identifier.name = "undefined"; _ });
      right
    } ->
      undef_test loc op right

  (* expr op void(...) *)
  | loc, Binary { Binary.
      operator = (Binary.Equal | Binary.StrictEqual |
                  Binary.NotEqual | Binary.StrictNotEqual) as op;
      left;
      right = _, Unary ({ Unary.operator = Unary.Void; _ }) as void_arg
    } ->
      ignore (expression cx type_params_map void_arg);
      undef_test loc op left

  (* void(...) op expr *)
  | loc, Binary { Binary.
      operator = (Binary.Equal | Binary.StrictEqual |
                  Binary.NotEqual | Binary.StrictNotEqual) as op;
      left = _, Unary ({ Unary.operator = Unary.Void; _ }) as void_arg;
      right
    } ->
      ignore (expression cx type_params_map void_arg);
      undef_test loc op right

  (* expr op true; expr op false *)
  | loc, Binary { Binary.
      operator = (Binary.Equal | Binary.StrictEqual |
                  Binary.NotEqual | Binary.StrictNotEqual) as op;
      left;
      right = _, Literal { Literal.value = Literal.Boolean value; _ }
    } ->
      bool_test loc op left value

  (* true op expr; false op expr *)
  | loc, Binary { Binary.
      operator = (Binary.Equal | Binary.StrictEqual |
                  Binary.NotEqual | Binary.StrictNotEqual) as op;
      left = _, Literal { Literal.value = Literal.Boolean value; _ };
      right
    } ->
      bool_test loc op right value

  (* typeof expr ==/=== string *)
  | loc, Binary { Binary.operator = Binary.Equal | Binary.StrictEqual;
      left = _, Unary { Unary.operator = Unary.Typeof; argument; _ };
      right = str_loc, Literal { Literal.value = Literal.String s; _ }
    } ->
      typeof_test loc true argument s str_loc

  (* typeof expr !=/!== string *)
  | loc, Binary { Binary.operator = Binary.NotEqual | Binary.StrictNotEqual;
      left = _, Unary { Unary.operator = Unary.Typeof; argument; _ };
      right = str_loc, Literal { Literal.value = Literal.String s; _ }
    } ->
      typeof_test loc false argument s str_loc

  (* string ==/=== typeof expr *)
  | loc, Binary { Binary.operator = Binary.Equal | Binary.StrictEqual;
      left = str_loc, Literal { Literal.value = Literal.String s; _ };
      right = _, Unary { Unary.operator = Unary.Typeof; argument; _ }
    } ->
      typeof_test loc true argument s str_loc

  (* string !=/!== typeof expr *)
  | loc, Binary { Binary.operator = Binary.NotEqual | Binary.StrictNotEqual;
      left = str_loc, Literal { Literal.value = Literal.String s; _ };
      right = _, Unary { Unary.operator = Unary.Typeof; argument; _ }
    } ->
      typeof_test loc false argument s str_loc

  (* expr.name ===/!== value *)
  | loc, Binary { Binary.
      operator = (Binary.StrictEqual | Binary.StrictNotEqual) as op;
      left = _, Member {
        Member._object;
        property = Member.PropertyIdentifier (_,
          { Identifier.name; _ });
        _ };
      right;
    } ->
      sentinel_prop_test loc op _object name right

  (* value ===/!== expr.name *)
  | loc, Binary { Binary.
      operator = (Binary.StrictEqual | Binary.StrictNotEqual) as op;
      left;
      right = _, Member {
        Member._object;
        property = Member.PropertyIdentifier (_,
          { Identifier.name; _ });
        _ }
    } ->
      sentinel_prop_test loc op _object name left

  (* Array.isArray(expr) *)
  | _, Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Identifier.name = "Array"; _ });
        property = Member.PropertyIdentifier (_,
          { Identifier.name = "isArray"; _ });
        _ };
      arguments = [Expression arg]
    } -> (
      match refinable_lvalue arg with
      | Some name, t ->
          result BoolT.t name t ArrP true
      | None, t ->
          empty_result BoolT.t
    )

  (* test1 && test2 *)
  | loc, Logical { Logical.operator = Logical.And; left; right } ->
      let reason = mk_reason "&&" loc in
      let t1, map1, not_map1, xts1 = predicates_of_condition cx type_params_map left in
      let t2, map2, not_map2, xts2 = Env_js.in_refined_env cx reason map1 xts1
        (fun () -> predicates_of_condition cx type_params_map right)
      in
      (
        Flow_js.mk_tvar_where cx reason (fun t ->
          Flow_js.flow cx (t1, AndT (reason, t2, t));
        ),
        mk_and map1 map2,
        mk_or not_map1 not_map2,
        Scope.KeyMap.union xts1 xts2
      )

  (* test1 || test2 *)
  | loc, Logical { Logical.operator = Logical.Or; left; right } ->
      let reason = mk_reason "||" loc in
      let t1, map1, not_map1, xts1 = predicates_of_condition cx type_params_map left in
      let t2, map2, not_map2, xts2 = Env_js.in_refined_env cx reason not_map1 xts1
        (fun () -> predicates_of_condition cx type_params_map right)
      in
      (
        Flow_js.mk_tvar_where cx reason (fun t ->
          Flow_js.flow cx (t1, OrT (reason, t2, t));
        ),
        mk_or map1 map2,
        mk_and not_map1 not_map2,
        Scope.KeyMap.union xts1 xts2
      )

  (* !test *)
  | loc, Unary { Unary.operator = Unary.Not; argument; _ } ->
      let (t, map, not_map, xts) = predicates_of_condition cx type_params_map argument in
      (BoolT.at loc, not_map, map, xts)

  (* fallthrough case: evaluate test expr, no refinements *)
  | e ->
      empty_result (expression cx type_params_map e)
))

(* Conditional expressions are checked like expressions, except that property
   accesses are allowed even when such properties do not exist. This
   accommodates the common JavaScript idiom of testing for the existence of a
   property before using that property. *)
and condition cx type_params_map e =
  expression ~is_cond:true cx type_params_map e

(* Property lookups become non-strict when processing conditional expressions
   (see above).

   TODO: It should be possible to factor the processing of LHS / reference
   expressions out of `expression`, somewhat like what assignment_lhs does. That
   would make everything involving Refinement be in the same place.
*)
and get_prop ~is_cond cx reason tobj (prop_reason, name) =
  Flow_js.mk_tvar_where cx reason (fun t ->
    let get_prop_u =
      if is_cond
      then LookupT (reason, None, [], name, LowerBoundT t)
      else GetPropT (reason, (prop_reason, name), t)
    in
    Flow_js.flow cx (tobj, get_prop_u)
  )

(* TODO: switch to TypeScript specification of Object *)
and static_method_call_Object cx type_params_map loc prop_loc m args_ = Ast.Expression.(
  let reason = mk_reason (spf "Object.%s" m) loc in
  match (m, args_) with
  | ("create", [ Expression e ]) ->
    let proto = expression cx type_params_map e in
    Flow_js.mk_object_with_proto cx reason proto

  | ("create", [ Expression e;
                 Expression (_, Object { Object.properties }) ]) ->
    let proto = expression cx type_params_map e in
    let pmap = prop_map_of_object cx type_params_map properties in
    let map = pmap |> SMap.mapi (fun x spec ->
      let reason = prefix_reason (spf ".%s of " x) reason in
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        Flow_js.flow cx (spec, GetPropT(reason, (reason, "value"), tvar));
      )
    ) in
    Flow_js.mk_object_with_map_proto cx reason map proto

  | ("getPrototypeOf", [ Expression e ]) ->
    let o = expression cx type_params_map e in
    Flow_js.mk_tvar_where cx reason (fun tvar ->
      Flow_js.flow cx (o, GetPropT(reason, (reason, "__proto__"), tvar));
    )

  | (("getOwnPropertyNames" | "keys"), [ Expression e ]) ->
    let arr_reason = mk_reason "array type" loc in
    let o = expression cx type_params_map e in
    ArrT (arr_reason,
      Flow_js.mk_tvar_where cx arr_reason (fun tvar ->
        let keys_reason = prefix_reason "element of " reason in
        Flow_js.flow cx (o, GetKeysT(keys_reason, tvar));
      ),
      []
    )

  | ("defineProperty", [ Expression e;
                         Expression (ploc, Literal
                           { Ast.Literal.value = Ast.Literal.String x; _ });
                         Expression config ]) ->
    let o = expression cx type_params_map e in
    let spec = expression cx type_params_map config in
    let tvar = Flow_js.mk_tvar cx reason in
    let prop_reason = mk_reason (spf "property `%s`" x) ploc in
    Flow_js.flow cx (spec, GetPropT(reason, (reason, "value"), tvar));
    Flow_js.flow cx (o, SetPropT (reason, (prop_reason, x), tvar));
    o

  | ("defineProperties", [ Expression e;
                         Expression (_, Object { Object.properties }) ]) ->
    let o = expression cx type_params_map e in
    let pmap = prop_map_of_object cx type_params_map properties in
    pmap |> SMap.iter (fun x spec ->
      let reason = prefix_reason (spf ".%s of " x) reason in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.flow cx (spec, GetPropT(reason, (reason, "value"), tvar));
      Flow_js.flow cx (o, SetPropT (reason, (reason, x), tvar));
    );
    o

  | ("assign", (Expression e)::others) ->
    let this = expression cx type_params_map e in
    let those = List.map (expression_or_spread cx type_params_map) others in
    chain_objects cx reason this those

  (* Freezing an object literal is supported since there's no way it could
     have been mutated elsewhere *)
  | ("freeze", [Expression ((_, Object _) as e)]) ->
    let t = Flow_js.mk_tvar_where cx reason (fun tvar ->
      Flow_js.flow cx (expression cx type_params_map e, ObjFreezeT (reason, tvar));
    ) in
    let reason_prop = mk_reason "property `freeze`" prop_loc in
    Flow_js.static_method_call cx "Object" reason reason_prop m [t]

  (* TODO *)
  | (("seal" | "preventExtensions"), args)
  | ("freeze", args)

  | (_, args) ->
      let reason_prop = mk_reason (spf "property `%s`" m) prop_loc in
      let argts = List.map (expression_or_spread cx type_params_map) args in
      Flow_js.static_method_call cx "Object" reason reason_prop m argts
)

and mk_extends cx type_params_map = function
  | (None, None) ->
      let root = MixedT (reason_of_string "Object") in
      root
  | (None, _) ->
      assert false (* type args with no head expr *)
  | (Some c, targs) ->
      let params = match targs with
      | None -> None
      | Some (_, { Ast.Type.ParameterInstantiation.params; }) -> Some params in
      mk_nominal_type ~for_type:false cx (reason_of_t c) type_params_map (c, params)

and mk_interface_super cx structural reason_c map = function
  | (Some id, targs) ->
      let desc = if structural then "extends" else "mixins" in
      let lookup_mode = if structural then ForType else ForValue in
      let i = convert_qualification ~lookup_mode cx desc id in
      let reason = reason_of_t i in
      let params = match targs with
      | None -> None
      | Some (_, { Ast.Type.ParameterInstantiation.params; }) -> Some params in
      mk_nominal_type cx reason map (i, params)
  | _ ->
      let root = MixedT (reason_of_string "Object") in
      root

and body_loc = Ast.Statement.FunctionDeclaration.(function
  | BodyBlock (loc, _) -> loc
  | BodyExpression (loc, _) -> loc
)

(* Makes signatures for fields and methods in a class. *)
and mk_class_signature cx reason_c type_params_map superClass body = Ast.Class.(
  let _, { Body.body = elements } = body in

  (* In case there is no constructor, pick up a default one. *)
  let default_methods = match superClass with
    | None ->
        (* Parent class constructors simply return new instances, which is
           indicated by the VoidT return type *)
        SMap.singleton "constructor" {
          meth_reason = replace_reason "default constructor" reason_c;
          meth_tparams = [];
          meth_tparams_map = SMap.empty;
          meth_params = FuncParams.empty;
          meth_return_type = VoidT.t;
        }
    | Some _ ->
        (* Subclass default constructors are technically of the form
           (...args) => { super(...args) }, but we can approximate that using
           flow's existing inheritance machinery. *)
        (* TODO: Does this distinction matter for the type checker? *)
        SMap.empty
  in

  let default_sfields =
    let reason = prefix_reason "`name` property of" reason_c in
    SMap.singleton "name" (StrT.why reason)
  in

  (* NOTE: We used to mine field declarations from field assignments in a
     constructor as a convenience, but it was not worth it: often, all that did
     was exchange a complaint about a missing field for a complaint about a
     missing annotation. Moreover, it caused fields declared in the super class
     to be redeclared if they were assigned in the constructor. So we don't do
     it. In the future, we could do it again, but only for private fields. *)

  let static_sig = {
    sig_fields = default_sfields;
    sig_methods = SMap.empty;
    sig_getters = SMap.empty;
    sig_setters = SMap.empty;
  } in

  let inst_sig = {
    sig_fields = SMap.empty;
    sig_methods = default_methods;
    sig_getters = SMap.empty;
    sig_setters = SMap.empty;
  } in

  List.fold_left (fun (static_sig, inst_sig) -> function
    (* instance and static methods *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_,
          { Ast.Identifier.name; _ });
        value = _, { Ast.Expression.Function.params; defaults; rest;
          returnType; typeParameters; body; _ };
        kind;
        static;
        decorators;
      }) ->

      warn_or_ignore_decorators cx decorators;

      (match kind with
      | Method.Get | Method.Set when not (are_getters_and_setters_enabled ()) ->
        let msg = "get/set properties not yet supported" in
        Flow_js.add_error cx [mk_reason "" loc, msg]
      | _ -> ());

      let typeparams, type_params_map =
        mk_type_param_declarations cx type_params_map typeParameters in

      let meth_params, meth_return_type = mk_params_ret cx type_params_map
        (params, defaults, rest) (body, returnType) in
      let reason_desc = (match kind with
      | Method.Method -> spf "method `%s`" name
      | Method.Constructor -> "constructor"
      | Method.Get -> spf "getter for `%s`" name
      | Method.Set -> spf "setter for `%s`" name) in
      let meth_reason = mk_reason reason_desc loc in
      let method_sig = {
        meth_reason;
        meth_tparams = typeparams;
        meth_tparams_map = type_params_map;
        meth_params;
        meth_return_type;
      } in

      (match kind, static with
      | (Method.Constructor | Method.Method), true ->
        {
          static_sig with
          sig_methods = SMap.add name method_sig static_sig.sig_methods;
          sig_getters = SMap.remove name static_sig.sig_getters;
          sig_setters = SMap.remove name static_sig.sig_setters;
        },
        inst_sig
      | Method.Get, true ->
        {
          static_sig with
          sig_methods = SMap.remove name static_sig.sig_methods;
          sig_getters = SMap.add name method_sig static_sig.sig_getters;
        },
        inst_sig
      | Method.Set, true ->
        {
          static_sig with
          sig_methods = SMap.remove name static_sig.sig_methods;
          sig_setters = SMap.add name method_sig static_sig.sig_setters;
        },
        inst_sig
      | (Method.Constructor | Method.Method), false ->
        static_sig,
        {
          inst_sig with
          sig_methods = SMap.add name method_sig inst_sig.sig_methods;
          sig_getters = SMap.remove name inst_sig.sig_getters;
          sig_setters = SMap.remove name inst_sig.sig_setters;
        }
      | Method.Get, false ->
        static_sig,
        {
          inst_sig with
          sig_methods = SMap.remove name inst_sig.sig_methods;
          sig_getters = SMap.add name method_sig inst_sig.sig_getters;
        }
      | Method.Set, false ->
        static_sig,
        {
          inst_sig with
          sig_methods = SMap.remove name inst_sig.sig_methods;
          sig_setters = SMap.add name method_sig inst_sig.sig_setters;
        }
      )

    (* fields *)
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Identifier
          (_, { Ast.Identifier.name; _ });
        typeAnnotation;
        value;
        static;
      }) ->
        (match value with
          | None -> ()
          | Some value -> FlowConfig.(Opts.(
            let opts = (FlowConfig.get_unsafe ()).options in
            let (config_setting, reason_subject) =
              if static then
                (opts.esproposal_class_static_fields, "class static field")
              else
                (opts.esproposal_class_instance_fields, "class instance field")
            in
            match config_setting with
            | ESPROPOSAL_ENABLE
            | ESPROPOSAL_IGNORE -> ()
            | ESPROPOSAL_WARN ->
                let reason =
                  mk_reason (spf "Experimental %s usage" reason_subject) loc
                in
                let msg = (spf
                  ("%ss are an active early stage feature proposal that may " ^^
                  "change. Additionally, Flow does not yet typecheck the " ^^
                  "right-hand side of field initializers. Support for this " ^^
                  "is being tracked in " ^^
                  "https://github.com/facebook/flow/issues/850")
                  (String.capitalize reason_subject)
                ) in
                Flow_js.add_warning cx [reason, msg]
          ))
        );
        let r = mk_reason (spf "class property `%s`" name) loc in
        let t = mk_type_annotation cx type_params_map r typeAnnotation in
        if static then (
          {
            static_sig with
            sig_fields = SMap.add name t static_sig.sig_fields;
            sig_getters = SMap.remove name static_sig.sig_getters;
            sig_setters = SMap.remove name static_sig.sig_setters;
          },
          inst_sig
        ) else (
          {
            static_sig with
            sig_getters = inst_sig.sig_getters;
            sig_setters = inst_sig.sig_setters;
          },
          {
            inst_sig with
            sig_fields = SMap.add name t inst_sig.sig_fields;
            sig_getters = SMap.remove name inst_sig.sig_getters;
            sig_setters = SMap.remove name inst_sig.sig_setters;
          }
        )

    (* literal LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Literal _;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Literal _;
        _
      }) ->
        let msg = "literal properties not yet supported" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        (static_sig, inst_sig)

    (* computed LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Computed _;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Computed _;
        _
      }) ->
        let msg = "computed property keys not supported" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        (static_sig, inst_sig)
  ) (static_sig, inst_sig) elements
)

(* Processes the bodies of instance and static class members (methods/fields). *)
and mk_class_elements cx instance_info static_info tparams body = Ast.Class.(
  let _, { Body.body = elements } = body in
  let (opt_static_fields, opt_inst_fields) = FlowConfig.(Opts.(
    let opts = (FlowConfig.get_unsafe ()).options in
    (opts.esproposal_class_static_fields, opts.esproposal_class_instance_fields)
  )) in
  List.iter (function
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_,
          { Ast.Identifier.name; _ });
        value = _, { Ast.Expression.Function.params; defaults; rest;
          returnType; typeParameters; body; async; generator; _ };
        static;
        kind;
        decorators;
      }) ->

      warn_or_ignore_decorators cx decorators;

      let this, super, class_sig =
        if static then static_info else instance_info
      in

      let sigs_to_use = match kind with
      | Method.Constructor
      | Method.Method -> class_sig.sig_methods
      | Method.Get -> class_sig.sig_getters
      | Method.Set -> class_sig.sig_setters
      in

      let {
        meth_reason;
        meth_tparams;
        meth_tparams_map;
        meth_return_type;
        meth_params;
      } = SMap.find_unsafe name sigs_to_use in

      let save_return_exn = Abnormal.(swap Return false) in
      let save_throw_exn = Abnormal.(swap Throw false) in
      Flow_js.generate_tests cx meth_reason meth_tparams (fun map_ ->
        let tparams_map =
          meth_tparams_map |> SMap.map (Flow_js.subst cx map_) in
        let meth_params = FuncParams.subst cx map_ meth_params in
        let return_type = Flow_js.subst cx map_ meth_return_type in
        (* determine if we are in a derived constructor *)
        let derived_ctor = match super with
          | ClassT (MixedT _) -> false
          | MixedT _ -> false
          | _ -> name = "constructor"
        in
        let function_kind = function_kind ~async ~generator in
        let yield, next = if generator then (
          Flow_js.mk_tvar cx (prefix_reason "yield of " meth_reason),
          Flow_js.mk_tvar cx (prefix_reason "next of " meth_reason)
        ) else (
          MixedT (replace_reason "no yield" meth_reason),
          MixedT (replace_reason "no next" meth_reason)
        ) in
        mk_body None cx tparams_map ~kind:function_kind ~derived_ctor
          meth_params return_type body this super yield next;
      );
      ignore Abnormal.(swap Return save_return_exn);
      ignore Abnormal.(swap Throw save_throw_exn)

    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Identifier (_,
          { Ast.Identifier.name; _ });
        value;
        static;
        _;
      }) -> (
        match value with
        | None -> ()
        | Some value ->
          let this, super, class_sig =
            if static then static_info else instance_info
          in

          let config_opt =
            if static then opt_static_fields else opt_inst_fields
          in

          if config_opt <> FlowConfig.Opts.ESPROPOSAL_IGNORE then (
            (* determine if we are in a derived class *)
            let derived_ctor = match super with
              | ClassT (MixedT _) -> false
              | MixedT _ -> false
              | _ -> name = "constructor"
            in

            (* Infer the initializer in a function-like scope *)
            let orig_ctx = Env_js.peek_env () in
            let new_ctx = Env_js.clone_env orig_ctx in
            let reason = mk_reason (spf "field initializer for `%s`" name) loc in
            Env_js.update_env cx reason new_ctx;
            Env_js.havoc_all ();

            let initializer_scope = Scope.fresh () in
            initialize_this_super derived_ctor this super initializer_scope;
            Env_js.push_var_scope cx initializer_scope;

            let init_t = expression cx tparams value in
            let field_t = SMap.find_unsafe name class_sig.sig_fields in
            Flow_js.flow cx (init_t, field_t);

            Env_js.pop_var_scope ();
            Env_js.update_env cx reason orig_ctx
          );
      )

    | _ -> ()
  ) elements
)

and mk_methodtype method_sig =
  let ft = FunT (
    method_sig.meth_reason,
    Flow_js.dummy_static method_sig.meth_reason,
    Flow_js.dummy_prototype,
    Flow_js.mk_functiontype
      (FuncParams.tlist method_sig.meth_params)
      ?params_names:(Some (FuncParams.names method_sig.meth_params))
      method_sig.meth_return_type
  ) in
  if (method_sig.meth_tparams = [])
  then ft
  else PolyT (method_sig.meth_tparams, ft)

and extract_setter_type = function
  | FunT (_, _, _, { params_tlist = [param_t]; _; }) -> param_t
  | _ ->  failwith "Setter property with unexpected type"

and extract_getter_type = function
  | FunT (_, _, _, { return_t; _; }) -> return_t
  | _ -> failwith "Getter property with unexpected type"

and extract_class_name class_loc  = Ast.Class.(function {id; _;} ->
  match id with
  | Some(name_loc, {Ast.Identifier.name; _;}) -> (name_loc, name)
  | None -> (class_loc, "<<anonymous class>>")
)

(* Process a class definition, returning a (polymorphic) class type. A class
   type is a wrapper around an instance type, which contains types of instance
   members, a pointer to the super instance type, and a container for types of
   static members. The static members can be thought of as instance members of a
   "metaclass": thus, the static type is itself implemented as an instance
   type. *)
and mk_class = Ast.Class.(
  let merge_getters_and_setters cx getters setters =
    SMap.fold
      (fun name setter_type getters_and_setters ->
        match SMap.get name getters_and_setters with
        | Some prop_t ->
          Flow_js.unify cx setter_type prop_t;
          getters_and_setters
        | None ->
          SMap.add name setter_type getters_and_setters
      )
      (SMap.map extract_setter_type setters)
      (SMap.map extract_getter_type getters)

  in fun cx type_params_map loc reason_c {
    id=_;
    body;
    superClass;
    typeParameters;
    superTypeParameters;
    implements;
  } ->

  (* As a running example, let's say the class declaration is:
     class C<X> extends D<X> { f: X; m<Y: X>(x: Y): X { ... } }
  *)

  (* TODO *)
  if implements <> [] then
    let msg = "implements not supported" in
    Flow_js.add_error cx [mk_reason "" loc, msg]
  else ();

  (* type parameters: <X> *)
  let typeparams, type_params_map =
    mk_type_param_declarations cx type_params_map typeParameters in

  let arg_polarities = List.fold_left (fun acc tp ->
    SMap.add tp.name tp.polarity acc
  ) SMap.empty typeparams in

  (* fields: { f: X }, methods_: { m<Y: X>(x: Y): X } *)
  let (static_sig, inst_sig) =
    mk_class_signature cx reason_c type_params_map superClass body
  in

  let id = Flow_js.mk_nominal cx in

  (* super: D<X> *)
  let extends = opt_map (expression cx type_params_map) superClass, superTypeParameters in
  let super = mk_extends cx type_params_map extends in
  let super_static = ClassT (super) in

  let static_reason = prefix_reason "statics of " reason_c in

  Flow_js.generate_tests cx reason_c typeparams (fun map_ ->
    (* map_: [X := T] where T = mixed, _|_ *)

    (* super: D<T> *)
    let super = Flow_js.subst cx map_ super in
    let super_static = Flow_js.subst cx map_ super_static in

    let subst_method_sig cx map_ method_sig = {
      method_sig with

      (* typeparams = <Y: X> *)
      meth_tparams = List.map (fun typeparam ->
        { typeparam with bound = Flow_js.subst cx map_ typeparam.bound }
      ) method_sig.meth_tparams;

      meth_tparams_map =
        SMap.map (Flow_js.subst cx map_) method_sig.meth_tparams_map;

      (* params = (x: Y), ret = T *)
      meth_params = FuncParams.subst cx map_ method_sig.meth_params;
      meth_return_type = Flow_js.subst cx map_ method_sig.meth_return_type;
    } in

    (* Substitue type vars on fields and methods *)
    let inst_sig_substituted = {
      sig_fields = SMap.map (Flow_js.subst cx map_) inst_sig.sig_fields;
      sig_methods = SMap.map (subst_method_sig cx map_) inst_sig.sig_methods;
      sig_getters = SMap.map (subst_method_sig cx map_) inst_sig.sig_getters;
      sig_setters = SMap.map (subst_method_sig cx map_) inst_sig.sig_setters;
    } in
    let static_sig_substituted = {
      sig_fields = SMap.map (Flow_js.subst cx map_) static_sig.sig_fields;
      sig_methods = SMap.map (subst_method_sig cx map_) static_sig.sig_methods;
      sig_getters = SMap.map (subst_method_sig cx map_) static_sig.sig_getters;
      sig_setters = SMap.map (subst_method_sig cx map_) static_sig.sig_setters;
    } in

    (* Generate types for each element *)
    let methods = SMap.map mk_methodtype inst_sig_substituted.sig_methods in
    let getters = SMap.map mk_methodtype inst_sig_substituted.sig_getters in
    let setters = SMap.map mk_methodtype inst_sig_substituted.sig_setters in
    let smethods = SMap.map mk_methodtype static_sig_substituted.sig_methods in
    let sgetters = SMap.map mk_methodtype static_sig_substituted.sig_getters in
    let ssetters = SMap.map mk_methodtype static_sig_substituted.sig_setters in

    (* If there is a both a getter and a setter, then flow the setter type to
     * the getter. Otherwise just use the getter type or the setter type *)
    let sgetters_and_setters = merge_getters_and_setters cx sgetters ssetters in
    let getters_and_setters = merge_getters_and_setters cx getters setters in

    (* Treat getters and setters as fields *)
    let sfields =
      SMap.fold SMap.add sgetters_and_setters static_sig_substituted.sig_fields
    in
    let fields =
      SMap.fold SMap.add getters_and_setters inst_sig_substituted.sig_fields
    in

    let static_instance = {
      class_id = 0;
      type_args = type_params_map |> SMap.map (Flow_js.subst cx map_);
      arg_polarities;
      fields_tmap = Flow_js.mk_propmap cx sfields;
      methods_tmap = Flow_js.mk_propmap cx smethods;
      mixins = false;
      structural = false;
    } in
    Flow_js.flow cx (super_static, SuperT(reason_c, static_instance));
    let static = InstanceT (
      static_reason,
      MixedT.t,
      super_static,
      static_instance
    ) in

    let instance = {
      class_id = id;
      type_args = type_params_map |> SMap.map (Flow_js.subst cx map_);
      arg_polarities;
      fields_tmap = Flow_js.mk_propmap cx fields;
      methods_tmap = Flow_js.mk_propmap cx methods;
      mixins = false;
      structural = false;
    } in
    Flow_js.flow cx (super, SuperT(reason_c, instance));
    let this = InstanceT (reason_c,static,super,instance) in

    mk_class_elements cx
      (this, super, inst_sig_substituted)
      (ClassT this, super_static, static_sig_substituted)
      map_
      body;
  );

  let enforce_void_return method_sig =
    let meth_return_type =
      if (is_void cx method_sig.meth_return_type)
      then (VoidT.at (loc_of_t method_sig.meth_return_type))
      else method_sig.meth_return_type
    in
    mk_methodtype {method_sig with meth_return_type;}
  in

  let methods = SMap.map enforce_void_return inst_sig.sig_methods in
  let getters = SMap.map enforce_void_return inst_sig.sig_getters in
  let setters = SMap.map enforce_void_return inst_sig.sig_setters in
  let smethods = SMap.map enforce_void_return static_sig.sig_methods in
  let sgetters = SMap.map enforce_void_return static_sig.sig_getters in
  let ssetters = SMap.map enforce_void_return static_sig.sig_setters in

  (* If there is a both a getter and a setter, then flow the setter type to
    * the getter. Otherwise just use the getter type or the setter type *)
  let sgetters_and_setters = merge_getters_and_setters cx sgetters ssetters in
  let getters_and_setters = merge_getters_and_setters cx getters setters in

  (* Treat getters and setters as fields *)
  let sfields = SMap.fold SMap.add sgetters_and_setters static_sig.sig_fields in
  let fields = SMap.fold SMap.add getters_and_setters inst_sig.sig_fields in

  let static_instance = {
    class_id = 0;
    type_args = type_params_map;
    arg_polarities;
    fields_tmap = Flow_js.mk_propmap cx sfields;
    methods_tmap = Flow_js.mk_propmap cx smethods;
    mixins = false;
    structural = false;
  } in
  let static = InstanceT (
    static_reason,
    MixedT.t,
    super_static,
    static_instance
  ) in

  let instance = {
    class_id = id;
    type_args = type_params_map;
    arg_polarities;
    fields_tmap = Flow_js.mk_propmap cx fields;
    methods_tmap = Flow_js.mk_propmap cx methods;
    mixins = false;
    structural = false;
  } in
  let this = InstanceT (reason_c, static, super, instance) in

  if (typeparams = [])
  then
    ClassT this
  else
    PolyT(typeparams, ClassT this)
)

and extract_extends cx structural = function
  | [] -> [None,None]
  | [loc, {Ast.Type.Generic.id; typeParameters}] ->
      [Some id, typeParameters]
  | (loc, {Ast.Type.Generic.id; typeParameters})::others ->
      if structural
      then (Some id, typeParameters)::(extract_extends cx structural others)
      else
        let msg = "A class cannot extend multiple classes!" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        []

and extract_mixins cx =
  List.map (fun (loc, {Ast.Type.Generic.id; typeParameters}) ->
    (Some id, typeParameters)
  )

(* Processes a declare class or interface. One difference between a declare
   class and an interface is that the the A ~> B check is structural if B is an
   interface and nominal if B is a declare class. If you set the structural flag
   to true, then this interface will be checked structurally. Another difference
   is that a declare class may have mixins, but an interface may not. *)
(* TODO: this function shares a lot of code with mk_class. Sometimes that causes
   bad divergence; e.g., bugs around the handling of generics were fixed in
   mk_class but not in mk_interface. This code should be consolidated soon,
   ideally when we provide full support for interfaces. *)
and mk_interface cx reason_i typeparams type_params_map
    (sfmap, smmap, fmap, mmap) extends mixins structural =
  let id = Flow_js.mk_nominal cx in

  let extends = extract_extends cx structural extends in
  let mixins = extract_mixins cx mixins in
  let super_reason = prefix_reason "super of " reason_i in
  (* mixins override extends *)
  let interface_supers =
    List.map (mk_interface_super cx false super_reason type_params_map) mixins @
    List.map (mk_interface_super cx true super_reason type_params_map) extends
  in
  let super = match interface_supers with
    | [] -> AnyT.t
    | [t] -> t
    | ts -> IntersectionT(super_reason, ts)
  in

  let super_static = ClassT(super) in
  let static_reason = prefix_reason "statics of " reason_i in

  let arg_polarities = type_params_map |> SMap.map (fun t -> match t with
  | BoundT { polarity; _ } -> polarity
  | _ -> assert_false (spf "Expected BoundT but found %s" (string_of_ctor t))
  ) in

  Flow_js.generate_tests cx reason_i typeparams (fun map_ ->
    let super = Flow_js.subst cx map_ super in
    let super_static = Flow_js.subst cx map_ super_static in

    let fmap = fmap |> SMap.map (Flow_js.subst cx map_) in
    let sfmap = sfmap |> SMap.map (Flow_js.subst cx map_) in
    let mmap = mmap |> SMap.map (Flow_js.subst cx map_) in
    let smmap = smmap |> SMap.map (Flow_js.subst cx map_) in

    let static_instance = {
      class_id = 0;
      type_args = type_params_map |> SMap.map (Flow_js.subst cx map_);
      arg_polarities;
      fields_tmap = Flow_js.mk_propmap cx sfmap;
      methods_tmap = Flow_js.mk_propmap cx smmap;
      mixins = false;
      structural;
    } in
    Flow_js.flow cx (super_static, SuperT(reason_i, static_instance));
    let instance = {
      class_id = id;
      type_args = type_params_map |> SMap.map (Flow_js.subst cx map_);
      arg_polarities;
      fields_tmap = Flow_js.mk_propmap cx fmap;
      methods_tmap = Flow_js.mk_propmap cx mmap;
      mixins = false;
      structural;
    } in
    Flow_js.flow cx (super, SuperT(reason_i, instance));
  );

  let static_instance = {
    class_id = 0;
    type_args = type_params_map;
    arg_polarities;
    fields_tmap = Flow_js.mk_propmap cx sfmap;
    methods_tmap = Flow_js.mk_propmap cx smmap;
    mixins = false;
    structural;
  } in
  let static = InstanceT (
    static_reason,
    MixedT.t,
    super_static,
    static_instance
  ) in
  let instance = {
    class_id = id;
    type_args = type_params_map;
    arg_polarities;
    fields_tmap = Flow_js.mk_propmap cx fmap;
    methods_tmap = Flow_js.mk_propmap cx mmap;
    mixins = false;
    structural;
  } in
  let this = InstanceT (reason_i, static, super, instance) in

  if typeparams = []
  then ClassT(this)
  else PolyT (typeparams, ClassT(this))

(* Given a function declaration and types for `this` and `super`, extract a
   signature consisting of type parameters, parameter types, parameter names,
   and return type, check the body against that signature by adding `this`
   and super` to the environment, and return the signature. *)
and function_decl id cx type_params_map (reason:reason) ~kind
  type_params params ret body this super =

  let typeparams, type_params_map =
    mk_type_param_declarations cx type_params_map type_params in

  let params, ret =
    mk_params_ret cx type_params_map params (body, ret) in

  let save_return_exn = Abnormal.(swap Return false) in
  let save_throw_exn = Abnormal.(swap Throw false) in
  Flow_js.generate_tests cx reason typeparams (fun map_ ->
    let type_params_map =
      type_params_map |> SMap.map (Flow_js.subst cx map_) in
    let params = FuncParams.subst cx map_ params in
    let ret = Flow_js.subst cx map_ ret in

    let yield, next = if kind = Scope.Generator then (
      Flow_js.mk_tvar cx (prefix_reason "yield of " reason),
      Flow_js.mk_tvar cx (prefix_reason "next of " reason)
    ) else (
      MixedT (replace_reason "no yield" reason),
      MixedT (replace_reason "no next" reason)
    ) in

    mk_body id cx type_params_map ~kind params ret body this super yield next;
  );

  ignore Abnormal.(swap Return save_return_exn);
  ignore Abnormal.(swap Throw save_throw_exn);

  let ret =
    if (is_void cx ret)
    then (VoidT.at (loc_of_t ret))
    else ret
  in

  (typeparams,params,ret)

and is_void cx = function
  | OpenT(_,id) ->
      Flow_js.check_types cx id (function | VoidT _ -> true | _ -> false)
  | _ -> false

(* When in a derived constructor, initialize this and super to undefined. *)
and initialize_this_super derived_ctor this super scope = Scope.(
  if derived_ctor then (
    let undefined =
      let msg = "uninitialized this (expected super constructor call)" in
        VoidT (replace_reason msg (reason_of_t this))
    in
      undefine_internal "this" undefined this scope;
      undefine_internal "super" undefined super scope
  )
  else (
    let loc = loc_of_t this in
    add_entry (internal_name "this") (Entry.new_var ~loc this) scope;
    let loc = loc_of_t super in
    add_entry (internal_name "super") (Entry.new_var ~loc super) scope;
  )
)

(* For internal names, we can afford to initialize with undefined and fatten the
   declared type to allow undefined. This works because we always look up the
   flow-sensitive type of internals, and don't havoc them. However, the same
   trick wouldn't work for normal uninitialized locals, e.g., so it cannot be
   used in general to track definite assignments. *)
and undefine_internal x undefined t scope = Scope.(
  (* TODO state uninitialized *)
  let loc = loc_of_t t in
  let entry = Entry.new_var ~loc ~specific:undefined (OptionalT t) in
  add_entry (internal_name x) entry scope
)

(* Switch back to the declared type for an internal name. *)
and define_internal cx reason x =
  let ix = internal_name x in
  let opt = Env_js.get_var_declared_type cx ix reason in
  Env_js.set_var cx ix (Flow_js.filter_optional cx reason opt) reason

and mk_body id cx type_params_map ~kind ?(derived_ctor=false)
    params ret body this super yield next =

  let loc = Ast.Statement.FunctionDeclaration.(match body with
    | BodyBlock (loc, _)
    | BodyExpression (loc, _) -> loc
  ) in
  let reason = mk_reason "function body" loc in

  let env =  Env_js.peek_env () in
  let new_env = Env_js.clone_env env in

  Env_js.update_env cx reason new_env;
  Env_js.havoc_all();

  (* create and prepopulate function scope *)
  let function_scope = Scope.fresh ~var_scope_kind:kind () in

  (* push the scope early so default exprs can reference earlier params *)
  Env_js.push_var_scope cx function_scope;

  (* add param bindings *)
  params |> FuncParams.iter Scope.(fun (name, t, loc) ->
    (* add default value as lower bound, if provided *)
    FuncParams.with_default name (fun expr ->
      let te = expression cx type_params_map expr in
      Flow_js.flow cx (te, t)
    ) params;
    (* add to scope *)
    let reason = mk_reason (spf "param `%s`" name) loc in
    Env_js.bind_implicit_let ~state:State.Initialized
      Entry.ParamBinding cx name t reason);

  (* early-add our own name binding for recursive calls *)
  (match id with
  | None -> ()
  | Some (loc, { Ast.Identifier.name; _ }) ->
    let entry = Scope.Entry.new_var ~loc (AnyT.at loc) in
    Scope.add_entry name entry function_scope);

  (* special bindings for this, super, and return value slot *)
  initialize_this_super derived_ctor this super function_scope;
  Scope.(
    let new_entry t =
      Entry.(new_const ~loc:(loc_of_t t) ~state:State.Initialized t)
    in
    add_entry (internal_name "yield") (new_entry yield) function_scope;
    add_entry (internal_name "next") (new_entry next) function_scope;
    add_entry (internal_name "return") (new_entry ret) function_scope
  );

  let stmts = Ast.Statement.(match body with
    | FunctionDeclaration.BodyBlock (_, { Block.body }) ->
        body
    | FunctionDeclaration.BodyExpression expr ->
        [ fst expr, Return { Return.argument = Some expr } ]
  ) in

  (* decl/type visit pre-pass *)
  toplevel_decls cx type_params_map stmts;

  (* statement visit pass *)
  let is_void = Abnormal.(
    match catch_control_flow_exception (fun () ->
      toplevels cx type_params_map stmts
    ) with
    | Some Return -> false
    | Some Throw -> false (* NOTE *)
    | Some exn -> throw_control_flow_exception exn (* NOTE *)
    | None -> true
  ) in

  (* build return type for void funcs *)
  (if is_void then
    let loc = loc_of_t ret in
    let void_t = Scope.(match kind with
    | Ordinary ->
      VoidT (mk_reason "return undefined" loc)
    | Async ->
      let reason = mk_reason "return Promise<Unit>" loc in
      let promise = Flow_js.get_builtin cx "Promise" reason in
      TypeAppT (promise, [VoidT.at loc])
    | Generator ->
      let reason = mk_reason "return Generator<Yield,void,Next>" loc in
      let ret = VoidT.at loc in
      Flow_js.get_builtin_typeapp cx reason "Generator" [yield; ret; next]
    | Module -> assert_false "module scope as function activation"
    | Global -> assert_false "global scope as function activation"
    ) in
    Flow_js.flow cx (void_t, ret)
  );

  Env_js.pop_var_scope ();

  Env_js.update_env cx reason env

and before_pos loc =
  Loc.(
    let line = loc.start.line in
    let column = loc.start.column in
    let offset = loc.start.offset in
    { loc with
        start = { line = line; column = column - 1; offset = offset - 1; };
        _end = { line = line; column = column; offset = offset; }
    }
  )

and mk_params_ret cx type_params_map params (body, ret_type_opt) =
  let (params, defaults, rest) = params in
  let defaults = if defaults = [] && params <> []
    then List.map (fun _ -> None) params
    else defaults
  in

  let params = List.fold_left2 (FuncParams.add cx type_params_map)
    FuncParams.empty params defaults in

  let params = match rest with
  | Some ident -> FuncParams.add_rest cx type_params_map params ident
  | None -> params in

  let phantom_return_loc = Ast.Statement.FunctionDeclaration.(match body with
    | BodyBlock (loc, _) -> before_pos loc
    | BodyExpression (loc, _) -> loc
  ) in

  let return_type = mk_type_annotation cx type_params_map
    (mk_reason "return" phantom_return_loc) ret_type_opt in

  params, return_type

(* Process a function definition, returning a (polymorphic) function type. *)
and mk_function id cx type_params_map reason ?(kind=Scope.Ordinary)
  type_params params ret body this =
  (* Normally, functions do not have access to super. *)
  let super = MixedT (replace_reason "empty super object" reason) in
  let signature =
    function_decl id cx type_params_map reason ~kind type_params params ret body this super
  in
  mk_function_type cx reason this signature

(* Process an arrow function, returning a (polymorphic) function type. *)
and mk_arrow id cx type_params_map reason ~kind type_params params ret body this super =
  let signature =
    function_decl id cx type_params_map reason ~kind type_params params ret body this super
  in
  (* Do not expose the type of `this` in the function's type. The call to
     function_decl above has already done the necessary checking of `this` in
     the body of the function. Now we want to avoid re-binding `this` to
     objects through which the function may be called. *)
  mk_function_type cx reason Flow_js.dummy_this signature

(* Make a function type given the receiver type and the signature extracted from
   a function declaration. *)
and mk_function_type cx reason this signature =
  let typeparams, params, ret = signature in

  (* prepare type *)
  let proto_reason = replace_reason "prototype" reason in
  let prototype = mk_object cx proto_reason in
  let static_reason = prefix_reason "statics of " reason in
  let static = Flow_js.mk_object_with_proto cx static_reason (FunProtoT static_reason) in

  let funtype = {
    this_t = this;
    params_tlist = FuncParams.tlist params;
    params_names = Some (FuncParams.names params);
    return_t = ret;
    closure_t = Env_js.peek_frame ();
    changeset = Env_js.retrieve_closure_changeset ()
  } in

  if (typeparams = [])
  then
    FunT (reason, static, prototype, funtype)
  else
    PolyT (typeparams, FunT(reason, static, prototype, funtype))

(* This function is around for the sole purpose of modeling some method-like
   behaviors of non-ES6 React classes. It is otherwise deprecated. *)
and mk_method cx type_params_map reason ?(kind=Scope.Ordinary)
  params ret body this super =
  let (_,params,ret) =
    function_decl None cx type_params_map ~kind reason None params ret body this super
  in
  let params_tlist = FuncParams.tlist params in
  let params_names = Some (FuncParams.names params) in
  let frame = Env_js.peek_frame () in
  FunT (
    reason,
    Flow_js.dummy_static reason,
    Flow_js.dummy_prototype,
    Flow_js.mk_functiontype2 params_tlist ?params_names ret frame
  )


(**********)
(* Driver *)
(**********)

(* cross-module GC toggle *)
let xmgc_enabled = ref true

(**************************************)

let cross_module_gc =
  let require_set = ref SSet.empty in
  fun cx ms reqs ->
    if !xmgc_enabled then (
      reqs |> SSet.iter (fun r -> require_set := SSet.add r !require_set);
      ms |> SSet.iter (fun m -> require_set := SSet.remove m !require_set);

      let ins = SSet.elements !require_set in
      Gc_js.do_gc cx ins
    )

let force_annotations cx =
  let m = Modulename.to_string (Context.module_name cx) in
  let tvar = Flow_js.lookup_module cx m in
  let reason, id = open_tvar tvar in
  let before = Errors_js.ErrorSet.cardinal (Context.errors cx) in
  Flow_js.enforce_strict cx id;
  let after = Errors_js.ErrorSet.cardinal (Context.errors cx) in
  if (after > before) then
    Context.add_tvar cx id Constraint_js.(Root {
      rank = 0; constraints = Resolved AnyT.t
    })

(* core inference, assuming setup and teardown happens elsewhere *)
let infer_core cx type_params_map statements =
  try
    statements |> toplevel_decls cx type_params_map;
    statements |> toplevels cx type_params_map;
  with
    | Abnormal.Exn _ ->
        let msg = "abnormal control flow" in
        Flow_js.add_warning cx [mk_reason "" Loc.({
          none with source = Some (Context.file cx)
        }), msg]
    | exc ->
        let msg = fmt_exc exc in
        Flow_js.add_warning cx [mk_reason "" Loc.({
          none with source = Some (Context.file cx)
        }), msg]

(* There's a .flowconfig option to specify suppress_comments regexes. Any
 * comments that match those regexes will suppress any errors on the next line
 *)
let scan_for_suppressions =
  let should_suppress suppress_comments comment =
    List.exists (fun r -> Str.string_match r comment 0) suppress_comments

  in fun cx comments ->
    let config = FlowConfig.get_unsafe () in
    let suppress_comments = FlowConfig.(config.options.Opts.suppress_comments) in
    let should_suppress = should_suppress suppress_comments in

    (* Bail immediately if we're not using error suppressing comments *)
    if suppress_comments <> []
    then List.iter (function
      | loc, Ast.Comment.Block comment
      | loc, Ast.Comment.Line comment when should_suppress comment ->
          Context.add_error_suppression cx loc
      | _ -> ()) comments

(* build module graph *)
let infer_ast ?(gc=true) ~metadata ~filename ~module_name ast =
  Flow_js.Cache.clear();

  let loc, statements, comments = ast in

  let cx = Flow_js.fresh_context metadata filename module_name in
  let checked = Context.is_checked cx in

  let exported_module_name = Modulename.to_string module_name in
  let reason_exports_module =
    reason_of_string (spf "exports of module `%s`" exported_module_name) in

  let local_exports_var = Flow_js.mk_tvar cx reason_exports_module in

  let module_scope = Scope.(
    let scope = fresh ~var_scope_kind:Module () in

    add_entry "exports"
      (Entry.new_var ~loc:(loc_of_t local_exports_var) local_exports_var)
      scope;

    add_entry (internal_name "exports")
      (Entry.new_var
        ~loc:(loc_of_reason reason_exports_module)
        ~specific:(EmptyT (replace_reason "undefined exports" reason_exports_module))
        (AnyT reason_exports_module))
      scope;

    scope
  ) in

  Env_js.init_env cx module_scope;

  let reason = mk_reason "exports" Loc.({
    none with source = Some filename
  }) in

  if checked then (
    let init_exports = mk_object cx reason in
    set_module_exports cx reason init_exports;

    let type_params_map = SMap.empty in

    let initial_module_t = exports cx in

    (* infer *)
    Flow_js.flow cx (init_exports, local_exports_var);
    infer_core cx type_params_map statements;

    scan_for_suppressions cx comments;

    let module_t = Context.(
      match Context.module_exports_type cx with
      (* CommonJS with a clobbered module.exports *)
      | CommonJSModule(Some(loc)) ->
        let module_exports_t = get_module_exports cx reason in
        let reason = mk_reason "exports" loc in
        mk_commonjs_module_t cx reason_exports_module reason module_exports_t

      (* CommonJS with a mutated 'exports' object *)
      | CommonJSModule(None) ->
        mk_commonjs_module_t cx reason_exports_module reason local_exports_var

      (* Uses standard ES module exports *)
      | ESModule -> mk_module_t cx reason_exports_module
    ) in
    Flow_js.flow cx (module_t, initial_module_t)
  ) else (
    Flow_js.unify cx (exports cx) AnyT.t
  );

  (* insist that whatever type flows into exports is fully annotated *)
  force_annotations cx;

  (if gc then
    let ins = SSet.elements (Context.required cx) in
    let out = exported_module_name in
    Gc_js.do_gc cx (out::ins));

  cx

let apply_docblock_overrides metadata docblock_info =
  (* TODO: Facebook uses a @preventMunge annotation to force `munge_underscores`
   * off on a per-file basis. We should parse the comments like we do above. *)
  Context.(match Docblock.flow docblock_info with
  | None -> metadata
  | Some Docblock.OptIn -> { metadata with checked = true; }
  | Some Docblock.OptInWeak -> { metadata with checked = true; weak = true }

  (* --all (which sets metadata.checked = true) overrides @noflow, so there are
     currently no scenarios where we'd change checked = true to false. in the
     future, there may be a case where checked defaults to true (but is not
     forced to be true ala --all), but for now we do *not* want to force
     checked = false here. *)
  | Some Docblock.OptOut -> metadata
  )

(* Given a filename, retrieve the parsed AST, derive a module name,
   and invoke the local (infer) pass. This will build and return a
   fresh context object for the module. *)
let infer_module ~metadata filename =
  let ast, info = Parsing_service_js.get_ast_and_info_unsafe filename in
  let module_name = Module_js.exported_module filename info in
  let metadata = apply_docblock_overrides metadata info in
  infer_ast ~metadata ~filename ~module_name ast

(* Map.union: which is faster, union M N or union N M when M > N?
   union X Y = fold add X Y which means iterate over X, adding to Y
   So running time is roughly X * log Y.

   Now, when M > N, we have M * log N > N * log M.
   So do union N M as long as N may override M for overlapping keys.
*)

(* Copy context from cx_other to cx *)
let copy_context cx cx_other =
  Context.set_envs cx
    (IMap.union (Context.envs cx_other) (Context.envs cx));
  Context.set_property_maps cx
    (IMap.union (Context.property_maps cx_other) (Context.property_maps cx));
  Context.set_globals cx
    (SSet.union (Context.globals cx_other) (Context.globals cx));
  Context.set_graph cx
    (IMap.union (Context.graph cx_other) (Context.graph cx))

(* Connect the builtins object in master_cx to the builtins reference in some
   arbitrary cx. *)
let implicit_require_strict cx master_cx cx_to =
  let from_t = Flow_js.lookup_module master_cx Files_js.lib_module in
  let to_t = Flow_js.lookup_module cx_to Files_js.lib_module in
  Flow_js.flow cx (from_t, to_t)

(* Connect the export of cx_from to its import in cx_to. This happens in some
   arbitrary cx, so cx_from and cx_to should have already been copied to cx. *)
let explicit_impl_require_strict cx (cx_from, r, resolved_r, cx_to) =
  let resolved_r = Modulename.to_string resolved_r in
  let from_t = Flow_js.lookup_module cx_from resolved_r in
  let to_t = Flow_js.lookup_module cx_to r in
  Flow_js.flow cx (from_t, to_t)

(* Connect a export of a declared module to its import in cxs_to. This happens
   in some arbitrary cx, so cx_to should have already been copied to cx. *)
let explicit_decl_require_strict cx (m, resolved_m, cx_to) =
  let loc = SMap.find_unsafe m (Context.require_loc cx_to) in
  let reason = Reason_js.mk_reason m loc in

  (* lookup module declaration from builtin context *)
  (* TODO: cache in modulemap *)

  let m_name = Modulename.to_string resolved_m in
  let from_t = Flow_js.mk_tvar cx reason in
  Flow_js.lookup_builtin cx (internal_module_name m_name) reason None from_t;

  (* flow the declared module type to importing context *)
  let to_t = Flow_js.lookup_module cx_to m in
  Flow_js.flow cx (from_t, to_t)

(* Merge a component with its "implicit requires" and "explicit requires." The
   implicit requires are those defined in libraries. For the explicit
   requires, we need to merge only those parts of the dependency graph that the
   component immediately depends on. (We assume that this merging is part of a
   recursive process that has already handled recursive dependencies.)

   Now, by definition, files in a component can bidirectionally depend only on
   other files in the component. All other dependencies are unidirectional.

   Let dep_cxs contain the (optimized) contexts of all dependencies that are
   unidirectional, and let component_cxs contain the contexts of the files in
   the component. Let master_cx be the (optimized) context of libraries.

   Let implementations contain the dependency edges between contexts in
   component_cxs and dep_cxs, and declarations contain the dependency edges from
   component_cxs to master_cx.

   We assume that the first context in component_cxs is that of the leader (cx):
   this serves as the "host" for the merging. Let the remaining contexts in
   component_cxs be other_cxs.

   1. Copy dep_cxs, other_cxs, and master_cx to the host cx.

   2. Link the edges in implementations.

   3. Link the edges in declarations.

   4. Link the local references to libraries in master_cx and component_cxs.
*)
let merge_component_strict component_cxs dep_cxs
    implementations declarations master_cx =
  let cx, other_cxs = List.hd component_cxs, List.tl component_cxs in
  Flow_js.Cache.clear();

  dep_cxs |> List.iter (copy_context cx);
  other_cxs |> List.iter (copy_context cx);
  copy_context cx master_cx;

  implementations |> List.iter (explicit_impl_require_strict cx);

  declarations |> List.iter (explicit_decl_require_strict cx);

  other_cxs |> List.iter (implicit_require_strict cx master_cx);
  implicit_require_strict cx master_cx cx;

  ()

(* After merging dependencies into a context (but before optimizing the
   context), it is important to restore the parts of the context that were
   copied from other, already optimized contexts (dep_cxs and master_cx, see
   above comment for details on what they mean). Indeed, merging is an
   imperative process, and there is no guarantee that those parts of the context
   would have remained unchanged.

   Restoration maintains consistency for "diamond-shaped" dependency relations:
   it forces two contexts B and C that depend on the same context A to agree on
   the meaning of the parts of A they share (and that meaning is dictated by A
   itself), and so some context D that depends on both B and C (and perhaps A
   too) is never confused when merging them.
*)
let restore cx dep_cxs master_cx =
  dep_cxs |> List.iter (copy_context cx);
  copy_context cx master_cx

(* load a parsed library file.
   processing is similar to an ordinary module, except that
   a) symbols from prior library loads are suppressed if found,
   b) bindings are added as properties to the builtin object
 *)
let load_lib_file ~verbose ~exclude_syms file statements comments
  save_errors save_suppressions =

  Flow_js.Cache.clear();

  let cx = Flow_js.fresh_context { Context.
    checked = false;
    weak = false;
    munge_underscores = false; (* no sense supporting private props in libs *)
    verbose;
    is_declaration_file = false;
  } file (Modulename.String Files_js.lib_module) in

  let module_scope = Scope.fresh () in
  Env_js.init_env ~exclude_syms cx module_scope;

  let type_params_map = SMap.empty in

  infer_core cx type_params_map statements;
  scan_for_suppressions cx comments;

  module_scope |> Scope.(iter_entries Entry.(fun name entry ->
    Flow_js.set_builtin cx name (actual_type entry)
  ));

  let master_cx = Flow_js.master_cx () in
  copy_context master_cx cx;
  implicit_require_strict master_cx master_cx cx;

  let errs = Context.errors cx in
  Context.remove_all_errors cx;
  save_errors file errs;
  save_suppressions file (Context.error_suppressions cx);

  SMap.keys Scope.(module_scope.entries)
