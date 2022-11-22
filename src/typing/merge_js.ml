(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ALocFuzzyMap = Loc_collections.ALocFuzzyMap

module PierceImplicitInstantiation : Implicit_instantiation.S with type output = Type.t =
  Implicit_instantiation.Make
    (struct
      type output = Type.t

      let on_pinned_tparam _ _ _ inferred = inferred

      let on_constant_tparam_missing_bounds _ _ tparam =
        match tparam.Type.default with
        | None -> tparam.Type.bound
        | Some t -> t

      let on_missing_bounds cx ~use_op name tparam ~tparam_binder_reason ~instantiation_reason =
        if tparam.Type.default = None then
          Flow_js.add_output
            cx
            (Error_message.EImplicitInstantiationUnderconstrainedError
               {
                 bound = Subst_name.string_of_subst_name name;
                 reason_call = instantiation_reason;
                 reason_tparam = tparam_binder_reason;
                 use_op;
               }
            );
        Type.AnyT.error tparam_binder_reason

      let on_upper_non_t cx ~use_op name u _ ~tparam_binder_reason ~instantiation_reason =
        if Build_mode.dev then
          let msg =
            Subst_name.string_of_subst_name name
            ^ " contains a non-Type.t upper bound "
            ^ Type.string_of_use_ctor u
            ^ Type.(
                match u with
                | UseT (_, TypeDestructorTriggerT (_, _, _, d, _)) ->
                  " " ^ Debug_js.string_of_destructor d
                | _ -> ""
              )
          in
          Flow_js_utils.add_output
            cx
            (Error_message.EImplicitInstantiationTemporaryError
               (Reason.aloc_of_reason tparam_binder_reason, msg)
            )
        else
          Flow_js_utils.add_output
            cx
            (Error_message.EImplicitInstantiationUnderconstrainedError
               {
                 bound = Subst_name.string_of_subst_name name;
                 reason_call = instantiation_reason;
                 reason_tparam = tparam_binder_reason;
                 use_op;
               }
            );
        Type.AnyT.error tparam_binder_reason
    end)
    (Flow_js.FlowJs)

let create_cx_with_context_optimizer init_cx master_cx ~reducer ~f =
  let file = Context.file init_cx in
  let metadata = Context.metadata init_cx in
  let aloc_table = Utils_js.FilenameMap.find file (Context.aloc_tables init_cx) in
  let ccx = Context.make_ccx master_cx in
  let res = f () in
  Context.merge_into
    ccx
    {
      Type.TypeContext.graph = reducer#get_reduced_graph;
      trust_graph = reducer#get_reduced_trust_graph;
      property_maps = reducer#get_reduced_property_maps;
      call_props = reducer#get_reduced_call_props;
      export_maps = reducer#get_reduced_export_maps;
      evaluated = reducer#get_reduced_evaluated;
    };
  let cx = Context.make ccx metadata file aloc_table Context.PostInference in
  (cx, res)

let detect_sketchy_null_checks cx master_cx =
  let add_error ~loc ~null_loc kind falsy_loc =
    Error_message.ESketchyNullLint { kind; loc; null_loc; falsy_loc } |> Flow_js.add_output cx
  in
  let detect_function exists_excuses loc exists_check =
    ExistsCheck.(
      let exists_excuse =
        Loc_collections.ALocMap.find_opt loc exists_excuses |> Base.Option.value ~default:empty
      in
      match exists_check.null_loc with
      | None -> ()
      | Some null_loc ->
        let add_error = add_error ~loc ~null_loc in
        if Base.Option.is_none exists_excuse.bool_loc then
          Base.Option.iter exists_check.bool_loc ~f:(add_error Lints.SketchyNullBool);
        if Base.Option.is_none exists_excuse.number_loc then
          Base.Option.iter exists_check.number_loc ~f:(add_error Lints.SketchyNullNumber);
        if Base.Option.is_none exists_excuse.bigint_loc then
          Base.Option.iter exists_check.bigint_loc ~f:(add_error Lints.SketchyNullBigInt);
        if Base.Option.is_none exists_excuse.string_loc then
          Base.Option.iter exists_check.string_loc ~f:(add_error Lints.SketchyNullString);
        if Base.Option.is_none exists_excuse.mixed_loc then
          Base.Option.iter exists_check.mixed_loc ~f:(add_error Lints.SketchyNullMixed);
        if Base.Option.is_none exists_excuse.enum_bool_loc then
          Base.Option.iter exists_check.enum_bool_loc ~f:(add_error Lints.SketchyNullEnumBool);
        if Base.Option.is_none exists_excuse.enum_number_loc then
          Base.Option.iter exists_check.enum_number_loc ~f:(add_error Lints.SketchyNullEnumNumber);
        if Base.Option.is_none exists_excuse.enum_bigint_loc then
          Base.Option.iter exists_check.enum_bigint_loc ~f:(add_error Lints.SketchyNullEnumBigInt);
        if Base.Option.is_none exists_excuse.enum_string_loc then
          Base.Option.iter exists_check.enum_string_loc ~f:(add_error Lints.SketchyNullEnumString);
        ()
    )
  in
  let exists_checks =
    let open Loc_collections in
    let open ExistsCheck in
    let checks = Context.exists_checks cx in
    if not @@ ALocMap.is_empty checks then
      let reducer =
        object
          inherit
            Context_optimizer.context_optimizer
              ~no_lowers:(fun _ r -> Type.EmptyT.make r (Type.bogus_trust ())) as super

          method! type_ cx pole t =
            let open Type in
            match t with
            | ModuleT _
            | EvalT _
            | ThisClassT _
            | TypeDestructorTriggerT _
            | OpenPredT _
            | DefT
                ( _,
                  _,
                  ( InstanceT _ | ClassT _ | FunT _ | ArrT _ | ObjT _ | PolyT _
                  | ReactAbstractComponentT _ )
                ) ->
              t
            | _ -> super#type_ cx pole t
        end
      in

      let (cx, checks) =
        create_cx_with_context_optimizer cx master_cx ~reducer ~f:(fun () ->
            ALocMap.map (Type.TypeSet.map (reducer#type_ cx Polarity.Neutral)) checks
        )
      in

      let rec make_checks seen cur_checks loc t =
        let open Type in
        let open TypeUtil in
        let open Reason in
        match t with
        | AnnotT (_, t, _) -> make_checks seen cur_checks loc t
        | OpenT (_, id) when ISet.mem id seen -> cur_checks
        | OpenT (_, id) ->
          Context.find_resolved cx t
          |> Base.Option.value_map
               ~f:(make_checks (ISet.add id seen) cur_checks loc)
               ~default:cur_checks
        (* Ignore AnyTs for sketchy null checks; otherwise they'd always trigger the lint. *)
        | AnyT _ -> cur_checks
        | GenericT { bound = t; _ }
        | OpaqueT (_, { underlying_t = Some t; _ })
        | OpaqueT (_, { underlying_t = None; super_t = Some t; _ }) ->
          make_checks seen cur_checks loc t
        | MaybeT (r, t) ->
          let acc = make_checks seen cur_checks loc t in
          let acc = make_checks seen acc loc (NullT.why r (Trust.bogus_trust ())) in
          make_checks seen acc loc (VoidT.why r (Trust.bogus_trust ()))
        | OptionalT { reason = r; type_ = t; _ } ->
          let acc = make_checks seen cur_checks loc t in
          make_checks seen acc loc (VoidT.why r (Trust.bogus_trust ()))
        | UnionT (_, rep) ->
          UnionRep.members rep
          |> Base.List.fold ~f:(fun acc t -> make_checks seen acc loc t) ~init:cur_checks
        | _ ->
          let t_loc =
            let reason = reason_of_t t in
            match annot_aloc_of_reason reason with
            | Some loc -> Some loc
            | None -> Some (def_aloc_of_reason reason)
          in
          let exists_check =
            ALocMap.find_opt loc cur_checks |> Base.Option.value ~default:ExistsCheck.empty
          in
          let exists_check =
            match Type_filter.maybe t with
            | DefT (_, _, EmptyT) -> exists_check
            | _ -> { exists_check with null_loc = t_loc }
          in
          let exists_check =
            match t |> Type_filter.not_exists |> Type_filter.not_maybe with
            | DefT (_, _, BoolT _) -> { exists_check with bool_loc = t_loc }
            | DefT (_, _, StrT _) -> { exists_check with string_loc = t_loc }
            | DefT (_, _, NumT _) -> { exists_check with number_loc = t_loc }
            | DefT (_, _, BigIntT _) -> { exists_check with bigint_loc = t_loc }
            | DefT (_, _, MixedT _) -> { exists_check with mixed_loc = t_loc }
            | DefT (_, _, EnumT { representation_t = DefT (_, _, BoolT _); _ }) ->
              { exists_check with enum_bool_loc = t_loc }
            | DefT (_, _, EnumT { representation_t = DefT (_, _, StrT _); _ }) ->
              { exists_check with enum_string_loc = t_loc }
            | DefT (_, _, EnumT { representation_t = DefT (_, _, NumT _); _ }) ->
              { exists_check with enum_number_loc = t_loc }
            | DefT (_, _, EnumT { representation_t = DefT (_, _, BigIntT _); _ }) ->
              { exists_check with enum_bigint_loc = t_loc }
            | _ -> exists_check
          in
          if exists_check = ExistsCheck.empty then
            cur_checks
          else
            ALocMap.add loc exists_check cur_checks
      in

      ALocMap.fold
        (fun loc tset acc ->
          Type.TypeSet.fold (fun t acc -> make_checks ISet.empty acc loc t) tset acc)
        checks
        ALocMap.empty
    else
      ALocMap.empty
  in

  Loc_collections.ALocMap.iter (detect_function (Context.exists_excuses cx)) exists_checks

let detect_test_prop_misses cx =
  let misses = Context.test_prop_get_never_hit cx in
  Base.List.iter
    ~f:(fun (prop_name, (reason_prop, reason_obj), use_op, suggestion) ->
      Flow_js.add_output
        cx
        (Error_message.EPropNotFound { prop_name; reason_prop; reason_obj; use_op; suggestion }))
    misses

let detect_unnecessary_optional_chains cx =
  Base.List.iter
    ~f:(fun (loc, lhs_reason) ->
      Flow_js.add_output cx (Error_message.EUnnecessaryOptionalChain (loc, lhs_reason)))
    (Context.unnecessary_optional_chains cx)

let detect_unnecessary_invariants cx =
  Base.List.iter
    ~f:(fun (loc, reason) ->
      Flow_js.add_output cx (Error_message.EUnnecessaryInvariant (loc, reason)))
    (Context.unnecessary_invariants cx)

let detect_es6_import_export_errors = Strict_es6_import_export.detect_errors

let detect_escaped_generics results =
  Base.List.iter
    ~f:(fun (cx, _, (_, { Flow_ast.Program.statements; _ })) ->
      Generic_escape.scan_for_escapes cx ~add_output:Flow_js.add_output statements)
    results

let detect_non_voidable_properties cx =
  (* This function approximately checks whether VoidT can flow to the provided
   * type without actually creating the flow so as not to disturb type inference.
   * Even though this is happening post-merge, it is possible to encounter an
   * unresolved tvar, in which case it conservatively returns false.
   *)
  let rec is_voidable seen_ids =
    Type.(
      function
      | OpenT (_, id) ->
        (* tvar is recursive: conservatively assume it is non-voidable *)
        if ISet.mem id seen_ids then
          false
        else (
          match Flow_js_utils.possible_types cx id with
          (* tvar has no lower bounds: we conservatively assume it's non-voidable
           * except in the special case when it also has no upper bounds
           *)
          | [] -> Flow_js_utils.possible_uses cx id = []
          (* tvar is resolved: look at voidability of the resolved type *)
          | [t] -> is_voidable (ISet.add id seen_ids) t
          (* tvar is unresolved: conservatively assume it is non-voidable *)
          | _ -> false
        )
      (* a union is voidable if any of its members are voidable *)
      | UnionT (_, rep) -> UnionRep.members rep |> List.exists (is_voidable seen_ids)
      (* an intersection is voidable if all of its members are voidable *)
      | IntersectionT (_, rep) -> InterRep.members rep |> List.for_all (is_voidable seen_ids)
      (* trivially voidable *)
      | MaybeT _
      | DefT (_, _, (VoidT | MixedT (Mixed_everything | Mixed_non_null)))
      | OptionalT _
      | AnyT _ ->
        true
      (* conservatively assume all other types are non-voidable *)
      | _ -> false
    )
  in
  let check_properties (property_map : Type.Properties.id) :
      ALoc.t Property_assignment.error list SMap.t -> unit =
    let pmap = Context.find_props cx property_map in
    SMap.iter (fun name errors ->
        let should_error =
          match NameUtils.Map.find_opt (Reason.OrdinaryName name) pmap with
          | Some (Type.Field (_, t, _)) -> not @@ is_voidable ISet.empty t
          | _ -> true
        in
        if should_error then
          List.iter
            (fun { Property_assignment.loc; desc } ->
              Flow_js.add_output cx (Error_message.EUninitializedInstanceProperty (loc, desc)))
            errors
    )
  in
  List.iter
    (fun {
           Context.public_property_map;
           private_property_map;
           errors = { Property_assignment.public_property_errors; private_property_errors };
         } ->
      check_properties public_property_map public_property_errors;
      check_properties private_property_map private_property_errors)
    (Context.voidable_checks cx)

class resolver_visitor =
  (* TODO: replace this with the context_optimizer *)
  let no_lowers _cx r = Type.Unsoundness.merged_any r in
  object (self)
    inherit [unit] Type_mapper.t as super

    method! type_ cx map_cx t =
      let open Type in
      match t with
      | OpenT (r, id) -> Flow_js_utils.merge_tvar ~filter_empty:true ~no_lowers cx r id
      | EvalT (t', dt, _id) ->
        let t'' = self#type_ cx map_cx t' in
        let dt' = self#defer_use_type cx map_cx dt in
        if t' == t'' && dt == dt' then
          t
        else
          Flow_cache.Eval.id cx t'' dt'
      | _ -> super#type_ cx map_cx t

    (* Only called from type_ and the CreateObjWithComputedPropT use case *)
    method tvar _cx _seen _r id = id

    (* overridden in type_ *)
    method eval_id _cx _map_cx _id = assert false

    method props cx map_cx id =
      let props_map = Context.find_props cx id in
      let props_map' =
        NameUtils.Map.ident_map (Type.Property.ident_map_t (self#type_ cx map_cx)) props_map
      in
      let id' =
        if props_map == props_map' then
          id
        (* When mapping results in a new property map, we have to use a
           generated id, rather than a location from source. *)
        else
          Context.generate_property_map cx props_map'
      in
      id'

    (* These should already be fully-resolved. *)
    method exports _cx _map_cx id = id

    method call_prop cx map_cx id =
      let t = Context.find_call cx id in
      let t' = self#type_ cx map_cx t in
      if t == t' then
        id
      else
        Context.make_call_prop cx t'
  end

(* It's unfortunate that this function and prepare_implicit_instantiation_checks are here,
 * but ocamlbuild erroneously detects a cycle when using the context optimizer in
 * implicit_instantiation.ml. dune does not detect that cycle, and neither does buck.
 *)
let reduce_implicit_instantiation_check cx check =
  let open Implicit_instantiation_check in
  let { lhs; poly_t = (loc, tparams, t); operation = (use_op, reason_op, op) } = check in
  (* The tvars get resolved either way.
     Erroring on unresolved tvars is undesirable for the post-inference pass *)
  let lhs' = Tvar_resolver.resolved_t cx lhs in
  let tparams' = Nel.ident_map (Tvar_resolver.resolved_typeparam cx) tparams in
  let t' = Tvar_resolver.resolved_t cx t in
  let op' =
    match op with
    | Call calltype -> Call (Tvar_resolver.resolved_fun_call_type cx calltype)
    | Constructor (targs, args) ->
      let targs = Tvar_resolver.resolved_type_args cx targs in
      let args = ListUtils.ident_map (Tvar_resolver.resolved_call_arg cx) args in
      Constructor (targs, args)
    | Jsx { clone; component; config; targs; children = (children, children_spread) } ->
      let reduce_t = Tvar_resolver.resolved_t cx in
      let children = (ListUtils.ident_map reduce_t children, Option.map reduce_t children_spread) in
      let targs = Tvar_resolver.resolved_type_args cx targs in
      Jsx { clone; component = reduce_t component; config = reduce_t config; targs; children }
  in
  { lhs = lhs'; poly_t = (loc, tparams', t'); operation = (use_op, reason_op, op') }

let prepare_implicit_instantiation_checks ~cx implicit_instantiation_checks =
  let implicit_instantiation_checks =
    Base.List.map ~f:(reduce_implicit_instantiation_check cx) implicit_instantiation_checks
  in
  implicit_instantiation_checks

let check_implicit_instantiations cx typed_ast file_sig =
  let () =
    let file = Context.file cx in
    let ty_normalizer_options = Ty_normalizer_env.default_options in
    let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~file_sig ~typed_ast in
    let implicit_instantiation_ty_results =
      Loc_collections.ALocFuzzyMap.mapi
        (fun loc result ->
          let tparams_rev =
            Base.Option.value
              ~default:[]
              (Typed_ast_utils.find_tparams_rev_at_location typed_ast loc)
          in
          List.map
            (fun (t, name) ->
              let scheme = { Type.TypeScheme.type_ = t; tparams_rev } in
              ( (match Ty_normalizer.from_scheme ~options:ty_normalizer_options ~genv scheme with
                | Ok (Ty.Type ty) -> Some ty
                | Ok (Ty.Decl (Ty.ClassDecl (s, _))) -> Some (Ty.TypeOf (Ty.TSymbol s))
                | _ -> None),
                name
              ))
            result)
        (Context.implicit_instantiation_results cx)
    in
    Context.set_implicit_instantiation_ty_results cx implicit_instantiation_ty_results
  in
  Context.run_in_post_inference_mode cx (fun () ->
      if Context.run_post_inference_implicit_instantiation cx then (
        let implicit_instantiation_checks = Context.implicit_instantiation_checks cx in
        let implicit_instantiation_checks =
          prepare_implicit_instantiation_checks ~cx implicit_instantiation_checks
        in
        let saved_errors = Context.errors cx in
        Context.reset_errors cx Flow_error.ErrorSet.empty;
        let run_enable_post_inference_targ_widened_check cx check output_map =
          let {
            Implicit_instantiation_check.poly_t = (_, tparams, _);
            operation = (_, reason_op, _);
            _;
          } =
            check
          in
          let pierce_solution =
            tparams
            |> Nel.to_list
            |> Base.List.map ~f:(fun tparam -> Subst_name.Map.find_opt tparam.Type.name output_map)
            |> Base.Option.all
          in
          let inferred_solution =
            Context.implicit_instantiation_results cx
            |> ALocFuzzyMap.find_opt (Reason.aloc_of_reason reason_op)
          in
          match Base.Option.both inferred_solution pierce_solution with
          | Some (inferred_solution, pierce_solution)
            when List.length inferred_solution = List.length pierce_solution ->
            Base.List.iter2_exn inferred_solution pierce_solution ~f:(fun (t1, name) t2 ->
                let errors_before = Context.errors cx in
                Tvar_resolver.resolve cx t1;
                Tvar_resolver.resolve cx t2;
                Flow_js.unify cx t1 t2;
                if not @@ Flow_error.ErrorSet.equal (Context.errors cx) errors_before then
                  Flow_js_utils.add_output
                    cx
                    Error_message.(
                      EImplicitInstantiationWidenedError
                        { reason_call = reason_op; bound = Subst_name.string_of_subst_name name }
                    )
            )
          | Some _ -> ()
          | None -> ()
        in
        Context.run_in_implicit_instantiation_mode cx (fun () ->
            PierceImplicitInstantiation.fold
              ~implicit_instantiation_cx:cx
              ~cx
              ~init:()
              ~f:(fun cx _ check output_map ->
                if Context.enable_post_inference_targ_widened_check cx then
                  run_enable_post_inference_targ_widened_check cx check output_map)
              ~post:(fun ~cx ~implicit_instantiation_cx:_ ->
                let new_errors = Context.errors cx in
                Context.reset_errors cx saved_errors;
                Flow_error.ErrorSet.iter
                  (fun error ->
                    Error_message.(
                      match Flow_error.msg_of_error error with
                      | EImplicitInstantiationUnderconstrainedError _
                      | EImplicitInstantiationWidenedError _
                      | EImplicitInstantiationTemporaryError _ ->
                        Context.add_error cx error
                      | _ -> ()
                    ))
                  new_errors)
              implicit_instantiation_checks
        )
      )
  )

let detect_matching_props_violations init_cx master_cx =
  let open Type in
  let peek =
    let open Type in
    let rec loop cx acc seen t =
      match t with
      | OpenT (_, id) ->
        let (root_id, constraints) = Context.find_constraints cx id in
        if ISet.mem root_id seen then
          acc
        else
          let seen = ISet.add root_id seen in
          (match constraints with
          | Constraint.Resolved (_, t)
          | Constraint.FullyResolved (_, (lazy t)) ->
            loop cx acc seen t
          | Constraint.Unresolved bounds ->
            let ts = TypeMap.keys bounds.Constraint.lower in
            List.fold_left (fun a t -> loop cx a seen t) acc ts)
      | AnnotT (_, t, _) -> loop cx acc seen t
      | _ -> List.rev (t :: acc)
    in
    (fun cx t -> loop cx [] ISet.empty t)
  in
  let is_lit t =
    match drop_generic t with
    | DefT (_, _, (BoolT (Some _) | StrT (Literal _) | NumT (Literal _))) -> true
    | _ -> false
  in
  let matching_props_checks =
    Base.List.filter_map (Context.matching_props init_cx) ~f:(fun (prop_name, other_loc, obj_loc) ->
        let env = Context.environment init_cx in
        Env.check_readable init_cx Env_api.ExpressionLoc other_loc;
        let sentinel =
          Base.Option.value_exn (Loc_env.find_write env Env_api.ExpressionLoc other_loc)
        in
        match peek init_cx sentinel with
        | [t] when is_lit t ->
          let obj_t = Env.provider_type_for_def_loc init_cx env obj_loc in
          Some (sentinel, prop_name, sentinel, obj_t)
        | _ -> None
    )
  in
  match matching_props_checks with
  | [] -> ()
  | _ ->
    let reducer =
      let no_lowers _cx r = Type.Unsoundness.merged_any r in
      new Context_optimizer.context_optimizer ~no_lowers
    in
    let step cx (reason, key, sentinel, obj) =
      (* Limit the check to promitive literal sentinels *)
      let use_op =
        Op
          (MatchingProp
             {
               op = reason;
               obj = TypeUtil.reason_of_t obj;
               key;
               sentinel_reason = TypeUtil.reason_of_t sentinel;
             }
          )
      in
      (* If `obj` is a GenericT, we replace it with it's upper bound, since ultimately it will flow into
         `sentinel` rather than the other way around. *)
      Flow_js.flow cx (MatchingPropT (reason, key, sentinel), UseT (use_op, drop_generic obj))
    in
    let (cx, checks) =
      create_cx_with_context_optimizer init_cx master_cx ~reducer ~f:(fun () ->
          Base.List.map matching_props_checks ~f:(fun (reason, prop_name, sentinel, obj_t) ->
              ( TypeUtil.reason_of_t reason,
                prop_name,
                reducer#type_ init_cx Polarity.Neutral sentinel,
                reducer#type_ init_cx Polarity.Neutral obj_t
              )
          )
      )
    in
    Base.List.iter ~f:(step cx) checks;
    let new_errors = Context.errors cx in
    Flow_error.ErrorSet.iter (Context.add_error init_cx) new_errors

let detect_literal_subtypes =
  let open Type in
  let lb_visitor = new resolver_visitor in
  let ub_visitor =
    let rec unwrap = function
      | GenericT { bound; _ } -> unwrap bound
      | t -> t
    in
    object (_self)
      inherit resolver_visitor as super

      method! type_ cx map_cx t = t |> super#type_ cx map_cx |> unwrap
    end
  in
  fun cx ->
    let checks = Context.literal_subtypes cx in
    List.iter
      (fun (loc, check) ->
        let env = Context.environment cx in
        let u_def = Env.provider_type_for_def_loc cx env loc in
        let l =
          match check with
          | Env_api.SingletonNum (lit_loc, sense, num, raw) ->
            let reason = lit_loc |> Reason.(mk_reason (RNumberLit raw)) in
            DefT (reason, bogus_trust (), NumT (Literal (Some sense, (num, raw))))
          | Env_api.SingletonBool (lit_loc, b) ->
            let reason = lit_loc |> Reason.(mk_reason (RBooleanLit b)) in
            DefT (reason, bogus_trust (), BoolT (Some b))
          | Env_api.SingletonStr (lit_loc, sense, str) ->
            let reason = lit_loc |> Reason.(mk_reason (RStringLit (OrdinaryName str))) in
            DefT (reason, bogus_trust (), StrT (Literal (Some sense, Reason.OrdinaryName str)))
        in
        let l = lb_visitor#type_ cx () l in
        let u_def = ub_visitor#type_ cx () u_def in
        Flow_js.flow cx (l, UseT (Op (Internal Refinement), u_def)))
      checks

let check_constrained_writes init_cx master_cx =
  let checks = Context.constrained_writes init_cx in
  if not @@ Base.List.is_empty checks then (
    let reducer =
      let mk_reason =
        let open Reason in
        let open Utils_js in
        update_desc_reason (function
            | RIdentifier (OrdinaryName name) -> RCustom (spf "variable `%s` of unknown type" name)
            | RParameter (Some name)
            | RRestParameter (Some name) ->
              RUnknownParameter name
            | RTypeParam (name, _, _) ->
              RCustom
                (spf "unknown implicit instantiation of `%s`" (Subst_name.string_of_subst_name name))
            | desc -> desc
            )
      in
      new Context_optimizer.context_optimizer ~no_lowers:(fun _ r ->
          Type.EmptyT.make (mk_reason r) (Type.bogus_trust ())
      )
    in
    let (cx, checks) =
      create_cx_with_context_optimizer init_cx master_cx ~reducer ~f:(fun () ->
          Base.List.map
            ~f:(fun (t, use_op, u_def) ->
              let open Type in
              let open Constraint in
              let u_def = reducer#type_ init_cx Polarity.Neutral u_def in
              let (mk_use_op, use_op) =
                let rec loop = function
                  | Frame ((ConstrainedAssignment _ as frame), op) ->
                    (TypeUtil.mod_use_op_of_use_t (fun op -> Frame (frame, op)), op)
                  | Op _ as op -> ((fun x -> x), op)
                  | Frame (frame, op) ->
                    let (f, op) = loop op in
                    (f, Frame (frame, op))
                in
                loop use_op
              in
              let u = UseT (use_op, u_def) in
              match t with
              | OpenT (_, id) ->
                let (_, constraints) = Context.find_constraints init_cx id in
                begin
                  match constraints with
                  | Unresolved { lower; _ } ->
                    TypeMap.bindings lower
                    |> Base.List.map ~f:(fun (t, (_, use_op)) ->
                           let t = reducer#type_ init_cx Polarity.Neutral t in
                           (t, mk_use_op (Flow_js.flow_use_op init_cx use_op u))
                       )
                  | Resolved (use_op, _)
                  | FullyResolved (use_op, _) ->
                    let t = reducer#type_ init_cx Polarity.Neutral t in
                    [(t, mk_use_op (Flow_js.flow_use_op init_cx use_op u))]
                end
              | _ ->
                let t = reducer#type_ init_cx Polarity.Neutral t in
                [(t, mk_use_op u)])
            checks
          |> List.flatten
      )
    in
    Base.List.iter ~f:(Flow_js.flow cx) checks;

    let new_errors = Context.errors cx in
    Flow_error.ErrorSet.iter (Context.add_error init_cx) new_errors
  )

let get_lint_severities metadata strict_mode lint_severities =
  if metadata.Context.strict || metadata.Context.strict_local then
    StrictModeSettings.fold
      (fun lint_kind lint_severities ->
        LintSettings.set_value lint_kind (Severity.Err, None) lint_severities)
      strict_mode
      lint_severities
  else
    lint_severities

(* Post-merge errors.
 *
 * At this point, all dependencies have been merged and the component has been
 * linked together. Any constraints should have already been evaluated, which
 * means we can complain about things that either haven't happened yet, or
 * which require complete knowledge of tvar bounds.
 *)
let post_merge_checks cx master_cx ast tast metadata file_sig =
  let results = [(cx, ast, tast)] in
  check_constrained_writes cx master_cx;
  detect_sketchy_null_checks cx master_cx;
  detect_non_voidable_properties cx;
  check_implicit_instantiations cx tast file_sig;
  detect_test_prop_misses cx;
  detect_unnecessary_optional_chains cx;
  detect_unnecessary_invariants cx;
  detect_es6_import_export_errors cx metadata results;
  detect_escaped_generics results;
  detect_matching_props_violations cx master_cx;
  detect_literal_subtypes cx

let optimize_builtins cx =
  let reducer =
    let no_lowers _ r = Type.AnyT (r, Type.AnyError (Some Type.UnresolvedName)) in
    new Context_optimizer.context_optimizer ~no_lowers
  in
  let builtins = Context.builtins cx in
  let on_missing name t =
    let reason = TypeUtil.reason_of_t t in
    Flow_js.flow_t cx (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)), t);
    Flow_js.add_output
      cx
      (Error_message.EBuiltinLookupFailed { reason; name = Some name; potential_generator = None })
  in
  Builtins.optimize_entries builtins ~on_missing ~optimize:(reducer#type_ cx Polarity.Neutral);
  Context.set_graph cx reducer#get_reduced_graph;
  Context.set_trust_graph cx reducer#get_reduced_trust_graph;
  Context.set_property_maps cx reducer#get_reduced_property_maps;
  Context.set_call_props cx reducer#get_reduced_call_props;
  Context.set_export_maps cx reducer#get_reduced_export_maps;
  Context.set_evaluated cx reducer#get_reduced_evaluated
