(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Type
open Reason
open Loc_collections

module type S = sig
  include Env_sig.S

  val resolve_env_entry : Context.t -> Type.t -> ALoc.t -> unit
end

module New_env : S = struct
  (* The new env handles all the logic for regular variables internally, but
     internal variables still need to be handled by name, which makes them
     incompatible with the new environment as it stands. Eventually we'll need
     to figure out a different approach for them, but that different approach will
     vary per-variable (e.g. maybe_exhaustively_checked will go away entirely, returns will
     not be treated as variables, and this/super can probably be included in the new env), and
     currently they rely on quite a bit of the existing env behavior (such as initialization state,
     merging, etc) so it doesn't make sense to build a separate system for them now (it would be
     pretty big if we did).

     Instead, the new env includes the old env directly. The old env's interface for things that
     are unrelated to the new env (e.g. pushing scopes) are left intact, while things that
     can apply to either regular variables or to internals are shadowed with a function that decides
     what interface to use.*)
  module Old_env = Env.Env

  let peek_env = Old_env.peek_env
  let merge_env = Old_env.merge_env
  let update_env = Old_env.update_env
  let clone_env = Old_env.clone_env
  let copy_env = Old_env.copy_env
  let widen_env = Old_env.widen_env
  let havoc_heap_refinements_with_propname = Old_env.havoc_heap_refinements_with_propname
  let havoc_local_refinements = Old_env.havoc_local_refinements
  let havoc_heap_refinements = Old_env.havoc_heap_refinements
  let havoc_vars = Old_env.havoc_vars
  let reset_current_activation = Old_env.reset_current_activation
  let havoc_all = Old_env.havoc_all
  let refine_expr = Old_env.refine_expr
  let get_current_env_refi = Old_env.get_current_env_refi
  let save_excluded_symbols = Old_env.save_excluded_symbols
  let set_internal_var = Old_env.set_internal_var
  let get_internal_var = Old_env.get_internal_var
  let get_class_entries = Old_env.get_class_entries
  let bind_class = Old_env.bind_class
  let restore_excluded_symbols = Old_env.restore_excluded_symbols
  let trunc_env = Old_env.trunc_env
  let env_depth = Old_env.env_depth
  let in_lex_scope = Old_env.in_lex_scope
  let pop_var_scope = Old_env.pop_var_scope
  let push_var_scope = Old_env.push_var_scope
  let in_predicate_scope = Old_env.in_predicate_scope
  let in_generator_scope = Old_env.in_generator_scope
  let in_async_scope = Old_env.in_async_scope
  let var_scope_kind = Old_env.var_scope_kind
  let string_of_env = Old_env.string_of_env
  let in_global_scope = Old_env.in_global_scope
  let in_toplevel_scope = Old_env.in_toplevel_scope
  let is_provider = Old_env.is_provider
  let install_provider = Old_env.install_provider

  type t = Old_env.t
  type scope = Old_env.scope

  let this_type_params = ref ALocMap.empty

  (************************)
  (* Helpers **************)
  (************************)

  let record_expression_type_if_needed cx loc t =
    let env = Context.environment cx in
    match Loc_env.find_write env loc with
    | None -> ()
    | Some w -> Flow_js.unify cx ~use_op:unknown_use t w

  exception LocEnvEntryNotFound of ALoc.t

  let find_var { Env_api.env_values; _ } loc =
    match ALocMap.find_opt loc env_values with
    | None -> raise (LocEnvEntryNotFound loc)
    | Some x -> x

  let find_refi { Env_api.refinement_of_id; _ } = refinement_of_id

  let find_providers { Env_api.providers; _ } loc =
    Env_api.Provider_api.providers_of_def providers loc
    |> Base.Option.value_map ~f:snd ~default:[]
    |> Base.List.map ~f:Reason.aloc_of_reason

  let is_def_loc_annotated { Env_api.providers; _ } loc =
    let providers = Env_api.Provider_api.providers_of_def providers loc in
    match providers with
    | Some (Find_providers.AnnotatedVar, _) -> true
    | _ -> false

  let provider_type_for_def_loc env def_loc =
    let { Loc_env.var_info; _ } = env in
    let providers =
      find_providers var_info def_loc
      |> Base.List.map ~f:(Loc_env.find_write env)
      |> Base.Option.all
    in
    match providers with
    | None -> assert_false (spf "Missing providers for %s" (ALoc.debug_to_string def_loc))
    | Some [] -> MixedT.make (mk_reason (RCustom "no providers") def_loc) (Trust.bogus_trust ())
    | Some [t] -> t
    | Some (t1 :: t2 :: ts) ->
      UnionT (mk_reason (RCustom "providers") def_loc, UnionRep.make t1 t2 ts)

  (*************)
  (*  Reading  *)
  (*************)
  open Env_sig.LookupMode

  (** Computes the phi type for a node given all its lower bounds
 *  Currently, this just produces a new type variable with the types of
 *  all the incoming writes as lower bounds. In the future, however, we
 *  may want to compute a more specific least upper bound for these writes.
 *)
  let phi cx reason ts =
    Tvar.mk_where cx reason (fun tvar -> Base.List.iter ts ~f:(fun t -> Flow_js.flow_t cx (t, tvar)))

  let rec predicate_of_refinement cx =
    Env_api.Refi.(
      function
      | AndR (r1, r2) -> AndP (predicate_of_refinement cx r1, predicate_of_refinement cx r2)
      | OrR (r1, r2) -> OrP (predicate_of_refinement cx r1, predicate_of_refinement cx r2)
      | NotR r -> NotP (predicate_of_refinement cx r)
      | TruthyR loc -> ExistsP (Some loc)
      | NullR -> NullP
      | UndefinedR -> VoidP
      | MaybeR -> MaybeP
      | InstanceOfR (loc, _) ->
        (* Instanceof refinements store the loc they check against, which is a read in the env *)
        let reason = mk_reason (RCustom "RHS of `instanceof` operator") loc in
        let t = read_entry ~for_type:false cx loc reason in
        Flow_js.flow cx (t, AssertInstanceofRHST reason);
        LeftP (InstanceofTest, t)
      | IsArrayR -> ArrP
      | BoolR loc -> BoolP loc
      | FunctionR -> FunP
      | NumberR loc -> NumP loc
      | ObjectR -> ObjP
      | StringR loc -> StrP loc
      | SymbolR loc -> SymbolP loc
      | SingletonBoolR { loc; sense = _; lit } -> SingletonBoolP (loc, lit)
      | SingletonStrR { loc; sense; lit } -> SingletonStrP (loc, sense, lit)
      | SingletonNumR { loc; sense; lit } -> SingletonNumP (loc, sense, lit)
      | SentinelR (prop, loc) ->
        let env = Context.environment cx in
        let other_t = Base.Option.value_exn (Loc_env.find_write env loc) in
        LeftP (SentinelProp prop, other_t)
      | LatentR { func = (func_loc, _); index } ->
        (* Latent refinements store the loc of the callee, which is a read in the env *)
        let reason = mk_reason (RCustom "Function call") func_loc in
        let t = read_entry ~for_type:false cx func_loc reason in
        LatentP (t, index)
      | PropExistsR { propname; loc } ->
        PropExistsP (propname, mk_reason (RProperty (Some (OrdinaryName propname))) loc)
    )

  and refine cx reason loc refi t =
    Base.Option.value_map
      ~f:(fun predicate ->
        let predicate = predicate |> snd |> predicate_of_refinement cx in
        let reason = mk_reason (RRefined (desc_of_reason reason)) loc in
        Tvar.mk_no_wrap_where cx reason (fun tvar ->
            Flow_js.flow cx (t, PredicateT (predicate, tvar))
        ))
      ~default:t
      refi

  and read_entry ~for_type cx loc reason =
    let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
    let rec type_of_state states refi =
      Base.List.map
        ~f:(function
          | Env_api.Undefined reason
          | Env_api.Uninitialized reason ->
            Type.(VoidT.make reason |> with_trust Trust.bogus_trust)
          | Env_api.Undeclared (name, def_loc) ->
            Flow_js.add_output
              cx
              Error_message.(
                EBindingError (EReferencedBeforeDeclaration, loc, OrdinaryName name, def_loc)
              );
            Type.(AnyT.make (AnyError None) reason)
          | Env_api.UndeclaredClass { name; def } when not for_type ->
            let def_loc = aloc_of_reason def in
            Flow_js.add_output
              cx
              Error_message.(
                EBindingError (EReferencedBeforeDeclaration, loc, OrdinaryName name, def_loc)
              );
            Type.(AnyT.make (AnyError None) reason)
          | Env_api.UndeclaredClass { def; _ } ->
            Debug_js.Verbose.print_if_verbose
              cx
              [
                spf
                  "reading %s from location %s"
                  (ALoc.debug_to_string loc)
                  (Reason.aloc_of_reason def |> ALoc.debug_to_string);
              ];
            Base.Option.value_exn (Reason.aloc_of_reason def |> Loc_env.find_write env)
          | Env_api.With_ALoc.Write reason ->
            Debug_js.Verbose.print_if_verbose
              cx
              [
                spf
                  "reading %s from location %s"
                  (ALoc.debug_to_string loc)
                  (Reason.aloc_of_reason reason |> ALoc.debug_to_string);
              ];
            Base.Option.value_exn (Reason.aloc_of_reason reason |> Loc_env.find_write env)
          | Env_api.With_ALoc.Refinement { refinement_id; writes } ->
            find_refi var_info refinement_id |> Base.Option.some |> type_of_state writes
          | Env_api.With_ALoc.Global name ->
            Flow_js.get_builtin cx (Reason.OrdinaryName name) reason
          | Env_api.With_ALoc.Unreachable loc ->
            let reason = mk_reason (RCustom "unreachable value") loc in
            EmptyT.make reason (Trust.bogus_trust ())
          | Env_api.With_ALoc.Projection loc -> Base.Option.value_exn (Loc_env.find_write env loc))
        states
      |> phi cx reason
      |> refine cx reason loc refi
    in
    let var_state = find_var var_info loc in
    type_of_state var_state None

  let get_this_type_param_if_necessary ~otherwise name loc =
    if name = OrdinaryName "this" then
      match ALocMap.find_opt loc !this_type_params with
      | Some t -> t
      | None -> otherwise ()
    else
      otherwise ()

  let get_refinement cx key loc =
    let reason = mk_reason (Key.reason_desc key) loc in
    try Some (read_entry ~for_type:false cx loc reason) with
    | LocEnvEntryNotFound _ -> None

  let get_var ?(lookup_mode = ForValue) cx name loc =
    let for_type =
      match lookup_mode with
      | ForType -> true
      | _ -> false
    in
    ignore lookup_mode;
    let name = OrdinaryName name in
    get_this_type_param_if_necessary name loc ~otherwise:(fun () ->
        read_entry ~for_type cx loc (mk_reason (RIdentifier name) loc)
    )

  let query_var ?(lookup_mode = ForValue) cx name ?desc loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.query_var ~lookup_mode cx name ?desc loc
    | _ ->
      get_this_type_param_if_necessary name loc ~otherwise:(fun () ->
          let desc =
            match desc with
            | Some desc -> desc
            | None -> RIdentifier name
          in
          let for_type =
            match lookup_mode with
            | ForType -> true
            | _ -> false
          in
          read_entry ~for_type cx loc (mk_reason desc loc)
      )

  let var_ref ?(lookup_mode = ForValue) cx ?desc name loc =
    let t = query_var ~lookup_mode cx name ?desc loc in
    Flow_js.reposition cx loc t

  let is_global_var cx _ loc =
    let { Loc_env.var_info; _ } = Context.environment cx in
    let rec local_def_exists states =
      Base.List.exists
        ~f:(function
          | Env_api.With_ALoc.Undefined _ -> true
          | Env_api.With_ALoc.Uninitialized _ -> true
          | Env_api.With_ALoc.UndeclaredClass _ -> true
          | Env_api.With_ALoc.Write _ -> true
          | Env_api.With_ALoc.Unreachable _ -> true
          | Env_api.With_ALoc.Undeclared _ -> true
          | Env_api.With_ALoc.Refinement { refinement_id = _; writes } -> local_def_exists writes
          | Env_api.With_ALoc.Projection _ -> true
          | Env_api.With_ALoc.Global _ -> false)
        states
      |> not
    in
    try
      let var_state = find_var var_info loc in
      local_def_exists var_state
    with
    | LocEnvEntryNotFound _ -> false

  let local_scope_entry_exists cx loc name = not (is_global_var cx name loc)

  let get_var_annotation cx name loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.get_var_annotation cx name loc
    | OrdinaryName _ -> (* TODO *) None

  let get_var_declared_type ?(lookup_mode = ForValue) cx name loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.get_var_declared_type ~lookup_mode cx name loc
    | OrdinaryName _ ->
      let env = Context.environment cx in
      provider_type_for_def_loc env loc

  let constraining_type ~default cx name loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.constraining_type ~default cx name loc
    | OrdinaryName _ ->
      let env = Context.environment cx in
      Base.Option.value ~default (Loc_env.find_write env loc)

  (*************)
  (*  Writing  *)
  (*************)

  let set_expr cx _key loc _o e =
    let env = Context.environment cx in
    match Loc_env.find_write env loc with
    | None ->
      (* As below, this entry is empty if the refinement is never read from *)
      ()
    | Some w -> Flow_js.unify cx ~use_op:unknown_use e w

  let set_env_entry cx ~use_op t loc =
    let ({ Loc_env.var_info; resolved; _ } as env) = Context.environment cx in
    if not (ALocSet.mem loc resolved) then begin
      Debug_js.Verbose.print_if_verbose cx [spf "writing to location %s" (ALoc.debug_to_string loc)];
      begin
        match Loc_env.find_write env loc with
        | None ->
          (* If we don't see a spot for this write, it's because it's never read from. *)
          ()
        | Some w -> Flow_js.unify cx ~use_op t w
      end;

      (* We only perform a subtyping check if this is an assigning write. We call
       * writes to immutable bindings non-assigning writes. For example:
       * const x: number = 3;
       * x = 'string';
       *
       * Since the x = 'string' doesn't actually assign a value to x, we should
       * not perform a subtyping check and a second error saying string is incompatible
       * with number. We should only emit an error saying that a const cannot be reassigned. *)
      match ALocMap.find_opt loc var_info.Env_api.env_entries with
      | Some Env_api.NonAssigningWrite -> ()
      | _ ->
        if not (is_provider cx loc) then
          let general = provider_type_for_def_loc env loc in
          if is_def_loc_annotated var_info loc then
            Flow_js.flow cx (t, UseT (use_op, general))
          else
            Context.add_constrained_write cx (t, UseT (use_op, general))
    end else
      Debug_js.Verbose.print_if_verbose
        cx
        [spf "Location %s already fully resolved" (ALoc.debug_to_string loc)]

  let resolve_env_entry cx t loc =
    set_env_entry cx ~use_op:unknown_use t loc;
    let ({ Loc_env.resolved; _ } as env) = Context.environment cx in
    Context.set_environment cx { env with Loc_env.resolved = ALocSet.add loc resolved }

  let subtype_entry cx ~use_op t loc =
    let env = Context.environment cx in
    match Loc_env.find_write env loc with
    | None ->
      (* If we don't see a spot for this write it is because the annotated
       * binding being looked up here is one that caused a redeclaration
       * error *)
      assert (
        ALocMap.find_opt loc env.Loc_env.var_info.Env_api.env_entries
        = Some Env_api.NonAssigningWrite
      )
    | Some w -> Flow_js.flow cx (t, UseT (use_op, w))

  (* init_entry is called on variable declarations (not assignments), and `t`
     is the RHS type. If the variable is annotated, we just need to check t against
     its type; but if it's not annotated, the RHS t becomes the variable's type. *)
  let init_entry cx ~use_op ~has_anno t loc =
    if has_anno then
      subtype_entry cx ~use_op t loc
    else
      set_env_entry cx ~use_op t loc

  let set_var cx ~use_op _name t loc = set_env_entry cx ~use_op t loc
  let bind cx t loc = set_env_entry cx ~use_op:Type.unknown_use t loc
  let bind_var ?state:_ cx _ t loc = bind cx (TypeUtil.type_t_of_annotated_or_inferred t) loc
  let bind_let ?state:_ cx _ t loc = bind cx (TypeUtil.type_t_of_annotated_or_inferred t) loc

  let bind_implicit_let ?state kind cx name t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.bind_implicit_let ?state kind cx name t loc
    | OrdinaryName _ -> bind cx (TypeUtil.type_t_of_annotated_or_inferred t) loc

  let bind_fun ?state cx name t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.bind_fun ?state cx name t loc
    | OrdinaryName _ -> bind cx t loc

  let bind_implicit_const ?state:_ _ cx _ t loc =
    bind cx (TypeUtil.type_t_of_annotated_or_inferred t) loc

  let bind_const ?state:_ cx _ t loc = bind cx (TypeUtil.type_t_of_annotated_or_inferred t) loc
  let bind_import cx _ t loc = bind cx t loc

  let bind_declare_var cx name t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.bind_declare_var cx name t loc
    | OrdinaryName _ -> bind cx t loc

  let bind_declare_fun cx ~predicate name t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.bind_declare_fun cx ~predicate name t loc
    | OrdinaryName _ -> bind cx t loc

  let bind_type ?state:(_ = Scope.State.Declared) cx _name t loc = bind cx t loc
  let bind_import_type cx _name t loc = bind cx t loc
  let bind_this_tparam ~state:_ _cx t loc = this_type_params := ALocMap.add loc t !this_type_params

  let declare_let cx name =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.declare_let cx name
    | OrdinaryName _ -> Fn.const ()

  let declare_implicit_let kind cx name =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.declare_implicit_let kind cx name
    | OrdinaryName _ -> Fn.const ()

  let declare_const cx name =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.declare_const cx name
    | OrdinaryName _ -> Fn.const ()

  let declare_implicit_const kind cx name =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.declare_implicit_const kind cx name
    | OrdinaryName _ -> Fn.const ()

  let init_var cx ~use_op name ~has_anno t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.init_var cx ~use_op name ~has_anno t loc
    | OrdinaryName _ -> init_entry ~has_anno cx ~use_op t loc

  let init_let cx ~use_op name ~has_anno t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.init_let cx ~use_op name ~has_anno t loc
    | OrdinaryName _ -> init_entry ~has_anno cx ~use_op t loc

  let init_implicit_let kind cx ~use_op name ~has_anno t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.init_implicit_let kind cx ~use_op name ~has_anno t loc
    | OrdinaryName _ -> init_entry ~has_anno cx ~use_op t loc

  let init_fun cx ~use_op name t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.init_fun cx ~use_op name t loc
    | OrdinaryName _ -> set_env_entry cx ~use_op t loc

  let init_const cx ~use_op name ~has_anno t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.init_const cx ~use_op name ~has_anno t loc
    | OrdinaryName _ -> init_entry ~has_anno cx ~use_op t loc

  let init_implicit_const kind cx ~use_op name ~has_anno t loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.init_implicit_const kind cx ~use_op name ~has_anno t loc
    | OrdinaryName _ -> init_entry ~has_anno cx ~use_op t loc

  let init_type cx _name t loc = set_env_entry cx ~use_op:unknown_use t loc
  let pseudo_init_declared_type _ _ _ = ()

  let unify_declared_type ?(lookup_mode = ForValue) cx name loc t =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.unify_declared_type ~lookup_mode cx name loc t
    | OrdinaryName _ -> set_env_entry cx ~use_op:unknown_use t loc

  let unify_declared_fun_type cx name loc t =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.unify_declared_fun_type cx name loc t
    | OrdinaryName _ -> set_env_entry cx ~use_op:unknown_use t loc

  (************************)
  (* Variable Declaration *)
  (************************)

  let init_env ?exclude_syms cx scope =
    Old_env.init_env ?exclude_syms cx scope;
    let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
    ALocMap.fold
      (fun loc env_entry env ->
        match env_entry with
        | Env_api.AssigningWrite reason ->
          let t = Inferred (Tvar.mk cx reason) in
          (* Treat everything as inferred for now for the purposes of annotated vs inferred *)
          Loc_env.initialize env loc t
        | Env_api.NonAssigningWrite -> env)
      var_info.Env_api.env_entries
      env
    |> Context.set_environment cx

  let find_entry cx name ?desc loc =
    match name with
    | InternalName _
    | InternalModuleName _ ->
      Old_env.find_entry cx name ?desc loc
    | OrdinaryName name ->
      assert_false (spf "Looking up an ordinary name entry %s in new environment" name)

  (* This is mostly copied from the same function in env.ml, but with refinements of ordinary names
     being ignored, because that happens in the name_resolver. *)
  let refine_with_preds cx loc preds orig_types =
    let refine_type orig_type pred refined_type =
      Flow_js.flow cx (orig_type, PredicateT (pred, refined_type))
    in
    let mk_refi_type orig_type pred refi_reason =
      refine_type orig_type pred |> Tvar.mk_no_wrap_where cx refi_reason
    in
    let refine_with_pred key pred acc =
      let refi_reason = mk_reason (RRefined (Key.reason_desc key)) loc in
      match key with
      | (OrdinaryName _, _) ->
        (* new env doesn't need to refine ordinary names. The EnvBuilder computes
         * the refinements in advance *)
        acc
      | _ ->
        let orig_type = Key_map.find key orig_types in
        let refi_type = mk_refi_type orig_type pred refi_reason in
        let change = Old_env.refine_expr key loc refi_type orig_type in
        Changeset.add_refi change acc
    in
    Key_map.fold refine_with_pred preds Changeset.empty

  (* Directly copied from env.ml, needed because it calls refine_with_preds *)
  let in_refined_env cx loc preds orig_types f =
    let oldset = Changeset.Global.clear () in
    let orig_env = peek_env () in
    let new_env = clone_env orig_env in
    update_env loc new_env;
    let _ = refine_with_preds cx loc preds orig_types in
    let result = f () in
    let newset = Changeset.Global.merge oldset in
    merge_env cx loc (orig_env, orig_env, new_env) newset;
    update_env loc orig_env;
    result

  let new_env = true

  let discriminant_after_negated_cases cx switch_loc refinement_key_opt _discriminant =
    let reason_desc =
      match refinement_key_opt with
      | None -> RCustom "discriminant of switch"
      | Some refinement_key -> Key.reason_desc refinement_key
    in
    try Some (read_entry ~for_type:false cx switch_loc (mk_reason reason_desc switch_loc)) with
    | LocEnvEntryNotFound _ -> None

  let init_import ~lookup_mode:_ cx _name loc t = set_env_entry ~use_op:unknown_use cx t loc
end
