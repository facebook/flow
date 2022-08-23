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
module EnvMap = Env_api.EnvMap

module LookupMode = struct
  type t =
    | ForValue
    | ForType
    | ForTypeof
end

open LookupMode

let get_global_value_type cx name reason =
  match Context.global_value_cache_find_opt cx name with
  | Some t -> t
  | None ->
    let t = Flow_js.get_builtin cx name reason in
    Context.add_global_value_cache_entry cx name t;
    t

let get_class_entries cx =
  let { Loc_env.class_stack; class_bindings; _ } = Context.environment cx in
  Base.List.fold
    ~f:(fun lst l ->
      ALocMap.find_opt l class_bindings |> Base.Option.value_map ~f:(fun c -> c :: lst) ~default:lst)
    ~init:[]
    class_stack

let set_scope_kind cx k =
  let env = Context.environment cx in
  let old = env.Loc_env.scope_kind in
  Context.set_environment cx { env with Loc_env.scope_kind = k };
  old

let in_class_scope cx loc f =
  let ({ Loc_env.class_stack; _ } as env) = Context.environment cx in
  Context.set_environment cx { env with Loc_env.class_stack = loc :: class_stack };
  let res = f () in
  let env = Context.environment cx in
  Context.set_environment cx { env with Loc_env.class_stack };
  res

let pop_var_scope cx kind =
  let (_ : Scope.var_scope_kind) = set_scope_kind cx kind in
  ()

let push_var_scope cx scope =
  let kind =
    match scope.Scope.kind with
    | Scope.VarScope kind -> kind
    | _ -> Utils_js.assert_false "push_var_scope on non-var scope"
  in
  set_scope_kind cx kind

let is_var_kind cx k = (Context.environment cx).Loc_env.scope_kind = k

let in_predicate_scope cx = is_var_kind cx Scope.Predicate

let in_async_scope cx = is_var_kind cx Scope.Async || is_var_kind cx Scope.AsyncGenerator

let var_scope_kind cx = (Context.environment cx).Loc_env.scope_kind

let in_global_scope cx = is_var_kind cx Scope.Global

let in_toplevel_scope cx = is_var_kind cx Scope.Module

let is_provider cx id_loc =
  let { Loc_env.var_info = { Env_api.providers; _ }; _ } = Context.environment cx in
  Env_api.Provider_api.is_provider providers id_loc

let this_type_params = ref ALocMap.empty

let valid_declaration_check cx name loc =
  let { Loc_env.var_info = { Env_api.scopes = info; ssa_values = values; providers; _ }; _ } =
    Context.environment cx
  in
  let error null_write =
    let null_write =
      Base.Option.map
        ~f:(fun null_loc -> Error_message.{ null_loc; initialized = ALoc.equal loc null_loc })
        null_write
    in
    Flow_js.add_output
      cx
      Error_message.(
        EInvalidDeclaration { declaration = mk_reason (RIdentifier name) loc; null_write }
      )
  in
  match Invalidation_api.declaration_validity info values providers loc with
  | Invalidation_api.Valid -> ()
  | Invalidation_api.NotWritten -> error None
  | Invalidation_api.NullWritten null_loc -> error (Some null_loc)

(* We don't want the new-env to throw if we encounter some new case in the wild for which we did
 * not adequately prepare. Instead, we return `any` in prod mode, but still crash in build mode.
 *)
let with_debug_exn cx loc f =
  try f () with
  | exn ->
    let exn = Exception.wrap exn in
    if Build_mode.dev then
      Exception.reraise exn
    else (
      Flow_js_utils.add_output cx (Error_message.EInternal (loc, Error_message.MissingEnvWrite loc));
      AnyT.at (AnyError None) loc
    )

let t_option_value_exn cx loc t = with_debug_exn cx loc (fun () -> Base.Option.value_exn t)

(************************)
(* Helpers **************)
(************************)

let check_readable cx kind loc =
  match (Context.env_mode cx, Context.current_phase cx <> Context.InitLib) with
  | (Options.(SSAEnv Enforced), true) ->
    let ({ Loc_env.under_resolution; var_info; _ } as env) = Context.environment cx in
    begin
      match Loc_env.find_write env kind loc with
      | None ->
        (match EnvMap.find_opt (kind, loc) var_info.Env_api.env_entries with
        | Some Env_api.NonAssigningWrite -> ()
        | _ -> Flow_js_utils.add_output cx Error_message.(EInternal (loc, MissingEnvWrite loc)))
      | Some (OpenT (_, id)) ->
        if not (Loc_env.is_readable env kind loc) then
          Flow_js_utils.add_output cx Error_message.(EInternal (loc, ReadOfUnreachedTvar kind))
        else if not (Env_api.EnvSet.mem (kind, loc) under_resolution) then begin
          match Context.find_graph cx id with
          | Type.Constraint.FullyResolved _ -> ()
          | _ ->
            Flow_js_utils.add_output cx Error_message.(EInternal (loc, ReadOfUnresolvedTvar kind))
        end
      | Some t -> assert_false ("Expect only OpenTs in env, instead we have " ^ Debug_js.dump_t cx t)
    end
  | _ -> ()

let record_expression_type_if_needed cx kind loc t =
  let env = Context.environment cx in
  match (Loc_env.find_write env kind loc, Context.env_mode cx) with
  | (_, Options.(SSAEnv (Reordered | Enforced)))
  (* Fully resolved env doesn't need to write here *)
  | (None, _) ->
    ()
  | (Some w, _) ->
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      (lazy [spf "recording expression at location %s" (Reason.string_of_aloc loc)]);
    Flow_js.unify cx ~use_op:unknown_use t w;
    begin
      match kind with
      | Env_api.(OrdinaryNameLoc | ExpressionLoc) ->
        let env' = Loc_env.update_reason env kind loc (TypeUtil.reason_of_t t) in
        Context.set_environment cx env'
      | _ -> ()
    end

let find_var_opt { Env_api.env_values; _ } loc =
  match ALocMap.find_opt loc env_values with
  | Some x -> Ok x
  | None -> Error loc

let find_refi { Env_api.refinement_of_id; _ } = refinement_of_id

let find_providers { Env_api.providers; _ } loc =
  Env_api.Provider_api.providers_of_def providers loc
  |> Base.Option.value_map ~f:(fun { Env_api.Provider_api.providers; _ } -> providers) ~default:[]
  |> Base.List.map ~f:(fun { Env_api.Provider_api.reason; _ } -> Reason.aloc_of_reason reason)

let is_def_loc_annotated { Env_api.providers; _ } loc =
  let providers = Env_api.Provider_api.providers_of_def providers loc in
  match providers with
  | Some { Env_api.Provider_api.state = Find_providers.AnnotatedVar _; _ } -> true
  | _ -> false

let is_def_loc_predicate_function { Env_api.providers; _ } loc =
  let providers = Env_api.Provider_api.providers_of_def providers loc in
  match providers with
  | Some { Env_api.Provider_api.state = Find_providers.AnnotatedVar { predicate }; _ } -> predicate
  | _ -> false

let provider_type_for_def_loc ?(intersect = false) cx env def_loc =
  let { Loc_env.var_info; _ } = env in
  let providers =
    find_providers var_info def_loc
    |> Base.List.filter_map ~f:(fun loc ->
           check_readable cx Env_api.OrdinaryNameLoc loc;
           match Loc_env.find_ordinary_write env loc with
           | Some w -> Some w
           | None ->
             (match EnvMap.find_opt_ordinary loc var_info.Env_api.env_entries with
             | None
             | Some Env_api.NonAssigningWrite ->
               None
             | _ ->
               assert_false
                 (spf
                    "Missing provider write at %s for %s"
                    (Reason.string_of_aloc loc)
                    (Reason.string_of_aloc def_loc)
                 ))
       )
  in
  match providers with
  | [] -> MixedT.make (mk_reason (RCustom "no providers") def_loc) (Trust.bogus_trust ())
  | [t] -> t
  | t1 :: t2 :: ts when intersect ->
    IntersectionT (mk_reason (RCustom "providers") def_loc, InterRep.make t1 t2 ts)
  | t1 :: t2 :: ts -> UnionT (mk_reason (RCustom "providers") def_loc, UnionRep.make t1 t2 ts)

(*************)
(*  Reading  *)
(*************)

(** Computes the phi type for a node given all its lower bounds
 *  Currently, this just produces a new type variable with the types of
 *  all the incoming writes as lower bounds. In the future, however, we
 *  may want to compute a more specific least upper bound for these writes.
 *)
let phi cx reason ts =
  match ts with
  | [t] -> t
  | _ ->
    Tvar.mk_where cx reason (fun tvar -> Base.List.iter ts ~f:(fun t -> Flow_js.flow_t cx (t, tvar)))

let rec predicate_of_refinement cx =
  Env_api.Refi.(
    function
    | AndR (r1, r2) -> AndP (predicate_of_refinement cx r1, predicate_of_refinement cx r2)
    | OrR (r1, r2) -> OrP (predicate_of_refinement cx r1, predicate_of_refinement cx r2)
    | NotR r -> NotP (predicate_of_refinement cx r)
    | TruthyR -> ExistsP
    | NullR -> NullP
    | UndefinedR -> VoidP
    | MaybeR -> MaybeP
    | InstanceOfR (loc, _) ->
      let env = Context.environment cx in
      (* Instanceof refinements store the loc they check against, which is a read in the env *)
      let reason = mk_reason (RCustom "RHS of `instanceof` operator") loc in
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        ( lazy
          [spf "reading from location %s (in instanceof refinement)" (Reason.string_of_aloc loc)]
          );
      check_readable cx Env_api.ExpressionLoc loc;
      let t = t_option_value_exn cx loc (Loc_env.find_write env Env_api.ExpressionLoc loc) in
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
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "reading from location %s (in sentinel refinement)" (Reason.string_of_aloc loc)]);
      check_readable cx Env_api.ExpressionLoc loc;
      let other_t = t_option_value_exn cx loc (Loc_env.find_write env Env_api.ExpressionLoc loc) in
      LeftP (SentinelProp prop, other_t)
    | LatentR { func = (func_loc, _); index } ->
      (* Latent refinements store the loc of the callee, which is a read in the env *)
      let reason = mk_reason (RCustom "Function call") func_loc in
      let t = read_entry_exn ~lookup_mode:ForValue cx func_loc reason in
      LatentP (t, index)
    | PropExistsR { propname; loc } ->
      PropExistsP (propname, mk_reason (RProperty (Some (OrdinaryName propname))) loc)
  )

and refine cx reason loc refi t =
  Base.Option.value_map
    ~f:(fun predicate ->
      let predicate = predicate |> snd |> predicate_of_refinement cx in
      let reason = mk_reason (RRefined (desc_of_reason reason)) loc in
      Tvar.mk_no_wrap_where cx reason (fun tvar -> Flow_js.flow cx (t, PredicateT (predicate, tvar))))
    ~default:t
    refi

and type_of_state ~lookup_mode cx env loc reason write_locs val_id refi =
  let { Loc_env.var_info; _ } = env in
  let rec type_of_state states val_id refi =
    let t =
      lazy
        (Base.List.map
           ~f:(fun entry ->
             match (entry, lookup_mode) with
             | (Env_api.Undefined reason, _)
             | (Env_api.Uninitialized reason, _) ->
               Type.(VoidT.make reason |> with_trust Trust.bogus_trust)
             | (Env_api.Number reason, _) -> Type.(NumT.make reason |> with_trust Trust.bogus_trust)
             | (Env_api.DeclaredFunction loc, _) ->
               provider_type_for_def_loc ~intersect:true cx env loc
             | (Env_api.Undeclared (_name, def_loc), (ForType | ForTypeof)) ->
               check_readable cx Env_api.OrdinaryNameLoc def_loc;
               t_option_value_exn cx def_loc (Loc_env.find_ordinary_write env def_loc)
             | (Env_api.Undeclared (name, def_loc), ForValue) ->
               Flow_js.add_output
                 cx
                 Error_message.(
                   EBindingError (EReferencedBeforeDeclaration, loc, OrdinaryName name, def_loc)
                 );
               Type.(AnyT.make (AnyError None) reason)
             | (Env_api.UndeclaredClass { def; _ }, (ForType | ForTypeof)) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading %s from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.aloc_of_reason def |> Reason.string_of_aloc);
                 ];
               let loc = Reason.aloc_of_reason def in
               check_readable cx Env_api.OrdinaryNameLoc loc;
               t_option_value_exn cx loc (Loc_env.find_ordinary_write env loc)
             | (Env_api.UndeclaredClass { name; def }, _) ->
               let def_loc = aloc_of_reason def in
               Flow_js.add_output
                 cx
                 Error_message.(
                   EBindingError (EReferencedBeforeDeclaration, loc, OrdinaryName name, def_loc)
                 );
               Type.(AnyT.make (AnyError None) reason)
             | (Env_api.With_ALoc.EmptyArray { reason; arr_providers = _ }, _)
             | (Env_api.With_ALoc.Write reason, _) ->
               Debug_js.Verbose.print_if_verbose_lazy
                 cx
                 ( lazy
                   [
                     spf
                       "reading %s from location %s"
                       (Reason.string_of_aloc loc)
                       (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                   ]
                   );
               let loc = Reason.aloc_of_reason reason in
               check_readable cx Env_api.OrdinaryNameLoc loc;
               t_option_value_exn cx loc (Loc_env.find_ordinary_write env loc)
             | (Env_api.With_ALoc.IllegalWrite reason, _) ->
               Debug_js.Verbose.print_if_verbose_lazy
                 cx
                 ( lazy
                   [
                     spf
                       "reading %s from illegal write location %s"
                       (Reason.string_of_aloc loc)
                       (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                   ]
                   );
               Type.(AnyT.make (AnyError None) reason)
             | (Env_api.With_ALoc.Refinement { refinement_id; writes; write_id }, _) ->
               find_refi var_info refinement_id |> Base.Option.some |> type_of_state writes write_id
             | (Env_api.With_ALoc.Global name, _) ->
               get_global_value_type cx (Reason.OrdinaryName name) reason
             | (Env_api.With_ALoc.GlobalThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading global this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Base.Option.value_exn
                 ( Reason.aloc_of_reason reason |> fun loc ->
                   check_readable cx Env_api.GlobalThisLoc loc;
                   Loc_env.find_write env Env_api.GlobalThisLoc loc
                 )
             | (Env_api.With_ALoc.IllegalThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading illegal this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Type.(AnyT.make (AnyError None) reason)
             | (Env_api.With_ALoc.FunctionThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading function this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Base.Option.value_exn
                 ( Reason.aloc_of_reason reason |> fun loc ->
                   check_readable cx Env_api.FunctionThisLoc loc;
                   Loc_env.find_write env Env_api.FunctionThisLoc loc
                 )
             | (Env_api.With_ALoc.ClassInstanceThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading instance this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Base.Option.value_exn
                 ( Reason.aloc_of_reason reason |> fun loc ->
                   check_readable cx Env_api.ClassInstanceThisLoc loc;
                   Loc_env.find_write env Env_api.ClassInstanceThisLoc loc
                 )
             | (Env_api.With_ALoc.ClassStaticThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading static this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Base.Option.value_exn
                 ( Reason.aloc_of_reason reason |> fun loc ->
                   check_readable cx Env_api.ClassStaticThisLoc loc;
                   Loc_env.find_write env Env_api.ClassStaticThisLoc loc
                 )
             | (Env_api.With_ALoc.ClassInstanceSuper reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading instance super(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Base.Option.value_exn
                 ( Reason.aloc_of_reason reason |> fun loc ->
                   check_readable cx Env_api.ClassInstanceSuperLoc loc;
                   Loc_env.find_write env Env_api.ClassInstanceSuperLoc loc
                 )
             | (Env_api.With_ALoc.ClassStaticSuper reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading %s from illegal write location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.aloc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Base.Option.value_exn
                 ( Reason.aloc_of_reason reason |> fun loc ->
                   check_readable cx Env_api.ClassStaticSuperLoc loc;
                   Loc_env.find_write env Env_api.ClassStaticSuperLoc loc
                 )
             | (Env_api.With_ALoc.Exports, _) ->
               let file_loc = Loc.{ none with source = Some (Context.file cx) } |> ALoc.of_loc in
               check_readable cx Env_api.GlobalExportsLoc file_loc;
               t_option_value_exn
                 cx
                 file_loc
                 (Loc_env.find_write env Env_api.GlobalExportsLoc file_loc)
             | (Env_api.With_ALoc.ModuleScoped _, _) -> Type.(AnyT.at AnnotatedAny loc)
             | (Env_api.With_ALoc.Unreachable loc, _) ->
               let reason = mk_reason (RCustom "unreachable value") loc in
               EmptyT.make reason (Trust.bogus_trust ())
             | (Env_api.With_ALoc.Projection loc, _) ->
               check_readable cx Env_api.OrdinaryNameLoc loc;
               t_option_value_exn cx loc (Loc_env.find_ordinary_write env loc))
           states
        |> phi cx reason
        )
    in
    let t =
      match val_id with
      | Some id ->
        let for_value = lookup_mode = ForValue in
        (match Context.env_cache_find_opt cx ~for_value id with
        | None ->
          let t = Lazy.force t in
          Context.add_env_cache_entry cx ~for_value id t;
          t
        | Some t -> t)
      | _ -> Lazy.force t
    in
    t |> refine cx reason loc refi
  in
  type_of_state write_locs val_id refi

and read_entry ~lookup_mode cx loc reason =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  match find_var_opt var_info loc with
  | Error loc -> Error loc
  | Ok { Env_api.def_loc; write_locs; val_kind; name; id } ->
    (match (val_kind, name, def_loc, lookup_mode) with
    | (Some (Env_api.Type { imported }), Some name, Some def_loc, (ForValue | ForTypeof)) ->
      Flow_js.add_output
        cx
        (Error_message.EBindingError
           (Error_message.ETypeInValuePosition { imported; name }, loc, OrdinaryName name, def_loc)
        );
      Ok (AnyT.at (AnyError None) loc)
    | _ -> Ok (type_of_state ~lookup_mode cx env loc reason write_locs id None))

and read_entry_exn ~lookup_mode cx loc reason =
  with_debug_exn cx loc (fun () ->
      match read_entry ~lookup_mode cx loc reason with
      | Error loc -> failwith (Utils_js.spf "LocEnvEntryNotFound %s" (Reason.string_of_aloc loc))
      | Ok x -> x
  )

let ref_entry_exn ~lookup_mode cx loc reason =
  let t = read_entry_exn ~lookup_mode cx loc reason in
  Flow_js.reposition cx loc t

let get_module_exports cx loc =
  let env = Context.environment cx in
  t_option_value_exn cx loc (Loc_env.find_write env Env_api.DeclareModuleExportsLoc loc)

let get_this_type_param_if_necessary ~otherwise name loc =
  if name = OrdinaryName "this" then
    match ALocMap.find_opt loc !this_type_params with
    | Some t -> t
    | None -> otherwise ()
  else
    otherwise ()

let get_refinement cx key loc =
  let reason = mk_reason (Key.reason_desc key) loc in
  match read_entry ~lookup_mode:ForValue cx loc reason with
  | Ok x -> Some (Flow_js.reposition cx loc x)
  | Error _ -> None

let get_var ?(lookup_mode = ForValue) cx name loc =
  ignore lookup_mode;
  let name = OrdinaryName name in
  get_this_type_param_if_necessary name loc ~otherwise:(fun () ->
      read_entry_exn ~lookup_mode cx loc (mk_reason (RIdentifier name) loc)
  )

let query_var ?(lookup_mode = ForValue) cx name ?desc loc =
  get_this_type_param_if_necessary name loc ~otherwise:(fun () ->
      let desc =
        match desc with
        | Some desc -> desc
        | None -> RIdentifier name
      in
      read_entry_exn ~lookup_mode cx loc (mk_reason desc loc)
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
        | Env_api.With_ALoc.Number _ -> true
        | Env_api.With_ALoc.DeclaredFunction _ -> true
        | Env_api.With_ALoc.Uninitialized _ -> true
        | Env_api.With_ALoc.UndeclaredClass _ -> true
        | Env_api.With_ALoc.EmptyArray _ -> true
        | Env_api.With_ALoc.Write _ -> true
        | Env_api.With_ALoc.IllegalWrite _ -> true
        | Env_api.With_ALoc.Unreachable _ -> true
        | Env_api.With_ALoc.Undeclared _ -> true
        | Env_api.With_ALoc.Refinement { refinement_id = _; writes; write_id = _ } ->
          local_def_exists writes
        | Env_api.With_ALoc.Projection _ -> true
        | Env_api.With_ALoc.GlobalThis _ -> true
        | Env_api.With_ALoc.IllegalThis _ -> true
        | Env_api.With_ALoc.FunctionThis _ -> true
        | Env_api.With_ALoc.ClassInstanceThis _ -> true
        | Env_api.With_ALoc.ClassStaticThis _ -> true
        | Env_api.With_ALoc.ClassInstanceSuper _ -> true
        | Env_api.With_ALoc.ClassStaticSuper _ -> true
        | Env_api.With_ALoc.Exports -> true
        | Env_api.With_ALoc.ModuleScoped _ -> true
        | Env_api.With_ALoc.Global _ -> false)
      states
    |> not
  in
  match find_var_opt var_info loc with
  | Ok { Env_api.def_loc = _; write_locs; val_kind = _; name = _; id = _ } ->
    local_def_exists write_locs
  | Error _ -> false

let local_scope_entry_exists cx loc name = not (is_global_var cx name loc)

let get_var_declared_type ?(lookup_mode = ForValue) ?(is_declared_function = false) cx name loc =
  match (name, lookup_mode) with
  | ((OrdinaryName _ | InternalModuleName _), ForType)
  | (InternalModuleName _, ForValue) ->
    let env = Context.environment cx in
    check_readable cx Env_api.OrdinaryNameLoc loc;
    (match Loc_env.find_ordinary_write env loc with
    | Some t -> t
    | None ->
      Flow_js_utils.add_output cx (Error_message.EInternal (loc, Error_message.MissingEnvWrite loc));
      Type.(AnyT.at (AnyError None) loc))
  | _ ->
    let env = Context.environment cx in
    provider_type_for_def_loc cx env ~intersect:is_declared_function loc

let constraining_type ~default cx _name loc =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  match EnvMap.find_opt_ordinary loc var_info.Env_api.env_entries with
  | Some Env_api.NonAssigningWrite -> default
  | _ ->
    let providers =
      find_providers var_info loc
      |> Base.List.map ~f:(fun loc ->
             check_readable cx Env_api.OrdinaryNameLoc loc;
             Loc_env.find_ordinary_write env loc
         )
      |> Base.Option.all
    in
    (match providers with
    | None
    | Some [] ->
      default
    | Some [t] -> t
    | Some (t1 :: t2 :: ts) -> UnionT (mk_reason (RCustom "providers") loc, UnionRep.make t1 t2 ts))

(*************)
(*  Writing  *)
(*************)

let set_expr cx _key loc ~refined ~original:_ =
  let env = Context.environment cx in
  Debug_js.Verbose.print_if_verbose cx [spf "set expr at location %s" (Reason.string_of_aloc loc)];
  match (Loc_env.find_ordinary_write env loc, Context.env_mode cx) with
  | (_, Options.(SSAEnv (Reordered | Enforced)))
  (* Fully resolved env doesn't need to write here *)
  | (None, _) ->
    (* As below, this entry is empty if the refinement is never read from *)
    ()
  | (Some w, _) -> Flow_js.unify cx ~use_op:unknown_use refined w

(* Unifies `t` with the entry in the loc_env's map. This allows it to be looked up for Write
 * entries reported by the name_resolver as well as providers for the provider analysis *)
let unify_write_entry cx ~use_op t def_loc_type loc =
  let env = Context.environment cx in
  Debug_js.Verbose.print_if_verbose
    cx
    [spf "writing to %s %s" (Env_api.show_def_loc_type def_loc_type) (Reason.string_of_aloc loc)];
  check_readable cx def_loc_type loc;
  match Loc_env.find_write env def_loc_type loc with
  | None ->
    (* If we don't see a spot for this write, it's because it's never read from. *)
    ()
  | Some w -> Flow_js.unify cx ~use_op w t

(* Subtypes the given type against the providers for a def loc. Should be used on assignments to
 * non-import value bindings *)
let subtype_against_providers cx ~use_op ?potential_global_name t loc =
  let ({ Loc_env.var_info = { Env_api.providers; scopes; _ } as var_info; _ } as env) =
    Context.environment cx
  in
  (* We only perform a subtyping check if this is an assigning write. We call
   * writes to immutable bindings non-assigning writes. For example:
   * const x: number = 3;
   * x = 'string';
   *
   * Since the x = 'string' doesn't actually assign a value to x, we should
   * not perform a subtyping check and a second error saying string is incompatible
   * with number. We should only emit an error saying that a const cannot be reassigned. *)
  match EnvMap.find_opt_ordinary loc var_info.Env_api.env_entries with
  | Some Env_api.NonAssigningWrite -> ()
  | Some (Env_api.GlobalWrite _) ->
    if is_provider cx loc then
      Base.Option.iter potential_global_name ~f:(fun name ->
          let name = Reason.OrdinaryName name in
          ignore @@ get_global_value_type cx name (mk_reason (RIdentifier name) loc)
      )
  | _ ->
    if not (is_provider cx loc) then
      let general = provider_type_for_def_loc cx env loc in
      if is_def_loc_annotated var_info loc then
        Flow_js.flow cx (t, UseT (use_op, general))
      else
        let use_op =
          match Scope_api.With_ALoc.(def_of_use_opt scopes loc) with
          | Some { Scope_api.With_ALoc.Def.locs = (declaration, _); actual_name = name; _ } ->
            let provider_locs =
              Base.Option.value_map
                ~f:(fun { Env_api.Provider_api.providers; _ } -> providers)
                ~default:[]
                (Env_api.Provider_api.providers_of_def providers loc)
            in
            Frame
              ( ConstrainedAssignment
                  {
                    name;
                    declaration;
                    providers =
                      Base.List.map
                        ~f:(fun { Env_api.Provider_api.reason; _ } -> poly_loc_of_reason reason)
                        provider_locs;
                    array = false;
                  },
                use_op
              )
          | None -> use_op
        in
        Context.add_constrained_write cx (t, use_op, general)

let assign_env_value_entry cx ~use_op ?potential_global_name t loc =
  begin
    match Context.env_mode cx with
    | Options.(SSAEnv (Reordered | Enforced)) -> ()
    | _ -> unify_write_entry cx ~use_op t Env_api.OrdinaryNameLoc loc
  end;
  subtype_against_providers cx ~use_op ?potential_global_name t loc

(* Sanity check for predicate functions: If there are multiple declare function
 * providers, make sure none of them have a predicate. *)
let check_predicate_declare_function cx ~predicate name loc =
  let { Loc_env.var_info; _ } = Context.environment cx in
  match Env_api.Provider_api.providers_of_def var_info.Env_api.providers loc with
  (* This check is only relevant when there are multiple providers *)
  | Some
      { Env_api.Provider_api.providers = { Env_api.Provider_api.reason = def_reason; _ } :: _; _ }
    ->
    let def_loc = Reason.aloc_of_reason def_reason in
    let def_loc_is_pred = is_def_loc_predicate_function var_info def_loc in
    (* Raise an error for an overload (other than the first one) if:
     * - The first overload is a predicate function (`def_loc_is_pred`), or
     * - The current overload is a predicate function (`predicate`). *)
    if def_loc <> loc && (predicate || def_loc_is_pred) then
      Flow_js_utils.add_output
        cx
        (Error_message.EBindingError (Error_message.ENameAlreadyBound, loc, name, def_loc))
  | _ -> ()

let resolve_env_entry ~use_op ~update_reason cx t kind loc =
  unify_write_entry cx ~use_op t kind loc;
  let env = Context.environment cx in
  let env =
    if update_reason then
      Loc_env.update_reason env kind loc (TypeUtil.reason_of_t t)
    else
      env
  in
  Context.set_environment cx env

let subtype_entry cx ~use_op t loc =
  let env = Context.environment cx in
  check_readable cx Env_api.OrdinaryNameLoc loc;
  match Loc_env.find_ordinary_write env loc with
  | None ->
    (* If we don't see a spot for this write it is because the annotated
     * binding being looked up here is one that caused a redeclaration
     * error *)
    assert (
      EnvMap.find_opt_ordinary loc env.Loc_env.var_info.Env_api.env_entries
      = Some Env_api.NonAssigningWrite
    )
  | Some w -> Flow_js.flow cx (t, UseT (use_op, w))

(* init_entry is called on variable declarations (not assignments), and `t`
   is the RHS type. If the variable is annotated, we just need to check t against
   its type; but if it's not annotated, the RHS t becomes the variable's type. *)
let init_entry cx ~use_op ~has_anno:_ t loc =
  let { Loc_env.var_info; _ } = Context.environment cx in
  if is_def_loc_annotated var_info loc then
    subtype_entry cx ~use_op t loc
  else
    assign_env_value_entry cx ~use_op t loc

let set_var cx ~use_op name t loc =
  assign_env_value_entry cx ~use_op ~potential_global_name:name t loc

let set_module_exports cx _loc t =
  let env = Context.environment cx in
  unify_write_entry
    cx
    ~use_op:unknown_use
    t
    Env_api.DeclareModuleExportsLoc
    env.Loc_env.declare_module_exports_write_loc

let bind cx t ~kind loc =
  match Context.env_mode cx with
  | Options.(SSAEnv Enforced) -> ()
  | _ -> unify_write_entry cx ~use_op:Type.unknown_use t kind loc

let bind_var ?state:_ cx name t loc =
  valid_declaration_check cx (OrdinaryName name) loc;
  (* TODO: Vars can be bound multiple times and we need to make sure that the
   * annots are all compatible with each other. For that reason, we subtype
   * against providers when just binding a var *)
  assign_env_value_entry cx ~use_op:unknown_use (TypeUtil.type_t_of_annotated_or_inferred t) loc

let bind_let ?state:_ cx name t loc =
  valid_declaration_check cx (OrdinaryName name) loc;
  bind cx (TypeUtil.type_t_of_annotated_or_inferred t) ~kind:Env_api.OrdinaryNameLoc loc

let bind_function_this cx t loc =
  unify_write_entry cx ~use_op:Type.unknown_use t Env_api.FunctionThisLoc loc

let bind_class_instance_this cx t loc =
  unify_write_entry cx ~use_op:Type.unknown_use t Env_api.ClassInstanceThisLoc loc

let bind_class_static_this cx t loc =
  unify_write_entry cx ~use_op:Type.unknown_use t Env_api.ClassStaticThisLoc loc

let bind_class_instance_super cx t loc =
  unify_write_entry cx ~use_op:Type.unknown_use t Env_api.ClassInstanceSuperLoc loc

let bind_class_static_super cx t loc =
  unify_write_entry cx ~use_op:Type.unknown_use t Env_api.ClassStaticSuperLoc loc

let bind_implicit_let ?state:_ _kind cx name t loc =
  valid_declaration_check cx name loc;
  match (Context.env_mode cx, t) with
  | (Options.(SSAEnv Reordered), Annotated _) -> ()
  | _ -> bind cx (TypeUtil.type_t_of_annotated_or_inferred t) ~kind:Env_api.OrdinaryNameLoc loc

let bind_fun ?state:_ cx _name t loc = bind cx t ~kind:Env_api.OrdinaryNameLoc loc

let bind_implicit_const ?state:_ _ cx _ t loc =
  match (Context.env_mode cx, t) with
  | (Options.(SSAEnv Reordered), Annotated _) -> ()
  | _ -> bind cx (TypeUtil.type_t_of_annotated_or_inferred t) ~kind:Env_api.OrdinaryNameLoc loc

let bind_const ?state:_ cx _ t loc =
  bind cx (TypeUtil.type_t_of_annotated_or_inferred t) ~kind:Env_api.OrdinaryNameLoc loc

let bind_declare_fun cx ~predicate name t loc =
  check_predicate_declare_function cx ~predicate name loc;
  bind cx t ~kind:Env_api.OrdinaryNameLoc loc

let bind_this_tparam ~state:_ _cx t loc = this_type_params := ALocMap.add loc t !this_type_params

let bind_class_self_type cx class_loc _self class_t_internal =
  bind cx class_t_internal ~kind:Env_api.ClassSelfLoc class_loc

let init_var cx ~use_op _name ~has_anno t loc = init_entry ~has_anno cx ~use_op t loc

let init_let cx ~use_op _name ~has_anno t loc = init_entry ~has_anno cx ~use_op t loc

let init_implicit_let _kind cx ~use_op _name ~has_anno t loc = init_entry ~has_anno cx ~use_op t loc

let init_fun cx ~use_op _name t loc = assign_env_value_entry cx ~use_op t loc

let init_const cx ~use_op _name ~has_anno t loc = init_entry ~has_anno cx ~use_op t loc

let init_implicit_const _kind cx ~use_op _name ~has_anno t loc =
  init_entry ~has_anno cx ~use_op t loc

let unify_declared_type ?lookup_mode:_ ?is_func:_ cx _name loc t =
  match Context.env_mode cx with
  | Options.(SSAEnv (Reordered | Enforced)) -> ()
  | _ -> unify_write_entry cx ~use_op:unknown_use t Env_api.OrdinaryNameLoc loc

let read_declared_type ?lookup_mode:_ ?is_func:_ cx _name reason loc =
  Tvar.mk_where cx reason (fun t ->
      unify_write_entry cx ~use_op:unknown_use t Env_api.OrdinaryNameLoc loc
  )

let unify_declared_fun_type cx _name loc t =
  match Context.env_mode cx with
  | Options.(SSAEnv (Reordered | Enforced)) -> ()
  | _ -> unify_write_entry cx ~use_op:unknown_use t Env_api.OrdinaryNameLoc loc

(************************)
(* Variable Declaration *)
(************************)
let init_env ?exclude_syms:_ cx program_loc scope =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  let initialize_entry def_loc_type loc env_entry env =
    let env' =
      match env_entry with
      | Env_api.AssigningWrite reason
      | Env_api.GlobalWrite reason ->
        let t = Tvar.mk cx reason in
        Loc_env.initialize env def_loc_type loc t
      | Env_api.NonAssigningWrite ->
        if is_provider cx loc then
          (* If an illegal write is considered as a provider, we still need to give it a
             slot to prevent crashing in code that queries provider types. *)
          let reason = mk_reason (RCustom "non-assigning provider") loc in
          let t =
            Tvar.mk_no_wrap_where cx reason (fun (_, id) ->
                Flow_js.resolve_id cx id (AnyT.error reason)
            )
          in
          Loc_env.initialize env def_loc_type loc t
        else
          env
    in
    Context.set_environment cx env';
    env'
  in

  let env =
    EnvMap.fold
      (fun (def_loc_type, loc) env_entry env ->
        (* Array providers must be initialized first *)
        match def_loc_type with
        | Env_api.ArrayProviderLoc -> initialize_entry def_loc_type loc env_entry env
        | _ -> env)
      var_info.Env_api.env_entries
      env
    |> EnvMap.fold
         (fun (def_loc_type, loc) env_entry env ->
           match def_loc_type with
           | Env_api.ArrayProviderLoc -> env
           | _ -> initialize_entry def_loc_type loc env_entry env)
         var_info.Env_api.env_entries
  in
  let initialize_this () =
    let t = ObjProtoT (mk_reason (RCustom "global object") program_loc) in
    let w = Base.Option.value_exn (Loc_env.find_write env Env_api.GlobalThisLoc program_loc) in
    let (_, id) = Type.open_tvar w in
    Flow_js.resolve_id cx id t
  in
  initialize_this ();
  let kind =
    match scope.Scope.kind with
    | Scope.VarScope kind -> kind
    | _ -> failwith "Unexpected lexical scope"
  in
  let env =
    {
      env with
      Loc_env.scope_kind = kind;
      readable =
        Env_api.EnvSet.(
          empty
          |> add (Env_api.GlobalThisLoc, program_loc)
          |> add
               ( Env_api.GlobalExportsLoc,
                 Loc.{ none with source = Some (Context.file cx) } |> ALoc.of_loc
               )
        );
    }
  in
  Context.set_environment cx env

let discriminant_after_negated_cases cx switch_loc refinement_key_opt _discriminant =
  let reason_desc =
    match refinement_key_opt with
    | None -> RCustom "discriminant of switch"
    | Some refinement_key -> Key.reason_desc refinement_key
  in
  match read_entry ~lookup_mode:ForValue cx switch_loc (mk_reason reason_desc switch_loc) with
  | Ok t -> Some t
  | Error _ -> None

let init_import ~lookup_mode:_ cx _name loc t =
  match Context.env_mode cx with
  | Options.(SSAEnv (Reordered | Enforced)) -> ()
  | _ -> unify_write_entry ~use_op:unknown_use cx t Env_api.OrdinaryNameLoc loc

let get_next cx loc =
  let name = InternalName "next" in
  read_entry_exn ~lookup_mode:ForValue cx loc (mk_reason (RIdentifier name) loc)

let init_class_self_type cx loc _reason =
  let env = Context.environment cx in
  check_readable cx Env_api.ClassSelfLoc loc;
  Base.Option.value_exn (Loc_env.find_write env Env_api.ClassSelfLoc loc)

let init_declare_module_synthetic_module_exports cx ~export_type loc reason _module_scope =
  let env = Context.environment cx in
  let module_toplevel_members =
    ALocMap.find loc env.Loc_env.var_info.Env_api.module_toplevel_members
  in
  match Context.module_kind cx with
  | Module_info.ES _ -> ()
  | Module_info.CJS clobbered ->
    let () =
      match clobbered with
      | Some _ -> ()
      | None ->
        let props =
          Base.List.fold
            module_toplevel_members
            ~init:NameUtils.Map.empty
            ~f:(fun acc (name, { Env_api.write_locs; val_kind; id; _ }) ->
              match val_kind with
              | Some Env_api.Value ->
                let t = type_of_state ~lookup_mode:ForValue cx env loc reason write_locs id None in
                Properties.add_field name Polarity.Positive (Some loc) t acc
              | _ -> acc
          )
        in
        let proto = ObjProtoT reason in
        let t = Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~props proto in
        set_module_exports cx loc t
    in
    Base.List.iter
      module_toplevel_members
      ~f:(fun (name, { Env_api.write_locs; val_kind; id; _ }) ->
        match val_kind with
        | Some (Env_api.Type { imported = false }) ->
          let t = type_of_state ~lookup_mode:ForType cx env loc reason write_locs id None in
          export_type cx name (Some loc) t
        | _ -> ()
    )

let init_builtins_from_libdef cx _module_scope =
  let env = Context.environment cx in
  let filename = Context.file cx in
  let read_loc = Loc.{ none with source = Some filename } |> ALoc.of_loc in
  let read_reason =
    let desc = Reason.(RModule (OrdinaryName (File_key.to_string filename))) in
    Reason.mk_reason desc read_loc
  in
  Base.List.map
    env.Loc_env.var_info.Env_api.toplevel_members
    ~f:(fun (name, { Env_api.write_locs; val_kind; id; _ }) ->
      let lookup_mode =
        match val_kind with
        | Some (Env_api.Type _) -> ForType
        | _ -> ForValue
      in
      let t = type_of_state ~lookup_mode cx env read_loc read_reason write_locs id None in
      Flow_js.set_builtin cx name t;
      name
  )
