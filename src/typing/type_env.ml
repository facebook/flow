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
module EnvSet = Env_api.EnvSet

module LookupMode = struct
  type t =
    | ForValue
    | ForType
    | ForTypeof
end

open LookupMode

let get_class_entries cx =
  let { Loc_env.class_stack; class_bindings; _ } = Context.environment cx in
  Base.List.fold
    ~f:(fun lst l ->
      ALocMap.find_opt l class_bindings |> Base.Option.value_map ~f:(fun c -> c :: lst) ~default:lst)
    ~init:[]
    class_stack

let has_hint cx loc =
  if Context.typing_mode cx <> Context.CheckingMode then
    false
  else
    let { Loc_env.hint_map; _ } = Context.environment cx in
    ALocMap.find_opt loc hint_map |> Base.Option.value_map ~f:fst ~default:false

let get_hint cx loc =
  if Context.typing_mode cx <> Context.CheckingMode then
    Type.hint_unavailable
  else
    let { Loc_env.hint_map; _ } = Context.environment cx in
    ALocMap.find_opt loc hint_map |> Base.Option.value ~default:Type.hint_unavailable

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

let is_var_kind cx k = (Context.environment cx).Loc_env.scope_kind = k

let in_async_scope cx = is_var_kind cx Name_def.Async || is_var_kind cx Name_def.AsyncGenerator

let var_scope_kind cx = (Context.environment cx).Loc_env.scope_kind

let in_global_scope cx = is_var_kind cx Name_def.Global

let in_toplevel_scope cx = is_var_kind cx Name_def.Module

let is_provider cx id_loc =
  let { Loc_env.var_info = { Env_api.providers; _ }; _ } = Context.environment cx in
  Env_api.Provider_api.is_provider providers id_loc

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

let with_debug_exn_error cx loc ~f ~error =
  try f () with
  | exn ->
    let exn = Exception.wrap exn in
    if Build_mode.dev then
      Exception.reraise exn
    else (
      Flow_js_utils.add_output cx (Error_message.EInternal (loc, Error_message.MissingEnvWrite loc));
      error ()
    )

let t_option_value_exn cx loc t =
  with_debug_exn cx loc (fun () ->
      match t with
      | Some t -> t
      | None ->
        failwith
          (Utils_js.spf "Missing location entry: %s" (ALoc.debug_to_string ~include_source:true loc))
  )

(************************)
(* Helpers **************)
(************************)

let checked_find_loc_env_write_opt cx kind loc : Type.t option =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  match
    (EnvMap.find_opt (kind, loc) var_info.Env_api.env_entries, Loc_env.find_write env kind loc)
  with
  | (Some Env_api.NonAssigningWrite, None) -> None
  | (Some Env_api.NonAssigningWrite, Some (Loc_env.TypeEntry { t; _ })) -> Some t
  | (_, None) ->
    Flow_js_utils.add_output cx Error_message.(EInternal (loc, MissingEnvWrite loc));
    None
  | (_, Some (Loc_env.TypeEntry { t; state = _ })) -> Some t

let checked_find_loc_env_write cx kind loc =
  checked_find_loc_env_write_opt cx kind loc |> t_option_value_exn cx loc

let find_var_opt { Env_api.env_values; _ } loc =
  match ALocMap.find_opt loc env_values with
  | Some x -> Ok x
  | None -> Error loc

let find_refi { Env_api.refinement_of_id; _ } = refinement_of_id

let find_providers { Env_api.providers; _ } loc =
  Env_api.Provider_api.providers_of_def providers loc
  |> Base.Option.value_map ~f:(fun { Env_api.Provider_api.providers; _ } -> providers) ~default:[]
  |> Base.List.map ~f:(fun { Env_api.Provider_api.reason; _ } -> Reason.loc_of_reason reason)

let is_def_loc_annotated { Env_api.providers; _ } loc =
  let providers = Env_api.Provider_api.providers_of_def providers loc in
  match providers with
  | Some { Env_api.Provider_api.state = Find_providers.AnnotatedVar _; _ } -> true
  | _ -> false

let provider_type_for_def_loc ?(intersect = false) cx env def_loc =
  let { Loc_env.var_info; _ } = env in
  let providers =
    find_providers var_info def_loc
    |> Base.List.filter_map ~f:(fun loc ->
           match checked_find_loc_env_write_opt cx Env_api.OrdinaryNameLoc loc with
           | Some w -> Some w
           | None ->
             (match EnvMap.find_opt_ordinary loc var_info.Env_api.env_entries with
             | None
             | Some Env_api.NonAssigningWrite ->
               None
             | _ ->
               Flow_js_utils.add_output
                 cx
                 Error_message.(
                   EInternal
                     ( loc,
                       EnvInvariant
                         (Env_api.Impossible
                            (spf
                               "Missing provider write at %s for %s"
                               (Reason.string_of_aloc loc)
                               (Reason.string_of_aloc def_loc)
                            )
                         )
                     )
                 );
               None)
       )
  in
  match providers with
  | [] -> MixedT.make (mk_reason (RCustom "no providers") def_loc)
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
    let tvar = Tvar.mk cx reason in
    let errs =
      Base.List.concat_map
        ~f:(function
          | Ok t ->
            Flow_js.flow_t cx (t, tvar);
            []
          | Error (t, errs) ->
            Flow_js.flow_t cx (t, tvar);
            Nel.to_list errs)
        ts
    in
    Tvar_resolver.resolve cx tvar;
    (match errs with
    | [] -> Ok tvar
    | hd :: tl -> Error (tvar, (hd, tl)))

let read_pred_func_info_exn cx loc =
  let f () = ALocMap.find loc (Context.environment cx).Loc_env.pred_func_map in
  let error () = lazy (unknown_use, loc, AnyT.at (AnyError None) loc, None, []) in
  with_debug_exn_error cx loc ~f ~error

(* Returns [true] iff the input type is potentially a predicate function. *)
let maybe_predicate_function =
  let simplify_callee cx func_t =
    let errors = Context.errors cx in
    let result =
      Context.run_and_rolled_back_cache cx (fun () ->
          let reason = TypeUtil.reason_of_t func_t in
          Tvar.mk_no_wrap_where cx reason (fun t ->
              let u =
                CallT
                  {
                    use_op = unknown_use;
                    reason;
                    call_action = ConcretizeCallee t;
                    return_hint = hint_unavailable;
                  }
              in
              Flow_js.flow cx (func_t, u)
          )
      )
    in
    Context.reset_errors cx errors;
    result
  in
  let on_ground t =
    match t with
    | AnyT _ -> false
    | DefT (_, FunT (_, { predicate; _ }))
    | DefT (_, PolyT { t_out = DefT (_, FunT (_, { predicate; _ })); _ }) ->
      Base.Option.is_some predicate
    | DefT _ -> false
    | _ -> true (* yes: safe option *)
  in
  let rec on_non_inter cx t =
    match t with
    | DefT _ -> on_ground t
    | _ ->
      t
      |> simplify_callee cx
      |> Context.find_resolved cx
      |> Base.Option.value_map ~f:on_ground ~default:true
  and on_concrete cx t =
    match t with
    | IntersectionT (_, rep) -> rep |> InterRep.members |> Base.List.exists ~f:(on_t cx)
    | t -> on_non_inter cx t
  and on_t cx t =
    let reason = TypeUtil.reason_of_t t in
    let ts = Flow_js.possible_concrete_types_for_inspection cx reason t in
    Base.List.exists ~f:(on_concrete cx) ts
  in
  on_t

let predicate_of_refinement cx =
  let open Env_api.Refi in
  let rec pred = function
    | AndR (r1, r2) ->
      (match (pred r1, pred r2) with
      | (Some p1, Some p2) -> Some (AndP (p1, p2))
      | (Some p, None)
      | (None, Some p) ->
        Some p
      | (None, None) -> None)
    | OrR (r1, r2) ->
      (match (pred r1, pred r2) with
      | (Some p1, Some p2) -> Some (OrP (p1, p2))
      | (None, _)
      | (_, None) ->
        None)
    | NotR r ->
      (match pred r with
      | None -> None
      | Some p -> Some (NotP p))
    | TruthyR -> Some ExistsP
    | NullR -> Some NullP
    | UndefinedR -> Some VoidP
    | MaybeR -> Some MaybeP
    | InstanceOfR (loc, _) ->
      (* Instanceof refinements store the loc they check against, which is a read in the env *)
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        ( lazy
          [spf "reading from location %s (in instanceof refinement)" (Reason.string_of_aloc loc)]
          );
      let t = checked_find_loc_env_write cx Env_api.ExpressionLoc loc in
      Type_operation_utils.TypeAssertions.assert_instanceof_rhs cx t;
      Some (BinaryP (InstanceofTest, t))
    | IsArrayR -> Some ArrP
    | BoolR loc -> Some (BoolP loc)
    | FunctionR -> Some FunP
    | NumberR loc -> Some (NumP loc)
    | BigIntR loc -> Some (BigIntP loc)
    | ObjectR -> Some ObjP
    | StringR loc -> Some (StrP loc)
    | SymbolR loc -> Some (SymbolP loc)
    | SingletonBoolR { loc; sense = _; lit } -> Some (SingletonBoolP (loc, lit))
    | SingletonStrR { loc; sense; lit } -> Some (SingletonStrP (loc, sense, lit))
    | SingletonNumR { loc; sense; lit } -> Some (SingletonNumP (loc, sense, lit))
    | SingletonBigIntR { loc; sense; lit } -> Some (SingletonBigIntP (loc, sense, lit))
    | SentinelR (prop, loc) ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "reading from location %s (in sentinel refinement)" (Reason.string_of_aloc loc)]);
      let other_t = checked_find_loc_env_write cx Env_api.ExpressionLoc loc in
      Some (BinaryP (SentinelProp prop, other_t))
    | LatentR { func = (func_loc, _); index; _ } ->
      let (lazy (_, _, t, _, _)) =
        ALocMap.find func_loc (Context.environment cx).Loc_env.pred_func_map
      in
      if maybe_predicate_function cx t then
        Some (LatentP (read_pred_func_info_exn cx func_loc, index))
      else
        None
    | PropNullishR { propname; loc } ->
      Some
        (NotP (PropNonMaybeP (propname, mk_reason (RProperty (Some (OrdinaryName propname))) loc)))
    | PropExistsR { propname; loc } ->
      Some (PropExistsP (propname, mk_reason (RProperty (Some (OrdinaryName propname))) loc))
  in
  pred

(* actual_source_read reports whether the environment entry read is actually meaningful in the
 * source code. e.g. foo.bar is an actual read, but a switch statement is not an actual read, but
 * it's a synthetic read generated in the env builder so we check other things. *)
let refine cx ~actual_source_read reason loc refi res =
  Base.Option.value_map
    ~f:(fun predicate ->
      let map_t t =
        let predicate = predicate |> snd |> predicate_of_refinement cx in
        let reason = mk_reason (RRefined (desc_of_reason reason)) loc in
        match predicate with
        | None -> t
        | Some predicate ->
          (match Predicate_kit.run_predicate_track_changes cx t predicate reason with
          | Predicate_kit.TypeUnchanged _t -> t
          | Predicate_kit.TypeChanged t ->
            if actual_source_read then Context.add_refined_location cx loc;
            t)
      in
      match res with
      | Ok t -> Ok (map_t t)
      | Error (t, errs) -> Error (map_t t, errs))
    ~default:res
    refi

let res_of_state ~lookup_mode ~val_kind cx env loc reason write_locs val_id refi =
  let { Loc_env.var_info; _ } = env in
  let find_write_exn kind reason =
    let loc = Reason.loc_of_reason reason in
    checked_find_loc_env_write cx kind loc
  in
  let rec res_of_state states val_id refi =
    let t =
      lazy
        (Base.List.map
           ~f:(fun entry ->
             match (entry, lookup_mode) with
             | (Env_api.Undefined reason, _)
             | (Env_api.Uninitialized reason, _) ->
               Ok Type.(VoidT.make reason)
             | (Env_api.Number reason, _) -> Ok Type.(NumT.make reason)
             | (Env_api.DeclaredFunction loc, _) ->
               Ok (provider_type_for_def_loc ~intersect:true cx env loc)
             | (Env_api.Undeclared (_name, def_loc), ForType) ->
               Ok (checked_find_loc_env_write cx Env_api.OrdinaryNameLoc def_loc)
             | (Env_api.Undeclared (name, def_loc), (ForValue | ForTypeof)) ->
               Error
                 ( Type.(AnyT.make (AnyError None) reason),
                   Nel.one (Env_api.ReferencedBeforeDeclaration { name; def_loc })
                 )
             | (Env_api.With_ALoc.EmptyArray { reason; arr_providers = _ }, _)
             | (Env_api.With_ALoc.Write reason, _) ->
               Debug_js.Verbose.print_if_verbose_lazy
                 cx
                 ( lazy
                   [
                     spf
                       "reading %s from location %s"
                       (Reason.string_of_aloc loc)
                       (Reason.loc_of_reason reason |> Reason.string_of_aloc);
                   ]
                   );
               Ok (find_write_exn Env_api.OrdinaryNameLoc reason)
             | (Env_api.With_ALoc.IllegalWrite reason, _) ->
               Debug_js.Verbose.print_if_verbose_lazy
                 cx
                 ( lazy
                   [
                     spf
                       "reading %s from illegal write location %s"
                       (Reason.string_of_aloc loc)
                       (Reason.loc_of_reason reason |> Reason.string_of_aloc);
                   ]
                   );
               Ok Type.(AnyT.make (AnyError None) reason)
             | (Env_api.With_ALoc.Refinement { refinement_id; writes; write_id }, _) ->
               find_refi var_info refinement_id |> Base.Option.some |> res_of_state writes write_id
             | (Env_api.With_ALoc.Global name, (ForValue | ForTypeof)) ->
               Flow_js_utils.lookup_builtin_value_result cx name reason
             | (Env_api.With_ALoc.Global name, ForType) ->
               Flow_js_utils.lookup_builtin_type_result cx name reason
             | (Env_api.With_ALoc.GlobalThis reason, _) -> Ok (ObjProtoT reason)
             | (Env_api.With_ALoc.IllegalThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading illegal this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.loc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Ok Type.(AnyT.make (AnyError None) reason)
             | (Env_api.With_ALoc.FunctionThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading function this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.loc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Ok (find_write_exn Env_api.FunctionThisLoc reason)
             | (Env_api.With_ALoc.ClassInstanceThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading instance this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.loc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Ok (find_write_exn Env_api.ClassInstanceThisLoc reason)
             | (Env_api.With_ALoc.ClassStaticThis reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading static this(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.loc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Ok (find_write_exn Env_api.ClassStaticThisLoc reason)
             | (Env_api.With_ALoc.ClassInstanceSuper reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading instance super(%s) from location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.loc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Ok (find_write_exn Env_api.ClassInstanceSuperLoc reason)
             | (Env_api.With_ALoc.ClassStaticSuper reason, _) ->
               Debug_js.Verbose.print_if_verbose
                 cx
                 [
                   spf
                     "reading %s from illegal write location %s"
                     (Reason.string_of_aloc loc)
                     (Reason.loc_of_reason reason |> Reason.string_of_aloc);
                 ];
               Ok (find_write_exn Env_api.ClassStaticSuperLoc reason)
             | (Env_api.With_ALoc.ModuleScoped _, _) -> Ok Type.(AnyT.at AnnotatedAny loc)
             | (Env_api.With_ALoc.Unreachable loc, _) ->
               let reason = mk_reason (RCustom "unreachable value") loc in
               Ok (EmptyT.make reason)
             | (Env_api.With_ALoc.Projection loc, _) ->
               Ok (checked_find_loc_env_write cx Env_api.OrdinaryNameLoc loc))
           states
        |> phi cx reason
        )
    in
    let t =
      match val_id with
      | Some id ->
        if Context.typing_mode cx <> Context.CheckingMode then
          Lazy.force t
        else
          let for_value =
            match lookup_mode with
            | ForValue
            | ForTypeof ->
              true
            | ForType -> false
          in
          (match Context.env_cache_find_opt cx ~for_value id with
          | None ->
            let t = Lazy.force t in
            Context.add_env_cache_entry cx ~for_value id t;
            t
          | Some t -> t)
      | _ -> Lazy.force t
    in
    (* When val_kind = Internal, the read is a synthetic read and it's internal to Flow.
     * For these reads, we will not highlight refined values. *)
    t |> refine cx ~actual_source_read:(val_kind <> Env_api.Internal) reason loc refi
  in
  match res_of_state write_locs val_id refi with
  | Ok t -> Ok (Tvar_resolver.resolved_t cx t)
  | Error (t, err) -> Error (Tvar_resolver.resolved_t cx t, err)

let type_of_state ~lookup_mode ~val_kind cx env loc reason write_locs val_id refi =
  res_of_state ~lookup_mode ~val_kind cx env loc reason write_locs val_id refi
  |> Flow_js_utils.apply_env_errors cx loc

let read_entry ~lookup_mode cx loc reason =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  match find_var_opt var_info loc with
  | Error loc -> Error loc
  | Ok { Env_api.def_loc; write_locs; val_kind; name; id } ->
    (match (val_kind, name, def_loc, lookup_mode) with
    | ( Env_api.Type { imported; type_only_namespace },
        Some name,
        Some def_loc,
        (ForValue | ForTypeof)
      ) ->
      Flow_js.add_output
        cx
        (Error_message.EBindingError
           ( Error_message.ETypeInValuePosition { imported; type_only_namespace; name },
             loc,
             OrdinaryName name,
             def_loc
           )
        );
      Ok (AnyT.at (AnyError None) loc)
    | _ ->
      let t = type_of_state ~lookup_mode ~val_kind cx env loc reason write_locs id None in
      Ok t)

let read_entry_exn ~lookup_mode cx loc reason =
  with_debug_exn cx loc (fun () ->
      match read_entry ~lookup_mode cx loc reason with
      | Error loc -> failwith (Utils_js.spf "LocEnvEntryNotFound %s" (Reason.string_of_aloc loc))
      | Ok x -> x
  )

let read_to_predicate cx var_info ({ Env_api.write_locs; _ }, _, _) =
  let predicates =
    Base.List.filter_map write_locs ~f:(function
        | Env_api.With_ALoc.Refinement { refinement_id; _ } ->
          find_refi var_info refinement_id |> snd |> predicate_of_refinement cx
        | _ -> None
        )
  in
  predicates
  |> Nel.of_list
  |> Base.Option.map ~f:(fun (p, rest) -> Base.List.fold rest ~init:p ~f:(fun acc p -> OrP (acc, p)))

let predicate_refinement_maps cx loc =
  let { Loc_env.var_info; _ } = Context.environment cx in
  let { Env_api.predicate_refinement_maps; _ } = var_info in
  let to_predicate_key_map map =
    map
    |> SMap.elements
    |> Base.List.filter_map ~f:(fun (name, read) ->
           read_to_predicate cx var_info read
           |> Base.Option.map ~f:(fun p -> ((OrdinaryName name, []), p))
       )
    |> Key_map.of_list
  in
  match ALocMap.find_opt loc predicate_refinement_maps with
  | None -> None
  | Some (expr_reason, p_map, n_map) ->
    Some (expr_reason, lazy (to_predicate_key_map p_map, to_predicate_key_map n_map))

let type_guard_at_return cx reason ~param_loc ~return_loc ~pos_write_locs ~neg_refi =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  let rec is_invalid (acc_result, acc_locs) write_loc =
    match write_loc with
    | Env_api.Write reason when ALoc.equal (Reason.loc_of_reason reason) param_loc ->
      (acc_result, acc_locs)
    | Env_api.Refinement { refinement_id = _; writes; write_id = _ } ->
      Base.List.fold_left ~init:(acc_result, acc_locs) writes ~f:is_invalid
    | Env_api.Write r -> (true, Reason.loc_of_reason r :: acc_locs)
    | _ ->
      (* These cases are unlikely. Not all of them provide an error location.
       * Just record the error without loc. *)
      (true, acc_locs)
  in
  let (is_invalid, invalid_writes) =
    Base.List.fold_left pos_write_locs ~init:(false, []) ~f:is_invalid
  in
  if is_invalid then
    Error invalid_writes
  else
    let lookup_mode = LookupMode.ForValue in
    let val_kind = Env_api.Internal in
    let t =
      type_of_state ~lookup_mode ~val_kind cx env return_loc reason pos_write_locs None None
    in
    Ok (t, read_to_predicate cx var_info neg_refi)

let ref_entry_exn ~lookup_mode cx loc reason =
  let t = read_entry_exn ~lookup_mode cx loc reason in
  Flow_js.reposition cx loc t

let find_write cx kind reason =
  let loc = Reason.loc_of_reason reason in
  match checked_find_loc_env_write_opt cx kind loc with
  | Some t -> t
  | None -> AnyT.error reason

let get_refinement cx key loc =
  let reason = mk_reason (Key.reason_desc key) loc in
  match read_entry ~lookup_mode:ForValue cx loc reason with
  | Ok x -> Some (Flow_js.reposition cx loc x)
  | Error _ -> None

let get_var ?(lookup_mode = ForValue) cx name loc =
  ignore lookup_mode;
  let name = OrdinaryName name in
  read_entry_exn ~lookup_mode cx loc (mk_reason (RIdentifier name) loc)

let query_var ?(lookup_mode = ForValue) cx name ?desc loc =
  let desc =
    match desc with
    | Some desc -> desc
    | None -> RIdentifier name
  in
  read_entry_exn ~lookup_mode cx loc (mk_reason desc loc)

let intrinsic_ref cx ?desc name loc =
  let desc =
    match desc with
    | Some desc -> desc
    | None -> RIdentifier name
  in
  let reason = mk_reason desc loc in
  let read =
    let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
    match find_var_opt var_info loc with
    | Error loc -> Error loc
    | Ok { Env_api.def_loc; write_locs; val_kind; name; id } ->
      (match (val_kind, name, def_loc) with
      | (Env_api.Type { imported = _; type_only_namespace = _ }, Some _, Some _)
      | (_, _, None) ->
        Error loc
      | (_, _, Some def_loc) ->
        let t = res_of_state ~lookup_mode:ForValue ~val_kind cx env loc reason write_locs id None in
        Ok (t, def_loc))
  in
  match read with
  | Error _ -> None
  | Ok ((Ok x | Error (x, _)), def_loc) -> Some (Flow_js.reposition cx loc x, def_loc)

let var_ref ?(lookup_mode = ForValue) cx ?desc name loc =
  let t = query_var ~lookup_mode cx name ?desc loc in
  Flow_js.reposition cx loc t

let sig_var_ref ?(lookup_mode = ForValue) cx ?desc name loc =
  let desc =
    match desc with
    | Some desc -> desc
    | None -> RIdentifier name
  in
  let reason = mk_reason desc loc in
  AnnotT (reason, query_var cx ~lookup_mode name loc, true)

let read_class_self_type cx loc =
  match checked_find_loc_env_write_opt cx Env_api.ClassSelfLoc loc with
  | Some t -> t
  | None ->
    (* When checked_find_loc_env_write_opt returns None, we are reading an unreachable entry *)
    EmptyT.why (mk_reason REmpty loc)

let is_global_var cx loc =
  let { Loc_env.var_info; _ } = Context.environment cx in
  match find_var_opt var_info loc with
  | Ok read -> Env_api.With_ALoc.is_global_var read
  | Error _ -> false

let local_scope_entry_exists cx loc = not (is_global_var cx loc)

let get_var_declared_type ?(lookup_mode = ForValue) ?(is_declared_function = false) cx name loc =
  match (name, lookup_mode) with
  | (OrdinaryName _, ForType) ->
    (match checked_find_loc_env_write_opt cx Env_api.OrdinaryNameLoc loc with
    | Some t -> t
    | None ->
      Flow_js_utils.add_output cx (Error_message.EInternal (loc, Error_message.MissingEnvWrite loc));
      Type.(AnyT.at (AnyError None) loc))
  | _ ->
    let env = Context.environment cx in
    provider_type_for_def_loc cx env ~intersect:is_declared_function loc

let constraining_type ~default cx name loc =
  let { Loc_env.var_info; _ } = Context.environment cx in
  match EnvMap.find_opt_ordinary loc var_info.Env_api.env_entries with
  | Some Env_api.NonAssigningWrite -> default
  | _ ->
    let providers =
      find_providers var_info loc
      |> Base.List.map ~f:(fun loc -> checked_find_loc_env_write_opt cx Env_api.OrdinaryNameLoc loc)
      |> Base.Option.all
    in
    (match providers with
    | None
    | Some [] ->
      default
    | Some [t] -> t
    | Some (t1 :: t2 :: ts) ->
      UnionT (mk_reason (RIdentifier (OrdinaryName name)) loc, UnionRep.make t1 t2 ts))

(*************)
(*  Writing  *)
(*************)

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
          if Base.Option.is_none (Flow_js_utils.lookup_builtin_value_opt cx name) then
            Flow_js_utils.add_output cx Error_message.(EBuiltinNameLookupFailed { loc; name })
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
                        ~f:(fun { Env_api.Provider_api.reason; _ } -> loc_of_reason reason)
                        provider_locs;
                  },
                use_op
              )
          | None -> use_op
        in
        Context.add_post_inference_subtyping_check cx t use_op general

let make_env_entries_under_resolution cx entries =
  let env = Context.environment cx in
  let update ((def_loc_kind, loc) as key) =
    match EnvMap.find_opt key env.Loc_env.types with
    | None -> ()
    | Some (Loc_env.TypeEntry { t; state }) ->
      let reason = TypeUtil.reason_of_t t in
      state :=
        lazy
          ( Flow_js_utils.add_output
              cx
              Error_message.(EInternal (loc, ForcedReadOfUnderResolutionTvar def_loc_kind));
            AnyT.error reason
          )
  in
  EnvSet.iter update entries

(* Resolve `t` with the entry in the loc_env's map. This allows it to be looked up for Write
 * entries reported by the name_resolver as well as providers for the provider analysis *)
let resolve_env_entry cx t kind loc =
  Debug_js.Verbose.print_if_verbose
    cx
    [spf "writing to %s %s" (Env_api.show_def_loc_type kind) (Reason.string_of_aloc loc)];
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  match
    (EnvMap.find_opt (kind, loc) var_info.Env_api.env_entries, Loc_env.find_write env kind loc)
  with
  | (Some Env_api.NonAssigningWrite, _) -> ()
  | (_, None) -> Flow_js_utils.add_output cx Error_message.(EInternal (loc, MissingEnvWrite loc))
  | (_, Some (Loc_env.TypeEntry { t = _; state })) ->
    Tvar_resolver.resolve cx t;
    state :=
      lazy
        (* Unwrap possible OpenT so that OpenT doesn't wrap another OpenT.
         * This has to be done lazily, so that we don't force tvars until we
         * resolved all entries in a component. *)
        (match t with
        | OpenT (r, id) -> Flow_js_utils.merge_tvar ~no_lowers:(fun _ r -> DefT (r, EmptyT)) cx r id
        | t -> t)

let subtype_entry cx ~use_op t loc =
  let env = Context.environment cx in
  match checked_find_loc_env_write_opt cx Env_api.OrdinaryNameLoc loc with
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
let init_entry cx ~use_op t loc =
  let { Loc_env.var_info; _ } = Context.environment cx in
  if is_def_loc_annotated var_info loc then subtype_entry cx ~use_op t loc

let set_var cx ~use_op name t loc =
  subtype_against_providers cx ~use_op ~potential_global_name:name t loc

let bind_function_param cx t loc = resolve_env_entry cx t Env_api.FunctionParamLoc loc

let bind_function_this cx t loc =
  if Context.typing_mode cx = Context.CheckingMode then
    resolve_env_entry cx t Env_api.FunctionThisLoc loc

let bind_class_instance_this cx t loc = resolve_env_entry cx t Env_api.ClassInstanceThisLoc loc

let bind_class_static_this cx t loc = resolve_env_entry cx t Env_api.ClassStaticThisLoc loc

let bind_class_instance_super cx t loc = resolve_env_entry cx t Env_api.ClassInstanceSuperLoc loc

let bind_class_static_super cx t loc = resolve_env_entry cx t Env_api.ClassStaticSuperLoc loc

let bind_class_self_type cx t loc = resolve_env_entry cx t Env_api.ClassSelfLoc loc

let init_var cx ~use_op t loc = init_entry cx ~use_op t loc

let init_let cx ~use_op t loc = init_entry cx ~use_op t loc

let init_implicit_let cx ~use_op t loc = init_entry cx ~use_op t loc

let init_const cx ~use_op t loc = init_entry cx ~use_op t loc

let init_implicit_const cx ~use_op t loc = init_entry cx ~use_op t loc

let read_declared_type cx reason loc =
  match checked_find_loc_env_write_opt cx Env_api.OrdinaryNameLoc loc with
  | Some t -> t
  | None -> EmptyT.make reason

(************************)
(* Variable Declaration *)
(************************)
let init_env cx toplevel_scope_kind =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  let initialize_entry def_loc_type loc env_entry env =
    let env' =
      match env_entry with
      | Env_api.AssigningWrite reason
      | Env_api.GlobalWrite reason ->
        let state =
          ref
            ( lazy
              ( Flow_js_utils.add_output
                  cx
                  Error_message.(EInternal (loc_of_reason reason, ReadOfUnreachedTvar def_loc_type));
                AnyT.error reason
              )
              )
        in
        let t =
          Tvar.mk_fully_resolved_lazy
            cx
            reason
            (* During initialization, all these lazy tvars are created, but not all of them are
             * ready for forcing. The ones that are ready for forcing will be separately added to
             * the list after each component resolution. *)
            ~force_post_component:false
            (lazy (Lazy.force !state))
        in
        Loc_env.initialize env def_loc_type loc (Loc_env.TypeEntry { t; state })
      | Env_api.NonAssigningWrite ->
        if is_provider cx loc then
          (* If an illegal write is considered as a provider, we still need to give it a
             slot to prevent crashing in code that queries provider types. *)
          let reason = mk_reason RAnyImplicit loc in
          let state = ref (lazy (AnyT.error reason)) in
          let t =
            Tvar.mk_fully_resolved_lazy
              cx
              reason
              ~force_post_component:false
              (lazy (Lazy.force !state))
          in
          Loc_env.initialize env def_loc_type loc (Loc_env.TypeEntry { t; state })
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
  let env = { env with Loc_env.scope_kind = toplevel_scope_kind } in
  Context.set_environment cx env

let discriminant_after_negated_cases cx switch_loc refinement_key_opt =
  let reason_desc =
    match refinement_key_opt with
    | None -> RCustom "discriminant of switch"
    | Some refinement_key -> Key.reason_desc refinement_key
  in
  match read_entry ~lookup_mode:ForValue cx switch_loc (mk_reason reason_desc switch_loc) with
  | Ok t -> Some t
  | Error _ -> None

let get_next cx loc =
  let name = InternalName "next" in
  read_entry_exn ~lookup_mode:ForValue cx loc (mk_reason (RIdentifier name) loc)
