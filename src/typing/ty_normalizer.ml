(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pervasives
open Utils_js
open Reason
open Loc_collections
module Env = Ty_normalizer_env
module T = Type
module VSet = ISet
module File_sig = File_sig.With_ALoc

(* The type normalizer converts infered types (of type `Type.t`) under a context
   cx to the simplified form of type `Ty.t`. It is called by various modules,
   e.g. type-at-pos, coverage, dump-types, and so is parameterized by a
   configuration struct, instantiated by the client.

   The type normalizer should only be used on types arising from "fully merged"
   contexts -- that is, contexts which have all dependencies copied in and
   constraints evaluated.
*)

(* Error reporting *)

type error_kind =
  | BadMethodType
  | BadBoundT
  | BadCallProp
  | BadClassT
  | BadThisClassT
  | BadPoly
  | BadTypeAlias
  | BadTypeApp
  | BadInlineInterfaceExtends
  | BadInternalT
  | BadInstanceT
  | BadEvalT
  | BadUse
  | ShadowTypeParam
  | UnsupportedTypeCtor
  | UnsupportedUseCtor
  | TypeTooBig
  | RecursionLimit

type error = error_kind * string

let error_kind_to_string = function
  | BadMethodType -> "Bad method type"
  | BadBoundT -> "Unbound type parameter"
  | BadCallProp -> "Bad call property"
  | BadClassT -> "Bad class"
  | BadThisClassT -> "Bad this class"
  | BadPoly -> "Bad polymorphic type"
  | BadTypeAlias -> "Bad type alias"
  | BadTypeApp -> "Bad type application"
  | BadInlineInterfaceExtends -> "Bad inline interface extends"
  | BadInternalT -> "Bad internal type"
  | BadInstanceT -> "Bad instance type"
  | BadEvalT -> "Bad eval"
  | BadUse -> "Bad use"
  | ShadowTypeParam -> "Shadowed type parameters"
  | UnsupportedTypeCtor -> "Unsupported type constructor"
  | UnsupportedUseCtor -> "Unsupported use constructor"
  | TypeTooBig -> "Type too big"
  | RecursionLimit -> "recursion limit"

let error_to_string (kind, msg) = spf "[%s] %s" (error_kind_to_string kind) msg

module NormalizerMonad : sig
  module State : sig
    type t

    val empty : t
  end

  val run_type :
    options:Env.options ->
    genv:Env.genv ->
    imported_names:Ty.imported_ident ALocMap.t ->
    tparams:(ALoc.t * string) list ->
    State.t ->
    Type.t ->
    (Ty.t, error) result * State.t

  val run_imports : options:Env.options -> genv:Env.genv -> Ty.imported_ident ALocMap.t
end = struct
  type id_key =
    | TVarKey of int
    | EvalKey of Type.Eval.id

  (* A cache for resolved OpenTs/EvalTs. We cache the result even when the output
   * is an error. This is mostly useful for the batch call (`from_types`). The
   * key to this map is the Type.tvar identifier or an Eval.id.
   *)
  module Cache = struct
    type t = {
      open_ts: (Ty.t, error) result IMap.t;
      eval_ts: (Ty.t, error) result Type.Eval.Map.t;
    }

    let empty = { open_ts = IMap.empty; eval_ts = Type.Eval.Map.empty }

    let find i c =
      match i with
      | TVarKey i -> IMap.find_opt i c.open_ts
      | EvalKey i -> Type.Eval.Map.find_opt i c.eval_ts

    let update i t c =
      match i with
      | TVarKey i -> { c with open_ts = IMap.add i t c.open_ts }
      | EvalKey i -> { c with eval_ts = Type.Eval.Map.add i t c.eval_ts }
  end

  module State = struct
    type t = {
      (* Source of fresh ints for creating new Ty.tvar's *)
      counter: int;
      cache: Cache.t;
      (* This set is useful for synthesizing recursive types. It holds the set
         of type variables that are encountered "free". We say that a type
         variable is free when it appears in the body of its own definition.
         The process of calculating free variables in a type could be
         implemented post-fact. The reason we prefer to keep this in the state
         instead is performance, since it trivializes the "check if variable
         appears free".
       *)
      free_tvars: VSet.t;
    }

    let empty = { counter = 0; cache = Cache.empty; free_tvars = VSet.empty }
  end

  include StateResult.Make (State)

  (* Monadic helper functions *)
  let mapM f xs = all (Base.List.map ~f xs)

  (* Each run of the monad gets assigned its own id. *)
  let run_id = ref 0

  (* Wrapper around 'run' that assigns a distinct id to every run of the monad. *)
  let run state f =
    let result = run state f in
    incr run_id;
    result

  let get_run_id () = !run_id

  let optMapM f = function
    | Some xs -> mapM f xs >>| Base.Option.return
    | None as y -> return y

  let optM f = function
    | Some x -> f x >>| Base.Option.return
    | None as y -> return y

  let _fstMapM f (x, y) = f x >>| mk_tuple_swapped y

  let sndMapM f (x, y) = f y >>| mk_tuple x

  let concat_fold_m f xs = mapM f xs >>| Base.List.concat

  let fresh_num =
    State.(
      let%bind st = get in
      let n = st.counter in
      let%map _ = put { st with counter = n + 1 } in
      n)

  let terr ~kind ?msg t =
    let t_str = Base.Option.map t ~f:(fun t -> spf "Raised on type: %s" (Type.string_of_ctor t)) in
    let msg = ListUtils.cat_maybes [msg; t_str] |> String.concat ", " in
    error (kind, msg)

  (* Update state *)

  let find_in_cache i =
    let open State in
    let%map state = get in
    Cache.find i state.cache

  let update_cache i t =
    let open State in
    let%bind state = get in
    put { state with cache = Cache.update i t state.cache }

  let add_to_free_tvars v =
    let open State in
    modify (fun state -> { state with free_tvars = VSet.add v state.free_tvars })

  (* Lookup a type parameter T in the current environment. There are three outcomes:
     1. T appears in env and for its first occurence locations match. This means it
        is not shadowed by another parameter with the same name. In this case
        return the type parameter.
     2. T appears in env but is not the first occurence. This means that some other
        type parameter shadows it. We split cases depending on the value of
        Config.opt_flag_shadowed_type_params:
        - true: flag a warning, since the type is not well-formed in this context.
        - false: return the type normally ignoring the warning.
     3. The type parameter is not in env. Do the default action.
  *)
  let lookup_tparam ~default env t tp_name tp_loc =
    let pred (loc, name) = name = tp_name && loc = tp_loc in
    match List.find_opt pred env.Env.tparams with
    | Some _ ->
      (* If we care about shadowing of type params, then flag an error *)
      if Env.flag_shadowed_type_params env then
        let shadow_pred (_, name) = name = tp_name in
        match List.find_opt shadow_pred env.Env.tparams with
        | Some (loc, _) when loc <> tp_loc -> terr ~kind:ShadowTypeParam (Some t)
        | Some _ -> return (Ty.Bound (tp_loc, tp_name))
        | None -> assert false
      else
        return (Ty.Bound (tp_loc, tp_name))
    | None -> default t

  (**************)
  (* Type ctors *)
  (**************)

  let generic_class name targs = Ty.mk_generic_class name targs

  let generic_interface name targs = Ty.mk_generic_interface name targs

  let generic_talias name targs = Ty.mk_generic_talias name targs

  let builtin_t name = generic_talias (Ty.builtin_symbol name) None

  let generic_builtin_t name ts = generic_talias (Ty.builtin_symbol name) (Some ts)

  let empty_type = Ty.Bot Ty.EmptyType

  let empty_matching_prop_t = Ty.Bot Ty.EmptyMatchingPropT

  let mk_empty bot_kind =
    match bot_kind with
    | Ty.EmptyType -> empty_type
    | Ty.EmptyMatchingPropT -> empty_matching_prop_t
    | Ty.EmptyTypeDestructorTriggerT _
    | Ty.NoLowerWithUpper _ ->
      Ty.Bot bot_kind

  (*********************)
  (* Recursive types   *)
  (*********************)

  (* There are three phases in resolving a type variable:

     A. UNSEEN: Type variable has not been seen yet. It does not appear in the
        type cache.

     B. UNDER RESOLUTION: Variable has been seen at least once and it is set to
        "under resolution". It appears in the type_cache as a mapping from
        the original `Type.tvar` to a fresh `Ty.tvar`. This binding is important
        for termination purposes as well as determining if the variable appears
        free in a type context.

     C. RESOLVED: All lower bounds of the variable have been normalized and so
        the variable is considered resolved. The cache is updated with the final
        type or an error message.

  *)

  module Recursive = struct
    (* Helper functions *)

    (* Replace a recursive type variable r with a symbol sym in the type t. *)
    let subst =
      let o =
        Ty.(
          object
            inherit [_] map_ty as super

            method! on_t env t =
              let t =
                match (t, env) with
                | (TVar (i, ts), (r, sym)) when r = i -> generic_talias sym ts
                | _ -> t
              in
              super#on_t env t
          end)
      in
      (fun r sym t -> o#on_t (r, sym) t)

    (* We shouldn't really create bare Mu types for two reasons.

       First, substitutions and type applications may replace the recursive variables with
       a non-recursive type part, rendering the entire type non-recursive. When
       `check_recursive` is set to true, we first perform the check.

       Second, TypeAliases and Mu types can both imply recursion, so we have little
       to gain from nesting them. Therefore, before creating a Mu type, we should check if
       the type under the Mu is a TypeAlias and if it is then use that structure to
       express the recursion. To do this we replace 'i' for 'ta_name' in the body of
       the alias.

       PV: The second one is a seemingly awkward fix. However, I am not very concerned as
       `TypeAlias` is abusively treated as a type. Eventually, the goal is to include it
       in a "module element" category rather than the Ty.t structure.
    *)
    let mk_mu ~definitely_appears i t =
      Ty.(
        if definitely_appears || Ty_utils.tvar_appears_in_type ~is_toplevel:true (Ty.RVar i) t then
          match t with
          | TypeAlias { ta_name; _ } -> subst (RVar i) ta_name t
          | _ -> Mu (i, t)
        else
          t)

    (* When inferring recursive types, the top-level appearances of the recursive
       variable should be eliminated. This visitor performs the following
       transformations:

         (recursive var: X , type: X           ) ==> Bot
         (recursive var: X , type: X | t       ) ==> Bot | t
         (recursive var: X , type: X & t       ) ==> Top & t
         (recursive var: X , type: mu Y . X | t) ==> mu Y . Bot | t

       The visitor only descends down to the first concrete constructor
       (e.g. Function, Class) and is applied to the subparts of unions,
       intersections and recursive types.

       It is expected to followed by type minimization, so that the introduced
       Bot and Top can be eliminated.
    *)
    let remove_toplevel_tvar =
      Ty.(
        let o =
          object (self)
            inherit [_] endo_ty

            method env_zero =
              function
              | `Union -> Bot (NoLowerWithUpper NoUpper)
              | `Inter -> Top

            method! on_t env t =
              match (env, t) with
              | ((v, _), Union (t0, t1, ts)) ->
                let t0' = self#on_t (v, `Union) t0 in
                let ts' = self#on_list self#on_t (v, `Union) (t1 :: ts) in
                if t0 == t0' && ts == ts' then
                  t
                else
                  Ty.mk_union (t0', ts')
              | ((v, _), Inter (t0, t1, ts)) ->
                let t0' = self#on_t (v, `Inter) t0 in
                let ts' = self#on_list self#on_t (v, `Inter) (t1 :: ts) in
                if t0 == t0' && ts == ts' then
                  t
                else
                  Ty.mk_inter (t0', ts')
              | ((v, mode), TVar (Ty.RVar v', _)) when v = v' -> self#env_zero mode
              | (_, Mu (v, rt)) ->
                let rt' = self#on_t env rt in
                if rt == rt' then
                  t
                else
                  mk_mu ~definitely_appears:false v rt'
              | (_, _) -> t
          end
        in
        (fun v t -> o#on_t (v, `Union) t))

    (* Constructing recursive types.

       This function is expected to be called after fully normalizing the lower
       bounds of a type variable `v` and constructing a type `t`. `free_vars` is
       the set of free variables appearing in `t`. This information is available
       from the state of the monad.

       To determine if we truly have a recursive type we take the following into
       account:

        - If `v` does NOT appear in `free_vars`, then `t` is NOT recursive, so
          we return it as-is.

        - If `v` appears in `free_vars`, we may be dealing with a recursive type
          but we also might have a degenerate case like this one:

            Mu (v, v | string)

          which is not a recursive type. (It is equivalent to string.)
          So, first we simplify the type by performing the "remove_top_level_tvar"
          transformation and some subsequent simplifications. Then if the type
          changed we check again if `v` is in the free variables.

          NOTE that we need to recompute free vars since the simplifications might
          have eliminated some of them. Here we use the FreeVars module. This is
          an expensive pass, which is why we avoid doing it if it's definitely
          not a recursive type.
    *)
    let make free_vars i t =
      if not (VSet.mem i free_vars) then
        t
      else
        (* Recursive, but still might be a degenerate case *)
        let t' = remove_toplevel_tvar i t in
        let changed = not (t == t') in
        (* If not changed then all free_vars are still in, o.w. recompute free vars *)
        mk_mu ~definitely_appears:(not changed) i t'

    (* Normalize potentially recursive types.
     *
     * Input here is a unique `id` (e.g. tvar) and a type-level operation `f`.
     * To avoid non-termination we make use of the tvar_cache, that holds the result
     * of ids that have been resolved, or are "in resolution". Depending on whether
     * we have seen `id` before, there are a few cases here:
     *
     * a. The variable is "under resolution": We return the recursive variable as
     *    result and update the set of free_tvars, which is later used to construct
     *    the normalized recursive type.
     *
     * b. The variable is "resolved": Return the result.
     *
     * c. The variable resolution led to an error: Propagate error.
     *
     * d. The variable has never been seen before:
     *    - We set the variable to "under resolution" and introduce a recursive
     *      variable 'rid' to represent the normalized result.
     *    - We evaluate the type (`f ()`). This typically includes a Flow_js constraint
     *      level computation, like resolving lower-bounds, evaluating a type
     *      destructor.
     *    - We remove 'rid' from free_tvars, since it will no longer be in scope.
     *    - We return the result.
     *)
    let with_cache id ~f =
      let open State in
      match%bind find_in_cache id with
      | Some (Ok Ty.(TVar (RVar v, _) as t)) ->
        let%map () = add_to_free_tvars v in
        t
      | Some (Ok t) -> return t
      | Some (Error s) -> error s
      | None ->
        let%bind rid = fresh_num in
        let rvar = Ty.RVar rid in
        (* Set current variable "under resolution" *)
        let%bind () = update_cache id (Ok (Ty.TVar (rvar, None))) in
        let%bind in_st = get in
        let (result, out_st) = run in_st (f ()) in
        let result = Base.Result.map ~f:(make out_st.free_tvars rid) result in
        (* Reset state by removing the current tvar from the free vars set *)
        let out_st = { out_st with free_tvars = VSet.remove rid out_st.free_tvars } in
        let%bind () = put out_st in
        (* Update cache with final result *)
        let%bind () = update_cache id result in
        (* Throw the error if one was encountered *)
        (match result with
        | Ok ty -> return ty
        | Error e -> error e)
  end

  module Substitution = struct
    open Ty

    (* NOTE Traversing huge types may lead to merge-timeouts. We cut off the size
     * of the recursion at 10K nodes. *)
    let max_size = 10000

    exception SizeCutOff

    let size = ref 0

    let init_env tparams types =
      let rec step acc = function
        | ([], _)
        | (_, []) ->
          acc
        | (p :: ps, t :: ts) -> step (SMap.add p.tp_name t acc) (ps, ts)
      in
      step SMap.empty (tparams, types)

    let remove_params env = function
      | None -> env
      | Some ps -> List.fold_left (fun e p -> SMap.remove p.tp_name e) env ps

    let visitor =
      object
        inherit [_] endo_ty as super

        method! on_t env t =
          size := !size + 1;
          if !size > max_size then raise SizeCutOff;
          match t with
          | TypeAlias { ta_tparams = ps; _ }
          | Fun { fun_type_params = ps; _ } ->
            let env = remove_params env ps in
            super#on_t env t
          | Bound (_, name) ->
            begin
              match SMap.find_opt name env with
              | Some t' -> t'
              | None -> super#on_t env t
            end
          | _ -> super#on_t env t
      end

    (* Replace a list of type parameters with a list of types in the given type.
     * These lists might not match exactly in length. *)
    let run vs ts t =
      let env = init_env vs ts in
      size := 0;
      match visitor#on_t env t with
      | exception SizeCutOff -> terr ~kind:TypeTooBig None
      | t' -> return t'
  end

  (***********************)
  (* Construct built-ins *)
  (***********************)

  let opt_param = Ty.{ prm_optional = true }

  let non_opt_param = Ty.{ prm_optional = false }

  let mk_fun ?(params = []) ?rest ?tparams ?(static = Ty.(TypeOf FunProto)) ret =
    Ty.(
      Fun
        {
          fun_params = params;
          fun_rest_param = rest;
          fun_return = ret;
          fun_type_params = tparams;
          fun_static = static;
        })

  let mk_tparam ?bound ?(pol = Ty.Neutral) ?default name =
    Ty.{ tp_name = name; tp_bound = bound; tp_polarity = pol; tp_default = default }

  let symbol_from_loc env sym_def_loc sym_name =
    let open File_key in
    let symbol_source = ALoc.source sym_def_loc in
    let sym_provenance =
      match symbol_source with
      | Some (LibFile def_source) ->
        let current_source = Env.(env.genv.file) in
        if File_key.to_string current_source = def_source then
          Ty.Local
        else
          Ty.Library { Ty.imported_as = ALocMap.find_opt sym_def_loc env.Env.imported_names }
      | Some (SourceFile def_source) ->
        let current_source = Env.(env.genv.file) in
        if File_key.to_string current_source = def_source then
          Ty.Local
        else
          Ty.Remote { Ty.imported_as = ALocMap.find_opt sym_def_loc env.Env.imported_names }
      | Some (JsonFile _)
      | Some (ResourceFile _) ->
        Ty.Local
      | Some Builtins -> Ty.Builtin
      | None -> Ty.Local
    in
    let sym_anonymous = sym_name = "<<anonymous class>>" in
    { Ty.sym_provenance; sym_name; sym_anonymous; sym_def_loc }

  (* NOTE Due to repositioning, `reason_loc` may not point to the actual location
     where `name` was defined. *)
  let symbol_from_reason env reason name =
    let def_loc = Reason.def_aloc_of_reason reason in
    symbol_from_loc env def_loc name

  let remove_targs_matching_defaults targs tparams =
    let matches_default targ tparam = Some targ = Ty.(tparam.tp_default) in
    let rec remove_if_able targ_lst tparam_lst =
      match (targ_lst, tparam_lst) with
      (* Recursive case. Recurse, then if this is now the last targ (if later ones were eliminated),
       * remove it if it matches the tparam default. *)
      | (targ :: targ_rst, tparam :: tparam_rst) ->
        let targ_rst = remove_if_able targ_rst tparam_rst in
        if ListUtils.is_empty targ_rst && matches_default targ tparam then
          []
        else
          targ :: targ_rst
      | ([], []) (* Base case *)
      | ([], _ :: _) (* Fewer targs than tparams to begin with. *)
      | (_ :: _, []) (* More targs than tparams. This shouldn't happen. *) ->
        targ_lst
    in
    match (targs, tparams) with
    | (Some targ_lst, Some tparam_lst) -> Some (remove_if_able targ_lst tparam_lst)
    | _ -> targs

  module Reason_utils = struct
    let local_type_alias_symbol env reason =
      match desc_of_reason ~unwrap:false reason with
      | RTypeAlias (name, Some loc, _) -> return (symbol_from_loc env loc name)
      | RType name -> return (symbol_from_reason env reason name)
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract local type alias name from reason: " ^ desc in
        terr ~kind:BadTypeAlias ~msg None

    let imported_type_alias_symbol env reason =
      match desc_of_reason ~unwrap:false reason with
      | RNamedImportedType (name, _)
      | RDefaultImportedType (name, _)
      | RImportStarType name
      | RImportStarTypeOf name
      | RImportStar name ->
        return (symbol_from_reason env reason name)
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract imported type alias name from reason: " ^ desc in
        terr ~kind:BadTypeAlias ~msg None

    let instance_symbol env reason =
      match desc_of_reason reason with
      | RType name
      | RIdentifier name ->
        (* class or interface declaration *)
        let symbol = symbol_from_reason env reason name in
        return symbol
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract instance name from reason: " ^ desc in
        terr ~kind:BadInstanceT ~msg None
  end

  (*************************)
  (* Main transformation   *)
  (*************************)

  let rec type__ =
    let type_debug ~env ~depth t state =
      let cx = Env.get_cx env in
      let prefix = spf "%*s[Norm|run_id:%d|depth:%d]" (2 * depth) "" (get_run_id ()) depth in
      prerr_endlinef "%s Input: %s\n" prefix (Debug_js.dump_t cx t);
      let result = type_with_expand_members ~env t state in
      let result_str =
        match result with
        | (Ok ty, _) -> "[Ok] " ^ Ty_debug.dump_t ty
        | (Error e, _) -> "[Error] " ^ error_to_string e
      in
      prerr_endlinef "%s Output: %s\n" prefix result_str;
      result
    in
    fun ~env t ->
      let env = Env.descend env in
      let options = env.Env.options in
      let depth = env.Env.depth - 1 in
      match Env.max_depth env with
      | Some max_depth when depth > max_depth -> terr ~kind:RecursionLimit (Some t)
      | _ ->
        if options.Env.verbose_normalizer then
          type_debug ~env ~depth t
        else
          type_with_expand_members ~env t

  (* If we're interested in seeing the type's members, then we should skip
   * recovering type parameters/aliases. *)
  and type_with_expand_members ~env t =
    match Env.get_member_expansion_info env with
    | Some (_, env) -> type_after_reason ~env:(Env.continue_expanding_members env) t
    | None -> type_poly ~env t

  (* Before we start pattern-matching on the structure of the input type, we can
     reconstruct some types based on attached reasons. Two cases are of interest here:
     - Type parameters: we use RPolyTest reasons for these
     - Type aliases: we use RTypeAlias reasons for these *)
  and type_poly ~env t =
    (* The RPolyTest description is used for types that represent type parameters.
       When normalizing, we want such types to be replaced by the type parameter,
       whose name is part of the description, but only in the case that the parameter
       is in scope. The reason we need to make this distinction is that bound tests
       may exit the scope of the structure that introduced them, in which case we
       do not perform the substitution. There instead we unfold the underlying type. *)
    let reason = Type.reason_of_t t in
    match desc_of_reason ~unwrap:false reason with
    | RPolyTest (name, _) ->
      let loc = Reason.def_aloc_of_reason reason in
      let default t = type_with_alias_reason ~env t in
      lookup_tparam ~default env t name loc
    | _ -> type_with_alias_reason ~env t

  and type_with_alias_reason ~env t =
    if Env.expand_type_aliases env then
      type_after_reason ~env t
    else
      let reason = Type.reason_of_t t in
      Type.(
        (* These type are treated as transparent when it comes to the type alias
         * annotation.
         *
         * TypeDestructorTriggerT might hold a type-app, so avoid using the type here.
         * Instead, do the fallback action which is to normalize to Bot. The trigger
         * should have actually produced another concrete type as a lower bound. *)
        match t with
        | OpenT _
        | TypeDestructorTriggerT _ ->
          type_after_reason ~env t
        | EvalT _ when Env.evaluate_type_destructors env -> type_after_reason ~env t
        | _ ->
          begin
            match desc_of_reason ~unwrap:false reason with
            | RTypeAlias (name, Some _, _) ->
              (* The default action is to avoid expansion by using the type alias name,
                 when this can be trusted. The one case where we want to skip this process
                 is when recovering the body of a type alias A. In that case the environment
                 field under_type_alias will be 'Some A'. If the type alias name in the reason
                 is also A, then we are still at the top-level of the type-alias, so we
                 proceed by expanding one level preserving the same environment. *)
              let continue =
                match env.Env.under_type_alias with
                | Some name' -> name = name'
                | None -> false
              in
              if continue then
                type_after_reason ~env t
              else
                let symbol = symbol_from_reason env reason name in
                return (generic_talias symbol None)
            | _ ->
              (* We are now beyond the point of the one-off expansion. Reset the environment
                 assigning None to under_type_alias, so that aliases are used in subsequent
                 invocations. *)
              let env = Env.{ env with under_type_alias = None } in
              type_after_reason ~env t
          end)

  and type_after_reason ~env t =
    let open Type in
    match t with
    | OpenT (_, id) -> type_variable ~env id
    | BoundT (reason, name) -> bound_t ~env reason name
    | AnnotT (_, t, _) -> type__ ~env t
    | EvalT (t, d, id) -> eval_t ~env t id d
    | ExactT (_, t) -> exact_t ~env t
    | CustomFunT (_, f) -> custom_fun ~env f
    | InternalT i -> internal_t t i
    | MatchingPropT _ -> return (mk_empty Ty.EmptyMatchingPropT)
    | DefT (_, _, MixedT _) -> return Ty.Top
    | AnyT (_, kind) -> return (Ty.Any (any_t kind))
    | DefT (_, _, VoidT) -> return Ty.Void
    | DefT (r, _, NumT (Literal (_, (_, x)))) when Env.preserve_inferred_literal_types env ->
      primitive ~env (Ty.Num (Some x)) r "Number"
    | DefT (r, _, NumT (Truthy | AnyLiteral | Literal _)) -> primitive ~env (Ty.Num None) r "Number"
    | DefT (r, _, StrT (Literal (_, x))) when Env.preserve_inferred_literal_types env ->
      primitive ~env (Ty.Str (Some x)) r "String"
    | DefT (r, _, StrT (Truthy | AnyLiteral | Literal _)) -> primitive ~env (Ty.Str None) r "String"
    | DefT (r, _, BoolT (Some x)) when Env.preserve_inferred_literal_types env ->
      primitive ~env (Ty.Bool (Some x)) r "Boolean"
    | DefT (r, _, BoolT _) -> primitive ~env (Ty.Bool None) r "Boolean"
    | DefT (_, _, EmptyT _) -> return (mk_empty Ty.EmptyType)
    | DefT (_, _, NullT) -> return Ty.Null
    | DefT (r, _, SymbolT) -> primitive ~env Ty.Symbol r "Symbol"
    | DefT (r, _, SingletonNumT (_, lit)) -> primitive ~env (Ty.NumLit lit) r "Number"
    | DefT (r, _, SingletonStrT lit) -> primitive ~env (Ty.StrLit lit) r "String"
    | DefT (r, _, SingletonBoolT lit) -> primitive ~env (Ty.BoolLit lit) r "Boolean"
    | MaybeT (_, t) ->
      let%map t = type__ ~env t in
      Ty.mk_union (Ty.Void, [Ty.Null; t])
    | OptionalT { reason = _; type_ = t; use_desc = _ } ->
      let%map t = type__ ~env t in
      Ty.mk_union (Ty.Void, [t])
    | DefT (_, _, FunT (static, _, f)) ->
      let%map t = fun_ty ~env static f None in
      Ty.Fun t
    | DefT (r, _, ObjT o) -> obj_ty ~env r o
    | DefT (r, _, ArrT a) -> arr_ty ~env r a
    | UnionT (_, rep) ->
      let (t0, (t1, ts)) = UnionRep.members_nel rep in
      let%bind t0 = type__ ~env t0 in
      let%bind t1 = type__ ~env t1 in
      let%map ts = mapM (type__ ~env) ts in
      Ty.mk_union (t0, t1 :: ts)
    | IntersectionT (_, rep) ->
      let (t0, (t1, ts)) = InterRep.members_nel rep in
      let%bind t0 = type__ ~env t0 in
      let%bind t1 = type__ ~env t1 in
      let%map ts = mapM (type__ ~env) ts in
      Ty.mk_inter (t0, t1 :: ts)
    | DefT (_, _, PolyT { tparams = ps; t_out = t; _ }) -> poly_ty ~env t ps
    | DefT (r, _, TypeT (kind, t)) -> type_t ~env r kind t None
    | TypeAppT (_, _, t, ts) -> type_app ~env t (Some ts)
    | DefT (r, _, InstanceT (static, super, _, t)) -> instance_t ~env r static super t
    | DefT (_, _, ClassT t) -> class_t ~env t None
    | DefT (_, _, IdxWrapper t) ->
      Base.Option.iter
        (Env.get_member_expansion_info env)
        ~f:(fun (Env.{ member_expansion_options = { idx_hook; _ }; _ }, _) -> idx_hook ());
      type__ ~env t
    | DefT (_, _, ReactAbstractComponentT { config; instance }) ->
      let%bind config = type__ ~env config in
      let%bind instance = type__ ~env instance in
      return
        (generic_talias
           (Ty_symbol.builtin_symbol "React$AbstractComponent")
           (Some [config; instance]))
    | ThisClassT (_, t) -> this_class_t ~env t None
    | ThisTypeAppT (_, c, _, ts) ->
      begin
        match Env.get_member_expansion_info env with
        | None -> type_app ~env c ts
        | Some (_, env) -> type__ ~env:(Env.continue_expanding_members env) c
      end
    | KeysT (_, t) ->
      let%map ty = type__ ~env t in
      Ty.Utility (Ty.Keys ty)
    | OpaqueT (r, o) -> opaque_t ~env r o
    | ReposT (_, t) -> type__ ~env t
    | ShapeT (_, t) ->
      let%map t = type__ ~env t in
      Ty.Utility (Ty.Shape t)
    | TypeDestructorTriggerT (_, r, _, _, _) ->
      let loc = Reason.def_aloc_of_reason r in
      return (mk_empty (Ty.EmptyTypeDestructorTriggerT loc))
    | MergedT (_, uses) -> merged_t ~env uses
    | ExistsT _ -> return Ty.(Utility Exists)
    | ObjProtoT r -> primitive ~env Ty.(TypeOf ObjProto) r "Object"
    | FunProtoT r -> primitive ~env Ty.(TypeOf FunProto) r "Function"
    | OpenPredT { base_t = t; m_pos = _; m_neg = _; reason = _ } -> type__ ~env t
    | FunProtoApplyT _ ->
      if Env.expand_internal_types env then
        (* Function.prototype.apply: (thisArg: any, argArray?: any): any *)
        return
          Ty.(
            mk_fun
              ~params:
                [
                  (Some "thisArg", explicit_any, non_opt_param);
                  (Some "argArray", explicit_any, opt_param);
                ]
              explicit_any)
      else
        return Ty.(TypeOf FunProtoApply)
    | FunProtoBindT _ ->
      if Env.expand_internal_types env then
        (* Function.prototype.bind: (thisArg: any, ...argArray: Array<any>): any *)
        return
          Ty.(
            mk_fun
              ~params:[(Some "thisArg", explicit_any, non_opt_param)]
              ~rest:
                ( Some "argArray",
                  Arr { arr_readonly = false; arr_literal = false; arr_elt_t = explicit_any } )
              explicit_any)
      else
        return Ty.(TypeOf FunProtoBind)
    | FunProtoCallT _ ->
      if Env.expand_internal_types env then
        (* Function.prototype.call: (thisArg: any, ...argArray: Array<any>): any *)
        return
          Ty.(
            mk_fun
              ~params:[(Some "thisArg", explicit_any, non_opt_param)]
              ~rest:
                ( Some "argArray",
                  Arr { arr_readonly = false; arr_literal = false; arr_elt_t = explicit_any } )
              explicit_any)
      else
        return Ty.(TypeOf FunProtoCall)
    | ModuleT (reason, exports, _) -> module_t env reason exports t
    | NullProtoT _ -> return Ty.Null
    | DefT (reason, trust, EnumObjectT enum) -> enum_t ~env reason trust enum
    | DefT (reason, _, EnumT { enum_name; _ }) ->
      let symbol = symbol_from_reason env reason enum_name in
      return (Ty.Generic (symbol, Ty.EnumKind, None))
    | DefT (_, _, CharSetT s) -> return (Ty.CharSet (String_utils.CharSet.to_string s))

  and primitive ~env ty reason builtin =
    match Env.get_member_expansion_info env with
    | None -> return ty
    | Some (_, env) ->
      let t = Flow_js.get_builtin_type Env.(env.genv.cx) reason builtin in
      type__ ~env:(Env.expand_primitive_members env) t

  and type_variable ~env id =
    let (root_id, constraints) =
      (* Use `root_id` as a proxy for `id` *)
      Context.find_constraints Env.(env.genv.cx) id
    in
    Recursive.with_cache (TVarKey root_id) ~f:(fun () -> resolve_bounds ~env constraints)

  (* Resolving a type variable amounts to normalizing its lower bounds and
     taking their union.
  *)
  and resolve_bounds ~env = function
    | Constraint.Resolved (_, t)
    | Constraint.FullyResolved (_, t) ->
      type__ ~env t
    | Constraint.Unresolved bounds ->
      (match%bind resolve_from_lower_bounds ~env bounds with
      | [] -> empty_with_upper_bounds ~env bounds
      | hd :: tl -> return (Ty.mk_union ~flattened:true (hd, tl)))

  and resolve_from_lower_bounds ~env bounds =
    T.TypeMap.keys bounds.Constraint.lower
    |> mapM (fun t ->
           let%map ty = type__ ~env t in
           Nel.to_list (Ty.bk_union ty))
    >>| Base.List.concat
    >>| Base.List.dedup_and_sort ~compare:Pervasives.compare

  and empty_with_upper_bounds ~env bounds =
    let uses = T.UseTypeMap.keys bounds.Constraint.upper in
    let%map use_kind = uses_t ~env uses in
    Ty.Bot (Ty.NoLowerWithUpper use_kind)

  and any_t = function
    | T.Annotated -> Ty.Annotated
    | T.AnyError -> Ty.AnyError
    | T.Unsound k -> Ty.Unsound (unsoundness_any_t k)
    | T.Untyped -> Ty.Untyped

  and unsoundness_any_t = function
    | T.BoundFunctionThis -> Ty.BoundFunctionThis
    | T.ComputedNonLiteralKey -> Ty.ComputedNonLiteralKey
    | T.Constructor -> Ty.Constructor
    | T.DummyStatic -> Ty.DummyStatic
    | T.Existential -> Ty.Existential
    | T.Exports -> Ty.Exports
    | T.FunctionPrototype -> Ty.FunctionPrototype
    | T.InferenceHooks -> Ty.InferenceHooks
    | T.InstanceOfRefinement -> Ty.InstanceOfRefinement
    | T.Merged -> Ty.Merged
    | T.ResolveSpread -> Ty.ResolveSpread
    | T.Unchecked -> Ty.Unchecked
    | T.Unimplemented -> Ty.Unimplemented
    | T.UnresolvedType -> Ty.UnresolvedType
    | T.WeakContext -> Ty.WeakContext

  and bound_t ~env reason name =
    let { Ty.sym_def_loc; sym_name; _ } = symbol_from_reason env reason name in
    return (Ty.Bound (sym_def_loc, sym_name))

  and fun_ty ~env static f fun_type_params =
    let%bind fun_static = type__ ~env static in
    let { T.params; rest_param; return_t; _ } = f in
    let%bind fun_params = mapM (fun_param ~env) params in
    let%bind fun_rest_param = fun_rest_param_t ~env rest_param in
    let%bind fun_return = type__ ~env return_t in
    return { Ty.fun_params; fun_rest_param; fun_return; fun_type_params; fun_static }

  and method_ty ~env t =
    let rec go = function
      | Ty.Fun ft -> return [ft]
      | Ty.Inter (t1, t2, ts) -> concat_fold_m go (t1 :: t2 :: ts)
      | _ -> terr ~kind:BadMethodType (Some t)
    in
    let%bind ty = type__ ~env t in
    go ty

  and fun_param ~env (x, t) =
    let%bind (t, prm_optional) = opt_t ~env t in
    return (x, t, { Ty.prm_optional })

  and fun_rest_param_t ~env = function
    | Some (x, _, t) ->
      let%map t = type__ ~env t in
      Some (x, t)
    | _ -> return None

  and obj_ty ~env reason o =
    let { T.flags; props_tmap; call_t; dict_t; proto_t } = o in
    let { T.exact = obj_exact; T.frozen = obj_frozen; _ } = flags in
    let obj_literal =
      match Reason.desc_of_reason reason with
      | Reason.RObjectLit -> true
      | _ -> false
    in
    let%map obj_props =
      match Env.get_member_expansion_info env with
      | None -> obj_props ~env props_tmap call_t dict_t
      | Some (Env.{ member_expansion_options = { include_proto_members = false; _ }; _ }, env) ->
        obj_props ~env props_tmap call_t dict_t
      | Some (Env.{ member_expansion_options = { include_proto_members = true; _ }; _ }, env) ->
        let%bind proto = type__ ~env:(Env.continue_expanding_members env) proto_t in
        let%map obj_props = obj_props ~env props_tmap call_t dict_t in
        Ty.SpreadProp proto :: obj_props
    in
    Ty.Obj { Ty.obj_exact; obj_frozen; obj_literal; obj_props }

  and obj_prop ~env (x, p) =
    let from_proto = Env.within_primitive env in
    match p with
    | T.Field (_, t, polarity) ->
      let fld_polarity = type_polarity polarity in
      let%map (t, fld_optional) = opt_t ~env t in
      [Ty.(NamedProp { name = x; prop = Field (t, { fld_polarity; fld_optional }); from_proto })]
    | T.Method (_, t) ->
      let%map tys = method_ty ~env t in
      List.map (fun ty -> Ty.NamedProp { name = x; prop = Ty.Method ty; from_proto }) tys
    | T.Get (_, t) ->
      let%map t = type__ ~env t in
      [Ty.NamedProp { name = x; prop = Ty.Get t; from_proto }]
    | T.Set (_, t) ->
      let%map t = type__ ~env t in
      [Ty.NamedProp { name = x; prop = Ty.Set t; from_proto }]
    | T.GetSet (loc1, t1, loc2, t2) ->
      let%bind p1 = obj_prop ~env (x, T.Get (loc1, t1)) in
      let%map p2 = obj_prop ~env (x, T.Set (loc2, t2)) in
      p1 @ p2

  and call_prop_from_t ~env t =
    let ts =
      match t with
      | T.IntersectionT (_, rep) -> T.InterRep.members rep
      | t -> [t]
    in
    let%map ts = concat_fold_m (method_ty ~env) ts in
    Base.List.map ~f:(fun t -> Ty.CallProp t) ts

  and obj_props =
    (* call property *)
    let do_calls ~env = function
      | Some call_id ->
        let cx = Env.get_cx env in
        let ft = Context.find_call cx call_id in
        call_prop_from_t ~env ft
      | None -> return []
    in
    let do_props ~env props = concat_fold_m (obj_prop ~env) props in
    let do_dict ~env = function
      | Some d ->
        let { T.dict_polarity; dict_name; key; value } = d in
        let dict_polarity = type_polarity dict_polarity in
        let%bind dict_key = type__ ~env key in
        let%map dict_value = type__ ~env value in
        [Ty.IndexProp { Ty.dict_polarity; dict_name; dict_key; dict_value }]
      | None -> return []
    in
    fun ~env props_id call_id_opt dict ->
      let cx = Env.get_cx env in
      let props = SMap.bindings (Context.find_props cx props_id) in
      let%bind call_props = do_calls ~env call_id_opt in
      let%bind props = do_props ~env props in
      let%map dict = do_dict ~env dict in
      call_props @ props @ dict

  and arr_ty ~env reason elt_t =
    let arr_literal =
      match Reason.desc_of_reason reason with
      | Reason.RArrayLit -> true
      | _ -> false
    in
    match (elt_t, Env.get_member_expansion_info env) with
    | (T.ArrayAT (t, _), None) ->
      let%map t = type__ ~env t in
      Ty.Arr { Ty.arr_readonly = false; arr_literal; arr_elt_t = t }
    | (T.ROArrayAT t, None) ->
      let%map t = type__ ~env t in
      Ty.Arr { Ty.arr_readonly = true; arr_literal; arr_elt_t = t }
    | (T.TupleAT (_, ts), None) ->
      let%map ts = mapM (type__ ~env) ts in
      Ty.Tup ts
    | (T.ArrayAT _, Some (_, env)) ->
      type__
        ~env:(Env.expand_instance_members env)
        (Flow_js.get_builtin (Env.get_cx env) "Array" reason)
    | (T.ROArrayAT _, Some (_, env))
    | (T.TupleAT _, Some (_, env)) ->
      type__
        ~env:(Env.expand_instance_members env)
        (Flow_js.get_builtin (Env.get_cx env) "$ReadOnlyArray" reason)

  (* Used for instances of React.createClass(..) *)
  and react_component =
    let react_props ~env ~default props name =
      match SMap.find name props with
      | exception Not_found -> return default
      | Type.Field (_, t, _) -> type__ ~env t
      | _ -> return default
    in
    let react_static_props ~env static =
      let cx = Env.(env.genv.cx) in
      match static with
      | T.DefT (_, _, T.ObjT { T.props_tmap; _ }) ->
        Context.find_props cx props_tmap
        |> SMap.bindings
        |> mapM (fun (name, p) -> obj_prop ~env (name, p))
        >>| List.concat
      | _ -> return []
    in
    let inexactify = function
      | Ty.Obj ({ Ty.obj_exact = true; _ } as obj) -> Ty.Obj { obj with Ty.obj_exact = false }
      | ty -> ty
    in
    fun ~env static own_props ->
      let cx = Env.(env.genv.cx) in
      let own_props = Context.find_props cx own_props in
      let%bind props_ty = react_props ~env ~default:Ty.explicit_any own_props "props" in
      let%bind state_ty = react_props ~env ~default:Ty.explicit_any own_props "state" in
      let%map static_flds = react_static_props ~env static in
      (* The inferred type for state is unsealed, which has its exact bit set.
       * However, Ty.t does not account for unsealed and exact sealed objects are
       * incompatible with exact and unsealed, so making state inexact here. *)
      let state_ty = inexactify state_ty in
      let parent_instance = generic_builtin_t "React$Component" [props_ty; state_ty] in
      let parent_class = Ty.Utility (Ty.Class parent_instance) in
      (*
       * {
       *   +propTypes: {
       *     foo: React$PropType$Primitive$Required<string>,
       *     bar: React$PropType$Primitive<number>,
       *   },
       *   defaultProps: { ... },
       * }
       *)
      let props_obj =
        Ty.Obj
          { Ty.obj_exact = false; obj_frozen = false; obj_literal = false; obj_props = static_flds }
      in
      Ty.mk_inter (parent_class, [props_obj])

  and enum_t ~env enum_reason trust enum =
    match Env.get_member_expansion_info env with
    | None ->
      let { T.enum_name; _ } = enum in
      let symbol = symbol_from_reason env enum_reason enum_name in
      return Ty.(EnumDecl symbol)
    | Some (_, env) ->
      let { T.members; representation_t; _ } = enum in
      let enum_t = T.mk_enum_type ~loc:(def_aloc_of_reason enum_reason) ~trust enum in
      let proto_t =
        Flow_js.get_builtin_typeapp
          Env.(env.genv.cx)
          enum_reason
          "$EnumProto"
          [enum_t; representation_t]
      in
      let%bind proto_ty = type__ ~env:(Env.expand_primitive_members env) proto_t in
      let%bind enum_ty = type__ ~env enum_t in
      let%map members_ty =
        SMap.keys members
        |> List.map (fun name ->
               Ty.(
                 NamedProp
                   {
                     name;
                     prop = Field (enum_ty, { fld_polarity = Positive; fld_optional = false });
                     from_proto = false;
                   }))
        |> return
      in
      Ty.mk_object (Ty.SpreadProp proto_ty :: members_ty)

  and instance_t =
    let to_generic ~env kind r inst =
      let%bind symbol = Reason_utils.instance_symbol env r in
      let%map tys = mapM (fun (_, _, t, _) -> type__ ~env t) inst.T.type_args in
      let targs =
        match tys with
        | [] -> None
        | _ -> Some tys
      in
      Ty.Generic (symbol, kind, targs)
    in
    let to_object ~env member_expansion_info super own_props proto_props =
      let%bind own_ty_props = obj_props ~env own_props None None in
      let%bind proto_ty_props = obj_props ~env proto_props None None in
      let%map obj_props =
        if Env.(member_expansion_info.member_expansion_options.include_proto_members) then
          let%map super_ty = type__ ~env:(Env.expand_instance_members env) super in
          (Ty.SpreadProp super_ty :: own_ty_props) @ proto_ty_props
        else
          return (own_ty_props @ proto_ty_props)
      in
      Ty.(Obj { obj_exact = true; obj_frozen = false; obj_literal = false; obj_props })
    in
    fun ~env r static super inst ->
      let { T.inst_kind; own_props; inst_call_t; proto_props; _ } = inst in
      match inst_kind with
      | T.InterfaceKind { inline = true } -> inline_interface ~env super own_props inst_call_t
      | T.InterfaceKind { inline = false } -> to_generic ~env Ty.InterfaceKind r inst
      | T.ClassKind ->
        begin
          match Env.get_member_expansion_info env with
          | None -> to_generic ~env Ty.ClassKind r inst
          | Some (Env.{ instance_member_expansion_mode = IMStatic; _ }, env) ->
            type__ ~env:(Env.continue_expanding_members env) static
          | Some ((Env.{ instance_member_expansion_mode = IMUnset | IMInstance; _ } as info), env)
            ->
            to_object ~env info super own_props proto_props
        end

  and inline_interface =
    let rec extends = function
      | Ty.Generic g -> return [g]
      | Ty.Inter (t1, t2, ts) -> mapM extends (t1 :: t2 :: ts) >>| Base.List.concat
      | Ty.TypeOf Ty.ObjProto (* interface {} *)
      | Ty.TypeOf Ty.FunProto (* interface { (): void } *) ->
        (* Do not contribute to the extends clause *)
        return []
      | _ ->
        (* Top-level syntax only allows generics in extends *)
        terr ~kind:BadInlineInterfaceExtends None
    in
    let fix_dict_props props =
      let (key, value, pole, props) =
        List.fold_left
          (fun (key, value, pole, ps) p ->
            match p with
            | Ty.NamedProp { name = "$key"; prop = Ty.Field (t, _); _ } ->
              (* The $key's polarity is fixed to neutral so we ignore it *)
              (Some t, value, pole, ps)
            | Ty.NamedProp { name = "$value"; prop = Ty.Field (t, { Ty.fld_polarity; _ }); _ } ->
              (* The dictionary's polarity is determined by that of $value *)
              (key, Some t, Some fld_polarity, ps)
            | _ -> (key, value, pole, p :: ps))
          (None, None, None, [])
          props
      in
      let props = List.rev props in
      match (key, value, pole) with
      | (Some dict_key, Some dict_value, Some dict_polarity) ->
        let ind_prop =
          Ty.IndexProp
            { Ty.dict_polarity; dict_name = None; (* This seems to be lost *)
                                                  dict_key; dict_value }
        in
        ind_prop :: props
      | (_, _, _) -> props
    in
    fun ~env super own_props inst_call_t ->
      let%bind super = type__ ~env super in
      let%bind if_extends = extends super in
      let%map obj_props = obj_props ~env own_props inst_call_t None (* dict *) in
      let obj_props = fix_dict_props obj_props in
      let if_body = { Ty.obj_exact = false; obj_frozen = false; obj_literal = false; obj_props } in
      Ty.InlineInterface { Ty.if_extends; if_body }

  and class_t =
    let go ~env ps ty =
      match ty with
      | Ty.Generic (name, kind, _) ->
        begin
          match kind with
          | Ty.InterfaceKind -> return (Ty.InterfaceDecl (name, ps))
          | Ty.TypeAliasKind
          | Ty.EnumKind ->
            return (Ty.Utility (Ty.Class ty))
          | Ty.ClassKind ->
            (* If some parameters have been passed, then we are in the `PolyT-ThisClassT`
             * case of Flow_js.canonicalize_imported_type. This case should still be
             * normalized to an abstract class declaration. If no parameters are passed
             * then this is a `Class<T>` with an instance T. *)
            begin
              match ps with
              | Some _ -> return (Ty.ClassDecl (name, ps))
              | None -> return (Ty.Utility (Ty.Class ty))
            end
        end
      | Ty.Utility (Ty.Class _ | Ty.Exists)
      | Ty.Bot _
      | Ty.Any _
      | Ty.Top
      | Ty.Union _
      | Ty.Inter _ ->
        return (Ty.Utility (Ty.Class ty))
      | Ty.Bound (bloc, bname) ->
        let pred (loc, name) = name = bname && loc = bloc in
        if List.exists pred env.Env.tparams then
          return (Ty.Utility (Ty.Class (Ty.Bound (bloc, bname))))
        else
          terr ~kind:BadClassT ~msg:"bound" None
      | ty -> terr ~kind:BadClassT ~msg:(Ty_debug.string_of_ctor ty) None
    in
    fun ~env t ps ->
      match (t, Env.get_member_expansion_info env) with
      | (T.DefT (r, _, T.InstanceT (static, _, _, inst)), _)
        when desc_of_reason ~unwrap:false r = RReactComponent ->
        let { Type.own_props; _ } = inst in
        react_component ~env static own_props
      | (_, Some (_, env)) -> type__ ~env:(Env.continue_expanding_members env) t
      | (_, None) ->
        let%bind t = type__ ~env t in
        go ~env ps t

  and this_class_t ~env t ps =
    match Env.get_member_expansion_info env with
    | Some (Env.{ instance_member_expansion_mode = IMUnset; _ }, env) ->
      type__ ~env:(Env.expand_static_members env) t
    | Some (Env.{ instance_member_expansion_mode = IMInstance | IMStatic; _ }, env) ->
      type__ ~env:(Env.continue_expanding_members env) t
    | None ->
      (match%bind type__ ~env t with
      | Ty.Generic (name, Ty.ClassKind, _) -> return (Ty.ClassDecl (name, ps))
      | ty -> terr ~kind:BadThisClassT ~msg:(Ty_debug.string_of_ctor ty) (Some t))

  and poly_ty ~env t typeparams =
    let (env, results) =
      Nel.fold_left
        (fun (env, rs) tp ->
          let r = type_param ~env tp in
          let loc = Reason.def_aloc_of_reason tp.T.reason in
          let name = tp.T.name in
          (Env.add_typeparam env (loc, name), r :: rs))
        (env, [])
        typeparams
    in
    let%bind ps = List.rev results |> all in
    let ps =
      match ps with
      | [] -> None
      | _ -> Some ps
    in
    match t with
    | T.DefT (_, _, T.ClassT t) -> class_t ~env t ps
    | T.ThisClassT (_, t) -> this_class_t ~env t ps
    | T.DefT (r, _, T.TypeT (kind, t)) -> type_t ~env r kind t ps
    | T.DefT (_, _, T.FunT (static, _, f)) ->
      let%map fun_t = fun_ty ~env static f ps in
      Ty.Fun fun_t
    | _ -> terr ~kind:BadPoly (Some t)

  (* Type Aliases *)
  and type_t =
    Type.(
      (* NOTE the use of the reason within `t` instead of the one passed with
       the constructor TypeT. The latter is an RType, which is somewhat more
       unwieldy as it is used more pervasively. *)
      let local env t ta_tparams =
        let reason = TypeUtil.reason_of_t t in
        let%bind symbol = Reason_utils.local_type_alias_symbol env reason in
        let env = Env.{ env with under_type_alias = Some symbol.Ty.sym_name } in
        let%bind ta_type = type__ ~env t in
        return (Ty.named_alias symbol ?ta_tparams ~ta_type)
      in
      let import env r t ps =
        let%bind symbol = Reason_utils.imported_type_alias_symbol env r in
        let env = Env.{ env with under_type_alias = Some symbol.Ty.sym_name } in
        let%bind ty = type__ ~env t in
        match ty with
        | Ty.TypeAlias _ -> terr ~kind:BadTypeAlias ~msg:"nested type alias" None
        | Ty.ClassDecl _
        | Ty.InterfaceDecl _ ->
          (* Normalize imports of the form "import typeof { C } from 'm';" (where C
           is defined as a class/interface in 'm') as a Ty.ClassDecl/InterfaceDecl,
           instead of Ty.TypeAlias.
           The provenance information on the class should point to the defining
           location. This way we avoid the indirection of the import location on
           the alias symbol. *)
          return ty
        | _ -> return (Ty.named_alias symbol ?ta_tparams:ps ~ta_type:ty)
      in
      let opaque env t ps =
        match t with
        | OpaqueT (r, o) -> opaque_type_t ~env r o ps
        | _ -> terr ~kind:BadTypeAlias ~msg:"opaque" (Some t)
      in
      let type_param env r t =
        match desc_of_reason r with
        | RType name ->
          let symbol = symbol_from_reason env r name in
          return (Ty.named_alias symbol)
        | RThisType -> type__ ~env t
        | desc -> terr ~kind:BadTypeAlias ~msg:(spf "type param: %s" (string_of_desc desc)) (Some t)
      in
      fun ~env r kind t ps ->
        match kind with
        | TypeAliasKind -> local env t ps
        | ImportClassKind -> class_t ~env t ps
        | ImportTypeofKind -> import env r t ps
        | OpaqueKind -> opaque env t ps
        | TypeParamKind -> type_param env r t
        (* The following cases are not common *)
        | InstanceKind -> terr ~kind:BadTypeAlias ~msg:"instance" (Some t))

  and exact_t ~env t = type__ ~env t >>| Ty.mk_exact

  and type_app =
    let go ~env ~expand_type_aliases targs = function
      | Ty.ClassDecl (name, _) -> return (generic_class name targs)
      | Ty.InterfaceDecl (name, _) -> return (generic_interface name targs)
      | Ty.TypeAlias { Ty.ta_name; ta_tparams; ta_type } ->
        begin
          match ta_type with
          | Some ta_type when expand_type_aliases ->
            begin
              match Base.Option.both ta_tparams targs with
              | Some (ps, ts) -> Substitution.run ps ts ta_type
              | None -> return ta_type
            end
          | _ ->
            let targs =
              if Env.omit_targ_defaults env then
                remove_targs_matching_defaults targs ta_tparams
              else
                targs
            in
            return (generic_talias ta_name targs)
        end
      | Ty.(Any _ | Bot _ | Top) as ty -> return ty
      (* "Fix" type application on recursive types *)
      | Ty.TVar (Ty.RVar v, None) -> return (Ty.TVar (Ty.RVar v, targs))
      | Ty.Utility (Ty.Class _) as ty when Base.Option.is_none targs -> return ty
      | ty -> terr ~kind:BadTypeApp ~msg:(Ty_debug.string_of_ctor ty) None
    in
    fun ~env t targs ->
      let%bind ty = type__ ~env t in
      let (env, expand_type_aliases) =
        match (Env.expand_type_aliases env, Env.get_member_expansion_info env) with
        | (true, _) -> (env, true)
        | (_, Some (_, env)) -> (env, true)
        | _ -> (env, false)
      in
      let%bind targs = optMapM (type__ ~env) targs in
      go ~env ~expand_type_aliases targs ty

  and opaque_t ~env reason opaque_type =
    let name = opaque_type.Type.opaque_name in
    let opaque_symbol = symbol_from_reason env reason name in
    return (generic_talias opaque_symbol None)

  (* We are being a bit lax here with opaque types so that we don't have to
     introduce a new constructor in Ty.t to support all kinds of OpaqueT.
     If an underlying type is available, then we use that as the alias body.
     If not, we check for a super type and use that if there is one.
     Otherwise, we fall back to a bodyless TypeAlias.
  *)
  and opaque_type_t ~env reason opaque_type ta_tparams =
    Type.(
      let name = opaque_type.opaque_name in
      let current_source = Env.current_file env in
      let opaque_source = ALoc.source (def_aloc_of_reason reason) in
      let opaque_symbol = symbol_from_reason env reason name in
      (* Compare the current file (of the query) and the file that the opaque
       type is defined. If they differ, then hide the underlying/super type.
       Otherwise, display the underlying/super type. *)
      if Some current_source <> opaque_source then
        return (Ty.named_alias ?ta_tparams opaque_symbol)
      else
        let t_opt =
          match opaque_type with
          | { underlying_t = Some t; _ } (* opaque type A = number; *)
          | { super_t = Some t; _ } ->
            Some t (* declare opaque type B: number; *)
          | _ -> None
          (* declare opaque type C; *)
          (* TODO: This will potentially report a remote name.
         The same fix for T25963804 should be applied here as well. *)
        in
        let%map ta_type = option (type__ ~env) t_opt in
        Ty.named_alias ?ta_tparams ?ta_type opaque_symbol)

  and custom_fun_expanded ~env =
    Type.(
      function
      (* Object.assign: (target: any, ...sources: Array<any>): any *)
      | ObjectAssign ->
        return
          Ty.(
            mk_fun
              ~params:[(Some "target", explicit_any, non_opt_param)]
              ~rest:
                ( Some "sources",
                  Arr { arr_readonly = false; arr_literal = false; arr_elt_t = explicit_any } )
              explicit_any)
      (* Object.getPrototypeOf: (o: any): any *)
      | ObjectGetPrototypeOf ->
        return Ty.(mk_fun ~params:[(Some "o", explicit_any, non_opt_param)] explicit_any)
      (* Object.setPrototypeOf: (o: any, p: any): any *)
      | ObjectSetPrototypeOf ->
        let params =
          [(Some "o", Ty.explicit_any, non_opt_param); (Some "p", Ty.explicit_any, non_opt_param)]
        in
        return (mk_fun ~params Ty.explicit_any)
      (* var idx:
       <IdxObject: Any, IdxResult>
       (obj: IdxObject, pathCallback: (demaybefiedObj: IdxObject) => IdxResult)
       => ?IdxResult;
    *)
      | Idx ->
        let idxObject = Ty.Bound (ALoc.none, "IdxObject") in
        let idxResult = Ty.Bound (ALoc.none, "IdxResult") in
        let tparams = [mk_tparam ~bound:Ty.explicit_any "IdxObject"; mk_tparam "IdxResult"] in
        let pathCallback =
          mk_fun ~params:[(Some "demaybefiedObj", idxObject, non_opt_param)] idxResult
        in
        let params =
          [
            (Some "obj", idxObject, non_opt_param);
            (Some "pathCallback", pathCallback, non_opt_param);
          ]
        in
        return (mk_fun ~tparams ~params (Ty.mk_maybe idxResult))
      (* var TypeAssertIs: <TypeAssertT>(value: mixed) => boolean *)
      | TypeAssertIs ->
        let tparams = [mk_tparam "TypeAssertT"] in
        let params = [(Some "value", Ty.Top, non_opt_param)] in
        return (mk_fun ~tparams ~params (Ty.Bool None))
      (*  var TypeAssertThrows: <TypeAssertT>(value: mixed) => TypeAssertT *)
      | TypeAssertThrows ->
        let tparams = [mk_tparam "TypeAssertT"] in
        let params = [(Some "value", Ty.Top, non_opt_param)] in
        let ret = Ty.Bound (ALoc.none, "TypeAssertT") in
        return (mk_fun ~tparams ~params ret)
      (* Result<T> = {success: true, value: T} | {success: false, error: string}
      var TypeAssertWraps: <TypeAssertT>(value: mixed) => Result<TypeAssertT> *)
      | TypeAssertWraps ->
        let tparams = [mk_tparam "TypeAssertT"] in
        let params = [(Some "value", Ty.Top, non_opt_param)] in
        let result_fail_ty =
          Ty.mk_object
            (Ty.mk_field_props
               [("success", Ty.BoolLit false, false); ("error", Ty.Str None, false)])
        in
        let result_succ_ty =
          Ty.mk_object
            (Ty.mk_field_props
               [("success", Ty.BoolLit true, false); ("value", builtin_t "TypeAssertT", false)])
        in
        let ret = Ty.mk_union (result_fail_ty, [result_succ_ty]) in
        return (mk_fun ~tparams ~params ret)
      (* debugPrint: (_: any[]) => void *)
      | DebugPrint ->
        return
          Ty.(
            mk_fun
              ~params:
                [
                  ( Some "_",
                    Arr { arr_readonly = false; arr_literal = false; arr_elt_t = explicit_any },
                    non_opt_param );
                ]
              Void)
      (* debugThrow: () => empty *)
      | DebugThrow -> return (mk_fun (mk_empty Ty.EmptyType))
      (* debugSleep: (seconds: number) => void *)
      | DebugSleep -> return Ty.(mk_fun ~params:[(Some "seconds", Num None, non_opt_param)] Void)
      (* reactPropType: any (TODO) *)
      | ReactPropType _ -> return Ty.explicit_any
      (* reactCreateClass: (spec: any) => ReactClass<any> *)
      | ReactCreateClass ->
        let params = [(Some "spec", Ty.explicit_any, non_opt_param)] in
        let x = Ty.builtin_symbol "ReactClass" in
        return (mk_fun ~params (generic_talias x (Some [Ty.explicit_any])))
      (*
       * 1. Component class:
       *    <T>(name: ReactClass<T>, config: T, children?: any) => React$Element<T>
       *
       * 2. Stateless functional component
       *    type SFC<T> = (config: T, context: any) => React$Element<T>
       *    <T>(fn: SFC<T>, config: T, children?: any) => React$Element<T>
       *)
      | ReactCreateElement
      | ReactCloneElement
      | ReactElementFactory _ ->
        return
          Ty.(
            let param_t = mk_tparam "T" in
            let tparams = [param_t] in
            let t = Bound (ALoc.none, "T") in
            let params =
              [
                (Some "name", generic_builtin_t "ReactClass" [t], non_opt_param);
                (Some "config", t, non_opt_param);
                (Some "children", explicit_any, opt_param);
              ]
            in
            let reactElement = generic_builtin_t "React$Element" [t] in
            let f1 = mk_fun ~tparams ~params reactElement in
            let params =
              [(Some "config", t, non_opt_param); (Some "context", explicit_any, non_opt_param)]
            in
            let sfc = mk_fun ~tparams ~params reactElement in
            let params =
              [
                (Some "fn", sfc, non_opt_param);
                (Some "config", t, non_opt_param);
                (Some "children", explicit_any, opt_param);
              ]
            in
            let f2 = mk_fun ~tparams ~params reactElement in
            mk_inter (f1, [f2]))
      (* Fallback *)
      | t -> custom_fun_short ~env t)

  and custom_fun_short ~env =
    Type.(
      function
      | ObjectAssign -> return (builtin_t "Object$Assign")
      | ObjectGetPrototypeOf -> return (builtin_t "Object$GetPrototypeOf")
      | ObjectSetPrototypeOf -> return (builtin_t "Object$SetPrototypeOf")
      | Compose false -> return (builtin_t "$Compose")
      | Compose true -> return (builtin_t "$ComposeReverse")
      | ReactPropType t -> react_prop_type ~env t
      | ReactCreateClass -> return (builtin_t "React$CreateClass")
      | ReactCreateElement -> return (builtin_t "React$CreateElement")
      | ReactCloneElement -> return (builtin_t "React$CloneElement")
      | ReactElementFactory t ->
        let%map t = type__ ~env t in
        generic_builtin_t "React$ElementFactory" [t]
      | Idx -> return (builtin_t "$Facebookism$Idx")
      (* var TypeAssertIs: <TypeAssertT>(value: mixed) => boolean *)
      | TypeAssertIs ->
        let tparams = [mk_tparam "TypeAssertT"] in
        let params = [(Some "value", Ty.Top, non_opt_param)] in
        return (mk_fun ~tparams ~params (Ty.Bool None))
      (*  var TypeAssertThrows: <TypeAssertT>(value: mixed) => TypeAssertT *)
      | TypeAssertThrows ->
        let tparams = [mk_tparam "TypeAssertT"] in
        let params = [(Some "value", Ty.Top, non_opt_param)] in
        let ret = builtin_t "TypeAssertT" in
        return (mk_fun ~tparams ~params ret)
      (* Result<T> = {success: true, value: T} | {success: false, error: string}
      var TypeAssertWraps: <TypeAssertT>(value: mixed) => Result<TypeAssertT> *)
      | TypeAssertWraps ->
        let tparams = [mk_tparam "TypeAssertT"] in
        let params = [(Some "value", Ty.Top, non_opt_param)] in
        let result_fail_ty =
          Ty.mk_object
            (Ty.mk_field_props
               [("success", Ty.BoolLit false, false); ("error", Ty.Str None, false)])
        in
        let result_succ_ty =
          Ty.mk_object
            (Ty.mk_field_props
               [("success", Ty.BoolLit true, false); ("value", builtin_t "TypeAssertT", false)])
        in
        let ret = Ty.mk_union (result_fail_ty, [result_succ_ty]) in
        return (mk_fun ~tparams ~params ret)
      | DebugPrint -> return (builtin_t "$Flow$DebugPrint")
      | DebugThrow -> return (builtin_t "$Flow$DebugThrow")
      | DebugSleep -> return (builtin_t "$Flow$DebugSleep"))

  and custom_fun ~env t =
    if Env.expand_internal_types env then
      custom_fun_expanded ~env t
    else
      custom_fun_short ~env t

  and react_prop_type ~env =
    T.React.PropType.(
      function
      | Primitive (is_req, t) ->
        let%map t = type__ ~env t in
        generic_builtin_t
          ( if is_req then
            "React$PropType$Primitive$Required"
          else
            "React$PropType$Primitive" )
          [t]
      | Complex ArrayOf -> return (builtin_t "React$PropType$ArrayOf")
      | Complex InstanceOf -> return (builtin_t "React$PropType$ArrayOf")
      | Complex ObjectOf -> return (builtin_t "React$PropType$dbjectOf")
      | Complex OneOf -> return (builtin_t "React$PropType$OneOf")
      | Complex OneOfType -> return (builtin_t "React$PropType$OneOfType")
      | Complex Shape -> return (builtin_t "React$PropType$Shape"))

  and internal_t t =
    Type.(
      function
      | ChoiceKitT _
      | ExtendsT _
      | ReposUpperT _ ->
        terr ~kind:BadInternalT (Some t))

  and param_bound ~env = function
    | T.DefT (_, _, T.MixedT _) -> return None
    | bound ->
      let%bind b = type__ ~env bound in
      return (Some b)

  and default_t ~env = function
    | Some d ->
      let%bind d = type__ ~env d in
      return (Some d)
    | _ -> return None

  and type_param ~env tp =
    let { T.name; bound; polarity; default; _ } = tp in
    let tp_polarity = type_polarity polarity in
    let%bind tp_bound = param_bound ~env bound in
    let%map tp_default = default_t ~env default in
    { Ty.tp_name = name; tp_bound; tp_polarity; tp_default }

  and opt_t ~env t =
    let (t, opt) =
      match t with
      | T.OptionalT { reason = _; type_ = t; use_desc = _ } -> (t, true)
      | t -> (t, false)
    in
    let%map t = type__ ~env t in
    (t, opt)

  and type_polarity = function
    | Polarity.Positive -> Ty.Positive
    | Polarity.Negative -> Ty.Negative
    | Polarity.Neutral -> Ty.Neutral

  (************)
  (* EvalT    *)
  (************)
  and spread =
    let spread_of_ty = function
      | Ty.Obj { Ty.obj_props; _ } -> obj_props
      | t -> [Ty.SpreadProp t]
    in
    let obj_exact target =
      match target with
      | Type.Object.Spread.(Annot { make_exact }) -> return make_exact
      | Type.Object.Spread.Value _ -> terr ~kind:BadEvalT ~msg:"spread-target-value" None
    in
    let mk_spread ty target prefix_tys head_slice =
      let obj_props = prefix_tys @ spread_of_ty ty in
      let obj_props =
        match head_slice with
        | None -> obj_props
        | Some obj -> obj_props @ spread_of_ty obj
      in
      let%map obj_exact = obj_exact target in
      Ty.Obj { Ty.obj_props; obj_exact; obj_literal = false; obj_frozen = false (* default *) }
    in
    let spread_operand_slice ~env { T.Object.Spread.reason = _; prop_map; dict } =
      Type.TypeTerm.(
        let obj_exact = true in
        let obj_frozen = false in
        let obj_literal = false in
        let props = SMap.fold (fun k p acc -> (k, p) :: acc) prop_map [] in
        let%bind obj_props = concat_fold_m (obj_prop ~env) props in
        let%bind obj_props =
          match dict with
          | Some { key; value; dict_name; dict_polarity } ->
            let%bind dict_key = type__ ~env key in
            let%bind dict_value = type__ ~env value in
            return
              ( Ty.IndexProp
                  {
                    Ty.dict_polarity = type_polarity dict_polarity;
                    dict_name;
                    dict_key;
                    dict_value;
                  }
              :: obj_props )
          | None -> return obj_props
        in
        return (Ty.Obj { Ty.obj_exact; obj_frozen; obj_literal; obj_props }))
    in
    let spread_operand ~env = function
      | T.Object.Spread.Type t -> type__ ~env t
      | T.Object.Spread.Slice slice -> spread_operand_slice ~env slice
    in
    fun ~env ty target ts_rev head_slice ->
      let%bind head_slice =
        match head_slice with
        | None -> return None
        | Some s ->
          let%bind s = spread_operand_slice ~env s in
          return (Some s)
      in
      let%bind tys_rev = mapM (spread_operand ~env) ts_rev in
      let prefix_tys =
        List.fold_left (fun acc t -> List.rev_append (spread_of_ty t) acc) [] tys_rev
      in
      mk_spread ty target prefix_tys head_slice

  and type_destructor_t ~env use_op reason id t d =
    if Env.evaluate_type_destructors env then
      let cx = Env.get_cx env in
      Context.with_normalizer_mode cx (fun cx ->
          (* The evaluated type might be recursive. To avoid non-termination we
           * wrap the evaluation with our caching mechanism. *)
          Recursive.with_cache (EvalKey id) ~f:(fun () ->
              let trace = Trace.dummy_trace in
              match Flow_js.mk_type_destructor cx ~trace use_op reason t d id with
              | exception Flow_js.Attempted_operation_on_bound _ ->
                type_destructor_unevaluated ~env t d
              | (_, tout) -> type__ ~env tout))
    else
      type_destructor_unevaluated ~env t d

  and type_destructor_unevaluated ~env t d =
    let%bind ty = type__ ~env t in
    match d with
    | T.NonMaybeType -> return (Ty.Utility (Ty.NonMaybeType ty))
    | T.ReadOnlyType -> return (Ty.Utility (Ty.ReadOnly ty))
    | T.ValuesType -> return (Ty.Utility (Ty.Values ty))
    | T.ElementType t' ->
      let%map ty' = type__ ~env t' in
      Ty.Utility (Ty.ElementType (ty, ty'))
    | T.CallType ts ->
      let%map tys = mapM (type__ ~env) ts in
      Ty.Utility (Ty.Call (ty, tys))
    | T.TypeMap (T.ObjectMap t') ->
      let%map ty' = type__ ~env t' in
      Ty.Utility (Ty.ObjMap (ty, ty'))
    | T.TypeMap (T.ObjectMapi t') ->
      let%map ty' = type__ ~env t' in
      Ty.Utility (Ty.ObjMapi (ty, ty'))
    | T.PropertyType k -> return (Ty.Utility (Ty.PropertyType (ty, Ty.StrLit k)))
    | T.TypeMap (T.TupleMap t') ->
      let%map ty' = type__ ~env t' in
      Ty.Utility (Ty.TupleMap (ty, ty'))
    | T.RestType (T.Object.Rest.Sound, t') ->
      let%map ty' = type__ ~env t' in
      Ty.Utility (Ty.Rest (ty, ty'))
    | T.RestType (T.Object.Rest.IgnoreExactAndOwn, t') ->
      let%map ty' = type__ ~env t' in
      Ty.Utility (Ty.Diff (ty, ty'))
    | T.SpreadType (target, operands, head_slice) -> spread ~env ty target operands head_slice
    | T.ReactElementPropsType -> return (Ty.Utility (Ty.ReactElementPropsType ty))
    | T.ReactElementConfigType -> return (Ty.Utility (Ty.ReactElementConfigType ty))
    | T.ReactElementRefType -> return (Ty.Utility (Ty.ReactElementRefType ty))
    | T.ReactConfigType default_props ->
      let%map default_props' = type__ ~env default_props in
      Ty.Utility (Ty.ReactConfigType (ty, default_props'))
    | (T.RestType (T.Object.Rest.ReactConfigMerge _, _) | T.Bind _) as d ->
      terr ~kind:BadEvalT ~msg:(Debug_js.string_of_destructor d) None

  and latent_pred_t ~env id t =
    let cx = Env.get_cx env in
    let evaluated = Context.evaluated cx in
    let t' =
      match T.Eval.Map.find_opt id evaluated with
      | Some evaled_t -> evaled_t
      | None -> t
    in
    type__ ~env t'

  and eval_t ~env t id = function
    | Type.LatentPredT _ -> latent_pred_t ~env id t
    | Type.TypeDestructorT (use_op, r, d) -> type_destructor_t ~env use_op r id t d

  and module_t =
    let mk_module env symbol_opt exports =
      let cjs_export = optM (type__ ~env) T.(exports.cjs_export) in
      let exports =
        Context.find_exports Env.(env.genv.cx) T.(exports.exports_tmap)
        |> SMap.map snd
        |> SMap.bindings
        |> mapM (type__ ~env |> sndMapM)
      in
      let%bind exports = exports in
      let%map cjs_export = cjs_export in
      Ty.Module (symbol_opt, Ty.{ exports; cjs_export })
    in
    fun env reason exports t ->
      match desc_of_reason reason with
      | RModule name
      | RCommonJSExports name
      | RUntypedModule name ->
        let symbol = symbol_from_reason env reason name in
        mk_module env (Some symbol) exports
      | RExports -> mk_module env None exports
      | _ -> terr ~kind:UnsupportedTypeCtor (Some t)

  and uses_t =
    let rec uses_t_aux ~env acc uses =
      match uses with
      | [] ->
        begin
          match acc with
          | [] -> return Ty.NoUpper
          | hd :: tl -> return (Ty.SomeKnownUpper (Ty.mk_inter (hd, tl)))
        end
      | T.UseT (_, t) :: rest
      | T.TypeCastT (_, t) :: rest ->
        let%bind t = type__ ~env t in
        uses_t_aux ~env (t :: acc) rest
      | T.ReposLowerT (_, _, u) :: rest -> uses_t_aux ~env acc (u :: rest)
      (* skip these *)
      | T.CJSExtractNamedExportsT _ :: rest -> uses_t_aux ~env acc rest
      | u :: _ -> return (Ty.SomeUnknownUpper (T.string_of_use_ctor u))
    in
    (fun ~env uses -> uses_t_aux ~env [] uses)

  and merged_t ~env uses =
    match%bind uses_t ~env uses with
    | Ty.SomeUnknownUpper _ ->
      (* un-normalizable *)
      terr ~kind:BadUse None
    | Ty.NoUpper ->
      (* shouldn't happen - MergedT has at least one use by construction *)
      return (mk_empty (Ty.NoLowerWithUpper Ty.NoUpper))
    | Ty.SomeKnownUpper t ->
      (* return the recorded use type *)
      return t

  let run_type ~options ~genv ~imported_names ~tparams state t =
    let env = Env.init ~options ~genv ~tparams ~imported_names in
    let (result, state) = run state (type__ ~env t) in
    let result =
      match result with
      | Ok t when options.Env.optimize_types ->
        let { Env.merge_bot_and_any_kinds = merge_kinds; _ } = options in
        Ok (Ty_utils.simplify_type ~merge_kinds ~sort:false t)
      | _ -> result
    in
    (result, state)

  (* Before we start normalizing the input type we populate our environment with
     aliases that are in scope due to typed imports. These appear inside
     File_sig.module_sig.requires. This step includes the normalization
     of all imported types and the creation of a map to hold bindings of imported
     names to location of definition. This map will be used later to determine
     whether a located name (symbol) appearing is part of the file's imports or a
     remote (hidden or non-imported) name.
  *)
  module Imports = struct
    open File_sig

    (* Collect the names and locations of types that are available as we scan
     * the imports. Later we'll match them with some remote defining loc. *)
    type acc_t = Ty.imported_ident list

    let from_imported_locs_map ~import_mode map (acc : acc_t) =
      SMap.fold
        (fun _remote remote_map acc ->
          SMap.fold
            (fun local imported_locs_nel acc ->
              Nel.fold_left
                (fun acc { local_loc; _ } -> (local_loc, local, import_mode) :: acc)
                acc
                imported_locs_nel)
            remote_map
            acc)
        map
        acc

    let rec from_binding ~import_mode binding (acc : acc_t) =
      match binding with
      | BindIdent (loc, name) -> (loc, name, import_mode) :: acc
      | BindNamed map ->
        List.fold_left (fun acc (_, binding) -> from_binding ~import_mode binding acc) acc map

    let from_bindings ~import_mode bindings_opt acc =
      match bindings_opt with
      | Some bindings -> from_binding ~import_mode bindings acc
      | None -> acc

    let from_require require (acc : acc_t) =
      match require with
      | Require { source = _; require_loc = _; bindings } ->
        from_bindings ~import_mode:Ty.ValueMode bindings acc
      | Import { import_loc = _; source = _; named; ns = _; types; typesof; typesof_ns = _ } ->
        (* TODO import namespaces (`ns`) as modules that might contain imported types *)
        acc
        |> from_imported_locs_map ~import_mode:Ty.ValueMode named
        |> from_imported_locs_map ~import_mode:Ty.TypeMode types
        |> from_imported_locs_map ~import_mode:Ty.TypeofMode typesof
      | ImportDynamic _
      | Import0 _ ->
        acc

    let extract_imported_idents requires =
      List.fold_left (fun acc require -> from_require require acc) [] requires

    let extract_schemes typed_ast (imported_locs : acc_t) =
      List.fold_left
        (fun acc (loc, name, import_mode) ->
          match Typed_ast_utils.find_exact_match_annotation typed_ast loc with
          | Some scheme -> (name, loc, import_mode, scheme) :: acc
          | None -> acc)
        []
        imported_locs

    let extract_ident ~options ~genv scheme =
      let { Type.TypeScheme.tparams; type_ = t } = scheme in
      let imported_names = ALocMap.empty in
      let env = Env.init ~options ~genv ~tparams ~imported_names in
      match%map type__ ~env t with
      | Ty.TypeAlias { Ty.ta_name = { Ty.sym_def_loc; _ }; _ }
      | Ty.ClassDecl ({ Ty.sym_def_loc; _ }, _)
      | Ty.InterfaceDecl ({ Ty.sym_def_loc; _ }, _) ->
        Some sym_def_loc
      | Ty.Utility (Ty.Class (Ty.Generic ({ Ty.sym_def_loc; _ }, _, None))) ->
        (* This is an acceptable proxy only if the class is not polymorphic *)
        Some sym_def_loc
      | _ -> None

    let normalize_imports ~options ~genv imported_schemes : Ty.imported_ident ALocMap.t =
      let state = State.empty in
      let (_, result) =
        List.fold_left
          (fun (st, acc) (name, loc, import_mode, scheme) ->
            match run st (extract_ident ~options ~genv scheme) with
            | (Ok (Some def_loc), st) -> (st, ALocMap.add def_loc (loc, name, import_mode) acc)
            | (Ok None, st) ->
              (* unrecognizable remote type *)
              (st, acc)
            | (Error _, st) ->
              (* normalization error *)
              (st, acc))
          (state, ALocMap.empty)
          imported_schemes
      in
      result
  end

  let run_imports ~options ~genv =
    Imports.(
      let { Env.file_sig = { File_sig.module_sig = { File_sig.requires; _ }; _ }; typed_ast; _ } =
        genv
      in
      extract_imported_idents requires
      |> extract_schemes typed_ast
      |> normalize_imports ~options ~genv)
end

open NormalizerMonad

(* Exposed API *)

let print_normalizer_banner env =
  if env.Env.verbose_normalizer then
    let banner =
      "\n========================================"
      ^ " Normalization "
      ^ "=======================================\n"
    in
    prerr_endlinef "%s" banner

let from_schemes ~options ~genv schemes =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (_, result) =
    ListUtils.fold_map
      (fun state (a, scheme) ->
        let { Type.TypeScheme.tparams; type_ = t } = scheme in
        match run_type ~options ~genv ~imported_names ~tparams state t with
        | (Ok t, state) -> (state, (a, Ok t))
        | (Error s, state) -> (state, (a, Error s)))
      State.empty
      schemes
  in
  result

let from_types ~options ~genv ts =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (_, result) =
    ListUtils.fold_map
      (fun state (a, t) ->
        match run_type ~options ~genv ~imported_names ~tparams:[] state t with
        | (Ok t, state) -> (state, (a, Ok t))
        | (Error s, state) -> (state, (a, Error s)))
      State.empty
      ts
  in
  result

let from_scheme ~options ~genv scheme =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let { Type.TypeScheme.tparams; type_ = t } = scheme in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams State.empty t in
  result

let from_type ~options ~genv t =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams:[] State.empty t in
  result

let fold_hashtbl ~options ~genv ~f ~g ~htbl init =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (result, _) =
    Hashtbl.fold
      (fun loc x (acc, state) ->
        let { Type.TypeScheme.tparams; type_ = t } = g x in
        let (result, state) = run_type ~options ~genv ~imported_names ~tparams state t in
        (f acc (loc, result), state))
      htbl
      (init, State.empty)
  in
  result
