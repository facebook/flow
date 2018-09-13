(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pervasives
open Utils_js
open Reason

module Env = Ty_normalizer_env
module T = Type
module VSet = ISet

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
  | BadPoly
  | BadTypeAlias
  | BadTypeApp
  | BadInternalT
  | BadInstanceT
  | BadEvalT
  | BadUse
  | ShadowTypeParam
  | UnsupportedTypeCtor
  | UnsupportedUseCtor

type error = error_kind * string

let error_kind_to_string = function
  | BadMethodType -> "Bad method type"
  | BadBoundT -> "Unbound type parameter"
  | BadCallProp -> "Bad call property"
  | BadClassT -> "Bad class"
  | BadPoly -> "Bad polymorphic type"
  | BadTypeAlias -> "Bad type alias"
  | BadTypeApp -> "Bad type application"
  | BadInternalT -> "Bad internal type"
  | BadInstanceT -> "Bad instance type"
  | BadEvalT -> "Bad eval"
  | BadUse -> "Bad use"
  | ShadowTypeParam -> "Shadowed type parameters"
  | UnsupportedTypeCtor -> "Unsupported type constructor"
  | UnsupportedUseCtor -> "Unsupported use constructor"

let error_to_string (kind, msg) =
  spf "[%s] %s" (error_kind_to_string kind) msg


module NormalizerMonad : sig

  module State : sig
    type t
    val empty: t
  end

  val run_type :
    options:Env.options ->
    genv:Env.genv ->
    imported_names:Loc.t SMap.t ->
    tparams:Type.typeparam list ->
    State.t ->
    Type.t ->
    (Ty.t, error) result * State.t

  val run_imports:
    options:Env.options ->
    genv:Env.genv ->
    State.t ->
    Loc.t SMap.t * State.t

end = struct

  module State = struct

    type t = {

      (* Source of fresh ints for creating new Ty.tvar's *)
      counter: int;

      (* A cache for resolved type variables.

         We cache the result even when the output is an error, to avoid
         recomputing tvar resolution. This is mostly useful for the batch call
         (`from_types`).

         The key to this map is the Type.tvar `ident`.
      *)
      tvar_cache: (Ty.t, error) result IMap.t;

      (* A cache for resolved EvalTs

         It is important to cache these results because we should only invoke
         Flow_js.evaluate_type_destructor once for every EvalT. If we fail to do
         so the result for calls that result to exceptions will not be cached in
         Context.evaluated and subsequent calls may result to different results.
         In particular we might get Empty instead of an exception.
      *)
      eval_t_cache: (Ty.t, error) result IMap.t;

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

    let empty = {
      counter = 0;
      tvar_cache = IMap.empty;
      eval_t_cache = IMap.empty;
      free_tvars = VSet.empty;
    }

  end

  include StateResult.Make(State)

  (* Monadic helper functions *)
  let mapM f xs = all (List.map f xs)
  let concat_fold_m f xs = mapM f xs >>| List.concat

  let fresh_num =
    let open State in
    get >>= fun st ->
    let n = st.counter in
    put { st with counter = n + 1 } >>| fun _ -> n

  let terr ~kind ?msg t =
    let t_str = Option.map t
      ~f:(fun t -> spf "Raised on type: %s" (Type.string_of_ctor t))
    in
    let msg = ListUtils.cat_maybes [msg; t_str] |> String.concat "\n" in
    error (kind, msg)


  (* Type caches *)

  let update_tvar_cache i t =
    let open State in
    get >>= fun st ->
    let tvar_cache = IMap.add i t st.tvar_cache in
    put { st with tvar_cache }

  let find_tvar root_id =
    let open State in
    get >>| fun st ->
    IMap.get root_id st.tvar_cache

  let update_eval_t_cache i t =
    let open State in
    get >>= fun st ->
    let eval_t_cache = IMap.add i t st.eval_t_cache in
    put { st with eval_t_cache }

  let find_eval_t id =
    let open State in
    get >>| fun st ->
    IMap.get id st.eval_t_cache


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
    let open Type in
    let pred { name; reason; _ } = (
      name = tp_name &&
      Reason.def_loc_of_reason reason = tp_loc
    ) in
    match List.find_opt pred env.Env.tparams with
    | Some _ ->
      (* If we care about shadowing of type params, then flag an error *)
      if Env.flag_shadowed_type_params env then
        let shadow_pred { name; _ } = (name = tp_name) in
        match List.find_opt shadow_pred env.Env.tparams with
        | Some { reason; _ }
          when Reason.def_loc_of_reason reason <> tp_loc ->
          terr ~kind:ShadowTypeParam (Some t)
        | Some _ ->
          return Ty.(Bound (Symbol (Local tp_loc, tp_name)))
        | None -> assert false
      else
        return Ty.(Bound (Symbol (Local tp_loc, tp_name)))
    | None ->
      default t



  (**************)
  (* Type ops   *)
  (**************)

  (* Simplify union/intersection types

     This visitor:
     - removes identical nodes from union and intersection types. (At the moment
       the comparison used is `Pervasives.compare`, but perhaps something more
       clever can replace this.)
     - removes the neutral element for union (resp. intersection) types, which
       is the bottom (resp. top) type.

     The Any state of this visitor is used to capture any change to the type
     structure.
  *)
  let simplify_unions_inters =
    let open Ty in
    let simplify_zero_one ~zero ~one =
      let rec simplify_aux acc = function
      | [] -> acc
      | t::ts ->
        if t = zero then [t]
        else if t = one then simplify_aux acc ts
        else simplify_aux (t::acc) ts
      in
      simplify_aux []
    in
    let o = object (self)
      inherit [_] endo_ty
      method private simplify env ~break ~zero ~one ~make ts =
        let ts' = self#on_list self#on_t env ts in
        let ts' = List.concat (List.map break ts') in
        let ts' = List.sort Pervasives.compare ts' in
        let ts' = ListUtils.uniq ts' in
        let ts' = simplify_zero_one ~zero ~one ts' in
        if List.length ts = List.length ts'
        then None (* no change *)
        else Some (make ts')

      method! on_t env t =
        match t with
        | Union (t0,t1,ts) ->
          let opt = self#simplify ~break:Ty.bk_union ~zero:Ty.Top
            ~one:Ty.Bot ~make:Ty.mk_union env (t0::t1::ts) in
          Option.value ~default:t opt
        | Inter (t0,t1,ts) ->
          let opt = self#simplify ~break:Ty.bk_inter ~zero:Ty.Bot
            ~one:Ty.Top ~make:Ty.mk_inter env (t0::t1::ts) in
          Option.value ~default:t opt
        (* WARNING: do not descend to other constructors or this will get slow *)
        | _ -> t
    end in
    let rec go t =
      let t' = o#on_t () t in
      if t == t' then t else go t'
    in
    fun t -> go t

  (* We wrap the union and intersection constructors with the following
     functions that keep types as small as possible.
  *)
  let uniq_union ts = ts |> Ty.mk_union |> simplify_unions_inters
  let uniq_inter ts = ts |> Ty.mk_inter |> simplify_unions_inters



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
    let subst  =
      let o = Ty.(object
        inherit [_] map_ty as super
        method! on_t env t =
          let t = match t, env with
          | TVar (i, ts), (r, sym) when r = i -> Generic (sym, true, ts)
          | _ -> t in
          super#on_t env t
      end) in
      fun r sym t ->
        o#on_t (r, sym) t

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
      let open Ty in
      if definitely_appears || Ty_utils.appears_in_t ~is_top:true i t then
        match t with
        | TypeAlias { ta_name; _ } -> subst (RVar i) ta_name t
        | _ -> Mu (i, t)
      else t

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
      let open Ty in
      let o = object (self)
        inherit [_] endo_ty
        method env_zero =
          function `Union -> Bot | `Inter -> Top
        method! on_t env t =
          match env, t with
          | (v, _), Union (t0,t1,ts) ->
            let ts = t0::t1::ts in
            let ts' = self#on_list self#on_t (v, `Union) ts in
            if ts == ts' then t else Ty.mk_union ts'
          | (v, _), Inter (t0,t1,ts) ->
            let ts = t0::t1::ts in
            let ts' = self#on_list self#on_t (v, `Inter) ts in
            if ts == ts' then t else Ty.mk_inter ts'
          | (v, mode), TVar (Ty.RVar v', _) when v = v' ->
            self#env_zero mode
          | _, Mu (v, rt) ->
            let rt' = self#on_t env rt in
            if rt == rt' then t
            else mk_mu ~definitely_appears:false v rt'
          | _, _ -> t
      end in
      fun v t -> o#on_t (v, `Union) t


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
      if not (VSet.mem i free_vars) then t else
      (* Recursive, but still might be a degenerate case *)
      let t' = remove_toplevel_tvar i t in
      let changed = not (t == t') in
      let t' = if changed then simplify_unions_inters t' else t' in
      (* If not changed then all free_vars are still in, o.w. recompute free vars *)
      mk_mu ~definitely_appears:(not changed) i t'
  end


  module Substitution = struct
    open Ty

    let init_env tparams types =
      let rec step acc = function
      | [], _ | _, [] -> acc
      | p::ps, t::ts -> step (SMap.add p.tp_name t acc) (ps, ts)
      in
      step SMap.empty (tparams, types)

    let remove_params env = function
    | None -> env
    | Some ps -> List.fold_left (fun e p -> SMap.remove p.tp_name e) env ps

    let visitor = object
      inherit [_] endo_ty as super
      method! on_t env t =
        match t with
        | TypeAlias { ta_tparams = ps; _ }
        | Fun { fun_type_params = ps; _ } ->
          let env = remove_params env ps in
          super#on_t env t
        | Bound (Symbol (_, name)) ->
          let t' = Option.value (SMap.get name env) ~default:t in
          super#on_t env t'
        | _ ->
          super#on_t env t
    end
      (* Replace a list of type parameters with a list of types in the given type.
       * These lists might not match exactly in length. *)
    let run vs ts t =
      let env = init_env vs ts in
      let t' = visitor#on_t env t in
      if t != t' then simplify_unions_inters t' else t'
  end

  (***********************)
  (* Construct built-ins *)
  (***********************)

  let opt_param = Ty.({ prm_optional = true })
  let non_opt_param = Ty.({ prm_optional = false })

  let mk_fun ?(params=[]) ?rest ?tparams ret = Ty.(
    Fun {
      fun_params = params;
      fun_rest_param = rest;
      fun_return = ret;
      fun_type_params = tparams;
    }
  )

  let mk_tparam ?bound ?(pol=Ty.Neutral) ?default name = Ty.({
    tp_name = name;
    tp_bound = bound;
    tp_polarity = pol;
    tp_default = default;
  })

  let symbol_from_loc env loc name =
    let open File_key in
    let symbol_source = Loc.source loc in
    let provenance =
      match symbol_source with
      | Some LibFile _ -> Ty.Library loc
      | Some (SourceFile _) ->
        let current_source = Env.(env.genv.file) in
        (* Locally defined name *)
        if Some current_source = symbol_source
        then Ty.Local loc
        (* Otherwise it is one of:
           - Imported, or
           - Remote (defined in a different file but not imported in this one) *)
        else (match SMap.get name env.Env.imported_names with
          | Some loc' when loc = loc' -> Ty.Imported loc
          | _ -> Ty.Remote loc)
      | Some (JsonFile _)
      | Some (ResourceFile _) -> Ty.Local loc
      | Some Builtins -> Ty.Builtin
      | None -> Ty.Local loc
    in
    Ty.Symbol (provenance, name)

  (* TODO due to repositioninig `reason_loc` may not point to the actual
     location where `name` was defined. *)
  let symbol_from_reason env reason name =
    let def_loc = Reason.def_loc_of_reason reason in
    symbol_from_loc env def_loc name


  (*************************)
  (* Main transformation   *)
  (*************************)

  let rec type__ ~env t = type_poly ~env t

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
      let loc = Reason.def_loc_of_reason reason in
      let default t = type_with_alias_reason ~env t in
      lookup_tparam ~default env t name loc
    | _ ->
      type_with_alias_reason ~env t

  and type_with_alias_reason ~env t =
    if Env.expand_type_aliases env then type_after_reason ~env t else
    let reason = Type.reason_of_t t in
    let open Type in
    (* These type are treated as transparent when it comes to the type alias
       annotation. *)
    match t with
    | OpenT _ | EvalT _ ->
      type_after_reason ~env t
    | _ ->
      begin match desc_of_reason ~unwrap:false reason with
      | RTypeAlias (name, true, _) ->
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
        if continue then type_after_reason ~env t else
        let symbol = symbol_from_reason env reason name in
        return (Ty.named_t symbol)
      | _ ->
        (* We are now beyond the point of the one-off expansion. Reset the environment
           assigning None to under_type_alias, so that aliases are used in subsequent
           invocations. *)
        let env = Env.{ env with under_type_alias = None } in
        type_after_reason ~env t
      end

  and type_after_reason ~env t =
    let open Type in
    let env = Env.descend env in
    match t with
    | OpenT (_, id) -> type_variable ~env id
    | BoundT (reason, name, _) -> bound_t ~env reason name
    | AnnotT (_, t, _) -> type__ ~env t
    | EvalT (t, d, id) -> eval_t ~env t id d
    | ExactT (_, t) -> exact_t ~env t
    | CustomFunT (_, f) -> custom_fun ~env f
    | InternalT i -> internal_t ~env t i
    | MatchingPropT _ -> return Ty.Bot
    | AnyWithUpperBoundT t ->
      type__ ~env t >>| fun ty ->
      Ty.generic_builtin_t "$Subtype" [ty]
    | AnyWithLowerBoundT t ->
      type__ ~env t >>| fun ty ->
      Ty.generic_builtin_t "$Supertype" [ty]
    | DefT (_, MixedT _) -> return Ty.Top
    | DefT (_, AnyT) -> return Ty.Any
    | DefT (_, AnyObjT) -> return Ty.AnyObj
    | DefT (_, AnyFunT) -> return Ty.AnyFun
    | DefT (_, VoidT) -> return Ty.Void
    | DefT (_, NumT _) -> return Ty.Num
    | DefT (_, StrT _) -> return Ty.Str
    | DefT (_, BoolT _) -> return Ty.Bool
    | DefT (_, EmptyT) -> return Ty.Bot
    | DefT (_, NullT) -> return Ty.Null
    | DefT (_, SingletonNumT (_, lit)) -> return (Ty.NumLit lit)
    | DefT (_, SingletonStrT lit) -> return (Ty.StrLit lit)
    | DefT (_, SingletonBoolT lit) -> return (Ty.BoolLit lit)
    | DefT (_, MaybeT t) ->
      type__ ~env t >>| fun t -> uniq_union [Ty.Void; Ty.Null; t]
    | DefT (_, OptionalT t) ->
      type__ ~env t >>| fun t -> uniq_union [Ty.Void; t]
    | DefT (_, FunT (_, _, f)) ->
      fun_ty ~env f None >>| fun t -> Ty.Fun t
    | DefT (_, ObjT o) ->
      obj_ty ~env o >>| fun t -> Ty.Obj t
    | DefT (_, ArrT a) -> arr_ty ~env a
    | DefT (_, UnionT rep) ->
      let t0, (t1, ts) = UnionRep.members_nel rep in
      type__ ~env t0 >>= fun t0 ->
      type__ ~env t1 >>= fun t1 ->
      mapM (type__ ~env) ts >>| fun ts ->
      uniq_union (t0::t1::ts)
    | DefT (_, IntersectionT rep) ->
      let t0, (t1, ts) = InterRep.members_nel rep in
      type__ ~env t0 >>= fun t0 ->
      type__ ~env t1 >>= fun t1 ->
      mapM (type__ ~env) ts >>| fun ts ->
      uniq_inter (t0::t1::ts)
    | DefT (_, PolyT (ps, t, _)) -> poly_ty ~env t ps
    | DefT (r, TypeT (kind, t)) -> type_t ~env r kind t None
    | DefT (_, TypeAppT (_, t, ts)) -> type_app ~env t ts
    | DefT (r, InstanceT (_, _, _, t)) -> instance_t ~env r t
    | DefT (_, ClassT t) -> class_t ~env t None
    | DefT (_, IdxWrapper t) -> type__ ~env t
    | ThisClassT (_, t) -> this_class_t ~env t None
    (* NOTE For now we are ignoring the "this" type here. *)
    | ThisTypeAppT (_, c, _, None) -> type__ ~env c
    | ThisTypeAppT (_, c, _, Some ts) -> type_app ~env c ts
    | KeysT (_, t) ->
      type__ ~env t >>| fun ty ->
      Ty.generic_builtin_t "$Keys" [ty]
    | OpaqueT (r, o) -> opaque_t ~env r o
    | ReposT (_, t) -> type__ ~env t
    | ShapeT t -> type__ ~env t
    | TypeDestructorTriggerT _ -> return Ty.Bot
    | MergedT (_, uses) -> merged_t ~env uses
    | ExistsT _ -> return Ty.Exists
    | ObjProtoT _ -> return (Ty.builtin_t "Object.prototype")
    | FunProtoT _ -> return (Ty.builtin_t "Function.prototype")
    | OpenPredT (_, t, _, _) -> type__ ~env t

    | FunProtoApplyT _ ->
      if Env.expand_internal_types env then
        (* Function.prototype.apply: (thisArg: any, argArray?: any): any *)
        return Ty.(mk_fun
          ~params:[
            (Some "thisArg", Any, non_opt_param);
            (Some "argArray", Any, opt_param);
          ]
          Any)
      else
        return Ty.(TypeOf (Ty.builtin_symbol "Function.prototype.apply"))

    | FunProtoBindT _ ->
      if Env.expand_internal_types env then
        (* Function.prototype.bind: (thisArg: any, ...argArray: Array<any>): any *)
        return Ty.(mk_fun
          ~params:[(Some "thisArg", Any, non_opt_param)]
          ~rest:(Some "argArray", Arr { arr_readonly = false; arr_elt_t = Any })
          Any)
      else
         return Ty.(TypeOf (Ty.builtin_symbol "Function.prototype.bind"))

    | FunProtoCallT _ ->
      if Env.expand_internal_types env then
        (* Function.prototype.call: (thisArg: any, ...argArray: Array<any>): any *)
        return Ty.(mk_fun
          ~params:[(Some "thisArg", Any, non_opt_param)]
          ~rest:(Some "argArray", Arr { arr_readonly = false; arr_elt_t = Any })
          Any)
      else
         return Ty.(TypeOf (Ty.builtin_symbol "Function.prototype.call"))

    | ModuleT (reason, _, _) -> module_t env reason t

    | DefT (_, CharSetT _)
    | NullProtoT _ ->
      terr ~kind:UnsupportedTypeCtor (Some t)


  (* Type variable normalization (input: a type variable `id`)

     Step 1: Use `root_id` as a proxy for `id`.

     Step 2: Check the cache in case root_id has been computed before. There are
             several cases here:
             a. The variable is "under resolution" state (recursive variable).
                Exit with the cached result and add the variable to the free
                variable set.
             b. The variable is "resolved". Return the result.
             c. The variable resolution led to an error. Propagate error.
             d. The variable has never been seen before. Go to step 3.

     Step 3: Start variable resolution.
  *)
  and type_variable ~env id =
    let root_id, constraints =                          (* step 1 *)
      Context.find_constraints Env.(env.genv.cx) id in
    find_tvar root_id >>= function                      (* step 2 *)
      | Some (Ok Ty.(TVar (Ty.RVar v, _) as t)) ->      (* step 2a *)
        modify State.(fun st -> { st with
          free_tvars = VSet.add v st.free_tvars
        }) >>= fun _ ->
        return t
      | Some (Ok t) -> return t                         (* step 2b *)

      | Some (Error s) -> error s                       (* step 2c *)
      | None ->                                         (* step 2d *)
        resolve_tvar ~env constraints root_id           (* step 3 *)

  (* Resolve a type variable (encountered for the first time)

     Resolving a type variable can either succeed and return a Ty.t, or fail and
     return an error. Since we are caching the result of this resolution we need
     to save a `(Ty.t, error) result`. For this reason we isolate the execution
     of the monad under the current state and cache the "monadic" result.
  *)
  and resolve_tvar ~env cons root_id =
    let open State in
    fresh_num >>= fun rid ->
    let rvar = Ty.RVar rid in
    (* Set current variable "under resolution" *)
    update_tvar_cache root_id (Ok (Ty.TVar (rvar, None))) >>= fun _ ->
    get >>= fun in_st ->

    (* Resolve the tvar *)
    let ty_res, out_st = run in_st (resolve_bounds ~env cons) in

    (* Create a recursive type (if needed) *)
    let ty_res = Core_result.map
      ~f:(Recursive.make out_st.free_tvars rid) ty_res
    in

    (* Reset state by removing the current tvar from the free vars set *)
    let out_st =
      { out_st with free_tvars = VSet.remove rid out_st.free_tvars }
    in
    put out_st >>= fun _ ->

    (* Update cache with final result *)
    update_tvar_cache root_id ty_res >>= fun _ ->

    (* Throw the error if one was encountered *)
    match ty_res with
    | Ok ty -> return ty
    | Error e -> error e

  (* Resolving a type variable amounts to normalizing its lower bounds and
     taking their union.
  *)
  and resolve_bounds ~env =
    let open Constraint in
    function
    | Resolved t -> type__ ~env t
    | Unresolved bounds ->
      let ts = T.TypeMap.keys bounds.lower in
      mapM (type__ ~env) ts >>|
      uniq_union

  and bound_t ~env reason name =
    let symbol = symbol_from_reason env reason name in
    return (Ty.Bound symbol)

  and fun_ty ~env f fun_type_params =
    let {T.params; rest_param; return_t; _} = f in
    mapM (fun_param ~env) params >>= fun fun_params ->
    fun_rest_param_t ~env rest_param >>= fun fun_rest_param ->
    type__ ~env return_t >>= fun fun_return ->
    return {Ty.fun_params; fun_rest_param; fun_return; fun_type_params}

  and method_ty ~env t =
    let open Type in
    match t with
    | DefT (_, FunT (_, _, f)) ->
      fun_ty ~env f None
    | DefT (_, PolyT (ps, DefT (_, FunT (_, _, f)), _)) ->
      mapM (type_param ~env) ps >>= fun ps ->
      fun_ty ~env f (Some ps)
    | _ ->
      terr ~kind:BadMethodType (Some t)

  and fun_param ~env (x, t) =
    opt_t ~env t >>= fun (t, prm_optional) ->
    return (x, t, { Ty.prm_optional })

  and fun_rest_param_t ~env = function
    | Some (x, _, t) -> type__ ~env t >>| fun t -> Some (x,t)
    | _ -> return None

  and obj_ty ~env {T.flags = {T.exact; T.frozen; _}; props_tmap; dict_t; _} =
    let obj_exact = exact in
    let obj_frozen = frozen in
    obj_props ~env props_tmap dict_t >>| fun obj_props ->
    {Ty.obj_exact; obj_frozen; obj_props}

  and obj_props ~env id dict =
    let dispatch (x, p) =
      if x = "$call"
        then call_prop ~env p
        else obj_prop ~env x p
    in
    let cx = Env.get_cx env in
    let props = SMap.bindings (Context.find_props cx id) in
    concat_fold_m dispatch props >>= fun obj_props ->
    match dict with
    | Some d -> index_prop ~env d >>| fun i -> i::obj_props
    | None -> return obj_props

  and obj_prop ~env x p =
    match p with
    | T.Field (_, t, polarity) ->
      let fld_polarity = type_polarity polarity in
      opt_t ~env t >>| fun (t, fld_optional) ->
      [Ty.(NamedProp (x, Field (t, {fld_polarity; fld_optional})))]
    | T.Method (_, t) ->
      method_ty ~env t >>| fun t -> [Ty.NamedProp (x, Ty.Method t)]
    | T.Get (_, t) ->
      type__ ~env t >>| fun t -> [Ty.NamedProp (x, Ty.Get t)]
    | T.Set (_, t) ->
      type__ ~env t >>| fun t -> [Ty.NamedProp (x, Ty.Set t)]
    | T.GetSet (loc1, t1, loc2, t2) ->
      obj_prop ~env x (T.Get (loc1, t1)) >>= fun p1 ->
      obj_prop ~env x (T.Set (loc2, t2)) >>| fun p2 ->
      p1@p2

  and call_prop ~env =
    let intersection = function
    | T.DefT (_, T.IntersectionT rep) -> T.InterRep.members rep
    | t -> [t]
    in
    let multi_call ts =
      mapM (method_ty ~env) ts >>| fun ts ->
      List.map (fun t -> Ty.CallProp t) ts
    in
    function
    | T.Method (_, t) -> intersection t |> multi_call
    | T.Field (_, t, _) -> intersection t |> multi_call
    | _ -> terr ~kind:BadCallProp None

  and index_prop ~env d =
    let {T.dict_polarity; dict_name; key; value} = d in
    let dict_polarity = type_polarity dict_polarity in
    type__ ~env key >>= fun dict_key ->
    type__ ~env value >>| fun dict_value ->
    Ty.(IndexProp {dict_polarity; dict_name; dict_key; dict_value})

  and arr_ty ~env = function
    | T.ArrayAT (t, _) ->
      type__ ~env t >>| fun t ->
      Ty.(Arr { arr_readonly = false; arr_elt_t = t})
    | T.ROArrayAT t ->
      type__ ~env t >>| fun t ->
      Ty.(Arr { arr_readonly = true; arr_elt_t = t})
    | T.TupleAT (_, ts) ->
      mapM (type__ ~env) ts >>| fun ts -> Ty.Tup ts
    | T.EmptyAT ->
      return Ty.Bot

  and name_of_instance_reason r =
    (* This should cover all cases but throw an error just in case. *)
    match desc_of_reason ~unwrap:false r  with
    | RType name
    | RIdentifier name -> return name
    | RReactComponent -> return "React$Component"
    | r ->
      let msg = spf "could not extract name from reason: %s"
        (Reason.string_of_desc r) in
      terr ~kind:BadInstanceT ~msg None

  and instance_t ~env r inst =
    let open Type in
    name_of_instance_reason r >>= fun name ->
    let symbol = symbol_from_reason env r name in
    mapM (fun (_, _, t, _) -> type__ ~env t) inst.type_args >>| function
    | [] -> Ty.Generic (symbol, inst.structural, None)
    | xs -> Ty.Generic (symbol, inst.structural, Some xs)

  and class_t ~env t ps =
    let rec class_t_aux = function
    | Ty.Class (name, structural, _) ->
      return (Ty.Class (name, structural, ps))
    | Ty.Generic (name, structural, _) ->
      return (Ty.Class (name, structural, ps))
    | (Ty.Bot | Ty.Exists | Ty.Any | Ty.AnyObj | Ty.Top) as b ->
      return b
    | Ty.Union (t0,t1,ts) ->
      class_t_aux t0 >>= fun t0 ->
      class_t_aux t1 >>= fun t1 ->
      mapM class_t_aux ts >>| fun ts ->
      uniq_union (t0::t1::ts)
    | Ty.Inter (t0,t1,ts) ->
      class_t_aux t0 >>= fun t0 ->
      class_t_aux t1 >>= fun t1 ->
      mapM class_t_aux ts >>| fun ts ->
      uniq_inter (t0::t1::ts)
    | Ty.Bound (Ty.Symbol (prov, sym_name)) ->
      let pred Type.{ name; reason; _ } = (
        name = sym_name &&
        Reason.def_loc_of_reason reason = Ty.loc_of_provenance prov
      ) in
      begin match List.find_opt pred env.Env.tparams with
      | Some Type.{ bound; _ } -> type__ ~env bound >>= class_t_aux
      | _ -> terr ~kind:BadClassT ~msg:"bound" (Some t)
      end
    | ty ->
      terr ~kind:BadClassT ~msg:(Ty_debug.string_of_ctor ty) (Some t)
    in
    type__ ~env t >>= class_t_aux

  and this_class_t ~env t ps =
    class_t ~env t ps

  and poly_ty ~env t typeparams =
    let env, results = List.fold_left (fun (env, rs) typeparam ->
      let r = type_param ~env typeparam in
      (Env.add_typeparam env typeparam, r::rs)
    ) (env, []) typeparams in
    List.rev results |> all >>= fun ps ->
    let ps = match ps with [] -> None | _ -> Some ps in
    match t with
    | T.DefT (_, T.ClassT t) -> class_t ~env t ps
    | T.ThisClassT (_, t) -> this_class_t ~env t ps
    | T.DefT (r, T.TypeT (kind, t)) -> type_t ~env r kind t ps
    | T.DefT (_, T.FunT (_, _, f)) ->
      fun_ty ~env f ps >>| fun fun_t -> Ty.Fun fun_t
    | _ ->
      terr ~kind:BadPoly (Some t)

  (* Type Aliases *)
  and type_t =
    let open Type in
    (* NOTE the use of the reason within `t` instead of the one passed with
       the constructor TypeT. The latter is an RType, which is somewhat more
       unwieldy as it is used more pervasively. *)
    let local env t ta_tparams =
      let reason = TypeUtil.reason_of_t t in
      match desc_of_reason ~unwrap:false reason with
      | RTypeAlias (name, true, _) ->
        let env = Env.{ env with under_type_alias = Some name } in
        type__ ~env t >>= fun ta_type ->
        let symbol = symbol_from_reason env reason name in
        return (Ty.named_alias symbol ?ta_tparams ~ta_type)
      | _ -> terr ~kind:BadTypeAlias ~msg:"local" (Some t)
    in
    let import_symbol env r t =
      match desc_of_reason ~unwrap:false r with
      | RNamedImportedType (name, _)
      | RDefaultImportedType (name, _)
      | RImportStarType name
      | RImportStarTypeOf name
      | RImportStar name ->
        return (symbol_from_reason env r name)
      | _ -> terr ~kind:BadTypeAlias ~msg:"import" (Some t)
    in
    let import env r t ps =
      import_symbol env r t >>= fun symbol ->
      let Ty.Symbol (_, name) = symbol in
      let env = Env.{ env with under_type_alias = Some name } in
      type__ ~env t >>= function
      | Ty.TypeAlias _ ->
        terr ~kind:BadTypeAlias ~msg:"nested type alias" None
      | Ty.Class _ as t ->
        (* Normalize imports of the form "import typeof { C } from 'm';" (where C
           is defined as a class in 'm') as a Ty.Class, instead of Ty.TypeAlias.
           The provenance information on the class should point to the defining
           location. This way we avoid the indirection of the import location on
           the alias symbol. *)
        return t
      | t ->
        return (Ty.named_alias symbol ?ta_tparams:ps ~ta_type:t)
    in
    let import_typeof env r t ps = import env r t ps in
    let import_fun env r t ps = import env r t ps in
    let opaque env t ps =
      match t with
      | OpaqueT (r, o) -> opaque_type_t ~env r o ps
      | _ -> terr ~kind:BadTypeAlias ~msg:"opaque" (Some t)
    in
    fun ~env r kind t ps ->
      match kind with
      | TypeAliasKind -> local env t ps
      | ImportClassKind -> class_t ~env t ps
      | ImportTypeofKind -> import_typeof env r t ps
      | ImportFunKind -> import_fun env r t ps
      | OpaqueKind -> opaque env t ps
      (* The following cases are not common *)
      | TypeParamKind -> terr ~kind:BadTypeAlias ~msg:"typeparam" (Some t)
      | InstanceKind -> terr ~kind:BadTypeAlias ~msg:"instance" (Some t)

  and exact_t ~env t =
    type__ ~env  t >>| Ty.mk_exact

  and type_app ~env t targs =
    type__ ~env t >>= fun ty ->
    mapM (type__ ~env) targs >>= fun targs ->
    match ty with
    | Ty.Class (name, _, _) ->
      return (Ty.generic_t name targs)
    | Ty.TypeAlias { Ty.ta_name; ta_tparams; ta_type } ->
      let t = if Env.expand_type_aliases env then
        match ta_tparams, ta_type with
        | Some ps, Some t -> Substitution.run ps targs t
        | None, Some t -> t
        | _ -> Ty.generic_t ta_name targs
      else
        Ty.generic_t ta_name targs
      in return t
    | Ty.Any ->
      return Ty.Any
    (* "Fix" type application on recursive types *)
    | Ty.TVar (Ty.RVar v, None) ->
      return (Ty.TVar (Ty.RVar v, Some targs))
    | Ty.Bot ->
      return Ty.Bot
    | _ ->
      let msg = spf "Normalized receiver type: %s" (Ty_debug.dump_t ty) in
      terr ~kind:BadTypeApp ~msg (Some t)

  and opaque_t ~env reason opaque_type =
    let name = opaque_type.Type.opaque_name in
    let opaque_symbol = symbol_from_reason env reason name in
    return (Ty.named_t opaque_symbol)

  (* We are being a bit lax here with opaque types so that we don't have to
     introduce a new constructor in Ty.t to support all kinds of OpaqueT.
     If an underlying type is available, then we use that as the alias body.
     If not, we check for a super type and use that if there is one.
     Otherwise, we fall back to a bodyless TypeAlias.
  *)
  and opaque_type_t ~env reason opaque_type ta_tparams =
    let open Type in
    let name = opaque_type.opaque_name in
    let current_source = Env.current_file env in
    let opaque_source = Loc.source (def_loc_of_reason reason) in
    let opaque_symbol = symbol_from_reason env reason name in
    (* Compare the current file (of the query) and the file that the opaque
       type is defined. If they differ, then hide the underlying/super type.
       Otherwise, display the underlying/super type. *)
    if Some current_source <> opaque_source then
      return (Ty.named_alias ?ta_tparams opaque_symbol)
    else
      let t_opt = match opaque_type with
      | { underlying_t = Some t; _ }       (* opaque type A = number; *)
      | { super_t = Some t; _ } -> Some t  (* declare opaque type B: number; *)
      | _ -> None                          (* declare opaque type C; *)
      (* TODO: This will potentially report a remote name.
         The same fix for T25963804 should be applied here as well. *)
      in
      option (type__ ~env) t_opt >>| fun ta_type ->
      Ty.named_alias ?ta_tparams ?ta_type opaque_symbol

  and custom_fun_expanded ~env =
    let open Type in
    function
    (* Object.assign: (target: any, ...sources: Array<any>): any *)
    | ObjectAssign -> return Ty.(mk_fun
        ~params:[(Some "target", Any, non_opt_param)]
        ~rest:(Some "sources", Arr { arr_readonly = false; arr_elt_t = Any })
        Any
      )

    (* Object.getPrototypeOf: (o: any): any *)
    | ObjectGetPrototypeOf ->
      return Ty.(mk_fun ~params:[(Some "o", Any, non_opt_param)] Any)

    (* Object.setPrototypeOf: (o: any, p: any): any *)
    | ObjectSetPrototypeOf ->
      let params = [
        (Some "o", Ty.Any, non_opt_param);
        (Some "p", Ty.Any, non_opt_param);
      ] in
      return (mk_fun ~params Ty.Any)

    (* var idx:
       <IdxObject: AnyObj, IdxResult>
       (obj: IdxObject, pathCallback: (demaybefiedObj: IdxObject) => IdxResult)
       => ?IdxResult;
    *)
    | Idx ->
      let idxObject = Ty.builtin_t "IdxObject" in
      let idxResult = Ty.builtin_t "IdxResult" in
      let tparams = [
        mk_tparam ~bound:Ty.AnyObj "IdxObject";
        mk_tparam "IdxResult";
      ]
      in
      let pathCallback = mk_fun
        ~params:[(Some "demaybefiedObj", idxObject, non_opt_param)]
        idxResult
      in
      let params = [
        (Some "obj", idxObject, non_opt_param);
        (Some "pathCallback", pathCallback, non_opt_param);
      ]
      in
      return (mk_fun ~tparams ~params (Ty.mk_maybe idxResult))

    (* var TypeAssertIs: <TypeAssertT>(value: mixed) => boolean *)
    | TypeAssertIs ->
      let tparams = [ mk_tparam "TypeAssertT" ] in
      let params = [ (Some "value", Ty.Top, non_opt_param) ] in
      return (mk_fun ~tparams ~params Ty.Bool)

    (*  var TypeAssertThrows: <TypeAssertT>(value: mixed) => TypeAssertT *)
    | TypeAssertThrows ->
      let tparams = [ mk_tparam "TypeAssertT" ] in
      let params = [ (Some "value", Ty.Top, non_opt_param) ] in
      let ret = Ty.builtin_t "TypeAssertT" in
      return (mk_fun ~tparams ~params ret)

    (* Result<T> = {success: true, value: T} | {success: false, error: string}
      var TypeAssertWraps: <TypeAssertT>(value: mixed) => Result<TypeAssertT> *)
    | TypeAssertWraps ->
      let tparams = [ mk_tparam "TypeAssertT" ] in
      let params = [ (Some "value", Ty.Top, non_opt_param) ] in
      let result_fail_ty = Ty.mk_object (Ty.mk_field_props [
        ("success", Ty.BoolLit false, false); ("error", Ty.Str, false)
      ]) in
      let result_succ_ty = Ty.mk_object (Ty.mk_field_props [
        ("success", Ty.BoolLit true, false); ("value", Ty.builtin_t "TypeAssertT", false)
      ]) in
      let ret = Ty.mk_union [result_fail_ty; result_succ_ty] in
      return (mk_fun ~tparams ~params ret)

    (* debugPrint: (_: any[]) => void *)
    | DebugPrint -> return Ty.(
        mk_fun ~params:[
          (Some "_", Arr { arr_readonly = false; arr_elt_t = Any }, non_opt_param)
        ] Void
      )

    (* debugThrow: () => empty *)
    | DebugThrow -> return (mk_fun Ty.Bot)

    (* debugSleep: (seconds: number) => void *)
    | DebugSleep -> return Ty.(
        mk_fun ~params:[(Some "seconds", Num, non_opt_param)] Void
      )

    (* reactPropType: any (TODO) *)
    | ReactPropType _ -> return Ty.Any

    (* reactCreateClass: (spec: any) => ReactClass<any> *)
    | ReactCreateClass -> return Ty.(mk_fun
        ~params:[(Some "spec", Any, non_opt_param)]
        (generic_builtin_t "ReactClass" [Any])
      )

    (* 1. Component class:
          <T>(name: ReactClass<T>, config: T, children?: any) => React$Element<T>
       2. Stateless functional component
          type SFC<T> = (config: T, context: any) => React$Element<T>
          <T>(fn: SFC<T>, config: T, children?: any) => React$Element<T>
    *)
    | ReactCreateElement
    | ReactCloneElement
    | ReactElementFactory _ -> return Ty.(
        let param_t = mk_tparam "T" in
        let tparams = [param_t] in
        let t = Bound (Symbol (Builtin, "T")) in
        let params = [
          (Some "name", generic_builtin_t "ReactClass" [t], non_opt_param);
          (Some "config", t, non_opt_param);
          (Some "children", Any, opt_param);
        ]
        in
        let reactElement = generic_builtin_t "React$Element" [t] in
        let f1 = mk_fun ~tparams ~params reactElement in
        let params = [
          (Some "config", t, non_opt_param);
          (Some "context", Any, non_opt_param);
        ]
        in
        let sfc = mk_fun ~tparams ~params reactElement in
        let params = [
          (Some "fn", sfc, non_opt_param);
          (Some "config", t, non_opt_param);
          (Some "children", Any, opt_param);
        ]
        in
        let f2 = mk_fun ~tparams ~params reactElement in
        mk_inter [f1; f2]
      )

    (* Fallback *)
    | t -> custom_fun_short ~env t

  and custom_fun_short ~env =
    let open Type in
    function
    | ObjectAssign -> return (Ty.builtin_t "Object$Assign")
    | ObjectGetPrototypeOf -> return (Ty.builtin_t "Object$GetPrototypeOf")
    | ObjectSetPrototypeOf -> return (Ty.builtin_t "Object$SetPrototypeOf")
    | Compose false -> return (Ty.builtin_t "$Compose")
    | Compose true -> return (Ty.builtin_t "$ComposeReverse")
    | ReactPropType t -> react_prop_type ~env t
    | ReactCreateClass -> return (Ty.builtin_t "React$CreateClass")
    | ReactCreateElement -> return (Ty.builtin_t "React$CreateElement")
    | ReactCloneElement -> return (Ty.builtin_t "React$CloneElement")
    | ReactElementFactory t ->
      type__ ~env t >>| fun t ->
      Ty.generic_builtin_t "React$ElementFactory" [t]
    | Idx -> return (Ty.builtin_t "$Facebookism$Idx")
    (* var TypeAssertIs: <TypeAssertT>(value: mixed) => boolean *)
    | TypeAssertIs ->
      let tparams = [ mk_tparam "TypeAssertT" ] in
      let params = [ (Some "value", Ty.Top, non_opt_param) ] in
      return (mk_fun ~tparams ~params Ty.Bool)

    (*  var TypeAssertThrows: <TypeAssertT>(value: mixed) => TypeAssertT *)
    | TypeAssertThrows ->
      let tparams = [ mk_tparam "TypeAssertT" ] in
      let params = [ (Some "value", Ty.Top, non_opt_param) ] in
      let ret = Ty.builtin_t "TypeAssertT" in
      return (mk_fun ~tparams ~params ret)

    (* Result<T> = {success: true, value: T} | {success: false, error: string}
      var TypeAssertWraps: <TypeAssertT>(value: mixed) => Result<TypeAssertT> *)
    | TypeAssertWraps ->
      let tparams = [ mk_tparam "TypeAssertT" ] in
      let params = [ (Some "value", Ty.Top, non_opt_param) ] in
      let result_fail_ty = Ty.mk_object (Ty.mk_field_props [
        ("success", Ty.BoolLit false, false); ("error", Ty.Str, false)
      ]) in
      let result_succ_ty = Ty.mk_object (Ty.mk_field_props [
        ("success", Ty.BoolLit true, false); ("value", Ty.builtin_t "TypeAssertT", false)
      ]) in
      let ret = Ty.mk_union [result_fail_ty; result_succ_ty] in
      return (mk_fun ~tparams ~params ret)

    | DebugPrint -> return (Ty.builtin_t "$Flow$DebugPrint")
    | DebugThrow -> return (Ty.builtin_t "$Flow$DebugThrow")
    | DebugSleep -> return (Ty.builtin_t "$Flow$DebugSleep")

  and custom_fun ~env t =
    if Env.expand_internal_types env
    then custom_fun_expanded ~env t
    else custom_fun_short ~env t

  and react_prop_type ~env =
    let open T.React.PropType in
    function
    | Primitive (_, t)   ->
      type__ ~env t >>| fun t ->
      Ty.generic_builtin_t "React$PropType$Primitive" [t]
    | Complex ArrayOf -> return (Ty.builtin_t "React$PropType$ArrayOf")
    | Complex InstanceOf -> return (Ty.builtin_t "React$PropType$ArrayOf")
    | Complex ObjectOf -> return (Ty.builtin_t "React$PropType$dbjectOf")
    | Complex OneOf -> return (Ty.builtin_t "React$PropType$OneOf")
    | Complex OneOfType -> return (Ty.builtin_t "React$PropType$OneOfType")
    | Complex Shape -> return (Ty.builtin_t "React$PropType$Shape")

  and internal_t ~env t =
    let open Type in
    function
    | ChoiceKitT _
    | ExtendsT _
    | ReposUpperT _ ->
      terr ~kind:BadInternalT (Some t)
    | OptionalChainVoidT r -> type__ ~env (DefT (r, VoidT));

  and param_bound ~env = function
    | T.DefT (_, T.MixedT _) -> return None
    | bound -> type__ ~env bound >>= fun b -> return (Some b)

  and default_t ~env = function
    | Some d -> type__ ~env d >>= fun d -> return (Some d)
    | _ -> return None

  and type_param ~env { T.name; bound; polarity; default; _ } =
    let tp_polarity = type_polarity polarity in
    param_bound ~env bound >>= fun tp_bound ->
    default_t ~env default >>= fun tp_default ->
    return { Ty.tp_name = name; tp_bound; tp_polarity; tp_default }

  and opt_t ~env t =
    let t, opt = match t with
    | T.DefT (_, T.OptionalT t) -> (t, true)
    | t -> (t, false)
    in
    type__ ~env t >>| fun t -> (t, opt)

  and type_polarity = function
    | T.Positive -> Ty.Positive
    | T.Negative -> Ty.Negative
    | T.Neutral -> Ty.Neutral

  (************)
  (* EvalT    *)
  (************)

  and destructuring_t ~env t id r s =
    let cx = Env.get_cx env in
    let result = try
      (* eval_selector may throw for BoundT. Catching here. *)
      Ok (Flow_js.eval_selector cx r t s id)
    with
      exn -> Error (spf "Exception:%s" (Printexc.to_string exn))
    in
    match result with
    | Ok tout -> type__ ~env tout
    | Error msg -> terr ~kind:BadEvalT ~msg (Some t)

  and named_type_destructor_t ~env t d =
    let open Type in
    let from_name n ts =
      mapM (type__ ~env) ts >>| fun tys ->
      Ty.generic_builtin_t n tys
    in
    match d with
    | NonMaybeType -> from_name "$NonMaybeType" [t]
    | ReadOnlyType -> from_name "$ReadOnlyType" [t]
    | ValuesType -> from_name "$Values" [t]
    | ElementType t' -> from_name "$ElementType" [t; t']
    | CallType ts -> from_name "$Call" (t::ts)
    | ReactElementPropsType -> from_name "React$ElementProps" [t]
    | ReactElementConfigType -> from_name "React$ElementConfig" [t]
    | ReactElementRefType -> from_name "React$ElementRef" [t]
    | PropertyType k ->
      let r = mk_reason (RStringLit k) Loc.none in
      from_name "$PropertyType" [t; DefT (r, SingletonStrT k)]
    | TypeMap (ObjectMap t') -> from_name "$ObjMap" [t; t']
    | TypeMap (ObjectMapi t') -> from_name "$ObjMapi" [t; t']
    | TypeMap (TupleMap t') -> from_name "$TupleMap" [t; t']
    | RestType (Object.Rest.Sound, t') -> from_name "$Rest" [t; t']
    | RestType (Object.Rest.IgnoreExactAndOwn, t') -> from_name "$Diff" [t; t']
    | RestType (Object.Rest.ReactConfigMerge, _) | Bind _ | SpreadType _ ->
      terr ~kind:BadEvalT ~msg:(Debug_js.string_of_destructor d) (Some t)

  and resolve_type_destructor_t ~env t id use_op reason d =
    let cx = Env.get_cx env in
    let trace = Trace.dummy_trace in
    let result =
      try
        Ok (snd (Flow_js.mk_type_destructor cx ~trace use_op reason t d id))
      with
        (* Allow bounds *)
        | Flow_js.Not_expect_bound s -> Error s
        (* But re-raise any other exception *)
        | exn -> raise exn
    in
    match result with
    | Ok t -> type__ ~env t
    | Error _s -> named_type_destructor_t ~env t d

  and evaluate_type_destructor ~env t id use_op reason d =
    let cx = Env.get_cx env in
    let evaluated = Context.evaluated cx in
    match IMap.get id evaluated with
    | Some cached_t -> type__ ~env cached_t
    | None -> resolve_type_destructor_t ~env t id use_op reason d

  and type_destructor_t ~env t id use_op reason d =
    find_eval_t id >>= function
    | Some (Ok t) -> return t
    | Some (Error e) -> error e
    | None ->
      get >>= fun in_st ->
      (* To store the complete result (including error case) we need to run
         evaluation outside the monad and then update the state of the main monad.
      *)
      let result, out_st = run in_st (
        evaluate_type_destructor ~env t id use_op reason d
      ) in
      put out_st >>= fun _ ->
      update_eval_t_cache id result >>= fun _ ->
      begin match result with
      | Ok ty -> return ty
      | Error e -> error e end

  and eval_t ~env t id = function
    | Type.DestructuringT (r, s) ->
      destructuring_t ~env t id r s
    | Type.TypeDestructorT (use_op, reason, d) ->
      type_destructor_t ~env t id use_op reason d

  and module_t env reason t =
    match desc_of_reason reason with
    | RModule name
    | RCommonJSExports name
    | RUntypedModule name ->
      let symbol = symbol_from_reason env reason name in
      return (Ty.Module symbol)
    | _ ->
      terr ~kind:UnsupportedTypeCtor (Some t)

  and use_t ~env = function
    | T.UseT (_, t) -> type__ ~env t
    | T.ReposLowerT (_, _, u) -> use_t ~env u
    | u ->
      let msg = spf "Use: %s" (Type.string_of_use_ctor u) in
      terr ~kind:BadUse ~msg None

  and merged_t ~env uses =
    if Env.fall_through_merged env
    then return Ty.Top
    else mapM (use_t ~env) uses >>| uniq_inter

  let run_type ~options ~genv ~imported_names ~tparams state t =
    let env = Env.init ~options ~genv ~tparams ~imported_names in
    run state (type__ ~env t)

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

    let from_imported_locs local imported_locs acc =
      let { local_loc; _ } = imported_locs in
      SMap.add local local_loc acc

    let from_imported_locs_map map acc =
      SMap.fold (fun _remote remote_map acc ->
        SMap.fold (fun local imported_locs_nel acc ->
          Nel.fold_left (fun acc imported_locs  ->
            from_imported_locs local imported_locs acc
          ) acc imported_locs_nel
        ) remote_map acc
      ) map acc

    let from_binding binding acc =
      match binding with
      | BindIdent (loc, x) -> SMap.add x loc acc
      | BindNamed map -> from_imported_locs_map map acc

    let from_bindings bindings_opt acc =
      Option.value_map ~default:acc ~f:(fun bs -> from_binding bs acc)
        bindings_opt

    let from_require require acc =
      match require with
      | Require { source=_; require_loc=_; bindings } ->
        from_bindings bindings acc
      | Import { source=_; named; ns=_; types; typesof; typesof_ns=_; } ->
        (* TODO import namespaces (`ns`) as modules that might contain imported types *)
        acc
        |> from_imported_locs_map named
        |> from_imported_locs_map types
        |> from_imported_locs_map typesof
      | ImportDynamic _
      | Import0 _ -> acc

    let from_requires requires =
      List.fold_left (fun acc require ->
        from_require require acc
      ) SMap.empty requires

    let extract_schemes type_table imported_locs =
      SMap.fold (fun x loc acc ->
        match Type_table.find_type_info type_table loc with
        | Some (_, e, _) -> SMap.add x e acc
        | None -> acc
      ) imported_locs SMap.empty

    let extract_ident ~options ~genv (x, scheme) = Ty.(
      let { Type.TypeScheme.tparams; type_ = t } = scheme in
      let env = Env.init ~options ~genv ~tparams ~imported_names:SMap.empty in
      type__ ~env t >>| fun ty ->
      match ty with
      | TypeAlias { ta_name = Symbol (p, _) ; _ }
      | Class (Symbol (p, _), _, _) ->
        Some (x, loc_of_provenance p)
      | _ -> None
    )

    let extract_idents ~options ~genv imported_schemes =
      mapM (extract_ident ~options ~genv) (SMap.bindings imported_schemes) >>|
      List.fold_left (fun acc x ->
        match x with
        | Some (x, id) -> SMap.add x id acc
        | None -> acc
      ) SMap.empty

  end

  let run_imports ~options ~genv state =
    let open Imports in
    let file_sig = genv.Env.file_sig in
    let requires = File_sig.(file_sig.module_sig.requires) in
    let type_table = genv.Env.type_table in
    let imported_locs = from_requires requires in
    let imported_schemes = extract_schemes type_table imported_locs in
    match run state (extract_idents ~options ~genv imported_schemes) with
    | Ok x, state -> x, state
    | Error _, state ->
      (* Fall back to empty imports map.
       * TODO provide more fine grained handling of errors
       *)
      SMap.empty, state

end

open NormalizerMonad

(* Exposed API *)

let from_schemes ~options ~genv schemes =
  let imported_names, state = run_imports ~options ~genv State.empty in
  let _, result = ListUtils.fold_map (fun state (a, scheme) ->
    let { Type.TypeScheme.tparams; type_ = t } = scheme in
    match run_type ~options ~genv ~imported_names ~tparams state t with
    | Ok t, state -> state, (a, Ok t)
    | Error s, state -> state, (a, Error s)
  ) state schemes in
  result

let from_types ~options ~genv ts =
  let imported_names, state = run_imports ~options ~genv State.empty in
  let _, result = ListUtils.fold_map (fun state (a, t) ->
    match run_type ~options ~genv ~imported_names ~tparams:[] state t with
    | Ok t, state -> state, (a, Ok t)
    | Error s, state -> state, (a, Error s)
  ) state ts in
  result

let from_scheme ~options ~genv scheme =
  let imported_names, state = run_imports ~options ~genv State.empty in
  let { Type.TypeScheme.tparams; type_ = t } = scheme in
  let result, _ = run_type ~options ~genv ~imported_names ~tparams state t in
  result

let from_type ~options ~genv t =
  let imported_names, state = run_imports ~options ~genv State.empty in
  let result, _ = run_type ~options ~genv ~imported_names ~tparams:[] state t in
  result

let fold_hashtbl ~options ~genv ~f ~g ~htbl init =
  let imported_names, state = run_imports ~options ~genv State.empty in
  let result, _ = Hashtbl.fold (fun loc x (acc, state) ->
    let { Type.TypeScheme.tparams; type_ = t } = g x in
    let result, state = run_type ~options ~genv ~imported_names ~tparams state t in
    f acc (loc, result), state
  ) htbl (init, state) in
  result
