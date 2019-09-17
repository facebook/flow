(**
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
    tparams:Type.typeparam list ->
    State.t ->
    Type.t ->
    (Ty.t, error) result * State.t

  val run_imports : options:Env.options -> genv:Env.genv -> Ty.imported_ident ALocMap.t
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

    let empty = { counter = 0; tvar_cache = IMap.empty; free_tvars = VSet.empty }
  end

  include StateResult.Make (State)

  (* Monadic helper functions *)
  let mapM f xs = all (Core_list.map ~f xs)

  let optMapM f = function
    | Some xs -> mapM f xs >>| Option.return
    | None as y -> return y

  let optM f = function
    | Some x -> f x >>| Option.return
    | None as y -> return y

  let _fstMapM f (x, y) = f x >>| mk_tuple_swapped y

  let sndMapM f (x, y) = f y >>| mk_tuple x

  let concat_fold_m f xs = mapM f xs >>| Core_list.concat

  let fresh_num =
    State.(
      let%bind st = get in
      let n = st.counter in
      let%map _ = put { st with counter = n + 1 } in
      n)

  let terr ~kind ?msg t =
    let t_str = Option.map t ~f:(fun t -> spf "Raised on type: %s" (Type.string_of_ctor t)) in
    let msg = ListUtils.cat_maybes [msg; t_str] |> String.concat "\n" in
    error (kind, msg)

  (* Type caches *)

  let update_tvar_cache i t =
    State.(
      let%bind st = get in
      let tvar_cache = IMap.add i t st.tvar_cache in
      put { st with tvar_cache })

  let find_tvar root_id =
    State.(
      let%map st = get in
      IMap.get root_id st.tvar_cache)

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
    Type.(
      let pred { name; reason; _ } = name = tp_name && Reason.def_aloc_of_reason reason = tp_loc in
      match List.find_opt pred env.Env.tparams with
      | Some _ ->
        (* If we care about shadowing of type params, then flag an error *)
        if Env.flag_shadowed_type_params env then
          let shadow_pred { name; _ } = name = tp_name in
          match List.find_opt shadow_pred env.Env.tparams with
          | Some { reason; _ } when Reason.def_aloc_of_reason reason <> tp_loc ->
            terr ~kind:ShadowTypeParam (Some t)
          | Some _ -> return (Ty.Bound (tp_loc, tp_name))
          | None -> assert false
        else
          return (Ty.Bound (tp_loc, tp_name))
      | None -> default t)

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
              match SMap.get name env with
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

  let mk_fun ?(params = []) ?rest ?tparams ret =
    Ty.(
      Fun
        { fun_params = params; fun_rest_param = rest; fun_return = ret; fun_type_params = tparams })

  let mk_tparam ?bound ?(pol = Ty.Neutral) ?default name =
    Ty.{ tp_name = name; tp_bound = bound; tp_polarity = pol; tp_default = default }

  let symbol_from_loc env def_loc name =
    File_key.(
      let symbol_source = ALoc.source def_loc in
      let provenance =
        match symbol_source with
        | Some (LibFile _) -> Ty.Library
        | Some (SourceFile def_source) ->
          let current_source = Env.(env.genv.file) in
          if File_key.to_string current_source = def_source then
            Ty.Local
          else
            Ty.Remote { Ty.imported_as = ALocMap.get def_loc env.Env.imported_names }
        | Some (JsonFile _)
        | Some (ResourceFile _) ->
          Ty.Local
        | Some Builtins -> Ty.Builtin
        | None -> Ty.Local
      in
      let anonymous = name = "<<anonymous class>>" in
      { Ty.provenance; name; anonymous; def_loc })

  (* TODO due to repositioninig `reason_loc` may not point to the actual
     location where `name` was defined. *)
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
    Type.(
      let env = Env.descend env in
      match t with
      | OpenT (_, id) -> type_variable ~env id
      | BoundT (reason, name, _) -> bound_t ~env reason name
      | AnnotT (_, t, _) -> type__ ~env t
      | EvalT (t, d, id) -> eval_t ~env t id d
      | ExactT (_, t) -> exact_t ~env t
      | CustomFunT (_, f) -> custom_fun ~env f
      | InternalT i -> internal_t ~env t i
      | MatchingPropT _ -> return (mk_empty Ty.EmptyMatchingPropT)
      | AnyWithUpperBoundT t ->
        let%map t = type__ ~env t in
        Ty.Utility (Ty.Subtype t)
      | AnyWithLowerBoundT t ->
        let%map t = type__ ~env t in
        Ty.Utility (Ty.Supertype t)
      | DefT (_, _, MixedT _) -> return Ty.Top
      | AnyT (_, kind) -> return (Ty.Any (any_t kind))
      | DefT (_, _, VoidT) -> return Ty.Void
      | DefT (_, _, NumT (Literal (_, (_, x)))) when Env.preserve_inferred_literal_types env ->
        return (Ty.Num (Some x))
      | DefT (_, _, NumT (Truthy | AnyLiteral | Literal _)) -> return (Ty.Num None)
      | DefT (_, _, StrT (Literal (_, x))) when Env.preserve_inferred_literal_types env ->
        return (Ty.Str (Some x))
      | DefT (_, _, StrT (Truthy | AnyLiteral | Literal _)) -> return (Ty.Str None)
      | DefT (_, _, BoolT (Some x)) when Env.preserve_inferred_literal_types env ->
        return (Ty.Bool (Some x))
      | DefT (_, _, BoolT _) -> return (Ty.Bool None)
      | DefT (_, _, EmptyT _) -> return (mk_empty Ty.EmptyType)
      | DefT (_, _, NullT) -> return Ty.Null
      | DefT (_, _, SingletonNumT (_, lit)) -> return (Ty.NumLit lit)
      | DefT (_, _, SingletonStrT lit) -> return (Ty.StrLit lit)
      | DefT (_, _, SingletonBoolT lit) -> return (Ty.BoolLit lit)
      | MaybeT (_, t) ->
        let%map t = type__ ~env t in
        Ty.mk_union (Ty.Void, [Ty.Null; t])
      | OptionalT (_, t) ->
        let%map t = type__ ~env t in
        Ty.mk_union (Ty.Void, [t])
      | DefT (_, _, FunT (_, _, f)) ->
        let%map t = fun_ty ~env f None in
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
      | DefT (_, _, PolyT (_, ps, t, _)) -> poly_ty ~env t ps
      | DefT (r, _, TypeT (kind, t)) -> type_t ~env r kind t None
      | TypeAppT (_, _, t, ts) -> type_app ~env t (Some ts)
      | DefT (r, _, InstanceT (_, super, _, t)) -> instance_t ~env r super t
      | DefT (_, _, ClassT t) -> class_t ~env t None
      | DefT (_, _, IdxWrapper t) -> type__ ~env t
      | DefT (_, _, ReactAbstractComponentT { config; instance }) ->
        let%bind config = type__ ~env config in
        let%bind instance = type__ ~env instance in
        return
          (generic_talias
             (Ty_symbol.builtin_symbol "React$AbstractComponent")
             (Some [config; instance]))
      | ThisClassT (_, t) -> this_class_t ~env t None
      | ThisTypeAppT (_, c, _, ts) -> type_app ~env c ts
      | KeysT (_, t) ->
        let%map ty = type__ ~env t in
        Ty.Utility (Ty.Keys ty)
      | OpaqueT (r, o) -> opaque_t ~env r o
      | ReposT (_, t) -> type__ ~env t
      | ShapeT t ->
        let%map t = type__ ~env t in
        Ty.Utility (Ty.Shape t)
      | TypeDestructorTriggerT (_, r, _, _, _) ->
        let loc = Reason.def_aloc_of_reason r in
        return (mk_empty (Ty.EmptyTypeDestructorTriggerT loc))
      | MergedT (_, uses) -> merged_t ~env uses
      | ExistsT _ -> return Ty.(Utility Exists)
      | ObjProtoT _ -> return Ty.(TypeOf ObjProto)
      | FunProtoT _ -> return Ty.(TypeOf FunProto)
      | OpenPredT (_, t, _, _) -> type__ ~env t
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
      | DefT (_, _, CharSetT _) -> terr ~kind:UnsupportedTypeCtor (Some t))

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
    let (root_id, constraints) =
      (* step 1 *)
      Context.find_constraints Env.(env.genv.cx) id
    in
    match%bind find_tvar root_id with
    (* step 2 *)
    | Some (Ok Ty.(TVar (RVar v, _) as t)) ->
      (* step 2a *)
      let mod_state st = State.{ st with free_tvars = VSet.add v st.free_tvars } in
      let%map () = modify mod_state in
      t
    | Some (Ok t) -> return t (* step 2b *)
    | Some (Error s) -> error s (* step 2c *)
    | None ->
      (* step 2d *)
      resolve_tvar ~env constraints root_id

  (* step 3 *)

  (* Resolve a type variable (encountered for the first time)

     Resolving a type variable can either succeed and return a Ty.t, or fail and
     return an error. Since we are caching the result of this resolution we need
     to save a `(Ty.t, error) result`. For this reason we isolate the execution
     of the monad under the current state and cache the "monadic" result.
  *)
  and resolve_tvar ~env cons root_id =
    State.(
      let%bind rid = fresh_num in
      let rvar = Ty.RVar rid in
      (* Set current variable "under resolution" *)
      let%bind _ = update_tvar_cache root_id (Ok (Ty.TVar (rvar, None))) in
      let%bind in_st = get in
      (* Resolve the tvar *)
      let (ty_res, out_st) = run in_st (resolve_bounds ~env cons) in
      (* Create a recursive type (if needed) *)
      let ty_res = Core_result.map ~f:(Recursive.make out_st.free_tvars rid) ty_res in
      (* Reset state by removing the current tvar from the free vars set *)
      let out_st = { out_st with free_tvars = VSet.remove rid out_st.free_tvars } in
      let%bind _ = put out_st in
      (* Update cache with final result *)
      let%bind _ = update_tvar_cache root_id ty_res in
      (* Throw the error if one was encountered *)
      match ty_res with
      | Ok ty -> return ty
      | Error e -> error e)

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
    >>| Core_list.concat
    >>| Core_list.dedup

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
    let { Ty.def_loc; name; _ } = symbol_from_reason env reason name in
    return (Ty.Bound (def_loc, name))

  and fun_ty ~env f fun_type_params =
    let { T.params; rest_param; return_t; _ } = f in
    let%bind fun_params = mapM (fun_param ~env) params in
    let%bind fun_rest_param = fun_rest_param_t ~env rest_param in
    let%bind fun_return = type__ ~env return_t in
    return { Ty.fun_params; fun_rest_param; fun_return; fun_type_params }

  and method_ty ~env t =
    Type.(
      match t with
      | DefT (_, _, FunT (_, _, f)) -> fun_ty ~env f None
      | DefT (_, _, PolyT (_, ps, DefT (_, _, FunT (_, _, f)), _)) ->
        let%bind ps = mapM (type_param ~env) (Nel.to_list ps) in
        fun_ty ~env f (Some ps)
      | _ -> terr ~kind:BadMethodType (Some t))

  and fun_param ~env (x, t) =
    let%bind (t, prm_optional) = opt_t ~env t in
    return (x, t, { Ty.prm_optional })

  and fun_rest_param_t ~env = function
    | Some (x, _, t) ->
      let%map t = type__ ~env t in
      Some (x, t)
    | _ -> return None

  and obj_ty ~env reason o =
    let { T.flags; props_tmap; call_t; dict_t; _ } = o in
    let { T.exact = obj_exact; T.frozen = obj_frozen; _ } = flags in
    let obj_literal =
      match Reason.desc_of_reason reason with
      | Reason.RObjectLit -> true
      | _ -> false
    in
    let%map obj_props = obj_props ~env props_tmap call_t dict_t in
    Ty.Obj { Ty.obj_exact; obj_frozen; obj_literal; obj_props }

  and obj_prop ~env (x, p) =
    match p with
    | T.Field (_, t, polarity) ->
      let fld_polarity = type_polarity polarity in
      let%map (t, fld_optional) = opt_t ~env t in
      [Ty.(NamedProp (x, Field (t, { fld_polarity; fld_optional })))]
    | T.Method (_, t) ->
      let%map t = method_ty ~env t in
      [Ty.NamedProp (x, Ty.Method t)]
    | T.Get (_, t) ->
      let%map t = type__ ~env t in
      [Ty.NamedProp (x, Ty.Get t)]
    | T.Set (_, t) ->
      let%map t = type__ ~env t in
      [Ty.NamedProp (x, Ty.Set t)]
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
    let%map ts = mapM (method_ty ~env) ts in
    Core_list.map ~f:(fun t -> Ty.CallProp t) ts

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
    match elt_t with
    | T.ArrayAT (t, _) ->
      let%map t = type__ ~env t in
      Ty.Arr { Ty.arr_readonly = false; arr_literal; arr_elt_t = t }
    | T.ROArrayAT t ->
      let%map t = type__ ~env t in
      Ty.Arr { Ty.arr_readonly = true; arr_literal; arr_elt_t = t }
    | T.TupleAT (_, ts) ->
      let%map ts = mapM (type__ ~env) ts in
      Ty.Tup ts

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
          {
            Ty.obj_exact = false;
            obj_frozen = false;
            obj_literal = false;
            obj_props = static_flds;
          }
      in
      Ty.mk_inter (parent_class, [props_obj])

  and instance_t =
    let to_generic ~env kind r inst =
      match desc_of_reason ~unwrap:false r with
      | RType name
      | RIdentifier name ->
        (* class or interface declaration *)
        let symbol = symbol_from_reason env r name in
        let%map tys = mapM (fun (_, _, t, _) -> type__ ~env t) inst.T.type_args in
        let targs =
          match tys with
          | [] -> None
          | _ -> Some tys
        in
        Ty.Generic (symbol, kind, targs)
      | r ->
        let desc = Reason.string_of_desc r in
        let msg = "could not extract name from reason: " ^ desc in
        terr ~kind:BadInstanceT ~msg None
    in
    fun ~env r super inst ->
      let { T.inst_kind; own_props; inst_call_t; _ } = inst in
      match inst_kind with
      | T.InterfaceKind { inline = true } -> inline_interface ~env super own_props inst_call_t
      | T.InterfaceKind { inline = false } -> to_generic ~env Ty.InterfaceKind r inst
      | T.ClassKind -> to_generic ~env Ty.ClassKind r inst

  and inline_interface =
    let rec extends = function
      | Ty.Generic g -> return [g]
      | Ty.Inter (t1, t2, ts) -> mapM extends (t1 :: t2 :: ts) >>| Core_list.concat
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
            | Ty.NamedProp ("$key", Ty.Field (t, _)) ->
              (* The $key's polarity is fixed to neutral so we ignore it *)
              (Some t, value, pole, ps)
            | Ty.NamedProp ("$value", Ty.Field (t, { Ty.fld_polarity; _ })) ->
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
    let rec go ~env ps ty =
      match ty with
      | Ty.Generic (name, kind, _) ->
        begin
          match kind with
          | Ty.InterfaceKind -> return (Ty.InterfaceDecl (name, ps))
          | Ty.TypeAliasKind -> return (Ty.Utility (Ty.Class ty))
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
      | Ty.Bound (loc, bname) ->
        let pred Type.{ name; reason; _ } =
          name = bname && Reason.def_aloc_of_reason reason = loc
        in
        begin
          match List.find_opt pred env.Env.tparams with
          | Some Type.{ bound; _ } ->
            let%bind b = type__ ~env bound in
            go ~env ps b
          | _ -> terr ~kind:BadClassT ~msg:"bound" None
        end
      | ty -> terr ~kind:BadClassT ~msg:(Ty_debug.string_of_ctor ty) None
    in
    fun ~env t ps ->
      match t with
      | T.DefT (r, _, T.InstanceT (static, _, _, inst))
        when desc_of_reason ~unwrap:false r = RReactComponent ->
        let { Type.own_props; _ } = inst in
        react_component ~env static own_props
      | _ ->
        let%bind t = type__ ~env t in
        go ~env ps t

  and this_class_t ~env t ps =
    let%bind ty = type__ ~env t in
    match ty with
    | Ty.Generic (name, Ty.ClassKind, _) -> return (Ty.ClassDecl (name, ps))
    | _ -> terr ~kind:BadThisClassT ~msg:(Ty_debug.string_of_ctor ty) (Some t)

  and poly_ty ~env t typeparams =
    let (env, results) =
      Nel.fold_left
        (fun (env, rs) typeparam ->
          let r = type_param ~env typeparam in
          (Env.add_typeparam env typeparam, r :: rs))
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
    | T.DefT (_, _, T.FunT (_, _, f)) ->
      let%map fun_t = fun_ty ~env f ps in
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
        match desc_of_reason ~unwrap:false reason with
        | RTypeAlias (name, true, _) ->
          let env = Env.{ env with under_type_alias = Some name } in
          let%bind ta_type = type__ ~env t in
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
        let%bind symbol = import_symbol env r t in
        let { Ty.name; _ } = symbol in
        let env = Env.{ env with under_type_alias = Some name } in
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
      let import_typeof env r t ps = import env r t ps in
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
        | OpaqueKind -> opaque env t ps
        (* The following cases are not common *)
        | TypeParamKind -> terr ~kind:BadTypeAlias ~msg:"typeparam" (Some t)
        | InstanceKind -> terr ~kind:BadTypeAlias ~msg:"instance" (Some t))

  and exact_t ~env t = type__ ~env t >>| Ty.mk_exact

  and type_app =
    let go ~env targs = function
      | Ty.ClassDecl (name, _) -> return (generic_class name targs)
      | Ty.InterfaceDecl (name, _) -> return (generic_interface name targs)
      | Ty.TypeAlias { Ty.ta_name; ta_tparams; ta_type } ->
        begin
          match ta_type with
          | Some ta_type when Env.expand_type_aliases env ->
            begin
              match Option.both ta_tparams targs with
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
      | Ty.Utility (Ty.Class _) as ty when Option.is_none targs -> return ty
      | ty -> terr ~kind:BadTypeApp ~msg:(Ty_debug.string_of_ctor ty) None
    in
    fun ~env t targs ->
      let%bind ty = type__ ~env t in
      let%bind targs = optMapM (type__ ~env) targs in
      go ~env targs ty

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

  and internal_t ~env t =
    Type.(
      function
      | ChoiceKitT _
      | ExtendsT _
      | ReposUpperT _ ->
        terr ~kind:BadInternalT (Some t)
      | OptionalChainVoidT r -> type__ ~env (DefT (r, bogus_trust (), VoidT)))

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
      | T.OptionalT (_, t) -> (t, true)
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
      | Type.Object.Spread.Value -> terr ~kind:BadEvalT ~msg:"spread-target-value" None
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

  and type_destructor_t ~env id t d =
    if Env.evaluate_type_destructors env then
      let cx = Env.get_cx env in
      let evaluated = Context.evaluated cx in
      match IMap.get id evaluated with
      | Some t -> type__ ~env t
      | None -> type_destructor_unevaluated ~env t d
    (* fallback *)
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
      match IMap.get id evaluated with
      | Some evaled_t -> evaled_t
      | None -> t
    in
    type__ ~env t'

  and eval_t ~env t id = function
    | Type.LatentPredT _ -> latent_pred_t ~env id t
    | Type.TypeDestructorT (_, _, d) -> type_destructor_t ~env id t d

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
      | T.UseT (_, t) :: rest ->
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
      | Ty.TypeAlias { Ty.ta_name = { Ty.def_loc; _ }; _ }
      | Ty.ClassDecl ({ Ty.def_loc; _ }, _)
      | Ty.InterfaceDecl ({ Ty.def_loc; _ }, _) ->
        Some def_loc
      | Ty.Utility (Ty.Class (Ty.Generic ({ Ty.def_loc; _ }, _, None))) ->
        (* This is an acceptable proxy only if the class is not polymorphic *)
        Some def_loc
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

let from_schemes ~options ~genv schemes =
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
  let imported_names = run_imports ~options ~genv in
  let { Type.TypeScheme.tparams; type_ = t } = scheme in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams State.empty t in
  result

let from_type ~options ~genv t =
  let imported_names = run_imports ~options ~genv in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams:[] State.empty t in
  result

let fold_hashtbl ~options ~genv ~f ~g ~htbl init =
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
