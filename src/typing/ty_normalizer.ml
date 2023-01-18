(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Reason
open Loc_collections
module Env = Ty_normalizer_env
module T = Type
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
  | UnexpectedTypeCtor of string
  | UnsupportedTypeCtor
  | UnsupportedUseCtor
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
  | UnexpectedTypeCtor c -> spf "Unexpected type constructor (%s)" c
  | UnsupportedTypeCtor -> "Unsupported type constructor"
  | UnsupportedUseCtor -> "Unsupported use constructor"
  | RecursionLimit -> "recursion limit"

let error_to_string (kind, msg) = spf "[%s] %s" (error_kind_to_string kind) msg

(* Utility that determines the next immediate concrete constructor, ie. reads
 * through OpenTs and AnnotTs. This is useful in determining, for example, the
 * toplevel cosntructor and adjusting the logic accordingly. *)
module Lookahead = struct
  type t =
    | Recursive
    | LowerBounds of Type.t list

  exception RecursiveExn

  let peek =
    let rec loop cx acc seen t =
      match t with
      | T.OpenT (_, id) ->
        let (root_id, constraints) = Context.find_constraints cx id in
        if ISet.mem root_id seen then
          raise RecursiveExn
        else
          let seen = ISet.add root_id seen in
          (match constraints with
          | T.Constraint.Resolved (_, t)
          | T.Constraint.FullyResolved (_, (lazy t)) ->
            loop cx acc seen t
          | T.Constraint.Unresolved bounds ->
            let ts = T.TypeMap.keys bounds.T.Constraint.lower in
            List.fold_left (fun a t -> loop cx a seen t) acc ts)
      | T.AnnotT (_, t, _) -> loop cx acc seen t
      | _ -> List.rev (t :: acc)
    in
    fun cx t ->
      match loop cx [] ISet.empty t with
      | exception RecursiveExn -> Recursive
      | ts -> LowerBounds ts
end

module NormalizerMonad : sig
  module State : sig
    type t

    val empty : t
  end

  val run_type :
    options:Env.options ->
    genv:Env.genv ->
    imported_names:Ty.imported_ident ALocMap.t ->
    tparams_rev:Type.typeparam list ->
    State.t ->
    Type.t ->
    (Ty.elt, error) result * State.t

  val run_imports : options:Env.options -> genv:Env.genv -> Ty.imported_ident ALocMap.t

  val run_expand_members :
    idx_hook:(unit -> unit) ->
    force_instance:bool ->
    options:Env.options ->
    genv:Env.genv ->
    imported_names:Ty.imported_ident Loc_collections.ALocMap.t ->
    tparams_rev:Type.typeparam list ->
    State.t ->
    Type.t ->
    (Ty.t, error) result * State.t

  val run_expand_literal_union :
    options:Env.options ->
    genv:Env.genv ->
    imported_names:Ty.imported_ident Loc_collections.ALocMap.t ->
    tparams_rev:Type.typeparam list ->
    State.t ->
    Type.t ->
    (Ty.t, error) result * State.t
end = struct
  type id_key =
    | TVarKey of int
    | EvalKey of Type.Eval.id

  module State = struct
    type t = {
      rec_tvar_ids: ISet.t;
      rec_eval_ids: Type.EvalIdSet.t;
    }

    let empty = { rec_tvar_ids = ISet.empty; rec_eval_ids = Type.EvalIdSet.empty }
  end

  include StateResult.Make (State)
  open Let_syntax

  (* [id] is the identifier of the most recent OpenT or EvalT seen until we reach
   * a conctete constructor for a type. It is passed down recursively through types
   * like AnnotT, UnionT, MaybeT, etc., but will be turned to None when we descend
   * into an ObjT for example. It is useful in determining trivially recursive types
   * of the form `T = T | number`. *)
  type fn_t = env:Env.t -> ?id:id_key -> Type.t -> (Ty.t, error) t

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

  let concat_fold_m f xs = mapM f xs >>| Base.List.concat

  let terr ~kind ?msg t =
    let t_str = Base.Option.map t ~f:(fun t -> spf "Raised on type: %s" (Type.string_of_ctor t)) in
    let msg = Base.List.filter_opt [msg; t_str] |> String.concat ", " in
    error (kind, msg)

  let descend env t =
    let depth = env.Env.depth in
    let env = Env.descend env in
    match Env.max_depth env with
    | Some max_depth when depth > max_depth -> terr ~kind:RecursionLimit (Some t)
    | _ -> return env

  (* Update state *)
  let add_rec_id id =
    let open State in
    match id with
    | TVarKey id ->
      modify (fun state -> { state with rec_tvar_ids = ISet.add id state.rec_tvar_ids })
    | EvalKey id ->
      modify (fun state -> { state with rec_eval_ids = Type.EvalIdSet.add id state.rec_eval_ids })

  let is_rec_id id =
    let%map state = get in
    match id with
    | TVarKey id -> ISet.mem id state.State.rec_tvar_ids
    | EvalKey id -> Type.EvalIdSet.mem id state.State.rec_eval_ids

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
    let pred { T.name; reason; _ } = name = tp_name && tp_loc = Reason.def_aloc_of_reason reason in
    match List.find_opt pred env.Env.tparams_rev with
    | Some _ ->
      (* If we care about shadowing of type params, then flag an error *)
      if Env.flag_shadowed_type_params env then
        let shadow_pred { T.name; _ } = name = tp_name in
        match List.find_opt shadow_pred env.Env.tparams_rev with
        | Some { T.reason; _ } when Reason.def_aloc_of_reason reason <> tp_loc ->
          terr ~kind:ShadowTypeParam (Some t)
        | Some _ -> return (Ty.Bound (tp_loc, Subst_name.string_of_subst_name tp_name))
        | None -> assert false
      else
        return (Ty.Bound (tp_loc, Subst_name.string_of_subst_name tp_name))
    | None -> default t

  (**************)
  (* Type ctors *)
  (**************)

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
        }
    )

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
      | None -> Ty.Local
    in
    let sym_anonymous = sym_name = OrdinaryName "<<anonymous class>>" in
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
        if Base.List.is_empty targ_rst && matches_default targ tparam then
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

  let app_intersection ~f rep =
    let (t0, (t1, ts)) = T.InterRep.members_nel rep in
    let%bind t0 = f t0 in
    let%bind t1 = f t1 in
    let%map ts = mapM f ts in
    Ty.mk_inter (t0, t1 :: ts)

  let app_union ~from_bounds ~f rep =
    let (t0, (t1, ts)) = T.UnionRep.members_nel rep in
    let%bind t0 = f t0 in
    let%bind t1 = f t1 in
    let%map ts = mapM f ts in
    Ty.mk_union ~from_bounds (t0, t1 :: ts)

  let should_eval_skip_aliases ~env () =
    match Env.evaluate_type_destructors env with
    | Env.EvaluateNone -> false
    | Env.EvaluateSome -> false
    | Env.EvaluateAll -> true

  let should_evaluate_destructor ~env ~force_eval d =
    force_eval
    ||
    match Env.evaluate_type_destructors env with
    | Env.EvaluateNone -> false
    | Env.EvaluateSome ->
      T.(
        (match d with
        | NonMaybeType
        | PropertyType _
        | ElementType _
        | OptionalIndexedAccessNonMaybeType _
        | OptionalIndexedAccessResultType _
        | CallType _ ->
          true
        | ReadOnlyType
        | PartialType
        | SpreadType _
        | RestType _
        | ValuesType
        | TypeMap _
        | ReactElementPropsType
        | ReactElementConfigType
        | ReactElementRefType
        | ReactConfigType _
        | IdxUnwrapType ->
          false)
      )
    | Env.EvaluateAll -> true

  (* Arguments:
   * - cont: apply when destructuring returned a single type
   * - default: apply when destructuring returned 0 or >1 types, or a recursive type
   * - non_eval: apply when no destructuring happened
   *)
  let eval_t ~env ~(cont : fn_t) ~(default : fn_t) ~non_eval ?(force_eval = false) (t, d, id) =
    match d with
    | T.TypeDestructorT (use_op, reason, d) ->
      if should_evaluate_destructor ~env ~force_eval d then
        let cx = Env.get_cx env in
        let trace = Trace.dummy_trace in
        let (_, tout) = Flow_js.mk_type_destructor cx ~trace use_op reason t d id in
        match Lookahead.peek (Env.get_cx env) tout with
        | Lookahead.LowerBounds [t] -> cont ~env ~id:(EvalKey id) t
        | _ -> default ~env tout
      else
        non_eval ~env t d
    | T.LatentPredT _ ->
      let cx = Env.get_cx env in
      let evaluated = Context.evaluated cx in
      let t' =
        match T.Eval.Map.find_opt id evaluated with
        | Some evaled_t -> evaled_t
        | None -> t
      in
      cont ~env t'

  let type_variable ~env ~(cont : fn_t) id =
    let uses_t =
      let rec uses_t_aux acc uses =
        match uses with
        | [] ->
          let acc =
            (* When combining the upper bounds of a tvar, the zero-element is not
             * the `empty` type, but rather `mixed`. However, to gather this list of
             * types below we have abusively used the same normalization function
             * `cont` that we would for lower bounds. So, to reverse the unwanted
             * effect of short-circuiting to `empty` when calling `mk_inter` below,
             * we remove all `empty` types that correspond to "no upper-bounds" here.
             *)
            Base.List.filter
              ~f:(function
                | Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper) -> false
                | _ -> true)
              acc
          in
          (match acc with
          | [] -> return Ty.NoUpper
          | hd :: tl -> return (Ty.SomeKnownUpper (Ty.mk_inter (hd, tl))))
        | T.UseT (_, t) :: rest
        | T.TypeCastT (_, t) :: rest ->
          let%bind t = cont ~env ~id:(TVarKey id) t in
          uses_t_aux (t :: acc) rest
        | T.ReposLowerT (_, _, u) :: rest -> uses_t_aux acc (u :: rest)
        (* skip these *)
        | T.AssertImportIsValueT _ :: rest
        | T.CJSExtractNamedExportsT _ :: rest ->
          uses_t_aux acc rest
        | u :: _ -> return (Ty.SomeUnknownUpper (T.string_of_use_ctor u))
      in
      (fun uses -> uses_t_aux [] uses)
    in
    let empty_with_upper_bounds bounds =
      let uses = Base.List.map ~f:fst (T.Constraint.UseTypeMap.keys bounds.T.Constraint.upper) in
      let%map use_kind = uses_t uses in
      Ty.Bot (Ty.NoLowerWithUpper use_kind)
    in
    let resolve_from_lower_bounds bounds =
      T.TypeMap.keys bounds.T.Constraint.lower
      |> mapM (fun t ->
             let%map ty = cont ~env ~id:(TVarKey id) t in
             Nel.to_list (Ty.bk_union ty)
         )
      >>| Base.List.concat
      >>| Base.List.dedup_and_sort ~compare:Stdlib.compare
    in
    let resolve_bounds = function
      | T.Constraint.Resolved (_, t)
      | T.Constraint.FullyResolved (_, (lazy t)) ->
        cont ~env ~id:(TVarKey id) t
      | T.Constraint.Unresolved bounds ->
        (match%bind resolve_from_lower_bounds bounds with
        | [] -> empty_with_upper_bounds bounds
        | hd :: tl -> return (Ty.mk_union ~from_bounds:true ~flattened:true (hd, tl)))
    in
    let (_, constraints) = Context.find_constraints Env.(env.genv.cx) id in
    resolve_bounds constraints

  let maybe_t ~env ?id ~(cont : fn_t) t =
    let%map t = cont ~env ?id t in
    Ty.mk_union ~from_bounds:false (Ty.Void, [Ty.Null; t])

  let optional_t ~env ?id ~(cont : fn_t) t =
    let%map t = cont ~env ?id t in
    Ty.mk_union ~from_bounds:false (Ty.Void, [t])

  let type_app_t ~env ~(cont : fn_t) reason use_op c ts =
    let cx = Env.get_cx env in
    let trace = Trace.dummy_trace in
    let reason_op = reason in
    let reason_tapp = reason in
    let t = Flow_js.mk_typeapp_instance_annot cx ~trace ~use_op ~reason_op ~reason_tapp c ts in
    cont ~env t

  module Reason_utils = struct
    let local_type_alias_symbol env reason =
      match desc_of_reason ~unwrap:false reason with
      | REnum name -> return (symbol_from_reason env reason (Reason.OrdinaryName name))
      | RTypeAlias (name, Some loc, _) -> return (symbol_from_loc env loc (Reason.OrdinaryName name))
      | RType name -> return (symbol_from_reason env reason name)
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract local type alias name from reason: " ^ desc in
        terr ~kind:BadTypeAlias ~msg None

    let imported_type_alias_symbol env reason =
      match desc_of_reason ~unwrap:false reason with
      | RNamedImportedType (_, name)
      | RDefaultImportedType (name, _)
      | RImportStarType name
      | RImportStarTypeOf name
      | RImportStar name ->
        return (symbol_from_reason env reason (Reason.OrdinaryName name))
      | RType name -> return (symbol_from_reason env reason name)
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract imported type alias name from reason: " ^ desc in
        terr ~kind:BadTypeAlias ~msg None

    let opaque_type_alias_symbol env reason =
      match desc_of_reason ~unwrap:false reason with
      | ROpaqueType name -> return (symbol_from_reason env reason (Reason.OrdinaryName name))
      | RType name -> return (symbol_from_reason env reason name)
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract opaque name from reason: " ^ desc in
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

    let module_symbol_opt env reason =
      match desc_of_reason reason with
      | RModule name ->
        let symbol = symbol_from_reason env reason name in
        return (Some symbol)
      | RCommonJSExports name
      | RUntypedModule name ->
        let symbol = symbol_from_reason env reason (Reason.OrdinaryName name) in
        return (Some symbol)
      | RExports -> return None
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract module name from reason: " ^ desc in
        terr ~kind:UnsupportedTypeCtor ~msg None

    let is_module_reason r =
      match desc_of_reason r with
      | RModule _
      | RCommonJSExports _
      | RUntypedModule _
      | RExports ->
        true
      | _ -> false
  end

  module TypeConverter : sig
    val convert_t : ?skip_reason:bool -> env:Env.t -> Type.t -> (Ty.t, error) t

    val convert_type_params_t :
      env:Env.t -> T.typeparam Nel.t -> (Env.t * Ty.type_param list option, error) t

    val convert_react_component_class : env:Env.t -> T.t -> Type.Properties.id -> (Ty.t, error) t

    val convert_instance_t :
      env:Env.t -> Reason.reason -> Type.super -> Type.insttype -> (Ty.t, error_kind * string) t

    val convert_inline_interface :
      env:Env.t -> Type.super -> T.Properties.id -> int option -> (Ty.t, error) t

    val convert_obj_props_t :
      env:Env.t ->
      ?inherited:bool ->
      ?source:Ty.prop_source ->
      T.Properties.id ->
      int option ->
      (Ty.prop list, error) t

    val convert_obj_t :
      env:Env.t ->
      ?inherited:bool ->
      ?source:Ty.prop_source ->
      Reason.reason ->
      Type.objtype ->
      (Ty.obj_t, error) t

    val convert_type_destructor_unevaluated : env:Env.t -> Type.t -> T.destructor -> (Ty.t, error) t
  end = struct
    let rec type_debug ~env ?id ~depth t state =
      let cx = Env.get_cx env in
      let prefix = spf "%*s[Norm|run_id:%d|depth:%d]" (2 * depth) "" (get_run_id ()) depth in
      prerr_endlinef "%s Input: %s\n" prefix (Debug_js.dump_t cx t);
      let result = type_with_alias_reason ~env ?id t state in
      let result_str =
        match result with
        | (Ok ty, _) -> "[Ok] " ^ Ty_debug.dump_t ty
        | (Error e, _) -> "[Error] " ^ error_to_string e
      in
      prerr_endlinef "%s Output: %s\n" prefix result_str;
      result

    and type__ ~env ?id t =
      let%bind env = descend env t in
      let options = env.Env.options in
      let depth = env.Env.depth - 1 in
      if options.Env.verbose_normalizer then
        type_debug ~env ?id ~depth t
      else
        type_with_alias_reason ~env ?id t

    and type_with_alias_reason ~env ?id t =
      let open Type in
      (* These type are treated as transparent when it comes to the type alias
       * annotation.
       *
       * TypeDestructorTriggerT might hold a type-app, so avoid using the type here.
       * Instead, do the fallback action which is to normalize to Bot. The trigger
       * should have actually produced another concrete type as a lower bound. *)
      match t with
      | OpenT _
      | TypeDestructorTriggerT _ ->
        type_ctor ~env ?id ~cont:type_with_alias_reason t
      | EvalT _ when should_eval_skip_aliases ~env () ->
        type_ctor ~env ~cont:type_with_alias_reason t
      | _ -> begin
        match desc_of_reason ~unwrap:false (TypeUtil.reason_of_t t) with
        | RTypeAlias (name, Some loc, _) ->
          (* The default action is to avoid expansion by using the type alias name,
             when this can be trusted. The one case where we want to skip this process
             is when recovering the body of a type alias A. In that case the environment
             field under_type_alias will be 'Some A'. If the type alias name in the reason
             is also A, then we are still at the top-level of the type-alias, so we
             proceed by expanding one level preserving the same environment. *)
          let symbol = symbol_from_loc env loc (Reason.OrdinaryName name) in
          return (generic_talias symbol None)
        | _ ->
          (* We are now beyond the point of the one-off expansion. Reset the environment
             assigning None to under_type_alias, so that aliases are used in subsequent
             invocations. *)
          type_ctor ~env ?id ~cont:type_with_alias_reason t
      end

    and type_ctor ~env ?id ~(cont : fn_t) t =
      let open Type in
      match t with
      | OpenT (_, id') ->
        let (root_id, _) = Context.find_constraints (Env.get_cx env) id' in
        if id = Some (TVarKey root_id) then
          return Ty.(Bot (NoLowerWithUpper NoUpper))
        else
          if%bind is_rec_id (TVarKey root_id) then
            return (Ty.Any Ty.Recursive)
          else if ISet.mem root_id env.Env.seen_tvar_ids then
            let%map () = add_rec_id (TVarKey root_id) in
            Ty.Any Ty.Recursive
          else
            let env = { env with Env.seen_tvar_ids = ISet.add root_id env.Env.seen_tvar_ids } in
            type_variable ~env ~cont:type__ root_id
      | GenericT { bound; reason; name; _ } ->
        let loc = Reason.def_aloc_of_reason reason in
        let default _ = type__ ~env bound in
        lookup_tparam ~default env bound name loc
      | AnnotT (_, t, _) -> type__ ~env ?id t
      | EvalT (t, d, id') ->
        if id = Some (EvalKey id') then
          return Ty.(Bot (NoLowerWithUpper NoUpper))
        else
          if%bind is_rec_id (EvalKey id') then
            return (Ty.Any Ty.Recursive)
          else if Type.EvalIdSet.mem id' env.Env.seen_eval_ids then
            let%map () = add_rec_id (EvalKey id') in
            Ty.Any Ty.Recursive
          else
            let env =
              { env with Env.seen_eval_ids = Type.EvalIdSet.add id' env.Env.seen_eval_ids }
            in
            eval_t ~env ~cont ~default:type__ ~non_eval:type_destructor_unevaluated (t, d, id')
      | ExactT (_, t) -> exact_t ~env t
      | CustomFunT (_, f) -> custom_fun ~env f
      | InternalT i -> internal_t t i
      | MatchingPropT _ -> return (mk_empty Ty.EmptyMatchingPropT)
      | DefT (_, _, MixedT _) -> return Ty.Top
      | AnyT (reason, kind) -> return (Ty.Any (any_t reason kind))
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
      | DefT (_, _, BigIntT (Literal (_, (_, x)))) when Env.preserve_inferred_literal_types env ->
        return (Ty.BigInt (Some x))
      | DefT (_, _, BigIntT (Truthy | AnyLiteral | Literal _)) -> return (Ty.BigInt None)
      | DefT (_, _, EmptyT) -> return (mk_empty Ty.EmptyType)
      | DefT (_, _, NullT) -> return Ty.Null
      | DefT (_, _, SymbolT) -> return Ty.Symbol
      | DefT (_, _, SingletonNumT (_, lit)) -> return (Ty.NumLit lit)
      | DefT (_, _, SingletonStrT lit) -> return (Ty.StrLit lit)
      | DefT (_, _, SingletonBoolT lit) -> return (Ty.BoolLit lit)
      | DefT (_, _, SingletonBigIntT (_, lit)) -> return (Ty.BigIntLit lit)
      | MaybeT (_, t) -> maybe_t ~env ?id ~cont:type__ t
      | OptionalT { type_ = t; _ } -> optional_t ~env ?id ~cont:type__ t
      | DefT (_, _, FunT (static, f)) ->
        let%map t = fun_ty ~env static f None in
        Ty.Fun t
      | DefT (r, _, ObjT o) ->
        let%map o = obj_ty ~env r o in
        Ty.Obj o
      | DefT (r, _, ArrT a) -> arr_ty ~env r a
      | UnionT (_, rep) -> app_union ~from_bounds:false ~f:(type__ ~env ?id) rep
      | IntersectionT (_, rep) -> app_intersection ~f:(type__ ~env ?id) rep
      | DefT (_, _, PolyT { tparams = ps; t_out = t; _ }) -> poly_ty ~env t ps
      | TypeAppT (_, _, t, ts) -> type_app ~env t (Some ts)
      | DefT (r, _, InstanceT (_, super, _, t)) -> instance_t ~env r super t
      | DefT (_, _, ClassT t) -> class_t ~env t
      | DefT (_, _, IdxWrapper t) -> type__ ~env t
      | DefT (_, _, ReactAbstractComponentT { config; instance }) ->
        let%bind config = type__ ~env config in
        let%bind instance = type__ ~env instance in
        return
          (generic_talias
             (Ty_symbol.builtin_symbol (Reason.OrdinaryName "React$AbstractComponent"))
             (Some [config; instance])
          )
      | ThisClassT (_, t, _, _) -> this_class_t ~env t
      | ThisTypeAppT (_, c, _, ts) -> type_app ~env c ts
      | KeysT (_, t) ->
        let%map ty = type__ ~env t in
        Ty.Utility (Ty.Keys ty)
      | OpaqueT (r, o) -> opaque_t ~env r o
      | ShapeT (_, t) ->
        let%map t = type__ ~env t in
        Ty.Utility (Ty.Shape t)
      | TypeDestructorTriggerT (_, r, _, _, _) ->
        let loc = Reason.def_aloc_of_reason r in
        return (mk_empty (Ty.EmptyTypeDestructorTriggerT loc))
      | ObjProtoT _ -> return Ty.(TypeOf ObjProto)
      | FunProtoT _ -> return Ty.(TypeOf FunProto)
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
                explicit_any
            )
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
                    Arr { arr_readonly = false; arr_literal = None; arr_elt_t = explicit_any }
                  )
                explicit_any
            )
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
                    Arr { arr_readonly = false; arr_literal = None; arr_elt_t = explicit_any }
                  )
                explicit_any
            )
        else
          return Ty.(TypeOf FunProtoCall)
      | NullProtoT _ -> return Ty.Null
      | DefT (_, _, EnumObjectT _) -> terr ~kind:(UnexpectedTypeCtor "EnumObjectT") None
      | DefT (reason, _, EnumT _) ->
        let%bind symbol = Reason_utils.local_type_alias_symbol env reason in
        return (Ty.Generic (symbol, Ty.EnumKind, None))
      | DefT (_, _, CharSetT s) -> return (Ty.CharSet (String_utils.CharSet.to_string s))
      (* Top-level only *)
      | DefT (_, _, TypeT _)
      | ModuleT _ ->
        terr ~kind:(UnexpectedTypeCtor (string_of_ctor t)) (Some t)

    and any_t reason kind =
      match kind with
      | T.(AnnotatedAny | CatchAny) ->
        let aloc = Reason.aloc_of_reason reason in
        Ty.Annotated aloc
      | T.AnyError kind -> Ty.AnyError (any_error_kind kind)
      | T.Unsound k -> Ty.Unsound (unsoundness_any_t k)
      | T.Untyped -> Ty.Untyped
      | T.Placeholder -> Ty.Placeholder

    and any_error_kind = function
      | Some T.UnresolvedName -> Some Ty.UnresolvedName
      | Some T.MissingAnnotation -> Some Ty.MissingAnnotation
      | None -> None

    and unsoundness_any_t = function
      | T.BoundFunctionThis -> Ty.BoundFunctionThis
      | T.ComputedNonLiteralKey -> Ty.ComputedNonLiteralKey
      | T.Constructor -> Ty.Constructor
      | T.DummyStatic -> Ty.DummyStatic
      | T.Exports -> Ty.Exports
      | T.FunctionPrototype -> Ty.FunctionPrototype
      | T.InferenceHooks -> Ty.InferenceHooks
      | T.InstanceOfRefinement -> Ty.InstanceOfRefinement
      | T.Merged -> Ty.Merged
      | T.ResolveSpread -> Ty.ResolveSpread
      | T.Unchecked -> Ty.Unchecked
      | T.Unimplemented -> Ty.Unimplemented
      | T.UnresolvedType -> Ty.UnresolvedType
      | T.NonBindingParameter -> Ty.NonBindingParameter

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

    and obj_ty ~env ?(inherited = false) ?(source = Ty.Other) reason o =
      let obj_def_loc = Some (Reason.def_aloc_of_reason reason) in
      let { T.flags; props_tmap; call_t; _ } = o in
      let { T.obj_kind; T.frozen = obj_frozen; _ } = flags in
      let obj_literal =
        if Env.(env.options.preserve_inferred_literal_types) then
          Some (Reason.is_literal_object_reason reason)
        else
          None
      in
      let%bind obj_props = obj_props_t ~env ~inherited ~source props_tmap call_t in
      let%map obj_kind =
        match obj_kind with
        | T.Exact -> return Ty.ExactObj
        | T.Indexed d ->
          let { T.dict_polarity; dict_name; key; value } = d in
          let dict_polarity = type_polarity dict_polarity in
          let%bind dict_key = type__ ~env key in
          let%bind dict_value = type__ ~env value in
          return (Ty.IndexedObj { Ty.dict_polarity; dict_name; dict_key; dict_value })
        | T.Inexact -> return Ty.InexactObj
      in
      { Ty.obj_def_loc; obj_kind; obj_frozen; obj_literal; obj_props }

    and obj_prop_t =
      (* Value-level object types should not have properties of type type alias. For
         convience reasons it is possible for a non-module-like Type.ObjT to include
         such types as properties. Here we explicitly filter them out, since we
         cannot use type__ to normalize them.
      *)
      let is_type_alias = function
        | T.DefT (_, _, T.TypeT _)
        | T.DefT (_, _, T.PolyT { t_out = T.DefT (_, _, T.TypeT _); _ }) ->
          true
        | _ -> false
      in
      let keep_field ~env t =
        match Lookahead.peek (Env.get_cx env) t with
        | Lookahead.LowerBounds [t] -> not (is_type_alias t)
        | _ -> true
      in
      fun ~env ?(inherited = false) ?(source = Ty.Other) (x, p) ->
        match p with
        | T.Field (loc_opt, t, polarity) ->
          if keep_field ~env t then
            let def_loc = Some (Base.Option.value loc_opt ~default:(TypeUtil.loc_of_t t)) in
            let polarity = type_polarity polarity in
            let%map (t, optional) = opt_t ~env t in
            let prop = Ty.Field { t; polarity; optional } in
            [Ty.NamedProp { name = x; prop; inherited; source; def_loc }]
          else
            return []
        | T.Method (loc_opt, t) ->
          let%map tys = method_ty ~env t in
          let def_loc = Some (Base.Option.value loc_opt ~default:(TypeUtil.loc_of_t t)) in
          Base.List.map
            ~f:(fun ty ->
              Ty.NamedProp { name = x; prop = Ty.Method ty; inherited; source; def_loc })
            tys
        | T.Get (loc_opt, t) ->
          let def_loc = Some (Base.Option.value loc_opt ~default:(TypeUtil.loc_of_t t)) in
          let%map t = type__ ~env t in
          [Ty.NamedProp { name = x; prop = Ty.Get t; inherited; source; def_loc }]
        | T.Set (loc_opt, t) ->
          let def_loc = Some (Base.Option.value loc_opt ~default:(TypeUtil.loc_of_t t)) in
          let%map t = type__ ~env t in
          [Ty.NamedProp { name = x; prop = Ty.Set t; inherited; source; def_loc }]
        | T.GetSet (loc1, t1, loc2, t2) ->
          let%bind p1 = obj_prop_t ~env (x, T.Get (loc1, t1)) in
          let%map p2 = obj_prop_t ~env (x, T.Set (loc2, t2)) in
          p1 @ p2

    and call_prop_from_t ~env t =
      let ts =
        match t with
        | T.IntersectionT (_, rep) -> T.InterRep.members rep
        | t -> [t]
      in
      let%map ts = concat_fold_m (method_ty ~env) ts in
      Base.List.map ~f:(fun t -> Ty.CallProp t) ts

    and obj_props_t =
      (* call property *)
      let do_calls ~env = function
        | Some call_id ->
          let cx = Env.get_cx env in
          let ft = Context.find_call cx call_id in
          call_prop_from_t ~env ft
        | None -> return []
      in
      let do_props ~env ~inherited ~source props =
        concat_fold_m (obj_prop_t ~env ~inherited ~source) props
      in
      fun ~env ?(inherited = false) ?(source = Ty.Other) props_id call_id_opt ->
        let cx = Env.get_cx env in
        let props =
          NameUtils.Map.bindings (Context.find_props cx props_id)
          |> Base.List.map ~f:(fun (k, v) -> (k, v))
        in
        let%bind call_props = do_calls ~env call_id_opt in
        let%map props = do_props ~env ~inherited ~source props in
        call_props @ props

    and arr_ty ~env reason elt_t =
      let desc = Reason.desc_of_reason reason in
      let arr_literal =
        if Env.(env.options.preserve_inferred_literal_types) then
          Some
            (match desc with
            | RArrayLit -> true
            | _ -> false)
        else
          None
      in
      match (elt_t, desc) with
      | (T.ArrayAT (_t, Some ts), RRestArrayLit _) ->
        let%map elements =
          mapM
            (fun t ->
              let%map t = type__ ~env t in
              Ty.TupleElement { name = None; t; polarity = Ty.Neutral })
            ts
        in
        Ty.Tup elements
      | (T.ArrayAT (t, _), _) ->
        let%map t = type__ ~env t in
        Ty.Arr { Ty.arr_readonly = false; arr_literal; arr_elt_t = t }
      | (T.ROArrayAT t, _) ->
        let%map t = type__ ~env t in
        Ty.Arr { Ty.arr_readonly = true; arr_literal; arr_elt_t = t }
      | (T.TupleAT { elements = elements'; _ }, _) ->
        let%map elements =
          mapM
            (fun (T.TupleElement { name; t; polarity }) ->
              let%map t = type__ ~env t in
              Ty.TupleElement { name; t; polarity = type_polarity polarity })
            elements'
        in
        Ty.Tup elements

    (* Used for instances of React.createClass(..) *)
    and react_component_instance =
      let react_props ~env ~default props name =
        match NameUtils.Map.find (OrdinaryName name) props with
        | exception Not_found -> return default
        | Type.Field (_, t, _) -> type__ ~env t
        | _ -> return default
      in
      let inexactify = function
        | Ty.Obj ({ Ty.obj_kind = Ty.ExactObj; _ } as obj) ->
          Ty.Obj { obj with Ty.obj_kind = Ty.InexactObj }
        | ty -> ty
      in
      fun ~env own_props ->
        let cx = Env.(env.genv.cx) in
        let own_props = Context.find_props cx own_props in
        let%bind props_ty = react_props ~env ~default:Ty.explicit_any own_props "props" in
        let%bind state_ty = react_props ~env ~default:Ty.explicit_any own_props "state" in
        let state_ty = inexactify state_ty in
        return (generic_builtin_t (Reason.OrdinaryName "React$Component") [props_ty; state_ty])

    (* Used for return of React.createClass(..) *)
    and react_component_class =
      let react_static_props ~env static =
        let cx = Env.(env.genv.cx) in
        match static with
        | T.DefT (_, _, T.ObjT { T.props_tmap; _ }) ->
          Context.find_props cx props_tmap
          |> NameUtils.Map.bindings
          |> mapM (fun (name, p) -> obj_prop_t ~env (name, p))
          >>| Base.List.concat
        | _ -> return []
      in
      fun ~env static own_props ->
        let%bind static_flds = react_static_props ~env static in
        let%bind parent_instance = react_component_instance ~env own_props in
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
              Ty.obj_def_loc = None;
              obj_kind = Ty.InexactObj;
              obj_frozen = false;
              obj_literal = None;
              obj_props = static_flds;
            }
        in
        return (Ty.mk_inter (parent_class, [props_obj]))

    and to_generic ~env kind r inst =
      let%bind symbol = Reason_utils.instance_symbol env r in
      let%map tys = mapM (fun (_, _, t, _) -> type__ ~env t) inst.T.type_args in
      let targs =
        match tys with
        | [] -> None
        | _ -> Some tys
      in
      Ty.Generic (symbol, kind, targs)

    and instance_t ~env r super inst =
      let { T.inst_kind; own_props; inst_call_t; _ } = inst in
      let desc = desc_of_reason ~unwrap:false r in
      match (inst_kind, desc) with
      | (_, Reason.RReactComponent) -> react_component_instance ~env own_props
      | (T.InterfaceKind { inline = true }, _) -> inline_interface ~env super own_props inst_call_t
      | (T.InterfaceKind { inline = false }, _) -> to_generic ~env Ty.InterfaceKind r inst
      | (T.ClassKind, _) -> to_generic ~env Ty.ClassKind r inst

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
              | Ty.NamedProp { name = OrdinaryName "$key"; prop = Ty.Field { t; _ }; _ } ->
                (* The $key's polarity is fixed to neutral so we ignore it *)
                (Some t, value, pole, ps)
              | Ty.NamedProp { name = OrdinaryName "$value"; prop = Ty.Field { t; polarity; _ }; _ }
                ->
                (* The dictionary's polarity is determined by that of $value *)
                (key, Some t, Some polarity, ps)
              | _ -> (key, value, pole, p :: ps))
            (None, None, None, [])
            props
        in
        let props = List.rev props in
        match (key, value, pole) with
        | (Some dict_key, Some dict_value, Some dict_polarity) ->
          (props, Some { Ty.dict_polarity; dict_name = None; dict_key; dict_value })
        | (_, _, _) -> (props, None)
      in
      fun ~env super own_props inst_call_t ->
        let%bind super = type__ ~env super in
        let%bind if_extends = extends super in
        let%map if_props = obj_props_t ~env own_props inst_call_t in
        let (if_props, if_dict) = fix_dict_props if_props in
        Ty.InlineInterface { Ty.if_extends; if_props; if_dict }

    (* The Class<T> utility type *)
    and class_t ~env t =
      match t with
      | T.DefT (r, _, T.InstanceT (static, _, _, inst))
        when desc_of_reason ~unwrap:false r = RReactComponent ->
        let { Type.own_props; _ } = inst in
        react_component_class ~env static own_props
      | _ ->
        let%map ty = type__ ~env t in
        Ty.Utility (Ty.Class ty)

    and this_class_t ~env t =
      let open Type in
      match t with
      | DefT (r, _, InstanceT (_, _, _, { inst_kind = ClassKind; _ })) ->
        let%map symbol = Reason_utils.instance_symbol env r in
        Ty.TypeOf (Ty.TSymbol symbol)
      | _ -> terr ~kind:BadThisClassT ~msg:(string_of_ctor t) (Some t)

    and type_params_t ~env tparams =
      let (env, results) =
        Nel.fold_left
          (fun (env, rs) tp ->
            let r = type_param ~env tp in
            (Env.add_typeparam env tp, r :: rs))
          (env, [])
          tparams
      in
      let%map ps = List.rev results |> all in
      let ps =
        match ps with
        | [] -> None
        | _ -> Some ps
      in
      (env, ps)

    and poly_ty ~env t typeparams =
      let open Type in
      match t with
      | ThisClassT (_, t, _, _) -> this_class_t ~env t
      | DefT (_, _, FunT (static, f)) ->
        let%bind (env, ps) = type_params_t ~env typeparams in
        let%map fun_t = fun_ty ~env static f ps in
        Ty.Fun fun_t
      | _ -> terr ~kind:BadPoly (Some t)

    and exact_t ~env t = type__ ~env t >>| Ty.mk_exact

    and type_app =
      let mk_generic ~env symbol kind tparams targs =
        let%bind targs = optMapM (type__ ~env) targs in
        let%map targs =
          if Env.omit_targ_defaults env then
            (* Disable the option for recursive calls to type_params_t to avoid
             * infinite recursion in cases like `class C<T: C<any>> {}` *)
            let env = Env.{ env with options = { env.options with omit_targ_defaults = false } } in
            let%map (_, tparams) = type_params_t ~env tparams in
            remove_targs_matching_defaults targs tparams
          else
            return targs
        in
        Ty.Generic (symbol, kind, targs)
      in
      let instance_app ~env r inst tparams targs =
        let%bind symbol = Reason_utils.instance_symbol env r in
        let kind =
          match inst.T.inst_kind with
          | T.InterfaceKind _ -> Ty.InterfaceKind
          | T.ClassKind -> Ty.ClassKind
        in
        mk_generic ~env symbol kind tparams targs
      in
      let type_t_app ~env r kind tparams targs =
        let open Type in
        let%bind symbol =
          match kind with
          | TypeAliasKind
          | InstanceKind ->
            Reason_utils.local_type_alias_symbol env r
          | ImportTypeofKind
          | ImportClassKind
          | ImportEnumKind ->
            Reason_utils.imported_type_alias_symbol env r
          | OpaqueKind -> Reason_utils.opaque_type_alias_symbol env r
          | TypeParamKind -> terr ~kind:BadTypeAlias ~msg:"TypeParamKind" None
        in
        mk_generic ~env symbol Ty.TypeAliasKind tparams targs
      in
      let singleton_poly ~env targs tparams t =
        let open Type in
        match t with
        | ThisClassT (_, DefT (r, _, InstanceT (_, _, _, i)), _, _)
        | DefT (_, _, TypeT (_, DefT (r, _, InstanceT (_, _, _, i))))
        | DefT (_, _, ClassT (DefT (r, _, InstanceT (_, _, _, i)))) ->
          instance_app ~env r i tparams targs
        | DefT (r, _, TypeT (kind, _)) -> type_t_app ~env r kind tparams targs
        | DefT (_, _, ClassT (TypeAppT (_, _, t, _))) -> type_app ~env t targs
        | _ ->
          let msg = "PolyT:" ^ Type.string_of_ctor t in
          terr ~kind:BadTypeApp ~msg None
      in
      let singleton ~env targs t =
        let open Type in
        match t with
        | AnyT _ -> type__ ~env t
        | DefT (_, _, PolyT { tparams; t_out; _ }) -> singleton_poly ~env targs tparams t_out
        | ThisClassT (_, t, _, _)
        | DefT (_, _, TypeT (_, t)) ->
          (* This is likely an error - cannot apply on non-polymorphic type.
           * E.g type Foo = any; var x: Foo<number> *)
          type__ ~env t
        | DefT (_, _, ClassT t) when targs = None ->
          (* For example see tests/type-at-pos_class/FluxStore.js *)
          let%map t = type__ ~env t in
          Ty.Utility (Ty.Class t)
        | _ ->
          (* This is most likely already a Flow error: E.g.
           *
           *   function f<A>(): void { }
           *   type Foo = f<number>;
           *
           * gives "Cannot use function as a type."
           *)
          let msg = Type.string_of_ctor t in
          terr ~kind:BadTypeApp ~msg None
      in
      fun ~env t targs ->
        match Lookahead.peek (Env.get_cx env) t with
        | Lookahead.Recursive -> terr ~kind:BadTypeApp ~msg:"recursive" (Some t)
        | Lookahead.LowerBounds [] ->
          (* It's unlikely that an upper bound would be useful here *)
          return (Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper))
        | Lookahead.LowerBounds ts ->
          (* TypeAppT distributes over multiple lower bounds. *)
          let%map tys = mapM (singleton ~env targs) ts in
          (match tys with
          | [] -> Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper)
          | t :: ts -> Ty.mk_union ~from_bounds:true (t, ts))

    and opaque_t ~env reason opaque_type =
      let name = opaque_type.Type.opaque_name in
      let opaque_symbol = symbol_from_reason env reason (Reason.OrdinaryName name) in
      return (generic_talias opaque_symbol None)

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
                    Arr { arr_readonly = false; arr_literal = None; arr_elt_t = explicit_any }
                  )
                explicit_any
            )
        (* Object.getPrototypeOf: (o: any): any *)
        | ObjectGetPrototypeOf ->
          return Ty.(mk_fun ~params:[(Some "o", explicit_any, non_opt_param)] explicit_any)
        (* Object.setPrototypeOf: (o: any, p: any): any *)
        | ObjectSetPrototypeOf ->
          let params =
            [(Some "o", Ty.explicit_any, non_opt_param); (Some "p", Ty.explicit_any, non_opt_param)]
          in
          return (mk_fun ~params Ty.explicit_any)
        (* debugPrint: (_: any[]) => void *)
        | DebugPrint ->
          return
            Ty.(
              mk_fun
                ~params:
                  [
                    ( Some "_",
                      Arr { arr_readonly = false; arr_literal = None; arr_elt_t = explicit_any },
                      non_opt_param
                    );
                  ]
                Void
            )
        (* debugThrow: () => empty *)
        | DebugThrow -> return (mk_fun (mk_empty Ty.EmptyType))
        (* debugSleep: (seconds: number) => void *)
        | DebugSleep -> return Ty.(mk_fun ~params:[(Some "seconds", Num None, non_opt_param)] Void)
        (* reactPropType: any (TODO) *)
        | ReactPropType _ -> return Ty.explicit_any
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
                  ( Some "name",
                    generic_builtin_t (Reason.OrdinaryName "ReactClass") [t],
                    non_opt_param
                  );
                  (Some "config", t, non_opt_param);
                  (Some "children", explicit_any, opt_param);
                ]
              in
              let reactElement = generic_builtin_t (Reason.OrdinaryName "React$Element") [t] in
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
              mk_inter (f1, [f2])
            )
        (* Fallback *)
        | t -> custom_fun_short ~env t
      )

    and custom_fun_short ~env =
      Type.(
        function
        | ObjectAssign -> return (builtin_t (Reason.OrdinaryName "Object$Assign"))
        | ObjectGetPrototypeOf -> return (builtin_t (Reason.OrdinaryName "Object$GetPrototypeOf"))
        | ObjectSetPrototypeOf -> return (builtin_t (Reason.OrdinaryName "Object$SetPrototypeOf"))
        | Compose false -> return (builtin_t (Reason.OrdinaryName "$Compose"))
        | Compose true -> return (builtin_t (Reason.OrdinaryName "$ComposeReverse"))
        | ReactPropType t -> react_prop_type ~env t
        | ReactCreateElement -> return (builtin_t (Reason.OrdinaryName "React$CreateElement"))
        | ReactCloneElement -> return (builtin_t (Reason.OrdinaryName "React$CloneElement"))
        | ReactElementFactory t ->
          let%map t = type__ ~env t in
          generic_builtin_t (Reason.OrdinaryName "React$ElementFactory") [t]
        | DebugPrint -> return (builtin_t (Reason.OrdinaryName "$Flow$DebugPrint"))
        | DebugThrow -> return (builtin_t (Reason.OrdinaryName "$Flow$DebugThrow"))
        | DebugSleep -> return (builtin_t (Reason.OrdinaryName "$Flow$DebugSleep"))
      )

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
            (Reason.OrdinaryName
               ( if is_req then
                 "React$PropType$Primitive$Required"
               else
                 "React$PropType$Primitive"
               )
            )
            [t]
        | Complex ArrayOf -> return (builtin_t (Reason.OrdinaryName "React$PropType$ArrayOf"))
        | Complex InstanceOf -> return (builtin_t (Reason.OrdinaryName "React$PropType$ArrayOf"))
        | Complex ObjectOf -> return (builtin_t (Reason.OrdinaryName "React$PropType$dbjectOf"))
        | Complex OneOf -> return (builtin_t (Reason.OrdinaryName "React$PropType$OneOf"))
        | Complex OneOfType -> return (builtin_t (Reason.OrdinaryName "React$PropType$OneOfType"))
        | Complex Shape -> return (builtin_t (Reason.OrdinaryName "React$PropType$Shape"))
      )

    and internal_t t =
      Type.(
        function
        | ChoiceKitT _
        | ExtendsT _ ->
          terr ~kind:BadInternalT (Some t)
      )

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
      { Ty.tp_name = Subst_name.string_of_subst_name name; tp_bound; tp_polarity; tp_default }

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
        let obj_kind =
          if obj_exact then
            Ty.ExactObj
          else
            Ty.InexactObj
        in
        Ty.Obj
          {
            Ty.obj_def_loc = None;
            obj_props;
            obj_kind;
            obj_literal = None;
            obj_frozen = false (* default *);
          }
      in
      let spread_operand_slice ~env { T.Object.Spread.reason = _; prop_map; dict; _ } =
        Type.TypeTerm.(
          let obj_frozen = false in
          let obj_literal = None in
          let props = NameUtils.Map.fold (fun k p acc -> (k, p) :: acc) prop_map [] in
          let%bind obj_props = concat_fold_m (obj_prop_t ~env) props in
          let%bind obj_kind =
            match dict with
            | Some { key; value; dict_name; dict_polarity } ->
              let%bind dict_key = type__ ~env key in
              let%bind dict_value = type__ ~env value in
              return
                (Ty.IndexedObj
                   {
                     Ty.dict_polarity = type_polarity dict_polarity;
                     dict_name;
                     dict_key;
                     dict_value;
                   }
                )
            | None -> return Ty.ExactObj
          in
          return (Ty.Obj { Ty.obj_def_loc = None; obj_kind; obj_frozen; obj_literal; obj_props })
        )
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

    and type_destructor_unevaluated ~env t d =
      let%bind ty = type__ ~env t in
      match d with
      | T.NonMaybeType -> return (Ty.Utility (Ty.NonMaybeType ty))
      | T.ReadOnlyType -> return (Ty.Utility (Ty.ReadOnly ty))
      | T.PartialType -> return (Ty.Utility (Ty.Partial ty))
      | T.ValuesType -> return (Ty.Utility (Ty.Values ty))
      | T.ElementType { index_type } ->
        let%map index = type__ ~env index_type in
        (match index_type with
        | T.DefT (_, _, T.SingletonStrT _) ->
          (* `$ElementType<X, "foo">` is not equivalent to `X["foo"]`
           * for instances. Indexed access produces PropertyType when
           * the index is a string literal. *)
          Ty.Utility (Ty.ElementType (ty, index))
        | _ -> Ty.IndexedAccess { _object = ty; index; optional = false })
      | T.OptionalIndexedAccessNonMaybeType { index } ->
        let%map index' =
          match index with
          | T.OptionalIndexedAccessTypeIndex index_type -> type__ ~env index_type
          | T.OptionalIndexedAccessStrLitIndex name -> return @@ Ty.StrLit name
        in
        Ty.IndexedAccess { _object = ty; index = index'; optional = true }
      | T.OptionalIndexedAccessResultType _ -> return ty
      | T.CallType ts ->
        let%map tys = mapM (type__ ~env) ts in
        Ty.Utility (Ty.Call (ty, tys))
      | T.TypeMap (T.ObjectMap t') ->
        let%map ty' = type__ ~env t' in
        Ty.Utility (Ty.ObjMap (ty, ty'))
      | T.TypeMap (T.ObjectMapi t') ->
        let%map ty' = type__ ~env t' in
        Ty.Utility (Ty.ObjMapi (ty, ty'))
      | T.TypeMap T.ObjectKeyMirror -> return (Ty.Utility (Ty.ObjKeyMirror ty))
      | T.TypeMap (T.ObjectMapConst t') ->
        let%map ty' = type__ ~env t' in
        Ty.Utility (Ty.ObjMapConst (ty, ty'))
      | T.PropertyType { name } ->
        let index = Ty.StrLit name in
        return @@ Ty.IndexedAccess { _object = ty; index; optional = false }
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
      | T.IdxUnwrapType -> return (Ty.Utility (Ty.IdxUnwrapType ty))
      | T.RestType ((T.Object.Rest.Omit | T.Object.Rest.ReactConfigMerge _), _) as d ->
        terr ~kind:BadEvalT ~msg:(Debug_js.string_of_destructor d) None

    let rec type_ctor_ = type_ctor ~cont:type_ctor_

    let reset_env env =
      {
        env with
        Env.under_type_alias = Env.SymbolSet.empty;
        seen_tvar_ids = ISet.empty;
        seen_eval_ids = Type.EvalIdSet.empty;
      }

    let convert_t ?(skip_reason = false) ~env =
      if skip_reason then
        type_ctor_ ~env:(reset_env env) ?id:None
      else
        type__ ~env ?id:None

    let convert_type_params_t ~env = type_params_t ~env:(reset_env env)

    let convert_react_component_class ~env = react_component_class ~env:(reset_env env)

    let convert_instance_t ~env = instance_t ~env:(reset_env env)

    let convert_inline_interface ~env = inline_interface ~env:(reset_env env)

    let convert_obj_props_t ~env = obj_props_t ~env:(reset_env env)

    let convert_obj_t ~env = obj_ty ~env:(reset_env env)

    let convert_type_destructor_unevaluated ~env = type_destructor_unevaluated ~env:(reset_env env)
  end

  module ElementConverter : sig
    val convert_toplevel : env:Env.t -> Type.t -> (Ty.elt, error) t
  end = struct
    (* We are being a bit lax here with opaque types so that we don't have to
     * introduce a new constructor in Ty.t to support all kinds of OpaqueT.
     * If an underlying type is available, then we use that as the alias body.
     * If not, we check for a super type and use that if there is one.
     * Otherwise, we fall back to a bodyless TypeAlias.
     *)
    let opaque_type_t ~env reason opaque_type tparams =
      let open Type in
      let name = opaque_type.opaque_name in
      let current_source = Env.current_file env in
      let opaque_source = ALoc.source (def_aloc_of_reason reason) in
      let name = symbol_from_reason env reason (Reason.OrdinaryName name) in
      (* Compare the current file (of the query) and the file that the opaque
         type is defined. If they differ, then hide the underlying/super type.
         Otherwise, display the underlying/super type. *)
      if Some current_source <> opaque_source then
        return (Ty.TypeAliasDecl { import = false; name; tparams; type_ = None })
      else
        let t_opt =
          match opaque_type with
          | { underlying_t = Some t; _ } (* opaque type A = number; *)
          | { super_t = Some t; _ } ->
            Some t (* declare opaque type B: number; *)
          | _ -> None
          (* declare opaque type C; *)
          (* TODO: This will potentially report a remote name.
           * The same fix for T25963804 should be applied here as well. *)
        in
        let%map type_ = option (TypeConverter.convert_t ~env) t_opt in
        Ty.TypeAliasDecl { import = false; name; tparams; type_ }

    let type_t =
      let open Type in
      let local env reason t tparams =
        let%bind name = Reason_utils.local_type_alias_symbol env reason in
        let env = Env.set_type_alias name env in
        let%map t = TypeConverter.convert_t ~skip_reason:true ~env t in
        Ty.Decl (Ty.TypeAliasDecl { import = false; name; tparams; type_ = Some t })
      in
      let import env reason t tparams =
        let%bind name = Reason_utils.imported_type_alias_symbol env reason in
        let env = Env.set_type_alias name env in
        let%map t = TypeConverter.convert_t ~env t in
        Ty.Decl (Ty.TypeAliasDecl { name; import = true; tparams; type_ = Some t })
      in
      let opaque env t ps =
        match t with
        | OpaqueT (r, o) ->
          let%map o' = opaque_type_t ~env r o ps in
          Ty.Decl o'
        | _ -> terr ~kind:BadTypeAlias ~msg:"opaque" (Some t)
      in
      let type_param env r t =
        match desc_of_reason r with
        | RType name ->
          let loc = Reason.def_aloc_of_reason r in
          let default t = TypeConverter.convert_t ~env t in
          let%map p =
            lookup_tparam ~default env t (Subst_name.Name (display_string_of_name name)) loc
          in
          Ty.Type p
        | desc -> terr ~kind:BadTypeAlias ~msg:(spf "type param: %s" (string_of_desc desc)) (Some t)
      in
      let class_ env t =
        let ct = T.DefT (TypeUtil.reason_of_t t, T.bogus_trust (), T.ClassT t) in
        let%map c = TypeConverter.convert_t ~env ct in
        Ty.Type c
      in
      fun ~env r kind t ps ->
        match kind with
        | TypeAliasKind -> local env r t ps
        | ImportClassKind -> class_ env t
        | ImportEnumKind -> terr ~kind:(UnexpectedTypeCtor "EnumObjectT") None
        | ImportTypeofKind -> import env r t ps
        | OpaqueKind -> opaque env t ps
        | TypeParamKind -> type_param env r t
        (* The following cases are not common *)
        | InstanceKind -> terr ~kind:BadTypeAlias ~msg:"instance" (Some t)

    (* The normalizer input, Type.t, is a rather flat structure. It encompasses types
     * that expressions might have (e.g. number, string, object), but also types that
     * represent declarations (e.g. class and type alias declarations). This representation
     * makes it harder to enforce invariants that intuitively should exist. E.g.
     *
     * - Type alias, class declaration types, etc. do not nest.
     *
     * - Type aliases, class declarations and modules are toplevel or parts of modules.
     *
     * To restore these, we trap Type.t constructors that should only appear at the
     * toplevel, like modules, type aliases, etc.
     *)
    let rec toplevel =
      let open Type in
      let class_or_interface_decl ~env r tparams static super inst =
        let%bind ps =
          match tparams with
          | Some tparams ->
            let%map (_, ps) = TypeConverter.convert_type_params_t ~env tparams in
            ps
          | None -> return None
        in
        let { T.inst_kind; own_props; inst_call_t; _ } = inst in
        let desc = desc_of_reason ~unwrap:false r in
        match (inst_kind, desc) with
        | (_, Reason.RReactComponent) ->
          let%map ty = TypeConverter.convert_react_component_class ~env static own_props in
          Ty.Type ty
        | (T.InterfaceKind { inline = false }, _) ->
          let%map symbol = Reason_utils.instance_symbol env r in
          Ty.Decl (Ty.InterfaceDecl (symbol, ps))
        | (T.InterfaceKind { inline = true }, _) ->
          let%map ty = TypeConverter.convert_inline_interface ~env super own_props inst_call_t in
          Ty.Type ty
        | (T.ClassKind, _) ->
          let%map symbol = Reason_utils.instance_symbol env r in
          Ty.Decl (Ty.ClassDecl (symbol, ps))
      in
      let enum_decl ~env reason =
        let%bind symbol = Reason_utils.local_type_alias_symbol env reason in
        return (Ty.Decl Ty.(EnumDecl symbol))
      in
      let singleton_poly ~env ~orig_t tparams = function
        (* Imported interfaces *)
        | DefT (_, _, TypeT (ImportClassKind, DefT (r, _, InstanceT (static, super, _, inst)))) ->
          class_or_interface_decl ~env r (Some tparams) static super inst
        (* Classes *)
        | ThisClassT (_, DefT (r, _, InstanceT (static, super, _, inst)), _, _)
        (* Interfaces *)
        | DefT (_, _, ClassT (DefT (r, _, InstanceT (static, super, _, inst)))) ->
          class_or_interface_decl ~env r (Some tparams) static super inst
        (* See flow_js.ml canonicalize_imported_type, case of PolyT (ThisClassT):
           The initial abstraction is wrapper within an abstraction and a type application.
           The current case unwraps the abstraction and application to reveal the
           initial imported type. *)
        | DefT (_, _, ClassT (TypeAppT (_, _, t, _))) -> toplevel ~env t
        (* Type Aliases *)
        | DefT (r, _, TypeT (kind, t)) ->
          let%bind (env, ps) = TypeConverter.convert_type_params_t ~env tparams in
          type_t ~env r kind t ps
        | _ ->
          let%map ty = TypeConverter.convert_t ~env orig_t in
          Ty.Type ty
      in
      let singleton ~env ~orig_t t =
        match t with
        (* Polymorphic variants - see singleton_poly *)
        | DefT (_, _, PolyT { tparams; t_out; _ }) -> singleton_poly ~env ~orig_t tparams t_out
        (* Modules *)
        | ModuleT (reason, exports, _) ->
          let%map m = module_t ~env reason exports in
          Ty.Decl m
        | DefT (r, _, ObjT o) when Reason_utils.is_module_reason r ->
          let%map (name, exports, default) = module_of_object ~env r o in
          Ty.Decl (Ty.ModuleDecl { name; exports; default })
        (* Monomorphic Classes/Interfaces *)
        | ThisClassT (_, DefT (r, _, InstanceT (static, super, _, inst)), _, _)
        | DefT (_, _, ClassT (DefT (r, _, InstanceT (static, super, _, inst))))
        | DefT (_, _, TypeT (InstanceKind, DefT (r, _, InstanceT (static, super, _, inst))))
        | DefT (_, _, TypeT (ImportClassKind, DefT (r, _, InstanceT (static, super, _, inst)))) ->
          class_or_interface_decl ~env r None static super inst
        (* Enums *)
        | DefT (reason, _, EnumObjectT _)
        | DefT (_, _, TypeT (ImportEnumKind, DefT (reason, _, EnumT _))) ->
          enum_decl ~env reason
        (* Monomorphic Type Aliases *)
        | DefT (r, _, TypeT (kind, t)) ->
          let r =
            match kind with
            | ImportClassKind -> r
            | _ -> TypeUtil.reason_of_t t
          in
          type_t ~env r kind t None
        (* Types *)
        | _ ->
          let%map t = TypeConverter.convert_t ~env orig_t in
          Ty.Type t
      in
      fun ~env t ->
        match Lookahead.peek (Env.get_cx env) t with
        | Lookahead.LowerBounds [l] -> singleton ~env ~orig_t:t l
        | Lookahead.Recursive
        | Lookahead.LowerBounds _ ->
          let%map t = TypeConverter.convert_t ~env t in
          Ty.Type t

    and module_t =
      let open Type in
      let from_cjs_export ~env = function
        | None -> return None
        | Some exports ->
          (match Lookahead.peek (Env.get_cx env) exports with
          | Lookahead.LowerBounds [DefT (r, _, ObjT o)] ->
            let%map (_, _, default) = module_of_object ~env r o in
            default
          | Lookahead.Recursive
          | Lookahead.LowerBounds _ ->
            let%map t = TypeConverter.convert_t ~env exports in
            Some t)
      in
      let from_exports_tmap ~env exports_tmap =
        let step (x, (_loc, t)) =
          match%map toplevel ~env t with
          | Ty.Decl d -> d
          | Ty.Type t -> Ty.VariableDecl (x, t)
        in
        Context.find_exports (Env.get_cx env) exports_tmap |> NameUtils.Map.bindings |> mapM step
      in
      fun ~env reason { exports_tmap; cjs_export; _ } ->
        let%bind name = Reason_utils.module_symbol_opt env reason in
        let%bind exports = from_exports_tmap ~env exports_tmap in
        let%map default = from_cjs_export ~env cjs_export in
        Ty.ModuleDecl { name; exports; default }

    and module_of_object =
      let obj_module_props ~env props_id =
        let step (decls, default) (x, t, _pol) =
          match%map toplevel ~env t with
          | Ty.Type (Ty.Obj _) when x = Reason.OrdinaryName "default" -> (decls, default)
          | Ty.Type t when x = Reason.OrdinaryName "default" -> (decls, Some t)
          | Ty.Type t -> (Ty.VariableDecl (x, t) :: decls, default)
          | Ty.Decl d -> (d :: decls, default)
        in
        let rec loop acc xs =
          match xs with
          | [] -> return acc
          | (x, T.Field (_, t, p)) :: tl ->
            let%bind acc' = step acc (x, t, p) in
            loop acc' tl
          | _ -> terr ~kind:UnsupportedTypeCtor ~msg:"module-prop" None
        in
        let cx = Env.get_cx env in
        let props = NameUtils.Map.bindings (Context.find_props cx props_id) in
        loop ([], None) props
      in
      fun ~env reason o ->
        let%bind name = Reason_utils.module_symbol_opt env reason in
        let%map (exports, default) = obj_module_props ~env o.T.props_tmap in
        (name, exports, default)

    let convert_toplevel = toplevel
  end

  let run_type_aux
      ~(f : env:Env.t -> T.t -> ('a, error) t)
      ~(simpl : merge_kinds:bool -> ?sort:bool -> 'a -> 'a)
      ~options
      ~genv
      ~imported_names
      ~tparams_rev
      state
      t : ('a, error) result * State.t =
    let env = Env.init ~options ~genv ~tparams_rev ~imported_names in
    let (result, state) = run state (f ~env t) in
    let result =
      match result with
      | Ok t when options.Env.optimize_types ->
        let { Env.merge_bot_and_any_kinds = merge_kinds; _ } = options in
        Ok (simpl ~merge_kinds ~sort:false t)
      | _ -> result
    in
    (result, state)

  let run_type = run_type_aux ~f:ElementConverter.convert_toplevel ~simpl:Ty_utils.simplify_elt

  (* Before we start normalizing the input type we populate our environment with
   * aliases that are in scope due to typed imports. These appear inside
   * File_sig.module_sig.requires. This step includes the normalization
   * of all imported types and the creation of a map to hold bindings of imported
   * names to location of definition. This map will be used later to determine
   * whether a located name (symbol) appearing is part of the file's imports or a
   * remote (hidden or non-imported) name.
   *)

  let normalize_imports =
    let open Ty in
    let def_loc_of_ty = function
      | Utility (Class (Generic ({ sym_def_loc; _ }, _, None)))
      (* This is an acceptable proxy only if the class is not polymorphic *)
      | TypeOf (TSymbol { sym_def_loc; _ }) ->
        Some sym_def_loc
      | _ -> None
    in
    let def_loc_of_decl = function
      | TypeAliasDecl { import = false; name = { sym_def_loc; _ }; _ }
      | ClassDecl ({ sym_def_loc; _ }, _)
      | InterfaceDecl ({ sym_def_loc; _ }, _)
      | EnumDecl { sym_def_loc; _ } ->
        Some sym_def_loc
      | TypeAliasDecl { import = true; type_ = Some t; _ } -> def_loc_of_ty t
      | _ -> None
    in
    let def_loc_of_elt = function
      | Type t -> def_loc_of_ty t
      | Decl d -> def_loc_of_decl d
    in
    let convert ~options ~genv scheme =
      let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
      let imported_names = ALocMap.empty in
      let env = Env.init ~options ~genv ~tparams_rev ~imported_names in
      let%map ty = ElementConverter.convert_toplevel ~env t in
      def_loc_of_elt ty
    in
    fun ~options ~genv imported_schemes : Ty.imported_ident ALocMap.t ->
      let state = State.empty in
      let (_, result) =
        List.fold_left
          (fun (st, acc) (name, loc, import_mode, scheme) ->
            match run st (convert ~options ~genv scheme) with
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

  let run_imports ~options ~genv =
    let { Env.file_sig; typed_ast; cx; _ } = genv in
    Ty_normalizer_imports.extract_imported_idents file_sig
    |> Ty_normalizer_imports.extract_schemes cx typed_ast
    |> normalize_imports ~options ~genv

  module type EXPAND_MEMBERS_CONVERTER = sig
    val idx_hook : unit -> unit

    val force_instance : bool
  end

  (* Expand the toplevel structure of the input type into an object type. This is
   * useful for services like autocomplete for properties.
   *)
  module ExpandMembersConverter (Conf : EXPAND_MEMBERS_CONVERTER) : sig
    val convert_t : env:Env.t -> Type.t -> (Ty.t, error) t
  end = struct
    open Conf

    (* Sets how to expand members upon encountering an InstanceT:
     * - if set to IMStatic then expand the static members
     * - if set to IMUnset or IMInstance then expand the instance members.
     *
     * We distinguish between this being not yet set (IMUnset) and this being explicitly
     * set to instance (IMInstance) for the sake of determining how to update this flag
     * upon a ThisClassT:
     * - if the flag was IMUnset, then we know we want to proceed by setting it to IMStatic
     *   so that the InstanceT within is looked at as a static class.
     * - if the flag was IMInstance then we could be looking at the superclass of another
     *   InstanceT, in which case we want to look at the superclass as an instance.
     *)
    type instance_mode =
      | IMUnset
      | IMStatic
      | IMInstance

    let no_members =
      Ty.(
        Obj
          {
            obj_def_loc = None;
            obj_kind = ExactObj;
            obj_frozen = false;
            obj_literal = None;
            obj_props = [];
          }
      )

    let rec arr_t ~env ~inherited r a =
      let builtin =
        match a with
        | T.ArrayAT _ -> "Array"
        | T.ROArrayAT _
        | T.TupleAT _ ->
          "$ReadOnlyArray"
      in
      type__
        ~env
        ~inherited
        ~source:(Ty.PrimitiveProto builtin)
        ~imode:IMInstance
        (Flow_js.get_builtin (Env.get_cx env) (OrdinaryName builtin) r)

    and member_expand_object ~env ~inherited ~source super implements inst =
      let { T.own_props; proto_props; _ } = inst in
      let%bind own_ty_props =
        TypeConverter.convert_obj_props_t ~env ~inherited ~source own_props None
      in
      let%bind proto_ty_props =
        TypeConverter.convert_obj_props_t ~env ~inherited:true ~source proto_props None
      in
      let%bind super_props =
        let%map super_ty = type__ ~env ~inherited:true ~source ~imode:IMInstance super in
        [Ty.SpreadProp super_ty]
      in
      let%map interface_props =
        mapM
          (fun t ->
            let%map ty = type__ ~env ~inherited:true ~source:Ty.Interface ~imode:IMInstance t in
            Ty.SpreadProp ty)
          implements
      in
      (* The order of these props is significant to ty_members which will take the
         last one in case of name conflicts. They are ordered here by distance in
         the prototype chain (and interface members last), so, for example,
         overriding methods will have priority. *)
      let obj_props = interface_props @ super_props @ proto_ty_props @ own_ty_props in
      Ty.Obj
        {
          Ty.obj_def_loc = None;
          obj_kind = Ty.InexactObj;
          obj_frozen = false;
          obj_literal = None;
          obj_props;
        }

    and enum_t ~env ~inherited reason trust enum =
      let { T.members; representation_t; _ } = enum in
      let enum_t = T.mk_enum_type ~trust reason enum in
      let enum_object_t = T.DefT (reason, trust, T.EnumObjectT enum) in
      let proto_t =
        Flow_js.get_builtin_typeapp
          Env.(env.genv.cx)
          reason
          (OrdinaryName "$EnumProto")
          [enum_object_t; enum_t; representation_t]
      in
      let%bind proto_ty =
        type__ ~env ~inherited:true ~source:(Ty.PrimitiveProto "$EnumProto") ~imode:IMUnset proto_t
      in
      let%map enum_ty = TypeConverter.convert_t ~env enum_t in
      let members_ty =
        List.map
          (fun (name, loc) ->
            let prop = Ty.Field { t = enum_ty; polarity = Ty.Positive; optional = false } in
            Ty.NamedProp
              { name = OrdinaryName name; prop; inherited; source = Ty.Other; def_loc = Some loc })
          (SMap.bindings members)
      in
      Ty.Obj
        {
          Ty.obj_def_loc = Some (Reason.def_aloc_of_reason reason);
          obj_kind = Ty.InexactObj;
          obj_frozen = false;
          obj_literal = None;
          obj_props = Ty.SpreadProp proto_ty :: members_ty;
        }

    and obj_t ~env ~inherited ~source ~imode reason o =
      let%bind obj = TypeConverter.convert_obj_t ~env ~inherited ~source reason o in
      let%map extra_props =
        let%map proto = type__ ~env ~inherited:true ~source ~imode o.T.proto_t in
        [Ty.SpreadProp proto]
      in
      { obj with Ty.obj_props = obj.Ty.obj_props @ extra_props }

    and primitive ~env reason builtin =
      let t = Flow_js.get_builtin_type (Env.get_cx env) reason (OrdinaryName builtin) in
      type__ ~env ~inherited:true ~source:(Ty.PrimitiveProto builtin) ~imode:IMUnset t

    and instance_t ~env ~inherited ~source ~imode r static super implements inst =
      let { T.inst_kind; _ } = inst in
      let desc = desc_of_reason ~unwrap:false r in
      match (inst_kind, desc, imode) with
      | (_, Reason.RReactComponent, _) -> TypeConverter.convert_instance_t ~env r super inst
      | (T.ClassKind, _, IMStatic) -> type__ ~env ~inherited ~source ~imode static
      | (T.ClassKind, _, (IMUnset | IMInstance))
      | (T.InterfaceKind _, _, _) ->
        member_expand_object ~env ~inherited ~source super implements inst

    and opaque_t ~env ~inherited ~source ~imode r opaquetype =
      let current_source = Env.current_file env in
      let opaque_source = ALoc.source (def_aloc_of_reason r) in
      (* Compare the current file (of the query) and the file that the opaque
         type is defined. If they differ, then hide the underlying type. *)
      let same_file = Some current_source = opaque_source in
      match opaquetype with
      | { Type.underlying_t = Some t; _ } when same_file -> type__ ~env ~inherited ~source ~imode t
      | { Type.super_t = Some t; _ } -> type__ ~env ~inherited ~source ~imode t
      | _ -> return no_members

    and this_class_t ~env ~inherited ~source ~imode t =
      match imode with
      | IMUnset when not force_instance -> type__ ~env ~inherited ~source ~imode:IMStatic t
      | _ -> type__ ~env ~inherited ~source ~imode t

    and type__ ~env ?id ~inherited ~source ~(imode : instance_mode) t =
      let open Type in
      match t with
      | OpenT (_, id') ->
        let (root_id, _) = Context.find_constraints (Env.get_cx env) id' in
        if id = Some (TVarKey root_id) then
          return Ty.(Bot (NoLowerWithUpper NoUpper))
        else
          if%bind is_rec_id (TVarKey root_id) then
            return (Ty.Any Ty.Recursive)
          else if ISet.mem root_id env.Env.seen_tvar_ids then
            let%map () = add_rec_id (TVarKey root_id) in
            Ty.Any Ty.Recursive
          else
            let env = { env with Env.seen_tvar_ids = ISet.add root_id env.Env.seen_tvar_ids } in
            type_variable ~env ~cont:(type__ ~inherited ~source ~imode) id'
      | AnnotT (_, t, _) -> type__ ~env ~inherited ~source ~imode t
      | DefT (_, _, IdxWrapper t) ->
        idx_hook ();
        type__ ~env ~inherited ~source ~imode t
      | ThisTypeAppT (_, c, _, _) -> type__ ~env ~inherited ~source ~imode c
      | DefT (r, _, (NumT _ | SingletonNumT _)) -> primitive ~env r "Number"
      | DefT (r, _, (StrT _ | SingletonStrT _)) -> primitive ~env r "String"
      | DefT (r, _, (BoolT _ | SingletonBoolT _)) -> primitive ~env r "Boolean"
      | DefT (r, _, SymbolT) -> primitive ~env r "Symbol"
      | DefT (_, _, EnumT _) -> return no_members
      | ObjProtoT r -> primitive ~env r "Object"
      | FunProtoT r -> primitive ~env r "Function"
      | DefT (r, _, ObjT o) ->
        let%map o = obj_t ~env ~inherited ~source ~imode r o in
        Ty.Obj o
      | DefT (_, _, ClassT t) -> type__ ~env ~inherited ~source ~imode t
      | DefT (r, _, ArrT a) -> arr_t ~env ~inherited r a
      | DefT (r, tr, EnumObjectT e) -> enum_t ~env ~inherited r tr e
      | DefT (r, _, InstanceT (static, super, implements, inst)) ->
        instance_t ~env ~inherited ~source ~imode r static super implements inst
      | ThisClassT (_, t, _, _) -> this_class_t ~env ~inherited ~source ~imode t
      | DefT (_, _, PolyT { tparams; t_out; _ }) ->
        let tparams_rev = List.rev (Nel.to_list tparams) @ env.Env.tparams_rev in
        let env = Env.{ env with tparams_rev } in
        type__ ~env ~inherited ~source ~imode t_out
      | MaybeT (_, t) -> maybe_t ~env ?id ~cont:(type__ ~inherited ~source ~imode) t
      | IntersectionT (_, rep) ->
        app_intersection ~f:(type__ ~env ?id ~inherited ~source ~imode) rep
      | UnionT (_, rep) ->
        app_union ~from_bounds:false ~f:(type__ ~env ?id ~inherited ~source ~imode) rep
      | DefT (_, _, FunT (static, _)) -> type__ ~env ~inherited ~source ~imode static
      | TypeAppT (r, use_op, t, ts) ->
        type_app_t ~env ~cont:(type__ ~inherited ~source ~imode) r use_op t ts
      | DefT (_, _, TypeT (_, t)) -> type__ ~env ~inherited ~source ~imode t
      | OptionalT { type_ = t; _ } -> optional_t ~env ?id ~cont:(type__ ~inherited ~source ~imode) t
      | EvalT (t, d, id') ->
        if id = Some (EvalKey id') then
          return Ty.(Bot (NoLowerWithUpper NoUpper))
        else
          if%bind is_rec_id (EvalKey id') then
            return (Ty.Any Ty.Recursive)
          else if Type.EvalIdSet.mem id' env.Env.seen_eval_ids then
            let%map () = add_rec_id (EvalKey id') in
            Ty.Any Ty.Recursive
          else
            let env =
              { env with Env.seen_eval_ids = Type.EvalIdSet.add id' env.Env.seen_eval_ids }
            in
            eval_t
              ~env
              ~cont:(type__ ~inherited ~source ~imode)
              ~default:(fun ~env ?id:_ -> TypeConverter.convert_t ~env ~skip_reason:false)
              ~non_eval:TypeConverter.convert_type_destructor_unevaluated
              ~force_eval:true
              (t, d, id')
      | ExactT (_, t) -> type__ ~env ~inherited ~source ~imode t
      | GenericT { bound; _ } -> type__ ~env ~inherited ~source ~imode bound
      | OpaqueT (r, o) -> opaque_t ~env ~inherited ~source ~imode r o
      | t -> TypeConverter.convert_t ~env t

    let convert_t ~env t = type__ ~env ~inherited:false ~source:Ty.Other ~imode:IMUnset t
  end

  let run_expand_members ~idx_hook ~force_instance =
    let module Converter = ExpandMembersConverter (struct
      let idx_hook = idx_hook

      let force_instance = force_instance
    end) in
    run_type_aux ~f:Converter.convert_t ~simpl:Ty_utils.simplify_type

  (* A kind of shallow type normalizer that is only concerned with expanding types
     which could contribute literals to a union. All other types immediately yield
     empty. This strong base case allows expansion in cases that might present
     performance issues (e.g., expanding through type aliases) in the standard
     TypeConverter type normalizer.

     This is useful for autocomplete based on a type's upper bound.
  *)
  module ExpandLiteralUnionConverter : sig
    val convert_t : env:Env.t -> Type.t -> (Ty.t, error) t
  end = struct
    let rec type__ ~env ?id t =
      let open Type in
      let%bind env = descend env t in
      match t with
      | OpenT (_, id') ->
        let (root_id, _) = Context.find_constraints (Env.get_cx env) id' in
        if id = Some (TVarKey root_id) then
          return Ty.(Bot (NoLowerWithUpper NoUpper))
        else
          if%bind is_rec_id (TVarKey root_id) then
            return (Ty.Any Ty.Recursive)
          else if ISet.mem root_id env.Env.seen_tvar_ids then
            let%map () = add_rec_id (TVarKey root_id) in
            Ty.Any Ty.Recursive
          else
            let env = { env with Env.seen_tvar_ids = ISet.add root_id env.Env.seen_tvar_ids } in
            type_variable ~env ~cont:type__ root_id
      | AnnotT (_, t, _) -> type__ ~env ?id t
      | UnionT (_, rep) -> app_union ~from_bounds:false ~f:(type__ ~env ?id) rep
      | IntersectionT (_, rep) -> app_intersection ~f:(type__ ~env ?id) rep
      | TypeAppT (r, use_op, t, ts) -> type_app_t ~env ~cont:type__ r use_op t ts
      | EvalT (t, d, id') ->
        if id = Some (EvalKey id') then
          return Ty.(Bot (NoLowerWithUpper NoUpper))
        else
          if%bind is_rec_id (EvalKey id') then
            return (Ty.Any Ty.Recursive)
          else if Type.EvalIdSet.mem id' env.Env.seen_eval_ids then
            let%map () = add_rec_id (EvalKey id') in
            Ty.Any Ty.Recursive
          else
            let env =
              { env with Env.seen_eval_ids = Type.EvalIdSet.add id' env.Env.seen_eval_ids }
            in
            eval_t
              ~env
              ~cont:type__
              ~default:(fun ~env ?id:_ -> TypeConverter.convert_t ~env ~skip_reason:false)
              ~non_eval:TypeConverter.convert_type_destructor_unevaluated
              ~force_eval:true
              (t, d, id')
      | MaybeT (_, t) -> maybe_t ~env ?id ~cont:type__ t
      | OptionalT { type_ = t; _ } -> optional_t ~env ?id ~cont:type__ t
      | DefT (_, _, SingletonNumT (_, lit)) -> return (Ty.NumLit lit)
      | DefT (_, _, SingletonStrT lit) -> return (Ty.StrLit lit)
      | DefT (_, _, SingletonBoolT lit) -> return (Ty.BoolLit lit)
      | DefT (_, _, BoolT _) -> return (Ty.Bool None)
      | DefT (_, _, NullT) -> return Ty.Null
      | _ -> return empty_type

    let convert_t ~env t = type__ ~env t
  end

  let run_expand_literal_union =
    run_type_aux ~f:ExpandLiteralUnionConverter.convert_t ~simpl:Ty_utils.simplify_type
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
    Base.List.fold_map
      ~f:(fun state (a, scheme) ->
        let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
        match run_type ~options ~genv ~imported_names ~tparams_rev state t with
        | (Ok t, state) -> (state, (a, Ok t))
        | (Error s, state) -> (state, (a, Error s)))
      ~init:State.empty
      schemes
  in
  result

let from_types ~options ~genv ts =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (_, result) =
    Base.List.fold_map
      ~f:(fun state (a, t) ->
        match run_type ~options ~genv ~imported_names ~tparams_rev:[] state t with
        | (Ok t, state) -> (state, (a, Ok t))
        | (Error s, state) -> (state, (a, Error s)))
      ~init:State.empty
      ts
  in
  result

let from_scheme ~options ~genv scheme =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams_rev State.empty t in
  result

let from_type ~options ~genv t =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (result, _) = run_type ~options ~genv ~imported_names ~tparams_rev:[] State.empty t in
  result

let fold_hashtbl ~options ~genv ~f ~g ~htbl init =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let (result, _) =
    Hashtbl.fold
      (fun loc x (acc, state) ->
        let { Type.TypeScheme.tparams_rev; type_ = t } = g x in
        let (result, state) = run_type ~options ~genv ~imported_names ~tparams_rev state t in
        (f acc (loc, result), state))
      htbl
      (init, State.empty)
  in
  result

let expand_members ~idx_hook ~force_instance ~options ~genv scheme =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
  let (result, _) =
    run_expand_members
      ~options
      ~genv
      ~idx_hook
      ~force_instance
      ~imported_names
      ~tparams_rev
      State.empty
      t
  in
  result

let expand_literal_union ~options ~genv scheme =
  print_normalizer_banner options;
  let imported_names = run_imports ~options ~genv in
  let { Type.TypeScheme.tparams_rev; type_ = t } = scheme in
  let (result, _) =
    run_expand_literal_union ~options ~genv ~imported_names ~tparams_rev State.empty t
  in
  result

let debug_string_of_t cx t =
  let typed_ast =
    (ALoc.none, { Flow_ast.Program.statements = []; comments = None; all_comments = [] })
  in
  let module_sig = { File_sig.requires = []; module_kind = File_sig.ES } in
  let file_sig = { File_sig.module_sig; declare_modules = SMap.empty } in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~file_sig ~typed_ast in
  match from_type ~options:Ty_normalizer_env.default_options ~genv t with
  | Error (e, _) -> Utils_js.spf "<Error %s>" (error_kind_to_string e)
  | Ok elt -> Ty_printer.string_of_elt_single_line ~exact_by_default:true elt
