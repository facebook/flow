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

(* The type normalizer converts inferred types (of type `Type.t`) under a context
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
  | BadCallProp
  | BadClassT
  | BadMappedType
  | BadThisClassT
  | BadPoly
  | BadTypeAlias
  | BadTypeApp
  | BadInlineInterfaceExtends
  | BadInstanceT
  | BadEvalT
  | BadUse
  | ShadowTypeParam
  | SyntheticBoundT
  | UnexpectedTypeCtor of string
  | UnsupportedTypeCtor
  | UnsupportedUseCtor
  | RecursionLimit

type error = error_kind * string

let error_kind_to_string = function
  | BadMethodType -> "Bad method type"
  | BadCallProp -> "Bad call property"
  | BadClassT -> "Bad class"
  | BadMappedType -> "Bad mapped type"
  | BadThisClassT -> "Bad this class"
  | BadPoly -> "Bad polymorphic type"
  | BadTypeAlias -> "Bad type alias"
  | BadTypeApp -> "Bad type application"
  | BadInlineInterfaceExtends -> "Bad inline interface extends"
  | BadInstanceT -> "Bad instance type"
  | BadEvalT -> "Bad eval"
  | BadUse -> "Bad use"
  | ShadowTypeParam -> "Shadowed type parameters"
  | SyntheticBoundT -> "Synthetic type parameter"
  | UnexpectedTypeCtor c -> spf "Unexpected type constructor (%s)" c
  | UnsupportedTypeCtor -> "Unsupported type constructor"
  | UnsupportedUseCtor -> "Unsupported use constructor"
  | RecursionLimit -> "recursion limit"

let error_to_string (kind, msg) = spf "[%s] %s" (error_kind_to_string kind) msg

(* Utility that determines the next immediate concrete constructor, ie. reads
 * through OpenTs and AnnotTs. This is useful in determining, for example, the
 * toplevel constructor and adjusting the logic accordingly. *)
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
          | T.Constraint.Resolved t -> loop cx acc seen t
          | T.Constraint.FullyResolved s -> loop cx acc seen (Context.force_fully_resolved_tvar cx s)
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

module type S = sig
  module State : sig
    type t

    val empty : t

    val found_computed_type : t -> bool
  end

  val run_type : genv:Env.genv -> State.t -> Type.t -> (Ty.elt, error) result * State.t

  val run_module_type :
    genv:Env.genv -> State.t -> Type.moduletype -> (Ty.decl, error) result * State.t

  val normalize_imports :
    Context.t ->
    File_sig.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t option ->
    Env.options ->
    (string * ALoc.t * Ty.import_mode * Type.t) list ->
    Ty.imported_ident Loc_collections.ALocMap.t

  val run_expand_members :
    force_instance:bool ->
    ?allowed_prop_names:Reason.name list ->
    genv:Env.genv ->
    State.t ->
    Type.t ->
    (Ty.t, error) result * State.t

  val run_expand_literal_union :
    genv:Env.genv -> State.t -> Type.t -> (Ty.t, error) result * State.t
end

module type INPUT = sig
  val eval :
    Context.t ->
    should_eval:bool ->
    cont:(Type.t -> 'a) ->
    default:(Type.t -> 'a) ->
    non_eval:(Type.t -> Type.destructor -> 'a) ->
    Type.t * Type.defer_use_t * Type.Eval.id ->
    'a

  val keys :
    Context.t ->
    should_evaluate:bool ->
    cont:(Type.t -> 'a) ->
    default:(unit -> 'a) ->
    Reason.t ->
    Type.t ->
    'a

  val typeapp :
    Context.t ->
    cont:(Type.t -> 'a) ->
    type_:(Type.t -> 'a) ->
    app:('a -> 'a list -> 'a) ->
    from_value:bool ->
    Reason.t ->
    Type.t ->
    Type.t list ->
    'a

  val builtin_type : Context.t -> cont:(Type.t -> 'a) -> Reason.t -> string -> 'a

  val builtin_typeapp :
    Context.t ->
    cont:(Type.t -> 'a) ->
    type_:(Type.t -> 'a) ->
    app:('a -> 'a list -> 'a) ->
    Reason.t ->
    string ->
    Type.t list ->
    'a
end

module Make (I : INPUT) : S = struct
  type id_key =
    | TVarKey of int
    | EvalKey of Type.Eval.id

  module State = struct
    type t = {
      rec_tvar_ids: ISet.t;
      rec_eval_ids: Type.EvalIdSet.t;
      found_computed_type: bool;
    }

    let empty =
      {
        rec_tvar_ids = ISet.empty;
        rec_eval_ids = Type.EvalIdSet.empty;
        found_computed_type = false;
      }

    let found_computed_type { found_computed_type = x; _ } = x
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

  let rev_mapM f xs = all (Base.List.rev_map ~f xs)

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

  (**************)
  (* Type ctors *)
  (**************)

  let generic_talias name targs = Ty.mk_generic_talias name targs

  let empty_type = Ty.Bot Ty.EmptyType

  let mk_empty bot_kind =
    match bot_kind with
    | Ty.EmptyType -> empty_type
    | Ty.NoLowerWithUpper _ -> Ty.Bot bot_kind

  (* This is intended to only be used by ty_normaizer_debug *)
  let app_on_generic c ts =
    let%bind c = c in
    let%map ts = all ts in
    match c with
    | Ty.Generic (s, k, _) -> Ty.Generic (s, k, Some ts)
    | _ -> c

  (***********************)
  (* Construct built-ins *)
  (***********************)

  let non_opt_param = Ty.{ prm_optional = false }

  let mk_fun
      ?(params = [])
      ?(effect_ = Ty.Arbitrary)
      ?rest
      ?tparams
      ?(static = Ty.(TypeOf (FunProto, None)))
      ret =
    Ty.(
      Fun
        {
          fun_params = params;
          fun_rest_param = rest;
          fun_return = ret;
          fun_type_params = tparams;
          fun_static = static;
          fun_effect = effect_;
        }
    )

  let symbol_from_loc env sym_def_loc sym_name =
    let open File_key in
    let symbol_source = ALoc.source sym_def_loc in
    let sym_provenance =
      match symbol_source with
      | Some (LibFile def_source) ->
        let current_source = Context.file Env.(get_cx env) in
        if File_key.to_string current_source = def_source then
          Ty.Local
        else
          Ty.Library { Ty.imported_as = ALocMap.find_opt sym_def_loc (Env.imported_names env) }
      | Some (SourceFile def_source) ->
        let current_source = Context.file Env.(get_cx env) in
        if File_key.to_string current_source = def_source then
          Ty.Local
        else
          Ty.Remote { Ty.imported_as = ALocMap.find_opt sym_def_loc (Env.imported_names env) }
      | Some (JsonFile _)
      | Some (ResourceFile _) ->
        Ty.Local
      | None -> Ty.Local
    in
    let sym_anonymous = sym_name = OrdinaryName "<<anonymous class>>" in
    { Ty.sym_provenance; sym_name; sym_anonymous; sym_def_loc }

  let ty_symbol_from_symbol env symbol =
    symbol_from_loc
      env
      (FlowSymbol.def_loc_of_symbol symbol)
      (OrdinaryName (FlowSymbol.name_of_symbol symbol))

  (* NOTE Due to repositioning, `reason_loc` may not point to the actual location
     where `name` was defined. *)
  let symbol_from_reason env reason name =
    let def_loc = Reason.def_loc_of_reason reason in
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

  (* Sometimes, we need to inspect Type.t so that we can avoid doing the work of
   * printing giant types repeatedly. See should_force_eval_to_avoid_giant_types below. *)
  let rec unwrap_unless_aliased ~env t =
    let open T in
    match t with
    | OpenT (r, id) ->
      Flow_js_utils.merge_tvar_opt Env.(env.genv.cx) r id
      |> Base.Option.bind ~f:(unwrap_unless_aliased ~env)
    | EvalT (_, _, id) when should_eval_skip_aliases ~env () ->
      Eval.Map.find_opt id (Context.evaluated Env.(env.genv.cx))
      |> Base.Option.bind ~f:(unwrap_unless_aliased ~env)
    | t ->
      (* Type aliases won't be expanded, so we don't waste time printing giant types.
       * As a result, we don't have to unwrap them. *)
      (match desc_of_reason ~unwrap:false (TypeUtil.reason_of_t t) with
      | RTypeAlias (_, Some _, _) -> None
      | _ -> Some t)

  (*
  Without the heuristic, given

  ```
  declare opaque type Id<T>;
  type Obj = $ReadOnly<{ ...giant object... }>
  type IDMap<+Obj: {+[string]: mixed}> = {[Key in keyof Obj]: Id<Obj[Key]>}
  declare const props: { obj: $ReadOnly<IDMap<Obj>> };
  props.obj;
  ```

  we will print something like

  ```
  {
    field1: Id<{...full giant type...}['field1'],
    field2: Id<{...full giant type...}['field2']>,
    ...
  }
  ```

  This heuristic prevents that by detecting we have an indexed access EvalT on objects,
  and then we force the evaluation so we will not hit the potentially expensive unevaluated case.
  *)
  let should_force_eval_to_avoid_giant_types ~env t d =
    let open T in
    match d with
    | PropertyType _
    | ElementType { index_type = DefT (_, (SingletonStrT _ | StrGeneralT _)) }
    | OptionalIndexedAccessNonMaybeType _
    | OptionalIndexedAccessResultType _ ->
      (match unwrap_unless_aliased ~env t with
      | Some (DefT (_, ObjT _)) -> true
      | _ -> false)
    | NonMaybeType
    | ElementType _
    | ExactType
    | ReadOnlyType
    | PartialType
    | RequiredType
    | SpreadType _
    | SpreadTupleType _
    | RestType _
    | ValuesType
    | ConditionalType _
    | TypeMap _
    | ReactElementPropsType
    | ReactElementConfigType
    | ReactCheckComponentConfig _
    | ReactDRO _
    | MakeHooklike
    | MappedType _
    | EnumType ->
      false

  let should_evaluate_destructor ~env ~force_eval t d =
    force_eval
    || should_force_eval_to_avoid_giant_types ~env t d
    || (match d with
       (* If we print out $Omit<Foo, 'bar'> unevaluated, it will end up with something like
        * Omit<Foo, {[K in 'bar']: mixed}>, which is our implementation detail and makes no sense
        * to users. Therefore, let's always evaluate. *)
       | T.RestType (T.Object.Rest.Omit, _) -> true
       | _ -> false)
    ||
    match Env.evaluate_type_destructors env with
    | Env.EvaluateNone -> false
    | Env.EvaluateSome ->
      T.(
        (match d with
        | MappedType _
        | NonMaybeType
        | PropertyType _
        | ElementType _
        | OptionalIndexedAccessNonMaybeType _
        | OptionalIndexedAccessResultType _
        | ConditionalType _ ->
          true
        | ExactType
        | ReadOnlyType
        | ReactDRO _
        | MakeHooklike
        | PartialType
        | RequiredType
        | SpreadType _
        | SpreadTupleType _
        | RestType _
        | ReactCheckComponentConfig _
        | ValuesType
        | TypeMap _
        | EnumType
        | ReactElementPropsType
        | ReactElementConfigType ->
          false)
      )
    | Env.EvaluateAll ->
      (match d with
      | T.ReactDRO _
      | T.MakeHooklike ->
        false
      | _ -> true)

  (* Arguments:
   * - cont: apply when destructuring returned a single type
   * - default: apply when destructuring returned 0 or >1 types, or a recursive type
   * - non_eval: apply when no destructuring happened
   *)
  let eval_t ~env ~(cont : fn_t) ~(default : fn_t) ~non_eval ?(force_eval = false) x =
    let (t, T.TypeDestructorT (_, _, d), id) = x in
    let cx = Env.get_cx env in
    let%bind () = modify (fun state -> { state with State.found_computed_type = true }) in
    let should_eval = should_evaluate_destructor ~env ~force_eval t d in
    I.eval
      cx
      ~should_eval
      ~cont:(cont ~env ~id:(EvalKey id))
      ~default:(default ~env)
      ~non_eval:(non_eval ~env)
      x

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
        | T.ReposLowerT { use_t = u; _ } :: rest -> uses_t_aux acc (u :: rest)
        (* skip these *)
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
      | T.Constraint.Resolved t -> cont ~env ~id:(TVarKey id) t
      | T.Constraint.FullyResolved s ->
        cont ~env ~id:(TVarKey id) (Context.force_fully_resolved_tvar Env.(env.genv.cx) s)
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

  let keys_t ~env ~(cont : fn_t) ?(force_eval = false) r t =
    let cx = Env.get_cx env in
    let default () =
      let%map ty = cont ~env t in
      Ty.Utility (Ty.Keys ty)
    in
    let%bind () = modify (fun state -> { state with State.found_computed_type = true }) in
    let should_evaluate = force_eval || Env.evaluate_type_destructors env <> Env.EvaluateNone in
    I.keys cx ~should_evaluate ~cont:(cont ~env) ~default r t

  module Reason_utils = struct
    let local_type_alias_symbol env reason =
      let rec loop = function
        | REnum { name = Some name } ->
          return (symbol_from_reason env reason (Reason.OrdinaryName name))
        | RTypeAlias (name, Some loc, _) ->
          return (symbol_from_loc env loc (Reason.OrdinaryName name))
        | RType name -> return (symbol_from_reason env reason name)
        | RUnionBranching (desc, _) -> loop desc
        | desc ->
          let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
          let msg = "could not extract local type alias name from reason: " ^ desc in
          terr ~kind:BadTypeAlias ~msg None
      in
      loop (desc_of_reason ~unwrap:false reason)

    let imported_type_alias_symbol env reason =
      let rec loop = function
        | RNamedImportedType (_, name)
        | RDefaultImportedType (name, _)
        | RImportStarType name
        | RImportStarTypeOf name
        | RImportStar name ->
          return (symbol_from_reason env reason (Reason.OrdinaryName name))
        | RType name -> return (symbol_from_reason env reason name)
        | RUnionBranching (desc, _) -> loop desc
        | desc ->
          let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
          let msg = "could not extract imported type alias name from reason: " ^ desc in
          terr ~kind:BadTypeAlias ~msg None
      in
      loop (desc_of_reason ~unwrap:false reason)

    let opaque_type_alias_symbol env reason =
      let rec loop = function
        | ROpaqueType name -> return (symbol_from_reason env reason (Reason.OrdinaryName name))
        | RType name -> return (symbol_from_reason env reason name)
        | RUnionBranching (desc, _) -> loop desc
        | desc ->
          let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
          let msg = "could not extract opaque name from reason: " ^ desc in
          terr ~kind:BadTypeAlias ~msg None
      in
      loop (desc_of_reason ~unwrap:false reason)

    let instance_symbol env reason =
      match desc_of_reason reason with
      | RType name
      | RIdentifier name ->
        (* class or interface declaration *)
        let symbol = symbol_from_reason env reason name in
        return symbol
      | RThisType ->
        let symbol = symbol_from_reason env reason (OrdinaryName "this") in
        return symbol
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract instance name from reason: " ^ desc in
        terr ~kind:BadInstanceT ~msg None

    let component_symbol env name reason = symbol_from_reason env reason (OrdinaryName name)

    let module_symbol_opt env reason =
      match desc_of_reason reason with
      | RModule name ->
        let symbol =
          symbol_from_reason
            env
            reason
            (Reason.OrdinaryName (Flow_import_specifier.display_userland name))
        in
        return (Some symbol)
      | RExports -> return None
      | desc ->
        let desc = Reason.show_virtual_reason_desc (fun _ _ -> ()) desc in
        let msg = "could not extract module name from reason: " ^ desc in
        terr ~kind:UnsupportedTypeCtor ~msg None
  end

  module TypeConverter : sig
    val convert_t : ?skip_reason:bool -> env:Env.t -> Type.t -> (Ty.t, error) t

    val convert_type_params_t :
      env:Env.t -> T.typeparam Nel.t -> (Env.t * Ty.type_param list option, error) t

    val convert_inline_interface :
      env:Env.t -> Type.super -> T.Properties.id -> int option -> T.Object.dict -> (Ty.t, error) t

    val convert_obj_props_t :
      env:Env.t ->
      ?inherited:bool ->
      ?source:Ty.prop_source ->
      ?allowed_prop_names:Reason.name list ->
      T.Properties.id ->
      int option ->
      (Ty.prop list, error) t

    val convert_obj_t :
      env:Env.t ->
      ?inherited:bool ->
      ?source:Ty.prop_source ->
      ?allowed_prop_names:Reason.name list ->
      Reason.reason ->
      Type.objtype ->
      (Ty.obj_t, error) t

    val convert_component :
      env:Env.t ->
      Type.t ->
      Type.component_instance ->
      Type.t ->
      (Ty.component_props * Ty.t option * Ty.t option, error) t

    val convert_type_destructor_unevaluated : env:Env.t -> Type.t -> T.destructor -> (Ty.t, error) t
  end = struct
    let rec type_debug ~env ?id ~depth t state =
      let cx = Env.get_cx env in
      let prefix = spf "%*s[Norm|run_id:%d|depth:%d]" (2 * depth) "" (get_run_id ()) depth in
      prerr_endlinef "%s Input: %s\n" prefix (Debug_js.dump_t cx t);
      let result = type_with_alias_reason ~env ?id t state in
      let result_str =
        match result with
        | (Ok ty, _) -> "[Ok] " ^ Ty_debug.dump_t_EXPOSES_ABSTRACT_LOCS ty
        | (Error e, _) -> "[Error] " ^ error_to_string e
      in
      prerr_endlinef "%s Output: %s\n" prefix result_str;
      result

    and type__ ~env ?id t =
      let%bind env = descend env t in

      let depth = env.Env.depth - 1 in
      if Env.verbose env then
        type_debug ~env ?id ~depth t
      else
        type_with_alias_reason ~env ?id t

    and type_with_alias_reason ~env ?id t =
      let open Type in
      (* These type are treated as transparent when it comes to the type alias
       * annotation. *)
      match t with
      | OpenT _ -> type_ctor ~env ?id ~cont:type_with_alias_reason t
      | EvalT _ when should_eval_skip_aliases ~env () ->
        type_ctor ~env ~cont:type_with_alias_reason t
      | NamespaceT { namespace_symbol; _ } when env.Env.keep_only_namespace_name ->
        let symbol = ty_symbol_from_symbol env namespace_symbol in
        return (Ty.TypeOf (Ty.TSymbol symbol, None))
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
      | GenericT { bound; reason; name; _ } -> generic_t env bound reason name t
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
      | NamespaceT { namespace_symbol = _; values_type; types_tmap = _ } ->
        let env = { env with Env.keep_only_namespace_name = true } in
        cont ~env ?id values_type
      | DefT (_, MixedT _) -> return Ty.Top
      | AnyT (reason, kind) -> return (Ty.Any (any_t reason kind))
      | DefT (_, VoidT) -> return Ty.Void
      | DefT (_, NumGeneralT _) -> return Ty.Num
      | DefT (_, StrGeneralT _) -> return Ty.Str
      | DefT (_, BoolGeneralT) -> return Ty.Bool
      | DefT (_, BigIntGeneralT _) -> return Ty.BigInt
      | DefT (_, EmptyT) -> return (mk_empty Ty.EmptyType)
      | DefT (_, NullT) -> return Ty.Null
      | DefT (_, SymbolT) -> return Ty.Symbol
      | DefT (_, NumericStrKeyT (_, s)) -> return (Ty.StrLit (OrdinaryName s))
      | DefT (_, SingletonNumT { value = (_, lit); _ }) -> return (Ty.NumLit lit)
      | DefT (_, SingletonStrT { value = lit; _ }) -> return (Ty.StrLit lit)
      | DefT (_, SingletonBoolT { value = lit; _ }) -> return (Ty.BoolLit lit)
      | DefT (_, SingletonBigIntT { value = (_, lit); _ }) -> return (Ty.BigIntLit lit)
      | StrUtilT { reason = _; op; remainder = None } ->
        return
          (Ty.Utility
             (match op with
             | StrPrefix prefix -> Ty.StringPrefix { prefix; remainder = None }
             | StrSuffix suffix -> Ty.StringSuffix { suffix; remainder = None })
          )
      | StrUtilT { reason = _; op; remainder = Some remainder } ->
        let%bind remainder = type__ ~env remainder in
        return
          (Ty.Utility
             (match op with
             | StrPrefix prefix -> Ty.StringPrefix { prefix; remainder = Some remainder }
             | StrSuffix suffix -> Ty.StringSuffix { suffix; remainder = Some remainder })
          )
      | MaybeT (_, t) -> maybe_t ~env ?id ~cont:type__ t
      | OptionalT { type_ = t; _ } -> optional_t ~env ?id ~cont:type__ t
      | DefT (_, FunT (static, f)) ->
        let%map t = fun_ty ~env static f None in
        Ty.Fun t
      | DefT (r, ObjT o) ->
        let%map o = obj_ty ~env r o in
        Ty.Obj o
      | DefT (r, ArrT a) -> arr_ty ~env r a
      | UnionT (_, rep) -> app_union ~from_bounds:false ~f:(type__ ~env ?id) rep
      | IntersectionT (_, rep) -> app_intersection ~f:(type__ ~env ?id) rep
      | DefT (_, PolyT { tparams = ps; t_out = t; _ }) -> poly_ty ~env t ps
      | TypeAppT { reason = _; use_op = _; type_; targs; from_value; use_desc = _ } ->
        type_app ~env ~from_value type_ (Some targs)
      | ThisInstanceT (r, { super; inst; _ }, _, _)
      | DefT (r, InstanceT { super; inst; _ }) ->
        instance_t ~env r super inst
      | DefT (_, ClassT (ThisInstanceT (r, t, _, _))) -> this_class_t ~env r t
      | DefT (_, ClassT t) ->
        let%map ty = type__ ~env t in
        Ty.Utility (Ty.Class ty)
      | DefT
          ( r,
            ReactAbstractComponentT
              {
                config = _;
                instance = _;
                renders = _;
                component_kind = Nominal (_, name, inferred_targs);
              }
          ) ->
        let%bind inferred_targs =
          match inferred_targs with
          | None -> return None
          | Some inferred_targs ->
            let%bind inferred_targs = mapM (type__ ~env) inferred_targs in
            return (Some inferred_targs)
        in
        let symbol = Reason_utils.component_symbol env name r in
        return (Ty.TypeOf (Ty.TSymbol symbol, inferred_targs))
      | DefT (_, ReactAbstractComponentT { config; instance; renders; component_kind = _ }) ->
        let%bind (regular_props, ref_prop, renders) =
          convert_component ~env config instance renders
        in
        return (Ty.Component { regular_props; ref_prop; renders })
      | DefT (_, RendersT (IntrinsicRenders n)) -> return (Ty.StrLit (OrdinaryName n))
      | DefT (_, RendersT (NominalRenders { renders_id; renders_name; _ })) ->
        let symbol =
          Reason_utils.component_symbol
            env
            renders_name
            (mk_reason (RComponent (OrdinaryName renders_name)) (renders_id :> ALoc.t))
        in
        return (Ty.Generic (symbol, Ty.ComponentKind, None))
      | DefT (_, RendersT (StructuralRenders { renders_variant; renders_structural_type })) ->
        let%bind ty = type_ctor ~env ~cont renders_structural_type in
        let variant =
          match renders_variant with
          | T.RendersNormal -> Ty.RendersNormal
          | T.RendersMaybe -> Ty.RendersMaybe
          | T.RendersStar -> Ty.RendersStar
        in
        return (Ty.Renders (ty, variant))
      | DefT (r, RendersT DefaultRenders) ->
        let ty =
          Ty.Generic
            ( Reason_utils.component_symbol
                env
                "React.Node"
                (mk_reason (RComponent (OrdinaryName "React.Node")) (loc_of_reason r)),
              Ty.ComponentKind,
              None
            )
        in
        return (Ty.Renders (ty, Ty.RendersNormal))
      | ThisTypeAppT (_, c, _, ts) -> type_app ~env ~from_value:false c ts
      | KeysT (r, t) -> keys_t ~env ~cont:type__ r t
      | OpaqueT (r, o) -> opaque_t ~env r o
      | ObjProtoT _ -> return Ty.(TypeOf (ObjProto, None))
      | FunProtoT _ -> return Ty.(TypeOf (FunProto, None))
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
                (ReturnType explicit_any)
            )
        else
          return Ty.(TypeOf (FunProtoBind, None))
      | NullProtoT _ -> return Ty.Null
      | DefT (reason, EnumObjectT _) ->
        let%map symbol = Reason_utils.local_type_alias_symbol env reason in
        Ty.TypeOf (Ty.TSymbol symbol, None)
      | DefT (reason, EnumValueT _) ->
        let%map symbol = Reason_utils.local_type_alias_symbol env reason in
        Ty.Generic (symbol, Ty.EnumKind, None)
      (* Top-level only *)
      | DefT (_, TypeT _) -> terr ~kind:(UnexpectedTypeCtor (string_of_ctor t)) (Some t)

    and any_t reason kind =
      match kind with
      | T.(AnnotatedAny | CatchAny) ->
        let aloc = Reason.loc_of_reason reason in
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
      | T.Constructor -> Ty.Constructor
      | T.DummyStatic -> Ty.DummyStatic
      | T.Exports -> Ty.Exports
      | T.InferenceHooks -> Ty.InferenceHooks
      | T.InstanceOfRefinement -> Ty.InstanceOfRefinement
      | T.Merged -> Ty.Merged
      | T.ResolveSpread -> Ty.ResolveSpread
      | T.Unchecked -> Ty.Unchecked
      | T.Unimplemented -> Ty.Unimplemented
      | T.UnresolvedType -> Ty.UnresolvedType
      | T.NonBindingPattern -> Ty.NonBindingPattern

    and fun_ty ~env static f fun_type_params =
      let%bind fun_static = type__ ~env static in
      let { T.params; rest_param; return_t; type_guard; effect_; _ } = f in
      let fun_effect =
        match effect_ with
        | T.HookAnnot
        | T.HookDecl _ ->
          Ty.Hook
        | T.ArbitraryEffect -> Ty.Arbitrary
        | T.AnyEffect -> Ty.Arbitrary (* TODO do we need an any-ful ty rep? *)
      in
      let%bind fun_params = mapM (fun_param ~env) params in
      let%bind fun_rest_param = fun_rest_param_t ~env rest_param in
      let%bind fun_return =
        match type_guard with
        | Some
            (T.TypeGuard
              { reason = _; one_sided; inferred = _; param_name = (_, x); type_guard = t }
              ) ->
          let%map t = type__ ~env t in
          Ty.TypeGuard (one_sided, x, t)
        | None ->
          let%map t = type__ ~env return_t in
          Ty.ReturnType t
      in
      return { Ty.fun_params; fun_rest_param; fun_return; fun_type_params; fun_static; fun_effect }

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

    and obj_ty ~env ?(inherited = false) ?(source = Ty.Other) ?allowed_prop_names reason o =
      let obj_def_loc = Some (Reason.def_loc_of_reason reason) in
      let { T.flags; props_tmap; call_t; _ } = o in
      let { T.obj_kind; _ } = flags in
      let obj_literal =
        if Env.preserve_inferred_literal_types env then
          Some (Reason.is_literal_object_reason reason)
        else
          None
      in
      let%bind obj_props =
        obj_props_t ~env ~inherited ~source ?allowed_prop_names props_tmap call_t
      in
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
      { Ty.obj_def_loc; obj_kind; obj_literal; obj_props }

    and obj_prop_t =
      let def_locs ~fallback_t p =
        match T.Property.def_locs p with
        | None -> [TypeUtil.loc_of_t fallback_t]
        | Some (hd, tl) -> hd :: tl
      in
      fun ~env ?(inherited = false) ?(source = Ty.Other) (x, p) ->
        match p with
        | T.Field { preferred_def_locs = _; key_loc = _; type_; polarity } ->
          let polarity = type_polarity polarity in
          let%map (t, optional) = opt_t ~env type_ in
          let prop = Ty.Field { t; polarity; optional } in
          [
            Ty.NamedProp
              { name = x; prop; inherited; source; def_locs = def_locs ~fallback_t:type_ p };
          ]
        | T.Method { key_loc = _; type_ = t } ->
          let%map tys = method_ty ~env t in
          Base.List.map
            ~f:(fun ty ->
              Ty.NamedProp
                {
                  name = x;
                  prop = Ty.Method ty;
                  inherited;
                  source;
                  def_locs = def_locs ~fallback_t:t p;
                })
            tys
        | T.Get { key_loc = _; type_ } ->
          let%map t = type__ ~env type_ in
          [
            Ty.NamedProp
              {
                name = x;
                prop = Ty.Get t;
                inherited;
                source;
                def_locs = def_locs ~fallback_t:type_ p;
              };
          ]
        | T.Set { key_loc = _; type_ } ->
          let%map t = type__ ~env type_ in
          [
            Ty.NamedProp
              {
                name = x;
                prop = Ty.Set t;
                inherited;
                source;
                def_locs = def_locs ~fallback_t:type_ p;
              };
          ]
        | T.GetSet { get_key_loc; get_type; set_key_loc; set_type } ->
          let%bind p1 = obj_prop_t ~env (x, T.Get { key_loc = get_key_loc; type_ = get_type }) in
          let%map p2 = obj_prop_t ~env (x, T.Set { key_loc = set_key_loc; type_ = set_type }) in
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
      fun ~env ?(inherited = false) ?(source = Ty.Other) ?allowed_prop_names props_id call_id_opt ->
        let cx = Env.get_cx env in
        let prop_map = Context.find_props cx props_id in
        let props =
          match allowed_prop_names with
          | Some names ->
            Base.List.filter_map
              ~f:(fun name ->
                match NameUtils.Map.find_opt name prop_map with
                | Some v -> Some (name, v)
                | None -> None)
              names
          | None -> NameUtils.Map.bindings prop_map
        in
        let%bind call_props = do_calls ~env call_id_opt in
        let%map props = do_props ~env ~inherited ~source props in
        call_props @ props

    and arr_ty ~env reason elt_t =
      let desc = Reason.desc_of_reason reason in
      let arr_literal =
        if Env.preserve_inferred_literal_types env then
          Some
            (match desc with
            | RArrayLit_UNSOUND -> true
            | _ -> false)
        else
          None
      in
      match (elt_t, desc) with
      | ( T.ArrayAT
            {
              elem_t = _;
              tuple_view = Some (T.TupleView { elements = elements'; inexact; arity = _ });
              react_dro = _;
            },
          RRestArrayLit _
        )
      | (T.TupleAT { elements = elements'; inexact; _ }, _) ->
        (* Heuristic to use $ReadOnly<> instead of repeating the polarity symbol
         * and a made up label in each element. *)
        let readonly =
          Base.List.for_all
            ~f:(fun (T.TupleElement { polarity; _ }) -> polarity = Polarity.Positive)
            elements'
        in
        let%map elements =
          mapM
            (fun (T.TupleElement { name; t; polarity; optional; reason = _ }) ->
              let t =
                match t with
                | T.OptionalT { type_; _ } when optional -> type_
                | _ -> t
              in
              let%map t = type__ ~env t in
              let polarity =
                if readonly then
                  Ty.Neutral
                else
                  type_polarity polarity
              in
              Ty.TupleElement { name; t; polarity; optional })
            elements'
        in
        let t = Ty.Tup { elements; inexact } in
        if readonly then
          Ty.Utility (Ty.ReadOnly t)
        else
          t
      | (T.ArrayAT { elem_t; _ }, _) ->
        let%map arr_elt_t = type__ ~env elem_t in
        Ty.Arr { Ty.arr_readonly = false; arr_literal; arr_elt_t }
      | (T.ROArrayAT (t, _), _) ->
        let%map t = type__ ~env t in
        Ty.Arr { Ty.arr_readonly = true; arr_literal; arr_elt_t = t }

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
      let { T.inst_kind; own_props; inst_call_t; inst_dict; _ } = inst in
      let desc = desc_of_reason ~unwrap:false r in
      match (inst_kind, desc) with
      | (T.InterfaceKind { inline = true }, _) ->
        inline_interface ~env super own_props inst_call_t inst_dict
      | (T.InterfaceKind { inline = false }, _) -> to_generic ~env Ty.InterfaceKind r inst
      | (T.ClassKind, _) -> to_generic ~env Ty.ClassKind r inst

    and inline_interface =
      let rec extends = function
        | Ty.Generic g -> return [g]
        | Ty.Inter (t1, t2, ts) -> mapM extends (t1 :: t2 :: ts) >>| Base.List.concat
        | Ty.TypeOf (Ty.ObjProto, _) (* interface {} *)
        | Ty.TypeOf (Ty.FunProto, _) (* interface { (): void } *) ->
          (* Do not contribute to the extends clause *)
          return []
        | _ ->
          (* Top-level syntax only allows generics in extends *)
          terr ~kind:BadInlineInterfaceExtends None
      in
      fun ~env super own_props inst_call_t inst_dict ->
        let%bind super = type__ ~env super in
        let%bind if_extends = extends super in
        let%bind if_props = obj_props_t ~env own_props inst_call_t in
        let%map if_dict =
          match inst_dict with
          | Some { T.dict_polarity; dict_name; key; value } ->
            let dict_polarity = type_polarity dict_polarity in
            let%bind dict_key = type__ ~env key in
            let%bind dict_value = type__ ~env value in
            return (Some { Ty.dict_polarity; dict_name; dict_key; dict_value })
          | None -> return None
        in
        Ty.InlineInterface { Ty.if_extends; if_props; if_dict }

    and convert_component ~env config instance renders =
      let%bind config = type__ ~env config in
      let%bind ref_prop =
        match instance with
        | Type.ComponentInstanceOmitted _ -> return None
        | Type.ComponentInstanceAvailableAsRefSetterProp t -> type__ ~env t >>| Base.Option.some
      in
      let%bind renders =
        match renders with
        | Type.(DefT (_, RendersT DefaultRenders)) -> return None
        | _ -> type__ ~env renders >>| Base.Option.some
      in
      let regular_props =
        let props_flattened =
          match config with
          | Ty.Obj
              {
                Ty.obj_def_loc = _;
                obj_literal = Some false | None;
                obj_props = [Ty.SpreadProp config];
                obj_kind = Ty.ExactObj;
              } ->
            Error config
          | Ty.Obj
              {
                Ty.obj_def_loc = _;
                obj_literal = Some false | None;
                obj_props;
                obj_kind = (Ty.ExactObj | Ty.InexactObj) as obj_kind;
              } ->
            Base.List.fold_result obj_props ~init:[] ~f:(fun acc -> function
              | Ty.NamedProp { name; prop = Ty.Field { t; polarity = _; optional }; def_locs; _ } ->
                let prop = Ty.FlattenedComponentProp { name; optional; def_locs; t } in
                Ok (prop :: acc)
              | _ -> Error config
            )
            |> Base.Result.map ~f:(fun props -> (props, obj_kind = Ty.InexactObj))
          | _ -> Error config
        in
        match props_flattened with
        | Ok (props, inexact) -> Ty.FlattenedComponentProps { props = List.rev props; inexact }
        | Error config -> Ty.UnflattenedComponentProps config
      in
      return (regular_props, ref_prop, renders)

    and this_class_t ~env r t =
      let open Type in
      match t with
      | { inst = { inst_kind = ClassKind; _ }; _ } ->
        let%map symbol = Reason_utils.instance_symbol env r in
        Ty.TypeOf (Ty.TSymbol symbol, None)
      | { inst = { inst_kind = InterfaceKind _; _ }; _ } ->
        terr ~kind:BadThisClassT ~msg:"InterfaceKind" (Some (DefT (r, InstanceT t)))

    and type_params_t ~env tparams =
      let results = Nel.map (fun tp -> type_param ~env tp) tparams in
      let%map ps = Nel.to_list results |> all in
      let ps =
        match ps with
        | [] -> None
        | _ -> Some ps
      in
      (env, ps)

    and poly_ty ~env t typeparams =
      let open Type in
      match t with
      | DefT (_, ClassT (ThisInstanceT (r, t, _, _))) -> this_class_t ~env r t
      | DefT (_, FunT (static, f)) ->
        let%bind (env, ps) = type_params_t ~env typeparams in
        let%map fun_t = fun_ty ~env static f ps in
        Ty.Fun fun_t
      | DefT (_, ReactAbstractComponentT _) -> type__ ~env t
      | _ -> terr ~kind:BadPoly (Some t)

    and type_app =
      let mk_generic ~env symbol kind tparams targs =
        let%bind targs = optMapM (type__ ~env) targs in
        let%map targs =
          if Env.omit_targ_defaults env then
            (* Disable the option for recursive calls to type_params_t to avoid
             * infinite recursion in cases like `class C<T: C<any>> {}` *)
            let env = Env.{ env with omit_targ_defaults = false } in
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
          | InstanceKind
          | RenderTypeKind ->
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
        | DefT
            ( r,
              TypeT
                ( _,
                  DefT
                    ( _,
                      InstanceT
                        { inst = { inst_kind = InterfaceKind { inline = true }; _ } as inst; _ }
                    )
                )
            )
        | DefT (_, TypeT (_, DefT (r, InstanceT { inst; _ })))
        | DefT (_, ClassT (ThisInstanceT (r, { inst; _ }, _, _)))
        | DefT (_, ClassT (DefT (r, InstanceT { inst; _ }))) ->
          instance_app ~env r inst tparams targs
        | DefT (r, TypeT (kind, _)) -> type_t_app ~env r kind tparams targs
        | DefT
            ( r,
              ReactAbstractComponentT
                { component_kind = Nominal (_, name, _); config = _; instance = _; renders = _ }
            ) ->
          let symbol = Reason_utils.component_symbol env name r in
          mk_generic ~env symbol Ty.ComponentKind tparams targs
        | DefT
            ( _,
              ClassT
                (TypeAppT { reason = _; use_op = _; type_; targs = _; from_value; use_desc = _ })
            ) ->
          type_app ~env ~from_value type_ targs
        | _ ->
          let msg = "PolyT:" ^ Type.string_of_ctor t in
          terr ~kind:BadTypeApp ~msg None
      in
      let rec singleton ~env ~from_value targs t =
        let open Type in
        match t with
        | AnyT _ -> type__ ~env t
        (* e.g. typeof functionDef<targ1, targ2> *)
        | DefT (reason, PolyT _) when from_value && Option.is_some targs ->
          I.typeapp
            (Env.get_cx env)
            ~cont:(type__ ~env)
            ~type_:(type__ ~env)
            ~app:app_on_generic
            ~from_value
            reason
            t
            (Option.get targs)
        | DefT (_, PolyT { tparams; t_out; _ }) -> singleton_poly ~env targs tparams t_out
        | DefT (_, ClassT (ThisInstanceT (r, t, _, _))) ->
          (* This is likely an error - cannot apply on non-polymorphic type.
           * E.g type Foo = any; var x: Foo<number> *)
          type__ ~env (DefT (r, InstanceT t))
        | DefT (_, TypeT (_, t)) ->
          (* This is likely an error - cannot apply on non-polymorphic type.
           * E.g type Foo = any; var x: Foo<number> *)
          type__ ~env t
        | DefT (_, ClassT t) when targs = None ->
          (* For example see tests/type-at-pos_class/FluxStore.js *)
          let%map t = type__ ~env t in
          Ty.Utility (Ty.Class t)
        | UnionT (_, union_rep) ->
          (* This case targeting UnionTs created during tvar_resolution. *)
          let%map tys = mapM (singleton ~env ~from_value targs) (union_rep |> T.UnionRep.members) in
          (match tys with
          | [] -> Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper)
          | t :: ts -> Ty.mk_union ~from_bounds:true (t, ts))
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
      fun ~env ~from_value t targs ->
        match Lookahead.peek (Env.get_cx env) t with
        | Lookahead.Recursive -> terr ~kind:BadTypeApp ~msg:"recursive" (Some t)
        | Lookahead.LowerBounds [] ->
          (* It's unlikely that an upper bound would be useful here *)
          return (Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper))
        | Lookahead.LowerBounds ts ->
          (* TypeAppT distributes over multiple lower bounds. *)
          let%map tys = mapM (singleton ~env ~from_value targs) ts in
          (match tys with
          | [] -> Ty.Bot (Ty.NoLowerWithUpper Ty.NoUpper)
          | t :: ts -> Ty.mk_union ~from_bounds:true (t, ts))

    and opaque_t ~env reason opaque_type =
      let { Type.opaque_type_args = targs; opaque_name; _ } = opaque_type in
      let opaque_symbol = symbol_from_reason env reason (Reason.OrdinaryName opaque_name) in
      let%map targs =
        match targs with
        | [] -> return None
        | _ -> optMapM (fun (_, _, t, _) -> type__ ~env t) (Some targs)
      in
      generic_talias opaque_symbol targs

    and subst_name ~env loc t bound name =
      match name with
      | Subst_name.Name name
      | Subst_name.Id (_, name)
      | Subst_name.Synthetic { op_kind = None; name; _ } ->
        (* When `op_kind` is None we can rely on `name`. *)
        return (Ty.Bound (loc, name))
      | Subst_name.Synthetic { op_kind = Some Subst_name.ReadOnly; ts = [name]; _ } ->
        let%map t = subst_name ~env loc t bound name in
        Ty.Utility (Ty.ReadOnly t)
      | Subst_name.Synthetic { op_kind = Some Subst_name.Partial; ts = [name]; _ } ->
        let%map t = subst_name ~env loc t bound name in
        Ty.Utility (Ty.Partial t)
      | Subst_name.Synthetic { op_kind = Some Subst_name.Required; ts = [name]; _ } ->
        let%map t = subst_name ~env loc t bound name in
        Ty.Utility (Ty.Required t)
      | Subst_name.Synthetic { op_kind = Some Subst_name.Spread; ts; _ } ->
        let%bind obj_props =
          mapM
            (fun b ->
              let%map t = subst_name ~env loc t bound b in
              Ty.SpreadProp t)
            ts
        in
        let%map ty_bound = type__ ~env bound in
        let (obj_props, obj_kind, obj_literal) =
          match ty_bound with
          | Ty.Obj { Ty.obj_props = bound_props; obj_kind; obj_literal; _ } ->
            (obj_props @ bound_props, obj_kind, obj_literal)
          | _ -> (obj_props, Ty.ExactObj, None)
        in
        Ty.Obj { Ty.obj_def_loc = None; obj_props; obj_kind; obj_literal }
      | Subst_name.Synthetic { op_kind = Some _; ts = _; name = _ } ->
        terr ~kind:SyntheticBoundT (Some t)

    and generic_t env bound reason name t =
      let loc = Reason.def_loc_of_reason reason in
      let pred { T.name = name'; reason; _ } =
        name = name' && loc = Reason.def_loc_of_reason reason
      in
      (* GenericT normalizes to Ty.Bound, except for conditional "infer" types. *)
      match List.find_opt pred env.Env.infer_tparams with
      | Some { T.name = name'; reason; bound; _ } ->
        let symbol =
          symbol_from_reason env reason (OrdinaryName (Subst_name.string_of_subst_name name'))
        in
        let%map bound = param_bound ~env bound in
        Ty.Infer (symbol, bound)
      | None -> subst_name ~env loc t bound name

    and param_bound ~env = function
      | T.DefT (_, T.MixedT _) -> return None
      | bound ->
        let%bind b = type__ ~env bound in
        return (Some b)

    and default_t ~env = function
      | Some d ->
        let%bind d = type__ ~env d in
        return (Some d)
      | _ -> return None

    and type_param ~env tp =
      let { T.name; bound; polarity; default; is_const; _ } = tp in
      let tp_polarity = type_polarity polarity in
      let%bind tp_bound = param_bound ~env bound in
      let%map tp_default = default_t ~env default in
      {
        Ty.tp_name = Subst_name.string_of_subst_name name;
        tp_bound;
        tp_polarity;
        tp_default;
        tp_const = is_const;
      }

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
    and spread_of_ty = function
      | Ty.Obj { Ty.obj_props; _ } -> obj_props
      | t -> [Ty.SpreadProp t]

    and spread =
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
        Ty.Obj { Ty.obj_def_loc = None; obj_props; obj_kind; obj_literal = None }
      in
      let spread_operand_slice ~env { T.Object.Spread.reason = _; prop_map; dict; _ } =
        Type.TypeTerm.(
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
          return (Ty.Obj { Ty.obj_def_loc = None; obj_kind; obj_literal; obj_props })
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

    and tuple_spread ~env ~inexact ty resolved_rev unresolved =
      let%bind head =
        rev_mapM
          (function
            | T.ResolvedArg (T.TupleElement { reason = _; name; t; polarity; optional }, _) ->
              let%map t = type__ ~env t in
              [Ty.TupleElement { name; t; polarity = type_polarity polarity; optional }]
            | T.ResolvedSpreadArg (r, arr, _) ->
              (match arr with
              | T.TupleAT { elements; _ } ->
                mapM
                  (function
                    | T.TupleElement { reason = _; name; t; polarity; optional } ->
                      let%map t = type__ ~env t in
                      Ty.TupleElement { name; t; polarity = type_polarity polarity; optional })
                  elements
              | _ ->
                let%map t = arr_ty ~env r arr in
                [Ty.TupleSpread { name = None; t }])
            | T.ResolvedAnySpreadArg (reason, src) ->
              let%map t = return (Ty.Any (any_t reason src)) in
              [Ty.TupleSpread { name = None; t }])
          resolved_rev
        >>| Base.List.concat
      in
      let spread_ty = Ty.TupleSpread { name = None; t = ty } in
      let%bind tail =
        mapM
          (function
            | T.UnresolvedArg (T.TupleElement { reason = _; name; t; polarity; optional }, _) ->
              let%map t = type__ ~env t in
              Ty.TupleElement { name; t; polarity = type_polarity polarity; optional }
            | T.UnresolvedSpreadArg t ->
              let%map t = type__ ~env t in
              Ty.TupleSpread { name = None; t })
          unresolved
      in
      return (Ty.Tup { elements = head @ (spread_ty :: tail); inexact })

    and check_component ~env ty pmap =
      let%bind map_props =
        NameUtils.Map.bindings pmap |> concat_fold_m (obj_prop_t ~env ?inherited:None ?source:None)
      in
      let obj_props = spread_of_ty ty @ map_props in
      let obj_kind = Ty.ExactObj in
      return (Ty.Obj { Ty.obj_def_loc = None; obj_props; obj_kind; obj_literal = None })

    and mapped_type ~env source property_type mapped_type_flags homomorphic =
      let%bind (key_tparam, prop) =
        Type.TypeTerm.(
          match property_type with
          | DefT (_, PolyT { tparams = (key_tparam, []); t_out; _ }) ->
            let%bind key_tparam_ty = type_param ~env key_tparam in
            let%bind property_ty = type__ ~env t_out in
            return (key_tparam_ty, property_ty)
          | _ -> terr ~kind:BadMappedType (Some property_type)
        )
      in
      let { Type.TypeTerm.variance; optional } = mapped_type_flags in
      let optional =
        match optional with
        | Type.MakeOptional -> Ty.MakeOptional
        | Type.RemoveOptional -> Ty.RemoveOptional
        | Type.KeepOptionality -> Ty.KeepOptionality
      in
      let flags = { Ty.optional; polarity = type_polarity variance } in
      let%bind homomorphic =
        Type.(
          match homomorphic with
          | Homomorphic -> return Ty.Homomorphic
          | SemiHomomorphic t ->
            let%bind t = type__ ~env t in
            return (Ty.SemiHomomorphic t)
          | Unspecialized -> return Ty.Unspecialized
        )
      in
      let prop = Ty.(MappedTypeProp { key_tparam; source; prop; flags; homomorphic }) in
      let obj_t =
        {
          Ty.obj_def_loc = None;
          obj_literal = None;
          obj_props = [prop];
          obj_kind = Ty.MappedTypeObj;
        }
      in
      return (Ty.Obj obj_t)

    and type_destructor_unevaluated ~env t d =
      let%bind ty = type__ ~env t in
      match d with
      | T.MakeHooklike
      | T.ReactDRO _ ->
        return ty
      | T.NonMaybeType -> return (Ty.Utility (Ty.NonMaybeType ty))
      | T.ExactType -> return (Ty.Utility (Ty.Exact ty))
      | T.ReadOnlyType -> return (Ty.Utility (Ty.ReadOnly ty))
      | T.EnumType -> return (Ty.Utility (Ty.Enum ty))
      | T.PartialType -> return (Ty.Utility (Ty.Partial ty))
      | T.RequiredType -> return (Ty.Utility (Ty.Required ty))
      | T.ValuesType -> return (Ty.Utility (Ty.Values ty))
      | T.ElementType { index_type } ->
        let%map index = type__ ~env index_type in
        Ty.IndexedAccess { _object = ty; index; optional = false }
      | T.OptionalIndexedAccessNonMaybeType { index } ->
        let%map index' =
          match index with
          | T.OptionalIndexedAccessTypeIndex index_type -> type__ ~env index_type
          | T.OptionalIndexedAccessStrLitIndex name -> return @@ Ty.StrLit name
        in
        Ty.IndexedAccess { _object = ty; index = index'; optional = true }
      | T.OptionalIndexedAccessResultType _ -> return ty
      | T.ConditionalType
          { distributive_tparam_name = _; infer_tparams; extends_t; true_t; false_t } ->
        let check_type = ty in
        let%bind extends_type = type__ ~env:{ env with Env.infer_tparams } extends_t in
        let%bind true_type = type__ ~env true_t in
        let%map false_type = type__ ~env false_t in
        Ty.Conditional { check_type; extends_type; true_type; false_type }
      | T.TypeMap T.ObjectKeyMirror -> return (Ty.Utility (Ty.ObjKeyMirror ty))
      | T.PropertyType { name } ->
        let index = Ty.StrLit name in
        return @@ Ty.IndexedAccess { _object = ty; index; optional = false }
      | T.RestType (T.Object.Rest.Omit, t') ->
        let%map ty' = type__ ~env t' in
        Ty.Utility (Ty.Omit (ty, ty'))
      | T.SpreadType (target, operands, head_slice) -> spread ~env ty target operands head_slice
      | T.SpreadTupleType { inexact; resolved_rev; unresolved; _ } ->
        tuple_spread ~env ~inexact ty resolved_rev unresolved
      | T.ReactCheckComponentConfig pmap -> check_component ~env ty pmap
      | T.ReactElementPropsType -> return (Ty.Utility (Ty.ReactElementPropsType ty))
      | T.ReactElementConfigType -> return (Ty.Utility (Ty.ReactElementConfigType ty))
      | T.RestType ((T.Object.Rest.SpreadReversal | T.Object.Rest.ReactConfigMerge _), _) as d ->
        terr ~kind:BadEvalT ~msg:(Debug_js.string_of_destructor d) None
      | T.MappedType { property_type; mapped_type_flags; homomorphic; distributive_tparam_name } ->
        let (property_type, homomorphic) =
          Flow_js_utils.substitute_mapped_type_distributive_tparams
            (Env.get_cx env)
            ~use_op:Type.unknown_use
            distributive_tparam_name
            ~property_type
            homomorphic
            ~source:t
        in
        mapped_type ~env ty property_type mapped_type_flags homomorphic

    let rec type_ctor_ = type_ctor ~cont:type_ctor_

    let reset_env env =
      { env with Env.seen_tvar_ids = ISet.empty; seen_eval_ids = Type.EvalIdSet.empty }

    let convert_t ?(skip_reason = false) ~env =
      if skip_reason then
        type_ctor_ ~env:(reset_env env) ?id:None
      else
        type__ ~env ?id:None

    let convert_type_params_t ~env = type_params_t ~env:(reset_env env)

    let convert_inline_interface ~env = inline_interface ~env:(reset_env env)

    let convert_obj_props_t ~env = obj_props_t ~env:(reset_env env)

    let convert_obj_t ~env = obj_ty ~env:(reset_env env)

    let convert_type_destructor_unevaluated ~env = type_destructor_unevaluated ~env:(reset_env env)
  end

  module ElementConverter : sig
    val module_t : env:Env.t -> reason -> T.exporttypes -> (Ty.decl, error) t

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
      let current_source = Context.file (Env.get_cx env) in
      let opaque_source = ALoc.source (def_loc_of_reason reason) in
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
          | { upper_t = Some t; _ } ->
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
        let%map t = TypeConverter.convert_t ~skip_reason:true ~env t in
        Ty.Decl (Ty.TypeAliasDecl { import = false; name; tparams; type_ = Some t })
      in
      let import env reason t tparams =
        let%bind name = Reason_utils.imported_type_alias_symbol env reason in
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
      let type_param r t =
        match desc_of_reason r with
        | RType name ->
          let loc = Reason.def_loc_of_reason r in
          return (Ty.Type (Ty.Bound (loc, display_string_of_name name)))
        | desc -> terr ~kind:BadTypeAlias ~msg:(spf "type param: %s" (string_of_desc desc)) (Some t)
      in
      let class_ env t =
        let ct = T.DefT (TypeUtil.reason_of_t t, T.ClassT t) in
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
        | TypeParamKind -> type_param r t
        (* The following cases are not common *)
        | InstanceKind -> terr ~kind:BadTypeAlias ~msg:"instance" (Some t)
        | RenderTypeKind -> terr ~kind:BadTypeAlias ~msg:"render type" (Some t)

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
      let class_or_interface_decl ~env r tparams super inst =
        let%bind ps =
          match tparams with
          | Some tparams ->
            let%map (_, ps) = TypeConverter.convert_type_params_t ~env tparams in
            ps
          | None -> return None
        in
        let { T.inst_kind; own_props; inst_call_t; inst_dict; _ } = inst in
        let desc = desc_of_reason ~unwrap:false r in
        match (inst_kind, desc) with
        | (T.InterfaceKind { inline = false }, _) ->
          let%map symbol = Reason_utils.instance_symbol env r in
          Ty.Decl (Ty.InterfaceDecl (symbol, ps))
        | (T.InterfaceKind { inline = true }, _) ->
          let%map ty =
            TypeConverter.convert_inline_interface ~env super own_props inst_call_t inst_dict
          in
          Ty.Type ty
        | (T.ClassKind, _) ->
          let%map symbol = Reason_utils.instance_symbol env r in
          Ty.Decl (Ty.ClassDecl (symbol, ps))
      in
      let component_decl ~env ?targs tparams config instance renders name reason =
        let%bind tparams =
          match tparams with
          | Some tparams ->
            let%map (_, ps) = TypeConverter.convert_type_params_t ~env tparams in
            ps
          | None -> return None
        in
        let%bind targs = optMapM (TypeConverter.convert_t ~env) targs in
        let%map (props, instance, renders) =
          TypeConverter.convert_component ~env config instance renders
        in
        Ty.Decl
          (Ty.NominalComponentDecl
             {
               name = Reason_utils.component_symbol env name reason;
               tparams;
               targs;
               props;
               instance;
               renders;
               is_type = Env.toplevel_is_type_identifier_reference env;
             }
          )
      in
      let enum_decl ~env reason =
        let%bind symbol = Reason_utils.local_type_alias_symbol env reason in
        return (Ty.Decl Ty.(EnumDecl symbol))
      in
      let singleton_poly ~env ~orig_t tparams = function
        (* Imported interfaces *)
        | DefT (_, TypeT (ImportClassKind, DefT (r, InstanceT { super; inst; _ }))) ->
          class_or_interface_decl ~env r (Some tparams) super inst
        (* Classes *)
        | DefT (_, ClassT (ThisInstanceT (r, { super; inst; _ }, _, _)))
        (* Interfaces *)
        | DefT (_, ClassT (DefT (r, InstanceT { super; inst; _ }))) ->
          class_or_interface_decl ~env r (Some tparams) super inst
        (* See flow_js.ml canonicalize_imported_type, case of PolyT (ThisClassT):
           The initial abstraction is wrapper within an abstraction and a type application.
           The current case unwraps the abstraction and application to reveal the
           initial imported type. *)
        | DefT
            ( _,
              ClassT
                (TypeAppT { reason = _; use_op = _; type_; targs = _; from_value = _; use_desc = _ })
            ) ->
          toplevel ~env type_
        | DefT
            ( reason,
              ReactAbstractComponentT
                { component_kind = Nominal (_, name, targs); config; instance; renders }
            ) ->
          component_decl ~env ?targs (Some tparams) config instance renders name reason
        (* Type Aliases *)
        | DefT (r, TypeT (kind, t)) ->
          let%bind (env, ps) = TypeConverter.convert_type_params_t ~env tparams in
          type_t ~env r kind t ps
        | _ ->
          let%map ty = TypeConverter.convert_t ~env orig_t in
          Ty.Type ty
      in
      let singleton ~env ~orig_t t =
        match t with
        (* Polymorphic variants - see singleton_poly *)
        | DefT (_, PolyT { tparams; t_out; _ }) -> singleton_poly ~env ~orig_t tparams t_out
        (* Namespaces *)
        | NamespaceT { namespace_symbol; values_type = DefT (_, ObjT o); types_tmap } ->
          (match FlowSymbol.kind_of_symbol namespace_symbol with
          | FlowSymbol.SymbolModule when not env.Env.keep_only_namespace_name ->
            let%bind (exports, default) = namespace_t ~env o types_tmap in
            let name = ty_symbol_from_symbol env namespace_symbol in
            return (Ty.Decl (Ty.ModuleDecl { name = Some name; exports; default }))
          | FlowSymbol.SymbolNamespace when not env.Env.keep_only_namespace_name ->
            let%bind (exports, _) = namespace_t ~env o types_tmap in
            let name = ty_symbol_from_symbol env namespace_symbol in
            return (Ty.Decl (Ty.NamespaceDecl { name = Some name; exports }))
          | _ ->
            let%map t = TypeConverter.convert_t ~env orig_t in
            Ty.Type t)
        (* Monomorphic Classes/Interfaces *)
        | DefT (_, ClassT (ThisInstanceT (r, { super; inst; _ }, _, _)))
        | DefT (_, ClassT (DefT (r, InstanceT { super; inst; _ })))
        | DefT (_, TypeT (InstanceKind, DefT (r, InstanceT { super; inst; _ })))
        | DefT (_, TypeT (ImportClassKind, DefT (r, InstanceT { super; inst; _ }))) ->
          class_or_interface_decl ~env r None super inst
        (* Enums *)
        | DefT (reason, EnumObjectT _)
        | DefT (_, TypeT (ImportEnumKind, DefT (reason, EnumValueT _))) ->
          enum_decl ~env reason
        | DefT
            ( reason,
              ReactAbstractComponentT
                { component_kind = Nominal (_, name, targs); config; instance; renders }
            ) ->
          component_decl ~env ?targs None config instance renders name reason
        | DefT
            (_, ReactAbstractComponentT { component_kind = Structural; config; instance; renders })
          when Env.toplevel_is_type_identifier_reference env ->
          let orig_reason = TypeUtil.reason_of_t orig_t in
          (match desc_of_reason orig_reason with
          | RIdentifier (OrdinaryName name) ->
            component_decl ~env None config instance renders name orig_reason
          | _ ->
            let%map t = TypeConverter.convert_t ~env orig_t in
            Ty.Type t)
        (* Monomorphic Type Aliases *)
        | DefT (r, TypeT (kind, t)) ->
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
        | Some (_def_loc, exports) ->
          (match Lookahead.peek (Env.get_cx env) exports with
          | Lookahead.LowerBounds [DefT (_, ObjT o)] ->
            let%map (_, default) = module_of_object ~env o in
            default
          | Lookahead.Recursive
          | Lookahead.LowerBounds _ ->
            let%map t = TypeConverter.convert_t ~env exports in
            Some t)
      in
      let from_exports_tmap ~env exports_tmap =
        let step (x, { name_loc = _; preferred_def_locs = _; type_ = t }) =
          match%map toplevel ~env t with
          | Ty.Decl d -> d
          | Ty.Type t -> Ty.VariableDecl (x, t)
        in
        exports_tmap |> NameUtils.Map.bindings |> mapM step
      in
      fun ~env reason { value_exports_tmap; type_exports_tmap; cjs_export; _ } ->
        let%bind name = Reason_utils.module_symbol_opt env reason in
        let exports_tmap =
          let cx = Env.get_cx env in
          NameUtils.Map.union
            (Context.find_exports cx value_exports_tmap)
            (Context.find_exports cx type_exports_tmap)
        in
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
          | (x, T.Field { type_; polarity; _ }) :: tl ->
            let%bind acc' = step acc (x, type_, polarity) in
            loop acc' tl
          | _ -> terr ~kind:UnsupportedTypeCtor ~msg:"module-prop" None
        in
        let cx = Env.get_cx env in
        let props = NameUtils.Map.bindings (Context.find_props cx props_id) in
        loop ([], None) props
      in
      (fun ~env o -> obj_module_props ~env o.T.props_tmap)

    and namespace_t =
      let step ~env decls (x, t, _pol) =
        match%map toplevel ~env t with
        | Ty.Type t -> Ty.VariableDecl (x, t) :: decls
        | Ty.Decl d -> d :: decls
      in
      let rec loop ~env acc xs =
        match xs with
        | [] -> return acc
        | (x, T.Field { type_; polarity; _ }) :: tl ->
          let%bind acc' = step ~env acc (x, type_, polarity) in
          loop ~env acc' tl
        | _ -> terr ~kind:UnsupportedTypeCtor ~msg:"namespace-prop" None
      in
      fun ~env values_type types_tmap ->
        let env = { env with Env.keep_only_namespace_name = true } in
        let%bind (exports, default) = module_of_object ~env values_type in
        let%map exports =
          types_tmap
          |> Context.find_props (Env.get_cx env)
          |> NameUtils.Map.bindings
          |> loop ~env exports
        in
        (exports, default)

    let convert_toplevel = toplevel
  end

  let run_type_aux
      ~(f : env:Env.t -> T.t -> ('a, error) t)
      ~(simpl : merge_kinds:bool -> ?sort:bool -> 'a -> 'a)
      ~genv
      state
      t : ('a, error) result * State.t =
    let env = Env.init ~genv in
    let (result, state) = run state (f ~env t) in
    let result =
      match result with
      | Ok t when Env.optimize_types env ->
        Ok (simpl ~merge_kinds:(Env.merge_bot_and_any_kinds env) ~sort:false t)
      | _ -> result
    in
    (result, state)

  let run_type = run_type_aux ~f:ElementConverter.convert_toplevel ~simpl:Ty_utils.simplify_elt

  let run_module_type ~genv state { T.module_reason; module_export_types; _ } :
      (Ty.decl, error) result * State.t =
    let env = Env.init ~genv in
    let (result, state) =
      run state (ElementConverter.module_t ~env module_reason module_export_types)
    in
    let result =
      match result with
      | Ok decl when Env.optimize_types env ->
        Ok (Ty_utils.simplify_decl ~merge_kinds:(Env.merge_bot_and_any_kinds env) ~sort:false decl)
      | result -> result
    in
    (result, state)

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
      | TypeOf (TSymbol { sym_def_loc; _ }, None) ->
        Some sym_def_loc
      | _ -> None
    in
    let def_loc_of_decl = function
      | TypeAliasDecl { import = false; name = { sym_def_loc; _ }; _ }
      | ClassDecl ({ sym_def_loc; _ }, _)
      | InterfaceDecl ({ sym_def_loc; _ }, _)
      | EnumDecl { sym_def_loc; _ }
      | NominalComponentDecl { name = { sym_def_loc; _ }; _ } ->
        Some sym_def_loc
      | TypeAliasDecl { import = true; type_ = Some t; _ } -> def_loc_of_ty t
      | TypeAliasDecl _
      | VariableDecl _
      | ModuleDecl _
      | NamespaceDecl _ ->
        None
    in
    let def_loc_of_elt = function
      | Type t -> def_loc_of_ty t
      | Decl d -> def_loc_of_decl d
    in
    let convert genv t =
      let env = Env.init ~genv in
      let%map ty = ElementConverter.convert_toplevel ~env t in
      def_loc_of_elt ty
    in
    fun cx file_sig typed_ast_opt options imported_ts : Ty.imported_ident ALocMap.t ->
      (* We shouldn't need to evaluate any destructors for imports. *)
      let options = { options with Env.evaluate_type_destructors = Env.EvaluateNone } in
      let genv =
        {
          Env.options;
          cx;
          file_sig;
          typed_ast_opt;
          imported_names = lazy Loc_collections.ALocMap.empty;
        }
      in
      let state = State.empty in
      let (_, result) =
        List.fold_left
          (fun (st, acc) (name, loc, import_mode, t) ->
            match run st (convert genv t) with
            | (Ok (Some def_loc), st) -> (st, ALocMap.add def_loc (loc, name, import_mode) acc)
            | (Ok None, st) ->
              (* unrecognizable remote type *)
              (st, acc)
            | (Error _, st) ->
              (* normalization error *)
              (st, acc))
          (state, ALocMap.empty)
          imported_ts
      in
      result

  module type EXPAND_MEMBERS_CONVERTER = sig
    val force_instance : bool

    (* When non-null, only this list of names will be considered in the result *)
    val allowed_prop_names : Reason.name list option
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
      Ty.(Obj { obj_def_loc = None; obj_kind = ExactObj; obj_literal = None; obj_props = [] })

    let rec arr_t ~env ~inherited r a =
      let builtin =
        match a with
        | T.ArrayAT _ -> "Array"
        | T.ROArrayAT _
        | T.TupleAT _ ->
          "$ReadOnlyArray"
      in
      let cont =
        type__ ~env ~inherited ~source:(Ty.PrimitiveProto builtin) ~imode:IMInstance ?id:None
      in
      let t =
        match a with
        | T.ArrayAT _ -> Flow_js_utils.lookup_builtin_value (Env.get_cx env) "Array" r
        | T.ROArrayAT _
        | T.TupleAT _ ->
          Flow_js_utils.lookup_builtin_type (Env.get_cx env) "$ReadOnlyArray" r
      in
      cont t

    and member_expand_object ~env ~inherited ~source super implements inst =
      let { T.own_props; proto_props; _ } = inst in
      let%bind own_ty_props =
        TypeConverter.convert_obj_props_t
          ~env
          ~inherited
          ~source
          ?allowed_prop_names:Conf.allowed_prop_names
          own_props
          None
      in
      let%bind proto_ty_props =
        TypeConverter.convert_obj_props_t
          ~env
          ~inherited:true
          ~source
          ?allowed_prop_names:Conf.allowed_prop_names
          proto_props
          None
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
      Ty.Obj { Ty.obj_def_loc = None; obj_kind = Ty.InexactObj; obj_literal = None; obj_props }

    and enum_t ~env ~inherited reason enum_info =
      let (members, representation_t) =
        match enum_info with
        | T.ConcreteEnum { T.members; representation_t; _ } -> (members, representation_t)
        | T.AbstractEnum { representation_t } -> (SMap.empty, representation_t)
      in
      let enum_value_t = T.mk_enum_type reason enum_info in
      let enum_object_t = T.DefT (reason, T.EnumObjectT { enum_value_t; enum_info }) in
      let%bind proto_ty =
        I.builtin_typeapp
          (Env.get_cx env)
          ~cont:(type__ ~env ~inherited:true ~source:(Ty.PrimitiveProto "$EnumProto") ~imode:IMUnset)
          ~type_:(convert_t ~env)
          ~app:app_on_generic
          reason
          "$EnumProto"
          [enum_object_t; enum_value_t; representation_t]
      in
      let%map enum_value_ty = TypeConverter.convert_t ~env enum_value_t in
      let members_ty =
        List.map
          (fun (name, loc) ->
            let prop = Ty.Field { t = enum_value_ty; polarity = Ty.Positive; optional = false } in
            Ty.NamedProp
              { name = OrdinaryName name; prop; inherited; source = Ty.Other; def_locs = [loc] })
          (SMap.bindings members)
      in
      Ty.Obj
        {
          Ty.obj_def_loc = Some (Reason.def_loc_of_reason reason);
          obj_kind = Ty.InexactObj;
          obj_literal = None;
          obj_props = Ty.SpreadProp proto_ty :: members_ty;
        }

    and obj_t ~env ~inherited ~source ~imode reason o =
      let%bind obj =
        TypeConverter.convert_obj_t
          ~env
          ~inherited
          ~source
          ?allowed_prop_names:Conf.allowed_prop_names
          reason
          o
      in
      let%map extra_props =
        let%map proto = type__ ~env ~inherited:true ~source ~imode o.T.proto_t in
        [Ty.SpreadProp proto]
      in
      { obj with Ty.obj_props = obj.Ty.obj_props @ extra_props }

    and primitive ~env reason name =
      let cont = type__ ~env ~inherited:true ~source:(Ty.PrimitiveProto name) ~imode:IMUnset in
      I.builtin_type (Env.get_cx env) ~cont reason name

    and instance_t ~env ~inherited ~source ~imode r static super implements inst =
      let { T.inst_kind; _ } = inst in
      let desc = desc_of_reason ~unwrap:false r in
      match (inst_kind, desc, imode) with
      | (T.ClassKind, _, IMStatic) -> type__ ~env ~inherited ~source ~imode static
      | (T.ClassKind, _, (IMUnset | IMInstance))
      | (T.InterfaceKind _, _, _) ->
        member_expand_object ~env ~inherited ~source super implements inst

    and opaque_t ~env ~inherited ~source ~imode r opaquetype =
      let current_source = Context.file (Env.get_cx env) in
      let opaque_source = ALoc.source (def_loc_of_reason r) in
      (* Compare the current file (of the query) and the file that the opaque
         type is defined. If they differ, then hide the underlying type. *)
      let same_file = Some current_source = opaque_source in
      match opaquetype with
      | { Type.underlying_t = Some t; _ } when same_file -> type__ ~env ~inherited ~source ~imode t
      | { Type.upper_t = Some t; _ } -> type__ ~env ~inherited ~source ~imode t
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
      | ThisTypeAppT (_, c, _, _) -> type__ ~env ~inherited ~source ~imode c
      | DefT (r, (NumGeneralT _ | SingletonNumT _)) -> primitive ~env r "Number"
      | DefT (r, (StrGeneralT _ | SingletonStrT _)) -> primitive ~env r "String"
      | DefT (r, (BoolGeneralT | SingletonBoolT _)) -> primitive ~env r "Boolean"
      | DefT (r, SymbolT) -> primitive ~env r "Symbol"
      | DefT (_, EnumValueT _) -> return no_members
      | ObjProtoT r -> primitive ~env r "Object"
      | FunProtoT r -> primitive ~env r "Function"
      | DefT (r, ObjT o) ->
        let%map o = obj_t ~env ~inherited ~source ~imode r o in
        Ty.Obj o
      | DefT (_, ClassT (ThisInstanceT (r, t, _, _))) ->
        this_class_t ~env ~inherited ~source ~imode (DefT (r, InstanceT t))
      | DefT (_, ClassT t) -> type__ ~env ~inherited ~source ~imode t
      | DefT (r, ArrT a) -> arr_t ~env ~inherited r a
      | DefT (r, EnumObjectT { enum_info; _ }) -> enum_t ~env ~inherited r enum_info
      | ThisInstanceT (r, { static; super; implements; inst }, _, _)
      | DefT (r, InstanceT { static; super; implements; inst }) ->
        instance_t ~env ~inherited ~source ~imode r static super implements inst
      | DefT (_, PolyT { t_out; _ }) -> type__ ~env ~inherited ~source ~imode t_out
      | MaybeT (_, t) -> maybe_t ~env ?id ~cont:(type__ ~inherited ~source ~imode) t
      | IntersectionT (_, rep) ->
        app_intersection ~f:(type__ ~env ?id ~inherited ~source ~imode) rep
      | UnionT (_, rep) ->
        app_union ~from_bounds:false ~f:(type__ ~env ?id ~inherited ~source ~imode) rep
      | DefT (_, FunT (static, _)) -> type__ ~env ~inherited ~source ~imode static
      | TypeAppT { reason; use_op = _; type_ = c; targs; from_value; use_desc = _ } ->
        let cont = type__ ~env ~inherited ~source ~imode in
        let type_ = convert_t ~env in
        I.typeapp (Env.get_cx env) ~cont ~type_ ~app:app_on_generic ~from_value reason c targs
      | DefT (_, TypeT (_, t)) -> type__ ~env ~inherited ~source ~imode t
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
      | GenericT { bound; _ } -> type__ ~env ~inherited ~source ~imode bound
      | OpaqueT (r, o) -> opaque_t ~env ~inherited ~source ~imode r o
      | DefT (reason, ReactAbstractComponentT _) ->
        I.builtin_type
          (Env.get_cx env)
          ~cont:(type__ ~env ~inherited ~source ~imode)
          reason
          "React$AbstractComponentStatics"
      | t -> TypeConverter.convert_t ~env t

    and convert_t ~env t = type__ ~env ~inherited:false ~source:Ty.Other ~imode:IMUnset t
  end

  let run_expand_members ~force_instance ?allowed_prop_names =
    let module Converter = ExpandMembersConverter (struct
      let force_instance = force_instance

      let allowed_prop_names = allowed_prop_names
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
      | TypeAppT { reason; use_op = _; type_ = t; targs; from_value; use_desc = _ } ->
        I.typeapp
          (Env.get_cx env)
          ~cont:(type__ ~env)
          ~type_:(type__ ~env)
          ~app:app_on_generic
          ~from_value
          reason
          t
          targs
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
      | KeysT (r, t) -> keys_t ~env ~cont:type__ r ~force_eval:true t
      | DefT (_, SingletonNumT { value = (_, lit); _ }) -> return (Ty.NumLit lit)
      | DefT (_, SingletonStrT { value = lit; _ }) -> return (Ty.StrLit lit)
      | DefT (_, SingletonBoolT { value = lit; _ }) -> return (Ty.BoolLit lit)
      | DefT (_, BoolGeneralT) -> return Ty.Bool
      | DefT (_, NullT) -> return Ty.Null
      | _ -> return empty_type

    let convert_t ~env t = type__ ~env t
  end

  let run_expand_literal_union =
    run_type_aux ~f:ExpandLiteralUnionConverter.convert_t ~simpl:Ty_utils.simplify_type
end
