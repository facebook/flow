(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Subst
open Type
open Constraint
open TypeUtil
open Utils_js
module FlowError = Flow_error

type cx = Context.t

type loc = ALoc.t

(* These possible_* functions would ideally be in constraint.ml, but since they use
 * Context and Context depends on Constraint we need to extract these functions
 * to a separate module in order to avoid a circular dependency *)

(* Def types that describe the solution of a type variable. *)
let possible_types cx id =
  types_of (Lazy.force (Context.find_graph cx id)) |> List.filter is_proper_def

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let possible_uses cx id =
  uses_of (Lazy.force (Context.find_graph cx id)) |> List.filter is_proper_use

let merge_tvar =
  let rec collect_lowers ~filter_empty cx seen acc = function
    | [] -> Base.List.rev acc
    | t :: ts ->
      (match t with
      (* Recursively unwrap unseen tvars *)
      | OpenT (_, id) ->
        if ISet.mem id seen then
          collect_lowers ~filter_empty cx seen acc ts
        (* already unwrapped *)
        else
          let seen = ISet.add id seen in
          collect_lowers ~filter_empty cx seen acc (possible_types cx id @ ts)
      | DefT (_, _, EmptyT) when filter_empty -> collect_lowers ~filter_empty cx seen acc ts
      (* Everything else becomes part of the merge typed *)
      | _ -> collect_lowers ~filter_empty cx seen (t :: acc) ts)
  in
  fun ?(filter_empty = false) ~no_lowers cx r id ->
    let lowers =
      let seen = ISet.singleton id in
      collect_lowers cx seen [] (possible_types cx id) ~filter_empty
    in
    match lowers with
    | [t] -> t
    | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)
    | [] -> no_lowers cx r

(** Type predicates *)

(* some types need to be resolved before proceeding further *)
let needs_resolution = function
  | OpenT _
  | UnionT _
  | OptionalT _
  | MaybeT _
  | AnnotT _ ->
    true
  | _ -> false

let is_generic = function
  | GenericT _ -> true
  | _ -> false

let is_object_prototype_method = function
  | OrdinaryName
      ( "isPrototypeOf" | "hasOwnProperty" | "propertyIsEnumerable" | "toLocaleString" | "toString"
      | "valueOf" ) ->
    true
  | _ -> false

(* This must list all of the properties on Function.prototype. *)
let is_function_prototype = function
  | OrdinaryName ("apply" | "bind" | "call" | "arguments" | "caller" | "length" | "name") -> true
  | x -> is_object_prototype_method x

(* neither object prototype methods nor callable signatures should be
 * implied by an object indexer type *)
let is_dictionary_exempt = function
  | x when is_object_prototype_method x -> true
  | _ -> false

(* NOTE: The following function looks similar to TypeUtil.quick_subtype, but is in fact more
   complicated: it avoids deep structural checks, admits `any`, etc. It might be worth it to
   simplify this function later. *)
let ground_subtype = function
  (* tvars are not considered ground, so they're not part of this relation *)
  | (OpenT _, _)
  | (_, UseT (_, OpenT _)) ->
    false
  | (UnionT _, _) -> false
  | (DefT (_, _, NumT _), UseT (_, DefT (_, _, NumT _)))
  | (DefT (_, _, StrT _), UseT (_, DefT (_, _, StrT _)))
  | (DefT (_, _, BoolT _), UseT (_, DefT (_, _, BoolT _)))
  | (DefT (_, _, SymbolT), UseT (_, DefT (_, _, SymbolT)))
  | (DefT (_, _, NullT), UseT (_, DefT (_, _, NullT)))
  | (DefT (_, _, VoidT), UseT (_, DefT (_, _, VoidT))) ->
    true
  | (DefT (_, _, NullT), UseT (_, DefT (_, _, MixedT (Mixed_non_maybe | Mixed_non_null))))
  | (DefT (_, _, VoidT), UseT (_, DefT (_, _, MixedT (Mixed_non_maybe | Mixed_non_void)))) ->
    false
  | (_, UseT (_, DefT (_, _, MixedT _))) -> true
  (* we handle the any propagation check later *)
  | (AnyT _, _) -> false
  | (_, UseT (_, AnyT _)) -> false
  (* opt: avoid builtin lookups *)
  | (ObjProtoT _, UseT (_, ObjProtoT _))
  | (FunProtoT _, UseT (_, FunProtoT _))
  | (FunProtoT _, UseT (_, ObjProtoT _))
  | (DefT (_, _, ObjT { proto_t = ObjProtoT _; _ }), UseT (_, ObjProtoT _))
  | (DefT (_, _, ObjT { proto_t = FunProtoT _; _ }), UseT (_, FunProtoT _))
  | (DefT (_, _, ObjT { proto_t = FunProtoT _; _ }), UseT (_, ObjProtoT _)) ->
    true
  | _ -> false

let numeric = function
  | DefT (_, _, NumT _) -> true
  | DefT (_, _, SingletonNumT _) -> true
  | _ -> false

let dateiform = function
  | DefT (reason, _, InstanceT _) -> DescFormat.name_of_instance_reason reason = "Date"
  | _ -> false

let numberesque = function
  | x -> numeric x || dateiform x

let function_like = function
  | DefT (_, _, ClassT _)
  | DefT (_, _, FunT _)
  | CustomFunT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _ ->
    true
  | _ -> false

let object_like = function
  | DefT (_, _, (ObjT _ | InstanceT _))
  | ObjProtoT _
  | FunProtoT _
  | AnyT _ ->
    true
  | t -> function_like t

let object_like_op = function
  | SetPropT _
  | GetPropT _
  | TestPropT _
  | MethodT _
  | LookupT _
  | MatchPropT _
  | GetProtoT _
  | SetProtoT _
  | SuperT _
  | GetKeysT _
  | HasOwnPropT _
  | GetValuesT _
  | ObjAssignToT _
  | ObjAssignFromT _
  | ObjRestT _
  | SetElemT _
  | GetElemT _
  | UseT (_, AnyT _) ->
    true
  | _ -> false

let function_like_op = function
  | CallT _
  | ConstructorT _
  | UseT (_, AnyT _) ->
    true
  | t -> object_like_op t

let equatable = function
  | (DefT (_, _, NumT _), DefT (_, _, NumT _))
  | (DefT (_, _, SingletonNumT _), DefT (_, _, SingletonNumT _))
  | (DefT (_, _, SingletonNumT _), DefT (_, _, NumT _))
  | (DefT (_, _, NumT _), DefT (_, _, SingletonNumT _))
  | (DefT (_, _, StrT _), DefT (_, _, StrT _))
  | (DefT (_, _, StrT _), DefT (_, _, SingletonStrT _))
  | (DefT (_, _, SingletonStrT _), DefT (_, _, StrT _))
  | (DefT (_, _, SingletonStrT _), DefT (_, _, SingletonStrT _))
  | (DefT (_, _, BoolT _), DefT (_, _, BoolT _))
  | (DefT (_, _, BoolT _), DefT (_, _, SingletonBoolT _))
  | (DefT (_, _, SingletonBoolT _), DefT (_, _, BoolT _))
  | (DefT (_, _, SingletonBoolT _), DefT (_, _, SingletonBoolT _))
  | (DefT (_, _, SymbolT), DefT (_, _, SymbolT))
  | (DefT (_, _, EmptyT), _)
  | (_, DefT (_, _, EmptyT))
  | (_, DefT (_, _, MixedT _))
  | (DefT (_, _, MixedT _), _)
  | (AnyT _, _)
  | (_, AnyT _)
  | (DefT (_, _, VoidT), _)
  | (_, DefT (_, _, VoidT))
  | (DefT (_, _, NullT), _)
  | (_, DefT (_, _, NullT)) ->
    true
  | ( DefT
        ( _,
          _,
          ( NumT _ | StrT _ | BoolT _ | SingletonNumT _ | SingletonStrT _ | SingletonBoolT _
          | SymbolT | EnumObjectT _ | EnumT _ ) ),
      _ )
  | ( _,
      DefT
        ( _,
          _,
          ( NumT _ | StrT _ | BoolT _ | SingletonNumT _ | SingletonStrT _ | SingletonBoolT _
          | SymbolT | EnumObjectT _ | EnumT _ ) ) ) ->
    false
  | _ -> true

let is_union_resolvable = function
  | EvalT _
  | KeysT _ ->
    true
  | _ -> false

(* some patterns need to be concretized before proceeding further *)
let patt_that_needs_concretization = function
  | OpenT _
  | UnionT _
  | MaybeT _
  | OptionalT _
  | AnnotT _ ->
    true
  | _ -> false

let parts_to_replace_t cx = function
  | DefT (_, _, ObjT { call_t = Some id; _ }) ->
    begin
      match Context.find_call cx id with
      | DefT (_, _, FunT (_, _, ft)) ->
        let ts =
          List.fold_left
            (fun acc (_, t) ->
              if patt_that_needs_concretization t then
                t :: acc
              else
                acc)
            []
            ft.params
        in
        (match ft.rest_param with
        | Some (_, _, t) when patt_that_needs_concretization t -> t :: ts
        | _ -> ts)
      | _ -> []
    end
  | DefT (_, _, FunT (_, _, ft)) ->
    let ts =
      List.fold_left
        (fun acc (_, t) ->
          if patt_that_needs_concretization t then
            t :: acc
          else
            acc)
        []
        ft.params
    in
    (match ft.rest_param with
    | Some (_, _, t) when patt_that_needs_concretization t -> t :: ts
    | _ -> ts)
  | _ -> []

(* for now, we only care about concretizating parts of functions and calls *)
let parts_to_replace cx = function
  | UseT (_, t) -> parts_to_replace_t cx t
  | CallT (_, _, callt) ->
    List.fold_left
      (fun acc -> function
        | Arg t
        | SpreadArg t
          when patt_that_needs_concretization t ->
          t :: acc
        | _ -> acc)
      []
      callt.call_args_tlist
  | _ -> []

(* replace unresolved types (xs) with resolved (ys) *)
let replace_parts =
  let rec replace_params acc = function
    | (ys, []) -> (ys, List.rev acc)
    | (ys, ((name, x) as param) :: params) ->
      if patt_that_needs_concretization x then
        replace_params ((name, List.hd ys) :: acc) (List.tl ys, params)
      else
        replace_params (param :: acc) (ys, params)
  in
  let replace_rest_param = function
    | (ys, None) -> (ys, None)
    | (ys, (Some (name, loc, x) as param)) ->
      if patt_that_needs_concretization x then
        (List.tl ys, Some (name, loc, List.hd ys))
      else
        (ys, param)
  in
  let replace_arg ys = function
    | Arg x when patt_that_needs_concretization x -> (Arg (List.hd ys), List.tl ys)
    | SpreadArg x when patt_that_needs_concretization x -> (SpreadArg (List.hd ys), List.tl ys)
    | arg -> (arg, ys)
  in
  let rec replace_args acc = function
    | (ys, []) -> (ys, List.rev acc)
    | (ys, arg :: args) ->
      let (arg, ys) = replace_arg ys arg in
      replace_args (arg :: acc) (ys, args)
  in
  fun cx resolved -> function
    | UseT (op, DefT (r1, t1, ObjT ({ call_t = Some id; _ } as o))) as u ->
      begin
        match Context.find_call cx id with
        | DefT (r2, t2, FunT (static, proto, ft)) ->
          let (resolved, params) = replace_params [] (resolved, ft.params) in
          let (resolved, rest_param) = replace_rest_param (resolved, ft.rest_param) in
          assert (resolved = []);
          let id' =
            Context.make_call_prop
              cx
              (DefT (r2, t2, FunT (static, proto, { ft with params; rest_param })))
          in
          UseT (op, DefT (r1, t1, ObjT { o with call_t = Some id' }))
        | _ -> u
      end
    | UseT (op, DefT (r, trust, FunT (t1, t2, ft))) ->
      let (resolved, params) = replace_params [] (resolved, ft.params) in
      let (resolved, rest_param) = replace_rest_param (resolved, ft.rest_param) in
      assert (resolved = []);
      UseT (op, DefT (r, trust, FunT (t1, t2, { ft with params; rest_param })))
    | CallT (op, r, callt) ->
      let (resolved, call_args_tlist) = replace_args [] (resolved, callt.call_args_tlist) in
      assert (resolved = []);
      CallT (op, r, { callt with call_args_tlist })
    | u -> u

(** Errors *)

let error_message_kind_of_lower = function
  | DefT (_, _, NullT) -> Some Error_message.Possibly_null
  | DefT (_, _, VoidT) -> Some Error_message.Possibly_void
  | MaybeT _ -> Some Error_message.Possibly_null_or_void
  | IntersectionT _
  | _ ->
    None

let error_message_kind_of_upper = function
  | GetPropT (_, _, Named (r, name), _) ->
    Error_message.IncompatibleGetPropT (aloc_of_reason r, Some name)
  | GetPropT (_, _, Computed t, _) -> Error_message.IncompatibleGetPropT (loc_of_t t, None)
  | GetPrivatePropT (_, _, _, _, _, _) -> Error_message.IncompatibleGetPrivatePropT
  | SetPropT (_, _, Named (r, name), _, _, _, _) ->
    Error_message.IncompatibleSetPropT (aloc_of_reason r, Some name)
  | SetPropT (_, _, Computed t, _, _, _, _) -> Error_message.IncompatibleSetPropT (loc_of_t t, None)
  | MatchPropT (_, _, Named (r, name), _) ->
    Error_message.IncompatibleMatchPropT (aloc_of_reason r, Some name)
  | MatchPropT (_, _, Computed t, _) -> Error_message.IncompatibleMatchPropT (loc_of_t t, None)
  | SetPrivatePropT (_, _, _, _, _, _, _, _) -> Error_message.IncompatibleSetPrivatePropT
  | MethodT (_, _, _, Named (r, name), _, _) ->
    Error_message.IncompatibleMethodT (aloc_of_reason r, Some name)
  | MethodT (_, _, _, Computed t, _, _) -> Error_message.IncompatibleMethodT (loc_of_t t, None)
  | CallT _ -> Error_message.IncompatibleCallT
  | ConstructorT _ -> Error_message.IncompatibleConstructorT
  | GetElemT (_, _, t, _) -> Error_message.IncompatibleGetElemT (loc_of_t t)
  | SetElemT (_, _, t, _, _, _) -> Error_message.IncompatibleSetElemT (loc_of_t t)
  | CallElemT (_, _, t, _) -> Error_message.IncompatibleCallElemT (loc_of_t t)
  | ElemT (_, _, DefT (_, _, ArrT _), _) -> Error_message.IncompatibleElemTOfArrT
  | ObjAssignFromT (_, _, _, _, ObjSpreadAssign) -> Error_message.IncompatibleObjAssignFromTSpread
  | ObjAssignFromT _ -> Error_message.IncompatibleObjAssignFromT
  | ObjRestT _ -> Error_message.IncompatibleObjRestT
  | ObjSealT _ -> Error_message.IncompatibleObjSealT
  | ArrRestT _ -> Error_message.IncompatibleArrRestT
  | SuperT _ -> Error_message.IncompatibleSuperT
  | MixinT _ -> Error_message.IncompatibleMixinT
  | SpecializeT _ -> Error_message.IncompatibleSpecializeT
  | ConcretizeTypeAppsT _ -> Error_message.IncompatibleSpecializeT
  | ThisSpecializeT _ -> Error_message.IncompatibleThisSpecializeT
  | VarianceCheckT _ -> Error_message.IncompatibleVarianceCheckT
  | GetKeysT _ -> Error_message.IncompatibleGetKeysT
  | HasOwnPropT
      ( _,
        r,
        ( DefT (_, _, StrT (Literal (_, name)))
        | GenericT { bound = DefT (_, _, StrT (Literal (_, name))); _ } ) ) ->
    Error_message.IncompatibleHasOwnPropT (aloc_of_reason r, Some name)
  | HasOwnPropT (_, r, _) -> Error_message.IncompatibleHasOwnPropT (aloc_of_reason r, None)
  | GetValuesT _ -> Error_message.IncompatibleGetValuesT
  | UnaryMinusT _ -> Error_message.IncompatibleUnaryMinusT
  | MapTypeT (_, _, (ObjectMap _ | ObjectMapi _), _) -> Error_message.IncompatibleMapTypeTObject
  | TypeAppVarianceCheckT _ -> Error_message.IncompatibleTypeAppVarianceCheckT
  | GetStaticsT _ -> Error_message.IncompatibleGetStaticsT
  | BindT _ -> Error_message.IncompatibleBindT
  | use_t -> Error_message.IncompatibleUnclassified (string_of_use_ctor use_t)

let use_op_of_lookup_action = function
  | ReadProp { use_op; _ }
  | WriteProp { use_op; _ }
  | LookupProp (use_op, _)
  | SuperProp (use_op, _)
  | MatchProp (use_op, _) ->
    use_op

exception SpeculativeError of Error_message.t

let add_output cx ?trace msg =
  let trace_reasons =
    match trace with
    | None -> []
    | Some trace ->
      (* format a trace into list of (reason, desc) pairs used
         downstream for obscure reasons, and then to messages *)
      let max_trace_depth = Context.max_trace_depth cx in
      if max_trace_depth = 0 then
        []
      else
        Trace.reasons_of_trace ~level:max_trace_depth trace
  in
  let is_enabled =
    match Error_message.kind_of_msg msg with
    | Errors.LintError lint_kind ->
      begin
        match Error_message.loc_of_msg msg with
        | Some loc ->
          ALoc.to_loc_with_tables (Context.aloc_tables cx) loc
          |> Error_suppressions.get_lint_settings (Context.severity_cover cx)
          |> Base.Option.value_map ~default:true ~f:(fun lint_settings ->
                 LintSettings.is_explicit lint_kind lint_settings
                 || LintSettings.get_value lint_kind lint_settings <> Severity.Off)
        | _ -> true
      end
    | _ -> true
  in
  (* If the lint error isn't enabled at this location and isn't explicitly suppressed, just don't
     even add it *)
  if not is_enabled then
    ()
  else if Speculation.speculating cx then
    if Error_message.is_lint_error msg then
      ignore @@ Speculation.defer_action cx (Speculation_state.ErrorAction msg)
    else (
      if Context.is_verbose cx then
        prerr_endlinef "\nspeculative_error: %s" (Debug_js.dump_error_message cx msg);
      raise (SpeculativeError msg)
    )
  else (
    if Context.is_verbose cx then
      prerr_endlinef "\nadd_output: %s" (Debug_js.dump_error_message cx msg);

    let error = FlowError.error_of_msg ~trace_reasons ~source_file:(Context.file cx) msg in
    (* catch no-loc errors early, before they get into error map *)
    if
      Flow_error.loc_of_error error
      |> Base.Option.value_map ~default:false ~f:(fun loc -> ALoc.source loc = None)
    then
      assert_false (spf "add_output: no source for error: %s" (Debug_js.dump_error_message cx msg));

    Context.add_error cx error
  )

let exact_obj_error cx trace obj_kind ~use_op ~exact_reason l =
  let error_kind =
    match obj_kind with
    | Indexed _ -> Error_message.Indexer
    | _ -> Error_message.Inexact
  in
  let reasons = FlowError.ordered_reasons (reason_of_t l, exact_reason) in
  add_output cx ~trace (Error_message.EIncompatibleWithExact (reasons, use_op, error_kind))

(** Unions *)

(** TODO: (1) Define a more general partial equality, that takes into
    account unified type variables. (2) Get rid of UnionRep.quick_mem. **)
let union_optimization_guard =
  (* Compare l to u. Flatten both unions and then check that each element
     of l is comparable to an element of u. Note that the comparator need not
     be symmetric. *)
  let union_compare cx comparator lts uts =
    let ts2 = Type_mapper.union_flatten cx uts in
    Type_mapper.union_flatten cx lts
    |> Base.List.for_all ~f:(fun t1 -> Base.List.exists ~f:(comparator t1) ts2)
  in
  let rec union_optimization_guard_impl seen cx comparator l u =
    match (l, u) with
    | (UnionT (_, rep1), UnionT (_, rep2)) ->
      rep1 = rep2
      ||
      (* Try O(n) check, then O(n log n) check, then O(n^2) check *)
      begin
        match (UnionRep.check_enum rep1, UnionRep.check_enum rep2) with
        | (Some enums1, Some enums2) -> UnionEnumSet.subset enums1 enums2
        | (_, _) ->
          let unwrap rep = UnionRep.members rep |> Base.List.map ~f:(Type_mapper.unwrap_type cx) in
          let lts = unwrap rep1 in
          let uts = unwrap rep2 in
          (* Pointwise subtyping check: O(N) *)
          if List.length lts = List.length uts && Base.List.for_all2_exn ~f:( = ) lts uts then
            true
          else if
            (* Check if u contains l after unwrapping annots, tvars and repos types.
               This is faster than the n^2 case below because it avoids flattening both
               unions *)
            Base.List.exists
              ~f:(fun u ->
                (not (TypeSet.mem u seen))
                && union_optimization_guard_impl (TypeSet.add u seen) cx comparator l u)
              uts
          then
            true
          else
            union_compare cx comparator lts uts
      end
    | _ -> false
  in
  union_optimization_guard_impl TypeSet.empty

(* Optimization where an union is a subset of another. Equality modulo
 * reasons is important for this optimization to be effective, since types
 * are repositioned everywhere. *)
let remove_predicate_from_union reason cx predicate =
  UnionRep.members
  %> Type_mapper.union_flatten cx
  %> Base.List.rev_filter ~f:(predicate %> not)
  %> union_of_ts reason

let iter_union ~f cx trace rep u =
  (* This is required so that our caches don't treat different branches of unions as the same type *)
  let union_reason i r = replace_desc_reason (RUnionBranching (desc_of_reason r, i)) r in
  UnionRep.members rep
  |> Base.List.iteri ~f:(fun i ->
         mod_reason_of_t (union_reason i) %> mk_tuple_swapped u %> f cx trace)

let map_union ~f cx trace rep reason =
  UnionRep.members rep
  |> Base.List.map ~f:(fun t -> Tvar.mk_where cx (reason_of_t t) (fun tout -> f cx trace t tout))
  |> union_of_ts reason

let iter_resolve_union ~f cx trace reason rep upper =
  (* We can't guarantee that tvars or typeapps get resolved, even though we'd like
   * to believe they will. Instead, we separate out all the resolvable types from
   * the union, resolve them (f), and then rejoin them with the other types once
   * they have been resolved. *)
  let (evals, resolved) = UnionRep.members rep |> List.partition is_union_resolvable in
  match evals with
  | first :: unresolved ->
    f cx trace (first, ResolveUnionT { reason; resolved; unresolved; upper; id = Reason.mk_id () })
  (* No evals, but we can't get here *)
  | [] -> ()

(** Generics *)

(** Harness for testing parameterized types. Given a test function and a list
    of type params, replace the type params with GenericTs and run the test function.
  *)
let check_with_generics : 'a. Context.t -> Type.typeparam list -> (Type.t SMap.t -> 'a) -> 'a =
  (* New generics mode: generate a GenericT from a generic *)
  let generic_bound cx prev_map { bound; name; reason = param_reason; is_this = _; _ } =
    let param_loc = aloc_of_reason param_reason in
    let bound = subst cx prev_map bound in
    let id = Context.make_generic_id cx name param_loc in
    let bound =
      mod_reason_of_t
        (fun bound_reason ->
          let annot_loc = annot_aloc_of_reason bound_reason in
          let desc = desc_of_reason ~unwrap:false bound_reason in
          opt_annot_reason ?annot_loc @@ mk_reason desc param_loc)
        bound
    in
    let generic = GenericT { reason = reason_of_t bound; name; id; bound } in
    SMap.add name generic prev_map
  in
  (* main - run f over a collection of arg maps generated for params *)
  fun cx params f ->
    if params = [] then
      f SMap.empty
    else
      let map = Base.List.fold_left ~f:(generic_bound cx) ~init:SMap.empty params in
      f map

let mk_poly_arity_reason tparams_loc =
  mk_reason (RCustom "See type parameters of definition here") tparams_loc

let poly_minimum_arity =
  let f n typeparam =
    if typeparam.default = None then
      n + 1
    else
      n
  in
  Nel.fold_left f 0

let position_generic_bound reason = mod_reason_of_t (Fn.const reason)

(** Object Subtyping *)

let speculative_object_write cx flds s up =
  let action = Speculation_state.UnsealedObjectProperty (flds, s, up) in
  if not (Speculation.defer_action cx action) then Context.set_prop cx flds s up

let string_key s reason =
  let key_reason = replace_desc_reason (RPropertyIsAString s) reason in
  DefT (key_reason, bogus_trust (), StrT (Literal (None, s)))

(* common case checking a function as an object *)
let quick_error_fun_as_obj cx trace ~use_op reason statics reason_o props =
  let statics_own_props =
    match statics with
    | DefT (_, _, ObjT { props_tmap; _ }) -> Some (Context.find_props cx props_tmap)
    | AnyT _
    | DefT (_, _, MixedT _) ->
      Some NameUtils.Map.empty
    | _ -> None
  in
  match statics_own_props with
  | Some statics_own_props ->
    let props_not_found =
      NameUtils.Map.filter
        (fun x p ->
          let optional =
            match p with
            | Field (_, OptionalT _, _) -> true
            | _ -> false
          in
          not (optional || is_function_prototype x || NameUtils.Map.mem x statics_own_props))
        props
    in
    NameUtils.Map.iter
      (fun x _ ->
        let use_op =
          Frame (PropertyCompatibility { prop = Some x; lower = reason; upper = reason_o }, use_op)
        in
        let reason_prop = update_desc_reason (fun desc -> RPropertyOf (x, desc)) reason_o in
        let err =
          Error_message.EPropNotFound
            { prop_name = Some x; reason_prop; reason_obj = reason; use_op; suggestion = None }
        in
        add_output cx ~trace err)
      props_not_found;
    not (NameUtils.Map.is_empty props_not_found)
  | None -> false

(** Instantiation *)

(* instantiate each param of a polymorphic type with its upper bound *)
let instantiate_poly_param_upper_bounds cx typeparams =
  let (_, revlist) =
    Nel.fold_left
      (fun (map, list) { name; bound; _ } ->
        let t = subst cx map bound in
        (SMap.add name t map, t :: list))
      (SMap.empty, [])
      typeparams
  in
  List.rev revlist

(** Builtins *)

let lookup_builtin_strict cx x reason =
  let builtins = Context.builtins cx in
  Builtins.get_builtin builtins x ~on_missing:(fun () ->
      add_output cx (Error_message.EBuiltinLookupFailed { reason; name = Some x });
      AnyT.error_of_kind UnresolvedName reason)

let lookup_builtin_with_default cx x default =
  let builtins = Context.builtins cx in
  Builtins.get_builtin builtins x ~on_missing:(fun () -> default)

let lookup_builtin_typeapp cx reason x targs =
  let t = lookup_builtin_strict cx x reason in
  typeapp reason t targs

let match_this_binding map f =
  match SMap.find "this" map with
  | ReposT (_, t) -> f t
  | _ -> failwith "not a this binding"

(**
 * Determines whether a property name should be considered "munged"/private when
 * the `munge_underscores` config option is set.
 *)
let is_munged_prop_name_with_munge name ~should_munge_underscores =
  Signature_utils.is_munged_property_name name && should_munge_underscores

let is_munged_prop_name cx name =
  is_munged_prop_name_with_munge
    name
    ~should_munge_underscores:(Context.should_munge_underscores cx)

let map_obj cx trust o reason_op ~map_t ~map_field =
  let props_tmap =
    Context.find_props cx o.props_tmap
    |> Properties.mapi_fields map_field
    |> Context.generate_property_map cx
  in
  let flags =
    {
      o.flags with
      obj_kind =
        Obj_type.map_dict
          (fun dict ->
            let value = map_t dict.key dict.value in
            { dict with value })
          o.flags.obj_kind;
    }
  in
  let reason = replace_desc_reason RObjectType reason_op in
  let t = DefT (reason, trust, ObjT { o with props_tmap; flags }) in
  if Obj_type.is_legacy_exact_DO_NOT_USE o.flags.obj_kind then
    ExactT (reason, t)
  else
    t

let check_untyped_import cx import_kind lreason ureason =
  match (import_kind, desc_of_reason lreason) with
  (* Use a special reason so we can tell the difference between an any-typed type import
   * from an untyped module and an any-typed type import from a nonexistent module. *)
  | ((ImportType | ImportTypeof), RUntypedModule module_name) ->
    let loc = Reason.aloc_of_reason ureason in
    let message = Error_message.EUntypedTypeImport (loc, module_name) in
    add_output cx message
  | (ImportValue, RUntypedModule module_name) ->
    let loc = Reason.aloc_of_reason ureason in
    let message = Error_message.EUntypedImport (loc, module_name) in
    add_output cx message
  | _ -> ()

module type Instantiation_helper_sig = sig
  val cache_instantiate :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    ?cache:Reason.t list ->
    Type.typeparam ->
    Reason.t ->
    Reason.t ->
    Type.t ->
    Type.t

  val reposition :
    Context.t ->
    ?trace:Type.trace ->
    ALoc.t ->
    ?desc:reason_desc ->
    ?annot_loc:ALoc.t ->
    Type.t ->
    Type.t

  val is_subtype : Context.t -> Type.trace -> use_op:use_op -> Type.t * Type.t -> unit

  val mk_targ : Context.t -> Type.typeparam -> Reason.t -> Reason.t -> Type.t

  val unresolved_id : Context.t -> Reason.t -> int

  val resolve_id : Context.t -> Type.trace -> use_op:use_op -> Type.tvar -> Type.t -> unit
end

module Instantiation_kit (H : Instantiation_helper_sig) = struct
  open H

  let cache_instantiate = cache_instantiate

  (* Instantiate a polymorphic definition given type arguments. *)
  let instantiate_poly_with_targs
      cx trace ~use_op ~reason_op ~reason_tapp ?cache ?errs_ref (tparams_loc, xs, t) ts =
    let minimum_arity = poly_minimum_arity xs in
    let maximum_arity = Nel.length xs in
    let reason_arity = mk_poly_arity_reason tparams_loc in
    if List.length ts > maximum_arity then (
      add_output
        cx
        ~trace
        (Error_message.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity));
      Base.Option.iter errs_ref ~f:(fun errs_ref ->
          errs_ref := Context.ETooManyTypeArgs (reason_arity, maximum_arity) :: !errs_ref)
    );
    let (map, _) =
      Nel.fold_left
        (fun (map, ts) typeparam ->
          let (t, ts) =
            match (typeparam, ts) with
            | ({ default = Some default; _ }, []) ->
              (* fewer arguments than params and we have a default *)
              (subst cx ~use_op map default, [])
            | ({ default = None; _ }, []) ->
              (* fewer arguments than params but no default *)
              add_output
                cx
                ~trace
                (Error_message.ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity));
              Base.Option.iter errs_ref ~f:(fun errs_ref ->
                  errs_ref := Context.ETooFewTypeArgs (reason_arity, minimum_arity) :: !errs_ref);
              (AnyT (reason_op, AnyError None), [])
            | (_, t :: ts) -> (t, ts)
          in
          let t_ = cache_instantiate cx trace ~use_op ?cache typeparam reason_op reason_tapp t in
          let frame = Frame (TypeParamBound { name = typeparam.name }, use_op) in
          is_subtype cx trace ~use_op:frame (t_, subst cx ~use_op map typeparam.bound);
          (SMap.add typeparam.name t_ map, ts))
        (SMap.empty, ts)
        xs
    in
    reposition cx ~trace (aloc_of_reason reason_tapp) (subst cx ~use_op map t)

  let mk_typeapp_of_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache id tparams_loc xs t ts =
    match cache with
    | Some cache ->
      instantiate_poly_with_targs
        cx
        trace
        ~use_op
        ~reason_op
        ~reason_tapp
        ~cache
        (tparams_loc, xs, t)
        ts
    | None ->
      let key = (id, ts) in
      let cache = Context.subst_cache cx in
      (match Hashtbl.find_opt cache key with
      | None ->
        let errs_ref = ref [] in
        let t =
          instantiate_poly_with_targs
            cx
            trace
            ~use_op
            ~reason_op
            ~reason_tapp
            ~errs_ref
            (tparams_loc, xs, t)
            ts
        in
        Hashtbl.add cache key (!errs_ref, t);
        t
      | Some (errs, t) ->
        errs
        |> List.iter (function
               | Context.ETooManyTypeArgs (reason_arity, maximum_arity) ->
                 let msg =
                   Error_message.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity)
                 in
                 add_output cx ~trace msg
               | Context.ETooFewTypeArgs (reason_arity, maximum_arity) ->
                 let msg =
                   Error_message.ETooFewTypeArgs (reason_tapp, reason_arity, maximum_arity)
                 in
                 add_output cx ~trace msg);
        t)

  (* Instantiate a polymorphic definition by creating fresh type arguments. *)
  let instantiate_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache (tparams_loc, xs, t) =
    let ts = xs |> Nel.map (fun typeparam -> mk_targ cx typeparam reason_op reason_tapp) in
    instantiate_poly_with_targs
      cx
      trace
      ~use_op
      ~reason_op
      ~reason_tapp
      ?cache
      (tparams_loc, xs, t)
      (Nel.to_list ts)

  (* Fix a this-abstracted instance type by tying a "knot": assume that the
     fixpoint is some `this`, substitute it as This in the instance type, and
     finally unify it with the instance type. Return the class type wrapping the
     instance type. *)
  let fix_this_class cx trace reason (r, i, is_this) =
    let i' =
      match Flow_cache.Fix.find cx is_this i with
      | Some i' -> i'
      | None ->
        let reason_i = reason_of_t i in
        let id = unresolved_id cx reason_i in
        let tvar = (reason_i, id) in
        let this = OpenT tvar in
        let this_generic =
          if is_this then
            GenericT
              {
                id = Context.make_generic_id cx "this" (def_aloc_of_reason r);
                reason;
                name = "this";
                bound = this;
              }
          else
            this
        in
        let i' = subst cx (SMap.singleton "this" this_generic) i in
        Flow_cache.Fix.add cx is_this i i';
        resolve_id cx trace ~use_op:unknown_use tvar i';
        i'
    in
    DefT (r, bogus_trust (), ClassT i')
end

(***********)
(* Imports *)
(***********)

let check_nonstrict_import cx trace is_strict imported_is_strict reason =
  if is_strict && not imported_is_strict then
    let loc = Reason.aloc_of_reason reason in
    let message = Error_message.ENonstrictImport loc in
    add_output cx ~trace message

(**************************************************************************)
(* Module imports                                                         *)
(*                                                                        *)
(* The process of importing from a module consists of reading from the    *)
(* foreign ModuleT type and generating a user-visible construct from it.  *)
(*                                                                        *)
(* For CommonJS imports (AKA 'require()'), if the foreign module is an ES *)
(* module we generate an object whose properties correspond to each of    *)
(* the named exports of the foreign module. If the foreign module is also *)
(* a CommonJS module, use the type of the foreign CommonJS exports value  *)
(* directly.                                                              *)
(*                                                                        *)
(* For ES imports (AKA `import` statements), simply generate a model of   *)
(* an ES ModuleNamespace object from the individual named exports of the  *)
(* foreign module. This object can then be passed up to "userland"        *)
(* directly (via `import * as`) or it can be used to extract individual   *)
(* exports from the foreign module (via `import {}` and `import X from`). *)
(**************************************************************************)

(* Import and export logic is shared between Flow_js and Annotation_inference.
 * Import_export_helper_sig is a collection of functions that create constraints in each
 * constraint engine. Type [r] represents the different styles of returning a
 * result. In Flow_js it is typical to provide constraints a type variable that
 * will receive the result of some operation, while in annotation inference to
 * directly return a type.
 *)
module type Import_export_helper_sig = sig
  type r

  val reposition :
    Context.t ->
    ?trace:Type.trace ->
    ALoc.t ->
    ?desc:reason_desc ->
    ?annot_loc:ALoc.t ->
    Type.t ->
    Type.t

  val assert_import_is_value : Context.t -> Type.trace -> Reason.t -> string -> Type.t -> unit

  val import_type : Context.t -> Type.trace -> Reason.t -> string -> Type.t -> Type.t

  val import_typeof : Context.t -> Type.trace -> Reason.t -> string -> Type.t -> Type.t

  val export_named :
    Context.t ->
    Type.trace ->
    Reason.t * (ALoc.t option * t) NameUtils.Map.t * export_kind ->
    Type.t ->
    r

  val export_named_fresh_var :
    Context.t ->
    Type.trace ->
    Reason.t * (ALoc.t option * t) NameUtils.Map.t * export_kind ->
    Type.t ->
    Type.t

  val export_type :
    Context.t ->
    Type.trace ->
    reason * name (* export_name *) * t (* target_module_t *) ->
    Type.t ->
    Type.t

  val cjs_extract_named_exports :
    Context.t -> Type.trace -> Reason.t * (Reason.t * Type.exporttypes * bool) -> Type.t -> Type.t

  val return : Context.t -> use_op:use_op -> Type.trace -> Type.t -> r

  val fix_this_class :
    Context.t -> Type.trace -> Reason.reason -> Reason.reason * Type.t * bool -> Type.t

  val mk_typeof_annotation : Context.t -> ?trace:Type.trace -> reason -> Type.t -> Type.t

  val error_type : Reason.t -> r
end

(*********************************************************************)
(* `import type` creates a properly-parameterized type alias for the *)
(* remote type -- but only for particular, valid remote types.       *)
(*********************************************************************)

(* TODO: This rule allows interpreting an object as a type!

   It is currently used to work with modules that export named types,
   e.g. 'react' or 'immutable'. For example, one can do

   `import type React from 'react'`

   followed by uses of `React` as a container of types in (say) type
   definitions like

   `type C = React.Component<any,any,any>`

   Fortunately, in that case `React` is stored as a type binding in the
   environment, so it cannot be used as a value.

   However, removing this special case causes no loss of expressibility
   (while making the model simpler). For example, in the above example we
   can write

   `import type { Component } from 'react'`

   followed by (say)

   `type C = Component<any,any,any>`

   Overall, we should be able to (at least conceptually) desugar `import
   type` to `import` followed by `type`.
*)
module ImportTypeT_kit (F : Import_export_helper_sig) = struct
  let canonicalize_imported_type cx trace reason t =
    match t with
    | DefT (_, trust, ClassT inst) -> Some (DefT (reason, trust, TypeT (ImportClassKind, inst)))
    | DefT
        (_, _, PolyT { tparams_loc; tparams = typeparams; t_out = DefT (_, trust, ClassT inst); id })
      ->
      Some
        (poly_type id tparams_loc typeparams (DefT (reason, trust, TypeT (ImportClassKind, inst))))
    (* delay fixing a polymorphic this-abstracted class until it is specialized,
       by transforming the instance type to a type application *)
    | DefT (_, _, PolyT { tparams_loc; tparams = typeparams; t_out = ThisClassT _; _ }) ->
      let targs = typeparams |> Nel.map (fun tp -> BoundT (tp.reason, tp.name)) |> Nel.to_list in
      let tapp = implicit_typeapp t targs in
      Some (poly_type (Context.generate_poly_id cx) tparams_loc typeparams (class_type tapp))
    | DefT (_, _, PolyT { t_out = DefT (_, _, TypeT _); _ }) -> Some t
    (* fix this-abstracted class when used as a type *)
    | ThisClassT (r, i, this) -> Some (F.fix_this_class cx trace reason (r, i, this))
    | DefT (enum_reason, trust, EnumObjectT enum) ->
      let enum_type = mk_enum_type ~trust enum_reason enum in
      Some (DefT (reason, trust, TypeT (ImportEnumKind, enum_type)))
    | DefT (_, _, TypeT _) -> Some t
    | AnyT _ -> Some t
    | _ -> None

  let on_concrete_type cx trace reason export_name exported_type =
    match (exported_type, export_name) with
    | ((ExactT (_, DefT (_, _, ObjT _)) | DefT (_, _, ObjT _)), "default") ->
      F.return cx trace ~use_op:unknown_use exported_type
    | (exported_type, _) ->
      (match canonicalize_imported_type cx trace reason exported_type with
      | Some imported_t -> F.return cx trace ~use_op:unknown_use imported_t
      | None ->
        add_output cx ~trace (Error_message.EImportValueAsType (reason, export_name));
        F.error_type reason)
end

(************************************************************************)
(* `import typeof` creates a properly-parameterized type alias for the  *)
(* "typeof" the remote export.                                          *)
(************************************************************************)

module ImportTypeofT_kit (F : Import_export_helper_sig) = struct
  let on_concrete_type cx trace reason export_name l =
    match l with
    | DefT
        ( _,
          _,
          PolyT
            {
              tparams_loc;
              tparams = typeparams;
              t_out = (DefT (_, _, ClassT _) | DefT (_, _, FunT _)) as lower_t;
              id;
            } ) ->
      let typeof_t = F.mk_typeof_annotation cx ~trace reason lower_t in
      F.return
        ~use_op:unknown_use
        cx
        trace
        (poly_type
           id
           tparams_loc
           typeparams
           (DefT (reason, bogus_trust (), TypeT (ImportTypeofKind, typeof_t))))
    | DefT (_, _, TypeT _)
    | DefT (_, _, PolyT { t_out = DefT (_, _, TypeT _); _ }) ->
      add_output cx ~trace (Error_message.EImportTypeAsTypeof (reason, export_name));
      F.error_type reason
    | _ ->
      let typeof_t = F.mk_typeof_annotation cx ~trace reason l in
      F.return
        ~use_op:unknown_use
        cx
        trace
        (DefT (reason, bogus_trust (), TypeT (ImportTypeofKind, typeof_t)))
end

module CJSRequireT_kit (F : Import_export_helper_sig) = struct
  (* require('SomeModule') *)
  let on_ModuleT cx trace (reason, is_strict) (module_reason, exports, imported_is_strict) =
    check_nonstrict_import cx trace is_strict imported_is_strict reason;
    let cjs_exports =
      match exports.cjs_export with
      | Some t ->
        (* reposition the export to point at the require(), like the object
           we create below for non-CommonJS exports *)
        F.reposition ~trace cx (aloc_of_reason reason) t
      | None ->
        let exports_tmap = Context.find_exports cx exports.exports_tmap in
        (* Convert ES module's named exports to an object *)
        let mk_exports_object () =
          let proto = ObjProtoT reason in
          let props =
            NameUtils.Map.map (fun (loc, t) -> Field (loc, t, Polarity.Positive)) exports_tmap
          in
          Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~frozen:true ~props proto
        in
        (* Use default export if option is enabled and module is not lib *)
        if Context.automatic_require_default cx && not (is_lib_reason_def module_reason) then
          match NameUtils.Map.find_opt (OrdinaryName "default") exports_tmap with
          | Some (_, default_t) -> default_t
          | _ -> mk_exports_object ()
        else
          mk_exports_object ()
    in
    F.return cx ~use_op:unknown_use trace cjs_exports
end

module ImportModuleNsT_kit (F : Import_export_helper_sig) = struct
  (* import * as X from 'SomeModule'; *)
  let on_ModuleT cx trace (reason, is_strict) (_, exports, imported_is_strict) =
    check_nonstrict_import cx trace is_strict imported_is_strict reason;
    let exports_tmap = Context.find_exports cx exports.exports_tmap in
    let props =
      NameUtils.Map.map (fun (loc, t) -> Field (loc, t, Polarity.Positive)) exports_tmap
    in
    let props =
      if Context.facebook_module_interop cx then
        props
      else
        match exports.cjs_export with
        | Some t ->
          (* TODO this Field should probably have a location *)
          let p = Field (None, t, Polarity.Positive) in
          NameUtils.Map.add (OrdinaryName "default") p props
        | None -> props
    in
    let obj_kind =
      if exports.has_every_named_export then
        Indexed
          {
            key = StrT.why reason |> with_trust bogus_trust;
            value = AnyT.untyped reason;
            dict_name = None;
            dict_polarity = Polarity.Neutral;
          }
      else
        Exact
    in
    let proto = ObjProtoT reason in
    let ns_obj = Obj_type.mk_with_proto cx reason ~obj_kind ~frozen:true ~props proto in
    F.return cx ~use_op:unknown_use trace ns_obj
end

module ImportDefaultT_kit (F : Import_export_helper_sig) = struct
  (* import [type] X from 'SomeModule'; *)
  let on_ModuleT
      cx
      trace
      (reason, import_kind, (local_name, module_name), is_strict)
      (module_reason, exports, imported_is_strict) =
    check_nonstrict_import cx trace is_strict imported_is_strict reason;
    let export_t =
      match exports.cjs_export with
      | Some t -> t
      | None ->
        let exports_tmap = Context.find_exports cx exports.exports_tmap in
        (match NameUtils.Map.find_opt (OrdinaryName "default") exports_tmap with
        | Some (_, t) -> t
        | None ->
          (*
           * A common error while using `import` syntax is to forget or
           * misunderstand the difference between `import foo from ...`
           * and `import {foo} from ...`. The former means to import the
           * default export to a local var called "foo", and the latter
           * means to import a named export called "foo" to a local var
           * called "foo".
           *
           * To help guide users here, if we notice that the module being
           * imported from has no default export (but it does have a named
           * export that fuzzy-matches the local name specified), we offer
           * that up as a possible "did you mean?" suggestion.
           *)
          (* TODO consider filtering these to OrdinaryNames only *)
          let known_exports =
            NameUtils.Map.keys exports_tmap |> List.rev_map display_string_of_name
          in
          let suggestion = typo_suggestion known_exports local_name in
          add_output cx ~trace (Error_message.ENoDefaultExport (reason, module_name, suggestion));
          AnyT.error module_reason)
    in
    let import_t =
      match import_kind with
      | ImportType -> F.import_type cx trace reason "default" export_t
      | ImportTypeof -> F.import_typeof cx trace reason "default" export_t
      | ImportValue ->
        F.assert_import_is_value cx trace reason "default" export_t;
        export_t
    in
    F.return cx ~use_op:unknown_use trace import_t
end

module ImportNamedT_kit (F : Import_export_helper_sig) = struct
  (* import {X} from 'SomeModule'; *)
  let on_ModuleT
      cx
      trace
      (reason, import_kind, export_name, module_name, is_strict)
      (_, exports, imported_is_strict) =
    check_nonstrict_import cx trace is_strict imported_is_strict reason;
    (*
     * When importing from a CommonJS module, we shadow any potential named
     * exports called "default" with a pointer to the raw `module.exports`
     * object
     *)
    let exports_tmap =
      let exports_tmap = Context.find_exports cx exports.exports_tmap in
      (* Drop locations; they are not needed here *)
      let exports_tmap = NameUtils.Map.map snd exports_tmap in
      match exports.cjs_export with
      | Some t -> NameUtils.Map.add (OrdinaryName "default") t exports_tmap
      | None -> exports_tmap
    in
    let has_every_named_export = exports.has_every_named_export in
    let import_t =
      match (import_kind, NameUtils.Map.find_opt (OrdinaryName export_name) exports_tmap) with
      | (ImportType, Some t) -> F.import_type cx trace reason export_name t
      | (ImportType, None) when has_every_named_export ->
        F.import_type cx trace reason export_name (AnyT.untyped reason)
      | (ImportTypeof, Some t) -> F.import_typeof cx trace reason export_name t
      | (ImportTypeof, None) when has_every_named_export ->
        F.import_typeof cx trace reason export_name (AnyT.untyped reason)
      | (ImportValue, Some t) ->
        F.assert_import_is_value cx trace reason export_name t;
        t
      | (ImportValue, None) when has_every_named_export ->
        let t = AnyT.untyped reason in
        F.assert_import_is_value cx trace reason export_name t;
        t
      | (_, None) ->
        let num_exports = NameUtils.Map.cardinal exports_tmap in
        let has_default_export = NameUtils.Map.mem (OrdinaryName "default") exports_tmap in
        let msg =
          if num_exports = 1 && has_default_export then
            Error_message.EOnlyDefaultExport (reason, module_name, export_name)
          else
            (* TODO consider filtering to OrdinaryNames only *)
            let known_exports =
              NameUtils.Map.keys exports_tmap |> List.rev_map display_string_of_name
            in
            let suggestion = typo_suggestion known_exports export_name in
            Error_message.ENoNamedExport (reason, module_name, export_name, suggestion)
        in
        add_output cx ~trace msg;
        AnyT.error reason
    in
    F.return cx trace ~use_op:unknown_use import_t
end

(**************************************************************************)
(* Module exports                                                         *)
(*                                                                        *)
(* Flow supports both CommonJS and standard ES modules as well as some    *)
(* interoperability semantics for communicating between the two module    *)
(* systems in both directions.                                            *)
(*                                                                        *)
(* In order to support both systems at once, Flow abstracts the notion of *)
(* module exports by storing a type map for each of the exports of a      *)
(* given module, and for each module there is a ModuleT that maintains    *)
(* this type map. The exported types are then considered immutable once   *)
(* the module has finished inference.                                     *)
(*                                                                        *)
(* When a type is set for the CommonJS exports value, we store it         *)
(* separately from the normal named exports tmap that ES exports are      *)
(* stored within. This allows us to distinguish CommonJS modules from ES  *)
(* modules when interpreting an ES import statement -- which is important *)
(* because ES ModuleNamespace objects built from CommonJS exports are a   *)
(* little bit magic.                                                      *)
(*                                                                        *)
(* For example: If a CommonJS module exports an object, we will extract   *)
(* each of the properties of that object and consider them as "named"     *)
(* exports for the purposes of an import statement elsewhere:             *)
(*                                                                        *)
(*   // CJSModule.js                                                      *)
(*   module.exports = {                                                   *)
(*     someNumber: 42                                                     *)
(*   };                                                                   *)
(*                                                                        *)
(*   // ESModule.js                                                       *)
(*   import {someNumber} from "CJSModule";                                *)
(*   var a: number = someNumber;                                          *)
(*                                                                        *)
(* We also map CommonJS export values to the "default" export for         *)
(* purposes of import statements in other modules:                        *)
(*                                                                        *)
(*   // CJSModule.js                                                      *)
(*   module.exports = {                                                   *)
(*     someNumber: 42                                                     *)
(*   };                                                                   *)
(*                                                                        *)
(*   // ESModule.js                                                       *)
(*   import CJSDefaultExport from "CJSModule";                            *)
(*   var a: number = CJSDefaultExport.someNumber;                         *)
(*                                                                        *)
(* Note that the ModuleT type is not intended to be surfaced to any       *)
(* userland-visible constructs. Instead it's meant as an internal         *)
(* construct that is only *mapped* to/from userland constructs (such as a *)
(* CommonJS exports object or an ES ModuleNamespace object).              *)
(**************************************************************************)

(* In the following rules, ModuleT appears in two contexts: as imported
   modules, and as modules to be exported.

   As a module to be exported, ModuleT denotes a "growing" module. In this
   form, its contents may change: e.g., its named exports may be
   extended. Conversely, the rules that drive this growing phase can expect
   to work only on ModuleT. In particular, modules that are not @flow never
   hit growing rules: they are modeled as `any`.

   On the other hand, as an imported module, ModuleT denotes a "fully
   formed" module. The rules hit by such a module don't grow it: they just
   take it apart and read it. The same rules could also be hit by modules
   that are not @flow, so the rules have to deal with `any`. *)

(* util that grows a module by adding named exports from a given map *)
module ExportNamedT_kit (F : Import_export_helper_sig) = struct
  let on_ModuleT cx trace (_, tmap, export_kind) lhs (_, { exports_tmap; _ }, _) =
    let add_export name export acc =
      let export' =
        match export_kind with
        | ExportValue -> export
        | ReExport ->
          (* Re-exports do not overwrite named exports from the local module. *)
          NameUtils.Map.find_opt name acc |> Base.Option.value ~default:export
        | ExportType -> export
      in
      NameUtils.Map.add name export' acc
    in
    Context.find_exports cx exports_tmap
    |> NameUtils.Map.fold add_export tmap
    |> Context.add_export_map cx exports_tmap;
    F.return cx trace ~use_op:unknown_use lhs
end

module AssertExportIsTypeT_kit (F : Import_export_helper_sig) = struct
  let rec is_type = function
    | DefT (_, _, ClassT _)
    | DefT (_, _, EnumObjectT _)
    | ThisClassT (_, _, _)
    | DefT (_, _, TypeT _)
    | AnyT _ ->
      true
    | DefT (_, _, PolyT { t_out = t'; _ }) -> is_type t'
    | _ -> false

  let on_concrete_type cx trace name l =
    if is_type l then
      F.return cx trace ~use_op:unknown_use l
    else
      let reason = reason_of_t l in
      add_output cx ~trace Error_message.(EExportValueAsType (reason, name));
      F.return ~use_op:unknown_use cx trace (AnyT.error reason)
end

(* Copy the named exports from a source module into a target module. Used
   to implement `export * from 'SomeModule'`, with the current module as
   the target and the imported module as the source. *)
module CopyNamedExportsT_kit (F : Import_export_helper_sig) = struct
  let on_ModuleT cx trace (reason, target_module_t) (_, source_exports, _) : F.r =
    let source_tmap = Context.find_exports cx source_exports.exports_tmap in
    F.export_named cx trace (reason, source_tmap, ReExport) target_module_t

  (* There is nothing to copy from a module exporting `any` or `Object`. *)
  let on_AnyT cx trace lreason (reason, target_module) =
    check_untyped_import cx ImportValue lreason reason;
    F.return ~use_op:unknown_use cx trace target_module
end

(* Copy only the type exports from a source module into a target module.
 * Used to implement `export type * from ...`. *)
module CopyTypeExportsT_kit (F : Import_export_helper_sig) = struct
  let on_ModuleT cx trace (reason, target_module_t) (_, source_exports, _) =
    let source_exports = Context.find_exports cx source_exports.exports_tmap in
    (* Remove locations. TODO at some point we may want to include them here. *)
    let source_exports = NameUtils.Map.map snd source_exports in
    let target_module_t =
      NameUtils.Map.fold
        (fun export_name export_t target_module_t ->
          F.export_type cx trace (reason, export_name, target_module_t) export_t)
        source_exports
        target_module_t
    in
    F.return cx trace ~use_op:unknown_use target_module_t

  (* There is nothing to copy from a module exporting `any` or `Object`. *)
  let on_AnyT cx trace lreason (reason, target_module) =
    check_untyped_import cx ImportValue lreason reason;
    F.return ~use_op:unknown_use cx trace target_module
end

(* Export a type from a given ModuleT, but only if the type is compatible
 * with `import type`/`export type`. When it is not compatible, it is simply
 * not added to the exports map.
 *
 * Note that this is very similar to `ExportNamedT` except that it only
 * exports one type at a time and it takes the type to be exported as a
 * lower (so that the type can be filtered post-resolution). *)
module ExportTypeT_kit (F : Import_export_helper_sig) = struct
  module ImportTypeTKit = ImportTypeT_kit (F)

  let on_concrete_type cx trace (reason, export_name, target_module_t) l =
    let is_type_export =
      match l with
      | DefT (_, _, ObjT _) when export_name = OrdinaryName "default" -> true
      | l -> ImportTypeTKit.canonicalize_imported_type cx trace reason l <> None
    in
    if is_type_export then
      (* TODO we may want to add location information here *)
      let named = NameUtils.Map.singleton export_name (None, l) in
      F.export_named cx trace (reason, named, ReExport) target_module_t
    else
      F.return cx trace ~use_op:unknown_use target_module_t
end

module CJSExtractNamedExportsT_kit (F : Import_export_helper_sig) = struct
  let on_concrete_type cx trace (reason, local_module) = function
    (* ObjT CommonJS export values have their properties turned into named exports. *)
    | DefT (_, _, ObjT o)
    | ExactT (_, DefT (_, _, ObjT o)) ->
      let { props_tmap; proto_t; _ } = o in
      (* Copy props from the prototype *)
      let module_t = F.cjs_extract_named_exports cx trace (reason, local_module) proto_t in
      (* Copy own props *)
      F.export_named
        cx
        trace
        (reason, Properties.extract_named_exports (Context.find_props cx props_tmap), ExportValue)
        module_t
    (* InstanceT CommonJS export values have their properties turned into named exports. *)
    | DefT (_, _, InstanceT (_, _, _, { own_props; proto_props; _ })) ->
      let module_t = ModuleT local_module in
      let extract_named_exports id =
        Context.find_props cx id
        |> NameUtils.Map.filter (fun x _ -> not (is_munged_prop_name cx x))
        |> Properties.extract_named_exports
      in
      (* Copy own props *)
      let module_t =
        F.export_named_fresh_var
          cx
          trace
          (reason, extract_named_exports own_props, ExportValue)
          module_t
      in
      (* Copy proto props *)
      (* TODO: own props should take precedence *)
      F.export_named cx trace (reason, extract_named_exports proto_props, ExportValue) module_t
    (* If the module is exporting any or Object, then we allow any named import. *)
    | AnyT _ ->
      let (module_t_reason, exporttypes, is_strict) = local_module in
      let module_t =
        ModuleT (module_t_reason, { exporttypes with has_every_named_export = true }, is_strict)
      in
      F.return cx trace ~use_op:unknown_use module_t
    (* All other CommonJS export value types do not get merged into the named
     * exports tmap in any special way. *)
    | _ -> F.return cx trace ~use_op:unknown_use (ModuleT local_module)
end
