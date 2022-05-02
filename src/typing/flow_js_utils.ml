(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
let possible_types cx id = types_of (Context.find_graph cx id) |> List.filter is_proper_def

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let possible_uses cx id = uses_of (Context.find_graph cx id) |> List.filter is_proper_use

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
          | SymbolT | EnumObjectT _ | EnumT _ )
        ),
      _
    )
  | ( _,
      DefT
        ( _,
          _,
          ( NumT _ | StrT _ | BoolT _ | SingletonNumT _ | SingletonStrT _ | SingletonBoolT _
          | SymbolT | EnumObjectT _ | EnumT _ )
        )
    ) ->
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
      | DefT (_, _, FunT (_, ft)) ->
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
  | DefT (_, _, FunT (_, ft)) ->
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
        | DefT (r2, t2, FunT (static, ft)) ->
          let (resolved, params) = replace_params [] (resolved, ft.params) in
          let (resolved, rest_param) = replace_rest_param (resolved, ft.rest_param) in
          assert (resolved = []);
          let id' =
            Context.make_call_prop cx (DefT (r2, t2, FunT (static, { ft with params; rest_param })))
          in
          UseT (op, DefT (r1, t1, ObjT { o with call_t = Some id' }))
        | _ -> u
      end
    | UseT (op, DefT (r, trust, FunT (t1, ft))) ->
      let (resolved, params) = replace_params [] (resolved, ft.params) in
      let (resolved, rest_param) = replace_rest_param (resolved, ft.rest_param) in
      assert (resolved = []);
      UseT (op, DefT (r, trust, FunT (t1, { ft with params; rest_param })))
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
  | GetPropT (_, _, _, Named (r, name), _) ->
    Error_message.IncompatibleGetPropT (aloc_of_reason r, Some name)
  | GetPropT (_, _, _, Computed t, _) -> Error_message.IncompatibleGetPropT (loc_of_t t, None)
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
  | SpecializeT (Op (ClassExtendsCheck _), _, _, _, _, _) -> Error_message.IncompatibleSuperT
  | SpecializeT _ -> Error_message.IncompatibleSpecializeT
  | ConcretizeTypeAppsT _ -> Error_message.IncompatibleSpecializeT
  | ThisSpecializeT _ -> Error_message.IncompatibleThisSpecializeT
  | VarianceCheckT _ -> Error_message.IncompatibleVarianceCheckT
  | GetKeysT _ -> Error_message.IncompatibleGetKeysT
  | HasOwnPropT
      ( _,
        r,
        ( DefT (_, _, StrT (Literal (_, name)))
        | GenericT { bound = DefT (_, _, StrT (Literal (_, name))); _ } )
      ) ->
    Error_message.IncompatibleHasOwnPropT (aloc_of_reason r, Some name)
  | HasOwnPropT (_, r, _) -> Error_message.IncompatibleHasOwnPropT (aloc_of_reason r, None)
  | GetValuesT _ -> Error_message.IncompatibleGetValuesT
  | UnaryMinusT _ -> Error_message.IncompatibleUnaryMinusT
  | MapTypeT (_, _, (ObjectMap _ | ObjectMapi _ | ObjectMapConst _ | ObjectKeyMirror), _) ->
    Error_message.IncompatibleMapTypeTObject
  | TypeAppVarianceCheckT _ -> Error_message.IncompatibleTypeAppVarianceCheckT
  | GetStaticsT _ -> Error_message.IncompatibleGetStaticsT
  | BindT _ -> Error_message.IncompatibleBindT
  | use_t -> Error_message.IncompatibleUnclassified (string_of_use_ctor use_t)

let use_op_of_lookup_action = function
  | ReadProp { use_op; _ }
  | WriteProp { use_op; _ }
  | LookupProp (use_op, _)
  | SuperProp (use_op, _)
  | MatchProp { use_op; _ } ->
    use_op

exception SpeculativeError of Error_message.t

(* [src_cx] is the context in which the error is created, and [dst_cx] the context
 * in which it is recorded. *)
let add_output_generic ~src_cx:cx ~dst_cx ?trace msg =
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
                 || LintSettings.get_value lint_kind lint_settings <> Severity.Off
             )
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

    Context.add_error dst_cx error
  )

let add_output cx ?trace msg = add_output_generic ~src_cx:cx ~dst_cx:cx ?trace msg

(* In annotation inference, errors are created in the exporting side (src_cx), and
 * are recorded in the importing one (dst_cx). *)
let add_annot_inference_error ~src_cx ~dst_cx msg : unit = add_output_generic ~src_cx ~dst_cx msg

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
  let unwrap_type cx t = Base.Option.value (Context.find_resolved cx t) ~default:t in
  (* Compare l to u. Flatten both unions and then check that each element
     of l is comparable to an element of u. Note that the comparator need not
     be symmetric. *)
  let union_compare cx comparator lts uts =
    if Context.is_verbose cx then prerr_endline "union_compare slow";
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
        (* Only optimize for enums, since this is the only fast path examined below.
         * Note that optimizing both reps with [UnionRep.optimize] can potentially
         * cause a `RecursionCheck.LimitExceeded` exception. (`tests/typeapp_termination`
         * is a sanity check against that.) *)
        if not (UnionRep.is_optimized_finally rep1) then
          UnionRep.optimize_enum_only ~flatten:(Type_mapper.union_flatten cx) rep1;
        if not (UnionRep.is_optimized_finally rep2) then
          UnionRep.optimize_enum_only ~flatten:(Type_mapper.union_flatten cx) rep2;

        match (UnionRep.check_enum rep1, UnionRep.check_enum rep2) with
        | (Some enums1, Some enums2) -> UnionEnumSet.subset enums1 enums2
        | (_, _) ->
          let unwrap rep = UnionRep.members rep |> Base.List.map ~f:(unwrap_type cx) in
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
         mod_reason_of_t (union_reason i) %> mk_tuple_swapped u %> f cx trace
     )

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

(* New generics mode: generate a GenericT from a generic *)

let generic_of_tparam cx ~f { bound; name; reason = param_reason; is_this = _; _ } =
  let param_loc = aloc_of_reason param_reason in
  let bound = f bound in
  let id = Context.make_generic_id cx name param_loc in
  let bound =
    mod_reason_of_t
      (fun bound_reason ->
        let annot_loc = annot_aloc_of_reason bound_reason in
        let desc = desc_of_reason ~unwrap:false bound_reason in
        opt_annot_reason ?annot_loc @@ mk_reason desc param_loc)
      bound
  in
  GenericT { reason = reason_of_t bound; name; id; bound }

let generic_bound cx prev_map ({ name; _ } as tparam) =
  let generic = generic_of_tparam cx ~f:(subst cx prev_map) tparam in
  (generic, Subst_name.Map.add name generic prev_map)

let mk_tparams cx params =
  let (map, rev_lst) =
    Base.List.fold_left
      ~f:(fun (prev_map, rev_lst) tparam ->
        let (generic, map) = generic_bound cx prev_map tparam in
        (map, generic :: rev_lst))
      ~init:(Subst_name.Map.empty, [])
      params
  in
  (map, List.rev rev_lst)

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
        (Subst_name.Map.add name t map, t :: list))
      (Subst_name.Map.empty, [])
      typeparams
  in
  List.rev revlist

(** Builtins *)

let lookup_builtin_strict cx x reason =
  let builtins = Context.builtins cx in
  Builtins.get_builtin builtins x ~on_missing:(fun () ->
      let potential_generator =
        Context.missing_module_generators cx
        |> Base.List.find ~f:(fun (pattern, _) -> Str.string_match pattern (uninternal_name x) 0)
        |> Base.Option.map ~f:snd
      in
      add_output
        cx
        (Error_message.EBuiltinLookupFailed { reason; name = Some x; potential_generator });
      AnyT.error_of_kind UnresolvedName reason
  )

let lookup_builtin_with_default cx x default =
  let builtins = Context.builtins cx in
  Builtins.get_builtin builtins x ~on_missing:(fun () -> default)

let lookup_builtin_typeapp cx reason x targs =
  let t = lookup_builtin_strict cx x reason in
  typeapp reason t targs

(**
 * Determines whether a property name should be considered "munged"/private when
 * the `munge_underscores` config option is set.
 *)
let is_munged_prop_name_with_munge name ~should_munge_underscores =
  Signature_utils.is_munged_property_name name && should_munge_underscores

let is_munged_prop_name cx name =
  is_munged_prop_name_with_munge name ~should_munge_underscores:(Context.should_munge_underscores cx)

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

let obj_key_mirror cx trust o reason_op =
  let map_t key t =
    match t with
    | OptionalT _ -> optional key
    | _ -> key
  in
  let map_field key t =
    let reason = replace_desc_reason (RStringLit key) reason_op in
    map_t (DefT (reason, bogus_trust (), SingletonStrT key)) t
  in
  map_obj cx trust o reason_op ~map_t ~map_field

let obj_map_const cx trust o reason_op target =
  let map_t _ t =
    match t with
    | OptionalT _ -> optional target
    | _ -> target
  in
  let map_field _ t = map_t target t in
  map_obj cx trust o reason_op ~map_t ~map_field

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
          errs_ref := Context.ETooManyTypeArgs (reason_arity, maximum_arity) :: !errs_ref
      )
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
                  errs_ref := Context.ETooFewTypeArgs (reason_arity, minimum_arity) :: !errs_ref
              );
              (AnyT (reason_op, AnyError None), [])
            | (_, t :: ts) -> (t, ts)
          in
          let t_ = cache_instantiate cx trace ~use_op ?cache typeparam reason_op reason_tapp t in
          let frame = Frame (TypeParamBound { name = typeparam.name }, use_op) in
          is_subtype cx trace ~use_op:frame (t_, subst cx ~use_op map typeparam.bound);
          (Subst_name.Map.add typeparam.name t_ map, ts))
        (Subst_name.Map.empty, ts)
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
                 add_output cx ~trace msg
               );
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
  let fix_this_class cx trace reason (r, i, is_this, this_name) =
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
                id = Context.make_generic_id cx this_name (def_aloc_of_reason r);
                reason;
                name = this_name;
                bound = this;
              }
          else
            this
        in
        let i' = subst cx (Subst_name.Map.singleton this_name this_generic) i in
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
    Context.t ->
    Type.trace ->
    Reason.reason ->
    Reason.reason * Type.t * bool * Subst_name.t ->
    Type.t

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
      let (_, targs) = typeparams |> Nel.to_list |> mk_tparams cx in
      let tapp = implicit_typeapp t targs in
      Some (poly_type (Type.Poly.generate_id ()) tparams_loc typeparams (class_type tapp))
    | DefT (_, _, PolyT { t_out = DefT (_, _, TypeT _); _ }) -> Some t
    (* fix this-abstracted class when used as a type *)
    | ThisClassT (r, i, this, this_name) ->
      Some (F.fix_this_class cx trace reason (r, i, this, this_name))
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
            }
        ) ->
      let typeof_t = F.mk_typeof_annotation cx ~trace reason lower_t in
      F.return
        ~use_op:unknown_use
        cx
        trace
        (poly_type
           id
           tparams_loc
           typeparams
           (DefT (reason, bogus_trust (), TypeT (ImportTypeofKind, typeof_t)))
        )
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
    | ThisClassT (_, _, _, _)
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

(*******************)
(* GetPropT helper *)
(*******************)

module Access_prop_options = struct
  type t = {
    use_op: Type.use_op;
    previously_seen_props: Type.Properties.Set.t;
    allow_method_access: bool;
    lookup_kind: Type.lookup_kind;
    (* Same `id` as in `GetPropT` and `TestPropT`: it represents some syntactic access. *)
    id: ident option;
  }
end

module type Get_prop_helper_sig = sig
  type r

  val read_prop :
    Context.t ->
    Type.trace ->
    Access_prop_options.t ->
    Reason.reason ->
    Reason.reason ->
    Type.t ->
    Type.t ->
    Reason.name ->
    Type.property NameUtils.Map.t ->
    r

  val dict_read_check : Context.t -> Type.trace -> use_op:Type.use_op -> Type.t * Type.t -> unit

  val cg_lookup :
    Context.t ->
    Type.trace ->
    obj_t:Type.t ->
    Type.t ->
    Reason.reason * Type.lookup_kind * Type.propref * use_op * Type.Properties.Set.t ->
    r

  val reposition :
    Context.t ->
    ?trace:Type.trace ->
    ALoc.t ->
    ?desc:reason_desc ->
    ?annot_loc:ALoc.t ->
    Type.t ->
    Type.t

  val enum_proto :
    Context.t -> Type.trace -> reason:Reason.t -> Reason.t * Trust.trust_rep * Type.enum_t -> Type.t

  val return : Context.t -> use_op:use_op -> Type.trace -> Type.t -> r

  val error_type : Reason.t -> r

  val cg_get_prop :
    Context.t ->
    Type.trace ->
    Type.t ->
    use_op * reason * Type.ident option * (Reason.t * Reason.name) ->
    r
end

module GetPropT_kit (F : Get_prop_helper_sig) = struct
  let on_InstanceT cx trace ~l ~id r super insttype use_op reason_op propref =
    match propref with
    | Named (_, OrdinaryName "constructor") ->
      F.return
        cx
        trace
        ~use_op:unknown_use
        (TypeUtil.class_type ?annot_loc:(annot_aloc_of_reason r) l)
    | Named (reason_prop, x) ->
      let own_props = Context.find_props cx insttype.own_props in
      let proto_props = Context.find_props cx insttype.proto_props in
      let fields = NameUtils.Map.union own_props proto_props in
      let lookup_kind =
        if insttype.has_unknown_react_mixins then
          NonstrictReturning (None, None)
        else
          Strict r
      in
      let options =
        {
          Access_prop_options.use_op;
          previously_seen_props = Properties.Set.of_list [insttype.own_props; insttype.proto_props];
          allow_method_access = false;
          lookup_kind;
          id;
        }
      in
      (* Instance methods cannot be unbound *)
      F.read_prop cx trace options reason_prop reason_op l super x fields
    | Computed _ ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = aloc_of_reason reason_op in
      add_output cx ~trace Error_message.(EInternal (loc, InstanceLookupComputed));
      F.error_type reason_op

  let on_EnumObjectT cx trace enum_reason trust enum access =
    let (_, access_reason, _, (prop_reason, member_name)) = access in
    let { members; _ } = enum in
    let error_invalid_access ~suggestion =
      let member_reason = replace_desc_reason (RIdentifier member_name) prop_reason in
      add_output
        cx
        ~trace
        (Error_message.EEnumInvalidMemberAccess
           { member_name = Some member_name; suggestion; reason = member_reason; enum_reason }
        );
      F.return cx trace ~use_op:unknown_use (AnyT.error access_reason)
    in
    (* We guarantee in the parser that enum member names won't start with lowercase
     * "a" through "z", these are reserved for methods. *)
    let is_valid_member_name name =
      Base.String.is_empty name || (not @@ Base.Char.is_lowercase name.[0])
    in
    match member_name with
    | OrdinaryName name when is_valid_member_name name ->
      if SMap.mem name members then
        let enum_type =
          F.reposition
            cx
            ~trace
            (aloc_of_reason access_reason)
            (mk_enum_type ~trust enum_reason enum)
        in
        F.return cx trace ~use_op:unknown_use enum_type
      else
        let suggestion = typo_suggestion (SMap.keys members |> List.rev) name in
        error_invalid_access ~suggestion
    | OrdinaryName _ ->
      let t = F.enum_proto cx trace ~reason:access_reason (enum_reason, trust, enum) in
      F.cg_get_prop cx trace t access
    | InternalName _
    | InternalModuleName _ ->
      error_invalid_access ~suggestion:None

  let on_array_length cx trace reason trust ts reason_op =
    (* Use definition as the reason for the length, as this is
     * the actual location where the length is in fact set. *)
    let loc = Reason.aloc_of_reason reason_op in
    let t = tuple_length reason trust ts in
    F.return cx trace ~use_op:unknown_use (F.reposition cx ~trace loc t)

  let get_obj_prop cx trace o propref reason_op =
    let named_prop =
      match propref with
      | Named (_, x) -> Context.get_prop cx o.props_tmap x
      | Computed _ -> None
    in
    let dict_t = Obj_type.get_dict_opt o.flags.obj_kind in
    match (propref, named_prop, dict_t) with
    | (_, Some prop, _) ->
      (* Property exists on this property map *)
      Some (prop, PropertyMapProperty)
    | (Named (_, x), None, Some { key; value; dict_polarity; _ }) when not (is_dictionary_exempt x)
      ->
      (* Dictionaries match all property reads *)
      F.dict_read_check cx trace ~use_op:unknown_use (string_key x reason_op, key);
      Some (Field (None, value, dict_polarity), IndexerProperty)
    | (Computed k, None, Some { key; value; dict_polarity; _ }) ->
      F.dict_read_check cx trace ~use_op:unknown_use (k, key);
      Some (Field (None, value, dict_polarity), IndexerProperty)
    | _ -> None

  let perform_read_prop_action cx trace use_op propref p ureason =
    match Property.read_t p with
    | Some t ->
      let loc = aloc_of_reason ureason in
      F.return cx trace ~use_op:unknown_use (F.reposition cx ~trace loc t)
    | None ->
      let (reason_prop, prop_name) =
        match propref with
        | Named (r, x) -> (r, Some x)
        | Computed t -> (reason_of_t t, None)
      in
      let msg = Error_message.EPropNotReadable { reason_prop; prop_name; use_op } in
      add_output cx ~trace msg;
      F.error_type ureason

  let read_obj_prop cx trace ~use_op o propref reason_obj reason_op lookup_info =
    let l = DefT (reason_obj, bogus_trust (), ObjT o) in
    match get_obj_prop cx trace o propref reason_op with
    | Some (p, _target_kind) ->
      Base.Option.iter ~f:(fun (id, _) -> Context.test_prop_hit cx id) lookup_info;
      perform_read_prop_action cx trace use_op propref p reason_op
    | None ->
      (match propref with
      | Named (reason_prop, name) ->
        let lookup_kind =
          if Obj_type.sealed_in_op reason_op o.flags.obj_kind then
            match lookup_info with
            | Some (id, lookup_default_tout) when Obj_type.is_exact o.flags.obj_kind ->
              let lookup_default =
                let r = replace_desc_reason (RMissingProperty (Some name)) reason_op in
                Some (DefT (r, bogus_trust (), VoidT), lookup_default_tout)
              in
              NonstrictReturning (lookup_default, Some (id, (reason_prop, reason_obj)))
            | _ -> Strict reason_obj
          else
            ShadowRead (None, Nel.one o.props_tmap)
        in
        let x = (reason_op, lookup_kind, propref, use_op, Properties.Set.singleton o.props_tmap) in
        F.cg_lookup cx trace ~obj_t:l o.proto_t x
      | Computed elem_t ->
        (match elem_t with
        | OpenT _ ->
          let loc = loc_of_t elem_t in
          add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedOpen));
          F.error_type reason_op
        | GenericT { bound = DefT (_, _, StrT (Literal _)); _ }
        | DefT (_, _, StrT (Literal _)) ->
          let loc = loc_of_t elem_t in
          add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedLiteral));
          F.error_type reason_op
        | AnyT _ -> F.return cx trace ~use_op:unknown_use (AnyT.untyped reason_op)
        | GenericT { bound = DefT (_, _, StrT _); _ }
        | GenericT { bound = DefT (_, _, NumT _); _ }
        | DefT (_, _, StrT _)
        | DefT (_, _, NumT _) ->
          (* string, and number keys are allowed, but there's nothing else to
             flow without knowing their literal values. *)
          F.return cx trace ~use_op:unknown_use (Unsoundness.why ComputedNonLiteralKey reason_op)
        | _ ->
          let reason_prop = reason_of_t elem_t in
          add_output cx ~trace (Error_message.EObjectComputedPropertyAccess (reason_op, reason_prop));
          F.error_type reason_op))
end

(***************)
(* ElemT utils *)
(***************)

let array_elem_check ~write_action cx trace l use_op reason reason_tup arrtype =
  let (value, ts, is_index_restricted, is_tuple) =
    match arrtype with
    | ArrayAT (value, ts) -> (value, ts, false, false)
    | TupleAT (value, ts) -> (value, Some ts, true, true)
    | ROArrayAT value -> (value, None, true, false)
  in
  let (can_write_tuple, value) =
    match l with
    | DefT (index_reason, _, NumT (Literal (_, (float_value, _)))) ->
      begin
        match ts with
        | None -> (false, value)
        | Some ts ->
          let index_string = Dtoa.ecma_string_of_float float_value in
          begin
            match int_of_string_opt index_string with
            | Some index ->
              let value_opt =
                try List.nth_opt ts index with
                | Invalid_argument _ -> None
              in
              begin
                match value_opt with
                | Some value -> (true, value)
                | None ->
                  if is_tuple then (
                    add_output
                      cx
                      ~trace
                      (Error_message.ETupleOutOfBounds
                         {
                           use_op;
                           reason;
                           reason_op = reason_tup;
                           length = List.length ts;
                           index = index_string;
                         }
                      );
                    (true, AnyT.error (mk_reason RTupleOutOfBoundsAccess (aloc_of_reason reason)))
                  ) else
                    (true, value)
              end
            | None ->
              (* not an integer index *)
              if is_tuple then (
                add_output
                  cx
                  ~trace
                  (Error_message.ETupleNonIntegerIndex
                     { use_op; reason = index_reason; index = index_string }
                  );
                (true, AnyT.error reason)
              ) else
                (true, value)
          end
      end
    | _ -> (false, value)
  in
  ( if is_index_restricted && (not can_write_tuple) && write_action then
    let error =
      match ts with
      | Some _ -> Error_message.ETupleUnsafeWrite { reason; use_op }
      | None -> Error_message.EROArrayWrite ((reason, reason_tup), use_op)
    in
    add_output cx ~trace error
  );
  (value, is_tuple)

let propref_for_elem_t ?on_named_prop = function
  | GenericT { bound = DefT (_, _, StrT (Literal (_, x))); reason = reason_x; _ }
  | DefT (reason_x, _, StrT (Literal (_, x))) ->
    let reason_named = replace_desc_reason (RStringLit x) reason_x in
    Base.Option.iter ~f:(fun f -> f reason_named) on_named_prop;
    let reason_prop = replace_desc_reason (RProperty (Some x)) reason_x in
    Named (reason_prop, x)
  | l -> Computed l

let keylist_of_props props reason_op =
  NameUtils.Map.fold
    (fun name _ acc ->
      match name with
      | OrdinaryName _ ->
        let reason = replace_desc_new_reason (RStringLit name) reason_op in
        DefT (reason, bogus_trust (), SingletonStrT name) :: acc
      | InternalName _
      | InternalModuleName _ ->
        acc)
    props
    []

let objt_to_obj_rest cx props_tmap flags reason xs =
  let props = Context.find_props cx props_tmap in
  let props = List.fold_left (fun map x -> NameUtils.Map.remove (OrdinaryName x) map) props xs in
  (* Remove shadow properties from rest result *)
  (* TODO consider converting to SMap here so downstream code doesn't need to
   * handle internal names *)
  let props = NameUtils.Map.filter (fun x _ -> not (is_internal_name x)) props in
  let proto = ObjProtoT reason in
  (* A rest result can not be exact if the source object is unsealed,
     because we may not have seen all the writes yet. *)
  let obj_kind =
    match flags.obj_kind with
    | UnsealedInFile _ when not (Obj_type.sealed_in_op reason flags.obj_kind) ->
      UnsealedInFile (ALoc.source (aloc_of_reason reason))
    | UnsealedInFile _
    | Exact ->
      Exact
    | Indexed d -> Indexed d
    | _ -> Inexact
  in
  Obj_type.mk_with_proto cx reason ~props proto ~obj_kind

(* $Values *)

let get_values_type_of_obj_t cx o reason =
  let { flags; proto_t = _; props_tmap = tmap; call_t = _ (* call props excluded from values *) } =
    o
  in
  (* Find all of the props. *)
  let props = Context.find_props cx tmap in
  (* Get the read type for all readable properties and discard the rest. *)
  let ts =
    NameUtils.Map.fold
      (fun _ prop ts ->
        match Property.read_t prop with
        | Some t ->
          let t =
            if flags.frozen then
              match t with
              | DefT (t_reason, trust, StrT (Literal (_, (OrdinaryName _ as name)))) ->
                let t_reason = replace_desc_reason (RStringLit name) t_reason in
                DefT (t_reason, trust, SingletonStrT name)
              | DefT (t_reason, trust, NumT (Literal (_, lit))) ->
                let t_reason = replace_desc_reason (RNumberLit (snd lit)) t_reason in
                DefT (t_reason, trust, SingletonNumT lit)
              | DefT (t_reason, trust, BoolT (Some lit)) ->
                let t_reason = replace_desc_reason (RBooleanLit lit) t_reason in
                DefT (t_reason, trust, SingletonBoolT lit)
              | _ -> t
            else
              t
          in
          t :: ts
        | None -> ts)
      props
      []
  in
  (* If the object has a dictionary value then add that to our types. *)
  let ts =
    match flags.obj_kind with
    | Indexed { value; _ } -> value :: ts
    | _ -> ts
  in
  (* Create a union type from all our selected types. *)
  Type_mapper.union_flatten cx ts |> union_of_ts reason

let get_values_type_of_instance_t cx own_props reason =
  (* Find all of the props. *)
  let props = Context.find_props cx own_props in
  (* Get the read type for all readable properties and discard the rest. *)
  let ts =
    NameUtils.Map.fold
      (fun key prop ts ->
        match Property.read_t prop with
        (* We don't want to include the property type if its name is the
           internal value "$key" because that will be the type for the instance
           index and not the value. *)
        | Some t when key != OrdinaryName "$key" -> t :: ts
        | _ -> ts)
      props
      []
  in
  (* Create a union type from all our selected types. *)
  Type_mapper.union_flatten cx ts |> union_of_ts reason
