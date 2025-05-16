(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type_subst
open Type
open Constraint
open TypeUtil
open Utils_js
module FlowError = Flow_error

type cx = Context.t

type loc = ALoc.t

(* For any constraints, return a list of def types that form either the lower
   bounds of the solution, or a singleton containing the solution itself. *)
let types_of cx : constraints -> TypeTerm.t list = function
  | Unresolved { lower; _ } -> TypeMap.keys lower
  | Resolved t -> [t]
  | FullyResolved s -> [Context.force_fully_resolved_tvar cx s]

let uses_of cx : constraints -> TypeTerm.use_t list = function
  | Unresolved { upper; _ } -> Base.List.map ~f:fst (UseTypeMap.keys upper)
  | Resolved t -> [TypeTerm.UseT (unknown_use, t)]
  | FullyResolved s -> [TypeTerm.UseT (unknown_use, Context.force_fully_resolved_tvar cx s)]

(* These possible_* functions would ideally be in constraint.ml, but since they use
 * Context and Context depends on Constraint we need to extract these functions
 * to a separate module in order to avoid a circular dependency *)

(* Def types that describe the solution of a type variable. *)
let possible_types cx id = types_of cx (Context.find_graph cx id)

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let possible_uses cx id = uses_of cx (Context.find_graph cx id)

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
    | DefT (_, EmptyT) when filter_empty -> collect_lowers ~filter_empty cx seen acc ts
    (* Everything else becomes part of the merge typed *)
    | _ -> collect_lowers ~filter_empty cx seen (t :: acc) ts)

let merge_tvar_opt ?(filter_empty = false) ?(union_kind = UnionRep.ResolvedKind) cx r id =
  let lowers =
    let seen = ISet.singleton id in
    collect_lowers cx seen [] (possible_types cx id) ~filter_empty
  in
  match lowers with
  | [t] -> Some t
  | t0 :: t1 :: ts -> Some (UnionT (r, UnionRep.make ~kind:union_kind t0 t1 ts))
  | [] -> None

let merge_tvar ?(filter_empty = false) ~no_lowers cx r id =
  match merge_tvar_opt ~filter_empty cx r id with
  | Some t -> t
  | None -> no_lowers cx r

module InvalidCyclicTypeValidation : sig
  val validate_local_type : Context.t -> Type.t -> unit

  val validate_type_sig_type :
    error_recursive:(Context.t -> Reason.t -> Type.t) -> Context.t -> Type.t -> unit
end = struct
  open Type
  open Reason
  open Utils_js

  class tvar_forcing_visitor =
    object (this)
      inherit [ISet.t * bool] Type_visitor.t as super

      method! call_prop _cx _pole acc _id = acc

      method! props _cx _pole acc _id = acc

      method! exports _cx _pole acc _id = acc

      method! type_ cx pole acc t =
        match t with
        (* composite types that allow self or cyclic reference *)
        | DefT (_, FunT _)
        | DefT (_, ObjT _)
        | DefT (_, ArrT _)
        | DefT (_, InstanceT _)
        | DefT (_, ReactAbstractComponentT _)
        | DefT (_, RendersT _)
        | DefT (_, EnumValueT _)
        | DefT (_, EnumObjectT _)
        (* We give up on TypeApp, since it will defend against cyclic types separately. T110320325 *)
        | TypeAppT _
        | ThisTypeAppT _ ->
          acc
        (* EvalT(t, d, _) behaves like TypeAppT, not everything there will be unconditionally
         * evaluated, but if we do have the eval result, we should visit it, since at that point,
         * the EvalT behaves like an OpenT. *)
        | EvalT (_, _, id) ->
          (match Eval.Map.find_opt id (Context.evaluated cx) with
          | None -> acc
          | Some t -> this#type_ cx pole acc t)
        (* Base types *)
        | DefT (_, NumGeneralT _)
        | DefT (_, NumT_UNSOUND _)
        | DefT (_, StrGeneralT _)
        | DefT (_, StrT_UNSOUND _)
        | DefT (_, BoolGeneralT)
        | DefT (_, BoolT_UNSOUND _)
        | DefT (_, BigIntGeneralT _)
        | DefT (_, BigIntT_UNSOUND _)
        | DefT (_, EmptyT)
        | DefT (_, MixedT _)
        | DefT (_, NullT)
        | DefT (_, VoidT)
        | DefT (_, SymbolT)
        | DefT (_, SingletonStrT _)
        | DefT (_, NumericStrKeyT _)
        | DefT (_, SingletonNumT _)
        | DefT (_, SingletonBoolT _)
        | DefT (_, SingletonBigIntT _)
        | GenericT _
        | AnyT _
        | FunProtoT _
        | ObjProtoT _
        | NullProtoT _
        | FunProtoBindT _
        | StrUtilT _
        (* composite types that don't allow self or cyclic reference *)
        | OpenT _
        | DefT (_, (ClassT _ | TypeT (_, _) | PolyT _))
        | ThisInstanceT _
        | IntersectionT _
        | UnionT _
        | MaybeT _
        | OptionalT _
        | KeysT _
        | AnnotT _
        | OpaqueT _
        | NamespaceT _ ->
          super#type_ cx pole acc t
    end

  let get_fully_resolved_state cx r id = function
    | Type.Constraint.FullyResolved s -> s
    | Type.Constraint.Resolved t ->
      failwith
        (spf
           "tvar (%s, %d) = %s is resolved but not fully resolved"
           (dump_reason r)
           id
           (Debug_js.dump_t cx ~depth:3 t)
        )
    | Type.Constraint.Unresolved _ -> failwith (spf "tvar (%s, %d) is unresolved" (dump_reason r) id)

  let validate_local_type =
    let visitor =
      object (this)
        inherit tvar_forcing_visitor

        method! tvar cx pole (seen, in_lazy_tvar) r id =
          let (root_id, constraints) = Context.find_constraints cx id in
          let s = get_fully_resolved_state cx r id constraints in
          if
            (not (ISet.mem root_id seen))
            && (in_lazy_tvar || ISet.mem root_id (Context.delayed_forcing_tvars cx))
          then
            let (seen, _) =
              Context.force_fully_resolved_tvar cx s
              |> this#type_ cx pole (ISet.add root_id seen, true)
            in
            (seen, in_lazy_tvar)
          else
            (seen, in_lazy_tvar)
      end
    in
    fun cx t ->
      let (_ : ISet.t * bool) = visitor#type_ cx Polarity.Positive (ISet.empty, false) t in
      ()

  let validate_type_sig_type ~error_recursive =
    let visitor =
      object (this)
        inherit tvar_forcing_visitor

        method! tvar cx pole (seen, in_lazy_tvar) r id =
          let (root_id, constraints) = Context.find_constraints cx id in
          let s = get_fully_resolved_state cx r id constraints in
          if not (ISet.mem root_id seen) then
            Constraint.ForcingState.force ~on_error:(error_recursive cx) s
            |> this#type_ cx pole (ISet.add root_id seen, in_lazy_tvar)
          else
            (seen, in_lazy_tvar)
      end
    in
    fun src_cx t ->
      let (_ : ISet.t * bool) = visitor#type_ src_cx Polarity.Positive (ISet.empty, false) t in
      ()
end

let unwrap_fully_resolved_open_t cx t =
  let open Type in
  let open Reason in
  let unwrapped =
    match t with
    | OpenT (r, id) ->
      (match Context.find_graph cx id with
      | Type.Constraint.FullyResolved s -> Context.force_fully_resolved_tvar cx s
      | Type.Constraint.Resolved t ->
        failwith
          (Utils_js.spf
             "tvar (%s, %d) = %s is resolved but not fully resolved"
             (dump_reason r)
             id
             (Debug_js.dump_t cx ~depth:3 t)
          )
      | Type.Constraint.Unresolved _ ->
        failwith (Utils_js.spf "tvar (%s, %d) is unresolved" (dump_reason r) id))
    | t -> t
  in
  InvalidCyclicTypeValidation.validate_local_type cx unwrapped;
  unwrapped

let map_on_resolved_type cx reason_op l f =
  Tvar.mk_fully_resolved_lazy
    cx
    reason_op
    (lazy (Context.run_in_signature_tvar_env cx (fun () -> f l |> unwrap_fully_resolved_open_t cx)))

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

let update_lit_type_from_annot cx = function
  | DefT (r, SingletonStrT { from_annot = false; _ })
  | DefT (r, SingletonNumT { from_annot = false; _ })
  | DefT (r, SingletonBoolT { from_annot = false; _ })
  | DefT (r, SingletonBigIntT { from_annot = false; _ }) ->
    if Context.in_implicit_instantiation cx then
      Context.record_primitive_literal_check cx (Reason.loc_of_reason r)
  | _ -> ()

(* NOTE: The following function looks similar to TypeUtil.quick_subtype, but is in fact more
   complicated: it avoids deep structural checks, admits `any`, etc. It might be worth it to
   simplify this function later. *)
let ground_subtype cx (l, u) =
  match (l, u) with
  (* tvars are not considered ground, so they're not part of this relation *)
  | (OpenT _, _)
  | (_, UseT (_, OpenT _)) ->
    false
  | (UnionT _, _) -> false
  | ( DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)),
      UseT (_, DefT (_, (NumGeneralT _ | NumT_UNSOUND _)))
    )
  | ( DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)),
      UseT (_, DefT (_, (StrGeneralT _ | StrT_UNSOUND _)))
    )
  | ( DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _)),
      UseT (_, DefT (_, (BoolGeneralT | BoolT_UNSOUND _)))
    )
  | ( DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _)),
      UseT (_, DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _)))
    )
  | (DefT (_, SymbolT), UseT (_, DefT (_, SymbolT)))
  | (DefT (_, NullT), UseT (_, DefT (_, NullT)))
  | (DefT (_, VoidT), UseT (_, DefT (_, VoidT))) ->
    true
  | ( StrUtilT { reason = _; op = StrPrefix prefix1; remainder = _ },
      UseT (_, StrUtilT { reason = _; op = StrPrefix prefix2; remainder = None })
    )
    when String.starts_with ~prefix:prefix2 prefix1 ->
    true
  | ( DefT (_, (StrT_UNSOUND (None, OrdinaryName s) | SingletonStrT { value = OrdinaryName s; _ })),
      UseT (_, StrUtilT { reason = _; op = StrPrefix prefix; remainder = None })
    )
    when String.starts_with ~prefix s ->
    update_lit_type_from_annot cx l;
    true
  | ( StrUtilT { reason = _; op = StrSuffix suffix1; remainder = _ },
      UseT (_, StrUtilT { reason = _; op = StrSuffix suffix2; remainder = None })
    )
    when String.ends_with ~suffix:suffix2 suffix1 ->
    true
  | ( DefT (_, (StrT_UNSOUND (None, OrdinaryName s) | SingletonStrT { value = OrdinaryName s; _ })),
      UseT (_, StrUtilT { reason = _; op = StrSuffix suffix; remainder = None })
    )
    when String.ends_with ~suffix s ->
    update_lit_type_from_annot cx l;
    true
  | ( StrUtilT { reason = _; op = StrPrefix arg | StrSuffix arg; remainder = _ },
      UseT (_, DefT (_, StrGeneralT Truthy))
    )
    when arg <> "" ->
    true
  | (StrUtilT _, UseT (_, DefT (_, StrGeneralT AnyLiteral))) -> true
  | (l, UseT (_, DefT (_, MixedT mixed_flavor))) -> TypeUtil.is_mixed_subtype l mixed_flavor
  (* we handle the any propagation check later *)
  | (AnyT _, _) -> false
  | (_, UseT (_, AnyT _)) -> false
  (* opt: avoid builtin lookups *)
  | (ObjProtoT _, UseT (_, ObjProtoT _))
  | (FunProtoT _, UseT (_, FunProtoT _))
  | (FunProtoT _, UseT (_, ObjProtoT _))
  | (DefT (_, ObjT { proto_t = ObjProtoT _; _ }), UseT (_, ObjProtoT _))
  | (DefT (_, ObjT { proto_t = FunProtoT _; _ }), UseT (_, FunProtoT _))
  | (DefT (_, ObjT { proto_t = FunProtoT _; _ }), UseT (_, ObjProtoT _)) ->
    true
  | _ -> false

let is_date = function
  | DefT (reason, InstanceT _) -> DescFormat.name_of_instance_reason reason = "Date"
  | _ -> false

let function_like = function
  | DefT (_, ClassT _)
  | DefT (_, FunT _)
  | FunProtoBindT _ ->
    true
  | _ -> false

let object_like = function
  | DefT (_, (ObjT _ | InstanceT _))
  | ThisInstanceT _
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
  | GetProtoT _
  | SetProtoT _
  | SuperT _
  | GetKeysT _
  | HasOwnPropT _
  | GetValuesT _
  | GetDictValuesT _
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

let is_union_resolvable = function
  | EvalT _
  | KeysT _ ->
    true
  | _ -> false

module TvarVisitors : sig
  val has_placeholders : Context.t -> Type.t -> bool

  val has_unresolved_tvars_or_placeholders : Context.t -> Type.t -> bool

  val has_unresolved_tvars : Context.t -> Type.t -> bool

  val has_unresolved_tvars_in_destructors : Context.t -> Type.destructor -> bool
end = struct
  exception EncounteredPlaceholderType

  exception EncounteredUnresolvedTvar

  let has_placeholders =
    let visitor =
      object (this)
        inherit [ISet.t] Type_visitor.t as super

        method! type_ cx pole seen t =
          match t with
          | AnyT (_, Placeholder) -> raise EncounteredPlaceholderType
          | t -> super#type_ cx pole seen t

        method! tvar cx pole seen _r id =
          let module C = Type.Constraint in
          let (root_id, constraints) = Context.find_constraints cx id in
          if ISet.mem root_id seen then
            seen
          else
            let seen = ISet.add root_id seen in
            match constraints with
            | C.FullyResolved _ -> seen
            | C.Resolved t -> this#type_ cx pole seen t
            | C.Unresolved bounds ->
              TypeMap.fold (fun t _ seen -> this#type_ cx pole seen t) bounds.C.lower seen
      end
    in
    fun cx t ->
      match visitor#type_ cx Polarity.Positive ISet.empty t with
      | exception EncounteredPlaceholderType -> true
      | _ -> false

  class has_unresolved_tvars_visitor =
    object (this)
      inherit [ISet.t] Type_visitor.t

      method! tvar cx pole seen _r id =
        let module C = Type.Constraint in
        let (root_id, constraints) = Context.find_constraints cx id in
        if ISet.mem root_id seen then
          seen
        else
          let seen = ISet.add root_id seen in
          match constraints with
          | C.FullyResolved _ -> seen
          | C.Resolved t -> this#type_ cx pole seen t
          | C.Unresolved _ -> raise EncounteredUnresolvedTvar
    end

  let has_unresolved_tvars_or_placeholders_visitor =
    object
      inherit has_unresolved_tvars_visitor as super

      method! type_ cx pole seen t =
        match t with
        | AnyT (_, Placeholder) -> raise EncounteredPlaceholderType
        | t -> super#type_ cx pole seen t
    end

  let has_unresolved_tvars_visitor = new has_unresolved_tvars_visitor

  let has_unresolved_tvars_or_placeholders cx t =
    match has_unresolved_tvars_or_placeholders_visitor#type_ cx Polarity.Positive ISet.empty t with
    | exception EncounteredUnresolvedTvar -> true
    | exception EncounteredPlaceholderType -> true
    | _ -> false

  let has_unresolved_tvars cx t =
    match has_unresolved_tvars_visitor#type_ cx Polarity.Positive ISet.empty t with
    | exception EncounteredUnresolvedTvar -> true
    | _ -> false

  let has_unresolved_tvars_in_destructors cx d =
    match has_unresolved_tvars_visitor#destructor cx ISet.empty d with
    | exception EncounteredUnresolvedTvar -> true
    | _ -> false
end

(** Errors *)

let error_message_kind_of_lower = function
  | DefT (_, NullT) -> Some Error_message.Possibly_null
  | DefT (_, VoidT) -> Some Error_message.Possibly_void
  | MaybeT _ -> Some Error_message.Possibly_null_or_void
  | IntersectionT _
  | _ ->
    None

let error_message_kind_of_upper = function
  | GetPropT { propref = Named { reason; name; _ }; _ } ->
    Error_message.IncompatibleGetPropT (loc_of_reason reason, Some name)
  | GetPropT { propref = Computed t; _ } -> Error_message.IncompatibleGetPropT (loc_of_t t, None)
  | GetPrivatePropT (_, _, _, _, _, _) -> Error_message.IncompatibleGetPrivatePropT
  | SetPropT (_, _, Named { reason; name; _ }, _, _, _, _) ->
    Error_message.IncompatibleSetPropT (loc_of_reason reason, Some name)
  | SetPropT (_, _, Computed t, _, _, _, _) -> Error_message.IncompatibleSetPropT (loc_of_t t, None)
  | SetPrivatePropT (_, _, _, _, _, _, _, _, _) -> Error_message.IncompatibleSetPrivatePropT
  | MethodT (_, _, _, Named { reason; name; _ }, _) ->
    Error_message.IncompatibleMethodT (loc_of_reason reason, Some name)
  | MethodT (_, _, _, Computed t, _) -> Error_message.IncompatibleMethodT (loc_of_t t, None)
  | CallT _ -> Error_message.IncompatibleCallT
  | GetElemT { key_t; _ } -> Error_message.IncompatibleGetElemT (loc_of_t key_t)
  | SetElemT (_, _, t, _, _, _) -> Error_message.IncompatibleSetElemT (loc_of_t t)
  | CallElemT (_, _, _, t, _) -> Error_message.IncompatibleCallElemT (loc_of_t t)
  | ElemT { obj = DefT (_, ArrT _); _ } -> Error_message.IncompatibleElemTOfArrT
  | ObjRestT _ -> Error_message.IncompatibleObjRestT
  | ArrRestT _ -> Error_message.IncompatibleArrRestT
  | SuperT _ -> Error_message.IncompatibleSuperT
  | MixinT _ -> Error_message.IncompatibleMixinT
  | SpecializeT (Op (ClassExtendsCheck _), _, _, _, _) -> Error_message.IncompatibleSuperT
  | SpecializeT _ -> Error_message.IncompatibleSpecializeT
  | ConcretizeTypeAppsT _ -> Error_message.IncompatibleSpecializeT
  | ThisSpecializeT _ -> Error_message.IncompatibleThisSpecializeT
  | GetKeysT _ -> Error_message.IncompatibleGetKeysT
  | HasOwnPropT
      ( _,
        r,
        (DefT (_, StrT_UNSOUND (_, name)) | GenericT { bound = DefT (_, StrT_UNSOUND (_, name)); _ })
      ) ->
    Error_message.IncompatibleHasOwnPropT (loc_of_reason r, Some name)
  | HasOwnPropT (_, r, _) -> Error_message.IncompatibleHasOwnPropT (loc_of_reason r, None)
  | GetValuesT _ -> Error_message.IncompatibleGetValuesT
  | GetDictValuesT _ -> Error_message.IncompatibleGetValuesT
  | MapTypeT (_, _, ObjectKeyMirror, _) -> Error_message.IncompatibleMapTypeTObject
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

exception SpeculationSingletonError

(* [src_cx] is the context in which the error is created, and [dst_cx] the context
 * in which it is recorded. *)
let add_output_generic ~src_cx:cx ~dst_cx msg =
  if Speculation.speculating cx then
    if Error_message.defered_in_speculation msg then
      Speculation.defer_error cx msg
    else (
      if Context.is_verbose cx then
        prerr_endlinef "\nspeculative_error: %s" (Debug_js.dump_error_message cx msg);
      raise (SpeculativeError msg)
    )
  else (
    if Context.is_verbose cx then
      prerr_endlinef "\nadd_output: %s" (Debug_js.dump_error_message cx msg);

    let error = FlowError.error_of_msg ~source_file:(Context.file cx) msg in
    (* catch no-loc errors early, before they get into error map *)
    if
      Flow_error.loc_of_error error
      |> Base.Option.value_map ~default:false ~f:(fun loc -> ALoc.source loc = None)
    then
      assert_false (spf "add_output: no source for error: %s" (Debug_js.dump_error_message cx msg));

    Context.add_error dst_cx error
  )

let add_output cx msg = add_output_generic ~src_cx:cx ~dst_cx:cx msg

(* In annotation inference, errors are created in the exporting side (src_cx), and
 * are recorded in the importing one (dst_cx). *)
let add_annot_inference_error ~src_cx ~dst_cx msg : unit = add_output_generic ~src_cx ~dst_cx msg

let exact_obj_error cx obj_kind ~use_op ~exact_reason l =
  let error_kind =
    match obj_kind with
    | Indexed _ -> Flow_intermediate_error_types.UnexpectedIndexer
    | _ -> Flow_intermediate_error_types.UnexpectedInexact
  in
  let reasons = FlowError.ordered_reasons (reason_of_t l, exact_reason) in
  add_output cx (Error_message.EIncompatibleWithExact (reasons, use_op, error_kind))

(** Unions *)

module UnionOptimizationGuardResult = struct
  type t =
    | True
    | Maybe
    | False of { diff: UnionEnumSet.t }
end

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
      if UnionRep.same_source rep1 rep2 then
        UnionOptimizationGuardResult.True
      else if UnionRep.same_structure rep1 rep2 then
        UnionOptimizationGuardResult.True
      (* Try O(n) check, then O(n log n) check, then O(n^2) check *)
      else begin
        (* Only optimize for enums, since this is the only fast path examined below.
         * Note that optimizing both reps with [UnionRep.optimize] can potentially
         * cause a `RecursionCheck.LimitExceeded` exception. (`tests/typeapp_termination`
         * is a sanity check against that.) *)
        if not (UnionRep.is_optimized_finally rep1) then
          UnionRep.optimize_enum_only ~flatten:(Type_mapper.union_flatten cx) rep1;
        if not (UnionRep.is_optimized_finally rep2) then
          UnionRep.optimize_enum_only ~flatten:(Type_mapper.union_flatten cx) rep2;

        match (UnionRep.check_enum_with_tag rep1, UnionRep.check_enum_with_tag rep2) with
        | (Some (enums1, tag1), Some (enums2, tag2)) ->
          if UnionEnumSet.subset enums1 enums2 then
            UnionOptimizationGuardResult.True
          else if Base.Option.is_some tag1 && tag1 = tag2 then
            UnionOptimizationGuardResult.False { diff = UnionEnumSet.diff enums1 enums2 }
          else
            UnionOptimizationGuardResult.Maybe
        | (_, _) ->
          let unwrap rep = UnionRep.members rep |> Base.List.map ~f:(unwrap_type cx) in
          let lts = unwrap rep1 in
          let uts = unwrap rep2 in
          (* Pointwise subtyping check: O(N) *)
          if List.length lts = List.length uts && Base.List.for_all2_exn ~f:( = ) lts uts then
            UnionOptimizationGuardResult.True
          else if
            (* Check if u contains l after unwrapping annots, tvars and repos types.
               This is faster than the n^2 case below because it avoids flattening both
               unions *)
            let guard u =
              (not (TypeSet.mem u seen))
              && union_optimization_guard_impl (TypeSet.add u seen) cx comparator l u
                 = UnionOptimizationGuardResult.True
            in
            Base.List.exists ~f:guard uts
          then
            UnionOptimizationGuardResult.True
          else if
            (* `union_compare` is potentially very expensive as it flattens the
             * input unions. Do not perform this check on synthetic unions.
             * Instead, allow the unfolding of the LHS union which might enable
             * other optimized path. *)
            (not (UnionRep.is_synthetic rep1)) && union_compare cx comparator lts uts
          then
            UnionOptimizationGuardResult.True
          else
            UnionOptimizationGuardResult.Maybe
      end
    | _ -> UnionOptimizationGuardResult.Maybe
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

let iter_union :
      't.
      f:(Context.t -> Type.DepthTrace.t -> Type.t * Type.use_t -> 't) ->
      init:'t ->
      join:('t -> 't -> 't) ->
      Context.t ->
      Type.DepthTrace.t ->
      Type.UnionRep.t ->
      Type.use_t ->
      't =
 fun ~f ~init ~join cx trace rep u ->
  (* This is required so that our caches don't treat different branches of unions as the same type *)
  let union_reason i r = replace_desc_reason (RUnionBranching (desc_of_reason r, i)) r in
  UnionRep.members rep
  |> Base.List.foldi ~init ~f:(fun i acc b ->
         let r = (mod_reason_of_t (union_reason i) %> mk_tuple_swapped u %> f cx trace) b in
         join acc r
     )

let map_union ~f cx trace rep reason =
  UnionRep.members rep
  |> Base.List.map ~f:(fun t -> Tvar.mk_where cx (reason_of_t t) (fun tout -> f cx trace t tout))
  |> union_of_ts reason

let map_inter ~f cx trace rep reason =
  let ts =
    InterRep.members rep
    |> Base.List.map ~f:(fun t -> Tvar.mk_where cx (reason_of_t t) (fun tout -> f cx trace t tout))
  in
  match ts with
  (* If we have no types then this is an error. *)
  | [] -> DefT (reason, EmptyT)
  (* If we only have one type then only that should be used. *)
  | [t0] -> t0
  (* If we have more than one type then we make a union type. *)
  | t0 :: t1 :: ts -> IntersectionT (reason, InterRep.make t0 t1 ts)

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
  let param_loc = loc_of_reason param_reason in
  let bound = f bound in
  let id = Context.make_generic_id cx name param_loc in
  let bound =
    mod_reason_of_t
      (fun bound_reason ->
        let annot_loc = annot_loc_of_reason bound_reason in
        let desc = desc_of_reason ~unwrap:false bound_reason in
        opt_annot_reason ?annot_loc @@ mk_reason desc param_loc)
      bound
  in
  GenericT { reason = param_reason; name; id; bound; no_infer = false }

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

let poly_minimum_arity =
  let f n typeparam =
    if typeparam.default = None then
      n + 1
    else
      n
  in
  Nel.fold_left f 0

(** This typing *)
let default_this_type cx ~needs_this_param func =
  let { Flow_ast.Function.params; body; _ } = func in
  let reason = mk_reason (RImplicitThis (RFunction RNormal)) (fst params) in
  if Signature_utils.This_finder.missing_this_annotation ~needs_this_param body params then (
    add_output
      cx
      (Error_message.EMissingLocalAnnotation
         { reason; hint_available = false; from_generic_function = false }
      );
    AnyT.make (AnyError (Some MissingAnnotation)) reason
  ) else
    MixedT.make reason

(** Object Subtyping *)

let string_key s reason =
  let key_reason = replace_desc_reason (RPropertyIsAString s) reason in
  DefT (key_reason, StrT_UNSOUND (None, s))

(* common case checking a function as an object *)
let quick_error_fun_as_obj cx ~use_op reason statics reason_o props =
  let statics_own_props =
    match statics with
    | DefT (_, ObjT { props_tmap; _ }) -> Some (Context.find_props cx props_tmap)
    | AnyT _
    | DefT (_, MixedT _) ->
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
            | Field { type_ = OptionalT _; _ } -> true
            | _ -> false
          in
          not (optional || is_function_prototype x || NameUtils.Map.mem x statics_own_props))
        props
    in
    if NameUtils.Map.is_empty props_not_found then
      false
    else if NameUtils.Map.is_empty statics_own_props && not (NameUtils.Map.is_empty props) then (
      let error_message =
        Error_message.EIncompatibleWithUseOp
          {
            reason_lower = reason;
            reason_upper = reason_o;
            use_op;
            explanation = Some Flow_intermediate_error_types.ExplanationFunctionsWithStaticsToObject;
          }
      in
      add_output cx error_message;
      true
    ) else (
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
          add_output cx err)
        props_not_found;
      true
    )
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

let emit_cacheable_env_error cx loc err =
  let open Error_message in
  let err =
    match err with
    | Env_api.ReferencedBeforeDeclaration { name; def_loc } ->
      EBindingError (EReferencedBeforeDeclaration, loc, OrdinaryName name, def_loc)
    | Env_api.BuiltinNameLookupFailed name -> EBuiltinNameLookupFailed { loc; name }
  in
  add_output cx err

let lookup_builtin_module_error cx module_name loc =
  let module_name = Flow_import_specifier.display_userland module_name in
  let potential_generator =
    Context.missing_module_generators cx
    |> Base.List.find ~f:(fun (pattern, _) -> Str.string_match pattern module_name 0)
    |> Base.Option.map ~f:snd
  in
  add_output
    cx
    (Error_message.EBuiltinModuleLookupFailed { loc; potential_generator; name = module_name });
  AnyT.error_of_kind UnresolvedName (mk_reason RAnyImplicit loc)

let lookup_builtin_name_error name loc =
  Error
    ( AnyT.error_of_kind UnresolvedName (mk_reason RAnyImplicit loc),
      Nel.one (Env_api.BuiltinNameLookupFailed name)
    )

let lookup_builtin_value_result cx x reason =
  match Context.builtin_value_opt cx x with
  | Some (_, t) -> Ok (TypeUtil.mod_reason_of_t (Base.Fn.const reason) t)
  | None -> lookup_builtin_name_error x (loc_of_reason reason)

let lookup_builtin_type_result cx x reason =
  match Context.builtin_type_opt cx x with
  | Some (_, t) -> Ok (TypeUtil.mod_reason_of_t (Base.Fn.const reason) t)
  | None -> lookup_builtin_name_error x (loc_of_reason reason)

let (lookup_builtin_value, lookup_builtin_type) =
  let apply_errors cx reason = function
    | Ok t -> t
    | Error (t, errs) ->
      Nel.iter (emit_cacheable_env_error cx (loc_of_reason reason)) errs;
      t
  in
  let lookup_builtin_value cx x reason =
    lookup_builtin_value_result cx x reason |> apply_errors cx reason
  in
  let lookup_builtin_type cx x reason =
    lookup_builtin_type_result cx x reason |> apply_errors cx reason
  in
  (lookup_builtin_value, lookup_builtin_type)

let lookup_builtin_typeapp cx reason x targs =
  let t = lookup_builtin_type cx x reason in
  typeapp ~from_value:false ~use_desc:false reason t targs

let builtin_promise_class_id cx =
  match Context.builtin_value_opt cx "Promise" with
  | Some (_, OpenT (_, id)) ->
    let (_, constraints) = Context.find_constraints cx id in
    begin
      match constraints with
      | Constraint.FullyResolved s ->
        (match Context.force_fully_resolved_tvar cx s with
        | DefT
            ( _,
              PolyT
                {
                  t_out = DefT (_, ClassT (ThisInstanceT (_, { inst = { class_id; _ }; _ }, _, _)));
                  _;
                }
            ) ->
          Some class_id
        | _ -> None)
      | _ -> None
    end
  | _ -> None

let is_builtin_class_id class_ref class_id cx =
  match Context.builtin_type_opt cx class_ref with
  | Some (_, OpenT (_, id)) ->
    let (_, constraints) = Context.find_constraints cx id in
    (match constraints with
    | Constraint.FullyResolved s ->
      (match Context.force_fully_resolved_tvar cx s with
      | DefT
          ( _,
            ( PolyT
                {
                  t_out =
                    DefT
                      ( _,
                        ClassT
                          ( DefT (_, InstanceT { inst = { class_id = ref_class_id; _ }; _ })
                          | ThisInstanceT (_, { inst = { class_id = ref_class_id; _ }; _ }, _, _) )
                      );
                  _;
                }
            | ClassT
                ( DefT (_, InstanceT { inst = { class_id = ref_class_id; _ }; _ })
                | ThisInstanceT (_, { inst = { class_id = ref_class_id; _ }; _ }, _, _) ) )
          ) ->
        ALoc.equal_id class_id ref_class_id
      | _ -> false)
    | _ -> false)
  | _ -> false

let is_builtin_iterable_class_id class_id cx = is_builtin_class_id "$Iterable" class_id cx

let builtin_react_element_opaque_id cx =
  match Context.builtin_type_opt cx "React$Element" with
  | Some (_, OpenT (_, id)) ->
    let (_, constraints) = Context.find_constraints cx id in
    begin
      match constraints with
      | Constraint.FullyResolved s ->
        (match Context.force_fully_resolved_tvar cx s with
        | DefT (_, PolyT { t_out = DefT (_, TypeT (OpaqueKind, OpaqueT (_, { opaque_id; _ }))); _ })
          ->
          Some opaque_id
        | _ -> None)
      | _ -> None
    end
  | _ -> None

let builtin_react_renders_exactly_opaque_id cx =
  match Context.builtin_type_opt cx "React$RendersExactly" with
  | Some (_, OpenT (_, id)) ->
    let (_, constraints) = Context.find_constraints cx id in
    begin
      match constraints with
      | Constraint.FullyResolved s ->
        (match Context.force_fully_resolved_tvar cx s with
        | DefT (_, PolyT { t_out = DefT (_, TypeT (OpaqueKind, OpaqueT (_, { opaque_id; _ }))); _ })
          ->
          Some opaque_id
        | _ -> None)
      | _ -> None
    end
  | _ -> None

let enum_proto cx ~reason ~enum_object_t ~enum_value_t ~representation_t =
  lookup_builtin_typeapp cx reason "$EnumProto" [enum_object_t; enum_value_t; representation_t]

(**
 * Determines whether a property name should be considered "munged"/private when
 * the `munge_underscores` config option is set.
 *)
let is_munged_prop_name_with_munge name ~should_munge_underscores =
  Signature_utils.is_munged_property_name name && should_munge_underscores

let is_munged_prop_name cx name =
  is_munged_prop_name_with_munge name ~should_munge_underscores:(Context.should_munge_underscores cx)

let obj_key_mirror cx o reason_op =
  let map_t key t =
    match t with
    | OptionalT _ -> optional key
    | _ -> key
  in
  let map_field key t =
    let reason = replace_desc_reason (RStringLit key) reason_op in
    map_t (DefT (reason, SingletonStrT { from_annot = true; value = key })) t
  in
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
  DefT (reason, ObjT { o with props_tmap; flags })

let namespace_type cx reason namespace_symbol values types =
  let add name { preferred_def_locs; name_loc; type_ } acc =
    NameUtils.Map.add
      name
      (Field { preferred_def_locs; key_loc = name_loc; type_; polarity = Polarity.Positive })
      acc
  in
  let props = NameUtils.Map.fold add values NameUtils.Map.empty in
  let proto = ObjProtoT reason in
  let values_type = Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~props proto in
  let types_tmap =
    Context.generate_property_map cx (NameUtils.Map.fold add types NameUtils.Map.empty)
  in
  NamespaceT { namespace_symbol; values_type; types_tmap }

let obj_is_readonlyish { Type.react_dro; _ } = Base.Option.is_some react_dro

let is_exception_to_react_dro = function
  | Named { name = OrdinaryName "current"; _ } -> true
  | _ -> false

(* Fix a this-abstracted instance type by tying a "knot": assume that the
   fixpoint is some `this`, substitute it as This in the instance type, and
   finally return the result as `this`. *)
let fix_this_instance cx reason (reason_i, i, is_this, this_name) =
  let cache_key = DefT (reason_i, InstanceT i) in
  match Flow_cache.Fix.find cx is_this cache_key with
  | Some i' -> i'
  | None ->
    let rec i' =
      lazy
        (let this = Tvar.mk_fully_resolved_lazy cx reason_i i' in
         let this_generic =
           if is_this then
             GenericT
               {
                 id = Context.make_generic_id cx this_name (def_loc_of_reason reason_i);
                 reason;
                 name = this_name;
                 bound = this;
                 no_infer = false;
               }
           else
             this
         in
         let i' =
           DefT
             ( reason_i,
               InstanceT (subst_instance_type cx (Subst_name.Map.singleton this_name this_generic) i)
             )
         in
         Flow_cache.Fix.add cx is_this cache_key i';
         i'
        )
    in
    Lazy.force i'

module type Instantiation_helper_sig = sig
  val reposition : Context.t -> ?trace:Type.DepthTrace.t -> ALoc.t -> Type.t -> Type.t

  val is_subtype : Context.t -> Type.DepthTrace.t -> use_op:use_op -> Type.t * Type.t -> unit

  val unify : Context.t -> Type.DepthTrace.t -> use_op:use_op -> Type.t * Type.t -> unit

  val mk_targ : Context.t -> Type.typeparam -> Reason.t -> Reason.t -> Type.t
end

module Instantiation_kit (H : Instantiation_helper_sig) = struct
  (* Instantiate a polymorphic definition given type arguments. *)
  let instantiate_poly_with_targs
      cx
      trace
      ~use_op
      ~reason_op
      ~reason_tapp
      ?errs_ref
      ?(unify_bounds = false)
      (tparams_loc, xs, t)
      ts =
    let minimum_arity = poly_minimum_arity xs in
    let maximum_arity = Nel.length xs in
    let arity_loc = tparams_loc in
    if List.length ts > maximum_arity then (
      add_output cx (Error_message.ETooManyTypeArgs { reason_tapp; arity_loc; maximum_arity });
      Base.Option.iter errs_ref ~f:(fun errs_ref ->
          errs_ref := Context.ETooManyTypeArgs (arity_loc, maximum_arity) :: !errs_ref
      )
    );
    let (map, _, all_ts_rev) =
      Nel.fold_left
        (fun (map, ts, all_ts) typeparam ->
          let (t, ts, all_ts) =
            match (typeparam, ts) with
            | ({ default = Some default; _ }, []) ->
              (* fewer arguments than params and we have a default *)
              (subst cx ~use_op map default, [], (default, typeparam.name) :: all_ts)
            | ({ default = None; _ }, []) ->
              (* fewer arguments than params but no default *)
              add_output cx (Error_message.ETooFewTypeArgs { reason_tapp; arity_loc; minimum_arity });
              Base.Option.iter errs_ref ~f:(fun errs_ref ->
                  errs_ref := Context.ETooFewTypeArgs (arity_loc, minimum_arity) :: !errs_ref
              );
              (AnyT (reason_op, AnyError None), [], all_ts)
            | (_, t :: ts) -> (t, ts, (t, typeparam.name) :: all_ts)
          in
          let frame = Frame (TypeParamBound { name = typeparam.name }, use_op) in
          if not (Context.in_implicit_instantiation cx) then
            if unify_bounds then
              H.unify cx trace ~use_op:frame (t, subst cx ~use_op map typeparam.bound)
            else
              H.is_subtype cx trace ~use_op:frame (t, subst cx ~use_op map typeparam.bound);
          (Subst_name.Map.add typeparam.name t map, ts, all_ts))
        (Subst_name.Map.empty, ts, [])
        xs
    in
    ( H.reposition
        cx
        ~trace
        (loc_of_reason reason_tapp)
        (subst cx ~use_op ~placeholder_no_infer:(Context.in_implicit_instantiation cx) map t),
      all_ts_rev
    )

  let mk_typeapp_of_poly cx trace ~use_op ~reason_op ~reason_tapp id tparams_loc xs t ts =
    let key = (id, ts) in
    let cache = Context.subst_cache cx in
    match Type.SubstCacheMap.find_opt key !cache with
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
        |> fst
      in
      cache := Type.SubstCacheMap.add key (!errs_ref, t) !cache;
      t
    | Some (errs, t) ->
      errs
      |> List.iter (function
             | Context.ETooManyTypeArgs (arity_loc, maximum_arity) ->
               let msg = Error_message.ETooManyTypeArgs { reason_tapp; arity_loc; maximum_arity } in
               add_output cx msg
             | Context.ETooFewTypeArgs (arity_loc, minimum_arity) ->
               let msg = Error_message.ETooFewTypeArgs { reason_tapp; arity_loc; minimum_arity } in
               add_output cx msg
             );
      t

  (* Instantiate a polymorphic definition by creating fresh type arguments. *)
  let instantiate_poly
      cx trace ~use_op ~reason_op ~reason_tapp ?(unify_bounds = false) (tparams_loc, xs, t) =
    let ts = xs |> Nel.map (fun typeparam -> H.mk_targ cx typeparam reason_op reason_tapp) in
    instantiate_poly_with_targs
      cx
      trace
      ~use_op
      ~reason_op
      ~reason_tapp
      ~unify_bounds
      (tparams_loc, xs, t)
      (Nel.to_list ts)
end

let mk_distributive_tparam_subst_fn cx ~use_op name distributed_t =
  let distributed_t =
    match distributed_t with
    | OpenT _ -> distributed_t
    | _ when not (Subst_name.Set.is_empty (free_var_finder cx distributed_t)) -> distributed_t
    | _ ->
      if TvarVisitors.has_unresolved_tvars cx distributed_t then (
        let r = TypeUtil.reason_of_t distributed_t in
        let tvar_id = Reason.mk_id () in
        Context.add_tvar
          cx
          tvar_id
          (Type.Constraint.create_root
             (Unresolved
                {
                  lower = TypeMap.singleton distributed_t (DepthTrace.dummy_trace, unknown_use);
                  upper = UseTypeMap.empty;
                  lowertvars = IMap.empty;
                  uppertvars = IMap.empty;
                }
             )
          );
        OpenT (r, tvar_id)
      ) else
        Tvar.mk_fully_resolved cx (TypeUtil.reason_of_t distributed_t) distributed_t
  in
  subst cx ~use_op (Subst_name.Map.singleton name distributed_t)

let substitute_mapped_type_distributive_tparams
    cx ~use_op distributive_tparam_name ~property_type homomorphic ~source =
  match distributive_tparam_name with
  | None -> (property_type, homomorphic)
  | Some name ->
    let subst = mk_distributive_tparam_subst_fn cx ~use_op name source in
    let homomorphic' =
      match homomorphic with
      | SemiHomomorphic t -> SemiHomomorphic (subst t)
      | Homomorphic
      | Unspecialized ->
        homomorphic
    in
    (subst property_type, homomorphic')

module ValueToTypeReferenceTransform = struct
  (* a component syntax value annotation becomes React$RendersExactly of that component *)
  let run_on_abstract_component cx reason_component reason_op l =
    let elem_reason =
      let desc = react_element_desc_of_component_reason reason_component in
      let annot_loc = loc_of_reason reason_op in
      annot_reason ~annot_loc (replace_desc_reason desc reason_op)
    in
    let t =
      Tvar.mk_fully_resolved
        cx
        elem_reason
        (lookup_builtin_type cx "React$RendersExactly" elem_reason)
    in
    TypeUtil.typeapp ~from_value:false ~use_desc:true elem_reason t [l]

  let run_on_concrete_type cx ~use_op reason_op kind = function
    | DefT
        ( _,
          PolyT
            {
              tparams_loc = _;
              tparams = ids;
              t_out = DefT (reason_component, ReactAbstractComponentT _) as t;
              _;
            }
        )
      when kind = RenderTypeKind ->
      let subst_map =
        Nel.fold_left
          (fun acc tparam -> Subst_name.Map.add tparam.name (AnyT.untyped reason_op) acc)
          Subst_name.Map.empty
          ids
      in
      let t = subst cx ~use_op subst_map t in
      run_on_abstract_component cx reason_component reason_op t
    | DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; _ }) ->
      add_output
        cx
        (Error_message.EMissingTypeArgs
           {
             reason_op;
             reason_tapp;
             arity_loc = tparams_loc;
             min_arity = poly_minimum_arity ids;
             max_arity = Nel.length ids;
           }
        );
      AnyT.error reason_tapp
    | DefT (_, ClassT it) ->
      (* a class value annotation becomes the instance type *)
      (match it with
      (* when a this-abstracted class flows to upper bounds, fix the class *)
      | ThisInstanceT (inst_r, i, this, this_name) ->
        fix_this_instance cx reason_op (inst_r, i, this, this_name)
      | _ -> it)
    | DefT (_, TypeT (_, t)) -> t
    | DefT (_, EnumObjectT { enum_value_t; _ }) ->
      (* an enum object value annotation becomes the enum type *)
      enum_value_t
    | DefT (enum_reason, EnumValueT _) ->
      add_output cx Error_message.(EEnumMemberUsedAsType { reason = reason_op; enum_reason });
      AnyT.error reason_op
    | DefT (reason_component, ReactAbstractComponentT _) as l ->
      run_on_abstract_component cx reason_component reason_op l
    | DefT (r, EmptyT)
    | AnyT (r, AnyError (Some MissingAnnotation)) ->
      add_output cx Error_message.(EValueUsedAsType { reason_use = reason_op });
      AnyT.error r
    | AnyT (_, AnyError _) as l ->
      (* Short-circut as we already error on the unresolved name. *)
      l
    | AnyT (r, _) ->
      add_output cx Error_message.(EAnyValueUsedAsType { reason_use = reason_op });
      AnyT.error r
    | t ->
      add_output cx Error_message.(EValueUsedAsType { reason_use = reason_op });
      AnyT.error (TypeUtil.reason_of_t t)
end

(***********)
(* Imports *)
(***********)

let check_nonstrict_import cx is_strict imported_is_strict reason =
  if is_strict && not imported_is_strict then
    let loc = Reason.loc_of_reason reason in
    let message = Error_message.ENonstrictImport loc in
    add_output cx message

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

  val reposition : Context.t -> ALoc.t -> Type.t -> Type.t

  val export_named :
    Context.t ->
    Reason.t * Type.named_symbol NameUtils.Map.t * Type.named_symbol NameUtils.Map.t * export_kind ->
    Type.t ->
    r

  val return : Context.t -> Type.t -> r
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
module ImportTypeTKit = struct
  let canonicalize_imported_type cx reason t =
    match t with
    (* fix this-abstracted class when used as a type *)
    | DefT (_, ClassT (ThisInstanceT (r, i, this, this_name))) ->
      Some (DefT (reason, ClassT (fix_this_instance cx reason (r, i, this, this_name))))
    | DefT (_, ClassT inst) -> Some (DefT (reason, TypeT (ImportClassKind, inst)))
    (* delay fixing a polymorphic this-abstracted class until it is specialized,
       by transforming the instance type to a type application *)
    | DefT
        ( _,
          PolyT { tparams_loc; tparams = typeparams; t_out = DefT (_, ClassT (ThisInstanceT _)); _ }
        ) ->
      let (_, targs) = typeparams |> Nel.to_list |> mk_tparams cx in
      let tapp = implicit_typeapp t targs in
      Some (poly_type (Type.Poly.generate_id ()) tparams_loc typeparams (class_type tapp))
    | DefT (_, PolyT { tparams_loc; tparams = typeparams; t_out = DefT (_, ClassT inst); id }) ->
      Some (poly_type id tparams_loc typeparams (DefT (reason, TypeT (ImportClassKind, inst))))
    | DefT (_, PolyT { t_out = DefT (_, TypeT _); _ }) -> Some t
    | DefT (_, EnumObjectT { enum_value_t; _ }) ->
      Some (DefT (reason, TypeT (ImportEnumKind, enum_value_t)))
    | DefT (_, ReactAbstractComponentT _) -> Some t
    | NamespaceT _ -> Some t
    | DefT (_, PolyT { t_out = DefT (_, ReactAbstractComponentT _); _ }) -> Some t
    | DefT (_, TypeT _) -> Some t
    | AnyT _ -> Some t
    | _ -> None

  let on_concrete_type cx reason export_name exported_type =
    match (exported_type, export_name) with
    | (DefT (_, ObjT _), "default") -> exported_type
    | (exported_type, _) ->
      (match canonicalize_imported_type cx reason exported_type with
      | Some imported_t -> imported_t
      | None ->
        add_output cx (Error_message.EImportValueAsType (reason, export_name));
        AnyT.error reason)
end

(************************************************************************)
(* `import typeof` creates a properly-parameterized type alias for the  *)
(* "typeof" the remote export.                                          *)
(************************************************************************)

module ImportTypeofTKit = struct
  let on_concrete_type cx reason export_name l =
    match l with
    | DefT
        ( _,
          PolyT { tparams_loc = _; tparams = _; t_out = DefT (_, ClassT (ThisInstanceT _)); id = _ }
        ) ->
      let typeof_t = TypeUtil.typeof_annotation reason l None in
      DefT (reason, TypeT (ImportTypeofKind, typeof_t))
    | DefT
        ( _,
          PolyT
            {
              tparams_loc;
              tparams = typeparams;
              t_out = DefT (_, (ClassT _ | FunT _ | ReactAbstractComponentT _)) as lower_t;
              id = _;
            }
        ) ->
      let typeof_t = TypeUtil.typeof_annotation reason lower_t None in

      poly_type
        (Poly.generate_id ())
        tparams_loc
        typeparams
        (DefT (reason, TypeT (ImportTypeofKind, typeof_t)))
    | DefT (_, TypeT _)
    | DefT (_, PolyT { t_out = DefT (_, TypeT _); _ }) ->
      add_output cx (Error_message.EImportTypeAsTypeof (reason, export_name));
      AnyT.error reason
    | _ ->
      let typeof_t = TypeUtil.typeof_annotation reason l None in
      DefT (reason, TypeT (ImportTypeofKind, typeof_t))
end

module CJSRequireTKit = struct
  (* require('SomeModule') *)
  let on_ModuleT cx ~reposition ~reason ~module_symbol ~is_strict ~standard_cjs_esm_interop module_
      =
    let {
      module_reason;
      module_export_types = exports;
      module_is_strict = imported_is_strict;
      module_available_platforms = _IGNORED_TODO;
    } =
      module_
    in
    check_nonstrict_import cx is_strict imported_is_strict reason;
    match exports.cjs_export with
    | Some (def_loc_opt, t) ->
      (* reposition the export to point at the require(), like the object
         we create below for non-CommonJS exports *)
      let def_loc =
        match def_loc_opt with
        | None -> def_loc_of_t t
        | Some l -> l
      in
      (reposition cx (loc_of_reason reason) t, def_loc)
    | None ->
      let value_exports_tmap = Context.find_exports cx exports.value_exports_tmap in
      let type_exports_tmap = Context.find_exports cx exports.type_exports_tmap in
      (* Convert ES module's named exports to an object *)
      let mk_exports_namespace () =
        let proto = ObjProtoT reason in
        let named_symbol_to_field { preferred_def_locs; name_loc; type_ } =
          Field { preferred_def_locs; key_loc = name_loc; type_; polarity = Polarity.Positive }
        in
        let value_props = NameUtils.Map.map named_symbol_to_field value_exports_tmap in
        let type_props = NameUtils.Map.map named_symbol_to_field type_exports_tmap in
        let values_type =
          Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~props:value_props proto
        in
        let types_tmap = Context.generate_property_map cx type_props in
        NamespaceT { namespace_symbol = module_symbol; values_type; types_tmap }
      in
      let t =
        if standard_cjs_esm_interop then
          lookup_builtin_typeapp
            cx
            reason
            "$Flow$EsmModuleMarkerWrapperInModuleRef"
            [mk_exports_namespace ()]
        else
          (* Use default export if option is enabled and module is not lib *)
          let automatic_require_default =
            Context.automatic_require_default cx && not (is_lib_reason_def module_reason)
          in
          if automatic_require_default then
            match NameUtils.Map.find_opt (OrdinaryName "default") value_exports_tmap with
            | Some { preferred_def_locs = _; name_loc = _; type_ } -> type_
            | _ -> mk_exports_namespace ()
          else
            mk_exports_namespace ()
      in
      let def_loc =
        let def_loc_of_export { preferred_def_locs; name_loc; type_ } =
          match preferred_def_locs with
          | Some l -> Nel.hd l
          | None ->
            (match name_loc with
            | Some l -> l
            | None -> def_loc_of_t type_)
        in
        match NameUtils.Map.find_opt (OrdinaryName "default") value_exports_tmap with
        | Some e -> def_loc_of_export e
        | None ->
          (match
             NameUtils.Map.fold
               (fun _ e acc ->
                 match acc with
                 | None -> Some (def_loc_of_export e)
                 | Some acc ->
                   let def_loc = def_loc_of_export e in
                   if ALoc.compare acc def_loc < 0 then
                     Some acc
                   else
                     Some def_loc)
               value_exports_tmap
               None
           with
          | Some l -> l
          | None -> def_loc_of_reason module_reason)
      in
      (t, def_loc)
end

(* import * as X from 'SomeModule'; *)
module ImportModuleNsTKit = struct
  let on_ModuleT cx ?(is_common_interface_module = false) (reason_op, is_strict) module_ =
    let {
      module_reason;
      module_export_types = exports;
      module_is_strict = imported_is_strict;
      module_available_platforms = _IGNORED_TODO;
    } =
      module_
    in
    if not is_common_interface_module then
      check_nonstrict_import cx is_strict imported_is_strict reason_op;
    let reason =
      module_reason
      |> Reason.repos_reason (loc_of_reason reason_op)
      |> Reason.replace_desc_reason (desc_of_reason reason_op)
    in
    let value_exports_tmap = Context.find_exports cx exports.value_exports_tmap in
    let type_exports_tmap = Context.find_exports cx exports.type_exports_tmap in
    let named_symbol_to_field { preferred_def_locs; name_loc; type_ } =
      Field { preferred_def_locs; key_loc = name_loc; type_; polarity = Polarity.Positive }
    in
    let value_props = NameUtils.Map.map named_symbol_to_field value_exports_tmap in
    let type_props = NameUtils.Map.map named_symbol_to_field type_exports_tmap in
    let value_props =
      if Context.facebook_module_interop cx then
        value_props
      else
        match exports.cjs_export with
        | Some (def_loc_opt, type_) ->
          let key_loc =
            Some
              (match def_loc_opt with
              | None -> def_loc_of_t type_
              | Some l -> l)
          in
          let p =
            Field { preferred_def_locs = None; key_loc; type_; polarity = Polarity.Positive }
          in
          NameUtils.Map.add (OrdinaryName "default") p value_props
        | None -> value_props
    in
    let obj_kind =
      if exports.has_every_named_export then
        Indexed
          {
            key = StrModuleT.why reason;
            value = AnyT.untyped reason;
            dict_name = None;
            dict_polarity = Polarity.Neutral;
          }
      else if is_common_interface_module then
        Inexact
      else
        Exact
    in
    let proto = ObjProtoT reason in
    let values_type = Obj_type.mk_with_proto cx reason ~obj_kind ~props:value_props proto in
    let types_tmap = Context.generate_property_map cx type_props in
    (values_type, types_tmap)
end

module ImportDefaultTKit = struct
  (* import [type] X from 'SomeModule'; *)
  let on_ModuleT
      cx ~with_concretized_type (reason, import_kind, (local_name, module_name), is_strict) module_
      =
    let {
      module_reason;
      module_export_types = exports;
      module_is_strict = imported_is_strict;
      module_available_platforms = _IGNORED_TODO;
    } =
      module_
    in
    check_nonstrict_import cx is_strict imported_is_strict reason;
    let (loc_opt, export_t) =
      match exports.cjs_export with
      | Some (def_loc_opt, t) ->
        let def_loc =
          Some
            (match def_loc_opt with
            | None -> def_loc_of_t t
            | Some l -> l)
        in
        (def_loc, t)
      | None ->
        let exports_tmap = Context.find_exports cx exports.value_exports_tmap in
        (match NameUtils.Map.find_opt (OrdinaryName "default") exports_tmap with
        | Some { preferred_def_locs = _; name_loc; type_ } -> (name_loc, type_)
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
          add_output cx (Error_message.ENoDefaultExport (reason, module_name, suggestion));
          (None, AnyT.error module_reason))
    in
    match import_kind with
    | ImportType ->
      ( loc_opt,
        with_concretized_type
          cx
          reason
          (ImportTypeTKit.on_concrete_type cx reason "default")
          export_t
      )
    | ImportTypeof ->
      ( loc_opt,
        with_concretized_type
          cx
          reason
          (ImportTypeofTKit.on_concrete_type cx reason "default")
          export_t
      )
    | ImportValue -> (loc_opt, export_t)
end

module ImportNamedTKit = struct
  (* import {X} from 'SomeModule'; *)
  let on_ModuleT
      cx ~with_concretized_type (reason, import_kind, export_name, module_name, is_strict) module_ =
    let {
      module_reason = _;
      module_export_types = exports;
      module_is_strict = imported_is_strict;
      module_available_platforms = _IGNORED_TODO;
    } =
      module_
    in
    check_nonstrict_import cx is_strict imported_is_strict reason;
    (*
     * When importing from a CommonJS module, we shadow any potential named
     * exports called "default" with a pointer to the raw `module.exports`
     * object
     *)
    let value_exports_tmap =
      let value_exports_tmap = Context.find_exports cx exports.value_exports_tmap in
      match exports.cjs_export with
      | Some (def_loc_opt, type_) ->
        let name_loc =
          Some
            (match def_loc_opt with
            | None -> def_loc_of_t type_
            | Some l -> l)
        in
        NameUtils.Map.add
          (OrdinaryName "default")
          { preferred_def_locs = None; name_loc; type_ }
          value_exports_tmap
      | None -> value_exports_tmap
    in
    let type_exports_tmap = Context.find_exports cx exports.type_exports_tmap in
    let has_every_named_export = exports.has_every_named_export in
    let exported_symbol_opt =
      match import_kind with
      | ImportValue -> NameUtils.Map.find_opt (OrdinaryName export_name) value_exports_tmap
      | ImportType
      | ImportTypeof ->
        (match NameUtils.Map.find_opt (OrdinaryName export_name) type_exports_tmap with
        | Some _ as s -> s
        | None -> NameUtils.Map.find_opt (OrdinaryName export_name) value_exports_tmap)
    in
    match (import_kind, exported_symbol_opt) with
    | (ImportType, Some { preferred_def_locs = _; name_loc; type_ }) ->
      ( name_loc,
        with_concretized_type
          cx
          reason
          (ImportTypeTKit.on_concrete_type cx reason export_name)
          type_
      )
    | (ImportType, None) when has_every_named_export ->
      ( None,
        with_concretized_type
          cx
          reason
          (ImportTypeTKit.on_concrete_type cx reason export_name)
          (AnyT.untyped reason)
      )
    | (ImportTypeof, Some { preferred_def_locs = _; name_loc; type_ }) ->
      ( name_loc,
        with_concretized_type
          cx
          reason
          (ImportTypeofTKit.on_concrete_type cx reason export_name)
          type_
      )
    | (ImportTypeof, None) when has_every_named_export ->
      ( None,
        with_concretized_type
          cx
          reason
          (ImportTypeofTKit.on_concrete_type cx reason export_name)
          (AnyT.untyped reason)
      )
    | (ImportValue, Some { preferred_def_locs = _; name_loc; type_ }) -> (name_loc, type_)
    | (ImportValue, None) when has_every_named_export ->
      let t = AnyT.untyped reason in
      (None, t)
    | (ImportValue, None) when NameUtils.Map.mem (OrdinaryName export_name) type_exports_tmap ->
      add_output cx (Error_message.EImportTypeAsValue (reason, export_name));
      (None, AnyT.error reason)
    | (_, None) ->
      let exports_tmap =
        match import_kind with
        | ImportValue -> value_exports_tmap
        | ImportType
        | ImportTypeof ->
          NameUtils.Map.union value_exports_tmap type_exports_tmap
      in
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
      add_output cx msg;
      (None, AnyT.error reason)
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
module ExportNamedTKit = struct
  let mod_ModuleT cx (value_tmap, type_tmap, export_kind) module_ =
    let {
      module_reason = _;
      module_export_types = { value_exports_tmap; type_exports_tmap; _ };
      module_is_strict = _;
      module_available_platforms = _;
    } =
      module_
    in
    let add_export name export acc =
      let export' =
        match export_kind with
        | DirectExport -> export
        | ReExport ->
          (* Re-exports do not overwrite named exports from the local module. *)
          NameUtils.Map.find_opt name acc |> Base.Option.value ~default:export
      in
      NameUtils.Map.add name export' acc
    in
    Context.find_exports cx value_exports_tmap
    |> NameUtils.Map.fold add_export value_tmap
    |> Context.add_export_map cx value_exports_tmap;
    Context.find_exports cx type_exports_tmap
    |> NameUtils.Map.fold add_export type_tmap
    |> Context.add_export_map cx type_exports_tmap
end

module AssertExportIsTypeTKit : sig
  val on_concrete_type : cx -> name -> Type.t -> Type.t
end = struct
  let rec is_type = function
    | DefT (_, ClassT _)
    | DefT (_, EnumObjectT _)
    | DefT (_, TypeT _)
    | NamespaceT _
    | AnyT _ ->
      true
    | DefT (_, PolyT { t_out = t'; _ }) -> is_type t'
    | _ -> false

  let on_concrete_type cx name l =
    if is_type l then
      l
    else
      let reason = reason_of_t l in
      add_output cx Error_message.(EExportValueAsType (reason, name));
      AnyT.error reason
end

(* Copy the named exports from a source module into a target module. Used
   to implement `export * from 'SomeModule'`, with the current module as
   the target and the imported module as the source. *)
module CopyNamedExportsTKit = struct
  let mod_ModuleT cx ~target_module_type module_ =
    let {
      module_reason = _;
      module_export_types = source_exports;
      module_is_strict = _;
      module_available_platforms = _;
    } =
      module_
    in
    ExportNamedTKit.mod_ModuleT
      cx
      ( Context.find_exports cx source_exports.value_exports_tmap,
        Context.find_exports cx source_exports.type_exports_tmap,
        ReExport
      )
      target_module_type
end

(* Export a type from a given ModuleT, but only if the type is compatible
 * with `import type`/`export type`. When it is not compatible, it is simply
 * not added to the exports map.
 *
 * Note that this is very similar to `ExportNamedT` except that it only
 * exports one type at a time and it takes the type to be exported as a
 * lower (so that the type can be filtered post-resolution). *)
module ExportTypeTKit = struct
  let on_concrete_type
      cx reason { preferred_def_locs; name_loc; type_ = l } (export_name, target_module_type) =
    let is_type_export =
      match l with
      | DefT (_, ObjT _) when export_name = OrdinaryName "default" -> true
      | l -> ImportTypeTKit.canonicalize_imported_type cx reason l <> None
    in
    if is_type_export then
      let named = NameUtils.Map.singleton export_name { preferred_def_locs; name_loc; type_ = l } in
      ExportNamedTKit.mod_ModuleT cx (NameUtils.Map.empty, named, ReExport) target_module_type
end

(* Copy only the type exports from a source module into a target module.
 * Used to implement `export type * from ...`. *)
module CopyTypeExportsTKit = struct
  let mod_ModuleT cx ~concretize_export_type (reason, target_module_type) module_ =
    let {
      module_reason = _;
      module_export_types = source_exports;
      module_is_strict = _;
      module_available_platforms = _;
    } =
      module_
    in
    let export_all exports_tmap =
      NameUtils.Map.iter
        (fun export_name { name_loc; preferred_def_locs; type_ } ->
          let type_ = concretize_export_type cx reason type_ in
          ExportTypeTKit.on_concrete_type
            cx
            reason
            { name_loc; preferred_def_locs; type_ }
            (export_name, target_module_type))
        (Context.find_exports cx exports_tmap)
    in
    export_all source_exports.value_exports_tmap;
    export_all source_exports.type_exports_tmap
end

module CJSExtractNamedExportsTKit = struct
  let rec on_type cx ~concretize (reason, local_module) t =
    match concretize t with
    | NamespaceT { namespace_symbol = _; values_type; types_tmap } ->
      (* Copy props from the values part *)
      let module_type = on_type cx ~concretize (reason, local_module) values_type in
      (* Copy type exports *)
      ExportNamedTKit.mod_ModuleT
        cx
        ( NameUtils.Map.empty,
          Properties.extract_named_exports (Context.find_props cx types_tmap),
          DirectExport
        )
        module_type;
      module_type
    (* ObjT CommonJS export values have their properties turned into named exports. *)
    | DefT (_, ObjT o) ->
      let { props_tmap; proto_t; _ } = o in
      (* Copy props from the prototype *)
      let module_type = on_type cx ~concretize (reason, local_module) proto_t in
      (* Copy own props *)
      ExportNamedTKit.mod_ModuleT
        cx
        ( Properties.extract_named_exports (Context.find_props cx props_tmap),
          NameUtils.Map.empty,
          DirectExport
        )
        module_type;
      module_type
    (* InstanceT CommonJS export values have their properties turned into named exports. *)
    | DefT (_, InstanceT { inst = { own_props; proto_props; _ }; _ }) ->
      let extract_named_exports id =
        Context.find_props cx id
        |> NameUtils.Map.filter (fun x _ -> not (is_munged_prop_name cx x))
        |> Properties.extract_named_exports
      in
      (* Copy own props *)
      ExportNamedTKit.mod_ModuleT
        cx
        (extract_named_exports own_props, NameUtils.Map.empty, DirectExport)
        local_module;

      (* Copy proto props *)
      (* TODO: own props should take precedence *)
      ExportNamedTKit.mod_ModuleT
        cx
        (extract_named_exports proto_props, NameUtils.Map.empty, DirectExport)
        local_module;
      local_module
    (* If the module is exporting any or Object, then we allow any named import. *)
    | AnyT _ ->
      let {
        module_reason;
        module_export_types = exporttypes;
        module_is_strict;
        module_available_platforms;
      } =
        local_module
      in
      {
        module_reason;
        module_export_types = { exporttypes with has_every_named_export = true };
        module_is_strict;
        module_available_platforms;
      }
    (* All other CommonJS export value types do not get merged into the named
     * exports tmap in any special way. *)
    | _ -> local_module
end

module ImportExportUtils : sig
  val get_module_type_or_any :
    Context.t ->
    ?perform_platform_validation:bool ->
    import_kind_for_untyped_import_validation:Type.import_kind option ->
    ALoc.t * Flow_import_specifier.userland ->
    (Type.moduletype, Type.t) result

  val get_imported_type :
    cx ->
    singleton_concretize_type_for_imports_exports:(cx -> reason -> Type.t -> Type.t) ->
    import_reason:reason ->
    module_name:Flow_import_specifier.userland ->
    source_module:(Type.moduletype, Type.t) result ->
    import_kind:Type.import_kind ->
    remote_name:string ->
    local_name:string ->
    ALoc.t option * Type.t

  val get_module_namespace_type :
    Context.t ->
    reason ->
    namespace_symbol:FlowSymbol.symbol ->
    (Type.moduletype, Type.t) result ->
    Type.t

  val import_namespace_specifier_type :
    cx ->
    reason ->
    import_kind:Flow_ast.Statement.ImportDeclaration.import_kind ->
    module_name:Flow_import_specifier.userland ->
    namespace_symbol:FlowSymbol.symbol ->
    source_module:(Type.moduletype, Type.t) result ->
    local_loc:loc ->
    Type.t

  val import_named_specifier_type :
    Context.t ->
    reason ->
    singleton_concretize_type_for_imports_exports:(cx -> reason -> Type.t -> Type.t) ->
    import_kind:Flow_ast.Statement.ImportDeclaration.import_kind ->
    module_name:Flow_import_specifier.userland ->
    source_module:(Type.moduletype, Type.t) result ->
    remote_name:string ->
    local_name:string ->
    ALoc.t option * Type.t

  val import_default_specifier_type :
    Context.t ->
    reason ->
    singleton_concretize_type_for_imports_exports:(cx -> reason -> Type.t -> Type.t) ->
    import_kind:Flow_ast.Statement.ImportDeclaration.import_kind ->
    module_name:Flow_import_specifier.userland ->
    source_module:(Type.moduletype, Type.t) result ->
    local_name:string ->
    ALoc.t option * Type.t

  val cjs_require_type :
    cx ->
    reason ->
    reposition:(cx -> ALoc.t -> Type.t -> Type.t) ->
    namespace_symbol:FlowSymbol.symbol ->
    standard_cjs_esm_interop:bool ->
    (Type.moduletype, Type.t) result ->
    ALoc.t option * Type.t

  val get_implicitly_imported_react_type :
    cx ->
    ALoc.t ->
    singleton_concretize_type_for_imports_exports:(cx -> reason -> Type.t -> Type.t) ->
    purpose:Flow_intermediate_error_types.expected_module_purpose ->
    Type.t
end = struct
  let check_platform_availability cx error_loc imported_module_available_platforms =
    let current_module_available_platforms = Context.available_platforms cx in
    match (current_module_available_platforms, imported_module_available_platforms) with
    | (None, None)
    | (None, Some _)
    | (Some _, None) ->
      ()
    | (Some required_platforms, Some available_platforms) ->
      let file_options = Context.((metadata cx).file_options) in
      let required_platforms =
        Platform_set.to_platform_string_set ~file_options required_platforms
      in
      let available_platforms =
        Platform_set.to_platform_string_set ~file_options available_platforms
      in
      let missing_platforms = SSet.diff required_platforms available_platforms in
      if SSet.cardinal missing_platforms > 0 then
        let message =
          Error_message.EMissingPlatformSupport
            { loc = error_loc; available_platforms; required_platforms }
        in
        add_output cx message

  let get_module_type_or_any
      cx
      ?(perform_platform_validation = false)
      ~import_kind_for_untyped_import_validation
      (loc, mref) =
    if Context.in_declare_module cx then
      match Context.builtin_module_opt cx mref with
      | Some (_, (lazy m)) -> Ok m
      | None -> Error (lookup_builtin_module_error cx mref loc)
    else
      let module_type_or_any =
        match Context.find_require cx (Flow_import_specifier.Userland mref) with
        | Context.TypedModule f -> f ()
        | Context.UncheckedModule module_def_loc ->
          Base.Option.iter import_kind_for_untyped_import_validation ~f:(fun import_kind ->
              match import_kind with
              | ImportType
              | ImportTypeof ->
                let message = Error_message.EUntypedTypeImport (loc, mref) in
                add_output cx message
              | ImportValue ->
                let message = Error_message.EUntypedImport (loc, mref) in
                add_output cx message
          );
          Error (AnyT.why Untyped (mk_reason (RModule mref) module_def_loc))
        | Context.MissingModule -> Error (lookup_builtin_module_error cx mref loc)
      in
      let need_platform_validation =
        perform_platform_validation && Files.multi_platform Context.((metadata cx).file_options)
      in
      ( if need_platform_validation then
        match module_type_or_any with
        | Ok m ->
          if need_platform_validation then
            check_platform_availability cx loc m.module_available_platforms
        | Error _ -> ()
      );
      module_type_or_any

  let get_imported_type
      cx
      ~singleton_concretize_type_for_imports_exports
      ~import_reason
      ~module_name
      ~source_module
      ~import_kind
      ~remote_name
      ~local_name =
    let is_strict = Context.is_strict cx in
    let name_def_loc_ref = ref None in
    let with_concretized_type cx r f t = f (singleton_concretize_type_for_imports_exports cx r t) in
    let t =
      match source_module with
      | Ok m ->
        let (name_loc_opt, t) =
          if remote_name = "default" then
            ImportDefaultTKit.on_ModuleT
              cx
              ~with_concretized_type
              (import_reason, import_kind, (local_name, module_name), is_strict)
              m
          else
            ImportNamedTKit.on_ModuleT
              cx
              ~with_concretized_type
              (import_reason, import_kind, remote_name, module_name, is_strict)
              m
        in
        name_def_loc_ref := name_loc_opt;
        t
      | Error t -> t
    in
    let name_def_loc = !name_def_loc_ref in
    (name_def_loc, t)

  let get_module_namespace_type cx reason ~namespace_symbol source_module =
    let is_strict = Context.is_strict cx in
    match source_module with
    | Ok m ->
      let (values_type, types_tmap) = ImportModuleNsTKit.on_ModuleT cx (reason, is_strict) m in
      NamespaceT { namespace_symbol; values_type; types_tmap }
    | Error t -> t

  let import_namespace_specifier_type
      cx import_reason ~import_kind ~module_name ~namespace_symbol ~source_module ~local_loc =
    let open Flow_ast.Statement in
    match import_kind with
    | ImportDeclaration.ImportType -> assert_false "import type * is a parse error"
    | ImportDeclaration.ImportTypeof ->
      let module_ns_t =
        get_module_namespace_type cx import_reason ~namespace_symbol source_module
      in
      let bind_reason = repos_reason local_loc import_reason in
      ImportTypeofTKit.on_concrete_type cx bind_reason "*" module_ns_t
    | ImportDeclaration.ImportValue ->
      let reason = mk_reason (RModule module_name) local_loc in
      let namespace_symbol = FlowSymbol.mk_module_symbol ~name:module_name ~def_loc:local_loc in
      get_module_namespace_type cx reason ~namespace_symbol source_module

  let type_kind_of_kind = function
    | Flow_ast.Statement.ImportDeclaration.ImportType -> Type.ImportType
    | Flow_ast.Statement.ImportDeclaration.ImportTypeof -> Type.ImportTypeof
    | Flow_ast.Statement.ImportDeclaration.ImportValue -> Type.ImportValue

  let import_named_specifier_type
      cx
      import_reason
      ~singleton_concretize_type_for_imports_exports
      ~import_kind
      ~module_name
      ~source_module
      ~remote_name
      ~local_name =
    let import_kind = type_kind_of_kind import_kind in
    get_imported_type
      cx
      ~singleton_concretize_type_for_imports_exports
      ~import_reason
      ~module_name
      ~source_module
      ~import_kind
      ~remote_name
      ~local_name

  let import_default_specifier_type
      cx
      import_reason
      ~singleton_concretize_type_for_imports_exports
      ~import_kind
      ~module_name
      ~source_module
      ~local_name =
    let import_kind = type_kind_of_kind import_kind in
    get_imported_type
      cx
      ~singleton_concretize_type_for_imports_exports
      ~import_reason
      ~module_name
      ~source_module
      ~import_kind
      ~remote_name:"default"
      ~local_name

  let cjs_require_type
      cx reason ~reposition ~namespace_symbol ~standard_cjs_esm_interop source_module =
    let is_strict = Context.is_strict cx in
    match source_module with
    | Ok m ->
      let (t, def_loc) =
        CJSRequireTKit.on_ModuleT
          cx
          ~reposition
          ~reason
          ~module_symbol:namespace_symbol
          ~is_strict
          ~standard_cjs_esm_interop
          m
      in
      (Some def_loc, t)
    | Error t -> (None, t)

  let get_implicitly_imported_react_type
      cx loc ~singleton_concretize_type_for_imports_exports ~purpose =
    let source_module =
      match Context.builtin_module_opt cx (Flow_import_specifier.userland "react") with
      | Some (_, (lazy module_type)) -> Ok module_type
      | None ->
        let reason = mk_reason (RModule (Flow_import_specifier.userland "react")) loc in
        add_output
          cx
          (Error_message.EExpectedModuleLookupFailed
             { loc; name = "react"; expected_module_purpose = purpose }
          );
        Error (AnyT.error reason)
    in
    let (name, import_kind) =
      match purpose with
      | Flow_intermediate_error_types.ReactModuleForJSXFragment -> ("Fragment", ImportValue)
      | Flow_intermediate_error_types.ReactModuleForReactClassComponent -> ("Component", ImportValue)
      | Flow_intermediate_error_types.ReactModuleForReactMixedElementType ->
        ("MixedElement", ImportType)
      | Flow_intermediate_error_types.ReactModuleForReactNodeType -> ("Node", ImportType)
      | Flow_intermediate_error_types.ReactModuleForReactRefSetterType -> ("RefSetter", ImportType)
    in
    let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
    get_imported_type
      cx
      ~singleton_concretize_type_for_imports_exports
      ~import_reason:reason
      ~module_name:(Flow_import_specifier.userland "react")
      ~source_module
      ~import_kind
      ~remote_name:name
      ~local_name:name
    |> snd
end

(*******************)
(* GetPropT helper *)
(*******************)

let check_method_unbinding cx ~use_op ~method_accessible ~reason_op ~propref ~(hint : lazy_hint_t) p
    =
  match p with
  | Method { key_loc; type_ = t } when not method_accessible ->
    let hint_result = (snd hint) ~expected_only:false reason_op in
    let valid_hint_t =
      match hint_result with
      | HintAvailable (t, _) ->
        let t =
          match t with
          | OpenT (_, id) ->
            let (_, constraints) = Context.find_constraints cx id in
            (match constraints with
            | FullyResolved tvar -> Context.force_fully_resolved_tvar cx tvar
            | Resolved t -> t
            | Unresolved _ -> t)
          | _ -> t
        in
        (match t with
        | DefT (_, MixedT _)
        | AnyT _ ->
          Some t
        | _ -> None)
      | _ -> None
    in
    (match valid_hint_t with
    | Some t -> Method { key_loc; type_ = t }
    | None ->
      let reason_op = reason_of_propref propref in
      add_output
        cx
        (Error_message.EMethodUnbinding { use_op; reason_op; reason_prop = reason_of_t t });
      Method { key_loc; type_ = Type.Properties.unbind_this_method t })
  | _ -> p

(* e.g. `0`, `-123, `234234` *)
let int_regex = Str.regexp {|^-?\(0\|[1-9][0-9]*\)$|}

let is_str_intlike str = Str.string_match int_regex str 0

let type_of_key_name cx name reason =
  let str_key () =
    let key_reason = replace_desc_reason (RPropertyIsAString name) reason in
    DefT (key_reason, StrT_UNSOUND (None, name))
  in
  let str = display_string_of_name name in
  (* We don't want the `NumericStrKeyT` type to leak out of the obj-to-obj
     subtyping check context. Other than `Object.key` and `$Keys`, which
     already we ensure always return a string, another way this type could
     leak out is during implicit instantiation, e.g. for a function like
     ```
     declare function f<K>({+[K]: mixed}): K;
     f({1: true});
     ```
     So, if we are in implicit instantiation, we always treat this as a string.
  *)
  if is_str_intlike str && not (Context.in_implicit_instantiation cx) then
    match Float.of_string_opt str with
    | Some value when Js_number.is_float_safe_integer value ->
      let key_reason = replace_desc_reason (RProperty (Some name)) reason in
      DefT (key_reason, NumericStrKeyT (value, str))
    | _ -> str_key ()
  else
    str_key ()

module type Get_prop_helper_sig = sig
  type r

  val dict_read_check :
    Context.t -> Type.DepthTrace.t -> use_op:Type.use_op -> Type.t * Type.t -> unit

  val cg_lookup :
    Context.t ->
    Type.DepthTrace.t ->
    obj_t:Type.t ->
    method_accessible:bool ->
    Type.t ->
    Reason.reason * Type.lookup_kind * Type.propref * use_op * Type.Properties.Set.t ->
    r

  val reposition : Context.t -> ?trace:Type.DepthTrace.t -> ALoc.t -> Type.t -> Type.t

  val mk_react_dro : Context.t -> use_op -> ALoc.t * Type.dro_type -> Type.t -> Type.t

  val mk_hooklike : Context.t -> use_op -> Type.t -> Type.t

  val return : Context.t -> use_op:use_op -> Type.DepthTrace.t -> Type.t -> r

  val error_type : Context.t -> Type.DepthTrace.t -> Reason.t -> r

  val cg_get_prop :
    Context.t ->
    Type.DepthTrace.t ->
    Type.t ->
    use_op * reason * Type.ident option * (Reason.t * Reason.name) ->
    r

  (* When looking up a prop on an object with an indexer we can do a subtyping check to see if
   * the prop exists instead of assuming the indexer catches all non-enumerated props. This
   * allows us to check if e.g. the indexer on a props object may contain the key/ref props.
   * It's not possible to do this in all contexts that GetPropTKit is used, like annotation
   * inference, so we allow this behavior to be disabled by passing None. Note that this will
   * likely introduce inconsistent semantics and is undesirable, but at the time of writing this
   * comment we had no alternative for annotation inference.
   *)
  val prop_overlaps_with_indexer : (Context.t -> Reason.name -> Reason.t -> Type.t -> bool) option
end

module GetPropT_kit (F : Get_prop_helper_sig) = struct
  let perform_read_prop_action cx trace use_op propref p ureason react_dro =
    match Property.read_t_of_property_type p with
    | Some t ->
      let loc = loc_of_reason ureason in
      let t =
        match react_dro with
        | Some dro when not (is_exception_to_react_dro propref) -> F.mk_react_dro cx use_op dro t
        | _ -> t
      in
      let t =
        match propref with
        | Named { name = OrdinaryName name; _ }
          when Context.hook_compatibility cx && Flow_ast_utils.hook_name name ->
          F.mk_hooklike cx use_op t
        | _ -> t
      in
      F.return cx trace ~use_op:unknown_use (F.reposition cx ~trace loc t)
    | None ->
      let (reason_prop, prop_name) =
        match propref with
        | Named { reason; name; from_indexed_access = false } -> (reason, Some name)
        | Named { reason; name = _; from_indexed_access = true } -> (reason, None)
        | Computed t -> (reason_of_t t, None)
      in
      let msg = Error_message.EPropNotReadable { reason_prop; prop_name; use_op } in
      add_output cx msg;
      F.error_type cx trace ureason

  let get_instance_prop cx trace ~use_op ~ignore_dicts inst propref reason_op =
    let dict = inst.inst_dict in
    let named_prop =
      match propref with
      | Named { name; _ } ->
        let own_props = Context.find_props cx inst.own_props in
        let proto_props = Context.find_props cx inst.proto_props in
        let props = NameUtils.Map.union own_props proto_props in
        NameUtils.Map.find_opt name props
      | Computed _ -> None
    in
    match (named_prop, propref, dict) with
    | (Some prop, _, _) -> Some (prop, PropertyMapProperty)
    | (None, Named { name; from_indexed_access = true; _ }, Some { key; value; dict_polarity; _ })
      when not ignore_dicts ->
      F.dict_read_check cx trace ~use_op (type_of_key_name cx name reason_op, key);
      Some
        ( Field
            { preferred_def_locs = None; key_loc = None; type_ = value; polarity = dict_polarity },
          IndexerProperty
        )
    | (None, Computed k, Some { key; value; dict_polarity; _ }) when not ignore_dicts ->
      F.dict_read_check cx trace ~use_op (k, key);
      Some
        ( Field
            { preferred_def_locs = None; key_loc = None; type_ = value; polarity = dict_polarity },
          IndexerProperty
        )
    | _ -> None

  let read_instance_prop
      cx
      trace
      ~use_op
      ~instance_t
      ~id
      ~method_accessible
      ~super
      ~lookup_kind
      ~hint
      ~skip_optional:_
      inst
      propref
      reason_op =
    match get_instance_prop cx trace ~use_op ~ignore_dicts:true inst propref reason_op with
    | Some (p, _target_kind) ->
      let p = check_method_unbinding cx ~use_op ~method_accessible ~reason_op ~propref ~hint p in
      Base.Option.iter id ~f:(Context.test_prop_hit cx);
      perform_read_prop_action
        cx
        trace
        use_op
        propref
        (Property.type_ p)
        reason_op
        inst.inst_react_dro
    | None ->
      let super =
        match name_of_propref propref with
        | Some name when is_munged_prop_name cx name -> ObjProtoT (reason_of_t super)
        | _ -> super
      in
      let ids = Properties.Set.of_list [inst.own_props; inst.proto_props] in
      F.cg_lookup
        cx
        trace
        ~obj_t:instance_t
        ~method_accessible
        super
        (reason_op, lookup_kind, propref, use_op, ids)

  let on_EnumObjectT cx trace enum_reason ~enum_object_t ~enum_value_t ~enum_info access =
    let (_, access_reason, _, (prop_reason, member_name)) = access in
    let { members; representation_t; _ } = enum_info in
    let error_invalid_access ~suggestion =
      let member_reason = replace_desc_reason (RIdentifier member_name) prop_reason in
      add_output
        cx
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
        let enum_value_t = F.reposition cx ~trace (loc_of_reason access_reason) enum_value_t in
        F.return cx trace ~use_op:unknown_use enum_value_t
      else
        let suggestion = typo_suggestion (SMap.keys members |> List.rev) name in
        error_invalid_access ~suggestion
    | OrdinaryName _ ->
      let t = enum_proto cx ~reason:access_reason ~enum_object_t ~enum_value_t ~representation_t in
      F.cg_get_prop cx trace t access
    | InternalName _ -> error_invalid_access ~suggestion:None

  let on_array_length cx trace reason ~inexact arity reason_op =
    (* Use definition as the reason for the length, as this is
     * the actual location where the length is in fact set. *)
    let loc = Reason.loc_of_reason reason_op in
    let t = tuple_length reason ~inexact arity in
    F.return cx trace ~use_op:unknown_use (F.reposition cx ~trace loc t)

  (* Under no_unchecked_indexed_access=true, indexed access on computed props should include void.
   * However, get_obj_prop might be called in other contexts where union with void doesn't make sense.
   * e.g. indexed access type or write `obj[foo] = ...`
   *
   * never_union_void_on_computed_prop_access is the flag to disable the union void behavior. *)
  let get_obj_prop
      cx trace use_op ~skip_optional ~never_union_void_on_computed_prop_access o propref reason_op =
    let named_prop =
      match propref with
      | Named { name; _ } -> Context.get_prop cx o.props_tmap name
      | Computed _ -> None
    in
    let union_void_if_instructed t =
      if Context.no_unchecked_indexed_access cx && not never_union_void_on_computed_prop_access then
        let r = reason_of_t t in
        UnionT (r, UnionRep.make t (VoidT.why r) [])
      else
        t
    in
    let dict_t = Obj_type.get_dict_opt o.flags.obj_kind in
    match (propref, named_prop, dict_t) with
    | (_, Some prop, _) ->
      (* Property exists on this property map *)
      let field =
        match Property.type_ prop with
        | OrdinaryField { type_ = OptionalT { type_; _ }; polarity } when skip_optional ->
          OrdinaryField { type_; polarity }
        | field -> field
      in
      Some (field, PropertyMapProperty)
    | (Named { name; _ }, None, Some { key; value; dict_polarity; _ })
      when not (is_dictionary_exempt name) ->
      (* Dictionaries match all property reads *)
      (match F.prop_overlaps_with_indexer with
      | Some prop_overlaps_with_indexer when not (TvarVisitors.has_unresolved_tvars cx key) ->
        if prop_overlaps_with_indexer cx name reason_op key then
          let type_ = union_void_if_instructed value in
          Some (OrdinaryField { type_; polarity = dict_polarity }, IndexerProperty)
        else
          None
      | _ ->
        F.dict_read_check cx trace ~use_op (type_of_key_name cx name reason_op, key);
        let type_ = union_void_if_instructed value in
        Some (OrdinaryField { type_; polarity = dict_polarity }, IndexerProperty))
    | (Computed k, None, Some { key; value; dict_polarity; _ }) ->
      F.dict_read_check cx trace ~use_op (k, key);
      let type_ = union_void_if_instructed value in
      Some (OrdinaryField { type_; polarity = dict_polarity }, IndexerProperty)
    | _ -> None

  let read_obj_prop
      cx trace ~use_op ~from_annot ~skip_optional o propref reason_obj reason_op lookup_info =
    let l = DefT (reason_obj, ObjT o) in
    match
      get_obj_prop
        cx
        trace
        use_op
        ~skip_optional
        ~never_union_void_on_computed_prop_access:from_annot
        o
        propref
        reason_op
    with
    | Some (p, _target_kind) ->
      Base.Option.iter ~f:(fun (id, _) -> Context.test_prop_hit cx id) lookup_info;
      perform_read_prop_action cx trace use_op propref p reason_op o.flags.react_dro
    | None ->
      (match propref with
      | Named { reason = reason_prop; name; from_indexed_access = _ } ->
        let lookup_kind =
          match lookup_info with
          | Some (id, lookup_default_tout) when Obj_type.is_exact o.flags.obj_kind ->
            let lookup_default =
              let r = replace_desc_reason (RMissingProperty (Some name)) reason_op in
              Some (DefT (r, VoidT), lookup_default_tout)
            in
            NonstrictReturning (lookup_default, Some (id, (reason_prop, reason_obj)))
          | _ -> Strict reason_obj
        in
        let x = (reason_op, lookup_kind, propref, use_op, Properties.Set.singleton o.props_tmap) in
        F.cg_lookup cx trace ~obj_t:l ~method_accessible:true o.proto_t x
      | Computed elem_t ->
        (match elem_t with
        | OpenT _ ->
          let loc = loc_of_t elem_t in
          add_output cx Error_message.(EInternal (loc, PropRefComputedOpen));
          F.error_type cx trace reason_op
        | GenericT { bound = DefT (_, (StrT_UNSOUND _ | SingletonStrT _)); _ }
        | DefT (_, (StrT_UNSOUND _ | SingletonStrT _)) ->
          let loc = loc_of_t elem_t in
          add_output cx Error_message.(EInternal (loc, PropRefComputedLiteral));
          F.error_type cx trace reason_op
        | GenericT
            {
              bound =
                DefT (_, (NumT_UNSOUND (_, (value, _)) | SingletonNumT { value = (value, _); _ }));
              _;
            }
        | DefT (_, (NumT_UNSOUND (_, (value, _)) | SingletonNumT { value = (value, _); _ })) ->
          let reason_prop = reason_of_t elem_t in
          let kind = Flow_intermediate_error_types.InvalidObjKey.kind_of_num_value value in
          add_output cx (Error_message.EObjectComputedPropertyAccess (reason_op, reason_prop, kind));
          F.error_type cx trace reason_op
        | AnyT (_, src) -> F.return cx trace ~use_op:unknown_use (AnyT.why src reason_op)
        | _ ->
          let reason_prop = reason_of_t elem_t in
          add_output
            cx
            (Error_message.EObjectComputedPropertyAccess
               (reason_op, reason_prop, Flow_intermediate_error_types.InvalidObjKey.Other)
            );
          F.error_type cx trace reason_op))
end

(***************)
(* ElemT utils *)
(***************)

(* See docs GetPropsKit.get_obj_prop for explanation of never_union_void_on_computed_prop_access *)
let array_elem_check
    ~write_action ~never_union_void_on_computed_prop_access cx l use_op reason reason_tup arrtype =
  let union_void_if_instructed elem_t =
    if Context.no_unchecked_indexed_access cx && not never_union_void_on_computed_prop_access then
      let r = reason_of_t elem_t in
      UnionT (r, UnionRep.make elem_t (VoidT.why r) [])
    else
      elem_t
  in
  let (elem_t, elements, is_index_restricted, is_tuple, tuple_is_inexact, react_dro) =
    match arrtype with
    | ArrayAT { elem_t; tuple_view; react_dro } ->
      let elements =
        Base.Option.map
          ~f:(fun (TupleView { elements; arity = _; inexact = _ }) -> elements)
          tuple_view
      in
      let elem_t = union_void_if_instructed elem_t in
      (elem_t, elements, false, false, false, react_dro)
    | TupleAT { elem_t; elements; arity = _; inexact; react_dro } ->
      let elem_t = union_void_if_instructed elem_t in
      (elem_t, Some elements, true, true, inexact, react_dro)
    | ROArrayAT (elem_t, react_dro) ->
      let elem_t = union_void_if_instructed elem_t in
      (elem_t, None, true, false, false, react_dro)
  in
  let (can_write_tuple, value, use_op) =
    match l with
    | DefT
        ( index_reason,
          (NumT_UNSOUND (_, (float_value, _)) | SingletonNumT { value = (float_value, _); _ })
        ) -> begin
      match elements with
      | None -> (false, elem_t, use_op)
      | Some elements ->
        let index_string = Dtoa.ecma_string_of_float float_value in
        begin
          match int_of_string_opt index_string with
          | Some index ->
            let value_opt =
              try List.nth_opt elements index with
              | Invalid_argument _ -> None
            in
            begin
              match value_opt with
              | Some (TupleElement { t; polarity; optional; name; reason = _ }) ->
                if write_action && (not @@ Polarity.compat (polarity, Polarity.Negative)) then
                  add_output
                    cx
                    (Error_message.ETupleElementNotWritable { use_op; reason; index; name })
                else if (not write_action) && (not @@ Polarity.compat (polarity, Polarity.Positive))
                then
                  add_output
                    cx
                    (Error_message.ETupleElementNotReadable { use_op; reason; index; name });
                let (t, use_op) =
                  if write_action then
                    (* We don't allowing writing `undefined` to optional tuple elements.
                     * User can add `| void` to the element type if they want this behavior. *)
                    let t =
                      match (optional, t) with
                      | (true, OptionalT { type_; _ }) -> type_
                      | _ -> t
                    in
                    (t, Frame (TupleAssignment { upper_optional = optional }, use_op))
                  else
                    (t, use_op)
                in
                (true, t, use_op)
              | None ->
                if is_tuple then (
                  add_output
                    cx
                    (Error_message.ETupleOutOfBounds
                       {
                         use_op;
                         reason;
                         reason_op = reason_tup;
                         inexact = tuple_is_inexact;
                         length = List.length elements;
                         index = index_string;
                       }
                    );
                  ( true,
                    AnyT.error (mk_reason (RTupleOutOfBoundsAccess index) (loc_of_reason reason)),
                    use_op
                  )
                ) else
                  (true, elem_t, use_op)
            end
          | None ->
            (* not an integer index *)
            if is_tuple then (
              add_output
                cx
                (Error_message.ETupleNonIntegerIndex
                   { use_op; reason = index_reason; index = index_string }
                );
              (true, AnyT.error reason, use_op)
            ) else
              (true, elem_t, use_op)
        end
    end
    | _ -> (false, elem_t, use_op)
  in
  begin
    match react_dro with
    | Some dro when write_action ->
      add_output
        cx
        (Error_message.EROArrayWrite ((reason, reason_tup), Frame (ReactDeepReadOnly dro, use_op)))
    | _ -> ()
  end;
  ( if is_index_restricted && (not can_write_tuple) && write_action then
    let error =
      match elements with
      | Some _ -> Error_message.ETupleUnsafeWrite { reason; use_op }
      | None -> Error_message.EROArrayWrite ((reason, reason_tup), use_op)
    in
    add_output cx error
  );
  (value, is_tuple, use_op, react_dro)

let propref_for_elem_t cx l =
  match l with
  | OpaqueT (reason, { super_t = Some (DefT (_, SingletonStrT { value = name; _ })); _ })
  | GenericT
      { bound = DefT (_, (SingletonStrT { value = name; _ } | StrT_UNSOUND (_, name))); reason; _ }
  | DefT (reason, (SingletonStrT { value = name; _ } | StrT_UNSOUND (_, name))) ->
    update_lit_type_from_annot cx l;
    let reason = replace_desc_reason (RProperty (Some name)) reason in
    mk_named_prop ~reason ~from_indexed_access:true name
  | OpaqueT (reason_num, { super_t = Some (DefT (_, SingletonNumT { value = (value, raw); _ })); _ })
  | GenericT
      {
        bound =
          DefT (_, (NumT_UNSOUND (_, (value, raw)) | SingletonNumT { value = (value, raw); _ }));
        reason = reason_num;
        _;
      }
  | DefT (reason_num, (NumT_UNSOUND (_, (value, raw)) | SingletonNumT { value = (value, raw); _ }))
    when Js_number.is_float_safe_integer value ->
    update_lit_type_from_annot cx l;
    let reason = replace_desc_reason (RProperty (Some (OrdinaryName raw))) reason_num in
    let name = OrdinaryName (Dtoa.ecma_string_of_float value) in
    mk_named_prop ~reason ~from_indexed_access:true name
  | l -> Computed l

let keylist_of_props props reason_op =
  NameUtils.Map.fold
    (fun name _ acc ->
      match name with
      | OrdinaryName _ ->
        let reason = replace_desc_new_reason (RStringLit name) reason_op in
        DefT (reason, SingletonStrT { from_annot = true; value = name }) :: acc
      | InternalName _ -> acc)
    props
    []

let objt_to_obj_rest cx props_tmap ~reachable_targs ~obj_kind ~reason_op ~reason_obj xs =
  let props = Context.find_props cx props_tmap in
  let props = List.fold_left (fun map x -> NameUtils.Map.remove (OrdinaryName x) map) props xs in
  (* Remove shadow properties from rest result *)
  (* TODO consider converting to SMap here so downstream code doesn't need to
   * handle internal names *)
  let props = NameUtils.Map.filter (fun x _ -> not (is_internal_name x)) props in
  let use_op = Op (ObjectRest { op = replace_desc_reason (desc_of_reason reason_obj) reason_op }) in
  let props =
    NameUtils.Map.mapi
      (fun name -> function
        | Field { preferred_def_locs; key_loc; type_; polarity } ->
          if not @@ Polarity.compat (polarity, Polarity.Positive) then
            add_output
              cx
              (Error_message.EPropNotReadable
                 { reason_prop = reason_of_t type_; prop_name = Some name; use_op }
              );
          Field { preferred_def_locs; key_loc; type_; polarity = Polarity.Neutral }
        | Set { key_loc = _; type_ } as p ->
          add_output
            cx
            (Error_message.EPropNotReadable
               { reason_prop = reason_of_t type_; prop_name = Some name; use_op }
            );
          p
        | p -> p)
      props
  in
  let proto = ObjProtoT reason_op in
  Obj_type.mk_with_proto cx reason_op ~reachable_targs ~props proto ~obj_kind

(* $Values *)

let get_values_type_of_obj_t cx o reason =
  let {
    flags;
    proto_t = _;
    props_tmap = tmap;
    call_t = _ (* call props excluded from values *);
    reachable_targs = _;
  } =
    o
  in
  (* Find all of the props. *)
  let props = Context.find_props cx tmap in
  (* Get the read type for all readable properties and discard the rest. *)
  let ts =
    NameUtils.Map.fold
      (fun _ prop ts ->
        match Property.read_t prop with
        | Some t -> t :: ts
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

let get_values_type_of_instance_t cx own_props dict reason =
  (* Find all of the props. *)
  let props = Context.find_props cx own_props in
  (* Get the read type for all readable properties and discard the rest. *)
  let ts =
    NameUtils.Map.fold
      (fun _ prop ts ->
        match Property.read_t prop with
        | Some t -> t :: ts
        | _ -> ts)
      props
      []
  in
  let ts =
    match dict with
    | Some { value = dict_value; dict_polarity; _ }
      when Polarity.compat (dict_polarity, Polarity.Positive) ->
      dict_value :: ts
    | None -> ts
    | _ -> ts
  in
  (* Create a union type from all our selected types. *)
  Type_mapper.union_flatten cx ts |> union_of_ts reason

let any_mod_src_keep_placeholder new_src = function
  | Placeholder -> Placeholder
  | _ -> new_src

let unary_negate_lit ~annot_loc reason (value, raw) =
  let reason = annot_reason ~annot_loc @@ repos_reason annot_loc reason in
  let (value, raw) = Flow_ast_utils.negate_number_literal (value, raw) in
  let reason =
    Reason.update_desc_reason
      (function
        | RNumberLit _ -> RNumberLit raw
        | d -> d)
      reason
  in
  (reason, (value, raw))

let unary_negate_bigint_lit ~annot_loc reason (value, raw) =
  let reason = annot_reason ~annot_loc @@ repos_reason annot_loc reason in
  let (value, raw) = Flow_ast_utils.negate_bigint_literal (value, raw) in
  let reason =
    Reason.update_desc_reason
      (function
        | RBigIntLit _ -> RBigIntLit raw
        | d -> d)
      reason
  in
  (reason, (value, raw))

let flow_unary_arith cx l reason kind =
  let open UnaryArithKind in
  match (kind, l) with
  | (Minus, DefT (lreason, NumT_UNSOUND (_, lit))) ->
    let (reason, lit) = unary_negate_lit ~annot_loc:(loc_of_reason reason) lreason lit in
    DefT (reason, NumT_UNSOUND (None, lit))
  | (Minus, DefT (lreason, SingletonNumT { from_annot; value = lit })) ->
    let (reason, lit) = unary_negate_lit ~annot_loc:(loc_of_reason reason) lreason lit in
    DefT (reason, SingletonNumT { from_annot; value = lit })
  | (Minus, DefT (_, NumGeneralT _)) -> l
  | (Minus, DefT (lreason, BigIntT_UNSOUND (_, (value, raw)))) ->
    let (reason, lit) =
      unary_negate_bigint_lit ~annot_loc:(loc_of_reason reason) lreason (value, raw)
    in
    DefT (reason, BigIntT_UNSOUND (None, lit))
  | (Minus, DefT (lreason, SingletonBigIntT { from_annot; value = lit })) ->
    let (reason, lit) = unary_negate_bigint_lit ~annot_loc:(loc_of_reason reason) lreason lit in
    DefT (reason, SingletonBigIntT { from_annot; value = lit })
  | (Minus, DefT (_, BigIntGeneralT _)) -> l
  | (Plus, DefT (reason_bigint, (BigIntGeneralT _ | BigIntT_UNSOUND _))) ->
    add_output cx (Error_message.EBigIntNumCoerce reason_bigint);
    AnyT.error reason
  | (Plus, _) -> NumModuleT.why reason
  | (BitNot, DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _))) -> NumModuleT.why reason
  | (BitNot, DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _))) ->
    BigIntModuleT.why reason
  | (Update, DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _))) -> NumModuleT.why reason
  | (Update, DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _))) ->
    BigIntModuleT.why reason
  | (_, AnyT (_, src)) ->
    let src = any_mod_src_keep_placeholder Untyped src in
    AnyT.why src reason
  | (_, _) ->
    add_output cx (Error_message.EArithmeticOperand (reason_of_t l));
    AnyT.error reason

let flow_arith cx reason l r kind =
  let open ArithKind in
  let (_, op) = kind in
  match (op, l, r) with
  (* any <> _ *)
  (* _ <> any *)
  | (_, AnyT (_, src), _)
  | (_, _, AnyT (_, src)) ->
    AnyT.why src reason
  (* empty <> _ *)
  (* _ <> empty *)
  | (_, DefT (_, EmptyT), _)
  | (_, _, DefT (_, EmptyT)) ->
    EmptyT.why reason
  (* num <> num *)
  | ( _,
      DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)),
      DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _))
    ) ->
    NumModuleT.why reason
  | (RShift3, DefT (reason, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _)), _) ->
    add_output cx (Error_message.EBigIntRShift3 reason);
    AnyT.error reason
  (* bigint <> bigint *)
  | ( _,
      DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _)),
      DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _ | SingletonBigIntT _))
    ) ->
    BigIntModuleT.why reason
  (* str + str *)
  (* str + num *)
  (* num + str *)
  | ( Plus,
      DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)),
      DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _))
    )
  | ( Plus,
      DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)),
      DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _))
    )
  | ( Plus,
      DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)),
      DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _))
    ) ->
    StrModuleT.why reason
  | _ ->
    add_output
      cx
      (Error_message.EInvalidBinaryArith
         { reason_out = reason; reason_l = reason_of_t l; reason_r = reason_of_t r; kind }
      );
    AnyT.error reason

let is_same_instance_type { class_id = class_id1; _ } { class_id = class_id2; _ } =
  (* `ALoc.id_none` is not equal to anything for class ids *)
  ALoc.equal_id class_id1 class_id2 && (not @@ ALoc.equal_id class_id1 ALoc.id_none)

(* TypeAppT ~> TypeAppT has special behavior that flows the type arguments directly
 * instead of evaluating the types and then flowing the results to each other. This is
 * a bug that should be removed because it breaks transitivity, which can cause errors
 * on code like:
 * type ReadOnly<O> = {+[key in keyof O]: O[key]};
 * type O1 = {foo: number};
 * type O2 = {foo: string | number};
 * declare const x: ReadOnly<O1>;
 * (x: ReadOnly<O2>); // ERROR
 *
 * In order to prevent this in common mapped type and other cases, we do a best-effort
 * check to see if the RHS contains a mapped type, or other utility type. This traversal
 * is extremely limited and does not attempt to be exhaustive.
 *)
let wraps_utility_type cx tin =
  let seen_open_id = ref ISet.empty in
  let seen_eval_id = ref Eval.Set.empty in
  let rec loop t =
    match t with
    | EvalT (GenericT _, TypeDestructorT (_, _, ReadOnlyType), _)
    | EvalT (_, TypeDestructorT (_, _, ConditionalType { true_t = GenericT _; _ }), _)
    | EvalT (_, TypeDestructorT (_, _, ConditionalType { false_t = GenericT _; _ }), _) ->
      (* Handles special cases targeting tests/typeapp_opt/stylex.js *)
      true
    | EvalT (t, TypeDestructorT (_, _, ReadOnlyType), id) ->
      if Eval.Set.mem id !seen_eval_id then
        false
      else (
        seen_eval_id := Eval.Set.add id !seen_eval_id;
        loop t
      )
    | EvalT (_, TypeDestructorT (_, _, MappedType _), _) -> true
    | DefT (_, TypeT (_, t)) -> loop t
    | OpenT (_, id) ->
      let (root_id, constraints) = Context.find_constraints cx id in
      if ISet.mem root_id !seen_open_id then
        false
      else (
        seen_open_id := ISet.add root_id !seen_open_id;
        match constraints with
        | FullyResolved s -> loop (Context.force_fully_resolved_tvar cx s)
        | Resolved t -> loop t
        | _ -> false
      )
    | DefT (_, PolyT { t_out; _ }) -> loop t_out
    | TypeAppT { reason = _; use_op = _; type_; targs = _; from_value = _; use_desc = _ } ->
      loop type_
    | _ -> false
  in
  loop tin

(**********)
(* Tuples *)
(**********)

let validate_tuple_elements cx ~reason_tuple ~error_on_req_after_opt elements =
  let (valid, num_req, num_opt, _) =
    Base.List.fold elements ~init:(true, 0, 0, None) ~f:(fun acc element ->
        let (valid, num_req, num_opt, prev_element) = acc in
        let (TupleElement { optional; reason = reason_element; _ }) = element in
        if optional then
          (valid, num_req, num_opt + 1, Some element)
        else
          let valid =
            match prev_element with
            | Some (TupleElement { optional = true; reason = reason_optional; _ }) when valid ->
              if error_on_req_after_opt then
                add_output
                  cx
                  (Error_message.ETupleRequiredAfterOptional
                     { reason_tuple; reason_required = reason_element; reason_optional }
                  );
              false
            | _ -> valid
          in
          (valid, num_req + 1, num_opt, Some element)
    )
  in
  let arity = (num_req, num_req + num_opt) in
  (valid, arity)

let mk_tuple_type cx ~id ~mk_type_destructor ~inexact reason elements =
  let (resolved_rev, unresolved_rev, first_spread) =
    Base.List.fold elements ~init:([], [], None) ~f:(fun (resolved, unresolved, first_spread) el ->
        match (el, first_spread) with
        | (UnresolvedArg (el, generic), None) ->
          ((el, generic) :: resolved, unresolved, first_spread)
        | (UnresolvedSpreadArg t, None) -> (resolved, unresolved, Some (reason_of_t t, t))
        | (_, Some _) -> (resolved, el :: unresolved, first_spread)
    )
  in
  match first_spread with
  | Some (reason_spread, spread_t) ->
    let unresolved = List.rev unresolved_rev in
    let resolved_rev =
      Base.List.map ~f:(fun (el, generic) -> ResolvedArg (el, generic)) resolved_rev
    in
    mk_type_destructor
      cx
      unknown_use (* not used *)
      reason
      spread_t
      (SpreadTupleType { reason_tuple = reason; reason_spread; inexact; resolved_rev; unresolved })
      id
  | None ->
    let elements = Base.List.rev_map ~f:fst resolved_rev in
    let (valid, arity) =
      validate_tuple_elements cx ~reason_tuple:reason ~error_on_req_after_opt:true elements
    in
    if valid then
      let elem_t =
        if inexact then
          let reason_mixed = replace_desc_reason RTupleUnknownElementFromInexact reason in
          MixedT.make reason_mixed
        else
          let ts = tuple_ts_of_elements elements in
          let elem_t_reason = replace_desc_reason (RTupleElement { name = None }) reason in
          union_of_ts elem_t_reason ts
      in
      DefT (reason, ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro = None }))
    else
      AnyT.error reason

module RenderTypes : sig
  val mk_non_generic_render_type :
    Context.t ->
    reason ->
    Type.renders_variant ->
    ?force_post_component:bool ->
    concretize:(Context.t -> Type.t -> Type.t list) ->
    is_iterable_for_better_error:(Context.t -> Type.t -> bool) ->
    Type.t ->
    Type.t
end = struct
  let allow_child_renders ~parent_variant ~child_variant =
    match (parent_variant, child_variant) with
    | (RendersNormal, RendersNormal) -> true
    | (RendersNormal, (RendersMaybe | RendersStar)) -> false
    | (RendersMaybe, (RendersNormal | RendersMaybe)) -> true
    | (RendersMaybe, RendersStar) -> false
    | (RendersStar, _) -> true

  let ast_render_variant_of_render_variant = function
    | RendersNormal -> Flow_ast.Type.Renders.Normal
    | RendersMaybe -> Flow_ast.Type.Renders.Maybe
    | RendersStar -> Flow_ast.Type.Renders.Star

  type potential_fixable_error_kind =
    | InvalidRendersNullVoidFalse
    | InvalidRendersIterable

  type 'loc error_acc = {
    potential_fixable_error_acc: ('loc virtual_reason Nel.t * potential_fixable_error_kind) option;
    normal_errors: 'loc Error_message.t' list;
  }

  type 'loc render_type_normalization_context = {
    cx: Context.t;
    type_collector: TypeCollector.t;
    error_acc_ref: 'loc error_acc ref;
    arg_loc: 'loc;
    result_reason: 'loc virtual_reason;
    renders_variant: renders_variant;
    concretize: Type.t -> Type.t list;
    is_iterable_for_better_error: Type.t -> bool;
  }

  let merge_error_acc_with_potential_fixable_error normalization_cx new_error_opt =
    let { potential_fixable_error_acc; normal_errors } = !(normalization_cx.error_acc_ref) in
    let potential_fixable_error_acc =
      match (potential_fixable_error_acc, new_error_opt) with
      | (None, (r, k)) -> Some (Nel.one r, k)
      | (Some (rs, k1), (r, k2)) ->
        let k =
          match (k1, k2) with
          | (InvalidRendersNullVoidFalse, InvalidRendersNullVoidFalse) ->
            InvalidRendersNullVoidFalse
          | (InvalidRendersIterable, InvalidRendersIterable)
          | (InvalidRendersIterable, InvalidRendersNullVoidFalse)
          | (InvalidRendersNullVoidFalse, InvalidRendersIterable) ->
            InvalidRendersIterable
        in
        Some (Nel.cons r rs, k)
    in
    normalization_cx.error_acc_ref := { potential_fixable_error_acc; normal_errors }

  let merge_error_acc_with_normal_error normalization_cx e =
    let { potential_fixable_error_acc; normal_errors } = !(normalization_cx.error_acc_ref) in
    normalization_cx.error_acc_ref :=
      { potential_fixable_error_acc; normal_errors = e :: normal_errors }

  let rec on_concretized_renders_normalization normalization_cx ~resolved_elem_reason = function
    | DefT (r, (RendersT (NominalRenders _) as renders)) ->
      TypeCollector.add normalization_cx.type_collector (DefT (r, renders))
    | DefT
        ( r,
          RendersT (StructuralRenders { renders_variant = child_variant; renders_structural_type })
        ) ->
      if allow_child_renders ~parent_variant:normalization_cx.renders_variant ~child_variant then
        Base.List.iter
          (normalization_cx.concretize renders_structural_type)
          ~f:(on_concretized_renders_normalization normalization_cx ~resolved_elem_reason)
      else (
        TypeCollector.add normalization_cx.type_collector (AnyT.error normalization_cx.result_reason);
        merge_error_acc_with_normal_error
          normalization_cx
          (Error_message.EInvalidRendersTypeArgument
             {
               loc = normalization_cx.arg_loc;
               renders_variant =
                 ast_render_variant_of_render_variant normalization_cx.renders_variant;
               invalid_render_type_kind = Flow_intermediate_error_types.UncategorizedInvalidRenders;
               invalid_type_reasons = Nel.one r;
             }
          )
      )
    | DefT (r, RendersT DefaultRenders) ->
      TypeCollector.add normalization_cx.type_collector (AnyT.error normalization_cx.result_reason);
      merge_error_acc_with_normal_error
        normalization_cx
        (Error_message.EInvalidRendersTypeArgument
           {
             loc = normalization_cx.arg_loc;
             renders_variant = ast_render_variant_of_render_variant normalization_cx.renders_variant;
             invalid_render_type_kind = Flow_intermediate_error_types.UncategorizedInvalidRenders;
             invalid_type_reasons = Nel.one r;
           }
        )
    | t ->
      TypeCollector.add normalization_cx.type_collector (AnyT.error normalization_cx.result_reason);
      merge_error_acc_with_normal_error
        normalization_cx
        (Error_message.EInvalidRendersTypeArgument
           {
             loc = normalization_cx.arg_loc;
             renders_variant = ast_render_variant_of_render_variant normalization_cx.renders_variant;
             invalid_render_type_kind =
               Flow_intermediate_error_types.InvalidRendersStructural (TypeUtil.reason_of_t t);
             invalid_type_reasons = Nel.one resolved_elem_reason;
           }
        )

  let rec on_concretized_component_normalization normalization_cx ~resolved_elem_reason = function
    | DefT (_, PolyT { tparams_loc; tparams; t_out; id = _ }) ->
      let subst_map =
        tparams
        |> Nel.to_list
        |> Base.List.fold ~init:Subst_name.Map.empty ~f:(fun acc tparam ->
               Subst_name.Map.add tparam.name (Unsoundness.at Unchecked tparams_loc) acc
           )
      in
      on_concretized_component_normalization
        normalization_cx
        ~resolved_elem_reason
        (Type_subst.subst normalization_cx.cx subst_map t_out)
    | DefT
        ( _,
          ReactAbstractComponentT
            { component_kind = Nominal (renders_id, renders_name, _); renders = renders_super; _ }
        ) ->
      let t =
        DefT
          ( mk_reason
              (RRenderType (RType (OrdinaryName renders_name)))
              (loc_of_reason normalization_cx.result_reason),
            RendersT (NominalRenders { renders_id; renders_name; renders_super })
          )
      in
      TypeCollector.add normalization_cx.type_collector t
    | DefT (_, ReactAbstractComponentT { component_kind = Structural; renders = render_type; _ }) ->
      Base.List.iter
        (normalization_cx.concretize render_type)
        ~f:(on_concretized_renders_normalization normalization_cx ~resolved_elem_reason)
    | t ->
      TypeCollector.add normalization_cx.type_collector (AnyT.error normalization_cx.result_reason);
      merge_error_acc_with_normal_error
        normalization_cx
        (Error_message.EInvalidRendersTypeArgument
           {
             loc = normalization_cx.arg_loc;
             renders_variant = ast_render_variant_of_render_variant normalization_cx.renders_variant;
             invalid_render_type_kind =
               Flow_intermediate_error_types.InvalidRendersNonNominalElement (TypeUtil.reason_of_t t);
             invalid_type_reasons = Nel.one resolved_elem_reason;
           }
        )

  let on_concretized_bad_non_element_normalization normalization_cx = function
    | DefT (invalid_type_reason, BoolT_UNSOUND false)
    | DefT (invalid_type_reason, SingletonBoolT { value = false; _ })
    | DefT (invalid_type_reason, NullT)
    | DefT (invalid_type_reason, VoidT) ->
      TypeCollector.add normalization_cx.type_collector (AnyT.error normalization_cx.result_reason);
      merge_error_acc_with_potential_fixable_error
        normalization_cx
        (invalid_type_reason, InvalidRendersNullVoidFalse)
    | DefT (invalid_type_reason, ArrT _) ->
      merge_error_acc_with_potential_fixable_error
        normalization_cx
        (invalid_type_reason, InvalidRendersIterable)
    | t ->
      TypeCollector.add normalization_cx.type_collector (AnyT.error normalization_cx.result_reason);
      if normalization_cx.is_iterable_for_better_error t then
        merge_error_acc_with_potential_fixable_error
          normalization_cx
          (reason_of_t t, InvalidRendersIterable)
      else
        merge_error_acc_with_normal_error
          normalization_cx
          (Error_message.EInvalidRendersTypeArgument
             {
               loc = normalization_cx.arg_loc;
               renders_variant =
                 ast_render_variant_of_render_variant normalization_cx.renders_variant;
               invalid_render_type_kind = Flow_intermediate_error_types.UncategorizedInvalidRenders;
               invalid_type_reasons = Nel.one (reason_of_t t);
             }
          )

  let on_concretized_element_normalization normalization_cx = function
    | GenericT { reason = generic_reason; _ } ->
      TypeCollector.add normalization_cx.type_collector (AnyT.error normalization_cx.result_reason);
      merge_error_acc_with_normal_error
        normalization_cx
        (Error_message.EInvalidRendersTypeArgument
           {
             loc = normalization_cx.arg_loc;
             renders_variant = ast_render_variant_of_render_variant normalization_cx.renders_variant;
             invalid_render_type_kind = Flow_intermediate_error_types.InvalidRendersGenericT;
             invalid_type_reasons = Nel.one generic_reason;
           }
        )
    | DefT (reason, SingletonStrT { value = OrdinaryName "svg"; _ }) ->
      TypeCollector.add
        normalization_cx.type_collector
        (DefT (reason, RendersT (InstrinsicRenders "svg")))
    | OpaqueT
        ( element_r,
          {
            opaque_id;
            super_t = Some (DefT (_, ObjT { props_tmap; _ }));
            opaque_type_args = (_, _, component_t, _) :: (_ as _targs);
            _;
          }
        )
      when Some opaque_id = builtin_react_element_opaque_id normalization_cx.cx ->
      let c =
        match Context.find_monomorphized_component normalization_cx.cx props_tmap with
        | Some mono_component -> mono_component
        | None -> component_t
      in
      Base.List.iter
        (normalization_cx.concretize c)
        ~f:(on_concretized_component_normalization normalization_cx ~resolved_elem_reason:element_r)
    | OpaqueT
        (element_r, { opaque_id; opaque_type_args = (_, _, component_t, _) :: (_ as _targs); _ })
      when Some opaque_id = builtin_react_renders_exactly_opaque_id normalization_cx.cx ->
      Base.List.iter
        (normalization_cx.concretize component_t)
        ~f:(on_concretized_component_normalization normalization_cx ~resolved_elem_reason:element_r)
    | t -> on_concretized_bad_non_element_normalization normalization_cx t

  let normalize_render_type_argument
      cx ~arg_loc ~reason ~renders_variant ~concretize ~is_iterable_for_better_error input =
    let type_collector = TypeCollector.create () in
    let error_acc_ref = ref { potential_fixable_error_acc = None; normal_errors = [] } in
    let normalization_cx =
      {
        cx;
        type_collector;
        error_acc_ref;
        arg_loc;
        result_reason = reason;
        renders_variant;
        concretize = concretize cx;
        is_iterable_for_better_error = is_iterable_for_better_error cx;
      }
    in
    Base.List.iter
      (normalization_cx.concretize input)
      ~f:(on_concretized_element_normalization normalization_cx);
    let { potential_fixable_error_acc; normal_errors } = !error_acc_ref in
    Base.Option.iter potential_fixable_error_acc ~f:(fun (invalid_type_reasons, kind) ->
        add_output
          cx
          (Error_message.EInvalidRendersTypeArgument
             {
               loc = arg_loc;
               renders_variant = ast_render_variant_of_render_variant renders_variant;
               invalid_render_type_kind =
                 (match kind with
                 | InvalidRendersNullVoidFalse ->
                   Flow_intermediate_error_types.InvalidRendersNullVoidFalse
                 | InvalidRendersIterable -> Flow_intermediate_error_types.InvalidRendersIterable);
               invalid_type_reasons;
             }
          )
    );
    Base.List.iter normal_errors ~f:(fun e -> add_output cx e);
    let renders_structural_type = TypeCollector.collect type_collector |> union_of_ts reason in
    DefT (reason, RendersT (StructuralRenders { renders_variant; renders_structural_type }))

  let mk_non_generic_render_type
      cx reason renders_variant ?force_post_component ~concretize ~is_iterable_for_better_error t =
    Tvar.mk_fully_resolved_lazy
      cx
      reason
      ?force_post_component
      ( lazy
        (Context.run_in_signature_tvar_env cx (fun () ->
             normalize_render_type_argument
               cx
               ~arg_loc:(TypeUtil.loc_of_t t)
               ~reason
               ~renders_variant
               ~concretize
               ~is_iterable_for_better_error
               t
         )
        )
        )
end

module CalleeRecorder : sig
  type kind =
    | Tast
    | SigHelp
    | All

  val add_callee : Context.t -> kind -> Type.t -> Type.specialized_callee option -> unit

  val add_callee_use : Context.t -> kind -> Type.t -> Type.use_t -> unit

  val type_for_sig_help : Reason.t -> Type.specialized_callee -> Type.t

  val type_for_tast : Reason.t -> Type.specialized_callee -> Type.t

  val type_for_tast_opt : Reason.t -> Type.specialized_callee -> Type.t option
end = struct
  type kind =
    | Tast
    | SigHelp
    | All

  let add_tast cx l (Specialized_callee data) =
    match Context.speculation_id cx with
    | Some id
    (* It is possible that the call we are inspecting was initiated in a speculative
     * state. It is important to compare with the state during the beginning of the
     * call to determine if this is a true speculative candidate. *)
      when Some id <> data.init_speculation_state ->
      data.speculative_candidates <- (l, id) :: data.speculative_candidates
    | _ -> data.finalized <- l :: data.finalized

  (* For signature-help, we are intereseted in all branches of intersections, so
   * we include intersections in the accumulated result. Note that we discard nested
   * intersection types. These would appear under speculation, so we can effectively
   * enforce this constraint by checking that we are not in a speculation enviornment.
   * Also we skip voided out results in case of optional chaining. *)
  let add_signature_help cx l (Specialized_callee data) =
    if Base.Option.is_none (Context.speculation_id cx) then data.sig_help <- l :: data.sig_help

  let do_tast = function
    | Tast
    | All ->
      true
    | SigHelp -> false

  let do_sig_help = function
    | All
    | SigHelp ->
      true
    | Tast -> false

  let add_callee cx kind l specialized_callee =
    Base.Option.iter specialized_callee ~f:(fun specialized_callee ->
        (* Avoid recording results computed during implicit instantiation. We redo the
         * call after the instantiation portion using the concretized results. We will
         * record that latter call result. *)
        if not (Context.in_implicit_instantiation cx) then (
          if do_tast kind then add_tast cx l specialized_callee;
          if do_sig_help kind then add_signature_help cx l specialized_callee
        )
    )

  let add_callee_use cx kind l u =
    match u with
    | Type.CallT Type.{ call_action = Funcalltype { call_specialized_callee; _ }; _ } ->
      add_callee cx kind l call_specialized_callee
    | _ -> ()

  let type_for_sig_help reason specialized_callee =
    let (Specialized_callee { sig_help; _ }) = specialized_callee in
    union_of_ts reason sig_help

  let type_for_tast reason specialized_callee =
    let (Specialized_callee { finalized; _ }) = specialized_callee in
    union_of_ts reason finalized

  let type_for_tast_opt reason specialized_callee =
    let (Specialized_callee { finalized; _ }) = specialized_callee in
    match finalized with
    | [] -> None
    | _ -> Some (union_of_ts reason finalized)
end
