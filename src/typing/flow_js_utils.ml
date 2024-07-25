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
let possible_types cx id = types_of cx (Context.find_graph cx id) |> List.filter is_proper_def

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

let merge_tvar_opt ?(filter_empty = false) cx r id =
  let lowers =
    let seen = ISet.singleton id in
    collect_lowers cx seen [] (possible_types cx id) ~filter_empty
  in
  match lowers with
  | [t] -> Some t
  | t0 :: t1 :: ts -> Some (UnionT (r, UnionRep.make t0 t1 ts))
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
        | DefT (_, NumT _)
        | DefT (_, StrT _)
        | DefT (_, BoolT _)
        | DefT (_, BigIntT _)
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
        | ModuleT _
        | CustomFunT _
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
        | NamespaceT _
        | InternalEnforceUnionOptimizedT _ ->
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

(* NOTE: The following function looks similar to TypeUtil.quick_subtype, but is in fact more
   complicated: it avoids deep structural checks, admits `any`, etc. It might be worth it to
   simplify this function later. *)
let ground_subtype = function
  (* tvars are not considered ground, so they're not part of this relation *)
  | (OpenT _, _)
  | (_, UseT (_, OpenT _)) ->
    false
  | (UnionT _, _) -> false
  | (DefT (_, NumT _), UseT (_, DefT (_, NumT _)))
  | (DefT (_, StrT _), UseT (_, DefT (_, StrT _)))
  | (DefT (_, BoolT _), UseT (_, DefT (_, BoolT _)))
  | (DefT (_, BigIntT _), UseT (_, DefT (_, BigIntT _)))
  | (DefT (_, SymbolT), UseT (_, DefT (_, SymbolT)))
  | (DefT (_, NullT), UseT (_, DefT (_, NullT)))
  | (DefT (_, VoidT), UseT (_, DefT (_, VoidT))) ->
    true
  | ( StrUtilT { reason = _; op = StrPrefix prefix1; remainder = _ },
      UseT (_, StrUtilT { reason = _; op = StrPrefix prefix2; remainder = None })
    )
    when String.starts_with ~prefix:prefix2 prefix1 ->
    true
  | ( DefT (_, StrT (Literal (None, OrdinaryName s))),
      UseT (_, StrUtilT { reason = _; op = StrPrefix prefix; remainder = None })
    )
    when String.starts_with ~prefix s ->
    true
  | ( StrUtilT { reason = _; op = StrSuffix suffix1; remainder = _ },
      UseT (_, StrUtilT { reason = _; op = StrSuffix suffix2; remainder = None })
    )
    when String.ends_with ~suffix:suffix2 suffix1 ->
    true
  | ( DefT (_, StrT (Literal (None, OrdinaryName s))),
      UseT (_, StrUtilT { reason = _; op = StrSuffix suffix; remainder = None })
    )
    when String.ends_with ~suffix s ->
    true
  | ( StrUtilT { reason = _; op = StrPrefix arg | StrSuffix arg; remainder = _ },
      UseT (_, DefT (_, StrT Truthy))
    )
    when arg <> "" ->
    true
  | (StrUtilT _, UseT (_, DefT (_, StrT AnyLiteral))) -> true
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
  | CustomFunT _
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

let strict_equatable_error cond_context (l, r) =
  let comparison_error =
    lazy
      (match cond_context with
      | Some (SwitchTest { case_test_loc; switch_discriminant_loc }) ->
        let use_op =
          Op (SwitchRefinementCheck { test = case_test_loc; discriminant = switch_discriminant_loc })
        in
        Error_message.EIncompatibleWithUseOp
          { reason_lower = reason_of_t l; reason_upper = reason_of_t r; use_op }
      | _ ->
        let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
        Error_message.EComparison reasons)
  in
  match (l, r) with
  | (AnyT _, _)
  | (_, AnyT _) ->
    None
  (* We allow comparison between enums and enum values with the same id. *)
  | ( DefT (_, EnumObjectT { enum_info = ConcreteEnum { enum_id = id1; _ }; _ }),
      DefT (_, EnumObjectT { enum_info = ConcreteEnum { enum_id = id2; _ }; _ })
    )
  | ( DefT (_, EnumValueT (ConcreteEnum { enum_id = id1; _ })),
      DefT (_, EnumValueT (ConcreteEnum { enum_id = id2; _ }))
    )
    when ALoc.equal_id id1 id2 ->
    None
  (* We allow comparison between abstract and concrete enums and enum values. *)
  | (DefT (_, EnumObjectT _), DefT (_, EnumObjectT { enum_info = AbstractEnum _; _ }))
  | (DefT (_, EnumObjectT { enum_info = AbstractEnum _; _ }), DefT (_, EnumObjectT _))
  | (DefT (_, EnumValueT _), DefT (_, EnumValueT (AbstractEnum _)))
  | (DefT (_, EnumValueT (AbstractEnum _)), DefT (_, EnumValueT _)) ->
    None
  (* We allow the comparison of enums to null and void outside of switches. *)
  | (DefT (_, EnumValueT _), DefT (_, (NullT | VoidT)))
  | (DefT (_, (NullT | VoidT)), DefT (_, EnumValueT _)) -> begin
    match cond_context with
    | Some (SwitchTest _) -> Some (Lazy.force comparison_error)
    | None
    | Some _ ->
      None
  end
  (* We don't allow the comparison of enums and other types in general. *)
  | (DefT (_, EnumValueT _), _)
  | (_, DefT (_, EnumValueT _))
  | (DefT (_, EnumObjectT _), _)
  | (_, DefT (_, EnumObjectT _)) ->
    Some (Lazy.force comparison_error)
  (* We don't check other strict equality comparisons. *)
  | _ -> None

let strict_equatable cond_context args =
  strict_equatable_error cond_context args |> Base.Option.is_none

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
  | ElemT (_, _, DefT (_, ArrT _), _) -> Error_message.IncompatibleElemTOfArrT
  | ObjAssignFromT (_, _, _, _, ObjSpreadAssign) -> Error_message.IncompatibleObjAssignFromTSpread
  | ObjAssignFromT _ -> Error_message.IncompatibleObjAssignFromT
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
        ( DefT (_, StrT (Literal (_, name)))
        | GenericT { bound = DefT (_, StrT (Literal (_, name))); _ } )
      ) ->
    Error_message.IncompatibleHasOwnPropT (loc_of_reason r, Some name)
  | HasOwnPropT (_, r, _) -> Error_message.IncompatibleHasOwnPropT (loc_of_reason r, None)
  | GetValuesT _ -> Error_message.IncompatibleGetValuesT
  | GetDictValuesT _ -> Error_message.IncompatibleGetValuesT
  | MapTypeT (_, _, (ObjectMap _ | ObjectMapi _ | ObjectMapConst _ | ObjectKeyMirror), _) ->
    Error_message.IncompatibleMapTypeTObject
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

(** TODO: (1) Define a more general partial equality, that takes into
    account unified type variables. (2) Get rid of UnionRep.quick_mem. **)
let union_optimization_guard =
  let unwrap_type cx t = Base.Option.value (Context.find_resolved cx t) ~default:t in
  (* Compare l to u. Flatten both unions and then check that each element
     of l is comparable to an element of u. Note that the comparator need not
     be symmetric. *)
  let union_compare cx ~equiv comparator lts uts =
    if Context.is_verbose cx then prerr_endline "union_compare slow";
    let ts2 = Type_mapper.union_flatten cx uts in
    Type_mapper.union_flatten cx lts
    |> Base.List.for_all ~f:(fun t1 ->
           if equiv then
             Base.List.for_all ~f:(comparator t1) ts2
           else
             Base.List.exists ~f:(comparator t1) ts2
       )
  in
  (* The equiv bool flag customizes the how the comparator is run on the each pair of element (l, u)
     of two unions (ls, us).
     - When equiv=true, all pairs must satisfy `l comparator u` in order for the guard to return true.
     - When equiv=false, all l must have at least one u such that `l comparator u` in order for the
       guard to return true.
  *)
  let rec union_optimization_guard_impl seen cx ~equiv comparator l u =
    match (l, u) with
    | (UnionT (_, rep1), UnionT (_, rep2)) ->
      UnionRep.same_source rep1 rep2
      || UnionRep.same_structure rep1 rep2
      || (* Try O(n) check, then O(n log n) check, then O(n^2) check *)
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
            let guard u =
              (not (TypeSet.mem u seen))
              && union_optimization_guard_impl (TypeSet.add u seen) cx ~equiv comparator l u
            in
            if equiv then
              Base.List.for_all ~f:guard uts
            else
              Base.List.exists ~f:guard uts
          then
            true
          else
            union_compare cx ~equiv comparator lts uts
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
  DefT (key_reason, StrT (Literal (None, s)))

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
  let builtins = Context.builtins cx in
  match Builtins.get_builtin_value_opt builtins x with
  | Some t -> Ok (TypeUtil.mod_reason_of_t (Base.Fn.const reason) t)
  | None -> lookup_builtin_name_error x (loc_of_reason reason)

let lookup_builtin_type_result cx x reason =
  let builtins = Context.builtins cx in
  match Builtins.get_builtin_type_opt builtins x with
  | Some t -> Ok (TypeUtil.mod_reason_of_t (Base.Fn.const reason) t)
  | None -> lookup_builtin_name_error x (loc_of_reason reason)

let apply_env_errors cx loc = function
  | Ok t -> t
  | Error (t, errs) ->
    Nel.iter (emit_cacheable_env_error cx loc) errs;
    t

let lookup_builtin_value cx x reason =
  lookup_builtin_value_result cx x reason |> apply_env_errors cx (loc_of_reason reason)

let lookup_builtin_type cx x reason =
  lookup_builtin_type_result cx x reason |> apply_env_errors cx (loc_of_reason reason)

let lookup_builtin_value_opt cx x =
  let builtins = Context.builtins cx in
  Builtins.get_builtin_value_opt builtins x

let lookup_builtin_type_opt cx x =
  let builtins = Context.builtins cx in
  Builtins.get_builtin_type_opt builtins x

let lookup_builtin_typeapp cx reason x targs =
  let t = lookup_builtin_type cx x reason in
  typeapp ~from_value:false ~use_desc:false reason t targs

let get_builtin_module cx module_name reason =
  let builtins = Context.builtins cx in
  match Builtins.get_builtin_module_opt builtins module_name with
  | Some t -> t
  | None -> lookup_builtin_module_error cx module_name reason

let builtin_promise_class_id cx =
  let promise_t = lookup_builtin_value_opt cx "Promise" in
  match promise_t with
  | Some (OpenT (_, id)) ->
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
  let t = lookup_builtin_type_opt cx class_ref in
  match t with
  | Some (OpenT (_, id)) ->
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
  let t_opt = lookup_builtin_type_opt cx "React$Element" in
  match t_opt with
  | Some (OpenT (_, id)) ->
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

let map_obj cx o reason_op ~map_t ~map_field =
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

let obj_key_mirror cx o reason_op =
  let map_t key t =
    match t with
    | OptionalT _ -> optional key
    | _ -> key
  in
  let map_field key t =
    let reason = replace_desc_reason (RStringLit key) reason_op in
    map_t (DefT (reason, SingletonStrT key)) t
  in
  map_obj cx o reason_op ~map_t ~map_field

let obj_map_const cx o reason_op target =
  let map_t _ t =
    match t with
    | OptionalT _ -> optional target
    | _ -> target
  in
  let map_field _ t = map_t target t in
  map_obj cx o reason_op ~map_t ~map_field

let namespace_type cx reason namespace_symbol values types =
  let add name { preferred_def_locs; name_loc; type_ } acc =
    NameUtils.Map.add
      name
      (Field { preferred_def_locs; key_loc = name_loc; type_; polarity = Polarity.Positive })
      acc
  in
  let props = NameUtils.Map.fold add values NameUtils.Map.empty in
  let proto = ObjProtoT reason in
  let values_type = Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~frozen:true ~props proto in
  let types_tmap =
    Context.generate_property_map cx (NameUtils.Map.fold add types NameUtils.Map.empty)
  in
  NamespaceT { namespace_symbol; values_type; types_tmap }

let obj_is_readonlyish { Type.react_dro; frozen; _ } = Base.Option.is_some react_dro || frozen

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
  (* a component syntax value annotation becomes an element of that component *)
  let run_on_abstract_component cx reason_component reason_op l =
    let elem_reason =
      let desc = react_element_desc_of_component_reason reason_component in
      let annot_loc = loc_of_reason reason_op in
      annot_reason ~annot_loc (replace_desc_reason desc reason_op)
    in
    let t =
      Tvar.mk_fully_resolved cx elem_reason (lookup_builtin_type cx "React$Element" elem_reason)
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

  val export_named_fresh_var :
    Context.t ->
    Reason.t * Type.named_symbol NameUtils.Map.t * Type.named_symbol NameUtils.Map.t * export_kind ->
    Type.t ->
    Type.t

  val export_type :
    Context.t ->
    reason
    * (* name_loc *)
    ALoc.t option
    * (* preferred_def_locs *)
    ALoc.t Nel.t option
    * name (* export_name *)
    * t (* target_module_t *) ->
    Type.t ->
    Type.t

  val cjs_extract_named_exports : Context.t -> Reason.t * Type.moduletype -> Type.t -> Type.t

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
              id;
            }
        ) ->
      let typeof_t = TypeUtil.typeof_annotation reason lower_t None in

      poly_type id tparams_loc typeparams (DefT (reason, TypeT (ImportTypeofKind, typeof_t)))
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
  let on_ModuleT cx ~reposition (reason, module_symbol, is_strict, legacy_interop) module_ =
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
    | Some t ->
      (* reposition the export to point at the require(), like the object
         we create below for non-CommonJS exports *)
      reposition cx (loc_of_reason reason) t
    | None ->
      (* Use default export if option is enabled and module is not lib *)
      let automatic_require_default =
        (legacy_interop || Context.automatic_require_default cx)
        && not (is_lib_reason_def module_reason)
      in
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
          Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~frozen:true ~props:value_props proto
        in
        let types_tmap = Context.generate_property_map cx type_props in
        NamespaceT { namespace_symbol = module_symbol; values_type; types_tmap }
      in
      if automatic_require_default then
        match NameUtils.Map.find_opt (OrdinaryName "default") value_exports_tmap with
        | Some { preferred_def_locs = _; name_loc = _; type_ } -> type_
        | _ -> mk_exports_namespace ()
      else
        mk_exports_namespace ()
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
        | Some type_ ->
          (* TODO this Field should probably have a location *)
          let p =
            Field { preferred_def_locs = None; key_loc = None; type_; polarity = Polarity.Positive }
          in
          NameUtils.Map.add (OrdinaryName "default") p value_props
        | None -> value_props
    in
    let obj_kind =
      if exports.has_every_named_export then
        Indexed
          {
            key = StrT.why reason;
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
    let values_type =
      Obj_type.mk_with_proto cx reason ~obj_kind ~frozen:true ~props:value_props proto
    in
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
      | Some t -> (None, t)
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
      | Some type_ ->
        NameUtils.Map.add
          (OrdinaryName "default")
          { preferred_def_locs = None; name_loc = None; type_ }
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
module CopyNamedExportsT_kit (F : Import_export_helper_sig) = struct
  let on_ModuleT cx (reason, target_module_t) module_ : F.r =
    let {
      module_reason = _;
      module_export_types = source_exports;
      module_is_strict = _;
      module_available_platforms = _;
    } =
      module_
    in
    F.export_named
      cx
      ( reason,
        Context.find_exports cx source_exports.value_exports_tmap,
        Context.find_exports cx source_exports.type_exports_tmap,
        ReExport
      )
      target_module_t

  (* There is nothing to copy from a module exporting `any` or `Object`. *)
  let on_AnyT cx target_module = F.return cx target_module
end

(* Copy only the type exports from a source module into a target module.
 * Used to implement `export type * from ...`. *)
module CopyTypeExportsT_kit (F : Import_export_helper_sig) = struct
  let on_ModuleT cx (reason, target_module_t) module_ =
    let {
      module_reason = _;
      module_export_types = source_exports;
      module_is_strict = _;
      module_available_platforms = _;
    } =
      module_
    in
    let export_all exports_tmap =
      NameUtils.Map.fold
        (fun export_name { name_loc; preferred_def_locs; type_ } target_module_t ->
          F.export_type
            cx
            (reason, name_loc, preferred_def_locs, export_name, target_module_t)
            type_)
        (Context.find_exports cx exports_tmap)
    in
    let target_module_t =
      target_module_t
      |> export_all source_exports.value_exports_tmap
      |> export_all source_exports.type_exports_tmap
    in
    F.return cx target_module_t

  (* There is nothing to copy from a module exporting `any` or `Object`. *)
  let on_AnyT cx target_module = F.return cx target_module
end

(* Export a type from a given ModuleT, but only if the type is compatible
 * with `import type`/`export type`. When it is not compatible, it is simply
 * not added to the exports map.
 *
 * Note that this is very similar to `ExportNamedT` except that it only
 * exports one type at a time and it takes the type to be exported as a
 * lower (so that the type can be filtered post-resolution). *)
module ExportTypeT_kit (F : Import_export_helper_sig) = struct
  let on_concrete_type cx (reason, name_loc, preferred_def_locs, export_name, target_module_t) l =
    let is_type_export =
      match l with
      | DefT (_, ObjT _) when export_name = OrdinaryName "default" -> true
      | l -> ImportTypeTKit.canonicalize_imported_type cx reason l <> None
    in
    if is_type_export then
      let named = NameUtils.Map.singleton export_name { preferred_def_locs; name_loc; type_ = l } in
      F.export_named cx (reason, NameUtils.Map.empty, named, ReExport) target_module_t
    else
      F.return cx target_module_t
end

module CJSExtractNamedExportsT_kit (F : Import_export_helper_sig) = struct
  let on_concrete_type cx (reason, local_module) = function
    | NamespaceT { namespace_symbol = _; values_type; types_tmap } ->
      (* Copy props from the values part *)
      let module_t = F.cjs_extract_named_exports cx (reason, local_module) values_type in
      (* Copy type exports *)
      F.export_named
        cx
        ( reason,
          NameUtils.Map.empty,
          Properties.extract_named_exports (Context.find_props cx types_tmap),
          DirectExport
        )
        module_t
    (* ObjT CommonJS export values have their properties turned into named exports. *)
    | DefT (_, ObjT o) ->
      let { props_tmap; proto_t; _ } = o in
      (* Copy props from the prototype *)
      let module_t = F.cjs_extract_named_exports cx (reason, local_module) proto_t in
      (* Copy own props *)
      F.export_named
        cx
        ( reason,
          Properties.extract_named_exports (Context.find_props cx props_tmap),
          NameUtils.Map.empty,
          DirectExport
        )
        module_t
    (* InstanceT CommonJS export values have their properties turned into named exports. *)
    | DefT (_, InstanceT { inst = { own_props; proto_props; _ }; _ }) ->
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
          (reason, extract_named_exports own_props, NameUtils.Map.empty, DirectExport)
          module_t
      in
      (* Copy proto props *)
      (* TODO: own props should take precedence *)
      F.export_named
        cx
        (reason, extract_named_exports proto_props, NameUtils.Map.empty, DirectExport)
        module_t
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
      let module_t =
        ModuleT
          {
            module_reason;
            module_export_types = { exporttypes with has_every_named_export = true };
            module_is_strict;
            module_available_platforms;
          }
      in
      F.return cx module_t
    (* All other CommonJS export value types do not get merged into the named
     * exports tmap in any special way. *)
    | _ -> F.return cx (ModuleT local_module)
end

(*******************)
(* GetPropT helper *)
(*******************)

let rec unbind_this_method = function
  | DefT (r, FunT (static, ({ this_t = (this_t, This_Method { unbound = false }); _ } as ft))) ->
    DefT (r, FunT (static, { ft with this_t = (this_t, This_Method { unbound = true }) }))
  | DefT (r, PolyT { tparams_loc; tparams; t_out; id }) ->
    DefT (r, PolyT { tparams_loc; tparams; t_out = unbind_this_method t_out; id })
  | IntersectionT (r, rep) -> IntersectionT (r, InterRep.map unbind_this_method rep)
  | t -> t

let check_method_unbinding cx ~use_op ~method_accessible ~reason_op ~propref ~hint p =
  match p with
  | Method { key_loc; type_ = t }
    when (not method_accessible)
         && not (Context.allowed_method_unbinding cx (Reason.loc_of_reason reason_op)) ->
    let hint_result = (snd hint) reason_op in
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
      Method { key_loc; type_ = unbind_this_method t })
  | _ -> p

(* e.g. `0`, `-123, `234234` *)
let int_regex = Str.regexp {|^-?\(0\|[1-9][0-9]*\)$|}

let is_str_intlike str = Str.string_match int_regex str 0

let type_of_key_name cx name reason =
  let str_key () =
    let key_reason = replace_desc_reason (RPropertyIsAString name) reason in
    DefT (key_reason, StrT (Literal (None, name)))
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

  let get_obj_prop cx trace o propref reason_op =
    let named_prop =
      match propref with
      | Named { name; _ } -> Context.get_prop cx o.props_tmap name
      | Computed _ -> None
    in
    let dict_t = Obj_type.get_dict_opt o.flags.obj_kind in
    match (propref, named_prop, dict_t) with
    | (_, Some prop, _) ->
      (* Property exists on this property map *)
      Some (Property.type_ prop, PropertyMapProperty)
    | (Named { name; _ }, None, Some { key; value; dict_polarity; _ })
      when not (is_dictionary_exempt name) ->
      (* Dictionaries match all property reads *)
      F.dict_read_check cx trace ~use_op:unknown_use (type_of_key_name cx name reason_op, key);
      Some (OrdinaryField { type_ = value; polarity = dict_polarity }, IndexerProperty)
    | (Computed k, None, Some { key; value; dict_polarity; _ }) ->
      F.dict_read_check cx trace ~use_op:unknown_use (k, key);
      Some (OrdinaryField { type_ = value; polarity = dict_polarity }, IndexerProperty)
    | _ -> None

  let read_obj_prop cx trace ~use_op o propref reason_obj reason_op lookup_info =
    let l = DefT (reason_obj, ObjT o) in
    match get_obj_prop cx trace o propref reason_op with
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
        | GenericT { bound = DefT (_, StrT (Literal _)); _ }
        | DefT (_, StrT (Literal _)) ->
          let loc = loc_of_t elem_t in
          add_output cx Error_message.(EInternal (loc, PropRefComputedLiteral));
          F.error_type cx trace reason_op
        | AnyT (_, src) -> F.return cx trace ~use_op:unknown_use (AnyT.why src reason_op)
        | GenericT { bound = DefT (_, NumT lit); _ }
        | DefT (_, NumT lit) ->
          let reason_prop = reason_of_t elem_t in
          let kind = Flow_intermediate_error_types.InvalidObjKey.kind_of_num_lit lit in
          add_output cx (Error_message.EObjectComputedPropertyAccess (reason_op, reason_prop, kind));
          F.error_type cx trace reason_op
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

let array_elem_check ~write_action cx l use_op reason reason_tup arrtype =
  let (elem_t, elements, is_index_restricted, is_tuple, tuple_is_inexact, react_dro) =
    match arrtype with
    | ArrayAT { elem_t; tuple_view; react_dro } ->
      let elements =
        Base.Option.map
          ~f:(fun (TupleView { elements; arity = _; inexact = _ }) -> elements)
          tuple_view
      in
      (elem_t, elements, false, false, false, react_dro)
    | TupleAT { elem_t; elements; arity = _; inexact; react_dro } ->
      (elem_t, Some elements, true, true, inexact, react_dro)
    | ROArrayAT (elem_t, react_dro) -> (elem_t, None, true, false, false, react_dro)
  in
  let (can_write_tuple, value, use_op) =
    match l with
    | DefT (index_reason, NumT (Literal (_, (float_value, _)))) -> begin
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

let propref_for_elem_t = function
  | OpaqueT (reason, { super_t = Some (DefT (_, SingletonStrT name)); _ })
  | GenericT { bound = DefT (_, StrT (Literal (_, name))); reason; _ }
  | DefT (reason, StrT (Literal (_, name))) ->
    let reason = replace_desc_reason (RProperty (Some name)) reason in
    mk_named_prop ~reason ~from_indexed_access:true name
  | OpaqueT (reason_num, { super_t = Some (DefT (_, SingletonNumT (value, raw))); _ })
  | GenericT { bound = DefT (_, NumT (Literal (_, (value, raw)))); reason = reason_num; _ }
  | DefT (reason_num, NumT (Literal (_, (value, raw))))
    when Js_number.is_float_safe_integer value ->
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
        DefT (reason, SingletonStrT name) :: acc
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
        | Some t ->
          let t =
            if flags.frozen then
              match t with
              | DefT (t_reason, StrT (Literal (_, (OrdinaryName _ as name)))) ->
                let t_reason = replace_desc_reason (RStringLit name) t_reason in
                DefT (t_reason, SingletonStrT name)
              | DefT (t_reason, NumT (Literal (_, lit))) ->
                let t_reason = replace_desc_reason (RNumberLit (snd lit)) t_reason in
                DefT (t_reason, SingletonNumT lit)
              | DefT (t_reason, BoolT (Some lit)) ->
                let t_reason = replace_desc_reason (RBooleanLit lit) t_reason in
                DefT (t_reason, SingletonBoolT lit)
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

let flow_unary_arith cx l reason kind =
  let open UnaryArithKind in
  match (kind, l) with
  | (Minus, DefT (_, NumT (Literal (_, (value, raw))))) ->
    let (value, raw) = Flow_ast_utils.negate_number_literal (value, raw) in
    DefT (replace_desc_reason RNumber reason, NumT (Literal (None, (value, raw))))
  | (Minus, DefT (_, NumT (AnyLiteral | Truthy))) -> l
  | (Minus, DefT (_, BigIntT (Literal (_, (value, raw))))) ->
    let (value, raw) = Flow_ast_utils.negate_bigint_literal (value, raw) in
    DefT (replace_desc_reason RBigInt reason, BigIntT (Literal (None, (value, raw))))
  | (Minus, DefT (_, BigIntT (AnyLiteral | Truthy))) -> l
  | (Plus, DefT (reason_bigint, BigIntT _)) ->
    add_output cx (Error_message.EBigIntNumCoerce reason_bigint);
    AnyT.error reason
  | (Plus, _) -> NumT.why reason
  | (BitNot, DefT (_, NumT _)) -> NumT.why reason
  | (BitNot, DefT (_, BigIntT _)) -> BigIntT.why reason
  | (Update, DefT (_, NumT _)) -> NumT.why reason
  | (Update, DefT (_, BigIntT _)) -> BigIntT.why reason
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
  | (_, DefT (_, NumT _), DefT (_, NumT _)) -> NumT.why reason
  | (RShift3, DefT (reason, BigIntT _), _) ->
    add_output cx (Error_message.EBigIntRShift3 reason);
    AnyT.error reason
  (* bigint <> bigint *)
  | (_, DefT (_, BigIntT _), DefT (_, BigIntT _)) -> BigIntT.why reason
  (* str + str *)
  (* str + num *)
  (* num + str *)
  | (Plus, DefT (_, StrT _), DefT (_, StrT _))
  | (Plus, DefT (_, StrT _), DefT (_, NumT _))
  | (Plus, DefT (_, NumT _), DefT (_, StrT _)) ->
    StrT.why reason
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
 * In order to prevent this in common mapped type cases, we do a best-effort check to
 * see if the RHS contains a mapped type. This traversal is extremely limited and does
 * not attempt to be exhaustive.
 *)
let rec wraps_mapped_type cx = function
  | EvalT (_, TypeDestructorT (_, _, MappedType _), _) -> true
  | DefT (_, TypeT (_, t)) -> wraps_mapped_type cx t
  | OpenT (_, id) ->
    let (_, constraints) = Context.find_constraints cx id in
    (match constraints with
    | FullyResolved s -> wraps_mapped_type cx (Context.force_fully_resolved_tvar cx s)
    | Resolved t -> wraps_mapped_type cx t
    | _ -> false)
  | DefT (_, PolyT { t_out; _ }) -> wraps_mapped_type cx t_out
  | TypeAppT { reason = _; use_op = _; type_; targs = _; from_value = _; use_desc = _ } ->
    wraps_mapped_type cx type_
  | _ -> false

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

let mk_renders_type cx reason renders_variant ~mk_type_destructor t =
  mk_type_destructor
    cx
    unknown_use (* not used *)
    reason
    t
    (ReactPromoteRendersRepresentation
       {
         should_distribute = true;
         promote_structural_components = false;
         renders_variant;
         resolved_elem = None;
       }
    )
    (Eval.generate_id ())

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
    | Type.CallT
        Type.
          {
            call_action = Funcalltype { call_kind = RegularCallKind; call_specialized_callee; _ };
            _;
          } ->
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
