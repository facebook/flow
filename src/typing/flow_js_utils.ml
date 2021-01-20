(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Constraint
open Reason
open Subst
open Type
open TypeUtil
open Utils_js
module FlowError = Flow_error

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
  let possible_types = possible_types in
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
      (* Ignore empty in existentials. This behavior is sketchy, but the error
           behavior without this filtering is worse. If an existential accumulates
           an empty, we error but it's very non-obvious how the empty arose. *)
      | DefT (_, _, EmptyT flavor) when filter_empty flavor ->
        collect_lowers ~filter_empty cx seen acc ts
      (* Everything else becomes part of the merge typed *)
      | _ -> collect_lowers ~filter_empty cx seen (t :: acc) ts)
  in
  fun ?filter_empty cx r id ->
    (* Because the behavior of existentials are so difficult to predict, they
         enjoy some special casing here. When existential types are finally
         removed, this logic can be removed. *)
    let existential =
      Reason.(
        match desc_of_reason r with
        | RExistential -> true
        | _ -> false)
    in
    let filter_empty flavor =
      existential
      ||
      match filter_empty with
      | Some filter_empty -> filter_empty flavor
      | None -> false
    in
    let lowers =
      let seen = ISet.singleton id in
      collect_lowers cx seen [] (possible_types cx id) ~filter_empty
    in
    match lowers with
    | [t] -> t
    | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)
    | [] ->
      let uses = possible_uses cx id in
      if uses = [] || existential then
        AnyT.locationless Unsoundness.existential
      else
        MergedT (r, uses)

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
  | "isPrototypeOf"
  | "hasOwnProperty"
  | "propertyIsEnumerable"
  | "toLocaleString"
  | "toString"
  | "valueOf" ->
    true
  | _ -> false

(* This must list all of the properties on Function.prototype. *)
let is_function_prototype = function
  | "apply"
  | "bind"
  | "call"
  | "arguments"
  | "caller"
  | "length"
  | "name" ->
    true
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
  | (DefT (_, _, EmptyT _), _)
  | (_, DefT (_, _, EmptyT _))
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
  | ReadProp { use_op; _ } -> Some use_op
  | WriteProp { use_op; _ } -> Some use_op
  | LookupProp (use_op, _) -> Some use_op
  | SuperProp (use_op, _) -> Some use_op
  | MatchProp (use_op, _) -> Some use_op

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
      Some SMap.empty
    | _ -> None
  in
  match statics_own_props with
  | Some statics_own_props ->
    let props_not_found =
      SMap.filter
        (fun x p ->
          let optional =
            match p with
            | Field (_, OptionalT _, _) -> true
            | _ -> false
          in
          not (optional || is_function_prototype x || SMap.mem x statics_own_props))
        props
    in
    SMap.iter
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
    not (SMap.is_empty props_not_found)
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

(* Every context has a local reference to builtins (along with local references
   to other modules that are discovered during type checking, such as modules
   required by it, the module it provides, and so on). *)
let mk_builtins cx =
  let builtins = Tvar.mk cx (builtin_reason (RCustom "module")) in
  Context.add_module cx Files.lib_module_ref builtins

(* Local references to modules can be looked up. *)
let lookup_module cx m = Context.find_module cx m

(* The builtins reference is accessed just like references to other modules. *)
let builtins cx = lookup_module cx Files.lib_module_ref

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
