(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module describes the subtyping algorithm that forms the core of
   typechecking. The algorithm (in its basic form) is described in Francois
   Pottier's thesis. The main data structures maintained by the algorithm are:
   (1) for every type variable, which type variables form its lower and upper
   bounds (i.e., flow in and out of the type variable); and (2) for every type
   variable, which concrete types form its lower and upper bounds. Every new
   subtyping constraint added to the system is deconstructed into its subparts,
   until basic flows between type variables and other type variables or concrete
   types remain; these flows are then viewed as links in a chain, bringing
   together further concrete types and type variables to participate in
   subtyping. This process continues till a fixpoint is reached---which itself
   is guaranteed to exist, and is usually reached in very few steps. *)

open Utils_js
open Loc_collections
open Reason
open Constraint
open Type
open TypeUtil
open Debug_js.Verbose
module FlowError = Flow_error

(* type exemplar set - reasons are not considered in compare *)
module TypeExSet = Set.Make (struct
  include Type

  let compare = reasonless_compare
end)

(**************************************************************)

(* Check that id1 is not linked to id2. *)
let not_linked (id1, _bounds1) (_id2, bounds2) =
  (* It suffices to check that id1 is not already in the lower bounds of
     id2. Equivalently, we could check that id2 is not already in the upper
     bounds of id1. *)
  not (IMap.mem id1 bounds2.lowertvars)

(**********)
(* frames *)
(**********)

(* note: this is here instead of Env because of circular deps:
  Env is downstream of Flow_js due general utility funcs such as
  Tvar.mk and builtins services. If the flow algorithm can
  be split away from these, then Env can be moved upstream and
  this code can be merged into it. *)

(* background:
   - each scope has an id. scope ids are unique, mod cloning
   for path-dependent analysis.
   - an environment is a scope list
   - each context holds a map of environment snapshots, keyed
   by their topmost scope ids
   - every function type contains a frame id, which maps to
   the environment in which it was defined; as well as a
   changeset containing its reads/writes/refinements on
   closed-over variables

   Given frame ids for calling function and called function and
   the changeset of the called function, here we retrieve the
   environment snapshots for the two functions, find the prefix
   of scopes they share, and havoc the variables in the called
   function's write set which live in those scopes.
 *)
let havoc_call_env =
  Scope.(
    let overlapped_call_scopes func_env call_env =
      let rec loop = function
        | (func_scope :: func_scopes, call_scope :: call_scopes) when func_scope.id = call_scope.id
          ->
          call_scope :: loop (func_scopes, call_scopes)
        | _ -> []
      in
      loop (List.rev func_env, List.rev call_env)
    in
    let havoc_entry cx scope ((_, name, _) as entry_ref) =
      if Context.is_verbose cx then
        prerr_endlinef
          "%shavoc_entry %s %s"
          (Context.pid_prefix cx)
          (Changeset.string_of_entry_ref entry_ref)
          (Debug_js.string_of_scope cx scope);
      match get_entry name scope with
      | Some _ ->
        havoc_entry name scope;
        Changeset.(if Global.is_active () then Global.change_var entry_ref)
      | None ->
        (* global scopes may lack entries, if function closes over
         path-refined global vars (artifact of deferred lookup) *)
        if is_global scope then
          ()
        else
          assert_false
            (spf
               "missing entry %S in scope %d: { %s }"
               name
               scope.id
               (String.concat ", " (SMap.fold (fun n _ acc -> n :: acc) scope.entries [])))
    in
    let havoc_refi cx scope ((_, key, _) as refi_ref) =
      if Context.is_verbose cx then
        prerr_endlinef
          "%shavoc_refi %s"
          (Context.pid_prefix cx)
          (Changeset.string_of_refi_ref refi_ref);
      match get_refi key scope with
      | Some _ ->
        havoc_refi key scope;
        Changeset.(if Global.is_active () then Global.change_refi refi_ref)
      | None ->
        (* global scopes may lack entries, if function closes over
         path-refined global vars (artifact of deferred lookup) *)
        if is_global scope then
          ()
        else
          assert_false
            (spf
               "missing refi %S in scope %d: { %s }"
               (Key.string_of_key key)
               scope.id
               (String.concat
                  ", "
                  (Key_map.fold (fun k _ acc -> Key.string_of_key k :: acc) scope.refis [])))
    in
    fun cx func_frame call_frame changeset ->
      if func_frame = 0 || call_frame = 0 || Changeset.is_empty changeset then
        ()
      else
        let func_env = IMap.find_opt func_frame (Context.envs cx) in
        let call_env = IMap.find_opt call_frame (Context.envs cx) in
        Base.Option.iter (Base.Option.both func_env call_env) ~f:(fun (func_env, call_env) ->
            overlapped_call_scopes func_env call_env
            |> List.iter (fun ({ id; _ } as scope) ->
                   Changeset.include_scopes [id] changeset
                   |> Changeset.iter_writes (havoc_entry cx scope) (havoc_refi cx scope))))

(********************************************************************)

(* visit an optional evaluated type at an evaluation id *)
let visit_eval_id cx id f =
  match Eval.Map.find_opt id (Context.evaluated cx) with
  | None -> ()
  | Some t -> f t

(***************)
(* strict mode *)
(***************)

(* For any constraints, return a list of def types that form either the lower
   bounds of the solution, or a singleton containing the solution itself. *)
let types_of constraints =
  match constraints with
  | Unresolved { lower; _ } -> TypeMap.keys lower
  | Resolved (_, t)
  | FullyResolved (_, t) ->
    [t]

(* Def types that describe the solution of a type variable. *)
let possible_types cx id = types_of (Context.find_graph cx id) |> List.filter is_proper_def

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let uses_of constraints =
  match constraints with
  | Unresolved { upper; _ } -> UseTypeMap.keys upper
  | Resolved (use_op, t)
  | FullyResolved (use_op, t) ->
    [UseT (use_op, t)]

let possible_uses cx id = uses_of (Context.find_graph cx id) |> List.filter is_proper_use

(**************)
(* builtins *)
(**************)

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

module ImplicitTypeArgument = Instantiation_utils.ImplicitTypeArgument
module TypeAppExpansion = Instantiation_utils.TypeAppExpansion
module Cache = Flow_cache

(*********************************************************************)

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

(********************)
(* subtype relation *)
(********************)

(* Sometimes we expect types to be def types. For example, when we see a flow
   constraint from type l to type u, we expect l to be a def type. As another
   example, when we see a unification constraint between t1 and t2, we expect
   both t1 and t2 to be def types. *)

(* Recursion limiter. We proxy recursion depth with trace depth,
   which is either equal or pretty close.
   When check is called with a trace whose depth exceeds a constant
   limit, we throw a LimitExceeded exception.
 *)

module RecursionCheck : sig
  exception LimitExceeded of Trace.t

  val check : Context.t -> Trace.t -> unit
end = struct
  exception LimitExceeded of Trace.t

  (* check trace depth as a proxy for recursion depth
     and throw when limit is exceeded *)
  let check cx trace =
    if Trace.trace_depth trace >= Context.recursion_limit cx then raise (LimitExceeded trace)
end

(* The main problem with constant folding is infinite recursion. Consider a loop
 * that keeps adding 1 to a variable x, which is initialized to 0. If we
 * constant fold x naively, we'll recurse forever, inferring that x has the type
 * (0 | 1 | 2 | 3 | 4 | etc). What we need to do is recognize loops and stop
 * doing constant folding.
 *
 * One solution is for constant-folding-location to keep count of how many times
 * we have seen a reason at a given position in the array.
 * Then, when we've seen it multiple times in the same place, we can decide
 * to stop doing constant folding.
 *)

module ConstFoldMap = WrappedMap.Make (struct
  type t = reason * int

  let compare = Stdlib.compare
end)

module ConstFoldExpansion : sig
  val guard : int -> reason * int -> (int -> 't) -> 't
end = struct
  let rmaps : int ConstFoldMap.t IMap.t ref = ref IMap.empty

  let get_rmap id = IMap.find_opt id !rmaps |> Base.Option.value ~default:ConstFoldMap.empty

  let increment reason_with_pos rmap =
    match ConstFoldMap.find_opt reason_with_pos rmap with
    | None -> (0, ConstFoldMap.add reason_with_pos 1 rmap)
    | Some count -> (count, ConstFoldMap.add reason_with_pos (count + 1) rmap)

  let guard id reason_with_pos f =
    let (count, rmap) = get_rmap id |> increment reason_with_pos in
    rmaps := IMap.add id rmap !rmaps;
    f count
end

exception Not_expect_bound of string

exception Attempted_operation_on_bound of string

let with_evaluated_cache cx id evaluated f tvar =
  Context.set_evaluated cx (Eval.Map.add id (OpenT tvar) evaluated);
  try f tvar
  with Attempted_operation_on_bound _ as exn when Context.in_normalizer_mode cx ->
    let e = Exception.wrap exn in
    (* Raised exceptions are not recorded on the constraint graph or the eval-cache.
       If this exception is hit during normalization, then `tvar` will likely not
       have the expected lower-bounds. To avoid reusing this spurious result the
       next time we evaluate the same EvalT, we need to restore the evaluation
       cache. *)
    Context.set_evaluated cx evaluated;
    Exception.reraise e

(* Sometimes we don't expect to see type parameters, e.g. when they should have
   been substituted away. *)
let not_expect_bound cx t =
  match t with
  | BoundT _ when not (Context.in_normalizer_mode cx) ->
    raise
      (Not_expect_bound
         (spf
            "Did not expect %s : %s"
            (string_of_ctor t)
            (Debug_js.string_of_reason cx (reason_of_t t))))
  | _ -> ()

let not_expect_bound_use cx t = lift_to_use (not_expect_bound cx) t

(* Sometimes we expect to see only proper def types. Proper def types make sense
   as use types. *)
let expect_proper_def t =
  if not (is_proper_def t) then assert_false (spf "Did not expect %s" (string_of_ctor t))

let expect_proper_def_use t = lift_to_use expect_proper_def t

let check_nonstrict_import cx trace is_strict imported_is_strict reason =
  if is_strict && not imported_is_strict then
    let loc = Reason.aloc_of_reason reason in
    let message = Error_message.ENonstrictImport loc in
    add_output cx ~trace message

let subst = Subst.subst

let check_canceled =
  let count = ref 0 in
  fun () ->
    let n = (!count + 1) mod 128 in
    count := n;
    if n = 0 then WorkerCancel.check_should_exit ()

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
  | HasOwnPropT (_, r, Literal (_, name)) ->
    Error_message.IncompatibleHasOwnPropT (aloc_of_reason r, Some name)
  | HasOwnPropT (_, r, _) -> Error_message.IncompatibleHasOwnPropT (aloc_of_reason r, None)
  | GetValuesT _ -> Error_message.IncompatibleGetValuesT
  | UnaryMinusT _ -> Error_message.IncompatibleUnaryMinusT
  | MapTypeT (_, _, (ObjectMap _ | ObjectMapi _), _) -> Error_message.IncompatibleMapTypeTObject
  | TypeAppVarianceCheckT _ -> Error_message.IncompatibleTypeAppVarianceCheckT
  | GetStaticsT _ -> Error_message.IncompatibleGetStaticsT
  | use_t -> Error_message.IncompatibleUnclassified (string_of_use_ctor use_t)

let use_op_of_lookup_action = function
  | ReadProp { use_op; _ } -> Some use_op
  | WriteProp { use_op; _ } -> Some use_op
  | LookupProp (use_op, _) -> Some use_op
  | SuperProp (use_op, _) -> Some use_op
  | MatchProp (use_op, _) -> Some use_op

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

let function_use = function
  | UseT (_, DefT (_, _, FunT _)) -> true
  | _ -> false

let object_like = function
  | DefT (_, _, (ObjT _ | InstanceT _))
  | ObjProtoT _
  | FunProtoT _
  | AnyT _ ->
    true
  | t -> function_like t

let object_use = function
  | UseT (_, DefT (_, _, ObjT _)) -> true
  | _ -> false

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

let strict_equatable_error cond_context (l, r) =
  let comparison_error =
    lazy
      (match cond_context with
      | Some (SwitchTest { case_test_reason; switch_discriminant_reason }) ->
        let use_op =
          Op
            (SwitchCheck
               { case_test = case_test_reason; switch_discriminant = switch_discriminant_reason })
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
  (* No comparisons of enum objects are allowed. *)
  | (DefT (_, _, EnumObjectT _), _)
  | (_, DefT (_, _, EnumObjectT _)) ->
    Some (Lazy.force comparison_error)
  (* We allow comparison between enums of the same type. *)
  | (DefT (_, _, EnumT { enum_id = id1; _ }), DefT (_, _, EnumT { enum_id = id2; _ }))
    when ALoc.equal_id id1 id2 ->
    None
  (* We allow the comparison of enums to null and void outside of switches. *)
  | (DefT (_, _, EnumT _), DefT (_, _, (NullT | VoidT)))
  | (DefT (_, _, (NullT | VoidT)), DefT (_, _, EnumT _)) ->
    begin
      match cond_context with
      | Some (SwitchTest _) -> Some (Lazy.force comparison_error)
      | None
      | Some _ ->
        None
    end
  (* We don't allow the comparison of enums and other types in general. *)
  | (DefT (_, _, EnumT _), _)
  | (_, DefT (_, _, EnumT _)) ->
    Some (Lazy.force comparison_error)
  (* We don't check other strict equality comparisons. *)
  | _ -> None

let strict_equatable cond_context args =
  strict_equatable_error cond_context args |> Base.Option.is_none

let is_concrete t =
  match t with
  | EvalT _
  | AnnotT _
  | ExactT _
  | MaybeT _
  | OptionalT _
  | TypeAppT _
  | ThisTypeAppT _
  | OpenT _ ->
    false
  | _ -> true

let is_literal_type t =
  match t with
  | DefT (_, _, SingletonStrT _)
  | DefT (_, _, SingletonNumT _)
  | DefT (_, _, SingletonBoolT _) ->
    true
  | _ -> false

let position_generic_bound reason = mod_reason_of_t (Fn.const reason)

(* Creates a union from a list of types. Since unions require a minimum of two
   types this function will return an empty type when there are no types in the
   list, or the list head when there is one type in the list. *)
let union_of_ts reason ts =
  match ts with
  (* If we have no types then this is an error. *)
  | [] -> DefT (reason, bogus_trust (), EmptyT Bottom)
  (* If we only have one type then only that should be used. *)
  | [t0] -> t0
  (* If we have more than one type then we make a union type. *)
  | t0 :: t1 :: ts -> UnionT (reason, UnionRep.make t0 t1 ts)
  (* generics *)

(** Harness for testing parameterized types. Given a test function and a list
    of type params, generate a bunch of argument maps and invoke the test
    function on each, using Reason.TestID to keep the reasons generated by
    each test disjoint from the others.

    In the general case we simply test every combination of p = bot, p = bound
    for each param p. For many parameter lists this will be more than strictly
    necessary, but determining the minimal set of tests for interrelated params
    is subtle. For now, our only refinement is to isolate all params with an
    upper bound of MixedT (making them trivially unrelated to each other) and
    generate a smaller set of argument maps for these which only cover a) bot,
    bound for each param, and b) every pairwise bot/bound combination. These
    maps are then used as seeds for powersets over the remaining params.

    NOTE: Since the same AST is traversed by each generated test, the order
    of generated tests is important for the proper functioning of hooks that
    record information on the side as ASTs are traversed. Adopting the
    convention that the last traversal "wins" (which would happen, e.g, when
    the recorded information at a location is replaced every time that
    location is encountered), we want the last generated test to always be
    the one where all type parameters are substituted by their bounds
    (instead of Bottom), so that the recorded information is the same as if
    all type parameters were indeed erased and replaced by their bounds.
  *)
and check_with_generics : 'a. Context.t -> Type.typeparam list -> (Type.t SMap.t -> 'a) -> 'a =
  (* make bot type for given param *)
  let mk_bot cx _ { name; reason; is_this; _ } =
    let desc =
      RPolyTest
        ( name,
          RIncompatibleInstantiation name,
          Reason.aloc_of_reason reason |> Context.make_generic_id cx name |> Generic.aloc_of_id,
          is_this )
    in
    DefT (replace_desc_reason desc reason, bogus_trust (), EmptyT Zeroed)
  in
  (* make bound type for given param and argument map *)
  let mk_bound cx prev_args { bound; name; reason = param_reason; is_this; _ } =
    (* For the top bound, we match the reason locations that appear in the
     * respective bot bound:
     * - 'loc' is the location of the type parameter (may be repositioned later)
     * - 'def_loc' is the location of the type parameter, and
     * - 'annot_loc_opt' is the location of the bound (if present).
     *)
    mod_reason_of_t
      (fun bound_reason ->
        let param_loc = Reason.aloc_of_reason param_reason in
        let annot_loc = annot_aloc_of_reason bound_reason in
        let desc = desc_of_reason ~unwrap:false bound_reason in
        opt_annot_reason ?annot_loc
        @@ mk_reason
             (RPolyTest
                ( name,
                  desc,
                  Context.make_generic_id cx name param_loc |> Generic.aloc_of_id,
                  is_this ))
             param_loc)
      (subst cx prev_args bound)
  in
  (* make argument map by folding mk_arg over param list *)
  let mk_argmap mk_arg =
    List.fold_left (fun acc ({ name; _ } as p) -> SMap.add name (mk_arg acc p) acc) SMap.empty
  in
  (* for each p, a map with p bot and others bound + map with all bound *)
  let linear cx = function
    | [] -> [SMap.empty]
    | params ->
      let all = mk_argmap (mk_bound cx) params in
      let each =
        Base.List.map
          ~f:(fun ({ name; _ } as p) -> SMap.add name (mk_bot cx SMap.empty p) all)
          params
      in
      List.rev (all :: each)
  in
  (* a map for every combo of bot/bound params *)
  let powerset cx params arg_map =
    let none = mk_argmap (mk_bot cx) params in
    List.fold_left
      (fun maps ({ name; _ } as p) ->
        let bots = Base.List.map ~f:(SMap.add name (SMap.find name none)) maps in
        let bounds = Base.List.map ~f:(fun m -> SMap.add name (mk_bound cx m p) m) maps in
        bots @ bounds)
      [arg_map]
      params
  in
  (* New generics mode: generate a GenericT from a generic *)
  let generic_bound cx prev_map { bound; name; reason = param_reason; is_this; _ } =
    let param_loc = aloc_of_reason param_reason in
    let bound = subst cx prev_map bound in
    let id = Context.make_generic_id cx name param_loc in
    let bound =
      mod_reason_of_t
        (fun bound_reason ->
          let annot_loc = annot_aloc_of_reason bound_reason in
          let desc = desc_of_reason ~unwrap:false bound_reason in
          opt_annot_reason ?annot_loc
          @@ mk_reason (RPolyTest (name, desc, id |> Generic.aloc_of_id, is_this)) param_loc)
        bound
    in
    let generic = GenericT { reason = reason_of_t bound; name; id; bound } in
    SMap.add name generic prev_map
  in
  (* main - run f over a collection of arg maps generated for params *)
  fun cx params f ->
    if params = [] then
      f SMap.empty
    else if Context.generate_tests cx then
      let is_free = function
        | { bound = DefT (_, _, MixedT _); _ } -> true
        | _ -> false
      in
      let (free_params, dep_params) = List.partition is_free params in
      let free_sets = linear cx free_params in
      let powersets = Base.List.map ~f:(powerset cx dep_params) free_sets in
      let (hd_map, tl_maps) =
        match List.flatten powersets with
        | x :: xs -> (x, xs)
        | [] -> assert false
      in
      Base.List.fold_left ~f:(Fn.const (TestID.run f)) ~init:(f hd_map) tl_maps
    else
      let map = Base.List.fold_left ~f:(generic_bound cx) ~init:SMap.empty params in
      f map

let inherited_method x = x <> "constructor"

let match_this_binding map f =
  match SMap.find "this" map with
  | ReposT (_, t) -> f t
  | _ -> failwith "not a this binding"

let poly_minimum_arity =
  let f n typeparam =
    if typeparam.default = None then
      n + 1
    else
      n
  in
  Nel.fold_left f 0

(********************** start of slab **********************************)
module M__flow
    (ReactJs : React_kit.REACT)
    (AssertGround : Flow_common.ASSERT_GROUND)
    (CheckPolarity : Flow_common.CHECK_POLARITY)
    (TrustChecking : Flow_common.TRUST_CHECKING)
    (CustomFunKit : Custom_fun_kit.CUSTOM_FUN)
    (ObjectKit : Object_kit.OBJECT) =
struct
  (** NOTE: Do not call this function directly. Instead, call the wrapper
    functions `rec_flow`, `join_flow`, or `flow_opt` (described below) inside
    this module, and the function `flow` outside this module. **)
  let rec __flow cx ((l : Type.t), (u : Type.use_t)) trace =
    if ground_subtype (l, u) then (
      if Context.trust_tracking cx then TrustChecking.trust_flow_to_use_t cx trace l u;
      print_types_if_verbose cx trace (l, u)
    ) else if Cache.FlowConstraint.get cx (l, u) then
      print_types_if_verbose cx trace ~note:"(cached)" (l, u)
    else (
      print_types_if_verbose cx trace (l, u);
      if Context.trust_tracking cx then TrustChecking.trust_flow_to_use_t cx trace l u;

      (* limit recursion depth *)
      RecursionCheck.check cx trace;

      (* Check if this worker has been told to cancel *)
      check_canceled ();

      (* Expect that l is a def type. On the other hand, u may be a use type or a
       def type: the latter typically when we have annotations. *)

      (* Type parameters should always be substituted out, and as such they should
         never appear "exposed" in flows. (They can still appear bound inside
         polymorphic definitions.)

         An exception to this is when calling Flow_js from the normalizer. There,
         BoundTs have not been substituted with their bounds. Doing so typically
         leads to poor quality of normalized types when the BoundTs appear under
         EvalT. The following checks take this into account in banning BoundTs. *)
      not_expect_bound cx l;
      not_expect_bound_use cx u;

      (* Types that are classified as def types but don't make sense as use types
       should not appear as use types. *)
      expect_proper_def_use u;

      (* Before processing the flow action, check that it is not deferred. If it
       is, then when speculation is complete, the action either fires or is
       discarded depending on whether the case that created the action is
       selected or not. *)
      if Speculation.defer_action cx (Speculation_state.FlowAction (l, u)) then
        print_if_verbose cx trace ~indent:1 ["deferred during speculation"]
      else if
        match l with
        | AnyT _ ->
          (* Either propagate AnyT through the use type, or short-circuit because any <: u trivially *)
          any_propagated cx trace l u
        | GenericT { bound; name; reason; id } -> handle_generic cx trace bound reason id name u
        | _ -> false
        (* Either propagate AnyT through the def type, or short-circuit because l <: any trivially *)
      then
        ()
      else if
        match u with
        | UseT (use_op, (AnyT _ as any)) -> any_propagated_use cx trace use_op any l
        | _ -> false
      then
        ()
      else if
        match l with
        | DefT (_, _, EmptyT flavor) -> empty_success flavor u
        | _ -> false
      then
        ()
      else
        match (l, u) with
        (********)
        (* eval *)
        (********)
        | (EvalT (_, _, id1), UseT (_, EvalT (_, _, id2))) when Type.Eval.equal_id id1 id2 ->
          if Context.is_verbose cx then prerr_endline "EvalT ~> EvalT fast path"
        | (EvalT (t, TypeDestructorT (use_op', reason, d), id), _) ->
          let (_, result) = mk_type_destructor cx ~trace use_op' reason t d id in
          rec_flow cx trace (result, u)
        | (_, UseT (use_op, EvalT (t, TypeDestructorT (use_op', reason, d), id))) ->
          let (slingshot, result) = mk_type_destructor cx ~trace use_op' reason t d id in
          if slingshot then
            rec_flow cx trace (result, ReposUseT (reason, false, use_op, l))
          else
            rec_flow cx trace (l, UseT (use_op, result))
        | (EvalT (t, LatentPredT (reason, p), i), _) ->
          rec_flow cx trace (eval_latent_pred cx ~trace reason t p i, u)
        (******************)
        (* process X ~> Y *)
        (******************)
        | (OpenT (_, tvar1), UseT (use_op, OpenT (_, tvar2))) ->
          let (id1, constraints1) = Context.find_constraints cx tvar1 in
          let (id2, constraints2) = Context.find_constraints cx tvar2 in
          (match (constraints1, constraints2) with
          | (Unresolved bounds1, Unresolved bounds2) ->
            if not_linked (id1, bounds1) (id2, bounds2) then (
              add_upper_edges ~new_use_op:use_op cx trace (id1, bounds1) (id2, bounds2);
              add_lower_edges cx trace ~new_use_op:use_op (id1, bounds1) (id2, bounds2);
              flows_across cx trace ~use_op bounds1.lower bounds2.upper
            )
          | (Unresolved bounds1, (Resolved (use_op', t2) | FullyResolved (use_op', t2))) ->
            let t2_use = flow_use_op cx use_op' (UseT (use_op, t2)) in
            edges_and_flows_to_t cx trace (id1, bounds1) t2_use
          | ((Resolved (_, t1) | FullyResolved (_, t1)), Unresolved bounds2) ->
            edges_and_flows_from_t cx trace ~new_use_op:use_op t1 (id2, bounds2)
          | ( (Resolved (_, t1) | FullyResolved (_, t1)),
              (Resolved (use_op', t2) | FullyResolved (use_op', t2)) ) ->
            let t2_use = flow_use_op cx use_op' (UseT (use_op, t2)) in
            rec_flow cx trace (t1, t2_use))
        (******************)
        (* process Y ~> U *)
        (******************)
        | (OpenT (r, tvar), t2) ->
          let t2 =
            match desc_of_reason r with
            | RTypeParam _ -> mod_use_op_of_use_t (fun op -> Frame (ImplicitTypeParam, op)) t2
            | _ -> t2
          in
          let (id1, constraints1) = Context.find_constraints cx tvar in
          (match constraints1 with
          | Unresolved bounds1 -> edges_and_flows_to_t cx trace (id1, bounds1) t2
          | Resolved (_, t1)
          | FullyResolved (_, t1) ->
            rec_flow cx trace (t1, t2))
        (******************)
        (* process L ~> X *)
        (******************)
        | (t1, UseT (use_op, OpenT (_, tvar))) ->
          let (id2, constraints2) = Context.find_constraints cx tvar in
          (match constraints2 with
          | Unresolved bounds2 ->
            edges_and_flows_from_t cx trace ~new_use_op:use_op t1 (id2, bounds2)
          | Resolved (use_op', t2)
          | FullyResolved (use_op', t2) ->
            let t2_use = flow_use_op cx use_op' (UseT (use_op, t2)) in
            rec_flow cx trace (t1, t2_use))
        (*****************************************)
        (* BoundTs - only used for normalization *)
        (*****************************************)
        | (BoundT (_, lname), UseT (_, BoundT (_, uname))) when lname = uname ->
          assert (Context.in_normalizer_mode cx);
          ()
        | (BoundT (_, _), ReposLowerT (reason, use_desc, u)) ->
          assert (Context.in_normalizer_mode cx);
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (BoundT (_, name), _) ->
          assert (Context.in_normalizer_mode cx);
          raise (Attempted_operation_on_bound name)
        (*****************)
        (* any with uses *)
        (*****************)
        | (_, UseT (_, MergedT (_, uses))) -> List.iter (fun u -> rec_flow cx trace (l, u)) uses
        | (MergedT (reason, _), _) -> rec_flow cx trace (Unsoundness.why Merged reason, u)
        (****************)
        (* eval, contd. *)
        (****************)
        | (_, UseT (use_op, EvalT (t, LatentPredT (reason, p), i))) ->
          rec_flow cx trace (l, UseT (use_op, eval_latent_pred cx ~trace reason t p i))
        (***************************)
        (* type destructor trigger *)
        (***************************)
        (* For evaluating type destructors we add a trigger, TypeDestructorTriggerT,
         * to both sides of a type. When TypeDestructorTriggerT sees a new upper or
         * lower bound we destruct that bound and flow the result in the same
         * direction to some tout type. *)

        (* Don't let two TypeDestructorTriggerTs reach each other or else we quickly
         * run into non-termination scenarios. *)
        | (TypeDestructorTriggerT _, UseT (_, TypeDestructorTriggerT _)) -> ()
        | (l, UseT (_, TypeDestructorTriggerT (use_op', reason, repos, d, tout))) ->
          let l =
            match repos with
            | None -> l
            | Some (reason, use_desc) -> reposition_reason cx ~trace reason ~use_desc l
          in
          eval_destructor cx ~trace use_op' reason l d tout
        | ( TypeDestructorTriggerT (use_op', reason, _, d, tout),
            UseT (use_op, AnnotT (r, t, use_desc)) ) ->
          let tout' =
            Tvar.mk_no_wrap_where cx reason (fun tout' ->
                let repos = Some (r, use_desc) in
                rec_flow
                  cx
                  trace
                  (t, UseT (use_op, TypeDestructorTriggerT (use_op', reason, repos, d, tout'))))
          in
          rec_flow cx trace (tout', ReposUseT (reason, false, use_op, OpenT tout))
        | (TypeDestructorTriggerT (use_op', reason, _, d, tout), UseT (use_op, u)) ->
          (* With the same "slingshot" trick used by AnnotT, hold the lower bound
           * at bay until result itself gets concretized, and then flow the lower
           * bound to that concrete type. *)
          let t =
            Tvar.mk_no_wrap_where cx reason (fun t ->
                eval_destructor cx ~trace use_op' reason u d t)
          in
          let use_desc = false in
          rec_flow cx trace (t, ReposUseT (reason, use_desc, use_op, OpenT tout))
        (* Ignore any non-type uses. The implementation of type destructors operate
         * solely on types and not arbitrary uses. We also don't want to add errors
         * for arbitrary uses that get added to the subject of our trigger in type
         * destruction evaluation.
         *
         * This may be a risky behavior when considering tvars with *only* non-type
         * uses. However, such tvars are rare and often come from non-sensical
         * programs.
         *
         * Type destructors, currently, may only be created as type annotations.
         * This means that the type is either always 0->1, or it is a polymorphic
         * type argument which will be instantiated with an open tvar. Polymorphic
         * type arguments will also always get some type upper bound with the
         * default type being MixedT. We destruct these upper bounds. *)
        | (TypeDestructorTriggerT _, _) -> ()
        (************************)
        (* Full type resolution *)
        (************************)

        (* Full resolution of a type involves (1) walking the type to collect a
       bunch of unresolved tvars (2) emitting constraints that, once those tvars
       are resolved, recursively trigger the process for the resolved types (3)
       finishing when no unresolved tvars remain.

       (1) is covered in ResolvableTypeJob. Below, we cover (2) and (3).

       For (2), we emit a FullyResolveType constraint on any unresolved tvar
       found by (1). These unresolved tvars are chosen so that they have the
       following nice property, called '0->1': they remain unresolved until, at
       some point, they are unified with a concrete type. Moreover, the act of
       resolution coincides with the appearance of one (the first and the last)
       upper bound. (In general, unresolved tvars can accumulate an arbitrary
       number of lower and upper bounds over its lifetime.) More details can be
       found in bindings_of_jobs.

       For (3), we create a special "goal" tvar that acts like a promise for
       fully resolving the original type, and emit a Trigger constraint on the
       goal when no more work remains.

       The main client of full type resolution is checking union and
       intersection types. The check itself is modeled by a TryFlow constraint,
       which is guarded by a goal tvar that corresponds to some full type
       resolution requirement. Eventually, this goal is "triggered," which in
       turn triggers the check. (The name "TryFlow" refers to the technique used
       in the check, which literally tries each branch of the union or
       intersection in turn, maintaining some matching state as it goes: see
       speculative_matches for details). *)
        | (t, ChoiceKitUseT (reason, FullyResolveType id)) ->
          fully_resolve_type cx trace reason id t
        | (InternalT (ChoiceKitT (_, Trigger)), ChoiceKitUseT (reason, TryFlow (i, spec))) ->
          speculative_matches cx trace reason i spec
        (* Intersection types need a preprocessing step before they can be checked;
       this step brings it closer to parity with the checking of union types,
       where the preprocessing effectively happens "automatically." This
       apparent asymmetry is explained in prep_try_intersection.

       Here, it suffices to note that the preprocessing step involves
       concretizing some types. Type concretization is distinct from full type
       resolution. Whereas full type resolution is a recursive process that
       needs careful orchestration, type concretization is a relatively simple
       one-step process: a tvar is concretized when any lower bound appears on
       it. Also, unlike full type resolution, the tvars that are concretized
       don't necessarily have the 0->1 property: they could be concretized at
       different types, as more and more lower bounds appear. *)
        | (UnionT (_, urep), IntersectionPreprocessKitT (_, ConcretizeTypes _)) ->
          flow_all_in_union cx trace urep u
        | (MaybeT (lreason, t), IntersectionPreprocessKitT (_, ConcretizeTypes _)) ->
          let lreason = replace_desc_reason RNullOrVoid lreason in
          rec_flow cx trace (NullT.make lreason |> with_trust Trust.bogus_trust, u);
          rec_flow cx trace (VoidT.make lreason |> with_trust Trust.bogus_trust, u);
          rec_flow cx trace (t, u)
        | ( OptionalT { reason = r; type_ = t; use_desc },
            IntersectionPreprocessKitT (_, ConcretizeTypes _) ) ->
          rec_flow cx trace (VoidT.why_with_use_desc ~use_desc r |> with_trust Trust.bogus_trust, u);
          rec_flow cx trace (t, u)
        | (AnnotT (r, t, use_desc), IntersectionPreprocessKitT (_, ConcretizeTypes _)) ->
          (* TODO: directly derive loc and desc from the reason of tvar *)
          let loc = aloc_of_reason r in
          let desc =
            if use_desc then
              Some (desc_of_reason r)
            else
              None
          in
          rec_flow cx trace (reposition ~trace cx loc ?desc t, u)
        | ( t,
            IntersectionPreprocessKitT
              (reason, ConcretizeTypes (unresolved, resolved, IntersectionT (r, rep), u)) ) ->
          prep_try_intersection cx trace reason unresolved (t :: resolved) u r rep
        (*****************************)
        (* Refinement type subtyping *)
        (*****************************)
        | (_, RefineT (reason, LatentP (fun_t, idx), tvar)) ->
          flow cx (fun_t, CallLatentPredT (reason, true, idx, l, tvar))
        (*************)
        (* Debugging *)
        (*************)
        | (_, DebugPrintT reason) ->
          let str = Debug_js.dump_t ~depth:10 cx l in
          add_output cx ~trace (Error_message.EDebugPrint (reason, str))
        | (DefT (_, _, NumT (Literal (_, (n, _)))), DebugSleepT _) ->
          let n = ref n in
          while !n > 0.0 do
            WorkerCancel.check_should_exit ();
            Unix.sleepf (min !n 1.0);
            n := !n -. 1.
          done
        (*************************)
        (* repositioning, part 1 *)
        (*************************)

        (* if a ReposT is used as a lower bound, `reposition` can reposition it *)
        | (ReposT (reason, l), _) -> rec_flow cx trace (reposition_reason cx ~trace reason l, u)
        (* if a ReposT is used as an upper bound, wrap the now-concrete lower bound
       in a `ReposUpperT`, which will repos `u` when `u` becomes concrete. *)
        | (_, UseT (use_op, ReposT (reason, u))) ->
          rec_flow cx trace (InternalT (ReposUpperT (reason, l)), UseT (use_op, u))
        | (InternalT (ReposUpperT (reason, l)), UseT (use_op, u)) ->
          (* since this guarantees that `u` is not an OpenT, it's safe to use
         `reposition` on the upper bound here. *)
          let u = reposition_reason cx ~trace reason u in
          rec_flow cx trace (l, UseT (use_op, u))
        | (InternalT (ReposUpperT (_, l)), _) -> rec_flow cx trace (l, u)
        (***************)
        (* annotations *)
        (***************)

        (* Special cases where we want to recursively concretize types within the
       lower bound. *)
        | (UnionT (r, rep), ReposUseT (reason, use_desc, use_op, l)) ->
          let rep = UnionRep.ident_map (annot use_desc) rep in
          let loc = aloc_of_reason reason in
          let annot_loc = annot_aloc_of_reason reason in
          let r = opt_annot_reason ?annot_loc @@ repos_reason loc r in
          let r =
            if use_desc then
              replace_desc_reason (desc_of_reason reason) r
            else
              r
          in
          rec_flow cx trace (l, UseT (use_op, UnionT (r, rep)))
        | (MaybeT (r, u), ReposUseT (reason, use_desc, use_op, l)) ->
          let loc = aloc_of_reason reason in
          let annot_loc = annot_aloc_of_reason reason in
          let r = opt_annot_reason ?annot_loc @@ repos_reason loc r in
          let r =
            if use_desc then
              replace_desc_reason (desc_of_reason reason) r
            else
              r
          in
          rec_flow cx trace (l, UseT (use_op, MaybeT (r, annot use_desc u)))
        | ( OptionalT { reason = r; type_ = u; use_desc = use_desc_optional_t },
            ReposUseT (reason, use_desc, use_op, l) ) ->
          let loc = aloc_of_reason reason in
          let annot_loc = annot_aloc_of_reason reason in
          let r = opt_annot_reason ?annot_loc @@ repos_reason loc r in
          let r =
            if use_desc then
              replace_desc_reason (desc_of_reason reason) r
            else
              r
          in
          rec_flow
            cx
            trace
            ( l,
              UseT
                ( use_op,
                  OptionalT { reason = r; type_ = annot use_desc u; use_desc = use_desc_optional_t }
                ) )
        (* Waits for a def type to become concrete, repositions it as an upper UseT
       using the stored reason. This can be used to store a reason as it flows
       through a tvar. *)
        | (u_def, ReposUseT (reason, use_desc, use_op, l)) ->
          let u = reposition_reason cx ~trace reason ~use_desc u_def in
          rec_flow cx trace (l, UseT (use_op, u))
        (* The sink component of an annotation constrains values flowing
       into the annotated site. *)
        | (_, UseT (use_op, AnnotT (r, t, use_desc))) ->
          rec_flow cx trace (t, ReposUseT (r, use_desc, use_op, l))
        (* Don't widen annotations *)
        | (AnnotT _, ObjKitT (use_op, _, Object.Resolve Object.Next, Object.ObjectWiden _, tout)) ->
          rec_flow_t cx trace ~use_op (l, tout)
        (* The source component of an annotation flows out of the annotated
       site to downstream uses. *)
        | (AnnotT (r, t, use_desc), u) ->
          let t = reposition_reason ~trace cx r ~use_desc t in
          rec_flow cx trace (t, u)
        (****************************************************************)
        (* BecomeT unifies a tvar with an incoming concrete lower bound *)
        (****************************************************************)
        | (_, BecomeT (reason, t)) when is_proper_def l ->
          let l = reposition ~trace cx (aloc_of_reason reason) l in
          rec_unify cx trace ~use_op:unknown_use ~unify_any:true l t
        (***********************)
        (* guarded unification *)
        (***********************)

        (* Utility to unify a pair of types based on a trigger. Triggers are
        commonly type variables that are set up to record when certain
        operations have been processed: until then, they remain latent. For
        example, we can respond to events such as "a property is added," "a
        refinement succeeds," etc., by setting up unification constraints that
        are processed only when the corresponding triggers fire. *)
        | (_, UnifyT (t, t_other)) ->
          rec_unify cx trace ~use_op:unknown_use ~unify_any:true t t_other
        (***************************)
        (* type cast e.g. `(x: T)` *)
        (***************************)
        | (DefT (reason, trust, EnumT enum), TypeCastT (use_op, cast_to_t)) ->
          rec_flow cx trace (cast_to_t, EnumCastT { use_op; enum = (reason, trust, enum) })
        | (UnionT _, TypeCastT (_, (UnionT _ as u)))
          when union_optimization_guard cx (Context.trust_errors cx |> TypeUtil.quick_subtype) l u
          ->
          ()
        | (UnionT (_, rep1), TypeCastT _) -> flow_all_in_union cx trace rep1 u
        | (_, TypeCastT (use_op, cast_to_t)) -> rec_flow cx trace (l, UseT (use_op, cast_to_t))
        (**********************************************************************)
        (* enum cast e.g. `(x: T)` where `x` is an `EnumT`                    *)
        (* We allow enums to be explicitly cast to their representation type. *)
        (* When we specialize `TypeCastT` when the LHS is an `EnumT`, the     *)
        (* `cast_to_t` of `TypeCastT` must then be resolved. So we call flow  *)
        (* with it on the LHS, and `EnumCastT` on the RHS. When we actually   *)
        (* turn this into a `UseT`, it must placed back on the RHS.           *)
        (**********************************************************************)
        | (cast_to_t, EnumCastT { use_op; enum = (_, _, { representation_t; _ }) })
          when TypeUtil.quick_subtype (Context.trust_errors cx) representation_t cast_to_t ->
          rec_flow cx trace (representation_t, UseT (use_op, cast_to_t))
        | (cast_to_t, EnumCastT { use_op; enum = (reason, trust, enum) }) ->
          rec_flow cx trace (DefT (reason, trust, EnumT enum), UseT (use_op, cast_to_t))
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

    **)
        | ((ExactT (_, DefT (_, _, ObjT _)) | DefT (_, _, ObjT _)), ImportTypeT (_, "default", t))
          ->
          rec_flow_t ~use_op:unknown_use cx trace (l, t)
        | (exported_type, ImportTypeT (reason, export_name, t)) ->
          (match canonicalize_imported_type cx trace reason exported_type with
          | Some imported_t -> rec_flow_t ~use_op:unknown_use cx trace (imported_t, t)
          | None -> add_output cx ~trace (Error_message.EImportValueAsType (reason, export_name)))
        (************************************************************************)
        (* `import typeof` creates a properly-parameterized type alias for the  *)
        (* "typeof" the remote export.                                          *)
        (************************************************************************)
        | ( DefT
              ( _,
                _,
                PolyT
                  {
                    tparams_loc;
                    tparams = typeparams;
                    t_out = (DefT (_, _, ClassT _) | DefT (_, _, FunT _)) as lower_t;
                    id;
                  } ),
            ImportTypeofT (reason, _, t) ) ->
          let typeof_t = mk_typeof_annotation cx ~trace reason lower_t in
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            ( poly_type
                id
                tparams_loc
                typeparams
                (DefT (reason, bogus_trust (), TypeT (ImportTypeofKind, typeof_t))),
              t )
        | ( (DefT (_, _, TypeT _) | DefT (_, _, PolyT { t_out = DefT (_, _, TypeT _); _ })),
            ImportTypeofT (reason, export_name, _) ) ->
          add_output cx ~trace (Error_message.EImportTypeAsTypeof (reason, export_name))
        | (_, ImportTypeofT (reason, _, t)) ->
          let typeof_t = mk_typeof_annotation cx ~trace reason l in
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (DefT (reason, bogus_trust (), TypeT (ImportTypeofKind, typeof_t)), t)
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
        | (ModuleT (_, { exports_tmap; _ }, _), ExportNamedT (reason, tmap, export_kind, tout)) ->
          let add_export name export acc =
            let export' =
              match export_kind with
              | ExportValue -> export
              | ReExport ->
                (* Re-exports do not overwrite named exports from the local module. Further, they do
                 * not need to be checked, as the original module has already performed the check. *)
                SMap.find_opt name acc |> Base.Option.value ~default:export
              | ExportType ->
                (* If it's of the form `export type` then check to make sure it's actually a type. *)
                let (loc, t) = export in
                let t' = Tvar.mk cx (reason_of_t t) in
                rec_flow cx trace (t, AssertExportIsTypeT (reason, name, t'));
                (loc, t')
            in
            SMap.add name export' acc
          in
          Context.find_exports cx exports_tmap
          |> SMap.fold add_export tmap
          |> Context.add_export_map cx exports_tmap;
          rec_flow_t ~use_op:unknown_use cx trace (l, tout)
        | (_, AssertExportIsTypeT (_, name, t_out)) ->
          if is_type l then
            rec_flow_t ~use_op:unknown_use cx trace (l, t_out)
          else
            let reason = reason_of_t l in
            add_output cx ~trace Error_message.(EExportValueAsType (reason, name));
            rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason, t_out)
        (* Copy the named exports from a source module into a target module. Used
        to implement `export * from 'SomeModule'`, with the current module as
        the target and the imported module as the source. *)
        | (ModuleT (_, source_exports, _), CopyNamedExportsT (reason, target_module_t, t_out)) ->
          let source_tmap = Context.find_exports cx source_exports.exports_tmap in
          rec_flow cx trace (target_module_t, ExportNamedT (reason, source_tmap, ReExport, t_out))
        (*
         * Copy only the type exports from a source module into a target module.
         * Used to implement `export type * from ...`.
         *)
        | (ModuleT (_, source_exports, _), CopyTypeExportsT (reason, target_module_t, t_out)) ->
          let source_exports = Context.find_exports cx source_exports.exports_tmap in
          (* Remove locations. TODO at some point we may want to include them here. *)
          let source_exports = SMap.map snd source_exports in
          let target_module_t =
            SMap.fold
              (fun export_name export_t target_module_t ->
                Tvar.mk_where cx reason (fun t ->
                    rec_flow
                      cx
                      trace
                      (export_t, ExportTypeT (reason, export_name, target_module_t, t))))
              source_exports
              target_module_t
          in
          rec_flow_t ~use_op:unknown_use cx trace (target_module_t, t_out)
        (*
         * Export a type from a given ModuleT, but only if the type is compatible
         * with `import type`/`export type`. When it is not compatible, it is simply
         * not added to the exports map.
         *
         * Note that this is very similar to `ExportNamedT` except that it only
         * exports one type at a time and it takes the type to be exported as a
         * lower (so that the type can be filtered post-resolution).
         *)
        | (l, ExportTypeT (reason, export_name, target_module_t, t_out)) ->
          let is_type_export =
            match l with
            | DefT (_, _, ObjT _) when export_name = "default" -> true
            | l -> canonicalize_imported_type cx trace reason l <> None
          in
          if is_type_export then
            rec_flow
              cx
              trace
              ( target_module_t,
                ExportNamedT
                  ( reason,
                    (* TODO we may want to add location information here *)
                    SMap.singleton export_name (None, l),
                    ReExport,
                    t_out ) )
          else
            rec_flow_t ~use_op:unknown_use cx trace (target_module_t, t_out)
        (* There is nothing to copy from a module exporting `any` or `Object`. *)
        | ( AnyT (lreason, _),
            ( CopyNamedExportsT (reason, target_module, t)
            | CopyTypeExportsT (reason, target_module, t) ) ) ->
          let () =
            match desc_of_reason lreason with
            (* Use a special reason so we can tell the difference between an any-typed import
             * from an untyped module and an any-typed import from a nonexistent module. *)
            | RUntypedModule module_name ->
              let loc = Reason.aloc_of_reason reason in
              let message = Error_message.EUntypedImport (loc, module_name) in
              add_output cx ~trace message
            | _ -> ()
          in
          rec_flow_t ~use_op:unknown_use cx trace (target_module, t)
        (*
         * ObjT CommonJS export values have their properties turned into named
         * exports
         *)
        | ( ( DefT (_, _, ObjT { props_tmap; proto_t; _ })
            | ExactT (_, DefT (_, _, ObjT { props_tmap; proto_t; _ })) ),
            CJSExtractNamedExportsT (reason, (module_t_reason, exporttypes, is_strict), t_out) ) ->
          (* Copy props from the prototype *)
          let module_t =
            Tvar.mk_where cx reason (fun t ->
                rec_flow
                  cx
                  trace
                  ( proto_t,
                    CJSExtractNamedExportsT (reason, (module_t_reason, exporttypes, is_strict), t)
                  ))
          in
          (* Copy own props *)
          rec_flow
            cx
            trace
            ( module_t,
              ExportNamedT
                ( reason,
                  Properties.extract_named_exports (Context.find_props cx props_tmap),
                  ExportValue,
                  t_out ) )
        (*
         * InstanceT CommonJS export values have their properties turned into named
         * exports
         *)
        | ( DefT (_, _, InstanceT (_, _, _, { own_props; proto_props; _ })),
            CJSExtractNamedExportsT (reason, (module_t_reason, exporttypes, is_strict), t_out) ) ->
          let module_t = ModuleT (module_t_reason, exporttypes, is_strict) in
          let extract_named_exports id =
            Context.find_props cx id
            |> SMap.filter (fun x _ -> not (is_munged_prop_name cx x))
            |> Properties.extract_named_exports
          in
          (* Copy own props *)
          let module_t =
            Tvar.mk_where cx reason (fun t ->
                rec_flow
                  cx
                  trace
                  (module_t, ExportNamedT (reason, extract_named_exports own_props, ExportValue, t)))
          in
          (* Copy proto props *)
          (* TODO: own props should take precedence *)
          rec_flow
            cx
            trace
            (module_t, ExportNamedT (reason, extract_named_exports proto_props, ExportValue, t_out))
        (* If the module is exporting any or Object, then we allow any named
         * import
         *)
        | (AnyT _, CJSExtractNamedExportsT (_, (module_t_reason, exporttypes, is_strict), t_out)) ->
          let module_t =
            ModuleT (module_t_reason, { exporttypes with has_every_named_export = true }, is_strict)
          in
          rec_flow_t ~use_op:unknown_use cx trace (module_t, t_out)
        (*
         * All other CommonJS export value types do not get merged into the named
         * exports tmap in any special way.
         *)
        | (_, CJSExtractNamedExportsT (_, (module_t_reason, exporttypes, is_strict), t_out)) ->
          let module_t = ModuleT (module_t_reason, exporttypes, is_strict) in
          rec_flow_t ~use_op:unknown_use cx trace (module_t, t_out)
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

        (* require('SomeModule') *)
        | (ModuleT (module_reason, exports, imported_is_strict), CJSRequireT (reason, t, is_strict))
          ->
          check_nonstrict_import cx trace is_strict imported_is_strict reason;
          let cjs_exports =
            match exports.cjs_export with
            | Some t ->
              (* reposition the export to point at the require(), like the object
             we create below for non-CommonJS exports *)
              reposition ~trace cx (aloc_of_reason reason) t
            | None ->
              let exports_tmap = Context.find_exports cx exports.exports_tmap in
              (* Convert ES module's named exports to an object *)
              let mk_exports_object () =
                let proto = ObjProtoT reason in
                let props =
                  SMap.map (fun (loc, t) -> Field (loc, t, Polarity.Positive)) exports_tmap
                in
                Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~frozen:true ~props proto
              in
              (* Use default export if option is enabled and module is not lib *)
              if Context.automatic_require_default cx && not (is_lib_reason_def module_reason) then
                match SMap.find_opt "default" exports_tmap with
                | Some (_, default_t) -> default_t
                | _ -> mk_exports_object ()
              else
                mk_exports_object ()
          in
          rec_flow_t ~use_op:unknown_use cx trace (cjs_exports, t)
        (* import * as X from 'SomeModule'; *)
        | (ModuleT (_, exports, imported_is_strict), ImportModuleNsT (reason, t, is_strict)) ->
          check_nonstrict_import cx trace is_strict imported_is_strict reason;
          let exports_tmap = Context.find_exports cx exports.exports_tmap in
          let props = SMap.map (fun (loc, t) -> Field (loc, t, Polarity.Positive)) exports_tmap in
          let props =
            if Context.facebook_module_interop cx then
              props
            else
              match exports.cjs_export with
              | Some t ->
                (* TODO this Field should probably have a location *)
                let p = Field (None, t, Polarity.Positive) in
                SMap.add "default" p props
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
          rec_flow_t ~use_op:unknown_use cx trace (ns_obj, t)
        (* import [type] X from 'SomeModule'; *)
        | ( ModuleT (module_reason, exports, imported_is_strict),
            ImportDefaultT (reason, import_kind, (local_name, module_name), t, is_strict) ) ->
          check_nonstrict_import cx trace is_strict imported_is_strict reason;
          let export_t =
            match exports.cjs_export with
            | Some t -> t
            | None ->
              let exports_tmap = Context.find_exports cx exports.exports_tmap in
              (match SMap.find_opt "default" exports_tmap with
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
                let known_exports = SMap.keys exports_tmap in
                let suggestion = typo_suggestion known_exports local_name in
                add_output
                  cx
                  ~trace
                  (Error_message.ENoDefaultExport (reason, module_name, suggestion));
                AnyT.error module_reason)
          in
          let import_t =
            match import_kind with
            | ImportType ->
              Tvar.mk_where cx reason (fun tvar ->
                  rec_flow cx trace (export_t, ImportTypeT (reason, "default", tvar)))
            | ImportTypeof ->
              Tvar.mk_where cx reason (fun tvar ->
                  rec_flow cx trace (export_t, ImportTypeofT (reason, "default", tvar)))
            | ImportValue ->
              rec_flow cx trace (export_t, AssertImportIsValueT (reason, "default"));
              export_t
          in
          rec_flow_t ~use_op:unknown_use cx trace (import_t, t)
        (* import {X} from 'SomeModule'; *)
        | ( ModuleT (_, exports, imported_is_strict),
            ImportNamedT (reason, import_kind, export_name, module_name, t, is_strict) ) ->
          check_nonstrict_import cx trace is_strict imported_is_strict reason;

          (*
           * When importing from a CommonJS module, we shadow any potential named
           * exports called "default" with a pointer to the raw `module.exports`
           * object
           *)
          let exports_tmap =
            let exports_tmap = Context.find_exports cx exports.exports_tmap in
            (* Drop locations; they are not needed here *)
            let exports_tmap = SMap.map snd exports_tmap in
            match exports.cjs_export with
            | Some t -> SMap.add "default" t exports_tmap
            | None -> exports_tmap
          in
          let has_every_named_export = exports.has_every_named_export in
          let import_t =
            match (import_kind, SMap.find_opt export_name exports_tmap) with
            | (ImportType, Some t) ->
              Tvar.mk_where cx reason (fun tvar ->
                  rec_flow cx trace (t, ImportTypeT (reason, export_name, tvar)))
            | (ImportType, None) when has_every_named_export ->
              let t = AnyT.untyped reason in
              Tvar.mk_where cx reason (fun tvar ->
                  rec_flow cx trace (t, ImportTypeT (reason, export_name, tvar)))
            | (ImportTypeof, Some t) ->
              Tvar.mk_where cx reason (fun tvar ->
                  rec_flow cx trace (t, ImportTypeofT (reason, export_name, tvar)))
            | (ImportTypeof, None) when has_every_named_export ->
              let t = AnyT.untyped reason in
              Tvar.mk_where cx reason (fun tvar ->
                  rec_flow cx trace (t, ImportTypeofT (reason, export_name, tvar)))
            | (ImportValue, Some t) ->
              rec_flow cx trace (t, AssertImportIsValueT (reason, export_name));
              t
            | (ImportValue, None) when has_every_named_export ->
              let t = AnyT.untyped reason in
              rec_flow cx trace (t, AssertImportIsValueT (reason, export_name));
              t
            | (_, None) ->
              let num_exports = SMap.cardinal exports_tmap in
              let has_default_export = SMap.find_opt "default" exports_tmap <> None in
              let msg =
                if num_exports = 1 && has_default_export then
                  Error_message.EOnlyDefaultExport (reason, module_name, export_name)
                else
                  let known_exports = SMap.keys exports_tmap in
                  let suggestion = typo_suggestion known_exports export_name in
                  Error_message.ENoNamedExport (reason, module_name, export_name, suggestion)
              in
              add_output cx ~trace msg;
              AnyT.error reason
          in
          rec_flow_t ~use_op:unknown_use cx trace (import_t, t)
        | (AnyT (lreason, src), (CJSRequireT (reason, t, _) | ImportModuleNsT (reason, t, _))) ->
          let () =
            match desc_of_reason lreason with
            (* Use a special reason so we can tell the difference between an any-typed import
             * from an untyped module and an any-typed import from a nonexistent module. *)
            | RUntypedModule module_name ->
              let loc = Reason.aloc_of_reason reason in
              let message = Error_message.EUntypedImport (loc, module_name) in
              add_output cx ~trace message
            | _ -> ()
          in
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src reason, t)
        | (AnyT (lreason, src), ImportDefaultT (reason, import_kind, _, t, _)) ->
          let () =
            match (import_kind, desc_of_reason lreason) with
            (* Use a special reason so we can tell the difference between an any-typed type import
             * from an untyped module and an any-typed import from a nonexistent module. *)
            | ((ImportType | ImportTypeof), RUntypedModule module_name) ->
              let loc = Reason.aloc_of_reason reason in
              let message = Error_message.EUntypedTypeImport (loc, module_name) in
              add_output cx ~trace message
            | (ImportValue, RUntypedModule module_name) ->
              let loc = Reason.aloc_of_reason reason in
              let message = Error_message.EUntypedImport (loc, module_name) in
              add_output cx ~trace message
            | _ -> ()
          in
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src reason, t)
        | (AnyT (lreason, src), ImportNamedT (reason, import_kind, _, _, t, _)) ->
          let () =
            match (import_kind, desc_of_reason lreason) with
            (* Use a special reason so we can tell the difference between an any-typed type import
             * from an untyped module and an any-typed type import from a nonexistent module. *)
            | ((ImportType | ImportTypeof), RUntypedModule module_name) ->
              let loc = Reason.aloc_of_reason reason in
              let message = Error_message.EUntypedTypeImport (loc, module_name) in
              add_output cx ~trace message
            | (ImportValue, RUntypedModule module_name) ->
              let loc = Reason.aloc_of_reason reason in
              let message = Error_message.EUntypedImport (loc, module_name) in
              add_output cx ~trace message
            | _ -> ()
          in
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src reason, t)
        | ( (DefT (_, _, PolyT { t_out = DefT (_, _, TypeT _); _ }) | DefT (_, _, TypeT _)),
            AssertImportIsValueT (reason, name) ) ->
          add_output cx ~trace (Error_message.EImportTypeAsValue (reason, name))
        | (_, AssertImportIsValueT (_, _)) -> ()
        (*******************************)
        (* common implicit conversions *)
        (*******************************)
        | (_, UseT (_, DefT (_, _, NumT _))) when numeric l -> ()
        (*
         * Handling for the idx() custom function.
         *
         * idx(a, a => a.b.c) is a 2-arg function with semantics meant to simlify
         * the process of extracting a property from a chain of maybe-typed property
         * accesses.
         *
         * As an example, if you consider an object type such as:
         *
         *   {
         *     me: ?{
         *       firstName: string,
         *       lastName: string,
         *       friends: ?Array<User>,
         *     }
         *   }
         *
         * The process of getting to the friends of my first friend (safely) looks
         * something like this:
         *
         *   let friendsOfFriend = obj.me && obj.me.friends && obj.me.friends[0]
         *                         && obj.me.friends[0].friends;
         *
         * This is verbose to say the least. To simplify, we can define a function
         * called idx() as:
         *
         *   function idx(obj, callback) {
         *     try { return callback(obj); } catch (e) {
         *       if (isNullPropertyAccessError(e)) {
         *         return null;
         *       } else {
         *         throw e;
         *       }
         *     }
         *   }
         *
         * This function can then be used to safely dive into the aforementioned
         * object tersely:
         *
         *  let friendsOfFriend = idx(obj, obj => obj.me.friends[0].friends);
         *
         * If we assume these semantics, then we can model the type of this function
         * by wrapping the `obj` parameter in a special signifying wrapper type that
         * is only valid against use types associated with property accesses. Any
         * time this specially wrapper type flows into a property access operation,
         * we:
         *
         * 1) Strip away any potential MaybeT from the contained type
         * 2) Forward the un-Maybe'd type on to the access operation
         * 3) Wrap the result back in the special wrapper
         *
         * We can then flow this wrapped `obj` to a call on the callback function,
         * remove the wrapper from the return type, and return that value wrapped in
         * a MaybeT.
         *
         * ...of course having a `?.` operator in the language would be a nice
         *    reason to throw all of this clownerous hackery away...
         *)
        | ( CustomFunT (lreason, Idx),
            CallT
              ( use_op,
                reason_op,
                {
                  call_this_t;
                  call_targs;
                  call_args_tlist;
                  call_tout;
                  call_closure_t;
                  call_strict_arity;
                } ) ) ->
          let tout =
            match (call_targs, call_args_tlist) with
            | (None, [Arg obj; Arg cb]) ->
              let wrapped_obj = DefT (reason_op, bogus_trust (), IdxWrapper obj) in
              let callback_result =
                Tvar.mk_no_wrap_where cx reason_op (fun call_tout ->
                    rec_flow
                      cx
                      trace
                      ( cb,
                        CallT
                          ( use_op,
                            reason_op,
                            {
                              call_this_t;
                              call_targs = None;
                              call_args_tlist = [Arg wrapped_obj];
                              call_tout;
                              call_closure_t;
                              call_strict_arity;
                            } ) ))
              in
              let unwrapped_t =
                Tvar.mk_where cx reason_op (fun t ->
                    rec_flow cx trace (callback_result, IdxUnwrap (reason_op, t)))
              in
              let maybe_r = update_desc_reason (fun desc -> RMaybe desc) reason_op in
              MaybeT (maybe_r, unwrapped_t)
            | (None, SpreadArg t1 :: SpreadArg t2 :: _) ->
              add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t1, SpreadArgument));
              add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t2, SpreadArgument));
              AnyT.error reason_op
            | (None, SpreadArg t :: _)
            | (None, _ :: SpreadArg t :: _) ->
              let spread_loc = loc_of_t t in
              add_output cx ~trace Error_message.(EUnsupportedSyntax (spread_loc, SpreadArgument));
              AnyT.error reason_op
            | (Some _, _) ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = aloc_of_reason reason_op;
                      is_new = false;
                      reason_arity = lreason;
                      expected_arity = 0;
                    });
              AnyT.error reason_op
            | _ ->
              (* Why is idx strict about arity? No other functions are. *)
              add_output cx ~trace Error_message.(EIdxArity reason_op);
              AnyT.error reason_op
          in
          rec_flow_t ~use_op:unknown_use cx trace (tout, OpenT call_tout)
        (* Unwrap idx() callback param *)
        | (DefT (_, _, IdxWrapper obj), IdxUnwrap (_, t)) ->
          rec_flow_t ~use_op:unknown_use cx trace (obj, t)
        | (_, IdxUnwrap (_, t)) -> rec_flow_t ~use_op:unknown_use cx trace (l, t)
        (* De-maybe-ify an idx() property access *)
        | (MaybeT (_, inner_t), IdxUnMaybeifyT _)
        | (OptionalT { reason = _; type_ = inner_t; use_desc = _ }, IdxUnMaybeifyT _) ->
          rec_flow cx trace (inner_t, u)
        | (DefT (_, _, NullT), IdxUnMaybeifyT _) -> ()
        | (DefT (_, _, VoidT), IdxUnMaybeifyT _) -> ()
        | (_, IdxUnMaybeifyT (_, t))
          when match l with
               | UnionT _
               | IntersectionT _ ->
                 false
               | _ -> true ->
          rec_flow_t ~use_op:unknown_use cx trace (l, t)
        (* The set of valid uses of an idx() callback parameter. In general this
       should be limited to the various forms of property access operations. *)
        | (DefT (idx_reason, trust, IdxWrapper obj), ReposLowerT (reason_op, use_desc, u)) ->
          let repositioned_obj =
            Tvar.mk_where cx reason_op (fun t ->
                rec_flow cx trace (obj, ReposLowerT (reason_op, use_desc, UseT (unknown_use, t))))
          in
          rec_flow cx trace (DefT (idx_reason, trust, IdxWrapper repositioned_obj), u)
        | (DefT (idx_reason, trust, IdxWrapper obj), GetPropT (use_op, reason_op, propname, t_out))
          ->
          let de_maybed_obj =
            Tvar.mk_where cx idx_reason (fun t ->
                rec_flow cx trace (obj, IdxUnMaybeifyT (idx_reason, t)))
          in
          let prop_type =
            Tvar.mk_no_wrap_where cx reason_op (fun t ->
                rec_flow cx trace (de_maybed_obj, GetPropT (use_op, reason_op, propname, t)))
          in
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (DefT (idx_reason, trust, IdxWrapper prop_type), OpenT t_out)
        | ( DefT (idx_reason, trust, IdxWrapper obj),
            GetPrivatePropT (use_op, reason_op, name, class_bindings, static, t_out) ) ->
          let de_maybed_obj =
            Tvar.mk_where cx idx_reason (fun t ->
                rec_flow cx trace (obj, IdxUnMaybeifyT (idx_reason, t)))
          in
          let prop_type =
            Tvar.mk_no_wrap_where cx reason_op (fun t ->
                rec_flow
                  cx
                  trace
                  ( de_maybed_obj,
                    GetPrivatePropT (use_op, reason_op, name, class_bindings, static, t) ))
          in
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (DefT (idx_reason, trust, IdxWrapper prop_type), OpenT t_out)
        | (DefT (idx_reason, trust, IdxWrapper obj), GetElemT (use_op, reason_op, prop, t_out)) ->
          let de_maybed_obj =
            Tvar.mk_where cx idx_reason (fun t ->
                rec_flow cx trace (obj, IdxUnMaybeifyT (idx_reason, t)))
          in
          let prop_type =
            Tvar.mk_no_wrap_where cx reason_op (fun t ->
                rec_flow cx trace (de_maybed_obj, GetElemT (use_op, reason_op, prop, t)))
          in
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (DefT (idx_reason, trust, IdxWrapper prop_type), OpenT t_out)
        | (DefT (reason, _, IdxWrapper _), UseT _) ->
          add_output cx ~trace (Error_message.EIdxUse1 reason)
        | (DefT (reason, _, IdxWrapper _), _) ->
          add_output cx ~trace (Error_message.EIdxUse2 reason)
        (*********************)
        (* type assert calls *)
        (*********************)
        | (CustomFunT (fun_reason, TypeAssertIs), CallT (use_op, reason_op, call_type))
        | (CustomFunT (fun_reason, TypeAssertThrows), CallT (use_op, reason_op, call_type))
        | (CustomFunT (fun_reason, TypeAssertWraps), CallT (use_op, reason_op, call_type)) ->
          let call_loc = aloc_of_reason reason_op in
          let fun_loc = aloc_of_reason fun_reason in
          let fun_reason_new = mk_reason RFunctionType fun_loc in
          (* Add Flow errors for calls that attempt to assert types that cannot be
      checked at runtime. *)
          let reason = mk_reason (RCustom "TypeAssert library function") call_loc in
          let return_t =
            match call_type.call_targs with
            | None ->
              add_output cx ~trace (Error_message.ETooFewTypeArgs (reason, reason, 1));
              AnyT.at (AnyError None) fun_loc
            | Some [ExplicitArg t] ->
              let (kind, return_t) =
                match l with
                | CustomFunT (_, TypeAssertIs) ->
                  (Context.Is, BoolT.at fun_loc |> with_trust bogus_trust)
                | CustomFunT (_, TypeAssertThrows) -> (Context.Throws, t)
                | CustomFunT (_, TypeAssertWraps) ->
                  (* For TypeAssertWraps, return type is Result<T> *)
                  let mk_bool b =
                    DefT (mk_reason (RBooleanLit b) fun_loc, bogus_trust (), SingletonBoolT b)
                  in
                  let pmap_fail =
                    SMap.empty
                    |> Properties.add_field "success" Polarity.Neutral None (mk_bool false)
                    |> Properties.add_field
                         "error"
                         Polarity.Neutral
                         None
                         (StrT.at fun_loc |> with_trust bogus_trust)
                  in
                  let pmap_succ =
                    SMap.empty
                    |> Properties.add_field "success" Polarity.Neutral None (mk_bool true)
                    |> Properties.add_field "value" Polarity.Neutral None t
                  in
                  let (id_succ, id_fail) =
                    ( Context.generate_property_map cx pmap_fail,
                      Context.generate_property_map cx pmap_succ )
                  in
                  let reason = mk_reason (RCustom "Result<T>") fun_loc in
                  let (obj_fail, obj_succ) =
                    ( mk_object_def_type ~reason ~call:None id_fail dummy_prototype,
                      mk_object_def_type ~reason ~call:None id_succ dummy_prototype )
                  in
                  ( Context.Wraps,
                    UnionT (mk_reason RUnion fun_loc, UnionRep.make obj_fail obj_succ []) )
                | _ -> failwith "cannot reach this case"
              in
              Context.add_type_assert cx call_loc (kind, TypeUtil.loc_of_t t);
              return_t
            | Some _ ->
              add_output cx ~trace (Error_message.ETooManyTypeArgs (reason, reason, 1));
              AnyT.at (AnyError None) fun_loc
          in
          let funtype =
            DefT
              ( fun_reason_new,
                bogus_trust (),
                FunT
                  ( dummy_static reason,
                    mk_reason RPrototype fun_loc |> Unsoundness.function_proto_any,
                    {
                      this_t = mk_reason RThis fun_loc |> MixedT.make |> with_trust bogus_trust;
                      params = [(Some "value", MixedT.at fun_loc |> with_trust bogus_trust)];
                      rest_param = None;
                      return_t;
                      is_predicate = false;
                      closure_t = 0;
                      changeset = Changeset.empty;
                      def_reason = fun_reason_new;
                    } ) )
          in
          rec_flow
            cx
            trace
            (funtype, CallT (use_op, reason_op, { call_type with call_targs = None }))
        (*********************)
        (* optional chaining *)
        (*********************)
        | (DefT (_, _, VoidT), OptionalChainT (r', lhs_reason, _, _, void_out)) ->
          Context.mark_optional_chain cx (aloc_of_reason r') lhs_reason ~useful:true;
          rec_flow_t ~use_op:unknown_use cx trace (l, void_out)
        | (DefT (r, trust, NullT), OptionalChainT (r', lhs_reason, _, _, void_out)) ->
          let void =
            match desc_of_reason r with
            | RNull ->
              (* to avoid error messages like "null is incompatible with null",
                 give VoidT that arise from `null` annotations a new description
                 explaining why it is void and not null *)
              DefT (replace_desc_reason RVoidedNull r, trust, VoidT)
            | _ -> DefT (r, trust, VoidT)
          in
          Context.mark_optional_chain cx (aloc_of_reason r') lhs_reason ~useful:true;
          rec_flow_t ~use_op:unknown_use cx trace (void, void_out)
        | (_, OptionalChainT (r', lhs_reason, this, t_out, _))
          when match l with
               | MaybeT _
               | OptionalT _
               | UnionT _
               | IntersectionT _ ->
                 false
               | _ -> true ->
          Context.mark_optional_chain
            cx
            (aloc_of_reason r')
            lhs_reason
            ~useful:
              (match l with
              | DefT (_, _, MixedT _)
              | AnyT _ ->
                true
              | _ -> false);
          rec_flow_t ~use_op:unknown_use cx trace (l, this);
          rec_flow cx trace (l, t_out)
        (*************)
        (* invariant *)
        (*************)
        | (_, InvariantT r') ->
          Context.mark_invariant
            cx
            (aloc_of_reason r')
            (reason_of_t l)
            ~useful:
              (match Type_filter.not_exists l with
              | DefT (_, _, EmptyT Bottom) -> false
              | _ -> true)
        (***************)
        (* maybe types *)
        (***************)

        (* The type maybe(T) is the same as null | undefined | UseT *)
        | (DefT (r, trust, (NullT | VoidT)), UseT (use_op, MaybeT (_, tout)))
        | (DefT (r, trust, (NullT | VoidT)), FilterMaybeT (use_op, tout)) ->
          rec_flow_t cx trace ~use_op (EmptyT.why r trust, tout)
        | (DefT (r, trust, MixedT Mixed_everything), UseT (use_op, MaybeT (_, tout)))
        | (DefT (r, trust, MixedT Mixed_everything), FilterMaybeT (use_op, tout)) ->
          rec_flow_t cx trace ~use_op (DefT (r, trust, MixedT Mixed_non_maybe), tout)
        | (OptionalT { reason = _; type_ = tout; use_desc = _ }, FilterMaybeT _)
        | (MaybeT (_, tout), FilterMaybeT _) ->
          rec_flow cx trace (tout, u)
        | (MaybeT _, ReposLowerT (reason_op, use_desc, u)) ->
          (* Don't split the maybe type into its constituent members. Instead,
         reposition the entire maybe type. *)
          let loc = aloc_of_reason reason_op in
          let desc =
            if use_desc then
              Some (desc_of_reason reason_op)
            else
              None
          in
          rec_flow cx trace (reposition cx ~trace loc ?desc l, u)
        | (MaybeT (r, t), DestructuringT (reason, DestructAnnot, s, tout, _)) ->
          let f t =
            AnnotT
              ( reason,
                Tvar.mk_no_wrap_where cx reason (fun tvar ->
                    rec_flow
                      cx
                      trace
                      (t, DestructuringT (reason, DestructAnnot, s, tvar, Reason.mk_id ()))),
                false )
          in
          let void_t = VoidT.why r |> with_trust bogus_trust in
          let null_t = NullT.why r |> with_trust bogus_trust in
          let t = push_type_alias_reason r t in
          let rep = UnionRep.make (f void_t) (f null_t) [f t] in
          rec_unify cx trace ~use_op:unknown_use (UnionT (reason, rep)) (OpenT tout)
        | (MaybeT (_, t), ObjAssignFromT (_, _, _, _, ObjAssign _)) ->
          (* This isn't correct, but matches the existing incorrectness of spreads
           * today. In particular, spreading `null` and `void` become {}. The wrong
           * part is that spreads should distribute through unions, so `{...?T}`
           * should be `{...null}|{...void}|{...T}`, which simplifies to `{}`. *)
          rec_flow cx trace (t, u)
        | (MaybeT (r, t), UseT (_, MaybeT _)) ->
          let t = push_type_alias_reason r t in
          rec_flow cx trace (t, u)
        | (MaybeT _, ResolveUnionT { reason; resolved; unresolved; upper; id }) ->
          resolve_union cx trace reason id resolved unresolved l upper
        | (MaybeT (reason, t), _) ->
          let reason = replace_desc_reason RNullOrVoid reason in
          let t = push_type_alias_reason reason t in
          rec_flow cx trace (NullT.make reason |> with_trust Trust.bogus_trust, u);
          rec_flow cx trace (VoidT.make reason |> with_trust Trust.bogus_trust, u);
          rec_flow cx trace (t, u)
        (******************)
        (* optional types *)
        (******************)

        (* The type optional(T) is the same as undefined | UseT *)
        | ( DefT (r, trust, VoidT),
            UseT (use_op, OptionalT { reason = _; type_ = tout; use_desc = _ }) )
        | (DefT (r, trust, VoidT), FilterOptionalT (use_op, tout)) ->
          rec_flow_t cx trace ~use_op (EmptyT.why r trust, tout)
        | (OptionalT { reason = _; type_ = tout; use_desc = _ }, FilterOptionalT _) ->
          rec_flow cx trace (tout, u)
        | (OptionalT _, ReposLowerT (reason, use_desc, u)) ->
          (* Don't split the optional type into its constituent members. Instead,
         reposition the entire optional type. *)
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | ( OptionalT { reason = r; type_ = t; use_desc },
            DestructuringT (reason, DestructAnnot, s, tout, _) ) ->
          let f t =
            AnnotT
              ( reason,
                Tvar.mk_no_wrap_where cx reason (fun tvar ->
                    rec_flow
                      cx
                      trace
                      (t, DestructuringT (reason, DestructAnnot, s, tvar, Reason.mk_id ()))),
                false )
          in
          let void_t = VoidT.why_with_use_desc ~use_desc r |> with_trust bogus_trust in
          let rep = UnionRep.make (f void_t) (f t) [] in
          rec_unify cx trace ~use_op:unknown_use (UnionT (reason, rep)) (OpenT tout)
        | ( OptionalT { reason = _; type_ = t; use_desc = _ },
            ObjAssignFromT (_, _, _, _, ObjAssign _) ) ->
          (* This isn't correct, but matches the existing incorrectness of spreads
           * today. In particular, spreading `null` and `void` become {}. The wrong
           * part is that spreads should distribute through unions, so `{...?T}`
           * should be `{...null}|{...void}|{...T}`, which simplifies to `{}`. *)
          rec_flow cx trace (t, u)
        | (OptionalT { reason = _; type_ = t; use_desc = _ }, UseT (_, OptionalT _))
        | (OptionalT { reason = _; type_ = t; use_desc = _ }, UseT (_, MaybeT _)) ->
          rec_flow cx trace (t, u)
        | (OptionalT _, ResolveUnionT { reason; resolved; unresolved; upper; id }) ->
          resolve_union cx trace reason id resolved unresolved l upper
        | (OptionalT { reason = r; type_ = t; use_desc }, _) ->
          rec_flow cx trace (VoidT.why_with_use_desc ~use_desc r |> with_trust Trust.bogus_trust, u);
          rec_flow cx trace (t, u)
        (*****************)
        (* logical types *)
        (*****************)

        (* !x when x is of unknown truthiness *)
        | (DefT (_, trust, BoolT None), NotT (reason, tout))
        | (DefT (_, trust, StrT AnyLiteral), NotT (reason, tout))
        | (DefT (_, trust, NumT AnyLiteral), NotT (reason, tout)) ->
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (BoolT.at (aloc_of_reason reason) trust, OpenT tout)
        (* !x when x is falsy *)
        | (DefT (_, trust, BoolT (Some false)), NotT (reason, tout))
        | (DefT (_, trust, SingletonBoolT false), NotT (reason, tout))
        | (DefT (_, trust, StrT (Literal (_, ""))), NotT (reason, tout))
        | (DefT (_, trust, SingletonStrT ""), NotT (reason, tout))
        | (DefT (_, trust, NumT (Literal (_, (0., _)))), NotT (reason, tout))
        | (DefT (_, trust, SingletonNumT (0., _)), NotT (reason, tout))
        | (DefT (_, trust, NullT), NotT (reason, tout))
        | (DefT (_, trust, VoidT), NotT (reason, tout)) ->
          let reason = replace_desc_reason (RBooleanLit true) reason in
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (DefT (reason, trust, BoolT (Some true)), OpenT tout)
        | (UnionT (_, rep), NotT _) -> flow_all_in_union cx trace rep u
        (* !x when x is truthy *)
        | (_, NotT (reason, tout)) ->
          let reason = replace_desc_reason (RBooleanLit false) reason in
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (DefT (reason, bogus_trust (), BoolT (Some false)), OpenT tout)
        | (left, AndT (_, right, u)) ->
          begin
            match left with
            | DefT (reason, _, NumT _) ->
              add_output
                cx
                ~trace
                (Error_message.ESketchyNumberLint (Lints.SketchyNumberAnd, reason))
            | _ -> ()
          end;

          (* a falsy && b ~> a
         a truthy && b ~> b
         a && b ~> a falsy | b *)
          (match Type_filter.exists left with
          | DefT (_, _, EmptyT Bottom) ->
            (* falsy *)
            rec_flow cx trace (left, PredicateT (NotP (ExistsP None), u))
          | _ ->
            (match Type_filter.not_exists left with
            | DefT (_, _, EmptyT Bottom) ->
              (* truthy *)
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))
            | _ ->
              rec_flow cx trace (left, PredicateT (NotP (ExistsP None), u));
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))))
        | (left, OrT (_, right, u)) ->
          (* a truthy || b ~> a
         a falsy || b ~> b
         a || b ~> a truthy | b *)
          (match Type_filter.not_exists left with
          | DefT (_, _, EmptyT Bottom) ->
            (* truthy *)
            rec_flow cx trace (left, PredicateT (ExistsP None, u))
          | _ ->
            (match Type_filter.exists left with
            | DefT (_, _, EmptyT Bottom) ->
              (* falsy *)
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))
            | _ ->
              rec_flow cx trace (left, PredicateT (ExistsP None, u));
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))))
        (* a not-nullish ?? b ~> a
         a nullish ?? b ~> b
         a ?? b ~> a not-nullish | b *)
        | (left, NullishCoalesceT (_, right, u)) ->
          (match Type_filter.maybe left with
          | DefT (_, _, EmptyT Bottom)
          (* This `AnyT` case is required to have similar behavior to the other logical operators. *)
          | AnyT _ ->
            (* not-nullish *)
            rec_flow cx trace (left, PredicateT (NotP MaybeP, u))
          | _ ->
            (match Type_filter.not_maybe left with
            | DefT (_, _, EmptyT Bottom) ->
              (* nullish *)
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))
            | _ ->
              rec_flow cx trace (left, PredicateT (NotP MaybeP, u));
              rec_flow cx trace (right, UseT (unknown_use, OpenT u))))
        | (_, ReactKitT (use_op, reason_op, React.CreateElement0 (clone, config, children, tout)))
          ->
          let tool = React.CreateElement (clone, l, config, children, tout) in
          rec_flow cx trace (l, ReactKitT (use_op, reason_op, tool))
        (*********************)
        (* type applications *)
        (*********************)

        (* Sometimes a polymorphic class may have a polymorphic method whose return
       type is a type application on the same polymorphic class, possibly
       expanded. See Array#map or Array#concat, e.g. It is not unusual for
       programmers to reuse variables, assigning the result of a method call on
       a variable to itself, in which case we could get into cycles of unbounded
       instantiation. We use caching to cut these cycles. Caching relies on
       reasons (see module Cache.I). This is OK since intuitively, there should
       be a unique instantiation of a polymorphic definition for any given use
       of it in the source code.

       In principle we could use caching more liberally, but we don't because
       not all use types arise from source code, and because reasons are not
       perfect. Indeed, if we tried caching for all use types, we'd lose
       precision and report spurious errors.

       Also worth noting is that we can never safely cache def types. This is
       because substitution of type parameters in def types does not affect
       their reasons, so we'd trivially lose precision. *)
        | (ThisTypeAppT (reason_tapp, c, this, ts), _) ->
          let reason_op = reason_of_use_t u in
          let tc = specialize_class cx trace ~reason_op ~reason_tapp c ts in
          instantiate_this_class cx trace reason_tapp tc this (Upper u)
        | (_, UseT (use_op, ThisTypeAppT (reason_tapp, c, this, ts))) ->
          let reason_op = reason_of_t l in
          let tc = specialize_class cx trace ~reason_op ~reason_tapp c ts in
          instantiate_this_class cx trace reason_tapp tc this (Lower (use_op, l))
        | (TypeAppT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (TypeAppT (reason_tapp, use_op, c, ts), MethodT (_, _, _, _, _, _)) ->
          let reason_op = reason_of_use_t u in
          let t = mk_typeapp_instance cx ~trace ~use_op ~reason_op ~reason_tapp ~cache:[] c ts in
          rec_flow cx trace (t, u)
        (* If we have a TypeAppT (c, ts) ~> TypeAppT (c, ts) then we want to
         * concretize both cs to PolyTs so that we may referentially compare them.
         * We cannot compare the non-concretized versions since they may have been
         * reposition, they may be two OpenTs from different locations, or any other
         * way you can access the same PolyT via different means that results in a
         * different c being passed to TypeAppT.
         *
         * We use the ConcretizeTypeAppsT use type to concretize both the c of our
         * upper and lower TypeAppT bound. We start by concretizing the upper bound
         * which we signal by setting the final element in ConcretizeTypeAppsT to
         * true. *)
        | (TypeAppT (r1, op1, c1, ts1), UseT (use_op, TypeAppT (r2, op2, c2, ts2))) ->
          if TypeAppExpansion.push_unless_loop cx (c1, ts1) then (
            if TypeAppExpansion.push_unless_loop cx (c2, ts2) then (
              rec_flow
                cx
                trace
                (c2, ConcretizeTypeAppsT (use_op, (ts2, op2, r2), (c1, ts1, op1, r1), true));
              TypeAppExpansion.pop ()
            );
            TypeAppExpansion.pop ()
          )
        (* When we have concretized the c for our upper bound TypeAppT then we want
         * to concretize the lower bound. We flip all our arguments to
         * ConcretizeTypeAppsT and set the final element to false to signal that we
         * have concretized the upper bound's c.
         *
         * If the upper bound's c is not a PolyT then we will fall down to an
         * incompatible use error. *)
        | ( (DefT (_, _, PolyT _) as c2),
            ConcretizeTypeAppsT (use_op, (ts2, op2, r2), (c1, ts1, op1, r1), true) ) ->
          rec_flow
            cx
            trace
            (c1, ConcretizeTypeAppsT (use_op, (ts1, op1, r1), (c2, ts2, op2, r2), false))
        (* When we have concretized the c for our lower bound TypeAppT then we can
         * finally run our TypeAppT ~> TypeAppT logic. If we have referentially the
         * same PolyT for each TypeAppT then we want to check the type arguments
         * only. (Checked in the when condition.) If we do not have the same PolyT
         * for each TypeAppT then we want to expand our TypeAppTs and compare the
         * expanded results.
         *
         * If the lower bound's c is not a PolyT then we will fall down to an
         * incompatible use error.
         *
         * The upper bound's c should always be a PolyT here since we could not have
         * made it here if it was not given the logic of our earlier case. *)
        | ( DefT (_, _, PolyT { id = id1; _ }),
            ConcretizeTypeAppsT
              (use_op, (ts1, _, r1), (DefT (_, _, PolyT { id = id2; _ }), ts2, _, r2), false) )
          when id1 = id2 && List.length ts1 = List.length ts2 ->
          let targs = List.map2 (fun t1 t2 -> (t1, t2)) ts1 ts2 in
          rec_flow cx trace (l, TypeAppVarianceCheckT (use_op, r1, r2, targs))
        (* This is the case which implements the expansion for our
         * TypeAppT (c, ts) ~> TypeAppT (c, ts) when the cs are unequal. *)
        | ( DefT (_, _, PolyT { tparams_loc = tparams_loc1; tparams = xs1; t_out = t1; id = id1 }),
            ConcretizeTypeAppsT
              ( use_op,
                (ts1, op1, r1),
                ( DefT
                    (_, _, PolyT { tparams_loc = tparams_loc2; tparams = xs2; t_out = t2; id = id2 }),
                  ts2,
                  op2,
                  r2 ),
                false ) ) ->
          let (op1, op2) =
            match root_of_use_op use_op with
            | UnknownUse -> (op1, op2)
            | _ -> (use_op, use_op)
          in
          let t1 =
            mk_typeapp_instance_of_poly
              cx
              trace
              ~use_op:op2
              ~reason_op:r2
              ~reason_tapp:r1
              id1
              tparams_loc1
              xs1
              t1
              ts1
          in
          let t2 =
            mk_typeapp_instance_of_poly
              cx
              trace
              ~use_op:op1
              ~reason_op:r1
              ~reason_tapp:r2
              id2
              tparams_loc2
              xs2
              t2
              ts2
          in
          rec_flow cx trace (t1, UseT (use_op, t2))
        | (TypeAppT (reason_tapp, use_op, c, ts), _) ->
          if TypeAppExpansion.push_unless_loop cx (c, ts) then (
            let reason_op = reason_of_use_t u in
            let t = mk_typeapp_instance cx ~trace ~use_op ~reason_op ~reason_tapp c ts in
            rec_flow cx trace (t, u);
            TypeAppExpansion.pop ()
          )
        | (_, UseT (use_op, TypeAppT (reason_tapp, use_op_tapp, c, ts))) ->
          if TypeAppExpansion.push_unless_loop cx (c, ts) then (
            let reason_op = reason_of_t l in
            let t =
              mk_typeapp_instance cx ~trace ~use_op:use_op_tapp ~reason_op ~reason_tapp c ts
            in
            rec_flow cx trace (l, UseT (use_op, t));
            TypeAppExpansion.pop ()
          )
        (**********************)
        (*    opaque types    *)
        (**********************)

        (* If the ids are equal, we use flow_type_args to make sure that the type arguments of each
         * are compatible with each other. If there are no type args, this doesn't do anything *)
        | ( OpaqueT (lreason, { opaque_id = id1; opaque_type_args = ltargs; _ }),
            UseT (use_op, OpaqueT (ureason, { opaque_id = id2; opaque_type_args = utargs; _ })) )
          when ALoc.equal_id id1 id2 ->
          flow_type_args cx trace ~use_op lreason ureason ltargs utargs
        (* Repositioning should happen before opaque types are considered so that we can
         * have the "most recent" location when we do look at the opaque type *)
        | (OpaqueT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        (* If the type is still in the same file it was defined, we allow it to
         * expose its underlying type information *)
        | (OpaqueT (r, { underlying_t = Some t; _ }), _)
          when ALoc.source (aloc_of_reason r) = ALoc.source (def_aloc_of_reason r) ->
          rec_flow cx trace (t, u)
        (* If the lower bound is in the same file as where the opaque type was defined,
         * we expose the underlying type information *)
        | (_, UseT (use_op, OpaqueT (r, { underlying_t = Some t; _ })))
          when ALoc.source (aloc_of_reason (reason_of_t l)) = ALoc.source (def_aloc_of_reason r) ->
          rec_flow cx trace (l, UseT (use_op, t))
        (*****************************************************************)
        (* Intersection type preprocessing for certain object predicates *)
        (*****************************************************************)

        (* Predicate refinements on intersections of object types need careful
       handling. An intersection of object types passes a predicate when any of
       those object types passes the predicate: however, the refined type must
       be the intersection as a whole, not the particular object type that
       passes the predicate! (For example, we may check some condition on
       property x and property y of { x: ... } & { y: ... } in sequence, and not
       expect to get property-not-found errors in the process.)

       Although this seems like a special case, it's not. An intersection of
       object types should behave more or less the same as a "concatenated"
       object type with all the properties of those object types. The added
       complication arises as an implementation detail, because we do not
       concatenate those object types explicitly. *)
        | (_, IntersectionPreprocessKitT (_, SentinelPropTest (sense, key, t, inter, tvar))) ->
          sentinel_prop_test_generic key cx trace tvar inter (sense, l, t)
        | ( _,
            IntersectionPreprocessKitT (_, PropExistsTest (sense, key, reason, inter, tvar, preds))
          ) ->
          prop_exists_test_generic key reason cx trace tvar inter sense preds l
        (***********************)
        (* Singletons and keys *)
        (***********************)

        (* Finite keysets over arbitrary objects can be represented by KeysT. While
        it is possible to also represent singleton string types using KeysT (by
        taking the keyset of an object with a single property whose key is that
        string and whose value is ignored), we can model them more directly
        using SingletonStrT. Specifically, SingletonStrT models a type
        annotation that looks like a string literal, which describes a singleton
        set containing that string literal. Going further, other uses of KeysT
        where the underlying object is created solely for the purpose of
        describing a keyset can be modeled using unions of singleton strings.

        One may also legitimately wonder why SingletonStrT(_, key) cannot be
        always replaced by StrT(_, Some key). The reason is that types of the
        latter form (string literal types) are inferred to be the type of string
        literals appearing as values, and we don't want to prematurely narrow
        down the type of the location where such values may appear, since that
        would preclude other strings to be stored in that location. Thus, by
        necessity we allow all string types to flow to StrT (whereas only
        exactly matching string literal types may flow to SingletonStrT).  **)
        | (DefT (rl, _, StrT actual), UseT (use_op, DefT (ru, _, SingletonStrT expected))) ->
          if TypeUtil.literal_eq expected actual then
            ()
          else
            (* TODO: ordered_reasons should not be necessary *)
            let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
            add_output
              cx
              ~trace
              (Error_message.EExpectedStringLit { reason_lower = rl; reason_upper = ru; use_op })
        | (DefT (rl, _, NumT actual), UseT (use_op, DefT (ru, _, SingletonNumT expected))) ->
          if TypeUtil.number_literal_eq expected actual then
            ()
          else
            (* TODO: ordered_reasons should not be necessary *)
            let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
            add_output
              cx
              ~trace
              (Error_message.EExpectedNumberLit { reason_lower = rl; reason_upper = ru; use_op })
        | (DefT (rl, _, BoolT actual), UseT (use_op, DefT (ru, _, SingletonBoolT expected))) ->
          if TypeUtil.boolean_literal_eq expected actual then
            ()
          else
            (* TODO: ordered_reasons should not be necessary *)
            let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
            add_output
              cx
              ~trace
              (Error_message.EExpectedBooleanLit { reason_lower = rl; reason_upper = ru; use_op })
        (*****************************************************)
        (* keys (NOTE: currently we only support string keys *)
        (*****************************************************)
        | (DefT (reason_s, _, StrT literal), UseT (use_op, KeysT (reason_op, o))) ->
          let reason_next =
            match literal with
            | Literal (_, x) -> replace_desc_new_reason (RProperty (Some x)) reason_s
            | _ -> replace_desc_new_reason RUnknownString reason_s
          in
          (* check that o has key x *)
          let u = HasOwnPropT (use_op, reason_next, literal) in
          rec_flow cx trace (o, ReposLowerT (reason_op, false, u))
        | (KeysT _, ToStringT (_, t)) ->
          (* KeysT outputs strings, so we know ToStringT will be a no-op. *)
          rec_flow cx trace (l, t)
        | (KeysT (reason1, o1), _) ->
          (* flow all keys of o1 to u *)
          rec_flow cx trace (o1, GetKeysT (reason1, u))
        (* helpers *)
        | ( DefT (reason_o, _, ObjT { props_tmap = mapr; flags; _ }),
            HasOwnPropT (use_op, reason_op, x) ) ->
          (match (x, flags.obj_kind) with
          (* If we have a literal string and that property exists *)
          | (Literal (_, x), _) when Context.has_prop cx mapr x -> ()
          (* If we have a dictionary, try that next *)
          | (_, Indexed { key; _ }) ->
            rec_flow_t ~use_op:unknown_use cx trace (DefT (reason_op, bogus_trust (), StrT x), key)
          | _ ->
            let (prop, suggestion) =
              match x with
              | Literal (_, prop) -> (Some prop, prop_typo_suggestion cx [mapr] prop)
              | _ -> (None, None)
            in
            let err =
              Error_message.EPropNotFound
                {
                  prop_name = prop;
                  reason_prop = reason_op;
                  reason_obj = reason_o;
                  use_op;
                  suggestion;
                }
            in
            add_output cx ~trace err)
        | ( DefT (reason_o, _, InstanceT (_, _, _, instance)),
            HasOwnPropT (use_op, reason_op, Literal (_, x)) ) ->
          let own_props = Context.find_props cx instance.own_props in
          (match SMap.find_opt x own_props with
          | Some _ -> ()
          | None ->
            let err =
              Error_message.EPropNotFound
                {
                  prop_name = Some x;
                  reason_prop = reason_op;
                  reason_obj = reason_o;
                  use_op;
                  suggestion = prop_typo_suggestion cx [instance.own_props] x;
                }
            in
            add_output cx ~trace err)
        | (DefT (reason_o, _, InstanceT (_, _, _, _)), HasOwnPropT (use_op, reason_op, _)) ->
          let err =
            Error_message.EPropNotFound
              {
                prop_name = None;
                reason_prop = reason_op;
                reason_obj = reason_o;
                use_op;
                suggestion = None;
              }
          in
          add_output cx ~trace err
        (* AnyT has every prop *)
        | (AnyT _, HasOwnPropT _) -> ()
        | (DefT (_, _, ObjT { flags; props_tmap; _ }), GetKeysT (reason_op, keys)) ->
          begin
            match flags.obj_kind with
            | UnsealedInFile _ ->
              rec_flow cx trace (StrT.why reason_op |> with_trust bogus_trust, keys)
            | _ ->
              let dict_t = Obj_type.get_dict_opt flags.obj_kind in
              (* flow the union of keys of l to keys *)
              let keylist =
                SMap.fold
                  (fun x _ acc ->
                    let reason = replace_desc_new_reason (RStringLit x) reason_op in
                    DefT (reason, bogus_trust (), SingletonStrT x) :: acc)
                  (Context.find_props cx props_tmap)
                  []
              in
              rec_flow cx trace (union_of_ts reason_op keylist, keys);
              Base.Option.iter dict_t (fun { key; _ } ->
                  rec_flow cx trace (key, ToStringT (reason_op, keys)))
          end
        | (DefT (_, _, InstanceT (_, _, _, instance)), GetKeysT (reason_op, keys)) ->
          (* methods are not enumerable, so only walk fields *)
          let own_props = Context.find_props cx instance.own_props in
          let keylist =
            SMap.fold
              (fun x _ acc ->
                let reason = replace_desc_new_reason (RStringLit x) reason_op in
                DefT (reason, bogus_trust (), SingletonStrT x) :: acc)
              own_props
              []
          in
          rec_flow cx trace (union_of_ts reason_op keylist, keys)
        | (AnyT _, GetKeysT (reason_op, keys)) ->
          rec_flow cx trace (StrT.why reason_op |> with_trust literal_trust, keys)
        (* In general, typechecking is monotonic in the sense that more constraints
        produce more errors. However, sometimes we may want to speculatively try
        out constraints, backtracking if they produce errors (and removing the
        errors produced). This is useful to typecheck union types and
        intersection types: see below. **)

        (* NOTE: It is important that any def type that simplifies to a union or
        intersection of other def types be processed before we process unions
        and intersections: otherwise we may get spurious errors. **)

        (**********)
        (* values *)
        (**********)
        | (DefT (_, _, ObjT o), GetValuesT (reason, values)) ->
          let {
            flags;
            proto_t = _;
            props_tmap = tmap;
            call_t = _ (* call props excluded from values *);
          } =
            o
          in
          (* Find all of the props. *)
          let props = Context.find_props cx tmap in
          (* Get the read type for all readable properties and discard the rest. *)
          let ts =
            SMap.fold
              (fun _ prop ts ->
                match Property.read_t prop with
                | Some t ->
                  let t =
                    if flags.frozen then
                      match t with
                      | DefT (t_reason, trust, StrT (Literal (_, lit))) ->
                        let t_reason = replace_desc_reason (RStringLit lit) t_reason in
                        DefT (t_reason, trust, SingletonStrT lit)
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
          let values_l = Type_mapper.union_flatten cx ts |> union_of_ts reason in
          rec_flow_t ~use_op:unknown_use cx trace (values_l, values)
        | (DefT (_, _, InstanceT (_, _, _, { own_props; _ })), GetValuesT (reason, values)) ->
          (* Find all of the props. *)
          let props = Context.find_props cx own_props in
          (* Get the read type for all readable properties and discard the rest. *)
          let ts =
            SMap.fold
              (fun key prop ts ->
                match Property.read_t prop with
                (* We don't want to include the property type if its name is the
           internal value "$key" because that will be the type for the instance
           index and not the value. *)
                | Some t when key != "$key" -> t :: ts
                | _ -> ts)
              props
              []
          in
          (* Create a union type from all our selected types. *)
          let values_l = Type_mapper.union_flatten cx ts |> union_of_ts reason in
          rec_flow_t ~use_op:unknown_use cx trace (values_l, values)
        (* Any will always be ok *)
        | (AnyT (_, src), GetValuesT (reason, values)) ->
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src reason, values)
        (********************************)
        (* union and intersection types *)
        (********************************)
        (* We don't want to miss any union optimizations because of unevaluated type destructors, so
           if our union contains any of these problematic types, we force it to resolve its elements before
           considering its upper bound *)
        | (_, ResolveUnionT { reason; resolved; unresolved; upper; id }) ->
          resolve_union cx trace reason id resolved unresolved l upper
        | (UnionT (reason, rep), FilterMaybeT (use_op, tout)) ->
          let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
          let void = VoidT.why reason |> with_trust bogus_trust in
          let null = NullT.why reason |> with_trust bogus_trust in
          let filter_void t = quick_subtype t void in
          let filter_null t = quick_subtype t null in
          let filter_null_and_void t = filter_void t || filter_null t in
          begin
            match UnionRep.check_enum rep with
            | Some _ ->
              rec_flow_t
                ~use_op
                cx
                trace
                (remove_predicate_from_union reason cx filter_null_and_void rep, tout)
            | None -> flow_all_in_union cx trace rep u
          end
        | (UnionT (reason, rep), upper) when UnionRep.members rep |> List.exists is_union_resolvable
          ->
          (* We can't guarantee that  tvars or typeapps get resolved, even though we'd like to believe they will. Instead,
             we separate out all the resolvable types from the union, resolve them, and then rejoin them with the other types once
             they have been resolved. *)
          let (evals, resolved) = UnionRep.members rep |> List.partition is_union_resolvable in
          begin
            match evals with
            | first :: unresolved ->
              rec_flow
                cx
                trace
                (first, ResolveUnionT { reason; resolved; unresolved; upper; id = Reason.mk_id () })
            (* No evals, but we can't get here *)
            | [] -> ()
          end
        (* Don't split the union type into its constituent members. Instead,
        reposition the entire union type. *)
        | (UnionT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (UnionT (reason, rep), MakeExactT (reason_op, k)) ->
          let ts = UnionRep.members rep in
          let f t = ExactT (reason_op, t) in
          let ts' = Base.List.map ts ~f in
          let reason' = repos_reason (aloc_of_reason reason_op) reason in
          continue cx trace (union_of_ts reason' ts') k
        | (UnionT _, SealGenericT { reason = _; id; name; cont }) ->
          let reason = reason_of_t l in
          continue cx trace (GenericT { reason; id; name; bound = l }) cont
        | (UnionT (_, rep), DestructuringT (reason, DestructAnnot, s, tout, _)) ->
          let (t0, (t1, ts)) = UnionRep.members_nel rep in
          let f t =
            AnnotT
              ( reason,
                Tvar.mk_no_wrap_where cx reason (fun tvar ->
                    rec_flow
                      cx
                      trace
                      (t, DestructuringT (reason, DestructAnnot, s, tvar, Reason.mk_id ()))),
                false )
          in
          let rep = UnionRep.make (f t0) (f t1) (Base.List.map ts ~f) in
          rec_unify cx trace ~use_op:unknown_use (UnionT (reason, rep)) (OpenT tout)
        | (UnionT _, ObjKitT (use_op, reason, resolve_tool, tool, tout)) ->
          ObjectKit.run cx trace ~use_op reason resolve_tool tool tout l
        | ( UnionT (r, _),
            CreateObjWithComputedPropT { reason; value = _; tout_tvar = (tout_reason, tout_id) } )
          ->
          Context.computed_property_add_multiple_lower_bounds cx tout_id;
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason, OpenT (tout_reason, tout_id));
          add_output
            cx
            ~trace
            (Error_message.EComputedPropertyWithUnion
               { computed_property_reason = reason; union_reason = r })
        (* cases where there is no loss of precision *)
        | (UnionT _, UseT (_, (UnionT _ as u)))
          when union_optimization_guard cx (Context.trust_errors cx |> TypeUtil.quick_subtype) l u
          ->
          if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path"
        (* Optimization to treat maybe and optional types as special unions for subset comparision *)
        | (UnionT (reason, rep), UseT (use_op, MaybeT (r, maybe))) ->
          let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
          let void = VoidT.why r |> with_trust bogus_trust in
          let null = NullT.why r |> with_trust bogus_trust in
          let filter_void t = quick_subtype t void in
          let filter_null t = quick_subtype t null in
          let filter_null_and_void t = filter_void t || filter_null t in
          let maybe = push_type_alias_reason r maybe in
          (* if the union doesn't contain void or null,
         then everything in it must be upper-bounded by maybe *)
          begin
            match
              ( UnionRep.quick_mem_enum ~quick_subtype void rep,
                UnionRep.quick_mem_enum ~quick_subtype null rep )
            with
            | (UnionRep.No, UnionRep.No) -> rec_flow_t ~use_op cx trace (l, maybe)
            | (UnionRep.Yes, UnionRep.No) ->
              rec_flow_t
                ~use_op
                cx
                trace
                (remove_predicate_from_union reason cx filter_void rep, maybe)
            | (UnionRep.No, UnionRep.Yes) ->
              rec_flow_t
                ~use_op
                cx
                trace
                (remove_predicate_from_union reason cx filter_null rep, maybe)
            | (UnionRep.Yes, UnionRep.Yes) ->
              rec_flow_t
                ~use_op
                cx
                trace
                (remove_predicate_from_union reason cx filter_null_and_void rep, maybe)
            | _ -> flow_all_in_union cx trace rep u
          end
        | (UnionT (reason, rep), UseT (use_op, OptionalT { reason = r; type_ = opt; use_desc })) ->
          let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
          let void = VoidT.why_with_use_desc ~use_desc r |> with_trust bogus_trust in
          let filter_void t = quick_subtype t void in
          (* if the union doesn't contain void, then everything in it must be upper-bounded by u *)
          begin
            match UnionRep.quick_mem_enum ~quick_subtype void rep with
            | UnionRep.No -> rec_flow_t ~use_op cx trace (l, opt)
            | UnionRep.Yes ->
              rec_flow_t
                ~use_op
                cx
                trace
                (remove_predicate_from_union reason cx filter_void rep, opt)
            | _ -> flow_all_in_union cx trace rep u
          end
        | ((UnionT (_, rep1) as u1), EqT { arg = UnionT _ as u2; _ }) ->
          if union_optimization_guard cx (curry equatable) u1 u2 then begin
            if Context.is_verbose cx then prerr_endline "UnionT ~> EqT fast path"
          end else
            flow_all_in_union cx trace rep1 u
        | ((UnionT (_, rep1) as u1), StrictEqT { arg = UnionT _ as u2; cond_context; _ }) ->
          if union_optimization_guard cx (curry (strict_equatable cond_context)) u1 u2 then begin
            if Context.is_verbose cx then prerr_endline "UnionT ~> StrictEqT fast path"
          end else
            flow_all_in_union cx trace rep1 u
        | (UnionT _, EqT { reason; flip; arg }) when needs_resolution arg || is_generic arg ->
          rec_flow cx trace (arg, EqT { reason; flip = not flip; arg = l })
        | (UnionT _, StrictEqT { reason; cond_context; flip; arg })
          when needs_resolution arg || is_generic arg ->
          rec_flow cx trace (arg, StrictEqT { reason; cond_context; flip = not flip; arg = l })
        | (UnionT (r, rep), SentinelPropTestT (_reason, l, _key, sense, sentinel, result)) ->
          (* we have the check l.key === sentinel where l.key is a union *)
          if sense then
            match sentinel with
            | UnionEnum.One enum ->
              let def =
                match enum with
                | UnionEnum.Str v -> SingletonStrT v
                | UnionEnum.Num v -> SingletonNumT v
                | UnionEnum.Bool v -> SingletonBoolT v
                | UnionEnum.Void -> VoidT
                | UnionEnum.Null -> NullT
              in
              (match
                 UnionRep.quick_mem_enum
                   (TypeUtil.quick_subtype (Context.trust_errors cx))
                   (DefT (r, Trust.bogus_trust (), def))
                   rep
               with
              | UnionRep.No -> () (* provably unreachable, so prune *)
              | UnionRep.Yes -> rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
              | UnionRep.Conditional _
              | UnionRep.Unknown ->
                (* inconclusive: the union is not concretized *)
                flow_all_in_union cx trace rep u)
            | UnionEnum.Many enums ->
              let acc =
                UnionEnumSet.fold
                  (fun enum acc ->
                    let def =
                      match enum with
                      | UnionEnum.Str v -> SingletonStrT v
                      | UnionEnum.Num v -> SingletonNumT v
                      | UnionEnum.Bool v -> SingletonBoolT v
                      | UnionEnum.Void -> VoidT
                      | UnionEnum.Null -> NullT
                    in
                    UnionRep.join_quick_mem_results
                      ( acc,
                        UnionRep.quick_mem_enum
                          (TypeUtil.quick_subtype (Context.trust_errors cx))
                          (DefT (r, Trust.bogus_trust (), def))
                          rep ))
                  enums
                  UnionRep.No
              in
              begin
                match acc with
                | UnionRep.No -> () (* provably unreachable, so prune *)
                | UnionRep.Yes -> rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
                | UnionRep.Conditional _
                | UnionRep.Unknown ->
                  (* inconclusive: the union is not concretized *)
                  flow_all_in_union cx trace rep u
              end
          else
            (* for l.key !== sentinel where l.key is a union, we can't really prove
           that the check is guaranteed to fail (assuming the union doesn't
           degenerate to a singleton) *)
            rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
        | ( UnionT (_, rep),
            PredicateT (((MaybeP | NotP MaybeP | ExistsP _ | NotP (ExistsP _)) as p), t) )
          when UnionRep.is_optimized_finally rep ->
          predicate cx trace t l p
        | (UnionT (_, rep), _)
          when match u with
               (* For l.key !== sentinel when sentinel has a union type, don't split the union. This
           prevents a drastic blowup of cases which can cause perf problems. *)
               | PredicateT (RightP (SentinelProp _, _), _)
               | PredicateT (NotP (RightP (SentinelProp _, _)), _) ->
                 false
               | _ -> true ->
          ( if Context.is_verbose cx then
            match u with
            | UseT (_, UnionT _) -> prerr_endline "UnionT ~> UnionT slow case"
            | UseT (_, IntersectionT _) -> prerr_endline "UnionT ~> IntersectionT slow case"
            | _ -> () );
          flow_all_in_union cx trace rep u
        | (_, UseT (use_op, IntersectionT (_, rep))) ->
          ( if Context.is_verbose cx then
            match l with
            | UnionT _ -> prerr_endline "IntersectionT ~> UnionT slow case"
            | _ -> () );
          InterRep.members rep |> List.iter (fun t -> rec_flow cx trace (l, UseT (use_op, t)))
        (* When a subtyping question involves a union appearing on the right or an
       intersection appearing on the left, the simplification rules are
       imprecise: we split the union / intersection into cases and try to prove
       that the subtyping question holds for one of the cases, but each of those
       cases may be unprovable, which might lead to spurious errors. In
       particular, obvious assertions such as (A | B) & C is a subtype of A | B
       cannot be proved if we choose to split the union first (discharging
       unprovable subgoals of (A | B) & C being a subtype of either A or B);
       dually, obvious assertions such as A & B is a subtype of (A & B) | C
       cannot be proved if we choose to simplify the intersection first
       (discharging unprovable subgoals of either A or B being a subtype of (A &
       B) | C). So instead, we try inclusion rules to handle such cases.

       An orthogonal benefit is that for large unions or intersections, checking
       inclusion is significantly faster that splitting for proving simple
       inequalities (O(n) instead of O(n^2) for n cases).  *)
        | (IntersectionT (_, rep), UseT (_, u)) when List.mem u (InterRep.members rep) -> ()
        (* String enum sets can be handled in logarithmic time by just
         * checking for membership in the set.
         *)
        | (DefT (reason_l, _, StrT (Literal (_, x))), UseT (use_op, UnionT (reason_u, rep)))
          when match UnionRep.check_enum rep with
               | Some enums ->
                 if not (UnionEnumSet.mem (UnionEnum.Str x) enums) then
                   add_output
                     cx
                     ~trace
                     (Error_message.EIncompatibleWithUseOp
                        {
                          reason_lower = reason_l;
                          reason_upper =
                            UnionRep.specialized_reason
                              ~reason_of_t:TypeUtil.reason_of_t
                              reason_u
                              rep;
                          use_op;
                        });
                 true
               | _ -> false ->
          ()
        | (_, UseT (_, UnionT (_, rep)))
          when let ts = Type_mapper.union_flatten cx @@ UnionRep.members rep in
               List.exists (TypeUtil.quick_subtype (Context.trust_errors cx) l) ts ->
          ()
        | (_, UseT (use_op, UnionT (r, rep))) ->
          (* Try the branches of the union in turn, with the goal of selecting the correct branch. This
         process is reused for intersections as well. See comments on try_union and
         try_intersection. *)
          try_union cx trace use_op l r rep
        | (_, FilterOptionalT (use_op, u)) -> rec_flow_t cx trace ~use_op (l, u)
        | (_, FilterMaybeT (use_op, u)) -> rec_flow_t cx trace ~use_op (l, u)
        (* maybe and optional types are just special union types *)
        | (t1, UseT (use_op, MaybeT (r2, t2))) ->
          let t2 = push_type_alias_reason r2 t2 in
          rec_flow cx trace (t1, UseT (use_op, t2))
        | (t1, UseT (use_op, OptionalT { reason = _; type_ = t2; use_desc = _ })) ->
          rec_flow cx trace (t1, UseT (use_op, t2))
        (* special treatment for some operations on intersections: these
        rules fire for particular UBs whose constraints can (or must)
        be resolved against intersection LBs as a whole, instead of
        by decomposing the intersection into its parts.
      *)

        (* lookup of properties **)
        | ( IntersectionT (_, rep),
            LookupT { reason; lookup_kind; ts = try_ts_on_failure; propref; lookup_action; ids } )
          ->
          let ts = InterRep.members rep in
          assert (ts <> []);

          (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
          rec_flow
            cx
            trace
            ( List.hd ts,
              LookupT
                {
                  reason;
                  lookup_kind;
                  ts = List.tl ts @ try_ts_on_failure;
                  propref;
                  lookup_action;
                  ids;
                } )
        | (IntersectionT _, TestPropT (reason, _, prop, tout)) ->
          rec_flow cx trace (l, GetPropT (unknown_use, reason, prop, tout))
        | ( IntersectionT _,
            OptionalChainT (r1, r2, this, TestPropT (reason, _, prop, tout), void_out) ) ->
          rec_flow
            cx
            trace
            (l, OptionalChainT (r1, r2, this, GetPropT (unknown_use, reason, prop, tout), void_out))
        (* extends **)
        | (IntersectionT (_, rep), ExtendsUseT (use_op, reason, try_ts_on_failure, l, u)) ->
          let (t, ts) = InterRep.members_nel rep in
          let try_ts_on_failure = Nel.to_list ts @ try_ts_on_failure in
          (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
          rec_flow cx trace (t, ExtendsUseT (use_op, reason, try_ts_on_failure, l, u))
        (* consistent override of properties **)
        | (IntersectionT (_, rep), SuperT (use_op, reason, derived)) ->
          InterRep.members rep
          |> List.iter (fun t ->
                 let u =
                   match use_op with
                   | Op (ClassExtendsCheck c) ->
                     let use_op = Op (ClassExtendsCheck { c with extends = reason_of_t t }) in
                     SuperT (use_op, reason, derived)
                   | _ -> u
                 in
                 rec_flow cx trace (t, u))
        (* structural subtype multiple inheritance **)
        | (IntersectionT (_, rep), ImplementsT (use_op, this)) ->
          InterRep.members rep
          |> List.iter (fun t ->
                 let u =
                   match use_op with
                   | Op (ClassImplementsCheck c) ->
                     let use_op = Op (ClassImplementsCheck { c with implements = reason_of_t t }) in
                     ImplementsT (use_op, this)
                   | _ -> u
                 in
                 rec_flow cx trace (t, u))
        (* object types: an intersection may satisfy an object UB without
        any particular member of the intersection doing so completely.
        Here we trap object UBs with more than one property, and
        decompose them into singletons.
        Note: should be able to do this with LookupT rather than
        slices, but that approach behaves in nonobvious ways. TODO why?
      *)
        | (IntersectionT _, UseT (use_op, DefT (r, _, ObjT { flags; props_tmap; proto_t; call_t })))
          when SMap.cardinal (Context.find_props cx props_tmap) > 1 ->
          Context.iter_real_props cx props_tmap (fun x p ->
              let pmap = SMap.singleton x p in
              let id = Context.generate_property_map cx pmap in
              let obj = mk_objecttype ~flags ~call:call_t id dummy_prototype in
              rec_flow cx trace (l, UseT (use_op, DefT (r, bogus_trust (), ObjT obj))));
          rec_flow cx trace (l, UseT (use_op, proto_t))
        (* predicates: prevent a predicate upper bound from prematurely decomposing
        an intersection lower bound *)
        | (IntersectionT _, PredicateT (pred, tout)) -> predicate cx trace tout l pred
        (* same for guards *)
        | (IntersectionT _, GuardT (pred, result, tout)) -> guard cx trace l pred result tout
        (* ObjAssignFromT copies multiple properties from its incoming LB.
        Here we simulate a merged object type by iterating over the
        entire intersection. *)
        | (IntersectionT (_, rep), ObjAssignFromT (use_op, reason_op, proto, tout, kind)) ->
          let tvar =
            List.fold_left
              (fun tout t ->
                let tvar =
                  match Cache.Fix.find cx reason_op t with
                  | Some tvar -> tvar
                  | None ->
                    Tvar.mk_where cx reason_op (fun tvar ->
                        Cache.Fix.add cx reason_op t tvar;
                        rec_flow cx trace (t, ObjAssignFromT (use_op, reason_op, proto, tvar, kind)))
                in
                rec_flow_t cx ~use_op trace (tvar, tout);
                tvar)
              (Tvar.mk cx reason_op)
              (InterRep.members rep)
          in
          rec_flow_t cx ~use_op trace (tvar, tout)
        (* This duplicates the (_, ReposLowerT u) near the end of this pattern
        match but has to appear here to preempt the (IntersectionT, _) in
        between so that we reposition the entire intersection. *)
        | (IntersectionT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (IntersectionT _, ObjKitT (use_op, reason, resolve_tool, tool, tout)) ->
          ObjectKit.run cx trace ~use_op reason resolve_tool tool tout l
        | (IntersectionT _, SealGenericT { reason = _; id; name; cont }) ->
          let reason = reason_of_t l in
          continue cx trace (GenericT { reason; id; name; bound = l }) cont
        (* CallT uses that arise from the CallType type destructor are processed
       without preparation (see below). This is because in these cases, the
       return type is intended to be 0-1, whereas preparation (as implemented
       currently) destroys 0-1 behavior. *)
        | (IntersectionT (r, rep), CallT (_, reason, _)) when is_calltype_reason reason ->
          try_intersection cx trace u r rep
        (* All other pairs with an intersection lower bound come here. Before
        further processing, we ensure that the upper bound is concretized. See
        prep_try_intersection for details. **)

        (* (After the above preprocessing step, try the branches of the intersection
       in turn, with the goal of selecting the correct branch. This process is
       reused for unions as well. See comments on try_union and
       try_intersection.)  *)
        | (IntersectionT (r, rep), u) ->
          prep_try_intersection cx trace (reason_of_use_t u) (parts_to_replace cx u) [] u r rep
        (************)
        (* matching *)
        (************)
        | (MatchingPropT (reason, x, t), UseT (use_op, l)) ->
          (* Things that can have properties are object-like (objects, instances,
         and their exact versions). Notably, "meta" types like union, annot,
         typeapp, eval, maybe, optional, and intersection should have boiled
         away by this point. Generics should have been "unsealed" as well. *)
          let propref = Named (reason, x) in
          let strict = NonstrictReturning (None, None) in
          let u =
            LookupT
              {
                reason;
                lookup_kind = strict;
                ts = [];
                propref;
                lookup_action = MatchProp (use_op, t);
                ids = Some Properties.Set.empty;
              }
          in
          rec_flow cx trace (l, u)
        | (MatchingPropT _, _) when is_use u -> () (* TODO: empty? *)
        (*************************)
        (* Resolving rest params *)
        (*************************)

        (* `any` is obviously fine as a spread element. `Object` is fine because
         * any Iterable can be spread, and `Object` is the any type that covers
         * iterable objects. *)
        | ( AnyT (r, _),
            ResolveSpreadT (use_op, reason_op, { rrt_resolved; rrt_unresolved; rrt_resolve_to }) )
          ->
          let rrt_resolved = ResolvedAnySpreadArg r :: rrt_resolved in
          resolve_spread_list_rec
            cx
            ~trace
            ~use_op
            ~reason_op
            (rrt_resolved, rrt_unresolved)
            rrt_resolve_to
        | (_, ResolveSpreadT (use_op, reason_op, { rrt_resolved; rrt_unresolved; rrt_resolve_to }))
          ->
          let reason = reason_of_t l in
          let (lt, generic) =
            match l with
            | GenericT { bound; id; reason; _ } -> (position_generic_bound reason bound, Some id)
            | _ -> (l, None)
          in
          let arrtype =
            match lt with
            | DefT (_, _, ArrT arrtype) ->
              (* Arrays *)
              arrtype
            | _ ->
              (* Non-array non-any iterables, opaque arrays, etc *)
              let resolve_to =
                match rrt_resolve_to with
                (* Spreading iterables in a type context is always OK *)
                | ResolveSpreadsToMultiflowSubtypeFull _ -> `Iterable
                (* Function.prototype.apply takes array-likes, not iterables *)
                | ResolveSpreadsToCallT _ -> `ArrayLike
                (* Otherwise we're spreading values *)
                | ResolveSpreadsToArray _
                | ResolveSpreadsToArrayLiteral _
                | ResolveSpreadsToCustomFunCall _
                | ResolveSpreadsToMultiflowCallFull _
                | ResolveSpreadsToMultiflowPartial _ ->
                  (* Babel's "loose mode" array spread transform deviates from
                   * the spec by assuming the spread argument is always an
                   * array. If the babel_loose_array_spread option is set, model
                   * this assumption.
                   *)
                  if Context.babel_loose_array_spread cx then
                    `Array
                  else
                    `Iterable
              in
              let element_tvar = Tvar.mk cx reason in
              let resolve_to_type =
                match resolve_to with
                | `ArrayLike ->
                  get_builtin_typeapp
                    cx
                    (replace_desc_new_reason
                       (RCustom "Array-like object expected for apply")
                       reason)
                    "$ArrayLike"
                    [element_tvar]
                | `Iterable ->
                  let targs =
                    [
                      element_tvar;
                      Unsoundness.why ResolveSpread reason;
                      Unsoundness.why ResolveSpread reason;
                    ]
                  in
                  get_builtin_typeapp
                    cx
                    (replace_desc_new_reason (RCustom "Iterable expected for spread") reason)
                    "$Iterable"
                    targs
                | `Array ->
                  DefT
                    ( replace_desc_new_reason (RCustom "Array expected for spread") reason,
                      bogus_trust (),
                      ArrT (ROArrayAT element_tvar) )
              in
              rec_flow_t ~use_op:unknown_use cx trace (l, resolve_to_type);
              ArrayAT (element_tvar, None)
          in
          let elemt = elemt_of_arrtype arrtype in
          begin
            match rrt_resolve_to with
            (* Any ResolveSpreadsTo* which does some sort of constant folding needs to
             * carry an id around to break the infinite recursion that constant
             * constant folding can trigger *)
            | ResolveSpreadsToArrayLiteral (id, elem_t, tout) ->
              (* You might come across code like
               *
               * for (let x = 1; x < 3; x++) { foo = [...foo, x]; }
               *
               * where every time you spread foo, you flow another type into foo. So
               * each time `l ~> ResolveSpreadT` is processed, it might produce a new
               * `l ~> ResolveSpreadT` with a new `l`.
               *
               * Here is how we avoid this:
               *
               * 1. We use ConstFoldExpansion to detect when we see a ResolveSpreadT
               *    upper bound multiple times
               * 2. When a ResolveSpreadT upper bound multiple times, we change it into
               *    a ResolveSpreadT upper bound that resolves to a more general type.
               *    This should prevent more distinct lower bounds from flowing in
               * 3. rec_flow caches (l,u) pairs.
               *)
              let reason_elemt = reason_of_t elemt in
              let pos = Base.List.length rrt_resolved in
              ConstFoldExpansion.guard id (reason_elemt, pos) (fun recursion_depth ->
                  match recursion_depth with
                  | 0 ->
                    (* The first time we see this, we process it normally *)
                    let rrt_resolved =
                      ResolvedSpreadArg (reason, arrtype, generic) :: rrt_resolved
                    in
                    resolve_spread_list_rec
                      cx
                      ~trace
                      ~use_op
                      ~reason_op
                      (rrt_resolved, rrt_unresolved)
                      rrt_resolve_to
                  | 1 ->
                    (* To avoid infinite recursion, let's deconstruct to a simpler case
                     * where we no longer resolve to a tuple but instead just resolve to
                     * an array. *)
                    rec_flow
                      cx
                      trace
                      ( l,
                        ResolveSpreadT
                          ( use_op,
                            reason_op,
                            {
                              rrt_resolved;
                              rrt_unresolved;
                              rrt_resolve_to = ResolveSpreadsToArray (elem_t, tout);
                            } ) )
                  | _ ->
                    (* We've already deconstructed, so there's nothing left to do *)
                    ())
            | ResolveSpreadsToMultiflowCallFull (id, _)
            | ResolveSpreadsToMultiflowSubtypeFull (id, _)
            | ResolveSpreadsToCustomFunCall (id, _, _)
            | ResolveSpreadsToMultiflowPartial (id, _, _, _) ->
              let reason_elemt = reason_of_t elemt in
              let pos = Base.List.length rrt_resolved in
              ConstFoldExpansion.guard id (reason_elemt, pos) (fun recursion_depth ->
                  match recursion_depth with
                  | 0 ->
                    (* The first time we see this, we process it normally *)
                    let rrt_resolved =
                      ResolvedSpreadArg (reason, arrtype, generic) :: rrt_resolved
                    in
                    resolve_spread_list_rec
                      cx
                      ~trace
                      ~use_op
                      ~reason_op
                      (rrt_resolved, rrt_unresolved)
                      rrt_resolve_to
                  | 1 ->
                    (* Consider
                     *
                     * function foo(...args) { foo(1, ...args); }
                     * foo();
                     *
                     * Because args is unannotated, we try to infer it. However, due to
                     * the constant folding we do with spread arguments, we'll first
                     * infer that it is [], then [] | [1], then [] | [1] | [1,1] ...etc
                     *
                     * We can recognize that we're stuck in a constant folding loop. But
                     * how to break it?
                     *
                     * In this case, we are constant folding by recognizing when args is
                     * a tuple or an array literal. We can break the loop by turning
                     * tuples or array literals into simple arrays.
                     *)
                    let new_arrtype =
                      match arrtype with
                      (* These can get us into constant folding loops *)
                      | ArrayAT (elemt, Some _)
                      | TupleAT (elemt, _) ->
                        ArrayAT (elemt, None)
                      (* These cannot *)
                      | ArrayAT (_, None)
                      | ROArrayAT _ ->
                        arrtype
                    in
                    let rrt_resolved =
                      ResolvedSpreadArg (reason, new_arrtype, generic) :: rrt_resolved
                    in
                    resolve_spread_list_rec
                      cx
                      ~trace
                      ~use_op
                      ~reason_op
                      (rrt_resolved, rrt_unresolved)
                      rrt_resolve_to
                  | _ -> ())
            (* no caching *)
            | ResolveSpreadsToArray _
            | ResolveSpreadsToCallT _ ->
              let rrt_resolved = ResolvedSpreadArg (reason, arrtype, generic) :: rrt_resolved in
              resolve_spread_list_rec
                cx
                ~trace
                ~use_op
                ~reason_op
                (rrt_resolved, rrt_unresolved)
                rrt_resolve_to
          end
        (* singleton lower bounds are equivalent to the corresponding
       primitive with a literal constraint. These conversions are
       low precedence to allow equality exploits above, such as
       the UnionT membership check, to fire.
       TODO we can move to a single representation for singletons -
       either SingletonFooT or (FooT <literal foo>) - if we can
       ensure that their meaning as upper bounds is unambiguous.
       Currently a SingletonFooT means the constrained type,
       but the literal in (FooT <literal>) is a no-op.
       Abstractly it should be totally possible to scrub literals
       from the latter kind of flow, but it's unclear how difficult
       it would be in practice.
     *)
        | ( DefT (_, _, (SingletonStrT _ | SingletonNumT _ | SingletonBoolT _)),
            ReposLowerT (reason, use_desc, u) ) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (DefT (reason, trust, SingletonStrT key), _) ->
          rec_flow cx trace (DefT (reason, trust, StrT (Literal (None, key))), u)
        | (DefT (reason, trust, SingletonNumT lit), _) ->
          rec_flow cx trace (DefT (reason, trust, NumT (Literal (None, lit))), u)
        | (DefT (reason, trust, SingletonBoolT b), _) ->
          rec_flow cx trace (DefT (reason, trust, BoolT (Some b)), u)
        (* NullProtoT is necessary as an upper bound, to distinguish between
       (ObjT _, NullProtoT _) constraints and (ObjT _, DefT (_, _, NullT)), but as
       a lower bound, it's the same as DefT (_, _, NullT) *)
        | (NullProtoT reason, _) -> rec_flow cx trace (DefT (reason, bogus_trust (), NullT), u)
        (************************************************************************)
        (* exact object types *)
        (************************************************************************)

        (* ExactT<X> comes from annotation, may behave as LB or UB *)

        (* when $Exact<LB> ~> UB, forward to MakeExactT *)
        | (ExactT (r, t), _) ->
          let t = push_type_alias_reason r t in
          rec_flow cx trace (t, MakeExactT (r, Upper u))
        (* ObjT LB ~> $Exact<UB>. make exact if exact and unsealed *)
        | (DefT (_, _, ObjT { flags; _ }), UseT (use_op, ExactT (r, t))) ->
          if Obj_type.is_exact_or_sealed r flags.obj_kind then
            let t = push_type_alias_reason r t in
            rec_flow cx trace (t, MakeExactT (r, Lower (use_op, l)))
          else
            exact_obj_error cx trace flags.obj_kind ~use_op ~exact_reason:r (l, t)
        (* any ~> $Exact<UB>. unwrap exact *)
        | ((DefT (_, _, EmptyT _) | AnyT _), UseT (use_op, ExactT (_, t))) ->
          rec_flow cx trace (l, UseT (use_op, t))
        (* Shapes need to be trapped here to avoid error-ing when used as exact types.
           Below (see "matching shapes of objects"), we have a rule that allows ShapeT(o)
           to be used just as o is allowed to be used. *)
        | (ShapeT (_, o), (UseT (_, ExactT _) | MakeExactT _)) -> rec_flow cx trace (o, u)
        (* Classes/Functions are "inexact" *)

        (* anything else ~> $Exact<UB>. error *)
        | (_, UseT (use_op, ExactT (r, t))) ->
          rec_flow cx trace (t, MakeExactT (r, Lower (use_op, l)))
        (* LB ~> MakeExactT (_, UB) exactifies LB, then flows result to UB *)

        (* exactify incoming LB object type, flow to UB *)
        | (DefT (r, trust, ObjT obj), MakeExactT (_, Upper u)) ->
          let obj_kind =
            match obj.flags.obj_kind with
            | Inexact -> Exact
            | k -> k
          in
          let exactobj = { obj with flags = { obj.flags with obj_kind } } in
          rec_flow cx trace (DefT (r, trust, ObjT exactobj), u)
        (* exactify incoming UB object type, flow to LB *)
        | (DefT (ru, trust, ObjT obj_u), MakeExactT (reason_op, Lower (use_op, l))) ->
          (* forward to standard obj ~> obj *)
          let ru = repos_reason (aloc_of_reason reason_op) ru in
          let obj_kind =
            match obj_u.flags.obj_kind with
            | Inexact -> Exact
            | k -> k
          in
          let xu = { obj_u with flags = { obj_u.flags with obj_kind } } in
          rec_flow cx trace (l, UseT (use_op, DefT (ru, trust, ObjT xu)))
        | (AnyT (_, src), MakeExactT (reason_op, k)) -> continue cx trace (AnyT.why src reason_op) k
        | (DefT (_, trust, VoidT), MakeExactT (reason_op, k)) ->
          continue cx trace (VoidT.why reason_op trust) k
        | (DefT (_, trust, EmptyT _), MakeExactT (reason_op, k)) ->
          continue cx trace (EmptyT.why reason_op trust) k
        (* unsupported kind *)
        | (_, MakeExactT (reason_op, k)) ->
          add_output cx ~trace (Error_message.EUnsupportedExact (reason_op, reason_of_t l));
          continue cx trace (AnyT.error reason_op) k
        (*******************************************)
        (* Refinement based on function predicates *)
        (*******************************************)

        (* Call to predicated (latent) functions *)

        (* Calls to functions appearing in predicate refinement contexts dispatch
       to this case. Here, the return type of the function holds the predicate
       that will refine the incoming `unrefined_t` and flow a filtered
       (refined) version of this type into `fresh_t`.

       What is important to note here is that `return_t` has no access to the
       function's parameter names. It will simply be an `OpenPredT` containing
       mappings from symbols (Key.t) that are (hopefully) the function's
       parameters to predicates. In other words, it is an "open" predicate over
       (free) variables, which *should* be the function's parameters.

       The `CallLatentPredT` use contains the index of the argument under
       refinement. By combining this information with the names of the
       parameters, we can arrive to the actual name (Key.t) of the parameter
       that gets refined, which can be used as a key into the `OpenPredT` that
       is expected to eventually flow to `return_t`.  Effectively, we are
       substituting the actual parameter to the refining call (here in the form
       of the index of the argument to the call) to the formal parameter of the
       function, and this information is stored in `CallOpenPredT` of the
       produced flow.

       Problematic cases (e.g. when the refining index is out of bounds w.r.t.
       `params`) raise errors, but also propagate the unrefined types (as if the
       refinement never took place).
    *)
        | ( DefT (lreason, _, FunT (_, _, { params; return_t; is_predicate = true; _ })),
            CallLatentPredT (reason, sense, index, unrefined_t, fresh_t) ) ->
          (* TODO: for the moment we only support simple keys (empty projection)
         that exactly correspond to the function's parameters *)
          let name_or_err =
            try
              let (name, _) = List.nth params (index - 1) in
              Ok name
            with
            | Invalid_argument _ -> Error ("Negative refinement index.", (lreason, reason))
            | Failure msg when msg = "nth" ->
              let r1 =
                update_desc_new_reason
                  (fun desc ->
                    RCustom
                      (spf
                         "%s that uses predicate on parameter at position %d"
                         (string_of_desc desc)
                         index))
                  reason
              in
              let r2 =
                update_desc_new_reason
                  (fun desc ->
                    RCustom (spf "%s with %d parameters" (string_of_desc desc) (List.length params)))
                  lreason
              in
              Error ("This is incompatible with", (r1, r2))
          in
          (match name_or_err with
          | Ok (Some name) ->
            let key = (name, []) in
            rec_flow cx trace (return_t, CallOpenPredT (reason, sense, key, unrefined_t, fresh_t))
          | Ok None ->
            let loc = aloc_of_reason lreason in
            add_output cx ~trace Error_message.(EInternal (loc, PredFunWithoutParamNames))
          | Error (msg, reasons) ->
            add_output cx ~trace (Error_message.EFunPredCustom (reasons, msg));
            rec_flow_t ~use_op:unknown_use cx trace (unrefined_t, OpenT fresh_t))
        (* Fall through all the remaining cases *)
        | (_, CallLatentPredT (_, _, _, unrefined_t, fresh_t)) ->
          rec_flow_t ~use_op:unknown_use cx trace (unrefined_t, OpenT fresh_t)
        (* Trap the return type of a predicated function *)
        | ( OpenPredT { m_pos = p_pos; m_neg = p_neg; reason = _; base_t = _ },
            CallOpenPredT (_, sense, key, unrefined_t, fresh_t) ) ->
          let preds =
            if sense then
              p_pos
            else
              p_neg
          in
          (match Key_map.find_opt key preds with
          | Some p -> rec_flow cx trace (unrefined_t, PredicateT (p, fresh_t))
          | _ -> rec_flow_t ~use_op:unknown_use cx trace (unrefined_t, OpenT fresh_t))
        (* Any other flow to `CallOpenPredT` does not actually refine the
       type in question so we just fall back to regular flow. *)
        | (_, CallOpenPredT (_, _, _, unrefined_t, fresh_t)) ->
          rec_flow_t ~use_op:unknown_use cx trace (unrefined_t, OpenT fresh_t)
        (********************************)
        (* Function-predicate subtyping *)
        (********************************)

        (* When decomposing function subtyping for predicated functions we need to
         * pair-up the predicates that each of the two functions established
         * before we can check for predicate implication. The predicates encoded
         * inside the two `OpenPredT`s refer to the formal parameters of the two
         * functions (which are not the same). `SubstOnPredT` is a use that does
         * this matching by carrying a substitution (`subst`) from keys from the
         * function in the left-hand side to keys in the right-hand side.
         *)
        | ( OpenPredT { base_t = t1; m_pos = _p_pos_1; m_neg = _p_neg_1; reason = _ },
            SubstOnPredT
              (use_op, _, _, OpenPredT { base_t = t2; m_pos = p_pos_2; m_neg = p_neg_2; reason = _ })
          )
          when Key_map.(is_empty p_pos_2 && is_empty p_neg_2) ->
          rec_flow_t ~use_op cx trace (t1, t2)
        (* Identical predicates are okay, just do the base type check. *)
        | ( OpenPredT { base_t = t1; m_pos = p_pos_1; m_neg = _; reason = lreason },
            UseT (use_op, OpenPredT { base_t = t2; m_pos = p_pos_2; m_neg = _; reason = ureason })
          ) ->
          if TypeUtil.pred_map_implies p_pos_1 p_pos_2 then
            rec_flow_t ~use_op cx trace (t1, t2)
          else
            let error =
              Error_message.EIncompatibleWithUseOp
                { reason_lower = lreason; reason_upper = ureason; use_op }
            in
            add_output cx ~trace error
        (*********************************************)
        (* Using predicate functions as regular ones *)
        (*********************************************)
        | (OpenPredT { base_t = l; m_pos = _; m_neg = _; reason = _ }, _) -> rec_flow cx trace (l, u)
        (********************)
        (* mixin conversion *)
        (********************)

        (* A class can be viewed as a mixin by extracting its immediate properties,
       and "erasing" its static and super *)
        | (ThisClassT (_, DefT (_, trust, InstanceT (_, _, _, instance)), is_this), MixinT (r, tvar))
          ->
          let static = ObjProtoT r in
          let super = ObjProtoT r in
          rec_flow
            cx
            trace
            ( this_class_type (DefT (r, trust, InstanceT (static, super, [], instance))) is_this,
              UseT (unknown_use, tvar) )
        | ( DefT
              ( _,
                _,
                PolyT
                  {
                    tparams_loc;
                    tparams = xs;
                    t_out = ThisClassT (_, DefT (_, trust, InstanceT (_, _, _, insttype)), is_this);
                    _;
                  } ),
            MixinT (r, tvar) ) ->
          let static = ObjProtoT r in
          let super = ObjProtoT r in
          let instance = DefT (r, trust, InstanceT (static, super, [], insttype)) in
          rec_flow
            cx
            trace
            ( poly_type
                (Context.generate_poly_id cx)
                tparams_loc
                xs
                (this_class_type instance is_this),
              UseT (unknown_use, tvar) )
        | (AnyT (_, src), MixinT (r, tvar)) ->
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src r, tvar)
        (* TODO: it is conceivable that other things (e.g. functions) could also be
       viewed as mixins (e.g. by extracting properties in their prototypes), but
       such enhancements are left as future work. *)

        (***************************************)
        (* generic function may be specialized *)
        (***************************************)

        (* Instantiate a polymorphic definition using the supplied type
       arguments. Use the instantiation cache if directed to do so by the
       operation. (SpecializeT operations are created when processing TypeAppT
       types, so the decision to cache or not originates there.) *)
        | ( DefT (_, _, PolyT { tparams_loc; tparams = xs; t_out = t; id }),
            SpecializeT (use_op, reason_op, reason_tapp, cache, ts, tvar) ) ->
          let ts = Base.Option.value ts ~default:[] in
          let t_ =
            mk_typeapp_of_poly
              cx
              trace
              ~use_op
              ~reason_op
              ~reason_tapp
              ?cache
              id
              tparams_loc
              xs
              t
              ts
          in
          rec_flow_t ~use_op:unknown_use cx trace (t_, tvar)
        | (DefT (_, _, PolyT { tparams = tps; _ }), VarianceCheckT (_, tparams, targs, polarity)) ->
          variance_check cx ~trace tparams polarity (Nel.to_list tps, targs)
        | (ThisClassT _, VarianceCheckT (_, _, [], _)) ->
          (* We will emit this constraint when walking an extends clause which does
           * not have explicit type arguments. The class has an implicit this type
           * parameter which needs to be specialized to the inheriting class, but
           * that is uninteresting for the variance check machinery. *)
          ()
        | ( DefT (_, _, PolyT { tparams_loc; tparams; _ }),
            TypeAppVarianceCheckT (use_op, reason_op, reason_tapp, targs) ) ->
          let minimum_arity = poly_minimum_arity tparams in
          let maximum_arity = Nel.length tparams in
          let reason_arity =
            mk_reason (RCustom "See type parameters of definition here") tparams_loc
          in
          if List.length targs > maximum_arity then
            add_output
              cx
              ~trace
              (Error_message.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity))
          else
            let (unused_targs, _, _) =
              Nel.fold_left
                (fun (targs, map1, map2) tparam ->
                  let { name; default; polarity; reason; _ } = tparam in
                  let flow_targs t1 t2 =
                    let use_op =
                      Frame
                        ( TypeArgCompatibility
                            {
                              name;
                              targ = reason;
                              lower = reason_op;
                              upper = reason_tapp;
                              polarity;
                            },
                          use_op )
                    in
                    match polarity with
                    | Polarity.Positive -> rec_flow cx trace (t1, UseT (use_op, t2))
                    | Polarity.Negative -> rec_flow cx trace (t2, UseT (use_op, t1))
                    | Polarity.Neutral -> rec_unify cx trace ~use_op t1 t2
                  in
                  match (default, targs) with
                  | (None, []) ->
                    (* fewer arguments than params but no default *)
                    add_output
                      cx
                      ~trace
                      (Error_message.ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity));
                    ([], map1, map2)
                  | (Some default, []) ->
                    let t1 = subst cx ~use_op map1 default in
                    let t2 = subst cx ~use_op map2 default in
                    flow_targs t1 t2;
                    ([], SMap.add name t1 map1, SMap.add name t2 map2)
                  | (_, (t1, t2) :: targs) ->
                    flow_targs t1 t2;
                    (targs, SMap.add name t1 map1, SMap.add name t2 map2))
                (targs, SMap.empty, SMap.empty)
                tparams
            in
            assert (unused_targs = [])
        (* empty targs specialization of non-polymorphic classes is a no-op *)
        | ((DefT (_, _, ClassT _) | ThisClassT _), SpecializeT (_, _, _, _, None, tvar)) ->
          rec_flow_t ~use_op:unknown_use cx trace (l, tvar)
        | (AnyT _, SpecializeT (_, _, _, _, _, tvar)) ->
          rec_flow_t ~use_op:unknown_use cx trace (l, tvar)
        (* this-specialize a this-abstracted class by substituting This *)
        | (ThisClassT (_, i, _), ThisSpecializeT (r, this, k)) ->
          let i = subst cx (SMap.singleton "this" this) i in
          continue_repos cx trace r i k
        (* this-specialization of non-this-abstracted classes is a no-op *)
        | (DefT (_, _, ClassT i), ThisSpecializeT (r, _this, k)) ->
          (* TODO: check that this is a subtype of i? *)
          continue_repos cx trace r i k
        | (AnyT _, ThisSpecializeT (r, _, k)) -> continue_repos cx trace r l k
        | (DefT (_, _, PolyT _), ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        | (ThisClassT _, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        (* When do we consider a polymorphic type <X:U> T to be a subtype of another
       polymorphic type <X:U'> T'? This is the subject of a long line of
       research. A rule that works (Cardelli/Wegner) is: force U = U', and prove
       that T is a subtype of T' for any X:U'. A more general rule that proves
       that U' is a subtype of U instead of forcing U = U' is known to cause
       undecidable subtyping (Pierce): the counterexamples are fairly
       pathological, but can be reliably constructed by exploiting the "switch"
       of bounds from U' to U (and back, with sufficient trickery), in ways that
       are difficult to detect statically.

       However, these results are somewhat tricky to interpret in Flow, since we
       are not proving stuff inductively: instead we are co-inductively assuming
       what we want to prove, and checking consistency.

       Separately, none of these rules capture the logical interpretation of the
       original subtyping question (interpreting subtyping as implication, and
       polymorphism as universal quantification). What we really want to show is
       that, for all X:U', there is some X:U such that T is a subtype of T'. But
       we already deal with statements of this form when checking polymorphic
       definitions! In particular, statements such as "there is some X:U...")
       correspond to "create a type variable with that constraint and ...", and
       statements such as "show that for all X:U" correspond to "show that for
       both X = bottom and X = U, ...".

       Thus, all we need to do when checking that any type flows to a
       polymorphic type is to follow the same principles used when checking that
       a polymorphic definition has a polymorphic type. This has the pleasant
       side effect that the type we're checking does not itself need to be a
       polymorphic type at all! For example, we can let a non-generic method be
       overridden with a generic method, as long as the non-generic signature
       can be derived as a specialization of the generic signature. *)

        (* some shortcuts **)
        | (DefT (_, _, PolyT { id = id1; _ }), UseT (_, DefT (_, _, PolyT { id = id2; _ })))
          when Poly.equal_id id1 id2 ->
          if Context.is_verbose cx then prerr_endline "PolyT ~> PolyT fast path"
        | ( DefT
              (r1, _, PolyT { tparams_loc = tparams_loc1; tparams = params1; t_out = t1; id = id1 }),
            UseT
              ( use_op,
                DefT
                  ( r2,
                    _,
                    PolyT { tparams_loc = tparams_loc2; tparams = params2; t_out = t2; id = id2 } )
              ) ) ->
          let n1 = Nel.length params1 in
          let n2 = Nel.length params2 in
          if n2 > n1 then
            add_output cx ~trace (Error_message.ETooManyTypeArgs (r2, r1, n1))
          else if n2 < n1 then
            add_output cx ~trace (Error_message.ETooFewTypeArgs (r2, r1, n1))
          else
            (* for equal-arity polymorphic types, flow param upper bounds, then instances parameterized
            by these *)
            let args1 = instantiate_poly_param_upper_bounds cx params1 in
            let args2 = instantiate_poly_param_upper_bounds cx params2 in
            List.iter2 (fun arg1 arg2 -> rec_flow_t cx trace ~use_op (arg2, arg1)) args1 args2;
            let inst1 =
              let r = reason_of_t t1 in
              mk_typeapp_of_poly
                cx
                trace
                ~use_op
                ~reason_op:r
                ~reason_tapp:r
                id1
                tparams_loc1
                params1
                t1
                args1
            in
            let inst2 =
              let r = reason_of_t t2 in
              mk_typeapp_of_poly
                cx
                trace
                ~use_op
                ~reason_op:r
                ~reason_tapp:r
                id2
                tparams_loc2
                params2
                t2
                args2
            in
            rec_flow_t ~use_op cx trace (inst1, inst2)
        (* general case **)
        | (_, UseT (use_op, DefT (_, _, PolyT { tparams = ids; t_out = t; _ }))) ->
          check_with_generics cx (Nel.to_list ids) (fun map_ ->
              rec_flow cx trace (l, UseT (use_op, subst cx ~use_op map_ t)))
        (* TODO: ideally we'd do the same when lower bounds flow to a
       this-abstracted class, but fixing the class is easier; might need to
       revisit *)
        | (_, UseT (use_op, ThisClassT (r, i, this))) ->
          let reason = reason_of_t l in
          rec_flow cx trace (l, UseT (use_op, fix_this_class cx trace reason (r, i, this)))
        (* This rule is hit when a polymorphic type appears outside a
        type application expression - i.e. not followed by a type argument list
        delimited by angle brackets.
        We want to require full expressions in type positions like annotations,
        but allow use of polymorphically-typed values - for example, in class
        extends clauses and at function call sites - without explicit type
        arguments, since typically they're easily inferred from context.
      *)
        | (DefT (reason_tapp, _, PolyT { tparams_loc; tparams = ids; t_out = t; _ }), _) ->
          let reason_op = reason_of_use_t u in
          begin
            match u with
            | UseT (use_op, DefT (_, _, TypeT _)) ->
              ignore use_op;

              (* TODO: add use op to missing type arg error? *)
              add_output
                cx
                ~trace
                (Error_message.EMissingTypeArgs
                   {
                     reason_tapp;
                     reason_arity = mk_poly_arity_reason tparams_loc;
                     min_arity = poly_minimum_arity ids;
                     max_arity = Nel.length ids;
                   })
            (* Special case for `_ instanceof C` where C is polymorphic *)
            | PredicateT ((RightP (InstanceofTest, _) | NotP (RightP (InstanceofTest, _))), _) ->
              let l =
                instantiate_poly_default_args
                  cx
                  trace
                  ~use_op:unknown_use
                  ~reason_op
                  ~reason_tapp
                  (tparams_loc, ids, t)
              in
              rec_flow cx trace (l, u)
            (* Special case for React.PropTypes.instanceOf arguments, which are an
         exception to type arg arity strictness, because it's not possible to
         provide args and we need to interpret the value as a type. *)
            | ReactKitT
                ( use_op,
                  reason_op,
                  (React.SimplifyPropType (React.SimplifyPropType.InstanceOf, _) as tool) ) ->
              let l =
                instantiate_poly_default_args
                  cx
                  trace
                  ~use_op
                  ~reason_op
                  ~reason_tapp
                  (tparams_loc, ids, t)
              in
              ReactJs.run cx trace ~use_op reason_op l tool
            (* Calls to polymorphic functions may cause non-termination, e.g. when the
         results of the calls feed back as subtle variations of the original
         arguments. This is similar to how we may have non-termination with
         method calls on type applications. Thus, it makes sense to replicate
         the specialization caching mechanism used in TypeAppT ~> MethodT to
         avoid non-termination in PolyT ~> CallT.

         As it turns out, we need a bit more work here. A call may invoke
         different cases of an overloaded polymorphic function on different
         arguments, so we use the reasons of arguments in addition to the reason
         of the call as keys for caching instantiations.

         On the other hand, even the reasons of arguments may not offer sufficient
         distinguishing power when the arguments have not been concretized:
         differently typed arguments could be incorrectly summarized by common
         type variables they flow to, causing spurious errors. In particular, we
         don't cache calls involved in the execution of mapped type operations
         ($TupleMap, $ObjectMap, $ObjectMapi) to avoid this problem.

         NOTE: This is probably not the final word on non-termination with
         generics. We need to separate the double duty of reasons in the current
         implementation as error positions and as caching keys. As error
         positions we should be able to subject reasons to arbitrary tweaking,
         without fearing regressions in termination guarantees.
      *)
            | CallT (use_op, _, calltype) when not (is_typemap_reason reason_op) ->
              begin
                match calltype.call_targs with
                | None ->
                  let arg_reasons =
                    Base.List.map
                      ~f:(function
                        | Arg t -> reason_of_t t
                        | SpreadArg t -> reason_of_t t)
                      calltype.call_args_tlist
                  in
                  let t_ =
                    instantiate_poly
                      cx
                      trace
                      ~use_op
                      ~reason_op
                      ~reason_tapp
                      ~cache:arg_reasons
                      (tparams_loc, ids, t)
                  in
                  rec_flow cx trace (t_, u)
                | Some targs ->
                  let t_ =
                    instantiate_poly_call_or_new
                      cx
                      trace
                      (tparams_loc, ids, t)
                      targs
                      ~use_op
                      ~reason_op
                      ~reason_tapp
                  in
                  rec_flow
                    cx
                    trace
                    (t_, CallT (use_op, reason_op, { calltype with call_targs = None }))
              end
            | ConstructorT (use_op, reason_op, Some targs, args, tout) ->
              let t_ =
                instantiate_poly_call_or_new
                  cx
                  trace
                  (tparams_loc, ids, t)
                  targs
                  ~use_op
                  ~reason_op
                  ~reason_tapp
              in
              rec_flow cx trace (t_, ConstructorT (use_op, reason_op, None, args, tout))
            | _ ->
              let use_op =
                match use_op_of_use_t u with
                | Some use_op -> use_op
                | None -> unknown_use
              in
              let t_ =
                instantiate_poly cx trace ~use_op ~reason_op ~reason_tapp (tparams_loc, ids, t)
              in
              rec_flow cx trace (t_, u)
          end
        (* when a this-abstracted class flows to upper bounds, fix the class *)
        | (ThisClassT (r, i, this), _) ->
          let reason = reason_of_use_t u in
          rec_flow cx trace (fix_this_class cx trace reason (r, i, this), u)
        (*****************************)
        (* React Abstract Components *)
        (*****************************)
        (*
         * In all of these cases, we check:
         *  1. configu <: configl
         *  2. default_propsl = default_propsu
         *  3. instancel <: instanceu
         *
         *  2. is necessary because we allow the default props of a component to be read and
         *  written.
         *
         *  1. Is necessary because we need to ensure that any config object that is passed to u
         *  is compatible with the config of l. This also is sufficient; unification is not required.
         *  We can think of AbstractComponents as some sort of callable that accepts a config object.
         *  The only place that the config object type would appear is in the callable signature, which
         *  is contravariant.
         *
         *  In reality, a component is turned into an element via createElement, which accepts a
         *  component and a config object. From there, it creates an object that will become the
         *  props of a component by combining the config object with the component's default props.
         *  This process creates a new fresh unaliased props object, which is passed to the component.
         *
         *  3. Is necessary because we need to ensure the ref passed in is compatible with the instance
         *  type of the component. React will assign ref.current to the instance of the component, so we
         *  need to ensure that the type we assign is compatible with the type ref.current.
         *)

        (* Class component ~> AbstractComponent *)
        | ( DefT (reasonl, _, ClassT this),
            UseT (use_op, DefT (_reasonu, _, ReactAbstractComponentT { config; instance })) ) ->
          (* Contravariant config check *)
          React_kit.get_config
            cx
            trace
            l
            ~use_op
            ~reason_op:reasonl
            ~rec_flow
            ~rec_flow_t
            ~rec_unify
            ~get_builtin_type
            ~add_output
            (React.GetConfig l)
            Polarity.Negative
            config;

          (* check instancel <: instanceu *)
          rec_flow_t cx trace ~use_op (this, instance)
        (* Function Component ~> AbstractComponent *)
        | ( DefT (reasonl, _, FunT (_, _, { return_t; _ })),
            UseT (use_op, DefT (_reasonu, _, ReactAbstractComponentT { config; instance })) ) ->
          (* Function components will not always have an annotation, so the config may
           * never resolve. To determine config compatibility, we instead
           * call createElement on the function with the given component to determine
           * the compatibility.
           *
           * We use ConfigCheck instead of CreateElement because:
           *  1. We can't perform the key check. If config is mixed, which can happen in
           *  polymorphic HOCs then the [string]: mixed indexer causes spurious errors.
           *  2. We check the ref here, so we don't need to check it in the config as well.
           *)
          rec_flow cx trace (l, ReactKitT (use_op, reasonl, React.ConfigCheck config));

          (* Ensure this is a function component *)
          rec_flow_t ~use_op cx trace (return_t, get_builtin_type cx reasonl "React$Node");

          (* A function component instance type is always void, so flow void to instance *)
          rec_flow_t
            cx
            trace
            ~use_op
            (VoidT.make (replace_desc_new_reason RVoid reasonl) |> with_trust bogus_trust, instance)
        (* Object Component ~> AbstractComponent *)
        | ( DefT (reasonl, _, ObjT { call_t = Some id; _ }),
            UseT (use_op, DefT (reasonu, trust, ReactAbstractComponentT { config; instance })) ) ->
          rec_flow cx trace (l, ReactKitT (use_op, reasonl, React.ConfigCheck config));

          (* Ensure the callable signature's return type is compatible with React.Node. We
           * do this by flowing it to (...empty): React.Node *)
          let funtype =
            mk_functiontype
              reasonu
              []
              ~rest_param:
                (Some
                   ( None,
                     aloc_of_reason reasonu,
                     EmptyT.why (replace_desc_new_reason REmpty reasonu) (bogus_trust ()) ))
              ~def_reason:reasonl
              (get_builtin_type cx reasonu "React$Node")
          in
          let mixed = MixedT.why reasonu (bogus_trust ()) in
          rec_flow_t
            ~use_op
            cx
            trace
            (Context.find_call cx id, DefT (reasonu, trust, FunT (mixed, mixed, funtype)));

          (* An object component instance type is always void, so flow void to instance *)
          rec_flow_t
            cx
            trace
            ~use_op
            (VoidT.make (replace_desc_new_reason RVoid reasonl) |> with_trust bogus_trust, instance)
        (* AbstractComponent ~> AbstractComponent *)
        | ( DefT (_reasonl, _, ReactAbstractComponentT { config = configl; instance = instancel }),
            UseT
              ( use_op,
                DefT
                  (_reasonu, _, ReactAbstractComponentT { config = configu; instance = instanceu })
              ) ) ->
          rec_flow_t cx trace ~use_op (configu, configl);
          rec_flow_t cx trace ~use_op (instancel, instanceu)
        (* When looking at properties of an AbstractComponent, we delegate to a union of
         * function component and class component
         *)
        | ( DefT (r, _, ReactAbstractComponentT _),
            (TestPropT _ | GetPropT _ | SetPropT _ | GetElemT _ | SetElemT _) ) ->
          let statics = get_builtin_type cx ~trace r "React$AbstractComponentStatics" in
          rec_flow cx trace (statics, u)
        (******************)
        (* React GetProps *)
        (******************)

        (* props is invariant in the class *)
        | (DefT (r, _, ClassT _), (ReactPropsToOut (_, props) | ReactInToProps (_, props))) ->
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (l, React_kit.component_class cx r ~get_builtin_typeapp props)
        (* Functions with rest params or that are predicates cannot be React components *)
        | ( DefT (reason, _, FunT (_, _, { params; rest_param = None; is_predicate = false; _ })),
            ReactPropsToOut (_, props) ) ->
          (* Contravariance *)
          Base.List.hd params
          |> Base.Option.value_map ~f:snd ~default:(Obj_type.mk ~obj_kind:Exact cx reason)
          |> fun t -> rec_flow_t ~use_op:unknown_use cx trace (t, props)
        | ( DefT
              ( reason,
                _,
                FunT (_, _, { params; return_t; rest_param = None; is_predicate = false; _ }) ),
            ReactInToProps (reason_op, props) ) ->
          (* Contravariance *)
          Base.List.hd params
          |> Base.Option.value_map ~f:snd ~default:(Obj_type.mk ~obj_kind:Exact cx reason)
          |> fun t ->
          rec_flow_t ~use_op:unknown_use cx trace (props, t);
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (return_t, get_builtin_type cx reason_op "React$Node")
        | (DefT (r, _, FunT _), (ReactInToProps (_, props) | ReactPropsToOut (_, props))) ->
          React.GetProps props
          |> React_kit.err_incompatible cx trace ~use_op:unknown_use ~add_output r
        | ( DefT (r, _, ObjT { call_t = Some id; _ }),
            (ReactInToProps (_, props) | ReactPropsToOut (_, props)) ) ->
          begin
            match Context.find_call cx id with
            | ( DefT (_, _, FunT (_, _, { rest_param = None; is_predicate = false; _ }))
              | DefT (_, _, PolyT { t_out = DefT (_, _, FunT _); _ }) ) as fun_t ->
              (* Keep the object's reason for better error reporting *)
              rec_flow cx trace (Fn.const r |> Fn.flip mod_reason_of_t fun_t, u)
            | _ ->
              React.GetProps props
              |> React_kit.err_incompatible cx trace ~use_op:unknown_use ~add_output r
          end
        | (AnyT _, ReactPropsToOut (_, props)) -> rec_flow_t ~use_op:unknown_use cx trace (l, props)
        | (AnyT _, ReactInToProps (_, props)) -> rec_flow_t ~use_op:unknown_use cx trace (props, l)
        | (DefT (r, _, _), (ReactPropsToOut (_, props) | ReactInToProps (_, props))) ->
          React.GetProps props
          |> React_kit.err_incompatible cx trace ~use_op:unknown_use ~add_output r
        (***********************************************)
        (* function types deconstruct into their parts *)
        (***********************************************)

        (* FunT ~> FunT *)
        | (DefT (lreason, _, FunT (_, _, ft1)), UseT (use_op, DefT (ureason, _, FunT (_, _, ft2))))
          ->
          let use_op =
            Frame
              ( FunCompatibility { lower = lreason; upper = ureason },
                (* The $call PropertyCompatibility is redundant when we have a
                 * FunCompatibility use_op. *)
                match use_op with
                | Frame (PropertyCompatibility { prop = Some "$call"; _ }, use_op) -> use_op
                | _ -> use_op )
          in
          rec_flow cx trace (ft2.this_t, UseT (use_op, ft1.this_t));
          let args = List.rev_map (fun (_, t) -> Arg t) ft2.params in
          let args =
            List.rev
              (match ft2.rest_param with
              | Some (_, _, rest) -> SpreadArg rest :: args
              | None -> args)
          in
          multiflow_subtype cx trace ~use_op ureason args ft1;

          (* Well-formedness adjustment: If this is predicate function subtyping,
             make sure to apply a latent substitution on the right-hand use to
             bridge the mismatch of the parameter naming. Otherwise, proceed with
             the subtyping of the return types normally. In general it should
             hold as an invariant that OpenPredTs (where free variables appear)
             should not flow to other OpenPredTs without wrapping the latter in
             SubstOnPredT.
           *)
          if ft2.is_predicate then
            if not ft1.is_predicate then
              (* Non-predicate functions are incompatible with predicate ones
                 TODO: somehow the original flow needs to be propagated as well *)
              add_output
                cx
                ~trace
                (Error_message.EFunPredCustom ((lreason, ureason), "Function is incompatible with"))
            else
              flow_predicate_func cx trace use_op (lreason, ft1) (ureason, ft2)
          else
            let use_op =
              Frame
                ( FunReturn { lower = reason_of_t ft1.return_t; upper = reason_of_t ft2.return_t },
                  use_op )
            in
            rec_flow cx trace (ft1.return_t, UseT (use_op, ft2.return_t))
        (* FunT ~> CallT *)
        | (DefT (reason_fundef, _, FunT (_, _, funtype)), CallT (use_op, reason_callsite, calltype))
          ->
          let { this_t = o1; params = _; return_t = t1; closure_t = func_scope_id; changeset; _ } =
            funtype
          in
          let {
            call_this_t = o2;
            call_targs;
            call_args_tlist = tins2;
            call_tout = t2;
            call_closure_t = call_scope_id;
            call_strict_arity;
          } =
            calltype
          in
          rec_flow cx trace (o2, UseT (use_op, o1));

          Base.Option.iter call_targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = aloc_of_reason reason_callsite;
                      is_new = false;
                      reason_arity = reason_fundef;
                      expected_arity = 0;
                    }));

          if call_strict_arity then
            multiflow_call cx trace ~use_op reason_callsite tins2 funtype
          else
            multiflow_subtype cx trace ~use_op reason_callsite tins2 funtype;

          (* flow return type of function to the tvar holding the return type of the
         call. clears the op stack because the result of the call is not the
         call itself. *)
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (reposition cx ~trace (aloc_of_reason reason_callsite) t1, OpenT t2);

          if Context.is_verbose cx then
            prerr_endlinef
              "%shavoc_call_env fundef %s callsite %s"
              (Context.pid_prefix cx)
              (Debug_js.string_of_reason cx reason_fundef)
              (Debug_js.string_of_reason cx reason_callsite);
          havoc_call_env cx func_scope_id call_scope_id changeset
        | (AnyT (reason_fundef, _), CallT (use_op, reason_op, calltype)) ->
          let {
            call_this_t;
            call_targs = _;
            (* An untyped receiver can't do anything with type args *)
            call_args_tlist;
            call_tout;
            call_closure_t = _;
            call_strict_arity = _;
          } =
            calltype
          in
          let any = AnyT.untyped reason_fundef in
          rec_flow_t cx ~use_op trace (call_this_t, any);
          call_args_iter (fun t -> rec_flow cx trace (t, UseT (use_op, any))) call_args_tlist;
          rec_flow_t cx ~use_op trace (AnyT.untyped reason_op, OpenT call_tout)
        | (_, FunImplicitVoidReturnT { use_op; return; void_t; _ }) ->
          rec_flow cx trace (void_t, UseT (use_op, return))
        (* Special handlers for builtin functions *)
        | ( CustomFunT (_, ObjectAssign),
            CallT
              ( use_op,
                reason_op,
                { call_targs = None; call_args_tlist = dest_t :: ts; call_tout; _ } ) ) ->
          let dest_t = extract_non_spread cx ~trace dest_t in
          let t = chain_objects cx ~trace reason_op dest_t ts in
          rec_flow_t cx ~use_op trace (t, OpenT call_tout)
        | ( CustomFunT (_, ObjectGetPrototypeOf),
            CallT (_, reason_op, { call_targs = None; call_args_tlist = arg :: _; call_tout; _ }) )
          ->
          let l = extract_non_spread cx ~trace arg in
          rec_flow cx trace (l, GetProtoT (reason_op, call_tout))
        | ( CustomFunT (_, ObjectSetPrototypeOf),
            CallT
              ( use_op,
                reason_op,
                { call_targs = None; call_args_tlist = arg1 :: arg2 :: _; call_tout; _ } ) ) ->
          let target = extract_non_spread cx ~trace arg1 in
          let proto = extract_non_spread cx ~trace arg2 in
          rec_flow cx trace (target, SetProtoT (reason_op, proto));
          rec_flow_t
            cx
            ~use_op
            trace
            (BoolT.why reason_op |> with_trust bogus_trust, OpenT call_tout)
        | ( DefT (reason, _, StrT (Literal (_, str))),
            UseT (use_op, DefT (reason_op, _, CharSetT chars)) ) ->
          let module CharSet = String_utils.CharSet in
          Error_message.(
            let (invalid, _) =
              String_utils.fold_left
                ~f:(fun (invalid, seen) chr ->
                  if not (CharSet.mem chr chars) then
                    (InvalidCharSetSet.add (InvalidChar chr) invalid, seen)
                  else if CharSet.mem chr seen then
                    (InvalidCharSetSet.add (DuplicateChar chr) invalid, seen)
                  else
                    (invalid, CharSet.add chr seen))
                ~acc:(InvalidCharSetSet.empty, CharSet.empty)
                str
            in
            if not (InvalidCharSetSet.is_empty invalid) then
              add_output
                cx
                ~trace
                (EInvalidCharSet
                   {
                     invalid = (replace_desc_reason (RStringLit str) reason, invalid);
                     valid = reason_op;
                     use_op;
                   }))
        | (DefT (reason, trust, CharSetT _), _) -> rec_flow cx trace (StrT.why reason trust, u)
        | (_, UseT (use_op, DefT (reason, trust, CharSetT _))) ->
          rec_flow cx trace (l, UseT (use_op, StrT.why reason trust))
        (* React prop type functions are modeled as a custom function type in Flow,
       so that Flow can exploit the extra information to gratuitously hardcode
       best-effort static checking of dynamic prop type validation.

       A prop type is either a primitive or some complex type, which is a
       function that simplifies to a primitive prop type when called. *)
        | ( CustomFunT (_, ReactPropType (React.PropType.Primitive (false, t))),
            GetPropT (_, reason_op, Named (_, "isRequired"), tout) ) ->
          let prop_type = React.PropType.Primitive (true, t) in
          rec_flow_t
            ~use_op:unknown_use
            cx
            trace
            (CustomFunT (reason_op, ReactPropType prop_type), OpenT tout)
        | (CustomFunT (reason, ReactPropType (React.PropType.Primitive (req, _))), _)
          when object_use u || function_use u || function_like_op u ->
          let builtin_name =
            if req then
              "ReactPropsCheckType"
            else
              "ReactPropsChainableTypeChecker"
          in
          let l = get_builtin_type cx ~trace reason builtin_name in
          rec_flow cx trace (l, u)
        | ( CustomFunT (_, ReactPropType (React.PropType.Complex kind)),
            CallT
              (use_op, reason_op, { call_targs = None; call_args_tlist = arg1 :: _; call_tout; _ })
          ) ->
          React.(
            let tool =
              match kind with
              | PropType.ArrayOf -> SimplifyPropType.ArrayOf
              | PropType.InstanceOf -> SimplifyPropType.InstanceOf
              | PropType.ObjectOf -> SimplifyPropType.ObjectOf
              | PropType.OneOf -> SimplifyPropType.OneOf ResolveArray
              | PropType.OneOfType -> SimplifyPropType.OneOfType ResolveArray
              | PropType.Shape -> SimplifyPropType.Shape ResolveObject
            in
            let t = extract_non_spread cx ~trace arg1 in
            rec_flow
              cx
              trace
              (t, ReactKitT (use_op, reason_op, SimplifyPropType (tool, OpenT call_tout))))
        | (CustomFunT (reason, ReactPropType (React.PropType.Complex kind)), _)
          when object_use u || function_use u || function_like_op u ->
          rec_flow cx trace (get_builtin_prop_type cx ~trace reason kind, u)
        | ( CustomFunT (_, ReactPropType (React.PropType.Primitive (is_req1, t1))),
            UseT (use_op, CustomFunT (_, ReactPropType (React.PropType.Primitive (is_req2, t2)))) )
          when (not is_req2) || is_req1 ->
          rec_unify cx trace ~use_op t1 t2
        | ( CustomFunT (_, ReactCreateClass),
            CallT
              (use_op, reason_op, { call_targs = None; call_args_tlist = arg1 :: _; call_tout; _ })
          ) ->
          let loc_op = aloc_of_reason reason_op in
          let loc_tapp = def_aloc_of_reason (fst call_tout) in
          let desc_tapp = desc_of_reason (fst call_tout) in
          let spec = extract_non_spread cx ~trace arg1 in
          let mk_tvar f = Tvar.mk cx (f reason_op |> derivable_reason) in
          let knot =
            {
              React.CreateClass.this = mk_tvar (replace_desc_reason RThisType);
              static = mk_tvar (replace_desc_reason RThisType);
              state_t =
                mk_tvar
                  (update_desc_reason (fun d ->
                       RTypeParam ("State", (d, loc_op), (desc_tapp, loc_tapp))));
              default_t =
                mk_tvar
                  (update_desc_reason (fun d ->
                       RTypeParam ("Default", (d, loc_op), (desc_tapp, loc_tapp))));
            }
          in
          rec_flow
            cx
            trace
            ( spec,
              ReactKitT
                ( use_op,
                  reason_op,
                  React.CreateClass (React.CreateClass.Spec [], knot, OpenT call_tout) ) )
        | (_, ReactKitT (use_op, reason_op, tool)) -> ReactJs.run cx trace ~use_op reason_op l tool
        (* Facebookisms are special Facebook-specific functions that are not
       expressable with our current type syntax, so we've hacked in special
       handling. Terminate with extreme prejudice. *)
        | ( CustomFunT (_, DebugPrint),
            CallT (use_op, reason_op, { call_targs = None; call_args_tlist; call_tout; _ }) ) ->
          List.iter
            (fun arg ->
              match arg with
              | Arg t -> rec_flow cx trace (t, DebugPrintT reason_op)
              | SpreadArg t ->
                add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t, SpreadArgument)))
            call_args_tlist;
          rec_flow_t
            cx
            ~use_op
            trace
            (VoidT.why reason_op |> with_trust bogus_trust, OpenT call_tout)
        | (CustomFunT (_, DebugThrow), CallT (_, reason_op, _)) ->
          raise (Error_message.EDebugThrow (aloc_of_reason reason_op))
        | ( CustomFunT (_, DebugSleep),
            CallT
              (use_op, reason_op, { call_targs = None; call_args_tlist = arg1 :: _; call_tout; _ })
          ) ->
          let t = extract_non_spread cx ~trace arg1 in
          rec_flow cx trace (t, DebugSleepT reason_op);
          rec_flow_t
            cx
            ~use_op
            trace
            (VoidT.why reason_op |> with_trust bogus_trust, OpenT call_tout)
        | ( CustomFunT
              ( lreason,
                ( (Compose _ | ReactCreateElement | ReactCloneElement | ReactElementFactory _) as
                kind ) ),
            CallT (use_op, reason_op, calltype) ) ->
          let {
            call_targs;
            call_args_tlist = args;
            call_tout = tout;
            call_this_t = _;
            call_closure_t = _;
            call_strict_arity = _;
          } =
            calltype
          in
          (* None of the supported custom funs are polymorphic, so error here
         instead of threading targs into spread resolution. *)
          Base.Option.iter call_targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = aloc_of_reason reason_op;
                      is_new = false;
                      reason_arity = lreason;
                      expected_arity = 0;
                    }));
          let make_op_nonlocal = function
            | FunCall op -> FunCall { op with local = false }
            | FunCallMethod op -> FunCallMethod { op with local = false }
            | op -> op
          in
          let use_op = mod_root_of_use_op make_op_nonlocal use_op in
          resolve_call_list
            cx
            ~trace
            ~use_op
            reason_op
            args
            (ResolveSpreadsToCustomFunCall (mk_id (), kind, OpenT tout))
        | ( CustomFunT (_, (ObjectAssign | ObjectGetPrototypeOf | ObjectSetPrototypeOf)),
            MethodT (use_op, reason_call, _, Named (_, "call"), action, _) ) ->
          rec_flow cx trace (l, apply_method_action use_op reason_call action)
        (* Custom functions are still functions, so they have all the prototype properties *)
        | (CustomFunT (r, _), _) when function_like_op u -> rec_flow cx trace (FunProtoT r, u)
        (*********************************************)
        (* object types deconstruct into their parts *)
        (*********************************************)

        (* ObjT -> ObjT *)
        | ( DefT (lreason, _, ObjT ({ props_tmap = lflds; _ } as l_obj)),
            UseT (use_op, (DefT (ureason, _, ObjT ({ props_tmap = uflds; _ } as u_obj)) as u_deft))
          ) ->
          Type_inference_hooks_js.dispatch_obj_to_obj_hook cx l u_deft;
          let print_fast_path =
            match Context.verbose cx with
            | Some _ -> true
            | _ -> false
          in
          if lflds = uflds then (
            if print_fast_path then prerr_endline "ObjT ~> ObjT fast path: yes"
          ) else (
            if print_fast_path then prerr_endline "ObjT ~> ObjT fast path: no";
            flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj)
          )
        | (DefT (_, _, ObjT _), UseT (_, NullProtoT _)) -> ()
        (* InstanceT -> ObjT *)
        | ( DefT (lreason, _, InstanceT _),
            UseT (use_op, DefT (ureason, _, ObjT { flags = { obj_kind = Exact; _ }; _ })) ) ->
          let reasons = FlowError.ordered_reasons (lreason, ureason) in
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithExact (reasons, use_op, Error_message.Inexact))
        | ( DefT
              ( lreason,
                _,
                InstanceT
                  (_, super, _, { own_props = lown; proto_props = lproto; inst_call_t = lcall; _ })
              ),
            UseT
              ( use_op,
                ( DefT (ureason, _, ObjT { props_tmap = uflds; proto_t = uproto; call_t = ucall; _ })
                as u_deft ) ) ) ->
          Type_inference_hooks_js.dispatch_instance_to_obj_hook cx l u_deft;

          let lflds =
            let own_props = Context.find_props cx lown in
            let proto_props = Context.find_props cx lproto in
            SMap.union own_props proto_props
          in
          Base.Option.iter ucall ~f:(fun ucall ->
              let prop_name = Some "$call" in
              let use_op =
                Frame
                  ( PropertyCompatibility { prop = prop_name; lower = lreason; upper = ureason },
                    use_op )
              in
              match lcall with
              | Some lcall ->
                rec_flow
                  cx
                  trace
                  (Context.find_call cx lcall, UseT (use_op, Context.find_call cx ucall))
              | None ->
                let reason_prop = replace_desc_reason (RProperty prop_name) ureason in
                let error_message =
                  if is_builtin_reason ALoc.source lreason then
                    Error_message.EBuiltinLookupFailed { reason = reason_prop; name = prop_name }
                  else
                    Error_message.EStrictLookupFailed
                      {
                        reason_prop;
                        reason_obj = lreason;
                        name = prop_name;
                        use_op = Some use_op;
                        suggestion = None;
                      }
                in
                add_output cx ~trace error_message);

          Context.iter_real_props cx uflds (fun s up ->
              let use_op =
                Frame
                  (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op)
              in
              let propref =
                let reason_prop = replace_desc_reason (RProperty (Some s)) ureason in
                Named (reason_prop, s)
              in
              match SMap.find_opt s lflds with
              | Some lp -> rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
              | _ ->
                let strict =
                  match up with
                  | Field (_, OptionalT _, _) -> NonstrictReturning (None, None)
                  | _ -> Strict lreason
                in
                rec_flow
                  cx
                  trace
                  ( super,
                    ReposLowerT
                      ( lreason,
                        false,
                        LookupT
                          {
                            reason = ureason;
                            lookup_kind = strict;
                            ts = [];
                            propref;
                            lookup_action = LookupProp (use_op, up);
                            ids = Some (Properties.Set.of_list [lown; lproto]);
                          } ) ));

          rec_flow cx trace (l, UseT (use_op, uproto))
        (* For some object `x` and constructor `C`, if `x instanceof C`, then the
       object is a subtype. We use `ExtendsT` to walk the proto chain of the
       object, in case it includes a nominal type. *)
        | (DefT (_, _, ObjT _), UseT (use_op, (DefT (_, _, InstanceT _) as u))) ->
          rec_flow cx trace (l, extends_use_type use_op l u)
        (****************************************)
        (* You can cast an object to a function *)
        (****************************************)
        | ( DefT (reason, _, (ObjT _ | InstanceT _)),
            ( UseT (use_op, (DefT (reason_op, _, FunT _) | AnyT (reason_op, _)))
            | CallT (use_op, reason_op, _) ) ) ->
          let prop_name = Some "$call" in
          let use_op =
            match u with
            | UseT (_, (DefT (_, _, FunT _) | AnyT _)) ->
              Frame
                ( PropertyCompatibility { prop = prop_name; lower = reason; upper = reason_op },
                  use_op )
            | _ -> use_op
          in
          let fun_t =
            match l with
            | DefT (_, _, ObjT { call_t = Some id; _ })
            | DefT (_, _, InstanceT (_, _, _, { inst_call_t = Some id; _ })) ->
              Context.find_call cx id
            | _ ->
              let reason_prop = replace_desc_reason (RProperty prop_name) reason_op in
              let error_message =
                if is_builtin_reason ALoc.source reason then
                  Error_message.EBuiltinLookupFailed { reason = reason_prop; name = prop_name }
                else
                  Error_message.EStrictLookupFailed
                    {
                      reason_prop;
                      reason_obj = reason;
                      name = prop_name;
                      use_op = Some use_op;
                      suggestion = None;
                    }
              in
              add_output cx ~trace error_message;
              AnyT.error reason_op
          in
          (match u with
          | UseT (_, (DefT (_, _, FunT _) as u_def))
          | UseT (_, (AnyT _ as u_def)) ->
            rec_flow cx trace (fun_t, UseT (use_op, u_def))
          | _ -> rec_flow cx trace (fun_t, u))
        (******************************)
        (* matching shapes of objects *)
        (******************************)

        (* When something of type ShapeT(o) is used, it behaves like it had type o.

           On the other hand, things that can be passed to something of type
           ShapeT(o) must be "subobjects" of o: they may have fewer properties, but
           those properties should be transferable to o.

           Because a property x with a type OptionalT(t) could be considered
           missing or having type t, we consider such a property to be transferable
           if t is a subtype of x's type in o. Otherwise, the property should be
           assignable to o.

           TODO: The type constructors ShapeT, ObjAssignToT/ObjAssignFromT,
           ObjRestT express related meta-operations on objects. Consolidate these
           meta-operations and ensure consistency of their semantics. **)
        | (ShapeT (r, o), _) -> rec_flow cx trace (reposition cx ~trace (aloc_of_reason r) o, u)
        | (DefT (reason, _, ObjT ({ call_t = None; _ } as o)), UseT (use_op, ShapeT (_, proto))) ->
          let props = Context.find_real_props cx o.props_tmap in
          match_shape cx trace ~use_op proto reason props
        | ( DefT (reason, _, InstanceT (_, _, _, ({ inst_call_t = None; _ } as i))),
            UseT (use_op, ShapeT (_, proto)) ) ->
          let own_props = Context.find_props cx i.own_props in
          let proto_props = Context.find_props cx i.proto_props in
          let proto_props =
            match i.inst_kind with
            | InterfaceKind _ -> proto_props
            | ClassKind -> SMap.remove "constructor" proto_props
          in
          let props = SMap.union own_props proto_props in
          match_shape cx trace ~use_op proto reason props
        (* Function definitions are incompatible with ShapeT. ShapeT is meant to
         * match an object type with a subset of the props in the type being
         * destructured. It would be complicated and confusing to use a function for
         * this.
         *
         * This invariant is important for the React setState() type definition. *)
        | (_, UseT (use_op, ShapeT (_, o))) ->
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithShape (reason_of_t l, reason_of_t o, use_op))
        | (AnyT (_, src), ObjTestT (reason_op, _, u)) ->
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src reason_op, u)
        | (_, ObjTestT (reason_op, default, u)) ->
          let u = ReposLowerT (reason_op, false, UseT (unknown_use, u)) in
          if object_like l then
            rec_flow cx trace (l, u)
          else
            rec_flow cx trace (default, u)
        | (AnyT (_, src), ObjTestProtoT (reason_op, u)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason_op, u)
        | (DefT (_, trust, NullT), ObjTestProtoT (reason_op, u)) ->
          rec_flow_t cx trace ~use_op:unknown_use (NullProtoT.why reason_op trust, u)
        | (_, ObjTestProtoT (reason_op, u)) ->
          let proto =
            if object_like l then
              reposition cx ~trace (aloc_of_reason reason_op) l
            else
              let () =
                add_output
                  cx
                  ~trace
                  (Error_message.EInvalidPrototype (aloc_of_reason reason_op, reason_of_t l))
              in
              ObjProtoT.why reason_op |> with_trust bogus_trust
          in
          rec_flow_t cx trace ~use_op:unknown_use (proto, u)
        (********************************************)
        (* array types deconstruct into their parts *)
        (********************************************)

        (* Arrays can flow to arrays *)
        | ( DefT (r1, _, ArrT (ArrayAT (t1, ts1))),
            UseT (use_op, DefT (r2, _, ArrT (ArrayAT (t2, ts2)))) ) ->
          let use_op = Frame (ArrayElementCompatibility { lower = r1; upper = r2 }, use_op) in
          let lit1 = desc_of_reason r1 = RArrayLit in
          let ts1 = Base.Option.value ~default:[] ts1 in
          let ts2 = Base.Option.value ~default:[] ts2 in
          array_flow cx trace use_op lit1 r1 (ts1, t1, ts2, t2)
        (* Tuples can flow to tuples with the same arity *)
        | ( DefT (r1, _, ArrT (TupleAT (_, ts1))),
            UseT (use_op, DefT (r2, _, ArrT (TupleAT (_, ts2)))) ) ->
          let fresh = desc_of_reason r1 = RArrayLit in
          let l1 = List.length ts1 in
          let l2 = List.length ts2 in
          if l1 <> l2 then
            add_output cx ~trace (Error_message.ETupleArityMismatch ((r1, r2), l1, l2, use_op));
          let n = ref 0 in
          iter2opt
            (fun t1 t2 ->
              match (t1, t2) with
              | (Some t1, Some t2) ->
                n := !n + 1;
                let use_op =
                  Frame (TupleElementCompatibility { n = !n; lower = r1; upper = r2 }, use_op)
                in
                flow_to_mutable_child cx trace use_op fresh t1 t2
              | _ -> ())
            (ts1, ts2)
        (* Arrays with known elements can flow to tuples *)
        | (DefT (r1, trust, ArrT (ArrayAT (t1, ts1))), UseT (use_op, DefT (r2, _, ArrT (TupleAT _))))
          ->
          begin
            match ts1 with
            | None -> add_output cx ~trace (Error_message.ENonLitArrayToTuple ((r1, r2), use_op))
            | Some ts1 -> rec_flow cx trace (DefT (r1, trust, ArrT (TupleAT (t1, ts1))), u)
          end
        (* Read only arrays are the super type of all tuples and arrays *)
        | ( DefT (r1, _, ArrT (ArrayAT (t1, _) | TupleAT (t1, _) | ROArrayAT t1)),
            UseT (use_op, DefT (r2, _, ArrT (ROArrayAT t2))) ) ->
          let use_op = Frame (ArrayElementCompatibility { lower = r1; upper = r2 }, use_op) in
          rec_flow cx trace (t1, UseT (use_op, t2))
        | (DefT (_, _, InstanceT _), UseT (use_op, DefT (r2, _, ArrT (ArrayAT (elemt, _))))) ->
          let arrt = get_builtin_typeapp cx ~trace r2 "Array" [elemt] in
          rec_flow cx trace (l, UseT (use_op, arrt))
        | (DefT (_, _, InstanceT _), UseT (use_op, DefT (r2, _, ArrT (ROArrayAT elemt)))) ->
          let arrt = get_builtin_typeapp cx ~trace r2 "$ReadOnlyArray" [elemt] in
          rec_flow cx trace (l, UseT (use_op, arrt))
        (**************************************************)
        (* instances of classes follow declared hierarchy *)
        (**************************************************)
        | (DefT (_, _, InstanceT _), UseT (use_op, (DefT (_, _, InstanceT _) as u))) ->
          rec_flow cx trace (l, extends_use_type use_op l u)
        | ( DefT (reason, _, InstanceT (_, super, implements, instance)),
            ExtendsUseT
              ( use_op,
                reason_op,
                try_ts_on_failure,
                l,
                (DefT (_, _, InstanceT (_, _, _, instance_super)) as u) ) ) ->
          if ALoc.equal_id instance.class_id instance_super.class_id then
            let { type_args = tmap1; _ } = instance in
            let { type_args = tmap2; _ } = instance_super in
            let ureason =
              update_desc_reason
                (function
                  | RExtends desc -> desc
                  | desc -> desc)
                reason_op
            in
            flow_type_args cx trace ~use_op reason ureason tmap1 tmap2
          else
            (* If this instance type has declared implementations, any structural
           tests have already been performed at the declaration site. We can
           then use the ExtendsT use type to search for a nominally matching
           implementation, thereby short-circuiting a potentially expensive
           structural test at the use site. *)
            let u = ExtendsUseT (use_op, reason_op, try_ts_on_failure @ implements, l, u) in
            rec_flow cx trace (super, ReposLowerT (reason, false, u))
        (********************************************************)
        (* runtime types derive static types through annotation *)
        (********************************************************)
        | (DefT (_, _, ClassT it), UseT (_, DefT (r, _, TypeT (_, t)))) ->
          (* a class value annotation becomes the instance type *)
          rec_flow cx trace (it, BecomeT (r, t))
        | (DefT (_, _, TypeT (_, l)), UseT (use_op, DefT (_, _, TypeT (_, u)))) ->
          rec_unify cx trace ~use_op ~unify_any:true l u
        | (DefT (lreason, trust, EnumObjectT enum), UseT (use_op, DefT (_r, _, TypeT (_, t)))) ->
          (* an enum object value annotation becomes the enum type *)
          let enum_type = mk_enum_type ~loc:(aloc_of_reason lreason) ~trust enum in
          rec_unify cx trace ~use_op enum_type t
        | (DefT (_, _, EnumT { enum_name; _ }), UseT (_, DefT (reason, _, TypeT _))) ->
          add_output cx ~trace Error_message.(EEnumMemberUsedAsType { reason; enum_name })
        (* non-class/function values used in annotations are errors *)
        | (_, UseT (_, DefT (reason_use, _, TypeT _))) ->
          (match l with
          (* Short-circut as we already error on the unresolved name. *)
          | AnyT (_, AnyError (Some UnresolvedName)) -> ()
          | AnyT _ -> add_output cx ~trace Error_message.(EAnyValueUsedAsType { reason_use })
          | _ -> add_output cx ~trace Error_message.(EValueUsedAsType { reason_use }))
        | (DefT (rl, _, ClassT l), UseT (use_op, DefT (_, _, ClassT u))) ->
          rec_flow cx trace (reposition cx ~trace (aloc_of_reason rl) l, UseT (use_op, u))
        | ( DefT (_, _, FunT (static1, prototype, _)),
            UseT (use_op, DefT (_, _, ClassT (DefT (_, _, InstanceT (static2, _, _, _)) as u_))) )
          ->
          rec_unify cx trace ~use_op static1 static2;
          rec_unify cx trace ~use_op prototype u_
        (*********************************************************)
        (* class types derive instance types (with constructors) *)
        (*********************************************************)
        | (DefT (reason, _, ClassT this), ConstructorT (use_op, reason_op, targs, args, t)) ->
          let reason_o = replace_desc_reason RConstructorReturn reason in
          let annot_loc = aloc_of_reason reason_op in
          (* early error if type args passed to non-polymorphic class *)
          Base.Option.iter targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = annot_loc;
                      is_new = true;
                      reason_arity = reason_of_t this;
                      expected_arity = 0;
                    }));

          (* call this.constructor(args) *)
          let ret =
            Tvar.mk_no_wrap_where cx reason_op (fun t ->
                let funtype = mk_methodcalltype this None args t in
                let propref = Named (reason_o, "constructor") in
                rec_flow
                  cx
                  trace
                  (this, MethodT (use_op, reason_op, reason_o, propref, CallM funtype, None)))
          in
          (* return this *)
          rec_flow cx trace (ret, ObjTestT (annot_reason ~annot_loc reason_op, this, t))
        (****************************************************************)
        (* function types derive objects through explicit instantiation *)
        (****************************************************************)
        | ( DefT (lreason, _, FunT (_, proto, ({ this_t = this; return_t = ret; _ } as ft))),
            ConstructorT (use_op, reason_op, targs, args, t) ) ->
          (* TODO: closure *)
          (* create new object **)
          let reason_c = replace_desc_reason RNewObject reason_op in
          let objtype =
            let obj_kind = UnsealedInFile (ALoc.source (loc_of_t proto)) in
            let flags = { default_flags with obj_kind } in
            let call = None in
            let pmap = Context.generate_property_map cx SMap.empty in
            mk_objecttype ~flags ~call pmap proto
          in
          let new_obj = DefT (reason_c, bogus_trust (), ObjT objtype) in
          (* error if type arguments are provided to non-polymorphic constructor **)
          Base.Option.iter targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = aloc_of_reason reason_op;
                      is_new = true;
                      reason_arity = lreason;
                      expected_arity = 0;
                    }));

          (* call function with this = new_obj, params = args **)
          rec_flow_t cx trace ~use_op:unknown_use (new_obj, this);
          multiflow_call cx trace ~use_op reason_op args ft;

          (* if ret is object-like, return ret; otherwise return new_obj **)
          let reason_o = replace_desc_reason RConstructorReturn reason_op in
          rec_flow cx trace (ret, ObjTestT (reason_o, new_obj, t))
        | (AnyT _, ConstructorT (use_op, reason_op, targs, args, t)) ->
          ignore targs;

          (* An untyped receiver can't do anything with type args *)
          call_args_iter
            (fun t -> rec_flow cx trace (t, UseT (use_op, AnyT.untyped reason_op)))
            args;
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.untyped reason_op, t)
        (* Since we don't know the signature of a method on AnyT, assume every
       parameter is an AnyT. *)
        | ( AnyT _,
            MethodT (use_op, reason_op, _, _, CallM { call_args_tlist; call_tout; _ }, prop_t) ) ->
          let any = AnyT.untyped reason_op in
          call_args_iter (fun t -> rec_flow cx trace (t, UseT (use_op, any))) call_args_tlist;
          Base.Option.iter
            ~f:(fun prop_t -> rec_flow_t cx trace ~use_op:unknown_use (any, prop_t))
            prop_t;
          rec_flow_t cx trace ~use_op:unknown_use (any, OpenT call_tout)
        | (AnyT _, MethodT (use_op, reason_op, _, _, (ChainM _ as chain), prop_t)) ->
          let any = AnyT.untyped reason_op in
          Base.Option.iter
            ~f:(fun prop_t -> rec_flow_t cx trace ~use_op:unknown_use (any, prop_t))
            prop_t;
          rec_flow cx trace (any, apply_method_action use_op reason_op chain)
        (*************************)
        (* statics can be read   *)
        (*************************)
        | (DefT (_, _, InstanceT (static, _, _, _)), GetStaticsT ((reason_op, _) as tout)) ->
          rec_flow cx trace (static, ReposLowerT (reason_op, false, UseT (unknown_use, OpenT tout)))
        | (AnyT (_, src), GetStaticsT ((reason_op, _) as tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason_op, OpenT tout)
        | (ObjProtoT _, GetStaticsT ((reason_op, _) as tout)) ->
          (* ObjProtoT not only serves as the instance type of the root class, but
         also as the statics of the root class. *)
          rec_flow cx trace (l, ReposLowerT (reason_op, false, UseT (unknown_use, OpenT tout)))
        (********************)
        (* __proto__ getter *)
        (********************)

        (* TODO: Fix GetProtoT for InstanceT (and ClassT).
       The __proto__ object of an instance is an ObjT having the properties in
       insttype.methods_tmap, not the super instance.  *)
        | (DefT (_, _, InstanceT (_, super, _, _)), GetProtoT (reason_op, t)) ->
          let proto = reposition cx ~trace (aloc_of_reason reason_op) super in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        | (DefT (_, _, ObjT { proto_t; _ }), GetProtoT (reason_op, t)) ->
          let proto = reposition cx ~trace (aloc_of_reason reason_op) proto_t in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        | (ObjProtoT _, GetProtoT (reason_op, t)) ->
          let proto = NullT.why reason_op |> with_trust bogus_trust in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        | (FunProtoT reason, GetProtoT (reason_op, t)) ->
          let proto = ObjProtoT (repos_reason (aloc_of_reason reason_op) reason) in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        | (AnyT _, GetProtoT (reason_op, t)) ->
          let proto = AnyT.untyped reason_op in
          rec_flow_t cx trace ~use_op:unknown_use (proto, OpenT t)
        (********************)
        (* __proto__ setter *)
        (********************)
        | (AnyT _, SetProtoT _) -> ()
        | (_, SetProtoT (reason_op, _)) ->
          add_output cx ~trace (Error_message.EUnsupportedSetProto reason_op)
        (********************************************************)
        (* instances of classes may have their fields looked up *)
        (********************************************************)
        | ( DefT (lreason, _, InstanceT (_, super, _, instance)),
            LookupT
              {
                reason = reason_op;
                lookup_kind = kind;
                ts = try_ts_on_failure;
                propref = Named (_, x) as propref;
                lookup_action = action;
                ids;
              } ) ->
          let own_props = Context.find_props cx instance.own_props in
          let proto_props = Context.find_props cx instance.proto_props in
          let pmap = SMap.union own_props proto_props in
          (match SMap.find_opt x pmap with
          | None ->
            (* If there are unknown mixins, the lookup should become nonstrict, as
           the searched-for property may be found in a mixin. *)
            let kind =
              match (instance.has_unknown_react_mixins, kind) with
              | (true, Strict _) -> NonstrictReturning (None, None)
              | _ -> kind
            in
            rec_flow
              cx
              trace
              ( super,
                LookupT
                  {
                    reason = reason_op;
                    lookup_kind = kind;
                    ts = try_ts_on_failure;
                    propref;
                    lookup_action = action;
                    ids =
                      Base.Option.map ids ~f:(fun ids ->
                          Properties.Set.add instance.own_props ids
                          |> Properties.Set.add instance.proto_props);
                  } )
          | Some p ->
            (match kind with
            | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
            | _ -> ());
            perform_lookup_action cx trace propref p PropertyMapProperty lreason reason_op action)
        | (DefT (_, _, InstanceT _), LookupT { reason = reason_op; propref = Computed _; _ }) ->
          (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
          let loc = aloc_of_reason reason_op in
          add_output cx ~trace Error_message.(EInternal (loc, InstanceLookupComputed))
        (********************************)
        (* ... and their fields written *)
        (********************************)
        | ( DefT (reason_c, _, InstanceT (_, super, _, instance)),
            SetPropT (use_op, reason_op, Named (reason_prop, x), mode, wr_ctx, tin, prop_t) ) ->
          let own_props = Context.find_props cx instance.own_props in
          let proto_props = Context.find_props cx instance.proto_props in
          let fields = SMap.union own_props proto_props in
          let strict = Strict reason_c in
          set_prop
            cx
            (Properties.Set.of_list [instance.own_props; instance.proto_props])
            ~mode
            ~wr_ctx
            trace
            ~use_op
            reason_prop
            reason_op
            strict
            l
            super
            x
            fields
            tin
            prop_t
        | (DefT (reason_c, _, InstanceT _), SetPrivatePropT (use_op, reason_op, x, _, [], _, _, _))
          ->
          add_output
            cx
            ~trace
            (Error_message.EPrivateLookupFailed ((reason_op, reason_c), x, use_op))
        | ( DefT (reason_c, _, InstanceT (_, _, _, instance)),
            SetPrivatePropT (use_op, reason_op, x, mode, scope :: scopes, static, tin, prop_tout) )
          ->
          if not (ALoc.equal_id scope.class_binding_id instance.class_id) then
            rec_flow
              cx
              trace
              (l, SetPrivatePropT (use_op, reason_op, x, mode, scopes, static, tin, prop_tout))
          else
            let map =
              if static then
                scope.class_private_static_fields
              else
                scope.class_private_fields
            in
            (match SMap.find_opt x (Context.find_props cx map) with
            | None ->
              add_output
                cx
                ~trace
                (Error_message.EPrivateLookupFailed ((reason_op, reason_c), x, use_op))
            | Some p ->
              let action =
                WriteProp { use_op; obj_t = l; prop_tout; tin; write_ctx = Normal; mode }
              in
              let propref = Named (reason_op, x) in
              perform_lookup_action cx trace propref p PropertyMapProperty reason_c reason_op action)
        | (DefT (_, _, InstanceT _), SetPropT (_, reason_op, Computed _, _, _, _, _)) ->
          (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
          let loc = aloc_of_reason reason_op in
          add_output cx ~trace Error_message.(EInternal (loc, InstanceLookupComputed))
        | ( DefT (reason_c, _, InstanceT (_, super, _, instance)),
            MatchPropT (use_op, reason_op, Named (reason_prop, x), prop_t) ) ->
          let own_props = Context.find_props cx instance.own_props in
          let proto_props = Context.find_props cx instance.proto_props in
          let fields = SMap.union own_props proto_props in
          let strict = Strict reason_c in
          match_prop
            cx
            (Properties.Set.of_list [instance.own_props; instance.proto_props])
            trace
            ~use_op
            reason_prop
            reason_op
            strict
            super
            x
            fields
            (OpenT prop_t)
        (*****************************)
        (* ... and their fields read *)
        (*****************************)
        | ((DefT (r, _, InstanceT _) as instance), GetPropT (_, _, Named (_, "constructor"), t)) ->
          rec_flow_t
            cx
            trace
            ~use_op:unknown_use
            (class_type ?annot_loc:(annot_aloc_of_reason r) instance, OpenT t)
        | ( DefT (reason_c, _, InstanceT (_, super, _, instance)),
            GetPropT (use_op, reason_op, Named (reason_prop, x), tout) ) ->
          let own_props = Context.find_props cx instance.own_props in
          let proto_props = Context.find_props cx instance.proto_props in
          let fields = SMap.union own_props proto_props in
          let strict =
            if instance.has_unknown_react_mixins then
              NonstrictReturning (None, None)
            else
              Strict reason_c
          in
          get_prop
            cx
            (Properties.Set.of_list [instance.own_props; instance.proto_props])
            trace
            ~use_op
            reason_prop
            reason_op
            strict
            l
            super
            x
            fields
            tout
        | (DefT (reason_c, _, InstanceT _), GetPrivatePropT (use_op, reason_op, x, [], _, _)) ->
          add_output
            cx
            ~trace
            (Error_message.EPrivateLookupFailed ((reason_op, reason_c), x, use_op))
        | ( DefT (reason_c, _, InstanceT (_, _, _, instance)),
            GetPrivatePropT (use_op, reason_op, x, scope :: scopes, static, tout) ) ->
          if not (ALoc.equal_id scope.class_binding_id instance.class_id) then
            rec_flow cx trace (l, GetPrivatePropT (use_op, reason_op, x, scopes, static, tout))
          else
            let map =
              if static then
                scope.class_private_static_fields
              else
                scope.class_private_fields
            in
            (match SMap.find_opt x (Context.find_props cx map) with
            | None ->
              add_output
                cx
                ~trace
                (Error_message.EPrivateLookupFailed ((reason_op, reason_c), x, use_op))
            | Some p ->
              let action = ReadProp { use_op; obj_t = l; tout } in
              let propref = Named (reason_op, x) in
              perform_lookup_action cx trace propref p PropertyMapProperty reason_c reason_op action)
        | (DefT (_, _, InstanceT _), GetPropT (_, reason_op, Computed _, _)) ->
          (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
          let loc = aloc_of_reason reason_op in
          add_output cx ~trace Error_message.(EInternal (loc, InstanceLookupComputed))
        (********************************)
        (* ... and their methods called *)
        (********************************)
        | ( DefT (reason_c, _, InstanceT (_, super, _, instance)),
            MethodT (use_op, reason_call, reason_lookup, Named (reason_prop, x), action, prop_t) )
          ->
          (* TODO: closure *)
          let own_props = Context.find_props cx instance.own_props in
          let proto_props = Context.find_props cx instance.proto_props in
          let props = SMap.union own_props proto_props in
          let tvar = Tvar.mk_no_wrap cx reason_lookup in
          let funt = OpenT (reason_lookup, tvar) in
          let strict =
            if instance.has_unknown_react_mixins then
              NonstrictReturning (None, None)
            else
              Strict reason_c
          in
          get_prop
            cx
            (Properties.Set.of_list [instance.own_props; instance.proto_props])
            trace
            ~use_op
            reason_prop
            reason_lookup
            strict
            l
            super
            x
            props
            (reason_lookup, tvar);
          Base.Option.iter
            ~f:(fun prop_t -> rec_flow_t cx ~use_op:unknown_use trace (funt, prop_t))
            prop_t;

          (* suppress ops while calling the function. if `funt` is a `FunT`, then
         `CallT` will set its own ops during the call. if `funt` is something
         else, then something like `VoidT ~> CallT` doesn't need the op either
         because we want to point at the call and undefined thing. *)
          rec_flow cx trace (funt, apply_method_action use_op reason_call action)
        | (DefT (_, _, InstanceT _), MethodT (_, reason_call, _, Computed _, _, _)) ->
          (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
          let loc = aloc_of_reason reason_call in
          add_output cx ~trace Error_message.(EInternal (loc, InstanceLookupComputed))
        (* In traditional type systems, object types are not extensible.  E.g., an
        object {x: 0, y: ""} has type {x: number; y: string}. While it is
        possible to narrow the object's type to hide some of its properties (aka
        width subtyping), extending its type to model new properties is
        impossible. This is not without reason: all object types would then be
        equatable via subtyping, thereby making them unsound.

        In JavaScript, on the other hand, objects can grow dynamically, and
        doing so is a common idiom during initialization (i.e., before they
        become available for general use). Objects that typically grow
        dynamically include not only object literals, but also prototypes,
        export objects, and so on. Thus, it is important to model this idiom.

        To balance utility and soundness, Flow's object types are extensible by
        default, but become sealed as soon as they are subject to width
        subtyping. However, implementing this simple idea needs a lot of care.

        To ensure that aliases have the same underlying type, object types are
        represented indirectly as pointers to records (rather than directly as
        records). And to ensure that typing is independent of the order in which
        fragments of code are analyzed, new property types can be added on gets
        as well as sets (and due to indirection, the new property types become
        immediately available to aliases).

        Looking up properties of an object, e.g. for the purposes of copying,
        when it is not fully initialized is prone to races, and requires careful
        manual reasoning about escape to avoid surprising results.

        Prototypes cause further complications. In JavaScript, objects inherit
        properties of their prototypes, and may override those properties. (This
        is similar to subclasses inheriting and overriding methods of
        superclasses.) At the same time, prototypes are extensible just as much
        as the objects they derive are. In other words, we want to maintain the
        invariant that an object's type is a subtype of its prototype's type,
        while letting them be extensible by default. This invariant is achieved
        by constraints that unify a property's type if and when that property
        exists both on the object and its prototype.

        Here's some example code with type calculations in comments. (We use the
        symbol >=> to denote a flow between a pair of types. The direction of
        flow roughly matches the pattern 'rvalue' >=> 'lvalue'.)

        var o = {}; // o:T, UseT |-> {}
        o.x = 4; // UseT |-> {x:X}, number >=> X
        var s:string = o.x; // ERROR: number >=> string

        function F() { } // F.prototype:P, P |-> {}
        var f = new F(); // f:O, O |-> {}&P

        F.prototype.m = function() { this.y = 4; } // P |-> {m:M}, ... >=> M
        f.m(); // O |-> {y:Y}&P, number >=> Y

    **)

        (**********************************************************************)
        (* objects can be assigned, i.e., their properties can be set in bulk *)
        (**********************************************************************)

        (* Special case any. Otherwise this will lead to confusing errors when any tranforms to an
       object type. *)
        | (AnyT _, ObjAssignToT (use_op, _, _, t, _)) -> rec_flow_t cx ~use_op trace (l, t)
        | (to_obj, ObjAssignToT (use_op, reason, from_obj, t, kind)) ->
          rec_flow cx trace (from_obj, ObjAssignFromT (use_op, reason, to_obj, t, kind))
        (* When some object-like type O1 flows to
        ObjAssignFromT(_,O2,X,ObjAssign), the properties of O1 are copied to
        O2, and O2 is linked to X to signal that the copying is done; the
        intention is that when those properties are read through X, they should
        be found (whereas this cannot be guaranteed when those properties are
        read through O2). However, there is an additional twist: this scheme
        may not work when O2 is unresolved. In particular, when O2 is
        unresolved, the constraints that copy the properties from O1 may race
        with reads of those properties through X as soon as O2 is resolved. To
        avoid this race, we make O2 flow to ObjAssignToT(_,O1,X,ObjAssign);
        when O2 is resolved, we make the switch. **)
        | ( DefT (lreason, _, ObjT { props_tmap = mapr; flags; _ }),
            ObjAssignFromT (use_op, reason_op, to_obj, t, ObjAssign _) ) ->
          Context.iter_props cx mapr (fun x p ->
              (* move the reason to the call site instead of the definition, so
           that it is in the same scope as the Object.assign, so that
           strictness rules apply. *)
              let reason_prop =
                lreason
                |> update_desc_reason (fun desc -> RPropertyOf (x, desc))
                |> repos_reason (aloc_of_reason reason_op)
              in
              match Property.read_t p with
              | Some t ->
                let propref = Named (reason_prop, x) in
                let t = filter_optional cx ~trace reason_prop t in
                rec_flow
                  cx
                  trace
                  ( to_obj,
                    SetPropT
                      (use_op, reason_prop, propref, Assign, Normal, OpenT (reason_prop, t), None)
                  )
              | None ->
                add_output
                  cx
                  ~trace
                  (Error_message.EPropNotReadable { reason_prop; prop_name = Some x; use_op }));
          (match flags.obj_kind with
          | Indexed _ -> rec_flow_t cx trace ~use_op (AnyT.make Untyped reason_op, t)
          | Exact
          | Inexact
          | UnsealedInFile _ ->
            rec_flow_t cx trace ~use_op (to_obj, t))
        | ( DefT (lreason, _, InstanceT (_, _, _, { own_props; proto_props; _ })),
            ObjAssignFromT (use_op, reason_op, to_obj, t, ObjAssign _) ) ->
          let own_props = Context.find_props cx own_props in
          let proto_props = Context.find_props cx proto_props in
          let props = SMap.union own_props proto_props in
          let props_to_skip = ["$key"; "$value"] in
          props
          |> SMap.iter (fun x p ->
                 if not (List.mem x props_to_skip) then
                   match Property.read_t p with
                   | Some t ->
                     let propref = Named (reason_op, x) in
                     rec_flow
                       cx
                       trace
                       (to_obj, SetPropT (use_op, reason_op, propref, Assign, Normal, t, None))
                   | None ->
                     add_output
                       cx
                       ~trace
                       (Error_message.EPropNotReadable
                          { reason_prop = lreason; prop_name = Some x; use_op }));
          rec_flow_t cx ~use_op trace (to_obj, t)
        (* AnyT has every prop, each one typed as `any`, so spreading it into an
       existing object destroys all of the keys, turning the result into an
       AnyT as well. TODO: wait for `to_obj` to be resolved, and then call
       `SetPropT (_, _, _, AnyT, _)` on all of its props. *)
        | (AnyT (_, src), ObjAssignFromT (use_op, reason, _, t, ObjAssign _)) ->
          rec_flow_t cx ~use_op trace (AnyT.make src reason, t)
        | (AnyT _, ObjAssignFromT (use_op, _, _, t, _)) -> rec_flow_t cx ~use_op trace (l, t)
        | (ObjProtoT _, ObjAssignFromT (use_op, _, to_obj, t, ObjAssign _)) ->
          rec_flow_t cx ~use_op trace (to_obj, t)
        (* Object.assign semantics *)
        | (DefT (_, _, (NullT | VoidT)), ObjAssignFromT (use_op, _, to_obj, tout, ObjAssign _)) ->
          rec_flow_t cx ~use_op trace (to_obj, tout)
        (* {...mixed} is the equivalent of {...{[string]: mixed}} *)
        | (DefT (reason, _, MixedT _), ObjAssignFromT (_, _, _, _, ObjAssign _)) ->
          let dict =
            {
              dict_name = None;
              key = StrT.make reason |> with_trust bogus_trust;
              value = l;
              dict_polarity = Polarity.Neutral;
            }
          in
          let o = Obj_type.mk_with_proto cx reason (ObjProtoT reason) ~obj_kind:(Indexed dict) in
          rec_flow cx trace (o, u)
        | (DefT (_, _, ArrT arrtype), ObjAssignFromT (use_op, r, o, t, ObjSpreadAssign)) ->
          begin
            match arrtype with
            | ArrayAT (elemt, None)
            | ROArrayAT elemt ->
              (* Object.assign(o, ...Array<x>) -> Object.assign(o, x) *)
              rec_flow cx trace (elemt, ObjAssignFromT (use_op, r, o, t, default_obj_assign_kind))
            | TupleAT (_, ts)
            | ArrayAT (_, Some ts) ->
              (* Object.assign(o, ...[x,y,z]) -> Object.assign(o, x, y, z) *)
              List.iter
                (fun from ->
                  rec_flow cx trace (from, ObjAssignFromT (use_op, r, o, t, default_obj_assign_kind)))
                ts
          end
        (*************************)
        (* objects can be copied *)
        (*************************)

        (* Note: The story around unsealed objects and rest is not great. One
       thought is to insert a special kind of shadow property into the host
       object, which directs all writes (other than those in `xs`) to the
       unsealed rest result object. For now, the design here is incomplete. *)
        | (DefT (reason_obj, _, ObjT { props_tmap; flags; _ }), ObjRestT (reason, xs, t, id)) ->
          ConstFoldExpansion.guard id (reason_obj, 0) (function
              | 0 ->
                let props = Context.find_props cx props_tmap in
                let props = List.fold_left (fun map x -> SMap.remove x map) props xs in
                (* Remove shadow properties from rest result *)
                let props = SMap.filter (fun x _ -> not (is_internal_name x)) props in
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
                  | _ -> Inexact
                in
                let o = Obj_type.mk_with_proto cx reason ~props proto ~obj_kind in
                rec_flow_t cx trace ~use_op:unknown_use (o, t)
              | _ -> ())
        | (DefT (reason, _, InstanceT (_, super, _, insttype)), ObjRestT (reason_op, xs, t, _)) ->
          (* Spread fields from super into an object *)
          let obj_super =
            Tvar.mk_where cx reason_op (fun tvar ->
                let u = ObjRestT (reason_op, xs, tvar, Reason.mk_id ()) in
                rec_flow cx trace (super, ReposLowerT (reason, false, u)))
          in
          (* Spread own props from the instance into another object *)
          let props = Context.find_props cx insttype.own_props in
          let props = List.fold_left (fun props x -> SMap.remove x props) props xs in
          let proto = ObjProtoT reason_op in
          let obj_inst = Obj_type.mk_unsealed cx reason_op ~props ~proto in
          (* ObjAssign the inst-generated obj into the super-generated obj *)
          let use_op = Op (ObjectSpread { op = reason_op }) in
          let o =
            Tvar.mk_where cx reason_op (fun tvar ->
                rec_flow
                  cx
                  trace
                  ( obj_inst,
                    ObjAssignFromT (use_op, reason_op, obj_super, tvar, default_obj_assign_kind) ))
          in
          rec_flow_t cx ~use_op trace (o, t)
        | (AnyT (_, src), ObjRestT (reason, _, t, _)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason, t)
        | (ObjProtoT _, ObjRestT (reason, _, t, _)) ->
          let obj = Obj_type.mk_unsealed cx reason ~proto:l in
          rec_flow_t cx trace ~use_op:unknown_use (obj, t)
        | (DefT (_, _, (NullT | VoidT)), ObjRestT (reason, _, t, _)) ->
          (* mirroring Object.assign semantics, treat null/void as empty objects *)
          let o = Obj_type.mk_unsealed cx reason in
          rec_flow_t cx trace ~use_op:unknown_use (o, t)
        (*************************************)
        (* objects can be copied-then-sealed *)
        (*************************************)
        | (DefT (_, _, ObjT { props_tmap = mapr; _ }), ObjSealT (reason, t)) ->
          let props = Context.find_props cx mapr in
          let new_obj = Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~props l in
          rec_flow_t cx trace ~use_op:unknown_use (new_obj, t)
        | (AnyT (_, src), ObjSealT (reason, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason, tout)
        (*******************************************)
        (* objects may have their fields looked up *)
        (*******************************************)
        | ( DefT (reason_obj, _, ObjT o),
            LookupT
              {
                reason = reason_op;
                lookup_kind = strict;
                ts = try_ts_on_failure;
                propref;
                lookup_action = action;
                ids;
              } ) ->
          (match get_obj_prop cx trace o propref reason_op with
          | Some (p, target_kind) ->
            (match strict with
            | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
            | _ -> ());
            perform_lookup_action cx trace propref p target_kind reason_obj reason_op action
          | None ->
            let strict =
              match (Obj_type.sealed_in_op reason_op o.flags.obj_kind, strict) with
              | (false, ShadowRead (strict, ids)) -> ShadowRead (strict, Nel.cons o.props_tmap ids)
              | (false, ShadowWrite ids) -> ShadowWrite (Nel.cons o.props_tmap ids)
              | _ -> strict
            in
            rec_flow
              cx
              trace
              ( o.proto_t,
                LookupT
                  {
                    reason = reason_op;
                    lookup_kind = strict;
                    ts = try_ts_on_failure;
                    propref;
                    lookup_action = action;
                    ids = Base.Option.map ids ~f:(Properties.Set.add o.props_tmap);
                  } ))
        | ( AnyT (reason, _),
            LookupT
              {
                reason = reason_op;
                lookup_kind = kind;
                ts = _;
                propref;
                lookup_action = action;
                ids = _;
              } ) ->
          (match action with
          | SuperProp (_, lp) when Property.write_t lp = None ->
            (* Without this exception, we will call rec_flow_p where
             * `write_t lp = None` and `write_t up = Some`, which is a polarity
             * mismatch error. Instead of this, we could "read" `mixed` from
             * covariant props, which would always flow into `any`. *)
            ()
          | _ ->
            let p = Field (None, AnyT.untyped reason_op, Polarity.Neutral) in
            (match kind with
            | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
            | _ -> ());
            perform_lookup_action cx trace propref p DynamicProperty reason reason_op action)
        (*****************************************)
        (* ... and their fields written *)
        (*****************************************)
        | ( DefT (_, _, ObjT { flags; _ }),
            SetPropT (use_op, _, Named (prop, "constructor"), _, _, _, _) ) ->
          if flags.frozen then
            add_output
              cx
              ~trace
              (Error_message.EPropNotWritable
                 { reason_prop = prop; prop_name = Some "constructor"; use_op })
        (* o.x = ... has the additional effect of o[_] = ... **)
        | (DefT (_, _, ObjT { flags; _ }), SetPropT (use_op, _, prop, _, _, _, _)) when flags.frozen
          ->
          let (reason_prop, prop) =
            match prop with
            | Named (r, prop) -> (r, Some prop)
            | Computed t -> (reason_of_t t, None)
          in
          add_output
            cx
            ~trace
            (Error_message.EPropNotWritable { reason_prop; prop_name = prop; use_op })
        | (DefT (reason_obj, _, ObjT o), SetPropT (use_op, reason_op, propref, mode, _, tin, prop_t))
          ->
          write_obj_prop cx trace ~use_op ~mode o propref reason_obj reason_op tin prop_t
        (* Since we don't know the type of the prop, use AnyT. *)
        | (AnyT _, SetPropT (use_op, reason_op, _, _, _, t, prop_t)) ->
          Base.Option.iter
            ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (AnyT.untyped reason_op, t))
            prop_t;
          rec_flow cx trace (t, UseT (use_op, AnyT.untyped reason_op))
        | (DefT (reason_obj, _, ObjT o), MatchPropT (use_op, reason_op, propref, proptype)) ->
          match_obj_prop cx trace ~use_op o propref reason_obj reason_op (OpenT proptype)
        | (AnyT _, MatchPropT (use_op, reason_op, _, t)) ->
          rec_flow cx trace (OpenT t, UseT (use_op, AnyT.untyped reason_op))
        (*****************************)
        (* ... and their fields read *)
        (*****************************)
        | (DefT (_, _, ObjT _), GetPropT (_, reason_op, Named (_, "constructor"), tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (Unsoundness.why Constructor reason_op, OpenT tout)
        | (DefT (reason_obj, _, ObjT o), GetPropT (use_op, reason_op, propref, tout)) ->
          read_obj_prop cx trace ~use_op o propref reason_obj reason_op tout
        | (AnyT _, GetPropT (_, reason_op, _, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.untyped reason_op, OpenT tout)
        (********************************)
        (* ... and their methods called *)
        (********************************)
        | (DefT (_, _, ObjT _), MethodT (_, _, _, Named (_, "constructor"), _, _)) -> ()
        | ( DefT (reason_obj, _, ObjT o),
            MethodT (use_op, reason_call, reason_lookup, propref, action, prop_t) ) ->
          let t =
            Tvar.mk_no_wrap_where cx reason_lookup (fun tout ->
                read_obj_prop cx trace ~use_op o propref reason_obj reason_lookup tout)
          in
          Base.Option.iter
            ~f:(fun prop_t -> rec_flow_t cx trace ~use_op:unknown_use (t, prop_t))
            prop_t;
          rec_flow cx trace (t, apply_method_action use_op reason_call action)
        (******************************************)
        (* strings may have their characters read *)
        (******************************************)
        | (DefT (reason_s, trust, StrT _), GetElemT (use_op, reason_op, index, tout)) ->
          rec_flow cx trace (index, UseT (use_op, NumT.why reason_s |> with_trust bogus_trust));
          rec_flow_t cx trace ~use_op:unknown_use (StrT.why reason_op trust, OpenT tout)
        (* Expressions may be used as keys to access objects and arrays. In
        general, we cannot evaluate such expressions at compile time. However,
        in some idiomatic special cases, we can; in such cases, we know exactly
        which strings/numbers the keys may be, and thus, we can use precise
        properties and indices to resolve the accesses. *)

        (**********************************************************************)
        (* objects/arrays may have their properties/elements written and read *)
        (**********************************************************************)
        | ( (DefT (_, _, (ObjT _ | ArrT _)) | AnyT _),
            SetElemT (use_op, reason_op, key, mode, tin, tout) ) ->
          rec_flow cx trace (key, ElemT (use_op, reason_op, l, WriteElem (tin, tout, mode)))
        | ((DefT (_, _, (ObjT _ | ArrT _)) | AnyT _), GetElemT (use_op, reason_op, key, tout)) ->
          rec_flow cx trace (key, ElemT (use_op, reason_op, l, ReadElem tout))
        | ( (DefT (_, _, (ObjT _ | ArrT _)) | AnyT _),
            CallElemT (reason_call, reason_lookup, key, action) ) ->
          let action = CallElem (reason_call, action) in
          rec_flow cx trace (key, ElemT (unknown_use, reason_lookup, l, action))
        | (_, ElemT (use_op, reason_op, (DefT (_, _, ObjT _) as obj), action)) ->
          elem_action_on_obj cx trace ~use_op l obj reason_op action
        | (_, ElemT (use_op, reason_op, (AnyT _ as obj), action)) ->
          let value = AnyT.untyped reason_op in
          perform_elem_action cx trace ~use_op ~restrict_deletes:false reason_op obj value action
        (* It is not safe to write to an unknown index in a tuple. However, any is
         * a source of unsoundness, so that's ok. `tup[(0: any)] = 123` should not
         * error when `tup[0] = 123` does not. *)
        | (AnyT _, ElemT (use_op, reason_op, (DefT (reason_tup, _, ArrT arrtype) as arr), action))
          ->
          begin
            match (action, arrtype) with
            | (WriteElem _, ROArrayAT _) ->
              let reasons = (reason_op, reason_tup) in
              add_output cx ~trace (Error_message.EROArrayWrite (reasons, use_op))
            | _ -> ()
          end;
          let value = elemt_of_arrtype arrtype in
          perform_elem_action cx trace ~use_op ~restrict_deletes:false reason_op arr value action
        | (l, ElemT (use_op, reason, (DefT (reason_tup, _, ArrT arrtype) as arr), action))
          when numeric l ->
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
                        (try List.nth_opt ts index with Invalid_argument _ -> None)
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
                                 });
                            ( true,
                              AnyT.error (mk_reason RTupleOutOfBoundsAccess (aloc_of_reason reason))
                            )
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
                             { use_op; reason = index_reason; index = index_string });
                        (true, AnyT.error reason)
                      ) else
                        (true, value)
                  end
              end
            | _ -> (false, value)
          in
          ( if is_index_restricted && not can_write_tuple then
            match action with
            (* These are safe to do with tuples and unknown indexes *)
            | ReadElem _
            | CallElem _ ->
              ()
            (* This isn't *)
            | WriteElem _ ->
              let error =
                match ts with
                | Some _ -> Error_message.ETupleUnsafeWrite { reason; use_op }
                | None -> Error_message.EROArrayWrite ((reason, reason_tup), use_op)
              in
              add_output cx ~trace error );

          perform_elem_action cx trace ~use_op ~restrict_deletes:is_tuple reason arr value action
        | (DefT (_, _, ArrT _), GetPropT (_, reason_op, Named (_, "constructor"), tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (Unsoundness.why Constructor reason_op, OpenT tout)
        | (DefT (_, _, ArrT _), SetPropT (_, _, Named (_, "constructor"), _, _, _, _))
        | (DefT (_, _, ArrT _), MethodT (_, _, _, Named (_, "constructor"), _, _)) ->
          ()
        (* computed properties *)
        | (_, CreateObjWithComputedPropT { reason; value; tout_tvar = (tout_reason, tout_id) }) ->
          let on_named_prop reason_named =
            match Context.computed_property_state_for_id cx tout_id with
            | None -> Context.computed_property_add_lower_bound cx tout_id reason_named
            | Some (Context.ResolvedOnce existing_lower_bound_reason) ->
              Context.computed_property_add_multiple_lower_bounds cx tout_id;
              add_output
                cx
                ~trace
                (Error_message.EComputedPropertyWithMultipleLowerBounds
                   {
                     existing_lower_bound_reason;
                     new_lower_bound_reason = reason_named;
                     computed_property_reason = reason;
                   })
            | Some Context.ResolvedMultipleTimes -> ()
          in
          let obj =
            Obj_type.mk_with_proto
              cx
              reason
              ~obj_kind:(UnsealedInFile (ALoc.source (aloc_of_reason reason)))
              ~props:SMap.empty
              (ObjProtoT reason)
          in
          elem_action_on_obj
            cx
            trace
            ~use_op:unknown_use
            ~on_named_prop
            l
            obj
            reason
            (WriteElem (value, Some (OpenT (tout_reason, tout_id)), Assign))
        (**************************************************)
        (* array pattern can consume the rest of an array *)
        (**************************************************)
        | (DefT (_, trust, ArrT arrtype), ArrRestT (_, reason, i, tout)) ->
          let arrtype =
            match arrtype with
            | ArrayAT (_, None)
            | ROArrayAT _ ->
              arrtype
            | ArrayAT (elemt, Some ts) -> ArrayAT (elemt, Some (Base.List.drop ts i))
            | TupleAT (elemt, ts) -> TupleAT (elemt, Base.List.drop ts i)
          in
          let a = DefT (reason, trust, ArrT arrtype) in
          rec_flow_t cx trace ~use_op:unknown_use (a, tout)
        | (AnyT (_, src), ArrRestT (_, reason, _, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.why src reason, tout)
        (*****************)
        (* destructuring *)
        (*****************)
        | (_, DestructuringT (reason, kind, s, tout, id)) ->
          begin
            match kind with
            | DestructAnnot ->
              (* NB: BecomeT used to enforce that 0->1 property is preserved. Is
               * currently necessary, since 0->1 annotations are not always
               * recursively 0->1 -- e.g., class instance types. *)
              let tvar = Tvar.mk_no_wrap cx reason in
              eval_selector cx ~trace reason l s (reason, tvar) id;
              rec_flow cx trace (OpenT (reason, tvar), BecomeT (reason, OpenT tout))
            | DestructInfer -> eval_selector cx ~trace reason l s tout id
          end
        (**************)
        (* object kit *)
        (**************)
        | (_, ObjKitT (use_op, reason, resolve_tool, tool, tout)) ->
          ObjectKit.run cx trace ~use_op reason resolve_tool tool tout l
        (**************************************************)
        (* function types can be mapped over a structure  *)
        (**************************************************)
        | (AnyT _, MapTypeT (_, reason_op, _, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.untyped reason_op, tout)
        | (DefT (_, trust, ArrT arrtype), MapTypeT (use_op, reason_op, TupleMap funt, tout)) ->
          let f x =
            let use_op = Frame (TupleMapFunCompatibility { value = reason_of_t x }, use_op) in
            EvalT (funt, TypeDestructorT (use_op, reason_op, CallType [x]), Eval.generate_id ())
          in
          let arrtype =
            match arrtype with
            | ArrayAT (elemt, ts) -> ArrayAT (f elemt, Base.Option.map ~f:(Base.List.map ~f) ts)
            | TupleAT (elemt, ts) -> TupleAT (f elemt, Base.List.map ~f ts)
            | ROArrayAT elemt -> ROArrayAT (f elemt)
          in
          let t =
            let reason = replace_desc_reason RArrayType reason_op in
            DefT (reason, trust, ArrT arrtype)
          in
          rec_flow_t cx trace ~use_op:unknown_use (t, tout)
        | (_, MapTypeT (use_op, reason, TupleMap funt, tout)) ->
          let iter = get_builtin cx ~trace "$iterate" reason in
          let elemt =
            EvalT (iter, TypeDestructorT (use_op, reason, CallType [l]), Eval.generate_id ())
          in
          let t = DefT (reason, bogus_trust (), ArrT (ROArrayAT elemt)) in
          rec_flow cx trace (t, MapTypeT (use_op, reason, TupleMap funt, tout))
        | (DefT (_, trust, ObjT o), MapTypeT (use_op, reason_op, ObjectMap funt, tout)) ->
          let map_t t =
            let (t, opt) =
              match t with
              | OptionalT { reason = _; type_ = t; use_desc = _ } -> (t, true)
              | _ -> (t, false)
            in
            let use_op = Frame (ObjMapFunCompatibility { value = reason_of_t t }, use_op) in
            let t =
              EvalT (funt, TypeDestructorT (use_op, reason_op, CallType [t]), Eval.generate_id ())
            in
            if opt then
              optional t
            else
              t
          in
          let props_tmap =
            Context.find_props cx o.props_tmap
            |> Properties.map_fields map_t
            |> Context.generate_property_map cx
          in
          let flags =
            {
              o.flags with
              obj_kind =
                Obj_type.map_dict
                  (fun dict ->
                    let value = map_t dict.value in
                    { dict with value })
                  o.flags.obj_kind;
            }
          in
          let mapped_t =
            let reason = replace_desc_reason RObjectType reason_op in
            let t = DefT (reason, trust, ObjT { o with props_tmap; flags }) in
            if Obj_type.is_legacy_exact_DO_NOT_USE o.flags.obj_kind then
              ExactT (reason, t)
            else
              t
          in
          rec_flow_t cx trace ~use_op:unknown_use (mapped_t, tout)
        | (DefT (_, trust, ObjT o), MapTypeT (use_op, reason_op, ObjectMapi funt, tout)) ->
          let mapi_t key t =
            let (t, opt) =
              match t with
              | OptionalT { reason = _; type_ = t; use_desc = _ } -> (t, true)
              | _ -> (t, false)
            in
            let use_op =
              Frame
                (ObjMapiFunCompatibility { key = reason_of_t key; value = reason_of_t t }, use_op)
            in
            let t =
              EvalT
                (funt, TypeDestructorT (use_op, reason_op, CallType [key; t]), Eval.generate_id ())
            in
            if opt then
              optional t
            else
              t
          in
          let mapi_field key t =
            let reason = replace_desc_reason (RStringLit key) reason_op in
            mapi_t (DefT (reason, bogus_trust (), SingletonStrT key)) t
          in
          let props_tmap =
            Context.find_props cx o.props_tmap
            |> Properties.mapi_fields mapi_field
            |> Context.generate_property_map cx
          in
          let flags =
            {
              o.flags with
              obj_kind =
                Obj_type.map_dict
                  (fun dict ->
                    let value = mapi_t dict.key dict.value in
                    { dict with value })
                  o.flags.obj_kind;
            }
          in
          let mapped_t =
            let reason = replace_desc_reason RObjectType reason_op in
            let t = DefT (reason, trust, ObjT { o with props_tmap; flags }) in
            if Obj_type.is_legacy_exact_DO_NOT_USE o.flags.obj_kind then
              ExactT (reason, t)
            else
              t
          in
          rec_flow_t cx trace ~use_op:unknown_use (mapped_t, tout)
        (***********************************************)
        (* functions may have their prototypes written *)
        (***********************************************)
        | ( DefT (_, _, FunT (_, t, _)),
            SetPropT (use_op, reason_op, Named (_, "prototype"), _, _, tin, _) ) ->
          rec_flow
            cx
            trace
            ( tin,
              ObjAssignFromT
                ( use_op,
                  reason_op,
                  t,
                  AnyT.locationless Unsoundness.function_proto,
                  default_obj_assign_kind ) )
        (*********************************)
        (* ... and their prototypes read *)
        (*********************************)
        | (DefT (_, _, FunT (_, t, _)), GetPropT (_, _, Named (_, "prototype"), tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (t, OpenT tout)
        | (DefT (reason, _, ClassT instance), GetPropT (_, _, Named (_, "prototype"), tout)) ->
          let instance = reposition cx ~trace (aloc_of_reason reason) instance in
          rec_flow_t cx trace ~use_op:unknown_use (instance, OpenT tout)
        (***************************************************************************)
        (* assignment of properties to module.exports;                             *)
        (* the only interesting case is where functions may have their statics set *)
        (***************************************************************************)
        | (_, ModuleExportsAssignT (_, assign, tout)) ->
          let l' =
            match l with
            | DefT (r, trust, FunT (statics, proto, ft)) ->
              let reason = reason_of_t statics in
              let statics' = mod_reason_of_t (fun _ -> reason) assign in
              DefT (r, trust, FunT (statics', proto, ft))
            | _ -> l
          in
          rec_flow_t cx trace ~use_op:unknown_use (l', tout)
        (***************************************************************)
        (* functions may be called by passing a receiver and arguments *)
        (***************************************************************)
        | ( FunProtoCallT _,
            CallT (use_op, reason_op, ({ call_this_t = func; call_args_tlist; _ } as funtype)) ) ->
          (* Drop the first argument in the use_op. *)
          let use_op =
            match use_op with
            | Op (FunCall { op; fn; args = _ :: args; local }) ->
              Op (FunCall { op; fn; args; local })
            | Op (FunCallMethod { op; fn; prop; args = _ :: args; local }) ->
              Op (FunCallMethod { op; fn; prop; args; local })
            | _ -> use_op
          in
          begin
            match call_args_tlist with
            (* func.call() *)
            | [] ->
              let funtype =
                {
                  funtype with
                  call_this_t = VoidT.why reason_op |> with_trust bogus_trust;
                  call_args_tlist = [];
                }
              in
              rec_flow cx trace (func, CallT (use_op, reason_op, funtype))
            (* func.call(this_t, ...call_args_tlist) *)
            | Arg call_this_t :: call_args_tlist ->
              let funtype = { funtype with call_this_t; call_args_tlist } in
              rec_flow cx trace (func, CallT (use_op, reason_op, funtype))
            (* func.call(...call_args_tlist) *)
            | (SpreadArg _ as first_arg) :: _ ->
              let call_this_t = extract_non_spread cx ~trace first_arg in
              let funtype = { funtype with call_this_t } in
              rec_flow cx trace (func, CallT (use_op, reason_op, funtype))
          end
        (*******************************************)
        (* ... or a receiver and an argument array *)
        (*******************************************)

        (* resolves the arguments... *)
        | ( FunProtoApplyT lreason,
            CallT (use_op, reason_op, ({ call_this_t = func; call_args_tlist; _ } as funtype)) ) ->
          (* Drop the specific AST derived argument reasons. Our new arguments come
           * from arbitrary positions in the array. *)
          let use_op =
            match use_op with
            | Op (FunCall { op; fn; args = _; local }) -> Op (FunCall { op; fn; args = []; local })
            | Op (FunCallMethod { op; fn; prop; args = _; local }) ->
              Op (FunCallMethod { op; fn; prop; args = []; local })
            | _ -> use_op
          in
          begin
            match call_args_tlist with
            (* func.apply() *)
            | [] ->
              let funtype =
                {
                  funtype with
                  call_this_t = VoidT.why reason_op |> with_trust bogus_trust;
                  call_args_tlist = [];
                }
              in
              rec_flow cx trace (func, CallT (use_op, reason_op, funtype))
            (* func.apply(this_arg) *)
            | [Arg this_arg] ->
              let funtype = { funtype with call_this_t = this_arg; call_args_tlist = [] } in
              rec_flow cx trace (func, CallT (use_op, reason_op, funtype))
            (* func.apply(this_arg, ts) *)
            | [first_arg; Arg ts] ->
              let call_this_t = extract_non_spread cx ~trace first_arg in
              let call_args_tlist = [SpreadArg ts] in
              let funtype = { funtype with call_this_t; call_args_tlist } in
              (* Ignoring `this_arg`, we're basically doing func(...ts). Normally
               * spread arguments are resolved for the multiflow application, however
               * there are a bunch of special-cased functions like bind(), call(),
               * apply, etc which look at the arguments a little earlier. If we delay
               * resolving the spread argument, then we sabotage them. So we resolve
               * it early *)
              let t =
                Tvar.mk_where cx reason_op (fun t ->
                    let resolve_to = ResolveSpreadsToCallT (funtype, t) in
                    resolve_call_list cx ~trace ~use_op reason_op call_args_tlist resolve_to)
              in
              rec_flow_t cx trace ~use_op:unknown_use (func, t)
            | [SpreadArg t1; SpreadArg t2] ->
              add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t1, SpreadArgument));
              add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t2, SpreadArgument))
            | [SpreadArg t]
            | [Arg _; SpreadArg t] ->
              add_output cx ~trace Error_message.(EUnsupportedSyntax (loc_of_t t, SpreadArgument))
            | _ :: _ :: _ :: _ ->
              Error_message.EFunctionCallExtraArg
                (mk_reason RFunctionUnusedArgument (aloc_of_reason lreason), lreason, 2, use_op)
              |> add_output cx ~trace
          end
        (************************************************************************)
        (* functions may be bound by passing a receiver and (partial) arguments *)
        (************************************************************************)
        | ( FunProtoBindT lreason,
            CallT
              ( use_op,
                reason_op,
                ( {
                    call_this_t = func;
                    call_targs;
                    call_args_tlist = first_arg :: call_args_tlist;
                    _;
                  } as funtype ) ) ) ->
          Base.Option.iter call_targs ~f:(fun _ ->
              add_output
                cx
                ~trace
                Error_message.(
                  ECallTypeArity
                    {
                      call_loc = aloc_of_reason reason_op;
                      is_new = false;
                      reason_arity = lreason;
                      expected_arity = 0;
                    }));
          let call_this_t = extract_non_spread cx ~trace first_arg in
          let call_targs = None in
          let funtype = { funtype with call_this_t; call_targs; call_args_tlist } in
          rec_flow cx trace (func, BindT (use_op, reason_op, funtype, false))
        | ( DefT (reason, _, FunT (_, _, ({ this_t = o1; _ } as ft))),
            BindT (use_op, reason_op, calltype, _) ) ->
          let {
            call_this_t = o2;
            call_targs = _;
            (* always None *)
            call_args_tlist = tins2;
            call_tout;
            call_closure_t = _;
            call_strict_arity = _;
          } =
            calltype
          in
          (* TODO: closure *)
          rec_flow_t cx trace ~use_op:unknown_use (o2, o1);

          let resolve_to =
            ResolveSpreadsToMultiflowPartial (mk_id (), ft, reason_op, OpenT call_tout)
          in
          resolve_call_list cx ~trace ~use_op reason tins2 resolve_to
        | (DefT (_, _, ObjT { call_t = Some id; _ }), BindT _) ->
          rec_flow cx trace (Context.find_call cx id, u)
        | (DefT (_, _, InstanceT (_, _, _, { inst_call_t = Some id; _ })), BindT _) ->
          rec_flow cx trace (Context.find_call cx id, u)
        | (AnyT _, BindT (use_op, reason, calltype, _)) ->
          let {
            call_this_t;
            call_targs = _;
            (* always None *)
            call_args_tlist;
            call_tout;
            call_closure_t = _;
            call_strict_arity = _;
          } =
            calltype
          in
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.untyped reason, call_this_t);
          call_args_iter
            (fun param_t -> rec_flow cx trace (AnyT.untyped reason, UseT (use_op, param_t)))
            call_args_tlist;
          rec_flow_t cx trace ~use_op:unknown_use (l, OpenT call_tout)
        | (_, BindT (_, _, { call_tout; _ }, true)) ->
          rec_flow_t cx trace ~use_op:unknown_use (l, OpenT call_tout)
        (***********************************************)
        (* You can use a function as a callable object *)
        (***********************************************)
        | ( DefT (_, _, FunT _),
            UseT
              ( use_op,
                DefT
                  ( _,
                    _,
                    ( ObjT { call_t = Some id; _ }
                    | InstanceT (_, _, _, { inst_call_t = Some id; _ }) ) ) ) ) ->
          let t = Context.find_call cx id in
          rec_flow cx trace (l, UseT (use_op, t))
        (* FunT ~> ObjT *)

        (* Previously, call properties were stored in the props map, and were
       checked against dictionary upper bounds. This is wrong, but useful for
       distinguishing between thunk-like types found in graphql-js.

       Now that call properties are stored separately, it is particularly
       egregious to emit this constraint. This only serves to maintain buggy
       behavior, which should be fixed, and this code removed. *)
        | ( DefT (lreason, _, FunT _),
            UseT (use_op, DefT (ureason, _, ObjT { flags = { obj_kind = Exact; _ }; _ })) ) ->
          let reasons = FlowError.ordered_reasons (lreason, ureason) in
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithExact (reasons, use_op, Error_message.Inexact))
        | ( DefT (lreason, _, FunT _),
            UseT (use_op, DefT (ureason, _, ObjT { flags = { obj_kind = Indexed udict; _ }; _ })) )
          ->
          let { value; dict_polarity; _ } = udict in
          let lit = is_literal_object_reason lreason in
          let s = "$call" in
          let use_op =
            Frame (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op)
          in
          let lp = Field (None, l, Polarity.Positive) in
          let up = Field (None, value, dict_polarity) in
          if lit then
            match (Property.read_t lp, Property.read_t up) with
            | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ()
          else
            let reason_prop = replace_desc_reason (RProperty (Some s)) lreason in
            let propref = Named (reason_prop, s) in
            rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
        (* TODO: This rule doesn't interact very well with union-type checking. It
       looks up Function.prototype, which currently doesn't appear structurally
       in the function type, and thus may not be fully resolved when the
       function type is checked with a union containing the object
       type. Ideally, we should either add Function.prototype to function types
       or fully resolve them when resolving function types, but either way we
       might bomb perf without additional work. Meanwhile, we need an immediate
       fix for the common case where this bug shows up. So leaving this comment
       here as a marker for future work, while going with a band-aid solution
       for now, as motivated below.

       Fortunately, it is quite hard for a function type to successfully
       check against an object type, and even more unlikely when the latter
       is part of a union: the object type must only contain
       Function.prototype methods or statics. Quickly confirming that the
       check would fail before looking up Function.prototype (while falling
       back to the general rule when we cannot guarantee failure) is a safe
       optimization in any case, and fixes the commonly observed case where
       the union type contains both a function type and a object type as
       members, clearly intending for function types to match the former
       instead of the latter. *)
        | ( DefT (reason, _, FunT (statics, _, _)),
            UseT (use_op, DefT (reason_o, _, ObjT { props_tmap; _ })) ) ->
          if
            not
              (quick_error_fun_as_obj
                 cx
                 trace
                 ~use_op
                 reason
                 statics
                 reason_o
                 (Context.find_props cx props_tmap))
          then
            rec_flow cx trace (statics, u)
        (* TODO: similar concern as above *)
        | ( DefT (reason, _, FunT (statics, _, _)),
            UseT
              ( use_op,
                DefT
                  ( reason_inst,
                    _,
                    InstanceT (_, _, _, { own_props; inst_kind = InterfaceKind _; _ }) ) ) ) ->
          if
            not
              (quick_error_fun_as_obj
                 cx
                 trace
                 ~use_op
                 reason
                 statics
                 reason_inst
                 (SMap.filter (fun x _ -> x = "constructor") (Context.find_props cx own_props)))
          then
            rec_flow cx trace (statics, u)
        (***************************************************************)
        (* Enable structural subtyping for upperbounds like interfaces *)
        (***************************************************************)
        | ( _,
            UseT
              (use_op, (DefT (_, _, InstanceT (_, _, _, { inst_kind = InterfaceKind _; _ })) as i))
          ) ->
          rec_flow cx trace (i, ImplementsT (use_op, l))
        | ((ObjProtoT _ | FunProtoT _ | DefT (_, _, NullT)), ImplementsT _) -> ()
        | ( DefT
              ( reason_inst,
                _,
                InstanceT
                  ( _,
                    super,
                    _,
                    { own_props; proto_props; inst_call_t; inst_kind = InterfaceKind _; _ } ) ),
            ImplementsT (use_op, t) ) ->
          structural_subtype cx trace ~use_op t reason_inst (own_props, proto_props, inst_call_t);
          rec_flow cx trace (super, ReposLowerT (reason_inst, false, ImplementsT (use_op, t)))
        | (_, ImplementsT _) ->
          add_output cx ~trace (Error_message.EUnsupportedImplements (reason_of_t l))
        (*********************************************************************)
        (* class A is a base class of class B iff                            *)
        (* properties in B that override properties in A or its base classes *)
        (* have the same signatures                                          *)
        (*********************************************************************)

        (* The purpose of SuperT is to establish consistency between overriding
        properties with overridden properties. As such, the lookups performed
        for the inherited properties are non-strict: they are not required to
        exist. **)
        | ( DefT (ureason, _, InstanceT (st, _, _, _)),
            SuperT (use_op, reason, Derived { own; proto; static }) ) ->
          let check_super l = check_super cx trace ~use_op reason ureason l in
          SMap.iter (check_super l) own;
          SMap.iter (fun x p -> if inherited_method x then check_super l x p) proto;

          (* TODO: inherited_method logic no longer applies for statics. It used to
         when call properties were included in the props, but that is no longer
         the case. All that remains is the "constructor" prop, which has no
         special meaning on the static object. *)
          SMap.iter (fun x p -> if inherited_method x then check_super st x p) static
        (***********************)
        (* opaque types part 2 *)
        (***********************)

        (* Don't refine opaque types based on its bound *)
        | (OpaqueT _, PredicateT (p, t)) -> predicate cx trace t l p
        | (OpaqueT _, GuardT (pred, result, sink)) -> guard cx trace l pred result sink
        | (OpaqueT _, SealGenericT { reason = _; id; name; cont }) ->
          let reason = reason_of_t l in
          continue cx trace (GenericT { reason; id; name; bound = l }) cont
        (* Preserve OpaqueT as consequent, but branch based on the bound *)
        | (OpaqueT (_, { super_t = Some t; _ }), CondT (r, then_t_opt, else_t, tout)) ->
          let then_t_opt =
            match then_t_opt with
            | Some _ -> then_t_opt
            | None -> Some l
          in
          rec_flow cx trace (t, CondT (r, then_t_opt, else_t, tout))
        (* Opaque types may be treated as their supertype when they are a lower bound for a use *)
        | (OpaqueT (_, { super_t = Some t; _ }), _) -> rec_flow cx trace (t, u)
        (***********************************************************)
        (* addition                                                *)
        (***********************************************************)
        | (l, AdderT (use_op, reason, flip, r, u)) ->
          flow_addition cx trace use_op reason flip l r u
        (*********************************************************)
        (* arithmetic/bitwise/update operations besides addition *)
        (*********************************************************)
        | (_, AssertArithmeticOperandT _) when numberesque l -> ()
        | (_, AssertArithmeticOperandT _) ->
          add_output cx ~trace (Error_message.EArithmeticOperand (reason_of_t l))
        (***********************************************************)
        (* coercion                                                *)
        (***********************************************************)

        (* string and number can be coerced to strings *)
        | (DefT (_, _, NumT _), UseT (Op (Coercion _), DefT (_, _, StrT _))) -> ()
        (**************************)
        (* relational comparisons *)
        (**************************)
        | (l, ComparatorT { reason; flip; arg = r }) -> flow_comparator cx trace reason flip l r
        | (l, EqT { reason; flip; arg = r }) -> flow_eq cx trace reason flip l r
        | (l, StrictEqT { reason; cond_context; flip; arg = r }) ->
          flow_strict_eq cx trace reason cond_context flip l r
        (************************)
        (* unary minus operator *)
        (************************)
        | (DefT (_, trust, NumT lit), UnaryMinusT (reason_op, t_out)) ->
          let num =
            match lit with
            | Literal (_, (value, raw)) ->
              let (value, raw) = Flow_ast_utils.negate_number_literal (value, raw) in
              DefT
                (replace_desc_reason RNumber reason_op, trust, NumT (Literal (None, (value, raw))))
            | AnyLiteral
            | Truthy ->
              l
          in
          rec_flow_t cx trace ~use_op:unknown_use (num, t_out)
        | (AnyT _, UnaryMinusT (reason_op, t_out)) ->
          rec_flow_t cx trace ~use_op:unknown_use (AnyT.untyped reason_op, t_out)
        (************************)
        (* binary `in` operator *)
        (************************)

        (* the left-hand side of a `(x in y)` expression is a string or number
       TODO: also, symbols *)
        | (DefT (_, _, StrT _), AssertBinaryInLHST _) -> ()
        | (DefT (_, _, NumT _), AssertBinaryInLHST _) -> ()
        | (_, AssertBinaryInLHST _) ->
          add_output cx ~trace (Error_message.EBinaryInLHS (reason_of_t l))
        (* the right-hand side of a `(x in y)` expression must be object-like *)
        | (DefT (_, _, ArrT _), AssertBinaryInRHST _) -> ()
        | (_, AssertBinaryInRHST _) when object_like l -> ()
        | (_, AssertBinaryInRHST _) ->
          add_output cx ~trace (Error_message.EBinaryInRHS (reason_of_t l))
        (******************)
        (* `for...in` RHS *)
        (******************)

        (* objects are allowed. arrays _could_ be, but are not because it's
       generally safer to use a for or for...of loop instead. *)
        | (_, AssertForInRHST _) when object_like l -> ()
        | ((AnyT _ | ObjProtoT _), AssertForInRHST _) -> ()
        (* null/undefined are allowed *)
        | (DefT (_, _, (NullT | VoidT)), AssertForInRHST _) -> ()
        | (DefT (enum_reason, _, EnumObjectT { enum_name; _ }), AssertForInRHST _) ->
          add_output
            cx
            ~trace
            (Error_message.EEnumNotIterable { reason = enum_reason; enum_name; for_in = true })
        | (_, AssertForInRHST _) -> add_output cx ~trace (Error_message.EForInRHS (reason_of_t l))
        (***********************************)
        (* iterable (e.g. RHS of `for..of` *)
        (***********************************)
        | (DefT (enum_reason, _, EnumObjectT { enum_name; _ }), AssertIterableT _) ->
          add_output
            cx
            ~trace
            (Error_message.EEnumNotIterable { reason = enum_reason; enum_name; for_in = false })
        | (_, AssertIterableT { use_op; reason; async; targs }) ->
          let iterable =
            if async then
              get_builtin_typeapp cx reason "$AsyncIterable" targs
            else
              get_builtin_typeapp cx reason "$Iterable" targs
          in
          rec_flow_t cx trace ~use_op (l, iterable)
        (**************************************)
        (* types may be refined by predicates *)
        (**************************************)
        | (_, PredicateT (p, t)) -> predicate cx trace t l p
        | (_, GuardT (pred, result, sink)) -> guard cx trace l pred result sink
        | (_, SentinelPropTestT (reason, obj, key, sense, enum, result)) ->
          sentinel_refinement cx trace l reason obj key sense enum result
        (*********************)
        (* functions statics *)
        (*********************)
        | (DefT (reason, _, FunT (static, _, _)), _) when object_like_op u ->
          rec_flow cx trace (static, ReposLowerT (reason, false, u))
        (*****************)
        (* class statics *)
        (*****************)

        (* For GetPrivatePropT and SetPrivatePropT, the instance id is needed to determine whether
         * or not the private static field exists on that class. Since we look through the scopes for
         * the type of the field, there is no need to look at the static member of the instance.
         * Instead, we just flip the boolean flag to true, indicating that when the
         * InstanceT ~> Set/GetPrivatePropT constraint is processed that we should look at the
         * private static fields instead of the private instance fields. *)
        | ( DefT (reason, _, ClassT instance),
            GetPrivatePropT (use_op, reason_op, x, scopes, _, tout) ) ->
          let u = GetPrivatePropT (use_op, reason_op, x, scopes, true, tout) in
          rec_flow cx trace (instance, ReposLowerT (reason, false, u))
        | ( DefT (reason, _, ClassT instance),
            SetPrivatePropT (use_op, reason_op, x, mode, scopes, _, tout, tp) ) ->
          let u = SetPrivatePropT (use_op, reason_op, x, mode, scopes, true, tout, tp) in
          rec_flow cx trace (instance, ReposLowerT (reason, false, u))
        | (DefT (reason, _, ClassT instance), _) when object_use u || object_like_op u ->
          let statics = (reason, Tvar.mk_no_wrap cx reason) in
          rec_flow cx trace (instance, GetStaticsT statics);
          rec_flow cx trace (OpenT statics, u)
        (************************)
        (* classes as functions *)
        (************************)

        (* When a class value flows to a function annotation or call site, check for
       the presence of a call property in the former (as a static) compatible
       with the latter.

       TODO: Call properties are excluded from the subclass compatibility
       checks, which makes it unsafe to call a Class<T> type like this.
       For example:

           declare class A { static (): string };
           declare class B extends A { static (): number }
           var klass: Class<A> = B;
           var foo: string = klass(); // passes, but `foo` is a number

       The same issue is also true for constructors, which are similarly
       excluded from subclass compatibility checks, but are allowed on ClassT
       types.
    *)
        | (DefT (reason, _, ClassT instance), (UseT (_, DefT (_, _, FunT _)) | CallT _)) ->
          let statics = (reason, Tvar.mk_no_wrap cx reason) in
          rec_flow cx trace (instance, GetStaticsT statics);
          rec_flow cx trace (OpenT statics, u)
        (*********)
        (* enums *)
        (*********)
        | ( DefT (_, _, EnumObjectT { enum_id = id1; _ }),
            UseT (_, DefT (_, _, EnumObjectT { enum_id = id2; _ })) )
          when ALoc.equal_id id1 id2 ->
          ()
        | ( DefT (enum_reason, trust, EnumObjectT ({ members; _ } as enum)),
            GetPropT (_, access_reason, Named (prop_reason, member_name), tout) ) ->
          (* We guarantee in the parser that enum member names won't start with lowercase
           * "a" through "z", these are reserved for methods. *)
          if (not @@ Base.String.is_empty member_name) && Base.Char.is_lowercase member_name.[0]
          then
            rec_flow
              cx
              trace
              (enum_proto cx trace ~reason:access_reason (enum_reason, trust, enum), u)
          else if SMap.mem member_name members then
            let enum_type =
              reposition
                cx
                ~trace
                (aloc_of_reason access_reason)
                (mk_enum_type ~loc:(def_aloc_of_reason enum_reason) ~trust enum)
            in
            rec_flow_t cx trace ~use_op:unknown_use (enum_type, OpenT tout)
          else
            let member_reason = replace_desc_reason (RIdentifier member_name) prop_reason in
            let suggestion = typo_suggestion (SMap.keys members) member_name in
            add_output
              cx
              ~trace
              (Error_message.EEnumInvalidMemberAccess
                 { member_name = Some member_name; suggestion; reason = member_reason; enum_reason });
            rec_flow_t cx trace ~use_op:unknown_use (AnyT.error access_reason, OpenT tout)
        | (DefT (_, _, EnumObjectT _), TestPropT (reason, _, prop, tout)) ->
          rec_flow cx trace (l, GetPropT (Op (GetProperty reason), reason, prop, tout))
        | (DefT (enum_reason, trust, EnumObjectT enum), MethodT (_, _, lookup_reason, Named _, _, _))
          ->
          rec_flow cx trace (enum_proto cx trace ~reason:lookup_reason (enum_reason, trust, enum), u)
        | (DefT (enum_reason, _, EnumObjectT _), GetElemT (_, _, elem, _)) ->
          let reason = reason_of_t elem in
          add_output
            cx
            ~trace
            (Error_message.EEnumInvalidMemberAccess
               { member_name = None; suggestion = None; reason; enum_reason })
        | (DefT (enum_reason, _, EnumObjectT _), SetPropT (_, op_reason, _, _, _, _, _))
        | (DefT (enum_reason, _, EnumObjectT _), SetElemT (_, op_reason, _, _, _, _)) ->
          add_output
            cx
            ~trace
            (Error_message.EEnumModification { loc = aloc_of_reason op_reason; enum_reason })
        | (DefT (enum_reason, _, EnumObjectT { enum_name; _ }), GetValuesT (op_reason, _)) ->
          add_output
            cx
            ~trace
            (Error_message.EEnumInvalidObjectUtil { reason = op_reason; enum_reason; enum_name })
        | ( DefT (_, _, EnumT { enum_id = id1; _ }),
            UseT (_, DefT (_, _, EnumT { enum_id = id2; _ })) )
          when ALoc.equal_id id1 id2 ->
          ()
        | (DefT (enum_reason, _, EnumT { representation_t; _ }), UseT (use_op, t))
          when TypeUtil.quick_subtype (Context.trust_errors cx) representation_t t ->
          let representation_type =
            match representation_t with
            | DefT (_, _, BoolT _) -> Some "boolean"
            | DefT (_, _, NumT _) -> Some "number"
            | DefT (_, _, StrT _) -> Some "string"
            | DefT (_, _, SymbolT) -> Some "symbol"
            | _ -> None
          in
          add_output
            cx
            ~trace
            (Error_message.EEnumIncompatible
               {
                 reason_lower = enum_reason;
                 reason_upper = reason_of_t t;
                 use_op;
                 representation_type;
               })
        (* Entry point to exhaustive checking logic - when resolving the discriminant as an enum. *)
        | ( DefT (enum_reason, _, EnumT enum),
            EnumExhaustiveCheckT
              {
                reason = check_reason;
                check =
                  EnumExhaustiveCheckPossiblyValid
                    { tool = EnumResolveDiscriminant; possible_checks; checks; default_case };
                incomplete_out;
                discriminant_after_check;
              } ) ->
          enum_exhaustive_check
            cx
            ~trace
            ~check_reason
            ~enum_reason
            ~enum
            ~possible_checks
            ~checks
            ~default_case
            ~incomplete_out
            ~discriminant_after_check
        (* Resolving the case tests. *)
        | ( _,
            EnumExhaustiveCheckT
              {
                reason = check_reason;
                check =
                  EnumExhaustiveCheckPossiblyValid
                    {
                      tool = EnumResolveCaseTest { discriminant_reason; discriminant_enum; check };
                      possible_checks;
                      checks;
                      default_case;
                    };
                incomplete_out;
                discriminant_after_check;
              } ) ->
          let (EnumCheck { member_name; _ }) = check in
          let { enum_id = enum_id_discriminant; members; _ } = discriminant_enum in
          let checks =
            match l with
            | DefT (_, _, EnumObjectT { enum_id = enum_id_check; _ })
              when ALoc.equal_id enum_id_discriminant enum_id_check && SMap.mem member_name members
              ->
              check :: checks
            (* If the check is not the same enum type, ignore it and continue. The user will
             * still get an error as the comparison between discriminant and case test will fail. *)
            | _ -> checks
          in
          enum_exhaustive_check
            cx
            ~trace
            ~check_reason
            ~enum_reason:discriminant_reason
            ~enum:discriminant_enum
            ~possible_checks
            ~checks
            ~default_case
            ~incomplete_out
            ~discriminant_after_check
        | ( DefT (_, _, EnumT { enum_name; members; _ }),
            EnumExhaustiveCheckT
              {
                reason;
                check = EnumExhaustiveCheckInvalid reasons;
                incomplete_out;
                discriminant_after_check = _;
              } ) ->
          let example_member = SMap.choose_opt members |> Base.Option.map ~f:fst in
          List.iter
            (fun reason ->
              add_output cx (Error_message.EEnumInvalidCheck { reason; enum_name; example_member }))
            reasons;
          enum_exhaustive_check_incomplete cx ~trace ~reason incomplete_out
        (* If the discriminant is empty, the check is successful. *)
        | ( DefT (_, _, EmptyT _),
            EnumExhaustiveCheckT
              {
                check =
                  ( EnumExhaustiveCheckInvalid _
                  | EnumExhaustiveCheckPossiblyValid { tool = EnumResolveDiscriminant; _ } );
                _;
              } ) ->
          ()
        (* Non-enum discriminants.
         * If `discriminant_after_check` is empty (e.g. because the discriminant has been refined
         * away by each case), then `trigger` will be empty, which will prevent the implicit void
         * return that could occur otherwise. *)
        | ( _,
            EnumExhaustiveCheckT
              {
                reason;
                check =
                  ( EnumExhaustiveCheckInvalid _
                  | EnumExhaustiveCheckPossiblyValid { tool = EnumResolveDiscriminant; _ } );
                incomplete_out;
                discriminant_after_check;
              } ) ->
          enum_exhaustive_check_incomplete
            cx
            ~trace
            ~reason
            ?trigger:discriminant_after_check
            incomplete_out
        (**************************************************************************)
        (* TestPropT is emitted for property reads in the context of branch tests.
       Such tests are always non-strict, in that we don't immediately report an
       error if the property is not found not in the object type. Instead, if
       the property is not found, we control the result type of the read based
       on the flags on the object type. For exact sealed object types, the
       result type is `void`; otherwise, it is "unknown". Indeed, if the
       property is not found in an exact sealed object type, we can be sure it
       won't exist at run time, so the read will return undefined; but for other
       object types, the property *might* exist at run time, and since we don't
       know what the type of the property would be, we set things up so that the
       result of the read cannot be used in any interesting way. *)
        (**************************************************************************)
        | (DefT (_, _, NullT), TestPropT (reason_op, _, propref, tout)) ->
          (* The wildcard TestPropT implementation forwards the lower bound to
         LookupT. This is unfortunate, because LookupT is designed to terminate
         (successfully) on NullT, but property accesses on null should be type
         errors. Ideally, we should prevent LookupT constraints from being
         syntax-driven, in order to preserve the delicate invariants that
         surround it. *)
          rec_flow cx trace (l, GetPropT (unknown_use, reason_op, propref, tout))
        | (DefT (r, trust, MixedT (Mixed_truthy | Mixed_non_maybe)), TestPropT (_, id, _, tout)) ->
          (* Special-case property tests of definitely non-null/non-void values to
         return mixed and treat them as a hit. *)
          Context.test_prop_hit cx id;
          rec_flow_t
            cx
            trace
            ~use_op:unknown_use
            (DefT (r, trust, MixedT Mixed_everything), OpenT tout)
        | (_, TestPropT (reason_op, id, propref, tout)) ->
          (* NonstrictReturning lookups unify their result, but we don't want to
         unify with the tout tvar directly, so we create an indirection here to
         ensure we only supply lower bounds to tout. *)
          let lookup_default =
            Tvar.mk_where cx reason_op (fun tvar ->
                rec_flow_t ~use_op:unknown_use cx trace (tvar, OpenT tout))
          in
          let name = name_of_propref propref in
          let test_info = Some (id, (reason_op, reason_of_t l)) in
          let lookup_default =
            match l with
            | DefT (_, _, ObjT { flags; _ }) when Obj_type.is_legacy_exact_DO_NOT_USE flags.obj_kind
              ->
              if Obj_type.sealed_in_op reason_op flags.obj_kind then
                let r = replace_desc_reason (RMissingProperty name) reason_op in
                Some (DefT (r, bogus_trust (), VoidT), lookup_default)
              else
                (* This is an unsealed object. We don't now when (or even if) this
                 * property access will resolve, since reads and writes can happen
                 * in any order.
                 *
                 * Due to this, we never error on property accesses. TODO: Build a
                 * separate mechanism unsealed objects that errors after merge if a
                 * shadow prop is read but never written.
                 *
                 * We also should not return a default type on lookup failure,
                 * because a later write could make the lookup succeed.
                 *)
                let () = Context.test_prop_hit cx id in
                None
            | _ ->
              (* Note: a lot of other types could in principle be considered
             "exact". For example, new instances of classes could have exact
             types; so could `super` references (since they are statically
             rather than dynamically bound). However, currently we don't support
             any other exact types. Considering exact types inexact is sound, so
             there is no problem falling back to the same conservative
             approximation we use for inexact types in those cases. *)
              let r = replace_desc_reason (RUnknownProperty name) reason_op in
              Some (DefT (r, bogus_trust (), MixedT Mixed_everything), lookup_default)
          in
          let lookup_kind = NonstrictReturning (lookup_default, test_info) in
          rec_flow
            cx
            trace
            ( l,
              LookupT
                {
                  reason = reason_op;
                  lookup_kind;
                  ts = [];
                  propref;
                  lookup_action = ReadProp { use_op = unknown_use; obj_t = l; tout };
                  ids = Some Properties.Set.empty;
                } )
        (************)
        (* indexing *)
        (************)
        | (DefT (_, _, InstanceT _), GetElemT (use_op, reason, i, t)) ->
          rec_flow
            cx
            trace
            (l, SetPropT (use_op, reason, Named (reason, "$key"), Assign, Normal, i, None));
          rec_flow cx trace (l, GetPropT (use_op, reason, Named (reason, "$value"), t))
        | (DefT (_, _, InstanceT _), SetElemT (use_op, reason, i, mode, tin, tout)) ->
          rec_flow
            cx
            trace
            (l, SetPropT (use_op, reason, Named (reason, "$key"), mode, Normal, i, None));
          rec_flow
            cx
            trace
            (l, SetPropT (use_op, reason, Named (reason, "$value"), mode, Normal, tin, None));
          Base.Option.iter ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (l, t)) tout
        (***************************)
        (* conditional type switch *)
        (***************************)

        (* Use our alternate if our lower bound is empty. *)
        | (DefT (_, _, EmptyT Bottom), CondT (_, _, else_t, tout)) ->
          rec_flow_t cx trace ~use_op:unknown_use (else_t, tout)
        (* Otherwise continue by Flowing out lower bound to tout. *)
        | (_, CondT (_, then_t_opt, _, tout)) ->
          let then_t =
            match then_t_opt with
            | Some t -> t
            | None -> l
          in
          rec_flow_t cx trace ~use_op:unknown_use (then_t, tout)
        (*************************)
        (* repositioning, part 2 *)
        (*************************)

        (* waits for a lower bound to become concrete, and then repositions it to
       the location stored in the ReposLowerT, which is usually the location
       where that lower bound was used; the lower bound's location (which is
       being overwritten) is where it was defined. *)
        | (_, ReposLowerT (reason, use_desc, u)) ->
          rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        (***********************************************************)
        (* generics                                                *)
        (***********************************************************)
        | (_, SealGenericT { reason = _; id; name; cont }) ->
          let reason = reason_of_t l in
          continue cx trace (GenericT { reason; id; name; bound = l }) cont
        | ( GenericT ({ bound = bound1; id = id1; reason = reason1; _ } as g1),
            UseT (use_op, GenericT ({ bound = bound2; id = id2; reason = reason2; _ } as g2)) ) ->
          begin
            match Generic.satisfies ~printer:(print_if_verbose_lazy cx trace) id1 id2 with
            | Generic.Satisfied ->
              rec_flow_t
                cx
                trace
                ~use_op
                (position_generic_bound reason1 bound1, position_generic_bound reason2 bound2)
            | Generic.Lower id ->
              rec_flow_t
                cx
                trace
                ~use_op
                (GenericT { g1 with id }, position_generic_bound reason2 bound2)
            | Generic.Upper id ->
              rec_flow_t
                cx
                trace
                ~use_op
                (position_generic_bound reason1 bound1, GenericT { g2 with id })
          end
        | (GenericT { reason; bound; _ }, _) ->
          rec_flow cx trace (position_generic_bound reason bound, u)
        | (_, UseT (use_op, GenericT { reason; name; id; _ })) ->
          let desc =
            RPolyTest (name, RIncompatibleInstantiation name, id |> Generic.aloc_of_id, false)
          in
          let bot = DefT (replace_desc_reason desc reason, literal_trust (), EmptyT Zeroed) in
          rec_flow_t cx trace ~use_op (l, bot)
        (***************)
        (* unsupported *)
        (***************)

        (* Lookups can be strict or non-strict, as denoted by the presence or
        absence of strict_reason in the following two pattern matches.
        Strictness derives from whether the object is sealed and was
        created in the same scope in which the lookup occurs - see
        mk_strict_lookup_reason below. The failure of a strict lookup
        to find the desired property causes an error; a non-strict one
        does not.
     *)
        | ( (DefT (_, _, NullT) | ObjProtoT _),
            LookupT
              { reason; lookup_kind; ts = next :: try_ts_on_failure; propref; lookup_action; ids }
          ) ->
          (* When s is not found, we always try to look it up in the next element in
         the list try_ts_on_failure. *)
          rec_flow
            cx
            trace
            ( next,
              LookupT { reason; lookup_kind; ts = try_ts_on_failure; propref; lookup_action; ids }
            )
        | ( (ObjProtoT _ | FunProtoT _),
            LookupT
              {
                reason = reason_op;
                lookup_kind = _;
                ts = [];
                propref = Named (_, "__proto__");
                lookup_action = ReadProp { use_op = _; obj_t = l; tout };
                ids = _;
              } ) ->
          (* __proto__ is a getter/setter on Object.prototype *)
          rec_flow cx trace (l, GetProtoT (reason_op, tout))
        | ( (ObjProtoT _ | FunProtoT _),
            LookupT
              {
                reason = reason_op;
                lookup_kind = _;
                ts = [];
                propref = Named (_, "__proto__");
                lookup_action =
                  WriteProp { use_op = _; obj_t = l; prop_tout = _; tin; write_ctx = _; mode = _ };
                ids = _;
              } ) ->
          (* __proto__ is a getter/setter on Object.prototype *)
          rec_flow cx trace (l, SetProtoT (reason_op, tin))
        | (ObjProtoT _, LookupT { reason = reason_op; ts = []; propref = Named (_, x); _ })
          when is_object_prototype_method x ->
          (* TODO: These properties should go in Object.prototype. Currently we
          model Object.prototype as a ObjProtoT, as an optimization against a
          possible deluge of shadow properties on Object.prototype, since it
          is shared by every object. **)
          rec_flow cx trace (get_builtin_type cx ~trace reason_op "Object", u)
        | (FunProtoT _, LookupT { reason = reason_op; propref = Named (_, x); _ })
          when is_function_prototype x ->
          (* TODO: Ditto above comment for Function.prototype *)
          rec_flow cx trace (get_builtin_type cx ~trace reason_op "Function", u)
        | ( (DefT (reason, _, NullT) | ObjProtoT reason | FunProtoT reason),
            LookupT
              {
                reason = reason_op;
                lookup_kind = Strict strict_reason;
                ts = [];
                propref = Named (reason_prop, x) as propref;
                lookup_action = action;
                ids;
              } ) ->
          let error_message =
            if is_builtin_reason ALoc.source reason then
              Error_message.EBuiltinLookupFailed { reason = reason_prop; name = Some x }
            else
              let use_op = use_op_of_lookup_action action in
              let suggestion =
                Base.Option.bind ids ~f:(fun ids ->
                    prop_typo_suggestion cx (Properties.Set.elements ids) x)
              in
              Error_message.EStrictLookupFailed
                { reason_prop; reason_obj = strict_reason; name = Some x; use_op; suggestion }
          in
          add_output cx ~trace error_message;
          let p = Field (None, AnyT.error_of_kind UnresolvedName reason_op, Polarity.Neutral) in
          perform_lookup_action cx trace propref p DynamicProperty reason reason_op action
        | ( (DefT (reason, _, NullT) | ObjProtoT reason | FunProtoT reason),
            LookupT
              {
                reason = reason_op;
                lookup_kind = Strict strict_reason;
                ts = [];
                propref = Computed elem_t as propref;
                lookup_action = action;
                ids = _;
              } ) ->
          (match elem_t with
          | OpenT _ ->
            let loc = loc_of_t elem_t in
            add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedOpen))
          | DefT (_, _, StrT (Literal _)) ->
            let loc = loc_of_t elem_t in
            add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedLiteral))
          | AnyT _ ->
            let p = Field (None, AnyT.untyped reason_op, Polarity.Neutral) in
            perform_lookup_action cx trace propref p DynamicProperty reason reason_op action
          | DefT (_, _, StrT _)
          | DefT (_, _, NumT _) ->
            (* string, and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
            let p =
              Field (None, Unsoundness.why ComputedNonLiteralKey reason_op, Polarity.Neutral)
            in
            perform_lookup_action cx trace propref p PropertyMapProperty reason reason_op action
          | _ ->
            let reason_prop = reason_of_t elem_t in
            let error_message =
              if is_builtin_reason ALoc.source reason then
                Error_message.EBuiltinLookupFailed { reason = reason_prop; name = None }
              else
                let use_op = use_op_of_lookup_action action in
                Error_message.EStrictLookupFailed
                  {
                    reason_prop;
                    reason_obj = strict_reason;
                    name = None;
                    use_op;
                    suggestion = None;
                  }
            in
            add_output cx ~trace error_message)
        | ( (DefT (reason, _, NullT) | ObjProtoT reason | FunProtoT reason),
            LookupT
              {
                reason = reason_op;
                lookup_kind = ShadowRead (strict, rev_proto_ids);
                ts = [];
                propref = Named (reason_prop, x) as propref;
                lookup_action = action;
                ids;
              } ) ->
          (* Emit error if this is a strict read. See `lookup_kinds` in types.ml. *)
          (match strict with
          | None -> ()
          | Some strict_reason ->
            let error_message =
              if is_builtin_reason ALoc.source reason then
                Error_message.EBuiltinLookupFailed { reason = reason_prop; name = Some x }
              else
                let use_op = use_op_of_lookup_action action in
                let suggestion =
                  Base.Option.bind ids ~f:(fun ids ->
                      prop_typo_suggestion cx (Properties.Set.elements ids) x)
                in
                Error_message.EStrictLookupFailed
                  { reason_prop; reason_obj = strict_reason; name = Some x; use_op; suggestion }
            in
            add_output cx ~trace error_message);

          (* Install shadow prop (if necessary) and link up proto chain. *)
          let prop_loc = def_aloc_of_reason reason_prop in
          let p = find_or_intro_shadow_prop cx trace reason_op x prop_loc (Nel.rev rev_proto_ids) in
          perform_lookup_action cx trace propref p PropertyMapProperty reason reason_op action
        | ( (DefT (reason, _, NullT) | ObjProtoT reason | FunProtoT reason),
            LookupT
              {
                reason = reason_op;
                lookup_kind = ShadowWrite rev_proto_ids;
                ts = [];
                propref = Named (lookup_reason, x) as propref;
                lookup_action = action;
                ids = _;
              } ) ->
          let (id, proto_ids) = Nel.rev rev_proto_ids in
          let pmap = Context.find_props cx id in
          (* Re-check written-to unsealed object to see if prop was added since we
           * last looked. See comment above `find` in `find_or_intro_shadow_prop`.
           *)
          let p =
            match SMap.find_opt x pmap with
            | Some p -> p
            | None ->
              (match SMap.find_opt (internal_name x) pmap with
              | Some p ->
                (* unshadow *)
                pmap
                |> SMap.remove (internal_name x)
                |> SMap.add x p
                |> Context.add_property_map cx id;
                p
              | None ->
                (* Create prop and link shadow props along the proto chain. *)
                let reason_prop = replace_desc_new_reason (RShadowProperty x) reason_op in
                let t = Tvar.mk cx reason_prop in
                let prop_loc = def_aloc_of_reason lookup_reason in
                (match proto_ids with
                | [] -> ()
                | id :: ids ->
                  let p_proto = find_or_intro_shadow_prop cx trace reason_op x prop_loc (id, ids) in
                  let t_proto = Property.assert_field p_proto in
                  rec_flow cx trace (t_proto, UnifyT (t_proto, t)));

                (* Add prop *)
                let p = Field (Some prop_loc, t, Polarity.Neutral) in
                pmap |> SMap.add x p |> Context.add_property_map cx id;
                p)
          in
          perform_lookup_action cx trace propref p PropertyMapProperty reason reason_op action
        | ( (DefT (_, _, NullT) | ObjProtoT _ | FunProtoT _),
            LookupT { lookup_kind = ShadowRead _; ts = []; propref = Computed elem_t; _ } ) ->
          let loc = loc_of_t elem_t in
          add_output cx ~trace Error_message.(EInternal (loc, ShadowReadComputed))
        | ( (DefT (_, _, NullT) | ObjProtoT _ | FunProtoT _),
            LookupT { lookup_kind = ShadowWrite _; ts = []; propref = Computed elem_t; _ } ) ->
          let loc = loc_of_t elem_t in
          add_output cx ~trace Error_message.(EInternal (loc, ShadowWriteComputed))
        (* LookupT is a non-strict lookup *)
        | ( (DefT (_, _, NullT) | ObjProtoT _ | FunProtoT _),
            LookupT
              {
                lookup_kind = NonstrictReturning (t_opt, test_opt);
                ts = [];
                propref;
                lookup_action = action;
                _;
              } ) ->
          (* don't fire

         ...unless a default return value is given. Two examples:

         1. A failure could arise when an unchecked module was looked up and
         not found declared, in which case we consider that module's exports to
         be `any`.

         2. A failure could arise also when an object property is looked up in
         a condition, in which case we consider the object's property to be
         `mixed`.
      *)
          let use_op = Base.Option.value ~default:unknown_use (use_op_of_lookup_action action) in
          Base.Option.iter test_opt ~f:(fun (id, reasons) ->
              Context.test_prop_miss cx id (name_of_propref propref) reasons use_op);

          begin
            match t_opt with
            | Some (not_found, t) -> rec_unify cx trace ~use_op ~unify_any:true t not_found
            | None -> ()
          end
        (* SuperT only involves non-strict lookups *)
        | (DefT (_, _, NullT), SuperT _)
        | (ObjProtoT _, SuperT _)
        | (FunProtoT _, SuperT _) ->
          ()
        (* ExtendsT searches for a nominal superclass. The search terminates with
        either failure at the root or a structural subtype check. **)
        | (AnyT _, ExtendsUseT _) -> ()
        | (DefT (lreason, _, ObjT { proto_t; _ }), ExtendsUseT _) ->
          let l = reposition cx ~trace (aloc_of_reason lreason) proto_t in
          rec_flow cx trace (l, u)
        | (DefT (reason, _, ClassT instance), ExtendsUseT _) ->
          let statics = (reason, Tvar.mk_no_wrap cx reason) in
          rec_flow cx trace (instance, GetStaticsT statics);
          rec_flow cx trace (OpenT statics, u)
        | (DefT (_, _, NullT), ExtendsUseT (use_op, reason, next :: try_ts_on_failure, l, u)) ->
          (* When seaching for a nominal superclass fails, we always try to look it
         up in the next element in the list try_ts_on_failure. *)
          rec_flow cx trace (next, ExtendsUseT (use_op, reason, try_ts_on_failure, l, u))
        | ( DefT (_, _, NullT),
            ExtendsUseT
              ( use_op,
                _,
                [],
                l,
                DefT
                  ( reason_inst,
                    _,
                    InstanceT
                      ( _,
                        super,
                        _,
                        { own_props; proto_props; inst_call_t; inst_kind = InterfaceKind _; _ } ) )
              ) ) ->
          structural_subtype cx trace ~use_op l reason_inst (own_props, proto_props, inst_call_t);
          rec_flow cx trace (l, UseT (use_op, super))
        (***********************)
        (* Object library call *)
        (***********************)
        | (ObjProtoT reason, _) ->
          let use_desc = true in
          let obj_proto = get_builtin_type cx ~trace reason ~use_desc "Object" in
          rec_flow cx trace (obj_proto, u)
        | (_, UseT (use_op, ObjProtoT reason)) ->
          let use_desc = true in
          let obj_proto = get_builtin_type cx ~trace reason ~use_desc "Object" in
          rec_flow cx trace (l, UseT (use_op, obj_proto))
        (*************************)
        (* Function library call *)
        (*************************)
        | (FunProtoT reason, _) ->
          let use_desc = true in
          let fun_proto = get_builtin_type cx ~trace reason ~use_desc "Function" in
          rec_flow cx trace (fun_proto, u)
        | (_, UseT (use_op, FunProtoT reason)) ->
          let use_desc = true in
          let fun_proto = get_builtin_type cx ~trace reason ~use_desc "Function" in
          rec_flow cx trace (l, UseT (use_op, fun_proto))
        | (_, ExtendsUseT (use_op, _, [], t, tc)) ->
          let (reason_l, reason_u) = FlowError.ordered_reasons (reason_of_t t, reason_of_t tc) in
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithUseOp
               { reason_lower = reason_l; reason_upper = reason_u; use_op })
        (*******************************)
        (* ToString abstract operation *)
        (*******************************)

        (* ToStringT passes through strings unchanged, and flows a generic StrT otherwise *)
        | (DefT (_, _, StrT _), ToStringT (_, t_out)) -> rec_flow cx trace (l, t_out)
        | (_, ToStringT (reason_op, t_out)) ->
          rec_flow cx trace (StrT.why reason_op |> with_trust bogus_trust, t_out)
        (**********************)
        (* Array library call *)
        (**********************)
        | ( DefT (reason, _, ArrT (ArrayAT (t, _))),
            (GetPropT _ | SetPropT _ | MethodT _ | LookupT _) ) ->
          rec_flow cx trace (get_builtin_typeapp cx ~trace reason "Array" [t], u)
        (*************************)
        (* Tuple "length" access *)
        (*************************)
        | (DefT (reason, trust, ArrT (TupleAT (_, ts))), GetPropT (_, _, Named (_, "length"), tout))
          ->
          (* Use definition as the reason for the length, as this is
           * the actual location where the length is in fact set. *)
          let reason_op = reason_of_use_t u in
          let loc = Reason.aloc_of_reason reason_op in
          let t = tuple_length reason trust ts in
          rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace loc t, OpenT tout)
        | ( DefT (reason, _, ArrT ((TupleAT _ | ROArrayAT _) as arrtype)),
            (GetPropT _ | SetPropT _ | MethodT _ | LookupT _) ) ->
          let t = elemt_of_arrtype arrtype in
          rec_flow cx trace (get_builtin_typeapp cx ~trace reason "$ReadOnlyArray" [t], u)
        (***********************)
        (* String library call *)
        (***********************)
        | (DefT (reason, _, StrT _), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "String", u)
        (***********************)
        (* Number library call *)
        (***********************)
        | (DefT (reason, _, NumT _), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "Number", u)
        (***********************)
        (* Boolean library call *)
        (***********************)
        | (DefT (reason, _, BoolT _), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "Boolean", u)
        (***********************)
        (* Symbol library call *)
        (***********************)
        | (DefT (reason, _, SymbolT), u) when primitive_promoting_use_t u ->
          rec_flow cx trace (get_builtin_type cx ~trace reason "Symbol", u)
        (*****************************************************)
        (* Nice error messages for mixed function refinement *)
        (*****************************************************)
        | ( DefT (lreason, _, MixedT Mixed_function),
            (MethodT _ | SetPropT _ | GetPropT _ | MatchPropT _ | LookupT _) ) ->
          rec_flow cx trace (FunProtoT lreason, u)
        | ( DefT (lreason, _, MixedT Mixed_function),
            (CallT (use_op, ureason, _) | UseT (use_op, DefT (ureason, _, FunT _))) ) ->
          add_output
            cx
            ~trace
            (Error_message.EIncompatible
               {
                 lower = (lreason, None);
                 upper = (ureason, Error_message.IncompatibleMixedCallT);
                 use_op = Some use_op;
                 branches = [];
               });
          rec_flow cx trace (AnyT.make (AnyError None) lreason, u)
        (* Special cases of FunT *)
        | (FunProtoApplyT reason, _)
        | (FunProtoBindT reason, _)
        | (FunProtoCallT reason, _) ->
          rec_flow cx trace (FunProtoT reason, u)
        | (_, LookupT { propref; lookup_action; _ }) ->
          let use_op = use_op_of_lookup_action lookup_action in
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleProp
               {
                 prop =
                   (match propref with
                   | Named (_, name) -> Some name
                   | Computed _ -> None);
                 reason_prop = reason_of_propref propref;
                 reason_obj = reason_of_t l;
                 special = error_message_kind_of_lower l;
                 use_op;
               })
        | (_, UseT (use_op, u)) ->
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithUseOp
               { reason_lower = reason_of_t l; reason_upper = reason_of_t u; use_op })
        | _ ->
          add_output
            cx
            ~trace
            (Error_message.EIncompatible
               {
                 lower = (reason_of_t l, error_message_kind_of_lower l);
                 upper = (reason_of_use_t u, error_message_kind_of_upper u);
                 use_op = use_op_of_use_t u;
                 branches = [];
               })
    )

  (**
 * Addition
 *
 * According to the spec, given l + r:
 *  - if l or r is a string, or a Date, or an object whose
 *    valueOf() returns an object, returns a string.
 *  - otherwise, returns a number
 *
 * Since we don't consider valueOf() right now, Date is no different than
 * any other object. The only things that are neither objects nor strings
 * are numbers, booleans, null, undefined and symbols. Since we can more
 * easily enumerate those things, this implementation inverts the check:
 * anything that is a number, boolean, null or undefined is treated as a
 * number; everything else is a string.
 *
 * However, if l or r is a number and the other side is invalid, then we assume
 * you were going for a number; generate an error on the invalid side; and flow
 * `number` out as the result of the addition, even though at runtime it will be
 * a string. Fixing the error will make the result type correct. The alternative
 * is that we would error on both l and r, saying neither is compatible with
 * `string`.
 *
 * We are less permissive than the spec when it comes to string coersion:
 * only numbers can be coerced, to allow things like `num + '%'`.
 *
 * TODO: handle symbols (which raise a TypeError, so should be banned)
 *
 **)
  and flow_addition cx trace use_op reason flip l r u =
    if needs_resolution r || is_generic r then
      rec_flow cx trace (r, AdderT (use_op, reason, not flip, l, u))
    else
      let (l, r) =
        if flip then
          (r, l)
        else
          (l, r)
      in
      let loc = aloc_of_reason reason in
      match (l, r) with
      | (DefT (_, _, StrT _), DefT (_, _, StrT _))
      | (DefT (_, _, StrT _), DefT (_, _, NumT _))
      | (DefT (_, _, NumT _), DefT (_, _, StrT _)) ->
        rec_flow_t cx trace ~use_op:unknown_use (StrT.at loc |> with_trust bogus_trust, u)
      (* unreachable additions are unreachable *)
      | (DefT (_, _, EmptyT Bottom), _)
      | (_, DefT (_, _, EmptyT Bottom)) ->
        rec_flow_t cx trace ~use_op:unknown_use (EmptyT.at loc |> with_trust bogus_trust, u)
      | (DefT (reason, _, MixedT _), _)
      | (_, DefT (reason, _, MixedT _)) ->
        add_output cx ~trace (Error_message.EAdditionMixed (reason, use_op))
      | (DefT (_, _, EmptyT Zeroed), t)
      | (t, DefT (_, _, EmptyT Zeroed)) ->
        rec_flow_t cx trace ~use_op:unknown_use (t, u)
      | (DefT (_, _, (NumT _ | BoolT _)), DefT (_, _, (NumT _ | BoolT _))) ->
        rec_flow_t cx trace ~use_op:unknown_use (NumT.at loc |> with_trust bogus_trust, u)
      | (DefT (_, _, StrT _), _) ->
        rec_flow cx trace (r, UseT (use_op, l));
        rec_flow_t cx trace ~use_op:unknown_use (StrT.at loc |> with_trust bogus_trust, u)
      | (_, DefT (_, _, StrT _)) ->
        rec_flow cx trace (l, UseT (use_op, r));
        rec_flow_t cx trace ~use_op:unknown_use (StrT.at loc |> with_trust bogus_trust, u)
      | (DefT (reason, _, (VoidT | NullT)), _)
      | (_, DefT (reason, _, (VoidT | NullT))) ->
        add_output cx ~trace (Error_message.EArithmeticOperand reason);
        rec_flow_t cx trace ~use_op:unknown_use (NumT.at loc |> with_trust bogus_trust, u)
      | (AnyT (_, src), _)
      | (_, AnyT (_, src)) ->
        rec_flow_t cx trace ~use_op:unknown_use (AnyT.at src loc, u)
      | (DefT (_, _, NumT _), _) ->
        rec_flow cx trace (r, UseT (use_op, l));
        rec_flow_t cx trace ~use_op:unknown_use (NumT.at loc |> with_trust bogus_trust, u)
      | (_, DefT (_, _, NumT _)) ->
        rec_flow cx trace (l, UseT (use_op, r));
        rec_flow_t cx trace ~use_op:unknown_use (NumT.at loc |> with_trust bogus_trust, u)
      | (_, _) ->
        let fake_str = StrT.why reason |> with_trust bogus_trust in
        rec_flow cx trace (l, UseT (use_op, fake_str));
        rec_flow cx trace (r, UseT (use_op, fake_str));
        rec_flow cx trace (fake_str, UseT (use_op, u))

  (**
 * relational comparisons like <, >, <=, >=
 *
 * typecheck iff either of the following hold:
 *   number <> number = number
 *   string <> string = string
 **)
  and flow_comparator cx trace reason flip l r =
    if needs_resolution r || is_generic r then
      rec_flow cx trace (r, ComparatorT { reason; flip = not flip; arg = l })
    else
      let (l, r) =
        if flip then
          (r, l)
        else
          (l, r)
      in
      match (l, r) with
      | (DefT (_, _, StrT _), DefT (_, _, StrT _)) -> ()
      | (_, _) when numberesque l && numberesque r -> ()
      | (DefT (_, _, EmptyT _), _)
      | (_, DefT (_, _, EmptyT _)) ->
        ()
      | _ ->
        let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
        add_output cx ~trace (Error_message.EComparison reasons)

  (**
 * == equality
 *
 * typecheck iff they intersect (otherwise, unsafe coercions may happen).
 *
 * note: almost any types may be compared with === (in)equality.
 **)
  and flow_eq cx trace reason flip l r =
    if needs_resolution r || is_generic r then
      rec_flow cx trace (r, EqT { reason; flip = not flip; arg = l })
    else
      let (l, r) =
        if flip then
          (r, l)
        else
          (l, r)
      in
      if equatable (l, r) then
        ()
      else
        let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
        add_output cx ~trace (Error_message.ENonStrictEqualityComparison reasons)

  and flow_strict_eq cx trace reason cond_context flip l r =
    if needs_resolution r || is_generic r then
      rec_flow cx trace (r, StrictEqT { reason; cond_context; flip = not flip; arg = l })
    else
      let (l, r) =
        if flip then
          (r, l)
        else
          (l, r)
      in
      match strict_equatable_error cond_context (l, r) with
      | Some error -> add_output cx ~trace error
      | None -> ()

  and exact_obj_error cx trace obj_kind ~use_op ~exact_reason (l, u) =
    let error_kind =
      match obj_kind with
      | Indexed _ -> Error_message.Indexer
      | _ -> Error_message.Inexact
    in
    let reasons = FlowError.ordered_reasons (reason_of_t l, exact_reason) in
    add_output cx ~trace (Error_message.EIncompatibleWithExact (reasons, use_op, error_kind));

    (* Continue the Flow even after we've errored. Often, there is more that
      * is different then just the fact that the upper bound is exact and the
      * lower bound is not. This could easily hide errors in ObjT ~> ExactT *)
    rec_flow_t cx trace ~use_op (l, u)

  and flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj) =
    let { flags = lflags; call_t = lcall; props_tmap = lflds; proto_t = lproto } = l_obj in
    let { flags = rflags; call_t = ucall; props_tmap = uflds; proto_t = uproto } = u_obj in
    (* if inflowing type is literal (thus guaranteed to be
     unaliased), propertywise subtyping is sound *)
    let lit = is_literal_object_reason lreason || lflags.frozen in
    (* If both are dictionaries, ensure the keys and values are compatible
     with each other. *)
    let ldict = Obj_type.get_dict_opt lflags.obj_kind in
    let udict = Obj_type.get_dict_opt rflags.obj_kind in
    (match (ldict, udict) with
    | ( Some { key = lk; value = lv; dict_polarity = lpolarity; _ },
        Some { key = uk; value = uv; dict_polarity = upolarity; _ } ) ->
      (* Don't report polarity errors when checking the indexer key. We would
       * report these errors again a second time when checking values. *)
      rec_flow_p
        cx
        trace
        ~report_polarity:false
        ~use_op:(Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op))
        lreason
        ureason
        (Computed uk)
        (Field (None, lk, lpolarity), Field (None, uk, upolarity));
      rec_flow_p
        cx
        trace
        ~use_op:
          (Frame (PropertyCompatibility { prop = None; lower = lreason; upper = ureason }, use_op))
        lreason
        ureason
        (Computed uv)
        (Field (None, lv, lpolarity), Field (None, uv, upolarity))
    | _ -> ());

    if rflags.obj_kind = Exact && not (is_literal_object_reason ureason) then (
      Context.iter_real_props cx lflds (fun s _ ->
          if not (Context.has_prop cx uflds s) then
            let use_op =
              Frame
                ( PropertyCompatibility
                    {
                      prop = Some s;
                      (* Lower and upper are reversed in this case since the lower object
                       * is the one requiring the prop. *)
                      lower = ureason;
                      upper = lreason;
                    },
                  use_op )
            in
            let reason_prop = replace_desc_reason (RProperty (Some s)) lreason in
            let err =
              Error_message.EPropNotFound
                { prop_name = Some s; reason_prop; reason_obj = ureason; use_op; suggestion = None }
            in
            add_output cx ~trace err);
      Base.Option.iter lcall ~f:(fun _ ->
          if Base.Option.is_none ucall then
            let prop = Some "$call" in
            let use_op =
              Frame
                ( PropertyCompatibility
                    {
                      prop;
                      (* Lower and upper are reversed in this case since the lower object
                       * is the one requiring the prop. *)
                      lower = ureason;
                      upper = lreason;
                    },
                  use_op )
            in
            let reason_prop = replace_desc_reason (RProperty prop) lreason in
            let err =
              Error_message.EPropNotFound
                { prop_name = prop; reason_prop; reason_obj = ureason; use_op; suggestion = None }
            in
            add_output cx ~trace err)
    );

    (match ucall with
    | Some ucall ->
      let prop_name = Some "$call" in
      let use_op =
        Frame (PropertyCompatibility { prop = prop_name; lower = lreason; upper = ureason }, use_op)
      in
      (match lcall with
      | Some lcall ->
        rec_flow cx trace (Context.find_call cx lcall, UseT (use_op, Context.find_call cx ucall))
      | None ->
        let reason_prop = replace_desc_reason (RProperty prop_name) ureason in
        let error_message =
          if is_builtin_reason ALoc.source lreason then
            Error_message.EBuiltinLookupFailed { reason = reason_prop; name = prop_name }
          else
            Error_message.EStrictLookupFailed
              {
                reason_prop;
                reason_obj = lreason;
                name = prop_name;
                use_op = Some use_op;
                suggestion = None;
              }
        in
        add_output cx ~trace error_message)
    | None -> ());

    (* Properties in u must either exist in l, or match l's indexer. *)
    Context.iter_real_props cx uflds (fun s up ->
        let reason_prop = replace_desc_reason (RProperty (Some s)) ureason in
        let propref = Named (reason_prop, s) in
        let use_op' = use_op in
        let use_op =
          Frame (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op')
        in
        match (Context.get_prop cx lflds s, ldict) with
        | (Some lp, _) ->
          if lit then (
            (* prop from unaliased LB: check <:, then make exact *)
            (match (Property.read_t lp, Property.read_t up) with
            | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ());
            speculative_object_write cx lflds s up
          ) else
            (* prop from aliased LB *)
            rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
        | (None, Some { key; value; dict_polarity; _ }) when not (is_dictionary_exempt s) ->
          rec_flow
            cx
            trace
            ( string_key s reason_prop,
              UseT
                (Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op'), key)
            );
          let lp = Field (None, value, dict_polarity) in
          let up =
            match up with
            | Field (loc, OptionalT { reason = _; type_ = ut; use_desc = _ }, upolarity) ->
              Field (loc, ut, upolarity)
            | _ -> up
          in
          if lit then
            match (Property.read_t lp, Property.read_t up) with
            | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ()
          else
            rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
        | _ ->
          (* property doesn't exist in inflowing type *)
          (match up with
          | Field (_, OptionalT _, _) when lit ->
            (* if property is marked optional or otherwise has a maybe type,
           and if inflowing type is a literal (i.e., it is not an
           annotation), then we add it to the inflowing type as
           an optional property *)
            speculative_object_write cx lflds s up
          | Field (_, OptionalT _, Polarity.Positive)
            when Obj_type.is_exact_or_sealed ureason lflags.obj_kind ->
            rec_flow
              cx
              trace
              ( lproto,
                LookupT
                  {
                    reason = ureason;
                    lookup_kind = NonstrictReturning (None, None);
                    ts = [];
                    propref;
                    lookup_action = LookupProp (use_op, up);
                    ids = None;
                  } )
          | _ ->
            (* When an object type is unsealed, typing it as another object type should add properties
           of that object type to it as needed. We do this when not speculating, because adding
           properties changes state, and the state change is necessary to enforce
           consistency. *)
            if not (Obj_type.sealed_in_op ureason lflags.obj_kind) then
              speculative_object_write cx lflds s up
            else
              (* otherwise, look up the property in the prototype *)
              let strict =
                match (Obj_type.sealed_in_op ureason lflags.obj_kind, ldict) with
                | (false, None) -> ShadowRead (Some lreason, Nel.one lflds)
                | (true, None) -> Strict lreason
                | _ -> NonstrictReturning (None, None)
              in
              rec_flow
                cx
                trace
                ( lproto,
                  LookupT
                    {
                      reason = ureason;
                      lookup_kind = strict;
                      ts = [];
                      propref;
                      lookup_action = LookupProp (use_op, up);
                      ids = None;
                    } )));

    (* Any properties in l but not u must match indexer *)
    (match udict with
    | None -> ()
    | Some { key; value; dict_polarity; _ } ->
      let keys =
        Context.fold_real_props
          cx
          lflds
          (fun s lp keys ->
            if Context.has_prop cx uflds s then
              keys
            else
              let use_op =
                Frame
                  (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op)
              in
              let lp =
                match lp with
                | Field (loc, OptionalT { reason = _; type_ = lt; use_desc = _ }, lpolarity) ->
                  Field (loc, lt, lpolarity)
                | _ -> lp
              in
              let up = Field (None, value, dict_polarity) in
              begin
                if lit then
                  match (Property.read_t lp, Property.read_t up) with
                  | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
                  | _ -> ()
                else
                  let reason_prop = replace_desc_reason (RProperty (Some s)) lreason in
                  let propref = Named (reason_prop, s) in
                  rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
              end;
              string_key s lreason :: keys)
          []
        |> union_of_ts lreason
      in
      rec_flow
        cx
        trace
        ( keys,
          UseT (Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op), key)
        );

      (* Previously, call properties were stored in the props map, and were
       checked against dictionary upper bounds. This is wrong, but useful for
       distinguishing between thunk-like types found in graphql-js.

       Now that call properties are stored separately, it is particularly
       egregious to emit this constraint. This only serves to maintain buggy
       behavior, which should be fixed, and this code removed. *)
      (match (lcall, ucall) with
      | (Some lcall, None) ->
        let s = "$call" in
        let use_op =
          Frame (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op)
        in
        let lp =
          match Context.find_call cx lcall with
          | OptionalT { reason = _; type_ = t; use_desc = _ } -> Field (None, t, Polarity.Positive)
          | t -> Field (None, t, Polarity.Positive)
        in
        let up = Field (None, value, dict_polarity) in
        if lit then
          match (Property.read_t lp, Property.read_t up) with
          | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
          | _ -> ()
        else
          let reason_prop = replace_desc_reason (RProperty (Some s)) lreason in
          let propref = Named (reason_prop, s) in
          rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
      | _ -> ()));

    rec_flow
      cx
      trace
      (uproto, ReposUseT (ureason, false, use_op, DefT (lreason, bogus_trust (), ObjT l_obj)))

  (* Returns true when __flow should succeed immediately if EmptyT of a given
   flavor flows into u. *)
  and empty_success flavor u =
    match (flavor, u) with
    (* Work has to happen when Empty flows to these types whether the EmptyT
     originates from generic testing or elsewhere. This logic was previously
     captured in ground_subtype. *)
    | (_, UseT (_, OpenT _))
    | (_, UseT (_, TypeDestructorTriggerT _))
    | (_, ChoiceKitUseT _)
    | (_, CondT _)
    | (_, DestructuringT _)
    | (_, EnumExhaustiveCheckT _)
    | (_, MakeExactT _)
    | (_, ObjKitT _)
    | (_, ReposLowerT _)
    | (_, ReposUseT _)
    | (_, UnifyT _)
    | (_, SealGenericT _)
    | (_, ResolveUnionT _) ->
      false
    | (Bottom, _) -> true
    (* After this line, flavor is always Zeroed. *)
    (* Special cases: these cases actually utilize the fact that the LHS is Empty,
     either by specially propagating it or selecting cases, etc. *)
    | (_, UseT (_, ExactT _))
    | (_, AdderT _)
    | (_, AndT _)
    | (_, OrT _)
    (* Propagation cases: these cases don't use the fact that the LHS is
     empty, but they propagate the LHS to other types and trigger additional
     flows that may need to occur. *)
    | (_, UseT (_, DefT (_, _, PolyT _)))
    | (_, UseT (_, TypeAppT _))
    | (_, UseT (_, MaybeT _))
    | (_, UseT (_, MergedT _))
    | (_, UseT (_, OpaqueT _))
    | (_, UseT (_, OptionalT _))
    | (_, UseT (_, ReposT _))
    | (_, UseT (_, ThisClassT _))
    | (_, UseT (_, ThisTypeAppT _))
    | (_, UseT (_, UnionT _))
    | (_, AssertExportIsTypeT _)
    | (_, AssertImportIsValueT _)
    | (_, BecomeT _)
    | (_, BindT _)
    | (_, CallLatentPredT _)
    | (_, CallOpenPredT _)
    | (_, CJSExtractNamedExportsT _)
    | (_, ComparatorT _)
    | (_, DebugPrintT _)
    | (_, StrictEqT _)
    | (_, EqT _)
    | (_, ExportTypeT _)
    | (_, IdxUnwrap _)
    | (_, ImportTypeT _)
    | (_, ImportTypeofT _)
    | (_, IntersectionPreprocessKitT _)
    | (_, InvariantT _)
    | (_, MapTypeT (_, _, TupleMap _, _))
    | (_, NotT _)
    | (_, NullishCoalesceT _)
    | (_, ObjAssignToT _)
    | (_, ObjTestT _)
    | (_, ObjTestProtoT _)
    | (_, OptionalChainT _)
    | (_, SentinelPropTestT _)
    | (_, TestPropT _) ->
      false
    (* Error prevention: we should succeed because otherwise we'll hit
     a case with a wildcard on the LHS that raises an error, which in
     this situation would be spurious *)
    | (_, UseT (_, AnnotT _))
    | (_, UseT (_, EvalT _))
    | (_, UseT (_, DefT (_, _, TypeT _)))
    | (_, UseT (_, ShapeT _))
    | (_, AssertArithmeticOperandT _)
    | (_, AssertBinaryInLHST _)
    | (_, AssertBinaryInRHST _)
    | (_, AssertForInRHST _)
    | (_, LookupT _)
    | (_, ImplementsT _)
    | (_, SetProtoT _)
    (* No more work: we can succeed without flowing EmptyT any further
     because the relevant cases don't propagate the LHS to any other
     types; either the flow would succeed anyways or it would fall
     through to the final catch-all error case and cause a spurious
     error. *)
    | (_, UseT _)
    | (_, ArrRestT _)
    | (_, AssertIterableT _)
    | (_, CallElemT _)
    | (_, CallT _)
    | (_, CJSRequireT _)
    | (_, ConcretizeTypeAppsT _)
    | (_, ConstructorT _)
    | (_, CopyNamedExportsT _)
    | (_, CopyTypeExportsT _)
    | (_, CreateObjWithComputedPropT _)
    | (_, DebugSleepT _)
    | (_, ElemT _)
    | (_, ExportNamedT _)
    | (_, ExtendsUseT _)
    | (_, FunImplicitVoidReturnT _)
    | (_, GetElemT _)
    | (_, GetKeysT _)
    | (_, GetPrivatePropT _)
    | (_, GetPropT _)
    | (_, GetProtoT _)
    | (_, GetStaticsT _)
    | (_, GetValuesT _)
    | (_, GuardT _)
    | (_, HasOwnPropT _)
    | (_, IdxUnMaybeifyT _)
    | (_, ImportDefaultT _)
    | (_, ImportModuleNsT _)
    | (_, ImportNamedT _)
    | (_, MatchPropT _)
    | (_, MapTypeT _) (* Note the TupleMap case above *)
    | (_, MethodT _)
    | (_, MixinT _)
    | (_, ObjAssignFromT _)
    | (_, ObjRestT _)
    | (_, ObjSealT _)
    | (_, PredicateT _)
    | (_, ReactInToProps _)
    | (_, ReactKitT _)
    | (_, ReactPropsToOut _)
    | (_, RefineT _)
    | (_, ResolveSpreadT _)
    | (_, SetElemT _)
    | (_, SetPrivatePropT _)
    | (_, SetPropT _)
    | (_, SpecializeT _)
    | (_, SubstOnPredT _)
    | (_, SuperT _)
    | (_, ThisSpecializeT _)
    | (_, ToStringT _)
    | (_, TypeAppVarianceCheckT _)
    | (_, TypeCastT _)
    | (_, EnumCastT _)
    | (_, UnaryMinusT _)
    | (_, VarianceCheckT _)
    | (_, ModuleExportsAssignT _)
    | (_, FilterOptionalT _)
    | (_, FilterMaybeT _) ->
      true

  and handle_generic cx trace bound reason id name u =
    let make_generic t = GenericT { reason; id; name; bound = t } in
    let narrow_generic_with_continuation mk_use_t cont =
      let t_out' = (reason, Tvar.mk_no_wrap cx reason) in
      let use_t = mk_use_t t_out' in
      rec_flow cx trace (position_generic_bound reason bound, use_t);
      rec_flow cx trace (OpenT t_out', SealGenericT { reason; id; name; cont })
    in
    let narrow_generic_use mk_use_t use_t_out =
      narrow_generic_with_continuation mk_use_t (Upper use_t_out)
    in
    let narrow_generic ?(use_op = unknown_use) mk_use_t t_out =
      narrow_generic_use (fun v -> mk_use_t (OpenT v)) (UseT (use_op, t_out))
    in
    let narrow_generic_tvar ?(use_op = unknown_use) mk_use_t t_out =
      narrow_generic_use mk_use_t (UseT (use_op, OpenT t_out))
    in
    let wait_for_concrete_bound () =
      rec_flow cx trace (bound, SealGenericT { reason; id; name; cont = Upper u });
      true
    in
    let distribute_union_intersection () =
      match bound with
      | UnionT (_, rep) ->
        let (t1, (t2, ts)) = UnionRep.members_nel rep in
        let union_of_generics =
          UnionRep.make (make_generic t1) (make_generic t2) (Base.List.map ~f:make_generic ts)
        in
        rec_flow cx trace (UnionT (reason, union_of_generics), u);
        true
      | IntersectionT (_, rep) ->
        let (t1, (t2, ts)) = InterRep.members_nel rep in
        let inter_of_generics =
          InterRep.make (make_generic t1) (make_generic t2) (Base.List.map ~f:make_generic ts)
        in
        rec_flow cx trace (IntersectionT (reason, inter_of_generics), u);
        true
      | _ -> false
    in
    if
      match bound with
      | GenericT { bound; id = id'; _ } ->
        Generic.collapse id id'
        |> Base.Option.value_map ~default:false ~f:(fun id ->
               rec_flow cx trace (GenericT { reason; name; bound; id }, u);
               true)
      | KeysT _ ->
        rec_flow
          cx
          trace
          (position_generic_bound reason bound, SealGenericT { reason; id; name; cont = Upper u });
        true
      | DefT (_, _, EmptyT flavor) -> empty_success flavor u
      | _ -> false
    then
      true
    else
      match u with
      (* In this set of cases, we flow the generic's upper bound to u. This is what we normally would do
          in the catch-all generic case anyways, but these rules are to avoid wildcards elsewhere in __flow. *)
      | AdderT _
      | EqT _
      | StrictEqT _
      | ComparatorT _
      | UnaryMinusT _
      | AssertArithmeticOperandT _
      | AssertForInRHST _
      | AssertBinaryInLHST _
      | AssertBinaryInRHST _
      | TestPropT _
      | OptionalChainT _
      | MapTypeT _
      (* the above case is not needed for correctness, but rather avoids a slow path in TupleMap *)
      | UseT (_, ShapeT _)
      | UseT (Op (Coercion _), DefT (_, _, StrT _)) ->
        rec_flow cx trace (position_generic_bound reason bound, u);
        true
      (* The LHS is what's actually getting refined--don't do anything special for the RHS *)
      | PredicateT (RightP _, _)
      | PredicateT (NotP (RightP _), _) ->
        false
      | PredicateT (pred, t_out) ->
        narrow_generic_tvar (fun t_out' -> PredicateT (pred, t_out')) t_out;
        true
      | ToStringT (r, t_out) ->
        narrow_generic_use (fun t_out' -> ToStringT (r, UseT (unknown_use, OpenT t_out'))) t_out;
        true
      | UseT (use_op, MaybeT (r, t_out)) ->
        narrow_generic ~use_op (fun t_out' -> UseT (use_op, MaybeT (r, t_out'))) t_out;
        true
      | UseT (use_op, OptionalT ({ type_ = t_out; _ } as opt)) ->
        narrow_generic
          ~use_op
          (fun t_out' -> UseT (use_op, OptionalT { opt with type_ = t_out' }))
          t_out;
        true
      | FilterMaybeT (use_op, t_out) ->
        narrow_generic (fun t_out' -> FilterMaybeT (use_op, t_out')) t_out;
        true
      | FilterOptionalT (use_op, t_out) ->
        narrow_generic (fun t_out' -> FilterOptionalT (use_op, t_out')) t_out;
        true
      | ObjRestT (r, xs, t_out, id) ->
        narrow_generic (fun t_out' -> ObjRestT (r, xs, t_out', id)) t_out;
        true
      | MakeExactT (reason_op, k) ->
        narrow_generic_with_continuation
          (fun t_out -> MakeExactT (reason_op, Upper (UseT (unknown_use, OpenT t_out))))
          k;
        true
      | UseT (use_op, ExactT (r, u)) ->
        if is_concrete bound then
          match bound with
          | DefT (_, _, ObjT { flags; _ }) when not @@ Obj_type.is_exact_or_sealed r flags.obj_kind
            ->
            exact_obj_error cx trace flags.obj_kind ~exact_reason:r ~use_op (make_generic bound, u);
            true
          | _ -> false
        else
          wait_for_concrete_bound ()
      (* Support "new this.constructor ()" *)
      | GetPropT (op, r, Named (x, "constructor"), t_out) ->
        if is_concrete bound then
          match bound with
          | DefT (_, _, InstanceT _) ->
            narrow_generic_tvar
              (fun t_out' -> GetPropT (op, r, Named (x, "constructor"), t_out'))
              t_out;
            true
          | _ -> false
        else
          wait_for_concrete_bound ()
      | ConstructorT (use_op, reason_op, targs, args, t_out) ->
        if is_concrete bound then
          match bound with
          | DefT (_, _, ClassT _) ->
            narrow_generic
              (fun t_out' -> ConstructorT (use_op, reason_op, targs, args, t_out'))
              t_out;
            true
          | _ -> false
        else
          wait_for_concrete_bound ()
      | ElemT _ ->
        if is_concrete bound && not (is_literal_type bound) then
          distribute_union_intersection ()
        else
          wait_for_concrete_bound ()
      | ObjKitT _
      | UseT (_, IntersectionT _) ->
        if is_concrete bound then
          distribute_union_intersection ()
        else
          wait_for_concrete_bound ()
      | UseT (_, (UnionT _ as u)) ->
        if union_optimization_guard cx (Context.trust_errors cx |> TypeUtil.quick_subtype) bound u
        then begin
          if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path (via a generic)";
          true
        end else if is_concrete bound then
          distribute_union_intersection ()
        else
          wait_for_concrete_bound ()
      | UseT (_, TypeDestructorTriggerT _) ->
        if is_concrete bound then
          false
        else
          wait_for_concrete_bound ()
      | ResolveSpreadT _ when not (is_concrete bound) -> wait_for_concrete_bound ()
      | _ -> false

  (* "Expands" any to match the form of a type. Allows us to reuse our propagation rules for any
   cases. Note that it is not always safe to do this (ie in the case of unions).
   Note: we can get away with a shallow (i.e. non-recursive) expansion here because the flow between
   the any-expanded type and the original will handle the any-propagation to any relevant positions,
   some of which may invoke this function when they hit the any propagation functions in the
   recusive call to __flow. *)
  and expand_any _cx any t =
    let only_any _ = any in
    match t with
    | DefT (r, trust, ArrT (ArrayAT _)) -> DefT (r, trust, ArrT (ArrayAT (any, None)))
    | DefT (r, trust, ArrT (TupleAT (_, ts))) ->
      DefT (r, trust, ArrT (TupleAT (any, Base.List.map ~f:only_any ts)))
    | OpaqueT (r, ({ underlying_t; super_t; opaque_type_args; _ } as opaquetype)) ->
      let opaquetype =
        {
          opaquetype with
          underlying_t = Base.Option.(underlying_t >>| only_any);
          super_t = Base.Option.(super_t >>| only_any);
          opaque_type_args =
            Base.List.(opaque_type_args >>| fun (str, r', _, polarity) -> (str, r', any, polarity));
        }
      in
      OpaqueT (r, opaquetype)
    | _ ->
      (* Just returning any would result in infinite recursion in most cases *)
      failwith "no any expansion defined for this case"

  and any_prop_to_function
      use_op
      {
        this_t;
        params;
        rest_param;
        return_t;
        closure_t = _;
        is_predicate = _;
        changeset = _;
        def_reason = _;
      }
      covariant
      contravariant =
    List.iter (snd %> contravariant ~use_op) params;
    Base.Option.iter ~f:(fun (_, _, t) -> contravariant ~use_op t) rest_param;
    contravariant ~use_op this_t;
    covariant ~use_op return_t

  (* types trapped for any propagation. Returns true if this function handles the any case, either
   by propagating or by doing the trivial case. False if the usetype needs to be handled
   separately. *)
  and any_propagated cx trace any u =
    let covariant_flow ~use_op t = rec_flow_t cx trace ~use_op (any, t) in
    let contravariant_flow ~use_op t = rec_flow_t cx trace ~use_op (t, any) in
    match u with
    | NotT (reason, t) ->
      rec_flow_t cx trace ~use_op:unknown_use (AnyT.why (AnyT.source any) reason, OpenT t);
      true
    | SubstOnPredT (use_op, _, _, OpenPredT { base_t = t; m_pos = _; m_neg = _; reason = _ }) ->
      covariant_flow ~use_op t;
      true
    | UseT (use_op, DefT (_, _, ArrT (ROArrayAT t))) (* read-only arrays are covariant *)
    | UseT (use_op, DefT (_, _, ClassT t)) (* mk_instance ~for_type:false *)
    | UseT (use_op, ExactT (_, t))
    | UseT (use_op, OpenPredT { base_t = t; m_pos = _; m_neg = _; reason = _ })
    | UseT (use_op, ShapeT (_, t)) ->
      covariant_flow ~use_op t;
      true
    | UseT (use_op, DefT (_, _, ReactAbstractComponentT { config; instance })) ->
      contravariant_flow ~use_op config;
      covariant_flow ~use_op instance;
      true
    (* Some types just need to be expanded and filled with any types *)
    | UseT (use_op, (DefT (_, _, ArrT (ArrayAT _)) as t))
    | UseT (use_op, (DefT (_, _, ArrT (TupleAT _)) as t))
    | UseT (use_op, (OpaqueT _ as t)) ->
      rec_flow_t cx trace ~use_op (expand_any cx any t, t);
      true
    | UseT (use_op, DefT (_, _, FunT (_, _, funtype))) ->
      (* function type *)
      any_prop_to_function use_op funtype covariant_flow contravariant_flow;
      true
    | ReactKitT (_, _, React.CreateClass (React.CreateClass.PropTypes _, _, _))
    | ReactKitT (_, _, React.SimplifyPropType _) ->
      (* Propagating through here causes exponential blowup. React PropTypes are deprecated
      anyways, so it is not unreasonable to just not trust them *)
      true
    | AdderT _
    | AndT _
    | ArrRestT _
    | BecomeT _
    | BindT _
    | CallT _
    | CallElemT _
    | CallLatentPredT _
    | CallOpenPredT _
    | ChoiceKitUseT _
    | CJSExtractNamedExportsT _
    | CJSRequireT _
    | CondT _
    | ConstructorT _
    | CopyNamedExportsT _
    | CopyTypeExportsT _
    | DestructuringT _
    | ElemT _
    | EnumExhaustiveCheckT _
    | ExportNamedT _
    | ExportTypeT _
    | AssertExportIsTypeT _
    | FunImplicitVoidReturnT _
    | GetElemT _
    | GetKeysT _
    | GetPrivatePropT _
    | GetPropT _
    | GetProtoT _
    | GetStaticsT _
    | GetValuesT _
    | GuardT _
    | IdxUnMaybeifyT _
    | IdxUnwrap _
    | ImportDefaultT _
    | ImportModuleNsT _
    | ImportNamedT _
    | ImportTypeT _
    | ImportTypeofT _
    | IntersectionPreprocessKitT _
    | ResolveUnionT _
    | LookupT _
    | MatchPropT _
    | MakeExactT _
    | MapTypeT _
    | MethodT _
    | MixinT _
    | NullishCoalesceT _
    | ObjKitT _
    | ObjRestT _
    | ObjSealT _
    | ObjTestProtoT _
    | ObjTestT _
    | OptionalChainT _
    | OrT _
    | PredicateT _
    | ReactKitT _
    | RefineT _
    | ReposLowerT _
    | ReposUseT _
    | ResolveSpreadT _
    | SealGenericT _
    | SentinelPropTestT _
    | SetElemT _
    | SetPropT _
    | ModuleExportsAssignT _
    | SpecializeT _
    | SubstOnPredT _
    (* Should be impossible. We only generate these with OpenPredTs. *)
    | TestPropT _
    | ThisSpecializeT _
    | ToStringT _
    | UnaryMinusT _
    | UnifyT _
    | UseT (_, AnnotT _) (* this transforms into a ReposUseT *)
    | UseT (_, MaybeT _) (* used to filter maybe *)
    | UseT (_, MergedT _) (* Already handled in __flow *)
    | UseT (_, OptionalT _) (* used to filter optional *)
    | ObjAssignFromT _
    (* Handled in __flow *)
    | ObjAssignToT _ (* Handled in __flow *)
    | UseT (_, ThisTypeAppT _)
    | UseT (_, DefT (_, _, TypeT _))
    | CreateObjWithComputedPropT _ (* Handled in __flow *)
    (* Should never occur, so we just defer to __flow to handle errors *)
    | UseT (_, InternalT _)
    | UseT (_, MatchingPropT _)
    | UseT (_, DefT (_, _, IdxWrapper _))
    | UseT (_, ModuleT _)
    | ReactPropsToOut _
    | ReactInToProps _
    (* Ideally, any would pollute every member of the union. However, it should be safe to only
     taint the type in the branch that flow picks when generating constraints for this, so
     this can be handled by the pre-existing rules *)
    | UseT (_, UnionT _)
    | UseT (_, IntersectionT _) (* Already handled in the wildcard case in __flow *)
    | UseT (_, OpenT _) ->
      false
    (* These types have no t_out, so can't propagate anything. Thus we short-circuit by returning
     true *)
    | AssertArithmeticOperandT _
    | AssertBinaryInLHST _
    | AssertBinaryInRHST _
    | AssertForInRHST _
    | AssertIterableT _
    | AssertImportIsValueT _
    | ComparatorT _
    | DebugPrintT _
    | DebugSleepT _
    | StrictEqT _
    | EqT _
    | HasOwnPropT _
    | ImplementsT _
    | InvariantT _
    | SetPrivatePropT _
    | SetProtoT _
    | SuperT _
    | TypeAppVarianceCheckT _
    | TypeCastT _
    | EnumCastT _
    | FilterOptionalT _
    | FilterMaybeT _
    | VarianceCheckT _
    | ConcretizeTypeAppsT _
    | ExtendsUseT _
    | UseT (_, KeysT _) (* Any won't interact with the type inside KeysT, so it can't be tainted *)
      ->
      true
    (* TODO: Punt on these for now, but figure out whether these should fall through or not *)
    | UseT (_, CustomFunT (_, ReactElementFactory _))
    | UseT (_, CustomFunT (_, ReactPropType _))
    | UseT (_, CustomFunT (_, ObjectAssign))
    | UseT (_, CustomFunT (_, ObjectGetPrototypeOf))
    | UseT (_, CustomFunT (_, ObjectSetPrototypeOf))
    | UseT (_, CustomFunT (_, Compose _))
    | UseT (_, CustomFunT (_, ReactCreateClass))
    | UseT (_, CustomFunT (_, ReactCreateElement))
    | UseT (_, CustomFunT (_, ReactCloneElement))
    | UseT (_, CustomFunT (_, Idx))
    | UseT (_, CustomFunT (_, TypeAssertIs))
    | UseT (_, CustomFunT (_, TypeAssertThrows))
    | UseT (_, CustomFunT (_, TypeAssertWraps))
    | UseT (_, CustomFunT (_, DebugPrint))
    | UseT (_, CustomFunT (_, DebugThrow))
    | UseT (_, CustomFunT (_, DebugSleep))
    | UseT (_, DefT (_, _, ObjT _))
    | UseT (_, DefT (_, _, InstanceT _))
    | UseT _ ->
      true

  (* Propagates any flows in case of contravariant/invariant subtypes: the any must pollute
   all types in contravariant positions when t <: any. *)
  and any_propagated_use cx trace use_op any l =
    let covariant_flow ~use_op t = rec_flow_t cx trace ~use_op (t, any) in
    let contravariant_flow ~use_op t = rec_flow_t cx trace ~use_op (any, t) in
    match l with
    | DefT (_, _, FunT (_, _, funtype)) ->
      (* function types are contravariant in the arguments *)
      any_prop_to_function use_op funtype covariant_flow contravariant_flow;
      true
    (* Some types just need to be expanded and filled with any types *)
    | (DefT (_, _, ArrT (ArrayAT _)) as t)
    | (DefT (_, _, ArrT (TupleAT _)) as t)
    | (OpaqueT _ as t) ->
      rec_flow_t cx trace ~use_op (t, expand_any cx any t);
      true
    | KeysT _ ->
      (* Keys cannot be tainted by any *)
      true
    | DefT (_, _, ClassT t)
    | DefT (_, _, ArrT (ROArrayAT t))
    | DefT (_, _, TypeT (_, t)) ->
      covariant_flow ~use_op t;
      true
    | DefT (_, _, ReactAbstractComponentT { config; instance }) ->
      contravariant_flow ~use_op config;
      covariant_flow ~use_op instance;
      true
    | GenericT { bound; _ } ->
      covariant_flow ~use_op bound;
      true
    (* These types have no negative positions in their lower bounds *)
    | ExistsT _
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _
    | FunProtoT _
    | ObjProtoT _
    | NullProtoT _ ->
      true
    (* Handled already in __flow *)
    | AnnotT _
    | ExactT _
    | ThisClassT _
    | ReposT _
    | EvalT _
    | MergedT _
    | OpenPredT _
    | InternalT (ReposUpperT _)
    | MatchingPropT _
    | ShapeT _
    | OptionalT _
    | MaybeT _
    | DefT (_, _, PolyT _)
    | TypeAppT _
    | UnionT _
    | IntersectionT _
    | ThisTypeAppT _ ->
      false
    (* Should never occur as the lower bound of any *)
    | BoundT _
    | InternalT (ChoiceKitT _)
    | InternalT (ExtendsT _)
    | ModuleT _ ->
      false
    (* Need special action later *)
    | OpenT _ -> false
    (* TODO: Punt on these for now, but figure out whether these should fall through or not *)
    | CustomFunT (_, ReactElementFactory _)
    | CustomFunT (_, ReactPropType _)
    | CustomFunT (_, ObjectAssign)
    | CustomFunT (_, ObjectGetPrototypeOf)
    | CustomFunT (_, ObjectSetPrototypeOf)
    | CustomFunT (_, Compose _)
    | CustomFunT (_, ReactCreateClass)
    | CustomFunT (_, ReactCreateElement)
    | CustomFunT (_, ReactCloneElement)
    | CustomFunT (_, Idx)
    | CustomFunT (_, TypeAssertIs)
    | CustomFunT (_, TypeAssertThrows)
    | CustomFunT (_, TypeAssertWraps)
    | CustomFunT (_, DebugPrint)
    | CustomFunT (_, DebugThrow)
    | CustomFunT (_, DebugSleep)
    | DefT (_, _, ObjT _)
    | DefT (_, _, InstanceT _)
    | DefT _
    | AnyT _
    | TypeDestructorTriggerT _ ->
      true

  (*********************)
  (* inheritance utils *)
  (*********************)
  and flow_type_args cx trace ~use_op lreason ureason targs1 targs2 =
    List.iter2
      (fun (x, targ_reason, t1, polarity) (_, _, t2, _) ->
        let use_op =
          Frame
            ( TypeArgCompatibility
                { name = x; targ = targ_reason; lower = lreason; upper = ureason; polarity },
              use_op )
        in
        match polarity with
        | Polarity.Negative -> rec_flow cx trace (t2, UseT (use_op, t1))
        | Polarity.Positive -> rec_flow cx trace (t1, UseT (use_op, t2))
        | Polarity.Neutral -> rec_unify cx trace ~use_op t1 t2)
      targs1
      targs2

  (* dispatch checks to verify that lower satisfies the structural
   requirements given in the tuple. *)
  (* TODO: own_props/proto_props is misleading, since they come from interfaces,
   which don't have an own/proto distinction. *)
  and structural_subtype cx trace ~use_op lower reason_struct (own_props_id, proto_props_id, call_id)
      =
    let lreason = reason_of_t lower in
    let own_props = Context.find_props cx own_props_id in
    let proto_props = Context.find_props cx proto_props_id in
    let call_t = Base.Option.map call_id ~f:(Context.find_call cx) in
    own_props
    |> SMap.iter (fun s p ->
           let use_op =
             Frame
               ( PropertyCompatibility { prop = Some s; lower = lreason; upper = reason_struct },
                 use_op )
           in
           match p with
           | Field (_, OptionalT { reason = _; type_ = t; use_desc = _ }, polarity) ->
             let propref =
               let reason_prop =
                 update_desc_reason (fun desc -> ROptional (RPropertyOf (s, desc))) reason_struct
               in
               Named (reason_prop, s)
             in
             rec_flow
               cx
               trace
               ( lower,
                 LookupT
                   {
                     reason = reason_struct;
                     lookup_kind = NonstrictReturning (None, None);
                     ts = [];
                     propref;
                     lookup_action = LookupProp (use_op, Field (None, t, polarity));
                     ids = Some Properties.Set.empty;
                   } )
           | _ ->
             let propref =
               let reason_prop =
                 update_desc_reason (fun desc -> RPropertyOf (s, desc)) reason_struct
               in
               Named (reason_prop, s)
             in
             rec_flow
               cx
               trace
               ( lower,
                 LookupT
                   {
                     reason = reason_struct;
                     lookup_kind = Strict lreason;
                     ts = [];
                     propref;
                     lookup_action = LookupProp (use_op, p);
                     ids = Some Properties.Set.empty;
                   } ));
    proto_props
    |> SMap.iter (fun s p ->
           let use_op =
             Frame
               ( PropertyCompatibility { prop = Some s; lower = lreason; upper = reason_struct },
                 use_op )
           in
           let propref =
             let reason_prop =
               update_desc_reason (fun desc -> RPropertyOf (s, desc)) reason_struct
             in
             Named (reason_prop, s)
           in
           rec_flow
             cx
             trace
             ( lower,
               LookupT
                 {
                   reason = reason_struct;
                   lookup_kind = Strict lreason;
                   ts = [];
                   propref;
                   lookup_action = LookupProp (use_op, p);
                   ids = Some Properties.Set.empty;
                 } ));
    call_t
    |> Base.Option.iter ~f:(fun ut ->
           let prop_name = Some "$call" in
           let use_op =
             Frame
               ( PropertyCompatibility { prop = prop_name; lower = lreason; upper = reason_struct },
                 use_op )
           in
           match lower with
           | DefT (_, _, ObjT { call_t = Some lid; _ })
           | DefT (_, _, InstanceT (_, _, _, { inst_call_t = Some lid; _ })) ->
             let lt = Context.find_call cx lid in
             rec_flow cx trace (lt, UseT (use_op, ut))
           | _ ->
             let reason_prop =
               update_desc_reason (fun desc -> RPropertyOf ("$call", desc)) reason_struct
             in
             let error_message =
               if is_builtin_reason ALoc.source lreason then
                 Error_message.EBuiltinLookupFailed { reason = reason_prop; name = prop_name }
               else
                 Error_message.EStrictLookupFailed
                   {
                     reason_prop;
                     reason_obj = lreason;
                     name = prop_name;
                     use_op = Some use_op;
                     suggestion = None;
                   }
             in
             add_output cx ~trace error_message)

  and check_super cx trace ~use_op lreason ureason t x p =
    let use_op =
      Frame (PropertyCompatibility { prop = Some x; lower = lreason; upper = ureason }, use_op)
    in
    let strict = NonstrictReturning (None, None) in
    let reason_prop = replace_desc_reason (RProperty (Some x)) lreason in
    lookup_prop cx Properties.Set.empty trace t reason_prop lreason strict x (SuperProp (use_op, p))

  and eval_latent_pred cx ?trace reason curr_t p i =
    let evaluated = Context.evaluated cx in
    match Eval.Map.find_opt i evaluated with
    | None ->
      Tvar.mk_no_wrap_where
        cx
        reason
        (with_evaluated_cache cx i evaluated (fun tvar ->
             flow_opt cx ?trace (curr_t, RefineT (reason, p, tvar))))
    | Some it -> it

  and eval_evalt cx ?trace t evaluator id =
    match evaluator with
    | LatentPredT (reason, pred) -> eval_latent_pred cx ?trace reason t pred id
    | TypeDestructorT (use_op, reason, d) ->
      let (_, result) =
        mk_type_destructor
          cx
          ~trace:(Base.Option.value ~default:Trace.dummy_trace trace)
          use_op
          reason
          t
          d
          id
      in
      result

  and eval_selector cx ?trace reason curr_t s tvar id =
    flow_opt
      cx
      ?trace
      ( curr_t,
        match s with
        | Prop (x, has_default) ->
          let lookup_ub () =
            let use_op = unknown_use in
            let action = ReadProp { use_op; obj_t = curr_t; tout = tvar } in
            (* LookupT unifies with the default with tvar. To get around that, we can create some
             * indirection with a fresh tvar in between to ensure that we only add a lower bound
             *)
            let default_tout =
              Tvar.mk_where cx reason (fun tout ->
                  flow_opt cx ?trace (tout, UseT (use_op, OpenT tvar)))
            in
            let void_reason = replace_desc_reason RVoid (fst tvar) in
            let strict =
              NonstrictReturning
                (Some (DefT (void_reason, bogus_trust (), VoidT), default_tout), None)
            in
            LookupT
              {
                reason;
                lookup_kind = strict;
                ts = [];
                propref = Named (reason, x);
                lookup_action = action;
                ids = Some Properties.Set.empty;
              }
          in
          (* We use GetPropT instead of a strict lookup because a strict lookup directly on
           * an unsealed object would cause an error. *)
          let getprop_ub () = GetPropT (unknown_use, reason, Named (reason, x), tvar) in
          if has_default then
            match curr_t with
            | DefT (_, _, NullT) -> getprop_ub ()
            | DefT (_, _, ObjT { flags = { obj_kind; _ }; proto_t = ObjProtoT _; _ })
              when Obj_type.is_legacy_exact_DO_NOT_USE obj_kind ->
              lookup_ub ()
            | _ -> getprop_ub ()
          else
            getprop_ub ()
        | Elem key -> GetElemT (unknown_use, reason, key, tvar)
        | ObjRest xs -> ObjRestT (reason, xs, OpenT tvar, id)
        | ArrRest i -> ArrRestT (unknown_use, reason, i, OpenT tvar)
        | Default -> PredicateT (NotP VoidP, tvar) )

  and mk_type_destructor cx ~trace use_op reason t d id =
    let evaluated = Context.evaluated cx in
    (* As an optimization, unwrap resolved tvars so that they are only evaluated
     * once to an annotation instead of a tvar that gets a bound on both sides. *)
    let t =
      match t with
      | GenericT { reason; name; id = g_id; bound = OpenT (_, id) } ->
        let (_, constraints) = Context.find_constraints cx id in
        (match constraints with
        | Resolved (_, t)
        | FullyResolved (_, t) ->
          GenericT { reason; name; id = g_id; bound = t }
        | Unresolved _ -> t)
      | OpenT (_, id) ->
        let (_, constraints) = Context.find_constraints cx id in
        (match constraints with
        | Resolved (_, t)
        | FullyResolved (_, t) ->
          t
        | Unresolved _ -> t)
      | _ -> t
    in
    let slingshot =
      match drop_generic t with
      | OpenT _
      | MergedT _ ->
        false
      | _ -> true
    in
    let result =
      match Eval.Map.find_opt id evaluated with
      | Some cached_t -> cached_t
      | None ->
        (* The OpenT branch is a correct implementation of type destructors for all
         * types. However, because it adds a constraint to both sides of a type we may
         * end up doing some work twice. So as an optimization for concrete types
         * we have a fall-through branch that only evaluates our type destructor once.
         * The second branch then uses AnnotT to both concretize the result for use
         * as a lower or upper bound and prevent new bounds from being added to
         * the result.
         *
         * MergedT should also get this treatment as it is a merged "description" of
         * an OpenT. *)
        let f tvar =
          match t with
          | OpenT _
          | MergedT _
          | GenericT { bound = OpenT _; _ }
          | GenericT { bound = MergedT _; _ } ->
            let x = TypeDestructorTriggerT (use_op, reason, None, d, tvar) in
            rec_flow_t cx trace ~use_op:unknown_use (t, x);
            rec_flow_t cx trace ~use_op:unknown_use (x, t)
          | GenericT { bound = AnnotT (r, t, use_desc); reason; name; id } ->
            let repos = Some (r, use_desc) in
            let x = TypeDestructorTriggerT (use_op, reason, repos, d, tvar) in
            rec_flow_t cx trace ~use_op:unknown_use (GenericT { reason; name; id; bound = t }, x)
          | AnnotT (r, t, use_desc) ->
            let repos = Some (r, use_desc) in
            let x = TypeDestructorTriggerT (use_op, reason, repos, d, tvar) in
            rec_flow_t cx trace ~use_op:unknown_use (t, x)
          | _ -> eval_destructor cx ~trace use_op reason t d tvar
        in
        Tvar.mk_no_wrap_where cx reason (with_evaluated_cache cx id evaluated f)
    in
    (slingshot, result)

  and eval_destructor cx ~trace use_op reason t d tout =
    let destruct_union ?(f = (fun t -> t)) r rep upper =
      let destructor = TypeDestructorT (use_op, reason, d) in
      let unresolved =
        UnionRep.members rep |> Base.List.map ~f:(fun t -> Cache.Eval.id cx (f t) destructor)
      in
      let (first, unresolved) = (List.hd unresolved, List.tl unresolved) in
      let u =
        ResolveUnionT { reason = r; unresolved; resolved = []; upper; id = Reason.mk_id () }
      in
      rec_flow cx trace (first, u)
    in
    let destruct_maybe ?(f = (fun t -> t)) r t upper =
      let destructor = TypeDestructorT (use_op, reason, d) in
      let reason = replace_desc_new_reason RNullOrVoid r in
      let rep =
        UnionRep.make
          (let null = NullT.make reason |> with_trust bogus_trust |> f in
           Cache.Eval.id cx null destructor)
          (let void = VoidT.make reason |> with_trust bogus_trust |> f in
           Cache.Eval.id cx void destructor)
          [Cache.Eval.id cx (f t) destructor]
      in
      rec_flow cx trace (UnionT (r, rep), upper)
    in
    match t with
    | GenericT { bound = OpaqueT (_, { underlying_t = Some t; _ }); reason = r; id; name }
      when ALoc.source (aloc_of_reason r) = ALoc.source (def_aloc_of_reason r) ->
      eval_destructor cx ~trace use_op reason (GenericT { bound = t; reason = r; id; name }) d tout
    | OpaqueT (r, { underlying_t = Some t; _ })
      when ALoc.source (aloc_of_reason r) = ALoc.source (def_aloc_of_reason r) ->
      eval_destructor cx ~trace use_op reason t d tout
    (* Specialize TypeAppTs before evaluating them so that we can handle special
   cases. Like the union case below. mk_typeapp_instance will return an AnnotT
   which will be fully resolved using the AnnotT case above. *)
    | GenericT { bound = TypeAppT (_, use_op_tapp, c, ts); reason = reason_tapp; id; name } ->
      let destructor = TypeDestructorT (use_op, reason, d) in
      let t =
        mk_typeapp_instance cx ~trace ~use_op:use_op_tapp ~reason_op:reason ~reason_tapp c ts
      in
      rec_flow
        cx
        trace
        ( Cache.Eval.id cx (GenericT { bound = t; name; id; reason = reason_tapp }) destructor,
          UseT (use_op, OpenT tout) )
    | TypeAppT (reason_tapp, use_op_tapp, c, ts) ->
      let destructor = TypeDestructorT (use_op, reason, d) in
      let t =
        mk_typeapp_instance cx ~trace ~use_op:use_op_tapp ~reason_op:reason ~reason_tapp c ts
      in
      rec_flow_t cx trace ~use_op:unknown_use (Cache.Eval.id cx t destructor, OpenT tout)
    (* If we are destructuring a union, evaluating the destructor on the union
   itself may have the effect of splitting the union into separate lower
   bounds, which prevents the speculative match process from working.
   Instead, we preserve the union by pushing down the destructor onto the
   branches of the unions. *)
    | UnionT (r, rep) -> destruct_union r rep (UseT (unknown_use, OpenT tout))
    | GenericT { reason; bound = UnionT (_, rep); id; name } ->
      destruct_union
        ~f:(fun bound -> GenericT { reason = reason_of_t bound; bound; id; name })
        reason
        rep
        (UseT (use_op, OpenT tout))
    | MaybeT (r, t) -> destruct_maybe r t (UseT (unknown_use, OpenT tout))
    | GenericT { reason; bound = MaybeT (_, t); id; name } ->
      destruct_maybe
        ~f:(fun bound -> GenericT { reason = reason_of_t bound; bound; id; name })
        reason
        t
        (UseT (use_op, OpenT tout))
    | AnnotT (r, t, use_desc) ->
      let t = reposition_reason ~trace cx r ~use_desc t in
      let destructor = TypeDestructorT (use_op, reason, d) in
      rec_flow_t cx trace ~use_op:unknown_use (Cache.Eval.id cx t destructor, OpenT tout)
    | GenericT { bound = AnnotT (_, t, use_desc); reason = r; name; id } ->
      let t = reposition_reason ~trace cx r ~use_desc t in
      let destructor = TypeDestructorT (use_op, reason, d) in
      rec_flow_t
        cx
        trace
        ~use_op
        (Cache.Eval.id cx (GenericT { reason = r; id; name; bound = t }) destructor, OpenT tout)
    | _ ->
      rec_flow
        cx
        trace
        ( t,
          match d with
          | NonMaybeType ->
            (* We intentionally use `unknown_use` here! When we flow to a tout we never
             * want to carry a `use_op`. We want whatever `use_op` the tout is used with
             * to win. *)
            FilterMaybeT (unknown_use, OpenT tout)
          | PropertyType x ->
            let reason_op = replace_desc_reason (RProperty (Some x)) reason in
            GetPropT (use_op, reason, Named (reason_op, x), tout)
          | ElementType t -> GetElemT (use_op, reason, t, tout)
          | Bind t -> BindT (use_op, reason, mk_methodcalltype t None [] tout, true)
          | SpreadType (options, todo_rev, head_slice) ->
            Object.(
              Object.Spread.(
                let tool = Resolve Next in
                let state =
                  {
                    todo_rev;
                    acc = Base.Option.value_map ~f:(fun x -> [InlineSlice x]) ~default:[] head_slice;
                    spread_id = Reason.mk_id ();
                    union_reason = None;
                    curr_resolve_idx = 0;
                  }
                in
                ObjKitT (use_op, reason, tool, Spread (options, state), OpenT tout)))
          | RestType (options, t) ->
            Object.(
              Object.Rest.(
                let tool = Resolve Next in
                let state = One t in
                ObjKitT (use_op, reason, tool, Rest (options, state), OpenT tout)))
          | ReadOnlyType -> Object.(ObjKitT (use_op, reason, Resolve Next, ReadOnly, OpenT tout))
          | ValuesType -> GetValuesT (reason, OpenT tout)
          | CallType args ->
            let args = Base.List.map ~f:(fun arg -> Arg arg) args in
            let call = mk_functioncalltype reason None args tout in
            let call = { call with call_strict_arity = false } in
            let use_op =
              match use_op with
              (* The following use ops are for operations that internally delegate to CallType. We
                 don't want to leak the internally delegation to error messages by pushing an
                 additional frame. Alternatively, we could have pushed here and filtered out when
                 rendering error messages, but that seems a bit wasteful. *)
              | Frame (TupleMapFunCompatibility _, _)
              | Frame (ObjMapFunCompatibility _, _)
              | Frame (ObjMapiFunCompatibility _, _) ->
                use_op
              (* For external CallType operations, we push an additional frame to distinguish their
                 error messages from those of "normal" calls. *)
              | _ -> Frame (CallFunCompatibility { n = List.length args }, use_op)
            in
            CallT (use_op, reason, call)
          | TypeMap tmap -> MapTypeT (use_op, reason, tmap, OpenT tout)
          | ReactElementPropsType -> ReactKitT (use_op, reason, React.GetProps (OpenT tout))
          | ReactElementConfigType -> ReactKitT (use_op, reason, React.GetConfig (OpenT tout))
          | ReactElementRefType -> ReactKitT (use_op, reason, React.GetRef (OpenT tout))
          | ReactConfigType default_props ->
            ReactKitT (use_op, reason, React.GetConfigType (default_props, OpenT tout)) )

  and variance_check cx ?trace tparams polarity = function
    | ([], _)
    | (_, []) ->
      (* ignore typeapp arity mismatch, since it's handled elsewhere *)
      ()
    | (tp :: tps, t :: ts) ->
      CheckPolarity.check_polarity cx ?trace tparams (Polarity.mult (polarity, tp.polarity)) t;
      variance_check cx ?trace tparams polarity (tps, ts)

  (* Instantiate a polymorphic definition given tparam instantiations in a Call or
   * New expression. *)
  and instantiate_poly_call_or_new
      cx trace ~use_op ~reason_op ~reason_tapp ?cache ?errs_ref (tparams_loc, xs, t) targs =
    let (_, ts) =
      Nel.fold_left
        (fun (targs, ts) typeparam ->
          match targs with
          | [] -> ([], ts)
          | ExplicitArg t :: targs -> (targs, t :: ts)
          | ImplicitArg (r, id) :: targs ->
            let reason = mk_reason RImplicitInstantiation (aloc_of_reason r) in
            let t = ImplicitTypeArgument.mk_targ cx typeparam reason reason_tapp in
            rec_flow_t cx trace ~use_op (t, OpenT (r, id));
            (targs, t :: ts))
        (targs, [])
        xs
    in
    instantiate_poly_with_targs
      cx
      trace
      ~use_op
      ~reason_op
      ~reason_tapp
      ?cache
      ?errs_ref
      (tparams_loc, xs, t)
      (List.rev ts)

  (* Instantiate a polymorphic definition given type arguments. *)
  and instantiate_poly_with_targs
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
          rec_flow_t cx trace ~use_op:frame (t_, subst cx ~use_op map typeparam.bound);
          (SMap.add typeparam.name t_ map, ts))
        (SMap.empty, ts)
        xs
    in
    reposition cx ~trace (aloc_of_reason reason_tapp) (subst cx ~use_op map t)

  (* Given a type parameter, a supplied type argument for specializing it, and a
   reason for specialization, either return the type argument or, when directed,
   look up the instantiation cache for an existing type argument for the same
   purpose and unify it with the supplied type argument. *)
  and cache_instantiate cx trace ~use_op ?cache typeparam reason_op reason_tapp t =
    match cache with
    | None -> t
    | Some rs ->
      (match desc_of_reason reason_tapp with
      (* This reason description cannot be trusted for caching purposes. *)
      | RTypeAppImplicit _ -> t
      | _ ->
        let t_ = Cache.PolyInstantiation.find cx reason_tapp typeparam (reason_op, rs) in
        rec_unify cx trace ~use_op ~unify_any:true t t_;
        t_)

  (* Instantiate a polymorphic definition with stated bound or 'any' for args *)
  (* Needed only for `instanceof` refis and React.PropTypes.instanceOf types *)
  and instantiate_poly_default_args cx trace ~use_op ~reason_op ~reason_tapp (tparams_loc, xs, t) =
    (* Remember: other_bound might refer to other type params *)
    let (ts, _) =
      Nel.fold_left
        (fun (ts, map) typeparam ->
          let t = Unsoundness.why InstanceOfRefinement reason_op in
          (t :: ts, SMap.add typeparam.name t map))
        ([], SMap.empty)
        xs
    in
    let ts = List.rev ts in
    instantiate_poly_with_targs cx trace ~use_op ~reason_op ~reason_tapp (tparams_loc, xs, t) ts

  (* Instantiate a polymorphic definition by creating fresh type arguments. *)
  and instantiate_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache (tparams_loc, xs, t) =
    let ts =
      xs
      |> Nel.map (fun typeparam -> ImplicitTypeArgument.mk_targ cx typeparam reason_op reason_tapp)
    in
    instantiate_poly_with_targs
      cx
      trace
      ~use_op
      ~reason_op
      ~reason_tapp
      ?cache
      (tparams_loc, xs, t)
      (Nel.to_list ts)

  (* instantiate each param of a polymorphic type with its upper bound *)
  and instantiate_poly_param_upper_bounds cx typeparams =
    let (_, revlist) =
      Nel.fold_left
        (fun (map, list) { name; bound; _ } ->
          let t = subst cx map bound in
          (SMap.add name t map, t :: list))
        (SMap.empty, [])
        typeparams
    in
    List.rev revlist

  and mk_poly_arity_reason tparams_loc =
    mk_reason (RCustom "See type parameters of definition here") tparams_loc

  (* Fix a this-abstracted instance type by tying a "knot": assume that the
   fixpoint is some `this`, substitute it as This in the instance type, and
   finally unify it with the instance type. Return the class type wrapping the
   instance type. *)
  and fix_this_class cx trace reason (r, i, is_this) =
    let i' =
      match Cache.Fix.find cx reason i with
      | Some i' -> i'
      | None ->
        let this = Tvar.mk cx reason in
        let this_generic =
          if is_this && not (Context.generate_tests cx) then
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
        Cache.Fix.add cx reason i i';
        rec_unify cx trace ~use_op:unknown_use this i';
        i'
    in
    DefT (r, bogus_trust (), ClassT i')

  and is_type = function
    | DefT (_, _, ClassT _)
    | DefT (_, _, EnumObjectT _)
    | ThisClassT (_, _, _)
    | DefT (_, _, TypeT _)
    | AnyT _ ->
      true
    | DefT (_, _, PolyT { t_out = t'; _ }) -> is_type t'
    | _ -> false

  and canonicalize_imported_type cx trace reason t =
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
    | ThisClassT (r, i, this) -> Some (fix_this_class cx trace reason (r, i, this))
    | DefT (enum_reason, trust, EnumObjectT enum) ->
      let enum_type = mk_enum_type ~loc:(aloc_of_reason enum_reason) ~trust enum in
      Some (DefT (reason, trust, TypeT (ImportEnumKind, enum_type)))
    | DefT (_, _, TypeT _) -> Some t
    | AnyT _ -> Some t
    | _ -> None

  (* Specialize This in a class. Eventually this causes substitution. *)
  and instantiate_this_class cx trace reason tc this k =
    rec_flow cx trace (tc, ThisSpecializeT (reason, this, k))

  (* Specialize targs in a class. This is somewhat different from
   mk_typeapp_instance, in that it returns the specialized class type, not the
   specialized instance type. *)
  and specialize_class cx trace ~reason_op ~reason_tapp c = function
    | None -> c
    | Some ts ->
      Tvar.mk_where cx reason_tapp (fun tout ->
          rec_flow
            cx
            trace
            (c, SpecializeT (unknown_use, reason_op, reason_tapp, None, Some ts, tout)))

  (* Object assignment patterns. In the `Object.assign` model (chain_objects), an
   existing object receives properties from other objects. This pattern suffers
   from "races" in the type checker, since the object supposed to receive
   properties is available even when the other objects supplying the properties
   are not yet available. *)
  and chain_objects cx ?trace reason this those =
    let result =
      List.fold_left
        (fun result that ->
          let (that, kind) =
            match that with
            | Arg t -> (t, default_obj_assign_kind)
            | SpreadArg t ->
              (* If someone does Object.assign({}, ...Array<obj>) we can treat it like
           Object.assign({}, obj). *)
              (t, ObjSpreadAssign)
          in
          Tvar.mk_where cx reason (fun t ->
              flow_opt
                cx
                ?trace
                (result, ObjAssignToT (Op (ObjectChain { op = reason }), reason, that, t, kind))))
        this
        those
    in
    reposition cx ?trace (aloc_of_reason reason) result

  (*********)
  (* enums *)
  (*********)
  and enum_proto cx trace ~reason (enum_reason, trust, enum) =
    let enum_t = DefT (enum_reason, trust, EnumT enum) in
    let { representation_t; _ } = enum in
    get_builtin_typeapp cx ~trace reason "$EnumProto" [enum_t; representation_t]

  and enum_exhaustive_check
      cx
      ~trace
      ~check_reason
      ~enum_reason
      ~enum
      ~possible_checks
      ~checks
      ~default_case
      ~incomplete_out
      ~discriminant_after_check =
    match possible_checks with
    (* No possible checks left to resolve, analyze the exhaustive check. *)
    | [] ->
      let { members; has_unknown_members; _ } = enum in
      let check_member (members_remaining, seen) (EnumCheck { reason; member_name }) =
        if not @@ SMap.mem member_name members_remaining then
          add_output
            cx
            ~trace
            (Error_message.EEnumMemberAlreadyChecked
               { reason; prev_check_reason = SMap.find member_name seen; enum_reason; member_name });
        (SMap.remove member_name members_remaining, SMap.add member_name reason seen)
      in
      let (left_over, _) = List.fold_left check_member (members, SMap.empty) checks in
      (match (SMap.is_empty left_over, default_case, has_unknown_members) with
      | (false, None, _) ->
        add_output
          cx
          ~trace
          (Error_message.EEnumNotAllChecked
             { reason = check_reason; enum_reason; left_to_check = SMap.keys left_over });
        enum_exhaustive_check_incomplete cx ~trace ~reason:check_reason incomplete_out
      (* When we have unknown members, a default is required even when we've checked all known members. *)
      | (true, None, true) ->
        add_output
          cx
          ~trace
          (Error_message.EEnumUnknownNotChecked { reason = check_reason; enum_reason });
        enum_exhaustive_check_incomplete cx ~trace ~reason:check_reason incomplete_out
      | (true, Some _, true) -> ()
      | (true, Some default_case_reason, false) ->
        add_output
          cx
          ~trace
          (Error_message.EEnumAllMembersAlreadyChecked { reason = default_case_reason; enum_reason })
      | _ -> ())
    (* There are still possible checks to resolve, continue to resolve them. *)
    | (obj_t, check) :: rest_possible_checks ->
      let exhaustive_check =
        EnumExhaustiveCheckT
          {
            reason = check_reason;
            check =
              EnumExhaustiveCheckPossiblyValid
                {
                  tool =
                    EnumResolveCaseTest
                      { discriminant_enum = enum; discriminant_reason = enum_reason; check };
                  possible_checks = rest_possible_checks;
                  checks;
                  default_case;
                };
            incomplete_out;
            discriminant_after_check;
          }
      in
      rec_flow cx trace (obj_t, exhaustive_check)

  and enum_exhaustive_check_incomplete
      cx ~trace ~reason ?(trigger = VoidT.why reason |> with_trust bogus_trust) incomplete_out =
    rec_flow_t cx trace ~use_op:unknown_use (trigger, incomplete_out)
    (*******************************************************)
    (* Entry points into the process of trying different   *)
    (* branches of union and intersection types.           *)
    (*******************************************************)

    (* The problem we're trying to solve here is common to checking unions and
   intersections: how do we make a choice between alternatives, when (i) we have
   only partial information (i.e., while we're in the middle of type inference)
   and when (ii) we want to avoid regret (i.e., by not committing to an
   alternative that might not work out, when alternatives that were not
   considered could have worked out)?

   To appreciate the problem, consider what happens without choice. Partial
   information is not a problem: we emit constraints that must be satisfied for
   something to work, and either those constraints fail (indicating a problem)
   or they don't fail (indicating no problem). With choice and partial
   information, we cannot naively emit constraints as we try alternatives
   *without also having a mechanism to roll back those constraints*. This is
   because those constraints don't *have* to be satisfied; some other
   alternative may end up not needing those constraints to be satisfied for
   things to work out!

   It is not too hard to imagine scary scenarios we can get into without a
   roll-back mechanism. (These scenarios are not theoretical, by the way: with a
   previous implementation of union and intersection types that didn't
   anticipate these scenarios, they consistently caused a lot of problems in
   real-world use cases.)

   * One bad state we can get into is where, when trying an alternative, we emit
   constraints hoping they would be satisfied, and they appear to work. So we
   commit to that particular alternative. Then much later find out that those
   constraints are unsatified, at which point we have lost the ability to try
   other alternatives that could have worked. This leads to a class of bugs
   where a union or intersection type contains cases that should have worked,
   but they don't.

   * An even worse state we can get into is where we do discover that an
   alternative won't work out while we're still in a position of choosing
   another alternative, but in the process of making that discovery we emit
   constraints that linger on in a ghost-like state. Meanwhile, we pick another
   alternative, it works out, and we move on. Except that much later the ghost
   constraints become unsatisfied, leading to much confusion on the source of
   the resulting errors. This leads to a class of bugs where we get spurious
   errors even when a union or intersection type seems to have worked.

   So, we just implement roll-back, right? Basically...yes. But rolling back
   constraints is really hard in the current implementation. Instead, we try to
   avoid processing constraints that have side effects as much as possible while
   trying alternatives: by ensuring that (1) we don't (need to) emit too many
   constraints that have side effects (2) those that we do emit get deferred,
   instead of being processed immediately, until a choice can be made, thereby
   not participating in the choice-making process.

   (1) How do we ensure we don't emit too many constraints that have side
   effects? By fully resolving types before they participate in the
   choice-making process. Basically, we want to have as much information as we
   can before trying alternatives. It is a nice property of our implementation
   that once types are resolved, constraints emitted against them don't have
   (serious) side effects: they get simplified and simplified until we either
   hit success or failure. The details of this process is described in
   ResolvableTypeJob and in resolve_bindings.

   (2) But not all types can be fully resolved. In particular, while union and
   intersection types themselves can be fully resolved, the lower and upper
   bounds we check them against could have still-to-be-inferred types in
   them. How do we ensure that for the potentially side-effectful constraints we
   do emit on these types, we avoid undue side effects? By explicitly marking
   these types as unresolved, and deferring the execution of constraints that
   involved such marked types until a choice can be made. The details of this
   process is described in Speculation.

   There is a necessary trade-off in the approach. In particular, (2) means that
   sometimes choices cannot be made: it is ambiguous which constraints should be
   executed when trying different alternatives. We detect such ambiguities
   (conservatively, but only when a best-effort choice-making strategy doesn't
   work), and ask for additional annotations to disambiguate the relevant
   alternatives. A particularly nice property of this approach is that it is
   complete: with enough annotations it is always possible to make a
   choice. Another "meta-feature" of this approach is that it leaves room for
   incremental improvement: e.g., we would need fewer additional annotations as
   we improve our inference algorithm to detect cases where more unresolved
   tvars can be fully resolved ahead of time (in other words, detect when they
   have the "0->1" property, discussed elsewhere, roughly meaning they are
   determined by annotations).
*)

  (** Every choice-making process on a union or intersection type is assigned a
    unique identifier, called the speculation_id. This identifier keeps track of
    unresolved tvars encountered when trying to fully resolve types. **)

  and try_union cx trace use_op l reason rep =
    let ts = UnionRep.members rep in
    let speculation_id = mk_id () in
    Speculation.init_speculation cx speculation_id;

    (* collect parts of the union type to be fully resolved *)
    let imap =
      (* since any final optimization must have happened after full resolution *)
      if UnionRep.is_optimized_finally rep then
        IMap.empty
      else
        ResolvableTypeJob.collect_of_types cx IMap.empty ts
    in
    (* collect parts of the lower bound to be fully resolved, while logging
     unresolved tvars *)
    let imap = ResolvableTypeJob.collect_of_type ~log_unresolved:speculation_id cx imap l in
    (* fully resolve the collected types *)
    resolve_bindings_init cx trace reason (bindings_of_jobs cx trace imap)
    @@ (* ...and then begin the choice-making process *)
    try_flow_continuation cx trace reason speculation_id (UnionCases (use_op, l, rep, ts))

  and try_intersection cx trace u reason rep =
    let ts = InterRep.members rep in
    let speculation_id = mk_id () in
    Speculation.init_speculation cx speculation_id;

    (* collect parts of the intersection type to be fully resolved *)
    let imap = ResolvableTypeJob.collect_of_types cx IMap.empty ts in
    (* collect parts of the upper bound to be fully resolved, while logging
     unresolved tvars *)
    let imap = ResolvableTypeJob.collect_of_use ~log_unresolved:speculation_id cx imap u in
    (* fully resolve the collected types *)
    resolve_bindings_init cx trace reason (bindings_of_jobs cx trace imap)
    @@ (* ...and then begin the choice-making process *)
    try_flow_continuation cx trace reason speculation_id (IntersectionCases (ts, u))
    (* Preprocessing for intersection types.

   Before feeding into the choice-making machinery described above, we
   preprocess upper bounds of intersection types. This preprocessing seems
   asymmetric, but paradoxically, it is not: the purpose of the preprocessing is
   to bring choice-making on intersections to parity with choice-making on
   unions.

   Consider what happens when a lower bound is checked against a union type. The
   lower bound is always concretized before a choice is made! In other words,
   even if we emit a flow from an unresolved tvar to a union type, the
   constraint fires only when the unresolved tvar has been concretized.

   Now, consider checking an intersection type with an upper bound. As an
   artifact of how tvars and concrete types are processed, the upper bound would
   appear to be concrete even though the actual parts of the upper bound that
   are involved in the choice-making may be unresolved! (These parts are the
   top-level input positions in the upper bound, which end up choosing between
   the top-level input positions in the members of the intersection type.) If we
   did not concretize the parts of the upper bound involved in choice-making, we
   would start the choice-making process at a disadvantage (compared to
   choice-making with a union type and an already concretized lower
   bound). Thus, we do an extra preprocessing step where we collect the parts of
   the upper bound to be concretized, and for each combination of concrete types
   for those parts, call the choice-making process.
*)

  (** The following function concretizes each tvar in unresolved in turn,
    recording their corresponding concrete lower bounds in resolved as it
    goes. At each step, it emits a ConcretizeTypes constraint on an unresolved
    tvar, which in turn calls into this function when a concrete lower bound
    appears on that tvar. **)
  and prep_try_intersection cx trace reason unresolved resolved u r rep =
    match unresolved with
    | [] -> try_intersection cx trace (replace_parts cx resolved u) r rep
    | tvar :: unresolved ->
      rec_flow
        cx
        trace
        ( tvar,
          intersection_preprocess_kit
            reason
            (ConcretizeTypes (unresolved, resolved, IntersectionT (r, rep), u)) )

  (* some patterns need to be concretized before proceeding further *)
  and patt_that_needs_concretization = function
    | OpenT _
    | UnionT _
    | MaybeT _
    | OptionalT _
    | AnnotT _ ->
      true
    | _ -> false

  (* for now, we only care about concretizating parts of functions and calls *)
  and parts_to_replace cx = function
    | UseT (_, DefT (_, _, ObjT { call_t = Some id; _ })) ->
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
    | UseT (_, DefT (_, _, FunT (_, _, ft))) ->
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
  and replace_parts =
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

  (************************)
  (* Full type resolution *)
  (************************)

  (* Here we continue where we left off at ResolvableTypeJob. Once we have
   collected a set of type resolution jobs, we create so-called bindings from
   these jobs. A binding is a (id, tvar) pair, where tvar is what needs to be
   resolved, and id is an identifier that serves as an index for that job.

   We don't try to fully resolve unresolved tvars that are not annotation
   sources or heads of type applications, since in general they don't satify the
   0->1 property. Instead:

   (1) When we're expecting them, e.g., when we're looking at inferred types, we
   mark them so that we can recognize them later, during speculative matching.

   (2) When we're not expecting them, e.g., when we're fully resolving union /
   intersection type annotations, we unify them as `any`. Ideally we wouldn't be
   worrying about this case, but who knows what cruft we might have accumulated
   on annotation types, so just getting that cruft out of the way.

   These decisions were made in ResolvableTypeJob.collect_of_types and are
   reflected in the use (or not) of OpenUnresolved (see below).
*)
  and bindings_of_jobs cx trace jobs =
    IMap.fold
      ResolvableTypeJob.(
        fun id job bindings ->
          match job with
          | OpenResolved -> bindings
          | Binding tvar -> (id, tvar) :: bindings
          | OpenUnresolved (log_unresolved, reason, id) ->
            begin
              match log_unresolved with
              | Some speculation_id ->
                Speculation.add_unresolved_to_speculation cx speculation_id id
              | None ->
                Unsoundness.unresolved_any reason |> resolve_id cx trace ~use_op:unknown_use id
            end;
            bindings)
      jobs
      []

  (* Entry point into full type resolution. Create an identifier for the goal
   tvar, and call the general full type resolution function below. *)
  and resolve_bindings_init cx trace reason bindings done_tvar =
    let id = create_goal cx done_tvar in
    resolve_bindings cx trace reason id bindings

  and create_goal cx tvar =
    let i = mk_id () in
    Graph_explorer.node (Context.type_graph cx) i;
    Context.set_goals cx (IMap.add i tvar (Context.goals cx));
    i

  (* Let id be the identifier associated with a tvar that is not yet
   resolved. (Here, resolved/unresolved refer to the state of the tvar in the
   context graph: does it point to Resolved _ or Unresolved _?) As soon as the
   tvar is resolved to some type, we generate some bindings by walking that
   type. Full type resolution at id now depends on full resolution of the
   ids/tvars in those bindings. The following function ensures that those
   dependencies are recorded and processed.

   Dependency management happens in Graph_explorer, using efficient data
   structures discussed therein. All we need to do here is to connect id to
   bindings in that graph, while taking care that (1) the conditions of adding
   edges to the graph are satisfied, and (2) cleaning up the effects of adding
   those edges to the graph. Finally (3) we request full type resolution of the
   bindings themselves.

   For (1), note that the graph only retains transitively closed dependencies
   from one kind of tvars to another kind of tvars. The former kind includes
   tvars that are resolved but not yet fully resolved. The latter kind includes
   tvars that are not yet resolved. Thus, in particular we must filter out
   bindings that correspond to fully resolved tvars (see
   is_unfinished_target). On the other hand, the fully_resolve_type function
   below already ensures that id is not yet fully resolved (via
   is_unexplored_source).

   For (2), after adding edges we might discover that some tvars are now fully
   resolved: this happens when, e.g., no new transitively closed dependencies
   get added on id, and full type resolution of some tvars depended only on id.
   If any of these fully resolved tvars were goal tvars, we trigger them.

   For (3) we emit a ResolveType constraint for each binding; when the
   corresponding tvar is resolved, the function fully_resolve_type below is
   called, which in turn calls back into this function (thus closing the
   recursive loop).
*)
  and resolve_bindings cx trace reason id bindings =
    let bindings = filter_bindings cx bindings in
    let fully_resolve_ids = connect_id_to_bindings cx id bindings in
    ISet.iter
      (fun id ->
        match IMap.find_opt id (Context.goals cx) with
        | None -> ()
        | Some tvar -> trigger cx trace reason tvar)
      fully_resolve_ids;
    List.iter (resolve_binding cx trace reason) bindings

  and fully_resolve_type cx trace reason id t =
    if is_unexplored_source cx id then
      let imap = ResolvableTypeJob.collect_of_type cx IMap.empty t in
      let bindings = bindings_of_jobs cx trace imap in
      (* NOTE: bindings_of_jobs might change the state of id because it resolves it, so check
       again. TODO: there must be a better way *)
      if is_unexplored_source cx id then resolve_bindings cx trace reason id bindings

  and filter_bindings cx = List.filter (fun (id, _) -> is_unfinished_target cx id)

  and connect_id_to_bindings cx id bindings =
    let (ids, _) = List.split bindings in
    Graph_explorer.edges (Context.type_graph cx) (id, ids)

  (* Sanity conditions on source and target before adding edges to the
   graph. Nodes are in one of three states, described in Graph_explorer:
   Not_found (corresponding to unresolved tvars), Found _ (corresponding to
   resolved but not yet fully resolved tvars), and Finished (corresponding to
   fully resolved tvars). *)
  and is_unexplored_source cx id =
    match Graph_explorer.stat_graph id (Context.type_graph cx) with
    | Graph_explorer.Finished -> false
    | Graph_explorer.Node_not_found -> false
    | Graph_explorer.Found node -> Graph_explorer.is_unexplored_node node

  and is_unfinished_target cx id =
    let type_graph = Context.type_graph cx in
    match Graph_explorer.stat_graph id type_graph with
    | Graph_explorer.Finished -> false
    | Graph_explorer.Node_not_found ->
      Graph_explorer.node type_graph id;
      true
    | Graph_explorer.Found node -> not (Graph_explorer.is_finished_node node)

  (** utils for creating toolkit types **)

  and choice_kit reason k = InternalT (ChoiceKitT (reason, k))

  and choice_kit_use reason k = ChoiceKitUseT (reason, k)

  and intersection_preprocess_kit reason k = IntersectionPreprocessKitT (reason, k)

  (** utils for emitting toolkit constraints **)

  and trigger cx trace reason done_tvar =
    rec_flow cx trace (choice_kit reason Trigger, UseT (unknown_use, done_tvar))

  and try_flow_continuation cx trace reason speculation_id spec =
    tvar_with_constraint cx ~trace (choice_kit_use reason (TryFlow (speculation_id, spec)))

  and resolve_binding cx trace reason (id, tvar) =
    rec_flow cx trace (OpenT tvar, choice_kit_use reason (FullyResolveType id))

  (************************)
  (* Speculative matching *)
  (************************)

  (* Speculatively match a pair of types, returning whether some error was
   encountered or not. Speculative matching happens in the context of a
   particular "branch": this context controls how some constraints emitted
   during the matching might be processed. See comments in Speculation for
   details on branches. See also speculative_matches, which calls this function
   iteratively and processes its results. *)
  and speculative_match cx trace branch l u =
    let typeapp_stack = TypeAppExpansion.get () in
    let constraint_cache_ref = Context.constraint_cache cx in
    let constraint_cache = !constraint_cache_ref in
    Speculation.set_speculative cx branch;
    let restore () =
      Speculation.restore_speculative cx;
      constraint_cache_ref := constraint_cache;
      TypeAppExpansion.set typeapp_stack
    in
    try
      rec_flow cx trace (l, u);
      restore ();
      None
    with
    | SpeculativeError err ->
      restore ();
      Some err
    | exn ->
      restore ();
      raise exn

  (* Speculatively match several alternatives in turn, as presented when checking
   a union or intersection type. This process maintains a so-called "match
   state" that describes the best possible choice found so far, and can
   terminate in various ways:

   (1) One of the alternatives definitely succeeds. This is straightforward: we
   can safely discard any later alternatives.

   (2) All alternatives fail. This is also straightforward: we emit an
   appropriate error message.

   (3) One of the alternatives looks promising (i.e., it doesn't immediately
   fail, but it doesn't immediately succeed either: some potentially
   side-effectful constraints, called actions, were emitted while trying the
   alternative, whose execution has been deferred), and all the later
   alternatives fail. In this scenario, we pick the promising alternative, and
   then fire the deferred actions. This is fine, because the choice cannot cause
   regret: the chosen alternative was the only one that had any chance of
   succeeding.

   (4) Multiple alternatives look promising, but the set of deferred actions
   emitted while trying the first of those alternatives form a subset of those
   emitted by later trials. Here we pick the first promising alternative (and
   fire the deferred actions). The reason this is fine is similar to (3): once
   again, the choice cannot cause any regret, because if it failed, then the
   later alternatives would have failed too. So the chosen alternative had the
   best chance of succeeding.

   (5) But sometimes, multiple alternatives look promising and we really can't
   decide which is best. This happens when the set of deferred actions emitted
   by them are incomparable, or later trials have more chances of succeeding
   than previous trials. Such scenarios typically point to real ambiguities, and
   so we ask for additional annotations on unresolved tvars to disambiguate.

   See Speculation for more details on terminology and low-level mechanisms used
   here, including what bits of information are carried by match_state and case,
   how actions are deferred and diff'd, etc.

   Because this process is common to checking union and intersection types, we
   abstract the latter into a so-called "spec." The spec is used to customize
   error messages and to ignore unresolved tvars that are deemed irrelevant to
   choice-making.
*)
  and speculative_matches cx trace r speculation_id spec =
    (* explore optimization opportunities *)
    if optimize_spec_try_shortcut cx trace r spec then
      ()
    else
      long_path_speculative_matches cx trace r speculation_id spec

  and long_path_speculative_matches cx trace r speculation_id spec =
    let open Speculation_state in
    (* extract stuff to ignore while considering actions *)
    let ignore = ignore_of_spec spec in
    (* split spec into a list of pairs of types to try speculative matching on *)
    let trials = trials_of_spec spec in
    (* Here match_state can take on various values:
     * (a) (NoMatch errs) indicates that everything has failed up to this point,
     *   with errors recorded in errs. Note that the initial value of acc is
     *   Some (NoMatch []).
     * (b) (ConditionalMatch case) indicates the a promising alternative has
     *    been found, but not chosen yet.
     *)
    let rec loop match_state = function
      | [] -> return match_state
      | (case_id, case_r, l, u) :: trials ->
        let case = { case_id; unresolved = ISet.empty; actions = [] } in
        (* speculatively match the pair of types in this trial *)
        let error = speculative_match cx trace { ignore; speculation_id; case } l u in
        (match error with
        | None ->
          (* no error, looking great so far... *)
          begin
            match match_state with
            | NoMatch _ ->
              (* everything had failed up to this point. so no ambiguity yet... *)
              if
                ISet.is_empty case.unresolved
                (* ...and no unresolved tvars encountered during the speculative
                 * match! This is great news. It means that this alternative will
                 * definitely succeed. Fire any deferred actions and short-cut. *)
              then
                fire_actions cx trace spec case.actions
              (* Otherwise, record that we've found a promising alternative. *)
              else
                loop (ConditionalMatch case) trials
            | ConditionalMatch prev_case ->
              (* umm, there's another previously found promising alternative *)
              (* so compute the difference in side effects between that alternative
               * and this *)
              let ts = Speculation.case_diff cx prev_case case in
              (* if the side effects of the previously found promising alternative
               * are fewer, then keep holding on to that alternative *)
              if ts = [] then
                loop match_state trials
              (* otherwise, we have an ambiguity; blame the unresolved tvars and
               * short-cut *)
              else
                let prev_case_id = prev_case.case_id in
                let cases : Type.t list = choices_of_spec spec in
                blame_unresolved cx trace prev_case_id case_id cases case_r ts
          end
        | Some err ->
          (* if an error is found, then throw away this alternative... *)
          begin
            match match_state with
            | NoMatch errs ->
              (* ...adding to the error list if no promising alternative has been
               * found yet *)
              loop (NoMatch (err :: errs)) trials
            | _ -> loop match_state trials
          end)
    and return = function
      | ConditionalMatch case ->
        (* best choice that survived, congrats! fire deferred actions  *)
        fire_actions cx trace spec case.actions
      | NoMatch msgs ->
        (* everything failed; make a really detailed error message listing out the
         * error found for each alternative *)
        let ts = choices_of_spec spec in
        assert (List.length ts = List.length msgs);
        let branches =
          Base.List.mapi
            ~f:(fun i msg ->
              let reason = reason_of_t (List.nth ts i) in
              (reason, msg))
            msgs
        in
        (* Add the error. *)
        begin
          match spec with
          | UnionCases (use_op, l, _rep, us) ->
            let reason = reason_of_t l in
            let reason_op = mk_union_reason r us in
            add_output
              cx
              ~trace
              (Error_message.EUnionSpeculationFailed { use_op; reason; reason_op; branches })
          | IntersectionCases (ls, upper) ->
            let err =
              let reason_lower = mk_intersection_reason r ls in
              match upper with
              | UseT (use_op, t) ->
                Error_message.EIncompatibleDefs
                  { use_op; reason_lower; reason_upper = reason_of_t t; branches }
              | _ ->
                Error_message.EIncompatible
                  {
                    use_op = use_op_of_use_t upper;
                    lower = (reason_lower, Some Error_message.Incompatible_intersection);
                    upper = (reason_of_use_t upper, error_message_kind_of_upper upper);
                    branches;
                  }
            in
            add_output cx ~trace err
        end
    in
    loop (NoMatch []) trials

  (* Make an informative error message that points out the ambiguity, and where
   additional annotations can help disambiguate. Recall that an ambiguity
   arises precisely when:

   (1) one alternative looks promising, but has some chance of failing

   (2) a later alternative also looks promising, and has some chance of not
   failing even if the first alternative fails

   ...with the caveat that "looks promising" and "some chance of failing" are
   euphemisms for some pretty conservative approximations made by Flow when it
   encounters potentially side-effectful constraints involving unresolved tvars
   during a trial.
*)
  and blame_unresolved cx trace prev_i i cases case_r tvars =
    let rs = tvars |> Base.List.map ~f:(fun (_, r) -> r) |> List.sort compare in
    let prev_case = reason_of_t (List.nth cases prev_i) in
    let case = reason_of_t (List.nth cases i) in
    add_output
      cx
      ~trace
      (Error_message.ESpeculationAmbiguous
         { reason = case_r; prev_case = (prev_i, prev_case); case = (i, case); cases = rs })

  and trials_of_spec = function
    | UnionCases (use_op, l, _rep, us) ->
      (* NB: Even though we know the use_op for the original constraint, don't
       embed it in the nested constraints to avoid unnecessary verbosity. We
       will unwrap the original use_op once in EUnionSpeculationFailed. *)
      Base.List.mapi ~f:(fun i u -> (i, reason_of_t l, l, UseT (Op (Speculation use_op), u))) us
    | IntersectionCases (ls, u) ->
      Base.List.mapi
        ~f:(fun i l ->
          (i, reason_of_use_t u, l, mod_use_op_of_use_t (fun use_op -> Op (Speculation use_op)) u))
        ls

  and choices_of_spec = function
    | UnionCases (_, _, _, ts)
    | IntersectionCases (ts, _) ->
      ts

  and ignore_of_spec = function
    | IntersectionCases (_, CallT (_, _, { call_tout = (_, id); _ })) -> Some id
    | IntersectionCases (_, GetPropT (_, _, _, (_, id))) -> Some id
    | _ -> None

  (* spec optimization *)
  (* Currently, the only optimizations we do are for enums and for disjoint unions.

   When a literal type is checked against a union of literal types, we hope the union is an enum and
   try to optimize the representation of the union as such. We also try to use our optimization to
   do a quick membership check, potentially avoiding the speculative matching process altogether.

   When an object type is checked against an union of object types, we hope the union is a disjoint
   union and try to guess and record sentinel properties across object types in the union. Later,
   during speculative matching, by checking sentinel properties first we force immediate match
   failures in the vast majority of cases without having to do any useless additional work.
*)
  and optimize_spec_try_shortcut cx trace reason_op = function
    | UnionCases (use_op, l, rep, _ts) ->
      if not (UnionRep.is_optimized_finally rep) then
        UnionRep.optimize
          rep
          ~reasonless_eq:TypeUtil.reasonless_eq
          ~flatten:(Type_mapper.union_flatten cx)
          ~find_resolved:(Context.find_resolved cx)
          ~find_props:(Context.find_props cx);
      begin
        match l with
        | DefT
            ( _,
              _,
              ( StrT (Literal _)
              | NumT (Literal _)
              | BoolT (Some _)
              | SingletonStrT _ | SingletonNumT _ | SingletonBoolT _ | VoidT | NullT ) ) ->
          shortcut_enum cx trace reason_op use_op l rep
        | DefT (_, _, ObjT _)
        | ExactT (_, DefT (_, _, ObjT _)) ->
          shortcut_disjoint_union cx trace reason_op use_op l rep
        | _ -> false
      end
    | IntersectionCases _ -> false

  and shortcut_enum cx trace reason_op use_op l rep =
    let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
    quick_mem_result cx trace reason_op use_op l rep @@ UnionRep.quick_mem_enum ~quick_subtype l rep

  and shortcut_disjoint_union cx trace reason_op use_op l rep =
    let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
    quick_mem_result cx trace reason_op use_op l rep
    @@ UnionRep.quick_mem_disjoint_union
         ~quick_subtype
         l
         rep
         ~find_resolved:(Context.find_resolved cx)
         ~find_props:(Context.find_props cx)

  and is_union_resolvable = function
    | EvalT _
    | KeysT _ ->
      true
    | _ -> false

  and resolve_union cx trace reason id resolved unresolved l upper =
    let continue resolved =
      match unresolved with
      | [] -> rec_flow cx trace (union_of_ts reason resolved, upper)
      | next :: rest ->
        rec_flow cx trace (next, ResolveUnionT { reason; resolved; unresolved = rest; upper; id })
    in
    match l with
    | DefT (_, _, EmptyT _) -> continue resolved
    | _ ->
      let reason_elemt = reason_of_t l in
      let pos = Base.List.length resolved in
      (* Union resolution can fall prey to the same sort of infinite recursion that array spreads can, so
      we can use the same constant folding guard logic that arrays do. To more fully understand how that works,
      see the comment there. *)
      ConstFoldExpansion.guard id (reason_elemt, pos) (function
          | 0 -> continue (l :: resolved)
          (* Unions are idempotent, so we can just skip any duplicated elements *)
          | 1 -> continue resolved
          | _ -> ())

  and quick_mem_result cx trace reason_op use_op l rep = function
    | UnionRep.Yes ->
      (* membership check succeeded *)
      true
    (* Our work here is done, so no need to continue. *)
    | UnionRep.No ->
      (* membership check failed *)
      let r = UnionRep.specialized_reason ~reason_of_t:TypeUtil.reason_of_t reason_op rep in
      rec_flow cx trace (l, UseT (use_op, DefT (r, bogus_trust (), EmptyT Bottom)));
      true
    (* Our work here is done, so no need to continue. *)
    | UnionRep.Conditional t ->
      (* conditional match *)
      rec_flow cx trace (l, UseT (use_op, t));
      true (* Our work here is done, so no need to continue. *)
    | UnionRep.Unknown ->
      (* membership check was inconclusive *)
      false

  (* Continue to speculative matching. *)

  (* When we fire_actions we also need to reconstruct the use_op for each action
   * since before beginning speculation we replaced each use_op with
   * an UnknownUse. *)
  and fire_actions cx trace spec =
    List.iter (function
        | (_, Speculation_state.FlowAction (l, u)) ->
          (match spec with
          | IntersectionCases (_, u') ->
            let use_op = use_op_of_use_t u' in
            (match use_op with
            | None -> rec_flow cx trace (l, u)
            | Some use_op ->
              rec_flow cx trace (l, mod_use_op_of_use_t (replace_speculation_root_use_op use_op) u))
          | UnionCases (use_op, _, _, _) ->
            rec_flow cx trace (l, mod_use_op_of_use_t (replace_speculation_root_use_op use_op) u))
        | (_, Speculation_state.UnifyAction (use_op, t1, t2)) ->
          (match spec with
          | IntersectionCases (_, u') ->
            let use_op' = use_op_of_use_t u' in
            (match use_op' with
            | None -> rec_unify cx trace t1 t2 ~use_op
            | Some use_op' ->
              rec_unify cx trace t1 t2 ~use_op:(replace_speculation_root_use_op use_op' use_op))
          | UnionCases (use_op', _, _, _) ->
            rec_unify cx trace t1 t2 ~use_op:(replace_speculation_root_use_op use_op' use_op))
        | (_, Speculation_state.ErrorAction msg) -> add_output cx ~trace msg
        | (_, Speculation_state.UnsealedObjectProperty (flds, s, up)) ->
          Context.set_prop cx flds s up)

  and speculative_object_write cx flds s up =
    let action = Speculation_state.UnsealedObjectProperty (flds, s, up) in
    if not (Speculation.defer_action cx action) then Context.set_prop cx flds s up

  and mk_union_reason r us =
    List.fold_left
      (fun reason t ->
        let rdesc = string_of_desc (desc_of_reason ~unwrap:false reason) in
        let tdesc = string_of_desc (desc_of_reason ~unwrap:false (reason_of_t t)) in
        let udesc =
          if not (String_utils.string_starts_with rdesc "union:") then
            spf "union: %s" tdesc
          else if String_utils.string_ends_with rdesc "..." then
            rdesc
          else if String_utils.string_ends_with rdesc (tdesc ^ "(s)") then
            rdesc
          else if String.length rdesc >= 256 then
            spf "%s | ..." rdesc
          else if String_utils.string_ends_with rdesc tdesc then
            spf "%s(s)" rdesc
          else
            spf "%s | %s" rdesc tdesc
        in
        replace_desc_reason (RCustom udesc) reason)
      r
      us

  and mk_intersection_reason r _ls =
    replace_desc_reason RIntersection r
    (* property lookup functions in objects and instances *)

  (**
 * Determines whether a property name should be considered "munged"/private when
 * the `munge_underscores` config option is set.
 *)
  and is_munged_prop_name cx name =
    is_munged_prop_name_with_munge
      name
      ~should_munge_underscores:(Context.should_munge_underscores cx)

  and is_munged_prop_name_with_munge name ~should_munge_underscores =
    Signature_utils.is_munged_property_name name && should_munge_underscores

  and prop_typo_suggestion cx ids =
    Base.List.(ids >>| Context.find_real_props cx >>= SMap.keys |> typo_suggestion)

  and lookup_prop cx previously_seen_props trace l reason_prop reason_op strict x action =
    let l =
      (* munge names beginning with single _ *)
      if is_munged_prop_name cx x then
        ObjProtoT (reason_of_t l)
      else
        l
    in
    let propref = Named (reason_prop, x) in
    rec_flow
      cx
      trace
      ( l,
        LookupT
          {
            reason = reason_op;
            lookup_kind = strict;
            ts = [];
            propref;
            lookup_action = action;
            ids = Some previously_seen_props;
          } )

  and access_prop cx previously_seen_props trace reason_prop reason_op strict super x pmap action =
    match SMap.find_opt x pmap with
    | Some p ->
      perform_lookup_action
        cx
        trace
        (Named (reason_prop, x))
        p
        PropertyMapProperty
        reason_prop
        reason_op
        action
    | None -> lookup_prop cx previously_seen_props trace super reason_prop reason_op strict x action

  and get_prop
      cx previously_seen_props trace ~use_op reason_prop reason_op strict l super x map tout =
    ReadProp { use_op; obj_t = l; tout }
    |> access_prop cx previously_seen_props trace reason_prop reason_op strict super x map

  and match_prop
      cx previously_seen_props trace ~use_op reason_prop reason_op strict super x pmap prop_t =
    MatchProp (use_op, prop_t)
    |> access_prop cx previously_seen_props trace reason_prop reason_op strict super x pmap

  and set_prop
      cx
      previously_seen_props
      ?(wr_ctx = Normal)
      ~mode
      trace
      ~use_op
      reason_prop
      reason_op
      strict
      l
      super
      x
      pmap
      tin
      prop_tout =
    let action = WriteProp { use_op; obj_t = l; prop_tout; tin; write_ctx = wr_ctx; mode } in
    access_prop cx previously_seen_props trace reason_prop reason_op strict super x pmap action

  and get_obj_prop cx trace o propref reason_op =
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
      rec_flow_t cx trace ~use_op:unknown_use (string_key x reason_op, key);
      Some (Field (None, value, dict_polarity), IndexerProperty)
    | (Computed k, None, Some { key; value; dict_polarity; _ }) ->
      rec_flow_t cx trace ~use_op:unknown_use (k, key);
      Some (Field (None, value, dict_polarity), IndexerProperty)
    | _ -> None

  and read_obj_prop cx trace ~use_op o propref reason_obj reason_op tout =
    let l = DefT (reason_obj, bogus_trust (), ObjT o) in
    match get_obj_prop cx trace o propref reason_op with
    | Some (p, target_kind) ->
      let action = ReadProp { use_op; obj_t = l; tout } in
      perform_lookup_action cx trace propref p target_kind reason_obj reason_op action
    | None ->
      (match propref with
      | Named _ ->
        let strict =
          if Obj_type.sealed_in_op reason_op o.flags.obj_kind then
            Strict reason_obj
          else
            ShadowRead (None, Nel.one o.props_tmap)
        in
        rec_flow
          cx
          trace
          ( o.proto_t,
            LookupT
              {
                reason = reason_op;
                lookup_kind = strict;
                ts = [];
                propref;
                lookup_action = ReadProp { use_op; obj_t = l; tout };
                ids = Some (Properties.Set.singleton o.props_tmap);
              } )
      | Computed elem_t ->
        (match elem_t with
        | OpenT _ ->
          let loc = loc_of_t elem_t in
          add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedOpen))
        | GenericT { bound = DefT (_, _, StrT (Literal _)); _ }
        | DefT (_, _, StrT (Literal _)) ->
          let loc = loc_of_t elem_t in
          add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedLiteral))
        | AnyT _ -> rec_flow_t cx trace ~use_op:unknown_use (AnyT.untyped reason_op, OpenT tout)
        | GenericT { bound = DefT (_, _, StrT _); _ }
        | GenericT { bound = DefT (_, _, NumT _); _ }
        | DefT (_, _, StrT _)
        | DefT (_, _, NumT _) ->
          (* string, and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
          rec_flow_t
            cx
            trace
            ~use_op:unknown_use
            (Unsoundness.why ComputedNonLiteralKey reason_op, OpenT tout)
        | _ ->
          let reason_prop = reason_of_t elem_t in
          add_output
            cx
            ~trace
            (Error_message.EObjectComputedPropertyAccess (reason_op, reason_prop))))

  and elem_action_on_obj cx trace ~use_op ?on_named_prop l obj reason_op action =
    let propref =
      match l with
      | GenericT { bound = DefT (_, _, StrT (Literal (_, x))); reason = reason_x; _ }
      | DefT (reason_x, _, StrT (Literal (_, x))) ->
        let reason_named = replace_desc_reason (RStringLit x) reason_x in
        Base.Option.iter ~f:(fun f -> f reason_named) on_named_prop;
        let reason_prop = replace_desc_reason (RProperty (Some x)) reason_x in
        Named (reason_prop, x)
      | _ -> Computed l
    in
    match action with
    | ReadElem t -> rec_flow cx trace (obj, GetPropT (use_op, reason_op, propref, t))
    | WriteElem (tin, tout, mode) ->
      rec_flow cx trace (obj, SetPropT (use_op, reason_op, propref, mode, Normal, tin, None));
      Base.Option.iter ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (obj, t)) tout
    | CallElem (reason_call, ft) ->
      rec_flow cx trace (obj, MethodT (use_op, reason_call, reason_op, propref, ft, None))

  and writelike_obj_prop cx trace ~use_op o propref reason_obj reason_op prop_t action =
    match get_obj_prop cx trace o propref reason_op with
    | Some (p, target_kind) ->
      perform_lookup_action cx trace propref p target_kind reason_obj reason_op action
    | None ->
      (match propref with
      | Named (reason_prop, prop) ->
        if Obj_type.is_exact_or_sealed reason_op o.flags.obj_kind then
          add_output
            cx
            ~trace
            (Error_message.EPropNotFound
               {
                 prop_name = Some prop;
                 reason_prop;
                 reason_obj;
                 use_op;
                 suggestion = prop_typo_suggestion cx [o.props_tmap] prop;
               })
        else
          let sealed = Obj_type.sealed_in_op reason_op o.flags.obj_kind in
          let strict =
            if sealed then
              Strict reason_obj
            else
              ShadowWrite (Nel.one o.props_tmap)
          in
          rec_flow
            cx
            trace
            ( o.proto_t,
              LookupT
                {
                  reason = reason_op;
                  lookup_kind = strict;
                  ts = [];
                  propref;
                  lookup_action = action;
                  ids = Some (Properties.Set.singleton o.props_tmap);
                } )
      | Computed elem_t ->
        (match elem_t with
        | OpenT _ ->
          let loc = loc_of_t elem_t in
          add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedOpen))
        | GenericT { bound = DefT (_, _, StrT (Literal _)); _ }
        | DefT (_, _, StrT (Literal _)) ->
          let loc = loc_of_t elem_t in
          add_output cx ~trace Error_message.(EInternal (loc, PropRefComputedLiteral))
        | AnyT _ -> rec_flow_t cx trace ~use_op:unknown_use (prop_t, AnyT.untyped reason_op)
        | GenericT { bound = DefT (_, _, StrT _); _ }
        | GenericT { bound = DefT (_, _, NumT _); _ }
        | DefT (_, _, StrT _)
        | DefT (_, _, NumT _) ->
          (* string and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
          rec_flow_t
            cx
            trace
            ~use_op:unknown_use
            (prop_t, Unsoundness.why ComputedNonLiteralKey reason_op)
        | _ ->
          let reason_prop = reason_of_t elem_t in
          add_output
            cx
            ~trace
            (Error_message.EObjectComputedPropertyAssign (reason_op, reason_prop))))

  and match_obj_prop cx trace ~use_op o propref reason_obj reason_op prop_t =
    MatchProp (use_op, prop_t)
    |> writelike_obj_prop cx trace ~use_op o propref reason_obj reason_op prop_t

  and write_obj_prop cx trace ~use_op ~mode o propref reason_obj reason_op tin prop_tout =
    let obj_t = DefT (reason_obj, bogus_trust (), ObjT o) in
    let action = WriteProp { use_op; obj_t; prop_tout; tin; write_ctx = Normal; mode } in
    writelike_obj_prop cx trace ~use_op o propref reason_obj reason_op tin action

  and match_shape cx trace ~use_op proto reason props =
    let reason_op = reason_of_t proto in
    SMap.iter
      (fun x p ->
        let reason_prop = update_desc_reason (fun desc -> RPropertyOf (x, desc)) reason in
        match Property.read_t p with
        | Some t ->
          let use_op =
            Frame
              (PropertyCompatibility { prop = Some x; upper = reason; lower = reason_op }, use_op)
          in
          let propref = Named (reason_prop, x) in
          let t = filter_optional cx ~trace reason_prop t in
          rec_flow cx trace (proto, MatchPropT (use_op, reason_op, propref, (reason_prop, t)))
        | None ->
          add_output
            cx
            ~trace
            (Error_message.EPropNotReadable { reason_prop; prop_name = Some x; use_op }))
      props

  and find_or_intro_shadow_prop cx trace reason_op x prop_loc =
    let intro_shadow_prop id =
      let reason_prop = replace_desc_reason (RShadowProperty x) reason_op in
      let t = Tvar.mk cx reason_prop in
      let p = Field (Some prop_loc, t, Polarity.Neutral) in
      Context.set_prop cx id (internal_name x) p;
      (t, p)
    in
    (* Given some shadow property type and a prototype chain (o.proto,
     * o.proto.proto, ...), link all types along the prototype chain together.
     * If there is a write to the prototype later on, we unify the property types
     * together. If there is no write, the property types are safely independent.
     *)
    let rec chain_link t = function
      | [] -> ()
      | id :: ids ->
        let t_proto = Property.assert_field (find (id, ids)) in
        rec_flow cx trace (t_proto, UnifyT (t_proto, t))
    (* Check at each step to see if a prop was added since we looked.
     *
     * Imports and builtins are merged in after local inference, potentially
     * deferring multiple shadow reads/writes on a tvar. If this shadow read
     * follow a deferred shadow write, a property will exist. If it follows a
     * deferred shadow read, a shadow property will exist. In either case, we
     * don't need to create a shadow property, nor do we need to continue
     * unifying up the proto chain, as the work is necessarily already done.
     *)
    and find (id, proto_ids) =
      match Context.get_prop cx id x with
      | Some p -> p
      | None ->
        (match Context.get_prop cx id (internal_name x) with
        | Some p -> p
        | None ->
          let (t, p) = intro_shadow_prop id in
          chain_link t proto_ids;
          p)
    in
    find

  (* filter out undefined from a type *)
  and filter_optional cx ?trace reason opt_t =
    let tvar = Tvar.mk_no_wrap cx reason in
    flow_opt cx ?trace (opt_t, FilterOptionalT (unknown_use, OpenT (reason, tvar)));
    tvar

  and update_sketchy_null cx opt_loc t =
    ExistsCheck.(
      match t with
      (* Ignore AnyTs for sketchy null checks; otherwise they'd always trigger the lint. *)
      | AnyT _ -> ()
      | UnionT (_, rep) ->
        UnionRep.members rep |> Base.List.iter ~f:(update_sketchy_null cx opt_loc)
      | _ ->
        (match opt_loc with
        | None -> ()
        | Some loc ->
          let t_loc =
            let reason = reason_of_t t in
            match annot_aloc_of_reason reason with
            | Some loc -> Some loc
            | None -> Some (def_aloc_of_reason reason)
          in
          let exists_checks = Context.exists_checks cx in
          let exists_check =
            ALocMap.find_opt loc exists_checks |> Base.Option.value ~default:ExistsCheck.empty
          in
          let exists_check =
            match Type_filter.maybe t with
            | DefT (_, _, EmptyT _) -> exists_check
            | _ -> { exists_check with null_loc = t_loc }
          in
          let exists_check =
            match t |> Type_filter.not_exists |> Type_filter.not_maybe with
            | DefT (_, _, BoolT _) -> { exists_check with bool_loc = t_loc }
            | DefT (_, _, StrT _) -> { exists_check with string_loc = t_loc }
            | DefT (_, _, NumT _) -> { exists_check with number_loc = t_loc }
            | DefT (_, _, MixedT _) -> { exists_check with mixed_loc = t_loc }
            | DefT (_, _, EnumT { representation_t = DefT (_, _, BoolT _); _ }) ->
              { exists_check with enum_bool_loc = t_loc }
            | DefT (_, _, EnumT { representation_t = DefT (_, _, StrT _); _ }) ->
              { exists_check with enum_string_loc = t_loc }
            | DefT (_, _, EnumT { representation_t = DefT (_, _, NumT _); _ }) ->
              { exists_check with enum_number_loc = t_loc }
            | _ -> exists_check
          in
          let exists_checks =
            if exists_check = ExistsCheck.empty then
              exists_checks
            else
              ALocMap.add loc exists_check exists_checks
          in
          Context.set_exists_checks cx exists_checks))

  (**********)
  (* guards *)
  (**********)
  and guard cx trace source pred result sink =
    match pred with
    | ExistsP loc ->
      update_sketchy_null cx loc source;
      begin
        match Type_filter.exists source with
        | DefT (_, _, EmptyT _) -> ()
        | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
      end
    | NotP (ExistsP loc) ->
      update_sketchy_null cx loc source;
      begin
        match Type_filter.not_exists source with
        | DefT (_, _, EmptyT _) -> ()
        | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
      end
    | MaybeP ->
      begin
        match Type_filter.maybe source with
        | DefT (_, _, EmptyT _) -> ()
        | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
      end
    | NotP MaybeP ->
      begin
        match Type_filter.not_maybe source with
        | DefT (_, _, EmptyT _) -> ()
        | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
      end
    | NotP (NotP p) -> guard cx trace source p result sink
    | _ ->
      let loc = aloc_of_reason (fst sink) in
      let pred_str = string_of_predicate pred in
      add_output cx ~trace Error_message.(EInternal (loc, UnsupportedGuardPredicate pred_str))

  (**************)
  (* predicates *)
  (**************)

  (* t - predicate output recipient (normally a tvar)
   l - incoming concrete LB (predicate input)
   result - guard result in case of success
   p - predicate *)
  and predicate cx trace t l p =
    match p with
    (************************)
    (* deconstruction of && *)
    (************************)
    | AndP (p1, p2) ->
      let reason = replace_desc_reason RAnd (fst t) in
      let tvar = (reason, Tvar.mk_no_wrap cx reason) in
      rec_flow cx trace (l, PredicateT (p1, tvar));
      rec_flow cx trace (OpenT tvar, PredicateT (p2, t))
    (************************)
    (* deconstruction of || *)
    (************************)
    | OrP (p1, p2) ->
      rec_flow cx trace (l, PredicateT (p1, t));
      rec_flow cx trace (l, PredicateT (p2, t))
    (*********************************)
    (* deconstruction of binary test *)
    (*********************************)

    (* when left is evaluated, store it and evaluate right *)
    | LeftP (b, r) -> rec_flow cx trace (r, PredicateT (RightP (b, l), t))
    | NotP (LeftP (b, r)) -> rec_flow cx trace (r, PredicateT (NotP (RightP (b, l)), t))
    (* when right is evaluated, call appropriate handler *)
    | RightP (b, actual_l) ->
      let r = l in
      let l = actual_l in
      binary_predicate cx trace true b l r t
    | NotP (RightP (b, actual_l)) ->
      let r = l in
      let l = actual_l in
      binary_predicate cx trace false b l r t
    (***********************)
    (* typeof _ ~ "boolean" *)
    (***********************)
    | BoolP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.boolean loc l, OpenT t)
    | NotP (BoolP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_boolean l, OpenT t)
    (***********************)
    (* typeof _ ~ "string" *)
    (***********************)
    | StrP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.string loc l, OpenT t)
    | NotP (StrP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_string l, OpenT t)
    (***********************)
    (* typeof _ ~ "symbol" *)
    (***********************)
    | SymbolP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.symbol loc l, OpenT t)
    | NotP (SymbolP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_symbol l, OpenT t)
    (*********************)
    (* _ ~ "some string" *)
    (*********************)
    | SingletonStrP (expected_loc, sense, lit) ->
      let filtered_str = Type_filter.string_literal expected_loc sense lit l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_str, OpenT t)
    | NotP (SingletonStrP (_, _, lit)) ->
      let filtered_str = Type_filter.not_string_literal lit l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_str, OpenT t)
    (*********************)
    (* _ ~ some number n *)
    (*********************)
    | SingletonNumP (expected_loc, sense, lit) ->
      let filtered_num = Type_filter.number_literal expected_loc sense lit l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_num, OpenT t)
    | NotP (SingletonNumP (_, _, lit)) ->
      let filtered_num = Type_filter.not_number_literal lit l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered_num, OpenT t)
    (***********************)
    (* typeof _ ~ "number" *)
    (***********************)
    | NumP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.number loc l, OpenT t)
    | NotP (NumP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_number l, OpenT t)
    (***********************)
    (* typeof _ ~ "function" *)
    (***********************)
    | FunP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.function_ l, OpenT t)
    | NotP FunP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_function l, OpenT t)
    (***********************)
    (* typeof _ ~ "object" *)
    (***********************)
    | ObjP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.object_ cx l, OpenT t)
    | NotP ObjP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_object l, OpenT t)
    (*******************)
    (* Array.isArray _ *)
    (*******************)
    | ArrP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.array l, OpenT t)
    | NotP ArrP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_array l, OpenT t)
    (***********************)
    (* typeof _ ~ "undefined" *)
    (***********************)
    | VoidP ->
      let filtered = Type_filter.undefined l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP VoidP ->
      let filtered = Type_filter.not_undefined l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (********)
    (* null *)
    (********)
    | NullP ->
      let filtered = Type_filter.null l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP NullP ->
      let filtered = Type_filter.not_null l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (*********)
    (* maybe *)
    (*********)
    | MaybeP ->
      let filtered = Type_filter.maybe l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP MaybeP ->
      let filtered = Type_filter.not_maybe l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (********)
    (* true *)
    (********)
    | SingletonBoolP (_, true) ->
      let filtered = Type_filter.true_ l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP (SingletonBoolP (_, true)) ->
      let filtered = Type_filter.not_true l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (*********)
    (* false *)
    (*********)
    | SingletonBoolP (_, false) ->
      let filtered = Type_filter.false_ l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP (SingletonBoolP (_, false)) ->
      let filtered = Type_filter.not_false l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    (************************)
    (* truthyness *)
    (************************)
    | ExistsP loc ->
      update_sketchy_null cx loc l;
      let filtered = Type_filter.exists l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | NotP (ExistsP loc) ->
      update_sketchy_null cx loc l;
      let filtered = Type_filter.not_exists l in
      rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
    | PropExistsP (key, r) -> prop_exists_test cx trace key r true l t
    | NotP (PropExistsP (key, r)) -> prop_exists_test cx trace key r false l t
    | PropNonMaybeP (key, r) -> prop_non_maybe_test cx trace key r true l t
    | NotP (PropNonMaybeP (key, r)) -> prop_non_maybe_test cx trace key r false l t
    (* classical logic i guess *)
    | NotP (NotP p) -> predicate cx trace t l p
    | NotP (AndP (p1, p2)) -> predicate cx trace t l (OrP (NotP p1, NotP p2))
    | NotP (OrP (p1, p2)) -> predicate cx trace t l (AndP (NotP p1, NotP p2))
    (********************)
    (* Latent predicate *)
    (********************)
    | LatentP (fun_t, idx) ->
      let reason = update_desc_reason (fun desc -> RPredicateCall desc) (reason_of_t fun_t) in
      rec_flow cx trace (fun_t, CallLatentPredT (reason, true, idx, l, t))
    | NotP (LatentP (fun_t, idx)) ->
      let neg_reason =
        update_desc_reason (fun desc -> RPredicateCallNeg desc) (reason_of_t fun_t)
      in
      rec_flow cx trace (fun_t, CallLatentPredT (neg_reason, false, idx, l, t))

  and prop_exists_test cx trace key reason sense obj result =
    prop_exists_test_generic
      key
      reason
      cx
      trace
      result
      obj
      sense
      (ExistsP None, NotP (ExistsP None))
      obj

  and prop_non_maybe_test cx trace key reason sense obj result =
    prop_exists_test_generic key reason cx trace result obj sense (NotP MaybeP, MaybeP) obj

  and prop_exists_test_generic key reason cx trace result orig_obj sense (pred, not_pred) = function
    | DefT (_, _, ObjT { flags; props_tmap; _ }) as obj ->
      (match Context.get_prop cx props_tmap key with
      | Some p ->
        (match Property.read_t p with
        | Some t ->
          (* prop is present on object type *)
          let pred =
            if sense then
              pred
            else
              not_pred
          in
          rec_flow cx trace (t, GuardT (pred, orig_obj, result))
        | None ->
          (* prop cannot be read *)
          add_output
            cx
            ~trace
            (Error_message.EPropNotReadable
               { reason_prop = reason; prop_name = Some key; use_op = unknown_use }))
      | None when Obj_type.is_exact_or_sealed (fst result) flags.obj_kind ->
        (* prop is absent from exact object type *)
        if sense then
          ()
        else
          rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
      | None ->
        (* prop is absent from inexact object type *)
        (* TODO: possibly unsound to filter out orig_obj here, but if we don't,
         case elimination based on prop existence checking doesn't work for
         (disjoint unions of) intersections of objects, where the prop appears
         in a different branch of the intersection. It is easy to avoid this
         unsoundness with slightly more work, but will wait until a
         refactoring of property lookup lands to revisit. Tracked by
         #11301092. *)
        if orig_obj = obj then rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result))
    | IntersectionT (_, rep) ->
      (* For an intersection of object types, try the test for each object type in
       turn, while recording the original intersection so that we end up with
       the right refinement. See the comment on the implementation of
       IntersectionPreprocessKit for more details. *)
      let reason = fst result in
      InterRep.members rep
      |> List.iter (fun obj ->
             rec_flow
               cx
               trace
               ( obj,
                 intersection_preprocess_kit
                   reason
                   (PropExistsTest (sense, key, reason, orig_obj, result, (pred, not_pred))) ))
    | _ -> rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)

  and binary_predicate cx trace sense test left right result =
    let handler =
      match test with
      | InstanceofTest -> instanceof_test
      | SentinelProp key -> sentinel_prop_test key
    in
    handler cx trace result (sense, left, right)

  and instanceof_test cx trace result = function
    (* instanceof on an ArrT is a special case since we treat ArrT as its own
      type, rather than an InstanceT of the Array builtin class. So, we resolve
      the ArrT to an InstanceT of Array, and redo the instanceof check. We do
      it at this stage instead of simply converting (ArrT, InstanceofP c)
      to (InstanceT(Array), InstanceofP c) because this allows c to be resolved
      first. *)
    | ( true,
        (DefT (reason, _, ArrT arrtype) as arr),
        DefT (r, _, ClassT (DefT (_, _, InstanceT _) as a)) ) ->
      let elemt = elemt_of_arrtype arrtype in
      let right = extends_type r arr a in
      let arrt = get_builtin_typeapp cx ~trace reason "Array" [elemt] in
      rec_flow cx trace (arrt, PredicateT (LeftP (InstanceofTest, right), result))
    | ( false,
        (DefT (reason, _, ArrT arrtype) as arr),
        DefT (r, _, ClassT (DefT (_, _, InstanceT _) as a)) ) ->
      let elemt = elemt_of_arrtype arrtype in
      let right = extends_type r arr a in
      let arrt = get_builtin_typeapp cx ~trace reason "Array" [elemt] in
      let pred = NotP (LeftP (InstanceofTest, right)) in
      rec_flow cx trace (arrt, PredicateT (pred, result))
    (* An object is considered `instanceof` a function F when it is constructed
      by F. Note that this is incomplete with respect to the runtime semantics,
      where instanceof is transitive: if F.prototype `instanceof` G, then the
      object is `instanceof` G. There is nothing fundamentally difficult in
      modeling the complete semantics, but we haven't found a need to do it. **)
    | (true, (DefT (_, _, ObjT { proto_t = proto2; _ }) as obj), DefT (_, _, FunT (_, proto1, _)))
      when proto1 = proto2 ->
      rec_flow_t cx trace ~use_op:unknown_use (obj, OpenT result)
    (* Suppose that we have an instance x of class C, and we check whether x is
      `instanceof` class A. To decide what the appropriate refinement for x
      should be, we need to decide whether C extends A, choosing either C or A
      based on the result. Thus, we generate a constraint to decide whether C
      extends A (while remembering C), which may recursively generate further
      constraints to decide super(C) extends A, and so on, until we hit the root
      class. (As a technical tool, we use Extends(_, _) to perform this
      recursion; it is also used elsewhere for running similar recursive
      subclass decisions.) **)
    | (true, (DefT (_, _, InstanceT _) as c), DefT (r, _, ClassT (DefT (_, _, InstanceT _) as a)))
      ->
      predicate cx trace result (extends_type r c a) (RightP (InstanceofTest, c))
    (* If C is a subclass of A, then don't refine the type of x. Otherwise,
      refine the type of x to A. (In general, the type of x should be refined to
      C & A, but that's hard to compute.) **)
    | ( true,
        DefT (reason, _, InstanceT (_, super_c, _, instance_c)),
        (InternalT (ExtendsT (_, c, DefT (_, _, InstanceT (_, _, _, instance_a)))) as right) ) ->
      (* TODO: intersection *)
      if ALoc.equal_id instance_a.class_id instance_c.class_id then
        rec_flow_t cx trace ~use_op:unknown_use (c, OpenT result)
      else
        (* Recursively check whether super(C) extends A, with enough context. **)
        let pred = LeftP (InstanceofTest, right) in
        let u = PredicateT (pred, result) in
        rec_flow cx trace (super_c, ReposLowerT (reason, false, u))
    (* If we are checking `instanceof Object` or `instanceof Function`, objects
      with `ObjProtoT` or `FunProtoT` should pass. *)
    | (true, ObjProtoT reason, (InternalT (ExtendsT _) as right)) ->
      let obj_proto = get_builtin_type cx ~trace reason ~use_desc:true "Object" in
      rec_flow cx trace (obj_proto, PredicateT (LeftP (InstanceofTest, right), result))
    | (true, FunProtoT reason, (InternalT (ExtendsT _) as right)) ->
      let fun_proto = get_builtin_type cx ~trace reason ~use_desc:true "Function" in
      rec_flow cx trace (fun_proto, PredicateT (LeftP (InstanceofTest, right), result))
    (* We hit the root class, so C is not a subclass of A **)
    | (true, DefT (_, _, NullT), InternalT (ExtendsT (r, _, a))) ->
      rec_flow_t
        cx
        trace
        ~use_op:unknown_use
        (reposition cx ~trace (aloc_of_reason r) a, OpenT result)
    (* If we're refining mixed with instanceof A, then flow A to the result *)
    | ( true,
        DefT (_, _, MixedT _),
        DefT (class_reason, _, ClassT (DefT (instance_reason, _, InstanceT _) as a)) ) ->
      let desc = desc_of_reason instance_reason in
      let loc = aloc_of_reason class_reason in
      rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace ~desc loc a, OpenT result)
    (* Prune the type when any other `instanceof` check succeeds (since this is
      impossible). *)
    | (true, _, _) -> ()
    | (false, DefT (_, _, ObjT { proto_t = proto2; _ }), DefT (_, _, FunT (_, proto1, _)))
      when proto1 = proto2 ->
      ()
    (* Like above, now suppose that we have an instance x of class C, and we
      check whether x is _not_ `instanceof` class A. To decide what the
      appropriate refinement for x should be, we need to decide whether C
      extends A, choosing either nothing or C based on the result. **)
    | (false, (DefT (_, _, InstanceT _) as c), DefT (r, _, ClassT (DefT (_, _, InstanceT _) as a)))
      ->
      predicate cx trace result (extends_type r c a) (NotP (RightP (InstanceofTest, c)))
    (* If C is a subclass of A, then do nothing, since this check cannot
      succeed. Otherwise, don't refine the type of x. **)
    | ( false,
        DefT (reason, _, InstanceT (_, super_c, _, instance_c)),
        (InternalT (ExtendsT (_, _, DefT (_, _, InstanceT (_, _, _, instance_a)))) as right) ) ->
      if ALoc.equal_id instance_a.class_id instance_c.class_id then
        ()
      else
        let u = PredicateT (NotP (LeftP (InstanceofTest, right)), result) in
        rec_flow cx trace (super_c, ReposLowerT (reason, false, u))
    | (false, ObjProtoT _, InternalT (ExtendsT (r, c, _))) ->
      (* We hit the root class, so C is not a subclass of A **)
      rec_flow_t
        cx
        trace
        ~use_op:unknown_use
        (reposition cx ~trace (aloc_of_reason r) c, OpenT result)
    (* Don't refine the type when any other `instanceof` check fails. **)
    | (false, left, _) -> rec_flow_t cx trace ~use_op:unknown_use (left, OpenT result)

  and sentinel_prop_test key cx trace result (sense, obj, t) =
    sentinel_prop_test_generic key cx trace result obj (sense, obj, t)

  and sentinel_prop_test_generic key cx trace result orig_obj =
    let desc_of_sentinel sentinel =
      match sentinel with
      | UnionEnum.(One (Str s)) -> RStringLit s
      | UnionEnum.(One (Num (_, n))) -> RNumberLit n
      | UnionEnum.(One (Bool b)) -> RBooleanLit b
      | UnionEnum.(One Null) -> RNull
      | UnionEnum.(One Void) -> RVoid
      | UnionEnum.(Many _enums) -> RUnionEnum
    in

    (* Evaluate a refinement predicate of the form

      obj.key eq value

      where eq is === or !==.

      * key is key
      * (sense, obj, value) are the sense of the test, obj and value as above,
      respectively.

      As with other predicate filters, the goal is to statically determine when
      the predicate is definitely satisfied and when it is definitely
      unsatisfied, and narrow the possible types of obj under those conditions,
      while not narrowing in all other cases.

      In this case, the predicate is definitely satisfied (respectively,
      definitely unsatisfied) when the type of the key property in the type obj
      can be statically verified as having (respectively, not having) value as
      its only inhabitant.

      When satisfied, type obj flows to the recipient type result (in other
      words, we allow all such types in the refined type for obj).

      Otherwise, nothing flows to type result (in other words, we don't allow
      any such type in the refined type for obj).

      Overall the filtering process is somewhat tricky to understand. Refer to
      the predicate function and its callers to understand how the context is
      set up so that filtering ultimately only depends on what flows to
      result. **)
    let flow_sentinel sense props_tmap obj sentinel =
      match Context.get_prop cx props_tmap key with
      | Some p ->
        (match Property.read_t p with
        | Some t ->
          let reason =
            let desc = RMatchingProp (key, desc_of_sentinel sentinel) in
            replace_desc_reason desc (fst result)
          in
          let test = SentinelPropTestT (reason, orig_obj, key, sense, sentinel, result) in
          rec_flow cx trace (t, test)
        | None ->
          let reason_obj = reason_of_t obj in
          add_output
            cx
            ~trace
            (Error_message.EPropNotReadable
               { reason_prop = reason_obj; prop_name = Some key; use_op = unknown_use }))
      | None ->
        (* TODO: possibly unsound to filter out orig_obj here, but if we
         don't, case elimination based on sentinel prop checking doesn't
         work for (disjoint unions of) intersections of objects, where the
         sentinel prop and the payload appear in different branches of the
         intersection. It is easy to avoid this unsoundness with slightly
         more work, but will wait until a refactoring of property lookup
         lands to revisit. Tracked by #11301092. *)
        if orig_obj = obj then rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
    in
    let sentinel_of_literal = function
      | DefT (_, _, StrT (Literal (_, value)))
      | DefT (_, _, SingletonStrT value) ->
        Some UnionEnum.(One (Str value))
      | DefT (_, _, NumT (Literal (_, value)))
      | DefT (_, _, SingletonNumT value) ->
        Some UnionEnum.(One (Num value))
      | DefT (_, _, BoolT (Some value))
      | DefT (_, _, SingletonBoolT value) ->
        Some UnionEnum.(One (Bool value))
      | DefT (_, _, VoidT) -> Some UnionEnum.(One Void)
      | DefT (_, _, NullT) -> Some UnionEnum.(One Null)
      | UnionT (_, rep) ->
        begin
          match UnionRep.check_enum rep with
          | Some enums -> Some UnionEnum.(Many enums)
          | None -> None
        end
      | _ -> None
    in
    fun (sense, obj, t) ->
      match sentinel_of_literal t with
      | Some s ->
        begin
          match obj with
          (* obj.key ===/!== literal value *)
          | DefT (_, _, ObjT { props_tmap; _ }) -> flow_sentinel sense props_tmap obj s
          (* instance.key ===/!== literal value *)
          | DefT (_, _, InstanceT (_, _, _, { own_props; _ })) ->
            (* TODO: add test for sentinel test on implements *)
            flow_sentinel sense own_props obj s
          (* tuple.length ===/!== literal value *)
          | DefT (reason, trust, ArrT (TupleAT (_, ts))) when key = "length" ->
            let test =
              let desc = RMatchingProp (key, desc_of_sentinel s) in
              let r = replace_desc_reason desc (fst result) in
              SentinelPropTestT (r, orig_obj, key, sense, s, result)
            in
            rec_flow cx trace (tuple_length reason trust ts, test)
          | IntersectionT (_, rep) ->
            (* For an intersection of object types, try the test for each object
           type in turn, while recording the original intersection so that we
           end up with the right refinement. See the comment on the
           implementation of IntersectionPreprocessKit for more details. *)
            let reason = fst result in
            InterRep.members rep
            |> List.iter (fun obj ->
                   rec_flow
                     cx
                     trace
                     ( obj,
                       intersection_preprocess_kit
                         reason
                         (SentinelPropTest (sense, key, t, orig_obj, result)) ))
          | _ ->
            (* not enough info to refine *)
            rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
        end
      | None ->
        (* not enough info to refine *)
        rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)

  and sentinel_refinement =
    let open UnionEnum in
    let enum_match sense = function
      | (DefT (_, _, StrT (Literal (_, value))), Str sentinel) when value = sentinel != sense ->
        true
      | (DefT (_, _, NumT (Literal (_, (value, _)))), Num (sentinel, _))
        when value = sentinel != sense ->
        true
      | (DefT (_, _, BoolT (Some value)), Bool sentinel) when value = sentinel != sense -> true
      | (DefT (_, _, NullT), Null)
      | (DefT (_, _, VoidT), Void) ->
        true
      | _ -> false
    in
    fun cx trace v reason l key sense enum result ->
      match (v, enum) with
      | (_, One e) when enum_match sense (v, e) && not sense -> ()
      | (DefT (_, _, StrT _), One (Str sentinel)) when enum_match sense (v, Str sentinel) -> ()
      | (DefT (_, _, NumT _), One (Num sentinel)) when enum_match sense (v, Num sentinel) -> ()
      | (DefT (_, _, BoolT _), One (Bool sentinel)) when enum_match sense (v, Bool sentinel) -> ()
      | (DefT (_, _, (StrT _ | NumT _ | BoolT _ | NullT | VoidT)), Many enums) when sense ->
        UnionEnumSet.iter
          (fun enum ->
            if enum_match sense (v, enum) |> not then
              sentinel_refinement cx trace v reason l key sense (One enum) result)
          enums
      | (DefT (_, _, StrT _), One (Str _))
      | (DefT (_, _, NumT _), One (Num _))
      | (DefT (_, _, BoolT _), One (Bool _))
      | (DefT (_, _, NullT), One Null)
      | (DefT (_, _, VoidT), One Void)
      | (DefT (_, _, (StrT _ | NumT _ | BoolT _ | NullT | VoidT)), Many _) ->
        rec_flow_t cx trace ~use_op:unknown_use (l, OpenT result)
      (* types don't match (would've been matched above) *)
      (* we don't prune other types like objects or instances, even though
           a test like `if (ObjT === StrT)` seems obviously unreachable, but
           we have to be wary of toString and valueOf on objects/instances. *)
      | (DefT (_, _, (StrT _ | NumT _ | BoolT _ | NullT | VoidT)), _) when sense -> ()
      | (DefT (_, _, (StrT _ | NumT _ | BoolT _ | NullT | VoidT)), _)
      | _ ->
        (* property exists, but is not something we can use for refinement *)
        rec_flow_t cx trace ~use_op:unknown_use (l, OpenT result)

  (*******************************************************************)
  (* /predicate *)
  (*******************************************************************)
  and pick_use_op cx op1 op2 =
    let ignore_root = function
      | UnknownUse -> true
      | Internal _ -> true
      (* If we are speculating then a Speculation use_op should be considered
       * "opaque". If we are not speculating then Speculation use_ops that escaped
       * (through benign tvars) should be ignored.
       *
       * Ideally we could replace the Speculation use_ops on benign tvars with their
       * underlying use_op after speculation ends. *)
      | Speculation _ -> not (Speculation.speculating cx)
      | _ -> false
    in
    if ignore_root (root_of_use_op op1) then
      op2
    else
      let root_of_op2 = root_of_use_op op2 in
      let should_replace =
        fold_use_op
          (* If the root of the previous use_op is UnknownUse and our alternate
           * use_op does not have an UnknownUse root then we use our
           * alternate use_op. *)
          ignore_root
          (fun should_replace -> function
            (* If the use was added to an implicit type param then we want to use
             * our alternate if the implicit type param use_op chain is inside
             * the implicit type param instantiation. Since we can't directly compare
             * abstract locations, we determine whether to do this using a heuristic
             * based on the 'locality' of the use_op root. *)
            | ImplicitTypeParam when not should_replace ->
              (match root_of_op2 with
              | FunCall { local; _ }
              | FunCallMethod { local; _ } ->
                local
              | Addition _
              | AssignVar _
              | Coercion _
              | DeleteVar _
              | DeleteProperty _
              | FunImplicitReturn _
              | FunReturnStatement _
              | GetProperty _
              | SetProperty _
              | UpdateProperty _
              | JSXCreateElement _
              | ObjectSpread _
              | ObjectChain _
              | TypeApplication _
              | Speculation _
              | InitField _ ->
                true
              | Cast _
              | SwitchCheck _
              | ClassExtendsCheck _
              | ClassImplementsCheck _
              | ClassOwnProtoCheck _
              | GeneratorYield _
              | Internal _
              | ReactCreateElementCall _
              | ReactGetIntrinsic _
              | MatchingProp _
              | UnknownUse ->
                false)
            | _ -> should_replace)
          op2
      in
      if should_replace then
        op1
      else
        op2

  and flow_use_op cx op1 u =
    mod_use_op_of_use_t (fun op2 -> pick_use_op cx op1 op2) u
    (***********************)
    (* bounds manipulation *)
    (***********************)

  (** The following general considerations apply when manipulating bounds.

    1. All type variables start out as roots, but some of them eventually become
    goto nodes. As such, bounds of roots may contain goto nodes. However, we
    never perform operations directly on goto nodes; instead, we perform those
    operations on their roots. It is tempting to replace goto nodes proactively
    with their roots to avoid this issue, but doing so may be expensive, whereas
    the union-find data structure amortizes the cost of looking up roots.

    2. Another issue is that while the bounds of a type variable start out
    empty, and in particular do not contain the type variable itself, eventually
    other type variables in the bounds may be unified with the type variable. We
    do not remove these type variables proactively, but instead filter them out
    when considering the bounds. In the future we might consider amortizing the
    cost of this filtering.

    3. When roots are resolved, they act like the corresponding concrete
    types. We maintain the invariant that whenever lower bounds or upper bounds
    contain resolved roots, they also contain the corresponding concrete types.

    4. When roots are unresolved (they have lower bounds and upper bounds,
    possibly consisting of concrete types as well as type variables), we
    maintain the invarant that every lower bound has already been propagated to
    every upper bound. We also maintain the invariant that the bounds are
    transitively closed modulo equivalence: for every type variable in the
    bounds, all the bounds of its root are also included.

**)

  (* for each l in ls: l => u *)
  and flows_to_t cx trace ls u =
    ls
    |> TypeMap.iter (fun l (trace_l, use_op) ->
           let u = flow_use_op cx use_op u in
           join_flow cx [trace_l; trace] (l, u))

  (* for each u in us: l => u *)
  and flows_from_t cx trace ~new_use_op l us =
    us
    |> UseTypeMap.iter (fun u trace_u ->
           let u = flow_use_op cx new_use_op u in
           join_flow cx [trace; trace_u] (l, u))

  (* for each l in ls, u in us: l => u *)
  and flows_across cx trace ~use_op ls us =
    ls
    |> TypeMap.iter (fun l (trace_l, use_op') ->
           us
           |> UseTypeMap.iter (fun u trace_u ->
                  let u = flow_use_op cx use_op' (flow_use_op cx use_op u) in
                  join_flow cx [trace_l; trace; trace_u] (l, u)))

  (* bounds.upper += u *)
  and add_upper u trace bounds = bounds.upper <- UseTypeMap.add u trace bounds.upper

  (* bounds.lower += l *)
  and add_lower l (trace, use_op) bounds =
    bounds.lower <- TypeMap.add l (trace, use_op) bounds.lower

  (* Helper for functions that follow. *)
  (* Given a map of bindings from tvars to traces, a tvar to skip, and an `each`
   function taking a tvar and its associated trace, apply `each` to all
   unresolved root constraints reached from the bound tvars, except those of
   skip_tvar. (Typically skip_tvar is a tvar that will be processed separately,
   so we don't want to redo that work. We also don't want to consider any tvar
   that has already been resolved, because the resolved type will be processed
   separately, too, as part of the bounds of skip_tvar. **)
  and iter_with_filter cx bindings skip_id each =
    bindings
    |> IMap.iter (fun id trace ->
           match Context.find_constraints cx id with
           | (root_id, Unresolved bounds) when root_id <> skip_id -> each (root_id, bounds) trace
           | _ -> ())
    (* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += t2
*)
    (* When going through bounds1.lowertvars, filter out id1. **)

  (** As an optimization, skip id1 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
  and edges_to_t cx trace ?(opt = false) (id1, bounds1) t2 =
    let max = Context.max_trace_depth cx in
    if not opt then add_upper t2 trace bounds1;
    iter_with_filter cx bounds1.lowertvars id1 (fun (_, bounds) (trace_l, use_op) ->
        let t2 = flow_use_op cx use_op t2 in
        add_upper t2 (Trace.concat_trace ~max [trace_l; trace]) bounds)
    (* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += t1
*)
    (* When going through bounds2.uppertvars, filter out id2. **)

  (** As an optimization, skip id2 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
  and edges_from_t cx trace ~new_use_op ?(opt = false) t1 (id2, bounds2) =
    let max = Context.max_trace_depth cx in
    if not opt then add_lower t1 (trace, new_use_op) bounds2;
    iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) (trace_u, use_op) ->
        let use_op = pick_use_op cx new_use_op use_op in
        add_lower t1 (Trace.concat_trace ~max [trace; trace_u], use_op) bounds)

  (* for each id' in id + bounds.lowertvars:
   id'.bounds.upper += us
*)
  and edges_to_ts ~new_use_op cx trace ?(opt = false) (id, bounds) us =
    let max = Context.max_trace_depth cx in
    us
    |> UseTypeMap.iter (fun u trace_u ->
           let u = flow_use_op cx new_use_op u in
           edges_to_t cx (Trace.concat_trace ~max [trace; trace_u]) ~opt (id, bounds) u)

  (* for each id' in id + bounds.uppertvars:
   id'.bounds.lower += ls
*)
  and edges_from_ts cx trace ~new_use_op ?(opt = false) ls (id, bounds) =
    let max = Context.max_trace_depth cx in
    ls
    |> TypeMap.iter (fun l (trace_l, use_op) ->
           let new_use_op = pick_use_op cx use_op new_use_op in
           edges_from_t cx (Trace.concat_trace ~max [trace_l; trace]) ~new_use_op ~opt l (id, bounds))
    (* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += t2
   for each l in bounds1.lower: l => t2
*)

  (** As an invariant, bounds1.lower should already contain id.bounds.lower for
    each id in bounds1.lowertvars. **)
  and edges_and_flows_to_t cx trace ?(opt = false) (id1, bounds1) t2 =
    if not (UseTypeMap.mem t2 bounds1.upper) then (
      edges_to_t cx trace ~opt (id1, bounds1) t2;
      flows_to_t cx trace bounds1.lower t2
    )
    (* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += t1
   for each u in bounds2.upper: t1 => u
*)

  (** As an invariant, bounds2.upper should already contain id.bounds.upper for
    each id in bounds2.uppertvars. **)
  and edges_and_flows_from_t cx trace ~new_use_op ?(opt = false) t1 (id2, bounds2) =
    if not (TypeMap.mem t1 bounds2.lower) then (
      edges_from_t cx trace ~new_use_op ~opt t1 (id2, bounds2);
      flows_from_t cx trace ~new_use_op t1 bounds2.upper
    )

  (* bounds.uppertvars += id *)
  and add_uppertvar id trace use_op bounds =
    bounds.uppertvars <- IMap.add id (trace, use_op) bounds.uppertvars

  (* bounds.lowertvars += id *)
  and add_lowertvar id trace use_op bounds =
    bounds.lowertvars <- IMap.add id (trace, use_op) bounds.lowertvars
    (* for each id in id1 + bounds1.lowertvars:
   id.bounds.uppertvars += id2
*)
    (* When going through bounds1.lowertvars, filter out id1. **)

  (** As an optimization, skip id1 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
  and edges_to_tvar cx trace ~new_use_op ?(opt = false) (id1, bounds1) id2 =
    let max = Context.max_trace_depth cx in
    if not opt then add_uppertvar id2 trace new_use_op bounds1;
    iter_with_filter cx bounds1.lowertvars id1 (fun (_, bounds) (trace_l, use_op) ->
        let use_op = pick_use_op cx use_op new_use_op in
        add_uppertvar id2 (Trace.concat_trace ~max [trace_l; trace]) use_op bounds)
    (* for each id in id2 + bounds2.uppertvars:
   id.bounds.lowertvars += id1
*)
    (* When going through bounds2.uppertvars, filter out id2. **)

  (** As an optimization, skip id2 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
  and edges_from_tvar cx trace ~new_use_op ?(opt = false) id1 (id2, bounds2) =
    let max = Context.max_trace_depth cx in
    if not opt then add_lowertvar id1 trace new_use_op bounds2;
    iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) (trace_u, use_op) ->
        let use_op = pick_use_op cx new_use_op use_op in
        add_lowertvar id1 (Trace.concat_trace ~max [trace; trace_u]) use_op bounds)

  (* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += bounds2.upper
   id.bounds.uppertvars += id2
   id.bounds.uppertvars += bounds2.uppertvars
*)
  and add_upper_edges ~new_use_op cx trace ?(opt = false) (id1, bounds1) (id2, bounds2) =
    let max = Context.max_trace_depth cx in
    edges_to_ts ~new_use_op cx trace ~opt (id1, bounds1) bounds2.upper;
    edges_to_tvar cx trace ~new_use_op ~opt (id1, bounds1) id2;
    iter_with_filter cx bounds2.uppertvars id2 (fun (tvar, _) (trace_u, use_op) ->
        let new_use_op = pick_use_op cx new_use_op use_op in
        let trace = Trace.concat_trace ~max [trace; trace_u] in
        edges_to_tvar cx trace ~new_use_op ~opt (id1, bounds1) tvar)

  (* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += bounds1.lower
   id.bounds.lowertvars += id1
   id.bounds.lowertvars += bounds1.lowertvars
*)
  and add_lower_edges cx trace ~new_use_op ?(opt = false) (id1, bounds1) (id2, bounds2) =
    let max = Context.max_trace_depth cx in
    edges_from_ts cx trace ~new_use_op ~opt bounds1.lower (id2, bounds2);
    edges_from_tvar cx trace ~new_use_op ~opt id1 (id2, bounds2);
    iter_with_filter cx bounds1.lowertvars id1 (fun (tvar, _) (trace_l, use_op) ->
        let use_op = pick_use_op cx use_op new_use_op in
        let trace = Trace.concat_trace ~max [trace_l; trace] in
        edges_from_tvar cx trace ~new_use_op:use_op ~opt tvar (id2, bounds2))

  (***************)
  (* unification *)
  (***************)
  and unify_flip use_op = Frame (UnifyFlip, use_op)

  (* Chain a root to another root. If both roots are unresolved, this amounts to
   copying over the bounds of one root to another, and adding all the
   connections necessary when two non-unifiers flow to each other. If one or
   both of the roots are resolved, they effectively act like the corresponding
   concrete types. *)
  and goto cx trace ~use_op (id1, root1) (id2, root2) =
    match (root1.constraints, root2.constraints) with
    | (Unresolved bounds1, Unresolved bounds2) ->
      let cond1 = not_linked (id1, bounds1) (id2, bounds2) in
      let cond2 = not_linked (id2, bounds2) (id1, bounds1) in
      if cond1 then flows_across cx trace ~use_op bounds1.lower bounds2.upper;
      if cond2 then flows_across cx trace ~use_op:(unify_flip use_op) bounds2.lower bounds1.upper;
      if cond1 then (
        add_upper_edges cx trace ~new_use_op:use_op ~opt:true (id1, bounds1) (id2, bounds2);
        add_lower_edges cx trace ~new_use_op:use_op (id1, bounds1) (id2, bounds2)
      );
      if cond2 then (
        add_upper_edges cx trace ~new_use_op:(unify_flip use_op) (id2, bounds2) (id1, bounds1);
        add_lower_edges
          cx
          trace
          ~new_use_op:(unify_flip use_op)
          ~opt:true
          (id2, bounds2)
          (id1, bounds1)
      );
      Context.add_tvar cx id1 (Goto id2)
    | (Unresolved bounds1, (Resolved (_, t2) | FullyResolved (_, t2))) ->
      let t2_use = UseT (use_op, t2) in
      edges_and_flows_to_t cx trace ~opt:true (id1, bounds1) t2_use;
      edges_and_flows_from_t cx trace ~new_use_op:(unify_flip use_op) ~opt:true t2 (id1, bounds1);
      Context.add_tvar cx id1 (Goto id2)
    | ((Resolved (_, t1) | FullyResolved (_, t1)), Unresolved bounds2) ->
      let t1_use = UseT (unify_flip use_op, t1) in
      edges_and_flows_to_t cx trace ~opt:true (id2, bounds2) t1_use;
      edges_and_flows_from_t cx trace ~new_use_op:use_op ~opt:true t1 (id2, bounds2);
      Context.add_tvar cx id2 (Goto id1)
    | ((Resolved (_, t1) | FullyResolved (_, t1)), (Resolved (_, t2) | FullyResolved (_, t2))) ->
      (* replace node first, in case rec_unify recurses back to these tvars *)
      Context.add_tvar cx id1 (Goto id2);
      rec_unify cx trace ~use_op t1 t2

  (* Unify two type variables. This involves finding their roots, and making one
   point to the other. Ranks are used to keep chains short. *)
  and merge_ids cx trace ~use_op id1 id2 =
    let ((id1, root1), (id2, root2)) = (Context.find_root cx id1, Context.find_root cx id2) in
    if id1 = id2 then
      ()
    else if root1.rank < root2.rank then
      goto cx trace ~use_op (id1, root1) (id2, root2)
    else if root2.rank < root1.rank then
      goto cx trace ~use_op:(unify_flip use_op) (id2, root2) (id1, root1)
    else (
      Context.add_tvar cx id2 (Root { root2 with rank = root1.rank + 1 });
      goto cx trace ~use_op (id1, root1) (id2, root2)
    )

  (* Resolve a type variable to a type. This involves finding its root, and
   resolving to that type. *)
  and resolve_id cx trace ~use_op ?(fully_resolved = false) id t =
    let (id, root) = Context.find_root cx id in
    match root.constraints with
    | Unresolved bounds ->
      let constraints =
        if fully_resolved then
          FullyResolved (use_op, t)
        else
          Resolved (use_op, t)
      in
      Context.add_tvar cx id (Root { root with constraints });
      edges_and_flows_to_t cx trace ~opt:true (id, bounds) (UseT (use_op, t));
      edges_and_flows_from_t cx trace ~new_use_op:use_op ~opt:true t (id, bounds)
    | Resolved (_, t_)
    | FullyResolved (_, t_) ->
      rec_unify cx trace ~use_op t_ t

  (******************)

  (* Unification of two types *)

  (* It is potentially dangerous to unify a type variable to a type that "forgets"
   constraints during propagation. These types are "any-like": the canonical
   example of such a type is any. Overall, we want unification to be a sound
   "optimization," in the sense that replacing bidirectional flows with
   unification should not miss errors. But consider a scenario where we have a
   type variable with two incoming flows, string and any, and two outgoing
   flows, number and any. If we replace the flows from/to any with an
   unification with any, we will miss the string/number incompatibility error.

   However, unifying with any-like types is sometimes desirable /
   intentional. Thus, we limit the set of types on which unification is banned
   to just MergedT which is an internal type.
*)
  and ok_unify ~unify_any desc = function
    | AnyT _ ->
      (match desc with
      | RExistential -> true
      | _ -> unify_any)
    | MergedT _ -> false
    | _ -> true

  and __unify cx ~use_op ~unify_any t1 t2 trace =
    begin
      match Context.verbose cx with
      | Some { Verbose.indent; depth; enabled_during_flowlib = _ } ->
        let indent = String.make ((Trace.trace_depth trace - 1) * indent) ' ' in
        let pid = Context.pid_prefix cx in
        prerr_endlinef
          "\n%s%s%s =\n%s%s%s"
          indent
          pid
          (Debug_js.dump_t ~depth cx t1)
          indent
          pid
          (Debug_js.dump_t ~depth cx t2)
      | None -> ()
    end;

    (* If the type is the same type or we have already seen this type pair in our
     * cache then do not continue. *)
    if t1 = t2 then
      ()
    else (
      (* limit recursion depth *)
      RecursionCheck.check cx trace;

      (* In general, unifying t1 and t2 should have similar effects as flowing t1 to
     t2 and flowing t2 to t1. This also means that any restrictions on such
     flows should also be enforced here. In particular, we don't expect t1 or t2
     to be type parameters, and we don't expect t1 or t2 to be def types that
     don't make sense as use types. See __flow for more details. *)
      not_expect_bound cx t1;
      not_expect_bound cx t1;
      expect_proper_def t1;
      expect_proper_def t2;

      (* Before processing the unify action, check that it is not deferred. If it
     is, then when speculation is complete, the action either fires or is
     discarded depending on whether the case that created the action is
     selected or not. *)
      if not (Speculation.defer_action cx (Speculation_state.UnifyAction (use_op, t1, t2))) then
        match (t1, t2) with
        | (OpenT (_, id1), OpenT (_, id2)) -> merge_ids cx trace ~use_op id1 id2
        | (OpenT (r, id), t) when ok_unify ~unify_any (desc_of_reason r) t ->
          resolve_id cx trace ~use_op id t
        | (t, OpenT (r, id)) when ok_unify ~unify_any (desc_of_reason r) t ->
          resolve_id cx trace ~use_op:(unify_flip use_op) id t
        | (DefT (_, _, PolyT { id = id1; _ }), DefT (_, _, PolyT { id = id2; _ })) when id1 = id2 ->
          ()
        | ( DefT
              (r1, _, PolyT { tparams_loc = tparams_loc1; tparams = params1; t_out = t1; id = id1 }),
            DefT
              (r2, _, PolyT { tparams_loc = tparams_loc2; tparams = params2; t_out = t2; id = id2 })
          ) ->
          let n1 = Nel.length params1 in
          let n2 = Nel.length params2 in
          if n2 > n1 then
            add_output cx ~trace (Error_message.ETooManyTypeArgs (r2, r1, n1))
          else if n2 < n1 then
            add_output cx ~trace (Error_message.ETooFewTypeArgs (r2, r1, n1))
          else
            (* for equal-arity polymorphic types, unify param upper bounds
          with each other, then instances parameterized by these *)
            let args1 = instantiate_poly_param_upper_bounds cx params1 in
            let args2 = instantiate_poly_param_upper_bounds cx params2 in
            List.iter2 (rec_unify cx trace ~use_op) args1 args2;
            let inst1 =
              let r = reason_of_t t1 in
              mk_typeapp_of_poly
                cx
                trace
                ~use_op
                ~reason_op:r
                ~reason_tapp:r
                id1
                tparams_loc1
                params1
                t1
                args1
            in
            let inst2 =
              let r = reason_of_t t2 in
              mk_typeapp_of_poly
                cx
                trace
                ~use_op
                ~reason_op:r
                ~reason_tapp:r
                id2
                tparams_loc2
                params2
                t2
                args2
            in
            rec_unify cx trace ~use_op inst1 inst2
        | (DefT (_, _, ArrT (ArrayAT (t1, ts1))), DefT (_, _, ArrT (ArrayAT (t2, ts2)))) ->
          let ts1 = Base.Option.value ~default:[] ts1 in
          let ts2 = Base.Option.value ~default:[] ts2 in
          array_unify cx trace ~use_op (ts1, t1, ts2, t2)
        | (DefT (r1, _, ArrT (TupleAT (_, ts1))), DefT (r2, _, ArrT (TupleAT (_, ts2)))) ->
          let l1 = List.length ts1 in
          let l2 = List.length ts2 in
          if l1 <> l2 then
            add_output cx ~trace (Error_message.ETupleArityMismatch ((r1, r2), l1, l2, use_op));
          iter2opt
            (fun t1 t2 ->
              match (t1, t2) with
              | (Some t1, Some t2) -> rec_unify cx trace ~use_op t1 t2
              | _ -> ())
            (ts1, ts2)
        | ( DefT (lreason, _, ObjT { props_tmap = lflds; flags = lflags; _ }),
            DefT (ureason, _, ObjT { props_tmap = uflds; flags = uflags; _ }) ) ->
          (* ensure the keys and values are compatible with each other. *)
          let ldict = Obj_type.get_dict_opt lflags.obj_kind in
          let udict = Obj_type.get_dict_opt uflags.obj_kind in
          begin
            match (ldict, udict) with
            | (Some { key = lk; value = lv; _ }, Some { key = uk; value = uv; _ }) ->
              rec_unify
                cx
                trace
                lk
                uk
                ~use_op:
                  (Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op));
              rec_unify
                cx
                trace
                lv
                uv
                ~use_op:
                  (Frame
                     ( PropertyCompatibility { prop = None; lower = lreason; upper = ureason },
                       use_op ))
            | (Some _, None) ->
              let use_op =
                Frame
                  (PropertyCompatibility { prop = None; lower = ureason; upper = lreason }, use_op)
              in
              let lreason = replace_desc_reason RSomeProperty lreason in
              let err =
                Error_message.EPropNotFound
                  {
                    prop_name = None;
                    reason_prop = lreason;
                    reason_obj = ureason;
                    use_op;
                    suggestion = None;
                  }
              in
              add_output cx ~trace err
            | (None, Some _) ->
              let use_op =
                Frame
                  ( PropertyCompatibility { prop = None; lower = lreason; upper = ureason },
                    Frame (UnifyFlip, use_op) )
              in
              let ureason = replace_desc_reason RSomeProperty ureason in
              let err =
                Error_message.EPropNotFound
                  {
                    prop_name = None;
                    reason_prop = lreason;
                    reason_obj = ureason;
                    use_op;
                    suggestion = None;
                  }
              in
              add_output cx ~trace err
            | (None, None) -> ()
          end;

          let lpmap = Context.find_props cx lflds in
          let upmap = Context.find_props cx uflds in
          SMap.merge
            (fun x lp up ->
              ( if not (is_internal_name x || is_dictionary_exempt x) then
                match (lp, up) with
                | (Some p1, Some p2) -> unify_props cx trace ~use_op x lreason ureason p1 p2
                | (Some p1, None) ->
                  unify_prop_with_dict cx trace ~use_op x p1 lreason ureason udict
                | (None, Some p2) ->
                  unify_prop_with_dict cx trace ~use_op x p2 ureason lreason ldict
                | (None, None) -> () );
              None)
            lpmap
            upmap
          |> ignore
        | (DefT (_, _, FunT (_, _, funtype1)), DefT (_, _, FunT (_, _, funtype2)))
          when List.length funtype1.params = List.length funtype2.params ->
          rec_unify cx trace ~use_op funtype1.this_t funtype2.this_t;
          List.iter2
            (fun (_, t1) (_, t2) -> rec_unify cx trace ~use_op t1 t2)
            funtype1.params
            funtype2.params;
          rec_unify cx trace ~use_op funtype1.return_t funtype2.return_t
        | (TypeAppT (_, _, c1, ts1), TypeAppT (_, _, c2, ts2))
          when c1 = c2 && List.length ts1 = List.length ts2 ->
          List.iter2 (rec_unify cx trace ~use_op) ts1 ts2
        | (AnnotT (_, OpenT (_, id1), _), AnnotT (_, OpenT (_, id2), _)) ->
          (* It is tempting to unify the tvars here, but that would be problematic. These tvars should
        eventually resolve to the type definitions that these annotations reference. By unifying
        them, we might accidentally resolve one of the tvars to the type definition of the other,
        which would lead to confusing behavior.

        On the other hand, if the tvars are already resolved, then we can do something
        interesting... *)
          begin
            match (Context.find_graph cx id1, Context.find_graph cx id2) with
            | ( (Resolved (_, t1) | FullyResolved (_, t1)),
                (Resolved (_, t2) | FullyResolved (_, t2)) )
            (* Can we unify these types? Tempting, again, but annotations can refer to recursive type
             definitions, and we might get into an infinite loop (which could perhaps be avoided by
             a unification cache, but we'd rather not cache if we can get away with it).

             The alternative is to do naive unification, but we must be careful. In particular, it
             could cause confusing errors: recall that the naive unification of annotations goes
             through repositioning over these types.

             But if we simulate the same repositioning here, we won't really save anything. For
             example, these types could be essentially the same union, and repositioning them would
             introduce differences in their representations that would kill other
             optimizations. Thus, we focus on the special case where these types have the same
             reason, and then do naive unification. *)
              when Reason.concretize_equal
                     (Context.aloc_tables cx)
                     (reason_of_t t1)
                     (reason_of_t t2) ->
              naive_unify cx trace ~use_op t1 t2
            | _ -> naive_unify cx trace ~use_op t1 t2
          end
        | _ -> naive_unify cx trace ~use_op t1 t2
    )

  and unify_props cx trace ~use_op x r1 r2 p1 p2 =
    let use_op = Frame (PropertyCompatibility { prop = Some x; lower = r1; upper = r2 }, use_op) in
    (* If both sides are neutral fields, we can just unify once *)
    match (p1, p2) with
    | (Field (_, t1, Polarity.Neutral), Field (_, t2, Polarity.Neutral)) ->
      rec_unify cx trace ~use_op t1 t2
    | _ ->
      (* Otherwise, unify read/write sides separately. *)
      (match (Property.read_t p1, Property.read_t p2) with
      | (Some t1, Some t2) -> rec_unify cx trace ~use_op t1 t2
      | _ -> ());
      (match (Property.write_t p1, Property.write_t p2) with
      | (Some t1, Some t2) -> rec_unify cx trace ~use_op t1 t2
      | _ -> ());

      (* Error if polarity is not compatible both ways. *)
      let polarity1 = Property.polarity p1 in
      let polarity2 = Property.polarity p2 in
      if not (Polarity.compat (polarity1, polarity2) && Polarity.compat (polarity2, polarity1)) then
        add_output
          cx
          ~trace
          (Error_message.EPropPolarityMismatch ((r1, r2), Some x, (polarity1, polarity2), use_op))

  (* If some property `x` exists in one object but not another, ensure the
   property is compatible with a dictionary, or error if none. *)
  and unify_prop_with_dict cx trace ~use_op x p prop_obj_reason dict_reason dict =
    (* prop_obj_reason: reason of the object containing the prop
     dict_reason: reason of the object potentially containing a dictionary
     prop_reason: reason of the prop itself *)
    let prop_reason = replace_desc_reason (RProperty (Some x)) prop_obj_reason in
    match dict with
    | Some { key; value; dict_polarity; _ } ->
      rec_flow
        cx
        trace
        ( string_key x prop_reason,
          UseT
            ( Frame
                (IndexerKeyCompatibility { lower = dict_reason; upper = prop_obj_reason }, use_op),
              key ) );
      let p2 = Field (None, value, dict_polarity) in
      unify_props cx trace ~use_op x prop_obj_reason dict_reason p p2
    | None ->
      let use_op =
        Frame
          ( PropertyCompatibility { prop = Some x; lower = dict_reason; upper = prop_obj_reason },
            use_op )
      in
      let err =
        Error_message.EPropNotFound
          {
            prop_name = Some x;
            reason_prop = prop_reason;
            reason_obj = dict_reason;
            use_op;
            suggestion = None;
          }
      in
      add_output cx ~trace err

  (* TODO: Unification between concrete types is still implemented as
   bidirectional flows. This means that the destructuring work is duplicated,
   and we're missing some opportunities for nested unification. *)
  and naive_unify cx trace ~use_op t1 t2 =
    rec_flow_t cx trace ~use_op (t1, t2);
    rec_flow_t cx trace ~use_op:(unify_flip use_op) (t2, t1)

  (* mutable sites on parent values (i.e. object properties,
   array elements) must be typed invariantly when a value
   flows to the parent, unless the incoming value is fresh,
   in which case covariant typing is sound (since no alias
   will break if the subtyped child value is replaced by a
   non-subtyped value *)
  and flow_to_mutable_child cx trace use_op fresh t1 t2 =
    if fresh then
      rec_flow cx trace (t1, UseT (use_op, t2))
    else
      rec_unify cx trace ~use_op t1 t2

  (* Subtyping of arrays is complicated by tuples. Currently, there are three
   different kinds of types, all encoded by arrays:

   1. Array<T> (array type)
   2. [T1, T2] (tuple type)
   3. "internal" Array<X>[T1, T2] where T1 | T2 ~> X (array literal type)

   We have the following rules:

   (1) When checking types against Array<U>, the rules are not surprising. Array
   literal types behave like array types in these checks.

   * Array<T> ~> Array<U> checks T <~> U
   * [T1, T2] ~> Array<U> checks T1 | T2 ~> U
   * Array<X>[T1, T2] ~> Array<U> checks Array<X> ~> Array<U>

   (2) When checking types against [T1, T2], the rules are again not
   surprising. Array literal types behave like tuple types in these checks. We
   consider missing tuple elements to be undefined, following common usage (and
   consistency with missing call arguments).

   * Array<T> ~> [U1, U2] checks T ~> U1, T ~> U2
   * [T1, T2] ~> [U1, U2] checks T1 ~> U1 and T2 ~> U2
   * [T1, T2] ~> [U1] checks T1 ~> U1
   * [T1] ~> [U1, U2] checks T1 ~> U1 and void ~> U2
   * Array<X>[T1, T2] ~> [U1, U2] checks [T1, T2] ~> [U1, U2]

   (3) When checking types against Array<Y>[U1, U2], the rules are a bit
   unsound. Array literal types were not designed to appear as upper bounds. In
   particular, their summary element types are often overly precise. Checking
   individual element types of one array literal type against the summary
   element type of another array literal type can lead to crazy errors, so we
   currently drop such checks.

   TODO: Make these rules great again by computing more reasonable summary
   element types for array literal types.

   * Array<T> ~> Array<Y>[U1, U2] checks Array<T> ~> Array<Y>
   * [T1, T2] ~> Array<Y>[U1, U2] checks T1 ~> U1, T2 ~> U2
   * [T1, T2] ~> Array<Y>[U1] checks T1 ~> U1
   * [T1] ~> Array<Y>[U1, U2] checks T1 ~> U1
   * Array<X>[T1, T2] ~> Array<Y>[U1, U2] checks [T1, T2] ~> Array<Y>[U1, U2]

   *)
  and array_flow cx trace use_op lit1 r1 ?(index = 0) = function
    (* empty array / array literal / tuple flowing to array / array literal /
     tuple (includes several cases, analyzed below) *)
    | ([], e1, _, e2) ->
      (* if lower bound is an empty array / array literal *)
      if index = 0 then
        (* general element1 = general element2 *)
        flow_to_mutable_child cx trace use_op lit1 e1 e2
    (* otherwise, lower bound is an empty tuple (nothing to do) *)

    (* non-empty array literal / tuple ~> empty array / array literal / tuple *)
    | (_, e1, [], e2) ->
      (* general element1 < general element2 *)
      rec_flow cx trace (e1, UseT (use_op, e2))
    (* non-empty array literal / tuple ~> non-empty array literal / tuple *)
    | (t1 :: ts1, e1, t2 :: ts2, e2) ->
      (* specific element1 = specific element2 *)
      flow_to_mutable_child cx trace use_op lit1 t1 t2;
      array_flow cx trace use_op lit1 r1 ~index:(index + 1) (ts1, e1, ts2, e2)

  (* TODO: either ensure that array_unify is the same as array_flow both ways, or
   document why not. *)
  (* array helper *)
  and array_unify cx trace ~use_op = function
    | ([], e1, [], e2) ->
      (* general element1 = general element2 *)
      rec_unify cx trace ~use_op e1 e2
    | (ts1, _, [], e2)
    | ([], e2, ts1, _) ->
      (* specific element1 = general element2 *)
      List.iter (fun t1 -> rec_unify cx trace ~use_op t1 e2) ts1
    | (t1 :: ts1, e1, t2 :: ts2, e2) ->
      (* specific element1 = specific element2 *)
      rec_unify cx trace ~use_op t1 t2;
      array_unify cx trace ~use_op (ts1, e1, ts2, e2)

  (*******************************************************************)
  (* subtyping a sequence of arguments with a sequence of parameters *)
  (*******************************************************************)

  (* Process spread arguments and then apply the arguments to the parameters *)
  and multiflow_call cx trace ~use_op reason_op args ft =
    let resolve_to = ResolveSpreadsToMultiflowCallFull (mk_id (), ft) in
    resolve_call_list cx ~trace ~use_op reason_op args resolve_to

  (* Process spread arguments and then apply the arguments to the parameters *)
  and multiflow_subtype cx trace ~use_op reason_op args ft =
    let resolve_to = ResolveSpreadsToMultiflowSubtypeFull (mk_id (), ft) in
    resolve_call_list cx ~trace ~use_op reason_op args resolve_to

  (* Like multiflow_partial, but if there is no spread argument, it flows VoidT to
   * all unused parameters *)
  and multiflow_full
      cx ~trace ~use_op reason_op ~is_strict ~def_reason ~spread_arg ~rest_param (arglist, parlist)
      =
    let (unused_parameters, _) =
      multiflow_partial
        cx
        ~trace
        ~use_op
        reason_op
        ~is_strict
        ~def_reason
        ~spread_arg
        ~rest_param
        (arglist, parlist)
    in
    let _ =
      List.fold_left
        (fun n (_, param) ->
          let use_op = Frame (FunMissingArg { n; op = reason_op; def = def_reason }, use_op) in
          rec_flow cx trace (VoidT.why reason_op |> with_trust bogus_trust, UseT (use_op, param));
          n + 1)
        (List.length parlist - List.length unused_parameters + 1)
        unused_parameters
    in
    ()

  (* This is a tricky function. The simple description is that it flows all the
   * arguments to all the parameters. This function is used by
   * Function.prototype.apply, so after the arguments are applied, it returns the
   * unused parameters.
   *
   * It is a little trickier in that there may be a single spread argument after
   * all the regular arguments. There may also be a rest parameter.
   *)
  and multiflow_partial =
    let rec multiflow_non_spreads cx ~use_op n (arglist, parlist) =
      match (arglist, parlist) with
      (* Do not complain on too many arguments.
       This pattern is ubiqutous and causes a lot of noise when complained about.
       Note: optional/rest parameters do not provide a workaround in this case.
    *)
      | (_, [])
      (* No more arguments *)
      | ([], _) ->
        ([], arglist, parlist)
      | ((tin, _) :: tins, (name, tout) :: touts) ->
        (* flow `tin` (argument) to `tout` (param). *)
        let tout =
          let use_op =
            Frame (FunParam { n; name; lower = reason_of_t tin; upper = reason_of_t tout }, use_op)
          in
          UseT (use_op, tout)
        in
        let (used_pairs, unused_arglist, unused_parlist) =
          multiflow_non_spreads cx ~use_op (n + 1) (tins, touts)
        in
        ((tin, tout) :: used_pairs, unused_arglist, unused_parlist)
    in
    fun cx ~trace ~use_op ~is_strict ~def_reason ~spread_arg ~rest_param reason_op (arglist, parlist)
        ->
      (* Handle all the non-spread arguments and all the non-rest parameters *)
      let (used_pairs, unused_arglist, unused_parlist) =
        multiflow_non_spreads cx ~use_op 1 (arglist, parlist)
      in
      (* If there is a spread argument, it will consume all the unused parameters *)
      let (used_pairs, unused_parlist) =
        match spread_arg with
        | None -> (used_pairs, unused_parlist)
        | Some (spread_arg_elemt, _) ->
          (* The spread argument may be an empty array and to be 100% correct, we
           * should flow VoidT to every remaining parameter, however we don't. This
           * is consistent with how we treat arrays almost everywhere else *)
          ( used_pairs
            @ Base.List.map
                ~f:(fun (_, param) ->
                  let use_op =
                    Frame
                      ( FunRestParam
                          { lower = reason_of_t spread_arg_elemt; upper = reason_of_t param },
                        use_op )
                  in
                  (spread_arg_elemt, UseT (use_op, param)))
                unused_parlist,
            [] )
      in
      (* If there is a rest parameter, it will consume all the unused arguments *)
      match rest_param with
      | None ->
        ( if is_strict && Context.enforce_strict_call_arity cx then
          match unused_arglist with
          | [] -> ()
          | (first_unused_arg, _) :: _ ->
            Error_message.EFunctionCallExtraArg
              ( mk_reason RFunctionUnusedArgument (loc_of_t first_unused_arg),
                def_reason,
                List.length parlist,
                use_op )
            |> add_output cx ~trace );

        (* Flow the args and params after we add the EFunctionCallExtraArg error.
         * This improves speculation error reporting. *)
        List.iter (rec_flow cx trace) used_pairs;

        (unused_parlist, rest_param)
      | Some (name, loc, rest_param) ->
        List.iter (rec_flow cx trace) used_pairs;

        let orig_rest_reason = repos_reason loc (reason_of_t rest_param) in
        (* We're going to build an array literal with all the unused arguments
         * (and the spread argument if it exists). Then we're going to flow that
         * to the rest parameter *)
        let rev_elems =
          List.rev_map (fun (arg, generic) -> UnresolvedArg (arg, generic)) unused_arglist
        in
        let unused_rest_param =
          match spread_arg with
          | None ->
            (* If the rest parameter is consuming N elements, then drop N elements
             * from the rest parameter *)
            let rest_reason = reason_of_t rest_param in
            Tvar.mk_derivable_where cx rest_reason (fun tout ->
                let i = List.length rev_elems in
                rec_flow cx trace (rest_param, ArrRestT (use_op, orig_rest_reason, i, tout)))
          | Some _ ->
            (* If there is a spread argument, then a tuple rest parameter will error
             * anyway. So let's assume that the rest param is an array with unknown
             * arity. Dropping elements from it isn't worth doing *)
            rest_param
        in
        let elems =
          match spread_arg with
          | None -> List.rev rev_elems
          | Some (spread_arg_elemt, generic) ->
            let reason = reason_of_t spread_arg_elemt in
            let spread_array =
              DefT (reason, bogus_trust (), ArrT (ArrayAT (spread_arg_elemt, None)))
            in
            let spread_array =
              Base.Option.value_map
                ~f:(fun id ->
                  GenericT { id; bound = spread_array; reason; name = Generic.to_string id })
                ~default:spread_array
                generic
            in
            List.rev_append rev_elems [UnresolvedSpreadArg spread_array]
        in
        let arg_array_reason =
          replace_desc_reason (RRestArray (desc_of_reason reason_op)) reason_op
        in
        let arg_array =
          Tvar.mk_where cx arg_array_reason (fun tout ->
              let reason_op = arg_array_reason in
              let element_reason =
                replace_desc_reason Reason.inferred_union_elem_array_desc reason_op
              in
              let elem_t = Tvar.mk cx element_reason in
              ResolveSpreadsToArrayLiteral (mk_id (), elem_t, tout)
              |> resolve_spread_list cx ~use_op ~reason_op elems)
        in
        let () =
          let use_op =
            Frame
              ( FunRestParam { lower = reason_of_t arg_array; upper = reason_of_t rest_param },
                use_op )
          in
          rec_flow cx trace (arg_array, UseT (use_op, rest_param))
        in
        (unused_parlist, Some (name, loc, unused_rest_param))

  and resolve_call_list cx ~trace ~use_op reason_op args resolve_to =
    let unresolved =
      Base.List.map
        ~f:(function
          | Arg t -> UnresolvedArg (t, None)
          | SpreadArg t -> UnresolvedSpreadArg t)
        args
    in
    resolve_spread_list_rec cx ~trace ~use_op ~reason_op ([], unresolved) resolve_to

  and resolve_spread_list cx ~use_op ~reason_op list resolve_to =
    resolve_spread_list_rec cx ~use_op ~reason_op ([], list) resolve_to

  (* This function goes through the unresolved elements to find the next rest
   * element to resolve *)
  and resolve_spread_list_rec cx ?trace ~use_op ~reason_op (resolved, unresolved) resolve_to =
    match (resolved, unresolved) with
    | (resolved, []) ->
      finish_resolve_spread_list cx ?trace ~use_op ~reason_op (List.rev resolved) resolve_to
    | (resolved, UnresolvedArg (next, generic) :: unresolved) ->
      resolve_spread_list_rec
        cx
        ?trace
        ~use_op
        ~reason_op
        (ResolvedArg (next, generic) :: resolved, unresolved)
        resolve_to
    | (resolved, UnresolvedSpreadArg next :: unresolved) ->
      flow_opt
        cx
        ?trace
        ( next,
          ResolveSpreadT
            ( use_op,
              reason_op,
              { rrt_resolved = resolved; rrt_unresolved = unresolved; rrt_resolve_to = resolve_to }
            ) )

  (* Now that everything is resolved, we can construct whatever type we're trying
   * to resolve to. *)
  and finish_resolve_spread_list =
    (* Turn tuple rest params into single params *)
    let flatten_spread_args list =
      list
      |> Base.List.fold_left
           ~f:(fun acc param ->
             match param with
             | ResolvedSpreadArg (_, arrtype, generic) ->
               begin
                 match arrtype with
                 | ArrayAT (_, Some tuple_types)
                 | TupleAT (_, tuple_types) ->
                   Base.List.fold_left
                     ~f:(fun acc elem -> ResolvedArg (elem, generic) :: acc)
                     ~init:acc
                     tuple_types
                 | ArrayAT (_, None)
                 | ROArrayAT _ ->
                   param :: acc
               end
             | ResolvedAnySpreadArg _
             | ResolvedArg _ ->
               param :: acc)
           ~init:[]
      |> Base.List.rev
    in
    let spread_resolved_to_any =
      List.exists (function
          | ResolvedAnySpreadArg _ -> true
          | ResolvedArg _
          | ResolvedSpreadArg _ ->
            false)
    in
    let finish_array cx ~use_op ?trace ~reason_op ~resolve_to resolved elemt tout =
      (* Did `any` flow to one of the rest parameters? If so, we need to resolve
       * to a type that is both a subtype and supertype of the desired type. *)
      let result =
        if spread_resolved_to_any resolved then
          match resolve_to with
          (* Array<any> is a good enough any type for arrays *)
          | `Array -> DefT (reason_op, bogus_trust (), ArrT (ArrayAT (AnyT.untyped reason_op, None)))
          (* Array literals can flow to a tuple. Arrays can't. So if the presence
           * of an `any` forces us to degrade an array literal to Array<any> then
           * we might get a new error. Since introducing `any`'s shouldn't cause
           * errors, this is bad. Instead, let's degrade array literals to `any` *)
          | `Literal
          (* There is no AnyTupleT type, so let's degrade to `any`. *)
          | `Tuple ->
            AnyT.untyped reason_op
        else
          (* Spreads that resolve to tuples are flattened *)
          let elems = flatten_spread_args resolved in
          let tuple_types =
            match resolve_to with
            | `Literal
            | `Tuple ->
              elems
              (* If no spreads are left, then this is a tuple too! *)
              |> List.fold_left
                   (fun acc elem ->
                     match (acc, elem) with
                     | (None, _) -> None
                     | (_, ResolvedSpreadArg _) -> None
                     | (Some tuple_types, ResolvedArg (t, _)) -> Some (t :: tuple_types)
                     | (_, ResolvedAnySpreadArg _) -> failwith "Should not be hit")
                   (Some [])
              |> Base.Option.map ~f:List.rev
            | `Array -> None
          in

          (* We infer the array's general element type by looking at the type of
           * every element in the array *)
          let (tset, generic) =
            Generic.(
              List.fold_left
                (fun (tset, generic_state) elem ->
                  let (elemt, generic, ro) =
                    match elem with
                    | ResolvedSpreadArg (_, arrtype, generic) ->
                      (elemt_of_arrtype arrtype, generic, ro_of_arrtype arrtype)
                    | ResolvedArg (elemt, generic) -> (elemt, generic, ArraySpread.NonROSpread)
                    | ResolvedAnySpreadArg _ -> failwith "Should not be hit"
                  in
                  ( TypeExSet.add elemt tset,
                    ArraySpread.merge
                      ~printer:
                        (print_if_verbose_lazy
                           cx
                           (Base.Option.value trace ~default:Trace.dummy_trace))
                      generic_state
                      generic
                      ro ))
                (TypeExSet.empty, ArraySpread.Bottom)
                elems)
          in
          let generic = Generic.ArraySpread.to_option generic in

          (* composite elem type is an upper bound of all element types *)
          (* Should the element type of the array be the union of its element types?

         No. Instead of using a union, we use an unresolved tvar to
         represent the least upper bound of each element type. Effectively,
         this keeps the element type "open," at least locally.[*]

         Using a union pins down the element type prematurely, and moreover,
         might lead to speculative matching when setting elements or caling
         contravariant methods (`push`, `concat`, etc.) on the array.

         In any case, using a union doesn't quite work as intended today
         when the element types themselves could be unresolved tvars. For
         example, the following code would work even with unions:

         declare var o: { x: number; }
         var a = ["hey", o.x]; // no error, but is an error if 42 replaces o.x
         declare var i: number;
         a[i] = false;

         [*] Eventually, the element type does get pinned down to a union
         when it is part of the module's exports. In the future we might
         have to do that pinning more carefully, and using an unresolved
         tvar instead of a union here doesn't conflict with those plans.
      *)
          TypeExSet.elements tset |> List.iter (fun t -> flow cx (t, UseT (use_op, elemt)));

          let t =
            match (tuple_types, resolve_to) with
            | (_, `Array) -> DefT (reason_op, bogus_trust (), ArrT (ArrayAT (elemt, None)))
            | (_, `Literal) -> DefT (reason_op, bogus_trust (), ArrT (ArrayAT (elemt, tuple_types)))
            | (Some tuple_types, `Tuple) ->
              DefT (reason_op, bogus_trust (), ArrT (TupleAT (elemt, tuple_types)))
            | (None, `Tuple) -> DefT (reason_op, bogus_trust (), ArrT (ArrayAT (elemt, None)))
          in
          Base.Option.value_map
            ~f:(fun id ->
              GenericT { bound = t; id; name = Generic.to_string id; reason = reason_of_t t })
            ~default:t
            generic
      in
      flow_opt_t cx ~use_op ?trace (result, tout)
    in
    (* If there are no spread elements or if all the spread elements resolved to
     * tuples or array literals, then this is easy. We just flatten them all.
     *
     * However, if we have a spread that resolved to any or to an array of
     * unknown length, then we're in trouble. Basically, any remaining argument
     * might flow to any remaining parameter.
     *)
    let flatten_call_arg =
      let rec flatten cx r args spread resolved =
        if resolved = [] then
          (args, spread)
        else
          match spread with
          | None ->
            (match resolved with
            | ResolvedArg (t, generic) :: rest -> flatten cx r ((t, generic) :: args) spread rest
            | ResolvedSpreadArg (_, (ArrayAT (_, Some ts) | TupleAT (_, ts)), generic) :: rest ->
              let args = List.rev_append (List.map (fun t -> (t, generic)) ts) args in
              flatten cx r args spread rest
            | ResolvedSpreadArg (r, _, _) :: _
            | ResolvedAnySpreadArg r :: _ ->
              (* We weren't able to flatten the call argument list to remove all
               * spreads. This means we need to build a spread argument, with
               * unknown arity. *)
              let tset = TypeExSet.empty in
              flatten cx r args (Some (tset, Generic.ArraySpread.Bottom)) resolved
            | [] -> failwith "Empty list already handled")
          | Some (tset, generic) ->
            let (elemt, generic', ro, rest) =
              match resolved with
              | ResolvedArg (t, generic) :: rest ->
                (t, generic, Generic.ArraySpread.NonROSpread, rest)
              | ResolvedSpreadArg (_, arrtype, generic) :: rest ->
                (elemt_of_arrtype arrtype, generic, ro_of_arrtype arrtype, rest)
              | ResolvedAnySpreadArg reason :: rest ->
                (AnyT.untyped reason, None, Generic.ArraySpread.NonROSpread, rest)
              | [] -> failwith "Empty list already handled"
            in
            let tset = TypeExSet.add elemt tset in
            let generic =
              Generic.ArraySpread.merge
                ~printer:(print_if_verbose_lazy cx Trace.dummy_trace)
                generic
                generic'
                ro
            in
            flatten cx r args (Some (tset, generic)) rest
      in
      fun cx ~use_op r resolved ->
        let (args, spread) = flatten cx r [] None resolved in
        let spread =
          Base.Option.map
            ~f:(fun (tset, generic) ->
              let generic = Generic.ArraySpread.to_option generic in
              let r = mk_reason RArray (aloc_of_reason r) in
              ( Tvar.mk_where cx r (fun tvar ->
                    TypeExSet.iter (fun t -> flow cx (t, UseT (use_op, tvar))) tset),
                generic ))
            spread
        in
        (List.rev args, spread)
    in
    (* This is used for things like Function.prototype.bind, which partially
     * apply arguments and then return the new function. *)
    let finish_multiflow_partial cx ?trace ~use_op ~reason_op ft call_reason resolved tout =
      (* Multiflows always come out of a flow *)
      let trace =
        match trace with
        | Some trace -> trace
        | None -> failwith "All multiflows show have a trace"
      in
      let { params; rest_param; return_t; def_reason; _ } = ft in
      let (args, spread_arg) = flatten_call_arg cx ~use_op reason_op resolved in
      let (params, rest_param) =
        multiflow_partial
          cx
          ~trace
          ~use_op
          reason_op
          ~is_strict:true
          ~def_reason
          ~spread_arg
          ~rest_param
          (args, params)
      in
      let (params_names, params_tlist) = List.split params in
      (* e.g. "bound function type", positioned at reason_op *)
      let bound_reason =
        let desc = RBound (desc_of_reason reason_op) in
        replace_desc_reason desc call_reason
      in
      let def_reason = reason_op in
      let funt =
        DefT
          ( reason_op,
            bogus_trust (),
            FunT
              ( dummy_static bound_reason,
                dummy_prototype,
                mk_methodtype dummy_this params_tlist return_t ~rest_param ~def_reason ~params_names
              ) )
      in
      rec_flow_t cx trace ~use_op:unknown_use (funt, tout)
    in
    (* This is used for things like function application, where all the arguments
     * are applied to a function *)
    let finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict ft resolved =
      (* Multiflows always come out of a flow *)
      let trace =
        match trace with
        | Some trace -> trace
        | None -> failwith "All multiflows show have a trace"
      in
      let { params; rest_param; def_reason; _ } = ft in
      let (args, spread_arg) = flatten_call_arg cx ~use_op reason_op resolved in
      multiflow_full
        cx
        ~trace
        ~use_op
        reason_op
        ~is_strict
        ~def_reason
        ~spread_arg
        ~rest_param
        (args, params)
    in
    (* Similar to finish_multiflow_full but for custom functions. *)
    let finish_custom_fun_call cx ?trace ~use_op ~reason_op kind tout resolved =
      (* Multiflows always come out of a flow *)
      let trace =
        match trace with
        | Some trace -> trace
        | None -> failwith "All multiflows show have a trace"
      in
      let (args, spread_arg) = flatten_call_arg cx ~use_op reason_op resolved in
      let spread_arg = Base.Option.map ~f:fst spread_arg in
      let args = Base.List.map ~f:fst args in
      CustomFunKit.run cx trace ~use_op reason_op kind args spread_arg tout
    in
    (* This is used for things like Function.prototype.apply, whose second arg is
     * basically a spread argument that we'd like to resolve *)
    let finish_call_t cx ?trace ~use_op ~reason_op funcalltype resolved tin =
      let flattened = flatten_spread_args resolved in
      let call_args_tlist =
        Base.List.map
          ~f:(function
            | ResolvedArg (t, _) -> Arg t
            | ResolvedSpreadArg (r, arrtype, generic) ->
              let arr = DefT (r, bogus_trust (), ArrT arrtype) in
              let arr =
                Base.Option.value_map
                  ~f:(fun id ->
                    GenericT { bound = arr; reason = r; id; name = Generic.to_string id })
                  ~default:arr
                  generic
              in
              SpreadArg arr
            | ResolvedAnySpreadArg r -> SpreadArg (AnyT.untyped r))
          flattened
      in
      let call_t = CallT (use_op, reason_op, { funcalltype with call_args_tlist }) in
      flow_opt cx ?trace (tin, call_t)
    in
    fun cx ?trace ~use_op ~reason_op resolved resolve_to ->
      match resolve_to with
      | ResolveSpreadsToArrayLiteral (_, elem_t, tout) ->
        finish_array cx ~use_op ?trace ~reason_op ~resolve_to:`Literal resolved elem_t tout
      | ResolveSpreadsToArray (elem_t, tout) ->
        finish_array cx ~use_op ?trace ~reason_op ~resolve_to:`Array resolved elem_t tout
      | ResolveSpreadsToMultiflowPartial (_, ft, call_reason, tout) ->
        finish_multiflow_partial cx ?trace ~use_op ~reason_op ft call_reason resolved tout
      | ResolveSpreadsToMultiflowCallFull (_, ft) ->
        finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict:true ft resolved
      | ResolveSpreadsToMultiflowSubtypeFull (_, ft) ->
        finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict:false ft resolved
      | ResolveSpreadsToCustomFunCall (_, kind, tout) ->
        finish_custom_fun_call cx ?trace ~use_op ~reason_op kind tout resolved
      | ResolveSpreadsToCallT (funcalltype, tin) ->
        finish_call_t cx ?trace ~use_op ~reason_op funcalltype resolved tin

  and perform_lookup_action cx trace propref p target_kind lreason ureason = function
    | LookupProp (use_op, up) -> rec_flow_p cx trace ~use_op lreason ureason propref (p, up)
    | SuperProp (use_op, lp) -> rec_flow_p cx trace ~use_op ureason lreason propref (lp, p)
    | ReadProp { use_op; obj_t = _; tout } ->
      begin
        match Property.read_t p with
        | Some t ->
          let loc = aloc_of_reason ureason in
          rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace loc t, OpenT tout)
        | None ->
          let (reason_prop, prop_name) =
            match propref with
            | Named (r, x) -> (r, Some x)
            | Computed t -> (reason_of_t t, None)
          in
          let msg = Error_message.EPropNotReadable { reason_prop; prop_name; use_op } in
          add_output cx ~trace msg
      end
    | WriteProp { use_op; obj_t = _; tin; write_ctx; prop_tout; mode } ->
      begin
        match (Property.write_t ~ctx:write_ctx p, target_kind, mode) with
        | (Some t, IndexerProperty, Delete) ->
          (* Always OK to delete a property we found via an indexer *)
          let void = VoidT.why (reason_of_t t) |> with_trust literal_trust in
          Base.Option.iter
            ~f:(fun prop_tout -> rec_flow_t cx trace ~use_op:unknown_use (void, prop_tout))
            prop_tout
        | (Some t, _, _) ->
          rec_flow cx trace (tin, UseT (use_op, t));
          Base.Option.iter
            ~f:(fun prop_tout -> rec_flow_t cx trace ~use_op:unknown_use (t, prop_tout))
            prop_tout
        | (None, _, _) ->
          let (reason_prop, prop_name) =
            match propref with
            | Named (r, x) -> (r, Some x)
            | Computed t -> (reason_of_t t, None)
          in
          let msg = Error_message.EPropNotWritable { reason_prop; prop_name; use_op } in
          add_output cx ~trace msg
      end
    | MatchProp (use_op, tin) ->
      begin
        match Property.read_t p with
        | Some t -> rec_flow cx trace (tin, UseT (use_op, t))
        | None ->
          let (reason_prop, prop_name) =
            match propref with
            | Named (r, x) -> (r, Some x)
            | Computed t -> (reason_of_t t, None)
          in
          add_output cx ~trace (Error_message.EPropNotReadable { reason_prop; prop_name; use_op })
      end

  and perform_elem_action cx trace ~use_op ~restrict_deletes reason_op l value action =
    match (action, restrict_deletes) with
    | (ReadElem t, _) ->
      let loc = aloc_of_reason reason_op in
      rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace loc value, OpenT t)
    | (WriteElem (tin, tout, Assign), _)
    | (WriteElem (tin, tout, Delete), true) ->
      rec_flow cx trace (tin, UseT (use_op, value));
      Base.Option.iter ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (l, t)) tout
    | (WriteElem (tin, tout, Delete), false) ->
      (* Ok to delete arbitrary elements on arrays, not OK for tuples *)
      rec_flow
        cx
        trace
        (tin, UseT (use_op, VoidT.why (reason_of_t value) |> with_trust literal_trust));
      Base.Option.iter ~f:(fun t -> rec_flow_t cx trace ~use_op:unknown_use (l, t)) tout
    | (CallElem (reason_call, action), _) ->
      rec_flow cx trace (value, apply_method_action use_op reason_call action)

  and string_key s reason =
    let key_reason = replace_desc_reason (RPropertyIsAString s) reason in
    DefT (key_reason, bogus_trust (), StrT (Literal (None, s)))

  (* builtins, contd. *)
  and get_builtin cx ?trace x reason =
    Tvar.mk_no_wrap_where cx reason (fun builtin ->
        let propref = Named (reason, x) in
        flow_opt cx ?trace (builtins cx, GetPropT (unknown_use, reason, propref, builtin)))

  and lookup_builtin cx ?trace x reason strict builtin =
    let propref = Named (reason, x) in
    let l = builtins cx in
    flow_opt
      cx
      ?trace
      ( l,
        LookupT
          {
            reason;
            lookup_kind = strict;
            ts = [];
            propref;
            lookup_action = ReadProp { use_op = unknown_use; obj_t = l; tout = builtin };
            ids = Some Properties.Set.empty;
          } )

  and get_builtin_typeapp cx ?trace reason x targs =
    let t = get_builtin cx ?trace x reason in
    typeapp reason t targs

  (* Specialize a polymorphic class, make an instance of the specialized class. *)
  and mk_typeapp_instance cx ?trace ~use_op ~reason_op ~reason_tapp ?cache c ts =
    let t = Tvar.mk cx reason_tapp in
    flow_opt cx ?trace (c, SpecializeT (use_op, reason_op, reason_tapp, cache, Some ts, t));
    mk_instance_raw cx ?trace reason_tapp ~reason_type:(reason_of_t c) t

  and mk_typeapp_instance_of_poly cx trace ~use_op ~reason_op ~reason_tapp id tparams_loc xs t ts =
    let t = mk_typeapp_of_poly cx trace ~use_op ~reason_op ~reason_tapp id tparams_loc xs t ts in
    mk_instance cx ~trace reason_tapp t

  and mk_typeapp_of_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache id tparams_loc xs t ts =
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

  and mk_instance cx ?trace instance_reason ?use_desc c =
    mk_instance_raw cx ?trace instance_reason ?use_desc ~reason_type:instance_reason c

  and mk_instance_raw cx ?trace instance_reason ?(use_desc = false) ~reason_type c =
    (* Make an annotation. *)
    let source =
      Tvar.mk_where cx instance_reason (fun t ->
          (* this part is similar to making a runtime value *)
          flow_opt_t
            cx
            ?trace
            ~use_op:unknown_use
            (c, DefT (reason_type, bogus_trust (), TypeT (InstanceKind, t))))
    in
    AnnotT (instance_reason, source, use_desc)
    (* Optimization where an union is a subset of another. Equality modulo
    reasons is important for this optimization to be effective, since types
    are repositioned everywhere. *)

  (** TODO: (1) Define a more general partial equality, that takes into
    account unified type variables. (2) Get rid of UnionRep.quick_mem. **)
  and union_optimization_guard =
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
            let unwrap rep =
              UnionRep.members rep |> Base.List.map ~f:(Type_mapper.unwrap_type cx)
            in
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

  and remove_predicate_from_union reason cx predicate =
    UnionRep.members
    %> Type_mapper.union_flatten cx
    %> Base.List.rev_filter ~f:(predicate %> not)
    %> union_of_ts reason

  and reposition_reason cx ?trace reason ?(use_desc = false) t =
    reposition
      cx
      ?trace
      (aloc_of_reason reason)
      ?desc:
        ( if use_desc then
          Some (desc_of_reason ~unwrap:false reason)
        else
          None )
      ?annot_loc:(annot_aloc_of_reason reason)
      t

  (* set the position of the given def type from a reason *)
  and reposition cx ?trace (loc : ALoc.t) ?desc ?annot_loc t =
    let mod_reason reason =
      let reason = opt_annot_reason ?annot_loc @@ repos_reason loc reason in
      match desc with
      | Some d -> replace_desc_new_reason d reason
      | None -> reason
    in
    let rec recurse seen = function
      | OpenT (r, id) as t ->
        let reason = mod_reason r in
        let use_desc = Base.Option.is_some desc in
        let constraints = Context.find_graph cx id in
        begin
          match constraints with
          (* TODO: In the FullyResolved case, repositioning will cause us to "lose"
       the fully resolved status. We should be able to preserve it. *)
          | Resolved (use_op, t)
          | FullyResolved (use_op, t) ->
            (* A tvar may be resolved to a type that has special repositioning logic,
         like UnionT. We want to recurse to pick up that logic, but must be
         careful as the union may refer back to the tvar itself, causing a loop.
         To break the loop, we pass down a map of "already seen" tvars. *)
            (match IMap.find_opt id seen with
            | Some t -> t
            | None ->
              (* Create a fresh tvar which can be passed in `seen` *)
              let mk_tvar_where =
                if is_derivable_reason r then
                  Tvar.mk_derivable_where
                else
                  Tvar.mk_where
              in
              (* The resulting tvar should be fully resolved if this one is *)
              let fully_resolved =
                match constraints with
                | Resolved _ -> false
                | FullyResolved _ -> true
                | Unresolved _ -> assert_false "handled below"
              in
              mk_tvar_where cx reason (fun tvar ->
                  (* All `t` in `Resolved (_, t)` are concrete. Because `t` is a concrete
             type, `t'` is also necessarily concrete (i.e., reposition preserves
             open -> open, concrete -> concrete). The unification below thus
             results in resolving `tvar` to `t'`, so we end up with a resolved
             tvar whenever we started with one. *)
                  let t' = recurse (IMap.add id tvar seen) t in
                  (* resolve_id requires a trace param *)
                  let trace =
                    match trace with
                    | None -> Trace.unit_trace tvar (UseT (use_op, t'))
                    | Some trace ->
                      let max = Context.max_trace_depth cx in
                      Trace.rec_trace ~max tvar (UseT (use_op, t')) trace
                  in
                  let (_, id) = open_tvar tvar in
                  resolve_id cx trace ~use_op ~fully_resolved id t'))
          | Unresolved _ ->
            (* Try to re-use an already created repositioning tvar.
             * See repos_cache.ml for details. *)
            let repos_cache = Context.repos_cache cx in
            (match Repos_cache.find id reason !repos_cache with
            | Some t -> t
            | None ->
              let mk_tvar_where =
                if is_derivable_reason r then
                  Tvar.mk_derivable_where
                else
                  Tvar.mk_where
              in
              mk_tvar_where cx reason (fun tvar ->
                  repos_cache := Repos_cache.add reason t tvar !repos_cache;
                  flow_opt cx ?trace (t, ReposLowerT (reason, use_desc, UseT (unknown_use, tvar)))))
        end
      | EvalT (root, defer_use_t, id) as t ->
        (* Modifying the reason of `EvalT`, as we do for other types, is not
         enough, since it will only affect the reason of the resulting tvar.
         Instead, repositioning a `EvalT` should simulate repositioning the
         resulting tvar, i.e., flowing repositioned *lower bounds* to the
         resulting tvar. (Another way of thinking about this is that a `EvalT`
         is just as transparent as its resulting tvar.) *)
        let defer_use_t = mod_reason_of_defer_use_t mod_reason defer_use_t in
        let reason = reason_of_defer_use_t defer_use_t in
        let use_desc = Base.Option.is_some desc in
        begin
          match Cache.Eval.find_repos cx root defer_use_t id with
          | Some tvar -> tvar
          | None ->
            Tvar.mk_where cx reason (fun tvar ->
                Cache.Eval.add_repos cx root defer_use_t id tvar;
                flow_opt cx ?trace (t, ReposLowerT (reason, use_desc, UseT (unknown_use, tvar))))
        end
      | MaybeT (r, t) ->
        (* repositions both the MaybeT and the nested type. MaybeT represets `?T`.
         elsewhere, when we decompose into T | NullT | VoidT, we use the reason
         of the MaybeT for NullT and VoidT but don't reposition `t`, so that any
         errors on the NullT or VoidT point at ?T, but errors on the T point at
         T. *)
        let r = mod_reason r in
        MaybeT (r, recurse seen t)
      | OptionalT { reason; type_ = t; use_desc } ->
        let reason = mod_reason reason in
        OptionalT { reason; type_ = recurse seen t; use_desc }
      | UnionT (r, rep) ->
        let r = mod_reason r in
        let rep = UnionRep.ident_map (recurse seen) rep in
        UnionT (r, rep)
      | OpaqueT (r, opaquetype) ->
        let r = mod_reason r in
        OpaqueT
          ( r,
            {
              opaquetype with
              underlying_t = OptionUtils.ident_map (recurse seen) opaquetype.underlying_t;
              super_t = OptionUtils.ident_map (recurse seen) opaquetype.super_t;
            } )
      | ExactT (r, t) ->
        let r = mod_reason r in
        ExactT (r, recurse seen t)
      | GenericT ({ reason; bound; _ } as generic) ->
        GenericT { generic with reason = mod_reason reason; bound = recurse seen bound }
      | t -> mod_reason_of_t mod_reason t
    in
    recurse IMap.empty t

  (* Given the type of a value v, return the type term representing the `typeof v`
   annotation expression. If the type of v is a tvar, we need to take extra
   care. Annotations are designed to constrain types, and therefore should not
   themselves grow when used.

   The internal flag is expected to be true when the typeof operator has been
   synthesized by the signature generator (in types-first), rather than originate
   from a source level annotation. *)
  and mk_typeof_annotation cx ?trace reason ?(use_desc = false) ?(internal = false) t =
    let source =
      match t with
      | OpenT _ ->
        (* Ensure that `source` is a 0->1 type by creating a tvar that resolves to
           the first lower bound. If there are multiple lower bounds, the typeof
           itself is an error. *)
        Tvar.mk_where cx reason (fun t' -> flow_opt cx ?trace (t, BecomeT (reason, t')))
      | _ ->
        (* If this is not a tvar, then it should be 0->1 (see TODO). Note that
           BoundT types potentially appear unsubstituted at this point, so we can't
           emit constraints even if we wanted to. *)
        (* TODO: Even in this case, the type might recursively include tvars, which
           allows them to widen unexpectedly and may cause unpreditable behavior. *)
        t
    in
    let annot_loc =
      if internal then
        None
      else
        Some (aloc_of_reason reason)
    in
    AnnotT (opt_annot_reason ?annot_loc reason, source, use_desc)

  and get_builtin_type cx ?trace reason ?(use_desc = false) x =
    let t = get_builtin cx ?trace x reason in
    mk_instance cx ?trace reason ~use_desc t

  and get_builtin_prop_type cx ?trace reason tool =
    let x =
      React.PropType.(
        match tool with
        | ArrayOf -> "React$PropTypes$arrayOf"
        | InstanceOf -> "React$PropTypes$instanceOf"
        | ObjectOf -> "React$PropTypes$objectOf"
        | OneOf -> "React$PropTypes$oneOf"
        | OneOfType -> "React$PropTypes$oneOfType"
        | Shape -> "React$PropTypes$shape")
    in
    get_builtin_type cx ?trace reason x

  and flow_all_in_union cx trace rep u =
    (* This is required so that our caches don't treat different branches of unions as the same type *)
    let union_reason i r = replace_desc_reason (RUnionBranching (desc_of_reason r, i)) r in
    UnionRep.members rep
    |> Base.List.iteri ~f:(fun i ->
           mod_reason_of_t (union_reason i) %> mk_tuple_swapped u %> rec_flow cx trace)

  and call_args_iter f =
    List.iter (function
        | Arg t
        | SpreadArg t
        -> f t)

  (* There's a lot of code that looks at a call argument list and tries to do
   * something with one or two arguments. Usually this code assumes that the
   * argument is not a spread argument. This utility function helps with that *)
  and extract_non_spread cx ~trace = function
    | Arg t -> t
    | SpreadArg arr ->
      let reason = reason_of_t arr in
      let loc = loc_of_t arr in
      add_output cx ~trace Error_message.(EUnsupportedSyntax (loc, SpreadArgument));
      AnyT.error reason

  and set_builtin cx ?trace x t =
    let reason = builtin_reason (RCustom x) in
    let propref = Named (reason, x) in
    flow_opt
      cx
      ?trace
      (builtins cx, SetPropT (unknown_use, reason, propref, Assign, Normal, t, None))

  (* Wrapper functions around __flow that manage traces. Use these functions for
   all recursive calls in the implementation of __flow. *)

  (* Call __flow while concatenating traces. Typically this is used in code that
   propagates bounds across type variables, where nothing interesting is going
   on other than concatenating subtraces to make longer traces to describe
   transitive data flows *)
  and join_flow cx ts (t1, t2) =
    let max = Context.max_trace_depth cx in
    __flow cx (t1, t2) (Trace.concat_trace ~max ts)

  (* Call __flow while embedding traces. Typically this is used in code that
   simplifies a constraint to generate subconstraints: the current trace is
   "pushed" when recursing into the subconstraints, so that when we finally hit
   an error and walk back, we can know why the particular constraints that
   caused the immediate error were generated. *)
  and rec_flow cx trace (t1, t2) =
    let max = Context.max_trace_depth cx in
    __flow cx (t1, t2) (Trace.rec_trace ~max t1 t2 trace)

  and rec_flow_t cx trace ~use_op (t1, t2) = rec_flow cx trace (t1, UseT (use_op, t2))

  and flow_opt_p cx ?trace ~use_op ~report_polarity lreason ureason propref = function
    (* unification cases *)
    | (Field (_, lt, Polarity.Neutral), Field (_, ut, Polarity.Neutral)) ->
      unify_opt cx ?trace ~use_op lt ut
    (* directional cases *)
    | (lp, up) ->
      let x =
        match propref with
        | Named (_, x) -> Some x
        | Computed _ -> None
      in
      (match (Property.read_t lp, Property.read_t up) with
      | (Some lt, Some ut) -> flow_opt cx ?trace (lt, UseT (use_op, ut))
      | (None, Some _) when report_polarity ->
        add_output
          cx
          ?trace
          (Error_message.EPropPolarityMismatch
             ((lreason, ureason), x, (Property.polarity lp, Property.polarity up), use_op))
      | _ -> ());
      (match (Property.write_t lp, Property.write_t up) with
      | (Some lt, Some ut) -> flow_opt cx ?trace (ut, UseT (use_op, lt))
      | (None, Some _) when report_polarity ->
        add_output
          cx
          ?trace
          (Error_message.EPropPolarityMismatch
             ((lreason, ureason), x, (Property.polarity lp, Property.polarity up), use_op))
      | _ -> ())

  and rec_flow_p cx trace ~use_op ?(report_polarity = true) =
    flow_opt_p cx ~trace ~use_op ~report_polarity

  (* Ideally this function would not be required: either we call `flow` from
   outside without a trace (see below), or we call one of the functions above
   with a trace. However, there are some functions that need to call __flow,
   which are themselves called both from outside and inside (with or without
   traces), so they call this function instead. *)
  and flow_opt cx ?trace (t1, t2) =
    let trace =
      match trace with
      | None -> Trace.unit_trace t1 t2
      | Some trace ->
        let max = Context.max_trace_depth cx in
        Trace.rec_trace ~max t1 t2 trace
    in
    __flow cx (t1, t2) trace

  and flow_opt_t cx ~use_op ?trace (t1, t2) = flow_opt cx ?trace (t1, UseT (use_op, t2))

  (* Externally visible function for subtyping. *)
  (* Calls internal entry point and traps runaway recursion. *)
  and flow cx (lower, upper) =
    try flow_opt cx (lower, upper) with
    | RecursionCheck.LimitExceeded trace ->
      (* log and continue *)
      let rl = reason_of_t lower in
      let ru = reason_of_use_t upper in
      let reasons =
        if is_use upper then
          (ru, rl)
        else
          FlowError.ordered_reasons (rl, ru)
      in
      add_output cx ~trace (Error_message.ERecursionLimit reasons)
    | ex ->
      (* rethrow *)
      raise ex

  and flow_t cx (t1, t2) = flow cx (t1, UseT (unknown_use, t2))

  and flow_p cx ~use_op lreason ureason propref props =
    flow_opt_p cx ~use_op ~report_polarity:true lreason ureason propref props

  and tvar_with_constraint cx ?trace ?(derivable = false) u =
    let reason = reason_of_use_t u in
    let mk_tvar_where =
      if derivable then
        Tvar.mk_derivable_where
      else
        Tvar.mk_where
    in
    mk_tvar_where cx reason (fun tvar -> flow_opt cx ?trace (tvar, u))

  (* Wrapper functions around __unify that manage traces. Use these functions for
   all recursive calls in the implementation of __unify. *)
  and rec_unify cx trace ~use_op ?(unify_any = false) t1 t2 =
    let max = Context.max_trace_depth cx in
    __unify cx ~use_op ~unify_any t1 t2 (Trace.rec_trace ~max t1 (UseT (use_op, t2)) trace)

  and unify_opt cx ?trace ~use_op ?(unify_any = false) t1 t2 =
    let trace =
      match trace with
      | None -> Trace.unit_trace t1 (UseT (unknown_use, t2))
      | Some trace ->
        let max = Context.max_trace_depth cx in
        Trace.rec_trace ~max t1 (UseT (unknown_use, t2)) trace
    in
    __unify cx ~use_op ~unify_any t1 t2 trace

  (* Externally visible function for unification. *)
  (* Calls internal entry point and traps runaway recursion. *)
  and unify cx t1 t2 =
    try unify_opt cx ~use_op:unknown_use ~unify_any:true t1 t2 with
    | RecursionCheck.LimitExceeded trace ->
      (* log and continue *)
      let reasons = FlowError.ordered_reasons (reason_of_t t1, reason_of_t t2) in
      add_output cx ~trace (Error_message.ERecursionLimit reasons)
    | ex ->
      (* rethrow *)
      raise ex

  and continue cx trace t = function
    | Lower (use_op, l) -> rec_flow cx trace (l, UseT (use_op, t))
    | Upper u -> rec_flow cx trace (t, u)

  and continue_repos cx trace reason ?(use_desc = false) t = function
    | Lower (use_op, l) -> rec_flow cx trace (t, ReposUseT (reason, use_desc, use_op, l))
    | Upper u -> rec_flow cx trace (t, ReposLowerT (reason, use_desc, u))

  and flow_predicate_func =
    let rec subst_map (n, map) = function
      | ((Some k, _) :: ps1, (Some v, _) :: ps2) ->
        let map' =
          if k <> v then
            SMap.add k (v, []) map
          else
            (* Skip trivial entry *)
            map
        in
        subst_map (n + 1, map') (ps1, ps2)
      | (_, []) -> Ok map
      | ([], ps2) ->
        (* Flag an error if predicate counts do not coincide
           TODO: somehow the original flow needs to be propagated as well *)
        let n2 = n + List.length ps2 in
        Error (`ArityMismatch (n, n2))
      | ((None, _) :: _, _)
      | (_, (None, _) :: _) ->
        Error `NoParamNames
    in
    fun cx trace use_op (lreason, ft1) (ureason, ft2) ->
      match subst_map (0, SMap.empty) (ft1.params, ft2.params) with
      | Error (`ArityMismatch (n1, n2)) ->
        let mod_reason n =
          replace_desc_reason (RCustom (spf "predicate function with %d arguments" n))
        in
        let error =
          Error_message.EFunPredCustom
            ( (mod_reason n1 lreason, mod_reason n2 ureason),
              "Predicate function is incompatible with" )
        in
        add_output cx ~trace error
      | Error `NoParamNames ->
        let error = Error_message.(EInternal (aloc_of_reason ureason, PredFunWithoutParamNames)) in
        add_output cx ~trace error
      | Ok map ->
        let reason =
          update_desc_new_reason
            (fun desc -> RCustom (spf "predicate of %s" (string_of_desc desc)))
            (reason_of_t ft2.return_t)
        in
        (* We need to treat the return type of the predicated function as an
           annotation, to ensure that the LHS return type is checked against it,
           if ft2.return_t happens to be an OpenT. *)
        let out =
          if SMap.is_empty map then
            UseT (use_op, annot false ft2.return_t)
          else
            SubstOnPredT (use_op, reason, map, annot false ft2.return_t)
        in
        rec_flow cx trace (ft1.return_t, out)

  include AssertGround
  include CheckPolarity
  include TrustChecking
end

module rec FlowJs : Flow_common.S = struct
  module React = React_kit.Kit (FlowJs)
  module AssertGround = Assert_ground.Kit (FlowJs)
  module CheckPolarity = Check_polarity.Kit (FlowJs)
  module TrustKit = Trust_checking.TrustKit (FlowJs)
  module CustomFun = Custom_fun_kit.Kit (FlowJs)
  module ObjectKit = Object_kit.Kit (FlowJs)
  include M__flow (React) (AssertGround) (CheckPolarity) (TrustKit) (CustomFun) (ObjectKit)

  let add_output = add_output

  let union_of_ts = union_of_ts

  let check_with_generics = check_with_generics

  let match_this_binding = match_this_binding

  let widen_obj_type = ObjectKit.widen_obj_type
end

include FlowJs

(************* end of slab **************************************************)

(* Would rather this live elsewhere, but here because module DAG. *)
let mk_default cx reason =
  Default.fold
    ~expr:(fun t -> t)
    ~cons:(fun t1 t2 ->
      Tvar.mk_where cx reason (fun tvar ->
          flow_t cx (t1, tvar);
          flow_t cx (t2, tvar)))
    ~selector:(fun r t sel ->
      Tvar.mk_no_wrap_where cx r (fun tvar -> eval_selector cx r t sel tvar (Reason.mk_id ())))
