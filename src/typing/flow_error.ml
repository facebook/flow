(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Type
open Utils_js
open Reason

(** we keep a stack of reasons representing the operations
    taking place when flows are performed. the top op reason
    is used in messages for errors that take place during its
    residence.
  *)
module Ops : sig
  val clear : unit -> reason list
  val push : reason -> unit
  val pop : unit -> unit
  val peek : unit -> reason option
  val get : unit -> reason list
  val set : reason list -> unit
end = struct
  let ops = ref []
  let clear () = let orig = !ops in ops := []; orig
  let push r = ops := r :: !ops
  let pop () = ops := List.tl !ops
  let peek () = match !ops with r :: _ -> Some r | [] -> None
  let get () = !ops
  let set _ops = ops := _ops
end

type error_message =
  | EIncompatible of Type.t * Type.use_t
  | EIncompatibleDefs of reason * reason
  | EIncompatibleProp of Type.t * Type.use_t * reason
  | EDebugPrint of reason * string
  | EImportValueAsType of reason * string
  | EImportTypeAsTypeof of reason * string
  | EImportTypeAsValue of reason * string
  | EImportTypeofNamespace of reason * string * string
  | ENoDefaultExport of reason * string * string option
  | EOnlyDefaultExport of reason * string
  | ENoNamedExport of reason * string * string option
  | EMissingTypeArgs of reason * Type.typeparam list
  | EValueUsedAsType of (reason * reason)
  | EMutationNotAllowed of { reason: reason; reason_op: reason }
  | EExpectedStringLit of (reason * reason) * string * string Type.literal
  | EExpectedNumberLit of (reason * reason) * Type.number_literal * Type.number_literal Type.literal
  | EExpectedBooleanLit of (reason * reason) * bool * bool option
  | EPropNotFound of (reason * reason) * use_op
  | EPropAccess of (reason * reason) * string option * Type.property * Type.rw
  | EPropPolarityMismatch of (reason * reason) * string option * (Type.polarity * Type.polarity)
  | EPolarityMismatch of Type.typeparam * Type.polarity
  | EStrictLookupFailed of (reason * reason) * reason * string option
  | EFunCallParam of (reason * reason)
  | EFunCallThis of reason * reason * reason
  | EFunReturn of (reason * reason)
  | EFunImplicitReturn of (reason * reason)
  | EAddition of (reason * reason)
  | EAdditionMixed of reason
  | ECoercion of (reason * reason)
  | EComparison of (reason * reason)
  | ETupleArityMismatch of (reason * reason) * int * int
  | ENonLitArrayToTuple of (reason * reason)
  | ETupleOutOfBounds of (reason * reason) * int * int
  | ETupleUnsafeWrite of (reason * reason)
  | ESpeculationFailed of Type.t * Type.use_t * (reason * error_message) list
  | ESpeculationAmbiguous of (reason * reason) * (int * reason) * (int * reason) * reason list
  | EIncompatibleWithExact of (reason * reason)
  | EUnsupportedExact of (reason * reason)
  | EIdxArity of reason
  | EIdxUse1 of reason
  | EIdxUse2 of reason
  | EUnexpectedThisType of Loc.t
  | EInvalidRestParam of reason
  | ETypeParamArity of Loc.t * int
  | ETypeParamMinArity of Loc.t * int
  | ETooManyTypeArgs of reason * reason * int
  | ETooFewTypeArgs of reason * reason * int
  | EPropertyTypeAnnot of Loc.t
  | EExportsAnnot of Loc.t
  | EUnsupportedKeyInObjectType of Loc.t
  | EPredAnnot of Loc.t
  | ERefineAnnot of Loc.t
  | EUnexpectedTypeof of Loc.t
  | ECustom of (reason * reason) * string
  | EInternal of Loc.t * internal_error
  | EUnsupportedSyntax of Loc.t * unsupported_syntax
  | EIllegalName of Loc.t
  | EUseArrayLiteral of Loc.t
  | EMissingAnnotation of reason
  | EBindingError of binding_error * Loc.t * string * Scope.Entry.t
  | ERecursionLimit of (reason * reason)
  | EModuleOutsideRoot of Loc.t * string
  | EExperimentalDecorators of Loc.t
  | EExperimentalClassProperties of Loc.t * bool
  | EUnsafeGetSet of Loc.t
  | EExperimentalExportStarAs of Loc.t
  | EIndeterminateModuleType of Loc.t
  | EUnreachable of Loc.t
  | EInvalidSpread of { reason: reason; reason_op: reason }
  | EInvalidTypeof of Loc.t * string
  | EBinaryInLHS of reason
  | EBinaryInRHS of reason
  | EArithmeticOperand of reason
  | EForInRHS of reason
  | EObjectComputedPropertyAccess of (reason * reason)
  | EObjectComputedPropertyAssign of (reason * reason)
  | EInvalidLHSInAssignment of Loc.t
  | EIncompatibleWithUseOp of reason * reason * use_op
  | EUnsupportedImplements of reason
  | EReactKit of (reason * reason) * React.tool
  | EFunctionCallExtraArg of (reason * reason * int)
  | EUnsupportedSetProto of reason
  | EDuplicateModuleProvider of {
      module_name: string;
      provider: Loc.filename;
      conflict: Loc.filename
    }
  | EParseError of Loc.t * Parse_error.t
  | EDocblockError of Loc.t * docblock_error
  | EUnusedSuppression of Loc.t

and binding_error =
  | ENameAlreadyBound
  | EReferencedBeforeDeclaration
  | ETypeInValuePosition
  | ETypeAliasInValuePosition
  | EConstReassigned
  | EConstParamReassigned
  | EImportReassigned

and docblock_error =
  | MultipleFlowAttributes
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option

and internal_error =
  | PackageHeapNotFound of string
  | AbnormalControlFlow
  | UncaughtException of exn
  | MethodNotAFunction
  | OptionalMethod
  | OpenPredWithoutSubst
  | PredFunWithoutParamNames
  | UnsupportedGuardPredicate of string
  | BreakEnvMissingForCase
  | PropertyDescriptorPropertyCannotBeRead
  | ForInLHS
  | ForOfLHS
  | InstanceLookupComputed
  | PropRefComputedOpen
  | PropRefComputedLiteral
  | ShadowReadComputed
  | ShadowWriteComputed
  | RestParameterNotIdentifierPattern
  | InterfaceTypeSpread
  | InferJobException of exn
  | MergeJobException of exn

and unsupported_syntax =
  | ComprehensionExpression
  | GeneratorExpression
  | MetaPropertyExpression
  | ObjectPropertyLiteralNonString
  | ObjectPropertyGetSet
  | ObjectPropertyComputedGetSet
  | InvariantSpreadArgument
  | ClassPropertyLiteral
  | ClassPropertyComputed
  | ClassExtendsMultiple
  | ReactCreateClassPropertyNonInit
  | RequireDynamicArgument
  | RequireLazyDynamicArgument
  | CatchParameterAnnotation
  | CatchParameterDeclaration
  | DestructuringObjectPropertyLiteralNonString
  | DestructuringExpressionPattern
  | PredicateDeclarationForImplementation
  | PredicateDeclarationWithoutExpression
  | PredicateDeclarationAnonymousParameters
  | PredicateInvalidBody
  | PredicateVoidReturn
  | MultipleIndexers
  | SpreadArgument
  | ImportDynamicArgument

let rec locs_of_use_op acc = function
  | FunCallThis reason -> (loc_of_reason reason)::acc
  | PropertyCompatibility
      (_, lower_obj_reason, upper_obj_reason, use_op) ->
    let lower_loc = loc_of_reason lower_obj_reason in
    let upper_loc = loc_of_reason upper_obj_reason in
    locs_of_use_op (lower_loc::upper_loc::acc) use_op
  | Addition
  | Coercion
  | FunImplicitReturn
  | FunCallParam
  | FunReturn
  | TypeRefinement
  | UnknownUse
  | Internal _
  | MissingTupleElement _ -> acc

let locs_of_error_message = function
  | EIncompatible (l, u) ->
      let reason_l = reason_of_t l in
      let reason_u = reason_of_use_t u in
      [loc_of_reason reason_l; loc_of_reason reason_u]
  | EIncompatibleDefs (reason_l, reason_u) ->
      [loc_of_reason reason_l; loc_of_reason reason_u]
  | EIncompatibleProp (l, u, reason) ->
      let reason_l = reason_of_t l in
      let reason_u = reason_of_use_t u in
      [loc_of_reason reason; loc_of_reason reason_l; loc_of_reason reason_u]
  | EDebugPrint (reason, _) -> [loc_of_reason reason]
  | EImportValueAsType (reason, _) -> [loc_of_reason reason]
  | EImportTypeAsTypeof (reason, _) -> [loc_of_reason reason]
  | EImportTypeAsValue (reason, _) -> [loc_of_reason reason]
  | EImportTypeofNamespace (reason, _, _) -> [loc_of_reason reason]
  | ENoDefaultExport (reason, _, _) -> [loc_of_reason reason]
  | EOnlyDefaultExport (reason, _) -> [loc_of_reason reason]
  | ENoNamedExport (reason, _, _) -> [loc_of_reason reason]
  | EMissingTypeArgs (reason, _) -> [loc_of_reason reason]
  | EValueUsedAsType (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EMutationNotAllowed { reason; reason_op } ->
      [loc_of_reason reason_op; loc_of_reason reason]
  | EExpectedStringLit ((reason1, reason2), _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EExpectedNumberLit ((reason1, reason2), _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EExpectedBooleanLit ((reason1, reason2), _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EPropNotFound ((reason1, reason2), use_op) ->
      (loc_of_reason reason1)::(loc_of_reason reason2)::(locs_of_use_op [] use_op)
  | EPropAccess ((reason1, reason2), _, _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EPropPolarityMismatch ((reason1, reason2), _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EPolarityMismatch (tp, _) -> [loc_of_reason tp.reason]
  | EStrictLookupFailed ((reason1, reason2), _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EFunCallParam (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EFunCallThis (_, reason_u, reason_call) ->
      [loc_of_reason reason_u; loc_of_reason reason_call]
  | EFunReturn (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EFunImplicitReturn (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EAddition (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EAdditionMixed (reason) -> [loc_of_reason reason]
  | ECoercion (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EComparison (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | ETupleArityMismatch ((reason1, reason2), _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | ENonLitArrayToTuple (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | ETupleOutOfBounds ((reason1, reason2), _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | ETupleUnsafeWrite (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | ESpeculationFailed (l, u, _) ->
      let reason1 = reason_of_t l in
      let reason2 = reason_of_use_t u in
      [loc_of_reason reason1; loc_of_reason reason2]
  | ESpeculationAmbiguous ((reason1, reason2), _, _, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EIncompatibleWithExact (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EUnsupportedExact (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EIdxArity (reason) -> [loc_of_reason reason]
  | EIdxUse1 (reason) -> [loc_of_reason reason]
  | EIdxUse2 (reason) -> [loc_of_reason reason]
  | EUnexpectedThisType (loc) -> [loc]
  | EInvalidRestParam (reason) -> [loc_of_reason reason]
  | ETypeParamArity (loc, _) -> [loc]
  | ETypeParamMinArity (loc, _) -> [loc]
  | ETooManyTypeArgs (reason1, reason2, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | ETooFewTypeArgs (reason1, reason2, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EPropertyTypeAnnot (loc) -> [loc]
  | EExportsAnnot (loc) -> [loc]
  | EUnsupportedKeyInObjectType (loc) -> [loc]
  | EPredAnnot (loc) -> [loc]
  | ERefineAnnot (loc) -> [loc]
  | EUnexpectedTypeof (loc) -> [loc]
  | ECustom ((reason1, reason2), _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EInternal (loc, _) -> [loc]
  | EUnsupportedSyntax (loc, _) -> [loc]
  | EIllegalName (loc) -> [loc]
  | EUseArrayLiteral (loc) -> [loc]
  | EMissingAnnotation (reason) -> [loc_of_reason reason]
  | EBindingError (_, loc, _, entry) ->
      [loc; Scope.Entry.entry_loc entry]
  | ERecursionLimit (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EModuleOutsideRoot (loc, _) -> [loc]
  | EExperimentalDecorators (loc) -> [loc]
  | EExperimentalClassProperties (loc, _) -> [loc]
  | EUnsafeGetSet (loc) -> [loc]
  | EExperimentalExportStarAs (loc) -> [loc]
  | EIndeterminateModuleType (loc) -> [loc]
  | EUnreachable (loc) -> [loc]
  | EInvalidSpread { reason; reason_op } ->
      [loc_of_reason reason_op; loc_of_reason reason]
  | EInvalidTypeof (loc, _) -> [loc]
  | EBinaryInLHS (reason) -> [loc_of_reason reason]
  | EBinaryInRHS (reason) -> [loc_of_reason reason]
  | EArithmeticOperand (reason) -> [loc_of_reason reason]
  | EForInRHS (reason) -> [loc_of_reason reason]
  | EObjectComputedPropertyAccess (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EObjectComputedPropertyAssign (reason1, reason2) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EInvalidLHSInAssignment (loc) -> [loc]
  | EIncompatibleWithUseOp (reason1, reason2, use_op) ->
      (loc_of_reason reason1)::(loc_of_reason reason2)::(locs_of_use_op [] use_op)
  | EUnsupportedImplements (reason) -> [loc_of_reason reason]
  | EReactKit ((reason1, reason2), _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EFunctionCallExtraArg (reason1, reason2, _) ->
      [loc_of_reason reason1; loc_of_reason reason2]
  | EUnsupportedSetProto (reason) -> [loc_of_reason reason]
  | EDuplicateModuleProvider { module_name = _; provider; conflict; } ->
      [
        Loc.({ none with source = Some conflict });
        Loc.({ none with source = Some provider })
      ]
  | EParseError (loc, _) -> [loc]
  | EDocblockError (loc, _) -> [loc]
  | EUnusedSuppression (loc) -> [loc]

let loc_of_error ~op msg =
  match op with
  | Some reason -> loc_of_reason reason
  | None -> List.hd (locs_of_error_message msg)

(* decide reason order based on UB's flavor and blamability *)
let ordered_reasons rl ru =
  if (is_blamable_reason ru && not (is_blamable_reason rl))
  then ru, rl
  else rl, ru

let ordered_reasons_of_types l u =
  let rl = reason_of_t l in
  let ru = reason_of_use_t u in
  if is_use u then ru, rl else ordered_reasons rl ru

let rec error_of_msg ~trace_reasons ~op ~source_file =
  let open Errors in

  let mk_info reason extras =
    loc_of_reason reason, string_of_desc (desc_of_reason reason) :: extras
  in

  let info_of_reason r = mk_info r [] in

  let trace_infos = List.map info_of_reason trace_reasons in

  (* only on use-types - guard calls with is_use t *)
  let err_operation = function
    | UseT (_, t) ->
      failwith (spf "err_operation called on def type %s" (string_of_ctor t))

    | GetPropT _ -> "Property cannot be accessed on"
    | SetPropT _ -> "Property cannot be assigned on"
    | MethodT _ -> "Method cannot be called on"
    | CallT _ -> "Function cannot be called on"
    | ConstructorT _ -> "Constructor cannot be called on"
    | GetElemT _ -> "Computed property/element cannot be accessed on"
    | SetElemT _ -> "Computed property/element cannot be assigned on"
    | CallElemT _ -> "Computed property/element cannot be called on"
    | ElemT (_, _, ReadElem _) -> "Element cannot be accessed with"
    | ElemT (_, _, WriteElem _) -> "Element cannot be assigned with"
    | ElemT (_, _, CallElem _) -> "Element cannot be called with"
    | ObjAssignFromT (_, _, _, _, ObjSpreadAssign) ->
      "Expected array instead of"
    | ObjAssignToT _
    | ObjAssignFromT _ -> "Expected object instead of"
    | ObjRestT _ -> "Expected object instead of"
    | ObjSealT _ -> "Expected object instead of"
    | ArrRestT _ -> "Expected array instead of"
    | SuperT _ -> "Cannot inherit"
    | MixinT _ -> "Expected class instead of"
    | SpecializeT _ -> "Expected polymorphic type instead of"
    | ThisSpecializeT _ -> "Expected class instead of"
    | VarianceCheckT _ -> "Expected polymorphic type instead of"
    | LookupT _ -> "Property not found in"
    | GetKeysT _ -> "Expected object instead of"
    | HasOwnPropT _ -> "Property not found in"
    | UnaryMinusT _ -> "Expected number instead of"
    | MapTypeT (_, kind, _, _) ->
      (match kind with
      | TupleMap -> "Expected Iterable instead of"
      | ObjectMap
      | ObjectMapi -> "Expected object instead of")
    | CallLatentPredT _ -> "Expected predicated function instead of"
    | ResolveSpreadT (_, {rrt_resolve_to; _}) ->
      begin match rrt_resolve_to with
      | ResolveSpreadsToTuple _
      | ResolveSpreadsToArray _
      | ResolveSpreadsToArrayLiteral _
        -> "Expected spread element to be an iterable instead of"
      | ResolveSpreadsToMultiflowCallFull _
      | ResolveSpreadsToMultiflowSubtypeFull _
      | ResolveSpreadsToMultiflowPartial _
      | ResolveSpreadsToCallT _
        -> "Expected spread argument to be an iterable instead of"
      end
    | TypeAppVarianceCheckT _ -> "Expected polymorphic type instead of"
    | ObjSpreadT _ -> "Cannot spread properties from"
    (* unreachable or unclassified use-types. until we have a mechanical way
       to verify that all legit use types are listed above, we can't afford
       to throw on a use type, so mark the error instead *)
    | t ->
      spf "Type is incompatible with (unclassified use type: %s)"
        (string_of_use_ctor t)
  in

  let err_value = function
    | DefT (_, NullT) -> " possibly null value"
    | DefT (_, VoidT) -> " possibly undefined value"
    | DefT (_, MaybeT _) -> " possibly null or undefined value"
    | DefT (_, IntersectionT _)
    | DefT (_, MixedT Empty_intersection) -> " any member of intersection type"
    | _ -> ""
  in

  let err_msg_use l u = spf "%s%s" (err_operation u) (err_value l) in

  let msg_export export_name =
    if export_name = "default"
    then export_name
    else spf "`%s`" export_name
  in

  let poly_minimum_arity xs = List.(
    xs |> filter (fun typeparam -> typeparam.default = None) |> length
  ) in

  let typecheck_error_with_core_infos ?(suppress_op=false) ?extra core_msgs =
    let core_reasons = List.map fst core_msgs in
    let core_infos = List.map (fun (r, msgs) -> mk_info r msgs) core_msgs in

    (* Since pointing to endpoints in the library without any information on
       the code that uses those endpoints inconsistently is useless, we point
       to the file containing that code instead. Ideally, improvements in
       error reporting would cause this case to never arise.

       Additionally, we never suppress ops when this happens, because that is
       our last chance at relevant context. *)
    let lib_infos, suppress_op = if List.for_all is_lib_reason core_reasons then
        let loc = Loc.({ none with source = Some source_file }) in
        [loc, ["inconsistent use of library definitions"]], false
      else [], suppress_op
    in
    (* NOTE: We include the operation's reason in the error message, unless it
       overlaps *both* endpoints, exactly matches r1, or overlaps r1's origin *)
    let op_info = if suppress_op then None else
      match op, core_reasons with
      | Some r, r1::_ ->
        if r = r1 then None
        else if List.for_all (reasons_overlap r) core_reasons then None
        else Some (info_of_reason r)
      | _ -> None
    in
    (* main info is core info with optional lib line prepended, and optional
       extra info appended. ops/trace info is held separately in error *)
    let msg_infos = lib_infos @ core_infos in
    mk_error ?op_info ~trace_infos ?extra msg_infos
  in

  let typecheck_error msg ?suppress_op ?extra (r1, r2) =
    (* make core info from reasons, message, and optional extra infos *)
    let core_msgs = [
      r1, [msg];
      r2, [];
    ] in
    typecheck_error_with_core_infos ?suppress_op ?extra core_msgs
  in

  let prop_polarity_error reasons x p1 p2 =
    let prop_name = match x with
    | Some x -> spf "property `%s`" x
    | None -> "computed property"
    in
    let msg = spf "%s %s incompatible with %s use in"
      (String.capitalize_ascii (Polarity.string p1))
      prop_name
      (Polarity.string p2)
    in
    typecheck_error msg reasons
  in

  let extra_info_of_use_op l_reason u_reason nested_extra msg wrapper_msg =
    let infos = [
      mk_info l_reason [msg];
      mk_info u_reason []
    ] in
    [
      InfoNode (
        [Loc.none, [wrapper_msg]],
        [
          if nested_extra = []
          then InfoLeaf infos
          else InfoNode (infos, nested_extra)
        ]
      )
    ]
  in

  let rec unwrap_use_ops ((l_reason, u_reason), nested_extra, msg) = function
  | PropertyCompatibility
      (prop_name, lower_obj_reason, upper_obj_reason, use_op) ->
    let extra = extra_info_of_use_op
      l_reason u_reason nested_extra msg
      (spf "Property `%s` is incompatible:" prop_name)
    in
    let msg = "This type is incompatible with" in
    unwrap_use_ops ((lower_obj_reason, upper_obj_reason), extra, msg) use_op
  | FunReturn ->
    let msg =
      "This type is incompatible with the expected return type of" in
    (l_reason, u_reason), nested_extra, msg
  | FunCallParam ->
    let r1, r2, msg = if is_lib_reason l_reason then
      u_reason, l_reason, "This type is incompatible with an argument type of"
    else
      l_reason, u_reason,
        "This type is incompatible with the expected param type of"
    in
    (r1, r2), nested_extra, msg
  | _ ->
    (l_reason, u_reason), nested_extra, msg
  in

  function
  | EIncompatible (l, u) ->
      let rl = reason_of_t l in
      let ru = reason_of_use_t u in
      typecheck_error (err_msg_use l u) (ru, rl)

  | EIncompatibleDefs (reason_l, reason_u) ->
      typecheck_error "This type is incompatible with" (ordered_reasons reason_l reason_u)

  | EIncompatibleProp (l, u, reason_prop) ->
      (* when unexpected types flow into a GetPropT/SetPropT (e.g. void or
         other non-object-ish things), then use `reason_prop`, which
         represents the reason for the prop itself, not the lookup action. *)
      let reasons = reason_prop, reason_of_t l in
      typecheck_error (err_msg_use l u) reasons

  | EDebugPrint (r, str) ->
      mk_error ~trace_infos [mk_info r [str]]

  | EImportValueAsType (r, export_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "The %s export is a value, but not a type. `import type` only works \
         on type exports like type aliases, interfaces, and classes. If you \
         intended to import the type *of* a value, please use `import \
         typeof` instead."
        (msg_export export_name)]]

  | EImportTypeAsTypeof (r, export_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "The %s export is a type, but not a value. `import typeof` only \
         works on value exports like classes, vars, lets, etc. If you \
         intended to import a type alias or interface, please use `import \
         type` instead."
        (msg_export export_name)]]

  | EImportTypeAsValue (r, export_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "`%s` is a type, but not a value. In order to import it, please use \
         `import type`."
        export_name]]

  | EImportTypeofNamespace (r, local_name, module_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "This is invalid syntax. Maybe you meant: `import type \
         %s from %S`?"
        local_name
        module_name]]

  | ENoDefaultExport (r, module_name, suggestion) ->
      let msg = "This module has no default export." in
      let msg = match suggestion with
      | None -> msg
      | Some x ->
        msg ^ (spf " Did you mean `import {%s} from \"%s\"`?" x module_name)
      in
      mk_error ~trace_infos [mk_info r [msg]]

  | EOnlyDefaultExport (r, export_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "This module only has a default export. Did you mean \
         `import %s from ...`?"
        export_name]]

  | ENoNamedExport (r, export_name, suggestion) ->
      let msg =
        spf "This module has no named export called `%s`." export_name in
      let msg = match suggestion with
      | None -> msg
      | Some x -> msg ^ (spf " Did you mean `%s`?" x)
      in
      mk_error ~trace_infos [mk_info r [msg]]

  | EMissingTypeArgs (r, params) ->
      let min = poly_minimum_arity params in
      let max = List.length params in
      let arity, args = if min = max
        then spf "%d" max, if max = 1 then "argument" else "arguments"
        else spf "%d-%d" min max, "arguments"
      in
      mk_error ~trace_infos [mk_info r [spf
        "Application of polymorphic type needs \
         <list of %s %s>. (Can use `*` for inferrable ones)"
        arity args]]

  | EValueUsedAsType reasons ->
      typecheck_error
        "Ineligible value used in/as type annotation \
         (did you forget 'typeof'?)"
        reasons

  | EMutationNotAllowed { reason; reason_op } ->
      typecheck_error "Mutation not allowed on" (reason_op, reason)

  | EExpectedStringLit (reasons, expected, actual) ->
      let msg = match actual with
      | Literal (None, actual) ->
          spf "Expected string literal `%s`, got `%s` instead"
            expected actual
      | Truthy | AnyLiteral ->
          spf "Expected string literal `%s`" expected
      | Literal (Some sense, actual) ->
          spf "This %s check always %s because `%s` is not the same string as `%s`"
            (if sense then "===" else "!==")
            (if sense then "fails" else "succeeds")
            actual
            expected
      in
      typecheck_error msg reasons

  | EExpectedNumberLit (reasons, (expected, _), actual) ->
      let msg = match actual with
      | Literal (None, (actual, _)) ->
          spf "Expected number literal `%.16g`, got `%.16g` instead"
            expected actual
      | Truthy | AnyLiteral ->
          spf "Expected number literal `%.16g`" expected
      | Literal (Some sense, (actual, _)) ->
          spf "This %s check always %s because `%.16g` is not the same number as `%.16g`"
            (if sense then "===" else "!==")
            (if sense then "fails" else "succeeds")
            actual
            expected
      in
      typecheck_error msg reasons

  | EExpectedBooleanLit (reasons, expected, actual) ->
      let msg = match actual with
      | Some actual ->
          spf "Expected boolean literal `%b`, got `%b` instead"
            expected actual
      | None -> spf "Expected boolean literal `%b`" expected
      in
      typecheck_error msg reasons

  | EPropNotFound (reasons, use_op) ->
      let reasons, extra, msg =
        unwrap_use_ops (reasons, [], "Property not found in") use_op in
      typecheck_error ~extra msg reasons

  | EPropAccess (reasons, x, prop, rw) ->
      prop_polarity_error reasons x
        (Property.polarity prop)
        (Polarity.of_rw rw)

  | EPropPolarityMismatch (reasons, x, (p1, p2)) ->
      prop_polarity_error reasons x p1 p2

  | EPolarityMismatch (tp, p) ->
      mk_error ~trace_infos [mk_info tp.reason [spf
        "%s position (expected `%s` to occur only %sly)"
        (Polarity.string p)
        tp.name
        (Polarity.string tp.polarity)]]

  | EStrictLookupFailed (reasons, lreason, x) ->
    (* if we're looking something up on the global/builtin object, then tweak
       the error to say that `x` doesn't exist. We can tell this is the
       global object because that should be the only object created with
       `builtin_reason` instead of an actual location (see `Init_js.init`). *)
    if is_builtin_reason lreason
    then
      let msg = match x with
      | Some x when is_internal_module_name x -> "Required module not found"
      | _ -> "Could not resolve name"
      in
      mk_error ~trace_infos [mk_info (fst reasons) [msg]]
    else
      let msg = match x with
      | Some "$call" -> "Callable signature not found in"
      | Some "$key" | Some "$value" -> "Indexable signature not found in"
      | _ -> "Property not found in"
      in
      typecheck_error msg reasons

  | EFunCallParam (lreason, ureason) ->
      (* Special case: if the lower bound is from a libdef, then flip
         the message and the reasons, so we point at something in user code.

         For example, consider a libdef like:

           declare function foo(): Promise<string>

         and user code like:

           let callback = (x: number) => ...;
           foo().then(callback)

         The user's `callback` gets "called" with a `string` arg. `lreason` is
         the `string` from `Promise<string>` in the libdef, and `ureason` is
         `number` inside `callback`. *)
      let r1, r2, msg = if is_lib_reason lreason then
        ureason, lreason, "This type is incompatible with an argument type of"
      else
        lreason, ureason,
          "This type is incompatible with the expected param type of"
      in
      (* Ops telling us that we're in the middle of a function call are
         redundant when we already know that an arg didn't match a param. *)
      let suppress_op = match op with
      | Some r ->
        begin match desc_of_reason r with
        | RFunctionCall
        | RConstructorCall
        | RMethodCall _ -> true
        | _ -> false
        end
      | None -> false
      in
      typecheck_error ~suppress_op msg (r1, r2)

  | EFunCallThis (lreason, ureason, reason_call) ->
      let msg = "This function call's `this` type is incompatible with" in
      let extra = [InfoLeaf ([
        Loc.none, ["The call's `this` type is:"];
        info_of_reason (lreason);
      ])] in
      typecheck_error msg ~extra (reason_call, ureason)

  | EFunReturn reasons ->
      typecheck_error
        "This type is incompatible with the expected return type of"
        reasons

  | EFunImplicitReturn (lreason, ureason) ->
      mk_error ~trace_infos [mk_info ureason [spf
        "This type is incompatible with an implicitly-returned %s."
        (string_of_desc (desc_of_reason lreason))]]

  | EAddition reasons ->
      typecheck_error "This type cannot be added to" reasons

  | EAdditionMixed reason ->
      mk_error ~trace_infos [mk_info reason [
        "This type cannot be used in an addition because it is unknown \
         whether it behaves like a string or a number."]]

  | ECoercion reasons ->
      typecheck_error "This type cannot be coerced to" reasons

  | EComparison reasons ->
      typecheck_error "This type cannot be compared to" reasons

  | ETupleArityMismatch (reasons, l1, l2) ->
      let msg = spf
        "Tuple arity mismatch. This tuple has %d elements and cannot flow to \
        the %d elements of"
        l1
        l2 in
      typecheck_error msg reasons

  | ENonLitArrayToTuple reasons ->
      let msg =
        "Only tuples and array literals with known elements can flow to" in
      typecheck_error msg reasons

  | ETupleOutOfBounds (reasons, length, index) ->
      let msg = spf
        "Out of bound access. This tuple has %d elements and you tried to \
        access index %d of"
        length
        index in
      typecheck_error msg reasons

  | ETupleUnsafeWrite (reasons) ->
      let msg = spf
        "Flow will only let you modify a tuple if it knows exactly which \
        element of the tuple you are mutating. Unsafe mutation of" in
      typecheck_error msg reasons

  | ESpeculationFailed (l, u, branches) ->
      let reasons = ordered_reasons_of_types l u in
      let msg = if is_use u then err_msg_use l u else "This type is incompatible with" in
      let extra = List.mapi (fun i (r, msg) ->
        let err = error_of_msg ~trace_reasons:[] ~op ~source_file msg in
        let header_infos = [
          Loc.none, [spf "Member %d:" (i + 1)];
          info_of_reason r;
          Loc.none, ["Error:"];
        ] in
        let error_infos = infos_of_error err in
        let error_extra = extra_of_error err in
        let info_list = header_infos @ error_infos in
        let info_tree = match error_extra with
          | [] -> Errors.InfoLeaf (info_list)
          | _ -> Errors.InfoNode (info_list, error_extra)
        in
        info_tree
      ) branches in
      typecheck_error msg ~extra reasons

  | ESpeculationAmbiguous ((case_r, r), (prev_i, prev_case), (i, case), case_rs) ->
      let infos = List.map info_of_reason case_rs in
      let extra = [
        InfoLeaf [
          Loc.none, [spf "Case %d may work:" (prev_i + 1)];
          info_of_reason prev_case;
        ];
        InfoLeaf [
          Loc.none, [spf
            "But if it doesn't, case %d looks promising too:"
            (i + 1)];
          info_of_reason case;
        ];
        InfoLeaf (
          (Loc.none, [spf
            "Please provide additional annotation(s) to determine whether \
             case %d works (or consider merging it with case %d):"
            (prev_i + 1)
            (i + 1)]
          )::infos
        )
      ] in
      mk_error ~trace_infos ~extra [
        (mk_info case_r ["Could not decide which case to select"]);
        (info_of_reason r)
      ]

  | EIncompatibleWithExact reasons ->
      typecheck_error "Inexact type is incompatible with exact type" reasons

  | EUnsupportedExact reasons ->
      typecheck_error "Unsupported exact type" reasons

  | EIdxArity reason ->
      mk_error ~trace_infos [mk_info reason [
        "idx() function takes exactly two params!"
      ]]

  | EIdxUse1 reason ->
      mk_error ~trace_infos [mk_info reason [
        "idx() callback functions may not be annotated and they may only \
         access properties on the callback parameter!"
      ]]

  | EIdxUse2 reason ->
      mk_error ~trace_infos [mk_info reason [
        "idx() callbacks may only access properties on the callback \
         parameter!"
      ]]

  | EUnexpectedThisType loc ->
      mk_error ~trace_infos [loc, ["Unexpected use of `this` type"]]

  | EInvalidRestParam reason ->
      mk_error ~trace_infos ~kind:InferWarning [mk_info reason [
        "rest parameter should have an array type"
      ]]

  | ETypeParamArity (loc, n) ->
      let msg = spf "Incorrect number of type parameters (expected %n)" n in
      mk_error ~trace_infos [loc, [msg]]

  | ETypeParamMinArity (loc, n) ->
      let msg = spf
        "Incorrect number of type parameters (expected at least %n)" n
      in
      mk_error ~trace_infos [loc, [msg]]

  | ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity) ->
      let msg = spf
        "Too many type arguments. Expected at most %d"
        maximum_arity
      in
      mk_error ~trace_infos [
        mk_info reason_tapp [msg];
        mk_info reason_arity [];
      ]

  | ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity) ->
      let msg = spf
        "Too few type arguments. Expected at least %d"
        minimum_arity
      in
      mk_error ~trace_infos [
        mk_info reason_tapp [msg];
        mk_info reason_arity [];
      ]

  | EPropertyTypeAnnot loc ->
      let msg =
        "expected object type and string literal as arguments to \
         $PropertyType"
      in
      mk_error ~trace_infos [loc, [msg]]

  | EExportsAnnot loc ->
      mk_error ~trace_infos [loc, ["$Exports requires a string literal"]]

  | EUnsupportedKeyInObjectType loc ->
      mk_error ~trace_infos [loc, ["Unsupported key in object type"]]

  | EPredAnnot loc ->
      let msg =
        "expected number of refined variables (currently only supporting \
         one variable)"
      in
      mk_error ~trace_infos [loc, [msg]]

  | ERefineAnnot loc ->
      let msg =
        "expected base type and predicate type as arguments to $Refine"
      in
      mk_error ~trace_infos [loc, [msg]]

  | EUnexpectedTypeof loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Unexpected typeof expression"
      ]]

  | ECustom (reasons, msg) ->
      typecheck_error msg reasons

  | EInternal (loc, internal_error) ->
      let msg = match internal_error with
      | PackageHeapNotFound pkg ->
          spf "Package %S was not found in the PackageHeap!" pkg
      | AbnormalControlFlow ->
          "abnormal control flow"
      | UncaughtException exc ->
          Utils_js.fmt_exc exc
      | MethodNotAFunction ->
          "expected function type"
      | OptionalMethod ->
          "optional methods are not supported"
      | OpenPredWithoutSubst ->
          "OpenPredT ~> OpenPredT without substitution"
      | PredFunWithoutParamNames ->
          "FunT -> FunT no params"
      | UnsupportedGuardPredicate pred ->
          spf "Unsupported guard predicate (%s)" pred
      | BreakEnvMissingForCase ->
          "break env missing for case"
      | PropertyDescriptorPropertyCannotBeRead ->
          "Unexpected property in properties object"
      | ForInLHS ->
          "unexpected LHS in for...in"
      | ForOfLHS ->
          "unexpected LHS in for...of"
      | InstanceLookupComputed ->
          "unexpected computed property lookup on InstanceT"
      | PropRefComputedOpen ->
          "unexpected open computed property element type"
      | PropRefComputedLiteral ->
          "unexpected literal computed proprety element type"
      | ShadowReadComputed ->
          "unexpected shadow read on computed property"
      | ShadowWriteComputed ->
          "unexpected shadow write on computed property"
      | RestParameterNotIdentifierPattern ->
          "unexpected rest parameter, expected an identifier pattern"
      | InterfaceTypeSpread ->
          "unexpected spread property in interface"
      | InferJobException exc ->
          "infer_job exception: "^(Utils_js.fmt_exc exc)
      | MergeJobException exc ->
          "merge_job exception: "^(Utils_js.fmt_exc exc)
      in
      mk_error ~trace_infos ~kind:InternalError [loc, [
        spf "Internal error: %s" msg
      ]]

  | EUnsupportedSyntax (loc, unsupported_syntax) ->
      let msg = match unsupported_syntax with
        | ComprehensionExpression
        | GeneratorExpression
        | MetaPropertyExpression ->
            "not (sup)ported"
        | ObjectPropertyLiteralNonString ->
            "non-string literal property keys not supported"
        | ObjectPropertyGetSet ->
            "get/set properties not yet supported"
        | ObjectPropertyComputedGetSet ->
            "computed getters and setters are not yet supported"
        | InvariantSpreadArgument ->
            "unsupported arguments in call to invariant()"
        | ClassPropertyLiteral ->
            "literal properties not yet supported"
        | ClassPropertyComputed ->
            "computed property keys not supported"
        | ClassExtendsMultiple ->
            "A class cannot extend multiple classes!"
        | ReactCreateClassPropertyNonInit ->
            "unsupported property specification in createClass"
        | RequireDynamicArgument ->
            "The parameter passed to require() must be a literal string."
        | ImportDynamicArgument ->
            "The parameter passed to import() must be a literal string."
        | RequireLazyDynamicArgument ->
            "The first arg to requireLazy() must be a literal array of \
             string literals!"
        | CatchParameterAnnotation ->
            "type annotations for catch params not yet supported"
        | CatchParameterDeclaration ->
            "unsupported catch parameter declaration"
        | DestructuringObjectPropertyLiteralNonString ->
            "unsupported non-string literal object property in destructuring"
        | DestructuringExpressionPattern ->
            "unsupported expression pattern in destructuring"
        | PredicateDeclarationForImplementation ->
            "Cannot declare predicate when a function body is present."
        | PredicateDeclarationWithoutExpression ->
            "Predicate function declarations need to declare a predicate \
             expression."
        | PredicateDeclarationAnonymousParameters ->
            "Predicate function declarations cannot use anonymous function \
             parameters."
        | PredicateInvalidBody ->
            "Invalid body for predicate function. Expected a simple return \
             statement as body."
        | PredicateVoidReturn ->
            "Predicate functions need to return non-void."
        | MultipleIndexers ->
            "multiple indexers are not supported"
        | SpreadArgument ->
            "A spread argument is unsupported here"
      in
      mk_error ~trace_infos [loc, [msg]]

  | EIllegalName loc ->
      mk_error ~trace_infos [loc, ["illegal name"]]

  | EUseArrayLiteral loc ->
      mk_error ~trace_infos [loc, [
        "Use array literal instead of new Array(..)"
      ]]

  | EMissingAnnotation reason ->
      mk_error ~trace_infos [mk_info reason ["Missing annotation"]]

  | EBindingError (binding_error, loc, x, entry) ->
      let msg =
        match binding_error with
        | ENameAlreadyBound ->
            "name is already bound"
        | EReferencedBeforeDeclaration ->
            spf
              "%s referenced before declaration, or after skipped declaration"
              (Scope.Entry.string_of_kind entry)
        | ETypeInValuePosition ->
            "type referenced from value position"
        | ETypeAliasInValuePosition ->
            "type alias referenced from value position"
        | EConstReassigned
        | EImportReassigned ->
            spf "%s cannot be reassigned" (Scope.Entry.string_of_kind entry)
        | EConstParamReassigned ->
            spf
              "%s cannot be reassigned \
               (see experimental.const_params=true in .flowconfig)"
              (Scope.Entry.string_of_kind entry)
      in
      mk_error ~trace_infos [
        loc, [x; msg];
        Scope.Entry.entry_loc entry, [
          spf "%s %s" (Scope.Entry.string_of_kind entry) x
        ]
      ]

  | ERecursionLimit reasons ->
      typecheck_error "*** Recursion limit exceeded ***" reasons

  | EModuleOutsideRoot (loc, package_relative_to_root) ->
      let msg = spf
        "This modules resolves to %S, which is outside both your root \
         directory and all of the entries in the [include] section of your \
         .flowconfig. You should either add this directory to the [include] \
         section of your .flowconfig, move your .flowconfig file higher in \
         the project directory tree, or move this package under your Flow \
         root directory."
        package_relative_to_root
      in
      mk_error ~trace_infos [loc, [msg]]

  | EExperimentalDecorators loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Experimental decorator usage";
        "Decorators are an early stage proposal that may change. \
         Additionally, Flow does not account for the type implications \
         of decorators at this time."
      ]]

  | EExperimentalClassProperties (loc, static) ->
      let config_name, config_key =
        if static
        then "class static field", "class_static_fields"
        else "class instance field", "class_instance_fields"
      in
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        spf "Experimental %s usage" config_name;
        spf
          "%ss are an active early stage feature proposal that may change. \
           You may opt-in to using them anyway in Flow by putting \
           `esproposal.%s=enable` into the [options] section of your \
           .flowconfig."
          (String.capitalize_ascii config_name)
          config_key
      ]]

  | EUnsafeGetSet loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Potentially unsafe get/set usage";
        "Getters and setters with side effects are potentially unsafe and \
         disabled by default. You may opt-in to using them anyway by putting \
         `unsafe.enable_getters_and_setters=true` into the [options] section \
         of your .flowconfig.";
      ]]

  | EExperimentalExportStarAs loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Experimental `export * as` usage";
        "`export * as` is an active early stage feature proposal that may \
         change. You may opt-in to using it anyway by putting \
         `esproposal.export_star_as=enable` into the [options] section \
         of your .flowconfig";
      ]]

  | EIndeterminateModuleType loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Unable to determine module type (CommonJS vs ES) if both an export \
         statement and module.exports are used in the same module!"
      ]]

  | EUnreachable loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, ["unreachable code"]]

  | EInvalidSpread { reason; reason_op } ->
      typecheck_error "Cannot spread properties from" (reason_op, reason)

  | EInvalidTypeof (loc, typename) ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        spf "string literal `%s`" typename;
        "This value is not a valid `typeof` return value"
      ]]

  | EArithmeticOperand reason ->
      let msg = "The operand of an arithmetic operation must be a number." in
      mk_error ~trace_infos [mk_info reason [msg]]

  | EBinaryInLHS reason ->
      (* TODO: or symbol *)
      let msg =
        "The left-hand side of an `in` expression must be a \
         string or number." in
      mk_error ~trace_infos [mk_info reason [msg]]

  | EBinaryInRHS reason ->
      let msg =
        "The right-hand side of an `in` expression must be an \
         object or array." in
      mk_error ~trace_infos [mk_info reason [msg]]

  | EForInRHS reason ->
      let msg =
        "The right-hand side of a `for...in` statement must be an \
         object, null or undefined." in
      mk_error ~trace_infos [mk_info reason [msg]]

  | EObjectComputedPropertyAccess reasons ->
      typecheck_error "Computed property cannot be accessed with" reasons

  | EObjectComputedPropertyAssign reasons ->
      typecheck_error "Computed property cannot be assigned with" reasons

  | EInvalidLHSInAssignment loc ->
      let msg = "Invalid left-hand side in assignment expression" in
      mk_error ~trace_infos [loc, [msg]]

  | EIncompatibleWithUseOp (l_reason, u_reason, use_op) ->
      let reasons, extra, msg =
        let msg = "This type is incompatible with" in
        unwrap_use_ops ((l_reason, u_reason), [], msg) use_op in
      typecheck_error ~extra ~suppress_op:true msg reasons

  | EUnsupportedImplements reason ->
      mk_error ~trace_infos [mk_info reason [
        "Argument to implements clause must be an interface"]]

  | EReactKit (reasons, tool) ->
      let open React in
      let expected_prop_type = "Expected a React PropType instead of" in
      let resolve_object prop = function
      | ResolveObject -> "Expected an object instead of"
      | ResolveDict _ -> prop
      | ResolveProp _ -> prop
      in
      let resolve_array elem = function
      | ResolveArray -> "Expected an array instead of"
      | ResolveElem _ -> elem
      in
      let simplify_prop_type = SimplifyPropType.(function
      | ArrayOf -> expected_prop_type
      | InstanceOf -> "Expected a class type instead of"
      | ObjectOf -> expected_prop_type
      | OneOf tool -> resolve_array "Expected a literal type instead of" tool
      | OneOfType tool -> resolve_array expected_prop_type tool
      | Shape tool -> resolve_object expected_prop_type tool
      ) in
      let create_class = CreateClass.(function
      | Spec _ ->
        "Expected an exact object instead of"
      | Mixins _ ->
        "`mixins` should be a tuple instead of"
      | Statics _ ->
        "`statics` should be an object instead of"
      | PropTypes (_, tool) ->
        resolve_object expected_prop_type tool
      | DefaultProps _ ->
        "`defaultProps` should be an object instead of"
      | InitialState _ ->
        "`initialState` should be an object or null instead of"
      ) in
      let msg = match tool with
      | SimplifyPropType (tool, _) -> simplify_prop_type tool
      | CreateElement _ -> "Expected React component instead of"
      | CreateClass (tool, _, _) -> create_class tool
      in
      typecheck_error msg reasons

  | EFunctionCallExtraArg (unused_reason, def_reason, param_count) ->
    let core_msgs = [
      unused_reason, [];
    ] in
    let expects = match param_count with
    | 0 -> "expects no arguments"
    | 1 -> "expects no more than 1 argument"
    | n -> spf "expects no more than %d arguments" n in
    let extra = [
      InfoLeaf [loc_of_reason def_reason, [spf
        "%s %s"
        (string_of_desc (desc_of_reason def_reason))
        expects
      ]];
    ] in
    typecheck_error_with_core_infos ~extra core_msgs

  | EUnsupportedSetProto reason ->
      mk_error ~trace_infos [mk_info reason [
        "Prototype mutation not allowed"]]

  | EDuplicateModuleProvider {module_name; provider; conflict} ->
      mk_error ~kind:DuplicateProviderError [
        Loc.({ none with source = Some conflict }), [
          module_name; "Duplicate module provider"];
        Loc.({ none with source = Some provider }), [
          "current provider"]
      ]

  | EParseError (loc, parse_error) ->
    mk_error ~kind:ParseError [loc, [Parse_error.PP.error parse_error]]

  | EDocblockError (loc, err) ->
    let msg = match err with
    | MultipleFlowAttributes ->
      "Unexpected @flow declaration. Only one per file is allowed."
    | MultipleProvidesModuleAttributes ->
      "Unexpected @providesModule declaration. Only one per file is allowed."
    | MultipleJSXAttributes ->
      "Unexpected @jsx declaration. Only one per file is allowed."
    | InvalidJSXAttribute first_error ->
      "Invalid @jsx declaration. Should have form `@jsx LeftHandSideExpression` "^
      "with no spaces."^
      (match first_error with
      | None -> ""
      | Some first_error -> spf " Parse error: %s" first_error)
    in
    mk_error ~kind:ParseError [loc, [msg]]

  | EUnusedSuppression loc ->
    mk_error [loc, ["Error suppressing comment"; "Unused suppression"]]
