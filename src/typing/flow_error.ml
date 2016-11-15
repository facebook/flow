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
  | EIncompatibleProp of Type.t * Type.use_t * reason
  | EDebugPrint of reason * Type.t
  | EImportValueAsType of reason * string
  | EImportTypeAsTypeof of reason * string
  | EImportTypeAsValue of reason * string
  | EImportTypeofNamespace of reason * string * string
  | ENoDefaultExport of reason * string * string option
  | EOnlyDefaultExport of reason * string
  | ENoNamedExport of reason * string * string option
  | EMissingTypeArgs of reason * Type.typeparam list
  | EValueUsedAsType of (reason * reason)
  | EMutationNotAllowed of (reason * reason)
  | EExpectedStringLit of (reason * reason) * string * string Type.literal
  | EExpectedNumberLit of (reason * reason) * Type.number_literal * Type.number_literal Type.literal
  | EExpectedBooleanLit of (reason * reason) * bool * bool option
  | EPropNotFound of (reason * reason)
  | EPropAccess of (reason * reason) * string option * Type.property * Type.rw
  | EPropPolarityMismatch of (reason * reason) * string option * (Type.polarity * Type.polarity)
  | EPolarityMismatch of Type.typeparam * Type.polarity
  | EStrictLookupFailed of (reason * reason) * reason * string option
  | EFunCallParam of (reason * reason)
  | EFunCallThis of reason * reason * reason
  | EFunReturn of (reason * reason)
  | EFunImplicitReturn of (reason * reason)
  | EAddition of (reason * reason)
  | ECoercion of (reason * reason)
  | EComparison of (reason * reason)
  | EMissingTupleElement of (reason * reason) * int
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
  | EBindingError of binding_error * reason * string * Scope.Entry.t
  | ERecursionLimit of (reason * reason)
  | EModuleOutsideRoot of Loc.t * string
  | EExperimentalDecorators of Loc.t
  | EExperimentalClassProperties of Loc.t * bool
  | EUnsafeGetSet of Loc.t
  | EExperimentalExportStarAs of Loc.t
  | EIndeterminateModuleType of reason
  | EUnreachable of Loc.t
  | EInvalidTypeof of Loc.t * string
  | EBinaryInLHS of reason
  | EBinaryInRHS of reason
  | EArithmeticOperand of reason
  | EForInRHS of reason
  | EObjectComputedPropertyAccess of (reason * reason)
  | EObjectComputedPropertyAssign of (reason * reason)
  | EInvalidLHSInAssignment of Loc.t

and binding_error =
  | ENameAlreadyBound
  | EReferencedBeforeDeclaration
  | ETypeInValuePosition
  | ETypeAliasInValuePosition
  | EConstReassigned
  | EConstParamReassigned
  | EImportReassigned

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
  | RestArgumentNotIdentifierPattern

and unsupported_syntax =
  | ComprehensionExpression
  | GeneratorExpression
  | MetaPropertyExpression
  | ObjectPropertyLiteralNonString
  | ObjectPropertyGetSet
  | InvariantSpreadArgument
  | Implements
  | ClassPropertyLiteral
  | ClassPropertyComputed
  | ClassExtendsMultiple
  | ReactPropTypesPropertyLiteralNonString
  | ReactPropTypesPropertyGetSet
  | ReactPropTypesPropertyComputed
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

(** error services for typecheck pipeline -
    API for building and adding errors to context during
    typechecking, subject to speculation state.
  *)

module Impl : sig
  val ordered_reasons: Type.t -> Type.use_t -> reason * reason

  val add_output: Context.t -> ?trace:Trace.t -> error_message -> unit

  val warn_or_ignore_decorators:
    Context.t -> Loc.t -> unit

  val warn_or_ignore_class_properties:
    Context.t -> static:bool -> Loc.t -> unit

  val warn_unsafe_getters_setters:
    Context.t -> Loc.t -> unit

end = struct
  (* decide reason order based on UB's flavor and blamability *)
  let ordered_reasons l u =
    let rl = reason_of_t l in
    let ru = reason_of_use_t u in
    if is_use u || (is_blamable_reason ru && not (is_blamable_reason rl))
    then ru, rl
    else rl, ru

  let rec error_of_msg cx ?trace =
    let open Errors in

    let mk_info reason extras =
      loc_of_reason reason, string_of_desc (desc_of_reason reason) :: extras
    in

    let info_of_reason r = mk_info r [] in

    (* only on use-types - guard calls with is_use t *)
    let err_operation = function
      | UseT (_, t) ->
        failwith (spf "err_operation called on def type %s" (string_of_ctor t))

      | GetPropT _ -> "Property cannot be accessed on"
      | SetPropT _ -> "Property cannot be assigned on"
      | MethodT _ -> "Method cannot be called on"
      | CallT _ -> "Function cannot be called on"
      | ApplyT _ -> "Expected array of arguments instead of"
      | ConstructorT _ -> "Constructor cannot be called on"
      | GetElemT _ -> "Computed property/element cannot be accessed on"
      | SetElemT _ -> "Computed property/element cannot be assigned on"
      | CallElemT _ -> "Computed property/element cannot be called on"
      | ElemT (_, _, ReadElem _) -> "Element cannot be accessed with"
      | ElemT (_, _, WriteElem _) -> "Element cannot be assigned with"
      | ElemT (_, _, CallElem _) -> "Element cannot be called with"
      | ObjAssignT _ -> "Expected object instead of"
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
      | HasPropT _ -> "Property not found in"
      | UnaryMinusT _ -> "Expected number instead of"
      | MapTypeT (_, kind, _, _) ->
        (match kind with
        | TupleMap -> "Expected Iterable instead of"
        | ObjectMap
        | ObjectMapi -> "Expected object instead of")
      | ReactCreateElementT _ -> "Expected React component instead of"
      | CallLatentPredT _ -> "Expected predicated function instead of"
      | TypeAppVarianceCheckT _ -> "Expected polymorphic type instead of"
      (* unreachable or unclassified use-types. until we have a mechanical way
         to verify that all legit use types are listed above, we can't afford
         to throw on a use type, so mark the error instead *)
      | t ->
        spf "Type is incompatible with (unclassified use type: %s)"
          (string_of_use_ctor t)
    in

    let err_value = function
      | NullT _ -> " possibly null value"
      | VoidT _ -> " possibly undefined value"
      | MaybeT _ -> " possibly null or undefined value"
      | IntersectionT _
      | MixedT (_, Empty_intersection) -> " any member of intersection type"
      | _ -> ""
    in

    let err_msg l u =
      if is_use u
      then spf "%s%s" (err_operation u) (err_value l)
      else "This type is incompatible with"
    in

    let msg_export export_name =
      if export_name = "default"
      then export_name
      else spf "`%s`" export_name
    in

    let poly_minimum_arity xs = List.(
      xs |> filter (fun typeparam -> typeparam.default = None) |> length
    ) in

    let typecheck_error msg ?(suppress_op=false) ?extra (r1, r2) =
      let origin_r1 = origin_of_reason r1 in
      let origin_infos = opt_map_default (fun r -> [mk_info r []]) [] origin_r1 in
      (* make core info from reasons, message, and optional extra infos *)
      let core_infos = [
        mk_info r1 [msg];
        mk_info r2 []
      ] in
      (* Since pointing to endpoints in the library without any information on
         the code that uses those endpoints inconsistently is useless, we point
         to the file containing that code instead. Ideally, improvements in
         error reporting would cause this case to never arise. *)
      let lib_infos = if is_lib_reason r1 && is_lib_reason r2 then
          let loc = Loc.({ none with source = Some (Context.file cx) }) in
          [loc, ["inconsistent use of library definitions"]]
        else []
      in
      (* trace info *)
      let trace_infos = match trace with
      | None -> []
      | Some trace ->
        (* format a trace into list of (reason, desc) pairs used
         downstream for obscure reasons, and then to messages *)
        let max_trace_depth = Context.max_trace_depth cx in
        if max_trace_depth = 0 then [] else
          let strip_root = Context.should_strip_root cx in
          let root = Context.root cx in
          let prep_path r =
            if not strip_root then r
            else Reason.strip_root root r
          in
          Trace.reasons_of_trace ~prep_path ~level:max_trace_depth trace
          |> List.map info_of_reason
      in
      (* NOTE: We include the operation's reason in the error message, unless it
         overlaps *both* endpoints, exactly matches r1, or overlaps r1's origin *)
      let op_info = if suppress_op then None else
        match Ops.peek () with
        | Some r ->
          if r = r1 then None
          else if reasons_overlap r r1 && reasons_overlap r r2 then None
          else begin match origin_r1 with
          | Some or1 when reasons_overlap r or1 -> None
          | _ -> Some (info_of_reason r)
          end
        | _ -> None
      in
      (* main info is core info with optional lib line prepended, and optional
         extra info appended. ops/trace info is held separately in error *)
      let msg_infos = lib_infos @ origin_infos @ core_infos in
      mk_error ?op_info ~trace_infos ?extra msg_infos
    in

    let prop_polarity_error reasons x p1 p2 =
      let prop_name = match x with
      | Some x -> spf "property `%s`" x
      | None -> "computed property"
      in
      let msg = spf "%s %s incompatible with %s use in"
        (String.capitalize (Polarity.string p1))
        prop_name
        (Polarity.string p2)
      in
      typecheck_error msg reasons
    in

    function
    | EIncompatible (l, u) ->
        typecheck_error (err_msg l u) (ordered_reasons l u)

    | EIncompatibleProp (l, u, reason_prop) ->
        (* when unexpected types flow into a GetPropT/SetPropT (e.g. void or
           other non-object-ish things), then use `reason_prop`, which
           represents the reason for the prop itself, not the lookup action. *)
        let reasons = reason_prop, reason_of_t l in
        typecheck_error (err_msg l u) reasons

    | EDebugPrint (r, t) ->
        mk_error [mk_info r [Debug_js.jstr_of_t cx t]]

    | EImportValueAsType (r, export_name) ->
        mk_error [mk_info r [spf
          "The %s export is a value, but not a type. `import type` only works \
           on type exports like type aliases, interfaces, and classes. If you \
           intended to import the type *of* a value, please use `import \
           typeof` instead."
          (msg_export export_name)]]

    | EImportTypeAsTypeof (r, export_name) ->
        mk_error [mk_info r [spf
          "The %s export is a type, but not a value. `import typeof` only \
           works on value exports like classes, vars, lets, etc. If you \
           intended to import a type alias or interface, please use `import \
           type` instead."
          (msg_export export_name)]]

    | EImportTypeAsValue (r, export_name) ->
        mk_error [mk_info r [spf
          "`%s` is a type, but not a value. In order to import it, please use \
           `import type`."
          export_name]]

    | EImportTypeofNamespace (r, local_name, module_name) ->
        mk_error [mk_info r [spf
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
        mk_error [mk_info r [msg]]

    | EOnlyDefaultExport (r, export_name) ->
        mk_error [mk_info r [spf
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
        mk_error [mk_info r [msg]]

    | EMissingTypeArgs (r, params) ->
        let min = poly_minimum_arity params in
        let max = List.length params in
        let arity, args = if min = max
          then spf "%d" max, if max = 1 then "argument" else "arguments"
          else spf "%d-%d" min max, "arguments"
        in
        mk_error [mk_info r [spf
          "Application of polymorphic type needs \
           <list of %s %s>. (Can use `*` for inferrable ones)"
          arity args]]

    | EValueUsedAsType reasons ->
        typecheck_error
          "Ineligible value used in/as type annotation \
           (did you forget 'typeof'?)"
          reasons

    | EMutationNotAllowed reasons ->
        typecheck_error "Mutation not allowed on" reasons

    | EExpectedStringLit (reasons, expected, actual) ->
        let msg = match actual with
        | Literal actual ->
            spf "Expected string literal `%s`, got `%s` instead"
              expected actual
        | Truthy | AnyLiteral ->
            spf "Expected string literal `%s`" expected
        in
        typecheck_error msg reasons

    | EExpectedNumberLit (reasons, (expected, _), actual) ->
        let msg = match actual with
        | Literal (actual, _) ->
            spf "Expected number literal `%.16g`, got `%.16g` instead"
              expected actual
        | Truthy | AnyLiteral ->
            spf "Expected number literal `%.16g`" expected
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

    | EPropNotFound reasons ->
        typecheck_error "Property not found in" reasons

    | EPropAccess (reasons, x, prop, rw) ->
        prop_polarity_error reasons x
          (Property.polarity prop)
          (Polarity.of_rw rw)

    | EPropPolarityMismatch (reasons, x, (p1, p2)) ->
        prop_polarity_error reasons x p1 p2

    | EPolarityMismatch (tp, p) ->
        mk_error [mk_info tp.reason [spf
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
        mk_error [mk_info (fst reasons) [msg]]
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
        let suppress_op = match Ops.peek () with
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
        mk_error [mk_info ureason [spf
          "This type is incompatible with an implicitly-returned %s."
          (string_of_desc (desc_of_reason lreason))]]

    | EAddition reasons ->
        typecheck_error "This type cannot be added to" reasons

    | ECoercion reasons ->
        typecheck_error "This type cannot be coerced to" reasons

    | EComparison reasons ->
        typecheck_error "This type cannot be compared to" reasons

    | EMissingTupleElement (reasons, i) ->
        let msg = spf
          "This type is incompatible with a tuple type that expects \
           a %s element of non-optional type"
          (ordinal i)
        in
        typecheck_error msg reasons

    | ESpeculationFailed (l, u, branches) ->
        let reasons = ordered_reasons l u in
        let msg = err_msg l u in
        let extra = List.mapi (fun i (r, msg) ->
          let err = error_of_msg cx ?trace msg in
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
        mk_error ~extra [
          (mk_info case_r ["Could not decide which case to select"]);
          (info_of_reason r)
        ]

    | EIncompatibleWithExact reasons ->
        typecheck_error "Inexact type is incompatible with exact type" reasons

    | EUnsupportedExact reasons ->
        typecheck_error "Unsupported exact type" reasons

    | EIdxArity reason ->
        mk_error [mk_info reason ["idx() function takes exactly two params!"]]

    | EIdxUse1 reason ->
        mk_error [mk_info reason [
          "idx() callback functions may not be annotated and they may only \
           access properties on the callback parameter!"
        ]]

    | EIdxUse2 reason ->
        mk_error [mk_info reason [
          "idx() callbacks may only access properties on the callback \
           parameter!"
        ]]

    | EUnexpectedThisType loc ->
        mk_error [loc, ["Unexpected use of `this` type"]]

    | EInvalidRestParam reason ->
        mk_error ~kind:InferWarning [mk_info reason [
          "rest parameter should have an explicit array type (or type `any`)"
        ]]

    | ETypeParamArity (loc, n) ->
        let msg = spf "Incorrect number of type parameters (expected %n)" n in
        mk_error [loc, [msg]]

    | ETypeParamMinArity (loc, n) ->
        let msg = spf
          "Incorrect number of type parameters (expected at least %n)" n
        in
        mk_error [loc, [msg]]

    | ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity) ->
        let msg = spf
          "Too many type arguments. Expected at most %d"
          maximum_arity
        in
        mk_error [
          mk_info reason_tapp [msg];
          mk_info reason_arity [];
        ]

    | ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity) ->
        let msg = spf
          "Too few type arguments. Expected at least %d"
          minimum_arity
        in
        mk_error [
          mk_info reason_tapp [msg];
          mk_info reason_arity [];
        ]

    | EPropertyTypeAnnot loc ->
        let msg =
          "expected object type and string literal as arguments to \
           $PropertyType"
        in
        mk_error [loc, [msg]]

    | EExportsAnnot loc ->
        mk_error [loc, ["$Exports requires a string literal"]]

    | EUnsupportedKeyInObjectType loc ->
        mk_error [loc, ["Unsupported key in object type"]]

    | EPredAnnot loc ->
        let msg =
          "expected number of refined variables (currently only supporting \
           one variable)"
        in
        mk_error [loc, [msg]]

    | ERefineAnnot loc ->
        let msg =
          "expected base type and predicate type as arguments to $Refine"
        in
        mk_error [loc, [msg]]

    | EUnexpectedTypeof loc ->
        mk_error ~kind:InferWarning [loc, ["Unexpected typeof expression"]]

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
        | RestArgumentNotIdentifierPattern ->
            "unexpected rest argument, expected an identifier pattern"
        in
        mk_error ~kind:InternalError [loc, [spf "Internal error: %s" msg]]

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
          | InvariantSpreadArgument ->
              "unsupported arguments in call to invariant()"
          | Implements ->
              "implements not supported"
          | ClassPropertyLiteral ->
              "literal properties not yet supported"
          | ClassPropertyComputed ->
              "computed property keys not supported"
          | ClassExtendsMultiple ->
              "A class cannot extend multiple classes!"
          | ReactPropTypesPropertyLiteralNonString ->
              "non-string literal property keys not supported for React \
               propTypes"
          | ReactPropTypesPropertyGetSet ->
              "get/set properties not supported for React propTypes"
          | ReactPropTypesPropertyComputed ->
              "computed property keys not supported for React propTypes"
          | ReactCreateClassPropertyNonInit ->
              "unsupported property specification in createClass"
          | RequireDynamicArgument ->
              "The parameter passed to require() must be a literal string."
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
        in
        mk_error [loc, [msg]]

    | EIllegalName loc ->
        mk_error [loc, ["illegal name"]]

    | EUseArrayLiteral loc ->
        mk_error [loc, ["Use array literal instead of new Array(..)"]]

    | EMissingAnnotation reason ->
        mk_error [mk_info reason ["Missing annotation"]]

    | EBindingError (binding_error, reason, x, entry) ->
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
        mk_error [
          loc_of_reason reason, [x; msg];
          Scope.Entry.loc entry, [
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
        mk_error [loc, [msg]]

    | EExperimentalDecorators loc ->
        mk_error ~kind:InferWarning [loc, [
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
        mk_error ~kind:InferWarning [loc, [
          spf "Experimental %s usage" config_name;
          spf
            "%ss are an active early stage feature proposal that may change. \
             You may opt-in to using them anyway in Flow by putting \
             `esproposal.%s=enable` into the [options] section of your \
             .flowconfig."
            (String.capitalize config_name)
            config_key
        ]]

    | EUnsafeGetSet loc ->
        mk_error ~kind:InferWarning [loc, [
          "Potentially unsafe get/set usage";
          "Getters and setters with side effects are potentially unsafe and \
           disabled by default. You may opt-in to using them anyway by putting \
           `unsafe.enable_getters_and_setters=true` into the [options] section \
           of your .flowconfig.";
        ]]

    | EExperimentalExportStarAs loc ->
        mk_error ~kind:InferWarning [loc, [
          "Experimental `export * as` usage";
          "`export * as` is an active early stage feature proposal that may \
           change. You may opt-in to using it anyway by putting \
           `esproposal.export_star_as=enable` into the [options] section \
           of your .flowconfig";
        ]]

    | EIndeterminateModuleType reason ->
        mk_error ~kind:InferWarning [mk_info reason [
          "Unable to determine module type (CommonJS vs ES) if both an export \
           statement and module.exports are used in the same module!"
        ]]

    | EUnreachable loc ->
        mk_error ~kind:InferWarning [loc, ["unreachable code"]]

    | EInvalidTypeof (loc, typename) ->
        mk_error ~kind:InferWarning [loc, [
          spf "string literal `%s`" typename;
          "This value is not a valid `typeof` return value"
        ]]

    | EArithmeticOperand reason ->
        let msg = "The operand of an arithmetic operation must be a number." in
        mk_error [mk_info reason [msg]]

    | EBinaryInLHS reason ->
        (* TODO: or symbol *)
        let msg =
          "The left-hand side of an `in` expression must be a \
           string or number." in
        mk_error [mk_info reason [msg]]

    | EBinaryInRHS reason ->
        let msg =
          "The right-hand side of an `in` expression must be an \
           object or array." in
        mk_error [mk_info reason [msg]]

    | EForInRHS reason ->
        let msg =
          "The right-hand side of a `for...in` statement must be an \
           object, null or undefined." in
        mk_error [mk_info reason [msg]]

    | EObjectComputedPropertyAccess reasons ->
        typecheck_error "Computed property cannot be accessed with" reasons

    | EObjectComputedPropertyAssign reasons ->
        typecheck_error "Computed property cannot be assigned with" reasons

    | EInvalidLHSInAssignment loc ->
        let msg = "Invalid left-hand side in assignment expression" in
        mk_error [loc, [msg]]

  let add_output cx ?trace msg =
    let error = error_of_msg cx ?trace msg in
    if Context.is_verbose cx
    then prerr_endlinef "\nadd_output cx.file %S loc %s"
      (string_of_filename (Context.file cx))
      (string_of_loc (Errors.loc_of_error error));

    (* catch no-loc errors early, before they get into error map *)
    Errors.(
      if Loc.source (loc_of_error error) = None
      then assert_false (spf "add_output: no source for error: %s"
        (Hh_json.json_to_multiline (json_of_errors [error])))
    );

    Context.add_error cx error

  (** build typecheck error from msg, reasons, trace and extra info.
      Note: Ops stack is also queried, so this isn't a stateless function.
    *)
  let warn_or_ignore_decorators cx loc =
    match Context.esproposal_decorators cx with
    | Options.ESPROPOSAL_ENABLE -> failwith "Decorators cannot be enabled!"
    | Options.ESPROPOSAL_IGNORE -> ()
    | Options.ESPROPOSAL_WARN ->
      add_output cx (EExperimentalDecorators loc)

  let warn_or_ignore_class_properties cx ~static loc =
    let config_setting =
      if static
      then Context.esproposal_class_static_fields cx
      else Context.esproposal_class_instance_fields cx
    in
    match config_setting with
    | Options.ESPROPOSAL_ENABLE
    | Options.ESPROPOSAL_IGNORE -> ()
    | Options.ESPROPOSAL_WARN ->
      add_output cx (EExperimentalClassProperties (loc, static))

  let warn_unsafe_getters_setters cx loc =
    if not (Context.enable_unsafe_getters_and_setters cx)
    then add_output cx (EUnsafeGetSet loc)
end

include Impl
