(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Hash of signatures.

   Signature are just "resolved" types that serve to describe module
   exports. See Merge_js for how signatures are created from graphs. In
   particular, a signature can have cycles, and is thus represented as a system
   of equations from names to things: tvar ids point to types, propmap ids point
   to propmaps, etc., and types can in turn contain these names.

   When a file changes, we need to determine whether its signature has
   meaningfully changed in order to decide whether to recheck its dependents,
   (since the only way changes can meaningfully propagate to dependents is
   through the signature). How do we do this?

   The "structure" of a signature is captured by traversing it in some
   deterministic order, ignoring the specific names. Signatures are considered
   similar when their structures are equal. Note that this is a semantic, rather
   than a syntactic, notion: similar signatures may have different names in
   their representations. It is important to ignore such differences, since
   rechecking a file will trivially generate new names, even though nothing has
   changed meaningfully.

   Finally, these structures may be huge, so we instead compare their digests,
   tolerating improbable collisions (cf. SharedMem).
*)

(* NOTE: it's critical that these are all constant constructors, which are
 * represented as ints, because we hash in C assuming they are ints. Any
 * non-constant constructors will be blocks, and fail to hash properly. *)
type hash =
  (* def types *)
  | NumH
  | StrH
  | BoolH
  | EmptyH
  | MixedH
  | AnyH
  | NullH
  | VoidH
  | FunH
  | FunProtoH
  | FunProtoApplyH
  | FunProtoBindH
  | FunProtoCallH
  | ObjH
  | ObjProtoH
  | MatchingPropH
  | NullProtoH
  | ArrH
  | ClassH
  | OptionalH
  | EvalH
  | TypeAppH
  | ThisClassH
  | ThisTypeAppH
  | BoundH
  | ExistsH
  | ExactH
  | MaybeH
  | IntersectionH
  | UnionH
  | AnyWithLowerBoundH
  | AnyWithUpperBoundH
  | MergedH
  | AnyObjH
  | AnyFunH
  | ShapeH
  | KeysH
  | SingletonStrH
  | SingletonNumH
  | SingletonBoolH
  | TypeH
  | AnnotH
  | ModuleH
  | TvarDestructorH
  | CustomFunH
  | OpenPredH
  | CharSetH
  | ReposH
  (* use types *)
  | BindH
  | CallH
  | MethodH
  | SetPropH
  | SetPrivatePropH
  | GetPropH
  | GetPrivatePropH
  | TestPropH
  | SetElemH
  | GetElemH
  | CallElemH
  | GetStaticsH
  | GetProtoH
  | SetProtoH
  | ReposLowerH
  | ReposUseH
  | ConstructorH
  | SuperH
  | ImplementsH
  | MixinH
  | AdderH
  | ComparatorH
  | UnaryMinusH
  | AssertArithmeticOperandH
  | AssertBinaryInLHSH
  | AssertBinaryInRHSH
  | AssertForInRHSH
  | AssertRestParamH
  | PredicateH
  | GuardH
  | EqH
  | AndH
  | OrH
  | NullishCoalesceH
  | NotH
  | SpecializeH
  | ThisSpecializeH
  | VarianceCheckH
  | TypeAppVarianceCheckH
  | ConcretizeTypeAppsH
  | LookupH
  | ObjAssignToH
  | ObjAssignFromH
  | ObjFreezeH
  | ObjRestH
  | ObjSealH
  | ObjTestH
  | ObjTestProtoH
  | ArrRestH
  | UnifyH
  | BecomeH
  | GetKeysH
  | HasOwnPropH
  | GetValuesH
  | ElemH
  | MakeExactH
  | CJSRequireH
  | ImportModuleNsH
  | ImportDefaultH
  | ImportNamedH
  | ImportTypeH
  | ImportTypeofH
  | AssertImportIsValueH
  | CJSExtractNamedExportsH
  | CopyNamedExportsH
  | CopyTypeExportsH
  | ExportNamedH
  | ExportTypeH
  | MapTypeH
  | ReactKitH
  | ObjKitH
  | ChoiceKitUseH
  | IntersectionPreprocessKitH
  | DebugPrintH
  | DebugSleepH
  | SentinelPropTestH
  | IdxUnwrapH
  | IdxUnMaybeifyH
  | OptionalChainH
  | CallLatentPredH
  | CallOpenPredH
  | SubstOnPredH
  | RefineH
  | ResolveSpreadH
  | CondH
  | ExtendsUseH
  | ToStringH
  | InvariantH

let hash_of_def_ctor = Type.(function
  | InstanceT _ -> failwith "undefined hash of InstanceT"
  | PolyT _ -> failwith "undefined hash of PolyT"
  | IdxWrapper _ -> failwith "undefined hash of IdxWrapper"

  | AnyFunT -> AnyFunH
  | AnyObjT -> AnyObjH
  | AnyT -> AnyH
  | ArrT _ -> ArrH
  | BoolT _ -> BoolH
  | CharSetT _ -> CharSetH
  | ClassT _ -> ClassH
  | EmptyT -> EmptyH
  | FunT _ -> FunH
  | IntersectionT _ -> IntersectionH
  | MaybeT _ -> MaybeH
  | MixedT _ -> MixedH
  | NullT -> NullH
  | NumT _ -> NumH
  | ObjT _ -> ObjH
  | OptionalT _ -> OptionalH
  | SingletonBoolT _ -> SingletonBoolH
  | SingletonNumT _ -> SingletonNumH
  | SingletonStrT _ -> SingletonStrH
  | StrT _ -> StrH
  | TypeT _ -> TypeH
  | TypeAppT _ -> TypeAppH
  | VoidT -> VoidH
  | UnionT _ -> UnionH
)

let hash_of_ctor = Type.(function
  | OpenT _ -> failwith "undefined hash of OpenT"
  | InternalT _ -> failwith "undefined hash of InternalT"
  | OpaqueT _ -> failwith "undefined hash of OpaqueT"

  | AnnotT _ -> AnnotH
  | AnyWithLowerBoundT _ -> AnyWithLowerBoundH
  | AnyWithUpperBoundT _ -> AnyWithUpperBoundH
  | MergedT _ -> MergedH
  | BoundT _ -> BoundH
  | TypeDestructorTriggerT _ -> TvarDestructorH
  | CustomFunT _ -> CustomFunH
  | DefT (_, t) -> hash_of_def_ctor t
  | EvalT _ -> EvalH
  | ExactT _ -> ExactH
  | ExistsT _ -> ExistsH
  | FunProtoT _ -> FunProtoH
  | FunProtoApplyT _ -> FunProtoApplyH
  | FunProtoBindT _ -> FunProtoBindH
  | FunProtoCallT _ -> FunProtoCallH
  | KeysT _ -> KeysH
  | ModuleT _ -> ModuleH
  | NullProtoT _ -> NullProtoH
  | ObjProtoT _ -> ObjProtoH
  | MatchingPropT _ -> MatchingPropH
  | OpenPredT _ -> OpenPredH
  | ReposT _ -> ReposH
  | ShapeT _ -> ShapeH
  | ThisClassT _ -> ThisClassH
  | ThisTypeAppT _ -> ThisTypeAppH
)

let hash_of_use_ctor = Type.(function
  | UseT _ -> failwith "undefined hash of UseT"

  | BindT _ -> BindH
  | CallT _ -> CallH
  | MethodT _ -> MethodH
  | SetPropT _ -> SetPropH
  | SetPrivatePropT _ -> SetPrivatePropH
  | GetPropT _ -> GetPropH
  | GetPrivatePropT _ -> GetPrivatePropH
  | TestPropT _ -> TestPropH
  | SetElemT _ -> SetElemH
  | GetElemT _ -> GetElemH
  | CallElemT _ -> CallElemH
  | GetStaticsT _ -> GetStaticsH
  | GetProtoT _ -> GetProtoH
  | SetProtoT _ -> SetProtoH
  | ReposLowerT _ -> ReposLowerH
  | ReposUseT _ -> ReposUseH
  | ConstructorT _ -> ConstructorH
  | SuperT _ -> SuperH
  | ImplementsT _ -> ImplementsH
  | MixinT _ -> MixinH
  | AdderT _ -> AdderH
  | ComparatorT _ -> ComparatorH
  | UnaryMinusT _ -> UnaryMinusH
  | AssertArithmeticOperandT _ -> AssertArithmeticOperandH
  | AssertBinaryInLHST _ -> AssertBinaryInLHSH
  | AssertBinaryInRHST _ -> AssertBinaryInRHSH
  | AssertForInRHST _ -> AssertForInRHSH
  | AssertRestParamT _ -> AssertRestParamH
  | PredicateT _ -> PredicateH
  | GuardT _ -> GuardH
  | EqT _ -> EqH
  | AndT _ -> AndH
  | OrT _ -> OrH
  | NullishCoalesceT _ -> NullishCoalesceH
  | NotT _ -> NotH
  | SpecializeT _ -> SpecializeH
  | ThisSpecializeT _ -> ThisSpecializeH
  | VarianceCheckT _ -> VarianceCheckH
  | TypeAppVarianceCheckT _ -> TypeAppVarianceCheckH
  | ConcretizeTypeAppsT _ -> ConcretizeTypeAppsH
  | LookupT _ -> LookupH
  | ObjAssignToT _ -> ObjAssignToH
  | ObjAssignFromT _ -> ObjAssignFromH
  | ObjFreezeT _ -> ObjFreezeH
  | ObjRestT _ -> ObjRestH
  | ObjSealT _ -> ObjSealH
  | ObjTestT _ -> ObjTestH
  | ObjTestProtoT _ -> ObjTestProtoH
  | ArrRestT _ -> ArrRestH
  | UnifyT _ -> UnifyH
  | BecomeT _ -> BecomeH
  | GetKeysT _ -> GetKeysH
  | HasOwnPropT _ -> HasOwnPropH
  | GetValuesT _ -> GetValuesH
  | ElemT _ -> ElemH
  | MakeExactT _ -> MakeExactH
  | CJSRequireT _ -> CJSRequireH
  | ImportModuleNsT _ -> ImportModuleNsH
  | ImportDefaultT _ -> ImportDefaultH
  | ImportNamedT _ -> ImportNamedH
  | ImportTypeT _ -> ImportTypeH
  | ImportTypeofT _ -> ImportTypeofH
  | AssertImportIsValueT _ -> AssertImportIsValueH
  | CJSExtractNamedExportsT _ -> CJSExtractNamedExportsH
  | CopyNamedExportsT _ -> CopyNamedExportsH
  | CopyTypeExportsT _ -> CopyTypeExportsH
  | ExportNamedT _ -> ExportNamedH
  | ExportTypeT _ -> ExportTypeH
  | MapTypeT _ -> MapTypeH
  | ReactKitT _ -> ReactKitH
  | ObjKitT _ -> ObjKitH
  | ChoiceKitUseT _ -> ChoiceKitUseH
  | IntersectionPreprocessKitT _ -> IntersectionPreprocessKitH
  | DebugPrintT _ -> DebugPrintH
  | DebugSleepT _ -> DebugSleepH
  | SentinelPropTestT _ -> SentinelPropTestH
  | IdxUnwrap _ -> IdxUnwrapH
  | IdxUnMaybeifyT _ -> IdxUnMaybeifyH
  | OptionalChainT _ -> OptionalChainH
  | CallLatentPredT _ -> CallLatentPredH
  | CallOpenPredT _ -> CallOpenPredH
  | SubstOnPredT _ -> SubstOnPredH
  | RefineT _ -> RefineH
  | ResolveSpreadT _ -> ResolveSpreadH
  | CondT _ -> CondH
  | ExtendsUseT _ -> ExtendsUseH
  | ToStringT _ -> ToStringH
  | InvariantT _ -> InvariantH
)

let add = Xx.update
let add_int = Xx.update_int
let add_bool = Xx.update_int (* bools are ints *)

let add_option state f = function
  | None -> add_int state 0
  | Some x -> add_int state 1; f state x

let add_literal state f = Type.(function
  | Literal (_, x) -> add_int state 0; f state x
  | Truthy -> add_int state 1
  | AnyLiteral -> add_int state 2
)

let add_number_literal state (_, x) = add state x

let add_type state t =
  add_int state (hash_of_ctor t);
  let open Type in
  match t with
  | DefT (_, BoolT b) ->
    add_option state add_bool b
  | DefT (_, MixedT m) ->
    add_int state m
  | DefT (_, NumT n) ->
    add_literal state add_number_literal n
  | DefT (_, SingletonBoolT b) ->
    add_bool state b
  | DefT (_, SingletonNumT n) ->
    add_number_literal state n
  | DefT (_, SingletonStrT s) ->
    add state s
  | DefT (_, StrT s) ->
    add_literal state add s
  | _ -> ()

let add_use state use =
  add_int state (hash_of_use_ctor use)

let add_file_key state = File_key.(function
  | LibFile f ->
    add_int state 0; add state f
  | SourceFile f ->
    add_int state 1; add state f
  | JsonFile f ->
    add_int state 2; add state f
  | ResourceFile f ->
    add_int state 3; add state f
  | Builtins ->
    add_int state 4
)

let add_loc state loc =
  let open Loc in
  add_option state add_file_key loc.source;
  add_int state loc.start.line;
  add_int state loc.start.column;
  add_int state loc._end.line;
  add_int state loc._end.column

let add_reason state r =
  let open Reason in
  add_loc state (loc_of_reason r);
  add_loc state (def_loc_of_reason r)

let add_polarity = add_int

let add_prop state = Type.(function
  | Field (_, _, polarity) ->
    add_int state 0;
    add_int state polarity
  | Get _ -> add_int state 1
  | Set _ -> add_int state 2
  | GetSet _ -> add_int state 3
  | Method _ -> add_int state 4
)

let add_props_map state =
  SMap.iter (fun k p -> add state k; add_prop state p)

let add_exports_map state =
  SMap.iter (fun k _ -> add state k)
