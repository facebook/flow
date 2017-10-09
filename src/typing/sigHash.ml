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
type hash =
  | NumH of Type.number_literal Type.literal
  | StrH of string Type.literal
  | BoolH of bool option
  | EmptyH
  | MixedH of Type.mixed_flavor
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
  | TaintH
  | IntersectionH
  | UnionH
  | AnyWithLowerBoundH
  | AnyWithUpperBoundH
  | MergedH
  | AnyObjH
  | AnyFunH
  | ShapeH
  | DiffH
  | KeysH
  | SingletonStrH of string
  | SingletonNumH of Type.number_literal
  | SingletonBoolH of bool
  | TypeH
  | AnnotH
  | ModuleH
  | ExtendsH
  | ChoiceKitH
  | TvarDestructorH
  | CustomFunH
  | IdxWrapperH
  | OpenPredH
  | CharSetH
  | ReposH
  | ReposUpperH
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
  | SentinelPropTestH
  | IdxUnwrapH
  | IdxUnMaybeifyH
  | CallLatentPredH
  | CallOpenPredH
  | SubstOnPredH
  | RefineH
  | ResolveSpreadH
  | CondH

let hash_of_def_ctor = Type.(function
  | InstanceT _ -> failwith "undefined hash of InstanceT"
  | PolyT _ -> failwith "undefined hash of PolyT"

  | AnyFunT -> AnyFunH
  | AnyObjT -> AnyObjH
  | AnyT -> AnyH
  | ArrT _ -> ArrH
  | BoolT b -> BoolH b
  | CharSetT _ -> CharSetH
  | ClassT _ -> ClassH
  | EmptyT -> EmptyH
  | FunT _ -> FunH
  | IntersectionT _ -> IntersectionH
  | MaybeT _ -> MaybeH
  | MixedT m -> MixedH m
  | NullT -> NullH
  | NumT n -> NumH n
  | ObjT _ -> ObjH
  | OptionalT _ -> OptionalH
  | SingletonBoolT b -> SingletonBoolH b
  | SingletonNumT n -> SingletonNumH n
  | SingletonStrT s -> SingletonStrH s
  | StrT s -> StrH s
  | TypeT _ -> TypeH
  | TypeAppT _ -> TypeAppH
  | VoidT -> VoidH
  | UnionT _ -> UnionH
)

let hash_of_ctor = Type.(function
  | OpenT _ -> failwith "undefined hash of OpenT"
  | OpaqueT _ -> failwith "undefined hash of OpaqueT"

  | AnnotT _ -> AnnotH
  | AnyWithLowerBoundT _ -> AnyWithLowerBoundH
  | AnyWithUpperBoundT _ -> AnyWithUpperBoundH
  | MergedT _ -> MergedH
  | BoundT _ -> BoundH
  | ChoiceKitT _ -> ChoiceKitH
  | TypeDestructorTriggerT _ -> TvarDestructorH
  | CustomFunT _ -> CustomFunH
  | DefT (_, t) -> hash_of_def_ctor t
  | DiffT _ -> DiffH
  | EvalT _ -> EvalH
  | ExactT _ -> ExactH
  | ExistsT _ -> ExistsH
  | ExtendsT _ -> ExtendsH
  | FunProtoT _ -> FunProtoH
  | FunProtoApplyT _ -> FunProtoApplyH
  | FunProtoBindT _ -> FunProtoBindH
  | FunProtoCallT _ -> FunProtoCallH
  | IdxWrapper _ -> IdxWrapperH
  | KeysT _ -> KeysH
  | ModuleT _ -> ModuleH
  | NullProtoT _ -> NullProtoH
  | ObjProtoT _ -> ObjProtoH
  | MatchingPropT _ -> MatchingPropH
  | OpenPredT _ -> OpenPredH
  | ReposT _ -> ReposH
  | ReposUpperT _ -> ReposUpperH
  | ShapeT _ -> ShapeH
  | TaintT _ -> TaintH
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
  | SentinelPropTestT _ -> SentinelPropTestH
  | IdxUnwrap _ -> IdxUnwrapH
  | IdxUnMaybeifyT _ -> IdxUnMaybeifyH
  | CallLatentPredT _ -> CallLatentPredH
  | CallOpenPredT _ -> CallOpenPredH
  | SubstOnPredT _ -> SubstOnPredH
  | RefineT _ -> RefineH
  | ResolveSpreadT _ -> ResolveSpreadH
  | CondT _ -> CondH
)

type prop_hash =
  | FieldH of Type.polarity
  | GetH
  | SetH
  | GetSetH
  | MethodH

let hash_of_prop = Type.(function
  | Field (_, polarity) -> FieldH polarity
  | Get _ -> GetH
  | Set _ -> SetH
  | GetSet _ -> GetSetH
  | Method _ -> MethodH
)

type t = Digest.t
let empty = Digest.string ""

let add x t =
  Digest.string (Marshal.to_string (x, t) [])

let add_type type_ =
  add (hash_of_ctor type_)

let add_use use =
  add (hash_of_use_ctor use)

let add_props_map pmap =
  SMap.fold (fun k p h -> h |> add k |>  add (hash_of_prop p)) pmap

let add_exports_map tmap =
  add (SMap.keys tmap)
