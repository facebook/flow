(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
  | ArrH
  | ClassH
  | OptionalH
  | AbstractH
  | EvalH
  | PolyH
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
  | CustomFunH
  | IdxWrapperH
  | OpenPredH
  | TypeMapH
  | ReposH
  | ReposUpperH

let hash_of_def_ctor = Type.(function
  | InstanceT _ -> failwith "undefined hash of InstanceT"

  | AnyFunT -> AnyFunH
  | AnyObjT -> AnyObjH
  | AnyT -> AnyH
  | ArrT _ -> ArrH
  | BoolT b -> BoolH b
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
  | PolyT _ -> PolyH
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

  | AbstractT _ -> AbstractH
  | AnnotT _ -> AnnotH
  | AnyWithLowerBoundT _ -> AnyWithLowerBoundH
  | AnyWithUpperBoundT _ -> AnyWithUpperBoundH
  | BoundT _ -> BoundH
  | ChoiceKitT _ -> ChoiceKitH
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
  | ObjProtoT _ -> ObjProtoH
  | OpenPredT _ -> OpenPredH
  | ReposT _ -> ReposH
  | ReposUpperT _ -> ReposUpperH
  | ShapeT _ -> ShapeH
  | TaintT _ -> TaintH
  | ThisClassT _ -> ThisClassH
  | ThisTypeAppT _ -> ThisTypeAppH
  | TypeMapT _ -> TypeMapH
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

let add_props_map pmap =
  SMap.fold (fun k p h -> h |> add k |>  add (hash_of_prop p)) pmap

let add_exports_map tmap =
  add (SMap.keys tmap)
