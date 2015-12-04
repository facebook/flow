(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason_js
open Utils

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)

(* Some types represent definitions. These include numbers, strings, booleans,
   functions, classes, objects, arrays, and such. The shape of these types
   should be fairly obvious.
   Other types represent uses. These include function applications, class
   instantiations, property accesses, element accesses, operations such as
   addition, predicate refinements, etc. The shape of these types is somewhat
   trickier, but do follow a pattern. Typically, such a type consists of the
   arguments to the operation, and a type variable capturing the result of the
   operation. A full understanding of the semantics of such types requires a
   look at the subtyping relation, described in the module Flow_js. *)

(* Every type has (or should have, if not already) a "reason" for its
   existence. This information is captured in the type itself for now, but
   should be separated out in the future.
   Types that represent definitions point to the positions of such
   definitions (or values). Types that represent uses point to the positions of
   such uses (or operations). These reasons are logged, chained, etc. by the
   implementation of the subtyping algorithm, that effectively constructs a
   proof of the typing derivation based on these reasons as axioms. *)

type ident = int
type name = string

type t =
  (* open type variable *)
  (* A type variable (tvar) is an OpenT(reason, id) where id is an int index
     into a context's graph: a context's graph is a map from tvar ids to nodes
     (see below). *)
  (** Note: ids are globally unique. tvars are "owned" by a single context, but
      that context and its tvars may later be merged into other contexts. **)
  | OpenT of reason * ident

  (*************)
  (* def types *)
  (*************)

  (* TODO: constant types *)

  | NumT of reason * number_literal literal
  | StrT of reason * string literal
  | BoolT of reason * bool option
  | EmptyT of reason
  | MixedT of reason
  | AnyT of reason
  | NullT of reason
  | VoidT of reason

  | FunT of reason * static * prototype * funtype
  | FunProtoT of reason

  | ObjT of reason * objtype
  | ArrT of reason * t * t list

  (* type of a class *)
  | ClassT of t
  (* type of an instance of a class *)
  | InstanceT of reason * static * super * insttype

  (* type of an optional parameter *)
  | OptionalT of t
  (* type of a rest parameter *)
  | RestT of t
  | AbstractT of t

  (** A polymorphic type is like a type-level "function" that, when applied to
      lists of type arguments, generates types. Just like a function, a
      polymorphic type has a list of type parameters, represented as bound type
      variables. We say that type parameters are "universally quantified" (or
      "universal"): every substitution of type arguments for type parameters
      generates a type. Dually, we have "existentially quantified" (or
      "existential") type variables: such a type variable denotes some, possibly
      unknown, type. Universal type parameters may specify subtype constraints
      ("bounds"), which must be satisfied by any types they may be substituted
      by. Evaluation of existential types, which involves generating fresh type
      variables, never happens under polymorphic types; it is forced only when
      polymorphic types are applied. **)

  (* polymorphic type *)
  | PolyT of typeparam list * t
  (* type application *)
  | TypeAppT of t * t list
  (* bound type variable *)
  | BoundT of typeparam
  (* existential type variable *)
  | ExistsT of reason

  (* ? types *)
  | MaybeT of t

  | TaintT of reason

  (* & types *)
  | IntersectionT of reason * t list

  (* | types *)
  | UnionT of reason * t list

  (* generalizations of AnyT *)
  | UpperBoundT of t (* any upper bound of t *)
  | LowerBoundT of t (* any lower bound of t *)

  (* specializations of AnyT *)
  | AnyObjT of reason (* any object *)
  | AnyFunT of reason (* any function *)

  (* constrains some properties of an object *)
  | ShapeT of t
  | DiffT of t * t

  (* collects the keys of an object *)
  | KeysT of reason * t
  (* singleton string, matches exactly a given string literal *)
  | SingletonStrT of reason * string
  (* matches exactly a given number literal, for some definition of "exactly"
     when it comes to floats... *)
  | SingletonNumT of reason * number_literal
  (* singleton bool, matches exactly a given boolean literal *)
  | SingletonBoolT of reason * bool

  (* type aliases *)
  | TypeT of reason * t

  (* annotations *)
  (**
      A type that annotates a storage location performs two functions:
      * it constrains the types of values stored into the location
      * it masks the actual type of values retrieved from the location,
      giving instead a pro forma type which all such values are
      considered as having.

      In the former role, the annotated type behaves as an upper bound
      interacting with inflowing lower bounds - these interactions may
      occur e.g. as a result of values being stored to type-annotated
      variables, or arguments flowing to type-annotated parameters.

      In the latter role, the annotated type behaves as a lower bound,
      flowing to sites where values stored in the annotated location are
      used (such as users of a variable, or users of a parameter within
      a function body).

      When a type annotation resolves immediately to a concrete type
      (say, number = NumT or string = StrT), this single type would
      suffice to perform both roles. However, when an annotation has
      not yet been resolved, we can't simply use a type variable as a
      placeholder as we can elsewhere.

      TL;DR type variables are conductors; annotated types are insulators. :)

      For an annotated type, we must collect incoming lower bounds and
      downstream upper bounds without allowing them to interact with each
      other. If we did, the annotation would be "translucent", leaking
      type information about incoming values - failing to perform the
      second of the two roles noted above.

      Using a single type variable would allow exactly this propagation:
      it's essentially what type variables do.

      To accomplish the desired insulation we represent an annotation with
      a pair of type variables: a "sink" that collects lower bounds flowing
      into the annotation, and a "source" that collects downstream uses of
      the annotated location as upper bounds.

      The source tvar is linked to the unresolved definition's tvar.
      When that definition is resolved, the concrete type will flow
      into the annotation's source tvar as a lower bound.
      At that point two things will happen:

      * the source tvar will (as usual) evaluate the concrete definition
      against its accumulated upper bounds - this checks downstream use
      sites for compatibility with the annotated type.

      * a UnifyT edge, put in place at the time the AnnotT was built,
      will trigger a unification of the source and sink tvars. Critically,
      this will cause the concrete definition type to appear in the sink
      as an upper bound, prompting the check of all inflowing lower bounds
      against it.

      Of course, inflowing lower bounds and downstream upper bounds may
      continue to accumulate following this unification. As they do,
      they'll be checked against their respective sides of the bonded pair
      as usual.
  **)
  | AnnotT of t * t

  (* failure case for speculative matching *)
  | SpeculativeMatchFailureT of reason * t * t

  (* Stores exports (and potentially other metadata) for a module *)
  | ModuleT of reason * exporttypes

  (*************)
  (* use types *)
  (*************)

  (* operation on literals *)
  | SummarizeT of reason * t

  (* operations on runtime values, such as functions, objects, and arrays *)
  | CallT of reason * funtype
  | ApplyT of reason * t * funtype
  | MethodT of reason * propname * funtype
  | SetPropT of reason * propname * t
  | GetPropT of reason * propname * t
  | SetElemT of reason * t * t
  | GetElemT of reason * t * t
  | ReposLowerT of reason * t
  | ReposUpperT of reason * t

  (* operations on runtime types, such as classes and functions *)
  | ConstructorT of reason * t list * t
  | SuperT of reason * insttype
  | ExtendsT of t list * t * t

  (* overloaded +, could be subsumed by general overloading *)
  | AdderT of reason * t * t
  (* overloaded relational operator, could be subsumed by general overloading *)
  | ComparatorT of reason * t
  (* unary minus operator on numbers, allows negative number literals *)
  | UnaryMinusT of reason * t

  (* operation specifying a type refinement via a predicate *)
  | PredicateT of predicate * t

  (* == *)
  | EqT of reason * t

  (* logical operators *)
  | AndT of reason * t * t
  | OrT of reason * t * t
  | NotT of reason * t

  (* operation on polymorphic types *)
  (** SpecializeT(_, cache, targs, tresult) instantiates a polymorphic type with
      type arguments targs, and flows the result into tresult. If cache is set,
      it looks up a cache of existing instantiations for the type parameters of
      the polymorphic type, unifying the type arguments with those
      instantiations if such exist. **)
  | SpecializeT of reason * bool * t list * t

  (* operation on prototypes *)
  (** LookupT(_, strict, try_ts_on_failure, x, tresult) looks for property x in
      an object type, unifying its type with tresult. When x is not found, we
      have the following cases:

      (1) try_ts_on_failure is not empty, and we try to look for property x in
      the next object type in that list;

      (2) strict = None, so no error is reported;

      (3) strict = Some reason, so the position in reason is blamed.
  **)
  | LookupT of reason * reason option * t list * string * t

  (* operations on objects *)
  | ObjAssignT of reason * t * t * string list * bool
  | ObjFreezeT of reason * t
  | ObjRestT of reason * string list * t
  | ObjSealT of reason * t
  (** test that something is object-like, returning a default type otherwise **)
  | ObjTestT of reason * t * t

  (* assignment rest element in array pattern *)
  | ArrRestT of reason * int * t

  (* Guarded unification (bidirectional).
     Remodel as unidirectional GuardT(l,u)? *)
  | UnifyT of t * t

  (* unifies with incoming concrete lower bound *)
  | BecomeT of reason * t

  (* manage a worklist of types to be concretized *)
  | ConcretizeT of t * t list * t list * t
  (* sufficiently concrete type *)
  | ConcreteT of t

  (* Keys *)
  | GetKeysT of reason * t
  | HasOwnPropT of reason * string

  (* Element access *)
  | ElemT of reason * t * t

  (* Module import handling *)
  | CJSRequireT of reason * t
  | ImportModuleNsT of reason * t
  | ImportDefaultT of reason * (string * string) * t
  | ImportNamedT of reason * string * t
  | ImportTypeT of reason * t
  | ImportTypeofT of reason * t

  (* Module export handling *)
  | CJSExtractNamedExportsT of
      reason
      * (* local ModuleT *) t
      * (* 't_out' to receive the resolved ModuleT *) t_out
  | SetNamedExportsT of reason * t SMap.t * t_out

and predicate =
  | AndP of predicate * predicate
  | OrP of predicate * predicate
  | NotP of predicate

  (* mechanism to handle binary tests where both sides need to be evaluated *)
  | LeftP of binary_test * t
  | RightP of binary_test * t

  | ExistsP (* truthy *)
  | TrueP (* true *)
  | FalseP (* false *)
  | NullP (* null *)
  | MaybeP (* null or undefined *)

  | BoolP (* boolean *)
  | FunP (* function *)
  | NumP (* number *)
  | ObjP (* object *)
  | StrP (* string *)
  | VoidP (* undefined *)

  | ArrP (* Array.isArray *)

and binary_test =
  (* e1 instanceof e2 *)
  | Instanceof
  (* e1.key === e2 *)
  | SentinelProp of string

and 'a literal =
  | Literal of 'a
  | Truthy
  | Falsy
  | AnyLiteral

and number_literal = (float * string)

(* used by FunT and CallT *)
and funtype = {
  this_t: t;
  params_tlist: t list;
  params_names: string list option;
  return_t: t;
  closure_t: int;
  changeset: Changeset.t
}

and objtype = {
  flags: flags;
  dict_t: dicttype option;
  props_tmap: int;
  proto_t: prototype;
}

and propname = reason * name

and sealtype =
  | UnsealedInFile of Loc.filename option
  | Sealed

and flags = {
  frozen: bool;
  sealed: sealtype;
  exact: bool;
}

and dicttype = {
  dict_name: string option;
  key: t;
  value: t;
}

and polarity =
  | Negative      (* contravariant *)
  | Neutral       (* invariant *)
  | Positive      (* covariant *)

and insttype = {
  class_id: ident;
  type_args: t SMap.t;
  arg_polarities: polarity SMap.t;
  fields_tmap: int;
  methods_tmap: int;
  mixins: bool;
  structural: bool;
}

and exporttypes = {
  (**
   * tmap used to store individual, named ES exports as generated by `export`
   * statements in a module. Note that this includes `export type` as well.
   *
   * Note that CommonJS modules may also populate this tmap if their export
   * type is an object (that object's properties become named exports) or if
   * it has any "type" exports via `export type ...`.
   *)
  exports_tmap: int;

  (**
   * This stores the CommonJS export type when applicable and is used as the
   * exact return type for calls to require(). This slot doesn't apply to pure
   * ES modules.
   *)
  cjs_export: t option;
}

and typeparam = {
  reason: reason;
  name: string;
  bound: t;
  polarity: polarity
}

and prototype = t

and super = t

and static = t

and properties = t SMap.t

and t_out = t

let compare = Pervasives.compare

let open_tvar tvar =
  match tvar with
  | OpenT(reason,id) -> (reason,id)
  | _ -> assert false

module type PrimitiveType = sig
  val desc: string
  val make: reason -> t
end

module type PrimitiveT = sig
  val desc: string
  val t: t
  val at: Loc.t -> t
  val why: reason -> t
  val tag: string -> t
end

module Primitive (P: PrimitiveType) = struct
  let desc = P.desc
  let t = P.make (reason_of_string desc)
  let at tok = P.make (mk_reason desc tok)
  let why reason = P.make (replace_reason desc reason)
  let tag s = P.make (reason_of_string (desc ^ " (" ^ s ^ ")"))
  let make = P.make
end

module NumT = Primitive (struct
  let desc = "number"
  let make r = NumT (r, AnyLiteral)
end)

module StrT = Primitive (struct
  let desc = "string"
  let make r = StrT (r, AnyLiteral)
end)

module BoolT = Primitive (struct
  let desc = "boolean"
  let make r = BoolT (r, None)
end)

module MixedT = Primitive (struct
  let desc = "mixed"
  let make r = MixedT r
end)

module EmptyT = Primitive (struct
  let desc = ""
  let make r = EmptyT r
end)

module AnyT = Primitive (struct
  let desc = "any"
  let make r = AnyT r
end)

module VoidT = Primitive (struct
  let desc = "undefined"
  let make r = VoidT r
end)

module NullT = Primitive (struct
  let desc = "null"
  let make r = NullT r
end)

(* The typechecking algorithm often needs to maintain sets of types, or more
   generally, maps of types (for logging we need to associate some provenanced
   information to types). *)

module TypeSet : Set.S with type elt = t = Set.Make(struct
  type elt = t
  type t = elt
  let compare = compare
end)

module TypeMap : MapSig with type key = t = MyMap(struct
  type key = t
  type t = key
  let compare = compare
end)

(* def types vs. use types *)
let is_use = function
  | CallT _
  | ApplyT _
  | MethodT _
  | ReposLowerT _
  | SetPropT _
  | GetPropT _
  | SetElemT _
  | GetElemT _
  | ConstructorT _
  | SuperT _
  | ExtendsT _
  | AdderT _
  | AndT _
  | OrT _
  | ComparatorT _
  | PredicateT _
  | EqT _
  | SpecializeT _
  | LookupT _
  | ObjAssignT _
  | ObjFreezeT _
  | ObjRestT _
  | ObjSealT _
  | ObjTestT _
  | ArrRestT _
  | UnaryMinusT _
  | UnifyT _
  | GetKeysT _
  | HasOwnPropT _
  | ElemT _
  | ConcreteT _
  | ConcretizeT _
  | BecomeT _
  | CJSRequireT _
  | ImportModuleNsT _
  | ImportDefaultT _
  | ImportNamedT _
  | ImportTypeT _
  | ImportTypeofT _
  | CJSExtractNamedExportsT _
  | SetNamedExportsT _
    -> true

  | _ -> false


(* Usually types carry enough information about the "reason" for their
   existence (e.g., position in code, introduction/elimination rules in
   the type system), so printing the reason provides a good idea of what the
   type means to the programmer. *)

let rec reason_of_t = function
  (* note: keep in order of decls in constraint_js *)

  | OpenT (reason,_)

  | NumT (reason, _)
  | StrT (reason, _)
  | BoolT (reason, _)
  | EmptyT reason
  | MixedT reason
  | AnyT reason
  | NullT reason
  | VoidT reason

  | FunT (reason,_,_,_)
  | FunProtoT reason
      -> reason

  | PolyT (_,t) ->
      prefix_reason "polymorphic type: " (reason_of_t t)
  | BoundT typeparam ->
      typeparam.reason
  | ExistsT reason ->
      reason

  | ObjT (reason,_)
  | ArrT (reason,_,_)
      -> reason

  | ClassT t ->
      prefix_reason "class type: " (reason_of_t t)

  | InstanceT (reason,_,_,_)
  | SuperT (reason,_)

  | CallT (reason, _)
  | ApplyT (reason, _, _)

  | MethodT (reason,_,_)
  | SetPropT (reason,_,_)
  | GetPropT (reason,_,_)

  | SetElemT (reason,_,_)
  | GetElemT (reason,_,_)

  | ConstructorT (reason,_,_)

  | AdderT (reason,_,_)
  | ComparatorT (reason,_)
  | UnaryMinusT (reason, _)

  | AndT (reason, _, _)
  | OrT (reason, _, _)
  | NotT (reason, _)

  | TypeT (reason,_)
  | BecomeT (reason, _)
      -> reason

  | ReposLowerT (reason, _) -> reason
  | ReposUpperT (reason, _) -> reason

  | AnnotT (_, assume_t) ->
      reason_of_t assume_t

  | ExtendsT (_,_,t) ->
      prefix_reason "extends " (reason_of_t t)

  | OptionalT t ->
      prefix_reason "optional " (reason_of_t t)

  | RestT t ->
      prefix_reason "rest array of " (reason_of_t t)

  | AbstractT t ->
      prefix_reason "abstract " (reason_of_t t)

  | PredicateT (pred,t) -> reason_of_t t

  | EqT (reason, t) ->
      reason

  | SpecializeT(reason,_,_,_)
      -> reason

  | TypeAppT(t,_)
      -> prefix_reason "type application of " (reason_of_t t)

  | MaybeT t ->
      prefix_reason "?" (reason_of_t t)

  | TaintT (r) ->
      r

  | IntersectionT (reason, _) ->
      reason

  | UnionT (reason, _) ->
      reason

  | LookupT(reason, _, _, _, _) ->
      reason

  | UnifyT(_,t) ->
      reason_of_t t

  | ObjAssignT (reason, _, _, _, _)
  | ObjFreezeT (reason, _)
  | ObjRestT (reason, _, _)
  | ObjSealT (reason, _)
  | ObjTestT (reason, _, _)
    ->
      reason

  | ArrRestT (reason, _, _) ->
      reason

  | UpperBoundT (t)
  | LowerBoundT (t)
      -> reason_of_t t

  | AnyObjT reason ->
      reason
  | AnyFunT reason ->
      reason

  | ShapeT (t)
      -> reason_of_t t
  | DiffT (t, _)
      -> reason_of_t t

  | KeysT (reason, _)
  | SingletonStrT (reason, _)
  | SingletonNumT (reason, _)
  | SingletonBoolT (reason, _) -> reason

  | GetKeysT (reason, _) -> reason
  | HasOwnPropT (reason, _) -> reason

  | ElemT (reason, _, _) -> reason

  | ConcretizeT (t, _, _, _) -> reason_of_t t
  | ConcreteT (t) -> reason_of_t t

  | SpeculativeMatchFailureT (reason, _, _) -> reason

  | SummarizeT (reason, t) -> reason

  | ModuleT (reason, _) -> reason

  | CJSRequireT (reason, _) -> reason
  | ImportModuleNsT (reason, _) -> reason
  | ImportDefaultT (reason, _, _) -> reason
  | ImportNamedT (reason, _, _) -> reason
  | ImportTypeT (reason, _) -> reason
  | ImportTypeofT (reason, _) -> reason
  | CJSExtractNamedExportsT (reason, _, _) -> reason
  | SetNamedExportsT (reason, _, _) -> reason


(* helper: we want the tvar id as well *)
(* NOTE: uncalled for now, because ids are nondetermistic
   due to parallelism, which messes up test diffs. Should
   add a config, but for now must uncomment impl to use *)
let reason_of_t_add_id = reason_of_t
(* function
| OpenT (r, id) -> prefix_reason (spf "%d: " id) r
| t -> reason_of_t t *)


let desc_of_t t = desc_of_reason (reason_of_t t)

let loc_of_t t = loc_of_reason (reason_of_t t)

let rec loc_of_predicate = function
  | AndP (p1,p2)
  | OrP (p1,p2)
    -> loc_of_predicate p1

  | NotP p
    -> loc_of_predicate p

  | LeftP (_, t)
  | RightP (_, t)
    -> loc_of_t t

  | ExistsP
  | TrueP
  | FalseP
  | NullP
  | MaybeP

  | BoolP
  | FunP
  | NumP
  | ObjP
  | StrP
  | VoidP

  | ArrP
    -> Loc.none (* TODO!!!!!!!!!!!! *)


(* TODO make a type visitor *)
let rec mod_reason_of_t f = function

  | OpenT (reason, t) -> OpenT (f reason, t)
  | NumT (reason, t) -> NumT (f reason, t)
  | StrT (reason, t) -> StrT (f reason, t)
  | BoolT (reason, t) -> BoolT (f reason, t)
  | EmptyT reason -> EmptyT (f reason)
  | MixedT reason -> MixedT (f reason)
  | AnyT reason -> AnyT (f reason)
  | NullT reason -> NullT (f reason)
  | VoidT reason -> VoidT (f reason)

  | FunT (reason, s, p, ft) -> FunT (f reason, s, p, ft)
  | FunProtoT (reason) -> FunProtoT (f reason)
  | PolyT (plist, t) -> PolyT (plist, mod_reason_of_t f t)
  | BoundT { reason; name; bound; polarity } ->
    BoundT { reason = f reason; name; bound; polarity }
  | ExistsT reason -> ExistsT (f reason)
  | ObjT (reason, ot) -> ObjT (f reason, ot)
  | ArrT (reason, t, ts) -> ArrT (f reason, t, ts)

  | ClassT t -> ClassT (mod_reason_of_t f t)
  | InstanceT (reason, st, su, inst) -> InstanceT (f reason, st, su, inst)
  | SuperT (reason, inst) -> SuperT (f reason, inst)
  | ExtendsT (ts, t, tc) -> ExtendsT (ts, t, mod_reason_of_t f tc)

  | CallT (reason, ft) -> CallT (f reason, ft)
  | ApplyT (reason, l, ft) -> ApplyT (f reason, l, ft)

  | MethodT (reason, name, ft) -> MethodT(f reason, name, ft)
  | ReposLowerT (reason, t) -> ReposLowerT (f reason, t)
  | ReposUpperT (reason, t) -> ReposUpperT (f reason, t)
  | SetPropT (reason, n, t) -> SetPropT (f reason, n, t)
  | GetPropT (reason, n, t) -> GetPropT (f reason, n, t)

  | SetElemT (reason, it, et) -> SetElemT (f reason, it, et)
  | GetElemT (reason, it, et) -> GetElemT (f reason, it, et)

  | ConstructorT (reason, ts, t) -> ConstructorT (f reason, ts, t)

  | AdderT (reason, rt, lt) -> AdderT (f reason, rt, lt)
  | ComparatorT (reason, t) -> ComparatorT (f reason, t)
  | UnaryMinusT (reason, t) -> UnaryMinusT (f reason, t)

  | TypeT (reason, t) -> TypeT (f reason, t)
  | AnnotT (assert_t, assume_t) ->
      AnnotT (mod_reason_of_t f assert_t, mod_reason_of_t f assume_t)
  | BecomeT (reason, t) -> BecomeT (f reason, t)

  | OptionalT t -> OptionalT (mod_reason_of_t f t)

  | RestT t -> RestT (mod_reason_of_t f t)

  | AbstractT t -> AbstractT (mod_reason_of_t f t)

  | PredicateT (pred, t) -> PredicateT (pred, mod_reason_of_t f t)

  | EqT (reason, t) -> EqT (f reason, t)

  | AndT (reason, t1, t2) -> AndT (f reason, t1, t2)
  | OrT (reason, t1, t2) -> OrT (f reason, t1, t2)
  | NotT (reason, t) -> NotT (f reason, t)

  | SpecializeT(reason, cache, ts, t) -> SpecializeT (f reason, cache, ts, t)

  | TypeAppT (t, ts) -> TypeAppT (mod_reason_of_t f t, ts)

  | MaybeT t -> MaybeT (mod_reason_of_t f t)

  | TaintT (r) -> TaintT (f r)

  | IntersectionT (reason, ts) -> IntersectionT (f reason, ts)

  | UnionT (reason, ts) -> UnionT (f reason, ts)

  | LookupT (reason, r2, ts, x, t) -> LookupT (f reason, r2, ts, x, t)

  | UnifyT (t, t2) -> UnifyT (mod_reason_of_t f t, mod_reason_of_t f t2)

  | ObjAssignT (reason, t, t2, filter, resolve) ->
      ObjAssignT (f reason, t, t2, filter, resolve)
  | ObjFreezeT (reason, t) -> ObjFreezeT (f reason, t)
  | ObjRestT (reason, t, t2) -> ObjRestT (f reason, t, t2)
  | ObjSealT (reason, t) -> ObjSealT (f reason, t)
  | ObjTestT (reason, t1, t2) -> ObjTestT (f reason, t1, t2)

  | ArrRestT (reason, i, t) -> ArrRestT (f reason, i, t)

  | UpperBoundT t -> UpperBoundT (mod_reason_of_t f t)
  | LowerBoundT t -> LowerBoundT (mod_reason_of_t f t)

  | AnyObjT reason -> AnyObjT (f reason)
  | AnyFunT reason -> AnyFunT (f reason)

  | ShapeT t -> ShapeT (mod_reason_of_t f t)
  | DiffT (t1, t2) -> DiffT (mod_reason_of_t f t1, t2)

  | KeysT (reason, t) -> KeysT (f reason, t)
  | SingletonStrT (reason, t) -> SingletonStrT (f reason, t)
  | SingletonNumT (reason, t) -> SingletonNumT (f reason, t)
  | SingletonBoolT (reason, t) -> SingletonBoolT (f reason, t)

  | GetKeysT (reason, t) -> GetKeysT (f reason, t)
  | HasOwnPropT (reason, t) -> HasOwnPropT (f reason, t)

  | ElemT (reason, t, t2) -> ElemT (f reason, t, t2)

  | ConcretizeT (t1, ts1, ts2, t2) ->
      ConcretizeT (mod_reason_of_t f t1, ts1, ts2, t2)
  | ConcreteT t -> ConcreteT (mod_reason_of_t f t)

  | SpeculativeMatchFailureT (reason, t1, t2) ->
      SpeculativeMatchFailureT (f reason, t1, t2)

  | SummarizeT (reason, t) -> SummarizeT (f reason, t)

  | ModuleT (reason, exports) -> ModuleT (f reason, exports)

  | CJSRequireT (reason, t) -> CJSRequireT (f reason, t)
  | ImportModuleNsT (reason, t) -> ImportModuleNsT (f reason, t)
  | ImportDefaultT (reason, name, t) -> ImportDefaultT (f reason, name, t)
  | ImportNamedT (reason, name, t) -> ImportNamedT (f reason, name, t)
  | ImportTypeT (reason, t) -> ImportTypeT (f reason, t)
  | ImportTypeofT (reason, t) -> ImportTypeofT (f reason, t)

  | CJSExtractNamedExportsT (reason, t1, t2) -> CJSExtractNamedExportsT (f reason, t1, t2)
  | SetNamedExportsT (reason, tmap, t_out) -> SetNamedExportsT(f reason, tmap, t_out)


(* type comparison mod reason *)
let reasonless_compare =
  let swap_reason t r = mod_reason_of_t (fun _ -> r) t in
  fun t t' ->
    if t == t' then 0 else
    compare t (swap_reason t' (reason_of_t t))


(* printing *)
let string_of_ctor = function
  | OpenT _ -> "OpenT"
  | NumT _ -> "NumT"
  | StrT _ -> "StrT"
  | BoolT _ -> "BoolT"
  | EmptyT _ -> "EmptyT"
  | MixedT _ -> "MixedT"
  | AnyT _ -> "AnyT"
  | NullT _ -> "NullT"
  | VoidT _ -> "VoidT"
  | FunT _ -> "FunT"
  | FunProtoT _ -> "FunProtoT"
  | PolyT _ -> "PolyT"
  | BoundT _ -> "BoundT"
  | ExistsT _ -> "ExistsT"
  | ObjT _ -> "ObjT"
  | ArrT _ -> "ArrT"
  | ClassT _ -> "ClassT"
  | InstanceT _ -> "InstanceT"
  | SummarizeT _ -> "SummarizeT"
  | SuperT _ -> "SuperT"
  | ExtendsT _ -> "ExtendsT"
  | CallT _ -> "CallT"
  | ApplyT _ -> "ApplyT"
  | MethodT _ -> "MethodT"
  | ReposLowerT _ -> "ReposLowerT"
  | ReposUpperT _ -> "ReposUpperT"
  | SetPropT _ -> "SetPropT"
  | GetPropT _ -> "GetPropT"
  | SetElemT _ -> "SetElemT"
  | GetElemT _ -> "GetElemT"
  | ConstructorT _ -> "ConstructorT"
  | AdderT _ -> "AdderT"
  | ComparatorT _ -> "ComparatorT"
  | TypeT _ -> "TypeT"
  | AnnotT _ -> "AnnotT"
  | BecomeT _ -> "BecomeT"
  | OptionalT _ -> "OptionalT"
  | RestT _ -> "RestT"
  | AbstractT _ -> "AbstractT"
  | PredicateT _ -> "PredicateT"
  | EqT _ -> "EqT"
  | AndT _ -> "AndT"
  | OrT _ -> "OrT"
  | NotT _ -> "NotT"
  | SpecializeT _ -> "SpecializeT"
  | TypeAppT _ -> "TypeAppT"
  | MaybeT _ -> "MaybeT"
  | TaintT _ -> "TaintT"
  | IntersectionT _ -> "IntersectionT"
  | UnionT _ -> "UnionT"
  | LookupT _ -> "LookupT"
  | UnifyT _ -> "UnifyT"
  | ObjAssignT _ -> "ObjAssignT"
  | ObjFreezeT _ -> "ObjFreezeT"
  | ObjRestT _ -> "ObjRestT"
  | ObjSealT _ -> "ObjSealT"
  | ObjTestT _ -> "ObjTestT"
  | ArrRestT _ -> "ArrRestT"
  | UnaryMinusT _ -> "UnaryMinusT"
  | UpperBoundT _ -> "UpperBoundT"
  | LowerBoundT _ -> "LowerBoundT"
  | AnyObjT _ -> "AnyObjT"
  | AnyFunT _ -> "AnyFunT"
  | ShapeT _ -> "ShapeT"
  | DiffT _ -> "DiffT"
  | KeysT _ -> "KeysT"
  | SingletonStrT _ -> "SingletonStrT"
  | SingletonNumT _ -> "SingletonNumT"
  | SingletonBoolT _ -> "SingletonBoolT"
  | GetKeysT _ -> "GetKeysT"
  | HasOwnPropT _ -> "HasOwnPropT"
  | ElemT _ -> "ElemT"
  | ConcretizeT _ -> "ConcretizeT"
  | ConcreteT _ -> "ConcreteT"
  | SpeculativeMatchFailureT _ -> "SpeculativeMatchFailureT"
  | ImportModuleNsT _ -> "ImportModuleNsT"
  | ImportDefaultT _ -> "ImportDefaultT"
  | ImportNamedT _ -> "ImportNamedT"
  | ImportTypeT _ -> "ImportTypeT"
  | ImportTypeofT _ -> "ImportTypeofT"
  | ModuleT _ -> "ModuleT"
  | CJSRequireT _ -> "CJSRequireT"
  | CJSExtractNamedExportsT _ -> "CJSExtractNamedExportsT"
  | SetNamedExportsT _ -> "SetNamedExportsT"


let string_of_binary_test = function
  | Instanceof -> "instanceof"
  | SentinelProp key -> "sentinel prop " ^ key


let rec string_of_predicate = function
  | AndP (p1,p2) ->
      (string_of_predicate p1) ^ " && " ^ (string_of_predicate p2)
  | OrP (p1,p2) ->
      (string_of_predicate p1) ^ " || " ^ (string_of_predicate p2)
  | NotP p -> "not " ^ (string_of_predicate p)
  | LeftP (b, t) ->
      spf "left operand of %s with right operand = %s"
        (string_of_binary_test b) (desc_of_t t)
  | RightP (b, t) ->
      spf "right operand of %s with left operand = %s"
        (string_of_binary_test b) (desc_of_t t)
  | ExistsP -> "truthy"
  | TrueP -> "true"
  | FalseP -> "false"
  | NullP -> "null"
  | MaybeP -> "null or undefined"

  (* typeof *)
  | VoidP -> "undefined"
  | BoolP -> "boolean"
  | StrP -> "string"
  | NumP -> "number"
  | FunP -> "function"
  | ObjP -> "object"

  (* Array.isArray *)
  | ArrP -> "array"
