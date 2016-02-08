(**
 * Copyright (c) 2013-present, Facebook, Inc.
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

module rec TypeTerm : sig

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
    | FunProtoT of reason       (* Function.prototype *)
    | FunProtoApplyT of reason  (* Function.prototype.apply *)
    | FunProtoBindT of reason   (* Function.prototype.bind *)
    | FunProtoCallT of reason   (* Function.prototype.call *)

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

    (* type expression whose evaluation is deferred *)
    (* Usually a type expression is evaluated by splitting it into a def type and
       a use type, and flowing the former to the latter: the def type is the
       "main" argument, and the use type contains the type operation, other
       arguments, and a tvar to hold the result. However, sometimes a type
       expression may need to be kept in explicit form, with the type operation
       and other arguments split out into a "deferred" use type `defer_use_t`,
       whose evaluation state is tracked in the context by an identifier id: When
       defer_use_t is evaluated, id points to a tvar containing the result of
       evaluation. The explicit form simplifies other tasks, like substitution,
       but otherwise works in much the same way as usual. **)
    | EvalT of t * defer_use_t * int

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
    (* this-abstracted class *)
    | ThisClassT of t
    (* this instantiation *)
    | ThisTypeAppT of t * t * t list
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
    | UnionT of reason * UnionRep.t

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

    (* Stores exports (and potentially other metadata) for a module *)
    | ModuleT of reason * exporttypes

    (** Here's to the crazy ones. The misfits. The rebels. The troublemakers. The
        round pegs in the square holes. **)

    (* failure case for speculative matching *)
    | SpeculativeMatchT of reason * t * use_t
    (* repositioning uses *)
    | ReposUpperT of reason * t
    (* util for deciding subclassing relations *)
    | ExtendsT of t list * t * t

    (* Sigil representing functions that the type system is not expressive enough
       to annotate, so we customize their behavior internally. *)
    | CustomFunT of reason * custom_fun_kind

  and defer_use_t =
    (* type of a variable / parameter / property extracted from a pattern *)
    | DestructuringT of reason * selector

  and use_t =
    (* def types can be used as upper bounds *)
    | UseT of t

    (*************)
    (* use types *)
    (*************)

    (* operation on literals *)
    | SummarizeT of reason * t

    (* operations on runtime values, such as functions, objects, and arrays *)
    | ApplyT of reason * t * funtype
    | BindT of reason * funtype
    | CallT of reason * funtype
    | MethodT of reason * propname * funtype
    | SetPropT of reason * propname * t
    | GetPropT of reason * propname * t
    | SetElemT of reason * t * t
    | GetElemT of reason * t * t
    | ReposLowerT of reason * t

    (* operations on runtime types, such as classes and functions *)
    | ConstructorT of reason * t list * t
    | SuperT of reason * insttype
    | MixinT of reason * t

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

    (**
     * A special (internal-only) type used to "reify" a TypeT back down to it's
     * value-space type. One way to think of this is as the inverse of `typeof`.
     *
     * Note that there is no syntax for specifying this type. To surface such a
     * type would result in code that cannot be compiled without extensive type
     * info (or possibly at all!).
     *)
    | ReifyTypeT of reason * t_out

    (* operation on polymorphic types *)
    (** SpecializeT(_, cache, targs, tresult) instantiates a polymorphic type with
        type arguments targs, and flows the result into tresult. If cache is set,
        it looks up a cache of existing instantiations for the type parameters of
        the polymorphic type, unifying the type arguments with those
        instantiations if such exist. **)
    | SpecializeT of reason * bool * t list * t
    (* operation on this-abstracted classes *)
    | ThisSpecializeT of reason * t * t
    (* variance check on polymorphic types *)
    | VarianceCheckT of reason * t list * polarity

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

    (* Keys *)
    | GetKeysT of reason * t
    | HasOwnPropT of reason * string
    | HasPropT of reason * reason option * string

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
    | SetStarExportsT of reason * t * t_out

    | DebugPrintT of reason

    (** Here's to the crazy ones. The misfits. The rebels. The troublemakers. The
        round pegs in the square holes. **)

    (* manage a worklist of types to be concretized *)
    | ConcretizeT of t * t list * t list * use_t
    (* sufficiently concrete type *)
    | ConcreteT of use_t

  and predicate =
    | AndP of predicate * predicate
    | OrP of predicate * predicate
    | NotP of predicate

    (* mechanism to handle binary tests where both sides need to be evaluated *)
    | LeftP of binary_test * t
    | RightP of binary_test * t

    | ExistsP (* truthy *)
    | NullP (* null *)
    | MaybeP (* null or undefined *)

    | SingletonBoolP of bool (* true or false *)
    | SingletonStrP of string (* string literal *)

    | BoolP (* boolean *)
    | FunP (* function *)
    | NumP (* number *)
    | ObjP (* object *)
    | StrP (* string *)
    | VoidP (* undefined *)

    | ArrP (* Array.isArray *)

    (* `if (a.b)` yields `flow (a, PredicateT(PropExistsP "b", tout))` *)
    | PropExistsP of string

  and binary_test =
    (* e1 instanceof e2 *)
    | InstanceofTest
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

  and selector =
  | Prop of string
  | Elem of t
  | ObjRest of string list
  | ArrRest of int

  and prototype = t

  and super = t

  and static = t

  and properties = t SMap.t

  and t_out = t

  and custom_fun_kind =
  (* special builtins *)
  | ObjectAssign
  | ObjectGetPrototypeOf

  (* Facebookisms *)
  | Merge
  | MergeDeepInto
  | MergeInto
  | Mixin

end = TypeTerm

(* We encapsulate UnionT's internal structure
   so we can use specialized representations for
   unions with exploitable regularity and/or
   simplicity properties, e.g. enums.

   Representations are opaque. `make` chooses a
   representation internally, and client code which
   needs to interact with member types directly
   can do so via `members`, which provides access
   via the standard list representation.
 *)

and UnionRep : sig
  type t
  val make: TypeTerm.t list -> t
  val enum_base: t -> TypeTerm.t option
  val members: t -> TypeTerm.t list
  val map: (TypeTerm.t -> TypeTerm.t) -> t -> t
  val quick_mem: TypeTerm.t -> t -> bool option
end = struct

  (* canonicalize a type w.r.t. enum membership *)
  let canon t = TypeTerm.(
    match t with
    | SingletonStrT _ -> t
    | StrT (r, Literal s) -> SingletonStrT (r, s)
    | SingletonNumT _ -> t
    | NumT (r, Literal lit) -> SingletonNumT (r, lit)
    | _ -> t
  )

  (* enums are stored as singleton type sets *)
  module EnumSet : Set.S with type elt = TypeTerm.t = Set.Make(struct
    type elt = TypeTerm.t
    type t = elt
    let compare x y = TypeTerm.(
      match x, y with
      | SingletonStrT (_, a), SingletonStrT (_, b) ->
        Pervasives.compare a b
      | SingletonNumT (_, (a, _)), SingletonNumT (_, (b, _)) ->
        Pervasives.compare a b
      | _ -> Pervasives.compare x y
    )
  end)

  (* given a type, return corresponding enum base type if any *)
  let base_of_t = TypeTerm.(
    let str = Some (StrT (reason_of_string "string enum", AnyLiteral)) in
    let num = Some (NumT (reason_of_string "number enum", AnyLiteral)) in
    fun t ->
      match t with
      | SingletonStrT _ -> str
      | SingletonNumT _ -> num
      | _ -> None
  )

  (* union rep is:
     - list of members in declaration order
     - if union is an enum (set of singletons over a common base)
       then Some (base, set)
     (additional specializations probably to come)
   *)
  type t = TypeTerm.t list * (TypeTerm.t * EnumSet.t) option

  (* given a list of members, build a rep.
     specialized reps are used on compatible type lists *)
  let make tlist =
    let rec loop acc base = function
    | [] -> Some (base, acc)
    | t :: ts ->
      match base_of_t t with
      | Some tbase when tbase = base ->
        loop (EnumSet.add t acc) base ts
      | _ -> None
    in
    tlist,
    match tlist with
    | [] | [_] -> None
    | t :: ts ->
      match base_of_t t with
      | Some (_ as base) ->
        loop (EnumSet.singleton t) base ts
      | _ -> None

  (* rep's enum base type, if any *)
  let enum_base (_, enum) =
    match enum with
    | None -> None
    | Some (base, _) -> Some base

  (* rep's list of members *)
  let members (tlist, _) = tlist

  (* map rep r to rep s along type mapping f *)
  let map f rep = make (List.map f (members rep))

  (* quick membership test: Some true/false or None = needs full check *)
  let quick_mem t rep =
    let t = canon t in
    match rep with
    | tlist, None ->
      if List.mem t tlist then Some true else None
    | _, Some (base, tset) ->
      match base_of_t t with
      | Some tbase when tbase = base ->
        Some (EnumSet.mem t tset)
      | _ -> Some false
end

(* The typechecking algorithm often needs to maintain sets of types, or more
   generally, maps of types (for logging we need to associate some provenanced
   information to types).
   Type terms may also contain internal sets or maps.
*)

and TypeSet : Set.S with type elt = TypeTerm.t = Set.Make(struct
  type elt = TypeTerm.t
  type t = elt
  let compare = Pervasives.compare
end)

and TypeMap : MapSig with type key = TypeTerm.t = MyMap(struct
  type key = TypeTerm.t
  type t = key
  let compare = Pervasives.compare
end)

and UseTypeMap : MapSig with type key = TypeTerm.use_t = MyMap(struct
  type key = TypeTerm.use_t
  type t = key
  let compare = Pervasives.compare
end)

include TypeTerm

(*********************************************************)

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

(* lift an operation on Type.t to an operation on Type.use_t *)
let lift_to_use f = function
  | UseT t -> f t
  | _ -> ()

(* def types vs. use types *)
let is_use = function
  | UseT _ -> false
  | _ -> true

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
  | FunProtoApplyT reason
  | FunProtoBindT reason
  | FunProtoCallT reason
      -> reason

  | PolyT (_,t) ->
      prefix_reason "polymorphic type: " (reason_of_t t)
  | ThisClassT t ->
      prefix_reason "class type: " (reason_of_t t)
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
  | TypeT (reason,_)
      -> reason

  | ReposUpperT (reason, _) -> reason

  | AnnotT (_, assume_t) ->
      reason_of_t assume_t

  | OptionalT t ->
      prefix_reason "optional " (reason_of_t t)

  | RestT t ->
      prefix_reason "rest array of " (reason_of_t t)

  | AbstractT t ->
      prefix_reason "abstract " (reason_of_t t)

  | EvalT (_, defer_use_t, _) ->
      reason_of_defer_use_t defer_use_t

  | TypeAppT(t,_)
      -> prefix_reason "type application of " (reason_of_t t)

  | ThisTypeAppT(t,_,_)
      -> prefix_reason "this instantiation of " (reason_of_t t)

  | MaybeT t ->
      prefix_reason "?" (reason_of_t t)

  | TaintT (r) ->
      r

  | IntersectionT (reason, _) ->
      reason

  | UnionT (reason, _) ->
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

  | SpeculativeMatchT (reason, _, _) -> reason

  | ModuleT (reason, _) -> reason

  | CustomFunT (reason, _) -> reason

  | ExtendsT (_,_,t) ->
      prefix_reason "extends " (reason_of_t t)

and reason_of_defer_use_t = function
  | DestructuringT (reason, _) ->
      reason

and reason_of_use_t = function
  | UseT t -> reason_of_t t

  | BindT (reason, _)
  | ApplyT (reason, _, _)
  | CallT (reason, _)

  | MethodT (reason,_,_)
  | SetPropT (reason,_,_)
  | GetPropT (reason,_,_)

  | SetElemT (reason,_,_)
  | GetElemT (reason,_,_)

  | ConstructorT (reason,_,_)

  | SuperT (reason,_)
  | MixinT (reason, _)

  | AdderT (reason,_,_)
  | ComparatorT (reason,_)
  | UnaryMinusT (reason, _)

  | AndT (reason, _, _)
  | OrT (reason, _, _)
  | NotT (reason, _)
  | ReifyTypeT (reason, _)
  | BecomeT (reason, _)
  | ReposLowerT (reason, _)
      -> reason

  | PredicateT (_, t) -> reason_of_t t

  | EqT (reason, _) ->
      reason

  | SpecializeT(reason,_,_,_)
      -> reason

  | ThisSpecializeT(reason,_,_)
      -> reason

  | VarianceCheckT(reason,_,_)
      -> reason

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

  | GetKeysT (reason, _) -> reason
  | HasOwnPropT (reason, _) -> reason
  | HasPropT (reason, _, _) -> reason

  | ElemT (reason, _, _) -> reason

  | ConcretizeT (t, _, _, _) -> reason_of_t t
  | ConcreteT (t) -> reason_of_use_t t

  | SummarizeT (reason, _) -> reason

  | CJSRequireT (reason, _) -> reason
  | ImportModuleNsT (reason, _) -> reason
  | ImportDefaultT (reason, _, _) -> reason
  | ImportNamedT (reason, _, _) -> reason
  | ImportTypeT (reason, _) -> reason
  | ImportTypeofT (reason, _) -> reason
  | CJSExtractNamedExportsT (reason, _, _) -> reason
  | SetNamedExportsT (reason, _, _) -> reason
  | SetStarExportsT (reason, _, _) -> reason
  | DebugPrintT reason -> reason

(* helper: we want the tvar id as well *)
(* NOTE: uncalled for now, because ids are nondetermistic
   due to parallelism, which messes up test diffs. Should
   add a config, but for now must uncomment impl to use *)
let reason_of_t_add_id = reason_of_t
(* function
| OpenT (r, id) -> prefix_reason (spf "%d: " id) r
| t -> reason_of_t t *)
let reason_of_use_t_add_id = reason_of_use_t


let desc_of_t t = desc_of_reason (reason_of_t t)

let loc_of_t t = loc_of_reason (reason_of_t t)

let rec loc_of_predicate = function
  | AndP (p1, _)
  | OrP (p1, _)
    -> loc_of_predicate p1

  | NotP p
    -> loc_of_predicate p

  | LeftP (_, t)
  | RightP (_, t)
    -> loc_of_t t

  | ExistsP
  | NullP
  | MaybeP

  | SingletonBoolP _
  | SingletonStrP _

  | BoolP
  | FunP
  | NumP
  | ObjP
  | StrP
  | VoidP

  | ArrP
  | PropExistsP _
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
  | FunProtoApplyT (reason) -> FunProtoApplyT (f reason)
  | FunProtoBindT (reason) -> FunProtoBindT (f reason)
  | FunProtoCallT (reason) -> FunProtoCallT (f reason)
  | PolyT (plist, t) -> PolyT (plist, mod_reason_of_t f t)
  | ThisClassT t -> ThisClassT (mod_reason_of_t f t)
  | BoundT { reason; name; bound; polarity } ->
    BoundT { reason = f reason; name; bound; polarity }
  | ExistsT reason -> ExistsT (f reason)
  | ObjT (reason, ot) -> ObjT (f reason, ot)
  | ArrT (reason, t, ts) -> ArrT (f reason, t, ts)

  | ClassT t -> ClassT (mod_reason_of_t f t)
  | InstanceT (reason, st, su, inst) -> InstanceT (f reason, st, su, inst)

  | ReposUpperT (reason, t) -> ReposUpperT (f reason, t)
  | TypeT (reason, t) -> TypeT (f reason, t)
  | AnnotT (assert_t, assume_t) ->
      AnnotT (mod_reason_of_t f assert_t, mod_reason_of_t f assume_t)

  | OptionalT t -> OptionalT (mod_reason_of_t f t)

  | RestT t -> RestT (mod_reason_of_t f t)

  | AbstractT t -> AbstractT (mod_reason_of_t f t)

  | EvalT (t, defer_use_t, id) -> EvalT (t, mod_reason_of_defer_use_t f defer_use_t, id)

  | TypeAppT (t, ts) -> TypeAppT (mod_reason_of_t f t, ts)

  | ThisTypeAppT (t, this, ts) -> ThisTypeAppT (mod_reason_of_t f t, this, ts)

  | MaybeT t -> MaybeT (mod_reason_of_t f t)

  | TaintT (r) -> TaintT (f r)

  | IntersectionT (reason, ts) -> IntersectionT (f reason, ts)

  | UnionT (reason, ts) -> UnionT (f reason, ts)

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

  | SpeculativeMatchT (reason, t1, t2) ->
      SpeculativeMatchT (f reason, t1, t2)

  | ModuleT (reason, exports) -> ModuleT (f reason, exports)

  | ExtendsT (ts, t, tc) -> ExtendsT (ts, t, mod_reason_of_t f tc)

  | CustomFunT (reason, kind) -> CustomFunT (f reason, kind)

and mod_reason_of_defer_use_t f = function
  | DestructuringT (reason, s) -> DestructuringT (f reason, s)

and mod_reason_of_use_t f = function
  | UseT t -> UseT (mod_reason_of_t f t)

  | SuperT (reason, inst) -> SuperT (f reason, inst)
  | MixinT (reason, inst) -> MixinT (f reason, inst)

  | ApplyT (reason, l, ft) -> ApplyT (f reason, l, ft)
  | BindT (reason, ft) -> BindT (f reason, ft)
  | CallT (reason, ft) -> CallT (f reason, ft)

  | MethodT (reason, name, ft) -> MethodT(f reason, name, ft)
  | ReposLowerT (reason, t) -> ReposLowerT (f reason, t)
  | SetPropT (reason, n, t) -> SetPropT (f reason, n, t)
  | GetPropT (reason, n, t) -> GetPropT (f reason, n, t)

  | SetElemT (reason, it, et) -> SetElemT (f reason, it, et)
  | GetElemT (reason, it, et) -> GetElemT (f reason, it, et)

  | ConstructorT (reason, ts, t) -> ConstructorT (f reason, ts, t)

  | AdderT (reason, rt, lt) -> AdderT (f reason, rt, lt)
  | ComparatorT (reason, t) -> ComparatorT (f reason, t)
  | UnaryMinusT (reason, t) -> UnaryMinusT (f reason, t)

  | BecomeT (reason, t) -> BecomeT (f reason, t)

  | PredicateT (pred, t) -> PredicateT (pred, mod_reason_of_t f t)

  | EqT (reason, t) -> EqT (f reason, t)

  | AndT (reason, t1, t2) -> AndT (f reason, t1, t2)
  | OrT (reason, t1, t2) -> OrT (f reason, t1, t2)
  | NotT (reason, t) -> NotT (f reason, t)

  | ReifyTypeT (reason, t_out) -> ReifyTypeT (f reason, t_out)

  | SpecializeT(reason, cache, ts, t) -> SpecializeT (f reason, cache, ts, t)

  | ThisSpecializeT(reason, this, t) -> ThisSpecializeT (f reason, this, t)

  | VarianceCheckT(reason, ts, polarity) -> VarianceCheckT (f reason, ts, polarity)

  | LookupT (reason, r2, ts, x, t) -> LookupT (f reason, r2, ts, x, t)

  | UnifyT (t, t2) -> UnifyT (mod_reason_of_t f t, mod_reason_of_t f t2)

  | ObjAssignT (reason, t, t2, filter, resolve) ->
      ObjAssignT (f reason, t, t2, filter, resolve)
  | ObjFreezeT (reason, t) -> ObjFreezeT (f reason, t)
  | ObjRestT (reason, t, t2) -> ObjRestT (f reason, t, t2)
  | ObjSealT (reason, t) -> ObjSealT (f reason, t)
  | ObjTestT (reason, t1, t2) -> ObjTestT (f reason, t1, t2)

  | ArrRestT (reason, i, t) -> ArrRestT (f reason, i, t)
  | GetKeysT (reason, t) -> GetKeysT (f reason, t)
  | HasOwnPropT (reason, t) -> HasOwnPropT (f reason, t)
  | HasPropT (reason, strict, t) -> HasPropT (f reason, strict, t)

  | ElemT (reason, t, t2) -> ElemT (f reason, t, t2)

  | ConcretizeT (t1, ts1, ts2, t2) ->
      ConcretizeT (mod_reason_of_t f t1, ts1, ts2, t2)
  | ConcreteT t -> ConcreteT (mod_reason_of_use_t f t)

  | SummarizeT (reason, t) -> SummarizeT (f reason, t)

  | CJSRequireT (reason, t) -> CJSRequireT (f reason, t)
  | ImportModuleNsT (reason, t) -> ImportModuleNsT (f reason, t)
  | ImportDefaultT (reason, name, t) -> ImportDefaultT (f reason, name, t)
  | ImportNamedT (reason, name, t) -> ImportNamedT (f reason, name, t)
  | ImportTypeT (reason, t) -> ImportTypeT (f reason, t)
  | ImportTypeofT (reason, t) -> ImportTypeofT (f reason, t)

  | CJSExtractNamedExportsT (reason, t1, t2) -> CJSExtractNamedExportsT (f reason, t1, t2)
  | SetNamedExportsT (reason, tmap, t_out) -> SetNamedExportsT(f reason, tmap, t_out)
  | SetStarExportsT (reason, target_module_t, t_out) -> SetStarExportsT(f reason, target_module_t, t_out)
  | DebugPrintT reason -> DebugPrintT (f reason)


(* type comparison mod reason *)
let reasonless_compare =
  let swap_reason t r = mod_reason_of_t (fun _ -> r) t in
  fun t t' ->
    if t == t' then 0 else
    compare t (swap_reason t' (reason_of_t t))

(* Printing some types in parseable form relies on particular formats in
   corresponding reason descriptions. The following module formalizes the
   relevant conventions.

   TODO: Encoding formats in strings instead of ADTs is not ideal, obviously. *)

module DescFormat = struct
  (* InstanceT reasons have desc = name *)
  let instance_reason name loc =
    mk_reason name loc
  let name_of_instance_reason r =
    desc_of_reason r

  (* TypeT reasons have desc = type `name` *)
  let type_reason name loc =
    mk_reason (spf "type `%s`" name) loc
  let name_of_type_reason r =
    Str.global_replace (Str.regexp "type `\\(.*\\)`") "\\1" (desc_of_reason r)

end

(* printing *)
let string_of_defer_use_ctor = function
  | DestructuringT _ -> "DestructuringT"

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
  | FunProtoApplyT _ -> "FunProtoApplyT"
  | FunProtoBindT _ -> "FunProtoBindT"
  | FunProtoCallT _ -> "FunProtoCallT"
  | PolyT _ -> "PolyT"
  | ThisClassT _ -> "ThisClassT"
  | BoundT _ -> "BoundT"
  | ExistsT _ -> "ExistsT"
  | ObjT _ -> "ObjT"
  | ArrT _ -> "ArrT"
  | ClassT _ -> "ClassT"
  | InstanceT _ -> "InstanceT"
  | ReposUpperT _ -> "ReposUpperT"
  | TypeT _ -> "TypeT"
  | AnnotT _ -> "AnnotT"
  | OptionalT _ -> "OptionalT"
  | RestT _ -> "RestT"
  | AbstractT _ -> "AbstractT"
  | EvalT (_, defer_use_t, _) -> string_of_defer_use_ctor defer_use_t
  | TypeAppT _ -> "TypeAppT"
  | ThisTypeAppT _ -> "ThisTypeAppT"
  | MaybeT _ -> "MaybeT"
  | TaintT _ -> "TaintT"
  | IntersectionT _ -> "IntersectionT"
  | UnionT _ -> "UnionT"
  | SpeculativeMatchT _ -> "SpeculativeMatchT"
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
  | ModuleT _ -> "ModuleT"
  | ExtendsT _ -> "ExtendsT"
  | CustomFunT _ -> "CustomFunT"

let string_of_use_ctor = function
  | UseT t -> string_of_ctor t

  | SummarizeT _ -> "SummarizeT"
  | SuperT _ -> "SuperT"
  | MixinT _ -> "MixinT"
  | ApplyT _ -> "ApplyT"
  | BindT _ -> "BindT"
  | CallT _ -> "CallT"
  | MethodT _ -> "MethodT"
  | SetPropT _ -> "SetPropT"
  | GetPropT _ -> "GetPropT"
  | SetElemT _ -> "SetElemT"
  | GetElemT _ -> "GetElemT"
  | ConstructorT _ -> "ConstructorT"
  | AdderT _ -> "AdderT"
  | ComparatorT _ -> "ComparatorT"
  | ReposLowerT _ -> "ReposLowerT"
  | BecomeT _ -> "BecomeT"
  | PredicateT _ -> "PredicateT"
  | EqT _ -> "EqT"
  | AndT _ -> "AndT"
  | OrT _ -> "OrT"
  | NotT _ -> "NotT"
  | ReifyTypeT _ -> "ReifyTypeT"
  | SpecializeT _ -> "SpecializeT"
  | ThisSpecializeT _ -> "ThisSpecializeT"
  | VarianceCheckT _ -> "VarianceCheckT"
  | LookupT _ -> "LookupT"
  | UnifyT _ -> "UnifyT"
  | ObjAssignT _ -> "ObjAssignT"
  | ObjFreezeT _ -> "ObjFreezeT"
  | ObjRestT _ -> "ObjRestT"
  | ObjSealT _ -> "ObjSealT"
  | ObjTestT _ -> "ObjTestT"
  | ArrRestT _ -> "ArrRestT"
  | UnaryMinusT _ -> "UnaryMinusT"
  | GetKeysT _ -> "GetKeysT"
  | HasOwnPropT _ -> "HasOwnPropT"
  | HasPropT _ -> "HasPropT"
  | ElemT _ -> "ElemT"
  | ConcretizeT _ -> "ConcretizeT"
  | ConcreteT _ -> "ConcreteT"
  | ImportModuleNsT _ -> "ImportModuleNsT"
  | ImportDefaultT _ -> "ImportDefaultT"
  | ImportNamedT _ -> "ImportNamedT"
  | ImportTypeT _ -> "ImportTypeT"
  | ImportTypeofT _ -> "ImportTypeofT"
  | CJSRequireT _ -> "CJSRequireT"
  | CJSExtractNamedExportsT _ -> "CJSExtractNamedExportsT"
  | SetNamedExportsT _ -> "SetNamedExportsT"
  | SetStarExportsT _ -> "SetStarExportsT"
  | DebugPrintT _ -> "DebugPrintT"

let string_of_binary_test = function
  | InstanceofTest -> "instanceof"
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
  | NullP -> "null"
  | MaybeP -> "null or undefined"

  | SingletonBoolP false -> "false"
  | SingletonBoolP true -> "true"
  | SingletonStrP str -> spf "string `%s`" str

  (* typeof *)
  | VoidP -> "undefined"
  | BoolP -> "boolean"
  | StrP -> "string"
  | NumP -> "number"
  | FunP -> "function"
  | ObjP -> "object"

  (* Array.isArray *)
  | ArrP -> "array"

  | PropExistsP key -> spf "prop `%s` is truthy" key

module Polarity = struct
  (* Subtype relation for polarities, interpreting neutral as positive &
     negative: whenever compat(p1,p2) holds, things that have polarity p1 can
     appear in positions that have polarity p2. *)
  let compat = function
    | Positive, Positive
    | Negative, Negative
    | Neutral, _ -> true
    | _ -> false

  let inv = function
    | Positive -> Negative
    | Negative -> Positive
    | Neutral -> Neutral

  let mult = function
    | Positive, Positive -> Positive
    | Negative, Negative -> Positive
    | Neutral, _ | _, Neutral -> Neutral
    | _ -> Negative

  (* printer *)
  let string = function
    | Positive -> "covariant"
    | Negative -> "contravariant"
    | Neutral -> "invariant"
end
