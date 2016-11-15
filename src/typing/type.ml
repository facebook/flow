(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason
open Utils_js

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
type index = int

module rec TypeTerm : sig

  type t =
    (* open type variable *)
    (* A type variable (tvar) is an OpenT(reason, id) where id is an int index
       into a context's graph: a context's graph is a map from tvar ids to nodes
       (see below).

       Note: ids are globally unique. tvars are "owned" by a single context,
       but that context and its tvars may later be merged into other contexts.
     *)
    | OpenT of reason * ident

    (*************)
    (* def types *)
    (*************)

    (* TODO: constant types *)

    | NumT of reason * number_literal literal
    | StrT of reason * string literal
    | BoolT of reason * bool option
    | EmptyT of reason
    | MixedT of reason * mixed_flavor
    | AnyT of reason
    | NullT of reason
    | VoidT of reason

    | FunT of reason * static * prototype * funtype
    | FunProtoT of reason       (* Function.prototype *)
    | FunProtoApplyT of reason  (* Function.prototype.apply *)
    | FunProtoBindT of reason   (* Function.prototype.bind *)
    | FunProtoCallT of reason   (* Function.prototype.call *)

    | ObjT of reason * objtype
    | ObjProtoT of reason       (* Object.prototype *)
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
    (* Usually a type expression is evaluated by splitting it into a def type
       and a use type, and flowing the former to the latter: the def type is the
       "main" argument, and the use type contains the type operation, other
       arguments, and a tvar to hold the result. However, sometimes a type
       expression may need to be kept in explicit form, with the type operation
       and other arguments split out into a "deferred" use type `defer_use_t`,
       whose evaluation state is tracked in the context by an identifier id:
       When defer_use_t is evaluated, id points to a tvar containing the result
       of evaluation. The explicit form simplifies other tasks, like
       substitution, but otherwise works in much the same way as usual. *)
    | EvalT of t * defer_use_t * int

    (* A polymorphic type is like a type-level "function" that, when applied to
       lists of type arguments, generates types. Just like a function, a
       polymorphic type has a list of type parameters, represented as bound
       type variables. We say that type parameters are "universally quantified"
       (or "universal"): every substitution of type arguments for type
       parameters generates a type. Dually, we have "existentially quantified"
       (or "existential") type variables: such a type variable denotes some,
       possibly unknown, type. Universal type parameters may specify subtype
       constraints ("bounds"), which must be satisfied by any types they may be
       substituted by. Evaluation of existential types, which involves
       generating fresh type variables, never happens under polymorphic types;
       it is forced only when polymorphic types are applied. *)

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

    (* exact *)
    | ExactT of reason * t

    (* ? types *)
    | MaybeT of t

    | TaintT of reason

    (* & types *)
    | IntersectionT of reason * InterRep.t

    (* | types *)
    | UnionT of reason * UnionRep.t

    (* generalizations of AnyT *)
    | AnyWithLowerBoundT of t (* any supertype of t *)
    | AnyWithUpperBoundT of t (* any subtype of t *)

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

    (** Here's to the crazy ones. The misfits. The rebels. The troublemakers.
        The round pegs in the square holes. **)

    (* util for deciding subclassing relations *)
    | ExtendsT of t list * t * t

    (* toolkit for making choices *)
    | ChoiceKitT of reason * choice_tool

    (* Sigil representing functions that the type system is not expressive
       enough to annotate, so we customize their behavior internally. *)
    | CustomFunT of reason * custom_fun_kind

    (* Internal-only type that wraps object types for the CustomFunT(Idx)
       function *)
    | IdxWrapper of reason * t

    (** Predicate types **)

    (* `OpenPredT (reason, base_t, m_pos, m_neg)` wraps around a base type
       `base_t` and encodes additional information that hold in conditional
       contexts (in the form of logical predicates). This information is split
       into positive and negative versions in `m_pos` and `m_neg`. The
       predicates here are "open" in the sense that they contain free variable
       instances, which are the keys to the two maps.
    *)
    | OpenPredT of reason * t * predicate Key_map.t * predicate Key_map.t

    (* Map a FunT over a structure *)
    | TypeMapT of reason * type_map * t * t


  and defer_use_t =
    (* type of a variable / parameter / property extracted from a pattern *)
    | DestructuringT of reason * selector
    (* destructors that extract parts of various kinds of types *)
    (* TODO: in principle it should be possible to encode destructors as
       selectors (see above), but currently we don't because some selectors are
       programmed to do more than just destruct types---e.g., they handle
       defaults---and these additional behaviors cannot be covered by a simple
       implementation of destructors. *)
    | TypeDestructorT of reason * destructor

  and internal_use_op =
    | CopyEnv
    | MergeEnv
    | Refinement
    | WidenEnv

  and use_op =
    | Addition
    | Coercion
    | FunCallParam
    | FunCallThis of reason
    | FunImplicitReturn
    | FunReturn
    | Internal of internal_use_op
    | MissingTupleElement of int
    | TypeRefinement
    | UnknownUse

  and use_t =
    (* def types can be used as upper bounds *)
    | UseT of use_op * t

    (*************)
    (* use types *)
    (*************)

    (* operation on literals *)
    | SummarizeT of reason * t

    (* operations on runtime values, such as functions, objects, and arrays *)
    | ApplyT of reason * t * funtype
    | BindT of reason * funtype
    | CallT of reason * funtype
    | MethodT of (* call *) reason * (* lookup *) reason * propref * funtype
    | SetPropT of reason * propref * t
    | GetPropT of reason * propref * t
    | TestPropT of reason * propref * t
    | SetElemT of reason * t * t
    | GetElemT of reason * t * t
    | CallElemT of (* call *) reason * (* lookup *) reason * t * funtype
    | GetStaticsT of reason * t_out

    (* repositioning *)
    | ReposLowerT of reason * use_t
    | ReposUseT of reason * use_op * t

    (* operations on runtime types, such as classes and functions *)
    | ConstructorT of reason * t list * t
    | SuperT of reason * insttype
    | MixinT of reason * t

    (* overloaded +, could be subsumed by general overloading *)
    | AdderT of reason * t * t
    (* overloaded relational operator, could be subsumed by general
       overloading *)
    | ComparatorT of reason * t
    (* unary minus operator on numbers, allows negative number literals *)
    | UnaryMinusT of reason * t

    | AssertArithmeticOperandT of reason
    | AssertBinaryInLHST of reason
    | AssertBinaryInRHST of reason
    | AssertForInRHST of reason

    (* operation specifying a type refinement via a predicate *)
    | PredicateT of predicate * t

    (* like PredicateT, GuardT guards a subsequent flow with a predicate on an
       incoming type. Unlike PredicateT, the subsequent flow (if any) uses
       an arbitrary LB specified in the GuardT value, rather than the filtered
       result of the predicate itself *)
    | GuardT of predicate * t * t

    (* == *)
    | EqT of reason * t

    (* logical operators *)
    | AndT of reason * t * t
    | OrT of reason * t * t
    | NotT of reason * t

    (* operation on polymorphic types *)
    (** SpecializeT(_, _, cache, targs, tresult) instantiates a polymorphic type
        with type arguments targs, and flows the result into tresult. If cache
        is set, it looks up a cache of existing instantiations for the type
        parameters of the polymorphic type, unifying the type arguments with
        those instantiations if such exist.

        The first reason is the reason why we're specializing. The second
        reason points to the type application itself
    **)
    | SpecializeT of reason * reason * bool * t list * t
    (* operation on this-abstracted classes *)
    | ThisSpecializeT of reason * t * t
    (* variance check on polymorphic types *)
    | VarianceCheckT of reason * t list * polarity

    | TypeAppVarianceCheckT of reason * reason * (t * t) list

    (* operation on prototypes *)
    (** LookupT(_, strict, try_ts_on_failure, x, lookup_action) looks for
        property x in an object type and emits a constraint according to the
        provided lookup_action.

        When x is not found, we have the following cases:

        (1) try_ts_on_failure is not empty, and we try to look for property x in
        the next object type in that list;

        (2) strict = None, so no error is reported;

        (3) strict = Some reason, so the position in reason is blamed.
    **)
    | LookupT of reason * lookup_kind * t list * propref * lookup_action

    (* operations on objects *)
    | ObjAssignT of reason * t * t * string list * bool
    | ObjFreezeT of reason * t
    | ObjRestT of reason * string list * t
    | ObjSealT of reason * t
    (* test that something is object-like, returning a default type otherwise *)
    | ObjTestT of reason * t * t

    (* assignment rest element in array pattern *)
    | ArrRestT of reason * int * t

    (* Guarded unification *)
    | UnifyT of t * t (* bidirectional *)

    (* unifies with incoming concrete lower bound *)
    | BecomeT of reason * t

    (* Keys *)
    | GetKeysT of reason * t
    | HasOwnPropT of reason * string literal
    | HasPropT of reason * reason option * string literal

    (* Element access *)
    | ElemT of reason * t * elem_action

    (* exact ops *)
    | MakeExactT of reason * cont

    (* Module import handling *)
    | CJSRequireT of reason * t
    | ImportModuleNsT of reason * t
    | ImportDefaultT of reason * import_kind * (string * string) * t
    | ImportNamedT of reason * import_kind * string * t
    | ImportTypeT of reason * string * t
    | ImportTypeofT of reason * string * t
    | AssertImportIsValueT of reason * string

    (* Module export handling *)
    | CJSExtractNamedExportsT of
        reason
        * (* local ModuleT *) (reason * exporttypes)
        * (* 't_out' to receive the resolved ModuleT *) t_out
    | CopyNamedExportsT of reason * t * t_out
    | ExportNamedT of reason * t SMap.t * t_out

    (* Map a FunT over a structure *)
    | MapTypeT of reason * type_map * t * cont

    (**
     * An internal-only type to evaluate JSX expression. This type exists to
     * wait for the first parameter of a React.createElement call in order to
     * properly dispatch. That is, given a ReactClass, a stateless functional
     * component, or a string, do the right thing.
     *
     * This type shouldn't exist, but is a temporary measure until the type of
     * React.createElement can be represented by an intersection without running
     * into intersection bugs. *)
    | ReactCreateElementT of reason * t * t_out

    (* toolkit for making choices, contd. (appearing only as upper bounds) *)
    | ChoiceKitUseT of reason * choice_use_tool

    (* tools for preprocessing intersections *)
    | IntersectionPreprocessKitT of reason * intersection_preprocess_tool

    | DebugPrintT of reason

    | SentinelPropTestT of t * bool * sentinel_value * t_out

    | IdxUnwrap of reason * t_out
    | IdxUnMaybeifyT of reason * t_out

    (* Function predicate uses *)

    (**
     * The following two uses are used when a predicate function is called to
     * establish a predicate over one of its arguments.
     *)

    (**
     * The intended use for CallLatentPredT is to flow a predicated function
     * type to it. This function will refine the unrefined argument of
     * CallLatentPredT and use the second of the two type arguments as the
     * out-type. Also, since we might not yet know the function's formal
     * parameters (until the incoming flow gets concretized), we can only
     * keep the index of the argument that gets refined as part of the use.
     *
     * Flowing a non-predicate function type has no refining effect.
     *
     * This can be thought of as the equivalent of flows to `CallT` but for
     * predicated calls.
     *
     * The boolean part is the sense of the conditional check.
     *)
    | CallLatentPredT of reason * bool * index * t * t

    (**
     * CallOpenPredT is fired subsequently, after processing the flow
     * described above. This flow is necessary since the return type of the
     * predicate function (which determines the predicate it expresses)
     * might not be readily available. So, a flow to CallOpenPredT awaits
     * for the return_t of the function in question to be concretized,
     * while still holding the unrefined and refined versions of the variable
     * under refinement. In addition, since the structure (and hence the
     * function paramenters) of the function are now known (from the above
     * flow) we only keep the relevant key, which corresponds to the refining
     * parameter.
     *)
    | CallOpenPredT of reason * bool * Key.t * t * t

    (**
     * Even for the limited use of function predicates that is currently
     * allowed, we still have to build machinery to handle subtyping for
     * predicated function types.
     *
     * Let the following be the general form of function subtyping:
     *
     *   (xs:Ts): R / P(xs) <: (xs': Ts'): R' / P'(xs')
     *   \________________/    \______________________/
     *           T                        T'
     *
     * where `P(xs)` and `P'(xs')` are the open predicates established by
     * the functions in each side of the subtyping constraint. These
     * predicates are "open", in the sense that they have free occurrences of
     * each function's formal parameters (xs and xs', respectively).
     *
     * The constraint T <: T', causes the following (expected) sub-constraints:
     *
     *  - Ts' <: Ts
     *  - R <: R'
     *
     * and (additionally) to account for the predicates expressed by the two
     * functions a logical implication constraint:
     *
     *   P(xs) => [xs/xs'] P'(xs')
     *
     * Note in the above, that to be able to compare them, we need to first
     * substitute occurrence of xs' in P' for xs, which is denoted with
     * [xs/xs'].
     *
     * Valid flows to `SubstOnPredT (_, theta, t)` are from `OpenPredT`. This
     * is treated as an intermediate flow that adjusts the predicates of the
     * OpenPredT by applying a substitution `theta` to the predicates therein.
     *
     * NOTE: this substitution is not use at the moment since we don't yet
     * support subtyping of predicated functions, but the scaffolding might be
     * useful later on.
     *)
    | SubstOnPredT of reason * substitution * t

    (**
     * `RefineT (reason, pred, tvar)` is an instruction to refine an incoming
     * flow using the predicate `pred`. The result will be stored in `tvar`,
     * which is expected to be a type variable.
     *)
    | RefineT of reason * predicate * t

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
    | SingletonNumP of number_literal

    | BoolP (* boolean *)
    | FunP (* function *)
    | NumP (* number *)
    | ObjP (* object *)
    | StrP (* string *)
    | VoidP (* undefined *)

    | ArrP (* Array.isArray *)

    (* `if (a.b)` yields `flow (a, PredicateT(PropExistsP "b", tout))` *)
    | PropExistsP of reason * string

    (* Encondes the latent predicate associated with the i-th parameter
       of a function, whose type is the second element of the triplet. *)
    | LatentP of t * index

  and substitution = Key.t SMap.t

  and binary_test =
    (* e1 instanceof e2 *)
    | InstanceofTest
    (* e1.key === e2 *)
    | SentinelProp of string

  and 'a literal =
    | Literal of 'a
    | Truthy
    | AnyLiteral

  and number_literal = (float * string)

  and mixed_flavor =
    | Mixed_everything
    | Mixed_truthy
    | Mixed_non_maybe
    | Mixed_non_null
    | Mixed_non_void
    | Empty_intersection

  (* used by FunT and CallT *)
  and funtype = {
    this_t: t;
    params_tlist: t list;
    params_names: string list option;
    return_t: t;
    closure_t: int;
    is_predicate: bool;
    changeset: Changeset.t
  }

  and objtype = {
    flags: flags;
    dict_t: dicttype option;
    props_tmap: Properties.id;
    proto_t: prototype;
  }

  and cont = Lower of t | Upper of use_t

  (* LookupT is a general-purpose tool for traversing prototype chains in search
     of properties. In all cases, if the property is found somewhere along the
     prototype chain, the property type will unify with the output tvar. Lookup
     kinds control what happens when a property is not found.

   * Strict
     If the property is not found, emit an error. The reason should point to
     the original lookup location.

   * NonstrictReturning None
     If the property is not found, do nothing. Note that lookups of this kind
     will not add any constraints to the output tvar.

   * NonstrictReturning (Some (default, tout))
     If the property is not found, unify a default type with the *original*
     tvar from the lookup.

   * ShadowRead (strict, property_map_ids)
     If the property is not found, installs shadow properties into unsealed
     objects found along the prototype chain.

     Shadow reads can be strict or non-strict, and behaves identically to
     `Strict` and `NonstrictReturning None` respectively.

     Note: Shadow reads are only ever strict in ObjT -> ObjT flows, which
     ensures statements like `var o: { p: T } = {}` are an error. This
     behavior is also race-y, and possibly undesirable.

   * ShadowWrite property_map_ids
     If the property is not found, install a property on a single unsealed
     object type which originated the lookup. Also install shadow properties
     along the prototype chain, to ensure that the entire proto chain is subtype
     compatible. *)
  and lookup_kind =
  | Strict of reason
  | NonstrictReturning of (t * t) option
  | ShadowRead of reason option * Properties.id Nel.t
  | ShadowWrite of Properties.id Nel.t

  and lookup_action =
  | RWProp of t_out * rw
  | LookupProp of Property.t
  | SuperProp of Property.t

  and rw = Read | Write

  and elem_action =
    | ReadElem of t
    | WriteElem of t
    | CallElem of reason (* call *) * funtype

  and propref =
    | Named of reason * name
    | Computed of t

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
    dict_polarity: polarity;
  }

  and polarity = Negative | Neutral | Positive

  and property =
    | Field of t * polarity
    | Get of t
    | Set of t
    | GetSet of t * t

  and insttype = {
    class_id: ident;
    type_args: t SMap.t;
    arg_polarities: polarity SMap.t;
    fields_tmap: Properties.id;
    initialized_field_names: SSet.t;
    methods_tmap: Properties.id;
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
    exports_tmap: Exports.id;

    (**
     * This stores the CommonJS export type when applicable and is used as the
     * exact return type for calls to require(). This slot doesn't apply to pure
     * ES modules.
     *)
    cjs_export: t option;

    (**
     * Sometimes we claim the module exports any or Object, implying that it
     * has every named export
     *)
    has_every_named_export: bool;
  }

  and import_kind =
    | ImportType
    | ImportTypeof
    | ImportValue

  and typeparam = {
    reason: reason;
    name: string;
    bound: t;
    polarity: polarity;
    default: t option;
  }

  and selector =
  | Prop of string
  | Elem of t
  | ObjRest of string list
  | ArrRest of int
  | Default
  | Become
  | Refine of predicate

  and destructor =
  | NonMaybeType
  | PropertyType of string

  and type_map =
  | TupleMap
  | ObjectMap
  | ObjectMapi

  and prototype = t

  and super = t

  and static = t

  and t_out = t

  and custom_fun_kind =
  (* builtins *)
  | ObjectAssign
  | ObjectGetPrototypeOf

  (* 3rd party libs *)
  | ReactCreateElement

  (* Facebookisms *)
  | Merge
  | MergeDeepInto
  | MergeInto
  | Mixin
  | Idx

  and sentinel_value =
  | SentinelStr of string
  | SentinelNum of number_literal
  | SentinelBool of bool

  and choice_tool =
  | Trigger

  and choice_use_tool =
  | FullyResolveType of ident
  | TryFlow of int * spec

  and intersection_preprocess_tool =
  | ConcretizeTypes of t list * t list * t * use_t
  | SentinelPropTest of bool * string * t * t * t
  | PropExistsTest of bool * string * t * t

  and spec =
  | UnionCases of t * t list
  | IntersectionCases of t list * use_t

  (* A dependent predicate type consisting of:
     - the result type of the test (not always bool)
     - a map (lookup key -> type) of refinements which hold if the
       test is true
     - a map of refinements which hold if the test is false
  *)
  and dep_preds =
    t * predicate Key_map.t * predicate Key_map.t
end = TypeTerm

and Polarity : sig
  type t = TypeTerm.polarity

  val compat: t * t -> bool
  val inv: t -> t
  val mult: t * t -> t
  val of_rw: TypeTerm.rw -> t

  val string: t -> string
  val sigil: t -> string
end = struct
  open TypeTerm

  type t = polarity

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

  let of_rw = function
    | Read -> Positive
    | Write -> Negative

  (* printer *)
  let string = function
    | Positive -> "covariant"
    | Negative -> "contravariant"
    | Neutral -> "invariant"

  let sigil = function
    | Positive -> "+"
    | Negative -> "-"
    | Neutral -> ""
end

and Property : sig
  type t = TypeTerm.property

  val field: Polarity.t -> TypeTerm.t -> t

  val polarity: t -> Polarity.t

  val read_t: t -> TypeTerm.t option
  val write_t: t -> TypeTerm.t option
  val access: TypeTerm.rw -> t -> TypeTerm.t option

  val iter_t: (TypeTerm.t -> unit) -> t -> unit
  val fold_t: ('a -> TypeTerm.t -> 'a) -> 'a -> t -> 'a
  val map_t: (TypeTerm.t -> TypeTerm.t) -> t -> t
  val ident_map_t: (TypeTerm.t -> TypeTerm.t) -> t -> t
  val forall_t: (TypeTerm.t -> bool) -> t -> bool

  val assert_field: t -> TypeTerm.t
end = struct
  open TypeTerm

  type t = property

  let field polarity t = Field (t, polarity)

  let polarity = function
    | Field (_, polarity) -> polarity
    | Get _ -> Positive
    | Set _ -> Negative
    | GetSet _ -> Neutral

  let read_t = function
    | Field (t, polarity) ->
      if Polarity.compat (polarity, Positive)
      then Some t
      else None
    | Get t -> Some t
    | Set _ -> None
    | GetSet (t, _) -> Some t

  let write_t = function
    | Field (t, polarity) ->
      if Polarity.compat (polarity, Negative)
      then Some t
      else None
    | Get _ -> None
    | Set t -> Some t
    | GetSet (_, t) -> Some t

  let access = function
    | Read -> read_t
    | Write -> write_t

  let iter_t f = function
    | Field (t, _)
    | Get t
    | Set t ->
      f t
    | GetSet (t1, t2) ->
      f t1;
      f t2

  let fold_t f acc = function
    | Field (t, _)
    | Get t
    | Set t ->
      f acc t
    | GetSet (t1, t2) ->
      f (f acc t1) t2

  let map_t f = function
    | Field (t, polarity) -> Field (f t, polarity)
    | Get t -> Get (f t)
    | Set t -> Set (f t)
    | GetSet (t1, t2) -> GetSet (f t1, f t2)

  let ident_map_t f p =
    match p with
    | Field (t, polarity) ->
      let t_ = f t in
      if t_ == t then p else Field (t_, polarity)
    | Get t ->
      let t_ = f t in
      if t_ == t then p else Get t_
    | Set t ->
      let t_ = f t in
      if t_ == t then p else Set t_
    | GetSet (t1, t2) ->
      let t1_ = f t1 in
      let t2_ = f t2 in
      if t1_ == t1 && t2_ == t2 then p else GetSet (t1_, t2_)

  let forall_t f = fold_t (fun acc t -> acc && f t) true

  let assert_field = function
    | Field (t, _) -> t
    | _ -> assert_false "Unexpected field type"
end

and Properties : sig
  type t = Property.t SMap.t

  type id
  module Map : MyMap.S with type key = id
  type map = t Map.t

  val add_field: string -> Polarity.t -> TypeTerm.t -> t -> t
  val add_getter: string -> TypeTerm.t -> t -> t
  val add_setter: string -> TypeTerm.t -> t -> t

  val mk_id: unit -> id
  val fake_id: id
  val string_of_id: id -> string
  val extract_named_exports: t -> Exports.t

  val map_t: (TypeTerm.t -> TypeTerm.t) -> t -> t
  val map_fields: (TypeTerm.t -> TypeTerm.t) -> t -> t
  val mapi_fields: (string -> TypeTerm.t -> TypeTerm.t) -> t -> t
end = struct
  open TypeTerm

  type t = Property.t SMap.t

  type id = int
  module Map : MyMap.S with type key = id = MyMap.Make(struct
    type key = id
    type t = key
    let compare = Pervasives.compare
  end)
  type map = t Map.t

  let add_field x polarity t =
    SMap.add x (Field (t, polarity))

  let add_getter x get_t map =
    let p = match SMap.get x map with
    | Some (Set set_t) -> GetSet (get_t, set_t)
    | _ -> Get get_t
    in
    SMap.add x p map

  let add_setter x set_t map =
    let p = match SMap.get x map with
    | Some (Get get_t) -> GetSet (get_t, set_t)
    | _ -> Set set_t
    in
    SMap.add x p map

  let mk_id = Reason.mk_id
  let fake_id = 0
  let string_of_id = string_of_int

  let extract_named_exports pmap =
    SMap.fold (fun x p tmap ->
      match Property.read_t p with
      | Some t -> SMap.add x t tmap
      | None -> tmap
    ) pmap SMap.empty

  let map_t f = SMap.map (Property.map_t f)

  let map_fields f = SMap.map (function
    | Field (t, polarity) -> Field (f t, polarity)
    | p -> p
  )

  let mapi_fields f = SMap.mapi (fun k -> function
    | Field (t, polarity) -> Field (f k t, polarity)
    | p -> p
  )
end

and Exports : sig
  type t = TypeTerm.t SMap.t

  type id
  module Map : MyMap.S with type key = id
  type map = t Map.t

  val mk_id: unit -> id
  val string_of_id: id -> string
end = struct
  type t = TypeTerm.t SMap.t

  type id = int
  module Map : MyMap.S with type key = id = MyMap.Make(struct
    type key = id
    type t = key
    let compare = Pervasives.compare
  end)
  type map = t Map.t

  let mk_id = Reason.mk_id
  let string_of_id = string_of_int
end

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

  (** build a rep from list of members *)
  val make: TypeTerm.t -> TypeTerm.t -> TypeTerm.t list -> t

  (** enum base type, if any *)
  val enum_base: t -> TypeTerm.t option

  (** members in declaration order *)
  val members: t -> TypeTerm.t list

  val cons: TypeTerm.t -> t -> t

  val rev_append: t -> t -> t

  (** map rep r to rep r' along type mapping f *)
  val map: (TypeTerm.t -> TypeTerm.t) -> t -> t

  (** map rep r to rep r' along type mapping f. drops history. if nothing would
      be changed, returns the physically-identical rep. *)
  val ident_map: (TypeTerm.t -> TypeTerm.t) -> t -> t

  (** quick membership test: Some true/false or None = needs full check *)
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
    let str = Some (StrT (locationless_reason RStringEnum, AnyLiteral)) in
    let num = Some (NumT (locationless_reason RNumberEnum, AnyLiteral)) in
    fun t ->
      match t with
      | SingletonStrT _ -> str
      | SingletonNumT _ -> num
      | _ -> None
  )

  (** union rep is:
      - list of members in declaration order, with at least 2 elements
      - if union is an enum (set of singletons over a common base)
        then Some (base, set)
        (additional specializations probably to come)
   *)
  type t =
    TypeTerm.t * TypeTerm.t * TypeTerm.t list *
    (TypeTerm.t * EnumSet.t) option

  (* helper: add t to enum set if base matches *)
  let acc_enum (base, tset) t =
    match base_of_t t with
    | Some tbase when tbase = base ->
      Some (base, EnumSet.add t tset)
    | _ -> None

  (** given a list of members, build a rep.
      specialized reps are used on compatible type lists *)
  let make t0 t1 ts =
    let enum = Option.bind (base_of_t t0) (fun base ->
      ListUtils.fold_left_opt acc_enum (base, EnumSet.singleton t0) (t1::ts)
    ) in
    t0, t1, ts, enum

  let enum_base (_, _, _, enum) =
    match enum with
    | None -> None
    | Some (base, _) -> Some base

  let members (t0, t1, ts, _) = t0::t1::ts

  let cons t0 (t1, t2, ts, _) =
    make t0 t1 (t2::ts)

  let rev_append rep1 rep2 =
    match List.rev_append (members rep1) (members rep2) with
    | t0::t1::ts -> make t0 t1 ts
    | _ -> failwith "impossible"

  let map f (t0, t1, ts, _)  = make (f t0) (f t1) (List.map f ts)

  let ident_map f ((t0, t1, ts, _) as rep) =
    let t0_ = f t0 in
    let t1_ = f t1 in
    let changed = t0_ != t0 || t1_ != t1 in
    let rev_ts, changed = List.fold_left (fun (rev_ts, changed) t ->
      let t_ = f t in
      t_::rev_ts, changed || t_ != t
    ) ([], changed) ts in
    if changed then make t0_ t1_ (List.rev rev_ts) else rep

  let quick_mem t (t0, t1, ts, enum) =
    let t = canon t in
    match enum with
    | None ->
      if List.mem t (t0::t1::ts) then Some true else None
    | Some (base, tset) ->
      match base_of_t t with
      | Some tbase when tbase = base ->
        Some (EnumSet.mem t tset)
      | _ -> Some false
end

(* We encapsulate IntersectionT's internal structure.

   Representations are opaque. `make` chooses a
   representation internally, and client code which
   needs to interact with member types directly
   can do so via `members`, which provides access
   via the standard list representation.
 *)

and InterRep : sig
  type t

  (** build rep from list of members *)
  val make: TypeTerm.t -> TypeTerm.t -> TypeTerm.t list -> t

  (** member list in declaration order *)
  val members: t -> TypeTerm.t list

  (** map rep r to rep r' along type mapping f. drops history *)
  val map: (TypeTerm.t -> TypeTerm.t) -> t -> t

  val append: TypeTerm.t list -> t -> t

  (** map rep r to rep r' along type mapping f. drops history. if nothing would
      be changed, returns the physically-identical rep. *)
  val ident_map: (TypeTerm.t -> TypeTerm.t) -> t -> t

end = struct
  (** intersection rep is:
      - member list in declaration order
    *)
  type t =
    TypeTerm.t * TypeTerm.t * TypeTerm.t list

  let make t0 t1 ts = (t0, t1, ts)

  let members (t0, t1, ts) = t0::t1::ts

  let map f (t0, t1, ts) = make (f t0) (f t1) (List.map f ts)

  let append ts2 (t0, t1, ts1) = make t0 t1 (ts1 @ ts2)

  let ident_map f ((t0, t1, ts) as rep) =
    let t0_ = f t0 in
    let t1_ = f t1 in
    let changed = t0_ != t0 || t1_ != t1 in
    let rev_ts, changed = List.fold_left (fun (rev_ts, changed) member ->
      let member_ = f member in
      member_::rev_ts, changed || member_ != member
    ) ([], changed) ts in
    if changed then make t0_ t1_ (List.rev rev_ts) else rep

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

and TypeMap : MyMap.S with type key = TypeTerm.t = MyMap.Make (struct
  type key = TypeTerm.t
  type t = key
  let compare = Pervasives.compare
end)

and UseTypeMap : MyMap.S with type key = TypeTerm.use_t = MyMap.Make (struct
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
  val desc: reason_desc
  val make: reason -> t
end

module type PrimitiveT = sig
  val desc: reason_desc
  val t: t
  val at: Loc.t -> t
  val why: reason -> t
end

module Primitive (P: PrimitiveType) = struct
  let desc = P.desc
  let t = P.make (locationless_reason desc)
  let at tok = P.make (mk_reason desc tok)
  let why reason = P.make (replace_reason_const desc reason)
  let make = P.make
end

module NumT = Primitive (struct
  let desc = RNumber
  let make r = NumT (r, AnyLiteral)
end)

module StrT = Primitive (struct
  let desc = RString
  let make r = StrT (r, AnyLiteral)
end)

module BoolT = Primitive (struct
  let desc = RBoolean
  let make r = BoolT (r, None)
end)

module MixedT = Primitive (struct
  let desc = RMixed
  let make r = MixedT (r, Mixed_everything)
end)

module EmptyT = Primitive (struct
  let desc = REmpty
  let make r = EmptyT r
end)

module AnyT = Primitive (struct
  let desc = RAny
  let make r = AnyT r
end)

module VoidT = Primitive (struct
  let desc = RVoid
  let make r = VoidT r
end)

module NullT = Primitive (struct
  let desc = RNull
  let make r = NullT r
end)

module ObjProtoT = Primitive (struct
  let desc = RDummyPrototype
  let make r = ObjProtoT r
end)

(* lift an operation on Type.t to an operation on Type.use_t *)
let lift_to_use f = function
  | UseT (_, t) -> f t
  | _ -> ()

(* def types vs. use types *)
let is_use = function
  | UseT _ -> false
  | _ -> true

(* not all so-called def types can appear as use types *)
(* TODO: separate these misfits out *)
let is_proper_def = function
  | ChoiceKitT _ -> false
  | _ -> true

(* convenience *)
let is_bot = function
| EmptyT _ -> true
| _ -> false

let is_top = function
| MixedT _ -> true
| _ -> false

let is_any = function
| AnyT _ -> true
| _ -> false

(* Use types trapped for any propagation *)
let any_propagating_use_t = function
  | CallT _
  | GetPropT _
  | MethodT _
  | PredicateT _
  | GuardT _
  | AndT _
  | OrT _
  | ReposLowerT _
  | IntersectionPreprocessKitT _
  | CallOpenPredT _
  | CallLatentPredT _
  | CJSRequireT _
  | ImportModuleNsT _
  | ImportDefaultT _
  | ImportNamedT _
  | CJSExtractNamedExportsT _
  | CopyNamedExportsT _
  (* TODO: ...others *)
    -> true
  | _ -> false

(* Usually types carry enough information about the "reason" for their
   existence (e.g., position in code, introduction/elimination rules in
   the type system), so printing the reason provides a good idea of what the
   type means to the programmer. *)

let rec reason_of_t = function
  | OpenT (reason,_) -> reason
  | AbstractT t -> replace_reason (fun desc -> RAbstract desc) (reason_of_t t)
  | AnnotT (_, assume_t) -> reason_of_t assume_t
  | AnyFunT reason -> reason
  | AnyObjT reason -> reason
  | AnyT reason -> reason
  | AnyWithLowerBoundT (t) -> reason_of_t t
  | AnyWithUpperBoundT (t) -> reason_of_t t
  | ArrT (reason,_,_) -> reason
  | BoolT (reason, _) -> reason
  | BoundT typeparam -> typeparam.reason
  | ChoiceKitT (reason, _) -> reason
  | ClassT t -> replace_reason (fun desc -> RClassType desc) (reason_of_t t)
  | CustomFunT (reason, _) -> reason
  | DiffT (t, _) -> reason_of_t t
  | EmptyT reason -> reason
  | EvalT (_, defer_use_t, _) -> reason_of_defer_use_t defer_use_t
  | ExactT (reason, t) ->
      let desc = desc_of_reason (reason_of_t t) in
      replace_reason_const (RExactType desc) reason
  | ExistsT reason -> reason
  | ExtendsT (_,_,t) ->
      replace_reason (fun desc -> RExtends desc) (reason_of_t t)
  | FunProtoApplyT reason -> reason
  | FunProtoBindT reason -> reason
  | FunProtoCallT reason -> reason
  | FunProtoT reason -> reason
  | FunT (reason,_,_,_) -> reason
  | IdxWrapper (reason, _) -> reason
  | InstanceT (reason,_,_,_) -> reason
  | IntersectionT (reason, _) -> reason
  | KeysT (reason, _) -> reason
  | MaybeT t -> replace_reason (fun desc -> RMaybe desc) (reason_of_t t)
  | MixedT (reason, _) -> reason
  | ModuleT (reason, _) -> reason
  | NullT reason -> reason
  | NumT (reason, _) -> reason
  | ObjProtoT reason -> reason
  | ObjT (reason,_) -> reason
  | OpenPredT (reason, _, _, _) -> reason
  | OptionalT t -> replace_reason (fun desc -> ROptional desc) (reason_of_t t)
  | PolyT (_,t) -> replace_reason (fun desc -> RPolyType desc) (reason_of_t t)
  | RestT t -> replace_reason (fun desc -> RRestArray desc) (reason_of_t t)
  | ShapeT (t) -> reason_of_t t
  | SingletonBoolT (reason, _) -> reason
  | SingletonNumT (reason, _) -> reason
  | SingletonStrT (reason, _) -> reason
  | StrT (reason, _) -> reason
  | TaintT (r) -> r
  | ThisClassT t -> replace_reason (fun desc -> RClassType desc) (reason_of_t t)
  | ThisTypeAppT(t,_,_) ->
      replace_reason (fun desc -> RThisTypeApp desc) (reason_of_t t)
  | TypeAppT(t,_) -> replace_reason (fun desc -> RTypeApp desc) (reason_of_t t)
  | TypeMapT (reason, _, _, _) -> reason
  | TypeT (reason,_) -> reason
  | UnionT (reason, _) -> reason
  | VoidT reason -> reason

and reason_of_defer_use_t = function
  | DestructuringT (reason, _)
  | TypeDestructorT (reason, _) ->
      reason

and reason_of_use_t = function
  | UseT (_, t) -> reason_of_t t
  | AdderT (reason,_,_) -> reason
  | AndT (reason, _, _) -> reason
  | ApplyT (reason, _, _) -> reason
  | ArrRestT (reason, _, _) -> reason
  | AssertArithmeticOperandT reason -> reason
  | AssertBinaryInLHST reason -> reason
  | AssertBinaryInRHST reason -> reason
  | AssertForInRHST reason -> reason
  | AssertImportIsValueT (reason, _) -> reason
  | BecomeT (reason, _) -> reason
  | BindT (reason, _) -> reason
  | CallElemT (reason, _, _, _) -> reason
  | CallLatentPredT (reason, _, _, _, _) -> reason
  | CallOpenPredT (reason, _, _, _, _) -> reason
  | CallT (reason, _) -> reason
  | ChoiceKitUseT (reason, _) -> reason
  | CJSExtractNamedExportsT (reason, _, _) -> reason
  | CJSRequireT (reason, _) -> reason
  | ComparatorT (reason,_) -> reason
  | ConstructorT (reason,_,_) -> reason
  | CopyNamedExportsT (reason, _, _) -> reason
  | DebugPrintT reason -> reason
  | ElemT (reason, _, _) -> reason
  | EqT (reason, _) -> reason
  | ExportNamedT (reason, _, _) -> reason
  | GetElemT (reason,_,_) -> reason
  | GetKeysT (reason, _) -> reason
  | GetPropT (reason,_,_) -> reason
  | GetStaticsT (reason,_) -> reason
  | GuardT (_, _, t) -> reason_of_t t
  | HasOwnPropT (reason, _) -> reason
  | HasPropT (reason, _, _) -> reason
  | IdxUnMaybeifyT (reason, _) -> reason
  | IdxUnwrap (reason, _) -> reason
  | ImportDefaultT (reason, _, _, _) -> reason
  | ImportModuleNsT (reason, _) -> reason
  | ImportNamedT (reason, _, _, _) -> reason
  | ImportTypeofT (reason, _, _) -> reason
  | ImportTypeT (reason, _, _) -> reason
  | IntersectionPreprocessKitT (reason, _) -> reason
  | LookupT(reason, _, _, _, _) -> reason
  | MakeExactT (reason, _) -> reason
  | MapTypeT (reason, _, _, _) -> reason
  | MethodT (reason,_,_,_) -> reason
  | MixinT (reason, _) -> reason
  | NotT (reason, _) -> reason
  | ObjAssignT (reason, _, _, _, _) -> reason
  | ObjFreezeT (reason, _) -> reason
  | ObjRestT (reason, _, _) -> reason
  | ObjSealT (reason, _) -> reason
  | ObjTestT (reason, _, _) -> reason
  | OrT (reason, _, _) -> reason
  | PredicateT (_, t) -> reason_of_t t
  | ReactCreateElementT (reason, _, _) -> reason
  | RefineT (reason, _, _) -> reason
  | ReposLowerT (reason, _) -> reason
  | ReposUseT (reason, _, _) -> reason
  | SentinelPropTestT (_, _, _, result) -> reason_of_t result
  | SetElemT (reason,_,_) -> reason
  | SetPropT (reason,_,_) -> reason
  | SpecializeT(reason,_,_,_,_) -> reason
  | SubstOnPredT (reason, _, _) -> reason
  | SummarizeT (reason, _) -> reason
  | SuperT (reason,_) -> reason
  | TestPropT (reason, _, _) -> reason
  | ThisSpecializeT(reason,_,_) -> reason
  | UnaryMinusT (reason, _) -> reason
  | UnifyT (_,t) -> reason_of_t t
  | VarianceCheckT(reason,_,_) -> reason
  | TypeAppVarianceCheckT (reason, _, _) -> reason

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

(* TODO make a type visitor *)
let rec mod_reason_of_t f = function
  | OpenT (reason, t) -> OpenT (f reason, t)
  | AbstractT t -> AbstractT (mod_reason_of_t f t)
  | AnnotT (assert_t, assume_t) ->
      AnnotT (mod_reason_of_t f assert_t, mod_reason_of_t f assume_t)
  | AnyFunT reason -> AnyFunT (f reason)
  | AnyObjT reason -> AnyObjT (f reason)
  | AnyT reason -> AnyT (f reason)
  | AnyWithLowerBoundT t -> AnyWithLowerBoundT (mod_reason_of_t f t)
  | AnyWithUpperBoundT t -> AnyWithUpperBoundT (mod_reason_of_t f t)
  | ArrT (reason, t, ts) -> ArrT (f reason, t, ts)
  | BoolT (reason, t) -> BoolT (f reason, t)
  | BoundT { reason; name; bound; polarity; default; } ->
      BoundT { reason = f reason; name; bound; polarity; default; }
  | ChoiceKitT (reason, tool) -> ChoiceKitT (f reason, tool)
  | ClassT t -> ClassT (mod_reason_of_t f t)
  | CustomFunT (reason, kind) -> CustomFunT (f reason, kind)
  | DiffT (t1, t2) -> DiffT (mod_reason_of_t f t1, t2)
  | EmptyT reason -> EmptyT (f reason)
  | EvalT (t, defer_use_t, id) ->
      EvalT (t, mod_reason_of_defer_use_t f defer_use_t, id)
  | ExactT (reason, t) -> ExactT (f reason, t)
  | ExistsT reason -> ExistsT (f reason)
  | ExtendsT (ts, t, tc) -> ExtendsT (ts, t, mod_reason_of_t f tc)
  | FunProtoApplyT (reason) -> FunProtoApplyT (f reason)
  | FunProtoBindT (reason) -> FunProtoBindT (f reason)
  | FunProtoCallT (reason) -> FunProtoCallT (f reason)
  | FunProtoT (reason) -> FunProtoT (f reason)
  | FunT (reason, s, p, ft) -> FunT (f reason, s, p, ft)
  | IdxWrapper (reason, t) -> IdxWrapper (f reason, t)
  | InstanceT (reason, st, su, inst) -> InstanceT (f reason, st, su, inst)
  | IntersectionT (reason, ts) -> IntersectionT (f reason, ts)
  | KeysT (reason, t) -> KeysT (f reason, t)
  | MaybeT t -> MaybeT (mod_reason_of_t f t)
  | MixedT (reason, t) -> MixedT (f reason, t)
  | ModuleT (reason, exports) -> ModuleT (f reason, exports)
  | NullT reason -> NullT (f reason)
  | NumT (reason, t) -> NumT (f reason, t)
  | ObjProtoT reason -> ObjProtoT (f reason)
  | ObjT (reason, ot) -> ObjT (f reason, ot)
  | OpenPredT (reason, t, p, n) -> OpenPredT (f reason, t, p, n)
  | OptionalT t -> OptionalT (mod_reason_of_t f t)
  | PolyT (plist, t) -> PolyT (plist, mod_reason_of_t f t)
  | RestT t -> RestT (mod_reason_of_t f t)
  | ShapeT t -> ShapeT (mod_reason_of_t f t)
  | SingletonBoolT (reason, t) -> SingletonBoolT (f reason, t)
  | SingletonNumT (reason, t) -> SingletonNumT (f reason, t)
  | SingletonStrT (reason, t) -> SingletonStrT (f reason, t)
  | StrT (reason, t) -> StrT (f reason, t)
  | TaintT (r) -> TaintT (f r)
  | ThisClassT t -> ThisClassT (mod_reason_of_t f t)
  | ThisTypeAppT (t, this, ts) -> ThisTypeAppT (mod_reason_of_t f t, this, ts)
  | TypeAppT (t, ts) -> TypeAppT (mod_reason_of_t f t, ts)
  | TypeMapT (reason, kind, t1, t2) -> TypeMapT (f reason, kind, t1, t2)
  | TypeT (reason, t) -> TypeT (f reason, t)
  | UnionT (reason, ts) -> UnionT (f reason, ts)
  | VoidT reason -> VoidT (f reason)

and mod_reason_of_defer_use_t f = function
  | DestructuringT (reason, s) -> DestructuringT (f reason, s)
  | TypeDestructorT (reason, s) -> TypeDestructorT (f reason, s)

and mod_reason_of_use_t f = function
  | UseT (_, t) -> UseT (UnknownUse, mod_reason_of_t f t)
  | AdderT (reason, rt, lt) -> AdderT (f reason, rt, lt)
  | AndT (reason, t1, t2) -> AndT (f reason, t1, t2)
  | ApplyT (reason, l, ft) -> ApplyT (f reason, l, ft)
  | ArrRestT (reason, i, t) -> ArrRestT (f reason, i, t)
  | AssertArithmeticOperandT reason -> AssertArithmeticOperandT (f reason)
  | AssertBinaryInLHST reason -> AssertBinaryInLHST (f reason)
  | AssertBinaryInRHST reason -> AssertBinaryInRHST (f reason)
  | AssertForInRHST reason -> AssertForInRHST (f reason)
  | AssertImportIsValueT (reason, name) -> AssertImportIsValueT (f reason, name)
  | BecomeT (reason, t) -> BecomeT (f reason, t)
  | BindT (reason, ft) -> BindT (f reason, ft)
  | CallElemT (reason_call, reason_lookup, t, ft) ->
      CallElemT (f reason_call, reason_lookup, t, ft)
  | CallLatentPredT (reason, b, k, l, t) ->
      CallLatentPredT (f reason, b, k, l, t)
  | CallOpenPredT (reason, sense, key, l, t) ->
      CallOpenPredT (f reason, sense, key, l, t)
  | CallT (reason, ft) -> CallT (f reason, ft)
  | ChoiceKitUseT (reason, tool) -> ChoiceKitUseT (f reason, tool)
  | CJSExtractNamedExportsT (reason, exports, t2) ->
      CJSExtractNamedExportsT (f reason, exports, t2)
  | CJSRequireT (reason, t) -> CJSRequireT (f reason, t)
  | ComparatorT (reason, t) -> ComparatorT (f reason, t)
  | ConstructorT (reason, ts, t) -> ConstructorT (f reason, ts, t)
  | CopyNamedExportsT (reason, target_module_t, t_out) ->
      CopyNamedExportsT(f reason, target_module_t, t_out)
  | DebugPrintT reason -> DebugPrintT (f reason)
  | ElemT (reason, t, action) -> ElemT (f reason, t, action)
  | EqT (reason, t) -> EqT (f reason, t)
  | ExportNamedT (reason, tmap, t_out) -> ExportNamedT(f reason, tmap, t_out)
  | GetElemT (reason, it, et) -> GetElemT (f reason, it, et)
  | GetKeysT (reason, t) -> GetKeysT (f reason, t)
  | GetPropT (reason, n, t) -> GetPropT (f reason, n, t)
  | GetStaticsT (reason, t) -> GetStaticsT (f reason, t)
  | GuardT (pred, result, t) -> GuardT (pred, result, mod_reason_of_t f t)
  | HasOwnPropT (reason, prop) -> HasOwnPropT (f reason, prop)
  | HasPropT (reason, strict, prop) -> HasPropT (f reason, strict, prop)
  | IdxUnMaybeifyT (reason, t_out) -> IdxUnMaybeifyT (f reason, t_out)
  | IdxUnwrap (reason, t_out) -> IdxUnwrap (f reason, t_out)
  | ImportDefaultT (reason, import_kind, name, t) ->
      ImportDefaultT (f reason, import_kind, name, t)
  | ImportModuleNsT (reason, t) -> ImportModuleNsT (f reason, t)
  | ImportNamedT (reason, import_kind, name, t) ->
      ImportNamedT (f reason, import_kind, name, t)
  | ImportTypeofT (reason, name, t) -> ImportTypeofT (f reason, name, t)
  | ImportTypeT (reason, name, t) -> ImportTypeT (f reason, name, t)
  | IntersectionPreprocessKitT (reason, tool) ->
      IntersectionPreprocessKitT (f reason, tool)
  | LookupT (reason, r2, ts, x, t) -> LookupT (f reason, r2, ts, x, t)
  | MakeExactT (reason, t) -> MakeExactT (f reason, t)
  | MapTypeT (reason, kind, t, k) -> MapTypeT (f reason, kind, t, k)
  | MethodT (reason_call, reason_lookup, name, ft) ->
      MethodT (f reason_call, reason_lookup, name, ft)
  | MixinT (reason, inst) -> MixinT (f reason, inst)
  | NotT (reason, t) -> NotT (f reason, t)
  | ObjAssignT (reason, t, t2, filter, resolve) ->
      ObjAssignT (f reason, t, t2, filter, resolve)
  | ObjFreezeT (reason, t) -> ObjFreezeT (f reason, t)
  | ObjRestT (reason, t, t2) -> ObjRestT (f reason, t, t2)
  | ObjSealT (reason, t) -> ObjSealT (f reason, t)
  | ObjTestT (reason, t1, t2) -> ObjTestT (f reason, t1, t2)
  | OrT (reason, t1, t2) -> OrT (f reason, t1, t2)
  | PredicateT (pred, t) -> PredicateT (pred, mod_reason_of_t f t)
  | ReactCreateElementT (reason, t, tout) ->
      ReactCreateElementT (f reason, t, tout)
  | RefineT (reason, p, t) -> RefineT (f reason, p, t)
  | ReposLowerT (reason, t) -> ReposLowerT (f reason, t)
  | ReposUseT (reason, use_op, t) -> ReposUseT (f reason, use_op, t)
  | SentinelPropTestT (l, sense, sentinel, result) ->
      SentinelPropTestT (l, sense, sentinel, mod_reason_of_t f result)
  | SetElemT (reason, it, et) -> SetElemT (f reason, it, et)
  | SetPropT (reason, n, t) -> SetPropT (f reason, n, t)
  | SpecializeT(reason_op, reason_tapp, cache, ts, t) ->
      SpecializeT (f reason_op, reason_tapp, cache, ts, t)
  | SubstOnPredT (reason, subst, t) -> SubstOnPredT (f reason, subst, t)
  | SummarizeT (reason, t) -> SummarizeT (f reason, t)
  | SuperT (reason, inst) -> SuperT (f reason, inst)
  | TestPropT (reason, n, t) -> TestPropT (f reason, n, t)
  | ThisSpecializeT(reason, this, t) -> ThisSpecializeT (f reason, this, t)
  | UnaryMinusT (reason, t) -> UnaryMinusT (f reason, t)
  | UnifyT (t, t2) -> UnifyT (mod_reason_of_t f t, mod_reason_of_t f t2)
  | VarianceCheckT(reason, ts, polarity) ->
      VarianceCheckT (f reason, ts, polarity)
  | TypeAppVarianceCheckT (reason_op, reason_tapp, targs) ->
      TypeAppVarianceCheckT (f reason_op, reason_tapp, targs)

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
    mk_reason (RCustom name) loc

  let name_of_instance_reason r =
    string_of_desc (desc_of_reason r)

  (* TypeT reasons have desc = type `name` *)
  let type_reason name loc =
    mk_reason (RType name) loc

  let name_of_type_reason r =
    match desc_of_reason r with
    | RType name -> name
    | _ -> failwith "not a type reason"

end

(* printing *)
let string_of_defer_use_ctor = function
  | DestructuringT _ -> "DestructuringT"
  | TypeDestructorT _ -> "TypeDestructorT"

let string_of_ctor = function
  | OpenT _ -> "OpenT"
  | AbstractT _ -> "AbstractT"
  | AnnotT _ -> "AnnotT"
  | AnyFunT _ -> "AnyFunT"
  | AnyObjT _ -> "AnyObjT"
  | AnyT _ -> "AnyT"
  | AnyWithLowerBoundT _ -> "AnyWithLowerBoundT"
  | AnyWithUpperBoundT _ -> "AnyWithUpperBoundT"
  | ArrT _ -> "ArrT"
  | BoolT _ -> "BoolT"
  | BoundT _ -> "BoundT"
  | ChoiceKitT (_, tool) ->
    spf "ChoiceKitT %s" begin match tool with
    | Trigger -> "Trigger"
    end
  | ClassT _ -> "ClassT"
  | CustomFunT _ -> "CustomFunT"
  | DiffT _ -> "DiffT"
  | EmptyT _ -> "EmptyT"
  | EvalT _ -> "EvalT"
  | ExactT _ -> "ExactT"
  | ExistsT _ -> "ExistsT"
  | ExtendsT _ -> "ExtendsT"
  | FunProtoApplyT _ -> "FunProtoApplyT"
  | FunProtoBindT _ -> "FunProtoBindT"
  | FunProtoCallT _ -> "FunProtoCallT"
  | FunProtoT _ -> "FunProtoT"
  | FunT _ -> "FunT"
  | IdxWrapper _ -> "IdxWrapper"
  | InstanceT _ -> "InstanceT"
  | IntersectionT _ -> "IntersectionT"
  | KeysT _ -> "KeysT"
  | MaybeT _ -> "MaybeT"
  | MixedT _ -> "MixedT"
  | ModuleT _ -> "ModuleT"
  | NullT _ -> "NullT"
  | NumT _ -> "NumT"
  | ObjProtoT _ -> "ObjProtoT"
  | ObjT _ -> "ObjT"
  | OpenPredT _ -> "OpenPredT"
  | OptionalT _ -> "OptionalT"
  | PolyT _ -> "PolyT"
  | RestT _ -> "RestT"
  | ShapeT _ -> "ShapeT"
  | SingletonBoolT _ -> "SingletonBoolT"
  | SingletonNumT _ -> "SingletonNumT"
  | SingletonStrT _ -> "SingletonStrT"
  | StrT _ -> "StrT"
  | TaintT _ -> "TaintT"
  | ThisClassT _ -> "ThisClassT"
  | ThisTypeAppT _ -> "ThisTypeAppT"
  | TypeAppT _ -> "TypeAppT"
  | TypeMapT _ -> "TypeMapT"
  | TypeT _ -> "TypeT"
  | UnionT _ -> "UnionT"
  | VoidT _ -> "VoidT"

let string_of_internal_use_op = function
  | CopyEnv -> "CopyEnv"
  | MergeEnv -> "MergeEnv"
  | Refinement -> "Refinement"
  | WidenEnv -> "WidenEnv"

let string_of_use_op = function
  | Addition -> "Addition"
  | Coercion -> "Coercion"
  | FunCallParam -> "FunCallParam"
  | FunCallThis _ -> "FunCallThis"
  | FunImplicitReturn -> "FunImplicitReturn"
  | FunReturn -> "FunReturn"
  | Internal op -> spf "Internal %s" (string_of_internal_use_op op)
  | MissingTupleElement _ -> "MissingTupleElement"
  | TypeRefinement -> "TypeRefinement"
  | UnknownUse -> "UnknownUse"

let string_of_use_ctor = function
  | UseT (op, t) -> spf "UseT(%s, %s)" (string_of_use_op op) (string_of_ctor t)

  | AdderT _ -> "AdderT"
  | AndT _ -> "AndT"
  | ApplyT _ -> "ApplyT"
  | ArrRestT _ -> "ArrRestT"
  | AssertArithmeticOperandT _ -> "AssertArithmeticOperandT"
  | AssertBinaryInLHST _ -> "AssertBinaryInLHST"
  | AssertBinaryInRHST _ -> "AssertBinaryInRHST"
  | AssertForInRHST _ -> "AssertForInRHST"
  | AssertImportIsValueT _ -> "AssertImportIsValueT"
  | BecomeT _ -> "BecomeT"
  | BindT _ -> "BindT"
  | CallElemT _ -> "CallElemT"
  | CallLatentPredT _ -> "CallLatentPredT"
  | CallOpenPredT _ -> "CallOpenPredT"
  | CallT _ -> "CallT"
  | ChoiceKitUseT (_, tool) ->
    spf "ChoiceKitUseT %s" begin match tool with
    | FullyResolveType _ -> "FullyResolveType"
    | TryFlow _ -> "TryFlow"
    end
  | CJSExtractNamedExportsT _ -> "CJSExtractNamedExportsT"
  | CJSRequireT _ -> "CJSRequireT"
  | ComparatorT _ -> "ComparatorT"
  | ConstructorT _ -> "ConstructorT"
  | CopyNamedExportsT _ -> "CopyNamedExportsT"
  | DebugPrintT _ -> "DebugPrintT"
  | ElemT _ -> "ElemT"
  | EqT _ -> "EqT"
  | ExportNamedT _ -> "ExportNamedT"
  | GetElemT _ -> "GetElemT"
  | GetKeysT _ -> "GetKeysT"
  | GetPropT _ -> "GetPropT"
  | GetStaticsT _ -> "GetStaticsT"
  | GuardT _ -> "GuardT"
  | HasOwnPropT _ -> "HasOwnPropT"
  | HasPropT _ -> "HasPropT"
  | IdxUnMaybeifyT _ -> "IdxUnMaybeifyT"
  | IdxUnwrap _ -> "IdxUnwrap"
  | ImportDefaultT _ -> "ImportDefaultT"
  | ImportModuleNsT _ -> "ImportModuleNsT"
  | ImportNamedT _ -> "ImportNamedT"
  | ImportTypeofT _ -> "ImportTypeofT"
  | ImportTypeT _ -> "ImportTypeT"
  | IntersectionPreprocessKitT (_, tool) ->
    spf "IntersectionPreprocessKitT %s" begin match tool with
    | ConcretizeTypes _ -> "ConcretizeTypes"
    | SentinelPropTest _ -> "SentinelPropTest"
    | PropExistsTest _ -> "PropExistsTest"
    end
  | LookupT _ -> "LookupT"
  | MakeExactT _ -> "MakeExactT"
  | MapTypeT _ -> "MapTypeT"
  | MethodT _ -> "MethodT"
  | MixinT _ -> "MixinT"
  | NotT _ -> "NotT"
  | ObjAssignT _ -> "ObjAssignT"
  | ObjFreezeT _ -> "ObjFreezeT"
  | ObjRestT _ -> "ObjRestT"
  | ObjSealT _ -> "ObjSealT"
  | ObjTestT _ -> "ObjTestT"
  | OrT _ -> "OrT"
  | PredicateT _ -> "PredicateT"
  | ReactCreateElementT _ -> "ReactCreateElementT"
  | RefineT _ -> "RefineT"
  | ReposLowerT _ -> "ReposLowerT"
  | ReposUseT _ -> "ReposUseT"
  | SentinelPropTestT _ -> "SentinelPropTestT"
  | SetElemT _ -> "SetElemT"
  | SetPropT _ -> "SetPropT"
  | SpecializeT _ -> "SpecializeT"
  | SubstOnPredT _ -> "SubstOnPredT"
  | SummarizeT _ -> "SummarizeT"
  | SuperT _ -> "SuperT"
  | TestPropT _ -> "TestPropT"
  | ThisSpecializeT _ -> "ThisSpecializeT"
  | UnaryMinusT _ -> "UnaryMinusT"
  | UnifyT _ -> "UnifyT"
  | VarianceCheckT _ -> "VarianceCheckT"
  | TypeAppVarianceCheckT _ -> "TypeAppVarianceCheck"

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
        (string_of_binary_test b) (string_of_desc (desc_of_t t))
  | RightP (b, t) ->
      spf "right operand of %s with left operand = %s"
        (string_of_binary_test b) (string_of_desc (desc_of_t t))
  | ExistsP -> "truthy"
  | NullP -> "null"
  | MaybeP -> "null or undefined"

  | SingletonBoolP false -> "false"
  | SingletonBoolP true -> "true"
  | SingletonStrP str -> spf "string `%s`" str
  | SingletonNumP (_,raw) -> spf "number `%s`" raw

  (* typeof *)
  | VoidP -> "undefined"
  | BoolP -> "boolean"
  | StrP -> "string"
  | NumP -> "number"
  | FunP -> "function"
  | ObjP -> "object"

  (* Array.isArray *)
  | ArrP -> "array"

  | PropExistsP (_, key) -> spf "prop `%s` is truthy" key

  | LatentP (OpenT (_, id),i) -> spf "LatentPred(TYPE_%d, %d)" id i
  | LatentP (t,i) -> spf "LatentPred(%s, %d)" (string_of_ctor t) i

let literal_eq x = function
  | Literal y -> x = y
  | Truthy -> false
  | AnyLiteral -> false

let number_literal_eq (x, _) = function
  | Literal (y, _) -> x = y
  | Truthy -> false
  | AnyLiteral -> false

let boolean_literal_eq x = function
  | Some y -> x = y
  | None -> false

let name_of_propref = function
  | Named (_, x) -> Some x
  | Computed _ -> None

let reason_of_propref = function
  | Named (r, _) -> r
  | Computed t -> reason_of_t t
