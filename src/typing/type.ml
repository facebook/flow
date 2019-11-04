(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Polarity
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

type tvar = reason * ident

module rec TypeTerm : sig
  type t =
    (* open type variable *)
    (* A type variable (tvar) is an OpenT(reason, id) where id is an int index
       into a context's graph: a context's graph is a map from tvar ids to nodes
       (see below).

       Note: ids are globally unique. tvars are "owned" by a single context,
       but that context and its tvars may later be merged into other contexts.
     *)
    | OpenT of tvar
    (*************)
    (* def types *)
    (*************)
    | DefT of reason * Trust.trust_rep * def_t
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
    (* bound type variable *)
    | BoundT of reason * string * Polarity.t
    (* existential type variable *)
    | ExistsT of reason
    (* this-abstracted class *)
    | ThisClassT of reason * t
    (* this instantiation *)
    | ThisTypeAppT of reason * t * t * t list option
    (* type application *)
    | TypeAppT of reason * use_op * t * t list
    (* exact *)
    | ExactT of reason * t
    | FunProtoT of reason (* Function.prototype *)
    | ObjProtoT of reason (* Object.prototype *)
    (* Signifies the end of the prototype chain. Distinct from NullT when it
       appears as an upper bound of an object type, otherwise the same. *)
    | NullProtoT of reason
    | FunProtoApplyT of reason (* Function.prototype.apply *)
    | FunProtoBindT of reason (* Function.prototype.bind *)
    | FunProtoCallT of reason (* Function.prototype.call *)
    (* a merged tvar that had no lowers *)
    | MergedT of reason * use_t list
    (* constrains some properties of an object *)
    | ShapeT of t
    | MatchingPropT of reason * string * t
    (* & types *)
    | IntersectionT of reason * InterRep.t
    (* | types *)
    | UnionT of reason * UnionRep.t
    (* ? types *)
    | MaybeT of reason * t
    (* type of an optional parameter *)
    | OptionalT of {
        reason: reason;
        type_: t;
        use_desc: bool;
      }
    (* collects the keys of an object *)
    | KeysT of reason * t
    (* annotations *)
    (* A type that annotates a storage location performs two functions:

        * it constrains the types of values stored into the location

        * it masks the actual type of values retrieved from the location, giving
        instead a pro forma type which all such values are considered as having.

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

        We accomplish the insulation by wrapping a tvar with AnnotT, and using a
        "slingshot" trick to grab lowers bounds, wait for the wrapped tvar to
        resolve to a type, then release the lower bounds to the resolved
        type. Meanwhile, the tvar itself flows to its upper bounds as usual.

        Note on usage: AnnotT can be used as a general wrapper for tvars as long
        as the wrapped tvars are 0->1. If instead the possible types of a
        wrapped tvar are T1 and T2, then the current rules would flow T1 | T2 to
        upper bounds, and would flow lower bounds to T1 & T2. **)
    | AnnotT of reason * t * bool (* use_desc *)
    (* Opaque type aliases. The opaquetype.opaque_id is its unique id, opaquetype.underlying_t is
     * the underlying type, which we only allow access to when inside the file the opaque type
     * was defined, and opaquetype.super_t is the super type, which we use when an OpaqueT is
     * an upperbound in a file in which it was not defined. We also have
     * opaquetype.opaque_arg_polarities and opaquetype.opaque_type_args to compare polymorphic
     * opaque types. We need to keep track of these because underlying_t can be None if the opaque
     * type is defined in a libdef. We also keep track of the name of the opaque type in
     * opaquetype.name for pretty printing. *)
    | OpaqueT of reason * opaquetype
    (* Stores exports (and potentially other metadata) for a module *)
    | ModuleT of reason * exporttypes * bool (* is_strict *)
    (* Here's to the crazy ones. The misfits. The rebels. The troublemakers.
        The round pegs in the square holes. **)

    (* types that should never appear in signatures *)
    | InternalT of internal_t
    (* upper bound trigger for type destructors *)
    | TypeDestructorTriggerT of use_op * reason * (reason * bool) option * destructor * t
    (* Sigil representing functions that the type system is not expressive
       enough to annotate, so we customize their behavior internally. *)
    | CustomFunT of reason * custom_fun_kind
    (* Predicate types **)

    (* `OpenPredT (reason, base_t, m_pos, m_neg)` wraps around a base type
       `base_t` and encodes additional information that hold in conditional
       contexts (in the form of logical predicates). This information is split
       into positive and negative versions in `m_pos` and `m_neg`. The
       predicates here are "open" in the sense that they contain free variable
       instances, which are the keys to the two maps.
    *)
    | OpenPredT of {
        reason: reason;
        base_t: t;
        m_pos: predicate Key_map.t;
        m_neg: predicate Key_map.t;
      }
    | ReposT of reason * t
    | AnyT of reason * any_source

  and def_t =
    | NumT of number_literal literal
    | StrT of string literal
    | BoolT of bool option
    | EmptyT of empty_flavor
    | MixedT of mixed_flavor
    | NullT
    | VoidT
    | FunT of static * prototype * funtype
    | ObjT of objtype
    | ArrT of arrtype
    (* type of a class *)
    | ClassT of t
    (* type of an instance of a class *)
    | InstanceT of static * super * implements * insttype
    (* singleton string, matches exactly a given string literal *)
    | SingletonStrT of string
    (* matches exactly a given number literal, for some definition of "exactly"
       when it comes to floats... *)
    | SingletonNumT of number_literal
    (* singleton bool, matches exactly a given boolean literal *)
    | SingletonBoolT of bool
    (* A subset of StrT that represents a set of characters,
       e.g. RegExp flags *)
    | CharSetT of String_utils.CharSet.t
    (* type aliases *)
    | TypeT of type_t_kind * t
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
    | PolyT of {
        tparams_loc: ALoc.t;
        tparams: typeparam Nel.t;
        t_out: t;
        id: int;
      }
    (* Type that wraps object types for the CustomFunT(Idx) function *)
    | IdxWrapper of t
    (* React$AbstractComponent<Config, DefaultProps, Instance> *)
    | ReactAbstractComponentT of {
        config: t;
        instance: t;
      }
    (* Enum types *)
    | EnumT of enum_t
    | EnumObjectT of enum_t

  and defer_use_t =
    | LatentPredT of reason * predicate
    (* destructors that extract parts of various kinds of types *)
    | TypeDestructorT of use_op * reason * destructor

  and enum_t = {
    enum_id: ALoc.t;
    enum_name: string;
    members: SSet.t;
  }

  and internal_t =
    (* toolkit for making choices *)
    | ChoiceKitT of reason * choice_tool
    (* util for deciding subclassing relations *)
    | ExtendsT of reason * t * t
    | ReposUpperT of reason * t
    | OptionalChainVoidT of reason

  and internal_use_op =
    | CopyEnv
    | MergeEnv
    | Refinement
    | WidenEnv

  and 'loc virtual_root_use_op =
    | ObjectSpread of { op: 'loc virtual_reason }
    | ObjectChain of { op: 'loc virtual_reason }
    | Addition of {
        op: 'loc virtual_reason;
        left: 'loc virtual_reason;
        right: 'loc virtual_reason;
      }
    | AssignVar of {
        var: 'loc virtual_reason option;
        init: 'loc virtual_reason;
      }
    | Cast of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | ClassExtendsCheck of {
        def: 'loc virtual_reason;
        name: 'loc virtual_reason;
        extends: 'loc virtual_reason;
      }
    | ClassImplementsCheck of {
        def: 'loc virtual_reason;
        name: 'loc virtual_reason;
        implements: 'loc virtual_reason;
      }
    | ClassOwnProtoCheck of {
        prop: string;
        own_loc: 'loc option;
        proto_loc: 'loc option;
      }
    | Coercion of {
        from: 'loc virtual_reason;
        target: 'loc virtual_reason;
      }
    | DeleteProperty of {
        lhs: 'loc virtual_reason;
        prop: 'loc virtual_reason;
      }
    | DeleteVar of { var: 'loc virtual_reason }
    | FunCall of {
        op: 'loc virtual_reason;
        fn: 'loc virtual_reason;
        args: 'loc virtual_reason list;
        local: bool; (* Whether we can blame back to the function def *)
      }
    | FunCallMethod of {
        op: 'loc virtual_reason;
        fn: 'loc virtual_reason;
        prop: 'loc virtual_reason;
        args: 'loc virtual_reason list;
        local: bool; (* Whether we can blame back to the function def *)
      }
    | FunReturnStatement of { value: 'loc virtual_reason }
    | FunImplicitReturn of {
        fn: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | GeneratorYield of { value: 'loc virtual_reason }
    | GetProperty of 'loc virtual_reason
    | InitField of {
        op: 'loc virtual_reason;
        body: 'loc virtual_reason;
      }
    | Internal of internal_use_op
    | JSXCreateElement of {
        op: 'loc virtual_reason;
        component: 'loc virtual_reason;
      }
    | ReactCreateElementCall of {
        op: 'loc virtual_reason;
        component: 'loc virtual_reason;
        children: 'loc;
      }
    | ReactGetIntrinsic of { literal: 'loc virtual_reason }
    | Speculation of 'loc virtual_use_op
    | TypeApplication of { type': 'loc virtual_reason }
    | SetProperty of {
        lhs: 'loc virtual_reason;
        prop: 'loc virtual_reason;
        value: 'loc virtual_reason;
      }
    | UpdateProperty of {
        lhs: 'loc virtual_reason;
        prop: 'loc virtual_reason;
      }
    | UnknownUse

  and 'loc virtual_frame_use_op =
    | ArrayElementCompatibility of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | FunCompatibility of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | FunMissingArg of {
        n: int;
        op: 'loc virtual_reason;
        def: 'loc virtual_reason;
      }
    | FunParam of {
        n: int;
        name: string option;
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | FunRestParam of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | FunReturn of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | ImplicitTypeParam
    | IndexerKeyCompatibility of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | CallFunCompatibility of { n: int }
    | TupleMapFunCompatibility of { value: 'loc virtual_reason }
    | ObjMapFunCompatibility of { value: 'loc virtual_reason }
    | ObjMapiFunCompatibility of {
        key: 'loc virtual_reason;
        value: 'loc virtual_reason;
      }
    | PropertyCompatibility of {
        prop: string option;
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | ReactConfigCheck
    | ReactGetConfig of { polarity: Polarity.t }
    | TupleElementCompatibility of {
        n: int;
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | TypeArgCompatibility of {
        name: string;
        targ: 'loc virtual_reason;
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
        polarity: Polarity.t;
      }
    | TypeParamBound of { name: string }
    | UnifyFlip

  and 'loc virtual_use_op =
    | Op of 'loc virtual_root_use_op
    | Frame of 'loc virtual_frame_use_op * 'loc virtual_use_op

  and use_op = ALoc.t virtual_use_op

  and root_use_op = ALoc.t virtual_root_use_op

  and frame_use_op = ALoc.t virtual_frame_use_op

  and use_t =
    (* def types can be used as upper bounds *)
    | UseT of use_op * t
    (*************)
    (* use types *)
    (*************)

    (* operations on runtime values, such as functions, objects, and arrays *)
    | BindT of use_op * reason * funcalltype * bool (* pass-through *)
    | CallT of use_op * reason * funcalltype
    (* The last position is an optional type that probes into the type of the
       method called. This will be primarily used for type-table bookkeeping. *)
    | MethodT of
        use_op * (* call *) reason * (* lookup *) reason * propref * funcalltype * t option
    (* Similar to the last element of the MethodT *)
    | SetPropT of use_op * reason * propref * set_mode * write_ctx * t * t option
    (* The boolean flag indicates whether or not it is a static lookup. We cannot know this when
     * we generate the constraint, since the lower bound may be an unresolved OpenT. If it
     * resolves to a ClassT, we flip the flag to true, which causes us to check the private static
     * fields when the InstanceT ~> SetPrivatePropT constraint is processsed *)
    | SetPrivatePropT of
        use_op * reason * string * set_mode * class_binding list * bool * t * t option
    | GetPropT of use_op * reason * propref * t
    (* For shapes *)
    | MatchPropT of use_op * reason * propref * t
    (* The same comment on SetPrivatePropT applies here *)
    | GetPrivatePropT of use_op * reason * string * class_binding list * bool * t
    | TestPropT of reason * ident * propref * t
    (* SetElemT has a `tout` parameter to serve as a trigger for ordering
       operations. We only need this in one place: object literal initialization.
       In particular, a computed property in the object initializer users SetElemT
       to initialize the property value, but in order to avoid race conditions we
       need to ensure that reads happen after writes. *)
    | SetElemT of use_op * reason * t * set_mode * t * t option (*tout *)
    | GetElemT of use_op * reason * t * t
    | CallElemT of (* call *) reason * (* lookup *) reason * t * funcalltype
    | GetStaticsT of reason * t_out
    | GetProtoT of reason * t_out
    | SetProtoT of reason * t
    (* repositioning *)
    | ReposLowerT of reason * bool (* use_desc *) * use_t
    | ReposUseT of reason * bool (* use_desc *) * use_op * t
    (* operations on runtime types, such as classes and functions *)
    | ConstructorT of use_op * reason * targ list option * call_arg list * t
    | SuperT of use_op * reason * derived_type
    | ImplementsT of use_op * t
    | MixinT of reason * t
    | ToStringT of reason * use_t
    (* overloaded +, could be subsumed by general overloading *)
    | AdderT of use_op * reason * bool * t * t
    (* overloaded relational operator, could be subsumed by general
       overloading *)
    | ComparatorT of reason * bool * t
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
    | EqT of reason * bool * t
    (* logical operators *)
    | AndT of reason * t * t
    | OrT of reason * t * t
    | NullishCoalesceT of reason * t * t
    | NotT of reason * t
    (* operation on polymorphic types *)
    (* SpecializeT(_, _, _, cache, targs, tresult) instantiates a polymorphic type
        with type arguments targs, and flows the result into tresult. If cache
        is set, it looks up a cache of existing instantiations for the type
        parameters of the polymorphic type, unifying the type arguments with
        those instantiations if such exist.

        The first reason is the reason why we're specializing. The second
        reason points to the type application itself
    **)
    | SpecializeT of use_op * reason * reason * specialize_cache * t list option * t
    (* operation on this-abstracted classes *)
    | ThisSpecializeT of reason * t * cont
    (* variance check on polymorphic types *)
    | VarianceCheckT of reason * t list * Polarity.t
    | TypeAppVarianceCheckT of use_op * reason * reason * (t * t) list
    (* In TypeAppT (c, ts) ~> TypeAppT (c, ts) we need to check both cs against
     * each other which means that we must concretize them first. *)
    | ConcretizeTypeAppsT of
        (* The use_op from our original TypeAppT ~> TypeAppT *)
        use_op
        * (* The type args and reason for the TypeAppT that is currently the
           * lower bound *)
        (t list * use_op * reason)
        * (* The polymorphic type, its type args, and reason for the TypeAppT that
           * is currently the upper bound. *)
        (t * t list * use_op * reason)
        * (* A boolean which answers the question: Is the TypeAppT that is
           * currently our lower bound in fact our upper bound in the original
           * TypeAppT ~> TypeAppT? If the answer is yes then we need to flip our
           * tuples and flow the polymorphic type currently in our upper bound as
           * the lower bound. See the implementation of flow_js for more clarity. *)
          bool
    (* operation on prototypes *)
    (* LookupT(_, strict, try_ts_on_failure, x, lookup_action) looks for
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

    (* Resolves the object into which the properties are assigned *)
    | ObjAssignToT of use_op * reason * t * t * obj_assign_kind
    (* Resolves the object from which the properties are assigned *)
    | ObjAssignFromT of use_op * reason * t * t * obj_assign_kind
    | ObjFreezeT of reason * t
    | ObjRestT of reason * string list * t
    | ObjSealT of reason * t
    (* test that something is a valid proto (object-like or null) *)
    | ObjTestProtoT of reason * t_out
    (* test that something is object-like, returning a default type otherwise *)
    | ObjTestT of reason * t * t
    (* Assign properties to module.exports. The only interesting case is when module.exports is a
       function type, where we set the statics field of the function type. *)
    | ModuleExportsAssignT of reason * t * t
    (* assignment rest element in array pattern *)
    | ArrRestT of use_op * reason * int * t
    (* Guarded unification *)
    | UnifyT of t * t (* bidirectional *)
    (* unifies with incoming concrete lower bound *)
    | BecomeT of reason * t
    (* Keys *)
    | GetKeysT of reason * use_t
    | HasOwnPropT of use_op * reason * string literal
    (* Values *)
    | GetValuesT of reason * t
    (* Element access *)
    | ElemT of use_op * reason * t * elem_action
    (* exact ops *)
    | MakeExactT of reason * cont
    (*
     * Module import handling
     *
     * Why do the following have a is_strict flag, when that's already present in the context
     * local metadata? Because when checking cycles, during the merge we use the context of the
     * "leader" module, and thus the is_strict flag in the context won't be accurate.
     *)
    | CJSRequireT of reason * t * bool (* is_strict *)
    | ImportModuleNsT of reason * t * bool (* is_strict *)
    | ImportDefaultT of reason * import_kind * (string * string) * t * bool (* is_strict *)
    | ImportNamedT of reason * import_kind * string * string * t * bool (* is_strict *)
    | ImportTypeT of reason * string * t
    | ImportTypeofT of reason * string * t
    | AssertImportIsValueT of reason * string
    (* Module export handling *)
    | CJSExtractNamedExportsT of
        reason
        * (* local ModuleT *)
        (reason * exporttypes * bool)
        * (* is_strict *)
          (* 't_out' to receive the resolved ModuleT *) t_out
    | CopyNamedExportsT of reason * t * t_out
    | CopyTypeExportsT of reason * t * t_out
    | ExportNamedT of reason * (ALoc.t option * t) SMap.t (* exports_tmap *) * export_kind * t_out
    | ExportTypeT of reason * string (* export_name *) * t (* target_module_t *) * t_out
    | AssertExportIsTypeT of reason * string (* export name *) * t_out
    (* Map a FunT over a structure *)
    | MapTypeT of use_op * reason * type_map * t_out
    | ObjKitT of use_op * reason * Object.resolve_tool * Object.tool * t_out
    | ReactKitT of use_op * reason * React.tool
    | ChoiceKitUseT of reason * choice_use_tool
    (* tools for preprocessing intersections *)
    | IntersectionPreprocessKitT of reason * intersection_preprocess_tool
    | DebugPrintT of reason
    | DebugSleepT of reason
    | SentinelPropTestT of reason * t * string * bool * UnionEnum.star * t_out
    | IdxUnwrap of reason * t_out
    | IdxUnMaybeifyT of reason * t_out
    | OptionalChainT of reason * reason * use_t * (* voids *) t_out
    | InvariantT of reason
    (* Function predicate uses *)

    (*
     * The following two uses are used when a predicate function is called to
     * establish a predicate over one of its arguments.
     *)

    (*
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
    (*
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
    (*
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
    (*
     * `RefineT (reason, pred, tvar)` is an instruction to refine an incoming
     * flow using the predicate `pred`. The result will be stored in `tvar`,
     * which is expected to be a type variable.
     *)
    | RefineT of reason * predicate * t
    (* Spread elements show up in a bunch of places: array literals, function
     * parameters, function call arguments, method arguments. constructor
     * arguments, etc. Often we have logic that depends on what the spread
     * elements resolve to. ResolveSpreadT is a use type that waits for a list
     * of spread and non-spread elements to resolve, and then constructs
     * whatever type it resolves to *)
    | ResolveSpreadT of use_op * reason * resolve_spread_type
    (* CondT (_, then_t_opt, else_t, tout) is a branch, which flows `else_t`
     * into `tout` when the resolved lower bound is `empty`. If the resolved
     * lower bound is non-empty, it will flow either `Some then_t` or the lower
     * bound itself into `tout`. *)
    | CondT of reason * t option * t * t_out
    (* util for deciding subclassing relations *)
    | ExtendsUseT of use_op * reason * t list * t * t
    (* Models the GetProps React functionality *)
    | ReactPropsToOut of reason * t
    | ReactInToProps of reason * t
    (* Used to calculate a destructured binding. If annot is true, the lower
     * bound is an annotation (0->1), and t_out will be unified with the
     * destructured type. The caller should wrap the tvar with an AnnotT. *)
    | DestructuringT of reason * destruct_kind * selector * t_out
    | CreateObjWithComputedPropT of {
        reason: reason;
        value: t;
        tout_tvar: tvar;
      }

  (* Bindings created from destructuring annotations should themselves act like
   * annotations. That is, `var {p}: {p: string}; p = 0` should be an error,
   * because `p` should behave like a `string` annotation.
   *
   * We accomplish this by wrapping the binding itself in an AnnotT type. The
   * wrapped type must be 0->1, which is enforced with BecomeT.
   *
   * Since DestructuringT uses with the DestructAnnot kind should only encounter
   * annotations, the set of lower bounds will be a subset of all possible
   * types. The only important cases to handle are disjunctive types that would
   * violate the 0->1 property, like UnionT and MaybeT. *)
  and destruct_kind =
    | DestructAnnot
    | DestructInfer

  (* use_ts which can be part of an optional chain, with t_out factored out *)
  and opt_use_t =
    | OptCallT of use_op * reason * opt_funcalltype
    | OptGetPropT of use_op * reason * propref
    | OptGetPrivatePropT of use_op * reason * string * class_binding list * bool
    | OptTestPropT of reason * ident * propref
    | OptGetElemT of use_op * reason * t

  and opt_state =
    | NonOptional
    | NewChain
    | ContinueChain

  and specialize_cache = reason list option

  and predicate =
    | AndP of predicate * predicate
    | OrP of predicate * predicate
    | NotP of predicate
    (* mechanism to handle binary tests where both sides need to be evaluated *)
    | LeftP of binary_test * t
    | RightP of binary_test * t
    (* Only track locations of existence checks created when walking the AST *)
    | ExistsP (* truthy *) of ALoc.t option (* Location of the existence check *)
    | NullP (* null *)
    | MaybeP (* null or undefined *)
    | SingletonBoolP of ALoc.t * bool (* true or false *)
    | SingletonStrP of ALoc.t * bool * string (* string literal *)
    | SingletonNumP of ALoc.t * bool * number_literal
    | BoolP (* boolean *)
    | FunP (* function *)
    | NumP (* number *)
    | ObjP (* object *)
    | StrP (* string *)
    | SymbolP (* symbol *)
    | VoidP (* undefined *)
    | ArrP (* Array.isArray *)
    (* `if (a.b)` yields `flow (a, PredicateT(PropExistsP ("b", loc), tout))` *)
    | PropExistsP of string * ALoc.t option (* Location of the property in the existence check *)
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
    | Literal of bool option * 'a
    | Truthy
    | AnyLiteral

  and number_literal = float * string

  and mixed_flavor =
    | Mixed_everything
    | Mixed_truthy
    | Mixed_non_maybe
    | Mixed_non_null
    | Mixed_non_void
    | Mixed_function
    | Mixed_symbol

  and empty_flavor =
    | Bottom
    | Zeroed

  and any_source =
    | Annotated
    | AnyError
    | Unsound of unsoundness_kind
    | Untyped

  (* Tracks the kinds of unsoundness inherent in Flow. If you can't find a kind that matches
     your use case, make one *)
  and unsoundness_kind =
    | BoundFunctionThis
    | ComputedNonLiteralKey
    | Constructor
    | DummyStatic
    | Existential
    | Exports
    | FunctionPrototype
    | InferenceHooks
    | InstanceOfRefinement
    | Merged
    | ResolveSpread
    | Unchecked
    | Unimplemented
    | UnresolvedType
    | WeakContext

  and fun_param = string option * t

  and fun_rest_param = string option * ALoc.t * t

  (* used by FunT *)
  and funtype = {
    this_t: t;
    params: fun_param list;
    rest_param: fun_rest_param option;
    return_t: t;
    closure_t: int;
    is_predicate: bool;
    changeset: Changeset.t;
    def_reason: Reason.t;
  }

  (* Used by CallT and similar constructors *)
  and funcalltype = {
    call_this_t: t;
    call_targs: targ list option;
    call_args_tlist: call_arg list;
    call_tout: t;
    call_closure_t: int;
    call_strict_arity: bool;
  }

  and targ =
    (* This tvar gets lower bounds from the instantiations of _. It is used to power type-services
     * like type-at-pos and should not be used for type checking
     *)
    | ImplicitArg of tvar
    | ExplicitArg of t

  and opt_funcalltype = t * targ list option * call_arg list * int * bool

  and call_arg =
    | Arg of t
    | SpreadArg of t

  and arrtype =
    | ArrayAT of t * t list option
    (* TupleAT of elemt * tuple_types. Why do tuples carry around elemt? Well, so
     * that they don't need to recompute their general type when you do
     * myTuple[expr]
     *)
    | TupleAT of t * t list
    (* ROArrayAT(elemt) is the super type for all tuples and arrays for which
     * elemt is a supertype of every element type *)
    | ROArrayAT of t

  and objtype = {
    flags: flags;
    dict_t: dicttype option;
    props_tmap: Properties.id;
    proto_t: prototype;
    call_t: int option;
  }

  (* Object.assign(target, source1, ...source2) first resolves target then the
     sources. *)
  and obj_assign_kind =
    (* Obj.assign(target, source) with flag indicating whether source must be exact *)
    | ObjAssign of { assert_exact: bool }
    (* Obj.assign(target, ...source) *)
    | ObjSpreadAssign

  and cont =
    | Lower of use_op * t
    | Upper of use_t

  (* Instance types are represented as an InstanceT while statics are ObjT.
     For superclass compatibility checking, it suffices to just check the
     properties instead of creating the full InstanceT/ObjT. *)
  and derived_type =
    | Derived of {
        own: property SMap.t;
        proto: property SMap.t;
        static: property SMap.t;
      }

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
    | NonstrictReturning of (t * t) option * (ident * (reason * reason)) option
    | ShadowRead of reason option * Properties.id Nel.t
    | ShadowWrite of Properties.id Nel.t

  and lookup_action =
    | ReadProp of {
        use_op: use_op;
        obj_t: t;
        tout: t;
      }
    | WriteProp of {
        use_op: use_op;
        obj_t: t;
        prop_tout: t option;
        tin: t;
        write_ctx: write_ctx;
        mode: set_mode;
      }
    | LookupProp of use_op * Property.t
    | SuperProp of use_op * Property.t
    | MatchProp of use_op * t

  and write_ctx =
    | ThisInCtor
    | Normal

  (* Property writes can either be assignments (from `a.x = e`) or deletions
     (from `delete a.x`). For the most part, we can treat these the same
     (flowing void into `a.x` in a deletion), but if the property being deleted
     originates in an indexer, we need to know not to flow `void` into the
     indexer's type, which would cause an error. The `set_mode` type records
     whether a property write is an assignment or a deletion, to help handle
     this special case. *)
  and set_mode =
    | Delete
    | Assign

  (* See the above comment on `set_mode`--this type is the other half, which
     records whether a property originates in an indexer or from a property
     map. This is relevant when the property is being deleted--we should flow
     `void` to the property's type if it originates in a property map (to ensure
     that its type is nullable and raise an error if it's not) but not if it's
     from an indexer, where our current semantics are intentionally unsound with
     respect to undefined anyways. *)
  and property_source =
    | DynamicProperty
    | PropertyMapProperty
    | IndexerProperty

  (* WriteElem has a `tout` parameter to serve as a trigger for ordering
     operations. We only need this in one place: object literal initialization.
     In particular, a computed property in the object initializer users SetElemT
     to initialize the property value, but in order to avoid race conditions we
     need to ensure that reads happen after writes. *)
  and elem_action =
    | ReadElem of t
    | WriteElem of t * t option (* tout *) * set_mode
    | CallElem of reason (* call *) * funcalltype

  and propref =
    | Named of reason * name
    | Computed of t

  and sealtype =
    | UnsealedInFile of File_key.t option
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
    dict_polarity: Polarity.t;
  }

  (* Locations refer to the location of the identifier, if one exists *)
  and property =
    | Field of ALoc.t option * t * Polarity.t
    | Get of ALoc.t option * t
    | Set of ALoc.t option * t
    | GetSet of ALoc.t option * t * ALoc.t option * t
    | Method of ALoc.t option * t

  (* This has to go here so that Type doesn't depend on Scope *)
  and class_binding = {
    class_binding_id: ALoc.t;
    class_private_fields: Properties.id;
    class_private_static_fields: Properties.id;
  }

  and insttype = {
    class_id: ALoc.t;
    type_args: (string * reason * t * Polarity.t) list;
    own_props: Properties.id;
    proto_props: Properties.id;
    inst_call_t: int option;
    initialized_fields: SSet.t;
    initialized_static_fields: SSet.t;
    has_unknown_react_mixins: bool;
    inst_kind: instance_kind;
  }

  and instance_kind =
    | ClassKind
    | InterfaceKind of { inline: bool }

  and opaquetype = {
    opaque_id: ALoc.t;
    underlying_t: t option;
    super_t: t option;
    opaque_type_args: (string * reason * t * Polarity.t) list;
    opaque_name: string;
  }

  and exporttypes = {
    (*
     * tmap used to store individual, named ES exports as generated by `export`
     * statements in a module. Note that this includes `export type` as well.
     *
     * Note that CommonJS modules may also populate this tmap if their export
     * type is an object (that object's properties become named exports) or if
     * it has any "type" exports via `export type ...`.
     *)
    exports_tmap: Exports.id;
    (*
     * This stores the CommonJS export type when applicable and is used as the
     * exact return type for calls to require(). This slot doesn't apply to pure
     * ES modules.
     *)
    cjs_export: t option;
    (*
     * Sometimes we claim the module exports any or Object, implying that it
     * has every named export
     *)
    has_every_named_export: bool;
  }

  and import_kind =
    | ImportType
    | ImportTypeof
    | ImportValue

  and export_kind =
    | ExportType
    | ExportValue
    | ReExport

  and typeparam = {
    reason: reason;
    name: string;
    bound: t;
    polarity: Polarity.t;
    default: t option;
  }

  and typeparams_nonempty = ALoc.t * typeparam Nel.t

  and typeparams = typeparams_nonempty option

  and selector =
    | Prop of string * bool
    | Elem of t
    | ObjRest of string list
    | ArrRest of int
    | Default

  and destructor =
    | NonMaybeType
    | PropertyType of string
    | ElementType of t
    | Bind of t
    | ReadOnlyType
    | SpreadType of
        Object.Spread.target * Object.Spread.operand list * Object.Spread.operand_slice option
    | RestType of Object.Rest.merge_mode * t
    | ValuesType
    | CallType of t list
    | TypeMap of type_map
    | ReactElementPropsType
    | ReactElementConfigType
    | ReactElementRefType
    | ReactConfigType of t

  and type_map =
    | TupleMap of t
    | ObjectMap of t
    | ObjectMapi of t

  and prototype = t

  and super = t

  and static = t

  and implements = t list

  and t_out = t

  and custom_fun_kind =
    (* builtins *)
    | ObjectAssign
    | ObjectGetPrototypeOf
    | ObjectSetPrototypeOf
    (* common community functions *)
    | Compose of bool
    (* 3rd party libs *)
    | ReactPropType of React.PropType.t
    | ReactCreateClass
    | ReactCreateElement
    | ReactCloneElement
    | ReactElementFactory of t
    (* Facebookisms *)
    | Idx
    | TypeAssertIs
    | TypeAssertThrows
    | TypeAssertWraps
    (* Internal tools *)
    | DebugPrint
    | DebugThrow
    | DebugSleep

  and choice_tool = Trigger

  and choice_use_tool =
    | FullyResolveType of ident
    | TryFlow of int * spec

  and intersection_preprocess_tool =
    | ConcretizeTypes of t list * t list * t * use_t
    | SentinelPropTest of bool * string * t * t * t
    | PropExistsTest of bool * string * t * t

  and spec =
    | UnionCases of use_op * t * UnionRep.t * t list
    | IntersectionCases of t list * use_t

  (* A dependent predicate type consisting of:
     - the result type of the test (not always bool)
     - a map (lookup key -> type) of refinements which hold if the
       test is true
     - a map of refinements which hold if the test is false
  *)
  and dep_preds = t * predicate Key_map.t * predicate Key_map.t

  and resolve_spread_type = {
    (* This is the list of elements that are already resolved (that is have no
     * more unresolved spread elements *)
    rrt_resolved: resolved_param list;
    (* This is the list of elements that we have yet to resolve *)
    rrt_unresolved: unresolved_param list;
    (* Once all the elements have been resolved, this tells us what type to
     * construct *)
    rrt_resolve_to: spread_resolve;
  }

  and unresolved_param =
    | UnresolvedArg of t
    | UnresolvedSpreadArg of t

  and resolved_param =
    | ResolvedArg of t
    | ResolvedSpreadArg of reason * arrtype
    | ResolvedAnySpreadArg of reason

  and spread_resolve =
    (* Once we've finished resolving spreads, try to construct a tuple *)
    | ResolveSpreadsToTuple of int * t * t (* elem type, array type *)
    (* Once we've finished resolving spreads, try to construct an array with known element types *)
    | ResolveSpreadsToArrayLiteral of int * t * t (* elem type, array type *)
    (* Once we've finished resolving spreads, try to construct a non-tuple array *)
    | ResolveSpreadsToArray of t * t (* elem type, array type *)
    (* Once we've finished resolving spreads for a function's arguments, call the
     * function with those arguments *)
    | ResolveSpreadsToMultiflowCallFull of int * funtype
    | ResolveSpreadsToMultiflowSubtypeFull of int * funtype
    (* We can also call custom functions. *)
    | ResolveSpreadsToCustomFunCall of int * custom_fun_kind * t
    (* Once we've finished resolving spreads for a function's arguments,
     * partially apply the arguments to the function and return the resulting
     * function (basically what func.bind(that, ...args) does) *)
    | ResolveSpreadsToMultiflowPartial of int * funtype * reason * t
    | ResolveSpreadsToCallT of funcalltype * t

  (* Add some flavor to the TypeT constructor. For now this information is only
   * used by the type normalizer. *)
  and type_t_kind =
    | TypeAliasKind (* type A = T *)
    | TypeParamKind
    | OpaqueKind (* opaque type O [: T] = T' *)
    | ImportTypeofKind (* import typeof *)
    | ImportClassKind (* import type { SomeClass } from ... *)
    | InstanceKind
end =
  TypeTerm

and UnionEnum : sig
  type t =
    | Str of string
    | Num of TypeTerm.number_literal
    | Bool of bool
    | Void
    | Null

  val compare : t -> t -> int

  type star =
    | One of t
    | Many of UnionEnumSet.t
end = struct
  type t =
    | Str of string
    | Num of TypeTerm.number_literal
    | Bool of bool
    | Void
    | Null

  let compare = Pervasives.compare

  type star =
    | One of t
    | Many of UnionEnumSet.t
end

and UnionEnumSet : (Set.S with type elt = UnionEnum.t) = Set.Make (UnionEnum)

and Property : sig
  type t = TypeTerm.property

  val polarity : t -> Polarity.t

  val read_t : t -> TypeTerm.t option

  val write_t : ?ctx:TypeTerm.write_ctx -> t -> TypeTerm.t option

  val read_loc : t -> ALoc.t option

  val write_loc : t -> ALoc.t option

  val first_loc : t -> ALoc.t option

  val iter_t : (TypeTerm.t -> unit) -> t -> unit

  val fold_t : ('a -> TypeTerm.t -> 'a) -> 'a -> t -> 'a

  val map_t : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val ident_map_t : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val forall_t : (TypeTerm.t -> bool) -> t -> bool

  val assert_field : t -> TypeTerm.t
end = struct
  open TypeTerm

  type t = property

  let polarity = function
    | Field (_, _, polarity) -> polarity
    | Get _ -> Positive
    | Set _ -> Negative
    | GetSet _ -> Neutral
    | Method _ -> Positive

  let read_t = function
    | Field (_, t, polarity) ->
      if Polarity.compat (polarity, Positive) then
        Some t
      else
        None
    | Get (_, t) -> Some t
    | Set _ -> None
    | GetSet (_, t, _, _) -> Some t
    | Method (_, t) -> Some t

  let write_t ?(ctx = Normal) = function
    | Field (_, t, _) when ctx = ThisInCtor -> Some t
    | Field (_, t, polarity) ->
      if Polarity.compat (polarity, Negative) then
        Some t
      else
        None
    | Get _ -> None
    | Set (_, t) -> Some t
    | GetSet (_, _, _, t) -> Some t
    | Method _ -> None

  let read_loc = function
    | Field (loc, _, _)
    | Get (loc, _)
    | GetSet (loc, _, _, _)
    | Method (loc, _) ->
      loc
    | Set _ -> None

  let write_loc = function
    | Field (loc, _, _)
    | Set (loc, _)
    | GetSet (_, _, loc, _) ->
      loc
    | Method _
    | Get _ ->
      None

  let first_loc = function
    | Field (loc, _, _)
    | Get (loc, _)
    | Set (loc, _)
    | Method (loc, _) ->
      loc
    | GetSet (loc1_opt, _, loc2_opt, _) ->
      (match (loc1_opt, loc2_opt) with
      | (None, None) -> None
      | (Some loc, None)
      | (None, Some loc) ->
        Some loc
      | (Some loc1, Some loc2) ->
        let k = ALoc.compare loc1 loc2 in
        let loc =
          if k <= 0 then
            loc1
          else
            loc2
        in
        Some loc)

  let iter_t f = function
    | Field (_, t, _)
    | Get (_, t)
    | Set (_, t)
    | Method (_, t) ->
      f t
    | GetSet (_, t1, _, t2) ->
      f t1;
      f t2

  let fold_t f acc = function
    | Field (_, t, _)
    | Get (_, t)
    | Set (_, t)
    | Method (_, t) ->
      f acc t
    | GetSet (_, t1, _, t2) -> f (f acc t1) t2

  let map_t f = function
    | Field (loc, t, polarity) -> Field (loc, f t, polarity)
    | Get (loc, t) -> Get (loc, f t)
    | Set (loc, t) -> Set (loc, f t)
    | GetSet (loc1, t1, loc2, t2) -> GetSet (loc1, f t1, loc2, f t2)
    | Method (loc, t) -> Method (loc, f t)

  let ident_map_t f p =
    match p with
    | Field (loc, t, polarity) ->
      let t_ = f t in
      if t_ == t then
        p
      else
        Field (loc, t_, polarity)
    | Get (loc, t) ->
      let t_ = f t in
      if t_ == t then
        p
      else
        Get (loc, t_)
    | Set (loc, t) ->
      let t_ = f t in
      if t_ == t then
        p
      else
        Set (loc, t_)
    | GetSet (loc1, t1, loc2, t2) ->
      let t1_ = f t1 in
      let t2_ = f t2 in
      if t1_ == t1 && t2_ == t2 then
        p
      else
        GetSet (loc1, t1_, loc2, t2_)
    | Method (loc, t) ->
      let t_ = f t in
      if t_ == t then
        p
      else
        Method (loc, t_)

  let forall_t f = fold_t (fun acc t -> acc && f t) true

  let assert_field = function
    | Field (_, t, _) -> t
    | _ -> assert_false "Unexpected field type"
end

and Properties : sig
  type t = Property.t SMap.t

  type id

  module Map : MyMap.S with type key = id

  module Set : Set.S with type elt = id

  type map = t Map.t

  val add_field : string -> Polarity.t -> ALoc.t option -> TypeTerm.t -> t -> t

  val add_getter : string -> ALoc.t option -> TypeTerm.t -> t -> t

  val add_setter : string -> ALoc.t option -> TypeTerm.t -> t -> t

  val add_method : string -> ALoc.t option -> TypeTerm.t -> t -> t

  val generate_id : unit -> id

  val id_of_int : int -> id

  val id_as_int : id -> int option

  val id_of_aloc : ALoc.t -> id

  val fake_id : id

  val string_of_id : id -> string

  val extract_named_exports : t -> Exports.t

  val iter_t : (TypeTerm.t -> unit) -> t -> unit

  val map_t : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val map_fields : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val mapi_fields : (string -> TypeTerm.t -> TypeTerm.t) -> t -> t
end = struct
  open TypeTerm

  type t = Property.t SMap.t

  (* In order to minimize the frequency with which we unnecessarily compare
     equivalent objects, we assign all objects created at the top level of a
     source program an id of their location instead of an int. This way, if we
     see the object twice between the merge and check phases, we still hit
     the object to object fast path when checking *)
  type id =
    | Source of ALoc.t
    | Generated of int

  let compare_id id1 id2 =
    match (id1, id2) with
    | (Source loc1, Source loc2) -> ALoc.quick_compare loc1 loc2
    | (Generated i1, Generated i2) -> i1 - i2
    | (Source _, Generated _) -> -1
    | (Generated _, Source _) -> 1

  module Map : MyMap.S with type key = id = MyMap.Make (struct
    type t = id

    let compare = compare_id
  end)

  module Set : Set.S with type elt = id = Set.Make (struct
    type t = id

    let compare = compare_id
  end)

  type map = t Map.t

  let add_field x polarity loc t = SMap.add x (Field (loc, t, polarity))

  let add_getter x loc get_t map =
    let p =
      match SMap.get x map with
      | Some (Set (set_loc, set_t)) -> GetSet (loc, get_t, set_loc, set_t)
      | _ -> Get (loc, get_t)
    in
    SMap.add x p map

  let add_setter x loc set_t map =
    let p =
      match SMap.get x map with
      | Some (Get (get_loc, get_t)) -> GetSet (get_loc, get_t, loc, set_t)
      | _ -> Set (loc, set_t)
    in
    SMap.add x p map

  let add_method x loc t = SMap.add x (Method (loc, t))

  let id_of_int i = Generated i

  let id_as_int = function
    | Generated i -> Some i
    | _ -> None

  let generate_id = Reason.mk_id %> id_of_int

  let id_of_aloc loc = Source loc

  let fake_id = Generated 0

  let string_of_id = function
    | Generated id -> string_of_int id
    | Source id -> string_of_aloc id

  let extract_named_exports pmap =
    SMap.fold
      (fun x p tmap ->
        match Property.read_t p with
        | Some t -> SMap.add x (Property.read_loc p, t) tmap
        | None -> tmap)
      pmap
      SMap.empty

  let iter_t f = SMap.iter (fun _ -> Property.iter_t f)

  let map_t f = SMap.map (Property.map_t f)

  let map_fields f =
    SMap.map (function
        | Field (loc, t, polarity) -> Field (loc, f t, polarity)
        | p -> p)

  let mapi_fields f =
    SMap.mapi (fun k ->
      function
      | Field (loc, t, polarity) -> Field (loc, f k t, polarity)
      | p -> p)
end

and Exports : sig
  type t = (ALoc.t option * TypeTerm.t) SMap.t

  type id

  module Map : MyMap.S with type key = id

  type map = t Map.t

  val mk_id : unit -> id

  val string_of_id : id -> string
end = struct
  type t = (ALoc.t option * TypeTerm.t) SMap.t

  type id = int

  module Map : MyMap.S with type key = id = MyMap.Make (struct
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

  val make : TypeTerm.t -> TypeTerm.t -> TypeTerm.t list -> t
  (** build a rep from list of members *)

  val specialized_reason : reason -> t -> reason
  (** replace reason with specialized desc, if any *)

  val members : t -> TypeTerm.t list
  (** members in declaration order *)

  val members_nel : t -> TypeTerm.t * TypeTerm.t Nel.t

  val cons : TypeTerm.t -> t -> t

  val rev_append : t -> t -> t

  val ident_map : (TypeTerm.t -> TypeTerm.t) -> t -> t
  (** map rep r to rep r' along type mapping f. if nothing would be changed,
      returns the physically-identical rep. *)

  val optimize :
    t ->
    flatten:(TypeTerm.t list -> TypeTerm.t list) ->
    find_resolved:(TypeTerm.t -> TypeTerm.t option) ->
    find_props:(Properties.id -> TypeTerm.property SMap.t) ->
    unit

  val is_optimized_finally : t -> bool

  (** quick membership tests for enums and disjoint unions *)
  type quick_mem_result =
    | Yes
    | No
    | Conditional of TypeTerm.t
    | Unknown

  val join_quick_mem_results : quick_mem_result * quick_mem_result -> quick_mem_result

  val quick_mem_enum : bool -> TypeTerm.t -> t -> quick_mem_result

  val quick_mem_disjoint_union :
    find_resolved:(TypeTerm.t -> TypeTerm.t option) ->
    find_props:(Properties.id -> TypeTerm.property SMap.t) ->
    bool ->
    TypeTerm.t ->
    t ->
    quick_mem_result

  val check_enum : t -> UnionEnumSet.t option

  val string_of_specialization : t -> string

  val contains_only_flattened_types : TypeTerm.t list -> bool
end = struct
  (* canonicalize a type w.r.t. enum membership *)
  let canon =
    TypeTerm.(
      function
      | DefT (_, _, SingletonStrT lit)
      | DefT (_, _, StrT (Literal (_, lit))) ->
        Some (UnionEnum.Str lit)
      | DefT (_, _, SingletonNumT lit)
      | DefT (_, _, NumT (Literal (_, lit))) ->
        Some (UnionEnum.Num lit)
      | DefT (_, _, SingletonBoolT lit)
      | DefT (_, _, BoolT (Some lit)) ->
        Some (UnionEnum.Bool lit)
      | DefT (_, _, VoidT) -> Some UnionEnum.Void
      | DefT (_, _, NullT) -> Some UnionEnum.Null
      | _ -> None)

  let is_base =
    TypeTerm.(
      function
      | DefT (_, _, SingletonStrT _)
      | DefT (_, _, SingletonNumT _)
      | DefT (_, _, SingletonBoolT _)
      | DefT (_, _, VoidT)
      | DefT (_, _, NullT) ->
        true
      | _ -> false)

  (* disjoint unions are stored as singleton type maps *)
  module UnionEnumMap = MyMap.Make (UnionEnum)

  type finally_optimized_rep =
    | UnionEnum of UnionEnumSet.t
    | PartiallyOptimizedUnionEnum of UnionEnumSet.t
    | DisjointUnion of TypeTerm.t UnionEnumMap.t SMap.t
    | PartiallyOptimizedDisjointUnion of TypeTerm.t UnionEnumMap.t SMap.t
    | Empty
    | Singleton of TypeTerm.t
    | Unoptimized

  type t = TypeTerm.t * TypeTerm.t * TypeTerm.t list * finally_optimized_rep option ref
  (** union rep is:
      - list of members in declaration order, with at least 2 elements
      - if union is an enum (set of singletons over a common base)
        then Some (base, set)
        (additional specializations probably to come)
   *)

  (** given a list of members, build a rep.
      specialized reps are used on compatible type lists *)
  let make =
    let rec mk_enum tset = function
      | [] -> Some tset
      | t :: ts ->
        begin
          match canon t with
          | Some tcanon when is_base t -> mk_enum (UnionEnumSet.add tcanon tset) ts
          | _ -> None
        end
    in
    fun t0 t1 ts ->
      let enum =
        Option.(mk_enum UnionEnumSet.empty (t0 :: t1 :: ts) >>| (fun tset -> UnionEnum tset))
      in
      (t0, t1, ts, ref enum)

  let members (t0, t1, ts, _) = t0 :: t1 :: ts

  let members_nel (t0, t1, ts, _) = (t0, (t1, ts))

  let cons t0 (t1, t2, ts, _) = make t0 t1 (t2 :: ts)

  let rev_append rep1 rep2 =
    match List.rev_append (members rep1) (members rep2) with
    | t0 :: t1 :: ts -> make t0 t1 ts
    | _ -> failwith "impossible"

  let ident_map f ((t0, t1, ts, _) as rep) =
    let t0_ = f t0 in
    let t1_ = f t1 in
    let ts_ = ListUtils.ident_map f ts in
    let changed = t0_ != t0 || t1_ != t1 || ts_ != ts in
    if changed then
      make t0_ t1_ ts_
    else
      rep

  let specialized_reason r (_, _, _, specialization) =
    match !specialization with
    | Some Empty -> replace_desc_reason REmpty r
    | Some (Singleton t) -> TypeUtil.reason_of_t t
    | Some (UnionEnum _) -> replace_desc_reason RUnionEnum r
    | _ -> r

  (********** Optimizations **********)

  let is_optimized_finally (_, _, _, specialization) = !specialization <> None

  (* Private helper, must be called after full resolution. Ideally would be returned as a bit by
     TypeTerm.union_flatten, and kept in sync there. *)
  let contains_only_flattened_types =
    List.for_all
      TypeTerm.(
        function
        (* the only unresolved tvars at this point are those that instantiate polymorphic types *)
        | OpenT _
        (* some types may not be evaluated yet; TODO *)
        
        | EvalT _
        | TypeAppT _
        | KeysT _
        | IntersectionT _
        (* other types might wrap parts that are accessible directly *)
        
        | OpaqueT _
        | DefT (_, _, InstanceT _)
        | DefT (_, _, PolyT _) ->
          false
        | _ -> true)

  let enum_optimize =
    let split_enum =
      List.fold_left
        (fun (tset, partial) t ->
          match canon t with
          | Some tcanon when is_base t -> (UnionEnumSet.add tcanon tset, partial)
          | _ -> (tset, true))
        (UnionEnumSet.empty, false)
    in
    function
    | [] -> Empty
    | [t] -> Singleton t
    | ts ->
      let (tset, partial) = split_enum ts in
      if partial then
        if UnionEnumSet.is_empty tset then
          Unoptimized
        else
          PartiallyOptimizedUnionEnum tset
      else
        UnionEnum tset

  let canon_prop find_resolved p = Option.(Property.read_t p >>= find_resolved >>= canon)

  let base_prop find_resolved p =
    match Option.(Property.read_t p >>= find_resolved) with
    | Some t when is_base t -> canon t
    | _ -> None

  let props_of find_props t =
    TypeTerm.(
      match t with
      | DefT (_, _, ObjT { props_tmap; _ })
      | ExactT (_, DefT (_, _, ObjT { props_tmap; _ })) ->
        Some (find_props props_tmap)
      | _ -> None)

  let disjoint_union_optimize =
    let base_props_of find_resolved find_props t =
      Option.(
        props_of find_props t
        >>| fun prop_map ->
        SMap.fold
          (fun key p acc ->
            match base_prop find_resolved p with
            | Some enum -> SMap.add key (enum, t) acc
            | _ -> acc)
          prop_map
          SMap.empty)
    in
    let split_disjoint_union find_resolved find_props ts =
      List.fold_left
        (fun (candidates, partial) t ->
          match base_props_of find_resolved find_props t with
          | None -> (candidates, true)
          | Some base_props -> (base_props :: candidates, partial))
        ([], false)
        ts
    in
    let unique_values =
      let rec unique_values idx = function
        | [] -> Some idx
        | (enum, t) :: values ->
          begin
            match UnionEnumMap.get enum idx with
            | None -> unique_values (UnionEnumMap.add enum t idx) values
            | Some t' ->
              if TypeUtil.reasonless_eq t t' then
                unique_values idx values
              else
                None
          end
      in
      (fun values -> unique_values UnionEnumMap.empty values)
    in
    let unique idx =
      SMap.fold
        (fun key values acc ->
          match unique_values values with
          | None -> acc
          | Some idx -> SMap.add key idx acc)
        idx
        SMap.empty
    in
    let index candidates =
      match candidates with
      | [] -> SMap.empty
      | base_props :: candidates ->
        (* Compute the intersection of properties of objects that have singleton types *)
        let init = SMap.map (fun enum_t -> [enum_t]) base_props in
        let idx =
          List.fold_left
            (fun acc base_props ->
              SMap.merge
                (fun _key enum_t_opt values_opt ->
                  Option.(
                    both enum_t_opt values_opt >>| (fun (enum_t, values) -> List.cons enum_t values)))
                base_props
                acc)
            init
            candidates
        in
        (* Ensure that enums map to unique types *)
        unique idx
    in
    fun ~find_resolved ~find_props -> function
      | [] -> Empty
      | [t] -> Singleton t
      | ts ->
        let (candidates, partial) = split_disjoint_union find_resolved find_props ts in
        let map = index candidates in
        if SMap.is_empty map then
          Unoptimized
        else if partial then
          PartiallyOptimizedDisjointUnion map
        else
          DisjointUnion map

  let optimize rep ~flatten ~find_resolved ~find_props =
    let ts = flatten (members rep) in
    if contains_only_flattened_types ts then
      let opt = enum_optimize ts in
      let opt =
        match opt with
        | Unoptimized -> disjoint_union_optimize ~find_resolved ~find_props ts
        | _ -> opt
      in
      let (_, _, _, specialization) = rep in
      specialization := Some opt

  (********** Quick matching **********)

  type quick_mem_result =
    | Yes
    | No
    | Conditional of TypeTerm.t
    | Unknown

  let join_quick_mem_results = function
    | (Yes, _)
    | (_, Yes) ->
      Yes
    | (Unknown, _)
    | (_, Unknown) ->
      Unknown
    | (Conditional _, _)
    | (_, Conditional _) ->
      Unknown (* TODO *)
    | (No, No) -> No

  (* assume we know that l is a canonizable type *)
  let quick_mem_enum trust_checked l (_t0, _t1, _ts, specialization) =
    match canon l with
    | Some tcanon ->
      begin
        match !specialization with
        | None -> Unknown
        | Some Unoptimized -> Unknown
        | Some Empty -> No
        | Some (Singleton t) ->
          if TypeUtil.quick_subtype trust_checked l t then
            Yes
          else
            Conditional t
        | Some (DisjointUnion _) -> No
        | Some (PartiallyOptimizedDisjointUnion _) -> Unknown
        | Some (UnionEnum tset) ->
          if UnionEnumSet.mem tcanon tset then
            Yes
          else
            No
        | Some (PartiallyOptimizedUnionEnum tset) ->
          if UnionEnumSet.mem tcanon tset then
            Yes
          else
            Unknown
      end
    | None -> failwith "quick_mem_enum is defined only for canonizable type"

  let lookup_disjoint_union find_resolved prop_map ~partial map =
    SMap.fold
      (fun key idx acc ->
        if acc <> Unknown then
          acc
        else
          match SMap.get key prop_map with
          | Some p ->
            begin
              match canon_prop find_resolved p with
              | Some enum ->
                begin
                  match UnionEnumMap.get enum idx with
                  | Some t' -> Conditional t'
                  | None ->
                    if partial then
                      Unknown
                    else
                      No
                end
              | None -> Unknown
            end
          | None ->
            if partial then
              Unknown
            else
              No)
      map
      Unknown

  (* we know that l is an object type or exact object type *)
  let quick_mem_disjoint_union
      ~find_resolved ~find_props trust_checked l (_t0, _t1, _ts, specialization) =
    match props_of find_props l with
    | Some prop_map ->
      begin
        match !specialization with
        | None -> Unknown
        | Some Unoptimized -> Unknown
        | Some Empty -> No
        | Some (Singleton t) ->
          if TypeUtil.quick_subtype trust_checked l t then
            Yes
          else
            Conditional t
        | Some (DisjointUnion map) ->
          lookup_disjoint_union find_resolved prop_map ~partial:false map
        | Some (PartiallyOptimizedDisjointUnion map) ->
          lookup_disjoint_union find_resolved prop_map ~partial:true map
        | Some (UnionEnum _) -> No
        | Some (PartiallyOptimizedUnionEnum _) -> Unknown
      end
    | _ -> failwith "quick_mem_disjoint_union is defined only on object / exact object types"

  let check_enum (_, _, _, specialization) =
    match !specialization with
    | Some (UnionEnum enums) -> Some enums
    | _ -> None

  let string_of_specialization (_, _, _, spec) =
    match !spec with
    | Some (UnionEnum _) -> "Enum"
    | Some Empty -> "Empty"
    | Some Unoptimized -> "Unoptimized"
    | Some (Singleton _) -> "Singleton"
    | Some (PartiallyOptimizedDisjointUnion _) -> "Partially Optimized Disjoint Union"
    | Some (DisjointUnion _) -> "Disjoint Union"
    | Some (PartiallyOptimizedUnionEnum _) -> "Partially Optimized Enum"
    | None -> "No Specialization"
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

  val make : TypeTerm.t -> TypeTerm.t -> TypeTerm.t list -> t
  (** build rep from list of members *)

  val members : t -> TypeTerm.t list
  (** member list in declaration order *)

  val members_nel : t -> TypeTerm.t * TypeTerm.t Nel.t

  val map : (TypeTerm.t -> TypeTerm.t) -> t -> t
  (** map rep r to rep r' along type mapping f. drops history *)

  val append : TypeTerm.t list -> t -> t

  val ident_map : (TypeTerm.t -> TypeTerm.t) -> t -> t
  (** map rep r to rep r' along type mapping f. drops history. if nothing would
      be changed, returns the physically-identical rep. *)
end = struct
  type t = TypeTerm.t * TypeTerm.t * TypeTerm.t list
  (** intersection rep is:
      - member list in declaration order
    *)

  let make t0 t1 ts = (t0, t1, ts)

  let members (t0, t1, ts) = t0 :: t1 :: ts

  let members_nel (t0, t1, ts) = (t0, (t1, ts))

  let map f (t0, t1, ts) = make (f t0) (f t1) (Base.List.map ~f ts)

  let append ts2 (t0, t1, ts1) = make t0 t1 (ts1 @ ts2)

  let ident_map f ((t0, t1, ts) as rep) =
    let t0_ = f t0 in
    let t1_ = f t1 in
    let changed = t0_ != t0 || t1_ != t1 in
    let (rev_ts, changed) =
      List.fold_left
        (fun (rev_ts, changed) member ->
          let member_ = f member in
          (member_ :: rev_ts, changed || member_ != member))
        ([], changed)
        ts
    in
    if changed then
      make t0_ t1_ (List.rev rev_ts)
    else
      rep
end

(* The typechecking algorithm often needs to maintain sets of types, or more
   generally, maps of types (for logging we need to associate some provenanced
   information to types).
   Type terms may also contain internal sets or maps.
*)
and TypeSet : (Set.S with type elt = TypeTerm.t) = Set.Make (struct
  type elt = TypeTerm.t

  type t = elt

  let compare = Pervasives.compare
end)

and TypeMap : (MyMap.S with type key = TypeTerm.t) = MyMap.Make (struct
  type key = TypeTerm.t

  type t = key

  let compare = Pervasives.compare
end)

and UseTypeSet : (Set.S with type elt = TypeTerm.use_t) = Set.Make (struct
  type elt = TypeTerm.use_t

  type t = elt

  let compare = Pervasives.compare
end)

and UseTypeMap : (MyMap.S with type key = TypeTerm.use_t) = MyMap.Make (struct
  type key = TypeTerm.use_t

  type t = key

  let compare = Pervasives.compare
end)

and Object : sig
  type resolve_tool =
    (* Each part of a spread must be resolved in order to compute the result *)
    | Resolve of resolve
    (* In order to resolve an InstanceT, all supers must also be resolved to
       collect class properties, which are own. *)
    | Super of slice * resolve

  and resolve =
    | Next
    (* Resolve each element of a union or intersection *)
    | List0 of TypeTerm.t Nel.t * join
    | List of TypeTerm.t list * resolved Nel.t * join

  and join' =
    | And
    | Or

  (* This location is that of the entire intersection/union, not just the location of the &/| symbol *)
  and join = ALoc.t * join'

  (* A union type resolves to a resolved spread with more than one element *)
  and resolved = slice Nel.t

  and slice = {
    reason: reason;
    props: props;
    dict: dict;
    flags: TypeTerm.flags;
  }

  and props = prop SMap.t

  and prop = TypeTerm.t * bool

  (* own *)
  and dict = TypeTerm.dicttype option

  module Spread : sig
    (* This is the type we feed into SpreadType to be processed by object_kit. It's different
     * than slice because object_kit processes the properties in ways that do not need to
     * be exposed to other files. *)
    type operand_slice = {
      reason: reason;
      prop_map: Properties.t;
      dict: dict;
    }

    type operand =
      | Slice of operand_slice
      | Type of TypeTerm.t

    type acc_element =
      | ResolvedSlice of resolved
      | InlineSlice of operand_slice

    type state = {
      todo_rev: operand list;
      acc: acc_element list;
      spread_id: int;
      union_reason: reason option;
      curr_resolve_idx: int;
    }

    type target =
      (* When spreading values, the result is exact if all of the input types are
         also exact. If any input type is inexact, the output is inexact. *)
      | Value of { make_seal: TypeTerm.sealtype }
      (* It's more flexible to allow annotations to specify whether they should be
         exact or not. If the spread type is annotated to be exact, any inexact
         input types will cause a type error. *)
      | Annot of { make_exact: bool }
  end

  module Rest : sig
    type state =
      | One of TypeTerm.t
      | Done of resolved

    type merge_mode =
      | Sound
      | IgnoreExactAndOwn
      | ReactConfigMerge of Polarity.t
  end

  module ReactConfig : sig
    type state =
      | Config of {
          defaults: TypeTerm.t option;
          children: TypeTerm.t option;
        }
      | Defaults of {
          config: resolved;
          children: TypeTerm.t option;
        }
  end

  type tool =
    | ReadOnly
    | Spread of Spread.target * Spread.state
    | Rest of Rest.merge_mode * Rest.state
    | ReactConfig of ReactConfig.state
    | ObjectRep
    | ObjectWiden of ident
end =
  Object

and React : sig
  module PropType : sig
    type t =
      | Primitive of (is_required * TypeTerm.t)
      | Complex of complex

    and is_required = bool

    and complex =
      | ArrayOf
      | InstanceOf
      | ObjectOf
      | OneOf
      | OneOfType
      | Shape
  end

  type resolved_object = reason * Properties.t * TypeTerm.dicttype option * TypeTerm.flags

  type resolve_object =
    | ResolveObject
    | ResolveDict of (TypeTerm.dicttype * Properties.t * resolved_object)
    | ResolveProp of (string * Properties.t * resolved_object)

  type resolve_array =
    | ResolveArray
    | ResolveElem of TypeTerm.t list * TypeTerm.t list

  module SimplifyPropType : sig
    type tool =
      | ArrayOf
      | InstanceOf
      | ObjectOf
      | OneOf of resolve_array
      | OneOfType of resolve_array
      | Shape of resolve_object
  end

  module CreateClass : sig
    (* In order to derive a component instance type from a specification, we
     * need to resolve the spec object itself and a number of its fields. We do
     * this in order, accumulating the resolved information until we have enough
     * to compute the instance type. *)
    type tool =
      | Spec of stack_tail
      | Mixins of stack
      | Statics of stack
      | PropTypes of stack * resolve_object
      | DefaultProps of TypeTerm.t list * default_props option
      | InitialState of TypeTerm.t list * initial_state option

    (* When we encounter mixins, we push the current spec's props into a stack,
     * then resolve each mixin in turn. This is recursive, as mixins can have
     * mixins. *)
    and stack = stack_head * stack_tail

    and stack_head = resolved_object * spec

    and stack_tail = (stack_head * TypeTerm.t list * spec maybe_known list) list

    and spec = {
      obj: resolved_object;
      statics: statics option;
      prop_types: prop_types option;
      get_default_props: TypeTerm.t list;
      get_initial_state: TypeTerm.t list;
      unknown_mixins: reason list;
    }

    and statics = resolved_object maybe_known

    and prop_types = resolved_object maybe_known

    and default_props = resolved_object maybe_known

    and initial_state = resolved_object or_null maybe_known

    and 'a maybe_known =
      | Known of 'a
      | Unknown of reason

    and 'a or_null =
      | NotNull of 'a
      | Null of reason

    (* Components have some recursive dependencies. For example, the instance
     * type depends on the return value of its methods, but those methods also
     * depend on `this`. We use these tvars to "tie the knot" in those cases. *)
    type knot = {
      this: TypeTerm.t;
      static: TypeTerm.t;
      state_t: TypeTerm.t;
      default_t: TypeTerm.t;
    }
  end

  type tool =
    | CreateElement0 of bool * TypeTerm.t * (TypeTerm.t list * TypeTerm.t option) * TypeTerm.t_out
    | CreateElement of
        bool * TypeTerm.t * TypeTerm.t * (TypeTerm.t list * TypeTerm.t option) * TypeTerm.t_out
    | ConfigCheck of TypeTerm.t
    | GetProps of TypeTerm.t_out
    | GetConfig of TypeTerm.t_out
    | GetConfigType of TypeTerm.t * TypeTerm.t_out
    | GetRef of TypeTerm.t_out
    | SimplifyPropType of SimplifyPropType.tool * TypeTerm.t_out
    | CreateClass of CreateClass.tool * CreateClass.knot * TypeTerm.t_out
end =
  React

and TypeUtil : sig
  val reason_of_t : TypeTerm.t -> reason

  val reason_of_defer_use_t : TypeTerm.defer_use_t -> reason

  val reason_of_use_t : TypeTerm.use_t -> reason

  val reason_of_t_add_id : TypeTerm.t -> reason

  val reason_of_use_t_add_id : TypeTerm.use_t -> reason

  val desc_of_t : TypeTerm.t -> reason_desc

  val loc_of_t : TypeTerm.t -> ALoc.t

  val def_loc_of_t : TypeTerm.t -> ALoc.t

  val mod_reason_of_t : (reason -> reason) -> TypeTerm.t -> TypeTerm.t

  val mod_reason_of_defer_use_t :
    (reason -> reason) -> TypeTerm.defer_use_t -> TypeTerm.defer_use_t

  val mod_reason_of_use_t : (reason -> reason) -> TypeTerm.use_t -> TypeTerm.use_t

  val mod_reason_of_opt_use_t : (reason -> reason) -> TypeTerm.opt_use_t -> TypeTerm.opt_use_t

  val use_op_of_use_t : TypeTerm.use_t -> TypeTerm.use_op option

  val mod_use_op_of_use_t :
    (TypeTerm.use_op -> TypeTerm.use_op) -> TypeTerm.use_t -> TypeTerm.use_t

  val mod_root_of_use_op :
    (TypeTerm.root_use_op -> TypeTerm.root_use_op) -> TypeTerm.use_op -> TypeTerm.use_op

  val mod_loc_of_virtual_use_op :
    ('a -> 'b) -> 'a TypeTerm.virtual_use_op -> 'b TypeTerm.virtual_use_op

  val reasonless_compare : TypeTerm.t -> TypeTerm.t -> int

  val reasonless_eq : TypeTerm.t -> TypeTerm.t -> bool

  val literal_eq : string -> string TypeTerm.literal -> bool

  val number_literal_eq :
    TypeTerm.number_literal -> TypeTerm.number_literal TypeTerm.literal -> bool

  val boolean_literal_eq : bool -> bool option -> bool

  val quick_subtype : bool -> TypeTerm.t -> TypeTerm.t -> bool
end = struct
  open TypeTerm

  (* Usually types carry enough information about the "reason" for their
     existence (e.g., position in code, introduction/elimination rules in
     the type system), so printing the reason provides a good idea of what the
     type means to the programmer. *)

  let rec reason_of_t = function
    | OpenT (reason, _) -> reason
    | AnnotT (reason, _, _) -> reason
    | MergedT (reason, _) -> reason
    | BoundT (reason, _, _) -> reason
    | InternalT (ChoiceKitT (reason, _)) -> reason
    | TypeDestructorTriggerT (_, reason, _, _, _) -> reason
    | CustomFunT (reason, _) -> reason
    | DefT (reason, _, _) -> reason
    | EvalT (_, defer_use_t, _) -> reason_of_defer_use_t defer_use_t
    | ExactT (reason, _) -> reason
    | ExistsT reason -> reason
    | InternalT (ExtendsT (reason, _, _)) -> reason
    | FunProtoT reason -> reason
    | FunProtoApplyT reason -> reason
    | FunProtoBindT reason -> reason
    | FunProtoCallT reason -> reason
    | KeysT (reason, _) -> reason
    | ModuleT (reason, _, _) -> reason
    | NullProtoT reason -> reason
    | ObjProtoT reason -> reason
    | MatchingPropT (reason, _, _) -> reason
    | OpaqueT (reason, _) -> reason
    | OpenPredT { reason; m_pos = _; m_neg = _; base_t = _ } -> reason
    | InternalT (OptionalChainVoidT reason) -> reason
    | ReposT (reason, _) -> reason
    | InternalT (ReposUpperT (reason, _)) -> reason (* HUH? cf. mod_reason below *)
    | ShapeT t -> reason_of_t t
    | ThisClassT (reason, _) -> reason
    | ThisTypeAppT (reason, _, _, _) -> reason
    | TypeAppT (reason, _, _, _) -> reason
    | AnyT (reason, _) -> reason
    | UnionT (reason, _) -> reason
    | IntersectionT (reason, _) -> reason
    | MaybeT (reason, _) -> reason
    | OptionalT { reason; type_ = _; use_desc = _ } -> reason

  and reason_of_defer_use_t = function
    | LatentPredT (reason, _)
    | TypeDestructorT (_, reason, _) ->
      reason

  and reason_of_use_t = function
    | UseT (_, t) -> reason_of_t t
    | AdderT (_, reason, _, _, _) -> reason
    | AndT (reason, _, _) -> reason
    | ArrRestT (_, reason, _, _) -> reason
    | AssertArithmeticOperandT reason -> reason
    | AssertBinaryInLHST reason -> reason
    | AssertBinaryInRHST reason -> reason
    | AssertForInRHST reason -> reason
    | AssertImportIsValueT (reason, _) -> reason
    | BecomeT (reason, _) -> reason
    | BindT (_, reason, _, _) -> reason
    | CallElemT (reason, _, _, _) -> reason
    | CallLatentPredT (reason, _, _, _, _) -> reason
    | CallOpenPredT (reason, _, _, _, _) -> reason
    | CallT (_, reason, _) -> reason
    | ChoiceKitUseT (reason, _) -> reason
    | CJSExtractNamedExportsT (reason, _, _) -> reason
    | CJSRequireT (reason, _, _) -> reason
    | ComparatorT (reason, _, _) -> reason
    | ConstructorT (_, reason, _, _, _) -> reason
    | CopyNamedExportsT (reason, _, _) -> reason
    | CopyTypeExportsT (reason, _, _) -> reason
    | DebugPrintT reason -> reason
    | DebugSleepT reason -> reason
    | ElemT (_, reason, _, _) -> reason
    | EqT (reason, _, _) -> reason
    | ExportNamedT (reason, _, _, _) -> reason
    | ExportTypeT (reason, _, _, _) -> reason
    | AssertExportIsTypeT (reason, _, _) -> reason
    | ExtendsUseT (_, reason, _, _, _) -> reason
    | GetElemT (_, reason, _, _) -> reason
    | GetKeysT (reason, _) -> reason
    | GetValuesT (reason, _) -> reason
    | GetPropT (_, reason, _, _) -> reason
    | GetPrivatePropT (_, reason, _, _, _, _) -> reason
    | GetProtoT (reason, _) -> reason
    | GetStaticsT (reason, _) -> reason
    | GuardT (_, _, t) -> reason_of_t t
    | HasOwnPropT (_, reason, _) -> reason
    | IdxUnMaybeifyT (reason, _) -> reason
    | IdxUnwrap (reason, _) -> reason
    | ImplementsT (_, t) -> reason_of_t t
    | ImportDefaultT (reason, _, _, _, _) -> reason
    | ImportModuleNsT (reason, _, _) -> reason
    | ImportNamedT (reason, _, _, _, _, _) -> reason
    | ImportTypeofT (reason, _, _) -> reason
    | ImportTypeT (reason, _, _) -> reason
    | IntersectionPreprocessKitT (reason, _) -> reason
    | InvariantT reason -> reason
    | LookupT (reason, _, _, _, _) -> reason
    | MakeExactT (reason, _) -> reason
    | MapTypeT (_, reason, _, _) -> reason
    | MethodT (_, reason, _, _, _, _) -> reason
    | MixinT (reason, _) -> reason
    | NotT (reason, _) -> reason
    | NullishCoalesceT (reason, _, _) -> reason
    | ObjAssignToT (_, reason, _, _, _) -> reason
    | ObjAssignFromT (_, reason, _, _, _) -> reason
    | ObjFreezeT (reason, _) -> reason
    | ObjRestT (reason, _, _) -> reason
    | ObjSealT (reason, _) -> reason
    | ObjTestProtoT (reason, _) -> reason
    | ObjTestT (reason, _, _) -> reason
    | OptionalChainT (reason, _, _, _) -> reason
    | OrT (reason, _, _) -> reason
    | PredicateT (_, t) -> reason_of_t t
    | ReactKitT (_, reason, _) -> reason
    | RefineT (reason, _, _) -> reason
    | ReposLowerT (reason, _, _) -> reason
    | ReposUseT (reason, _, _, _) -> reason
    | ResolveSpreadT (_, reason, _) -> reason
    | SentinelPropTestT (_, _, _, _, _, result) -> reason_of_t result
    | SetElemT (_, reason, _, _, _, _) -> reason
    | SetPropT (_, reason, _, _, _, _, _) -> reason
    | SetPrivatePropT (_, reason, _, _, _, _, _, _) -> reason
    | SetProtoT (reason, _) -> reason
    | SpecializeT (_, _, reason, _, _, _) -> reason
    | ObjKitT (_, reason, _, _, _) -> reason
    | ModuleExportsAssignT (reason, _, _) -> reason
    | SubstOnPredT (reason, _, _) -> reason
    | SuperT (_, reason, _) -> reason
    | TestPropT (reason, _, _, _) -> reason
    | ThisSpecializeT (reason, _, _) -> reason
    | ToStringT (reason, _) -> reason
    | UnaryMinusT (reason, _) -> reason
    | UnifyT (_, t) -> reason_of_t t
    | VarianceCheckT (reason, _, _) -> reason
    | TypeAppVarianceCheckT (_, reason, _, _) -> reason
    | ConcretizeTypeAppsT (_, _, (_, _, _, reason), _) -> reason
    | CondT (reason, _, _, _) -> reason
    | MatchPropT (_, reason, _, _) -> reason
    | ReactPropsToOut (reason, _)
    | ReactInToProps (reason, _) ->
      reason
    | DestructuringT (reason, _, _, _) -> reason
    | CreateObjWithComputedPropT { reason; value = _; tout_tvar = _ } -> reason

  (* helper: we want the tvar id as well *)
  (* NOTE: uncalled for now, because ids are nondetermistic
     due to parallelism, which messes up test diffs. Should
     add a config, but for now must uncomment impl to use *)
  let reason_of_t_add_id = reason_of_t

  (* function
  | OpenT (r, id) -> prefix_reason (spf "%d: " id) r
  | t -> reason_of_t t *)
  let reason_of_use_t_add_id = reason_of_use_t

  let desc_of_t = reason_of_t %> desc_of_reason

  let loc_of_t = reason_of_t %> aloc_of_reason

  let def_loc_of_t = reason_of_t %> def_aloc_of_reason

  (* TODO make a type visitor *)
  let rec mod_reason_of_t f = function
    | OpenT (reason, id) -> OpenT (f reason, id)
    | AnnotT (reason, t, use_desc) -> AnnotT (f reason, t, use_desc)
    | MergedT (reason, uses) -> MergedT (f reason, uses)
    | BoundT (reason, name, polarity) -> BoundT (f reason, name, polarity)
    | InternalT (ChoiceKitT (reason, tool)) -> InternalT (ChoiceKitT (f reason, tool))
    | TypeDestructorTriggerT (use_op, reason, repos, d, t) ->
      TypeDestructorTriggerT (use_op, f reason, repos, d, t)
    | CustomFunT (reason, kind) -> CustomFunT (f reason, kind)
    | DefT (reason, trust, t) -> DefT (f reason, trust, t)
    | AnyT (reason, src) -> AnyT (f reason, src)
    | UnionT (reason, src) -> UnionT (f reason, src)
    | IntersectionT (reason, src) -> IntersectionT (f reason, src)
    | MaybeT (reason, src) -> MaybeT (f reason, src)
    | OptionalT { reason; type_; use_desc } -> OptionalT { reason = f reason; type_; use_desc }
    | EvalT (t, defer_use_t, id) -> EvalT (t, mod_reason_of_defer_use_t f defer_use_t, id)
    | ExactT (reason, t) -> ExactT (f reason, t)
    | ExistsT reason -> ExistsT (f reason)
    | InternalT (ExtendsT (reason, t1, t2)) -> InternalT (ExtendsT (f reason, t1, t2))
    | FunProtoApplyT reason -> FunProtoApplyT (f reason)
    | FunProtoT reason -> FunProtoT (f reason)
    | FunProtoBindT reason -> FunProtoBindT (f reason)
    | FunProtoCallT reason -> FunProtoCallT (f reason)
    | KeysT (reason, t) -> KeysT (f reason, t)
    | ModuleT (reason, exports, is_strict) -> ModuleT (f reason, exports, is_strict)
    | NullProtoT reason -> NullProtoT (f reason)
    | ObjProtoT reason -> ObjProtoT (f reason)
    | MatchingPropT (reason, k, v) -> MatchingPropT (f reason, k, v)
    | OpaqueT (reason, opaquetype) -> OpaqueT (f reason, opaquetype)
    | OpenPredT { reason; base_t; m_pos; m_neg } ->
      OpenPredT { reason = f reason; base_t; m_pos; m_neg }
    | InternalT (OptionalChainVoidT reason) -> InternalT (OptionalChainVoidT (f reason))
    | ReposT (reason, t) -> ReposT (f reason, t)
    | InternalT (ReposUpperT (reason, t)) -> InternalT (ReposUpperT (reason, mod_reason_of_t f t))
    | ShapeT t -> ShapeT (mod_reason_of_t f t)
    | ThisClassT (reason, t) -> ThisClassT (f reason, t)
    | ThisTypeAppT (reason, t1, t2, t3) -> ThisTypeAppT (f reason, t1, t2, t3)
    | TypeAppT (reason, t1, t2, t3) -> TypeAppT (f reason, t1, t2, t3)

  and mod_reason_of_defer_use_t f = function
    | LatentPredT (reason, p) -> LatentPredT (f reason, p)
    | TypeDestructorT (use_op, reason, s) -> TypeDestructorT (use_op, f reason, s)

  and mod_reason_of_use_t f = function
    | UseT (_, t) -> UseT (Op UnknownUse, mod_reason_of_t f t)
    | AdderT (use_op, reason, flip, rt, lt) -> AdderT (use_op, f reason, flip, rt, lt)
    | AndT (reason, t1, t2) -> AndT (f reason, t1, t2)
    | ArrRestT (use_op, reason, i, t) -> ArrRestT (use_op, f reason, i, t)
    | AssertArithmeticOperandT reason -> AssertArithmeticOperandT (f reason)
    | AssertBinaryInLHST reason -> AssertBinaryInLHST (f reason)
    | AssertBinaryInRHST reason -> AssertBinaryInRHST (f reason)
    | AssertForInRHST reason -> AssertForInRHST (f reason)
    | AssertImportIsValueT (reason, name) -> AssertImportIsValueT (f reason, name)
    | BecomeT (reason, t) -> BecomeT (f reason, t)
    | BindT (use_op, reason, ft, pass) -> BindT (use_op, f reason, ft, pass)
    | CallElemT (reason_call, reason_lookup, t, ft) ->
      CallElemT (f reason_call, reason_lookup, t, ft)
    | CallLatentPredT (reason, b, k, l, t) -> CallLatentPredT (f reason, b, k, l, t)
    | CallOpenPredT (reason, sense, key, l, t) -> CallOpenPredT (f reason, sense, key, l, t)
    | CallT (use_op, reason, ft) -> CallT (use_op, f reason, ft)
    | ChoiceKitUseT (reason, tool) -> ChoiceKitUseT (f reason, tool)
    | CJSExtractNamedExportsT (reason, exports, t2) ->
      CJSExtractNamedExportsT (f reason, exports, t2)
    | CJSRequireT (reason, t, is_strict) -> CJSRequireT (f reason, t, is_strict)
    | ComparatorT (reason, flip, t) -> ComparatorT (f reason, flip, t)
    | ConstructorT (use_op, reason, targs, args, tout) ->
      ConstructorT (use_op, f reason, targs, args, tout)
    | CopyNamedExportsT (reason, target_module_t, t_out) ->
      CopyNamedExportsT (f reason, target_module_t, t_out)
    | CopyTypeExportsT (reason, target_module_t, t_out) ->
      CopyTypeExportsT (f reason, target_module_t, t_out)
    | DebugPrintT reason -> DebugPrintT (f reason)
    | DebugSleepT reason -> DebugSleepT (f reason)
    | ElemT (use_op, reason, t, action) -> ElemT (use_op, f reason, t, action)
    | EqT (reason, flip, t) -> EqT (f reason, flip, t)
    | ExportNamedT (reason, tmap, export_kind, t_out) ->
      ExportNamedT (f reason, tmap, export_kind, t_out)
    | ExportTypeT (reason, name, t, t_out) -> ExportTypeT (f reason, name, t, t_out)
    | AssertExportIsTypeT (reason, export_name, t_out) ->
      AssertExportIsTypeT (f reason, export_name, t_out)
    | ExtendsUseT (use_op, reason, ts, t1, t2) -> ExtendsUseT (use_op, f reason, ts, t1, t2)
    | GetElemT (use_op, reason, it, et) -> GetElemT (use_op, f reason, it, et)
    | GetKeysT (reason, t) -> GetKeysT (f reason, t)
    | GetValuesT (reason, t) -> GetValuesT (f reason, t)
    | GetPropT (use_op, reason, n, t) -> GetPropT (use_op, f reason, n, t)
    | GetPrivatePropT (use_op, reason, name, bindings, static, t) ->
      GetPrivatePropT (use_op, f reason, name, bindings, static, t)
    | GetProtoT (reason, t) -> GetProtoT (f reason, t)
    | GetStaticsT (reason, t) -> GetStaticsT (f reason, t)
    | GuardT (pred, result, t) -> GuardT (pred, result, mod_reason_of_t f t)
    | HasOwnPropT (use_op, reason, prop) -> HasOwnPropT (use_op, f reason, prop)
    | IdxUnMaybeifyT (reason, t_out) -> IdxUnMaybeifyT (f reason, t_out)
    | IdxUnwrap (reason, t_out) -> IdxUnwrap (f reason, t_out)
    | ImplementsT (use_op, t) -> ImplementsT (use_op, mod_reason_of_t f t)
    | ImportDefaultT (reason, import_kind, name, t, is_strict) ->
      ImportDefaultT (f reason, import_kind, name, t, is_strict)
    | ImportModuleNsT (reason, t, is_strict) -> ImportModuleNsT (f reason, t, is_strict)
    | ImportNamedT (reason, import_kind, name, t, module_name, is_strict) ->
      ImportNamedT (f reason, import_kind, name, t, module_name, is_strict)
    | ImportTypeofT (reason, name, t) -> ImportTypeofT (f reason, name, t)
    | ImportTypeT (reason, name, t) -> ImportTypeT (f reason, name, t)
    | IntersectionPreprocessKitT (reason, tool) -> IntersectionPreprocessKitT (f reason, tool)
    | InvariantT reason -> InvariantT (f reason)
    | LookupT (reason, r2, ts, x, t) -> LookupT (f reason, r2, ts, x, t)
    | MakeExactT (reason, t) -> MakeExactT (f reason, t)
    | MapTypeT (use_op, reason, kind, t) -> MapTypeT (use_op, f reason, kind, t)
    | MethodT (use_op, reason_call, reason_lookup, name, ft, tm) ->
      MethodT (use_op, f reason_call, reason_lookup, name, ft, tm)
    | MixinT (reason, inst) -> MixinT (f reason, inst)
    | NotT (reason, t) -> NotT (f reason, t)
    | NullishCoalesceT (reason, t1, t2) -> NullishCoalesceT (f reason, t1, t2)
    | ObjAssignToT (op, reason, t, t2, kind) -> ObjAssignToT (op, f reason, t, t2, kind)
    | ObjAssignFromT (op, reason, t, t2, kind) -> ObjAssignFromT (op, f reason, t, t2, kind)
    | ObjFreezeT (reason, t) -> ObjFreezeT (f reason, t)
    | ObjRestT (reason, t, t2) -> ObjRestT (f reason, t, t2)
    | ObjSealT (reason, t) -> ObjSealT (f reason, t)
    | ObjTestProtoT (reason, t) -> ObjTestProtoT (f reason, t)
    | ObjTestT (reason, t1, t2) -> ObjTestT (f reason, t1, t2)
    | OptionalChainT (reason, lhs_reason, us, vs) -> OptionalChainT (f reason, lhs_reason, us, vs)
    | OrT (reason, t1, t2) -> OrT (f reason, t1, t2)
    | PredicateT (pred, t) -> PredicateT (pred, mod_reason_of_t f t)
    | ReactKitT (use_op, reason, tool) -> ReactKitT (use_op, f reason, tool)
    | RefineT (reason, p, t) -> RefineT (f reason, p, t)
    | ReposLowerT (reason, use_desc, t) -> ReposLowerT (f reason, use_desc, t)
    | ReposUseT (reason, use_desc, use_op, t) -> ReposUseT (f reason, use_desc, use_op, t)
    | ResolveSpreadT (use_op, reason_op, resolve) -> ResolveSpreadT (use_op, f reason_op, resolve)
    | SentinelPropTestT (reason_op, l, key, sense, sentinel, result) ->
      SentinelPropTestT (reason_op, l, key, sense, sentinel, mod_reason_of_t f result)
    | SetElemT (use_op, reason, it, mode, et, t) -> SetElemT (use_op, f reason, it, mode, et, t)
    | SetPropT (use_op, reason, n, mode, i, t, tp) -> SetPropT (use_op, f reason, n, mode, i, t, tp)
    | SetPrivatePropT (use_op, reason, n, mode, scopes, static, t, tp) ->
      SetPrivatePropT (use_op, f reason, n, mode, scopes, static, t, tp)
    | SetProtoT (reason, t) -> SetProtoT (f reason, t)
    | SpecializeT (use_op, reason_op, reason_tapp, cache, ts, t) ->
      SpecializeT (use_op, f reason_op, reason_tapp, cache, ts, t)
    | ObjKitT (use_op, reason, resolve_tool, tool, tout) ->
      ObjKitT (use_op, f reason, resolve_tool, tool, tout)
    | ModuleExportsAssignT (reason, ts, t) -> ModuleExportsAssignT (f reason, ts, t)
    | SubstOnPredT (reason, subst, t) -> SubstOnPredT (f reason, subst, t)
    | SuperT (op, reason, inst) -> SuperT (op, f reason, inst)
    | TestPropT (reason, id, n, t) -> TestPropT (f reason, id, n, t)
    | ThisSpecializeT (reason, this, k) -> ThisSpecializeT (f reason, this, k)
    | ToStringT (reason, t) -> ToStringT (f reason, t)
    | UnaryMinusT (reason, t) -> UnaryMinusT (f reason, t)
    | UnifyT (t, t2) -> UnifyT (mod_reason_of_t f t, mod_reason_of_t f t2)
    | VarianceCheckT (reason, ts, polarity) -> VarianceCheckT (f reason, ts, polarity)
    | TypeAppVarianceCheckT (use_op, reason_op, reason_tapp, targs) ->
      TypeAppVarianceCheckT (use_op, f reason_op, reason_tapp, targs)
    | ConcretizeTypeAppsT (use_op, t1, (t2, ts2, op2, r2), targs) ->
      ConcretizeTypeAppsT (use_op, t1, (t2, ts2, op2, f r2), targs)
    | CondT (reason, then_t, else_t, tout) -> CondT (f reason, then_t, else_t, tout)
    | MatchPropT (op, reason, prop, t) -> MatchPropT (op, f reason, prop, t)
    | ReactPropsToOut (reason, t) -> ReactPropsToOut (f reason, t)
    | ReactInToProps (reason, t) -> ReactInToProps (f reason, t)
    | DestructuringT (reason, a, s, t) -> DestructuringT (f reason, a, s, t)
    | CreateObjWithComputedPropT { reason; value; tout_tvar } ->
      CreateObjWithComputedPropT { reason = f reason; value; tout_tvar }

  and mod_reason_of_opt_use_t f = function
    | OptCallT (use_op, reason, ft) -> OptCallT (use_op, reason, ft)
    | OptGetPropT (use_op, reason, n) -> OptGetPropT (use_op, f reason, n)
    | OptGetPrivatePropT (use_op, reason, name, bindings, static) ->
      OptGetPrivatePropT (use_op, f reason, name, bindings, static)
    | OptTestPropT (reason, id, n) -> OptTestPropT (f reason, id, n)
    | OptGetElemT (use_op, reason, it) -> OptGetElemT (use_op, f reason, it)

  let rec util_use_op_of_use_t :
            'a. (use_t -> 'a) -> (use_t -> use_op -> (use_op -> use_t) -> 'a) -> use_t -> 'a =
   fun nope util u ->
    let util = util u in
    let nested_util u2 make2 =
      let result = util_use_op_of_use_t (fun _ -> None) (fun _ op make -> Some (op, make)) u2 in
      match result with
      | None -> nope u
      | Some (op, make) -> util op (fun op -> make2 (make op))
    in
    match u with
    | UseT (op, t) -> util op (fun op -> UseT (op, t))
    | BindT (op, r, f, b) -> util op (fun op -> BindT (op, r, f, b))
    | CallT (op, r, f) -> util op (fun op -> CallT (op, r, f))
    | MethodT (op, r1, r2, p, f, tm) -> util op (fun op -> MethodT (op, r1, r2, p, f, tm))
    | SetPropT (op, r, p, m, w, t, tp) -> util op (fun op -> SetPropT (op, r, p, m, w, t, tp))
    | SetPrivatePropT (op, r, s, m, c, b, t, tp) ->
      util op (fun op -> SetPrivatePropT (op, r, s, m, c, b, t, tp))
    | GetPropT (op, r, p, t) -> util op (fun op -> GetPropT (op, r, p, t))
    | MatchPropT (op, r, p, t) -> util op (fun op -> MatchPropT (op, r, p, t))
    | GetPrivatePropT (op, r, s, c, b, t) ->
      util op (fun op -> GetPrivatePropT (op, r, s, c, b, t))
    | SetElemT (op, r, t1, m, t2, t3) -> util op (fun op -> SetElemT (op, r, t1, m, t2, t3))
    | GetElemT (op, r, t1, t2) -> util op (fun op -> GetElemT (op, r, t1, t2))
    | ReposLowerT (r, d, u2) -> nested_util u2 (fun u2 -> ReposLowerT (r, d, u2))
    | ReposUseT (r, d, op, t) -> util op (fun op -> ReposUseT (r, d, op, t))
    | ConstructorT (op, r, targs, args, t) ->
      util op (fun op -> ConstructorT (op, r, targs, args, t))
    | SuperT (op, r, i) -> util op (fun op -> SuperT (op, r, i))
    | AdderT (op, d, f, l, r) -> util op (fun op -> AdderT (op, d, f, l, r))
    | ImplementsT (op, t) -> util op (fun op -> ImplementsT (op, t))
    | ToStringT (r, u2) -> nested_util u2 (fun u2 -> ToStringT (r, u2))
    | SpecializeT (op, r1, r2, c, ts, t) -> util op (fun op -> SpecializeT (op, r1, r2, c, ts, t))
    | TypeAppVarianceCheckT (op, r1, r2, ts) ->
      util op (fun op -> TypeAppVarianceCheckT (op, r1, r2, ts))
    | ConcretizeTypeAppsT (u, (ts1, op, r1), x2, b) ->
      util op (fun op -> ConcretizeTypeAppsT (u, (ts1, op, r1), x2, b))
    | ArrRestT (op, r, i, t) -> util op (fun op -> ArrRestT (op, r, i, t))
    | HasOwnPropT (op, r, p) -> util op (fun op -> HasOwnPropT (op, r, p))
    | GetKeysT (r, u2) -> nested_util u2 (fun u2 -> GetKeysT (r, u2))
    | ElemT (op, r, t, a) -> util op (fun op -> ElemT (op, r, t, a))
    | ObjKitT (op, r, x, y, t) -> util op (fun op -> ObjKitT (op, r, x, y, t))
    | ReactKitT (op, r, t) -> util op (fun op -> ReactKitT (op, r, t))
    | ResolveSpreadT (op, r, s) -> util op (fun op -> ResolveSpreadT (op, r, s))
    | ExtendsUseT (op, r, ts, a, b) -> util op (fun op -> ExtendsUseT (op, r, ts, a, b))
    | MapTypeT (op, r, k, t) -> util op (fun op -> MapTypeT (op, r, k, t))
    | ObjAssignToT (op, r, t1, t2, k) -> util op (fun op -> ObjAssignToT (op, r, t1, t2, k))
    | ObjAssignFromT (op, r, t1, t2, k) -> util op (fun op -> ObjAssignFromT (op, r, t1, t2, k))
    | MakeExactT (r, Lower (op, t)) -> util op (fun op -> MakeExactT (r, Lower (op, t)))
    | MakeExactT (_, _)
    | TestPropT (_, _, _, _)
    | CallElemT (_, _, _, _)
    | GetStaticsT (_, _)
    | GetProtoT (_, _)
    | SetProtoT (_, _)
    | MixinT (_, _)
    | ComparatorT (_, _, _)
    | UnaryMinusT (_, _)
    | AssertArithmeticOperandT _
    | AssertBinaryInLHST _
    | AssertBinaryInRHST _
    | AssertForInRHST _
    | PredicateT (_, _)
    | GuardT (_, _, _)
    | EqT (_, _, _)
    | AndT (_, _, _)
    | OrT (_, _, _)
    | NullishCoalesceT (_, _, _)
    | NotT (_, _)
    | ThisSpecializeT (_, _, _)
    | VarianceCheckT (_, _, _)
    | LookupT (_, _, _, _, _)
    | ObjFreezeT (_, _)
    | ObjRestT (_, _, _)
    | ObjSealT (_, _)
    | ObjTestProtoT (_, _)
    | ObjTestT (_, _, _)
    | UnifyT (_, _)
    | BecomeT (_, _)
    | GetValuesT (_, _)
    | CJSRequireT (_, _, _)
    | ImportModuleNsT (_, _, _)
    | ImportDefaultT (_, _, _, _, _)
    | ImportNamedT (_, _, _, _, _, _)
    | ImportTypeT (_, _, _)
    | ImportTypeofT (_, _, _)
    | AssertImportIsValueT (_, _)
    | CJSExtractNamedExportsT (_, _, _)
    | CopyNamedExportsT (_, _, _)
    | CopyTypeExportsT (_, _, _)
    | ExportNamedT (_, _, _, _)
    | ExportTypeT (_, _, _, _)
    | AssertExportIsTypeT (_, _, _)
    | ChoiceKitUseT (_, _)
    | IntersectionPreprocessKitT (_, _)
    | DebugPrintT _
    | DebugSleepT _
    | SentinelPropTestT (_, _, _, _, _, _)
    | IdxUnwrap (_, _)
    | IdxUnMaybeifyT (_, _)
    | OptionalChainT (_, _, _, _)
    | InvariantT _
    | CallLatentPredT (_, _, _, _, _)
    | CallOpenPredT (_, _, _, _, _)
    | SubstOnPredT (_, _, _)
    | RefineT (_, _, _)
    | CondT (_, _, _, _)
    | ReactPropsToOut _
    | ReactInToProps _
    | DestructuringT _
    | ModuleExportsAssignT _
    | CreateObjWithComputedPropT _ ->
      nope u

  let use_op_of_use_t = util_use_op_of_use_t (fun _ -> None) (fun _ op _ -> Some op)

  let mod_use_op_of_use_t f =
    util_use_op_of_use_t
      (fun u -> u)
      (fun u op make ->
        let op' = f op in
        if op' == op then
          u
        else
          make op')

  let rec mod_root_of_use_op f = function
    | Op op -> Op (f op)
    | Frame (fr, o) -> Frame (fr, mod_root_of_use_op f o)

  let rec mod_loc_of_virtual_use_op f =
    let mod_reason = Reason.map_reason_locs f in
    let mod_loc_of_root_use_op f = function
      | InitField { op; body } -> InitField { op = mod_reason op; body = mod_reason body }
      | ObjectSpread { op } -> ObjectSpread { op = mod_reason op }
      | ObjectChain { op } -> ObjectChain { op = mod_reason op }
      | Addition { op; left; right } ->
        Addition { op = mod_reason op; left = mod_reason left; right = mod_reason right }
      | AssignVar { var; init } ->
        AssignVar { var = Option.map ~f:mod_reason var; init = mod_reason init }
      | Cast { lower; upper } -> Cast { lower = mod_reason lower; upper = mod_reason upper }
      | ClassExtendsCheck { def; name; extends } ->
        ClassExtendsCheck
          { def = mod_reason def; name = mod_reason name; extends = mod_reason extends }
      | ClassImplementsCheck { def; name; implements } ->
        ClassImplementsCheck
          { def = mod_reason def; name = mod_reason name; implements = mod_reason implements }
      | ClassOwnProtoCheck { own_loc; proto_loc; prop } ->
        ClassOwnProtoCheck
          { prop; own_loc = Option.map ~f own_loc; proto_loc = Option.map ~f proto_loc }
      | Coercion { from; target } ->
        Coercion { from = mod_reason from; target = mod_reason target }
      | DeleteProperty { lhs; prop } ->
        DeleteProperty { lhs = mod_reason lhs; prop = mod_reason prop }
      | DeleteVar { var } -> DeleteVar { var = mod_reason var }
      | FunCall { op; fn; args; local } ->
        FunCall
          {
            local;
            op = mod_reason op;
            fn = mod_reason fn;
            args = Base.List.map ~f:mod_reason args;
          }
      | FunCallMethod { op; fn; args; prop; local } ->
        FunCallMethod
          {
            local;
            op = mod_reason op;
            fn = mod_reason fn;
            prop = mod_reason prop;
            args = Base.List.map ~f:mod_reason args;
          }
      | FunReturnStatement { value } -> FunReturnStatement { value = mod_reason value }
      | FunImplicitReturn { fn; upper } ->
        FunImplicitReturn { fn = mod_reason fn; upper = mod_reason upper }
      | GeneratorYield { value } -> GeneratorYield { value = mod_reason value }
      | GetProperty reason -> GetProperty (mod_reason reason)
      | Internal o -> Internal o
      | JSXCreateElement { op; component } ->
        JSXCreateElement { op = mod_reason op; component = mod_reason component }
      | ReactCreateElementCall { op; component; children } ->
        ReactCreateElementCall
          { op = mod_reason op; component = mod_reason component; children = f children }
      | ReactGetIntrinsic { literal } -> ReactGetIntrinsic { literal = mod_reason literal }
      | Speculation op -> Speculation (mod_loc_of_virtual_use_op f op)
      | TypeApplication { type' } -> TypeApplication { type' = mod_reason type' }
      | SetProperty { lhs; prop; value } ->
        SetProperty { lhs = mod_reason lhs; prop = mod_reason prop; value = mod_reason value }
      | UpdateProperty { lhs; prop } ->
        UpdateProperty { lhs = mod_reason lhs; prop = mod_reason prop }
      | UnknownUse -> UnknownUse
    in
    let mod_loc_of_frame_use_op = function
      | ArrayElementCompatibility { lower; upper } ->
        ArrayElementCompatibility { lower = mod_reason lower; upper = mod_reason upper }
      | FunCompatibility { lower; upper } ->
        FunCompatibility { lower = mod_reason lower; upper = mod_reason upper }
      | FunMissingArg { n; op; def } ->
        FunMissingArg { n; op = mod_reason op; def = mod_reason def }
      | FunParam { n; name; lower; upper } ->
        FunParam { n; name; lower = mod_reason lower; upper = mod_reason upper }
      | FunRestParam { lower; upper } ->
        FunRestParam { lower = mod_reason lower; upper = mod_reason upper }
      | FunReturn { lower; upper } ->
        FunReturn { lower = mod_reason lower; upper = mod_reason upper }
      | ImplicitTypeParam -> ImplicitTypeParam
      | IndexerKeyCompatibility { lower; upper } ->
        IndexerKeyCompatibility { lower = mod_reason lower; upper = mod_reason upper }
      | CallFunCompatibility { n } -> CallFunCompatibility { n }
      | TupleMapFunCompatibility { value } -> TupleMapFunCompatibility { value = mod_reason value }
      | ObjMapFunCompatibility { value } -> ObjMapFunCompatibility { value = mod_reason value }
      | ObjMapiFunCompatibility { key; value } ->
        ObjMapiFunCompatibility { key = mod_reason key; value = mod_reason value }
      | PropertyCompatibility { prop; lower; upper } ->
        PropertyCompatibility { prop; lower = mod_reason lower; upper = mod_reason upper }
      | ReactConfigCheck -> ReactConfigCheck
      | ReactGetConfig o -> ReactGetConfig o
      | TupleElementCompatibility { n; lower; upper } ->
        TupleElementCompatibility { n; lower = mod_reason lower; upper = mod_reason upper }
      | TypeArgCompatibility { name; targ; lower; upper; polarity } ->
        TypeArgCompatibility
          {
            name;
            polarity;
            targ = mod_reason targ;
            lower = mod_reason lower;
            upper = mod_reason upper;
          }
      | TypeParamBound o -> TypeParamBound o
      | UnifyFlip -> UnifyFlip
    in
    function
    | Op op -> Op (mod_loc_of_root_use_op f op)
    | Frame (fr, o) -> Frame (mod_loc_of_frame_use_op fr, mod_loc_of_virtual_use_op f o)

  (* type comparison mod reason *)
  let reasonless_compare =
    let rec swap_reason t2 t1 =
      match (t2, t1) with
      (* In reposition we also recurse and reposition some nested types. We need
       * to make sure we swap the types for these reasons as well. Otherwise our
       * optimized union ~> union check will not pass. *)
      | (MaybeT (_, t2), MaybeT (r, t1)) -> MaybeT (r, swap_reason t2 t1)
      | ( OptionalT { reason = _; type_ = t2; use_desc = _ },
          OptionalT { reason; type_ = t1; use_desc } ) ->
        OptionalT { reason; type_ = swap_reason t2 t1; use_desc }
      | (ExactT (_, t2), ExactT (r, t1)) -> ExactT (r, swap_reason t2 t1)
      | _ -> mod_reason_of_t (fun _ -> reason_of_t t1) t2
    in
    fun t1 t2 ->
      if t1 == t2 then
        0
      else
        compare t1 (swap_reason t2 t1)

  let reasonless_eq t1 t2 = reasonless_compare t1 t2 = 0

  let literal_eq x = function
    | Literal (_, y) -> x = y
    | Truthy -> false
    | AnyLiteral -> false

  let number_literal_eq (x, _) = function
    | Literal (_, (y, _)) -> x = y
    | Truthy -> false
    | AnyLiteral -> false

  let boolean_literal_eq x = function
    | Some y -> x = y
    | None -> false

  let trust_subtype_fixed tr1 tr2 =
    match (Trust.expand tr1, Trust.expand tr2) with
    | (Trust.Qualifier trust1, Trust.Qualifier trust2) -> Trust.subtype_trust trust1 trust2
    | _ -> false

  let quick_subtype trust_checked t1 t2 =
    Trust.(
      match (t1, t2) with
      | (DefT (_, ltrust, NumT _), DefT (_, rtrust, NumT _))
      | (DefT (_, ltrust, SingletonNumT _), DefT (_, rtrust, NumT _))
      | (DefT (_, ltrust, StrT _), DefT (_, rtrust, StrT _))
      | (DefT (_, ltrust, SingletonStrT _), DefT (_, rtrust, StrT _))
      | (DefT (_, ltrust, BoolT _), DefT (_, rtrust, BoolT _))
      | (DefT (_, ltrust, SingletonBoolT _), DefT (_, rtrust, BoolT _))
      | (DefT (_, ltrust, NullT), DefT (_, rtrust, NullT))
      | (DefT (_, ltrust, VoidT), DefT (_, rtrust, VoidT))
      | (DefT (_, ltrust, EmptyT _), DefT (_, rtrust, _))
      | (DefT (_, ltrust, _), DefT (_, rtrust, MixedT _)) ->
        (not trust_checked) || trust_subtype_fixed ltrust rtrust
      | (DefT (_, ltrust, EmptyT _), _) ->
        (not trust_checked) || trust_value_map ~f:is_public ~default:false ltrust
      | (_, DefT (_, rtrust, MixedT _)) ->
        (not trust_checked) || trust_value_map ~f:is_tainted ~default:false rtrust
      | (DefT (_, ltrust, StrT actual), DefT (_, rtrust, SingletonStrT expected)) ->
        ((not trust_checked) || trust_subtype_fixed ltrust rtrust) && literal_eq expected actual
      | (DefT (_, ltrust, NumT actual), DefT (_, rtrust, SingletonNumT expected)) ->
        ((not trust_checked) || trust_subtype_fixed ltrust rtrust)
        && number_literal_eq expected actual
      | (DefT (_, ltrust, BoolT actual), DefT (_, rtrust, SingletonBoolT expected)) ->
        ((not trust_checked) || trust_subtype_fixed ltrust rtrust)
        && boolean_literal_eq expected actual
      | _ -> reasonless_eq t1 t2)
end

(* Type scheme: a type and an attendant environment of type parameters.
 * See normalizer for use. *)
module TypeScheme = struct
  type t = {
    tparams: TypeTerm.typeparam list;
    type_: TypeTerm.t;
  }
end

include TypeTerm
include TypeUtil
include Trust

(**** Trust utilities ****)

let with_trust (trust_constructor : unit -> trust_rep) (type_constructor : trust_rep -> t) : t =
  trust_constructor () |> type_constructor

(*********************************************************)

let compare = Pervasives.compare

let open_tvar tvar =
  match tvar with
  | OpenT (reason, id) -> (reason, id)
  | _ -> assert false

module type PrimitiveType = sig
  val desc : reason_desc

  val make : reason -> trust_rep -> t
end

module Primitive (P : PrimitiveType) = struct
  let desc = P.desc

  let at tok = P.make (mk_annot_reason desc tok)

  let why reason = P.make (replace_desc_reason desc reason)

  let make = P.make

  let why_with_use_desc ~use_desc r trust =
    let r =
      if use_desc then
        r
      else
        replace_desc_reason P.desc r
    in
    P.make r trust
end

module NumT = Primitive (struct
  let desc = RNumber

  let make r trust = DefT (r, trust, NumT AnyLiteral)
end)

module StrT = Primitive (struct
  let desc = RString

  let make r trust = DefT (r, trust, StrT AnyLiteral)
end)

module BoolT = Primitive (struct
  let desc = RBoolean

  let make r trust = DefT (r, trust, BoolT None)
end)

module MixedT = Primitive (struct
  let desc = RMixed

  let make r trust = DefT (r, trust, MixedT Mixed_everything)
end)

module EmptyT = Primitive (struct
  let desc = REmpty

  let make r trust = DefT (r, trust, EmptyT Bottom)
end)

module AnyT = struct
  let desc = function
    | Annotated -> RAnyExplicit
    | _ -> RAnyImplicit

  let make source r = AnyT (r, source)

  let at source = mk_annot_reason (desc source) %> make source

  let why source = replace_desc_reason (desc source) %> make source

  let annot = why Annotated

  let error = why AnyError

  let untyped = why Untyped

  let locationless source = desc source |> locationless_reason |> make source

  let source = function
    | AnyT (_, s) -> s
    | _ -> failwith "not an any type"
end

module Unsoundness = struct
  let constructor = Unsound Constructor

  let computed_nonlit_key = Unsound ComputedNonLiteralKey

  let function_proto = Unsound FunctionPrototype

  let merged = Unsound Merged

  let instance_of_refi = Unsound InstanceOfRefinement

  let unresolved = Unsound UnresolvedType

  let resolve_spread = Unsound ResolveSpread

  let unimplemented = Unsound Unimplemented

  let weak_context = Unsound WeakContext

  let inference_hooks = Unsound InferenceHooks

  let exports = Unsound Exports

  let existential = Unsound Existential

  let bound_fn_this = Unsound BoundFunctionThis

  let dummy_static = Unsound DummyStatic

  let merged_any = AnyT.make merged

  let instance_of_refi_any = AnyT.make instance_of_refi

  let unresolved_any = AnyT.make unresolved

  let resolve_spread_any = AnyT.make resolve_spread

  let constructor_any = AnyT.make constructor

  let function_proto_any = AnyT.make function_proto

  let computed_nonlit_key_any = AnyT.make computed_nonlit_key

  let unimplemented_any = AnyT.make unimplemented

  let weak_context_any = AnyT.make weak_context

  let inference_hooks_any = AnyT.make inference_hooks

  let exports_any = AnyT.make exports

  let existential_any = AnyT.make existential

  let bound_fn_this_any = AnyT.make bound_fn_this

  let dummy_static_any = AnyT.make dummy_static

  let why kind = Unsound kind |> AnyT.why

  let at kind = Unsound kind |> AnyT.at

  (* Unsound types would be too noisy if we banned them in exports,
     so we allow them for now *)
  let banned_in_exports = function
    | Unsound _ -> false
    | _ -> true
end

module VoidT = Primitive (struct
  let desc = RVoid

  let make r trust = DefT (r, trust, VoidT)
end)

module NullT = Primitive (struct
  let desc = RNull

  let make r trust = DefT (r, trust, NullT)
end)

module ObjProtoT = Primitive (struct
  let desc = RDummyPrototype

  let make r _ = ObjProtoT r
end)

module NullProtoT = Primitive (struct
  let desc = RNull

  let make r _ = NullProtoT r
end)

(* USE WITH CAUTION!!! Locationless types should not leak to errors, otherwise
   they will cause error printing to crash.

   We use locationless reasons legitimately for normalizing. Also, because `any`
   doesn't cause errors, locationless `AnyT` is OK.
*)
module Locationless = struct
  module LocationLess (P : PrimitiveType) = struct
    let t = P.make (locationless_reason P.desc)
  end

  module NumT = LocationLess (NumT)
  module StrT = LocationLess (StrT)
  module BoolT = LocationLess (BoolT)
  module MixedT = LocationLess (MixedT)
  module EmptyT = LocationLess (EmptyT)
  module VoidT = LocationLess (VoidT)
  module NullT = LocationLess (NullT)
end

(* lift an operation on Type.t to an operation on Type.use_t *)
let lift_to_use f = function
  | UseT (_, t) -> f t
  | _ -> ()

(* def types vs. use types *)
let is_use = function
  | UseT _ -> false
  | _ -> true

(* not all so-called def types can appear as use types *)
let is_proper_def = function
  | InternalT _
  | MatchingPropT _ ->
    false
  | _ -> true

(* not all use types should appear in "merged" types *)
let is_proper_use = function
  (* Speculation should be completed by the end of merge. This does not hold
     today because non-0->1 things are erroneously considered 0->1, specifically
     type parameters and sometimes eval types. Until this situation is fixed, we
     can at least avoid these things leaking into dependent merge steps. *)
  | ChoiceKitUseT _ -> false
  | _ -> true

(* convenience *)
let is_bot = function
  | DefT (_, _, EmptyT _) -> true
  | _ -> false

let is_top = function
  | DefT (_, _, MixedT _) -> true
  | _ -> false

let is_any = function
  | AnyT _ -> true
  | _ -> false

(* Primitives, like string, will be promoted to their wrapper object types for
 * certain operations, like GetPropT, but not for others, like `UseT _`. *)
let primitive_promoting_use_t = function
  | CallElemT _
  | GetElemT _
  | GetPropT _
  | GetPrivatePropT _
  | GetProtoT _
  | MethodT _
  | TestPropT _ ->
    true
  (* "internal" use types, which should not be called directly on primitives,
   * but it's OK if they are in practice. TODO: consider making this an internal
   * error *)
  | LookupT _ -> true
  (* TODO: enumerate all use types *)
  | _ -> false

let rec fold_use_op f1 f2 = function
  | Op root -> f1 root
  | Frame (frame, use_op) ->
    let acc = fold_use_op f1 f2 use_op in
    f2 acc frame

let rec root_of_use_op = function
  | Op use_op -> use_op
  | Frame (_, use_op) -> root_of_use_op use_op

let replace_speculation_root_use_op =
  let rec loop new_parent_use_op = function
    | Op (Speculation _) -> Ok new_parent_use_op
    | Op _ -> Error new_parent_use_op
    | Frame (frame, parent_use_op) as use_op ->
      let parent_use_op' = loop new_parent_use_op parent_use_op in
      (match parent_use_op' with
      | Error _ as error -> error
      | Ok parent_use_op' ->
        if parent_use_op' == parent_use_op then
          Ok use_op
        else
          Ok (Frame (frame, parent_use_op')))
  in
  fun new_parent_use_op use_op ->
    match loop new_parent_use_op use_op with
    | Ok use_op -> use_op
    | Error use_op -> use_op

let aloc_of_root_use_op : root_use_op -> ALoc.t = function
  | InitField { op; _ }
  | ObjectSpread { op }
  | ObjectChain { op }
  | Addition { op; _ }
  | AssignVar { init = op; _ }
  | Cast { lower = op; _ }
  | ClassExtendsCheck { def = op; _ }
  | ClassImplementsCheck { def = op; _ }
  | Coercion { from = op; _ }
  | DeleteProperty { lhs = op; _ }
  | DeleteVar { var = op; _ }
  | FunCall { op; _ }
  | FunCallMethod { op; _ }
  | FunReturnStatement { value = op }
  | FunImplicitReturn { upper = op; _ }
  | GeneratorYield { value = op }
  | GetProperty op
  | JSXCreateElement { op; _ }
  | ReactCreateElementCall { op; _ }
  | TypeApplication { type' = op }
  | SetProperty { value = op; _ }
  | UpdateProperty { lhs = op; _ } ->
    aloc_of_reason op
  | ReactGetIntrinsic _
  | Speculation _
  | Internal _
  | UnknownUse
  | ClassOwnProtoCheck _ ->
    ALoc.none

(* Printing some types in parseable form relies on particular formats in
   corresponding reason descriptions. The following module formalizes the
   relevant conventions.

   TODO: Encoding formats in strings instead of ADTs is not ideal, obviously. *)

module DescFormat = struct
  (* InstanceT reasons have desc = name *)
  let instance_reason name loc = mk_reason (RType name) loc

  let name_of_instance_reason r =
    match desc_of_reason r with
    | RType name -> name
    | desc -> string_of_desc desc

  (* TypeT reasons have desc = type `name` *)
  let type_reason name loc = mk_reason (RType name) loc

  let name_of_type_reason r =
    match desc_of_reason r with
    | RType name -> name
    | _ -> failwith "not a type reason"
end

(* printing *)
let string_of_defer_use_ctor = function
  | LatentPredT _ -> "LatentPredT"
  | TypeDestructorT _ -> "TypeDestructorT"

let string_of_def_ctor = function
  | ArrT _ -> "ArrT"
  | BoolT _ -> "BoolT"
  | CharSetT _ -> "CharSetT"
  | ClassT _ -> "ClassT"
  | EmptyT _ -> "EmptyT"
  | EnumT _ -> "EnumT"
  | EnumObjectT _ -> "EnumObjectT"
  | FunT _ -> "FunT"
  | IdxWrapper _ -> "IdxWrapper"
  | InstanceT _ -> "InstanceT"
  | MixedT _ -> "MixedT"
  | NullT -> "NullT"
  | NumT _ -> "NumT"
  | ObjT _ -> "ObjT"
  | PolyT _ -> "PolyT"
  | ReactAbstractComponentT _ -> "ReactAbstractComponentT"
  | SingletonBoolT _ -> "SingletonBoolT"
  | SingletonNumT _ -> "SingletonNumT"
  | SingletonStrT _ -> "SingletonStrT"
  | StrT _ -> "StrT"
  | TypeT _ -> "TypeT"
  | VoidT -> "VoidT"

let string_of_ctor = function
  | OpenT _ -> "OpenT"
  | AnyT _ -> "AnyT"
  | AnnotT _ -> "AnnotT"
  | MergedT _ -> "MergedT"
  | BoundT _ -> "BoundT"
  | InternalT (ChoiceKitT (_, tool)) ->
    spf
      "ChoiceKitT %s"
      begin
        match tool with
        | Trigger -> "Trigger"
      end
  | TypeDestructorTriggerT _ -> "TypeDestructorTriggerT"
  | CustomFunT _ -> "CustomFunT"
  | DefT (_, _, t) -> string_of_def_ctor t
  | EvalT _ -> "EvalT"
  | ExactT _ -> "ExactT"
  | ExistsT _ -> "ExistsT"
  | InternalT (ExtendsT _) -> "ExtendsT"
  | FunProtoT _ -> "FunProtoT"
  | FunProtoApplyT _ -> "FunProtoApplyT"
  | FunProtoBindT _ -> "FunProtoBindT"
  | FunProtoCallT _ -> "FunProtoCallT"
  | KeysT _ -> "KeysT"
  | ModuleT _ -> "ModuleT"
  | NullProtoT _ -> "NullProtoT"
  | ObjProtoT _ -> "ObjProtoT"
  | MatchingPropT _ -> "MatchingPropT"
  | OpaqueT _ -> "OpaqueT"
  | OpenPredT _ -> "OpenPredT"
  | InternalT (OptionalChainVoidT _) -> "OptionalChainVoidT"
  | ReposT _ -> "ReposT"
  | InternalT (ReposUpperT _) -> "ReposUpperT"
  | ShapeT _ -> "ShapeT"
  | ThisClassT _ -> "ThisClassT"
  | ThisTypeAppT _ -> "ThisTypeAppT"
  | TypeAppT _ -> "TypeAppT"
  | UnionT _ -> "UnionT"
  | IntersectionT _ -> "IntersectionT"
  | OptionalT _ -> "OptionalT"
  | MaybeT _ -> "MaybeT"

let string_of_internal_use_op = function
  | CopyEnv -> "CopyEnv"
  | MergeEnv -> "MergeEnv"
  | Refinement -> "Refinement"
  | WidenEnv -> "WidenEnv"

let string_of_root_use_op (type a) : a virtual_root_use_op -> string = function
  | InitField _ -> "InitField"
  | ObjectSpread _ -> "ObjectSpread"
  | ObjectChain _ -> "ObjectChain"
  | Addition _ -> "Addition"
  | AssignVar _ -> "AssignVar"
  | Cast _ -> "Cast"
  | ClassExtendsCheck _ -> "ClassExtendsCheck"
  | ClassImplementsCheck _ -> "ClassImplementsCheck"
  | ClassOwnProtoCheck _ -> "ClassOwnProtoCheck"
  | Coercion _ -> "Coercion"
  | DeleteProperty _ -> "DeleteProperty"
  | DeleteVar _ -> "DeleteVar"
  | FunCall _ -> "FunCall"
  | FunCallMethod _ -> "FunCallMethod"
  | FunImplicitReturn _ -> "FunImplicitReturn"
  | FunReturnStatement _ -> "FunReturnStatement"
  | GeneratorYield _ -> "GeneratorYield"
  | GetProperty _ -> "GetProperty"
  | Internal op -> spf "Internal(%s)" (string_of_internal_use_op op)
  | JSXCreateElement _ -> "JSXCreateElement"
  | ReactCreateElementCall _ -> "ReactCreateElementCall"
  | ReactGetIntrinsic _ -> "ReactGetIntrinsic"
  | Speculation _ -> "Speculation"
  | TypeApplication _ -> "TypeApplication"
  | SetProperty _ -> "SetProperty"
  | UpdateProperty _ -> "UpdateProperty"
  | UnknownUse -> "UnknownUse"

let string_of_frame_use_op (type a) : a virtual_frame_use_op -> string = function
  | ArrayElementCompatibility _ -> "ArrayElementCompatibility"
  | FunCompatibility _ -> "FunCompatibility"
  | FunMissingArg _ -> "FunMissingArg"
  | FunParam _ -> "FunParam"
  | FunRestParam _ -> "FunRestParam"
  | FunReturn _ -> "FunReturn"
  | ImplicitTypeParam -> "ImplicitTypeParam"
  | IndexerKeyCompatibility _ -> "IndexerKeyCompatibility"
  | CallFunCompatibility _ -> "CallFunCompatibility"
  | TupleMapFunCompatibility _ -> "TupleMapFunCompatibility"
  | ObjMapFunCompatibility _ -> "ObjMapFunCompatibility"
  | ObjMapiFunCompatibility _ -> "ObjMapiFunCompatibility"
  | PropertyCompatibility _ -> "PropertyCompatibility"
  | ReactConfigCheck -> "ReactConfigCheck"
  | ReactGetConfig _ -> "ReactGetConfig"
  | TupleElementCompatibility _ -> "TupleElementCompatibility"
  | TypeArgCompatibility _ -> "TypeArgCompatibility"
  | TypeParamBound _ -> "TypeParamBound"
  | UnifyFlip -> "UnifyFlip"

let string_of_use_op (type a) : a virtual_use_op -> string = function
  | Op root -> string_of_root_use_op root
  | Frame (frame, _) -> string_of_frame_use_op frame

let string_of_use_op_rec : use_op -> string =
  fold_use_op string_of_root_use_op (fun acc use_op ->
      spf "%s(%s)" (string_of_frame_use_op use_op) acc)

let string_of_use_ctor = function
  | UseT (op, t) -> spf "UseT(%s, %s)" (string_of_use_op op) (string_of_ctor t)
  | AdderT _ -> "AdderT"
  | AndT _ -> "AndT"
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
    spf
      "ChoiceKitUseT %s"
      begin
        match tool with
        | FullyResolveType _ -> "FullyResolveType"
        | TryFlow _ -> "TryFlow"
      end
  | CJSExtractNamedExportsT _ -> "CJSExtractNamedExportsT"
  | CJSRequireT _ -> "CJSRequireT"
  | ComparatorT _ -> "ComparatorT"
  | ConstructorT _ -> "ConstructorT"
  | CopyNamedExportsT _ -> "CopyNamedExportsT"
  | CopyTypeExportsT _ -> "CopyTypeExportsT"
  | DebugPrintT _ -> "DebugPrintT"
  | DebugSleepT _ -> "DebugSleepT"
  | ElemT _ -> "ElemT"
  | EqT _ -> "EqT"
  | ExportNamedT _ -> "ExportNamedT"
  | ExportTypeT _ -> "ExportTypeT"
  | AssertExportIsTypeT _ -> "AssertExportIsTypeT"
  | ExtendsUseT _ -> "ExtendsUseT"
  | GetElemT _ -> "GetElemT"
  | GetKeysT _ -> "GetKeysT"
  | GetValuesT _ -> "GetValuesT"
  | GetPropT _ -> "GetPropT"
  | GetPrivatePropT _ -> "GetPrivatePropT"
  | GetProtoT _ -> "GetProtoT"
  | GetStaticsT _ -> "GetStaticsT"
  | GuardT _ -> "GuardT"
  | HasOwnPropT _ -> "HasOwnPropT"
  | IdxUnMaybeifyT _ -> "IdxUnMaybeifyT"
  | IdxUnwrap _ -> "IdxUnwrap"
  | ImplementsT _ -> "ImplementsT"
  | ImportDefaultT _ -> "ImportDefaultT"
  | ImportModuleNsT _ -> "ImportModuleNsT"
  | ImportNamedT _ -> "ImportNamedT"
  | ImportTypeofT _ -> "ImportTypeofT"
  | ImportTypeT _ -> "ImportTypeT"
  | IntersectionPreprocessKitT (_, tool) ->
    spf
      "IntersectionPreprocessKitT %s"
      begin
        match tool with
        | ConcretizeTypes _ -> "ConcretizeTypes"
        | SentinelPropTest _ -> "SentinelPropTest"
        | PropExistsTest _ -> "PropExistsTest"
      end
  | InvariantT _ -> "InvariantT"
  | LookupT _ -> "LookupT"
  | MakeExactT _ -> "MakeExactT"
  | MapTypeT _ -> "MapTypeT"
  | MethodT _ -> "MethodT"
  | MixinT _ -> "MixinT"
  | NotT _ -> "NotT"
  | NullishCoalesceT _ -> "NullishCoalesceT"
  | ObjAssignToT _ -> "ObjAssignToT"
  | ObjAssignFromT _ -> "ObjAssignFromT"
  | ObjFreezeT _ -> "ObjFreezeT"
  | ObjRestT _ -> "ObjRestT"
  | ObjSealT _ -> "ObjSealT"
  | ObjTestProtoT _ -> "ObjTestProtoT"
  | ObjTestT _ -> "ObjTestT"
  | OptionalChainT _ -> "OptionalChainT"
  | OrT _ -> "OrT"
  | PredicateT _ -> "PredicateT"
  | ReactKitT _ -> "ReactKitT"
  | RefineT _ -> "RefineT"
  | ReposLowerT _ -> "ReposLowerT"
  | ReposUseT _ -> "ReposUseT"
  | ResolveSpreadT (_, _, { rrt_resolve_to; _ }) ->
    spf
      "ResolveSpreadT(%s)"
      begin
        match rrt_resolve_to with
        | ResolveSpreadsToTuple _ -> "ResolveSpreadsToTuple"
        | ResolveSpreadsToArray _ -> "ResolveSpreadsToArray"
        | ResolveSpreadsToArrayLiteral (id, _, _) -> spf "ResolveSpreadsToArrayLiteral (%d)" id
        | ResolveSpreadsToMultiflowCallFull _ -> "ResolveSpreadsToMultiflowCallFull"
        | ResolveSpreadsToMultiflowSubtypeFull _ -> "ResolveSpreadsToMultiflowSubtypeFull"
        | ResolveSpreadsToCustomFunCall _ -> "ResolveSpreadsToCustomFunCall"
        | ResolveSpreadsToMultiflowPartial _ -> "ResolveSpreadsToMultiflowPartial"
        | ResolveSpreadsToCallT _ -> "ResolveSpreadsToCallT"
      end
  | SentinelPropTestT _ -> "SentinelPropTestT"
  | SetElemT _ -> "SetElemT"
  | SetPropT _ -> "SetPropT"
  | MatchPropT _ -> "MatchPropT"
  | SetPrivatePropT _ -> "SetPrivatePropT"
  | SetProtoT _ -> "SetProtoT"
  | SpecializeT _ -> "SpecializeT"
  | ObjKitT _ -> "ObjKitT"
  | SubstOnPredT _ -> "SubstOnPredT"
  | SuperT _ -> "SuperT"
  | TestPropT _ -> "TestPropT"
  | ThisSpecializeT _ -> "ThisSpecializeT"
  | ToStringT _ -> "ToStringT"
  | UnaryMinusT _ -> "UnaryMinusT"
  | UnifyT _ -> "UnifyT"
  | VarianceCheckT _ -> "VarianceCheckT"
  | TypeAppVarianceCheckT _ -> "TypeAppVarianceCheck"
  | ConcretizeTypeAppsT _ -> "ConcretizeTypeAppsT"
  | CondT _ -> "CondT"
  | ReactPropsToOut _ -> "ReactPropsToOut"
  | ReactInToProps _ -> "ReactInToProps"
  | DestructuringT _ -> "DestructuringT"
  | CreateObjWithComputedPropT _ -> "CreateObjWithComputedPropT"
  | ModuleExportsAssignT _ -> "ModuleExportsAssignT"

let string_of_binary_test = function
  | InstanceofTest -> "instanceof"
  | SentinelProp key -> "sentinel prop " ^ key

let rec string_of_predicate = function
  | AndP (p1, p2) -> string_of_predicate p1 ^ " && " ^ string_of_predicate p2
  | OrP (p1, p2) -> string_of_predicate p1 ^ " || " ^ string_of_predicate p2
  | NotP p -> "not " ^ string_of_predicate p
  | LeftP (b, t) ->
    spf
      "left operand of %s with right operand = %s"
      (string_of_binary_test b)
      (string_of_desc (desc_of_t t))
  | RightP (b, t) ->
    spf
      "right operand of %s with left operand = %s"
      (string_of_binary_test b)
      (string_of_desc (desc_of_t t))
  | ExistsP _ -> "truthy"
  | NullP -> "null"
  | MaybeP -> "null or undefined"
  | SingletonBoolP (_, false) -> "false"
  | SingletonBoolP (_, true) -> "true"
  | SingletonStrP (_, _, str) -> spf "string `%s`" str
  | SingletonNumP (_, _, (_, raw)) -> spf "number `%s`" raw
  (* typeof *)
  | VoidP -> "undefined"
  | BoolP -> "boolean"
  | StrP -> "string"
  | NumP -> "number"
  | FunP -> "function"
  | ObjP -> "object"
  | SymbolP -> "symbol"
  (* Array.isArray *)
  | ArrP -> "array"
  | PropExistsP (key, _) -> spf "prop `%s` is truthy" key
  | LatentP (OpenT (_, id), i) -> spf "LatentPred(TYPE_%d, %d)" id i
  | LatentP (t, i) -> spf "LatentPred(%s, %d)" (string_of_ctor t) i

let name_of_propref = function
  | Named (_, x) -> Some x
  | Computed _ -> None

let reason_of_propref = function
  | Named (r, _) -> r
  | Computed t -> reason_of_t t

and extract_setter_type = function
  | DefT (_, _, FunT (_, _, { params = [(_, param_t)]; _ })) -> param_t
  | _ -> failwith "Setter property with unexpected type"

and extract_getter_type = function
  | DefT (_, _, FunT (_, _, { return_t; _ })) -> return_t
  | _ -> failwith "Getter property with unexpected type"

and tuple_length reason trust ts =
  let r =
    let desc = RTupleLength (List.length ts) in
    replace_desc_reason desc reason
  in
  let t =
    let n = List.length ts in
    let float = Pervasives.float_of_int n in
    let string = Pervasives.string_of_int n in
    SingletonNumT (float, string)
  in
  DefT (r, trust, t)

and elemt_of_arrtype = function
  | ArrayAT (elemt, _)
  | ROArrayAT elemt
  | TupleAT (elemt, _) ->
    elemt

let optional ?annot_loc ?(use_desc = false) t =
  let reason = update_desc_new_reason (fun desc -> ROptional desc) (reason_of_t t) in
  let reason =
    match annot_loc with
    | Some annot_loc -> annot_reason ~annot_loc @@ repos_reason annot_loc reason
    | None -> reason
  in
  OptionalT { reason; type_ = t; use_desc }

let maybe t =
  let reason = update_desc_new_reason (fun desc -> RMaybe desc) (reason_of_t t) in
  MaybeT (reason, t)

let exact t = ExactT (reason_of_t t, t)

let class_type ?(structural = false) ?annot_loc t =
  let reason =
    if structural then
      reason_of_t t
    else
      update_desc_new_reason (fun desc -> RClass desc) (reason_of_t t)
  in
  let reason =
    match annot_loc with
    | Some annot_loc -> annot_reason ~annot_loc @@ repos_reason annot_loc reason
    | None -> reason
  in
  DefT (reason, bogus_trust (), ClassT t)

let this_class_type t =
  let reason = update_desc_new_reason (fun desc -> RClass desc) (reason_of_t t) in
  ThisClassT (reason, t)

let extends_type r l u =
  let reason = update_desc_reason (fun desc -> RExtends desc) r in
  InternalT (ExtendsT (reason, l, u))

let extends_use_type use_op l u =
  let reason = update_desc_new_reason (fun desc -> RExtends desc) (reason_of_t u) in
  ExtendsUseT (use_op, reason, [], l, u)

let poly_type id tparams_loc (tparams : typeparam Nel.t) t =
  let reason = update_desc_new_reason (fun desc -> RPolyType desc) (reason_of_t t) in
  let reason = annot_reason ~annot_loc:(aloc_of_reason reason) reason in
  DefT (reason, bogus_trust (), PolyT { tparams_loc; tparams; t_out = t; id })

let poly_type_of_tparam_list id tparams_loc tparams t =
  match tparams with
  | [] -> t
  | hd :: tl ->
    let tparams_nel = (hd, tl) in
    poly_type id tparams_loc tparams_nel t

let poly_type_of_tparams id (tparams : typeparams) t =
  match tparams with
  | None -> t
  | Some (tparams_loc, tparams_nel) -> poly_type id tparams_loc tparams_nel t

(* The implicit parameter specifies that the application is not a product of some
 * source level type application, but merely a tool for some other functionality,
 * e.g. canonicalize_imported_type in flow_js.ml. *)
let typeapp ?(implicit = false) ?annot_loc t targs =
  let reason =
    update_desc_new_reason
      (fun desc ->
        if implicit then
          RTypeAppImplicit desc
        else
          RTypeApp desc)
      (reason_of_t t)
  in
  let reason =
    match annot_loc with
    | Some annot_loc -> annot_reason ~annot_loc @@ repos_reason annot_loc reason
    | None -> reason
  in
  let use_op = Op (TypeApplication { type' = reason }) in
  TypeAppT (reason, use_op, t, targs)

let this_typeapp ?annot_loc t this targs =
  let reason =
    match targs with
    | Some _ -> update_desc_new_reason (fun desc -> RTypeApp desc) (reason_of_t t)
    | None -> reason_of_t t
  in
  let reason =
    match annot_loc with
    | Some annot_loc -> annot_reason ~annot_loc @@ repos_reason annot_loc reason
    | None -> reason
  in
  ThisTypeAppT (reason, t, this, targs)

let annot use_desc = function
  | OpenT (r, _) as t -> AnnotT (r, t, use_desc)
  | t -> t

let unknown_use = Op UnknownUse

(* The following functions are used as constructors for function types and
   object types, which unfortunately have many fields, not all of which are
   meaningful in all contexts. This part of the design should be revisited:
   perhaps the data types can be refactored to make them more specialized. *)

(* Methods may use a dummy statics object type to carry properties. We do not
   want to encourage this pattern, but we also don't want to block uses of this
   pattern. Thus, we compromise by not tracking the property types. *)
let dummy_static = update_desc_reason (fun desc -> RStatics desc) %> Unsoundness.dummy_static_any

let dummy_prototype = ObjProtoT (locationless_reason RDummyPrototype)

let bound_function_dummy_this = locationless_reason RDummyThis |> Unsoundness.bound_fn_this_any

let dummy_this = locationless_reason RDummyThis |> MixedT.make |> with_trust bogus_trust

let global_this reason =
  let reason = replace_desc_reason (RCustom "global object") reason in
  ObjProtoT reason

let default_obj_assign_kind = ObjAssign { assert_exact = false }

(* A method type is a function type with `this` specified. *)
let mk_methodtype
    this tins ~rest_param ~def_reason ?(frame = 0) ?params_names ?(is_predicate = false) tout =
  {
    this_t = this;
    params =
      (match params_names with
      | None -> Base.List.map ~f:(fun t -> (None, t)) tins
      | Some xs -> List.map2 (fun x t -> (x, t)) xs tins);
    rest_param;
    return_t = tout;
    is_predicate;
    closure_t = frame;
    changeset = Changeset.empty;
    def_reason;
  }

let mk_methodcalltype this targs args ?(frame = 0) ?(call_strict_arity = true) tout =
  {
    call_this_t = this;
    call_targs = targs;
    call_args_tlist = args;
    call_tout = tout;
    call_closure_t = frame;
    call_strict_arity;
  }

(* A bound function type is a function type with `this` = `any`. Typically, such
   a type is given to a method when it can be considered bound: in other words,
   when calling that method through any object would be fine, since the object
   would be ignored. *)
let mk_boundfunctiontype = mk_methodtype bound_function_dummy_this

(* A function type has `this` = `mixed`. Such a type can be given to functions
   that are meant to be called directly. On the other hand, it deliberately
   causes problems when they are given to methods in which `this` is used
   non-trivially: indeed, calling them directly would cause `this` to be bound
   to the global object, which is typically unintended. *)
let mk_functiontype reason = mk_methodtype (global_this reason)

let mk_functioncalltype reason = mk_methodcalltype (global_this reason)

let mk_opt_functioncalltype reason targs args clos strict =
  (global_this reason, targs, args, clos, strict)

(* An object type has two flags, sealed and exact. A sealed object type cannot
   be extended. An exact object type accurately describes objects without
   "forgeting" any properties: so to extend an object type with optional
   properties, the object type must be exact. Thus, as an invariant, "not exact"
   logically implies "sealed" (and by contrapositive, "not sealed" implies
   "exact"; in other words, exact and sealed cannot both be false).

   Types of object literals are exact, but can be sealed or unsealed. Object
   type annotations are sealed but not exact. *)

let default_flags = { sealed = UnsealedInFile None; exact = true; frozen = false }

let mk_objecttype ?(flags = default_flags) ~dict ~call pmap proto =
  { flags; proto_t = proto; props_tmap = pmap; dict_t = dict; call_t = call }

let mk_object_def_type ~reason ?(flags = default_flags) ~dict ~call pmap proto =
  let reason = update_desc_reason invalidate_rtype_alias reason in
  DefT (reason, bogus_trust (), ObjT (mk_objecttype ~flags ~dict ~call pmap proto))

let apply_opt_funcalltype (this, targs, args, clos, strict) t_out =
  {
    call_this_t = this;
    call_targs = targs;
    call_args_tlist = args;
    call_tout = t_out;
    call_closure_t = clos;
    call_strict_arity = strict;
  }

let create_intersection rep = IntersectionT (locationless_reason (RCustom "intersection"), rep)

let apply_opt_use opt_use t_out =
  match opt_use with
  | OptCallT (u, r, f) -> CallT (u, r, apply_opt_funcalltype f t_out)
  | OptGetPropT (u, r, p) -> GetPropT (u, r, p, t_out)
  | OptGetPrivatePropT (u, r, s, cbs, b) -> GetPrivatePropT (u, r, s, cbs, b, t_out)
  | OptTestPropT (r, i, p) -> TestPropT (r, i, p, t_out)
  | OptGetElemT (u, r, t) -> GetElemT (u, r, t, t_out)

let mk_enum_type ~loc ~trust enum =
  let { enum_name; _ } = enum in
  let reason = mk_reason (RType enum_name) loc in
  DefT (reason, trust, EnumT enum)

module TypeParams : sig
  val to_list : typeparams -> typeparam list

  val of_list : ALoc.t -> typeparam list -> typeparams

  val map : (typeparam -> typeparam) -> typeparams -> typeparams
end = struct
  let to_list tparams =
    Option.value_map tparams ~default:[] ~f:(fun (_loc, tparam_nel) -> Nel.to_list tparam_nel)

  let of_list tparams_loc tparams =
    match tparams with
    | [] -> None
    | hd :: tl -> Some (tparams_loc, (hd, tl))

  let map f tparams = Option.map ~f:(fun (loc, params) -> (loc, Nel.map f params)) tparams
end
