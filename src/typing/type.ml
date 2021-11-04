(*
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

type index = int

type sense = bool

type tvar = reason * ident

type number_literal = float * string [@@deriving ord]

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
    | EvalT of t * defer_use_t * Eval.id
    (* bound type variable *)
    | BoundT of reason * string
    | GenericT of {
        reason: reason;
        name: string;
        bound: t;
        id: Generic.id;
      }
    (* this-abstracted class. If `is_this` is true, then this literally comes from
       `this` as an annotation or expression, and should be fixed to an internal
       view of the class, which is a generic whose upper bound is the class. *)
    | ThisClassT of reason * t * (* is_this *) bool
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
    (* constrains some properties of an object *)
    | ShapeT of reason * t
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
    | ModuleT of (reason * exporttypes * bool) (* is_strict *)
    (* Here's to the crazy ones. The misfits. The rebels. The troublemakers.
       The round pegs in the square holes. **)
    (* types that should never appear in signatures *)
    | InternalT of internal_t
    (* upper bound trigger for type destructors *)
    | TypeDestructorTriggerT of use_op * reason * (reason * bool) option * destructor * tvar
    (* Sigil representing functions that the type system is not expressive
       enough to annotate, so we customize their behavior internally. *)
    | CustomFunT of reason * custom_fun_kind (* Predicate types **)
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
    (* TODO StrT should perhaps not allow internal names *)
    | StrT of name literal
    | BoolT of bool option
    | EmptyT
    | MixedT of mixed_flavor
    | NullT
    | VoidT
    | SymbolT
    | FunT of static * prototype * funtype
    | ObjT of objtype
    | ArrT of arrtype
    (* type of a class *)
    | ClassT of t
    (* type of an instance of a class *)
    | InstanceT of static * super * implements * insttype
    (* singleton string, matches exactly a given string literal *)
    (* TODO SingletonStrT should not include internal names *)
    | SingletonStrT of name
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
       parameters generates a type. Universal type parameters may specify subtype
       constraints ("bounds"), which must be satisfied by any types they may be
       substituted by. *)
    | PolyT of {
        tparams_loc: ALoc.t;
        tparams: typeparam Nel.t;
        t_out: t;
        id: Poly.id;
      }
    (* Type that wraps object types for the CustomFunT(Idx) function *)
    | IdxWrapper of t
    (* React$AbstractComponent<Config, Instance> *)
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
    enum_id: ALoc.id;
    members: ALoc.t SMap.t;
    representation_t: t;
    has_unknown_members: bool;
  }

  and internal_t =
    (* toolkit for making choices *)
    | ChoiceKitT of reason * choice_tool
    (* util for deciding subclassing relations *)
    | ExtendsT of reason * t * t
    | ReposUpperT of reason * t

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
        prop: name;
        own_loc: 'loc option;
        proto_loc: 'loc option;
      }
    | ClassMethodDefinition of {
        def: 'loc virtual_reason;
        name: 'loc virtual_reason;
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
    | IndexedTypeAccess of {
        _object: 'loc virtual_reason;
        index: 'loc virtual_reason;
      }
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
    | SwitchCheck of {
        case_test: 'loc virtual_reason;
        switch_discriminant: 'loc virtual_reason;
      }
    | MatchingProp of {
        op: 'loc virtual_reason;
        obj: 'loc virtual_reason;
        key: string;
        sentinel_reason: 'loc virtual_reason;
      }
    | UnknownUse

  and 'loc virtual_frame_use_op =
    | ConstrainedAssignment of {
        name: string;
        declaration: 'loc;
        providers: 'loc virtual_reason list;
      }
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
        prop: name option;
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
        use_op * (* call *) reason * (* lookup *) reason * propref * method_action * t option
    (* Similar to above, but stores information necessary to resolve a private method. *)
    | PrivateMethodT of
        use_op
        * (* call *) reason
        * (* lookup *) reason
        * (* prop *) string
        * class_binding list
        * (* static *) bool
        * method_action
        * (* prop_t *)
        t option
    (* Similar to the last element of the MethodT *)
    | SetPropT of use_op * reason * propref * set_mode * write_ctx * t * t option
    (* The boolean flag indicates whether or not it is a static lookup. We cannot know this when
     * we generate the constraint, since the lower bound may be an unresolved OpenT. If it
     * resolves to a ClassT, we flip the flag to true, which causes us to check the private static
     * fields when the InstanceT ~> SetPrivatePropT constraint is processsed *)
    | SetPrivatePropT of
        use_op * reason * string * set_mode * class_binding list * bool * t * t option
    | GetPropT of use_op * reason * propref * tvar
    (* For shapes *)
    | MatchPropT of use_op * reason * propref * tvar
    (* The same comment on SetPrivatePropT applies here *)
    | GetPrivatePropT of use_op * reason * string * class_binding list * bool * tvar
    | TestPropT of reason * ident * propref * tvar
    (* SetElemT has a `tout` parameter to serve as a trigger for ordering
       operations. We only need this in one place: object literal initialization.
       In particular, a computed property in the object initializer users SetElemT
       to initialize the property value, but in order to avoid race conditions we
       need to ensure that reads happen after writes. *)
    | SetElemT of use_op * reason * t * set_mode * t * t option (*tout *)
    | GetElemT of use_op * reason * t * tvar
    | CallElemT of (* call *) reason * (* lookup *) reason * t * method_action
    | GetStaticsT of tvar
    | GetProtoT of reason * tvar
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
    | ComparatorT of {
        reason: reason;
        flip: bool;
        arg: t;
      }
    (* unary minus operator on numbers, allows negative number literals *)
    | UnaryMinusT of reason * t
    | AssertArithmeticOperandT of reason
    | AssertBinaryInLHST of reason
    | AssertBinaryInRHST of reason
    | AssertForInRHST of reason
    | AssertInstanceofRHST of reason
    | AssertIterableT of {
        use_op: use_op;
        reason: reason;
        async: bool;
        targs: t list;
      }
    (* operation specifying a type refinement via a predicate *)
    | PredicateT of predicate * tvar
    (* like PredicateT, GuardT guards a subsequent flow with a predicate on an
       incoming type. Unlike PredicateT, the subsequent flow (if any) uses
       an arbitrary LB specified in the GuardT value, rather than the filtered
       result of the predicate itself *)
    | GuardT of predicate * t * tvar
    (* === *)
    | StrictEqT of {
        reason: Reason.t;
        cond_context: cond_context option;
        flip: bool;
        arg: t;
      }
    (* == *)
    | EqT of {
        reason: reason;
        flip: bool;
        arg: t;
      }
    (* logical operators *)
    | AndT of reason * t * tvar
    | OrT of reason * t * tvar
    | NullishCoalesceT of reason * t * tvar
    | NotT of reason * tvar
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
    | VarianceCheckT of reason * typeparam SMap.t * t list * Polarity.t
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
    (* LookupT(_, strict, try_ts_on_failure, x, lookup_action, ids) looks for
          property x in an object type and emits a constraint according to the
          provided lookup_action. It also carries with it a list of the prop_map ids it has already tried.

          When x is not found, we have the following cases:

          (1) try_ts_on_failure is not empty, and we try to look for property x in
          the next object type in that list;

          (2) strict = None, so no error is reported;

          (3) strict = Some reason, so the position in reason is blamed.
       **)
    | LookupT of {
        reason: reason;
        lookup_kind: lookup_kind;
        ts: t list;
        propref: propref;
        lookup_action: lookup_action;
        ids: Properties.Set.t option;
        method_accessible: bool;
      } (* operations on objects *)
    (* Resolves the object into which the properties are assigned *)
    | ObjAssignToT of use_op * reason * t * t * obj_assign_kind
    (* Resolves the object from which the properties are assigned *)
    | ObjAssignFromT of use_op * reason * t * t * obj_assign_kind
    | ObjRestT of reason * string list * t * int
    | ObjSealT of reason * t
    (* test that something is a valid proto (object-like or null) *)
    | ObjTestProtoT of reason * t_out
    (* test that something is object-like, returning a default type otherwise *)
    | ObjTestT of reason * t * t
    (* assignment rest element in array pattern *)
    | ArrRestT of use_op * reason * int * t
    (* Guarded unification *)
    | UnifyT of t * t (* bidirectional *)
    (* unifies with incoming concrete lower bound
     * empty_success is a hack that we will likely be able to get rid of once we move to
     * local inference. When empty_success is true, we short circuit on the EmptyT ~> BecomeT
     * flow. This is clearly a bug, but there are too many spurious errors due to typeof when
     * we try to fix it. *)
    | BecomeT of {
        reason: reason;
        t: t;
        empty_success: bool;
      }
    (* Keys *)
    | GetKeysT of reason * use_t
    | HasOwnPropT of use_op * reason * t (* The incoming string that we want to check against *)
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
          (* 't_out' to receive the resolved ModuleT *)
          t_out
    | CopyNamedExportsT of reason * t * t_out
    | CopyTypeExportsT of reason * t * t_out
    | CheckUntypedImportT of reason * import_kind
    | ExportNamedT of
        reason * (ALoc.t option * t) NameUtils.Map.t (* exports_tmap *) * export_kind * t_out
    | ExportTypeT of reason * name (* export_name *) * t (* target_module_t *) * t_out
    | AssertExportIsTypeT of reason * name (* export name *) * t_out
    (* Map a FunT over a structure *)
    | MapTypeT of use_op * reason * type_map * t_out
    | ObjKitT of use_op * reason * Object.resolve_tool * Object.tool * t_out
    | ReactKitT of use_op * reason * React.tool
    | ChoiceKitUseT of reason * choice_use_tool
    (* tools for preprocessing intersections *)
    | IntersectionPreprocessKitT of reason * intersection_preprocess_tool
    | DebugPrintT of reason
    | DebugSleepT of reason
    | SentinelPropTestT of reason * t * string * sense * UnionEnum.star * tvar
    | IdxUnwrap of reason * t_out
    | IdxUnMaybeifyT of reason * t_out
    | OptionalChainT of {
        reason: reason;
        lhs_reason: reason;
        this_t: t;
        t_out: use_t;
        voided_out: t_out;
      }
    | InvariantT of reason (* Function predicate uses *)
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
    | CallLatentPredT of reason * sense * index * t * tvar
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
    | CallOpenPredT of reason * sense * Key.t * t * tvar
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
     * NOTE: this substitution is not used at the moment since we don't yet
     * support subtyping of predicated functions, but the scaffolding might be
     * useful later on.
     *)
    | SubstOnPredT of use_op * reason * substitution * t
    (*
     * `RefineT (reason, pred, tvar)` is an instruction to refine an incoming
     * flow using the predicate `pred`. The result will be stored in `tvar`,
     * which is expected to be a type variable.
     *)
    | RefineT of reason * predicate * tvar
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
    | DestructuringT of reason * destruct_kind * selector * tvar * int
    | CreateObjWithComputedPropT of {
        reason: reason;
        value: t;
        tout_tvar: tvar;
      }
    (* Used to delay union lower bound handling until all the types in the union have been processed themselves *)
    | ResolveUnionT of {
        reason: reason;
        unresolved: t list;
        resolved: t list;
        upper: use_t;
        id: ident;
      }
    | TypeCastT of use_op * t
    | EnumCastT of {
        use_op: use_op;
        enum: reason * Trust.trust_rep * enum_t;
      }
    | EnumExhaustiveCheckT of {
        reason: reason;
        check: enum_possible_exhaustive_check_t;
        incomplete_out: t;
        discriminant_after_check: t option;
      }
    | FilterOptionalT of use_op * t
    | FilterMaybeT of use_op * t
    | FunImplicitVoidReturnT of {
        use_op: use_op;
        reason: reason;
        return: t;
        void_t: t;
      }
    | SealGenericT of {
        reason: reason;
        id: Generic.id;
        name: string;
        cont: cont;
      }
    | OptionalIndexedAccessT of {
        use_op: use_op;
        reason: reason;
        index: optional_indexed_access_index;
        tout_tvar: tvar;
      }

  and enum_check_t =
    | EnumCheck of {
        reason: reason;
        member_name: string;
      }

  (* Valid state transitions are:
   * EnumResolveDiscriminant -> EnumResolveCaseTest (with discriminant info populated)
   * EnumResolveCaseTest -> EnumResolveCaseTest *)
  and enum_exhaustive_check_tool_t =
    | EnumResolveDiscriminant
    | EnumResolveCaseTest of {
        discriminant_enum: enum_t;
        discriminant_reason: reason;
        check: enum_check_t;
      }

  and enum_possible_exhaustive_check_t =
    | EnumExhaustiveCheckPossiblyValid of {
        tool: enum_exhaustive_check_tool_t;
        (* We only convert a "possible check" into a "check" if it has the same
         * enum type as the discriminant. *)
        possible_checks: (t * enum_check_t) list;
        checks: enum_check_t list;
        default_case: reason option;
      }
    | EnumExhaustiveCheckInvalid of reason list

  and cond_context =
    | SwitchTest of {
        case_test_reason: reason;
        switch_discriminant_reason: reason;
      }
    | OtherTest

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
    | OptMethodT of
        use_op * (* call *) reason * (* lookup *) reason * propref * opt_method_action * t option
    | OptPrivateMethodT of
        use_op
        * (* call *) reason
        * (* lookup *) reason
        * string
        * class_binding list
        * bool
        * opt_method_action
        * t option
    | OptGetPropT of use_op * reason * propref
    | OptGetPrivatePropT of use_op * reason * string * class_binding list * bool
    | OptTestPropT of reason * ident * propref
    | OptGetElemT of use_op * reason * t
    | OptCallElemT of (* call *) reason * (* lookup *) reason * t * opt_method_action

  and opt_state =
    | NonOptional
    | NewChain
    | ContinueChain

  and method_action =
    | CallM of methodcalltype
    | ChainM of reason * reason * t * methodcalltype * t_out

  and opt_method_action =
    | OptCallM of opt_methodcalltype
    | OptChainM of reason * reason * t * opt_methodcalltype * t_out

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
    | BoolP of ALoc.t (* boolean *)
    | FunP (* function *)
    | NumP of ALoc.t (* number *)
    | ObjP (* object *)
    | StrP of ALoc.t (* string *)
    | SymbolP of ALoc.t (* symbol *)
    | VoidP (* undefined *)
    | ArrP (* Array.isArray *)
    (* `if (a.b)` yields `flow (a, PredicateT(PropExistsP ("b"), tout))` *)
    | PropExistsP of string * reason
    (* `if (a.b?.c)` yields `flow (a, PredicateT(PropNonMaybeP ("b"), tout))` *)
    | PropNonMaybeP of string * reason
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

  and mixed_flavor =
    | Mixed_everything
    | Mixed_truthy
    | Mixed_non_maybe
    | Mixed_non_null
    | Mixed_non_void
    | Mixed_function

  and any_source =
    | AnnotatedAny
    | AnyError of any_error_kind option
    | Unsound of unsoundness_kind
    | Untyped

  and any_error_kind = UnresolvedName

  (* Tracks the kinds of unsoundness inherent in Flow. If you can't find a kind that matches
     your use case, make one *)
  and unsoundness_kind =
    | BoundFunctionThis
    | ComputedNonLiteralKey
    | Constructor
    | DummyStatic
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
  (* FunTs carry around two `this` types, one to be used during subtyping and
     one to be treated as the param when the function is called. This is to allow
     more lenient subtyping between class methods without sacrificing soundness
     when calling functions. If subtype_this_t is None, param_this_t is used
     for both operations *)
  and funtype = {
    this_t: t * bool;
    (* use for subtyping? *)
    params: fun_param list;
    rest_param: fun_rest_param option;
    return_t: t;
    is_predicate: bool;
    def_reason: Reason.t;
  }

  (* Used by CallT and similar constructors *)
  and funcalltype = {
    call_this_t: t;
    call_targs: targ list option;
    call_args_tlist: call_arg list;
    call_tout: tvar;
    call_strict_arity: bool;
  }

  and methodcalltype = {
    meth_generic_this: t option;
    meth_targs: targ list option;
    meth_args_tlist: call_arg list;
    meth_tout: tvar;
    meth_strict_arity: bool;
  }

  and targ =
    (* This tvar gets lower bounds from the instantiations of _. It is used to power type-services
     * like type-at-pos and should not be used for type checking
     *)
    | ImplicitArg of tvar
    | ExplicitArg of t

  and opt_funcalltype = t * targ list option * call_arg list * bool

  and opt_methodcalltype = {
    opt_meth_generic_this: t option;
    opt_meth_targs: targ list option;
    opt_meth_args_tlist: call_arg list;
    opt_meth_strict_arity: bool;
  }

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
        own: property NameUtils.Map.t;
        proto: property NameUtils.Map.t;
        static: property NameUtils.Map.t;
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
        tout: tvar;
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
    | ReadElem of tvar
    | WriteElem of t * t option (* tout *) * set_mode
    | CallElem of reason * method_action

  and propref =
    | Named of reason * name
    | Computed of t

  and obj_kind =
    | Exact
    | Inexact
    | Indexed of dicttype
    | UnsealedInFile of File_key.t option

  and flags = {
    frozen: bool;
    obj_kind: obj_kind;
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
    class_binding_id: ALoc.id;
    class_private_fields: Properties.id;
    class_private_static_fields: Properties.id;
    class_private_methods: Properties.id;
    class_private_static_methods: Properties.id;
  }

  and insttype = {
    class_id: ALoc.id;
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
    opaque_id: ALoc.id;
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
    is_this: bool;
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
    | PropertyType of {
        name: name;
        (* For type normalizer purposes - in the future PropertyType will be removed. *)
        is_indexed_access: bool;
      }
    | ElementType of {
        index_type: t;
        (* For type normalizer purposes - in the future ElementType will be removed. *)
        is_indexed_access: bool;
      }
    | OptionalIndexedAccessNonMaybeType of { index: optional_indexed_access_index }
    | OptionalIndexedAccessResultType of { void_reason: reason }
    | Bind of t
    | ReadOnlyType
    | PartialType
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

  and optional_indexed_access_index =
    | OptionalIndexedAccessStrLitIndex of name
    | OptionalIndexedAccessTypeIndex of t

  and type_map =
    | TupleMap of t
    | ObjectMap of t
    | ObjectMapi of t
    | ObjectKeyMirror
    | ObjectMapConst of t

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
    | SentinelPropTest of bool * string * t * t * tvar
    | PropExistsTest of bool * string * reason * t * tvar * (predicate * predicate)

  and spec =
    | UnionCases of use_op * t * UnionRep.t * t list
    | IntersectionCases of t list * use_t

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
    | UnresolvedArg of t * Generic.id option
    | UnresolvedSpreadArg of t

  and resolved_param =
    | ResolvedArg of t * Generic.id option
    | ResolvedSpreadArg of reason * arrtype * Generic.id option
    | ResolvedAnySpreadArg of reason

  and spread_resolve =
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
    | ImportEnumKind
    | InstanceKind

  (*
     Terminology:

      * A step records a single test of lower bound against
      upper bound, analogous to an invocation of the flow function.

      * A step may have a tvar as its lower or upper bound (or both).
      tvars act as conduits for concrete types, so steps which
      begin or end in tvars may be joined with other steps
      representing tests which adjoin the same tvar.

      The resulting sequence of steps, corresponding to an invocation
      of the flow function followed by the extension of the original
      lower/upper pair through any adjacent type variables, forms the
      basis of a trace. (In trace dumps this is called a "path".)

      * When a step has been induced recursively from a prior invocation
      of the flow function, it's said to have the trace associated with
      that invocation as a parent.

      (Note that each step in a path may have its own parent: consider
      an incoming, recursively induced step joining with a dormant step
      attached to some tvar in an arbitrarily removed invocation of the
      flow function.)

      * A trace is just a sequence of steps along with a (possibly empty)
      parent trace for each step. Since steps may share parents,
      a trace forms a graph, though it is naturally built up as a tree
      when recorded during evaluation of the flow function.
      (The formatting we do in reasons_of_trace recovers the graph
      structure for readability.)
  *)
  type trace_step =
    | Step of {
        lower: t;
        upper: use_t;
        parent: trace_step list;
      }

  (* A list of steps and the depth of the trace, trace depth is 1 + the length of
     the longest ancestor chain in the trace. We keep this precomputed because

     a) actual ancestors may be thrown away due to externally imposed limits on trace
        depth;

     b) the recursion limiter in the flow function checks this on every call.
  *)
  type trace = trace_step list * int
end =
  TypeTerm

and UnionEnum : sig
  type t =
    (* TODO this should not allow internal names *)
    | Str of name
    | Num of number_literal
    | Bool of bool
    | Void
    | Null
  [@@deriving ord]

  type star =
    | One of t
    | Many of UnionEnumSet.t
end = struct
  type t =
    | Str of name
    | Num of number_literal
    | Bool of bool
    | Void
    | Null
  [@@deriving ord]

  type star =
    | One of t
    | Many of UnionEnumSet.t
end

and UnionEnumSet : (Flow_set.S with type elt = UnionEnum.t) = Flow_set.Make (UnionEnum)

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
  type t = Property.t NameUtils.Map.t

  type id

  module Map : WrappedMap.S with type key = id

  module Set : Flow_set.S with type elt = id

  type map = t Map.t

  val add_field : name -> Polarity.t -> ALoc.t option -> TypeTerm.t -> t -> t

  val add_getter : name -> ALoc.t option -> TypeTerm.t -> t -> t

  val add_setter : name -> ALoc.t option -> TypeTerm.t -> t -> t

  val add_method : name -> ALoc.t option -> TypeTerm.t -> t -> t

  val generate_id : unit -> id

  val id_of_int : int -> id

  val id_as_int : id -> int option

  val id_of_aloc_id : ALoc.id -> id

  val fake_id : id

  val string_of_id : id -> string

  val extract_named_exports : t -> Exports.t

  val iter_t : (TypeTerm.t -> unit) -> t -> unit

  val map_t : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val map_fields : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val mapi_fields : (name -> TypeTerm.t -> TypeTerm.t) -> t -> t
end = struct
  open TypeTerm

  type t = Property.t NameUtils.Map.t

  include Source_or_generated_id

  module Map : WrappedMap.S with type key = id = WrappedMap.Make (struct
    type t = id

    let compare = compare_id
  end)

  module Set : Flow_set.S with type elt = id = Flow_set.Make (struct
    type t = id

    let compare = compare_id
  end)

  type map = t Map.t

  let add_field x polarity loc t = NameUtils.Map.add x (Field (loc, t, polarity))

  let add_getter x loc get_t map =
    NameUtils.Map.adjust
      x
      (function
        | Some (Set (set_loc, set_t)) -> GetSet (loc, get_t, set_loc, set_t)
        | _ -> Get (loc, get_t))
      map

  let add_setter x loc set_t map =
    NameUtils.Map.adjust
      x
      (function
        | Some (Get (get_loc, get_t)) -> GetSet (get_loc, get_t, loc, set_t)
        | _ -> Set (loc, set_t))
      map

  let add_method x loc t = NameUtils.Map.add x (Method (loc, t))

  let fake_id = id_of_int 0

  let extract_named_exports pmap =
    NameUtils.Map.fold
      (fun x p tmap ->
        match Property.read_t p with
        | Some t -> NameUtils.Map.add x (Property.read_loc p, t) tmap
        | None -> tmap)
      pmap
      NameUtils.Map.empty

  let iter_t f = NameUtils.Map.iter (fun _ -> Property.iter_t f)

  let map_t f = NameUtils.Map.map (Property.map_t f)

  let map_fields f =
    NameUtils.Map.map (function
        | Field (loc, t, polarity) -> Field (loc, f t, polarity)
        | p -> p
        )

  let mapi_fields f =
    NameUtils.Map.mapi (fun k -> function
      | Field (loc, t, polarity) -> Field (loc, f k t, polarity)
      | p -> p
    )
end

and Eval : sig
  type id

  val compare_id : id -> id -> int

  val id_of_int : int -> id

  val id_as_int : id -> int option

  val id_of_aloc_id : ALoc.id -> id

  val string_of_id : id -> string

  val generate_id : unit -> id

  val equal_id : id -> id -> bool

  module Map : WrappedMap.S with type key = id
end = struct
  include Source_or_generated_id

  module Map : WrappedMap.S with type key = id = WrappedMap.Make (struct
    type key = id

    type t = key

    let compare = compare_id
  end)
end

and Poly : sig
  type id

  val compare_id : id -> id -> int

  val equal_id : id -> id -> bool

  val id_of_int : int -> id

  val id_as_int : id -> int option

  val id_of_aloc_id : ALoc.id -> id

  val string_of_id : id -> string

  val generate_id : unit -> id

  module Set : Flow_set.S with type elt = id
end = struct
  include Source_or_generated_id

  module Set : Flow_set.S with type elt = id = Flow_set.Make (struct
    type elt = id

    type t = elt

    let compare = compare_id
  end)
end

and Exports : sig
  type t = (ALoc.t option * TypeTerm.t) NameUtils.Map.t

  type id

  module Map : WrappedMap.S with type key = id

  type map = t Map.t

  val mk_id : unit -> id

  val string_of_id : id -> string
end = struct
  type t = (ALoc.t option * TypeTerm.t) NameUtils.Map.t

  type id = int

  module Map : WrappedMap.S with type key = id = WrappedMap.Make (struct
    type key = id

    type t = key

    let compare (x : int) y = Stdlib.compare x y
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
  val make : TypeTerm.t -> TypeTerm.t -> TypeTerm.t list -> t

  (** replace reason with specialized desc, if any *)
  val specialized_reason : reason_of_t:(TypeTerm.t -> reason) -> reason -> t -> reason

  (** members in declaration order *)
  val members : t -> TypeTerm.t list

  val members_nel : t -> TypeTerm.t * TypeTerm.t Nel.t

  val cons : TypeTerm.t -> t -> t

  val rev_append : t -> t -> t

  (** map rep r to rep r' along type mapping f. if nothing would be changed,
      returns the physically-identical rep. *)
  val ident_map : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val optimize :
    t ->
    reasonless_eq:(TypeTerm.t -> TypeTerm.t -> bool) ->
    flatten:(TypeTerm.t list -> TypeTerm.t list) ->
    find_resolved:(TypeTerm.t -> TypeTerm.t option) ->
    find_props:(Properties.id -> TypeTerm.property NameUtils.Map.t) ->
    unit

  val is_optimized_finally : t -> bool

  (** quick membership tests for enums and disjoint unions *)
  type quick_mem_result =
    | Yes
    | No
    | Conditional of TypeTerm.t
    | Unknown

  val join_quick_mem_results : quick_mem_result * quick_mem_result -> quick_mem_result

  val quick_mem_enum :
    quick_subtype:(TypeTerm.t -> TypeTerm.t -> bool) -> TypeTerm.t -> t -> quick_mem_result

  val quick_mem_disjoint_union :
    find_resolved:(TypeTerm.t -> TypeTerm.t option) ->
    find_props:(Properties.id -> TypeTerm.property NameUtils.Map.t) ->
    quick_subtype:(TypeTerm.t -> TypeTerm.t -> bool) ->
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
      | _ -> None
    )

  let is_base =
    TypeTerm.(
      function
      | DefT (_, _, SingletonStrT _)
      | DefT (_, _, SingletonNumT _)
      | DefT (_, _, SingletonBoolT _)
      | DefT (_, _, VoidT)
      | DefT (_, _, NullT) ->
        true
      | _ -> false
    )

  (* disjoint unions are stored as singleton type maps *)
  module UnionEnumMap = WrappedMap.Make (UnionEnum)

  type finally_optimized_rep =
    | UnionEnum of UnionEnumSet.t
    | PartiallyOptimizedUnionEnum of UnionEnumSet.t
    | DisjointUnion of TypeTerm.t UnionEnumMap.t NameUtils.Map.t
    | PartiallyOptimizedDisjointUnion of TypeTerm.t UnionEnumMap.t NameUtils.Map.t
    | Empty
    | Singleton of TypeTerm.t
    | Unoptimized

  (** union rep is:
      - list of members in declaration order, with at least 2 elements
      - if union is an enum (set of singletons over a common base)
        then Some (base, set)
        (additional specializations probably to come)
   *)
  type t = TypeTerm.t * TypeTerm.t * TypeTerm.t list * finally_optimized_rep option ref

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
        Base.Option.(mk_enum UnionEnumSet.empty (t0 :: t1 :: ts) >>| fun tset -> UnionEnum tset)
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

  let specialized_reason ~reason_of_t r (_, _, _, specialization) =
    match !specialization with
    | Some Empty -> replace_desc_reason REmpty r
    | Some (Singleton t) -> reason_of_t t
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
        | _ -> true
      )

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

  let canon_prop find_resolved p = Base.Option.(Property.read_t p >>= find_resolved >>= canon)

  let base_prop find_resolved p =
    match Base.Option.(Property.read_t p >>= find_resolved) with
    | Some t when is_base t -> canon t
    | _ -> None

  let props_of find_props t =
    TypeTerm.(
      match t with
      | DefT (_, _, ObjT { props_tmap; _ })
      | ExactT (_, DefT (_, _, ObjT { props_tmap; _ })) ->
        Some (find_props props_tmap)
      | _ -> None
    )

  let disjoint_union_optimize =
    let base_props_of find_resolved find_props t =
      Base.Option.(
        props_of find_props t >>| fun prop_map ->
        NameUtils.Map.fold
          (fun key p acc ->
            match base_prop find_resolved p with
            | Some enum -> NameUtils.Map.add key (enum, t) acc
            | _ -> acc)
          prop_map
          NameUtils.Map.empty
      )
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
      let rec unique_values ~reasonless_eq idx = function
        | [] -> Some idx
        | (enum, t) :: values ->
          begin
            match UnionEnumMap.find_opt enum idx with
            | None -> unique_values ~reasonless_eq (UnionEnumMap.add enum t idx) values
            | Some t' ->
              if reasonless_eq t t' then
                unique_values ~reasonless_eq idx values
              else
                None
          end
      in
      unique_values UnionEnumMap.empty
    in
    let unique ~reasonless_eq idx =
      NameUtils.Map.fold
        (fun key values acc ->
          match unique_values ~reasonless_eq values with
          | None -> acc
          | Some idx -> NameUtils.Map.add key idx acc)
        idx
        NameUtils.Map.empty
    in
    let index ~reasonless_eq candidates =
      match candidates with
      | [] -> NameUtils.Map.empty
      | base_props :: candidates ->
        (* Compute the intersection of properties of objects that have singleton types *)
        let init = NameUtils.Map.map (fun enum_t -> [enum_t]) base_props in
        let idx =
          List.fold_left
            (fun acc base_props ->
              NameUtils.Map.merge
                (fun _key enum_t_opt values_opt ->
                  Base.Option.(
                    both enum_t_opt values_opt >>| fun (enum_t, values) -> List.cons enum_t values
                  ))
                base_props
                acc)
            init
            candidates
        in
        (* Ensure that enums map to unique types *)
        unique ~reasonless_eq idx
    in
    fun ~reasonless_eq ~find_resolved ~find_props -> function
      | [] -> Empty
      | [t] -> Singleton t
      | ts ->
        let (candidates, partial) = split_disjoint_union find_resolved find_props ts in
        let map = index ~reasonless_eq candidates in
        if NameUtils.Map.is_empty map then
          Unoptimized
        else if partial then
          PartiallyOptimizedDisjointUnion map
        else
          DisjointUnion map

  let optimize rep ~reasonless_eq ~flatten ~find_resolved ~find_props =
    let ts = flatten (members rep) in
    if contains_only_flattened_types ts then
      let opt = enum_optimize ts in
      let opt =
        match opt with
        | Unoptimized -> disjoint_union_optimize ~reasonless_eq ~find_resolved ~find_props ts
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
  let quick_mem_enum ~quick_subtype l (_t0, _t1, _ts, specialization) =
    match canon l with
    | Some tcanon ->
      begin
        match !specialization with
        | None -> Unknown
        | Some Unoptimized -> Unknown
        | Some Empty -> No
        | Some (Singleton t) ->
          if quick_subtype l t then
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
    NameUtils.Map.fold
      (fun key idx acc ->
        if acc <> Unknown then
          acc
        else
          match NameUtils.Map.find_opt key prop_map with
          | Some p ->
            begin
              match canon_prop find_resolved p with
              | Some enum ->
                begin
                  match UnionEnumMap.find_opt enum idx with
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
      ~find_resolved ~find_props ~quick_subtype l (_t0, _t1, _ts, specialization) =
    match props_of find_props l with
    | Some prop_map ->
      begin
        match !specialization with
        | None -> Unknown
        | Some Unoptimized -> Unknown
        | Some Empty -> No
        | Some (Singleton t) ->
          if quick_subtype l t then
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

  (** build rep from list of members *)
  val make : TypeTerm.t -> TypeTerm.t -> TypeTerm.t list -> t

  (** member list in declaration order *)
  val members : t -> TypeTerm.t list

  val members_nel : t -> TypeTerm.t * TypeTerm.t Nel.t

  (** map rep r to rep r' along type mapping f. drops history *)
  val map : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val append : TypeTerm.t list -> t -> t

  (** map rep r to rep r' along type mapping f. drops history. if nothing would
      be changed, returns the physically-identical rep. *)
  val ident_map : (TypeTerm.t -> TypeTerm.t) -> t -> t
end = struct
  (** intersection rep is:
      - member list in declaration order
   *)
  type t = TypeTerm.t * TypeTerm.t * TypeTerm.t list

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
    flags: TypeTerm.flags;
    generics: Generic.spread_id;
    interface: (TypeTerm.static * TypeTerm.insttype) option;
  }

  and props = prop NameUtils.Map.t

  and prop = TypeTerm.t * bool * (* method *) bool

  (* own *)
  and dict = TypeTerm.dicttype option

  module Spread : sig
    (* This is the type we feed into SpreadType to be processed by object_kit. It's different
     * than slice because object_kit processes the properties in ways that do not need to
     * be exposed to other files. *)
    type operand_slice = {
      reason: reason;
      prop_map: Properties.t;
      generics: Generic.spread_id;
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

    type sealtype =
      | UnsealedInFile of File_key.t option
      | Sealed
      | Frozen

    type target =
      (* When spreading values, the result is exact if all of the input types are
         also exact. If any input type is inexact, the output is inexact. *)
      | Value of { make_seal: sealtype }
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
    | Partial
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
      | Primitive of is_required * TypeTerm.t
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

  type resolved_object = reason * Properties.t * TypeTerm.flags

  type resolve_object =
    | ResolveObject
    | ResolveDict of TypeTerm.dicttype * Properties.t * resolved_object
    | ResolveProp of name * Properties.t * resolved_object

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

let unknown_use = TypeTerm.(Op UnknownUse)

let name_of_propref = function
  | TypeTerm.Named (_, x) -> Some x
  | TypeTerm.Computed _ -> None

external use_t_compare : TypeTerm.use_t -> TypeTerm.use_t -> int = "caml_fast_generic_compare"
  [@@noalloc]

external type_term_compare : TypeTerm.t -> TypeTerm.t -> int = "caml_fast_generic_compare"
  [@@noalloc]

module UseTypeSet : Flow_set.S with type elt = TypeTerm.use_t = Flow_set.Make (struct
  type elt = TypeTerm.use_t

  type t = elt

  let compare = use_t_compare
end)

(* The typechecking algorithm often needs to maintain sets of types, or more
   generally, maps of types (for logging we need to associate some provenanced
   information to types).
   Type terms may also contain internal sets or maps.
*)
module TypeSet : Flow_set.S with type elt = TypeTerm.t = Flow_set.Make (struct
  type elt = TypeTerm.t

  type t = elt

  let compare = type_term_compare
end)

module TypeMap = Flow_map.Make (struct
  type key = TypeTerm.t

  type t = key

  let compare = type_term_compare
end)

module Constraint = struct
  module UseTypeKey = struct
    type speculation_id = int

    type case_id = int

    type t = TypeTerm.use_t * (speculation_id * case_id) option

    let compare (x, assoc1) (y, assoc2) =
      let v = [%derive.ord: (int * int) option] assoc1 assoc2 in
      if v = 0 then
        use_t_compare x y
      else
        v
  end

  module UseTypeMap = Flow_map.Make (UseTypeKey)

  (** Constraints carry type information that narrows down the possible solutions
      of tvar, and are of two kinds:

      - A Resolved constraint contains a concrete type that is considered by the
        type system to be the solution of the tvar carrying the constraint. In other
        words, the tvar is equivalent to this concrete type in all respects.

      - Unresolved constraints contain bounds that carry both concrete types and
        other tvars as upper and lower bounds (see below). *)
  type constraints =
    | Resolved of TypeTerm.use_op * TypeTerm.t
    | Unresolved of bounds
    | FullyResolved of TypeTerm.use_op * TypeTerm.t Lazy.t

  (** The bounds structure carries the evolving constraints on the solution of an
      unresolved tvar.

      - upper and lower hold concrete upper and lower bounds, respectively. At any
        point in analysis the aggregate lower bound of a tvar is (conceptually) the
        union of the concrete types in lower, and the aggregate upper bound is
        (conceptually) the intersection of the concrete types in upper. (Upper and
        lower are maps, with the types as keys, and trace information as values.)

      - lowertvars and uppertvars hold tvars which are also (latent) lower and
        upper bounds, respectively. See the __flow function for how these structures
        are populated and operated on.  Here the map keys are tvar ids, with trace
        info as values.

      The use_op in the lower TypeMap represents the use_op when a lower bound
      was added. *)
  and bounds = {
    mutable lower: (TypeTerm.trace * TypeTerm.use_op) TypeMap.t;
    mutable upper: TypeTerm.trace UseTypeMap.t;
    mutable lowertvars: (TypeTerm.trace * TypeTerm.use_op) IMap.t;
    mutable uppertvars: (TypeTerm.trace * TypeTerm.use_op) IMap.t;
  }

  include Union_find.Make (struct
    type t = constraints
  end)

  let new_bounds () =
    {
      lower = TypeMap.empty;
      upper = UseTypeMap.empty;
      lowertvars = IMap.empty;
      uppertvars = IMap.empty;
    }

  let new_unresolved_root () =
    let constraints = Lazy.from_val (Unresolved (new_bounds ())) in
    Root { rank = 0; constraints }

  (* For any constraints, return a list of def types that form either the lower
     bounds of the solution, or a singleton containing the solution itself. *)
  let types_of : constraints -> TypeTerm.t list = function
    | Unresolved { lower; _ } -> TypeMap.keys lower
    | Resolved (_, t)
    | FullyResolved (_, (lazy t)) ->
      [t]

  let uses_of : constraints -> TypeTerm.use_t list = function
    | Unresolved { upper; _ } -> Base.List.map ~f:fst (UseTypeMap.keys upper)
    | Resolved (use_op, t)
    | FullyResolved (use_op, (lazy t)) ->
      [TypeTerm.UseT (use_op, t)]

  let fully_resolved_node t =
    Root { rank = 0; constraints = lazy (FullyResolved (unknown_use, lazy t)) }
end

(**************************)
(* Annotation constraints *)
(**************************)
module AConstraint = struct
  type op =
    (* Imports *)
    | Annot_ImportNamedT of Reason.t * TypeTerm.import_kind * string * string * bool
    | Annot_ImportModuleNsT of Reason.t * bool
    | Annot_ImportTypeT of Reason.reason * string
    | Annot_ImportTypeofT of Reason.reason * string
    | Annot_ImportDefaultT of Reason.t * TypeTerm.import_kind * (string * string) * bool
    | Annot_CJSRequireT of Reason.t * bool
    (* Exports *)
    | Annot_CJSExtractNamedExportsT of Reason.t * (Reason.t * TypeTerm.exporttypes * bool)
    | Annot_ExportNamedT of
        Reason.t * (ALoc.t option * TypeTerm.t) NameUtils.Map.t * TypeTerm.export_kind
    | Annot_ExportTypeT of Reason.t * Reason.name * TypeTerm.t
    | Annot_AssertExportIsTypeT of Reason.t * name
    | Annot_CopyNamedExportsT of Reason.t * TypeTerm.t
    | Annot_CopyTypeExportsT of Reason.t * TypeTerm.t
    (* Other operations *)
    | Annot_SpecializeT of TypeTerm.use_op * Reason.t * Reason.t * TypeTerm.t list option
    | Annot_ThisSpecializeT of Reason.t * TypeTerm.t
    | Annot_UseT_TypeT of Reason.t
    | Annot_GetPropT of Reason.t * TypeTerm.use_op * TypeTerm.propref
    | Annot_GetElemT of Reason.t * TypeTerm.use_op * TypeTerm.t (* key *)
    | Annot_ElemT of Reason.t * TypeTerm.use_op * TypeTerm.t (* read action only *)
    | Annot_GetStaticsT of Reason.t
    | Annot_LookupT of Reason.t * TypeTerm.use_op * TypeTerm.propref
    | Annot_MakeExactT of Reason.t
    | Annot_ObjKitT of Reason.t * TypeTerm.use_op * Object.resolve_tool * Object.tool
    | Annot_ObjTestProtoT of Reason.t
    | Annot_MixinT of Reason.t
    | Annot_UnaryMinusT of Reason.t
    | Annot_NotT of Reason.t
    | Annot_ObjKeyMirror of Reason.t
    | Annot_ObjMapConst of Reason.t * TypeTerm.t
    | Annot_GetKeysT of Reason.t
    | Annot_ToStringT of Reason.t
    | Annot_ObjRestT of Reason.t * string list
    | Annot__Future_added_value__ of Reason.t

  (** This kind of constraint is meant to represent type annotations. Unlike the
      constraints described above that may gradually evolve towards the solution
      of an unresolved tvar, annotation constraints are resolved immediately.

      There are three kinds of constraints:

      - [Annot_unresolved _] is a constraint for which we have no information yet.
        This state is associated, for example, with a type variable representing
        a type alias that we have just started visiting, and so do not have a
        type for its body yet.

      - [Annot_op { op; id; _ }] expresses the fact that the current variable is the
        result of applying the opereation [op] on the type that another annotation
        variable [id] will resolve to.

      - [Annot_resolved] is the state of a fully-resolved annotation variable.

      An annotation variable starts off in the Annot_unresolved or Annot_op state
      and is resolved to the Annot_resolved state in one step.

      As inference proceeds other types can depend on the current annotation variable
      through an Annot_op constraint. This fact is recorded in the [dependents] set
      that is maintained for every unresolved annotation variable. It is important
      to keep this information around, so we can force these dependents into
      evaluation when the current variable becomes resolved (see resolve_id in
      annotation_inference.ml). An implied invariant is that all variables in the
      dependent set are of the Annot_op kind.

      While in one of the two initial states the respective annotation variable
      ids are only present in the annotation graph. Once resolved, the resolved
      type is immediately stored in the main type graph as FullyResolved constraint.

      In rare cases like

        // file rec-export.js
        import {p} from './rec-export';
        export {p};

      it is possible for a variable to never become resolved. This is the equivalent
      of a type variable (from above) to never accumulate any lower bounds. When
      such constraints get detected, the result is immediately replaced with 'any',
      which is similar to how we handle the above constraints, when that type
      variable is exported.
  *)
  type constraints =
    | Annot_unresolved of {
        reason: Reason.t;
        mutable dependents: ISet.t;
      }
    | Annot_op of {
        op: op;
        id: int;
        mutable dependents: ISet.t;
      }
    | Annot_resolved

  let string_of_operation = function
    | Annot_SpecializeT _ -> "Annot_SpecializeT"
    | Annot_ThisSpecializeT _ -> "Annot_ThisSpecializeT"
    | Annot_UseT_TypeT _ -> "Annot_UseT_TypeT"
    | Annot_CJSRequireT _ -> "Annot_CJSRequireT"
    | Annot_ImportTypeT _ -> "Annot_ImportTypeT"
    | Annot_ImportTypeofT _ -> "Annot_ImportTypeofT"
    | Annot_ImportNamedT _ -> "Annot_ImportNamedT"
    | Annot_ImportDefaultT _ -> "Annot_ImportDefaultT"
    | Annot_ImportModuleNsT _ -> "Annot_ImportModuleNsT"
    | Annot_CJSExtractNamedExportsT _ -> "Annot_CJSExtractNamedExportsT"
    | Annot_ExportNamedT _ -> "Annot_ExportNamedT"
    | Annot_ExportTypeT _ -> "Annot_ExportTypeT"
    | Annot_AssertExportIsTypeT _ -> "Annot_AssertExportIsTypeT"
    | Annot_CopyNamedExportsT _ -> "Annot_CopyNamedExportsT"
    | Annot_CopyTypeExportsT _ -> "Annot_CopyTypeExportsT"
    | Annot_GetPropT _ -> "Annot_GetPropT"
    | Annot_GetElemT _ -> "Annot_GetElemT"
    | Annot_ElemT _ -> "Annot_ElemT"
    | Annot_GetStaticsT _ -> "Annot_GetStaticsT"
    | Annot_LookupT _ -> "Annot_LookupT"
    | Annot_MakeExactT _ -> "Annot_MakeExactT"
    | Annot_ObjKitT _ -> "Annot_ObjKitT"
    | Annot_ObjTestProtoT _ -> "Annot_ObjTestProtoT"
    | Annot_MixinT _ -> "Annot_MixinT"
    | Annot_UnaryMinusT _ -> "Annot_UnaryMinusT"
    | Annot_NotT _ -> "Annot_NotT"
    | Annot_ObjKeyMirror _ -> "Annot_ObjKeyMirror"
    | Annot_ObjMapConst _ -> "Annot_ObjMapConst"
    | Annot_GetKeysT _ -> "Annot_GetKeysT"
    | Annot_ToStringT _ -> "Annot_ToStringT"
    | Annot_ObjRestT _ -> "Annot_ObjRestT"
    | Annot__Future_added_value__ _ -> "Annot__Future_added_value__"

  let reason_of_op = function
    | Annot_SpecializeT (_, r, _, _)
    | Annot_ThisSpecializeT (r, _)
    | Annot_UseT_TypeT r
    | Annot_CJSRequireT (r, _)
    | Annot_ImportTypeT (r, _)
    | Annot_ImportTypeofT (r, _)
    | Annot_ImportNamedT (r, _, _, _, _)
    | Annot_ImportDefaultT (r, _, _, _)
    | Annot_ImportModuleNsT (r, _)
    | Annot_CJSExtractNamedExportsT (r, _)
    | Annot_ExportNamedT (r, _, _)
    | Annot_ExportTypeT (r, _, _)
    | Annot_AssertExportIsTypeT (r, _)
    | Annot_CopyNamedExportsT (r, _)
    | Annot_CopyTypeExportsT (r, _)
    | Annot_GetPropT (r, _, _)
    | Annot_GetElemT (r, _, _)
    | Annot_ElemT (r, _, _)
    | Annot_GetStaticsT r
    | Annot_LookupT (r, _, _)
    | Annot_MakeExactT r
    | Annot_ObjKitT (r, _, _, _)
    | Annot_ObjTestProtoT r
    | Annot_UnaryMinusT r
    | Annot_NotT r
    | Annot_MixinT r
    | Annot_ObjKeyMirror r
    | Annot_ObjMapConst (r, _)
    | Annot_GetKeysT r
    | Annot_ToStringT r
    | Annot_ObjRestT (r, _)
    | Annot__Future_added_value__ r ->
      r

  let use_op_of_operation = function
    | Annot_SpecializeT (use_op, _, _, _)
    | Annot_GetPropT (_, use_op, _)
    | Annot_GetElemT (_, use_op, _)
    | Annot_ElemT (_, use_op, _)
    | Annot_LookupT (_, use_op, _)
    | Annot_ObjKitT (_, use_op, _, _) ->
      Some use_op
    | Annot_ThisSpecializeT _
    | Annot_UseT_TypeT _
    | Annot_CJSRequireT _
    | Annot_ImportTypeT _
    | Annot_ImportTypeofT _
    | Annot_ImportNamedT _
    | Annot_ImportDefaultT _
    | Annot_ImportModuleNsT _
    | Annot_CJSExtractNamedExportsT _
    | Annot_ExportNamedT _
    | Annot_ExportTypeT _
    | Annot_AssertExportIsTypeT _
    | Annot_CopyNamedExportsT _
    | Annot_CopyTypeExportsT _
    | Annot_GetStaticsT _
    | Annot_MakeExactT _
    | Annot_ObjTestProtoT _
    | Annot_UnaryMinusT _
    | Annot_NotT _
    | Annot_MixinT _
    | Annot_ObjKeyMirror _
    | Annot_ObjMapConst _
    | Annot_GetKeysT _
    | Annot_ToStringT _
    | Annot_ObjRestT _
    | Annot__Future_added_value__ _ ->
      None

  (* Used to produce prettier error messages for annotation inference. *)
  let display_reason_of_op = function
    | Annot_ObjKitT (r, _, _, tool) ->
      let desc =
        RCustom
          Object.(
            match tool with
            | ReadOnly -> "readonly"
            | Partial -> "partial"
            | Spread _ -> "spread"
            | Rest _ -> "rest"
            | ReactConfig _ -> "react config"
            | ObjectRep -> "object"
            | ObjectWiden _ -> "widening"
          )
      in
      replace_desc_reason desc r
    | Annot_MakeExactT r -> replace_desc_reason (RCustom "exact") r
    | Annot_GetStaticsT r -> replace_desc_reason (RCustom "statics") r
    | Annot_MixinT r -> replace_desc_reason (RCustom "mixins") r
    | Annot_UnaryMinusT r -> replace_desc_reason (RCustom "unary minus") r
    | Annot_NotT r -> replace_desc_reason (RCustom "unary not") r
    | Annot_GetPropT (r, _, propref) -> replace_desc_reason (RProperty (name_of_propref propref)) r
    | Annot_ObjRestT (r, _) -> replace_desc_reason (RCustom "rest") r
    | r -> reason_of_op r

  let to_annot_op_exn = function
    | Annot_unresolved _ -> failwith "to_annot_op_exn on unresolved"
    | Annot_resolved -> failwith "to_annot_op_exn on resolved"
    | Annot_op { op; _ } -> op

  include Union_find.Make (struct
    type t = constraints
  end)

  let new_root constraints = Root { rank = 0; constraints = lazy constraints }

  let fully_resolved_root = { rank = 0; constraints = lazy Annot_resolved }

  let fully_resolved_node = Root fully_resolved_root

  let deps_of_constraint = function
    | Annot_unresolved { dependents; _ } -> dependents
    | Annot_op { dependents; _ } -> dependents
    | Annot_resolved -> ISet.empty

  let update_deps_of_constraint ~f = function
    | Annot_unresolved d -> d.dependents <- f d.dependents
    | Annot_op d -> d.dependents <- f d.dependents
    | Annot_resolved -> ()
end

module TypeContext = struct
  type t = {
    (* map from tvar ids to nodes (type info structures) *)
    graph: Constraint.node IMap.t;
    (* map from tvar ids to trust nodes *)
    trust_graph: Trust_constraint.node IMap.t;
    (* obj types point to mutable property maps *)
    property_maps: Properties.map;
    (* indirection to support context opt *)
    call_props: TypeTerm.t IMap.t;
    (* modules point to mutable export maps *)
    export_maps: Exports.map;
    (* map from evaluation ids to types *)
    evaluated: TypeTerm.t Eval.Map.t;
  }
end

module FlowSet = struct
  type t = UseTypeSet.t TypeMap.t

  let empty = TypeMap.empty

  (* returns ref eq map if no change *)
  let add l u x =
    let f = function
      | None -> UseTypeSet.singleton u
      | Some us -> UseTypeSet.add u us
    in
    TypeMap.adjust l f x

  let fold f = TypeMap.fold (fun l -> UseTypeSet.fold (fun u -> f (l, u)))
end

(* Type scheme: a type and an attendant environment of type parameters.
 * See normalizer for use. *)
module TypeScheme = struct
  type t = {
    tparams_rev: TypeTerm.typeparam list;
    type_: TypeTerm.t;
  }
end

include TypeTerm
include Trust

(**** Trust utilities ****)

let with_trust (trust_constructor : unit -> trust_rep) (type_constructor : trust_rep -> t) : t =
  trust_constructor () |> type_constructor

(*********************************************************)

let compare = Stdlib.compare

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

module SymbolT = Primitive (struct
  let desc = RSymbol

  let make r trust = DefT (r, trust, SymbolT)
end)

module MixedT = Primitive (struct
  let desc = RMixed

  let make r trust = DefT (r, trust, MixedT Mixed_everything)
end)

module EmptyT = Primitive (struct
  let desc = REmpty

  let make r trust = DefT (r, trust, EmptyT)
end)

module AnyT = struct
  let desc = function
    | AnnotatedAny -> RAnyExplicit
    | _ -> RAnyImplicit

  let make source r = AnyT (r, source)

  let at source = mk_annot_reason (desc source) %> make source

  let why source = replace_desc_reason (desc source) %> make source

  let annot = why AnnotatedAny

  let error = why (AnyError None)

  let error_of_kind kind = why (AnyError (Some kind))

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

  let bound_fn_this_any = AnyT.make bound_fn_this

  let dummy_static_any = AnyT.make dummy_static

  let why kind = Unsound kind |> AnyT.why

  let at kind = Unsound kind |> AnyT.at
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
  | DefT (_, _, EmptyT) -> true
  | _ -> false

let is_top = function
  | DefT (_, _, MixedT _) -> true
  | _ -> false

let is_any = function
  | AnyT _ -> true
  | _ -> false

let drop_generic = function
  | GenericT { bound; _ } -> bound
  | t -> t

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
  | ClassMethodDefinition { def = op; _ }
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
  | IndexedTypeAccess { index = op; _ }
  | JSXCreateElement { op; _ }
  | ReactCreateElementCall { op; _ }
  | TypeApplication { type' = op }
  | SetProperty { value = op; _ }
  | UpdateProperty { lhs = op; _ }
  | SwitchCheck { case_test = op; _ }
  | MatchingProp { op; _ } ->
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
    | RType name -> display_string_of_name name
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
  | EmptyT -> "EmptyT"
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
  | SymbolT -> "SymbolT"
  | TypeT _ -> "TypeT"
  | VoidT -> "VoidT"

let string_of_ctor = function
  | OpenT _ -> "OpenT"
  | AnyT _ -> "AnyT"
  | AnnotT _ -> "AnnotT"
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
  | InternalT (ExtendsT _) -> "ExtendsT"
  | FunProtoT _ -> "FunProtoT"
  | FunProtoApplyT _ -> "FunProtoApplyT"
  | FunProtoBindT _ -> "FunProtoBindT"
  | FunProtoCallT _ -> "FunProtoCallT"
  | GenericT _ -> "GenericT"
  | KeysT _ -> "KeysT"
  | ModuleT _ -> "ModuleT"
  | NullProtoT _ -> "NullProtoT"
  | ObjProtoT _ -> "ObjProtoT"
  | MatchingPropT _ -> "MatchingPropT"
  | OpaqueT _ -> "OpaqueT"
  | OpenPredT _ -> "OpenPredT"
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
  | ClassMethodDefinition _ -> "ClassMethodDefinition"
  | Coercion _ -> "Coercion"
  | DeleteProperty _ -> "DeleteProperty"
  | DeleteVar _ -> "DeleteVar"
  | FunCall _ -> "FunCall"
  | FunCallMethod _ -> "FunCallMethod"
  | FunImplicitReturn _ -> "FunImplicitReturn"
  | FunReturnStatement _ -> "FunReturnStatement"
  | GeneratorYield _ -> "GeneratorYield"
  | GetProperty _ -> "GetProperty"
  | IndexedTypeAccess _ -> "IndexedTypeAccess"
  | Internal op -> spf "Internal(%s)" (string_of_internal_use_op op)
  | JSXCreateElement _ -> "JSXCreateElement"
  | ReactCreateElementCall _ -> "ReactCreateElementCall"
  | ReactGetIntrinsic _ -> "ReactGetIntrinsic"
  | Speculation _ -> "Speculation"
  | TypeApplication _ -> "TypeApplication"
  | SetProperty _ -> "SetProperty"
  | UpdateProperty _ -> "UpdateProperty"
  | SwitchCheck _ -> "SwitchCheck"
  | MatchingProp _ -> "MatchingProp"
  | UnknownUse -> "UnknownUse"

let string_of_frame_use_op (type a) : a virtual_frame_use_op -> string = function
  | ConstrainedAssignment _ -> "ConstrainedAssignment"
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
      spf "%s(%s)" (string_of_frame_use_op use_op) acc
  )

let string_of_use_ctor = function
  | UseT (op, t) -> spf "UseT(%s, %s)" (string_of_use_op op) (string_of_ctor t)
  | AdderT _ -> "AdderT"
  | AndT _ -> "AndT"
  | ArrRestT _ -> "ArrRestT"
  | AssertArithmeticOperandT _ -> "AssertArithmeticOperandT"
  | AssertBinaryInLHST _ -> "AssertBinaryInLHST"
  | AssertBinaryInRHST _ -> "AssertBinaryInRHST"
  | AssertForInRHST _ -> "AssertForInRHST"
  | AssertInstanceofRHST _ -> "AssertInstanceofRHST"
  | AssertIterableT _ -> "AssertIterableT"
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
  | CheckUntypedImportT _ -> "CheckUntypedImportT"
  | DebugPrintT _ -> "DebugPrintT"
  | DebugSleepT _ -> "DebugSleepT"
  | ElemT _ -> "ElemT"
  | EnumCastT _ -> "EnumCastT"
  | EnumExhaustiveCheckT _ -> "EnumExhaustiveCheckT"
  | EqT _ -> "EqT"
  | ExportNamedT _ -> "ExportNamedT"
  | ExportTypeT _ -> "ExportTypeT"
  | AssertExportIsTypeT _ -> "AssertExportIsTypeT"
  | ExtendsUseT _ -> "ExtendsUseT"
  | FunImplicitVoidReturnT _ -> "FunImplicitVoidReturnT"
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
  | PrivateMethodT _ -> "PrivateMethodT"
  | MixinT _ -> "MixinT"
  | NotT _ -> "NotT"
  | NullishCoalesceT _ -> "NullishCoalesceT"
  | ObjAssignToT _ -> "ObjAssignToT"
  | ObjAssignFromT _ -> "ObjAssignFromT"
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
  | StrictEqT _ -> "StrictEqT"
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
  | TypeCastT _ -> "TypeCastT"
  | ConcretizeTypeAppsT _ -> "ConcretizeTypeAppsT"
  | CondT _ -> "CondT"
  | ReactPropsToOut _ -> "ReactPropsToOut"
  | ReactInToProps _ -> "ReactInToProps"
  | DestructuringT _ -> "DestructuringT"
  | CreateObjWithComputedPropT _ -> "CreateObjWithComputedPropT"
  | ResolveUnionT _ -> "ResolveUnionT"
  | FilterOptionalT _ -> "FilterOptionalT"
  | FilterMaybeT _ -> "FilterMaybeT"
  | SealGenericT _ -> "SealGenericT"
  | OptionalIndexedAccessT _ -> "OptionalIndexedAccessT"

let string_of_binary_test = function
  | InstanceofTest -> "instanceof"
  | SentinelProp key -> "sentinel prop " ^ key

let rec string_of_predicate = function
  | AndP (p1, p2) -> string_of_predicate p1 ^ " && " ^ string_of_predicate p2
  | OrP (p1, p2) -> string_of_predicate p1 ^ " || " ^ string_of_predicate p2
  | NotP p -> "not " ^ string_of_predicate p
  | LeftP (b, t) ->
    spf "left operand of %s with right operand = %s" (string_of_binary_test b) (string_of_ctor t)
  | RightP (b, t) ->
    spf "right operand of %s with left operand = %s" (string_of_binary_test b) (string_of_ctor t)
  | ExistsP _ -> "truthy"
  | NullP -> "null"
  | MaybeP -> "null or undefined"
  | SingletonBoolP (_, false) -> "false"
  | SingletonBoolP (_, true) -> "true"
  | SingletonStrP (_, _, str) -> spf "string `%s`" str
  | SingletonNumP (_, _, (_, raw)) -> spf "number `%s`" raw
  (* typeof *)
  | VoidP -> "undefined"
  | BoolP _ -> "boolean"
  | StrP _ -> "string"
  | NumP _ -> "number"
  | FunP -> "function"
  | ObjP -> "object"
  | SymbolP _ -> "symbol"
  (* Array.isArray *)
  | ArrP -> "array"
  | PropExistsP (key, _) -> spf "prop `%s` is truthy" key
  | PropNonMaybeP (key, _) -> spf "prop `%s` is not null or undefined" key
  | LatentP (OpenT (_, id), i) -> spf "LatentPred(TYPE_%d, %d)" id i
  | LatentP (t, i) -> spf "LatentPred(%s, %d)" (string_of_ctor t) i

let string_of_type_t_kind = function
  | TypeAliasKind -> "TypeAliasKind"
  | TypeParamKind -> "TypeParamKind"
  | OpaqueKind -> "OpaqueKind"
  | ImportTypeofKind -> "ImportTypeofKind"
  | ImportClassKind -> "ImportClassKind"
  | ImportEnumKind -> "ImportEnumKind"
  | InstanceKind -> "InstanceKind"

let extract_setter_type = function
  | DefT (_, _, FunT (_, _, { params = [(_, param_t)]; _ })) -> param_t
  | _ -> failwith "Setter property with unexpected type"

let extract_getter_type = function
  | DefT (_, _, FunT (_, _, { return_t; _ })) -> return_t
  | _ -> failwith "Getter property with unexpected type"

let elemt_of_arrtype = function
  | ArrayAT (elemt, _)
  | ROArrayAT elemt
  | TupleAT (elemt, _) ->
    elemt

let ro_of_arrtype = function
  | ArrayAT _ -> Generic.ArraySpread.NonROSpread
  | _ -> Generic.ArraySpread.ROSpread

let annot use_desc = function
  | OpenT (r, _) as t -> AnnotT (r, t, use_desc)
  | t -> t

(* The following functions are used as constructors for function types and
   object types, which unfortunately have many fields, not all of which are
   meaningful in all contexts. This part of the design should be revisited:
   perhaps the data types can be refactored to make them more specialized. *)

(* Methods may use a dummy statics object type to carry properties. We do not
   want to encourage this pattern, but we also don't want to block uses of this
   pattern. Thus, we compromise by not tracking the property types. *)
let dummy_static = update_desc_reason (fun desc -> RStatics desc) %> Unsoundness.dummy_static_any

let dummy_prototype = ObjProtoT (locationless_reason RDummyPrototype)

let bound_function_dummy_this loc = mk_reason RDummyThis loc |> Unsoundness.bound_fn_this_any

let dummy_this loc = mk_reason RDummyThis loc |> MixedT.make |> with_trust bogus_trust

let implicit_mixed_this r =
  update_desc_reason (fun desc -> RImplicitThis desc) r |> MixedT.make |> with_trust bogus_trust

let global_this reason =
  let reason = replace_desc_reason (RCustom "global object") reason in
  ObjProtoT reason

let default_obj_assign_kind = ObjAssign { assert_exact = false }

(* A method type is a function type with `this` specified. *)
let mk_methodtype
    this_t
    ?(subtyping = true)
    tins
    ~rest_param
    ~def_reason
    ?params_names
    ?(is_predicate = false)
    tout =
  {
    this_t = (this_t, subtyping);
    params =
      (match params_names with
      | None -> Base.List.map ~f:(fun t -> (None, t)) tins
      | Some xs -> List.map2 (fun x t -> (x, t)) xs tins);
    rest_param;
    return_t = tout;
    is_predicate;
    def_reason;
  }

let mk_methodcalltype targs args ?meth_generic_this ?(meth_strict_arity = true) tout =
  {
    meth_generic_this;
    meth_targs = targs;
    meth_args_tlist = args;
    meth_tout = tout;
    meth_strict_arity;
  }

(* A bound function type is a method type whose `this` parameter has been
   bound to some type. Currently, if the function's `this` parameter is not
   explicitly annotated we model this unsoundly using `any`, but if it is
   then we create a methodtype with a specific `this` type.  *)

let mk_boundfunctiontype ~this = mk_methodtype this

(* A function type is a method type whose `this` parameter has been
   bound to to the global object. Currently, if the function's `this` parameter is not
   explicitly annotated we model this using `mixed`, but if it is
   then we create a methodtype with a specific `this` type.  *)

let mk_functiontype reason ?(this = global_this reason) = mk_methodtype this

let mk_boundfunctioncalltype this targs args ?(call_strict_arity = true) tout =
  {
    call_this_t = this;
    call_targs = targs;
    call_args_tlist = args;
    call_tout = tout;
    call_strict_arity;
  }

let mk_functioncalltype reason = mk_boundfunctioncalltype (global_this reason)

let mk_opt_functioncalltype reason targs args strict = (global_this reason, targs, args, strict)

let mk_opt_boundfunctioncalltype this targs args strict = (this, targs, args, strict)

let mk_opt_methodcalltype
    ?opt_meth_generic_this opt_meth_targs opt_meth_args_tlist opt_meth_strict_arity =
  { opt_meth_generic_this; opt_meth_targs; opt_meth_args_tlist; opt_meth_strict_arity }

(* An object type has two flags, sealed and exact. A sealed object type cannot
   be extended. An exact object type accurately describes objects without
   "forgeting" any properties: so to extend an object type with optional
   properties, the object type must be exact. Thus, as an invariant, "not exact"
   logically implies "sealed" (and by contrapositive, "not sealed" implies
   "exact"; in other words, exact and sealed cannot both be false).

   Types of object literals are exact, but can be sealed or unsealed. Object
   type annotations are sealed but not exact. *)

let default_flags = { obj_kind = Exact; frozen = false }

let mk_objecttype ?(flags = default_flags) ~call pmap proto =
  { flags; proto_t = proto; props_tmap = pmap; call_t = call }

let mk_object_def_type ~reason ?(flags = default_flags) ~call pmap proto =
  let reason = update_desc_reason invalidate_rtype_alias reason in
  DefT (reason, bogus_trust (), ObjT (mk_objecttype ~flags ~call pmap proto))

let apply_opt_funcalltype (this, targs, args, strict) t_out =
  {
    call_this_t = this;
    call_targs = targs;
    call_args_tlist = args;
    call_tout = t_out;
    call_strict_arity = strict;
  }

let apply_opt_methodcalltype
    { opt_meth_generic_this; opt_meth_targs; opt_meth_args_tlist; opt_meth_strict_arity } meth_tout
    =
  {
    meth_generic_this = opt_meth_generic_this;
    meth_targs = opt_meth_targs;
    meth_args_tlist = opt_meth_args_tlist;
    meth_tout;
    meth_strict_arity = opt_meth_strict_arity;
  }

let create_intersection rep = IntersectionT (locationless_reason (RCustom "intersection"), rep)

let apply_opt_action action t_out =
  match action with
  | OptCallM f -> CallM (apply_opt_methodcalltype f t_out)
  | OptChainM (exp_reason, lhs_reason, this, f, vs) ->
    ChainM (exp_reason, lhs_reason, this, apply_opt_methodcalltype f t_out, vs)

let apply_opt_use opt_use t_out =
  match opt_use with
  | OptMethodT (op, r1, r2, ref, action, prop_tout) ->
    MethodT (op, r1, r2, ref, apply_opt_action action t_out, prop_tout)
  | OptPrivateMethodT (op, r1, r2, p, scopes, static, action, prop_tout) ->
    PrivateMethodT (op, r1, r2, p, scopes, static, apply_opt_action action t_out, prop_tout)
  | OptCallT (u, r, f) -> CallT (u, r, apply_opt_funcalltype f t_out)
  | OptGetPropT (u, r, p) -> GetPropT (u, r, p, t_out)
  | OptGetPrivatePropT (u, r, s, cbs, b) -> GetPrivatePropT (u, r, s, cbs, b, t_out)
  | OptTestPropT (r, i, p) -> TestPropT (r, i, p, t_out)
  | OptGetElemT (u, r, t) -> GetElemT (u, r, t, t_out)
  | OptCallElemT (r1, r2, elt, call) -> CallElemT (r1, r2, elt, apply_opt_action call t_out)

let mk_enum_type ~trust reason enum =
  let reason =
    update_desc_reason
      (fun desc ->
        match desc with
        | REnum name -> RType (OrdinaryName name)
        | _ -> desc)
      reason
  in
  DefT (reason, trust, EnumT enum)

let call_of_method_app
    call_this_t { meth_generic_this; meth_targs; meth_args_tlist; meth_tout; meth_strict_arity } =
  {
    call_this_t = Base.Option.value ~default:call_this_t meth_generic_this;
    call_targs = meth_targs;
    call_args_tlist = meth_args_tlist;
    call_tout = meth_tout;
    call_strict_arity = meth_strict_arity;
  }

let apply_method_action use_op reason_call this_arg action =
  match action with
  | CallM app -> CallT (use_op, reason_call, call_of_method_app this_arg app)
  | ChainM (exp_reason, lhs_reason, this, app, vs) ->
    OptionalChainT
      {
        reason = exp_reason;
        lhs_reason;
        this_t = this;
        t_out = CallT (use_op, reason_call, call_of_method_app this_arg app);
        voided_out = vs;
      }

module TypeParams : sig
  val to_list : typeparams -> typeparam list

  val of_list : ALoc.t -> typeparam list -> typeparams

  val map : (typeparam -> typeparam) -> typeparams -> typeparams
end = struct
  let to_list tparams =
    Base.Option.value_map tparams ~default:[] ~f:(fun (_loc, tparam_nel) -> Nel.to_list tparam_nel)

  let of_list tparams_loc tparams =
    match tparams with
    | [] -> None
    | hd :: tl -> Some (tparams_loc, (hd, tl))

  let map f tparams = Base.Option.map ~f:(fun (loc, params) -> (loc, Nel.map f params)) tparams
end

type annotated_or_inferred =
  | Annotated of TypeTerm.t
  | Inferred of TypeTerm.t

let react_server_module_ref = "#flow-internal-react-server-module"
