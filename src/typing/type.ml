(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Polarity
open Reason
open FlowSymbol
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

type number_literal = float * string [@@deriving ord, eq]

type bigint_literal = int64 option * string [@@deriving ord, eq]

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
    | DefT of reason * def_t
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
    | GenericT of {
        reason: reason;
        name: Subst_name.t;
        bound: t;
        no_infer: bool;
        id: Generic.id;
      }
    (* this-abstracted instance. If `is_this` is true, then this literally comes from
       `this` as an annotation or expression, and should be fixed to an internal
       view of the class, which is a generic whose upper bound is the class. *)
    | ThisInstanceT of reason * instance_t * (* is_this *) bool * Subst_name.t
    (* this instantiation *)
    | ThisTypeAppT of reason * t * t * t list option
    (* type application *)
    | TypeAppT of {
        reason: reason;
        use_op: use_op;
        type_: t;
        targs: t list;
        from_value: bool;
        use_desc: bool;
      }
    | FunProtoT of reason (* Function.prototype *)
    | ObjProtoT of reason (* Object.prototype *)
    (* Signifies the end of the prototype chain. Distinct from NullT when it
       appears as an upper bound of an object type, otherwise the same. *)
    | NullProtoT of reason
    | FunProtoBindT of reason (* Function.prototype.bind *)
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
    (* advanced string types *)
    | StrUtilT of {
        reason: reason;
        op: str_util_op;
        remainder: t option;
      }
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
    (* Stores both values and types in the same namespace*)
    | NamespaceT of namespace_type
    (* Here's to the crazy ones. The misfits. The rebels. The troublemakers.
       The round pegs in the square holes. **)
    | AnyT of reason * any_source

  and def_t =
    | NumGeneralT of literal
    (* TODO StrT should perhaps not allow internal names *)
    | StrGeneralT of literal
    | BoolGeneralT
    | BigIntGeneralT of literal
    | EmptyT
    | MixedT of mixed_flavor
    | NullT
    | VoidT
    | SymbolT
    | FunT of static * funtype
    | ObjT of objtype
    | ArrT of arrtype
    (* type of a class *)
    | ClassT of t
    (* type of an instance of a class *)
    | InstanceT of instance_t
    (* singleton string, matches exactly a given string literal *)
    | SingletonStrT of {
        from_annot: bool;
        (* TODO SingletonStrT should not include internal names *)
        value: name;
      }
    (* This type is only to be used to represent numeric-like object keys in the
       context of object-to-object subtyping. It allows numeric-like object keys
       to be a subtype of both `number` and `string`, so that `{1: true}` can be
       a subtyped of `{[number]: boolean}`. Do not use outside of this context!

       The second element of the `number_literal` tuple, which is the `string`
       representation, is the key name.
    *)
    | NumericStrKeyT of number_literal
    (* matches exactly a given number literal, for some definition of "exactly"
       when it comes to floats... *)
    | SingletonNumT of {
        from_annot: bool;
        value: number_literal;
      }
    (* singleton bool, matches exactly a given boolean literal *)
    | SingletonBoolT of {
        from_annot: bool;
        value: bool;
      }
    | SingletonBigIntT of {
        from_annot: bool;
        value: bigint_literal;
      }
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
    (* React$AbstractComponent<Config, Instance, ReturnElement> *)
    | ReactAbstractComponentT of {
        config: t;
        instance: component_instance;
        renders: t;
        component_kind: component_kind;
      }
    | RendersT of canonical_renders_form
    (* Enum types *)
    | EnumValueT of enum_info
    | EnumObjectT of {
        enum_value_t: t;
        enum_info: enum_info;
      }

  and component_instance =
    | ComponentInstanceAvailableAsRefSetterProp of t
        (** React.RefSetter<Instance> in component(ref: React.RefSetter<Instance>) *)
    | ComponentInstanceOmitted of reason
        (** Modeling the absence of ref prop in components. e.g. component Foo() *)

  (* A syntactic render type "renders T" uses an EvalT to be translated into a canonical form.
   * The subtyping rules are much simpler to understand in these forms, so we use the render type
   * normalization logic defined in flow_js_utils to take a syntactic render type and turn it
   * into a RendersT (it will ALWAYS return a RendersT) of one of the canonical forms.
   *
   * The Structural (t) form guarantees that if you evaluate t you will not
   * get back a RendersT. If the argument is a UnionT, none of the members of that UnionT
   * will be Strcutrual RendersTs, but there may be nominal RendersTs in the union.
   *
   * Nominal render types make no guarantees about their super and they can be any type.
   * In practice, the only way to introduce Nominal render types is component syntax, and
   * components always use a render type as the `super`.
   *
   * Given component Foo:
   *  * Omitting render declaration would produce DefaultRenders
   *  * renders Foo would produce NominalRenders
   *  * renders (Foo | Foo) would produce a Structural UnionT with two Nominal elements
   *  * renders (Foo | number) would produce a Structural UnionT with number and Nominal Foo
   *  * renders number would produce a Structural number
   *)
  and canonical_renders_form =
    | IntrinsicRenders of string
    | NominalRenders of {
        renders_id: ALoc.id;
        renders_name: string;
        renders_super: t;
      }
    | StructuralRenders of {
        renders_variant: renders_variant;
        renders_structural_type: t;
      }
    | DefaultRenders

  and renders_variant =
    | RendersNormal
    | RendersMaybe
    | RendersStar

  and component_kind =
    | Structural
    | Nominal of ALoc.id * string * t list option (* Instantiation types - used for hover type *)

  and hint_eval_result =
    | HintAvailable of t * Hint.hint_kind
    | NoHint
    | EncounteredPlaceholder
    | DecompositionError

  and lazy_hint_compute = expected_only:bool -> ?skip_optional:bool -> reason -> hint_eval_result

  and lazy_hint_t = bool * lazy_hint_compute

  and defer_use_t =
    (* destructors that extract parts of various kinds of types *)
    | TypeDestructorT of use_op * reason * destructor

  and str_util_op =
    | StrPrefix of string
    | StrSuffix of string

  and enum_concrete_info = {
    enum_name: string;
    enum_id: ALoc.id;
    members: ALoc.t SMap.t;
    representation_t: t;
    has_unknown_members: bool;
  }

  and enum_info =
    | ConcreteEnum of enum_concrete_info
    | AbstractEnum of { representation_t: t }

  and 'loc virtual_root_use_op =
    | ObjectAddComputedProperty of { op: 'loc virtual_reason }
    | ObjectSpread of { op: 'loc virtual_reason }
    | ObjectRest of { op: 'loc virtual_reason }
    | ObjectChain of { op: 'loc virtual_reason }
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
    | ConformToCommonInterface of {
        self_sig_loc: 'loc;
        self_module_loc: 'loc;
        (* Whether the check originate from the import of the common module,
         * rather than at the implementation file against the common interface. *)
        originate_from_import: bool;
      }
    | DeclareComponentRef of { op: 'loc virtual_reason }
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
        type_guard: bool;
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
    | InferBoundCompatibilityCheck of {
        bound: 'loc virtual_reason;
        infer: 'loc virtual_reason;
      }
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
    | TypeApplication of { type_: 'loc virtual_reason }
    | SetProperty of {
        lhs: 'loc virtual_reason;
        prop: 'loc virtual_reason;
        value: 'loc virtual_reason;
      }
    | UpdateProperty of {
        lhs: 'loc virtual_reason;
        prop: 'loc virtual_reason;
      }
    | RefinementCheck of {
        test: 'loc virtual_reason;
        discriminant: 'loc virtual_reason;
      }
    | SwitchRefinementCheck of {
        test: 'loc;
        discriminant: 'loc;
      }
    | MatchingProp of {
        op: 'loc virtual_reason;
        obj: 'loc virtual_reason;
        key: string;
        sentinel_reason: 'loc virtual_reason;
      }
    | EvalMappedType of { mapped_type: 'loc virtual_reason }
    | TypeGuardIncompatibility of {
        guard_type: 'loc virtual_reason;
        param_name: string;
      }
    | RenderTypeInstantiation of { render_type: 'loc virtual_reason }
    | ComponentRestParamCompatibility of { rest_param: 'loc virtual_reason }
    | PositiveTypeGuardConsistency of {
        reason: 'loc virtual_reason;
        return_reason: 'loc virtual_reason;
        param_reason: 'loc virtual_reason;
        guard_type_reason: 'loc virtual_reason;
        is_return_false_statement: bool;
      }
    | UnknownUse

  and 'loc virtual_frame_use_op =
    | ConstrainedAssignment of {
        name: string;
        declaration: 'loc;
        providers: 'loc list;
      }
    | ReactDeepReadOnly of ('loc * dro_type)
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
    | OpaqueTypeLowerBoundCompatibility of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | OpaqueTypeUpperBoundCompatibility of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }
    | MappedTypeKeyCompatibility of {
        source_type: 'loc virtual_reason;
        mapped_type: 'loc virtual_reason;
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
        lower_optional: bool;
        upper_optional: bool;
      }
    | TupleAssignment of { upper_optional: bool }
    | TypeArgCompatibility of {
        name: Subst_name.t;
        targ: 'loc virtual_reason;
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
        polarity: Polarity.t;
      }
    | TypeParamBound of { name: Subst_name.t }
    | OpaqueTypeLowerBound of { opaque_t_reason: 'loc virtual_reason }
    | OpaqueTypeUpperBound of { opaque_t_reason: 'loc virtual_reason }
    | TypeGuardCompatibility
    | RendersCompatibility
    | UnifyFlip
    | EnumRepresentationTypeCompatibility of {
        lower: 'loc virtual_reason;
        upper: 'loc virtual_reason;
      }

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
    | BindT of use_op * reason * funcalltype
    | CallT of {
        use_op: use_op;
        reason: reason;
        call_action: call_action;
        return_hint: lazy_hint_t;
      }
    | ConditionalT of {
        use_op: use_op;
        reason: reason;
        distributive_tparam_name: Subst_name.t option;
        infer_tparams: typeparam list;
        extends_t: t;
        true_t: t;
        false_t: t;
        tout: tvar;
      }
    (* The last position is an optional type that probes into the type of the
       method called. This will be primarily used for type-table bookkeeping. *)
    | MethodT of use_op * (* call *) reason * (* lookup *) reason * propref * method_action
    (* Similar to above, but stores information necessary to resolve a private method. *)
    | PrivateMethodT of
        use_op
        * (* call *) reason
        * (* lookup *) reason
        * (* prop *) string
        * class_binding list
        * (* static *) bool
        * method_action
    (* Similar to the last element of the MethodT *)
    | SetPropT of use_op * reason * propref * set_mode * write_ctx * t * t option
    (* The boolean flag indicates whether or not it is a static lookup. We cannot know this when
     * we generate the constraint, since the lower bound may be an unresolved OpenT. If it
     * resolves to a ClassT, we flip the flag to true, which causes us to check the private static
     * fields when the InstanceT ~> SetPrivatePropT constraint is processed *)
    | SetPrivatePropT of
        use_op * reason * string * set_mode * class_binding list * bool * write_ctx * t * t option
    | GetTypeFromNamespaceT of {
        use_op: use_op;
        reason: reason;
        prop_ref: reason * name;
        tout: tvar;
      }
    | GetPropT of {
        use_op: use_op;
        reason: reason;
        id: ident option;
        from_annot: bool;
        skip_optional: bool;
        propref: propref;
        tout: tvar;
        hint: lazy_hint_t;
      }
    (* The same comment on SetPrivatePropT applies here *)
    | GetPrivatePropT of use_op * reason * string * class_binding list * bool * tvar
    | TestPropT of {
        use_op: use_op;
        reason: reason;
        id: ident;
        propref: propref;
        tout: tvar;
        hint: lazy_hint_t;
      }
    (* SetElemT has a `tout` parameter to serve as a trigger for ordering
       operations. We only need this in one place: object literal initialization.
       In particular, a computed property in the object initializer users SetElemT
       to initialize the property value, but in order to avoid race conditions we
       need to ensure that reads happen after writes. *)
    | SetElemT of use_op * reason * t * set_mode * t * t option (*tout *)
    | GetElemT of {
        use_op: use_op;
        reason: reason;
        id: ident option;
        from_annot: bool;
        skip_optional: bool;
        access_iterables: bool;
        key_t: t;
        tout: tvar;
      }
    | CallElemT of use_op * (* call *) reason * (* lookup *) reason * t * method_action
    | GetStaticsT of tvar
    | GetProtoT of reason * tvar
    | SetProtoT of reason * t
    (* repositioning *)
    | ReposLowerT of {
        reason: reason;
        use_desc: bool;
        use_t: use_t;
      }
    | ReposUseT of reason * bool (* use_desc *) * use_op * t
    (* operations on runtime types, such as classes and functions *)
    | ConstructorT of {
        use_op: use_op;
        reason: reason;
        targs: targ list option;
        args: call_arg list;
        tout: t;
        return_hint: lazy_hint_t;
        specialized_ctor: specialized_callee option;
      }
    | SuperT of use_op * reason * derived_type
    | ImplementsT of use_op * t
    | MixinT of reason * t
    | ToStringT of {
        orig_t: t option;
        reason: reason;
        t_out: use_t;
      }
    (* operation on polymorphic types *)
    (* SpecializeT(_, _, _, targs, tresult) instantiates a polymorphic type
          with type arguments targs, and flows the result into tresult.

          The first reason is the reason why we're specializing. The second
          reason points to the type application itself
       **)
    | SpecializeT of use_op * reason * reason * t list option * t
    (* operation on this-abstracted classes *)
    | ThisSpecializeT of reason * t * cont
    (* Convert a value to a type inference (e.g. ClassT -> InstanceT) *)
    | ValueToTypeReferenceT of use_op * reason * type_t_kind * t_out
    (* In TypeAppT (c, ts) ~> TypeAppT (c, ts) we need to check both cs against
     * each other which means that we must concretize them first. *)
    | ConcretizeTypeAppsT of
        (* The use_op from our original TypeAppT ~> TypeAppT *)
        use_op
        * (* The type args, from_value, and reason for the TypeAppT that is currently the
           * lower bound *)
        (t list * bool * use_op * reason)
        * (* The polymorphic type, its type args, from_value from TypeApp,
           * and reason for the TypeAppT that is currently the upper bound. *)
        (t * t list * bool * use_op * reason)
        * (* A boolean which answers the question: Is the TypeAppT that is
           * currently our lower bound in fact our upper bound in the original
           * TypeAppT ~> TypeAppT? If the answer is yes then we need to flip our
           * tuples and flow the polymorphic type currently in our upper bound as
           * the lower bound. See the implementation of flow_js for more clarity. *)
          bool
    (* operation on prototypes *)
    (* LookupT looks for a property in an object type and emits a constraint according to the
          provided lookup_action.
          It also carries with it a list of the prop_map ids it has already tried.

          When the property is not found, we have the following cases:
          - `try_ts_on_failure` is not empty: we try to look for property in
            the next object type in that list
          - else: we examine the `lookup_kind` - read the comment on that type
       **)
    | LookupT of {
        reason: reason;
        lookup_kind: lookup_kind;
        try_ts_on_failure: t list;
        propref: propref;
        lookup_action: lookup_action;
        ids: Properties.Set.t option;
        method_accessible: bool;
        ignore_dicts: bool;
      }
    (* operations on objects *)
    | ObjRestT of reason * string list * t * int
    (* test that something is a valid proto (object-like or null) *)
    | ObjTestProtoT of reason * t_out
    (* test that something is object-like, returning a default type otherwise *)
    | ObjTestT of reason * t * t
    (* assignment rest element in array pattern *)
    | ArrRestT of use_op * reason * int * t
    (* Keys *)
    | GetKeysT of reason * use_t
    | HasOwnPropT of use_op * reason * t (* The incoming string that we want to check against *)
    (* Values *)
    | GetValuesT of reason * t
    (* Values of a dictionary, `mixed` otherwise. *)
    | GetDictValuesT of reason * use_t
    (* Element access *)
    | ElemT of {
        use_op: use_op;
        reason: reason;
        obj: t;
        action: elem_action;
      }
    (* Map a FunT over a structure *)
    | MapTypeT of use_op * reason * type_map * t_out
    | ObjKitT of use_op * reason * Object.resolve_tool * Object.tool * t_out
    | ReactKitT of use_op * reason * React.tool
    (* tools for preprocessing types *)
    | ConcretizeT of {
        reason: reason;
        kind: concretization_kind;
        seen: ISet.t ref;
        collector: TypeCollector.t;
      }
    | OptionalChainT of {
        reason: reason;
        lhs_reason: reason;
        t_out: use_t;
        voided_out: t_out;
      }
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
    (* Used to calculate a destructured binding. If annot is true, the lower
     * bound is an annotation (0->1), and t_out will be unified with the
     * destructured type. The caller should wrap the tvar with an AnnotT. *)
    | DestructuringT of reason * destruct_kind * selector * tvar * int
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
        enum: reason * enum_info;
      }
    | EnumExhaustiveCheckT of {
        reason: reason;
        check: enum_possible_exhaustive_check_t;
        incomplete_out: t;
        discriminant_after_check: t option;
      }
    (* Used by `EnumType` type destructor - gets the EnumValueT or EnumObjectT from the other one. *)
    | GetEnumT of {
        use_op: use_op;
        reason: reason;
        orig_t: t option;
        kind: [ `GetEnumObject | `GetEnumValue ];
        tout: t;
      }
    | FilterOptionalT of use_op * t
    | FilterMaybeT of use_op * t
    | DeepReadOnlyT of tvar * react_dro
    | HooklikeT of tvar
    | SealGenericT of {
        reason: reason;
        id: Generic.id;
        name: Subst_name.t;
        no_infer: bool;
        cont: cont;
      }
    | OptionalIndexedAccessT of {
        use_op: use_op;
        reason: reason;
        index: optional_indexed_access_index;
        tout_tvar: tvar;
      }
    | CheckUnusedPromiseT of {
        reason: reason;
        async: bool;
      }
    | ExtractReactRefT of reason * t
    | WriteComputedObjPropCheckT of {
        reason: reason;
        reason_key: reason option;
        value_t: t;
        err_on_str_key: use_op * reason;
      }
    | CheckReactImmutableT of {
        lower_reason: reason;
        upper_reason: reason;
        use_op: use_op;
        dro_loc: ALoc.t;
      }
    (* When extracting the props of an abstract component, we don't want to produce empty, even
       when the config type of the component is empty, because that would allow unsound access
       to the props. This utility passes through everything else but converts empty to mixed.
       A better approach here may be to prevent these kinds of operations entirely *)
    | ConvertEmptyPropsToMixedT of reason * t
    | ExitRendersT of {
        renders_reason: reason;
        u: use_t;
      }
    (* delay a type destructor evaluation *)
    | EvalTypeDestructorT of {
        destructor_use_op: use_op;
        reason: reason;
        repos: (reason * bool) option;
        destructor: destructor;
        tout: tvar;
      }

  and enum_check_t =
    | EnumCheck of {
        case_test_loc: ALoc.t;
        member_name: string;
      }

  (* Valid state transitions are:
   * EnumResolveDiscriminant -> EnumResolveCaseTest (with discriminant info populated)
   * EnumResolveCaseTest -> EnumResolveCaseTest *)
  and enum_exhaustive_check_tool_t =
    | EnumResolveDiscriminant
    | EnumResolveCaseTest of {
        discriminant_enum: enum_concrete_info;
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
        default_case_loc: ALoc.t option;
      }
    | EnumExhaustiveCheckInvalid of ALoc.t list

  (* Bindings created from destructuring annotations should themselves act like
   * annotations. That is, `var {p}: {p: string}; p = 0` should be an error,
   * because `p` should behave like a `string` annotation.
   *
   * We accomplish this by wrapping the binding itself in an AnnotT type. *)
  and destruct_kind =
    | DestructAnnot
    | DestructInfer

  and call_action =
    | Funcalltype of funcalltype
    | ConcretizeCallee of tvar

  (* use_ts which can be part of an optional chain, with t_out factored out *)
  and opt_use_t =
    | OptCallT of {
        use_op: use_op;
        reason: reason;
        opt_funcalltype: opt_funcalltype;
        return_hint: lazy_hint_t;
      }
    | OptMethodT of use_op * (* call *) reason * (* lookup *) reason * propref * opt_method_action
    | OptPrivateMethodT of
        use_op
        * (* call *) reason
        * (* lookup *) reason
        * string
        * class_binding list
        * bool
        * opt_method_action
    | OptGetPropT of {
        use_op: use_op;
        reason: reason;
        id: ident option;
        propref: propref;
        hint: lazy_hint_t;
      }
    | OptGetPrivatePropT of use_op * reason * string * class_binding list * bool
    | OptTestPropT of use_op * reason * ident * propref * lazy_hint_t
    | OptGetElemT of use_op * reason * ident option * bool (* from annot *) * t
    | OptCallElemT of use_op * (* call *) reason * (* lookup *) reason * t * opt_method_action

  and opt_state =
    | NonOptional
    | NewChain
    | ContinueChain
    | AssertChain

  and method_action =
    | CallM of {
        methodcalltype: methodcalltype;
        return_hint: lazy_hint_t;
        specialized_callee: specialized_callee option;
      }
    | ChainM of {
        exp_reason: reason;
        lhs_reason: reason;
        methodcalltype: methodcalltype;
        voided_out: t_out;
        return_hint: lazy_hint_t;
        specialized_callee: specialized_callee option;
      }
    | NoMethodAction of t

  and opt_method_action =
    | OptCallM of {
        opt_methodcalltype: opt_methodcalltype;
        return_hint: lazy_hint_t;
        specialized_callee: specialized_callee option;
      }
    | OptChainM of {
        exp_reason: reason;
        lhs_reason: reason;
        opt_methodcalltype: opt_methodcalltype;
        voided_out: t_out;
        return_hint: lazy_hint_t;
        specialized_callee: specialized_callee option;
      }
    | OptNoMethodAction of t

  and predicate =
    | AndP of predicate * predicate
    | OrP of predicate * predicate
    | NotP of predicate
    (* mechanism to handle binary tests where both sides need to be evaluated *)
    | BinaryP of binary_test * t
    (* Only track locations of existence checks created when walking the AST *)
    | TruthyP (* truthy *)
    | NullP (* null *)
    | MaybeP (* null or undefined *)
    | SingletonBoolP of ALoc.t * bool (* true or false *)
    | SingletonStrP of ALoc.t * bool * string (* string literal *)
    | SingletonNumP of ALoc.t * bool * number_literal
    | SingletonBigIntP of ALoc.t * bool * bigint_literal
    | BoolP of ALoc.t (* boolean *)
    | FunP (* function *)
    | NumP of ALoc.t (* number *)
    | BigIntP of ALoc.t
    | ObjP (* object *)
    | StrP of ALoc.t (* string *)
    | SymbolP of ALoc.t (* symbol *)
    | VoidP (* undefined *)
    | ArrP (* Array.isArray *)
    | ArrLenP of {
        op: array_length_op;
        n: int;
      }
    (* `if ('b' in a)` yields `flow (a, PredicateT(PropExistsP ("b"), tout))` *)
    | PropExistsP of {
        propname: string;
        reason: reason;
      }
    (* `if (a.b)` yields `flow (a, PredicateT(PropTruthyP ("b"), tout))` *)
    | PropTruthyP of string * reason
    (* `if (a?.b === null)` yields `flow (a, PredicateT(PropIsExactlyNullP ("b"), tout))` *)
    | PropIsExactlyNullP of string * reason
    (* `if (a?.b !== undefined)` yields `flow (a, PredicateT(PropNonVoidP ("b"), tout))` *)
    | PropNonVoidP of string * reason
    (* `if (a.b?.c)` yields `flow (a, PredicateT(PropNonMaybeP ("b"), tout))` *)
    | PropNonMaybeP of string * reason
    (* Encodes the latent predicate associated with the [index]-th parameter
       of the function in type [t]. We also include information for all type arguments
       and argument types of the call, to enable polymorphic calls. *)
    | LatentP of pred_funcall_info Lazy.t * index list
    | LatentThisP of pred_funcall_info Lazy.t
    | ImpossibleP

  and predicate_concretizer_variant =
    | ConcretizeForGeneralPredicateTest
    | ConcretizeKeepOptimizedUnions
    | ConcretizeRHSForInstanceOfPredicateTest
    | ConcretizeRHSForLiteralPredicateTest

  and substitution = Key.t SMap.t

  and binary_test =
    (* e1 instanceof e2 *)
    | InstanceofTest
    (* e1.key === e2 *)
    | SentinelProp of string
    (* e1 === e2 *)
    | EqTest

  and array_length_op =
    | ArrLenEqual
    | ArrLenGreaterThanEqual

  and literal =
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
    | CatchAny
    | AnnotatedAny
    | AnyError of any_error_kind option
    | Unsound of unsoundness_kind
    | Untyped
    | Placeholder

  and any_error_kind =
    | UnresolvedName
    | MissingAnnotation

  (* Tracks the kinds of unsoundness inherent in Flow. If you can't find a kind that matches
     your use case, make one *)
  and unsoundness_kind =
    | BoundFunctionThis
    | Constructor
    | DummyStatic
    | Exports
    | InferenceHooks
    | InstanceOfRefinement
    | Merged
    | ResolveSpread
    | Unchecked
    | Unimplemented
    | UnresolvedType
    | NonBindingPattern

  and fun_param = string option * t

  and fun_rest_param = string option * ALoc.t * t

  (* used by FunT *)
  and funtype = {
    this_t: t * this_status;
    params: fun_param list;
    rest_param: fun_rest_param option;
    return_t: t;
    type_guard: type_guard option;
    def_reason: Reason.t;
    effect_: react_effect_type;
  }

  and react_effect_type =
    | HookDecl of ALoc.id
    | HookAnnot
    | ArbitraryEffect
    | AnyEffect

  and type_guard =
    | TypeGuard of {
        inferred: bool;
        reason: reason;
        param_name: ALoc.t * string;
        type_guard: t;
        one_sided: bool;
      }

  (* FunTs carry around two `this` types, one to be used during subtyping and
     one to be treated as the param when the function is called. This is to allow
     more lenient subtyping between class methods without sacrificing soundness
     when calling functions. *)
  and this_status =
    | This_Method of { unbound: bool }
    | This_Function

  (* A CallT constructor can be used to compute hints in calls to IntersectionTs.
   * (See `synthesis_speculation_call` in type_hint.ml.) We use speculation_hint_state
   * to keep track of the various states of hint computation during speculation.
   * The state is initialized to the "unset" phase. If an overload succeeds, it is
   * recorded along with the speculation path (list of speculation_ids that led
   * to this choice) in the "set" constructor. For each subsequent success there
   * are two cases:
   * (i) The successful speculation id belongs to the "set" speculation path (we
   * are basically popping off a long speculation path). In this case, the state
   * remains the same.
   * (ii) The successful speculation id has not been recorded in "set". This choice
   * is a sibling of the currently "set" choice. This is possible thanks to the
   * special behavior of ConcretizeT with union-like types. In this case, we
   * will only accept the overload if all sibling branches agree on the result.
   * Otherwise the result is deemed "invalid".
   *)
  and speculation_hint_state =
    | Speculation_hint_unset
    | Speculation_hint_invalid
    | Speculation_hint_set of (int list * t)

  (* speculation id * case id *)
  and spec_state = int * int

  (* This mutable structure will collect information on the specialized form of the
   * callee type of a function call. For example, when calling a function with type
   * `<X>(x: Array<X>) => X`, with `[""]`, it will collect `(x: Array<string>) => string`.
   * This type will be used to populate the TAST for the callee expression.
   *
   * When recording types not under speculation, then the results will be appended
   * to the [finalized] list.
   *
   * Under speculation we can't really commit to the specialized function we are
   * calling during a speculative branch. We record the type as a "speculative
   * candidate", and include the speculative state under which this addition was
   * made. After speculation is done (at the "fire action" part of Speculation_kit)
   * we determine if the branch was successful, and if so, we promote the type to
   * the finalized list.
   *
   * Note that for signature-help results we do not record types under speculation
   * so we only need a simple list to record signatures.
   *)
  and specialized_callee =
    | Specialized_callee of {
        (* Utilities for populating the typed AST *)
        mutable finalized: t list;
        mutable speculative_candidates: (t * spec_state) list;
        init_speculation_state: spec_state option;
        (* Utilities for signature help *)
        mutable sig_help: t list;
      }

  (* Used by CallT and similar constructors *)
  and funcalltype = {
    call_this_t: t;
    call_targs: targ list option;
    call_args_tlist: call_arg list;
    call_tout: tvar;
    call_strict_arity: bool;
    call_speculation_hint_state: speculation_hint_state ref option;
    call_specialized_callee: specialized_callee option;
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

  and opt_funcalltype = t * targ list option * call_arg list * bool * specialized_callee option

  and opt_methodcalltype = {
    opt_meth_generic_this: t option;
    opt_meth_targs: targ list option;
    opt_meth_args_tlist: call_arg list;
    opt_meth_strict_arity: bool;
  }

  and call_arg =
    | Arg of t
    | SpreadArg of t

  and pred_funcall_info = use_op * ALoc.t * t (* callee *) * targ list option * call_arg list

  and dro_type =
    | HookReturn
    | HookArg
    | Props
    | ImmutableAnnot
    | DebugAnnot

  and react_dro = ALoc.t * dro_type

  and tuple_view =
    | TupleView of {
        elements: tuple_element list;
        arity: int * int;
        inexact: bool;
      }

  and arrtype =
    | ArrayAT of {
        react_dro: react_dro option;
            (* Should elements of this array be treated as propagating read-only, and if so, what location is responsible *)
        elem_t: t;
        tuple_view: tuple_view option;
      }
    (* TupleAT of elemt * tuple_types. Why do tuples carry around elemt? Well, so
     * that they don't need to recompute their general type when you do
     * myTuple[expr]
     *)
    | TupleAT of {
        react_dro: react_dro option; (* As ArrayAT *)
        elem_t: t;
        elements: tuple_element list;
        (* Arity represents the range of valid arities, considering optional elements.
           It ranges from the number of required elements, to the total number of elements. *)
        arity: int * int;
        inexact: bool;
      }
    (* ROArrayAT(elemt) is the super type for all tuples and arrays for which
     * elemt is a supertype of every element type *)
    | ROArrayAT of t * (* react_dro, as above *) react_dro option

  and tuple_element =
    | TupleElement of {
        reason: reason;
        name: string option;
        t: t;
        polarity: Polarity.t;
        optional: bool;
      }

  and objtype = {
    flags: flags;
    props_tmap: Properties.id;
    proto_t: prototype;
    call_t: int option;
    (* reachable_targs is populated during substitution. We use those reachable
     * targs to avoid traversing the full objtype structure during any-propagation
     * and instead pollute the reachable_targs directly. *)
    reachable_targs: (t * Polarity.t) list;
  }

  (* Object.assign(target, source1, ...source2) first resolves target then the
     sources. *)
  and obj_assign_kind =
    (* Obj.assign(target, source) with flag indicating whether source must be exact *)
    | ObjAssign of { assert_exact: bool }
    (* Obj.assign(target, ...source) *)
    | ObjSpreadAssign

  and namespace_type = {
    namespace_symbol: symbol;
    values_type: t;
    types_tmap: Properties.id;
  }

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
     tvar from the lookup. *)
  and lookup_kind =
    | Strict of reason
    | NonstrictReturning of (t * t) option * (ident * (reason * reason)) option

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
    | LookupPropForTvarPopulation of {
        polarity: Polarity.t;
        tout: t;
      }
    | LookupPropForSubtyping of {
        use_op: use_op;
        prop: property_type;
        prop_name: name;
        reason_lower: reason;
        reason_upper: reason;
      }
    | SuperProp of use_op * property_type
    | MatchProp of {
        use_op: use_op;
        drop_generic: bool;
        prop_t: t;
      }

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
    | ReadElem of {
        id: ident option;
        from_annot: bool;
        skip_optional: bool;
        access_iterables: bool;
        tout: tvar;
      }
    | WriteElem of {
        tin: t;
        tout: t option;
        mode: set_mode;
      }
    | CallElem of reason * method_action

  and propref =
    | Named of {
        reason: reason;
        name: name;
        from_indexed_access: bool;
      }
    | Computed of t

  and obj_kind =
    | Exact
    | Inexact
    | Indexed of dicttype

  and flags = {
    react_dro: react_dro option;
    obj_kind: obj_kind;
  }

  and dicttype = {
    dict_name: string option;
    key: t;
    value: t;
    dict_polarity: Polarity.t;
  }

  (* key_loc refer to the location of the identifier, if one exists,
     preferred_def_locs refer to the (potentially multiple) def_loc that go-to-definition should
     jump to. If the field is None, go-to-definition will jump to key_loc. The field should only
     be populated if multiple def_locs make sense, or if we want to track fields where we want to
     make an extra go-to-def jump for better UX. *)
  and property =
    | Field of {
        preferred_def_locs: ALoc.t Nel.t option;
        key_loc: ALoc.t option;
        type_: t;
        polarity: Polarity.t;
      }
    | Get of {
        key_loc: ALoc.t option;
        type_: t;
      }
    | Set of {
        key_loc: ALoc.t option;
        type_: t;
      }
    | GetSet of {
        get_key_loc: ALoc.t option;
        get_type: t;
        set_key_loc: ALoc.t option;
        set_type: t;
      }
    | Method of {
        key_loc: ALoc.t option;
        type_: t;
      }

  and property_type =
    (* A field that is defined in the source file as a field. *)
    | OrdinaryField of {
        type_: t;
        polarity: Polarity.t;
      }
    (* Some properties that are normalized to a field. *)
    | SyntheticField of {
        get_type: t option;
        set_type: t option;
      }

  and named_symbol = {
    name_loc: ALoc.t option;
    preferred_def_locs: ALoc.t Nel.t option;
    type_: t;
  }

  (* This has to go here so that Type doesn't depend on Scope *)
  and class_binding = { class_binding_id: ALoc.id }

  and insttype = {
    class_name: string option;
    class_id: ALoc.id;
    type_args: (Subst_name.t * reason * t * Polarity.t) list;
    own_props: Properties.id;
    proto_props: Properties.id;
    inst_call_t: int option;
    initialized_fields: SSet.t;
    initialized_static_fields: SSet.t;
    inst_kind: instance_kind;
    inst_dict: Object.dict;
    class_private_fields: Properties.id;
    class_private_static_fields: Properties.id;
    class_private_methods: Properties.id;
    class_private_static_methods: Properties.id;
    inst_react_dro: react_dro option;
  }

  and instance_kind =
    | ClassKind
    | InterfaceKind of { inline: bool }

  and instance_t = {
    inst: insttype;
    static: t;
    super: t;
    implements: t list;
  }

  and opaquetype = {
    opaque_id: Opaque.id;
    underlying_t: t option;
    lower_t: t option;
    upper_t: t option;
    opaque_type_args: (Subst_name.t * reason * t * Polarity.t) list;
    opaque_name: string;
  }

  and exporttypes = {
    (*
     * tmap used to store individual, named ES exports as generated by `export`
     * statements in a module.
     *)
    value_exports_tmap: Exports.id;
    (*
     * Note that CommonJS modules may also populate this tmap if their export
     * type is an object (that object's properties become named exports) or if
     * it has any "type" exports via `export type ...`
     *)
    type_exports_tmap: Exports.id;
    (*
     * This stores the CommonJS export type when applicable and is used as the
     * exact return type for calls to require(). This slot doesn't apply to pure
     * ES modules.
     *)
    cjs_export: (ALoc.t option (* def_loc *) * t) option;
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
    | DirectExport
    | ReExport

  and typeparam = {
    reason: reason;
    name: Subst_name.t;
    bound: t;
    polarity: Polarity.t;
    default: t option;
    is_this: bool;
    is_const: bool;
  }

  and typeparams_nonempty = ALoc.t * typeparam Nel.t

  and typeparams = typeparams_nonempty option

  and moduletype = {
    module_reason: reason;
    module_export_types: exporttypes;
    module_is_strict: bool;
    module_available_platforms: Platform_set.t option;
  }

  and selector =
    | Prop of string * bool
    | Elem of t
    | ObjRest of string list
    | ArrRest of int
    | Default

  and destructor =
    | NonMaybeType
    | PropertyType of { name: name }
    | ElementType of { index_type: t }
    | OptionalIndexedAccessNonMaybeType of { index: optional_indexed_access_index }
    | OptionalIndexedAccessResultType of { void_reason: reason }
    | ExactType
    | ReadOnlyType
    | PartialType
    | RequiredType
    | SpreadType of
        Object.Spread.target * Object.Spread.operand list * Object.Spread.operand_slice option
    | SpreadTupleType of {
        reason_tuple: reason;
        reason_spread: reason;
        inexact: bool;
        resolved_rev: resolved_param list;
        unresolved: unresolved_param list;
      }
    | RestType of Object.Rest.merge_mode * t
    | ValuesType
    | ConditionalType of {
        distributive_tparam_name: Subst_name.t option;
        infer_tparams: typeparam list;
        extends_t: t;
        true_t: t;
        false_t: t;
      }
    | TypeMap of type_map
    | ReactElementPropsType
    | ReactElementConfigType
    | ReactCheckComponentConfig of Property.t NameUtils.Map.t
    | ReactDRO of react_dro
    | MakeHooklike
    | MappedType of {
        (* Homomorphic mapped types use an inline keyof: {[key in keyof O]: T} or a type parameter
         * bound by $Keys/keyof: type Homomorphic<Keys: $Keys<O>> = {[key in O]: T *)
        homomorphic: mapped_type_homomorphic_flag;
        distributive_tparam_name: Subst_name.t option;
        property_type: t;
        mapped_type_flags: mapped_type_flags;
      }
    | EnumType

  and mapped_type_homomorphic_flag =
    | Homomorphic
    | SemiHomomorphic of t
    | Unspecialized

  and mapped_type_optionality =
    | MakeOptional
    | RemoveOptional
    | KeepOptionality

  and mapped_type_flags = {
    variance: Polarity.t;
    optional: mapped_type_optionality;
  }

  and optional_indexed_access_index =
    | OptionalIndexedAccessStrLitIndex of name
    | OptionalIndexedAccessTypeIndex of t

  and type_map = ObjectKeyMirror

  and prototype = t

  and super = t

  and static = t

  and t_out = t

  (* Concretizers of resolved types: simplify types like EvalT, OpenT, TypeAppT,
   * etc. The order of the constructors below denotes the order in which the
   * respective catch-all cases appears in flow_js.ml. *)
  and concretization_kind =
    | ConcretizeForCJSExtractNamedExportsAndTypeExports
    | ConcretizeForImportsExports
    | ConcretizeForInspection
    | ConcretizeForPredicate of predicate_concretizer_variant
    | ConcretizeForOperatorsChecking
    | ConcretizeForObjectAssign
    | ConcretizeForSentinelPropTest
    | ConcretizeForMatchArg of { keep_unions: bool }
    | ConcretizeAll

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
    | UnresolvedArg of tuple_element * Generic.id option
    | UnresolvedSpreadArg of t

  and resolved_param =
    | ResolvedArg of tuple_element * Generic.id option
    | ResolvedSpreadArg of reason * arrtype * Generic.id option
    | ResolvedAnySpreadArg of reason * any_source

  and spread_resolve =
    | ResolveSpreadsToTupleType of {
        id: int;
        elem_t: t;
        inexact: bool;
        tout: t;
      }
    (* Once we've finished resolving spreads, try to construct an array with known element types *)
    | ResolveSpreadsToArrayLiteral of {
        id: int;
        as_const: bool;
        elem_t: t;
        tout: t;
      }
    (* Once we've finished resolving spreads, try to construct a non-tuple array *)
    | ResolveSpreadsToArray of t * t (* elem type, array type *)
    (* Once we've finished resolving spreads for a function's arguments, call the
     * function with those arguments *)
    | ResolveSpreadsToMultiflowCallFull of int * funtype
    | ResolveSpreadsToMultiflowSubtypeFull of int * funtype
    (* Once we've finished resolving spreads for a function's arguments,
     * partially apply the arguments to the function and return the resulting
     * function (basically what func.bind(that, ...args) does) *)
    | ResolveSpreadsToMultiflowPartial of int * funtype * reason * t

  and spread_array_resolve_to =
    | ResolveToArrayLiteral of { as_const: bool }
    | ResolveToArray
    | ResolveToTupleType of { inexact: bool }

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
    | RenderTypeKind
  (* T/U in renders T/renders (T | U). Render types do not require type arguments for polymorphic components *)

  external use_t_compare : use_t -> use_t -> int = "caml_fast_generic_compare" [@@noalloc]

  external type_term_compare : t -> t -> int = "caml_fast_generic_compare" [@@noalloc]

  type frozen_kind =
    | NotFrozen
    | FrozenProp
    | FrozenDirect
end =
  TypeTerm

and DepthTrace : sig
  type t

  val depth : t -> int

  val dummy_trace : t

  val unit_trace : t

  val rec_trace : t -> t

  val concat_trace : t list -> t
end = struct
  type t = int

  let depth d = d

  let dummy_trace = 0

  let unit_trace = 1

  let rec_trace parent_depth = parent_depth + 1

  let concat_trace ts = List.fold_left (fun acc d -> Base.Int.max acc d) 0 ts
end

and UnionEnum : sig
  type t =
    (* TODO this should not allow internal names *)
    | Str of name
    | Num of number_literal
    | Bool of bool
    | BigInt of bigint_literal
    | Void
    | Null
  [@@deriving ord, eq]

  type star =
    | One of t
    | Many of UnionEnumSet.t

  (**
   * A tag to describe the kind of values included in an enum union. This information
   * is useful given that [canon] will match various types to the same UnionEnum.t.
   * In some cases, we will want to know if the initial DefT types were actually
   * of the same kind.
   *)
  type tag =
    | SingletonStrTag
    | StrTag
    | NumericStrKeyTag
    | SingletonNumTag
    | NumTag
    | SingletonBigIntTag
    | BingIntTag
    | SingletonBoolTag
    | BoolTag
    | VoidTag
    | NullTag

  val to_string : t -> string
end = struct
  (* compare numeric literals based on float representation *)
  let equal_number_literal (a, _) (b, _) = a = b

  let compare_number_literal (a, _) (b, _) = compare a b

  type t =
    | Str of name
    | Num of number_literal
    | Bool of bool
    | BigInt of bigint_literal
    | Void
    | Null
  [@@deriving ord, eq]

  type star =
    | One of t
    | Many of UnionEnumSet.t

  type tag =
    | SingletonStrTag
    | StrTag
    | NumericStrKeyTag
    | SingletonNumTag
    | NumTag
    | SingletonBigIntTag
    | BingIntTag
    | SingletonBoolTag
    | BoolTag
    | VoidTag
    | NullTag

  let to_string = function
    | Str s -> display_string_of_name s
    | Num (_, f) -> f
    | Bool b -> spf "%b" b
    | BigInt (_, b) -> b
    | Void -> "undefined"
    | Null -> "null"
end

and UnionEnumSet : (Flow_set.S with type elt = UnionEnum.t) = Flow_set.Make (UnionEnum)

and Property : sig
  type t = TypeTerm.property

  val type_ : t -> TypeTerm.property_type

  val polarity : t -> Polarity.t

  val polarity_of_property_type : TypeTerm.property_type -> Polarity.t

  val read_t_of_property_type : TypeTerm.property_type -> TypeTerm.t option

  val read_t : t -> TypeTerm.t option

  val write_t_of_property_type :
    ?ctx:TypeTerm.write_ctx -> TypeTerm.property_type -> TypeTerm.t option

  val write_t : t -> TypeTerm.t option

  val read_loc : t -> ALoc.t option

  val write_loc : t -> ALoc.t option

  val first_loc : t -> ALoc.t option

  val def_locs : t -> ALoc.t Nel.t option

  val iter_t : (TypeTerm.t -> unit) -> t -> unit

  val fold_t : ('a -> TypeTerm.t -> 'a) -> 'a -> t -> 'a

  val map_t : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val ident_map_t : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val forall_t : (TypeTerm.t -> bool) -> t -> bool

  val assert_field : t -> TypeTerm.t
end = struct
  open TypeTerm

  type t = property

  let type_ = function
    | Field { type_; polarity; _ } -> OrdinaryField { type_; polarity }
    | Get { type_; _ }
    | Method { type_; _ } ->
      SyntheticField { get_type = Some type_; set_type = None }
    | Set { type_; _ } -> SyntheticField { get_type = None; set_type = Some type_ }
    | GetSet { get_type; set_type; _ } ->
      SyntheticField { get_type = Some get_type; set_type = Some set_type }

  let polarity = function
    | Field { polarity; _ } -> polarity
    | Get _ -> Positive
    | Set _ -> Negative
    | GetSet _ -> Neutral
    | Method _ -> Positive

  let polarity_of_property_type = function
    | OrdinaryField { polarity; _ } -> polarity
    | SyntheticField { get_type = None; set_type = None } -> failwith "Illegal property_type"
    | SyntheticField { get_type = Some _; set_type = None } -> Positive
    | SyntheticField { get_type = None; set_type = Some _ } -> Negative
    | SyntheticField { get_type = Some _; set_type = Some _ } -> Neutral

  let read_t_of_property_type = function
    | OrdinaryField { type_; polarity } ->
      if Polarity.compat (polarity, Positive) then
        Some type_
      else
        None
    | SyntheticField { get_type; _ } -> get_type

  let read_t p = p |> type_ |> read_t_of_property_type

  let write_t_of_property_type ?(ctx = Normal) = function
    | OrdinaryField { type_; _ } when ctx = ThisInCtor -> Some type_
    | OrdinaryField { type_; polarity } ->
      if Polarity.compat (polarity, Negative) then
        Some type_
      else
        None
    | SyntheticField { set_type; _ } -> set_type

  let write_t p = p |> type_ |> write_t_of_property_type ?ctx:None

  let read_loc = function
    | Field { key_loc = loc; _ }
    | Get { key_loc = loc; _ }
    | GetSet { get_key_loc = loc; _ }
    | Method { key_loc = loc; _ } ->
      loc
    | Set _ -> None

  let write_loc = function
    | Field { key_loc = loc; _ }
    | Set { key_loc = loc; _ }
    | GetSet { set_key_loc = loc; _ } ->
      loc
    | Method _
    | Get _ ->
      None

  let first_loc = function
    | Field { key_loc = loc; _ }
    | Get { key_loc = loc; _ }
    | Set { key_loc = loc; _ }
    | Method { key_loc = loc; _ } ->
      loc
    | GetSet { get_key_loc; set_key_loc; _ } ->
      (match (get_key_loc, set_key_loc) with
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

  let def_locs = function
    | Field { preferred_def_locs = Some locs; _ } -> Some locs
    | Field { preferred_def_locs = None; key_loc; _ }
    | Get { key_loc; _ }
    | Set { key_loc; _ }
    | Method { key_loc; _ } ->
      Base.Option.map ~f:Nel.one key_loc
    | GetSet { get_key_loc; set_key_loc; _ } ->
      (match (get_key_loc, set_key_loc) with
      | (None, None) -> None
      | (Some loc, None)
      | (None, Some loc) ->
        Some (Nel.one loc)
      | (Some loc1, Some loc2) -> Some (loc1, [loc2]))

  let iter_t f = function
    | Field { type_; _ }
    | Get { type_; _ }
    | Set { type_; _ }
    | Method { type_; _ } ->
      f type_
    | GetSet { get_type; set_type; _ } ->
      f get_type;
      f set_type

  let fold_t f acc = function
    | Field { type_; _ }
    | Get { type_; _ }
    | Set { type_; _ }
    | Method { type_; _ } ->
      f acc type_
    | GetSet { get_type; set_type; _ } -> f (f acc get_type) set_type

  let map_t f = function
    | Field { preferred_def_locs; key_loc; type_; polarity } ->
      Field { preferred_def_locs; key_loc; type_ = f type_; polarity }
    | Get { key_loc; type_ } -> Get { key_loc; type_ = f type_ }
    | Set { key_loc; type_ } -> Set { key_loc; type_ = f type_ }
    | GetSet { get_key_loc; get_type; set_key_loc; set_type } ->
      GetSet { get_key_loc; get_type = f get_type; set_key_loc; set_type = f set_type }
    | Method { key_loc; type_ } -> Method { key_loc; type_ = f type_ }

  let ident_map_t f p =
    match p with
    | Field { preferred_def_locs; key_loc; type_; polarity } ->
      let type_' = f type_ in
      if type_' == type_ then
        p
      else
        Field { preferred_def_locs; key_loc; type_ = type_'; polarity }
    | Get { key_loc; type_ } ->
      let type_' = f type_ in
      if type_' == type_ then
        p
      else
        Get { key_loc; type_ = type_' }
    | Set { key_loc; type_ } ->
      let type_' = f type_ in
      if type_' == type_ then
        p
      else
        Set { key_loc; type_ = type_' }
    | GetSet { get_key_loc; get_type; set_key_loc; set_type } ->
      let get_type' = f get_type in
      let set_type' = f set_type in
      if get_type' == get_type && set_type' == set_type then
        p
      else
        GetSet { get_key_loc; get_type = get_type'; set_key_loc; set_type = set_type' }
    | Method { key_loc; type_ } ->
      let type_' = f type_ in
      if type_' == type_ then
        p
      else
        Method { key_loc; type_ = type_' }

  let forall_t f = fold_t (fun acc t -> acc && f t) true

  let assert_field = function
    | Field { type_; _ } -> type_
    | _ -> assert_false "Unexpected field type"
end

and Properties : sig
  type t = Property.t NameUtils.Map.t

  type id

  module Map : WrappedMap.S with type key = id

  module Set : Flow_set.S with type elt = id

  type map = t Map.t

  val add_field :
    name ->
    Polarity.t ->
    ?preferred_def_locs:ALoc.t Nel.t ->
    key_loc:ALoc.t option ->
    TypeTerm.t ->
    t ->
    t

  val add_getter : name -> ALoc.t option -> TypeTerm.t -> t -> t

  val add_setter : name -> ALoc.t option -> TypeTerm.t -> t -> t

  val add_method : name -> ALoc.t option -> TypeTerm.t -> t -> t

  val generate_id : unit -> id

  val id_of_aloc_id : type_sig:bool -> ALoc.id -> id

  val equal_id : id -> id -> bool

  val string_of_id : id -> string

  val unbind_this_method : TypeTerm.t -> TypeTerm.t

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

  let add_field x polarity ?preferred_def_locs ~key_loc type_ =
    NameUtils.Map.add x (Field { preferred_def_locs; key_loc; type_; polarity })

  let add_getter x get_key_loc get_type map =
    NameUtils.Map.adjust
      x
      (function
        | Some (Set { key_loc = set_key_loc; type_ = set_type }) ->
          GetSet { get_key_loc; get_type; set_key_loc; set_type }
        | _ -> Get { key_loc = get_key_loc; type_ = get_type })
      map

  let add_setter x set_key_loc set_type map =
    NameUtils.Map.adjust
      x
      (function
        | Some (Get { key_loc = get_key_loc; type_ = get_type }) ->
          GetSet { get_key_loc; get_type; set_key_loc; set_type }
        | _ -> Set { key_loc = set_key_loc; type_ = set_type })
      map

  let add_method x key_loc type_ = NameUtils.Map.add x (Method { key_loc; type_ })

  let rec unbind_this_method = function
    | DefT (r, FunT (static, ({ this_t = (_, This_Method { unbound = false }); _ } as ft))) ->
      let any_this_t = TypeTerm.(AnyT (r, AnyError None)) in
      DefT (r, FunT (static, { ft with this_t = (any_this_t, This_Method { unbound = true }) }))
    | DefT (r, PolyT { tparams_loc; tparams; t_out; id }) ->
      DefT (r, PolyT { tparams_loc; tparams; t_out = unbind_this_method t_out; id })
    | IntersectionT (r, rep) -> IntersectionT (r, InterRep.map unbind_this_method rep)
    | t -> t

  let extract_named_exports pmap =
    NameUtils.Map.fold
      (fun x p tmap ->
        match Property.read_t p with
        | Some type_ ->
          let preferred_def_locs =
            match p with
            | Field { preferred_def_locs; _ } -> preferred_def_locs
            | _ -> None
          in
          let type_ =
            match p with
            (* The following code explicitly encodes the following unsound behavior:

               ```
               // a.js
               declare class Foo { bar(): void }
               declare const foo: Foo;
               module.exports = foo;

               // b.js
               import {bar} from './a'; // no method unbinding error
               bar();
               ```
            *)
            | Method _ -> unbind_this_method type_
            | _ -> type_
          in
          NameUtils.Map.add x { name_loc = Property.read_loc p; preferred_def_locs; type_ } tmap
        | None -> tmap)
      pmap
      NameUtils.Map.empty

  let iter_t f = NameUtils.Map.iter (fun _ -> Property.iter_t f)

  let map_t f = NameUtils.Map.map (Property.map_t f)

  let map_fields f =
    NameUtils.Map.map (function
        | Field { preferred_def_locs; key_loc; type_; polarity } ->
          Field { preferred_def_locs; key_loc; type_ = f type_; polarity }
        | p -> p
        )

  let mapi_fields f =
    NameUtils.Map.mapi (fun k -> function
      | Field { preferred_def_locs; key_loc; type_; polarity } ->
        Field { preferred_def_locs; key_loc; type_ = f k type_; polarity }
      | p -> p
    )
end

and Opaque : sig
  type id =
    | UserDefinedOpaqueTypeId of ALoc.id
    (* A special norminal type to test t ~> union optimization *)
    | InternalEnforceUnionOptimized

  val equal_id : id -> id -> bool

  val string_of_id : id -> string
end = struct
  type id =
    | UserDefinedOpaqueTypeId of ALoc.id
    | InternalEnforceUnionOptimized

  let equal_id id1 id2 =
    match (id1, id2) with
    | (UserDefinedOpaqueTypeId id1, UserDefinedOpaqueTypeId id2) -> ALoc.equal_id id1 id2
    | (InternalEnforceUnionOptimized, InternalEnforceUnionOptimized) -> true
    | _ -> false

  let string_of_id = function
    | UserDefinedOpaqueTypeId id -> "user-defined " ^ ALoc.show_id id
    | InternalEnforceUnionOptimized -> "InternalEnforceUnionOptimized"
end

and Eval : sig
  type id

  val compare_id : id -> id -> int

  val id_of_aloc_id : type_sig:bool -> ALoc.id -> id

  val string_of_id : id -> string

  val generate_id : unit -> id

  val equal_id : id -> id -> bool

  module Map : WrappedMap.S with type key = id

  module Set : Flow_set.S with type elt = id
end = struct
  include Source_or_generated_id

  module Map : WrappedMap.S with type key = id = WrappedMap.Make (struct
    type key = id

    type t = key

    let compare = compare_id
  end)

  module Set : Flow_set.S with type elt = id = Flow_set.Make (struct
    type t = id

    let compare = compare_id
  end)
end

and Poly : sig
  type id

  val compare_id : id -> id -> int

  val equal_id : id -> id -> bool

  val id_of_aloc_id : type_sig:bool -> ALoc.id -> id

  val string_of_id : id -> string

  val stable_string_of_id : id -> string

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
  type t = TypeTerm.named_symbol NameUtils.Map.t

  type id

  module Map : WrappedMap.S with type key = id

  type map = t Map.t

  val mk_id : unit -> id

  val string_of_id : id -> string
end = struct
  type t = TypeTerm.named_symbol NameUtils.Map.t

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

  type union_kind =
    | ProvidersKind
    | ConditionalKind
    | ImplicitInstiationKind
    | ResolvedKind
    | LogicalKind
    | UnknownKind

  val same_source : t -> t -> bool

  val same_structure : t -> t -> bool

  (** build a rep from list of members *)
  val make :
    ?source_aloc:ALoc.id -> ?kind:union_kind -> TypeTerm.t -> TypeTerm.t -> TypeTerm.t list -> t

  (** members in declaration order *)
  val members : t -> TypeTerm.t list

  val members_nel : t -> TypeTerm.t * TypeTerm.t Nel.t

  val cons : TypeTerm.t -> t -> t

  val rev_append : t -> t -> t

  val is_synthetic : t -> bool

  val union_kind : t -> union_kind

  val disjoint_object_union_props : t -> NameUtils.Set.t option

  (** map rep r to rep r' along type mapping f. if nothing would be changed,
      returns the physically-identical rep. *)
  val ident_map : ?always_keep_source:bool -> (TypeTerm.t -> TypeTerm.t) -> t -> t

  (* Optimization *)

  module UnionEnumMap : WrappedMap.S with type key = UnionEnum.t

  type finally_optimized_rep =
    (* [None] in the position of tag means that the values used to create this
     * enum union were not of the same tag. *)
    | EnumUnion of UnionEnumSet.t * UnionEnum.tag option
    | PartiallyOptimizedUnionEnum of UnionEnumSet.t
    | AlmostDisjointUnionWithPossiblyNonUniqueKeys of
        TypeTerm.t Nel.t UnionEnumMap.t NameUtils.Map.t
    | PartiallyOptimizedAlmostDisjointUnionWithPossiblyNonUniqueKeys of
        TypeTerm.t Nel.t UnionEnumMap.t NameUtils.Map.t
    | Empty
    | Singleton of TypeTerm.t

  type 'loc optimized_error =
    | ContainsUnresolved of 'loc virtual_reason
    | NoCandidateMembers (* E.g. `number | string` *)
    | NoCommonKeys

  val optimize :
    t ->
    reason_of_t:(TypeTerm.t -> ALoc.t virtual_reason) ->
    reasonless_eq:(TypeTerm.t -> TypeTerm.t -> bool) ->
    flatten:(TypeTerm.t list -> TypeTerm.t list) ->
    find_resolved:(TypeTerm.t -> TypeTerm.t option) ->
    find_props:(Properties.id -> TypeTerm.property NameUtils.Map.t) ->
    unit

  val optimize_ :
    t ->
    reason_of_t:(TypeTerm.t -> reason) ->
    reasonless_eq:(TypeTerm.t -> TypeTerm.t -> bool) ->
    flatten:(TypeTerm.t list -> TypeTerm.t list) ->
    find_resolved:(TypeTerm.t -> TypeTerm.t option) ->
    find_props:(Properties.id -> TypeTerm.property NameUtils.Map.t) ->
    (finally_optimized_rep, ALoc.t optimized_error) result

  val set_optimize : t -> (finally_optimized_rep, 'loc optimized_error) result -> unit

  val optimize_enum_only : t -> flatten:(TypeTerm.t list -> TypeTerm.t list) -> unit

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

  val check_enum_with_tag : t -> (UnionEnumSet.t * UnionEnum.tag option) option

  val string_of_specialization_ : finally_optimized_rep option -> string

  val string_of_specialization : t -> string

  val tag_of_member : TypeTerm.t -> UnionEnum.tag option
end = struct
  (* canonicalize a type w.r.t. enum membership *)
  let canon =
    TypeTerm.(
      function
      | DefT (_, SingletonStrT { value = lit; _ }) -> Some (UnionEnum.Str lit)
      | DefT (_, NumericStrKeyT (_, s)) -> Some (UnionEnum.Str (OrdinaryName s))
      | DefT (_, SingletonNumT { value = lit; _ }) -> Some (UnionEnum.Num lit)
      | DefT (_, SingletonBigIntT { value = lit; _ }) -> Some (UnionEnum.BigInt lit)
      | DefT (_, SingletonBoolT { value = lit; _ }) -> Some (UnionEnum.Bool lit)
      | DefT (_, VoidT) -> Some UnionEnum.Void
      | DefT (_, NullT) -> Some UnionEnum.Null
      | _ -> None
    )

  let is_base =
    TypeTerm.(
      function
      | DefT (_, NumericStrKeyT _)
      | DefT (_, SingletonStrT { from_annot = true; _ })
      | DefT (_, SingletonNumT { from_annot = true; _ })
      | DefT (_, SingletonBigIntT { from_annot = true; _ })
      | DefT (_, SingletonBoolT { from_annot = true; _ })
      | DefT (_, VoidT)
      | DefT (_, NullT) ->
        true
      | _ -> false
    )

  (* disjoint unions are stored as singleton type maps *)
  module UnionEnumMap = WrappedMap.Make (UnionEnum)

  type finally_optimized_rep =
    | EnumUnion of UnionEnumSet.t * UnionEnum.tag option
    | PartiallyOptimizedUnionEnum of UnionEnumSet.t
    | AlmostDisjointUnionWithPossiblyNonUniqueKeys of
        TypeTerm.t Nel.t UnionEnumMap.t NameUtils.Map.t
    | PartiallyOptimizedAlmostDisjointUnionWithPossiblyNonUniqueKeys of
        TypeTerm.t Nel.t UnionEnumMap.t NameUtils.Map.t
    | Empty
    | Singleton of TypeTerm.t

  type 'loc optimized_error =
    | ContainsUnresolved of 'loc virtual_reason
    | NoCandidateMembers
    | NoCommonKeys

  type union_kind =
    | ProvidersKind
    | ConditionalKind
    | ImplicitInstiationKind
    | ResolvedKind
    | LogicalKind
    | UnknownKind

  type t = {
    t0: TypeTerm.t;
    t1: TypeTerm.t;
    ts: TypeTerm.t list;
    (* optional source location of the union,
     * used as identity of the union for fast-path check. *)
    source_aloc: ALoc.id option;
    (* if union is an enum (set of singletons over a common base) then Some (base, set)
     * (additional specializations probably to come) *)
    specialization: finally_optimized_rep option ref;
    (* A union is synthetic roughly when it does not emerge from an annotation,
     * e.g. when it emerges as the collection of lower bounds during implicit
     * instantiation. *)
    kind: union_kind;
  }

  let same_source { source_aloc = s1; _ } { source_aloc = s2; _ } =
    match (s1, s2) with
    | (Some id1, Some id2) -> id1 = id2
    | _ -> false

  let same_structure { t0; t1; ts; _ } { t0 = t0'; t1 = t1'; ts = ts'; _ } =
    t0 = t0' && t1 = t1' && ts = ts'

  let tag_of_member =
    let open TypeTerm in
    let open UnionEnum in
    function
    | DefT (_, SingletonStrT _) -> Some SingletonStrTag
    | DefT (_, StrGeneralT _) -> Some StrTag
    | DefT (_, NumericStrKeyT _) -> Some NumericStrKeyTag
    | DefT (_, SingletonNumT _) -> Some SingletonNumTag
    | DefT (_, NumGeneralT _) -> Some NumTag
    | DefT (_, SingletonBigIntT _) -> Some SingletonBigIntTag
    | DefT (_, BigIntGeneralT _) -> Some BingIntTag
    | DefT (_, SingletonBoolT _) -> Some SingletonBoolTag
    | DefT (_, BoolGeneralT) -> Some BoolTag
    | DefT (_, VoidT) -> Some VoidTag
    | DefT (_, NullT) -> Some NullTag
    | _ -> None

  let is_synthetic { kind; _ } =
    match kind with
    | UnknownKind -> false
    | _ -> true

  let union_kind { kind; _ } = kind

  let disjoint_object_union_props { specialization; _ } =
    match !specialization with
    | Some (AlmostDisjointUnionWithPossiblyNonUniqueKeys map)
    | Some (PartiallyOptimizedAlmostDisjointUnionWithPossiblyNonUniqueKeys map) ->
      Some (NameUtils.Map.keys map |> NameUtils.Set.of_list)
    | _ -> None

  (** given a list of members, build a rep.
      specialized reps are used on compatible type lists *)
  let make =
    let rec mk_enum (tset, tag) = function
      | [] -> Some (tset, tag)
      | t :: ts -> begin
        match canon t with
        | Some tcanon when is_base t ->
          let tag = Utils_js.ite (tag = tag_of_member t) tag None in
          mk_enum (UnionEnumSet.add tcanon tset, tag) ts
        | _ -> None
      end
    in
    fun ?source_aloc ?(kind = UnknownKind) t0 t1 ts ->
      let enum =
        Base.Option.(
          mk_enum (UnionEnumSet.empty, tag_of_member t0) (t0 :: t1 :: ts) >>| fun (tset, tag) ->
          EnumUnion (tset, tag)
        )
      in
      { t0; t1; ts; source_aloc; specialization = ref enum; kind }

  let members { t0; t1; ts; _ } = t0 :: t1 :: ts

  let members_nel { t0; t1; ts; _ } = (t0, (t1, ts))

  let cons t0 { t0 = t1; t1 = t2; ts; _ } = make t0 t1 (t2 :: ts)

  let rev_append rep1 rep2 =
    match List.rev_append (members rep1) (members rep2) with
    | t0 :: t1 :: ts -> make t0 t1 ts
    | _ -> failwith "impossible"

  let ident_map
      ?(always_keep_source = false) f ({ t0; t1; ts; source_aloc = source; kind; _ } as rep) =
    let t0_ = f t0 in
    let t1_ = f t1 in
    let ts_ = ListUtils.ident_map f ts in
    let changed = t0_ != t0 || t1_ != t1 || ts_ != ts in
    if changed then
      let source_aloc =
        if always_keep_source then
          source
        else
          None
      in
      make ?source_aloc ~kind t0_ t1_ ts_
    else
      rep

  (********** Optimizations **********)

  let is_optimized_finally { specialization; _ } = !specialization <> None

  (* Private helper, must be called after full resolution. Ideally would be returned as a bit by
     TypeTerm.union_flatten, and kept in sync there. *)
  let has_unflattened_types ts =
    let open TypeTerm in
    let exception Found of TypeTerm.t in
    try
      List.iter
        (fun t ->
          match t with
          (* the only unresolved tvars at this point are those that instantiate polymorphic types *)
          | OpenT _
          (* some types may not be evaluated yet; TODO *)
          | EvalT _
          | TypeAppT _
          | KeysT _
          | IntersectionT _
          (* other types might wrap parts that are accessible directly *)
          | OpaqueT _
          | DefT (_, InstanceT _)
          | DefT (_, PolyT _) ->
            raise (Found t)
          | _ -> ())
        ts;
      None
    with
    | Found t -> Some t

  let enum_optimize =
    let split_enum ts =
      List.fold_left
        (fun (tset, acc_tag, partial) t ->
          match canon t with
          | Some tcanon when is_base t ->
            let acc_tag = Utils_js.ite (acc_tag = tag_of_member t) acc_tag None in
            (UnionEnumSet.add tcanon tset, acc_tag, partial)
          | _ -> (tset, None, true))
        (UnionEnumSet.empty, tag_of_member (List.hd ts), false)
        ts
    in
    function
    | [] -> Ok Empty
    | [t] -> Ok (Singleton t)
    | ts ->
      let (tset, tag, partial) = split_enum ts in
      if partial then
        if UnionEnumSet.is_empty tset then
          Error NoCandidateMembers
        else
          Ok (PartiallyOptimizedUnionEnum tset)
      else
        Ok (EnumUnion (tset, tag))

  let canon_prop find_resolved p = Base.Option.(Property.read_t p >>= find_resolved >>= canon)

  let base_prop find_resolved p =
    match Base.Option.(Property.read_t p >>= find_resolved) with
    | Some t when is_base t -> canon t
    | _ -> None

  let props_of find_props t =
    TypeTerm.(
      match t with
      | DefT (_, ObjT { props_tmap; _ }) -> Some (find_props props_tmap)
      | _ -> None
    )

  let disjoint_union_optimize =
    let base_props_of find_resolved find_props t =
      Base.Option.map (props_of find_props t) ~f:(fun prop_map ->
          NameUtils.Map.fold
            (fun key p acc ->
              match base_prop find_resolved p with
              | Some enum -> NameUtils.Map.add key (enum, t) acc
              | None -> acc)
            prop_map
            NameUtils.Map.empty
      )
    in
    (* Returns a tuple of (candidates, partial):
     *  - [candidates] is a list that contains a candidate for each union member.
     *    A candidate is a mapping from keys in that object to a UnionEnum value.
     *    Properties that do not have a UnionEnum representation are ignored.
     *  - [partial] is true iff there exist non-object-like members in the union
     *)
    let split_disjoint_union find_resolved find_props ts =
      List.fold_left
        (fun (candidates, partial) t ->
          match base_props_of find_resolved find_props t with
          | None -> (candidates, true)
          | Some base_props -> (base_props :: candidates, partial))
        ([], false)
        ts
    in
    let almost_unique_values =
      let rec almost_unique_values ~reasonless_eq (hybrid_idx : TypeTerm.t Nel.t UnionEnumMap.t) =
        function
        | [] -> hybrid_idx
        | (enum, t) :: values -> begin
          let hybrid_idx =
            UnionEnumMap.adjust
              enum
              (function
                | None -> Nel.one t
                | Some l when Nel.exists (reasonless_eq t) l ->
                  (* This corresponds to the case
                   * type T = { f: "a" };
                   * type Union = T | T;
                   *)
                  l
                | Some l -> Nel.cons t l)
              hybrid_idx
          in
          almost_unique_values ~reasonless_eq hybrid_idx values
        end
      in
      almost_unique_values UnionEnumMap.empty
    in
    let almost_unique ~reasonless_eq idx =
      NameUtils.Map.fold
        (fun key values acc ->
          let hybrid_idx = almost_unique_values ~reasonless_eq values in
          NameUtils.Map.add key hybrid_idx acc)
        idx
        NameUtils.Map.empty
    in
    let intersect_props (base_props, candidates) =
      (* Compute the intersection of properties of objects that have singleton types *)
      let init = NameUtils.Map.map (fun enum_t -> [enum_t]) base_props in
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
    fun ~reasonless_eq ~find_resolved ~find_props -> function
      | [] -> Ok Empty
      | [t] -> Ok (Singleton t)
      | ts ->
        let (candidates, partial) = split_disjoint_union find_resolved find_props ts in
        begin
          match candidates with
          | [] -> Error NoCandidateMembers
          | c :: cs ->
            let idx = intersect_props (c, cs) in
            if NameUtils.Map.is_empty idx then
              Error NoCommonKeys
            else
              (* Ensure that enums map to unique types *)
              let hybrid_map = almost_unique ~reasonless_eq idx in
              if partial then
                Ok (PartiallyOptimizedAlmostDisjointUnionWithPossiblyNonUniqueKeys hybrid_map)
              else
                Ok (AlmostDisjointUnionWithPossiblyNonUniqueKeys hybrid_map)
        end

  let optimize_ rep ~reason_of_t ~reasonless_eq ~flatten ~find_resolved ~find_props =
    let ts = flatten (members rep) in
    match has_unflattened_types ts with
    | None ->
      let opt = enum_optimize ts in
      (match opt with
      | Error _ -> disjoint_union_optimize ~reasonless_eq ~find_resolved ~find_props ts
      | _ -> opt)
    | Some t -> Error (ContainsUnresolved (reason_of_t t))

  let set_optimize rep opt =
    Base.Result.iter opt ~f:(fun opt ->
        let { specialization; _ } = rep in
        specialization := Some opt
    )

  let optimize rep ~reason_of_t ~reasonless_eq ~flatten ~find_resolved ~find_props =
    let opt = optimize_ rep ~reason_of_t ~reasonless_eq ~flatten ~find_resolved ~find_props in
    set_optimize rep opt

  let optimize_enum_only rep ~flatten =
    let ts = flatten (members rep) in
    if Base.Option.is_none (has_unflattened_types ts) then
      match enum_optimize ts with
      | Ok (EnumUnion _ as opt) ->
        let { specialization; _ } = rep in
        specialization := Some opt
      | _ -> ()

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
  let quick_mem_enum ~quick_subtype l { specialization; _ } =
    match canon l with
    | Some tcanon -> begin
      match !specialization with
      | None -> Unknown
      | Some Empty -> No
      | Some (Singleton t) ->
        if quick_subtype l t then
          Yes
        else
          Conditional t
      | Some (AlmostDisjointUnionWithPossiblyNonUniqueKeys _) -> No
      | Some (PartiallyOptimizedAlmostDisjointUnionWithPossiblyNonUniqueKeys _) -> Unknown
      | Some (EnumUnion (tset, _)) ->
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

  let lookup_almost_disjoint_union find_resolved prop_map ~partial map =
    NameUtils.Map.fold
      (fun key idx acc ->
        if acc <> Unknown then
          acc
        else
          match NameUtils.Map.find_opt key prop_map with
          | Some p -> begin
            match canon_prop find_resolved p with
            | Some enum -> begin
              match UnionEnumMap.find_opt enum idx with
              | Some (t, []) -> Conditional t
              | Some (_, _ :: _) -> Unknown
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
  let quick_mem_disjoint_union ~find_resolved ~find_props ~quick_subtype l { specialization; _ } =
    match props_of find_props l with
    | Some prop_map -> begin
      match !specialization with
      | None -> Unknown
      | Some Empty -> No
      | Some (Singleton t) ->
        if quick_subtype l t then
          Yes
        else
          Conditional t
      | Some (AlmostDisjointUnionWithPossiblyNonUniqueKeys map) ->
        lookup_almost_disjoint_union find_resolved prop_map ~partial:false map
      | Some (PartiallyOptimizedAlmostDisjointUnionWithPossiblyNonUniqueKeys map) ->
        lookup_almost_disjoint_union find_resolved prop_map ~partial:true map
      | Some (EnumUnion _) -> No
      | Some (PartiallyOptimizedUnionEnum _) -> Unknown
    end
    | _ -> failwith "quick_mem_disjoint_union is defined only on object / exact object types"

  let check_enum { specialization; _ } =
    match !specialization with
    | Some (EnumUnion (enums, _)) -> Some enums
    | _ -> None

  let check_enum_with_tag { specialization; _ } =
    match !specialization with
    | Some (EnumUnion (enums, tag)) -> Some (enums, tag)
    | _ -> None

  let string_of_specialization_ = function
    | Some (EnumUnion _) -> "Enum"
    | Some Empty -> "Empty"
    | Some (Singleton _) -> "Singleton"
    | Some (PartiallyOptimizedAlmostDisjointUnionWithPossiblyNonUniqueKeys _) ->
      "Partially Optimized Almost Disjoint Union with possibly non-unique keys"
    | Some (AlmostDisjointUnionWithPossiblyNonUniqueKeys _) ->
      "Almost Disjoint Union with possibly non-unique keys"
    | Some (PartiallyOptimizedUnionEnum _) -> "Partially Optimized Enum"
    | None -> "No Specialization"

  let string_of_specialization { specialization; _ } = string_of_specialization_ !specialization
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

  type inter_kind =
    | ImplicitInstiationKind
    | UnknownKind

  (** build rep from list of members *)
  val make : ?kind:inter_kind -> TypeTerm.t -> TypeTerm.t -> TypeTerm.t list -> t

  (** member list in declaration order *)
  val members : t -> TypeTerm.t list

  val members_nel : t -> TypeTerm.t * TypeTerm.t Nel.t

  (** map rep r to rep r' along type mapping f. drops history *)
  val map : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val append : ?kind:inter_kind -> TypeTerm.t list -> t -> t

  (** map rep r to rep r' along type mapping f. drops history. if nothing would
      be changed, returns the physically-identical rep. *)
  val ident_map : (TypeTerm.t -> TypeTerm.t) -> t -> t

  val inter_kind : t -> inter_kind
end = struct
  type inter_kind =
    | ImplicitInstiationKind
    | UnknownKind

  (** intersection rep is:
      - member list in declaration order
   *)
  type t = TypeTerm.t * TypeTerm.t * TypeTerm.t list * inter_kind

  let make ?(kind = UnknownKind) t0 t1 ts = (t0, t1, ts, kind)

  let members (t0, t1, ts, _) = t0 :: t1 :: ts

  let members_nel (t0, t1, ts, _) = (t0, (t1, ts))

  let map f (t0, t1, ts, kind) = make ~kind (f t0) (f t1) (Base.List.map ~f ts)

  let mem t (t0, t1, ts, _) = t = t0 || t = t1 || Base.List.mem ts t ~equal:( = )

  let append ?(kind = UnknownKind) ts2 rep =
    let ts2 = Base.List.filter ts2 ~f:(fun t -> not (mem t rep)) in
    let (t0, t1, ts1, _) = rep in
    make ~kind t0 t1 (Base.List.append ts1 ts2)

  let ident_map f ((t0, t1, ts, kind) as rep) =
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
      make ~kind t0_ t1_ (List.rev rev_ts)
    else
      rep

  let inter_kind (_, _, _, kind) = kind
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
    frozen: bool;
    generics: Generic.spread_id;
    interface: (TypeTerm.static * TypeTerm.insttype) option;
    reachable_targs: (TypeTerm.t * Polarity.t) list;
  }

  and props = prop NameUtils.Map.t

  and prop = {
    prop_t: TypeTerm.t;
    is_own: bool;
    is_method: bool;
    polarity: Polarity.t;
    key_loc: ALoc.t option;
  }

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
      reachable_targs: (TypeTerm.t * Polarity.t) list;
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
      | Sealed
      | Frozen
      | As_Const

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
      | SpreadReversal
      | ReactConfigMerge of Polarity.t
      | Omit
  end

  module ReactConfig : sig
    type state =
      | Config of { component_default_props: TypeTerm.t option }
      | Defaults of { config: resolved }

    type ref_manipulation =
      | FilterRef
      | KeepRef
      | AddRef of TypeTerm.t
  end

  type tool =
    | MakeExact
    | ReadOnly
    | Partial
    | Required
    | Spread of Spread.target * Spread.state
    | Rest of Rest.merge_mode * Rest.state
    | ReactConfig of {
        state: ReactConfig.state;
        ref_manipulation: ReactConfig.ref_manipulation;
      }
    | ReactCheckComponentConfig of Property.t NameUtils.Map.t
    | ObjectRep
    | ObjectMap of {
        prop_type: TypeTerm.t;
        mapped_type_flags: TypeTerm.mapped_type_flags;
        selected_keys_opt: TypeTerm.t option;
      }
end =
  Object

and React : sig
  type tool =
    | CreateElement of {
        component: TypeTerm.t;
        jsx_props: TypeTerm.t;
        tout: TypeTerm.t_out;
        targs: TypeTerm.targ list option;
        return_hint: TypeTerm.lazy_hint_t;
        record_monomorphized_result: bool;
        inferred_targs: (TypeTerm.t * Subst_name.Name.t) list option;
        specialized_component: TypeTerm.specialized_callee option;
      }
    | ConfigCheck of {
        props: TypeTerm.t;
        instance: TypeTerm.component_instance;
      }
    | GetProps of TypeTerm.t_out
    | GetConfig of TypeTerm.t_out
    | GetRef of TypeTerm.t_out
end =
  React

and ArithKind : sig
  type t' =
    | Plus
    | RShift3
    | Other

  type t = string * t'

  val arith_kind_of_binary_operator : Flow_ast.Expression.Binary.operator -> t

  val arith_kind_of_assignment_operator : Flow_ast.Expression.Assignment.operator -> t

  val string_of_arith_kind : t -> string
end = struct
  open Flow_ast.Expression

  type t' =
    | Plus
    | RShift3
    | Other

  type t = string * t'

  let arith_kind_of_binary_operator op =
    ( Flow_ast_utils.string_of_binary_operator op,
      match op with
      | Binary.Plus -> Plus
      | Binary.RShift3 -> RShift3
      | _ -> Other
    )

  let arith_kind_of_assignment_operator op =
    ( Flow_ast_utils.string_of_assignment_operator op,
      match op with
      | Assignment.PlusAssign -> Plus
      | Assignment.RShift3Assign -> RShift3
      | _ -> Other
    )

  let string_of_arith_kind (s, _) = s
end

and UnaryArithKind : sig
  type t =
    | Plus
    | Minus
    | BitNot
    | Update
end =
  UnaryArithKind

and UseTypeSet : (Flow_set.S with type elt = TypeTerm.use_t) = Flow_set.Make (struct
  type elt = TypeTerm.use_t

  type t = elt

  let compare = TypeTerm.use_t_compare
end)

(* The typechecking algorithm often needs to maintain sets of types, or more
   generally, maps of types (for logging we need to associate some provenanced
   information to types).
   Type terms may also contain internal sets or maps.
*)
and TypeSet : (Flow_set.S with type elt = TypeTerm.t) = Flow_set.Make (struct
  type elt = TypeTerm.t

  type t = elt

  let compare = TypeTerm.type_term_compare
end)

and TypeCollector : sig
  type t

  val create : unit -> t

  val add : t -> TypeTerm.t -> unit

  val collect : t -> TypeTerm.t list

  val iter : t -> f:(TypeTerm.t -> unit) -> unit
end = struct
  type t = TypeSet.t ref

  let create () = ref TypeSet.empty

  let add collector = function
    | TypeTerm.(DefT (_, EmptyT)) -> ()
    | t -> collector := TypeSet.add t !collector

  let collect collector = TypeSet.elements !collector

  let iter collector ~f = TypeSet.iter f !collector
end

let unknown_use = TypeTerm.(Op UnknownUse)

let name_of_propref = function
  | TypeTerm.Named { name; _ } -> Some name
  | TypeTerm.Computed _ -> None

module TypeMap = Flow_map.Make (struct
  type key = TypeTerm.t

  type t = key

  let compare = TypeTerm.type_term_compare
end)

module Constraint = struct
  module UseTypeKey = struct
    type speculation_id = int

    type case_id = int

    type t = TypeTerm.use_t * (speculation_id * case_id) option

    let compare (x, assoc1) (y, assoc2) =
      let v = [%derive.ord: (int * int) option] assoc1 assoc2 in
      if v = 0 then
        TypeTerm.use_t_compare x y
      else
        v
  end

  module UseTypeMap = Flow_map.Make (UseTypeKey)

  module ForcingState : sig
    type ('a, 'b) state

    type t = (TypeTerm.t, Reason.t) state

    type module_type = ((TypeTerm.moduletype, TypeTerm.t) result, Reason.t) state

    val of_lazy_t : error_reason:Reason.t -> TypeTerm.t Lazy.t -> t

    val of_non_lazy_t : TypeTerm.t -> t

    val of_lazy_module : Reason.t * TypeTerm.moduletype Lazy.t -> module_type

    val of_error_module : TypeTerm.t -> module_type

    val force : on_error:('b -> 'a) -> ('a, 'b) state -> 'a

    val already_forced_with_cyclic_error : ('a, 'b) state -> bool

    val get_forced_for_debugging : ('a, 'b) state -> 'a option

    val copy :
      on_error:('b -> 'a) -> visit_for_copier:('a -> unit) -> ('a, 'b) state -> ('a, 'b) state
  end = struct
    type 'a status =
      | Unforced
      | Forcing
      | Forced
      | ForcedWithCyclicError of 'a

    type ('a, 'b) state = {
      valid: 'a Lazy.t;
      error_reason: 'b option;
      mutable status: 'a status;
    }

    type t = (TypeTerm.t, Reason.t) state

    type module_type = ((TypeTerm.moduletype, TypeTerm.t) result, Reason.t) state

    let of_lazy_t ~error_reason valid =
      { valid; error_reason = Some error_reason; status = Unforced }

    let of_non_lazy_t t = { valid = lazy t; error_reason = None; status = Forced }

    let of_lazy_module (reason, lazy_module) =
      {
        valid = Lazy.map (fun m -> Ok m) lazy_module;
        error_reason = Some reason;
        status = Unforced;
      }

    let of_error_module t = { valid = lazy (Error t); error_reason = None; status = Forced }

    let force ~on_error s =
      match s.status with
      | Unforced ->
        s.status <- Forcing;
        let t = Lazy.force_val s.valid in
        (match s.status with
        | Forcing ->
          s.status <- Forced;
          t
        | ForcedWithCyclicError t -> t
        | Unforced -> failwith "Invalid state Unforced"
        | Forced -> failwith "Invalid state Forced")
      | Forced -> Lazy.force_val s.valid
      | ForcedWithCyclicError t -> t
      | Forcing ->
        let t = on_error (Base.Option.value_exn s.error_reason) in
        s.status <- ForcedWithCyclicError t;
        t

    let already_forced_with_cyclic_error s =
      match s.status with
      | Unforced
      | Forcing
      | Forced ->
        false
      | ForcedWithCyclicError _ -> true

    let get_forced_for_debugging s =
      match s.status with
      | Unforced
      | Forcing ->
        None
      | Forced -> Some (Lazy.force_val s.valid)
      | ForcedWithCyclicError t -> Some t

    let copy ~on_error ~visit_for_copier s =
      {
        valid =
          lazy
            (let t = force ~on_error s in
             visit_for_copier t;
             t
            );
        error_reason = s.error_reason;
        status = Unforced;
      }
  end

  (** Constraints carry type information that narrows down the possible solutions
      of tvar, and are of two kinds:

      - A Resolved constraint contains a concrete type that is considered by the
        type system to be the solution of the tvar carrying the constraint. In other
        words, the tvar is equivalent to this concrete type in all respects.

      - Unresolved constraints contain bounds that carry both concrete types and
        other tvars as upper and lower bounds (see below). *)
  type constraints =
    | Resolved of TypeTerm.t
    | Unresolved of bounds
    | FullyResolved of ForcingState.t

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
    mutable lower: (DepthTrace.t * TypeTerm.use_op) TypeMap.t;
    mutable upper: DepthTrace.t UseTypeMap.t;
    mutable lowertvars: (DepthTrace.t * TypeTerm.use_op) IMap.t;
    mutable uppertvars: (DepthTrace.t * TypeTerm.use_op) IMap.t;
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

  let new_unresolved_root () = create_root (Unresolved (new_bounds ()))
end

(**************************)
(* Annotation constraints *)
(**************************)
module AConstraint = struct
  type op =
    | Annot_ConcretizeForImportsExports of Reason.t * (TypeTerm.t -> TypeTerm.t)
    | Annot_ConcretizeForCJSExtractNamedExportsAndTypeExports of Reason.t
    | Annot_ConcretizeForInspection of Reason.t * TypeCollector.t
    (* Imports *)
    | Annot_ImportTypeofT of Reason.reason * string
    (* Exports *)
    | Annot_AssertExportIsTypeT of Reason.t * name
    (* Other operations *)
    | Annot_SpecializeT of TypeTerm.use_op * Reason.t * Reason.t * TypeTerm.t list option
    | Annot_ThisSpecializeT of Reason.t * TypeTerm.t
    | Annot_UseT_TypeT of Reason.t * TypeTerm.type_t_kind
    | Annot_GetTypeFromNamespaceT of {
        use_op: TypeTerm.use_op;
        reason: Reason.t;
        prop_ref: Reason.t * name;
      }
    | Annot_GetEnumT of Reason.t
    | Annot_GetPropT of {
        reason: Reason.t;
        use_op: TypeTerm.use_op;
        from_annot: bool;
        prop_ref: TypeTerm.propref;
      }
    | Annot_GetElemT of Reason.t * TypeTerm.use_op * TypeTerm.t (* key *)
    | Annot_ElemT of {
        reason: Reason.t;
        use_op: TypeTerm.use_op;
        from_annot: bool;
        source: TypeTerm.t;
      }
      (* read action only *)
    | Annot_GetStaticsT of Reason.t
    | Annot_LookupT of Reason.t * TypeTerm.use_op * TypeTerm.propref * TypeTerm.t
    | Annot_ObjKitT of Reason.t * TypeTerm.use_op * Object.resolve_tool * Object.tool
    | Annot_ObjTestProtoT of Reason.t
    | Annot_MixinT of Reason.t
    | Annot_ArithT of {
        reason: Reason.t;
        flip: bool;
        rhs_t: TypeTerm.t;
        kind: ArithKind.t;
      }
    | Annot_UnaryArithT of Reason.t * UnaryArithKind.t
    | Annot_NotT of Reason.t
    | Annot_ObjKeyMirror of Reason.t
    | Annot_DeepReadOnlyT of Reason.t * ALoc.t * TypeTerm.dro_type
    | Annot_GetKeysT of Reason.t
    | Annot_ToStringT of {
        orig_t: TypeTerm.t option;
        reason: Reason.t;
      }
    | Annot_ObjRestT of Reason.t * string list
    | Annot_GetValuesT of Reason.t

  (** This kind of constraint is meant to represent type annotations. Unlike the
      constraints described above that may gradually evolve towards the solution
      of an unresolved tvar, annotation constraints are resolved immediately.

      There are three kinds of constraints:

      - [Annot_unresolved _] is a constraint for which we have no information yet.
        This state is associated, for example, with a type variable representing
        a type alias that we have just started visiting, and so do not have a
        type for its body yet.

      - [Annot_op { op; id; _ }] expresses the fact that the current variable is the
        result of applying the operation [op] on the type that another annotation
        variable [id] will resolve to.

      An annotation variable starts off in the Annot_unresolved or Annot_op state
      and is resolved in one step by removing it.

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
  type t =
    | Annot_unresolved of {
        reason: Reason.t;
        mutable dependents: ISet.t;
      }
    | Annot_op of {
        op: op;
        id: int;
        mutable dependents: ISet.t;
      }

  let string_of_operation = function
    | Annot_SpecializeT _ -> "Annot_SpecializeT"
    | Annot_DeepReadOnlyT _ -> "Annot_DeepReadOnlyT"
    | Annot_ThisSpecializeT _ -> "Annot_ThisSpecializeT"
    | Annot_UseT_TypeT _ -> "Annot_UseT_TypeT"
    | Annot_ConcretizeForImportsExports _ -> "Annot_ConcretizeForImportsExports"
    | Annot_ConcretizeForCJSExtractNamedExportsAndTypeExports _ ->
      "Annot_ConcretizeForCJSExtractNamedExportsAndTypeExports"
    | Annot_ConcretizeForInspection _ -> "Annot_ConcretizeForInspection"
    | Annot_ImportTypeofT _ -> "Annot_ImportTypeofT"
    | Annot_AssertExportIsTypeT _ -> "Annot_AssertExportIsTypeT"
    | Annot_GetTypeFromNamespaceT _ -> "Annot_GetTypeFromNamespaceT"
    | Annot_GetPropT _ -> "Annot_GetPropT"
    | Annot_GetElemT _ -> "Annot_GetElemT"
    | Annot_GetEnumT _ -> "Annot_GetEnumT"
    | Annot_ElemT _ -> "Annot_ElemT"
    | Annot_GetStaticsT _ -> "Annot_GetStaticsT"
    | Annot_LookupT _ -> "Annot_LookupT"
    | Annot_ObjKitT _ -> "Annot_ObjKitT"
    | Annot_ObjTestProtoT _ -> "Annot_ObjTestProtoT"
    | Annot_MixinT _ -> "Annot_MixinT"
    | Annot_ArithT _ -> "Annot_ArithT"
    | Annot_UnaryArithT _ -> "Annot_UnaryArithT"
    | Annot_NotT _ -> "Annot_NotT"
    | Annot_ObjKeyMirror _ -> "Annot_ObjKeyMirror"
    | Annot_GetKeysT _ -> "Annot_GetKeysT"
    | Annot_ToStringT _ -> "Annot_ToStringT"
    | Annot_ObjRestT _ -> "Annot_ObjRestT"
    | Annot_GetValuesT _ -> "Annot_GetValuesT"

  let reason_of_op = function
    | Annot_ConcretizeForImportsExports (r, _)
    | Annot_ConcretizeForCJSExtractNamedExportsAndTypeExports r
    | Annot_ConcretizeForInspection (r, _)
    | Annot_SpecializeT (_, r, _, _)
    | Annot_ThisSpecializeT (r, _)
    | Annot_UseT_TypeT (r, _)
    | Annot_ImportTypeofT (r, _)
    | Annot_AssertExportIsTypeT (r, _)
    | Annot_GetTypeFromNamespaceT { reason = r; _ }
    | Annot_GetPropT { reason = r; _ }
    | Annot_GetElemT (r, _, _)
    | Annot_GetEnumT r
    | Annot_ElemT { reason = r; _ }
    | Annot_GetStaticsT r
    | Annot_LookupT (r, _, _, _)
    | Annot_ObjKitT (r, _, _, _)
    | Annot_ObjTestProtoT r
    | Annot_ArithT { reason = r; _ }
    | Annot_UnaryArithT (r, _)
    | Annot_NotT r
    | Annot_MixinT r
    | Annot_ObjKeyMirror r
    | Annot_GetKeysT r
    | Annot_ToStringT { reason = r; _ }
    | Annot_ObjRestT (r, _)
    | Annot_GetValuesT r
    | Annot_DeepReadOnlyT (r, _, _) ->
      r

  let use_op_of_operation = function
    | Annot_SpecializeT (use_op, _, _, _)
    | Annot_GetTypeFromNamespaceT { use_op; _ }
    | Annot_GetPropT { use_op; _ }
    | Annot_GetElemT (_, use_op, _)
    | Annot_ElemT { use_op; _ }
    | Annot_LookupT (_, use_op, _, _)
    | Annot_ObjKitT (_, use_op, _, _) ->
      Some use_op
    | Annot_ThisSpecializeT _
    | Annot_UseT_TypeT _
    | Annot_ConcretizeForImportsExports _
    | Annot_ConcretizeForCJSExtractNamedExportsAndTypeExports _
    | Annot_ConcretizeForInspection _
    | Annot_ImportTypeofT _
    | Annot_AssertExportIsTypeT _
    | Annot_GetStaticsT _
    | Annot_ObjTestProtoT _
    | Annot_ArithT _
    | Annot_UnaryArithT _
    | Annot_NotT _
    | Annot_MixinT _
    | Annot_ObjKeyMirror _
    | Annot_GetKeysT _
    | Annot_DeepReadOnlyT _
    | Annot_ToStringT _
    | Annot_ObjRestT _
    | Annot_GetValuesT _
    | Annot_GetEnumT _ ->
      None

  (* Used to produce prettier error messages for annotation inference. *)
  let display_reason_of_op = function
    | Annot_ObjKitT (r, _, _, tool) ->
      let desc =
        RCustom
          Object.(
            match tool with
            | MakeExact -> "exact"
            | ReactCheckComponentConfig _ -> "react check component config"
            | ReadOnly -> "readonly"
            | Partial -> "partial"
            | Required -> "required"
            | Spread _ -> "spread"
            | Rest _ -> "rest"
            | ReactConfig _ -> "react config"
            | ObjectRep -> "object"
            | ObjectMap _ -> "mapped type"
          )
      in
      replace_desc_reason desc r
    | Annot_GetStaticsT r -> replace_desc_reason (RCustom "statics") r
    | Annot_MixinT r -> replace_desc_reason (RCustom "mixins") r
    | Annot_UnaryArithT (r, _) -> replace_desc_reason (RCustom "unary minus") r
    | Annot_NotT r -> replace_desc_reason (RCustom "unary not") r
    | Annot_GetPropT { reason; prop_ref; _ } ->
      replace_desc_reason (RProperty (name_of_propref prop_ref)) reason
    | Annot_ObjRestT (r, _) -> replace_desc_reason (RCustom "rest") r
    | r -> reason_of_op r

  let to_annot_op_exn = function
    | Annot_unresolved _ -> failwith "to_annot_op_exn on unresolved"
    | Annot_op { op; _ } -> op

  let deps_of_constraint = function
    | Annot_unresolved { dependents; _ } -> dependents
    | Annot_op { dependents; _ } -> dependents

  let update_deps_of_constraint ~f = function
    | Annot_unresolved d -> d.dependents <- f d.dependents
    | Annot_op d -> d.dependents <- f d.dependents
end

module TypeContext = struct
  type t = {
    (* map from tvar ids to nodes (type info structures) *)
    graph: Constraint.node IMap.t;
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

module SubstCacheMap = WrappedMap.Make (struct
  type t = Poly.id * TypeTerm.t list

  let compare = Stdlib.compare
end)

module EvalIdCacheMap = WrappedMap.Make (struct
  type t = Eval.id

  let compare = Stdlib.compare
end)

module EvalIdSet = Flow_set.Make (struct
  type t = Eval.id

  let compare = Stdlib.compare
end)

module IdCacheMap = WrappedMap.Make (struct
  type t = TypeTerm.t * TypeTerm.defer_use_t

  let compare = Stdlib.compare
end)

module EvalReposCacheMap = WrappedMap.Make (struct
  type t = TypeTerm.t * TypeTerm.defer_use_t * Eval.id

  let compare = Stdlib.compare
end)

module FixCacheMap = WrappedMap.Make (struct
  type t = bool * TypeTerm.t

  let compare = Stdlib.compare
end)

module ConstFoldMap = WrappedMap.Make (struct
  type t = reason * int

  let compare = Stdlib.compare
end)

include TypeTerm

(*********************************************************)

let compare = Stdlib.compare

let open_tvar tvar =
  match tvar with
  | OpenT (reason, id) -> (reason, id)
  | _ -> assert false

module type PrimitiveType = sig
  val desc : reason_desc

  val make : reason -> t
end

module Primitive (P : PrimitiveType) = struct
  let desc = P.desc

  let at tok = P.make (mk_annot_reason desc tok)

  let why reason = P.make (replace_desc_reason desc reason)

  let make = P.make

  let why_with_use_desc ~use_desc r =
    let r =
      if use_desc then
        r
      else
        replace_desc_reason P.desc r
    in
    P.make r
end

module NumModuleT = Primitive (struct
  let desc = RNumber

  let make r = DefT (r, NumGeneralT AnyLiteral)
end)

module StrModuleT = Primitive (struct
  let desc = RString

  let make r = DefT (r, StrGeneralT AnyLiteral)
end)

module BoolModuleT = Primitive (struct
  let desc = RBoolean

  let make r = DefT (r, BoolGeneralT)
end)

module BigIntModuleT = Primitive (struct
  let desc = RBigInt

  let make r = DefT (r, BigIntGeneralT AnyLiteral)
end)

module SymbolT = Primitive (struct
  let desc = RSymbol

  let make r = DefT (r, SymbolT)
end)

module MixedT = Primitive (struct
  let desc = RMixed

  let make r = DefT (r, MixedT Mixed_everything)
end)

module EmptyT = Primitive (struct
  let desc = REmpty

  let make r = DefT (r, EmptyT)
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

  let placeholder = why Placeholder

  let locationless source = desc source |> locationless_reason |> make source
end

module Unsoundness = struct
  let constructor = Unsound Constructor

  let merged = Unsound Merged

  let instance_of_refi = Unsound InstanceOfRefinement

  let unresolved = Unsound UnresolvedType

  let resolve_spread = Unsound ResolveSpread

  let unimplemented = Unsound Unimplemented

  let inference_hooks = Unsound InferenceHooks

  let exports = Unsound Exports

  let bound_fn_this = Unsound BoundFunctionThis

  let dummy_static = Unsound DummyStatic

  let merged_any = AnyT.make merged

  let instance_of_refi_any = AnyT.make instance_of_refi

  let unresolved_any = AnyT.make unresolved

  let resolve_spread_any = AnyT.make resolve_spread

  let constructor_any = AnyT.make constructor

  let unimplemented_any = AnyT.make unimplemented

  let inference_hooks_any = AnyT.make inference_hooks

  let exports_any = AnyT.make exports

  let bound_fn_this_any = AnyT.make bound_fn_this

  let dummy_static_any = AnyT.make dummy_static

  let why kind = Unsound kind |> AnyT.why

  let at kind = Unsound kind |> AnyT.at
end

module VoidT = Primitive (struct
  let desc = RVoid

  let make r = DefT (r, VoidT)
end)

module NullT = Primitive (struct
  let desc = RNull

  let make r = DefT (r, NullT)
end)

module ObjProtoT = Primitive (struct
  let desc = RDummyPrototype

  let make r = ObjProtoT r
end)

module NullProtoT = Primitive (struct
  let desc = RNull

  let make r = NullProtoT r
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

  module NumModuleT = LocationLess (NumModuleT)
  module StrModuleT = LocationLess (StrModuleT)
  module BoolModuleT = LocationLess (BoolModuleT)
  module MixedT = LocationLess (MixedT)
  module EmptyT = LocationLess (EmptyT)
  module VoidT = LocationLess (VoidT)
  module NullT = LocationLess (NullT)
end

let hint_unavailable : lazy_hint_t = (false, (fun ~expected_only:_ ?skip_optional:_ _ -> NoHint))

(* convenience *)

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

let string_of_def_ctor = function
  | ArrT _ -> "ArrT"
  | BigIntGeneralT _ -> "BigIntT"
  | BoolGeneralT -> "BoolT"
  | ClassT _ -> "ClassT"
  | EmptyT -> "EmptyT"
  | EnumValueT _ -> "EnumValueT"
  | EnumObjectT _ -> "EnumObjectT"
  | FunT _ -> "FunT"
  | InstanceT _ -> "InstanceT"
  | MixedT _ -> "MixedT"
  | NullT -> "NullT"
  | NumGeneralT _ -> "NumT"
  | ObjT _ -> "ObjT"
  | PolyT _ -> "PolyT"
  | ReactAbstractComponentT _ -> "ReactAbstractComponentT"
  | RendersT _ -> "RendersT"
  | NumericStrKeyT _ -> "NumericStrKeyT"
  | SingletonBoolT _ -> "SingletonBoolT"
  | SingletonNumT _ -> "SingletonNumT"
  | SingletonStrT _ -> "SingletonStrT"
  | SingletonBigIntT _ -> "SingletonBigIntT"
  | StrGeneralT _ -> "StrT"
  | SymbolT -> "SymbolT"
  | TypeT _ -> "TypeT"
  | VoidT -> "VoidT"

let string_of_ctor = function
  | OpenT _ -> "OpenT"
  | AnyT (_, CatchAny) -> "AnyT (catch)"
  | AnyT (_, AnnotatedAny) -> "AnyT (annotated)"
  | AnyT (_, AnyError _) -> "AnyT (error)"
  | AnyT (_, Unsound _) -> "AnyT (unsound)"
  | AnyT (_, Untyped) -> "AnyT (untyped)"
  | AnyT (_, Placeholder) -> "AnyT (placeholder)"
  | AnnotT _ -> "AnnotT"
  | DefT (_, t) -> string_of_def_ctor t
  | EvalT _ -> "EvalT"
  | FunProtoT _ -> "FunProtoT"
  | FunProtoBindT _ -> "FunProtoBindT"
  | GenericT _ -> "GenericT"
  | KeysT _ -> "KeysT"
  | StrUtilT _ -> "StrUtilT"
  | NamespaceT _ -> "NamespaceT"
  | NullProtoT _ -> "NullProtoT"
  | ObjProtoT _ -> "ObjProtoT"
  | OpaqueT _ -> "OpaqueT"
  | ThisInstanceT _ -> "ThisInstanceT"
  | ThisTypeAppT _ -> "ThisTypeAppT"
  | TypeAppT _ -> "TypeAppT"
  | UnionT _ -> "UnionT"
  | IntersectionT _ -> "IntersectionT"
  | OptionalT _ -> "OptionalT"
  | MaybeT _ -> "MaybeT"

let string_of_root_use_op (type a) : a virtual_root_use_op -> string = function
  | InitField _ -> "InitField"
  | ObjectAddComputedProperty _ -> "ObjectAddComputedProperty"
  | ObjectSpread _ -> "ObjectSpread"
  | ObjectRest _ -> "ObjectRest"
  | ObjectChain _ -> "ObjectChain"
  | AssignVar _ -> "AssignVar"
  | Cast _ -> "Cast"
  | ClassExtendsCheck _ -> "ClassExtendsCheck"
  | ClassImplementsCheck _ -> "ClassImplementsCheck"
  | ClassOwnProtoCheck _ -> "ClassOwnProtoCheck"
  | ClassMethodDefinition _ -> "ClassMethodDefinition"
  | Coercion _ -> "Coercion"
  | ConformToCommonInterface _ -> "ConformToCommonInterface"
  | DeclareComponentRef _ -> "DeclareComponentRef"
  | DeleteProperty _ -> "DeleteProperty"
  | DeleteVar _ -> "DeleteVar"
  | FunCall _ -> "FunCall"
  | FunCallMethod _ -> "FunCallMethod"
  | FunImplicitReturn _ -> "FunImplicitReturn"
  | FunReturnStatement _ -> "FunReturnStatement"
  | GeneratorYield _ -> "GeneratorYield"
  | GetProperty _ -> "GetProperty"
  | IndexedTypeAccess _ -> "IndexedTypeAccess"
  | InferBoundCompatibilityCheck _ -> "InferBoundCompatibilityCheck"
  | JSXCreateElement _ -> "JSXCreateElement"
  | ReactCreateElementCall _ -> "ReactCreateElementCall"
  | ReactGetIntrinsic _ -> "ReactGetIntrinsic"
  | Speculation _ -> "Speculation"
  | TypeApplication _ -> "TypeApplication"
  | SetProperty _ -> "SetProperty"
  | UpdateProperty _ -> "UpdateProperty"
  | RefinementCheck _ -> "RefinementCheck"
  | SwitchRefinementCheck _ -> "SwitchRefinementCheck"
  | MatchingProp _ -> "MatchingProp"
  | EvalMappedType _ -> "EvalMappedType"
  | TypeGuardIncompatibility _ -> "TypeGuardIncompatibility"
  | RenderTypeInstantiation _ -> "RenderTypeInstantiation"
  | ComponentRestParamCompatibility _ -> "ComponentRestParamCompatibility"
  | PositiveTypeGuardConsistency _ -> "PositiveTypeGuardConsistency"
  | UnknownUse -> "UnknownUse"

let string_of_frame_use_op (type a) : a virtual_frame_use_op -> string = function
  | ConstrainedAssignment _ -> "ConstrainedAssignment"
  | ReactDeepReadOnly _ -> "ReactDeepReadOnly"
  | ArrayElementCompatibility _ -> "ArrayElementCompatibility"
  | FunCompatibility _ -> "FunCompatibility"
  | FunMissingArg _ -> "FunMissingArg"
  | FunParam _ -> "FunParam"
  | FunRestParam _ -> "FunRestParam"
  | FunReturn _ -> "FunReturn"
  | ImplicitTypeParam -> "ImplicitTypeParam"
  | IndexerKeyCompatibility _ -> "IndexerKeyCompatibility"
  | OpaqueTypeLowerBoundCompatibility _ -> "OpaqueTypeLowerBoundCompatibility"
  | OpaqueTypeUpperBoundCompatibility _ -> "OpaqueTypeUpperBoundCompatibility"
  | MappedTypeKeyCompatibility _ -> "MappedTypeKeyCompatibility"
  | PropertyCompatibility _ -> "PropertyCompatibility"
  | ReactConfigCheck -> "ReactConfigCheck"
  | ReactGetConfig _ -> "ReactGetConfig"
  | TupleElementCompatibility _ -> "TupleElementCompatibility"
  | TupleAssignment _ -> "TupleAssignment"
  | TypeArgCompatibility _ -> "TypeArgCompatibility"
  | TypeParamBound _ -> "TypeParamBound"
  | OpaqueTypeLowerBound _ -> "OpaqueTypeLowerBound"
  | OpaqueTypeUpperBound _ -> "OpaqueTypeUpperBound"
  | UnifyFlip -> "UnifyFlip"
  | TypeGuardCompatibility -> "TypeGuardCompatibility"
  | RendersCompatibility -> "RendersCompatibility"
  | EnumRepresentationTypeCompatibility _ -> "EnumRepresentationTypeCompatibility"

let string_of_use_op (type a) : a virtual_use_op -> string = function
  | Op root -> string_of_root_use_op root
  | Frame (frame, _) -> string_of_frame_use_op frame

let string_of_use_op_rec : use_op -> string =
  fold_use_op string_of_root_use_op (fun acc use_op ->
      spf "%s(%s)" (string_of_frame_use_op use_op) acc
  )

let string_of_predicate_concretizer_variant = function
  | ConcretizeForGeneralPredicateTest -> "ConcretizeForGeneralPredicateTest"
  | ConcretizeKeepOptimizedUnions -> "ConcretizeKeepOptimizedUnions"
  | ConcretizeRHSForInstanceOfPredicateTest -> "ConcretizeRHSForInstanceOfPredicateTest"
  | ConcretizeRHSForLiteralPredicateTest -> "ConcretizeRHSForLiteralPredicateTest"

let string_of_use_ctor = function
  | UseT (op, t) -> spf "UseT(%s, %s)" (string_of_use_op op) (string_of_ctor t)
  | ArrRestT _ -> "ArrRestT"
  | BindT _ -> "BindT"
  | CallElemT _ -> "CallElemT"
  | CallT _ -> "CallT"
  | ConstructorT _ -> "ConstructorT"
  | ElemT _ -> "ElemT"
  | EnumCastT _ -> "EnumCastT"
  | EnumExhaustiveCheckT _ -> "EnumExhaustiveCheckT"
  | GetEnumT _ -> "GetEnumT"
  | ConditionalT _ -> "ConditionalT"
  | ExtendsUseT _ -> "ExtendsUseT"
  | GetElemT _ -> "GetElemT"
  | GetKeysT _ -> "GetKeysT"
  | GetValuesT _ -> "GetValuesT"
  | GetDictValuesT _ -> "GetDictValuesT"
  | GetTypeFromNamespaceT _ -> "GetTypeFromNamespaceT"
  | GetPropT _ -> "GetPropT"
  | GetPrivatePropT _ -> "GetPrivatePropT"
  | GetProtoT _ -> "GetProtoT"
  | GetStaticsT _ -> "GetStaticsT"
  | HasOwnPropT _ -> "HasOwnPropT"
  | ImplementsT _ -> "ImplementsT"
  | ConcretizeT { kind; _ } ->
    spf
      "ConcretizeT %s"
      (match kind with
      | ConcretizeForImportsExports -> "ConcretizeForImportsExports"
      | ConcretizeForCJSExtractNamedExportsAndTypeExports ->
        "ConcretizeForCJSExtractNamedExportsAndTypeExports"
      | ConcretizeForInspection -> "ConcretizeForInspection"
      | ConcretizeForPredicate v ->
        "ConcretizeForPredicate(" ^ string_of_predicate_concretizer_variant v ^ ")"
      | ConcretizeForSentinelPropTest -> "ConcretizeForPredicate"
      | ConcretizeAll -> "ConcretizeAll"
      | ConcretizeForOperatorsChecking -> "ConcretizeForOperatorsChecking"
      | ConcretizeForObjectAssign -> "ConcretizeForObjectAssign"
      | ConcretizeForMatchArg { keep_unions } ->
        spf "ConcretizeForMatchArg {keep_unions=%b}" keep_unions)
  | LookupT _ -> "LookupT"
  | MapTypeT _ -> "MapTypeT"
  | MethodT _ -> "MethodT"
  | PrivateMethodT _ -> "PrivateMethodT"
  | MixinT _ -> "MixinT"
  | ObjRestT _ -> "ObjRestT"
  | ObjTestProtoT _ -> "ObjTestProtoT"
  | ObjTestT _ -> "ObjTestT"
  | OptionalChainT _ -> "OptionalChainT"
  | ReactKitT _ -> "ReactKitT"
  | ReposLowerT _ -> "ReposLowerT"
  | ReposUseT _ -> "ReposUseT"
  | ResolveSpreadT (_, _, { rrt_resolve_to; _ }) ->
    spf
      "ResolveSpreadT(%s)"
      begin
        match rrt_resolve_to with
        | ResolveSpreadsToArray _ -> "ResolveSpreadsToArray"
        | ResolveSpreadsToTupleType { id; _ } -> spf "ResolveSpreadsToTupleType (%d)" id
        | ResolveSpreadsToArrayLiteral { id; _ } -> spf "ResolveSpreadsToArrayLiteral (%d)" id
        | ResolveSpreadsToMultiflowCallFull _ -> "ResolveSpreadsToMultiflowCallFull"
        | ResolveSpreadsToMultiflowSubtypeFull _ -> "ResolveSpreadsToMultiflowSubtypeFull"
        | ResolveSpreadsToMultiflowPartial _ -> "ResolveSpreadsToMultiflowPartial"
      end
  | SetElemT _ -> "SetElemT"
  | SetPropT _ -> "SetPropT"
  | SetPrivatePropT _ -> "SetPrivatePropT"
  | SetProtoT _ -> "SetProtoT"
  | SpecializeT _ -> "SpecializeT"
  | ObjKitT _ -> "ObjKitT"
  | SuperT _ -> "SuperT"
  | TestPropT _ -> "TestPropT"
  | ThisSpecializeT _ -> "ThisSpecializeT"
  | ToStringT _ -> "ToStringT"
  | ValueToTypeReferenceT _ -> "ValueToTypeReferenceT"
  | TypeCastT _ -> "TypeCastT"
  | ConcretizeTypeAppsT _ -> "ConcretizeTypeAppsT"
  | CondT _ -> "CondT"
  | DestructuringT _ -> "DestructuringT"
  | ResolveUnionT _ -> "ResolveUnionT"
  | FilterOptionalT _ -> "FilterOptionalT"
  | ExtractReactRefT _ -> "ExtractReactRefT"
  | FilterMaybeT _ -> "FilterMaybeT"
  | DeepReadOnlyT _ -> "DeepReadOnlyT"
  | HooklikeT _ -> "HooklikeT"
  | SealGenericT _ -> "SealGenericT"
  | OptionalIndexedAccessT _ -> "OptionalIndexedAccessT"
  | CheckUnusedPromiseT _ -> "CheckUnusedPromiseT"
  | WriteComputedObjPropCheckT _ -> "WriteComputedObjPropCheckT"
  | CheckReactImmutableT _ -> "CheckReactImmutableT"
  | ConvertEmptyPropsToMixedT _ -> "ConvertEmptyPropsToMixedT"
  | ExitRendersT _ -> "ExitRendersT"
  | EvalTypeDestructorT _ -> "EvalTypeDestructorT"

let string_of_binary_test = function
  | InstanceofTest -> "instanceof"
  | SentinelProp key -> "sentinel prop " ^ key
  | EqTest -> "==="

let rec string_of_predicate = function
  | AndP (p1, p2) -> string_of_predicate p1 ^ " && " ^ string_of_predicate p2
  | OrP (p1, p2) -> string_of_predicate p1 ^ " || " ^ string_of_predicate p2
  | NotP p -> "not " ^ string_of_predicate p
  | BinaryP (b, OpenT (_, id)) ->
    spf "left operand of %s with right operand = OpenT(%d)" (string_of_binary_test b) id
  | BinaryP (b, t) ->
    spf "left operand of %s with right operand = %s" (string_of_binary_test b) (string_of_ctor t)
  | TruthyP -> "truthy"
  | NullP -> "null"
  | MaybeP -> "null or undefined"
  | SingletonBoolP (_, false) -> "false"
  | SingletonBoolP (_, true) -> "true"
  | SingletonStrP (_, _, str) -> spf "string `%s`" str
  | SingletonNumP (_, _, (_, raw)) -> spf "number `%s`" raw
  | SingletonBigIntP (_, _, (_, raw)) -> spf "bigint `%s`" raw
  (* typeof *)
  | VoidP -> "undefined"
  | BoolP _ -> "boolean"
  | StrP _ -> "string"
  | NumP _ -> "number"
  | BigIntP _ -> "bigint"
  | FunP -> "function"
  | ObjP -> "object"
  | SymbolP _ -> "symbol"
  (* Array.isArray *)
  | ArrP -> "array"
  | ArrLenP { op; n } ->
    let op =
      match op with
      | ArrLenEqual -> "==="
      | ArrLenGreaterThanEqual -> ">="
    in
    spf "array length %s %i" op n
  | PropExistsP { propname; _ } -> spf "prop `%s` exists" propname
  | PropTruthyP (key, _) -> spf "prop `%s` is truthy" key
  | PropIsExactlyNullP (key, _) -> spf "prop `%s` is exactly null" key
  | PropNonVoidP (key, _) -> spf "prop `%s` is not undefined" key
  | PropNonMaybeP (key, _) -> spf "prop `%s` is not null or undefined" key
  | LatentP ((lazy (_, _, OpenT (_, id), _, _)), is) ->
    spf "LatentPred(TYPE_%d, %s)" id (List.map string_of_int is |> String.concat ", ")
  | LatentP ((lazy (_, _, t, _, _)), is) ->
    spf "LatentPred(%s, %s)" (string_of_ctor t) (List.map string_of_int is |> String.concat ", ")
  | LatentThisP (lazy (_, _, OpenT (_, id), _, _)) -> spf "LatentThisPred(TYPE_%d)" id
  | LatentThisP (lazy (_, _, t, _, _)) -> spf "LatentThisPred(%s)" (string_of_ctor t)
  | ImpossibleP -> "impossible"

let string_of_type_t_kind = function
  | TypeAliasKind -> "TypeAliasKind"
  | TypeParamKind -> "TypeParamKind"
  | OpaqueKind -> "OpaqueKind"
  | ImportTypeofKind -> "ImportTypeofKind"
  | ImportClassKind -> "ImportClassKind"
  | ImportEnumKind -> "ImportEnumKind"
  | InstanceKind -> "InstanceKind"
  | RenderTypeKind -> "RenderTypeKind"

(** A setter's type is determined by its sole parameter.

    If it has more than one param, returns the type of the first param;
    if it has no params, returns `any`. If it isn't even a function,
    raises an exception because this is a Flow bug. *)
let extract_setter_type = function
  | DefT (_, FunT (_, { params = (_, param_t) :: _; _ })) -> param_t
  | DefT (reason, FunT _) -> AnyT.error reason
  | _ -> failwith "Setter property with unexpected type"

let extract_getter_type = function
  | DefT (_, FunT (_, { return_t; _ })) -> return_t
  | _ -> failwith "Getter property with unexpected type"

let elemt_of_arrtype = function
  | ArrayAT { elem_t; _ }
  | ROArrayAT (elem_t, _)
  | TupleAT { elem_t; _ } ->
    elem_t

let ro_of_arrtype = function
  | ArrayAT _ -> Generic.ArraySpread.NonROSpread
  | _ -> Generic.ArraySpread.ROSpread

let annot ~in_implicit_instantiation use_desc = function
  | OpenT (r, _) as t when not (is_instantiable_reason r && in_implicit_instantiation) ->
    AnnotT (r, t, use_desc)
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

let dummy_this loc = mk_reason RDummyThis loc |> MixedT.make

let implicit_mixed_this r = update_desc_reason (fun desc -> RImplicitThis desc) r |> MixedT.make

let global_this reason =
  let reason = replace_desc_reason (RCustom "global object") reason in
  ObjProtoT reason

let default_obj_assign_kind = ObjAssign { assert_exact = false }

(* A method type is a function type with `this` specified. *)
let mk_methodtype
    this_t
    ?(subtyping = This_Function)
    ?(effect_ = ArbitraryEffect)
    tins
    ~rest_param
    ~def_reason
    ?params_names
    ~type_guard
    tout =
  {
    this_t = (this_t, subtyping);
    params =
      (match params_names with
      | None -> Base.List.map ~f:(fun t -> (None, t)) tins
      | Some xs -> List.map2 (fun x t -> (x, t)) xs tins);
    rest_param;
    return_t = tout;
    type_guard;
    def_reason;
    effect_;
  }

let mk_methodcalltype targs args ?meth_generic_this ?(meth_strict_arity = true) tout =
  {
    meth_generic_this;
    meth_targs = targs;
    meth_args_tlist = args;
    meth_tout = tout;
    meth_strict_arity;
  }

(** A bound function type is a method type whose `this` parameter has been
  bound to some type. Currently, if the function's `this` parameter is not
  explicitly annotated we model this unsoundly using `any`, but if it is
  then we create a methodtype with a specific `this` type. *)
let mk_boundfunctiontype ~this = mk_methodtype ~subtyping:(This_Method { unbound = false }) this

(** A function type is a method type whose `this` parameter has been
  bound to to the global object. Currently, if the function's `this` parameter is not
  explicitly annotated we model this using `mixed`, but if it is
  then we create a methodtype with a specific `this` type. *)
let mk_functiontype reason ?(this = global_this reason) = mk_methodtype this

let mk_boundfunctioncalltype this targs args ?(call_strict_arity = true) tout =
  {
    call_this_t = this;
    call_targs = targs;
    call_args_tlist = args;
    call_tout = tout;
    call_strict_arity;
    call_speculation_hint_state = None;
    call_specialized_callee = None;
  }

let mk_functioncalltype reason = mk_boundfunctioncalltype (global_this reason)

let mk_opt_functioncalltype reason targs args strict instantiation_probe =
  (global_this reason, targs, args, strict, instantiation_probe)

let mk_opt_boundfunctioncalltype this targs args strict = (this, targs, args, strict)

let mk_opt_methodcalltype
    ?opt_meth_generic_this opt_meth_targs opt_meth_args_tlist opt_meth_strict_arity =
  { opt_meth_generic_this; opt_meth_targs; opt_meth_args_tlist; opt_meth_strict_arity }

let default_flags = { obj_kind = Exact; react_dro = None }

let mk_objecttype ?(flags = default_flags) ?(reachable_targs = []) ~call pmap proto =
  { flags; proto_t = proto; props_tmap = pmap; call_t = call; reachable_targs }

let mk_object_def_type ~reason ?(flags = default_flags) ~call pmap proto =
  let reason = update_desc_reason invalidate_rtype_alias reason in
  DefT (reason, ObjT (mk_objecttype ~flags ~call pmap proto))

let apply_opt_funcalltype (this, targs, args, strict, t_callee) t_out =
  Funcalltype
    {
      call_this_t = this;
      call_targs = targs;
      call_args_tlist = args;
      call_tout = t_out;
      call_strict_arity = strict;
      call_speculation_hint_state = None;
      call_specialized_callee = t_callee;
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
  | OptCallM { opt_methodcalltype; return_hint; specialized_callee } ->
    CallM
      {
        methodcalltype = apply_opt_methodcalltype opt_methodcalltype t_out;
        return_hint;
        specialized_callee;
      }
  | OptChainM
      { exp_reason; lhs_reason; opt_methodcalltype; voided_out; return_hint; specialized_callee } ->
    ChainM
      {
        exp_reason;
        lhs_reason;
        methodcalltype = apply_opt_methodcalltype opt_methodcalltype t_out;
        voided_out;
        return_hint;
        specialized_callee;
      }
  | OptNoMethodAction t -> NoMethodAction t

let apply_opt_use opt_use t_out =
  match opt_use with
  | OptMethodT (op, r1, r2, ref, action) -> MethodT (op, r1, r2, ref, apply_opt_action action t_out)
  | OptPrivateMethodT (op, r1, r2, p, scopes, static, action) ->
    PrivateMethodT (op, r1, r2, p, scopes, static, apply_opt_action action t_out)
  | OptCallT { use_op; reason; opt_funcalltype = f; return_hint } ->
    CallT { use_op; reason; call_action = apply_opt_funcalltype f t_out; return_hint }
  | OptGetPropT { use_op; reason; id; propref; hint } ->
    GetPropT
      { use_op; reason; id; from_annot = false; skip_optional = false; propref; tout = t_out; hint }
  | OptGetPrivatePropT (u, r, s, cbs, b) -> GetPrivatePropT (u, r, s, cbs, b, t_out)
  | OptTestPropT (use_op, reason, id, propref, hint) ->
    TestPropT { use_op; reason; id; propref; tout = t_out; hint }
  | OptGetElemT (use_op, reason, id, from_annot, key_t) ->
    GetElemT
      {
        use_op;
        reason;
        id;
        from_annot;
        skip_optional = false;
        access_iterables = false;
        key_t;
        tout = t_out;
      }
  | OptCallElemT (u, r1, r2, elt, call) -> CallElemT (u, r1, r2, elt, apply_opt_action call t_out)

let mk_enum_type reason enum_info =
  let reason =
    update_desc_reason
      (fun desc ->
        match desc with
        | REnum { name = Some name } -> RType (OrdinaryName name)
        | _ -> desc)
      reason
  in
  DefT (reason, EnumValueT enum_info)

let mk_enum_object_type reason enum_info =
  let enum_value_t = mk_enum_type reason enum_info in
  DefT (reason, EnumObjectT { enum_value_t; enum_info })

let call_of_method_app
    call_this_t
    call_specialized_callee
    { meth_generic_this; meth_targs; meth_args_tlist; meth_tout; meth_strict_arity } =
  {
    call_this_t = Base.Option.value ~default:call_this_t meth_generic_this;
    call_targs = meth_targs;
    call_args_tlist = meth_args_tlist;
    call_tout = meth_tout;
    call_strict_arity = meth_strict_arity;
    call_speculation_hint_state = None;
    call_specialized_callee;
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

let empty_tuple_view = TupleView { elements = []; arity = (0, 0); inexact = false }
