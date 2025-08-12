(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module defines types representing signatures extracted from Flow source
 * files. These signatures are used to increase the parallelism of type
 * checking, similar to header/interface files in other languages.
 *
 * Note that these definitions are not recursive, but rather parametric over
 * some type 'a. In practice these types are parameterized to create a fixed
 * point.
 *
 * This parameterization is useful to re-use the same structure through separate
 * phases of the analysis. For example, the parsing phase parameterizes these
 * types including the possibility of a name reference at every step. The
 * resolve phase then replaces these by-name references with the referent.
 *
 * As a matter of style, this module makes extensive use of inline records to
 * avoid record label conflicts.
 *)

type 'a smap = 'a SMap.t [@@deriving map, show { with_path = false }]

let iter_smap f = SMap.iter (fun _ x -> f x)

(* The PPX generated map functions will use the non-tail-recursive List.map
 * implementation. Use this type instead if the list might be very long. For
 * example, codegen can produce unions that are very large. *)
type 'a tailrec_list = 'a list [@@deriving iter, show { with_path = false }]

let map_tailrec_list f xs = Base.List.map ~f xs

type ('loc, 'a) targ =
  | ImplicitArg of 'loc
  | ExplicitArg of 'a
[@@deriving iter, map, show { with_path = false }]

type 'a arg =
  | Arg of 'a
  | SpreadArg of 'a
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) type_guard =
  | TypeGuard of {
      loc: 'loc;
      param_name: 'loc * string;
      type_guard: 'a;
      one_sided: bool;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) tparam =
  | TParam of {
      name_loc: 'loc;
      name: string;
      polarity: Polarity.t;
      bound: 'a option;
      default: 'a option;
      is_const: bool;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) tparams =
  | Mono
  | Poly of 'loc * ('loc, 'a) tparam * ('loc, 'a) tparam list
[@@deriving iter, map, show { with_path = false }]

type 'a fun_param =
  | FunParam of {
      name: string option;
      t: 'a;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) fun_rest_param =
  | FunRestParam of {
      name: string option;
      loc: 'loc;
      t: 'a;
    }
[@@deriving iter, map, show { with_path = false }]

type 'loc react_effect =
  | HookDecl of 'loc
  | HookAnnot
  | ArbitraryEffect
  | AnyEffect
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) fun_sig =
  | FunSig of {
      tparams: ('loc, 'a) tparams;
      params: 'a fun_param list;
      rest_param: ('loc, 'a) fun_rest_param option;
      this_param: 'a option;
      return: 'a;
      type_guard: ('loc, 'a) type_guard option;
      effect_: 'loc react_effect;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) component_param =
  | ComponentParam of {
      name: string;
      name_loc: 'loc;
      t: 'a;
    }
[@@deriving iter, map, show { with_path = false }]

type 'a component_rest_param = ComponentRestParam of { t: 'a }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) component_sig =
  | ComponentSig of {
      params_loc: 'loc;
      tparams: ('loc, 'a) tparams;
      params: ('loc, 'a) component_param list;
      rest_param: 'a component_rest_param option;
      renders: 'a;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) tuple_element =
  | TupleElement of {
      loc: 'loc;
      name: string option;
      t: 'a;
      polarity: Polarity.t;
      optional: bool;
    }
  | TupleSpread of {
      loc: 'loc;
      name: string option;
      t: 'a;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) obj_annot_proto =
  | ObjAnnotImplicitProto
  | ObjAnnotExplicitProto of 'loc * 'a
  | ObjAnnotCallable of { ts_rev: 'a Nel.t } (* implies function proto *)
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) accessor =
  | Get of 'loc * 'a
  | Set of 'loc * 'a
  | GetSet of 'loc * 'a * 'loc * 'a
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) obj_value_prop =
  | ObjValueField of 'loc * 'a * Polarity.t
  | ObjValueAccess of ('loc, 'a) accessor
  | ObjValueMethod of {
      id_loc: 'loc;
      fn_loc: 'loc;
      async: bool;
      generator: bool;
      def: ('loc, 'a) fun_sig;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) obj_annot_prop =
  | ObjAnnotField of 'loc * 'a * Polarity.t
  | ObjAnnotAccess of ('loc, 'a) accessor
  | ObjAnnotMethod of {
      id_loc: 'loc;
      fn_loc: 'loc;
      def: ('loc, 'a) fun_sig;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) interface_prop =
  | InterfaceField of 'loc option * 'a * Polarity.t
  | InterfaceAccess of ('loc, 'a) accessor
  | InterfaceMethod of ('loc * 'loc * ('loc, 'a) fun_sig) Nel.t
[@@deriving iter, map, show { with_path = false }]

type 'a obj_annot_dict =
  | ObjDict of {
      name: string option;
      polarity: Polarity.t;
      key: 'a;
      value: 'a;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) obj_spread_annot_elem =
  | ObjSpreadAnnotElem of 'a
  | ObjSpreadAnnotSlice of {
      dict: 'a obj_annot_dict option;
      props: ('loc, 'a) obj_annot_prop smap;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) obj_value_spread_elem =
  | ObjValueSpreadElem of 'a
  | ObjValueSpreadSlice of ('loc, 'a) obj_value_prop smap
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) class_extends =
  | ClassExplicitExtends of {
      loc: 'loc;
      t: 'a;
    }
  | ClassExplicitExtendsApp of {
      loc: 'loc;
      t: 'a;
      targs: 'a list;
    }
  | ClassImplicitExtends
  | ObjectPrototypeExtendsNull
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) class_mixins =
  | ClassMixin of {
      loc: 'loc;
      t: 'a;
    }
  | ClassMixinApp of {
      loc: 'loc;
      t: 'a;
      targs: 'a list;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) class_sig =
  | ClassSig of {
      tparams: ('loc, 'a) tparams;
      extends: ('loc, 'a) class_extends;
      implements: 'a list;
      static_props: ('loc, 'a) obj_value_prop smap;
      proto_props: ('loc, 'a) obj_value_prop smap;
      own_props: ('loc, 'a) obj_value_prop smap;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) declare_class_sig =
  | DeclareClassSig of {
      tparams: ('loc, 'a) tparams;
      extends: ('loc, 'a) class_extends;
      mixins: ('loc, 'a) class_mixins list;
      implements: 'a list;
      static_props: ('loc, 'a) interface_prop smap;
      own_props: ('loc, 'a) interface_prop smap;
      proto_props: ('loc, 'a) interface_prop smap;
      static_calls: 'a list;
      calls: 'a list;
      dict: 'a obj_annot_dict option;
      static_dict: 'a obj_annot_dict option;
    }
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) interface_sig =
  | InterfaceSig of {
      extends: 'a list;
      props: ('loc, 'a) interface_prop smap;
      calls: 'a list;
      dict: 'a obj_annot_dict option;
    }
[@@deriving iter, map, show { with_path = false }]

type enum_rep =
  | BoolRep of bool option
  | NumberRep of { truthy: bool }
  | StringRep of { truthy: bool }
  | SymbolRep
  | BigIntRep of { truthy: bool }
[@@deriving iter, map, show { with_path = false }]

(* Definitions represent the structure of things which can be found when
 * resolving a name, or as in the case of exports, a module reference.
 *
 * The signature format is designed to exploit these forms of indirection to
 * preserve sharing and minimize size. Two references to a given definition will
 * always be represented as pointers to a shared signature of that definition.
 *)
type ('loc, 'a) def =
  | TypeAlias of {
      id_loc: 'loc;
      name: string;
      tparams: ('loc, 'a) tparams;
      body: 'a;
    }
  | OpaqueType of {
      id_loc: 'loc;
      name: string;
      tparams: ('loc, 'a) tparams;
      lower_bound: 'a option;
      upper_bound: 'a option;
      body: 'a option;
    }
  | Interface of {
      id_loc: 'loc;
      name: string;
      tparams: ('loc, 'a) tparams;
      def: ('loc, 'a) interface_sig;
    }
  | ClassBinding of {
      id_loc: 'loc;
      name: string;
      def: ('loc, 'a) class_sig;
    }
  | DeclareClassBinding of {
      id_loc: 'loc;
      nominal_id_loc: 'loc;
      name: string;
      def: ('loc, 'a) declare_class_sig;
    }
  | FunBinding of {
      id_loc: 'loc;
      name: string;
      async: bool;
      generator: bool;
      fn_loc: 'loc;
      def: ('loc, 'a) fun_sig;
      statics: ('loc * 'a) smap;
    }
  | DeclareFun of {
      id_loc: 'loc;
      name: string;
      fn_loc: 'loc;
      def: ('loc, 'a) fun_sig;
      tail: ('loc * 'loc * ('loc, 'a) fun_sig) list;
    }
  | ComponentBinding of {
      id_loc: 'loc;
      name: string;
      fn_loc: 'loc;
      def: ('loc, 'a) component_sig;
    }
  | DisabledComponentBinding of {
      id_loc: 'loc;
      name: string;
    }
  | Variable of {
      id_loc: 'loc;
      name: string;
      def: 'a;
    }
  | EnumBinding of {
      id_loc: 'loc;
      name: string;
      rep: enum_rep;
      members: 'loc smap;
      has_unknown_members: bool;
    }
  | DisabledEnumBinding of {
      id_loc: 'loc;
      name: string;
    }
  | NamespaceBinding of {
      id_loc: 'loc;
      name: string;
      values: ('loc * 'a) smap;
      types: ('loc * 'a) smap;
    }
[@@deriving iter, map, show { with_path = false }]

(* These accessors will compile to code that does not have a branch because
 * id_loc and name have the same offset for each constructor. *)
let def_id_loc = function
  | TypeAlias { id_loc; _ }
  | OpaqueType { id_loc; _ }
  | Interface { id_loc; _ }
  | ClassBinding { id_loc; _ }
  | DeclareClassBinding { id_loc; _ }
  | FunBinding { id_loc; _ }
  | DeclareFun { id_loc; _ }
  | ComponentBinding { id_loc; _ }
  | DisabledComponentBinding { id_loc; _ }
  | Variable { id_loc; _ } ->
    id_loc
  | EnumBinding { id_loc; _ } -> id_loc
  | DisabledEnumBinding { id_loc; _ } -> id_loc
  | NamespaceBinding { id_loc; _ } -> id_loc

let def_name = function
  | TypeAlias { name; _ }
  | OpaqueType { name; _ }
  | Interface { name; _ }
  | ClassBinding { name; _ }
  | DeclareClassBinding { name; _ }
  | FunBinding { name; _ }
  | DeclareFun { name; _ }
  | ComponentBinding { name; _ }
  | DisabledComponentBinding { name; _ }
  | Variable { name; _ } ->
    name
  | EnumBinding { name; _ } -> name
  | DisabledEnumBinding { name; _ } -> name
  | NamespaceBinding { name; _ } -> name

(* The signature extractor relies heavily on annotations, but will extract
 * signatures corresponding to some literal expressions as well. The
 * representation of these things are kept distinct from annotations, below.
 *)
type ('loc, 'a) value =
  | ClassExpr of 'loc * ('loc, 'a) class_sig
  | FunExpr of {
      loc: 'loc;
      async: bool;
      generator: bool;
      def: ('loc, 'a) fun_sig;
      statics: ('loc * 'a) smap;
    }
  | StringVal of 'loc
  | StringLit of 'loc * string
  | LongStringLit of 'loc
  | NumberVal of 'loc
  | NumberLit of 'loc * float * string
  | BigIntVal of 'loc
  | BigIntLit of 'loc * int64 option * string
  | BooleanVal of 'loc
  | BooleanLit of 'loc * bool
  | NullLit of 'loc
  | DeclareModuleImplicitlyExportedObject of {
      loc: 'loc;
      module_name: Flow_import_specifier.userland;
      props: ('loc, 'a) obj_value_prop smap;
    }
  | ObjLit of {
      loc: 'loc;
      frozen: bool;
      proto: ('loc * 'a) option;
      props: ('loc, 'a) obj_value_prop smap;
    }
  | ObjSpreadLit of {
      loc: 'loc;
      frozen: bool;
      proto: ('loc * 'a) option;
      elems_rev: ('loc, 'a) obj_value_spread_elem Nel.t;
    }
  | EmptyConstArrayLit of 'loc
  | ArrayLit of 'loc * 'a * 'a tailrec_list
  | AsConst of ('loc, 'a) value
[@@deriving iter, map, show { with_path = false }]

type 'a obj_kind =
  | ExactObj
  | InexactObj
  | IndexedObj of 'a obj_annot_dict
[@@deriving iter, map, show { with_path = false }]

type ('loc, 'a) annot =
  | Any of 'loc
  | Mixed of 'loc
  | Empty of 'loc
  | Void of 'loc
  | Null of 'loc
  | Symbol of 'loc
  | Number of 'loc
  | BigInt of 'loc
  | String of 'loc
  | Boolean of 'loc
  | Exists of 'loc
  | Optional of 'a
  | Maybe of 'loc * 'a
  | Union of {
      loc: 'loc;
      t0: 'a;
      t1: 'a;
      ts: 'a tailrec_list;
    }
  | Intersection of {
      loc: 'loc;
      t0: 'a;
      t1: 'a;
      ts: 'a tailrec_list;
    }
  | Tuple of {
      loc: 'loc;
      elems_rev: ('loc, 'a) tuple_element tailrec_list;
      inexact: bool;
    }
  | Array of 'loc * 'a
  | ReadOnlyArray of 'loc * 'a
  | SingletonString of 'loc * string
  | SingletonNumber of 'loc * float * string
  | SingletonBigInt of 'loc * int64 option * string
  | SingletonBoolean of 'loc * bool
  | StringPrefix of {
      loc: 'loc;
      prefix: string;
      remainder: 'a option;
    }
  | StringSuffix of {
      loc: 'loc;
      suffix: string;
      remainder: 'a option;
    }
  | Typeof of {
      loc: 'loc;
      qname: string list;
      t: 'a;
      targs: 'a list option;
    }
  | Bound of {
      ref_loc: 'loc;
      name: string;
    }
  | NoInfer of 'a
  | PropertyType of {
      loc: 'loc;
      obj: 'a;
      prop: string;
    }
  | ElementType of {
      loc: 'loc;
      obj: 'a;
      elem: 'a;
    }
  | EnumValue of 'loc * 'a
  | Enum of 'loc * 'a
  | OptionalIndexedAccessNonMaybeType of {
      loc: 'loc;
      obj: 'a;
      index: 'a;
    }
  | OptionalIndexedAccessResultType of {
      loc: 'loc;
      non_maybe_result: 'a;
      void_loc: 'loc;
    }
  | NonMaybeType of 'loc * 'a
  | Omit of 'loc * 'a * 'a
  | ReadOnly of 'loc * 'a
  | Partial of 'loc * 'a
  | Required of 'loc * 'a
  | Keys of 'loc * 'a
  | Renders of {
      loc: 'loc;
      arg: 'a;
      variant: Flow_ast.Type.Renders.variant;
    }
  | ComponentMissingRenders of 'loc
  | Values of 'loc * 'a
  | Exact of 'loc * 'a
  | ExportsT of 'loc * Flow_import_specifier.userland
  | Conditional of {
      loc: 'loc;
      distributive_tparam: ('loc, 'a) tparam option;
      infer_tparams: ('loc, 'a) tparams;
      check_type: 'a;
      extends_type: 'a;
      true_type: 'a;
      false_type: 'a;
    }
  | ObjKeyMirror of {
      loc: 'loc;
      obj: 'a;
    }
  | ClassT of 'loc * 'a
  | Function_bind of 'loc
  | ReactElementProps of 'loc * 'a
  | ReactElementConfig of 'loc * 'a
  | ReactImmutable of 'loc * 'a
  | FunAnnot of 'loc * ('loc, 'a) fun_sig
  | ComponentAnnot of 'loc * ('loc, 'a) component_sig
  | MappedTypeAnnot of {
      loc: 'loc;
      source_type: 'a;
      property_type: 'a;
      key_tparam: ('loc, 'a) tparam;
      variance: Polarity.t;
      optional: Flow_ast.Type.Object.MappedType.optional_flag;
      inline_keyof: bool;
    }
  | ObjAnnot of {
      loc: 'loc;
      obj_kind: 'a obj_kind;
      props: ('loc, 'a) obj_annot_prop smap;
      proto: ('loc, 'a) obj_annot_proto;
    }
  | ObjSpreadAnnot of {
      loc: 'loc;
      exact: bool;
      elems_rev: ('loc, 'a) obj_spread_annot_elem Nel.t;
    }
  | InlineInterface of 'loc * ('loc, 'a) interface_sig
[@@deriving iter, map, show { with_path = false }]

(* Along with literal expressions, the signature extractor also encodes some
 * operations over values and annotations, like unary operators and
 * destructuring. *)
type 'a op =
  | Arith of Flow_ast.Expression.Binary.operator * 'a
  | GetProp of string
  | GetElem of 'a
  | Unary of Flow_ast.Expression.Unary.operator
  | Update
[@@deriving iter, map, show { with_path = false }]

(* Parsing out a signature can fail. There are three interesting failure modes,
 * which are all conflated in this type.
 *
 * 1. If there are insufficient annotations to extract a signature, we should
 *    should inform the user and proceed as if the unknown type was any.
 * 2. If some unsupported or unexpected syntax was detected, we should proceed
 *    as if the type was any, but not inform the user. The checking phase will
 *    uncover the same error.
 * 3. If there is some value syntax, which is just not supported by the
 *    signature extractor, we should fix the signature extractor to support that
 *    syntax. These represent missing functionality in the analysis which should
 *    be fixed.
 *)
type 'loc errno =
  | CheckError
  | BindingValidationError of 'loc Signature_error.binding_validation_t
  | SigError of 'loc Signature_error.t
[@@deriving show { with_path = false }, map]

type 'loc error = 'loc errno [@@deriving show { with_path = false }]
