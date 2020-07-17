(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

(* TODO: Run ocamlformat and fix all the detached comments. *)
[@@@ocamlformat "disable=true"]

type 'a smap = 'a SMap.t
[@@deriving map, show {with_path = false}]

let iter_smap f = SMap.iter (fun _ x -> f x)

type ('key, 'loc, 'a) predicate =
  | AndP of ('key, 'loc, 'a) predicate * ('key, 'loc, 'a) predicate
  | OrP of ('key, 'loc, 'a) predicate * ('key, 'loc, 'a) predicate
  | NotP of ('key, 'loc, 'a) predicate
  | ExistsP of 'key * 'loc option
  | InstanceofP of 'key * 'a
  | ArrP of 'key
  | NullP of 'key
  | MaybeP of 'key
  | SingletonStrP of 'key * 'loc * bool * string
  | SingletonBoolP of 'key * 'loc * bool
  | SingletonNumP of 'key * 'loc * bool * float * string
  | BoolP of 'key * 'loc
  | FunP of 'key
  | NumP of 'key * 'loc
  | ObjP of 'key
  | StrP of 'key * 'loc
  | SymbolP of 'key * 'loc
  | VoidP of 'key
  | SentinelStrP of 'key * string * 'loc * string
  | SentinelNumP of 'key * string * 'loc * float * string
  | SentinelBoolP of 'key * string * 'loc * bool
  | SentinelNullP of 'key * string * 'loc
  | SentinelVoidP of 'key * string * 'loc
  | SentinelExprP of 'key * string * 'a
  | LatentP of 'a * ('key * int) Nel.t
  [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) tparam = TParam of {
  loc: 'loc;
  name: string;
  polarity: Polarity.t;
  bound: 'a option;
  default: 'a option;
} [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) tparams =
  | Mono
  | Poly of 'loc * ('loc, 'a) tparam * ('loc, 'a) tparam list
  [@@deriving iter, map, show {with_path = false}]

type 'a fun_param = FunParam of {
  name: string option;
  t: 'a;
} [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) fun_rest_param = FunRestParam of {
  name: string option;
  loc: 'loc;
  t: 'a;
} [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) fun_sig = FunSig of {
  tparams: ('loc, 'a) tparams;
  params: 'a fun_param list;
  rest_param: ('loc, 'a) fun_rest_param option;
  return: 'a;
  predicate: ('loc * (string, 'loc, 'a) predicate option) option;
} [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) obj_annot_proto =
  | ObjAnnotImplicitProto
  | ObjAnnotExplicitProto of 'loc * 'a
  | ObjAnnotCallable of {ts_rev: 'a Nel.t} (* implies function proto *)
  [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) accessor =
  | Get of 'loc * 'a
  | Set of 'loc * 'a
  | GetSet of 'loc * 'a * 'loc * 'a
  [@@deriving iter, map, show {with_path = false}]

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
  [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) obj_annot_prop =
  | ObjAnnotField of 'loc * 'a * Polarity.t
  | ObjAnnotAccess of ('loc, 'a) accessor
  | ObjAnnotMethod of {
      id_loc: 'loc;
      fn_loc: 'loc;
      def: ('loc, 'a) fun_sig;
    }
  [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) interface_prop =
  | InterfaceField of 'loc option * 'a * Polarity.t
  | InterfaceAccess of ('loc, 'a) accessor
  | InterfaceMethod of ('loc * 'loc * ('loc, 'a) fun_sig) Nel.t
  [@@deriving iter, map, show {with_path = false}]

type 'a obj_annot_dict = ObjDict of {
  name: string option;
  polarity: Polarity.t;
  key: 'a;
  value: 'a;
} [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) obj_spread_annot_elem =
  | ObjSpreadAnnotElem of 'a
  | ObjSpreadAnnotSlice of {
      dict: 'a obj_annot_dict option;
      props: ('loc, 'a) obj_annot_prop smap;
    }
  [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) obj_value_spread_elem =
  | ObjValueSpreadElem of 'a
  | ObjValueSpreadSlice of ('loc, 'a) obj_value_prop smap
  [@@deriving iter, map, show {with_path = false}]

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
  [@@deriving iter, map, show {with_path = false}]

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
  [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) class_sig = ClassSig of {
  tparams: ('loc, 'a) tparams;
  extends: ('loc, 'a) class_extends;
  implements: 'a list;
  static_props: ('loc, 'a) obj_value_prop smap;
  proto_props: ('loc, 'a) obj_value_prop smap;
  own_props: ('loc, 'a) obj_value_prop smap;
} [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) declare_class_sig = DeclareClassSig of {
  tparams: ('loc, 'a) tparams;
  extends: ('loc, 'a) class_extends;
  mixins: ('loc, 'a) class_mixins list;
  implements: 'a list;
  static_props: ('loc, 'a) interface_prop smap;
  own_props: ('loc, 'a) interface_prop smap;
  proto_props: ('loc, 'a) interface_prop smap;
  static_calls: 'a list;
  calls: 'a list;
} [@@deriving iter, map, show {with_path = false}]

type ('loc, 'a) interface_sig = InterfaceSig of {
  extends: 'a list;
  props: ('loc, 'a) interface_prop smap;
  calls: 'a list;
} [@@deriving iter, map, show {with_path = false}]

type enum_rep =
  | BoolRep of bool option
  | NumberRep of { truthy: bool }
  | StringRep of { truthy: bool }
  | SymbolRep
  [@@deriving iter, map, show {with_path = false}]

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
      bound: 'a option;
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
      name: string;
      def: ('loc, 'a) declare_class_sig;
    }
  | FunBinding of {
      id_loc: 'loc;
      name: string;
      async: bool;
      generator: bool;
      def: ('loc, 'a) fun_sig;
      statics: ('loc * 'a) smap;
    }
  | DeclareFun of {
      id_loc: 'loc;
      name: string;
      def: ('loc, 'a) fun_sig;
      tail: ('loc * ('loc, 'a) fun_sig) list;
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
    }
  | DisabledEnumBinding of {
      id_loc: 'loc;
      name: string;
    }
  [@@deriving iter, map, show {with_path = false}]

(* These accessors will compile to code that does not have a branch because
 * id_loc and name have the same offset for each constructor. *)
let def_id_loc = function
  | TypeAlias {id_loc; _}
  | OpaqueType {id_loc; _}
  | Interface {id_loc; _}
  | ClassBinding {id_loc; _}
  | DeclareClassBinding {id_loc; _}
  | FunBinding {id_loc; _}
  | DeclareFun {id_loc; _}
  | Variable {id_loc; _} -> id_loc
  | EnumBinding {id_loc; _} -> id_loc
  | DisabledEnumBinding {id_loc; _} -> id_loc

let def_name = function
  | TypeAlias {name; _}
  | OpaqueType {name; _}
  | Interface {name; _}
  | ClassBinding {name; _}
  | DeclareClassBinding {name; _}
  | FunBinding {name; _}
  | DeclareFun {name; _}
  | Variable {name; _} -> name
  | EnumBinding {name; _} -> name
  | DisabledEnumBinding {name; _} -> name

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
  | BooleanVal of 'loc
  | BooleanLit of 'loc * bool
  | NullLit of 'loc
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
  | ArrayLit of 'loc * 'a * 'a list
  [@@deriving iter, map, show {with_path = false}]

type 'a obj_kind =
  | ExactObj
  | InexactObj
  | IndexedObj of 'a obj_annot_dict
  [@@deriving iter, map, show {with_path = false}]

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

  | Exists of {loc: 'loc; force: bool}

  | Optional of 'a
  | Maybe of 'loc * 'a
  | Union of {loc: 'loc; t0: 'a; t1: 'a; ts: 'a list}
  | Intersection of {loc: 'loc; t0: 'a; t1: 'a; ts: 'a list}

  | Tuple of {loc: 'loc; ts: 'a list}
  | Array of 'loc * 'a
  | ReadOnlyArray of 'loc * 'a

  | SingletonString of 'loc * string
  | SingletonNumber of 'loc * float * string
  | SingletonBigInt of 'loc * string
  | SingletonBoolean of 'loc * bool

  | Typeof of {
      loc: 'loc;
      qname: string list;
      t: 'a;
    }

  | Bound of {
      ref_loc: 'loc;
      name: string;
    }

  | TEMPORARY_Number of 'loc * float * string
  | TEMPORARY_String of 'loc * string
  | TEMPORARY_LongString of 'loc
  | TEMPORARY_Boolean of 'loc * bool
  | TEMPORARY_Object of 'a
  | TEMPORARY_Array of 'loc * 'a

  | AnyWithLowerBound of 'loc * 'a
  | AnyWithUpperBound of 'loc * 'a

  | PropertyType of {loc: 'loc; obj: 'a; prop: string}
  | ElementType of {loc: 'loc; obj: 'a; elem: 'a}
  | NonMaybeType of 'loc * 'a
  | Shape of 'loc * 'a
  | Diff of 'loc * 'a * 'a
  | ReadOnly of 'loc * 'a
  | Keys of 'loc * 'a
  | Values of 'loc * 'a
  | Exact of 'loc * 'a
  | Rest of 'loc * 'a * 'a
  | ExportsT of 'loc * string
  | Call of {loc: 'loc; fn: 'a; args: 'a list}
  | TupleMap of {loc: 'loc; tup: 'a; fn: 'a}
  | ObjMap of {loc: 'loc; obj: 'a; fn: 'a}
  | ObjMapi of {loc: 'loc; obj: 'a; fn: 'a}

  | CharSet of 'loc * string

  | ClassT of 'loc * 'a

  | Function_apply of 'loc
  | Function_bind of 'loc
  | Function_call of 'loc
  | Object_assign of 'loc
  | Object_getPrototypeOf of 'loc
  | Object_setPrototypeOf of 'loc

  | Compose of 'loc
  | ComposeReverse of 'loc

  | ReactAbstractComponent of {loc: 'loc; config: 'a; instance: 'a}
  | ReactConfig of {loc: 'loc; props: 'a; default: 'a}
  | ReactPropTypePrimitive of 'loc * 'a
  | ReactPropTypePrimitiveRequired of 'loc * 'a
  | ReactPropTypeArrayOf of 'loc
  | ReactPropTypeInstanceOf of 'loc
  | ReactPropTypeObjectOf of 'loc
  | ReactPropTypeOneOf of 'loc
  | ReactPropTypeOneOfType of 'loc
  | ReactPropTypeShape of 'loc
  | ReactCreateClass of 'loc
  | ReactCreateElement of 'loc
  | ReactCloneElement of 'loc
  | ReactElementFactory of 'loc * 'a
  | ReactElementProps of 'loc * 'a
  | ReactElementConfig of 'loc * 'a
  | ReactElementRef of 'loc * 'a

  | FacebookismIdx of 'loc
  | FacebookismTypeAssertIs of 'loc
  | FacebookismTypeAssertThrows of 'loc
  | FacebookismTypeAssertWraps of 'loc

  | FlowDebugPrint of 'loc
  | FlowDebugThrow of 'loc
  | FlowDebugSleep of 'loc

  | Pred of 'loc * int
  | Refine of {loc: 'loc; base: 'a; fn_pred: 'a; index: int}

  | Trusted of 'loc * 'a
  | Private of 'loc * 'a

  | FunAnnot of 'loc * ('loc, 'a) fun_sig

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
  [@@deriving iter, map, show {with_path = false}]

(* Along with literal expressions, the signature extractor also encodes some
 * operations over values and annotations, like unary operators and
 * destructuring. *)
type 'a op =
  | GetProp of string
  | GetElem of 'a
  | Unary of Flow_ast.Expression.Unary.operator
  [@@deriving iter, map, show {with_path = false}]

(* Parsing out a signature can fail. There are three interesting failure modes,
 * which are all conflated in this type.
 *
 * 1. If there are insufficient annotations to extract a signature, we should
 *    should inform the user and proceeed as if the unknown type was any.
 * 2. If some unsupported or unexpected syntax was detected, we should proceed
 *    as if the type was any, but not inform the user. The checking phase will
 *    uncover the same error.
 * 3. If there is some value syntax, which is just not supported by the
 *    signature extractor, we should fix the signature extractor to support that
 *    syntax. These represent missing functionality in the analysis which should
 *    be fixed.
 *)
type errno =
  | UnexpectedTypeof
  | TArgArity0
  | TArgArity1
  | TArgArity2
  | TArgArity3
  | TArgMinArity1
  | UnexpectedTArg
  | UnsupportedTemporaryType
  | MissingAnnotation
  | EmptyObjectLiteral
  | EmptyArrayLiteral
  | UnsupportedComputedProperty
  | UnsupportedArrayHole
  | UnsupportedArraySpread
  | UnsupportedTemporaryObjectProp
  | UnsupportedKeyMirrorProp
  | UnsupportedJSXElement
  | TODO_Literal
  | TODO_Expression
  [@@deriving show {with_path = false}]

type 'loc error = 'loc * errno [@@deriving show {with_path = false}]
