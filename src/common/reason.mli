(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mk_id: unit -> int

type reason_desc =
  | RNumber | RString | RBoolean | RMixed | REmpty | RAny | RVoid | RNull
  | RNullOrVoid
  | RStringLit of string
  | RNumberLit of string
  | RBooleanLit of bool
  | RMatchingProp of string * reason_desc
  | RObject
  | RObjectLit
  | RObjectType
  | RObjectClassName
  | RInterfaceType
  | RArray
  | RArrayLit
  | REmptyArrayLit
  | RArrayType
  | RROArrayType
  | RTupleType
  | RTupleElement
  | RTupleOutOfBoundsAccess
  | RFunction of reason_desc_function
  | RFunctionType
  | RFunctionBody
  | RFunctionCall of reason_desc
  | RFunctionCallType
  | RFunctionUnusedArgument
  | RJSXFunctionCall of string
  | RJSXIdentifier of string * string
  | RJSXElementProps of string
  | RJSXElement of string option
  | RJSXText
  | RFbt
  | RUnaryOperator of string * reason_desc
  | RBinaryOperator of string * reason_desc * reason_desc
  | RLogical of string * reason_desc * reason_desc
  | RAnyObject
  | RAnyFunction
  | RTemplateString
  | RUnknownString
  | REnum
  | RGetterSetterProperty
  | RThis
  | RThisType
  | RExistential
  | RTooFewArgs
  | RTooFewArgsExpectedRest
  | RUninitializedThis
  | RConstructorReturn
  | RNewObject
  | RUnion
  | RUnionType
  | RIntersection
  | RIntersectionType
  | RKeySet
  | RAnd
  | RConditional
  | RPrototype
  | RObjectPrototype
  | RFunctionPrototype
  | RDestructuring
  | RDefaultValue
  | RConstructor
  | RDefaultConstructor
  | RConstructorCall of reason_desc
  | RReturn
  | RImplicitReturn of reason_desc
  | RRegExp
  | RSuper
  | RNoSuper
  | RDummyPrototype
  | RDummyThis
  | RTupleMap
  | RObjectMap
  | RObjectMapi
  | RType of string
  | RTypeAlias of string * reason_desc
  | ROpaqueType of string
  | RTypeParam of string * reason_desc * Loc.t
  | RTypeof of string
  | RMethod of string option
  | RMethodCall of string option
  | RParameter of string option
  | RRestParameter of string option
  | RIdentifier of string
  | RIdentifierAssignment of string
  | RPropertyAssignment of string option
  | RProperty of string option
  | RPrivateProperty of string
  | RShadowProperty of string
  | RPropertyOf of string * reason_desc
  | RPropertyIsAString of string
  | RMissingProperty of string option
  | RUnknownProperty of string option
  | RUndefinedProperty of string
  | RSomeProperty
  | RNameProperty of reason_desc
  | RMissingAbstract of reason_desc
  | RFieldInitializer of string
  | RUntypedModule of string
  | RNamedImportedType of string
  | RImportStarType of string
  | RImportStarTypeOf of string
  | RImportStar of string
  | RDefaultImportedType of string * string
  | RCode of string
  | RCustom of string
  | RPolyType of reason_desc
  | RPolyTest of string * reason_desc
  | RExactType of reason_desc
  | ROptional of reason_desc
  | RMaybe of reason_desc
  | RRestArray of reason_desc
  | RAbstract of reason_desc
  | RTypeApp of reason_desc
  | RThisTypeApp of reason_desc
  | RExtends of reason_desc
  | RStatics of reason_desc
  | RSuperOf of reason_desc
  | RFrozen of reason_desc
  | RBound of reason_desc
  | RVarianceCheck of reason_desc
  | RPredicateOf of reason_desc
  | RPredicateCall of reason_desc
  | RPredicateCallNeg of reason_desc
  | RRefined of reason_desc
  | RIncompatibleInstantiation of string
  | RSpreadOf of reason_desc
  | RObjectPatternRestProp
  | RArrayPatternRestProp
  | RCommonJSExports of string
  | RModule of string
  | ROptionalChain

  | RReactProps
  | RReactElement of string option
  | RReactClass
  | RReactComponent
  | RReactStatics
  | RReactDefaultProps
  | RReactState
  | RReactPropTypes
  | RReactChildren
  | RReactChildrenOrType of reason_desc
  | RReactChildrenOrUndefinedOrType of reason_desc
  | RReactSFC

and reason_desc_function =
  | RAsync
  | RGenerator
  | RAsyncGenerator
  | RNormal

type reason
type t = reason (* convenience *)

module TestID: sig
  val run: ('a -> unit) -> 'a -> unit
end

val lexpos: string -> int -> int -> Lexing.position

(* reason constructor *)
val mk_reason: reason_desc -> Loc.t -> reason

(* ranges *)
val diff_range: Loc.t -> int * int
val in_range: Loc.t -> Loc.t -> bool

val string_of_desc: reason_desc -> string

val string_of_loc_pos: Loc.t -> string
val string_of_loc: ?strip_root:Path.t option -> Loc.t -> string
val json_of_loc: ?strip_root:Path.t option -> Loc.t -> Hh_json.json
val json_of_loc_props: ?strip_root:Path.t option -> Loc.t -> (string * Hh_json.json) list

val locationless_reason: reason_desc -> reason

val func_reason: Loc.t Ast.Function.t -> Loc.t -> reason

val is_internal_name: string -> bool
val internal_name: string -> string

val is_internal_module_name: string -> bool
val internal_module_name: string -> string
val uninternal_module_name: string -> string

val internal_pattern_name: Loc.t -> string

val is_instantiable_reason: reason -> bool

val is_constant_reason: reason -> bool

val is_typemap_reason: reason -> bool
val is_calltype_reason: reason -> bool

val is_nullish_reason: reason -> bool
val is_scalar_reason: reason -> bool
val is_array_reason: reason -> bool

val is_literal_object_reason: reason -> bool
val is_literal_array_reason: reason -> bool

val derivable_reason: reason -> reason
val is_derivable_reason: reason -> bool

val builtin_reason: reason_desc -> reason

(* reason location preds *)
val is_builtin_reason: reason -> bool
val is_lib_reason: reason -> bool
val is_blamable_reason: reason -> bool
val reasons_overlap: reason -> reason -> bool

val string_of_source: ?strip_root:Path.t option -> File_key.t -> string
val string_of_reason: ?strip_root:Path.t option -> reason -> string
val json_of_reason: ?strip_root:Path.t option -> reason -> Hh_json.json
val dump_reason: ?strip_root:Path.t option -> reason -> string

(* accessors *)
val loc_of_reason: reason -> Loc.t
val def_loc_of_reason: reason -> Loc.t
val annot_loc_of_reason: reason -> Loc.t option
val desc_of_reason: ?unwrap:bool -> reason -> reason_desc

(* simple way to get derived reasons whose descriptions are
   simple replacements of the original *)
val replace_reason: ?keep_def_loc:bool -> (reason_desc -> reason_desc) -> reason -> reason
val replace_reason_const: ?keep_def_loc:bool -> reason_desc -> reason -> reason

val repos_reason: Loc.t -> ?annot_loc:Loc.t -> reason -> reason
val annot_reason: reason -> reason

val do_patch: string list -> (int * int * string) list -> string

module ReasonMap : MyMap.S with type key = reason

val mk_expression_reason: Loc.t Ast.Expression.t -> reason

val unknown_elem_empty_array_desc: reason_desc
val inferred_union_elem_array_desc: reason_desc
