(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val mk_id: unit -> int

type reason_desc =
  | RNumber | RString | RBoolean | RMixed | REmpty | RAny | RVoid | RNull
  | RStringLit of string
  | RNumberLit of string
  | RBooleanLit of bool
  | RObject
  | RObjectLit
  | RObjectType
  | RObjectClassName
  | RArray
  | RArrayLit
  | REmptyArrayLit
  | RArrayType
  | RTupleType
  | RTupleElement
  | RFunction of reason_desc_function
  | RArrowFunction of reason_desc_function
  | RFunctionType
  | RFunctionBody
  | RFunctionCall
  | RJSXFunctionCall of string
  | RJSXIdentifier of string * string
  | RJSXElementProps of string
  | RJSXElement of string option
  | RAnyObject
  | RAnyFunction
  | RUnknownString
  | RStringEnum
  | RNumberEnum
  | RGetterFunction
  | RSetterFunction
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
  | RDestructuring
  | RConstructor
  | RConstructorCall
  | RReturn
  | RRegExp
  | RSuper
  | RNoSuper
  | RDummyPrototype
  | RDummyThis
  | RTupleMap
  | RObjectMap
  | RObjectMapi
  | RType of string
  | RTypeParam of string * reason_desc
  | RMethodCall of string option
  | RParameter of string
  | RRestParameter of string
  | RIdentifier of string
  | RIdentifierAssignment of string
  | RPropertyAssignment of string
  | RProperty of string option
  | RShadowProperty of string
  | RPropertyOf of string * reason_desc
  | RPropertyIsAString of string
  | RMissingProperty of string option
  | RUnknownProperty of string option
  | RSomeProperty
  | RNameProperty of reason_desc
  | RMissingAbstract of reason_desc
  | RFieldInitializer of string
  | RCustom of string
  | RPolyType of reason_desc
  | RClassType of reason_desc
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
  | RIncompatibleInstantiation of string
  | RSpreadOf of reason_desc
  | RObjectPatternRestProp
  | RArrayPatternRestProp

  | RReactElement of string option
  | RReactClass
  | RReactComponent
  | RReactStatics
  | RReactDefaultProps
  | RReactState
  | RReactComponentProps
  | RReactElementProps of string
  | RReactPropTypes
  | RPropTypeArray
  | RPropTypeFunc
  | RPropTypeObject
  | RPropTypeArrayOf
  | RPropTypeInstanceOf
  | RPropTypeObjectOf
  | RPropTypeOneOf
  | RPropTypeOneOfType
  | RPropTypeShape
  | RPropTypeFbt

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
val string_of_loc: Loc.t -> string
val json_of_loc: ?strip_root:Path.t option -> Loc.t -> Hh_json.json

val locationless_reason: reason_desc -> reason

val is_internal_name: string -> bool
val internal_name: string -> string

val is_internal_module_name: string -> bool
val internal_module_name: string -> string

val internal_pattern_name: Loc.t -> string

val is_instantiable_reason: reason -> bool

val is_constant_property_reason: reason -> bool

val is_method_call_reason: string -> reason -> bool

val derivable_reason: reason -> reason
val is_derivable_reason: reason -> bool

val builtin_reason: reason_desc -> reason

(* reason location preds *)
val is_builtin_reason: reason -> bool
val is_lib_reason: reason -> bool
val is_blamable_reason: reason -> bool
val reasons_overlap: reason -> reason -> bool

val string_of_reason: reason -> string
val json_of_reason: ?strip_root:Path.t option -> reason -> Hh_json.json
val dump_reason: reason -> string

(* accessors *)
val loc_of_reason: reason -> Loc.t

val desc_of_reason: reason -> reason_desc

val origin_of_reason: reason -> reason option

(* simple way to get derived reasons whose descriptions are
   simple replacements of the original *)
val replace_reason: (reason_desc -> reason_desc) -> reason -> reason
val replace_reason_const: reason_desc -> reason -> reason

val repos_reason: Loc.t -> reason -> reason

val update_origin_of_reason: reason option -> reason -> reason

val do_patch: string list -> (int * int * string) list -> string

val strip_root: Path.t -> reason -> reason
val strip_root_from_loc: Path.t -> Loc.t -> Loc.t
