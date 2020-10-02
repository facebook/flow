(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mk_id : unit -> int

type 'loc virtual_reason_desc =
  | RTrusted of 'loc virtual_reason_desc
  | RPrivate of 'loc virtual_reason_desc
  | RAnyExplicit
  | RAnyImplicit
  | RNumber
  | RBigInt
  | RString
  | RBoolean
  | RMixed
  | REmpty
  | RVoid
  | RNull
  | RVoidedNull
  | RSymbol
  | RExports
  | RNullOrVoid
  | RLongStringLit of int (* Max length *)
  | RStringLit of string
  | RNumberLit of string
  | RBigIntLit of string
  | RBooleanLit of bool
  | RMatchingProp of string * 'loc virtual_reason_desc
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
  | RTupleLength of int
  | RTupleOutOfBoundsAccess
  | RFunction of reason_desc_function
  | RFunctionType
  | RFunctionBody
  | RFunctionCall of 'loc virtual_reason_desc
  | RFunctionCallType
  | RFunctionUnusedArgument
  | RJSXFunctionCall of string
  | RJSXIdentifier of string * string
  | RJSXElementProps of string
  | RJSXElement of string option
  | RJSXText
  | RFbt
  | RUnaryOperator of string * 'loc virtual_reason_desc
  | RBinaryOperator of string * 'loc virtual_reason_desc * 'loc virtual_reason_desc
  | RLogical of string * 'loc virtual_reason_desc * 'loc virtual_reason_desc
  | RTemplateString
  | RUnknownString
  | RUnionEnum
  | REnum of string
  | REnumRepresentation of 'loc virtual_reason_desc
  | RGetterSetterProperty
  | RThis
  | RThisType
  | RExistential
  | RImplicitInstantiation
  | RTooFewArgs
  | RTooFewArgsExpectedRest
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
  | RConstructorCall of 'loc virtual_reason_desc
  | RReturn
  | RImplicitReturn of 'loc virtual_reason_desc
  | RRegExp
  | RSuper
  | RNoSuper
  | RDummyPrototype
  | RDummyThis
  | RTupleMap
  | RObjectMap
  | RObjectMapi
  | RType of string
  | RTypeAlias of string * 'loc option * 'loc virtual_reason_desc
  | ROpaqueType of string
  | RTypeParam of string * ('loc virtual_reason_desc * 'loc) * ('loc virtual_reason_desc * 'loc)
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
  | RMember of {
      object_: string;
      property: string;
    }
  | RPropertyOf of string * 'loc virtual_reason_desc
  | RPropertyIsAString of string
  | RMissingProperty of string option
  | RUnknownProperty of string option
  | RUndefinedProperty of string
  | RSomeProperty
  | RNameProperty of 'loc virtual_reason_desc
  | RMissingAbstract of 'loc virtual_reason_desc
  | RFieldInitializer of string
  | RUntypedModule of string
  | RNamedImportedType of string * string
  | RImportStarType of string
  | RImportStarTypeOf of string
  | RImportStar of string
  | RDefaultImportedType of string * string
  | RAsyncImport
  | RCode of string
  | RCustom of string
  | RPolyType of 'loc virtual_reason_desc
  | RPolyTest of string * 'loc virtual_reason_desc * ALoc.id * bool
  | RExactType of 'loc virtual_reason_desc
  | ROptional of 'loc virtual_reason_desc
  | RMaybe of 'loc virtual_reason_desc
  | RRestArray of 'loc virtual_reason_desc
  | RAbstract of 'loc virtual_reason_desc
  | RTypeApp of 'loc virtual_reason_desc
  | RTypeAppImplicit of 'loc virtual_reason_desc
  | RThisTypeApp of 'loc virtual_reason_desc
  | RExtends of 'loc virtual_reason_desc
  | RClass of 'loc virtual_reason_desc
  | RStatics of 'loc virtual_reason_desc
  | RSuperOf of 'loc virtual_reason_desc
  | RFrozen of 'loc virtual_reason_desc
  | RBound of 'loc virtual_reason_desc
  | RPredicateOf of 'loc virtual_reason_desc
  | RPredicateCall of 'loc virtual_reason_desc
  | RPredicateCallNeg of 'loc virtual_reason_desc
  | RRefined of 'loc virtual_reason_desc
  | RIncompatibleInstantiation of string
  | RSpreadOf of 'loc virtual_reason_desc
  | RShapeOf of 'loc virtual_reason_desc
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
  | RReactChildrenOrType of 'loc virtual_reason_desc
  | RReactChildrenOrUndefinedOrType of 'loc virtual_reason_desc
  | RReactSFC
  | RReactConfig
  | RPossiblyMissingPropFromObj of string * 'loc virtual_reason_desc
  | RWidenedObjProp of 'loc virtual_reason_desc
  | RUnionBranching of 'loc virtual_reason_desc * int

and reason_desc_function =
  | RAsync
  | RGenerator
  | RAsyncGenerator
  | RNormal
  | RUnknown

type reason_desc = ALoc.t virtual_reason_desc

type 'loc virtual_reason

type reason = ALoc.t virtual_reason

type concrete_reason = Loc.t virtual_reason

type t = reason (* convenience *)

module TestID : sig
  val run : ('a -> 'b) -> 'a -> 'b
end

(* reason constructor *)
val mk_reason : 'loc virtual_reason_desc -> 'loc -> 'loc virtual_reason

(* ranges *)
val in_range : Loc.t -> Loc.t -> bool

val string_of_desc : 'loc virtual_reason_desc -> string

val map_reason_locs : ('a -> 'b) -> 'a virtual_reason -> 'b virtual_reason

val map_desc_locs : ('a -> 'b) -> 'a virtual_reason_desc -> 'b virtual_reason_desc

val string_of_loc : ?strip_root:Path.t option -> Loc.t -> string

val string_of_aloc : ?strip_root:Path.t option -> ALoc.t -> string

val json_of_loc :
  ?strip_root:Path.t option ->
  ?catch_offset_errors:bool ->
  offset_table:Offset_utils.t option ->
  Loc.t ->
  Hh_json.json

val json_of_loc_props :
  ?strip_root:Path.t option ->
  ?catch_offset_errors:bool ->
  offset_table:Offset_utils.t option ->
  Loc.t ->
  (string * Hh_json.json) list

val json_of_source : ?strip_root:Path.t option -> File_key.t option -> Hh_json.json

val json_source_type_of_source : File_key.t option -> Hh_json.json

val locationless_reason : reason_desc -> reason

val func_reason : async:bool -> generator:bool -> ALoc.t -> reason

val is_internal_name : string -> bool

val internal_name : string -> string

val is_internal_module_name : string -> bool

val internal_module_name : string -> string

val uninternal_module_name : string -> string

val is_instantiable_reason : 'loc virtual_reason -> bool

val is_constant_reason : 'loc virtual_reason -> bool

val is_typemap_reason : 'loc virtual_reason -> bool

val is_calltype_reason : 'loc virtual_reason -> bool

val is_nullish_reason : 'loc virtual_reason -> bool

val is_scalar_reason : 'loc virtual_reason -> bool

val is_array_reason : 'loc virtual_reason -> bool

val is_literal_object_reason : 'loc virtual_reason -> bool

val is_literal_array_reason : 'loc virtual_reason -> bool

val derivable_reason : 'loc virtual_reason -> 'loc virtual_reason

val is_derivable_reason : 'loc virtual_reason -> bool

val builtin_reason : reason_desc -> reason

(* reason location preds *)
val is_builtin_reason : ('loc -> File_key.t option) -> 'loc virtual_reason -> bool

val is_lib_reason : reason -> bool

val is_lib_reason_def : reason -> bool

val is_blamable_reason : reason -> bool

val string_of_source : ?strip_root:Path.t option -> File_key.t -> string

val string_of_reason : ?strip_root:Path.t option -> reason -> string

val dump_reason : ?strip_root:Path.t option -> reason -> string

(* accessors *)
val poly_loc_of_reason : 'loc virtual_reason -> 'loc

val loc_of_reason : concrete_reason -> Loc.t

val aloc_of_reason : reason -> ALoc.t

val def_aloc_of_reason : reason -> ALoc.t

val def_loc_of_reason : concrete_reason -> Loc.t

val annot_aloc_of_reason : reason -> ALoc.t option

val desc_of_reason : ?unwrap:bool -> 'loc virtual_reason -> 'loc virtual_reason_desc

val annot_loc_of_reason : concrete_reason -> Loc.t option

(* simple way to get derived reasons whose descriptions are
   simple replacements of the original *)
(* replace desc, but keep loc, def_loc, annot_loc *)
val update_desc_reason :
  ('loc virtual_reason_desc -> 'loc virtual_reason_desc) ->
  'loc virtual_reason ->
  'loc virtual_reason

(* replace desc, keep loc, but clobber def_loc, annot_loc as in new reason *)
val update_desc_new_reason :
  ('loc virtual_reason_desc -> 'loc virtual_reason_desc) ->
  'loc virtual_reason ->
  'loc virtual_reason

(* replace desc, but keep loc, def_loc, annot_loc *)
val replace_desc_reason : 'loc virtual_reason_desc -> 'loc virtual_reason -> 'loc virtual_reason

(* replace desc, keep loc, but clobber def_loc, annot_loc as in new reason *)
val replace_desc_new_reason : 'loc virtual_reason_desc -> 'loc virtual_reason -> 'loc virtual_reason

(* replace loc, but keep def_loc *)
val repos_reason : 'loc -> 'loc virtual_reason -> 'loc virtual_reason

(* add / replace annot_loc, but keep loc and def_loc *)
val annot_reason : annot_loc:'loc -> 'loc virtual_reason -> 'loc virtual_reason

(* when annot_loc is given, same as annot_reason; otherwise, identity *)
val opt_annot_reason : ?annot_loc:'loc -> 'loc virtual_reason -> 'loc virtual_reason

(* create a new reason with annot_loc = loc: same as mk_reason followed by annot_reason *)
val mk_annot_reason : 'loc virtual_reason_desc -> 'loc -> 'loc virtual_reason

module ReasonMap : WrappedMap.S with type key = reason

val mk_expression_reason : (ALoc.t, ALoc.t) Flow_ast.Expression.t -> reason

val mk_pattern_reason : (ALoc.t, ALoc.t) Flow_ast.Pattern.t -> reason

val unknown_elem_empty_array_desc : reason_desc

val inferred_union_elem_array_desc : reason_desc

val invalidate_rtype_alias : 'loc virtual_reason_desc -> 'loc virtual_reason_desc

val code_desc_of_literal : 'loc Flow_ast.Literal.t -> string

val code_desc_of_expression : wrap:bool -> ('a, 'b) Flow_ast.Expression.t -> string

val code_desc_of_pattern : ('a, 'b) Flow_ast.Pattern.t -> string

(* Pass in any available aloc tables to be used when comparing abstract and concrete locations from
 * the same file. Usually `Context.aloc_tables` is a good choice, but if the context is not
 * available, the empty map may be appropriate. *)
val concretize_equal : ALoc.table Lazy.t Utils_js.FilenameMap.t -> t -> t -> bool

val pp_virtual_reason_desc :
  (Format.formatter -> 'loc -> Ppx_deriving_runtime.unit) ->
  Format.formatter ->
  'loc virtual_reason_desc ->
  Ppx_deriving_runtime.unit

val show_virtual_reason_desc :
  (Format.formatter -> 'loc -> Ppx_deriving_runtime.unit) -> 'loc virtual_reason_desc -> string

val show_reason_desc_function : reason_desc_function -> string
