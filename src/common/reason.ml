(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(* This module defines a general notion of trace, which is used in modules
   Type_inference_js and Flow_js to record how the typechecker reasons about
   code, systematically collecting, simplifying, and solving constraints. This
   is extremely useful, not only for debugging the typechecker but also to
   really understand why an error is reported. *)

(* Eventually, trace information should be printed out only in verbose mode,
   since Flow reports all errors it finds and the trace for every error can get
   quite detailed dependening on how far apart the "source" and "sink" are and
   how convoluted the flow between them is. *)

open Utils_js

type name = OrdinaryName of string [@@deriving eq, ord, show]

let display_string_of_name = function
  | OrdinaryName x -> x

let mk_id () = HeapIdent.make ""

(* Reasons are included in types mainly for error reporting, but sometimes we
   also use reasons in types to recover information on the source code that
   caused those reasons to be created. Two examples of such secondary uses of
   reasons are:

   - strictness analysis: we use reasons to locate the origin of an object and
   the origin of an operation on the object, and use such origins to determine
   whether certain errors should be suppressed.

   - termination analysis: we use reasons to limit instantiation of type
   parameters in polymorphic types at particular locations, to prevent the type
   checker from generating an unbounded number of constraints.
*)

type 'loc virtual_reason_desc =
  | RAnyExplicit
  | RAnyImplicit
  | RNumber
  | RBigInt
  | RString
  | RBoolean
  | RMixed
  | REmpty
  | REmptyArrayElement
  | RVoid
  | RNull
  | RVoidedNull
  | RSymbol
  | RExports
  | RNullOrVoid
  | RStringLit of name
  | RStringPrefix of { prefix: string }
  | RStringWithoutPrefix of { prefix: string }
  | RStringSuffix of { suffix: string }
  | RStringWithoutSuffix of { suffix: string }
  | RNumberLit of string
  | RBigIntLit of string
  | RBooleanLit of bool
  | RIndexedAccess of { optional: bool }
  | RConditionalType
  | RMatch
  | RMatchPattern
  | RMatchWildcard
  | RMatchingProp of string * 'loc virtual_reason_desc
  | RObject
  | RObjectLit
  | RObjectLit_UNSOUND
  | RConstObjectLit
  | RObjectType
  | RMappedType
  | RInterfaceType
  | RArray
  | RArrayLit
  | RArrayLit_UNSOUND
  | RConstArrayLit
  | REmptyArrayLit
  | RArrayType
  | RArrayElement
  | RArrayNthElement of int
  | RArrayHole
  | RInferredUnionElemArray of {
      instantiable: bool;
      is_empty: bool;
    }
  | RROArrayType
  | RTupleType
  | RTupleElement of { name: string option }
  | RTupleLength of int
  | RTupleOutOfBoundsAccess of int
  | RTupleUnknownElementFromInexact
  | RFunction of reason_desc_function
  | RFunctionType
  | RFunctionBody
  | RFunctionCall of 'loc virtual_reason_desc
  | RFunctionUnusedArgument
  | RJSXChild
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
  | REnum of { name: string option }
  | REnumMember of {
      enum: 'loc virtual_reason_desc;
      member_name: string;
    }
  | REnumUnknownMembers of 'loc virtual_reason_desc
  | RThis
  | RThisType
  | RImplicitInstantiation
  | RConstructorVoidReturn
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
  | RRegExp
  | RSuper
  | RDummyPrototype
  | RDummyThis
  | RImplicitThis of 'loc virtual_reason_desc
  | RObjectKeyMirror
  (* TODO type names should not be able to be internal names *)
  | RType of name
  | RTypeAlias of string * 'loc option (* reliable def loc *) * 'loc virtual_reason_desc
  | ROpaqueType of string
  | RTypeParam of
      Subst_name.t
      * ('loc virtual_reason_desc * 'loc)
      * (*reason op *)
      ('loc virtual_reason_desc * 'loc)
    (* reason tapp *)
  | RTypeParamDefault of 'loc virtual_reason_desc
  | RTypeParamBound of 'loc virtual_reason_desc
  | RTypeof of string
  | RMethod of string option
  | RMethodCall of string option
  | RParameter of string option
  | RRestParameter of string option
  | RPatternParameter of string
  | RIdentifier of name
  | RPropertyAssignment of string option
  | RProperty of name option
  | RPrivateProperty of string
  | RMember of {
      object_: string;
      property: string;
    }
  | RPropertyOf of name * 'loc virtual_reason_desc
  | RPropertyIsAString of name
  | RMissingProperty of name option
  | RUnknownProperty of name option
  | RUnknownUnspecifiedProperty of 'loc virtual_reason_desc
  | RUndefinedProperty of name
  | RSomeProperty
  | RNameProperty of 'loc virtual_reason_desc
  | RNamedImportedType of Flow_import_specifier.userland (* module *) * string (* local name *)
  | RImportStarType of string
  | RImportStarTypeOf of string
  | RImportStar of string
  | RDefaultImportedType of string * Flow_import_specifier.userland
  | RAsyncImport
  | RCode of string
  | RCustom of string
  | RNonnullAssert
  | RMixins
  | RUnaryMinus
  | RUnaryNot
  | RRest
  | RGlobalObject
  | RProviders
  | RForOfElement
  | RUpdate
  | RUnusedYield
  | RUnusedReturn
  | RCommonInterface
  | RContextualVariable
  | RNext
  | RModuleReference
  | RNewFunction
  | RNewArray
  | RArrayLength
  | RImportMeta
  | RAwait
  | RAsyncReturn
  | RCallableObjectType
  | RClassExtends
  | RClassMixins
  | RReactKey
  | RNoProviders
  | RPolyType of 'loc virtual_reason_desc
  | RExactType of 'loc virtual_reason_desc
  | RReadOnlyType
  | ROptional of 'loc virtual_reason_desc
  | RMaybe of 'loc virtual_reason_desc
  | RRestArrayLit of 'loc virtual_reason_desc
  | RTypeApp of 'loc virtual_reason_desc
  | RTypeAppImplicit of 'loc virtual_reason_desc
  | RExtends of 'loc virtual_reason_desc
  | RClass of 'loc virtual_reason_desc
  | RStatics of 'loc virtual_reason_desc
  | RSuperOf of 'loc virtual_reason_desc
  | RFrozen of 'loc virtual_reason_desc
  | RBound of 'loc virtual_reason_desc
  | RRefined of 'loc virtual_reason_desc
  | RRefinedElement of 'loc virtual_reason_desc
  | RIncompatibleInstantiation of Subst_name.t
  | RPartialOf of 'loc virtual_reason_desc
  | RRequiredOf of 'loc virtual_reason_desc
  | RObjectPatternRestProp
  | RArrayPatternRestProp
  | RModule of Flow_import_specifier.userland
  | RNamespace of string
  | ROptionalChain
  | RReactProps
  (* TODO React element names should not allow internal names *)
  | RReactElement of {
      name_opt: name option;
      from_component_syntax: bool;
    }
  | RReactDefaultProps
  | RReactChildren
  | RReactChildrenOrType of 'loc virtual_reason_desc
  | RReactChildrenOrUndefinedOrType of 'loc virtual_reason_desc
  | RReactRef
  | RPossiblyMissingPropFromObj of name * 'loc virtual_reason_desc
  | RUnionBranching of 'loc virtual_reason_desc * int
  | RUninitialized
  | RPossiblyUninitialized
  | RUnannotatedNext
  | RTypeGuard
  | RTypeGuardParam of string
  | RComponent of name
  | RComponentType
  | RPropsOfComponent of 'loc virtual_reason_desc
  | RInstanceOfComponent of 'loc virtual_reason_desc
  | RDefaultTypeArgumentAtIndex of {
      desc_type: 'loc virtual_reason_desc;
      desc_default: 'loc virtual_reason_desc;
      position: int;
    }
  | RRenderType of 'loc virtual_reason_desc
  | RRenderMaybeType of 'loc virtual_reason_desc
  | RRenderStarType of 'loc virtual_reason_desc
  | RRendersNothing
  | RAutocompleteToken
[@@deriving eq, ord, show]

and reason_desc_function =
  | RAsync
  | RGenerator
  | RAsyncGenerator
  | RNormal
  | RUnknown

type reason_desc = ALoc.t virtual_reason_desc

let rec map_desc_locs f = function
  | ( RAnyExplicit | RAnyImplicit | RNumber | RBigInt | RString | RBoolean | RMixed | REmpty | RVoid
    | RNull | RVoidedNull | RSymbol | RExports | RNullOrVoid | RStringLit _ | RStringPrefix _
    | RStringWithoutPrefix _ | RStringSuffix _ | RStringWithoutSuffix _ | RNumberLit _
    | RBigIntLit _ | RBooleanLit _ | RObject | RConstObjectLit | RObjectLit | RObjectLit_UNSOUND
    | RObjectType | RInterfaceType | RArray | RArrayLit | RArrayLit_UNSOUND | RConstArrayLit
    | REmptyArrayLit | RArrayType | RArrayElement | RArrayNthElement _ | RArrayHole | RROArrayType
    | RTupleType | RTupleElement _ | RTupleLength _ | RTupleOutOfBoundsAccess _
    | RTupleUnknownElementFromInexact | RFunction _ | RFunctionType | RFunctionBody
    | RFunctionUnusedArgument | RJSXChild | RJSXFunctionCall _ | RJSXIdentifier _
    | RJSXElementProps _ | RJSXElement _ | RJSXText | RFbt | RUninitialized | RPossiblyUninitialized
    | RUnannotatedNext | REmptyArrayElement | RMappedType | RTypeGuard | RTypeGuardParam _
    | RComponent _ | RComponentType | RInferredUnionElemArray _ ) as r ->
    r
  | RFunctionCall desc -> RFunctionCall (map_desc_locs f desc)
  | RUnknownUnspecifiedProperty desc -> RUnknownUnspecifiedProperty (map_desc_locs f desc)
  | RUnaryOperator (s, desc) -> RUnaryOperator (s, map_desc_locs f desc)
  | RBinaryOperator (s, d1, d2) -> RBinaryOperator (s, map_desc_locs f d1, map_desc_locs f d2)
  | RLogical (s, d1, d2) -> RLogical (s, map_desc_locs f d1, map_desc_locs f d2)
  | ( RTemplateString | RUnknownString | RUnionEnum | REnum _ | RThis | RThisType
    | RImplicitInstantiation | RConstructorVoidReturn | RUnion | RUnionType | RIntersection
    | RIntersectionType | RKeySet | RAnd | RConditional | RPrototype | RObjectPrototype
    | RFunctionPrototype | RDestructuring | RDefaultValue | RConstructor | RReturn
    | RDefaultConstructor | RRegExp | RSuper | RDummyPrototype | RDummyThis | RType _ | RTypeof _
    | RMethod _ | RMethodCall _ | RParameter _ | RRestParameter _ | RPatternParameter _
    | RIdentifier _ | RPropertyAssignment _ | RProperty _ | RPrivateProperty _ | RMember _
    | RPropertyIsAString _ | RMissingProperty _ | RUnknownProperty _ | RUndefinedProperty _
    | RSomeProperty | RNamedImportedType _ | RImportStarType _ | RImportStarTypeOf _ | RImportStar _
    | RDefaultImportedType _ | RAsyncImport | RCode _ | RCustom _ | RNonnullAssert | RMixins
    | RUnaryMinus | RUnaryNot | RRest | RGlobalObject | RProviders | RForOfElement | RUpdate
    | RUnusedYield | RUnusedReturn | RCommonInterface | RContextualVariable | RNext
    | RModuleReference | RNewFunction | RNewArray | RArrayLength | RImportMeta | RAwait
    | RAsyncReturn | RCallableObjectType | RClassExtends | RClassMixins | RReactKey | RNoProviders
    | RIncompatibleInstantiation _ | ROpaqueType _ | RObjectKeyMirror | RIndexedAccess _
    | RConditionalType | RRendersNothing | RAutocompleteToken | RMatch | RMatchPattern
    | RMatchWildcard ) as r ->
    r
  | RConstructorCall desc -> RConstructorCall (map_desc_locs f desc)
  | RTypeAlias (s, None, d) -> RTypeAlias (s, None, map_desc_locs f d)
  | RTypeAlias (s, Some b, d) -> RTypeAlias (s, Some (f b), map_desc_locs f d)
  | RTypeParam (s, (d1, l1), (d2, l2)) ->
    RTypeParam (s, (map_desc_locs f d1, f l1), (map_desc_locs f d2, f l2))
  | RTypeParamBound r -> RTypeParamBound (map_desc_locs f r)
  | RTypeParamDefault r -> RTypeParamDefault (map_desc_locs f r)
  | RPropertyOf (s, d) -> RPropertyOf (s, map_desc_locs f d)
  | RNameProperty desc -> RNameProperty (map_desc_locs f desc)
  | RPolyType desc -> RPolyType (map_desc_locs f desc)
  | RExactType desc -> RExactType (map_desc_locs f desc)
  | RReadOnlyType -> RReadOnlyType
  | ROptional desc -> ROptional (map_desc_locs f desc)
  | RMaybe desc -> RMaybe (map_desc_locs f desc)
  | RRestArrayLit desc -> RRestArrayLit (map_desc_locs f desc)
  | RTypeApp desc -> RTypeApp (map_desc_locs f desc)
  | RTypeAppImplicit desc -> RTypeAppImplicit (map_desc_locs f desc)
  | RExtends desc -> RExtends (map_desc_locs f desc)
  | RClass desc -> RClass (map_desc_locs f desc)
  | RStatics desc -> RStatics (map_desc_locs f desc)
  | RSuperOf desc -> RSuperOf (map_desc_locs f desc)
  | RFrozen desc -> RFrozen (map_desc_locs f desc)
  | RBound desc -> RBound (map_desc_locs f desc)
  | RRefined desc -> RRefined (map_desc_locs f desc)
  | RRefinedElement desc -> RRefinedElement (map_desc_locs f desc)
  | RPartialOf desc -> RPartialOf (map_desc_locs f desc)
  | RRequiredOf desc -> RRequiredOf (map_desc_locs f desc)
  | RMatchingProp (s, desc) -> RMatchingProp (s, map_desc_locs f desc)
  | RImplicitThis desc -> RImplicitThis (map_desc_locs f desc)
  | ( RObjectPatternRestProp | RArrayPatternRestProp | RModule _ | RNamespace _ | ROptionalChain
    | RReactProps | RReactElement _ | RReactDefaultProps | RReactChildren | RReactRef ) as r ->
    r
  | RReactChildrenOrType desc -> RReactChildrenOrType (map_desc_locs f desc)
  | RReactChildrenOrUndefinedOrType desc -> RReactChildrenOrUndefinedOrType (map_desc_locs f desc)
  | RPossiblyMissingPropFromObj (propname, desc) ->
    RPossiblyMissingPropFromObj (propname, map_desc_locs f desc)
  | RUnionBranching (desc, i) -> RUnionBranching (map_desc_locs f desc, i)
  | RPropsOfComponent desc -> RPropsOfComponent (map_desc_locs f desc)
  | RInstanceOfComponent desc -> RInstanceOfComponent (map_desc_locs f desc)
  | RDefaultTypeArgumentAtIndex { desc_type; desc_default; position } ->
    RDefaultTypeArgumentAtIndex
      {
        desc_type = map_desc_locs f desc_type;
        desc_default = map_desc_locs f desc_default;
        position;
      }
  | RRenderType desc -> RRenderType (map_desc_locs f desc)
  | RRenderMaybeType desc -> RRenderMaybeType (map_desc_locs f desc)
  | RRenderStarType desc -> RRenderStarType (map_desc_locs f desc)
  | REnumMember { enum; member_name } -> REnumMember { enum = map_desc_locs f enum; member_name }
  | REnumUnknownMembers desc -> REnumUnknownMembers (map_desc_locs f desc)

type 'loc virtual_reason = {
  desc: 'loc virtual_reason_desc;
  loc: 'loc;
  def_loc_opt: 'loc option;
  annot_loc_opt: 'loc option;
}
[@@deriving eq, ord]

type reason = ALoc.t virtual_reason [@@deriving eq, ord]

type concrete_reason = Loc.t virtual_reason

type t = reason

let concretize_equal aloc_tables = equal_virtual_reason (ALoc.concretize_equal aloc_tables)

let in_range loc range =
  Loc.(
    let (line, line1, line2) = (loc.start.line, range.start.line, range._end.line) in
    (line1 < line || (line = line1 && range.start.column <= loc.start.column))
    && (line < line2 || (line = line2 && loc._end.column <= range._end.column))
  )

let string_of_source ?(strip_root = None) =
  File_key.(
    function
    | LibFile file -> begin
      match strip_root with
      | Some root ->
        let root_str = spf "%s%s" (File_path.to_string root) Filename.dir_sep in
        if String.starts_with ~prefix:root_str file then
          spf "[LIB] %s" (Files.relative_path root_str file)
        else
          spf "[LIB] %s" (Filename.basename file)
      | None -> file
    end
    | SourceFile file
    | JsonFile file
    | ResourceFile file -> begin
      match strip_root with
      | Some root ->
        let root_str = spf "%s%s" (File_path.to_string root) Filename.dir_sep in
        Files.relative_path root_str file
      | None -> file
    end
  )

let string_of_loc ?(strip_root = None) loc =
  Loc.(
    match loc.source with
    | None -> ""
    | Some file -> spf "%s:%s" (string_of_source ~strip_root file) (Loc.to_string_no_source loc)
  )

let string_of_aloc ?(strip_root = None) aloc =
  match ALoc.source aloc with
  | None -> ""
  | Some file -> spf "%s:%s" (string_of_source ~strip_root file) (ALoc.to_string_no_source aloc)

let json_of_source ?(strip_root = None) =
  Hh_json.(
    function
    | Some x -> JSON_String (string_of_source ~strip_root x)
    | None -> JSON_Null
  )

let json_source_type_of_source =
  Hh_json.(
    function
    | Some (File_key.LibFile _) -> JSON_String "LibFile"
    | Some (File_key.SourceFile _) -> JSON_String "SourceFile"
    | Some (File_key.JsonFile _) -> JSON_String "JsonFile"
    | Some (File_key.ResourceFile _) -> JSON_String "ResourceFile"
    | None -> JSON_Null
  )

let json_of_loc_props ?(strip_root = None) ?(catch_offset_errors = false) ~offset_table loc =
  Hh_json.(
    Loc.(
      let offset_entry offset_table pos =
        let offset =
          try int_ (Offset_utils.offset offset_table pos) with
          | Offset_utils.Offset_lookup_failed _ as exn ->
            if catch_offset_errors then
              JSON_Null
            else
              raise exn
        in
        [("offset", offset)]
      in
      let start =
        [
          ("line", int_ loc.start.line);
          (* It's not ideal that we use a different column numbering system here
           * versus other places (like the estree translator) *)
          ("column", int_ (loc.start.column + 1));
        ]
        @
        match offset_table with
        | None -> []
        | Some table -> offset_entry table loc.start
      in
      let end_ =
        [("line", int_ loc._end.line); ("column", int_ loc._end.column)]
        @
        match offset_table with
        | None -> []
        | Some table -> offset_entry table loc._end
      in
      [
        ("source", json_of_source ~strip_root loc.source);
        ("type", json_source_type_of_source loc.source);
        ("start", JSON_Object start);
        ("end", JSON_Object end_);
      ]
    )
  )

let json_of_loc ?strip_root ?catch_offset_errors ~offset_table loc =
  Hh_json.(JSON_Object (json_of_loc_props ?strip_root ?catch_offset_errors ~offset_table loc))

(* reason constructors, accessors, etc. *)

let mk_reason_internal desc loc def_loc_opt annot_loc_opt =
  { desc; loc; def_loc_opt; annot_loc_opt }

let map_reason_locs f reason =
  let { def_loc_opt; annot_loc_opt; loc; desc } = reason in
  let loc' = f loc in
  let def_loc_opt' = Base.Option.map ~f def_loc_opt in
  let annot_loc_opt' = Base.Option.map ~f annot_loc_opt in
  let desc' = map_desc_locs f desc in
  { def_loc_opt = def_loc_opt'; annot_loc_opt = annot_loc_opt'; loc = loc'; desc = desc' }

let mk_reason desc aloc = mk_reason_internal desc aloc None None

(* Lift a string to a reason. Usually used as a dummy reason. *)
let locationless_reason desc = mk_reason_internal desc ALoc.none None None

let func_reason ~async ~generator =
  let func_desc =
    match (async, generator) with
    | (true, true) -> RAsyncGenerator
    | (true, false) -> RAsync
    | (false, true) -> RGenerator
    | (false, false) -> RNormal
  in
  mk_reason (RFunction func_desc)

let loc_of_reason r = r.loc

let def_loc_of_reason r =
  match r.def_loc_opt with
  | Some loc -> loc
  | None -> loc_of_reason r

let def_loc_opt_of_reason r = r.def_loc_opt

let annot_loc_of_reason r = r.annot_loc_opt

let mk_obj_lit_reason ~as_const ~frozen ~use_unsound_fallback loc =
  let desc =
    if frozen then
      RFrozen RObjectLit
    else if as_const then
      RConstObjectLit
    else if Lazy.force use_unsound_fallback then
      RObjectLit_UNSOUND
    else
      RObjectLit
  in
  mk_reason desc loc

let function_desc_prefix = function
  | RAsync -> "async "
  | RGenerator -> "generator "
  | RAsyncGenerator -> "async generator "
  | RNormal -> ""
  | RUnknown -> "unknown "

let prettify_react_util s =
  let length = String.length s in
  if length < 6 then
    s
  else if String.sub s 0 6 = "React$" then
    "React." ^ String.sub s 6 (length - 6)
  else
    s

let rec string_of_desc = function
  | RNumber -> "number"
  | RBigInt -> "bigint"
  | RString -> "string"
  | RBoolean -> "boolean"
  | RMixed -> "mixed"
  | REmpty -> "empty"
  | REmptyArrayElement -> "unknown element of empty array"
  | RAnyImplicit -> "implicit 'any'"
  | RAnyExplicit -> "explicit 'any'"
  | RVoid -> "undefined"
  | RNull -> "null"
  | RVoidedNull -> "undefined (result of null short-circuiting an optional chain)"
  | RNullOrVoid -> "null or undefined"
  | RSymbol -> "symbol"
  | RExports -> "exports"
  | RStringLit (OrdinaryName "") -> "empty string"
  | RStringLit x -> spf "string literal `%s`" (display_string_of_name x)
  | RStringPrefix { prefix } -> spf "string prefixed with `%s`" prefix
  | RStringWithoutPrefix { prefix } -> spf "string with prefix `%s` removed" prefix
  | RStringSuffix { suffix } -> spf "string suffixed with `%s`" suffix
  | RStringWithoutSuffix { suffix } -> spf "string with suffix `%s` removed" suffix
  | RNumberLit x -> spf "number literal `%s`" x
  | RBigIntLit x -> spf "bigint literal `%s`" x
  | RBooleanLit b -> spf "boolean literal `%s`" (string_of_bool b)
  | RIndexedAccess { optional } ->
    if optional then
      "optional indexed access"
    else
      "indexed access"
  | RConditionalType -> "conditional type"
  | RMatch -> "match"
  | RMatchPattern -> "match pattern"
  | RMatchWildcard -> "match wildcard"
  | RMatchingProp (k, v) -> spf "object with property `%s` that matches %s" k (string_of_desc v)
  | RObject -> "object"
  | RObjectLit
  | RObjectLit_UNSOUND ->
    "object literal"
  | RConstObjectLit -> "const object literal"
  | RObjectType -> "object type"
  | RMappedType -> "mapped type"
  | RInterfaceType -> "interface type"
  | RArray -> "array"
  | RArrayLit -> "array literal"
  | RArrayLit_UNSOUND -> "array literal"
  | RConstArrayLit -> "const array literal"
  | REmptyArrayLit -> "empty array literal"
  | RArrayType -> "array type"
  | RArrayElement -> "array element"
  | RArrayNthElement i -> spf "element %d" i
  | RArrayHole -> "undefined (due to array hole)"
  | RROArrayType -> "read-only array type"
  | RTupleType -> "tuple type"
  | RTupleElement { name } ->
    let suffix =
      match name with
      | Some name -> spf " (labeled '%s')" name
      | None -> ""
    in
    spf "%s%s" "tuple element" suffix
  | RTupleOutOfBoundsAccess i -> spf "undefined (out of bounds tuple access at index %d)" i
  | RTupleUnknownElementFromInexact -> "an unknown element of an inexact tuple type"
  | RTupleLength i -> spf "length `%d` (number) of tuple" i
  | RFunction func -> spf "%sfunction" (function_desc_prefix func)
  | RFunctionType -> "function type"
  | RFunctionBody -> "function body"
  | RFunctionCall d -> spf "call of %s" (string_of_desc d)
  | RFunctionUnusedArgument -> "unused function argument"
  | RJSXChild -> "JSX child"
  | RJSXFunctionCall raw_jsx -> spf "`%s(...)`" raw_jsx
  | RJSXIdentifier (_, name) -> spf "`%s`" name
  | RJSXElement x ->
    (match x with
    | Some x -> spf "JSX element `%s`" x
    | None -> "JSX element")
  | RJSXElementProps _ -> "props"
  | RJSXText -> spf "JSX text"
  | RFbt -> "`<fbt/>`"
  | RUnaryOperator (operator, value) -> spf "%s %s" operator (string_of_desc value)
  | RBinaryOperator (operator, left, right) ->
    spf "%s %s %s" (string_of_desc left) operator (string_of_desc right)
  | RLogical (operator, left, right) ->
    spf "%s %s %s" (string_of_desc left) operator (string_of_desc right)
  | RTemplateString -> "template string"
  | RUnknownString -> "some string with unknown value"
  | RUnionEnum -> "literal union"
  | REnum { name } ->
    (match name with
    | Some name -> spf "enum `%s`" name
    | None -> "enum")
  | REnumMember { enum; member_name } -> spf "member %s of enum %s" member_name (string_of_desc enum)
  | REnumUnknownMembers enum ->
    spf "the unknown members of enum %s (specified using `...`)" (string_of_desc enum)
  | RThis -> "this"
  | RThisType -> "`this` type"
  | RImplicitInstantiation -> "implicit instantiation"
  | RConstructorVoidReturn -> "constructor void return"
  | RUnion -> "union"
  | RUnionType -> "union type"
  | RIntersection -> "intersection"
  | RIntersectionType -> "intersection type"
  | RKeySet -> "key set"
  | RAnd -> "and"
  | RConditional -> "conditional"
  | RPrototype -> "prototype"
  | RObjectPrototype -> "object prototype"
  | RFunctionPrototype -> "function prototype"
  | RDestructuring -> "destructuring"
  | RDefaultValue -> "default value"
  | RConstructor -> "constructor"
  | RDefaultConstructor -> "default constructor"
  | RConstructorCall (RPolyType (RClass d)) -> string_of_desc d
  | RConstructorCall (RClass d) -> string_of_desc d
  | RConstructorCall d -> spf "new %s" (string_of_desc d)
  | RReturn -> "return"
  | RRegExp -> "regexp"
  | RSuper -> "super"
  | RDummyPrototype -> "empty prototype object"
  | RDummyThis -> "bound `this` in method"
  | RImplicitThis desc -> spf "implicit `this` parameter of %s" (string_of_desc desc)
  | RObjectKeyMirror -> "`$KeyMirror`"
  | RType x -> spf "`%s`" (prettify_react_util (display_string_of_name x))
  | RTypeAlias (x, _, _) -> spf "`%s`" (prettify_react_util x)
  | ROpaqueType x -> spf "`%s`" (prettify_react_util x)
  | RTypeParam (x, _, _) -> Subst_name.formatted_string_of_subst_name x
  | RTypeParamDefault r -> spf "%s (inferred from type parameter's default)" (string_of_desc r)
  | RTypeParamBound r -> spf "%s (inferred from type parameter's bound)" (string_of_desc r)
  | RTypeof x -> spf "`typeof %s`" x
  | RMethod (Some x) -> spf "method `%s`" x
  | RMethod None -> "computed method"
  | RIdentifier x -> spf "`%s`" (prettify_react_util (display_string_of_name x))
  | RMethodCall (Some x) -> spf "call of method `%s`" x
  | RMethodCall None -> "call of computed property"
  | RParameter (Some x) -> spf "`%s`" x
  | RParameter None -> "parameter"
  | RRestParameter (Some x) -> spf "rest parameter `%s`" x
  | RRestParameter None -> "rest parameter"
  | RPatternParameter x -> spf "pattern parameter `%s`" x
  | RProperty (Some x) -> spf "property `%s`" (display_string_of_name x)
  | RProperty None -> "computed property"
  | RPrivateProperty x -> spf "property `#%s`" x
  | RMember { object_; property } -> spf "`%s%s`" object_ property
  | RPropertyAssignment (Some x) -> spf "assignment of property `%s`" x
  | RPropertyAssignment None -> "assignment of computed property/element"
  | RPropertyOf (x, d) -> spf "property `%s` of %s" (display_string_of_name x) (string_of_desc d)
  | RPropertyIsAString (OrdinaryName "") -> "empty string"
  | RPropertyIsAString x -> spf "string `%s`" (display_string_of_name x)
  | RMissingProperty (Some x) ->
    spf "`void` (due to access of non-existent property `%s`)" (display_string_of_name x)
  | RMissingProperty None -> "`void` (due to access of a computed property which does not exist)"
  | RUnknownProperty (Some x) -> spf "property `%s` of unknown type" (display_string_of_name x)
  | RUnknownProperty None -> "computed property of unknown type"
  | RUnknownUnspecifiedProperty d ->
    spf "an unknown property that may exist on the inexact %s" (string_of_desc d)
  | RUndefinedProperty x -> spf "undefined property `%s`" (display_string_of_name x)
  | RSomeProperty -> "some property"
  | RNameProperty d -> spf "property `name` of %s" (string_of_desc d)
  | RNamedImportedType (m, _) ->
    spf "Named import from module `%s`" (Flow_import_specifier.display_userland m)
  | RImportStarType n -> spf "import type * as %s" n
  | RImportStarTypeOf n -> spf "import typeof * as %s" n
  | RImportStar n -> spf "import * as %s" n
  | RCode x -> "`" ^ x ^ "`"
  | RDefaultImportedType (_, m) ->
    spf "Default import from `%s`" (Flow_import_specifier.display_userland m)
  | RAsyncImport -> "async import"
  | RCustom x -> x
  | RNonnullAssert -> "!"
  | RMixins -> "mixins"
  | RUnaryMinus -> "unary minus"
  | RUnaryNot -> "unary not"
  | RRest -> "rest"
  | RGlobalObject -> "global object"
  | RProviders -> "providers"
  | RForOfElement -> "for-of element"
  | RUpdate -> "update"
  | RUnusedYield -> "unused yield"
  | RUnusedReturn -> "unused return"
  | RCommonInterface -> "common interface"
  | RContextualVariable -> "contextual variable"
  | RNext -> "next"
  | RModuleReference -> "module reference"
  | RNewFunction -> "new Function(..)"
  | RNewArray -> "new Array(..)"
  | RArrayLength -> "array length"
  | RImportMeta -> "import.meta"
  | RAwait -> "await"
  | RAsyncReturn -> "async return"
  | RCallableObjectType -> "callable object type"
  | RClassExtends -> "class extends"
  | RClassMixins -> "class mixins"
  | RReactKey -> "React key"
  | RNoProviders -> "no providers"
  | RPolyType (RClass d) -> string_of_desc d
  | RPolyType d -> string_of_desc d
  | RExactType d -> string_of_desc d
  | RReadOnlyType -> "`$ReadOnly`"
  | ROptional d -> spf "optional %s" (string_of_desc d)
  | RMaybe d ->
    let rec loop = function
      | RMaybe d -> loop d
      | d -> d
    in
    spf "nullable %s" (string_of_desc (loop d))
  | RRestArrayLit _ -> "rest array"
  | RTypeApp d -> string_of_desc d
  | RTypeAppImplicit d -> string_of_desc d
  | RExtends d -> spf "extends %s" (string_of_desc d)
  | RClass d -> spf "class %s" (string_of_desc d)
  | RStatics d -> spf "statics of %s" (string_of_desc d)
  | RSuperOf d -> spf "super of %s" (string_of_desc d)
  | RFrozen d -> spf "frozen %s" (string_of_desc d)
  | RBound d -> spf "bound %s" (string_of_desc d)
  | RRefined d -> spf "refined %s" (string_of_desc d)
  | RRefinedElement d -> spf "array element of refined %s" (string_of_desc d)
  | RIncompatibleInstantiation x -> Subst_name.formatted_string_of_subst_name x
  | RPartialOf d -> spf "partial %s" (string_of_desc d)
  | RRequiredOf d -> spf "required of %s" (string_of_desc d)
  | RObjectPatternRestProp -> "rest of object pattern"
  | RArrayPatternRestProp -> "rest of array pattern"
  | RModule x -> spf "module `%s`" (Flow_import_specifier.display_userland x)
  | RNamespace x -> spf "namespace %s" x
  | ROptionalChain -> "optional chain"
  | RReactProps -> "props"
  | RReactElement { name_opt; from_component_syntax = _ } ->
    (match name_opt with
    | Some x -> spf "`%s` element" (display_string_of_name x)
    | None -> "React element")
  | RReactDefaultProps -> "default props of React component"
  | RReactChildren -> "children array"
  | RReactChildrenOrType desc -> spf "children array or %s" (string_of_desc desc)
  | RReactChildrenOrUndefinedOrType desc -> spf "children array or %s" (string_of_desc desc)
  | RReactRef -> "React component ref"
  | RPossiblyMissingPropFromObj (propname, desc) ->
    spf
      "possibly missing property `%s` in %s"
      (display_string_of_name propname)
      (string_of_desc desc)
  | RUnionBranching (desc, _) -> string_of_desc desc
  | RUninitialized -> "uninitialized variable"
  | RPossiblyUninitialized -> "possibly uninitialized variable"
  | RUnannotatedNext -> "undefined (default `next` of unannotated generator function)"
  | RTypeGuard -> "type guard"
  | RTypeGuardParam s -> spf "type guard parameter `%s`" s
  | RComponent name -> spf "component %s" (display_string_of_name name)
  | RComponentType -> "component"
  | RPropsOfComponent desc -> spf "props of %s" (string_of_desc desc)
  | RInstanceOfComponent desc -> spf "instance of %s" (string_of_desc desc)
  | RDefaultTypeArgumentAtIndex { desc_type; desc_default; position } ->
    let position_suffix =
      match position with
      | 11 -> "th"
      | 12 -> "th"
      | 13 -> "th"
      | _ ->
        (match position mod 10 with
        | 1 -> "st"
        | 2 -> "nd"
        | 3 -> "rd"
        | _ -> "th")
    in
    spf
      "%s (default type argument for %s's %i%s position)"
      (string_of_desc desc_default)
      (string_of_desc desc_type)
      position
      position_suffix
  | RRenderType desc -> spf "renders %s" (string_of_desc desc)
  | RRenderMaybeType desc -> spf "renders? %s" (string_of_desc desc)
  | RRenderStarType desc -> spf "renders* %s" (string_of_desc desc)
  | RRendersNothing -> "a value that renders nothing"
  | RInferredUnionElemArray _ ->
    "inferred union of array element types "
    ^ "(alternatively, provide an annotation to summarize the array element type)"
  | RAutocompleteToken -> "autocomplete token"

let string_of_reason ?(strip_root = None) r =
  let spos = string_of_aloc ~strip_root (loc_of_reason r) in
  let desc = string_of_desc r.desc in
  if spos = "" then
    desc
  else if desc = "" then
    spos
  else
    spf "%s: %s" spos desc

let dump_reason ?(strip_root = None) r =
  spf "%s: %S" (string_of_aloc ~strip_root (loc_of_reason r)) (string_of_desc r.desc)

let rec unwrap_reason_desc = function
  | RTypeAlias (_, _, desc)
  | RUnionBranching (desc, _) ->
    unwrap_reason_desc desc
  | desc -> desc

let desc_of_reason ?(unwrap = true) r =
  if not unwrap then
    r.desc
  else
    unwrap_reason_desc r.desc

(* Instantiable reasons identify tvars that are created for the purpose of
   instantiation: they are fresh rather than shared, and should become types
   that flow to them. We assume these characteristics when performing
   speculative matching (even though we don't yet enforce them). *)
let is_instantiable_reason r =
  match desc_of_reason r with
  | RTypeParam _
  | RThisType ->
    true
  | RImplicitInstantiation -> true
  | RInferredUnionElemArray { instantiable; is_empty = _ } -> instantiable
  | _ -> false

let is_literal_object_reason r =
  match desc_of_reason r with
  | RObjectLit_UNSOUND
  | RObjectPatternRestProp
  | RFunction _
  | RStatics (RFunction _)
  | RReactProps
  | RReactElement _
  | RJSXElementProps _ ->
    true
  | _ -> false

let is_literal_array_reason r =
  match desc_of_reason r with
  | RArrayLit_UNSOUND
  | REmptyArrayLit
  | RRestArrayLit _
  | RReactChildren
  | RArrayPatternRestProp ->
    true
  | _ -> false

let is_literal_function_reason r =
  match desc_of_reason r with
  | RFunction _ -> true
  | _ -> false

let is_lib_reason r =
  r.loc |> ALoc.source |> Base.Option.value_map ~default:false ~f:File_key.is_lib_file

let is_lib_reason_def r =
  def_loc_of_reason r |> ALoc.source |> Base.Option.value_map ~default:false ~f:File_key.is_lib_file

let is_blamable_reason r = not (r.loc = ALoc.none || is_lib_reason r)

let is_promise_reason r =
  is_lib_reason r
  &&
  match desc_of_reason r with
  | RClass (RType (OrdinaryName "Promise")) -> true
  | _ -> false

(* reason transformers: *)

(* returns reason with new description and position of original *)
let update_desc_reason f r =
  mk_reason_internal
    (f (desc_of_reason ~unwrap:false r))
    (loc_of_reason r)
    r.def_loc_opt
    (annot_loc_of_reason r)

let update_desc_new_reason f r =
  mk_reason_internal (f (desc_of_reason ~unwrap:false r)) (loc_of_reason r) None None

let replace_desc_reason desc r = mk_reason_internal desc r.loc r.def_loc_opt r.annot_loc_opt

let replace_desc_new_reason desc r = mk_reason_internal desc r.loc None None

(* returns reason with new location and description of original *)
let repos_reason loc reason =
  let def_aloc_opt =
    let def_loc = def_loc_of_reason reason in
    if loc = def_loc then
      None
    else
      Some def_loc
  in
  mk_reason_internal reason.desc loc def_aloc_opt reason.annot_loc_opt

let annot_reason ~annot_loc reason = { reason with annot_loc_opt = Some annot_loc }

let opt_annot_reason ?annot_loc reason =
  match annot_loc with
  | None -> reason
  | Some annot_loc -> annot_reason ~annot_loc reason

let mk_annot_reason desc annot_loc = annot_reason ~annot_loc (mk_reason desc annot_loc)

module ReasonMap = WrappedMap.Make (struct
  type t = reason

  let compare = Stdlib.compare
end)

(* Creates a description string for an arbitrary expression. This description
 * will be used to describe some code in error messages which are designed to be
 * human readable.
 *
 * We want to keep these descriptions *short* so we omit a lot of information in
 * places where expressions may often go recursive. For instance, object and
 * array literals are abbreviated as [...] and {...} respectively.
 *
 * The wrap argument provides a rough heuristic for when wrapping is necessary.
 * We set wrap to true when we need to append something to the final expression.
 * Then expressions which need to be wrapped will call do_wrap. e.g.
 *
 *     (1 + 2).p
 *     o.p
 *     o[1 + 2]
 *
 * In the first example we need to wrap 1 + 2 to correctly print the property
 * access. However, we don't need to wrap o in o.p. In o[1 + 2] we don't need to
 * wrap 1 + 2 since it is already wrapped in a sense. *)
let rec code_desc_of_expression : 'loc 'tloc. wrap:bool -> ('loc, 'tloc) Ast.Expression.t -> string
    =
 fun ~wrap (_, x) ->
  let do_wrap =
    if wrap then
      fun s ->
    "(" ^ s ^ ")"
    else
      fun s ->
    s
  in
  let open Ast.Expression in
  match x with
  | Array { Array.elements = []; _ } -> "[]"
  | Array _ -> "[...]"
  | ArrowFunction { Ast.Function.body = Ast.Function.BodyExpression ((_, Object _) as e); _ } ->
    do_wrap ("(...) => (" ^ code_desc_of_expression ~wrap:false e ^ ")")
  | ArrowFunction { Ast.Function.body = Ast.Function.BodyExpression e; _ } ->
    do_wrap ("(...) => " ^ code_desc_of_expression ~wrap:false e)
  | ArrowFunction _ -> do_wrap "(...) => { ... }"
  | AsConstExpression { AsConstExpression.expression; _ } ->
    code_desc_of_expression ~wrap expression
  | AsExpression { AsExpression.expression; _ } -> code_desc_of_expression ~wrap expression
  | Assignment { Assignment.left; operator; right; comments = _ } ->
    let left = code_desc_of_pattern left in
    let right = code_desc_of_expression ~wrap:false right in
    let operator =
      match operator with
      | None -> "="
      | Some op -> Flow_ast_utils.string_of_assignment_operator op
    in
    do_wrap (left ^ " " ^ operator ^ " " ^ right)
  | Binary { Binary.operator; left; right; comments = _ } ->
    do_wrap (code_desc_of_operation left (`Binary operator) right)
  | Call { Call.callee; targs; arguments; comments = _ } ->
    let targs =
      match targs with
      | None -> ""
      | Some (_, { CallTypeArgs.arguments = []; comments = _ }) -> "<>"
      | Some (_, { CallTypeArgs.arguments = _ :: _; comments = _ }) -> "<...>"
    in
    let args =
      match arguments with
      | (_loc, { ArgList.arguments = []; comments = _ }) -> "()"
      | (_loc, { ArgList.arguments = _ :: _; comments = _ }) -> "(...)"
    in
    code_desc_of_expression ~wrap:true callee ^ targs ^ args
  | Class _ -> "class { ... }"
  | Conditional { Conditional.test; consequent; alternate; comments = _ } ->
    let wrap_test =
      match test with
      | (_, Conditional _) -> true
      | _ -> false
    in
    do_wrap
      (code_desc_of_expression ~wrap:wrap_test test
      ^ " ? "
      ^ code_desc_of_expression ~wrap:false consequent
      ^ " : "
      ^ code_desc_of_expression ~wrap:false alternate
      )
  | Function _ -> "function () { ... }"
  | Identifier (_, { Ast.Identifier.name = x; comments = _ }) -> x
  | Import { Import.argument; comments = _ } ->
    "import(" ^ code_desc_of_expression ~wrap:false argument ^ ")"
  | JSXElement x -> code_desc_of_jsx_element x
  | JSXFragment _ -> "<>...</>"
  | StringLiteral { Ast.StringLiteral.value; _ } when String.length value > 16 ->
    "'" ^ String.sub value 0 10 ^ "...'"
  | StringLiteral { Ast.StringLiteral.raw; _ } -> raw
  | NumberLiteral { Ast.NumberLiteral.raw; _ } -> raw
  | BooleanLiteral { Ast.BooleanLiteral.value; _ } ->
    if value then
      "true"
    else
      "false"
  | NullLiteral _ -> "null"
  | BigIntLiteral { Ast.BigIntLiteral.raw; _ } -> raw
  | RegExpLiteral { Ast.RegExpLiteral.raw; _ } -> raw
  | ModuleRefLiteral { Ast.ModuleRefLiteral.raw; _ } -> raw
  | Logical { Logical.operator; left; right; comments = _ } ->
    do_wrap (code_desc_of_operation left (`Logical operator) right)
  | Match _ -> "match"
  | Member { Member._object; property; comments = _ } ->
    let o = code_desc_of_expression ~wrap:true _object in
    let p = code_desc_of_property property in
    o ^ p
  | MetaProperty
      {
        MetaProperty.meta = (_, { Ast.Identifier.name = o; comments = _ });
        property = (_, { Ast.Identifier.name = p; comments = _ });
        comments = _;
      } ->
    o ^ "." ^ p
  | New { New.callee; targs; arguments; comments = _ } ->
    let targs =
      match targs with
      | None -> ""
      | Some (_, { CallTypeArgs.arguments = []; comments = _ }) -> "<>"
      | Some (_, { CallTypeArgs.arguments = _ :: _; comments = _ }) -> "<...>"
    in
    let args =
      match arguments with
      | None -> ""
      | Some (_loc, { ArgList.arguments = []; comments = _ }) -> "()"
      | Some (_loc, { ArgList.arguments = _ :: _; comments = _ }) -> "(...)"
    in
    do_wrap ("new " ^ code_desc_of_expression ~wrap:true callee ^ targs ^ args)
  | Object _ -> "{...}"
  | OptionalCall
      {
        OptionalCall.call = { Call.callee; targs; arguments; comments = _ };
        optional;
        filtered_out = _;
      } ->
    let targ_string =
      match targs with
      | None -> ""
      | Some (_, { CallTypeArgs.arguments = []; comments = _ }) -> "<>"
      | Some (_, { CallTypeArgs.arguments = _ :: _; comments = _ }) -> "<...>"
    in
    let arg_string =
      match arguments with
      | (_loc, { ArgList.arguments = []; comments = _ }) -> "()"
      | (_loc, { ArgList.arguments = _; comments = _ }) -> "(...)"
    in
    code_desc_of_expression ~wrap:true callee
    ^ (match optional with
      | OptionalCall.NonOptional -> ""
      | OptionalCall.Optional -> "?."
      | OptionalCall.AssertNonnull -> "!")
    ^ targ_string
    ^ arg_string
  | OptionalMember
      {
        OptionalMember.member = { Member._object; property; comments = _ };
        optional;
        filtered_out = _;
      } ->
    let o = code_desc_of_expression ~wrap:true _object in
    let p = code_desc_of_property ~optional property in
    o ^ p
  | Sequence { Sequence.expressions; comments = _ } ->
    code_desc_of_expression ~wrap (List.hd (List.rev expressions))
  | Super _ -> "super"
  | TaggedTemplate { TaggedTemplate.tag; _ } -> code_desc_of_expression ~wrap:true tag ^ "`...`"
  | TemplateLiteral _ -> "`...`"
  | This _ -> "this"
  | TSSatisfies { TSSatisfies.expression; _ }
  | TypeCast { TypeCast.expression; _ } ->
    code_desc_of_expression ~wrap expression
  | Unary { Unary.operator; argument; comments = _ } ->
    let x = code_desc_of_expression ~wrap:true argument in
    let (l, r) =
      Unary.(
        match operator with
        | Minus -> ("-", x)
        | Plus -> ("+", x)
        | Not -> ("!", x)
        | BitNot -> ("~", x)
        | Typeof -> ("typeof ", x)
        | Void -> ("void ", x)
        | Delete -> ("delete ", x)
        | Await -> ("await ", x)
        | Nonnull -> (x, "!")
      )
    in
    do_wrap (l ^ r)
  | Update { Update.operator; prefix; argument; comments = _ } ->
    let x = code_desc_of_expression ~wrap:true argument in
    let op =
      Update.(
        match operator with
        | Increment -> "++"
        | Decrement -> "--"
      )
    in
    do_wrap
      ( if prefix then
        op ^ x
      else
        x ^ op
      )
  | Yield { Yield.argument = Some x; delegate = false; _ } ->
    do_wrap ("yield " ^ code_desc_of_expression ~wrap:false x)
  | Yield { Yield.argument = Some x; delegate = true; _ } ->
    do_wrap ("yield* " ^ code_desc_of_expression ~wrap:false x)
  | Yield { Yield.argument = None; delegate = false; _ } -> "yield"
  | Yield { Yield.argument = None; delegate = true; _ } -> "yield*"

and code_desc_of_pattern (_, x) =
  let open Ast.Pattern in
  match x with
  | Object _ -> "{...}"
  | Array _ -> "[...]"
  | Identifier { Identifier.name = (_, { Ast.Identifier.name; comments = _ }); _ } -> name
  | Expression x -> code_desc_of_expression ~wrap:false x

(* Implementation of operator flattening logic lifted from Prettier:
 * https://github.com/prettier/prettier/blob/dd78f31aaf5b4522b780f13194d57308e5fdf53b/src/common/util.js#L328-L399 *)
and code_desc_of_operation =
  let open Ast.Expression in
  let string_of_operator = function
    | `Binary op -> Flow_ast_utils.string_of_binary_operator op
    | `Logical op ->
      (match op with
      | Logical.Or -> "||"
      | Logical.And -> "&&"
      | Logical.NullishCoalesce -> "??")
  in
  let should_flatten =
    Binary.(
      let precedence = function
        | `Logical Logical.Or -> 0
        | `Logical Logical.NullishCoalesce -> 0
        | `Logical Logical.And -> 1
        | `Binary BitOr -> 2
        | `Binary Xor -> 3
        | `Binary BitAnd -> 4
        | `Binary (Equal | NotEqual | StrictEqual | StrictNotEqual) -> 5
        | `Binary (LessThan | LessThanEqual | GreaterThan | GreaterThanEqual | In | Instanceof) -> 6
        | `Binary (LShift | RShift | RShift3) -> 7
        | `Binary (Plus | Minus) -> 8
        | `Binary (Mult | Div | Mod) -> 9
        | `Binary Exp -> 10
      in
      let equality = function
        | `Binary (Equal | NotEqual | StrictEqual | StrictNotEqual) -> true
        | _ -> false
      in
      let multiplicative = function
        | `Binary (Mult | Div | Mod) -> true
        | _ -> false
      in
      let bitshift = function
        | `Binary (LShift | RShift | RShift3) -> true
        | _ -> false
      in
      fun a b ->
        if precedence a <> precedence b then
          false
        else if a = `Binary Exp then
          false
        else if equality a && equality b then
          false
        else if (a = `Binary Mod && multiplicative b) || (b = `Binary Mod && multiplicative a) then
          false
        else if bitshift a && bitshift b then
          false
        else
          true
    )
  in
  fun left op right ->
    let wrap_left =
      match left with
      | (_, Binary { Binary.operator; _ }) -> not (should_flatten op (`Binary operator))
      | (_, Logical { Logical.operator; _ }) -> not (should_flatten op (`Logical operator))
      | _ -> true
    in
    let left = code_desc_of_expression ~wrap:wrap_left left in
    let right = code_desc_of_expression ~wrap:true right in
    let op = string_of_operator op in
    left ^ " " ^ op ^ " " ^ right

and code_desc_of_jsx_element x =
  let open Ast.JSX in
  match (snd x.opening_element).Opening.name with
  | Identifier (_, { Identifier.name; comments = _ }) -> "<" ^ name ^ " />"
  | NamespacedName
      ( _,
        {
          NamespacedName.namespace = (_, { Identifier.name = a; comments = __POS_OF__ });
          name = (_, { Identifier.name = b; comments = _ });
        }
      ) ->
    "<" ^ a ^ ":" ^ b ^ " />"
  | MemberExpression x ->
    let rec loop = function
      | ( _,
          {
            MemberExpression._object =
              MemberExpression.Identifier (_, { Identifier.name = a; comments = _ });
            property = (_, { Identifier.name = b; comments = _ });
          }
        ) ->
        a ^ "." ^ b
      | ( _,
          {
            MemberExpression._object = MemberExpression.MemberExpression a;
            property = (_, { Identifier.name = b; comments = _ });
          }
        ) ->
        loop a ^ "." ^ b
    in
    "<" ^ loop x ^ " />"

and code_desc_of_property ?(optional = Ast.Expression.OptionalMember.NonOptional) property =
  match property with
  | Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name = x; comments = _ }) ->
    (match optional with
    | Ast.Expression.OptionalMember.Optional -> "?."
    | Ast.Expression.OptionalMember.NonOptional -> "."
    | Ast.Expression.OptionalMember.AssertNonnull -> "!.")
    ^ x
  | Ast.Expression.Member.PropertyPrivateName (_, { Ast.PrivateName.name = x; comments = _ }) ->
    (match optional with
    | Ast.Expression.OptionalMember.Optional -> "?.#"
    | Ast.Expression.OptionalMember.NonOptional -> ".#"
    | Ast.Expression.OptionalMember.AssertNonnull -> "!.#")
    ^ x
  | Ast.Expression.Member.PropertyExpression x ->
    (match optional with
    | Ast.Expression.OptionalMember.Optional -> "?.["
    | Ast.Expression.OptionalMember.NonOptional -> "["
    | Ast.Expression.OptionalMember.AssertNonnull -> "![")
    ^ code_desc_of_expression ~wrap:false x
    ^ "]"

let rec mk_generic_expression_reason :
          'a 'b. f:('b -> 'a) -> ('a, 'b) Ast.Expression.t -> 'a virtual_reason =
  let open Ast.Expression in
  fun ~f -> function
    | (loc, TypeCast { TypeCast.expression; _ }) ->
      repos_reason (f loc) (mk_generic_expression_reason ~f expression)
    | (loc, Object _) -> f loc |> mk_reason RObjectLit_UNSOUND
    | (loc, Array _) -> f loc |> mk_reason RArrayLit
    | (loc, ArrowFunction { Ast.Function.async; _ }) -> f loc |> func_reason ~async ~generator:false
    | (loc, Function { Ast.Function.async; generator; _ }) -> f loc |> func_reason ~async ~generator
    | (loc, StringLiteral { Ast.StringLiteral.value = ""; _ }) ->
      f loc |> mk_reason (RStringLit (OrdinaryName ""))
    | (loc, TaggedTemplate _)
    | (loc, TemplateLiteral _) ->
      f loc |> mk_reason RTemplateString
    | (loc, Member { Member._object; property; comments = _ }) ->
      f loc
      |> mk_reason
           (RMember
              {
                object_ = code_desc_of_expression ~wrap:true _object;
                property = code_desc_of_property property;
              }
           )
    | (loc, _) as x -> f loc |> mk_reason (RCode (code_desc_of_expression ~wrap:false x))

let mk_expression_reason : ('a, 'a) Flow_ast.Expression.t -> 'a virtual_reason =
 (fun t -> mk_generic_expression_reason ~f:Base.Fn.id t)

let mk_typed_expression_reason t = mk_generic_expression_reason ~f:fst t

let mk_initial_arguments_reason =
  let open Ast.Expression in
  let rec helper = function
    | [] -> []
    | Expression x :: args -> mk_expression_reason x :: helper args
    | Spread _ :: _ -> []
  in
  (fun (_args_loc, { Ast.Expression.ArgList.arguments; comments = _ }) -> helper arguments)

let mk_pattern_reason ((loc, _) as patt) = mk_reason (RCode (code_desc_of_pattern patt)) loc

(* Classifies a reason description. These classifications can be used to
 * implement various asthetic behaviors in error messages when we would like to
 * distinguish between different error "classes".
 *
 * The classifications we currently support:
 *
 * - `Scalar: The type *cannot* recursively hold any other types. For example,
 *   number is a scalar but an object like {p: number} is not.
 * - `Nullish: The type is null or undefined. Nullish types are also `Scalar.
 * - `Array: The type is an array. This depends on Flow's custom implementation
 *   of arrays and tuples.
 * - `Unclassified: Everything else which hasn't been classified yet.
 *)
let classification_of_reason_desc desc =
  match unwrap_reason_desc desc with
  | RNumber
  | RBigInt
  | RString
  | RSymbol
  | RBoolean
  | RStringLit _
  | RStringPrefix _
  | RStringWithoutPrefix _
  | RStringSuffix _
  | RStringWithoutSuffix _
  | RNumberLit _
  | RBigIntLit _
  | RBooleanLit _
  | RJSXText
  | RFbt
  | RTemplateString
  | RUnknownString
  | RUnionEnum
  | RKeySet
  | RRegExp ->
    `Scalar
  | RVoid
  | RNull
  | RVoidedNull
  | RUninitialized
  | RPossiblyUninitialized
  | RNullOrVoid ->
    `Nullish
  | RArray
  | RArrayLit
  | RArrayLit_UNSOUND
  | RConstArrayLit
  | REmptyArrayLit
  | RArrayType
  | RROArrayType
  | RTupleType
  | RRestArrayLit _
  | RArrayPatternRestProp ->
    `Array
  | RMixed
  | REmpty
  | REmptyArrayElement
  | RAnyExplicit
  | RAnyImplicit
  | RArrayElement
  | RArrayNthElement _
  | RArrayHole
  | RInferredUnionElemArray _
  | RIndexedAccess _
  | RConditionalType
  | RMatch
  | RMatchPattern
  | RMatchWildcard
  | RMatchingProp _
  | RObject
  | RObjectLit
  | RObjectLit_UNSOUND
  | RConstObjectLit
  | RObjectType
  | RMappedType
  | RInterfaceType
  | RTupleElement _
  | RTupleLength _
  | RTupleOutOfBoundsAccess _
  | RTupleUnknownElementFromInexact
  | RComponent _
  | RComponentType
  | RPropsOfComponent _
  | RRenderType _
  | RRenderMaybeType _
  | RRenderStarType _
  | RRendersNothing
  | RInstanceOfComponent _
  | RDefaultTypeArgumentAtIndex _
  | RFunction _
  | RFunctionType
  | RFunctionBody
  | RFunctionCall _
  | RFunctionUnusedArgument
  | RJSXChild
  | RJSXFunctionCall _
  | RJSXIdentifier _
  | RJSXElementProps _
  | RJSXElement _
  | RUnaryOperator _
  | RBinaryOperator _
  | RLogical _
  | RThis
  | RThisType
  | RImplicitInstantiation
  | RConstructorVoidReturn
  | RUnion
  | RUnionType
  | RIntersection
  | RIntersectionType
  | RAnd
  | RConditional
  | RPrototype
  | RObjectPrototype
  | RFunctionPrototype
  | RDestructuring
  | RDefaultValue
  | RConstructor
  | RDefaultConstructor
  | RConstructorCall _
  | RReturn
  | RSuper
  | RDummyPrototype
  | RDummyThis
  | RImplicitThis _
  | RObjectKeyMirror
  | RType _
  | RTypeAlias _
  | ROpaqueType _
  | RTypeParam _
  | RTypeParamBound _
  | RTypeParamDefault _
  | RTypeof _
  | RMethod _
  | RMethodCall _
  | RParameter _
  | RRestParameter _
  | RPatternParameter _
  | RIdentifier _
  | RPropertyAssignment _
  | RProperty _
  | RPrivateProperty _
  | RMember _
  | RPropertyOf _
  | RPropertyIsAString _
  | RMissingProperty _
  | RUnknownProperty _
  | RUnknownUnspecifiedProperty _
  | RUndefinedProperty _
  | RSomeProperty
  | RNameProperty _
  | RNamedImportedType _
  | RImportStarType _
  | RImportStarTypeOf _
  | RImportStar _
  | RDefaultImportedType _
  | RAsyncImport
  | RCode _
  | RCustom _
  | RNonnullAssert
  | RMixins
  | RUnaryMinus
  | RUnaryNot
  | RRest
  | RGlobalObject
  | RProviders
  | RForOfElement
  | RUpdate
  | RUnusedYield
  | RUnusedReturn
  | RCommonInterface
  | RContextualVariable
  | RNext
  | RModuleReference
  | RNewFunction
  | RNewArray
  | RArrayLength
  | RImportMeta
  | RAwait
  | RAsyncReturn
  | RCallableObjectType
  | RClassExtends
  | RClassMixins
  | RReactKey
  | RNoProviders
  | RExports
  | RPolyType _
  | RExactType _
  | RReadOnlyType
  | ROptional _
  | RMaybe _
  | RTypeApp _
  | RTypeAppImplicit _
  | RExtends _
  | RClass _
  | RStatics _
  | RSuperOf _
  | RFrozen _
  | RBound _
  | RRefined _
  | RRefinedElement _
  | RIncompatibleInstantiation _
  | RPartialOf _
  | RRequiredOf _
  | RObjectPatternRestProp
  | RModule _
  | RNamespace _
  | ROptionalChain
  | RReactProps
  | RReactElement _
  | RReactDefaultProps
  | RReactChildren
  | RReactChildrenOrType _
  | RReactChildrenOrUndefinedOrType _
  | RReactRef
  | RPossiblyMissingPropFromObj _
  | RUnionBranching _
  | REnum _
  | REnumMember _
  | REnumUnknownMembers _
  | RUnannotatedNext
  | RTypeGuard
  | RTypeGuardParam _
  | RAutocompleteToken ->
    `Unclassified

let is_nullish_reason r = classification_of_reason_desc r.desc = `Nullish

let is_scalar_reason_desc desc =
  let c = classification_of_reason_desc desc in
  c = `Scalar || c = `Nullish

let is_scalar_reason r = is_scalar_reason_desc r.desc

let is_array_reason r = classification_of_reason_desc r.desc = `Array

let invalidate_rtype_alias = function
  | RTypeAlias (name, Some _, desc) -> RTypeAlias (name, None, desc)
  | desc -> desc

let react_element_desc_of_component_reason reason =
  match desc_of_reason reason with
  | RComponent name -> RReactElement { name_opt = Some name; from_component_syntax = true }
  | _ -> RReactElement { name_opt = None; from_component_syntax = false }

let range_string_of_loc ~strip_root loc =
  Loc.(
    let file =
      match loc.source with
      | Some file -> string_of_source ~strip_root file
      | None -> ""
    in
    let (l0, c0) = (loc.start.line, loc.start.column + 1) in
    let (l1, c1) = (loc._end.line, loc._end.column) in
    spf "%s:%d:%d,%d:%d" file l0 c0 l1 c1
  )

module ReasonSet = Flow_set.Make (struct
  type t = reason

  let compare = compare_reason
end)
