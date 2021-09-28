(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
open String_utils

type name =
  | OrdinaryName of string
  | InternalName of string
  | InternalModuleName of string
[@@deriving eq, ord, show]

let display_string_of_name = function
  | OrdinaryName x -> x
  | InternalName x -> spf ".%s" x
  | InternalModuleName x -> spf ".$module__%s" x

let mk_id () = Ident.make ""

(* Reasons are included in types mainly for error reporting, but sometimes we
   also use reasons in types to recover information on the source code that
   caused those reasons to be created. Two examples of such secondary uses of
   reasons are:

   - strictness analysis: we use reasons to locate the origin of an object and
   the origin of an operation on the object, and use such origins to determine
   whether certain errors should be suppressed.

   - termination analysis: we use reasons to limit instantiation of type
   parameters in polymorphic types at particular locations, to prevent the type
   checker from generating an unbounded number of constraints. The `pos` field
   of reasons is sufficient to distinguish code locations, except that as an
   implementation strategy for checking polymorphic definitions, we walk over
   the same source code multiple times to check it with different instantiations
   of type parameters, and to index "copies" of the reasons created in those
   passes over the same source code, we use an additional `test_id` field.
*)
module TestID = struct
  let _current = ref None

  (* Get current test id. *)
  let current () = !_current

  (* Call f on a, installing new_test_id as the current test_id, and restoring
     the current test_id when done. (See also the function mk_reason below.) *)
  let run f a =
    let test_id = current () in
    _current := Some (mk_id ());
    let b = f a in
    _current := test_id;
    b
end

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
  (* TODO String literals should not be able to include internal names *)
  | RStringLit of name
  | RNumberLit of string
  | RBigIntLit of string
  | RBooleanLit of bool
  | RIndexedAccess of { optional: bool }
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
  | REnum of string (* name *)
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
  | RImplicitThis of 'loc virtual_reason_desc
  | RTupleMap
  | RObjectMap
  | RObjectMapi
  (* TODO type names should not be able to be internal names *)
  | RType of name
  | RTypeAlias of string * 'loc option (* reliable def loc *) * 'loc virtual_reason_desc
  | ROpaqueType of string
  | RTypeParam of
      string * ('loc virtual_reason_desc * 'loc) * (*reason op *)
      ('loc virtual_reason_desc * 'loc) (* reason tapp *)
  | RTypeof of string
  | RMethod of string option
  | RMethodCall of string option
  | RParameter of string option
  | RRestParameter of string option
  | RIdentifier of name
  | RIdentifierAssignment of string
  | RPropertyAssignment of string option
  | RProperty of name option
  | RPrivateProperty of string
  | RShadowProperty of name
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
  | RMissingAbstract of 'loc virtual_reason_desc
  | RFieldInitializer of string
  | RUntypedModule of string
  | RNamedImportedType of string (* module *) * string (* local name *)
  | RImportStarType of string
  | RImportStarTypeOf of string
  | RImportStar of string
  | RDefaultImportedType of string * string
  | RAsyncImport
  | RCode of string
  | RCustom of string
  | RPolyType of 'loc virtual_reason_desc
  | RExactType of 'loc virtual_reason_desc
  | RReadOnlyType
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
  | RRefinedElement of 'loc virtual_reason_desc
  | RIncompatibleInstantiation of string
  | RSpreadOf of 'loc virtual_reason_desc
  | RShapeOf of 'loc virtual_reason_desc
  | RPartialOf of 'loc virtual_reason_desc
  | RObjectPatternRestProp
  | RArrayPatternRestProp
  | RCommonJSExports of string
  | RModule of name
  | ROptionalChain
  | RReactProps
  (* TODO React element names should not allow internal names *)
  | RReactElement of name option
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
  | RPossiblyMissingPropFromObj of name * 'loc virtual_reason_desc
  | RWidenedObjProp of 'loc virtual_reason_desc
  | RUnionBranching of 'loc virtual_reason_desc * int
  | RUninitialized
  | RPossiblyUninitialized
[@@deriving eq, show]

and reason_desc_function =
  | RAsync
  | RGenerator
  | RAsyncGenerator
  | RNormal
  | RUnknown

type reason_desc = ALoc.t virtual_reason_desc

let rec map_desc_locs f = function
  | ( RAnyExplicit | RAnyImplicit | RNumber | RBigInt | RString | RBoolean | RMixed | REmpty | RVoid
    | RNull | RVoidedNull | RSymbol | RExports | RNullOrVoid | RLongStringLit _ | RStringLit _
    | RNumberLit _ | RBigIntLit _ | RBooleanLit _ | RObject | RObjectLit | RObjectType
    | RObjectClassName | RInterfaceType | RArray | RArrayLit | REmptyArrayLit | RArrayType
    | RROArrayType | RTupleType | RTupleElement | RTupleLength _ | RTupleOutOfBoundsAccess
    | RFunction _ | RFunctionType | RFunctionBody | RFunctionCallType | RFunctionUnusedArgument
    | RJSXFunctionCall _ | RJSXIdentifier _ | RJSXElementProps _ | RJSXElement _ | RJSXText | RFbt
    | RUninitialized | RPossiblyUninitialized ) as r ->
    r
  | RFunctionCall desc -> RFunctionCall (map_desc_locs f desc)
  | RUnknownUnspecifiedProperty desc -> RUnknownUnspecifiedProperty (map_desc_locs f desc)
  | RUnaryOperator (s, desc) -> RUnaryOperator (s, map_desc_locs f desc)
  | RBinaryOperator (s, d1, d2) -> RBinaryOperator (s, map_desc_locs f d1, map_desc_locs f d2)
  | RLogical (s, d1, d2) -> RLogical (s, map_desc_locs f d1, map_desc_locs f d2)
  | ( RTemplateString | RUnknownString | RUnionEnum | REnum _ | RGetterSetterProperty | RThis
    | RThisType | RExistential | RImplicitInstantiation | RTooFewArgs | RTooFewArgsExpectedRest
    | RConstructorReturn | RNewObject | RUnion | RUnionType | RIntersection | RIntersectionType
    | RKeySet | RAnd | RConditional | RPrototype | RObjectPrototype | RFunctionPrototype
    | RDestructuring | RDefaultValue | RConstructor | RReturn | RDefaultConstructor | RRegExp
    | RSuper | RNoSuper | RDummyPrototype | RDummyThis | RTupleMap | RObjectMap | RType _
    | RTypeof _ | RMethod _ | RMethodCall _ | RParameter _ | RRestParameter _ | RIdentifier _
    | RIdentifierAssignment _ | RPropertyAssignment _ | RProperty _ | RPrivateProperty _
    | RShadowProperty _ | RMember _ | RPropertyIsAString _ | RMissingProperty _ | RUnknownProperty _
    | RUndefinedProperty _ | RSomeProperty | RFieldInitializer _ | RUntypedModule _
    | RNamedImportedType _ | RImportStarType _ | RImportStarTypeOf _ | RImportStar _
    | RDefaultImportedType _ | RAsyncImport | RCode _ | RCustom _ | RIncompatibleInstantiation _
    | ROpaqueType _ | RObjectMapi | RIndexedAccess _ ) as r ->
    r
  | REnumRepresentation desc -> REnumRepresentation (map_desc_locs f desc)
  | RConstructorCall desc -> RConstructorCall (map_desc_locs f desc)
  | RImplicitReturn desc -> RImplicitReturn (map_desc_locs f desc)
  | RTypeAlias (s, None, d) -> RTypeAlias (s, None, map_desc_locs f d)
  | RTypeAlias (s, Some b, d) -> RTypeAlias (s, Some (f b), map_desc_locs f d)
  | RTypeParam (s, (d1, l1), (d2, l2)) ->
    RTypeParam (s, (map_desc_locs f d1, f l1), (map_desc_locs f d2, f l2))
  | RPropertyOf (s, d) -> RPropertyOf (s, map_desc_locs f d)
  | RNameProperty desc -> RNameProperty (map_desc_locs f desc)
  | RMissingAbstract desc -> RMissingAbstract (map_desc_locs f desc)
  | RPolyType desc -> RPolyType (map_desc_locs f desc)
  | RExactType desc -> RExactType (map_desc_locs f desc)
  | RReadOnlyType -> RReadOnlyType
  | ROptional desc -> ROptional (map_desc_locs f desc)
  | RMaybe desc -> RMaybe (map_desc_locs f desc)
  | RRestArray desc -> RRestArray (map_desc_locs f desc)
  | RAbstract desc -> RAbstract (map_desc_locs f desc)
  | RTypeApp desc -> RTypeApp (map_desc_locs f desc)
  | RTypeAppImplicit desc -> RTypeAppImplicit (map_desc_locs f desc)
  | RThisTypeApp desc -> RThisTypeApp (map_desc_locs f desc)
  | RExtends desc -> RExtends (map_desc_locs f desc)
  | RClass desc -> RClass (map_desc_locs f desc)
  | RStatics desc -> RStatics (map_desc_locs f desc)
  | RSuperOf desc -> RSuperOf (map_desc_locs f desc)
  | RFrozen desc -> RFrozen (map_desc_locs f desc)
  | RBound desc -> RBound (map_desc_locs f desc)
  | RPredicateOf desc -> RPredicateOf (map_desc_locs f desc)
  | RPredicateCall desc -> RPredicateCall (map_desc_locs f desc)
  | RPredicateCallNeg desc -> RPredicateCallNeg (map_desc_locs f desc)
  | RRefined desc -> RRefined (map_desc_locs f desc)
  | RRefinedElement desc -> RRefinedElement (map_desc_locs f desc)
  | RSpreadOf desc -> RSpreadOf (map_desc_locs f desc)
  | RShapeOf desc -> RShapeOf (map_desc_locs f desc)
  | RPartialOf desc -> RPartialOf (map_desc_locs f desc)
  | RMatchingProp (s, desc) -> RMatchingProp (s, map_desc_locs f desc)
  | RTrusted desc -> RTrusted (map_desc_locs f desc)
  | RPrivate desc -> RPrivate (map_desc_locs f desc)
  | RImplicitThis desc -> RImplicitThis (map_desc_locs f desc)
  | ( RObjectPatternRestProp | RArrayPatternRestProp | RCommonJSExports _ | RModule _
    | ROptionalChain | RReactProps | RReactElement _ | RReactClass | RReactComponent | RReactStatics
    | RReactDefaultProps | RReactState | RReactPropTypes | RReactChildren ) as r ->
    r
  | RReactChildrenOrType desc -> RReactChildrenOrType (map_desc_locs f desc)
  | RReactChildrenOrUndefinedOrType desc -> RReactChildrenOrUndefinedOrType (map_desc_locs f desc)
  | (RReactSFC | RReactConfig) as r -> r
  | RPossiblyMissingPropFromObj (propname, desc) ->
    RPossiblyMissingPropFromObj (propname, map_desc_locs f desc)
  | RWidenedObjProp desc -> RWidenedObjProp (map_desc_locs f desc)
  | RUnionBranching (desc, i) -> RUnionBranching (map_desc_locs f desc, i)

type 'loc virtual_reason = {
  test_id: int option;
  derivable: bool;
  desc: 'loc virtual_reason_desc;
  loc: 'loc;
  def_loc_opt: 'loc option;
  annot_loc_opt: 'loc option;
}
[@@deriving eq]

type reason = ALoc.t virtual_reason

type concrete_reason = Loc.t virtual_reason

type t = reason

let concretize_equal aloc_tables = equal_virtual_reason (ALoc.concretize_equal aloc_tables)

let in_range loc range =
  Loc.(
    let (line, line1, line2) = (loc.start.line, range.start.line, range._end.line) in
    (line1 < line || (line = line1 && range.start.column <= loc.start.column))
    && (line < line2 || (line = line2 && loc._end.column <= range._end.column)))

let string_of_source ?(strip_root = None) =
  File_key.(
    function
    | Builtins -> "(builtins)"
    | LibFile file ->
      begin
        match strip_root with
        | Some root ->
          let root_str = spf "%s%s" (Path.to_string root) Filename.dir_sep in
          if string_starts_with file root_str then
            spf "[LIB] %s" (Files.relative_path root_str file)
          else
            spf "[LIB] %s" (Filename.basename file)
        | None -> file
      end
    | SourceFile file
    | JsonFile file
    | ResourceFile file ->
      begin
        match strip_root with
        | Some root ->
          let root_str = spf "%s%s" (Path.to_string root) Filename.dir_sep in
          Files.relative_path root_str file
        | None -> file
      end)

let string_of_loc ?(strip_root = None) loc =
  Loc.(
    match loc.source with
    | None
    | Some File_key.Builtins ->
      ""
    | Some file -> spf "%s:%s" (string_of_source ~strip_root file) (Loc.to_string_no_source loc))

let string_of_aloc ?(strip_root = None) aloc =
  match ALoc.source aloc with
  | None
  | Some File_key.Builtins ->
    ""
  | Some file -> spf "%s:%s" (string_of_source ~strip_root file) (ALoc.to_string_no_source aloc)

let json_of_source ?(strip_root = None) =
  Hh_json.(
    function
    | Some x -> JSON_String (string_of_source ~strip_root x)
    | None -> JSON_Null)

let json_source_type_of_source =
  Hh_json.(
    function
    | Some (File_key.LibFile _) -> JSON_String "LibFile"
    | Some (File_key.SourceFile _) -> JSON_String "SourceFile"
    | Some (File_key.JsonFile _) -> JSON_String "JsonFile"
    | Some (File_key.ResourceFile _) -> JSON_String "ResourceFile"
    | Some File_key.Builtins -> JSON_String "Builtins"
    | None -> JSON_Null)

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
      ]))

let json_of_loc ?strip_root ?catch_offset_errors ~offset_table loc =
  Hh_json.(JSON_Object (json_of_loc_props ?strip_root ?catch_offset_errors ~offset_table loc))

(* reason constructors, accessors, etc. *)

let mk_reason_with_test_id test_id desc loc def_loc_opt annot_loc_opt =
  { test_id; derivable = false; desc; loc; def_loc_opt; annot_loc_opt }

let map_reason_locs f reason =
  let { def_loc_opt; annot_loc_opt; loc; desc; test_id; derivable } = reason in
  let loc' = f loc in
  let def_loc_opt' = Base.Option.map ~f def_loc_opt in
  let annot_loc_opt' = Base.Option.map ~f annot_loc_opt in
  let desc' = map_desc_locs f desc in
  {
    def_loc_opt = def_loc_opt';
    annot_loc_opt = annot_loc_opt';
    loc = loc';
    desc = desc';
    test_id;
    derivable;
  }

(* The current test_id is included in every new reason. *)
let mk_reason desc aloc = mk_reason_with_test_id (TestID.current ()) desc aloc None None

(* Lift a string to a reason. Usually used as a dummy reason. *)
let locationless_reason desc = mk_reason_with_test_id None desc ALoc.none None None

let func_reason ~async ~generator =
  let func_desc =
    match (async, generator) with
    | (true, true) -> RAsyncGenerator
    | (true, false) -> RAsync
    | (false, true) -> RGenerator
    | (false, false) -> RNormal
  in
  mk_reason (RFunction func_desc)

let poly_loc_of_reason r = r.loc

let aloc_of_reason = poly_loc_of_reason

let loc_of_reason = poly_loc_of_reason

(* TODO return ALoc *)
let def_poly_loc_of_reason r =
  match r.def_loc_opt with
  | Some loc -> loc
  | None -> aloc_of_reason r

let def_aloc_of_reason = def_poly_loc_of_reason

let def_loc_of_reason = def_poly_loc_of_reason

let annot_poly_loc_of_reason r = r.annot_loc_opt

let annot_aloc_of_reason = annot_poly_loc_of_reason

let annot_loc_of_reason = annot_poly_loc_of_reason

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
  | RTrusted r -> spf "trusted %s" (string_of_desc r)
  | RPrivate r -> spf "private %s" (string_of_desc r)
  | RNumber -> "number"
  | RBigInt -> "bigint"
  | RString
  | RLongStringLit _ ->
    "string"
  | RBoolean -> "boolean"
  | RMixed -> "mixed"
  | REmpty -> "empty"
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
  | RNumberLit x -> spf "number literal `%s`" x
  | RBigIntLit x -> spf "bigint literal `%s`" x
  | RBooleanLit b -> spf "boolean literal `%s`" (string_of_bool b)
  | RIndexedAccess { optional } ->
    if optional then
      "optional indexed access"
    else
      "indexed access"
  | RMatchingProp (k, v) -> spf "object with property `%s` that matches %s" k (string_of_desc v)
  | RObject -> "object"
  | RObjectLit -> "object literal"
  | RObjectType -> "object type"
  | RObjectClassName -> "Object"
  | RInterfaceType -> "interface type"
  | RArray -> "array"
  | RArrayLit -> "array literal"
  | REmptyArrayLit -> "empty array literal"
  | RArrayType -> "array type"
  | RROArrayType -> "read-only array type"
  | RTupleType -> "tuple type"
  | RTupleElement -> "tuple element"
  | RTupleOutOfBoundsAccess -> "undefined (out of bounds tuple access)"
  | RTupleLength i -> spf "length `%d` (number) of tuple" i
  | RFunction func -> spf "%sfunction" (function_desc_prefix func)
  | RFunctionType -> "function type"
  | RFunctionBody -> "function body"
  | RFunctionCall d -> spf "call of %s" (string_of_desc d)
  | RFunctionCallType -> "`$Call`"
  | RFunctionUnusedArgument -> "unused function argument"
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
  | REnum name -> spf "enum `%s`" name
  | REnumRepresentation representation -> spf "%s enum" (string_of_desc representation)
  | RGetterSetterProperty -> "getter/setter property"
  | RThis -> "this"
  | RThisType -> "`this` type"
  | RExistential -> "existential"
  | RImplicitInstantiation -> "implicit instantiation"
  | RTooFewArgs -> "undefined (too few arguments)"
  | RTooFewArgsExpectedRest -> "undefined (too few arguments, expected default/rest parameters)"
  | RConstructorReturn -> "constructor return"
  | RNewObject -> "new object"
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
  | RImplicitReturn desc -> spf "implicitly-returned %s" (string_of_desc desc)
  | RRegExp -> "regexp"
  | RSuper -> "super"
  | RNoSuper -> "empty super object"
  | RDummyPrototype -> "empty prototype object"
  | RDummyThis -> "bound `this` in method"
  | RImplicitThis desc -> spf "implicit `this` parameter of %s" (string_of_desc desc)
  | RTupleMap -> "`$TupleMap`"
  | RObjectMap -> "`$ObjMap`"
  | RObjectMapi -> "`$ObjMapi`"
  | RType x -> spf "`%s`" (prettify_react_util (display_string_of_name x))
  | RTypeAlias (x, _, _) -> spf "`%s`" (prettify_react_util x)
  | ROpaqueType x -> spf "`%s`" (prettify_react_util x)
  | RTypeParam (x, _, _) -> spf "`%s`" x
  | RTypeof x -> spf "`typeof %s`" x
  | RMethod (Some x) -> spf "method `%s`" x
  | RMethod None -> "computed method"
  | RIdentifier x -> spf "`%s`" (prettify_react_util (display_string_of_name x))
  | RIdentifierAssignment x -> spf "assignment of identifier `%s`" x
  | RMethodCall (Some x) -> spf "call of method `%s`" x
  | RMethodCall None -> "call of computed property"
  | RParameter (Some x) -> spf "`%s`" x
  | RParameter None -> "parameter"
  | RRestParameter (Some x) -> spf "rest parameter `%s`" x
  | RRestParameter None -> "rest parameter"
  | RProperty (Some x) -> spf "property `%s`" (display_string_of_name x)
  | RProperty None -> "computed property"
  | RPrivateProperty x -> spf "property `#%s`" x
  | RMember { object_; property } -> spf "`%s%s`" object_ property
  | RPropertyAssignment (Some x) -> spf "assignment of property `%s`" x
  | RPropertyAssignment None -> "assignment of computed property/element"
  | RShadowProperty x -> spf ".%s" (display_string_of_name x)
  | RPropertyOf (x, d) -> spf "property `%s` of %s" (display_string_of_name x) (string_of_desc d)
  | RPropertyIsAString (OrdinaryName "") -> "empty string"
  | RPropertyIsAString x -> spf "string `%s`" (display_string_of_name x)
  | RMissingProperty (Some x) -> spf "property `%s` does not exist" (display_string_of_name x)
  | RMissingProperty None -> "computed property does not exist"
  | RUnknownProperty (Some x) -> spf "property `%s` of unknown type" (display_string_of_name x)
  | RUnknownProperty None -> "computed property of unknown type"
  | RUnknownUnspecifiedProperty d ->
    spf "an unknown property that may exist on the inexact %s" (string_of_desc d)
  | RUndefinedProperty x -> spf "undefined property `%s`" (display_string_of_name x)
  | RSomeProperty -> "some property"
  | RNameProperty d -> spf "property `name` of %s" (string_of_desc d)
  | RMissingAbstract d -> spf "undefined. Did you forget to declare %s?" (string_of_desc d)
  | RFieldInitializer x -> spf "field initializer for `%s`" x
  | RUntypedModule m -> spf "import from untyped module `%s`" m
  | RNamedImportedType (m, _) -> spf "Named import from module `%s`" m
  | RImportStarType n -> spf "import type * as %s" n
  | RImportStarTypeOf n -> spf "import typeof * as %s" n
  | RImportStar n -> spf "import * as %s" n
  | RCode x -> "`" ^ x ^ "`"
  | RDefaultImportedType (_, m) -> spf "Default import from `%s`" m
  | RAsyncImport -> "async import"
  | RCustom x -> x
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
  | RRestArray _ -> "rest array"
  | RAbstract d -> spf "abstract %s" (string_of_desc d)
  | RTypeApp d -> string_of_desc d
  | RTypeAppImplicit d -> string_of_desc d
  | RThisTypeApp d -> spf "this instantiation of %s" (string_of_desc d)
  | RExtends d -> spf "extends %s" (string_of_desc d)
  | RClass d -> spf "class %s" (string_of_desc d)
  | RStatics d -> spf "statics of %s" (string_of_desc d)
  | RSuperOf d -> spf "super of %s" (string_of_desc d)
  | RFrozen d -> spf "frozen %s" (string_of_desc d)
  | RBound d -> spf "bound %s" (string_of_desc d)
  | RPredicateOf d -> spf "predicate encoded in %s" (string_of_desc d)
  | RPredicateCall d -> spf "predicate call to %s" (string_of_desc d)
  | RPredicateCallNeg d -> spf "negation of predicate call to %s" (string_of_desc d)
  | RRefined d -> spf "refined %s" (string_of_desc d)
  | RRefinedElement d -> spf "array element of refined %s" (string_of_desc d)
  | RIncompatibleInstantiation x -> spf "`%s`" x
  | RSpreadOf d -> spf "spread of %s" (string_of_desc d)
  | RShapeOf d -> spf "%s" (string_of_desc d)
  | RPartialOf d -> spf "partial %s" (string_of_desc d)
  | RObjectPatternRestProp -> "rest of object pattern"
  | RArrayPatternRestProp -> "rest of array pattern"
  | RCommonJSExports x -> spf "module `%s`" x
  | RModule x -> spf "module `%s`" (display_string_of_name x)
  | ROptionalChain -> "optional chain"
  | RReactProps -> "props"
  | RReactElement x ->
    (match x with
    | Some x -> spf "`%s` element" (display_string_of_name x)
    | None -> "React element")
  | RReactClass -> "React class"
  | RReactComponent -> "React component"
  | RReactStatics -> "statics of React class"
  | RReactDefaultProps -> "default props of React component"
  | RReactState -> "state of React component"
  | RReactPropTypes -> "propTypes of React component"
  | RReactChildren -> "children array"
  | RReactChildrenOrType desc -> spf "children array or %s" (string_of_desc desc)
  | RReactChildrenOrUndefinedOrType desc -> spf "children array or %s" (string_of_desc desc)
  | RReactSFC -> "React stateless functional component"
  | RReactConfig -> "config of React component"
  | RPossiblyMissingPropFromObj (propname, desc) ->
    spf
      "possibly missing property `%s` in %s"
      (display_string_of_name propname)
      (string_of_desc desc)
  | RWidenedObjProp desc -> string_of_desc desc
  | RUnionBranching (desc, _) -> string_of_desc desc
  | RUninitialized -> "uninitialized variable"
  | RPossiblyUninitialized -> "possibly uninitialized variable"

let string_of_reason ?(strip_root = None) r =
  let spos = string_of_aloc ~strip_root (aloc_of_reason r) in
  let desc = string_of_desc r.desc in
  if spos = "" then
    desc
  else if desc = "" then
    spos
  else
    spf "%s: %s" spos desc

let dump_reason ?(strip_root = None) r =
  spf
    "%s: %S%s"
    (string_of_aloc ~strip_root (aloc_of_reason r))
    (string_of_desc r.desc)
    begin
      match r.test_id with
      | Some n -> spf " (test %d)" n
      | None -> ""
    end

let desc_of_reason =
  let rec loop = function
    | RTypeAlias (_, _, desc)
    | RUnionBranching (desc, _) ->
      loop desc
    | desc -> desc
  in
  fun ?(unwrap = true) r ->
    if not unwrap then
      r.desc
    else
      loop r.desc

let internal_name name = InternalName name

let internal_name_of_name name =
  match name with
  | OrdinaryName str -> internal_name str
  | InternalName _
  | InternalModuleName _ ->
    (* Already internal *)
    name

let is_internal_name = function
  | OrdinaryName _ -> false
  | InternalName _
  | InternalModuleName _ ->
    true

let internal_module_name name = InternalModuleName name

let is_internal_module_name = function
  | OrdinaryName _
  | InternalName _ ->
    false
  | InternalModuleName _ -> true

let uninternal_name = function
  | OrdinaryName x
  | InternalName x
  | InternalModuleName x ->
    x

(* Instantiable reasons identify tvars that are created for the purpose of
   instantiation: they are fresh rather than shared, and should become types
   that flow to them. We assume these characteristics when performing
   speculative matching (even though we don't yet enforce them). *)
let is_instantiable_reason r =
  match desc_of_reason r with
  | RTypeParam _
  | RThisType
  | RExistential ->
    true
  | RImplicitInstantiation -> true
  | _ -> false

(* TODO: Property accesses create unresolved tvars to hold results, even when
   the object(s) on which the property accesses happen may be resolved. This can
   and should be fixed, for various benefits including but not limited to more
   precise type inference. But meanwhile we need to consider results of property
   accesses that might result in sentinel property values as constants to decide
   membership in disjoint unions, instead of asking for unnecessary annotations
   to make progress. According to Facebook's style guide, constant properties
   should have names like CONSTANT_PROPERTY, so we bet that when properties with
   such names are accessed, their types have the 0->1 property.

   As an example, suppose that we have an object `Tags` that stores tags of a
   disjoint union, e.g. { ACTION_FOO: 'foo', ACTION_BAR: 'bar' }.

   Then the types of Tags.ACTION_FOO and Tags.ACTION_BAR are assumed to be 0->1.
*)
let is_constant_reason r =
  (* TODO consider avoiding the use of display_string_of_name here. For now, leaving as-is to
   * prevent a behavior change. *)
  let helper x =
    let len = String.length x in
    if len = 0 then
      false
    else
      is_not_lowercase x 0 (len - 1)
  in
  match desc_of_reason r with
  | RIdentifier x ->
    let x = display_string_of_name x in
    (* A single-letter variable name which happens to be upper-case should not
       be confused with a constant reason. This should really be further
       restricted to `const`-declared identifiers in scope. Or, better yet,
       removing this heuristic entirely. *)
    let len = String.length x in
    if len < 2 then
      false
    else
      is_not_lowercase x 0 (len - 1)
  | RProperty (Some x)
  | RPropertyOf (x, _)
  | RPropertyIsAString x ->
    helper (display_string_of_name x)
  | RPrivateProperty x
  | RMember { object_ = _; property = x } ->
    helper x
  | _ -> false

let is_typemap_reason r =
  match desc_of_reason r with
  | RTupleMap
  | RObjectMap
  | RObjectMapi ->
    true
  | _ -> false

let is_calltype_reason r =
  match desc_of_reason r with
  | RTupleMap
  | RObjectMap
  | RObjectMapi
  | RFunctionCallType ->
    true
  | _ -> false

let is_literal_object_reason r =
  match desc_of_reason r with
  | RObjectLit
  | RFrozen RObjectLit
  | RSpreadOf _
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
  | RArrayLit
  | REmptyArrayLit ->
    true
  | _ -> false

let is_derivable_reason r = r.derivable

let derivable_reason r = { r with derivable = true }

let builtin_reason desc =
  { Loc.none with Loc.source = Some File_key.Builtins }
  |> ALoc.of_loc
  |> mk_reason desc
  |> derivable_reason

let is_builtin_reason f r = r.loc |> f |> ( = ) (Some File_key.Builtins)

let is_lib_reason r =
  r.loc |> ALoc.source |> Base.Option.value_map ~default:false ~f:File_key.is_lib_file

let is_lib_reason_def r =
  def_aloc_of_reason r
  |> ALoc.source
  |> Base.Option.value_map ~default:false ~f:File_key.is_lib_file

let is_blamable_reason r = not (r.loc = ALoc.none || is_lib_reason r)

(* reason transformers: *)

(* returns reason with new description and position of original *)
let update_desc_reason f r =
  mk_reason_with_test_id
    r.test_id
    (f (desc_of_reason ~unwrap:false r))
    (poly_loc_of_reason r)
    r.def_loc_opt
    (annot_poly_loc_of_reason r)

let update_desc_new_reason f r =
  mk_reason_with_test_id
    r.test_id
    (f (desc_of_reason ~unwrap:false r))
    (poly_loc_of_reason r)
    None
    None

let replace_desc_reason desc r =
  mk_reason_with_test_id r.test_id desc r.loc r.def_loc_opt r.annot_loc_opt

let replace_desc_new_reason desc r = mk_reason_with_test_id r.test_id desc r.loc None None

(* returns reason with new location and description of original *)
let repos_reason loc reason =
  let def_aloc_opt =
    let def_loc = def_poly_loc_of_reason reason in
    if loc = def_loc then
      None
    else
      Some def_loc
  in
  mk_reason_with_test_id reason.test_id reason.desc loc def_aloc_opt reason.annot_loc_opt

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
let rec code_desc_of_expression ~wrap (_, x) =
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
      ^ code_desc_of_expression ~wrap:false alternate)
  | Function _ -> "function () { ... }"
  | Identifier (_, { Ast.Identifier.name = x; comments = _ }) -> x
  | Import { Import.argument; comments = _ } ->
    "import(" ^ code_desc_of_expression ~wrap:false argument ^ ")"
  | JSXElement x -> code_desc_of_jsx_element x
  | JSXFragment _ -> "<>...</>"
  | Ast.Expression.Literal x -> code_desc_of_literal x
  | Logical { Logical.operator; left; right; comments = _ } ->
    do_wrap (code_desc_of_operation left (`Logical operator) right)
  | Member { Member._object; property; comments = _ } ->
    let o = code_desc_of_expression ~wrap:true _object in
    let p = code_desc_of_property ~optional:false property in
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
  | OptionalCall { OptionalCall.call = { Call.callee; targs; arguments; comments = _ }; optional }
    ->
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
    ^ (if optional then
        "?."
      else
        "")
    ^ targ_string
    ^ arg_string
  | OptionalMember { OptionalMember.member = { Member._object; property; comments = _ }; optional }
    ->
    let o = code_desc_of_expression ~wrap:true _object in
    let p = code_desc_of_property ~optional property in
    o ^ p
  | Sequence { Sequence.expressions; comments = _ } ->
    code_desc_of_expression ~wrap (List.hd (List.rev expressions))
  | Super _ -> "super"
  | TaggedTemplate { TaggedTemplate.tag; _ } -> code_desc_of_expression ~wrap:true tag ^ "`...`"
  | TemplateLiteral _ -> "`...`"
  | This _ -> "this"
  | TypeCast { TypeCast.expression; _ } -> code_desc_of_expression ~wrap expression
  | Unary { Unary.operator; argument; comments = _ } ->
    let x = code_desc_of_expression ~wrap:true argument in
    let op =
      Unary.(
        match operator with
        | Minus -> "-"
        | Plus -> "+"
        | Not -> "!"
        | BitNot -> "~"
        | Typeof -> "typeof "
        | Void -> "void "
        | Delete -> "delete "
        | Await -> "await ")
    in
    do_wrap (op ^ x)
  | Update { Update.operator; prefix; argument; comments = _ } ->
    let x = code_desc_of_expression ~wrap:true argument in
    let op =
      Update.(
        match operator with
        | Increment -> "++"
        | Decrement -> "--")
    in
    do_wrap
      (if prefix then
        op ^ x
      else
        x ^ op)
  | Yield { Yield.argument = Some x; delegate = false; _ } ->
    do_wrap ("yield " ^ code_desc_of_expression ~wrap:false x)
  | Yield { Yield.argument = Some x; delegate = true; _ } ->
    do_wrap ("yield* " ^ code_desc_of_expression ~wrap:false x)
  | Yield { Yield.argument = None; delegate = false; _ } -> "yield"
  | Yield { Yield.argument = None; delegate = true; _ } -> "yield*"
  (* TODO *)
  | Comprehension _
  | Generator _ ->
    do_wrap "..."

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
          true)
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
        } ) ->
    "<" ^ a ^ ":" ^ b ^ " />"
  | MemberExpression x ->
    let rec loop = function
      | ( _,
          {
            MemberExpression._object =
              MemberExpression.Identifier (_, { Identifier.name = a; comments = _ });
            property = (_, { Identifier.name = b; comments = _ });
          } ) ->
        a ^ "." ^ b
      | ( _,
          {
            MemberExpression._object = MemberExpression.MemberExpression a;
            property = (_, { Identifier.name = b; comments = _ });
          } ) ->
        loop a ^ "." ^ b
    in
    "<" ^ loop x ^ " />"

and code_desc_of_literal x =
  let open Ast in
  match x.Literal.value with
  | Literal.String x when String.length x > 16 -> "'" ^ String.sub x 0 10 ^ "...'"
  | _ -> x.Literal.raw

and code_desc_of_property ~optional property =
  match property with
  | Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name = x; comments = _ }) ->
    (if optional then
      "?."
    else
      ".")
    ^ x
  | Ast.Expression.Member.PropertyPrivateName
      (_, { Ast.PrivateName.id = (_, { Ast.Identifier.name = x; comments = _ }); comments = _ }) ->
    (if optional then
      "?.#"
    else
      ".#")
    ^ x
  | Ast.Expression.Member.PropertyExpression x ->
    (if optional then
      "?.["
    else
      "[")
    ^ code_desc_of_expression ~wrap:false x
    ^ "]"

let rec mk_expression_reason =
  let open Ast.Expression in
  function
  | (loc, TypeCast { TypeCast.expression; _ }) -> repos_reason loc (mk_expression_reason expression)
  | (loc, Object _) -> mk_reason RObjectLit loc
  | (loc, Array _) -> mk_reason RArrayLit loc
  | (loc, ArrowFunction { Ast.Function.async; _ }) -> func_reason ~async ~generator:false loc
  | (loc, Function { Ast.Function.async; generator; _ }) -> func_reason ~async ~generator loc
  | (loc, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.String ""; _ }) ->
    mk_reason (RStringLit (OrdinaryName "")) loc
  | (loc, TaggedTemplate _) -> mk_reason RTemplateString loc
  | (loc, TemplateLiteral _) -> mk_reason RTemplateString loc
  | (loc, Member { Member._object; property; comments = _ }) ->
    mk_reason
      (RMember
         {
           object_ = code_desc_of_expression ~wrap:true _object;
           property = code_desc_of_property ~optional:false property;
         })
      loc
  | (loc, _) as x -> mk_reason (RCode (code_desc_of_expression ~wrap:false x)) loc

let mk_pattern_reason ((loc, _) as patt) = mk_reason (RCode (code_desc_of_pattern patt)) loc

(* TODO: replace RCustom descriptions with proper descriptions *)
let unknown_elem_empty_array_desc = RCustom "unknown element type of empty array"

let inferred_union_elem_array_desc =
  RCustom
    "inferred union of array element types (alternatively, provide an annotation to summarize the array element type)"

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
let classification_of_reason r =
  match desc_of_reason ~unwrap:true r with
  | RNumber
  | RBigInt
  | RString
  | RSymbol
  | RBoolean
  | RLongStringLit _
  | RStringLit _
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
  | REmptyArrayLit
  | RArrayType
  | RROArrayType
  | RTupleType
  | RRestArray _
  | RArrayPatternRestProp ->
    `Array
  | RMixed
  | REmpty
  | RAnyExplicit
  | RAnyImplicit
  | RIndexedAccess _
  | RMatchingProp _
  | RObject
  | RObjectLit
  | RObjectType
  | RObjectClassName
  | RInterfaceType
  | RTupleElement
  | RTupleLength _
  | RTupleOutOfBoundsAccess
  | RFunction _
  | RFunctionType
  | RFunctionBody
  | RFunctionCall _
  | RFunctionCallType
  | RFunctionUnusedArgument
  | RJSXFunctionCall _
  | RJSXIdentifier _
  | RJSXElementProps _
  | RJSXElement _
  | RUnaryOperator _
  | RBinaryOperator _
  | RLogical _
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
  | RImplicitReturn _
  | RSuper
  | RNoSuper
  | RDummyPrototype
  | RDummyThis
  | RImplicitThis _
  | RTupleMap
  | RObjectMap
  | RObjectMapi
  | RType _
  | RTypeAlias _
  | ROpaqueType _
  | RTypeParam _
  | RTypeof _
  | RMethod _
  | RMethodCall _
  | RParameter _
  | RRestParameter _
  | RIdentifier _
  | RIdentifierAssignment _
  | RPropertyAssignment _
  | RProperty _
  | RPrivateProperty _
  | RShadowProperty _
  | RMember _
  | RPropertyOf _
  | RPropertyIsAString _
  | RMissingProperty _
  | RUnknownProperty _
  | RUnknownUnspecifiedProperty _
  | RUndefinedProperty _
  | RSomeProperty
  | RNameProperty _
  | RMissingAbstract _
  | RFieldInitializer _
  | RUntypedModule _
  | RNamedImportedType _
  | RImportStarType _
  | RImportStarTypeOf _
  | RImportStar _
  | RDefaultImportedType _
  | RAsyncImport
  | RCode _
  | RCustom _
  | RExports
  | RPolyType _
  | RExactType _
  | RReadOnlyType
  | ROptional _
  | RMaybe _
  | RAbstract _
  | RTypeApp _
  | RTypeAppImplicit _
  | RThisTypeApp _
  | RExtends _
  | RClass _
  | RStatics _
  | RSuperOf _
  | RFrozen _
  | RBound _
  | RPredicateOf _
  | RPredicateCall _
  | RPredicateCallNeg _
  | RRefined _
  | RRefinedElement _
  | RIncompatibleInstantiation _
  | RSpreadOf _
  | RShapeOf _
  | RPartialOf _
  | RObjectPatternRestProp
  | RCommonJSExports _
  | RModule _
  | ROptionalChain
  | RReactProps
  | RReactElement _
  | RReactClass
  | RReactComponent
  | RReactStatics
  | RReactDefaultProps
  | RReactState
  | RReactPropTypes
  | RReactChildren
  | RReactChildrenOrType _
  | RReactChildrenOrUndefinedOrType _
  | RReactSFC
  | RReactConfig
  | RPossiblyMissingPropFromObj _
  | RWidenedObjProp _
  | RUnionBranching _
  | RTrusted _
  | RPrivate _
  | REnum _
  | REnumRepresentation _ ->
    `Unclassified

let is_nullish_reason r = classification_of_reason r = `Nullish

let is_scalar_reason r =
  let c = classification_of_reason r in
  c = `Scalar || c = `Nullish

let is_array_reason r = classification_of_reason r = `Array

let invalidate_rtype_alias = function
  | RTypeAlias (name, Some _, desc) -> RTypeAlias (name, None, desc)
  | desc -> desc
