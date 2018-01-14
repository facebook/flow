(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
  let current() = !_current

  (* Call f on a, installing new_test_id as the current test_id, and restoring
     the current test_id when done. (See also the function mk_reason below.) *)
  let run f a =
    let test_id = current () in
    _current := Some (mk_id ());
    f a;
    _current := test_id

end

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
  | RUnaryOperator of string * reason_desc
  | RBinaryOperator of string * reason_desc * reason_desc
  | RLogical of string * reason_desc * reason_desc
  | RAnyObject
  | RAnyFunction
  | RUnknownString
  | RStringEnum
  | RNumberEnum
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
  | RCustom of string
  | RPolyType of reason_desc
  | RPolyTest of reason_desc
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

type reason = {
  test_id: int option;
  derivable: bool;
  desc: reason_desc;
  loc: Loc.t;
  def_loc_opt: Loc.t option;
}

type t = reason

let lexpos file line col = {
  Lexing.pos_fname = file;
  Lexing.pos_lnum = line;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = col;
}

let diff_range loc = Loc.(
  let line1, line2 = loc.start.line, loc._end.line in
  (* TODO: Get rid of +1 which is here to ensure same behavior as old code
     using Pos.info_pos *)
  let start, end_  = loc.start.column + 1, loc._end.column in
  (line2 - line1, end_ - start)
)

let in_range loc range = Loc.(
  let line, line1, line2 = loc.start.line, range.start.line, range._end.line in
  (line1 < line || (line = line1 && range.start.column <= loc.start.column)) &&
  (line < line2 || (line = line2 && loc._end.column <= range._end.column))
)

let rec patch ll offset lines = function
  | [] -> ()
  | (l,c,str)::insertions ->
      let c = if l = ll then c + offset else c in
      let del = try Some (int_of_string str) with _ -> None in
      let line = lines.(l - 1) in
      let shift = match del with
      | Some n -> (* delete n chars at l, c *)
          lines.(l - 1) <- spf "%s%s"
            (string_before line c) (string_after line (c + n));
          -n
      | None -> (* insert str at l, c *)
          lines.(l - 1) <- spf "%s%s%s"
            (string_before line c) str (string_after line c);
          String.length str
      in
      let offset = (if l = ll then offset else 0) + shift in
      patch l offset lines insertions

let do_patch lines insertions =
  let lines = Array.of_list lines in
  patch 1 0 lines insertions;
  String.concat "\n" (Array.to_list lines)

let string_of_source ?(strip_root=None) = File_key.(function
  | Builtins -> "(builtins)"
  | LibFile file ->
    begin match strip_root with
    | Some root ->
      let root_str = spf "%s%s" (Path.to_string root) Filename.dir_sep in
      if string_starts_with file root_str
      then spf "[LIB] %s" (Files.relative_path root_str file)
      else spf "[LIB] %s" (Filename.basename file)
    | None -> file
    end
  | SourceFile file
  | JsonFile file
  | ResourceFile file ->
    begin match strip_root with
    | Some root ->
      let root_str = spf "%s%s" (Path.to_string root) Filename.dir_sep in
      Files.relative_path root_str file
    | None ->
      file
    end
)

let string_of_loc_pos loc = Loc.(
  let line = loc.start.line in
  let start = loc.start.column + 1 in
  let end_ = loc._end.column in
  if line <= 0 then
    "0:0"
  else if line = loc._end.line && start = end_ then
    spf "%d:%d" line start
  else if line != loc._end.line then
    spf "%d:%d,%d:%d" line start loc._end.line end_
  else
    spf "%d:%d-%d" line start end_
)

let string_of_loc ?(strip_root=None) loc = Loc.(
  match loc.source with
  | None
  | Some File_key.Builtins -> ""
  | Some file ->
    spf "%s:%s" (string_of_source ~strip_root file) (string_of_loc_pos loc)
)

let json_of_loc ?(strip_root=None) loc = Hh_json.(Loc.(
  JSON_Object [
    "source", (
      match loc.source with
      | Some x -> JSON_String (string_of_source ~strip_root x)
      | None -> JSON_Null
    );
    "type", (match loc.source with
    | Some File_key.LibFile _ -> JSON_String "LibFile"
    | Some File_key.SourceFile _ -> JSON_String "SourceFile"
    | Some File_key.JsonFile _ -> JSON_String "JsonFile"
    | Some File_key.ResourceFile _ -> JSON_String "ResourceFile"
    | Some File_key.Builtins -> JSON_String "Builtins"
    | None -> JSON_Null);
    "start", JSON_Object [
      "line", int_ loc.start.line;
      (* It's not ideal that we use a different column numbering system here
       * versus other places (like the estree translator) *)
      "column", int_ (loc.start.column + 1);
      "offset", int_ loc.start.offset;
    ];
    "end", JSON_Object [
      "line", int_ loc._end.line;
      "column", int_ loc._end.column;
      "offset", int_ loc._end.offset;
    ];
  ]
))

(* reason constructors, accessors, etc. *)

let mk_reason_with_test_id test_id desc loc def_loc_opt = {
  test_id;
  derivable = false;
  desc;
  loc;
  def_loc_opt;
}

(* The current test_id is included in every new reason. *)
let mk_reason desc loc =
  mk_reason_with_test_id (TestID.current()) desc loc None

(* Lift a string to a reason. Usually used as a dummy reason. *)
let locationless_reason desc =
  mk_reason_with_test_id None desc Loc.none None

let func_reason {Ast.Function.async; generator; _} =
  let func_desc = match async, generator with
  | true, true -> RAsyncGenerator
  | true, false -> RAsync
  | false, true -> RGenerator
  | false, false -> RNormal
  in
  mk_reason (RFunction func_desc)


let loc_of_reason r = r.loc

let def_loc_of_reason r =
  match r.def_loc_opt with
  | Some loc -> loc
  | None -> loc_of_reason r

let function_desc_prefix = function
  | RAsync -> "async "
  | RGenerator -> "generator "
  | RAsyncGenerator -> "async generator "
  | RNormal -> ""

let prettify_react_util s =
  let length = String.length s in
  if length < 6 then s
  else if ((String.sub s 0 6) = "React$") then ("React." ^ (String.sub s 6 (length - 6)))
  else s

let rec string_of_desc = function
  | RNumber -> "number"
  | RString -> "string"
  | RBoolean -> "boolean"
  | RMixed -> "mixed"
  | REmpty -> "empty"
  | RAny -> "any"
  | RVoid -> "undefined"
  | RNull -> "null"
  | RNullOrVoid -> "null or undefined"
  | RStringLit x -> spf "string literal `%s`" x
  | RNumberLit x -> spf "number literal `%s`" x
  | RBooleanLit b -> spf "boolean literal `%s`" (string_of_bool b)
  | RMatchingProp (k, v) ->
    spf "object with property `%s` that matches %s" k (string_of_desc v)
  | RObject -> "object"
  | RObjectLit -> "object literal"
  | RObjectType -> "object type"
  | RObjectClassName -> "Object"
  | RArray -> "array"
  | RArrayLit -> "array literal"
  | REmptyArrayLit -> "empty array literal"
  | RArrayType -> "array type"
  | RROArrayType -> "read-only array type"
  | RTupleType -> "tuple type"
  | RTupleElement -> "tuple element"
  | RTupleOutOfBoundsAccess -> "undefined (out of bounds tuple access)"
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
  | RUnaryOperator (operator, value) ->
    spf "%s %s" operator (string_of_desc value)
  | RBinaryOperator (operator, left, right) ->
    spf "%s %s %s" (string_of_desc left) operator (string_of_desc right)
  | RLogical (operator, left, right) ->
    spf "%s %s %s" (string_of_desc left) operator (string_of_desc right)
  | RAnyObject -> "any object"
  | RAnyFunction -> "any function"
  | RUnknownString -> "some string with unknown value"
  | RStringEnum -> "string enum"
  | RNumberEnum -> "number enum"
  | RGetterSetterProperty -> "getter/setter property"
  | RThis -> "this"
  | RThisType -> "`this` type"
  | RExistential -> "existential"
  | RTooFewArgs -> "undefined (too few arguments)"
  | RTooFewArgsExpectedRest ->
    "undefined (too few arguments, expected default/rest parameters)"
  | RUninitializedThis -> "uninitialized this (expected super constructor call)"
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
  | RConstructorCall (RPolyType (RStatics d)) -> string_of_desc d
  | RConstructorCall (RStatics d) -> string_of_desc d
  | RConstructorCall d -> spf "new %s" (string_of_desc d)
  | RReturn -> "return"
  | RRegExp -> "regexp"
  | RSuper -> "super"
  | RNoSuper -> "empty super object"
  | RDummyPrototype -> "empty prototype object"
  | RDummyThis -> "bound `this` in method"
  | RTupleMap -> "tuple map"
  | RObjectMap -> "object map"
  | RObjectMapi -> "object mapi"
  | RType x -> spf "`%s`" (prettify_react_util x)
  | RTypeAlias (x, _) -> spf "`%s`" (prettify_react_util x)
  | ROpaqueType x -> spf "`%s`" (prettify_react_util x)
  | RTypeParam (x, _, _) -> spf "`%s`" x
  | RMethod (Some x) -> spf "method `%s`" x
  | RMethod None -> "computed method"
  | RIdentifier x -> spf "`%s`" (prettify_react_util x)
  | RIdentifierAssignment x -> spf "assignment of identifier `%s`" x
  | RMethodCall (Some x) -> spf "call of method `%s`" x
  | RMethodCall None -> "call of computed property"
  | RParameter (Some x) -> spf "`%s`" x
  | RParameter None -> "parameter"
  | RRestParameter (Some x) -> spf "rest parameter `%s`" x
  | RRestParameter None -> "rest parameter"
  | RProperty (Some x) -> spf "property `%s`" x
  | RProperty None -> "computed property"
  | RPrivateProperty x -> spf "property `#%s`" x
  | RPropertyAssignment (Some x) -> spf "assignment of property `%s`" x
  | RPropertyAssignment None -> "assignment of computed property/element"
  | RShadowProperty x -> spf ".%s" x
  | RPropertyOf (x, d) -> spf "property `%s` of %s" x (string_of_desc d)
  | RPropertyIsAString x -> spf "string `%s`" x
  | RMissingProperty (Some x) -> spf "property `%s` does not exist" x
  | RMissingProperty None -> "computed property does not exist"
  | RUnknownProperty (Some x) -> spf "property `%s` of unknown type" x
  | RUnknownProperty None -> "computed property of unknown type"
  | RUndefinedProperty x -> spf "undefined property `%s`" x
  | RSomeProperty -> "some property"
  | RNameProperty d -> spf "property `name` of %s" (string_of_desc d)
  | RMissingAbstract d ->
    spf "undefined. Did you forget to declare %s?" (string_of_desc d)
  | RFieldInitializer x -> spf "field initializer for `%s`" x
  | RUntypedModule m -> spf "import from untyped module `%s`" m
  | RNamedImportedType m -> spf "Named import from module `%s`" m
  | RCustom x -> x
  | RPolyType (RStatics d) -> string_of_desc d
  | RPolyType d -> string_of_desc d
  | RPolyTest d -> string_of_desc d
  | RExactType d -> string_of_desc d
  | ROptional d -> spf "optional %s" (string_of_desc d)
  | RMaybe d ->
    let rec loop = function
    | RMaybe d -> loop d
    | d -> d
    in
    spf "nullable %s" (string_of_desc (loop d))
  | RRestArray d -> spf "rest parameter array of %s" (string_of_desc d)
  | RAbstract d -> spf "abstract %s" (string_of_desc d)
  | RTypeApp d -> string_of_desc d
  | RThisTypeApp d -> spf "this instantiation of %s" (string_of_desc d)
  | RExtends d -> spf "extends %s" (string_of_desc d)
  | RStatics d -> spf "statics of %s" (string_of_desc d)
  | RSuperOf d -> spf "super of %s" (string_of_desc d)
  | RFrozen d -> spf "frozen %s" (string_of_desc d)
  | RBound d -> spf "bound %s" (string_of_desc d)
  | RVarianceCheck d -> spf "variance check: %s" (string_of_desc d)
  | RPredicateOf d -> spf "predicate of %s" (string_of_desc d)
  | RPredicateCall d -> spf "predicate call to %s" (string_of_desc d)
  | RPredicateCallNeg d ->
    spf "negation of predicate call to %s" (string_of_desc d)
  | RRefined d -> spf "refined %s" (string_of_desc d)
  | RIncompatibleInstantiation x -> spf "`%s`" x
  | RSpreadOf d -> spf "spread of %s" (string_of_desc d)
  | RObjectPatternRestProp -> "rest of object pattern"
  | RArrayPatternRestProp -> "rest of array pattern"
  | RCommonJSExports x -> spf "module `%s`" x

  | RReactProps -> "props"
  | RReactElement x ->
    (match x with
    | Some x -> spf "`%s` element" x
    | None -> "React element")
  | RReactClass -> "React class"
  | RReactComponent -> "React component"
  | RReactStatics -> "statics of React class"
  | RReactDefaultProps -> "default props of React component"
  | RReactState -> "state of React component"
  | RReactPropTypes -> "propTypes of React component"
  | RReactChildren -> "children array"
  | RReactChildrenOrType desc ->
    spf "children array or %s" (string_of_desc desc)
  | RReactChildrenOrUndefinedOrType desc ->
    spf "children array or %s" (string_of_desc desc)
  | RReactSFC -> "React stateless functional component"

let string_of_reason ?(strip_root=None) r =
  let spos = string_of_loc ~strip_root (loc_of_reason r) in
  let desc = string_of_desc r.desc in
  if spos = ""
  then desc
  else (
    if desc = ""
    then spos
    else spf "%s:\n%s" spos desc
  )

let json_of_reason ?(strip_root=None) r = Hh_json.(
  JSON_Object ([
    "pos", json_of_loc ~strip_root (loc_of_reason r);
    "desc", JSON_String (string_of_desc r.desc)
  ])
)

let dump_reason ?(strip_root=None) r =
  spf "%s: %S%s"
    (string_of_loc ~strip_root (loc_of_reason r))
    (string_of_desc r.desc)
    begin match r.test_id with
    | Some n -> spf " (test %d)" n
    | None -> ""
    end

let desc_of_reason =
  let rec loop = function
  | RTypeAlias (_, desc)
  | RPolyTest desc
    -> loop desc
  | desc
    -> desc
  in
  fun ?(unwrap=true) r ->
    if not unwrap then r.desc else loop r.desc

let internal_name name =
  spf ".%s" name

let is_internal_name name =
  String.length name >= 1 && name.[0] = '.'

let internal_module_name name =
  spf ".$module__%s" name

let is_internal_module_name name =
  string_starts_with name ".$module__"

let internal_pattern_name loc =
  spf ".$pattern__%s" (string_of_loc loc)

(* Instantiable reasons identify tvars that are created for the purpose of
   instantiation: they are fresh rather than shared, and should become types
   that flow to them. We assume these characteristics when performing
   speculative matching (even though we don't yet enforce them). *)
let is_instantiable_reason r =
  match desc_of_reason r with
  | RTypeParam _
  | RThisType
  | RExistential -> true
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
  match desc_of_reason r with
  | RIdentifier x
  | RProperty (Some x)
  | RPrivateProperty x
  | RPropertyOf (x,_)
  | RPropertyIsAString x ->
    let len = String.length x in
    if len = 0
    then false
    else is_not_lowercase x 0 (len - 1)
  | _ -> false

let is_typemap_reason r =
  match desc_of_reason r with
  | RTupleMap
  | RObjectMap
  | RObjectMapi -> true
  | _ -> false

let is_calltype_reason r =
  match desc_of_reason r with
  | RTupleMap
  | RObjectMap
  | RObjectMapi
  | RFunctionCallType -> true
  | _ -> false

let is_derivable_reason r =
  r.derivable

let derivable_reason r =
  { r with derivable = true }

let builtin_reason desc =
  mk_reason desc { Loc.none with Loc.source = Some File_key.Builtins }
  |> derivable_reason

let is_builtin_reason r =
  Loc.(r.loc.source = Some File_key.Builtins)

let is_lib_reason r =
  (* TODO: use File_key.is_lib_file *)
  Loc.(match r.loc.source with
  | Some File_key.LibFile _ -> true
  | Some File_key.Builtins -> true
  | Some File_key.SourceFile _ -> false
  | Some File_key.JsonFile _ -> false
  | Some File_key.ResourceFile _ -> false
  | None -> false)

let is_blamable_reason r =
  not Loc.(r.loc = none || is_lib_reason r)

let reasons_overlap r1 r2 =
  Loc.(contains r1.loc r2.loc)

(* reason transformers: *)

(* returns reason with new description and position of original *)
let replace_reason f r =
  mk_reason (f (desc_of_reason ~unwrap:false r)) (loc_of_reason r)

let replace_reason_const ?(keep_def_loc=false) desc r =
  let def_loc_opt = if keep_def_loc then r.def_loc_opt else None in
  mk_reason_with_test_id r.test_id desc (loc_of_reason r) def_loc_opt

(* returns reason with new location and description of original *)
let repos_reason loc reason =
  let def_loc_opt =
    let def_loc = def_loc_of_reason reason in
    if loc = def_loc then None else Some def_loc
  in
  mk_reason_with_test_id reason.test_id (desc_of_reason ~unwrap:false reason) loc def_loc_opt

module ReasonMap = MyMap.Make(struct
  type t = reason
  let compare = Pervasives.compare
end)

(* Is this the reason of a scalar type? true if the type *cannot* recursively
 * hold any other types. For example, number is a scalar but an object like
 * {p: number} is not. *)
let is_scalar_reason r = match desc_of_reason ~unwrap:true r with
| RNumber
| RString
| RBoolean
| RVoid
| RNull
| RNullOrVoid
| RStringLit _
| RNumberLit _
| RBooleanLit _
| RJSXText
| RUnknownString
| RStringEnum
| RNumberEnum
| RKeySet
| RRegExp
  -> true
| RMixed
| REmpty
| RAny
| RMatchingProp _
| RObject
| RObjectLit
| RObjectType
| RObjectClassName
| RArray
| RArrayLit
| REmptyArrayLit
| RArrayType
| RROArrayType
| RTupleType
| RTupleElement
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
| RAnyObject
| RAnyFunction
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
| RNoSuper
| RDummyPrototype
| RDummyThis
| RTupleMap
| RObjectMap
| RObjectMapi
| RType _
| RTypeAlias _
| ROpaqueType _
| RTypeParam _
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
| RPropertyOf _
| RPropertyIsAString _
| RMissingProperty _
| RUnknownProperty _
| RUndefinedProperty _
| RSomeProperty
| RNameProperty _
| RMissingAbstract _
| RFieldInitializer _
| RUntypedModule _
| RNamedImportedType _
| RCustom _
| RPolyType _
| RPolyTest _
| RExactType _
| ROptional _
| RMaybe _
| RRestArray _
| RAbstract _
| RTypeApp _
| RThisTypeApp _
| RExtends _
| RStatics _
| RSuperOf _
| RFrozen _
| RBound _
| RVarianceCheck _
| RPredicateOf _
| RPredicateCall _
| RPredicateCallNeg _
| RRefined _
| RIncompatibleInstantiation _
| RSpreadOf _
| RObjectPatternRestProp
| RArrayPatternRestProp
| RCommonJSExports _
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
  -> false

(* Is this the reason of an array type? This depends on Flow's custom handling
 * of arrays. *)
let is_array_reason r = match desc_of_reason ~unwrap:true r with
| RArray
| RArrayLit
| REmptyArrayLit
| RArrayType
| RROArrayType
| RTupleType
| RRestArray _
| RArrayPatternRestProp
  -> true
| RNumber
| RString
| RBoolean
| RMixed
| REmpty
| RAny
| RVoid
| RNull
| RNullOrVoid
| RStringLit _
| RNumberLit _
| RBooleanLit _
| RMatchingProp _
| RObject
| RObjectLit
| RObjectType
| RObjectClassName
| RTupleElement
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
| RJSXText
| RUnaryOperator _
| RBinaryOperator _
| RLogical _
| RAnyObject
| RAnyFunction
| RUnknownString
| RStringEnum
| RNumberEnum
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
| RConstructorCall _
| RReturn
| RRegExp
| RSuper
| RNoSuper
| RDummyPrototype
| RDummyThis
| RTupleMap
| RObjectMap
| RObjectMapi
| RType _
| RTypeAlias _
| ROpaqueType _
| RTypeParam _
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
| RPropertyOf _
| RPropertyIsAString _
| RMissingProperty _
| RUnknownProperty _
| RUndefinedProperty _
| RSomeProperty
| RNameProperty _
| RMissingAbstract _
| RFieldInitializer _
| RUntypedModule _
| RNamedImportedType _
| RCustom _
| RPolyType _
| RPolyTest _
| RExactType _
| ROptional _
| RMaybe _
| RAbstract _
| RTypeApp _
| RThisTypeApp _
| RExtends _
| RStatics _
| RSuperOf _
| RFrozen _
| RBound _
| RVarianceCheck _
| RPredicateOf _
| RPredicateCall _
| RPredicateCallNeg _
| RRefined _
| RIncompatibleInstantiation _
| RSpreadOf _
| RObjectPatternRestProp
| RCommonJSExports _
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
  -> false
