(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
module Ast = Spider_monkey_ast

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

type reason = {
  test_id: int option;
  derivable: bool;
  desc: reason_desc;
  loc: Loc.t;
  (* origin is a persistent reason, immune to repos_reason *)
  origin: reason option;
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

let string_of_loc loc = Loc.(
  match loc.source with
  | None
  | Some Builtins -> ""
  | Some LibFile file
  | Some SourceFile file
  | Some JsonFile file
  | Some ResourceFile file ->
    spf "%s:%s" file (string_of_loc_pos loc)
)

(* helper: strip root from positions *)
let strip_root_from_loc root loc = Loc.(
  let source = match loc.source with
  | None -> None
  | Some Builtins -> Some Builtins
  | Some LibFile file ->
    let root_str = spf "%s%s" (Path.to_string root) Filename.dir_sep in
    if string_starts_with file root_str
    then Some (LibFile (spf "[LIB] %s" (Files.relative_path root_str file)))
    else Some (LibFile (spf "[LIB] %s" (Filename.basename file)))

  | Some (SourceFile _ | JsonFile _ | ResourceFile _ as filename) ->
    let root_str = spf "%s%s" (Path.to_string root) Filename.dir_sep in
    Some (Loc.filename_map (Files.relative_path root_str) filename)
  in
  { loc with source }
)

let json_of_loc ?(strip_root=None) loc = Hh_json.(Loc.(
  JSON_Object [
    "source", (
      let loc =
        match strip_root with
        | Some root -> strip_root_from_loc root loc
        | None -> loc
      in
      match loc.source with
      | Some x -> JSON_String (string_of_filename x)
      | None -> JSON_Null
    );
    "type", (match loc.source with
    | Some LibFile _ -> JSON_String "LibFile"
    | Some SourceFile _ -> JSON_String "SourceFile"
    | Some JsonFile _ -> JSON_String "JsonFile"
    | Some ResourceFile _ -> JSON_String "ResourceFile"
    | Some Builtins -> JSON_String "Builtins"
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

let mk_reason_with_test_id test_id desc loc ?(origin=None) () = {
  test_id;
  derivable = false;
  desc;
  loc;
  origin;
}

(* The current test_id is included in every new reason. *)
let mk_reason desc loc =
  mk_reason_with_test_id (TestID.current()) desc loc ()

(* Lift a string to a reason. Usually used as a dummy reason. *)
let locationless_reason desc =
  mk_reason_with_test_id None desc Loc.none ()

let loc_of_reason r = r.loc

let function_desc_prefix = function
  | RAsync -> "async "
  | RGenerator -> "generator "
  | RAsyncGenerator -> "async generator "
  | RNormal -> ""

let rec string_of_desc = function
  | RNumber -> "number"
  | RString -> "string"
  | RBoolean -> "boolean"
  | RMixed -> "mixed"
  | REmpty -> "empty"
  | RAny -> "any"
  | RVoid -> "undefined"
  | RNull -> "null"
  | RStringLit x -> spf "string literal `%s`" x
  | RNumberLit x -> spf "number literal `%s`" x
  | RBooleanLit b -> spf "boolean literal `%s`" (string_of_bool b)
  | RObject -> "object"
  | RObjectLit -> "object literal"
  | RObjectType -> "object type"
  | RObjectClassName -> "Object"
  | RArray -> "array"
  | RArrayLit -> "array literal"
  | REmptyArrayLit -> "empty array literal"
  | RArrayType -> "array type"
  | RTupleType -> "tuple type"
  | RTupleElement -> "tuple element"
  | RFunction func -> spf "%sfunction" (function_desc_prefix func)
  | RArrowFunction func -> spf "%sarrow function" (function_desc_prefix func)
  | RFunctionType -> "function type"
  | RFunctionBody -> "function body"
  | RFunctionCall -> "function call"
  | RJSXFunctionCall raw_jsx -> spf "JSX desugared to `%s(...)`" raw_jsx
  | RJSXIdentifier (raw_jsx, name) ->
      spf "JSX desugared to `%s(...)`. identifier %s" raw_jsx name
  | RJSXElement x ->
    (match x with
    | Some x -> spf "JSX element `%s`" x
    | None -> "JSX element")
  | RJSXElementProps x -> spf "props of JSX element `%s`" x
  | RAnyObject -> "any object"
  | RAnyFunction -> "any function"
  | RUnknownString -> "some string with unknown value"
  | RStringEnum -> "string enum"
  | RNumberEnum -> "number enum"
  | RGetterFunction -> "getter function"
  | RSetterFunction -> "setter function"
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
  | RDestructuring -> "destructuring"
  | RConstructor -> "constructor"
  | RConstructorCall -> "constructor call"
  | RReturn -> "return"
  | RRegExp -> "regexp"
  | RSuper -> "`super` pseudo-expression"
  | RNoSuper -> "empty super object"
  | RDummyPrototype -> "empty prototype object"
  | RDummyThis -> "bound `this` in method"
  | RTupleMap -> "tuple map"
  | RObjectMap -> "object map"
  | RObjectMapi -> "object mapi"
  | RType x -> spf "type `%s`" x
  | RTypeParam (x,d) -> spf "type parameter `%s` of %s" x (string_of_desc d)
  | RIdentifier x -> spf "identifier `%s`" x
  | RIdentifierAssignment x -> spf "assignment of identifier `%s`" x
  | RMethodCall (Some x) -> spf "call of method `%s`" x
  | RMethodCall None -> "call of computed property"
  | RParameter x -> spf "parameter `%s`" x
  | RRestParameter x -> spf "rest parameter `%s`" x
  | RProperty (Some x) -> spf "property `%s`" x
  | RProperty None -> "computed property"
  | RPropertyAssignment x -> spf "assignment of property `%s`" x
  | RShadowProperty x -> spf ".%s" x
  | RPropertyOf (x, d) -> spf "property `%s` of %s" x (string_of_desc d)
  | RPropertyIsAString x -> spf "property `%s` is a string" x
  | RMissingProperty (Some x) -> spf "property `%s` does not exist" x
  | RMissingProperty None -> "computed property does not exist"
  | RUnknownProperty (Some x) -> spf "property `%s` of unknown type" x
  | RUnknownProperty None -> "computed property of unknown type"
  | RSomeProperty -> "some property"
  | RNameProperty d -> spf "property `name` of %s" (string_of_desc d)
  | RMissingAbstract d ->
    spf "undefined. Did you forget to declare %s?" (string_of_desc d)
  | RFieldInitializer x -> spf "field initializer for `%s`" x
  | RCustom x -> x
  | RPolyType d -> spf "polymorphic type: %s" (string_of_desc d)
  | RClassType d -> spf "class type: %s" (string_of_desc d)
  | RExactType d -> spf "exact type: %s" (string_of_desc d)
  | ROptional d -> spf "optional %s" (string_of_desc d)
  | RMaybe d -> spf "?%s" (string_of_desc d)
  | RRestArray d -> spf "rest array of %s" (string_of_desc d)
  | RAbstract d -> spf "abstract %s" (string_of_desc d)
  | RTypeApp d -> spf "type application of %s" (string_of_desc d)
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
  | RIncompatibleInstantiation x -> spf "some incompatible instantiation of `%s`" x
  | RSpreadOf d -> spf "spread of %s" (string_of_desc d)
  | RObjectPatternRestProp -> "rest of object pattern"
  | RArrayPatternRestProp -> "rest of array pattern"

  | RReactElement x ->
    (match x with
    | Some x -> spf "React element `%s`" x
    | None -> "React element")
  | RReactClass -> "React class"
  | RReactComponent -> "React component"
  | RReactStatics -> "statics of React class"
  | RReactDefaultProps -> "default props of React component"
  | RReactState -> "state of React component"
  | RReactComponentProps -> "props of React component"
  | RReactElementProps x -> spf "props of React element `%s`" x
  | RReactPropTypes -> "propTypes of React component"
  | RPropTypeArray -> "array"
  | RPropTypeFunc -> "func"
  | RPropTypeObject -> "object"
  | RPropTypeArrayOf -> "arrayOf"
  | RPropTypeInstanceOf -> "instanceOf"
  | RPropTypeObjectOf -> "objectOf"
  | RPropTypeOneOf -> "oneOf"
  | RPropTypeOneOfType -> "oneOfType"
  | RPropTypeShape -> "shape"
  | RPropTypeFbt -> "Fbd"

let string_of_reason r =
  let spos = string_of_loc (loc_of_reason r) in
  let desc = string_of_desc r.desc in
  if spos = ""
  then desc
  else (
    if desc = ""
    then spos
    else spf "%s:\n%s" spos desc
  )

let json_of_reason ?(strip_root=None) r = Hh_json.(
  JSON_Object [
    "pos", json_of_loc ~strip_root (loc_of_reason r);
    "desc", JSON_String (string_of_desc r.desc)
  ]
)

let dump_reason r =
  spf "%s: %S%s"
    (string_of_loc (loc_of_reason r))
    (string_of_desc r.desc)
    begin match r.test_id with
    | Some n -> spf " (test %d)" n
    | None -> ""
    end

let desc_of_reason r =
  r.desc

let origin_of_reason r =
  r.origin

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
let is_constant_property_reason r =
  match desc_of_reason r with
  | RProperty (Some x)
  | RPropertyOf (x,_)
  | RPropertyIsAString x ->
    let len = String.length x in
    if len = 0
    then false
    else is_not_lowercase x 0 (len - 1)
  | _ -> false

let is_method_call_reason x r =
  match desc_of_reason r with
  | RMethodCall (Some y) -> x = y
  | _ -> false

let is_derivable_reason r =
  r.derivable

let derivable_reason r =
  { r with derivable = true }

let builtin_reason desc =
  mk_reason desc Loc.({ none with source = Some Builtins })
  |> derivable_reason

let is_builtin_reason r =
  Loc.(r.loc.source = Some Builtins)

let is_lib_reason r =
  Loc.(match r.loc.source with
  | Some LibFile _ -> true
  | Some Builtins -> true
  | Some SourceFile _ -> false
  | Some JsonFile _ -> false
  | Some ResourceFile _ -> false
  | None -> false)

let is_blamable_reason r =
  not Loc.(r.loc = none || is_lib_reason r)

let reasons_overlap r1 r2 =
  Loc.(contains r1.loc r2.loc)

(* reason transformers: *)

(* returns reason with new description and position of original *)
let replace_reason f r =
  mk_reason (f (desc_of_reason r)) (loc_of_reason r)

let replace_reason_const desc r =
  mk_reason desc (loc_of_reason r)

(* returns reason with new location and description of original *)
let repos_reason loc reason =
  mk_reason_with_test_id reason.test_id (desc_of_reason reason) loc
    ~origin:(origin_of_reason reason) ()

let update_origin_of_reason origin reason =
  { reason with origin = origin }

let strip_root root reason =
  let loc = strip_root_from_loc root (loc_of_reason reason) in
  repos_reason loc reason
