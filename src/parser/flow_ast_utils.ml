(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast

type 'loc binding = 'loc * string

type 'loc ident = 'loc * string [@@deriving show]

type 'loc source = 'loc * string [@@deriving show]

let rec fold_bindings_of_pattern =
  Pattern.(
    let property f acc =
      Object.(
        function
        | Property (_, { Property.pattern = p; _ })
        | RestElement (_, { RestElement.argument = p; comments = _ }) ->
          fold_bindings_of_pattern f acc p)
    in
    let element f acc =
      Array.(
        function
        | Hole _ -> acc
        | Element (_, { Element.argument = p; default = _ })
        | RestElement (_, { RestElement.argument = p; comments = _ }) ->
          fold_bindings_of_pattern f acc p)
    in
    fun f acc -> function
      | (_, Identifier { Identifier.name; annot; _ }) -> f acc name annot
      | (_, Object { Object.properties; _ }) -> List.fold_left (property f) acc properties
      | (_, Array { Array.elements; _ }) -> List.fold_left (element f) acc elements
      (* This is for assignment and default param destructuring `[a.b=1]=c`, ignore these for now. *)
      | (_, Expression _) -> acc)

let fold_bindings_of_variable_declarations f acc declarations =
  let open Flow_ast.Statement.VariableDeclaration in
  List.fold_left
    (fun acc -> function
      | (_, { Declarator.id = pattern; _ }) -> fold_bindings_of_pattern f acc pattern)
    acc
    declarations

let partition_directives statements =
  let open Flow_ast.Statement in
  let rec helper directives = function
    | ((_, Expression { Expression.directive = Some _; _ }) as directive) :: rest ->
      helper (directive :: directives) rest
    | rest -> (List.rev directives, rest)
  in
  helper [] statements

let negate_number_literal (value, raw) =
  let raw_len = String.length raw in
  let raw =
    if raw_len > 0 && raw.[0] = '-' then
      String.sub raw 1 (raw_len - 1)
    else
      "-" ^ raw
  in
  (~-.value, raw)

let loc_of_statement = fst

let loc_of_expression = fst

let loc_of_pattern = fst

let loc_of_ident = fst

let name_of_ident (_, { Identifier.name; comments = _ }) = name

let source_of_ident (loc, { Identifier.name; comments = _ }) = (loc, name)

let ident_of_source ?comments (loc, name) = (loc, { Identifier.name; comments })

let mk_comments ?(leading = []) ?(trailing = []) a = { Syntax.leading; trailing; internal = a }

let mk_comments_opt ?(leading = []) ?(trailing = []) () =
  match (leading, trailing) with
  | ([], []) -> None
  | (_, _) -> Some (mk_comments ~leading ~trailing ())

let mk_comments_with_internal_opt ?(leading = []) ?(trailing = []) ~internal =
  match (leading, trailing, internal) with
  | ([], [], []) -> None
  | _ -> Some (mk_comments ~leading ~trailing internal)

let merge_comments ~inner ~outer =
  let open Syntax in
  match (inner, outer) with
  | (None, c)
  | (c, None) ->
    c
  | (Some inner, Some outer) ->
    mk_comments_opt
      ~leading:(outer.leading @ inner.leading)
      ~trailing:(inner.trailing @ outer.trailing)
      ()

let merge_comments_with_internal ~inner ~outer =
  match (inner, outer) with
  | (inner, None) -> inner
  | (None, Some { Syntax.leading; trailing; _ }) ->
    mk_comments_with_internal_opt ~leading ~trailing ~internal:[]
  | ( Some { Syntax.leading = inner_leading; trailing = inner_trailing; internal },
      Some { Syntax.leading = outer_leading; trailing = outer_trailing; _ } ) ->
    mk_comments_with_internal_opt
      ~leading:(outer_leading @ inner_leading)
      ~trailing:(inner_trailing @ outer_trailing)
      ~internal

let split_comments comments =
  match comments with
  | None -> (None, None)
  | Some { Syntax.leading; trailing; _ } ->
    (mk_comments_opt ~leading (), mk_comments_opt ~trailing ())

let string_of_assignment_operator op =
  let open Flow_ast.Expression.Assignment in
  match op with
  | PlusAssign -> "+="
  | MinusAssign -> "-="
  | MultAssign -> "*="
  | ExpAssign -> "**="
  | DivAssign -> "/="
  | ModAssign -> "%="
  | LShiftAssign -> "<<="
  | RShiftAssign -> ">>="
  | RShift3Assign -> ">>>="
  | BitOrAssign -> "|="
  | BitXorAssign -> "^="
  | BitAndAssign -> "&="

let string_of_binary_operator op =
  let open Flow_ast.Expression.Binary in
  match op with
  | Equal -> "=="
  | NotEqual -> "!="
  | StrictEqual -> "==="
  | StrictNotEqual -> "!=="
  | LessThan -> "<"
  | LessThanEqual -> "<="
  | GreaterThan -> ">"
  | GreaterThanEqual -> ">="
  | LShift -> "<<"
  | RShift -> ">>"
  | RShift3 -> ">>>"
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Exp -> "**"
  | Div -> "/"
  | Mod -> "%"
  | BitOr -> "|"
  | Xor -> "^"
  | BitAnd -> "&"
  | In -> "in"
  | Instanceof -> "instanceof"

module ExpressionSort = struct
  type t =
    | Array
    | ArrowFunction
    | Assignment
    | Binary
    | Call
    | Class
    | Comprehension
    | Conditional
    | Function
    | Generator
    | Identifier
    | Import
    | JSXElement
    | JSXFragment
    | Literal
    | Logical
    | Member
    | MetaProperty
    | New
    | Object
    | OptionalCall
    | OptionalMember
    | Sequence
    | Super
    | TaggedTemplate
    | TemplateLiteral
    | This
    | TypeCast
    | Unary
    | Update
    | Yield
  [@@deriving show]

  let to_string = function
    | Array -> "array"
    | ArrowFunction -> "arrow function"
    | Assignment -> "assignment expression"
    | Binary -> "binary expression"
    | Call -> "call expression"
    | Class -> "class"
    | Comprehension -> "comprehension expression"
    | Conditional -> "conditional expression"
    | Function -> "function"
    | Generator -> "generator"
    | Identifier -> "identifier"
    | Import -> "import expression"
    | JSXElement -> "JSX element"
    | JSXFragment -> "JSX fragment"
    | Literal -> "literal"
    | Logical -> "logical expression"
    | Member -> "member expression"
    | MetaProperty -> "metaproperty expression"
    | New -> "new expression"
    | Object -> "object"
    | OptionalCall -> "optional call expression"
    | OptionalMember -> "optional member expression"
    | Sequence -> "sequence"
    | Super -> "`super` reference"
    | TaggedTemplate -> "tagged template expression"
    | TemplateLiteral -> "template literal"
    | This -> "`this` reference"
    | TypeCast -> "type cast"
    | Unary -> "unary expression"
    | Update -> "update expression"
    | Yield -> "yield expression"
end
