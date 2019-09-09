(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast

type 'loc binding = 'loc * string

type 'loc ident = 'loc * string

type 'loc source = 'loc * string

let rec fold_bindings_of_pattern =
  Pattern.(
    let property f acc =
      Object.(
        function
        | Property (_, { Property.pattern = (_, p); _ })
        | RestProperty (_, { RestProperty.argument = (_, p) }) ->
          fold_bindings_of_pattern f acc p)
    in
    let element f acc =
      Array.(
        function
        | None -> acc
        | Some (Element (_, { Element.argument = (_, p); default = _ }))
        | Some (RestElement (_, { RestElement.argument = (_, p) })) ->
          fold_bindings_of_pattern f acc p)
    in
    fun f acc -> function
      | Identifier { Identifier.name; _ } -> f acc name
      | Object { Object.properties; _ } -> List.fold_left (property f) acc properties
      | Array { Array.elements; _ } -> List.fold_left (element f) acc elements
      | Expression _ -> failwith "expression pattern")

let fold_bindings_of_variable_declarations f acc declarations =
  Flow_ast.Statement.VariableDeclaration.(
    List.fold_left
      (fun acc -> function
        | (_, { Declarator.id = (_, pattern); _ }) -> fold_bindings_of_pattern f acc pattern)
      acc
      declarations)

let partition_directives statements =
  Flow_ast.Statement.(
    let rec helper directives = function
      | ((_, Expression { Expression.directive = Some _; _ }) as directive) :: rest ->
        helper (directive :: directives) rest
      | rest -> (List.rev directives, rest)
    in
    helper [] statements)

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

let ident_of_source (loc, name) = (loc, { Identifier.name; comments = None })

let mk_comments ?(leading = []) ?(trailing = []) a = { Syntax.leading; trailing; internal = a }

let mk_comments_opt ?(leading = []) ?(trailing = []) () =
  match (leading, trailing) with
  | ([], []) -> None
  | (_, _) -> Some (mk_comments ~leading ~trailing ())

let string_of_assignment_operator op =
  Flow_ast.Expression.Assignment.(
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
    | BitAndAssign -> "&=")

let string_of_binary_operator op =
  Flow_ast.Expression.Binary.(
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
    | Instanceof -> "instanceof")

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
