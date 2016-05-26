(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
| Error
| EndOfFile
| Name
| QualifiedName
| Variable
(* Keywords *)
| Abstract
| Array
| Arraykey
| As
| Async
| Await
| Bool
| Break
| Case
| Catch
| Class
| Classname
| Clone
| Const
| Continue
| Default
| Do
| Echo
| Else
| Elseif
| Enum
| Extends
| Float
| Final
| Finally
| For
| Foreach
| Function
| If
| Implements
| Instanceof
| Insteadof
| Int
| Interface
| Mixed
| Namespace
| New
| Newtype
| Noreturn
| Num
| Parent
| Private
| Protected
| Public
| Require
| Require_once
| Resource
| Return
| Self
| Shape
| Static
| String
| Switch
| This
| Throw
| Trait
| Try
| Tuple
| Type
| Use
| Void
| While
| Yield
(* Punctuation *)
| LeftBracket
| RightBracket
| LeftParen
| RightParen
| LeftBrace
| RightBrace
| Dot
| MinusGreaterThan
| PlusPlus
| MinusMinus
| StarStar
| Star
| Plus
| Minus
| Tilde
| Exclamation
| Dollar
| Slash
| Percent
| LessThanLessThan
| GreaterThanGreaterThan
| LessThan
| GreaterThan
| LessThanEqual
| GreaterThanEqual
| EqualEqual
| EqualEqualEqual
| ExclamationEqual
| ExclamationEqualEqual
| Carat
| Bar
| Ampersand
| AmpersandAmpersand
| BarBar
| Question
| QuestionQuestion
| Colon
| Semicolon
| Equal
| StarStarEqual
| StarEqual
| SlashEqual
| PercentEqual
| PlusEqual
| MinusEqual
| DotEqual
| LessThanLessThanEqual
| GreaterThanGreaterThanEqual
| AmpersandEqual
| CaratEqual
| BarEqual
| Comma
| At
| ColonColon
| EqualGreaterThan
| EqualEqualGreaterThan
| QuestionMinusGreaterThan
| Backslash
| DotDotDot
| DollarDollar
| BarGreaterThan
(* Literals *)
| DecimalLiteral
| OctalLiteral
| HexadecimalLiteral
| BinaryLiteral
| FloatingLiteral
| SingleQuotedStringLiteral
| DoubleQuotedStringLiteral
| HeredocStringLiteral
| NowdocStringLiteral
| BooleanLiteral
| NullLiteral
(* XHP *)
| XHPElementName
| XHPStringLiteral
| XHPBody
| SlashGreaterThan
| LessThanSlash


let from_string keyword =
  match keyword with
  | "abstract" -> Some Abstract
  | "array" -> Some Array
  | "arraykey" -> Some Arraykey
  | "as" -> Some As
  | "async" -> Some Async
  | "await" -> Some Await
  | "bool" -> Some Bool
  | "break" -> Some Break
  | "case" -> Some Case
  | "catch" -> Some Catch
  | "class" -> Some Class
  | "classname" -> Some Classname
  | "clone" -> Some Clone
  | "const" -> Some Const
  | "continue" -> Some Continue
  | "default" -> Some Default
  | "do" -> Some Do
  | "echo" -> Some Echo
  | "else" -> Some Else
  | "elseif" -> Some Elseif
  | "enum" -> Some Enum
  | "extends" -> Some Extends
  | "false" -> Some BooleanLiteral
  | "float" -> Some Float
  | "final" -> Some Final
  | "finally" -> Some Finally
  | "for" -> Some For
  | "foreach" -> Some Foreach
  | "function" -> Some Function
  | "if" -> Some If
  | "implements" -> Some Implements
  | "instanceof" -> Some Instanceof
  | "insteadof" -> Some Insteadof
  | "int" -> Some Int
  | "interface" -> Some Interface
  | "mixed" -> Some Mixed
  | "namespace" -> Some Namespace
  | "new" -> Some New
  | "newtype" -> Some Newtype
  | "noreturn" -> Some Noreturn
  | "null" -> Some NullLiteral
  | "num" -> Some Num
  | "parent" -> Some Parent
  | "private" -> Some Private
  | "protected" -> Some Protected
  | "public" -> Some Public
  | "require" -> Some Require
  | "require_once" -> Some Require_once
  | "resource" -> Some Resource
  | "return" -> Some Return
  | "self" -> Some Self
  | "shape" -> Some Shape
  | "static" -> Some Static
  | "string" -> Some String
  | "switch" -> Some Switch
  | "this" -> Some This
  | "throw" -> Some Throw
  | "trait" -> Some Trait
  | "true" -> Some BooleanLiteral
  | "try" -> Some Try
  | "tuple" -> Some Tuple
  | "type" -> Some Type
  | "use" -> Some Use
  | "void" -> Some Void
  | "while" -> Some While
  | "yield" -> Some Yield
  | _ -> None

let to_string kind =
  match kind with
  | Abstract -> "abstract"
  | Array -> "array"
  | Arraykey -> "arraykey"
  | As -> "as"
  | Async -> "async"
  | Await -> "await"
  | Bool -> "bool"
  | Break -> "break"
  | Case -> "case"
  | Catch -> "catch"
  | Class -> "class"
  | Classname -> "classname"
  | Clone -> "clone"
  | Const -> "const"
  | Continue -> "continue"
  | Default -> "default"
  | Do -> "do"
  | Echo -> "echo"
  | Else -> "else"
  | Elseif -> "elseif"
  | Enum -> "enum"
  | Extends -> "extends"
  | Float -> "float"
  | Final -> "final"
  | Finally -> "finally"
  | For -> "for"
  | Foreach -> "foreach"
  | Function -> "function"
  | If -> "if"
  | Implements -> "implements"
  | Instanceof -> "instanceof"
  | Insteadof -> "insteadof"
  | Int -> "int"
  | Interface -> "interface"
  | Mixed -> "mixed"
  | Namespace -> "namespace"
  | New -> "new"
  | Newtype -> "newtype"
  | Noreturn -> "noreturn"
  | Num -> "num"
  | Parent -> "parent"
  | Private -> "private"
  | Protected -> "protected"
  | Public -> "public"
  | Require -> "require"
  | Require_once -> "require_once"
  | Resource -> "resource"
  | Return -> "return"
  | Self -> "self"
  | Shape -> "shape"
  | Static -> "static"
  | String -> "string"
  | Switch -> "switch"
  | This -> "this"
  | Throw -> "throw"
  | Trait -> "trait"
  | Try -> "try"
  | Tuple -> "tuple"
  | Type -> "type"
  | Use -> "use"
  | Void -> "void"
  | While -> "while"
  | Yield -> "yield"
  | LeftBracket -> "["
  | RightBracket -> "]"
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Dot -> "."
  | MinusGreaterThan -> "->"
  | PlusPlus -> "++"
  | MinusMinus -> "--"
  | StarStar -> "**"
  | Star -> "*"
  | Plus -> "+"
  | Minus -> "-"
  | Tilde -> "~"
  | Exclamation -> "!"
  | Dollar -> "$"
  | Slash -> "/"
  | Percent -> "%"
  | LessThanLessThan -> "<<"
  | GreaterThanGreaterThan -> ">>"
  | LessThan -> "<"
  | GreaterThan -> ">"
  | LessThanEqual -> "<="
  | GreaterThanEqual -> ">="
  | EqualEqual -> "=="
  | EqualEqualEqual -> "==="
  | ExclamationEqual -> "!="
  | ExclamationEqualEqual -> "!=="
  | Carat -> "^"
  | Bar -> "|"
  | Ampersand -> "&"
  | AmpersandAmpersand -> "&&"
  | BarBar -> "||"
  | Question -> "?"
  | QuestionQuestion -> "??"
  | Colon -> ":"
  | Semicolon -> ";"
  | Equal -> "="
  | StarStarEqual -> "**="
  | StarEqual -> "*="
  | SlashEqual -> "/="
  | PercentEqual -> "%="
  | PlusEqual -> "+="
  | MinusEqual -> "-="
  | DotEqual -> ".="
  | LessThanLessThanEqual -> "<<="
  | GreaterThanGreaterThanEqual -> ">>="
  | AmpersandEqual -> "&="
  | CaratEqual -> "^="
  | BarEqual -> "|="
  | Comma -> ","
  | At -> "@"
  | ColonColon -> "::"
  | EqualGreaterThan -> "=>"
  | EqualEqualGreaterThan -> "==>"
  | QuestionMinusGreaterThan -> "?->"
  | Backslash -> "\\"
  | DotDotDot -> "..."
  | DollarDollar -> "$$"
  | BarGreaterThan -> "|>"
  | Error -> "error"
  | EndOfFile -> "end of file"
  | Name -> "name"
  | QualifiedName -> "qualified_name"
  | Variable -> "variable"
  | DecimalLiteral -> "decimal_literal"
  | OctalLiteral -> "octal_literal"
  | HexadecimalLiteral -> "hexadecimal_literal"
  | BinaryLiteral -> "binary_literal"
  | FloatingLiteral -> "floating_literal"
  | SingleQuotedStringLiteral -> "single_quoted_string_literal"
  | DoubleQuotedStringLiteral -> "double_quoted_string_literal"
  | HeredocStringLiteral -> "heredoc_string_literal"
  | NowdocStringLiteral -> "nowdoc_string_literal"
  | BooleanLiteral -> "boolean_literal"
  | NullLiteral -> "null_literal"
  | XHPElementName -> "XHP_element_name"
  | XHPStringLiteral -> "XHP_string_literal"
  | XHPBody -> "XHP_body"
  | SlashGreaterThan -> "/>"
  | LessThanSlash -> "</"
