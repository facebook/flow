(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Full_fidelity_token_kind

type t =
| LambdaOperator
| IndexingOperator
| FunctionCallOperator
| YieldOperator
| AwaitOperator
| PipeOperator
| ConditionalQuestionOperator
| ConditionalColonOperator
| CoalesceOperator
| LogicalOrOperator
| ExclusiveOrOperator
| LogicalAndOperator
| OrOperator
| AndOperator
| EqualOperator
| StrictEqualOperator
| NotEqualOperator
| StrictNotEqualOperator
| LessThanOperator
| LessThanOrEqualOperator
| GreaterThanOperator
| GreaterThanOrEqualOperator
| LeftShiftOperator
| RightShiftOperator
| AdditionOperator
| SubtractionOperator
| ConcatenationOperator
| MultiplicationOperator
| DivisionOperator
| RemainderOperator
| LogicalNotOperator
| InstanceofOperator
| NotOperator
| PrefixIncrementOperator
| PrefixDecrementOperator
| PostfixIncrementOperator
| PostfixDecrementOperator
| CastOperator
| ExponentOperator
| ReferenceOperator
| ErrorControlOperator
| NewOperator
| CloneOperator
| AssignmentOperator
| AdditionAssignmentOperator
| SubtractionAssignmentOperator
| MultiplicationAssignmentOperator
| DivisionAssignmentOperator
| ConcatenationAssignmentOperator
| RemainderAssignmentOperator
| AndAssignmentOperator
| OrAssignmentOperator
| ExclusiveOrAssignmentOperator
| LeftShiftAssignmentOperator
| RightShiftAssignmentOperator
| MemberSelectionOperator
| NullSafeMemberSelectionOperator
| ScopeResolutionOperator
| UnaryPlusOperator
| UnaryMinusOperator

type assoc =
| LeftAssociative
| RightAssociative
| NotAssociative

let precedence operator =
  (* TODO: Import, eval *)
  (* TODO: Comma *)
  (* TODO: print *)
  (* TODO: elseif *)
  (* TODO: else *)
  (* TODO: endif *)
  (* TODO: variable operator $ *)
  match operator with

  | YieldOperator
  | AssignmentOperator | AdditionAssignmentOperator
  | SubtractionAssignmentOperator | MultiplicationAssignmentOperator
  | DivisionAssignmentOperator | ConcatenationAssignmentOperator
  | RemainderAssignmentOperator | AndAssignmentOperator
  | OrAssignmentOperator | ExclusiveOrAssignmentOperator
  | LeftShiftAssignmentOperator | RightShiftAssignmentOperator
  | LambdaOperator -> 1
  | PipeOperator -> 2
  | ConditionalQuestionOperator | ConditionalColonOperator -> 3
  | CoalesceOperator -> 4
  | LogicalOrOperator -> 5
  | LogicalAndOperator -> 6
  | OrOperator -> 7
  | ExclusiveOrOperator -> 8
  | AndOperator -> 9
  | EqualOperator | StrictEqualOperator
  | NotEqualOperator | StrictNotEqualOperator -> 10
  | LessThanOperator | LessThanOrEqualOperator
  | GreaterThanOperator | GreaterThanOrEqualOperator -> 11
  | LeftShiftOperator | RightShiftOperator -> 12
  | AdditionOperator | SubtractionOperator | ConcatenationOperator -> 13
  | MultiplicationOperator | DivisionOperator | RemainderOperator -> 14
  | InstanceofOperator -> 15
  | AwaitOperator| CastOperator
  | ReferenceOperator | ErrorControlOperator
  | PrefixIncrementOperator | PrefixDecrementOperator
  | LogicalNotOperator| NotOperator
  | UnaryPlusOperator | UnaryMinusOperator -> 16
  | NewOperator | CloneOperator
  | ExponentOperator
  | FunctionCallOperator
  | MemberSelectionOperator | NullSafeMemberSelectionOperator
  | PostfixIncrementOperator | PostfixDecrementOperator
  | IndexingOperator
  | ScopeResolutionOperator -> 17

let associativity operator =
  match operator with
  | EqualOperator | StrictEqualOperator | NotEqualOperator
  | StrictNotEqualOperator | LessThanOperator | LessThanOrEqualOperator
  | GreaterThanOperator | GreaterThanOrEqualOperator | InstanceofOperator
  | YieldOperator | NewOperator | CloneOperator | AwaitOperator
    -> NotAssociative

  | PipeOperator | ConditionalQuestionOperator | ConditionalColonOperator
  | LogicalOrOperator | ExclusiveOrOperator | LogicalAndOperator
  | OrOperator | AndOperator | LeftShiftOperator | RightShiftOperator
  | AdditionOperator | SubtractionOperator | ConcatenationOperator
  | MultiplicationOperator | DivisionOperator | RemainderOperator
  | MemberSelectionOperator | NullSafeMemberSelectionOperator
  | ScopeResolutionOperator | FunctionCallOperator | IndexingOperator
  (* Import, eval *)
  (* Comma *)
  (* elseif *)
  (* else *)
  (* endif *)
  (* variable operator $ *)
    -> LeftAssociative
  | CoalesceOperator| LogicalNotOperator | NotOperator | CastOperator
  | UnaryPlusOperator | UnaryMinusOperator  (* TODO: Correct? *)
  | ErrorControlOperator | ReferenceOperator (* TODO: Correct? *)
  | PostfixIncrementOperator | PostfixDecrementOperator
  | PrefixIncrementOperator | PrefixDecrementOperator | ExponentOperator
  | AssignmentOperator | AdditionAssignmentOperator
  | SubtractionAssignmentOperator | MultiplicationAssignmentOperator
  | DivisionAssignmentOperator | ConcatenationAssignmentOperator
  | RemainderAssignmentOperator | AndAssignmentOperator
  | OrAssignmentOperator | ExclusiveOrAssignmentOperator
  | LeftShiftAssignmentOperator | RightShiftAssignmentOperator
  | LambdaOperator
  (* print *)
    -> RightAssociative

let prefix_unary_from_token token =
  match token with
  | Yield -> YieldOperator
  | Await -> AwaitOperator
  | Exclamation -> LogicalNotOperator
  | Tilde -> NotOperator
  | PlusPlus -> PrefixIncrementOperator
  | MinusMinus -> PrefixDecrementOperator
  | Plus -> UnaryPlusOperator
  | Minus -> UnaryMinusOperator
  | Ampersand -> ReferenceOperator
  | At -> ErrorControlOperator
  | New -> NewOperator
  | Clone -> CloneOperator
  | _ -> failwith "not a unary operator"

(* Is this a token that can appear after an expression? *)
let is_trailing_operator_token token =
  match token with
  | PlusPlus
  | MinusMinus
  | LeftParen
  | LeftBracket
  | LeftBrace
  | Plus
  | Minus
  | Ampersand
  | Colon
  | EqualEqualGreaterThan
  | BarGreaterThan
  | Question
  | QuestionQuestion
  | BarBar
  | Carat
  | AmpersandAmpersand
  | Bar
  | EqualEqual
  | EqualEqualEqual
  | ExclamationEqual
  | ExclamationEqualEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | LessThanLessThan
  | GreaterThanGreaterThan
  | Dot
  | Star
  | Slash
  | Percent
  | Instanceof
  | StarStar
  | Equal
  | PlusEqual
  | MinusEqual
  | StarEqual
  | SlashEqual
  | DotEqual
  | PercentEqual
  | AmpersandEqual
  | BarEqual
  | CaratEqual
  | LessThanLessThanEqual
  | GreaterThanGreaterThanEqual
  | MinusGreaterThan
  | QuestionMinusGreaterThan
  | ColonColon -> true
  | _ -> false

let trailing_from_token token =
  match token with
  | BarGreaterThan -> PipeOperator
  | Question -> ConditionalQuestionOperator
  | Colon -> ConditionalColonOperator
  | QuestionQuestion -> CoalesceOperator
  | BarBar -> LogicalOrOperator
  | Carat -> ExclusiveOrOperator
  | AmpersandAmpersand -> LogicalAndOperator
  | Bar -> OrOperator
  | Ampersand -> AndOperator
  | EqualEqual -> EqualOperator
  | EqualEqualEqual -> StrictEqualOperator
  | ExclamationEqual -> NotEqualOperator
  | ExclamationEqualEqual -> StrictNotEqualOperator
  | LessThan -> LessThanOperator
  | LessThanEqual -> LessThanOrEqualOperator
  | GreaterThan -> GreaterThanOperator
  | GreaterThanEqual -> GreaterThanOrEqualOperator
  | LessThanLessThan -> LeftShiftOperator
  | GreaterThanGreaterThan -> RightShiftOperator
  | Plus -> AdditionOperator
  | Minus -> SubtractionOperator
  | Dot -> ConcatenationOperator
  | Star -> MultiplicationOperator
  | Slash -> DivisionOperator
  | Percent -> RemainderOperator
  | Instanceof -> InstanceofOperator
  | StarStar -> ExponentOperator
  | Equal -> AssignmentOperator
  | PlusEqual -> AdditionAssignmentOperator
  | MinusEqual -> SubtractionAssignmentOperator
  | StarEqual -> MultiplicationAssignmentOperator
  | SlashEqual -> DivisionAssignmentOperator
  | DotEqual -> ConcatenationAssignmentOperator
  | PercentEqual -> RemainderAssignmentOperator
  | AmpersandEqual -> AndAssignmentOperator
  | BarEqual -> OrAssignmentOperator
  | CaratEqual -> ExclusiveOrAssignmentOperator
  | LessThanLessThanEqual -> LeftShiftAssignmentOperator
  | GreaterThanGreaterThanEqual -> RightShiftAssignmentOperator
  | MinusGreaterThan -> MemberSelectionOperator
  | QuestionMinusGreaterThan -> NullSafeMemberSelectionOperator
  | ColonColon -> ScopeResolutionOperator
  | PlusPlus -> PostfixIncrementOperator
  | MinusMinus -> PostfixDecrementOperator
  | LeftParen -> FunctionCallOperator
  | LeftBracket -> IndexingOperator
  | LeftBrace -> IndexingOperator
  | _ -> failwith "not a trailing operator"

let is_binary_operator_token token =
  match token with
  | Plus
  | Minus
  | Ampersand
  | EqualEqualGreaterThan
  | BarGreaterThan
  | QuestionQuestion
  | BarBar
  | Carat
  | AmpersandAmpersand
  | Bar
  | EqualEqual
  | EqualEqualEqual
  | ExclamationEqual
  | ExclamationEqualEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | LessThanLessThan
  | GreaterThanGreaterThan
  | Dot
  | Star
  | Slash
  | Percent
  | StarStar
  | Equal
  | PlusEqual
  | MinusEqual
  | StarEqual
  | SlashEqual
  | DotEqual
  | PercentEqual
  | AmpersandEqual
  | BarEqual
  | CaratEqual
  | LessThanLessThanEqual
  | GreaterThanGreaterThanEqual
  | MinusGreaterThan
  | QuestionMinusGreaterThan -> true
  | _ -> false

let is_assignment operator =
  match operator with
  | AssignmentOperator
  | AdditionAssignmentOperator
  | SubtractionAssignmentOperator
  | MultiplicationAssignmentOperator
  | DivisionAssignmentOperator
  | ConcatenationAssignmentOperator
  | RemainderAssignmentOperator
  | AndAssignmentOperator
  | OrAssignmentOperator
  | ExclusiveOrAssignmentOperator
  | LeftShiftAssignmentOperator
  | RightShiftAssignmentOperator -> true
  | _ -> false

let to_string kind =
  match kind with
  | LambdaOperator -> "lambda"
  | IndexingOperator -> "indexing"
  | FunctionCallOperator -> "function_call"
  | YieldOperator -> "yield"
  | AwaitOperator -> "await"
  | PipeOperator -> "pipe"
  | ConditionalQuestionOperator -> "conditional"
  | ConditionalColonOperator -> "colon"
  | CoalesceOperator -> "coalesce"
  | LogicalOrOperator -> "logical_or"
  | ExclusiveOrOperator -> "exclusive_or"
  | LogicalAndOperator -> "logical_and"
  | OrOperator -> "or"
  | AndOperator -> "and"
  | EqualOperator -> "equal"
  | StrictEqualOperator -> "strict_equal"
  | NotEqualOperator -> "not_equal"
  | StrictNotEqualOperator -> "strict_not_equal"
  | LessThanOperator -> "less_than"
  | LessThanOrEqualOperator -> "less_than_or_equal"
  | GreaterThanOperator -> "greater_than"
  | GreaterThanOrEqualOperator -> "greater_than_or_equal"
  | LeftShiftOperator -> "left_shift"
  | RightShiftOperator -> "right_shift"
  | AdditionOperator -> "addition"
  | SubtractionOperator -> "subtraction"
  | ConcatenationOperator -> "concatenation"
  | MultiplicationOperator -> "multiplication"
  | DivisionOperator -> "division"
  | RemainderOperator -> "remainder"
  | LogicalNotOperator -> "logical_not"
  | InstanceofOperator -> "instanceof"
  | NotOperator -> "not"
  | PrefixIncrementOperator -> "prefix_increment"
  | PrefixDecrementOperator -> "prefix_decrement"
  | PostfixIncrementOperator -> "postfix_increment"
  | PostfixDecrementOperator -> "postfix_decrement"
  | CastOperator -> "cast"
  | ExponentOperator -> "exponentiation"
  | ReferenceOperator -> "reference"
  | ErrorControlOperator -> "error_control"
  | NewOperator -> "new"
  | CloneOperator -> "clone"
  | AssignmentOperator -> "assignment"
  | AdditionAssignmentOperator -> "addition_assignment"
  | SubtractionAssignmentOperator -> "subtraction_assignment"
  | MultiplicationAssignmentOperator -> "multiplication_assignment"
  | DivisionAssignmentOperator -> "division_assignment"
  | ConcatenationAssignmentOperator -> "concatenation_assignment"
  | RemainderAssignmentOperator -> "reminder_assignment"
  | AndAssignmentOperator -> "and_assignment"
  | OrAssignmentOperator -> "or_assignment"
  | ExclusiveOrAssignmentOperator -> "exclusive_or_assignment"
  | LeftShiftAssignmentOperator -> "left_shift_assignment"
  | RightShiftAssignmentOperator -> "right_shift_assignment"
  | MemberSelectionOperator -> "member_selection"
  | NullSafeMemberSelectionOperator -> "null_safe_member_selection"
  | ScopeResolutionOperator -> "scope_resolution"
  | UnaryPlusOperator -> "unary_plus"
  | UnaryMinusOperator -> "unary_minus"
