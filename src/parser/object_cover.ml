(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast
open Parser_common
open Parser_env

module type COVER = sig
  val as_expression : env -> object_cover -> Expression.t
  val as_pattern : ?err:Parse_error.t -> env -> object_cover -> Pattern.t
end

module Cover
  (Parse: PARSER)
: COVER = struct
  let as_expression env = function
    | Cover_expr expr -> expr
    | Cover_patt (expr, assignment_locs) ->
        List.iter (fun loc -> error_at env (loc, Parse_error.UnexpectedToken "=")) assignment_locs;
        expr

  let as_pattern ?(err = Parse_error.InvalidLHSInAssignment) env cover =
    let expr = match cover with
    | Cover_expr expr -> expr
    | Cover_patt (expr, _assignment_locs) -> expr
    in

    if not (Parse.is_assignable_lhs expr)
    then error_at env (fst expr, err);

    (match expr with
    | loc, Ast.Expression.Identifier (_, name)
      when is_restricted name ->
        strict_error_at env (loc, Error.StrictLHSAssignment)
    | _ -> ());

    Parse.pattern_from_expr env expr
end
