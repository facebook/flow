(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast
open Parser_common
open Parser_env

module type COVER = sig
  val as_expression : env -> pattern_cover -> (Loc.t, Loc.t) Expression.t

  val as_pattern : ?err:Parse_error.t -> env -> pattern_cover -> (Loc.t, Loc.t) Pattern.t

  val empty_errors : pattern_errors

  val rev_append_errors : pattern_errors -> pattern_errors -> pattern_errors

  val rev_errors : pattern_errors -> pattern_errors
end

module Cover (Parse : PARSER) : COVER = struct
  let as_expression env = function
    | Cover_expr expr -> expr
    | Cover_patt (expr, { if_expr; if_patt = _ }) ->
      List.iter (error_at env) if_expr;
      expr

  let as_pattern ?(err = Parse_error.InvalidLHSInAssignment) env cover =
    let expr =
      match cover with
      | Cover_expr expr -> expr
      | Cover_patt (expr, { if_expr = _; if_patt }) ->
        List.iter (error_at env) if_patt;
        expr
    in
    if not (Parse.is_assignable_lhs expr) then error_at env (fst expr, err);

    (match expr with
    | (loc, Flow_ast.Expression.Identifier (_, { Flow_ast.Identifier.name; comments = _ }))
      when is_restricted name ->
      strict_error_at env (loc, Parse_error.StrictLHSAssignment)
    | _ -> ());

    Parse.pattern_from_expr env expr

  let empty_errors = { if_patt = []; if_expr = [] }

  let rev_append_errors a b =
    { if_patt = List.rev_append a.if_patt b.if_patt; if_expr = List.rev_append a.if_expr b.if_expr }

  let rev_errors a = { if_patt = List.rev a.if_patt; if_expr = List.rev a.if_expr }
end
