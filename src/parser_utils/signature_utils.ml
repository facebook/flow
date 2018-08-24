(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_visitor

module Procedure_decider = struct
  class decider = object(this)
    inherit [bool] visitor ~init:true

    method private no =
      this#update_acc (fun _ -> false)

    method! function_ _loc (expr: (Loc.t, Loc.t) Flow_ast.Function.t) =
      expr

    method! return _loc (stmt: (Loc.t, Loc.t) Flow_ast.Statement.Return.t) =
      let open Flow_ast.Statement.Return in
      let { argument } = stmt in
      begin match argument with
        | None -> ()
        | Some _ -> this#no
      end;
      stmt

    method! function_body_any (body: (Loc.t, Loc.t) Flow_ast.Function.body) =
      begin match body with
        | Flow_ast.Function.BodyBlock (loc, block) ->
          ignore @@ this#function_body loc block
        | Flow_ast.Function.BodyExpression _ ->
          this#no
      end;
      body

  end

  let is (body: (Loc.t, Loc.t) Flow_ast.Function.body) =
    let decider = new decider in
    decider#eval decider#function_body_any body

end
