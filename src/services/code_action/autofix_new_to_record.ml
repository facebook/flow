(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target_loc =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method! expression expr =
      let open Flow_ast.Expression in
      let (loc, expr') = expr in
      if this#is_target loc then
        match expr' with
        | New { New.callee; targs; arguments; comments = _ } ->
          (match arguments with
          | Some (_, { ArgList.arguments = [Expression (obj_loc, Object obj)]; comments = _ }) ->
            ( loc,
              Record
                { Record.constructor = callee; targs; properties = (obj_loc, obj); comments = None }
            )
          | _ -> super#expression expr)
        | _ -> super#expression expr
      else
        super#expression expr
  end

let convert_new_to_record_expression ast loc =
  let mapper = new mapper loc in
  mapper#program ast
