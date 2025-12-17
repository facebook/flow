(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper ~record_name target_loc =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method! expression expr =
      let open Flow_ast.Expression in
      let (loc, expr') = expr in
      if this#is_target loc then
        match expr' with
        | Object obj ->
          let constructor =
            ( Loc.none,
              Identifier (Loc.none, { Flow_ast.Identifier.name = record_name; comments = None })
            )
          in
          ( loc,
            Record { Record.constructor; targs = None; properties = (loc, obj); comments = None }
          )
        | _ -> super#expression expr
      else
        super#expression expr
  end

let convert_object_to_record_expression ~record_name ast loc =
  let mapper = new mapper ~record_name loc in
  mapper#program ast
