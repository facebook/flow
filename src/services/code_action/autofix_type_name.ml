(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target_loc ~incorrect_name ~replacement_name =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method! generic_type loc t =
      let open Flow_ast.Type in
      let { Generic.id; targs; comments } = t in
      if Option.is_none targs || (not @@ this#is_target loc) then
        super#generic_type loc t
      else
        match id with
        | Generic.Identifier.Unqualified
            (id_loc, { Flow_ast.Identifier.name; comments = id_comments })
          when name = incorrect_name ->
          let id =
            Generic.Identifier.Unqualified
              (id_loc, { Flow_ast.Identifier.name = replacement_name; comments = id_comments })
          in
          { Generic.id; targs; comments }
        | _ -> super#generic_type loc t
  end

let convert_type kind ast loc =
  let incorrect_name = Error_message.IncorrectType.incorrect_of_kind kind in
  let replacement_name = Error_message.IncorrectType.replacement_of_kind kind in
  let mapper = new mapper loc ~incorrect_name ~replacement_name in
  mapper#program ast
