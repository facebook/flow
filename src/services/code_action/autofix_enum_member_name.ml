(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target fixed_name =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target as super

    method! enum_member_identifier ((id_loc, id) as identifier) =
      if this#is_target id_loc then
        (id_loc, { id with Flow_ast.Identifier.name = fixed_name })
      else
        super#enum_member_identifier identifier
  end

let capitalize_at_target ~fixed_name ast loc =
  let mapper = new mapper loc fixed_name in
  mapper#program ast
