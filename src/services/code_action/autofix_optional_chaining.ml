(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target as super

    method! expression e =
      let open Flow_ast.Expression in
      match e with
      | (loc, Member m) when this#is_target loc ->
        (loc, OptionalMember { OptionalMember.member = m; optional = true; filtered_out = loc })
      | _ -> super#expression e
  end

let add_optional_chaining ast loc =
  let mapper = new mapper loc in
  mapper#program ast
