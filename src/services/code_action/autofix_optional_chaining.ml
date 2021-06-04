(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target =
  object (this)
    inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

    method private target_contained_by loc = Loc.contains loc target

    method private is_target loc = Loc.equal target loc

    method loc_annot_contains_target = this#target_contained_by

    method! expression e =
      let open Flow_ast.Expression in
      match e with
      | (loc, Member m) when this#is_target loc ->
        (loc, OptionalMember { OptionalMember.member = m; optional = true })
      | _ -> super#expression e
  end

let add_optional_chaining ast loc =
  let mapper = new mapper loc in
  mapper#program ast
