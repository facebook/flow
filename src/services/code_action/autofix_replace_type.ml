(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target f =
  object (this)
    inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

    method private target_contained_by loc = Loc.contains loc target

    method private is_target loc = Loc.equal target loc

    method loc_annot_contains_target = this#target_contained_by

    method! type_ t =
      let (loc, t') = t in
      if this#is_target loc then
        (loc, f t')
      else
        super#type_ t
  end

let replace_type ~f ast loc =
  let mapper = new mapper loc f in
  mapper#program ast
