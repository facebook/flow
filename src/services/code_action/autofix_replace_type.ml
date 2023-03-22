(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target f =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target as super

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
