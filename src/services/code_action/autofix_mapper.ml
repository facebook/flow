(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target =
  object (this)
    inherit [Loc.t] Flow_ast_contains_mapper.mapper

    method target_contains loc = Loc.contains target loc

    method target_contained_by loc = Loc.contains loc target

    method is_target loc = Loc.equal target loc

    method loc_annot_contains_target = this#target_contained_by
  end
