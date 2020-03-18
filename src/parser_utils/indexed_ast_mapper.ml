(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class index_mapper =
  object
    inherit [Loc.t, Loc.t, ILoc.t, ILoc.t] Flow_polymorphic_ast_mapper.mapper

    val mutable counter = 0

    method on_loc_annot loc =
      let c = counter in
      counter <- counter + 1;
      (loc, c)

    method on_type_annot loc =
      let c = counter in
      counter <- counter + 1;
      (loc, c)
  end

class unindex_mapper =
  object
    inherit [ILoc.t, ILoc.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot (loc, _id) = loc

    method on_type_annot (loc, _id) = loc
  end
