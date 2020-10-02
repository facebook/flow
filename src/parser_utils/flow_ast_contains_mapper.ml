(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This mapper prunes expression and statements that are not relevent
   to a target from being mapped over and rebuilt. It doesn't prune the
   space as much as is possible, but doing so results in a much less
   maintainable piece of code. *)
class virtual ['L] mapper =
  object (this)
    inherit ['L] Flow_ast_mapper.mapper as super

    method virtual loc_annot_contains_target : 'L -> bool

    method! program ((l, _) as x) =
      if this#loc_annot_contains_target l then
        super#program x
      else
        x

    method! statement ((l, _) as x) =
      if this#loc_annot_contains_target l then
        super#statement x
      else
        x

    method! comment ((l, _) as x) =
      if this#loc_annot_contains_target l then
        super#comment x
      else
        x

    method! expression ((l, _) as x) =
      if this#loc_annot_contains_target l then
        super#expression x
      else
        x
  end
