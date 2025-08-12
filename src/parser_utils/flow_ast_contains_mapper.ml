(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This mapper prunes expression and statements that are not relevant
   to a target from being mapped over and rebuilt. It doesn't prune the
   space as much as is possible, but doing so results in a much less
   maintainable piece of code. *)
class mapper target =
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method private target_contains loc = Loc.contains target loc

    method private target_contained_by loc = Loc.contains loc target

    method private is_target loc = Loc.equal target loc

    method! program ((l, { Flow_ast.Program.all_comments; _ }) as x) =
      if
        this#target_contained_by l
        || Base.List.exists all_comments ~f:(fun (loc, _) -> this#target_contained_by loc)
      then
        super#program x
      else
        x

    method! statement ((l, _) as x) =
      if this#target_contained_by l then
        super#statement x
      else
        x

    method! comment ((l, _) as x) =
      if this#target_contained_by l then
        super#comment x
      else
        x

    method! expression ((l, _) as x) =
      if this#target_contained_by l then
        super#expression x
      else
        x
  end
