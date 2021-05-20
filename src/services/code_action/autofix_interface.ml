(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This class maps each node that contains the target until a node is contained
   by the target *)

class mapper target =
  object (this)
    inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

    method private target_contains loc = Loc.contains target loc

    method private target_contained_by loc = Loc.contains loc target

    method private is_target loc = Loc.equal target loc

    method loc_annot_contains_target = this#target_contained_by

    method! type_ t =
      let open Flow_ast.Type in
      match t with
      | (loc, Object ot) when this#is_target loc ->
        (* interfaces behave like inexact object, but they cannot have the explicit `...` syntax *)
        ( loc,
          Interface
            {
              Interface.body = (loc, Object.{ ot with inexact = false });
              extends = [];
              comments = None;
            } )
      | _ -> super#type_ t
  end

let replace_object_at_target ast loc =
  let mapper = new mapper loc in
  mapper#program ast
