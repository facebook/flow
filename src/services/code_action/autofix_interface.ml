(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This class maps each node that contains the target until a node is contained
   by the target *)

class mapper target =
  object (this)
    inherit Autofix_mapper.mapper target as super

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
            }
        )
      | _ -> super#type_ t
  end

let replace_object_at_target ast loc =
  let mapper = new mapper loc in
  mapper#program ast
