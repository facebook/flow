(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target_loc =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    val mutable found : bool = false

    method is_found = found

    method! statement stmt =
      if found then
        stmt
      else
        match stmt with
        | (loc, Flow_ast.Statement.Switch switch) when this#target_contained_by loc ->
          (match Switch_to_match.convert_switch ~placeholder_loc:Loc.none loc switch with
          | Some stmt' when stmt' != stmt ->
            found <- true;
            stmt'
          | _ -> stmt)
        | _ -> super#statement stmt
  end

let refactor ast loc =
  let mapper = new mapper loc in
  let ast' = mapper#program ast in
  if (not mapper#is_found) || ast' == ast then
    (* No change *)
    None
  else
    Some ast'
