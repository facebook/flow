(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Found of Loc.t

class searcher ~contains_loc =
  object (_this)
    inherit [Loc.t] Flow_ast_mapper.mapper

    method! identifier ((loc, _) as id) =
      if contains_loc loc then raise (Found loc);
      id
  end

let search_rename_loc ast cursor_loc =
  let s = new searcher ~contains_loc:(fun loc -> Loc.contains loc cursor_loc) in
  try
    ignore @@ s#program ast;
    None
  with
  | Found loc -> Some loc
