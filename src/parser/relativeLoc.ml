(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Same_line of {
      start: Loc.position;
      column_offset: int;
    }
  | Diff_line of {
      start: Loc.position;
      line_offset: int;
      column: int;
    }

let of_loc ({ Loc.start = base_pos; _end = pos; _ } : Loc.t) =
  let line_offset = pos.Loc.line - base_pos.Loc.line in
  if line_offset = 0 then
    Same_line { start = base_pos; column_offset = pos.Loc.column - base_pos.Loc.column }
  else
    Diff_line { start = base_pos; line_offset; column = pos.Loc.column }

let to_loc relative_loc source : Loc.t =
  match relative_loc with
  | Same_line { start; column_offset } ->
    {
      Loc.start;
      _end = { Loc.line = start.Loc.line; column = start.Loc.column + column_offset };
      source;
    }
  | Diff_line { start; line_offset; column } ->
    { Loc.start; _end = { Loc.line = start.Loc.line + line_offset; column }; source }
