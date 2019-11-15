(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type relative_position = {
  line_offset: int;
  (* If line_offset is 0, this is the column offset. If line_offset is nonzero, this is the absolute
  * column. *)
  column_or_offset: int;
}

type t = {
  start: Loc.position;
  end_: relative_position;
}

let relative_position_of_position base_pos pos =
  Loc.(
    let line_offset = pos.line - base_pos.line in
    let column_or_offset =
      if line_offset = 0 then
        pos.column - base_pos.column
      else
        pos.column
    in
    { line_offset; column_or_offset })

let position_of_relative_position base_pos relative_pos =
  Loc.(
    let line = base_pos.line + relative_pos.line_offset in
    let column =
      if relative_pos.line_offset = 0 then
        base_pos.column + relative_pos.column_or_offset
      else
        relative_pos.column_or_offset
    in
    { line; column })

let of_loc loc =
  let end_ = relative_position_of_position loc.Loc.start loc.Loc._end in
  { start = loc.Loc.start; end_ }

let to_loc relative_loc source =
  let end_ = position_of_relative_position relative_loc.start relative_loc.end_ in
  { Loc.source; start = relative_loc.start; _end = end_ }
