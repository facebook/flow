(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

let newline = "\n"
let print_state state =
  let b = Buffer.create 200 in
  let cgroup = state.Solve_state.chunk_group in

  List.iter cgroup.Chunk_group.chunks ~f:(fun c ->
    if Solve_state.has_split_before_chunk c state.Solve_state.rvm then begin
      Buffer.add_string b newline;
      let indent = Nesting.get_indent
        c.Chunk.nesting state.Solve_state.nesting_set in
      let indent = indent + cgroup.Chunk_group.block_indentation in
      Buffer.add_string b (String.make indent ' ')
    end else begin
      if c.Chunk.space_if_not_split then Buffer.add_string b " "
    end;
    Buffer.add_string b c.Chunk.text;
    ()
  );
  Buffer.contents b
