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
  List.iter state.Solve_state.chunks ~f:(fun c ->
    if Chunk.has_split_before c then Buffer.add_string b newline;
    Buffer.add_string b c.Chunk.text;
    ()
  );
  Buffer.contents b
