(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * This is an abstraction over source files used by the full-fidelity lexer.
 * Right now it is just a thin wrapper over a string, but in the future I'd
 * like to enhance this to efficiently support operations such as:
 *
 * - representing many different portions of a file without duplicating text
 * - lazily loading content from files
 * - caching file contents intelligently
 * - reporting when files are updated perhaps?
 *
 * We might not need all of these things, but it will be nice to have an
 * abstraction when we need some of them.
 *)

module OffsetMap = Full_fidelity_offset_map

type t = {
  text : string;
  offset_map : OffsetMap.t
}

let make content =
  { text = content; offset_map = OffsetMap.make content }

let from_file file =
  let content =
    try Sys_utils.cat (Relative_path.to_absolute file) with _ -> "" in
  make content

let length source_text =
  String.length source_text.text

let get source_text index =
  String.get source_text.text index

let sub source_text start length =
  let len = String.length source_text.text in
  if start >= len then
    ""
  else if start + length > len then
    String.sub source_text.text start (len - start)
  else
    String.sub source_text.text start length

(* Take a zero-based offset, produce a one-based (line, char) pair. *)
let offset_to_position source_text offset =
  OffsetMap.offset_to_position source_text.offset_map offset

(* Take a one-based (line, char) pair, produce a zero-based offset *)
let position_to_offset source_text (line, character) =
  OffsetMap.position_to_offset
