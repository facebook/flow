(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Strip the prefix of Pos.post_file so only relative path portion is used *)
let pos_to_relative pos =
  { pos with Pos.pos_file = Relative_path.suffix pos.Pos.pos_file }

let pos_str pos =
  Pos.string (pos_to_relative pos)

type key = Relative_path.t * int

module S = struct
  type t = key

  let compare = Pervasives.compare
end

module LineMap = Utils.MyMap(S)

let get_key pos =
  (pos.Pos.pos_file, pos.Pos.pos_start.Lexing.pos_lnum)
