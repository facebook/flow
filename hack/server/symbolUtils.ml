(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type key = Relative_path.t * int

module S = struct
  type t = key

  let compare = Pervasives.compare
end

module LineMap = Utils.MyMap(S)

let get_key pos =
  let line, _, _ = Pos.info_pos pos in
  (Pos.filename pos, line)
