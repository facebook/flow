(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let do_ _ _ =
  [], ([], (Naming.empty TypecheckerOptions.default, Relative_path.Map.empty))
let go _ _ _ _ = [], Relative_path.Set.empty
