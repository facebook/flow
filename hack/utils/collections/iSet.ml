(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

include Set.Make (IntKey)
let to_string iset =
  "{" ^ (String.concat "," (List.map string_of_int (elements iset))) ^ "}"
