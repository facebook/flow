(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

include Set.Make (IntKey)
let to_string iset =
  "{" ^ (String.concat "," (List.map string_of_int (elements iset))) ^ "}"
