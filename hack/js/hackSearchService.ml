(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Autocomplete code uses search service to provide global identifier
 * autocompletions. This service is not enabled in JS world, so here is a stub
 * for it *)

type search_result_type =
  | Class of string
  | Method of bool * string
  | ClassVar of bool * string
  | Function
  | Typedef
  | Constant

module MasterApi = struct
  let query_autocomplete input ~limit ~filter_map = []
end
