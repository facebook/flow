(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* We actually don't need a mutator or reader for module_name_candidates_cache. There are a few
 * reasons why:
 *
 * 1. It's really only used for memoization. We never remove or replace anything
 * 2. The code which populates it never changes during the lifetime of a server. So we never
 *    really need to roll anything back ever *)
let module_name_candidates_cache = Hashtbl.create 50

let memoize_with_module_name_candidates_cache ~f name =
  try Hashtbl.find module_name_candidates_cache name with
  | Not_found ->
    let result = f name in
    Hashtbl.add module_name_candidates_cache name result;
    result
