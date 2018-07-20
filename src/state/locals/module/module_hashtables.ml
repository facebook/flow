(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* hash table from module names to all known provider files.
   maintained and used by commit_modules and remove_files *)
(** TODO [perf]: investigate whether this takes too much memory **)
let all_providers = Hashtbl.create 0
let module_name_candidates_cache = Hashtbl.create 50
