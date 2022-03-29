(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val memoize_with_module_name_candidates_cache : f:(string -> string list) -> string -> string list
