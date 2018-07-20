(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val all_providers: (Modulename.t, Utils_js.FilenameSet.t) Hashtbl.t

val module_name_candidates_cache: (string, string list) Hashtbl.t
