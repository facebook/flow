(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type key = File_key.t

type patch = (int * int * string) list

val set_diff : (key -> patch -> unit) Expensive.t

val get_diff : key -> patch option

val remove_batch : Utils_js.FilenameSet.t -> unit
