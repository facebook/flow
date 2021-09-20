(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Flow_set.Make (StringKey)

let pp = make_pp StringKey.pp

let show sset = Format.asprintf "%a" pp sset

let to_string = show
