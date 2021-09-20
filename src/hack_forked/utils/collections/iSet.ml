(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Flow_set.Make (IntKey)

let pp = make_pp IntKey.pp

let show iset = Format.asprintf "%a" pp iset

let to_string = show
