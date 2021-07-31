(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

external get_build_revision : unit -> string = "hh_get_build_revision"

let build_revision = get_build_revision ()
