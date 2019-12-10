(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val extract_flowlib : no_flowlib:bool -> Path.t -> unit

val contents : bool -> (string * string) array
