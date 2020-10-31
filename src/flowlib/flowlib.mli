(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mkdir : no_flowlib:bool -> Path.t -> Path.t

val extract : no_flowlib:bool -> Path.t -> unit
