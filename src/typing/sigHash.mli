(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add : Xx.state -> string -> unit

val add_int : Xx.state -> int -> unit

val add_aloc : Xx.state -> ALoc.t -> unit

val add_type : Xx.state -> Type.t -> unit

val add_use : Xx.state -> Type.use_t -> unit

val add_reason : Xx.state -> Reason.t -> unit

val add_polarity : Xx.state -> Polarity.t -> unit

val add_props_map : Xx.state -> Type.Properties.t -> unit

val add_exports_map : Xx.state -> Type.Exports.t -> unit
