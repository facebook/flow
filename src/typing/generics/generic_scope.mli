(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val init : Generic_cx.t -> Generic_cx.t

val in_scope : Generic_cx.t -> ALoc.id list -> (Generic_cx.t -> 'a) -> 'a

val scope_id : unit -> Scope_id.t
