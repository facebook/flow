(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of FlowExitStatus

val exit : ?msg:string -> t -> 'a

val set_json_mode : pretty:bool -> unit

val unset_json_mode : unit -> unit
