(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type LOOP = sig
  type acc

  val main : acc -> acc Lwt.t

  val catch : acc -> Exception.t -> unit Lwt.t
end

module Make (Loop : LOOP) : sig
  val run : ?cancel_condition:'a Lwt_condition.t -> Loop.acc -> unit Lwt.t
end
