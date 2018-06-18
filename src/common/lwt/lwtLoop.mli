(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type LOOP = sig
  type acc
  val main: acc -> acc Lwt.t
  val catch: acc -> exn -> unit Lwt.t
end

module Make: functor (Loop: LOOP) -> sig
  val run: ?cancel_condition:'a Lwt_condition.t -> Loop.acc -> unit Lwt.t
end
