(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A well known condition variable. When we need to exit, we will broadcast on it. This allows
 * various bits of code to wait for this signal and execute clean up code when it's received *)
let signal = Lwt_condition.create ()
