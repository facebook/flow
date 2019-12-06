(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Warning: This is the ONLY code that should be using sigalrm. Any code using it should use Timer
 * instead.
 *
 * This is a timer which lets you schedule callbacks to be invoked later. There are a few things
 * you should know:
 *
 * 1. Timer will not work on Windows, since Unix.setitimer is not implemented there. If you are
 *    building a cross-platform feature, you'll need a Windows-specific implementation. For example,
 *    Timeout uses select instead of timers to implement timeouts on Windows.
 * 2. Timer is built using signals, which can cause your Unix calls to error with EINTR, since
 *    you are interupting them. Not a huge deal, just worth being aware of.
 *)

type t

(* Will invoke callback () after interval seconds *)
val set_timer : interval:float -> callback:(unit -> unit) -> t

(* Will prevent a future timer from firing *)
val cancel_timer : t -> unit
