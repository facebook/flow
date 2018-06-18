(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type state

val init: master_cx:Context.sig_t -> state

val mark: Context.t -> state -> state

val sweep: master_cx:Context.sig_t -> Context.t -> state -> unit
