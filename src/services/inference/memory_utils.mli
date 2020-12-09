(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val with_memory_profiling_lwt : profiling:Profiling_js.running -> (unit -> 'a Lwt.t) -> 'a Lwt.t

val with_memory_timer_lwt :
  ?options:Options.t -> string -> Profiling_js.running -> (unit -> 'a Lwt.t) -> 'a Lwt.t
