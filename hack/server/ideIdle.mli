(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Call this when server is idle *)
val go: unit -> unit

(**
 * Add a task to be performed when server is idle.
 *
 * First argument is a function that tells whether task has any work to do. It
 * should be non-blocking and be able to return answer without doing any heavy
 * computation.
 *
 * Second function is the task itself. It should finish in "reasonable" (from
 * server responsiveness point of view) time. Less than 100ms is a good
 * guideline. If the task takes longer than that, the function should take care
 * of chunking it and executing it in parts on subsequent invocations.
 *)
val add_task: (unit -> bool) -> (unit -> unit) -> unit

(* Tells whether running go will have any meaningful effect *)
val has_tasks: unit -> bool

val init: unit -> unit
