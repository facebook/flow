(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val type_check: ServerEnv.genv -> ServerEnv.env -> ServerEnv.env * int

(* just add also some debugging information on stdout *)
val check: ServerEnv.genv -> ServerEnv.env -> ServerEnv.env * int

val hook_after_parsing: (ServerEnv.genv -> (* old *) ServerEnv.env ->
    (* new *) ServerEnv.env -> Relative_path.Set.t -> unit) option ref

(****************************************************************************)
(* Debugging: Declared here to stop ocamlc yelling at us for unused defs *)
(****************************************************************************)

val print_defs: string -> ('a * string) list -> unit
val print_fast_pos: (('a * string) list * ('b * string) list) Utils.SMap.t -> unit
val print_fast: (Utils.SSet.t * Utils.SSet.t) Utils.SMap.t -> unit
