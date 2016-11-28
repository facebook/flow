(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val get_master_cx: Options.t -> Context.t
val restore_master_cx: Context.t -> unit

(* called to initialize library code on initial full pass.
   params are functions to save errors and suppressions:
   circular deps in Ocaml prevent direct calls from here
   to Types_js, where error management stuff lives.
 *)
val init :
  options: Options.t ->
  string list ->
  (Loc.filename -> Errors.ErrorSet.t -> unit) ->
  (Loc.filename -> Errors.ErrorSuppressions.t -> unit) ->
  (Loc.filename * bool) list

val init_graphql : Options.t -> unit
