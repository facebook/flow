(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* called to initialize library code on initial full pass.
   params are functions to save errors and suppressions:
   circular deps in Ocaml prevent direct calls from here
   to Types_js, where error management stuff lives.
 *)
val init :
  (string -> Errors_js.ErrorSet.t -> unit) ->
  (string -> Errors_js.ErrorSet.t -> unit) ->
  (string -> Errors_js.ErrorSuppressions.t -> unit) ->
  (string * bool) list
