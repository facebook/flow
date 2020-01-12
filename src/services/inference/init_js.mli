(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* called to initialize library code on initial full pass.
   params are functions to save errors and suppressions:
   circular deps in Ocaml prevent direct calls from here
   to Types_js, where error management stuff lives.
 *)
val init :
  options:Options.t ->
  reader:Mutator_state_reader.t ->
  string list ->
  (File_key.t * bool * Flow_error.ErrorSet.t * Flow_error.ErrorSet.t * Error_suppressions.t) list
  Lwt.t
