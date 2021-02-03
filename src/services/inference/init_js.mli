(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type init_result = {
  ok: bool;
  errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  warnings: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  suppressions: Error_suppressions.t;
  exports: Exports.t;
}

(* called to initialize library code on initial full pass.
   params are functions to save errors and suppressions:
   circular deps in Ocaml prevent direct calls from here
   to Types_js, where error management stuff lives.
*)
val init : options:Options.t -> reader:Mutator_state_reader.t -> string list -> init_result Lwt.t
