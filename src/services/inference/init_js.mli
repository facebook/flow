(**
 * Copyright (c) 2013-present, Facebook, Inc.
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
  options: Options.t ->
  string list ->
  (File_key.t *
    bool *
    Errors.ErrorSet.t *
    Error_suppressions.t_map *
    ExactCover.lint_severity_cover Utils_js.FilenameMap.t) list Lwt.t
