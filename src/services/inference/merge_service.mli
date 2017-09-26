(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

type 'a merge_results = (File_key.t * ('a, exn) result) list
type 'a merge_job =
  options:Options.t ->
  'a merge_results * File_key.t list ->
  File_key.t list ->
  'a merge_results * File_key.t list

val merge_strict_context:
  options: Options.t ->
  File_key.t list ->
  Context.t * Context.t

val merge_contents_context:
  Options.t ->
  File_key.t ->
  Loc.t Ast.program ->
  Docblock.t ->
  ensure_checked_dependencies: (Modulename.Set.t -> unit) ->
  Context.t

val merge_runner:
  job: 'a merge_job ->
  intermediate_result_callback: ('a merge_results Lazy.t -> unit) ->
  options: Options.t ->
  workers: Worker.t list option ->
  FilenameSet.t FilenameMap.t ->
  (File_key.t list) FilenameMap.t ->
  bool FilenameMap.t ->
  'a merge_results

val merge_strict:
  intermediate_result_callback:
    ((Errors.ErrorSet.t *
      Error_suppressions.t *
      ExactCover.lint_severity_cover) merge_results Lazy.t -> unit) ->
  options: Options.t ->
  workers: Worker.t list option ->
  FilenameSet.t FilenameMap.t ->
  (File_key.t list) FilenameMap.t ->
  bool FilenameMap.t ->
  (Errors.ErrorSet.t * Error_suppressions.t * ExactCover.lint_severity_cover) merge_results
