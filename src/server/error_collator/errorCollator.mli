(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val get_with_separate_warnings :
  profiling:Profiling_js.running ->
  reader:State_reader.t ->
  options:Options.t ->
  ServerEnv.env ->
  Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  * Flow_errors_utils.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t
  * (Loc.t Flow_errors_utils.printable_error * Loc_collections.LocSet.t) list

val get :
  profiling:Profiling_js.running ->
  reader:State_reader.t ->
  options:Options.t ->
  ServerEnv.env ->
  Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  * Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  * (Loc.t Flow_errors_utils.printable_error * Loc_collections.LocSet.t) list
