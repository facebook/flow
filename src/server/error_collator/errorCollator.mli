(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Incremental : sig
  val update_collated_errors :
    profiling:Profiling_js.running ->
    reader:Parsing_heaps.Reader_dispatcher.reader ->
    options:Options.t ->
    checked_files:CheckedSet.t ->
    ServerEnv.errors ->
    Incremental_collated_errors.t ->
    Incremental_collated_errors.t

  val update_local_collated_errors :
    reader:Parsing_heaps.Reader_dispatcher.reader ->
    options:Options.t ->
    Error_suppressions.t ->
    Flow_error.ErrorSet.t Utils_js.FilenameMap.t ->
    Incremental_collated_errors.t ->
    Incremental_collated_errors.t
end

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
