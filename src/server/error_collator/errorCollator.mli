(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Note on suppressions: The suppressions within the Server.errors parameter
    only account for the files that were most recently checked. Due to bugs in the
    checker it is possible for errors that are included in that same set to appear
    in files other than the ones in checked_files. To effectively suppress these
    we need to pass in an additional `all_suppressions` parameter. This is the
    suppression set we pass over to Error_suppressions.filter_suppressed_errors.
    Finally, we compute unused suppression warnings over the set of suppressions
    that we found during checking (not all_suppressions).
*)
val update_collated_errors :
  reader:Parsing_heaps.Reader_dispatcher.reader ->
  options:Options.t ->
  checked_files:CheckedSet.t ->
  all_suppressions:Error_suppressions.t ->
  ServerEnv.errors ->
  Collated_errors.t ->
  Collated_errors.t

val update_local_collated_errors :
  reader:Parsing_heaps.Reader_dispatcher.reader ->
  options:Options.t ->
  Error_suppressions.t ->
  Flow_error.ErrorSet.t Utils_js.FilenameMap.t ->
  Collated_errors.t ->
  Collated_errors.t

val get_with_separate_warnings :
  ServerEnv.env ->
  Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  * Flow_errors_utils.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t
  * (Loc.t Flow_errors_utils.printable_error * Loc_collections.LocSet.t) list

type error_resolution_stat = {
  time_to_resolve_all_type_errors: float option;
  time_to_resolve_all_type_errors_in_one_file: float option;
  time_to_resolve_all_subtyping_errors: float option;
  time_to_resolve_all_subtyping_errors_in_one_file: float option;
}

(* Update error_state_timestamps,
 * and return a collection of times to resolve different kinds of errors
 * under different initial conditions *)
val update_error_state_timestamps : Collated_errors.t -> Collated_errors.t * error_resolution_stat

val get :
  ServerEnv.env ->
  Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  * Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  * (Loc.t Flow_errors_utils.printable_error * Loc_collections.LocSet.t) list
