(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type progress = {
  total: int option;
  finished: int;
}

type event =
| Ready
| Init_start
| Parsing_progress of progress
| Resolving_dependencies_progress
| Calculating_dependencies_progress
| Merging_progress of progress
| Finishing_up
| Recheck_start
| Handling_request_start
| GC_start
| Collating_errors_start

type status

val initial_status: status
val update: event:event -> status:status -> status
val string_of_status: ?use_emoji:bool ->status -> string
val is_free: status -> bool
val is_significant_transition: status -> status -> bool
