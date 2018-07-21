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

type summary_info =
  | RecheckSummary of {
      dependent_file_count: int;
      changed_file_count: int;
      top_cycle: (File_key.t * int) option; (* name of cycle leader, and size of cycle *)
    }
  | CommandSummary of string
  | InitSummary

type summary = { duration: float; info: summary_info;}

type event =
| Ready
| Init_start
| Read_saved_state
| Load_saved_state_progress of progress
| Parsing_progress of progress
| Resolving_dependencies_progress
| Calculating_dependencies_progress
| Merging_progress of progress
| Canceling_progress of progress
| Finishing_up of summary
| Recheck_start
| Handling_request_start
| GC_start
| Collating_errors_start

type status

val initial_status: status
val update: event:event -> status:status -> status
val string_of_status: ?use_emoji:bool -> ?terse:bool ->status -> string
val is_free: status -> bool
val is_significant_transition: status -> status -> bool
val get_progress: status -> string option * int option * int option
val get_summary: status -> summary option
val log_of_summaries: root:Path.t -> summary list -> FlowEventLogger.persistent_delay
