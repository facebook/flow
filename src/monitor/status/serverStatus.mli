(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type progress = {
  total: int option;
  finished: int;
}

type deadline = float

type event =
  | Ready
  | Init_start
  | Fetch_saved_state_delay of string
  | Read_saved_state
  | Load_saved_state_progress of progress
  | Parsing_progress of progress
  | Indexing_progress of progress
  | Resolving_dependencies_progress
  | Calculating_dependencies_progress
  | Merging_progress of progress
  | Checking_progress of progress
  | Canceling_progress of progress
  | Finishing_up
  | Recheck_start
  | Handling_request_start
  | Handling_request_end
  | GC_start
  | Collating_errors_start
  | Watchman_wait_start of deadline option

type status

type restart_reason =
  | Server_out_of_date
  | Out_of_shared_memory
  | Restart

val initial_status : status

val update : event:event -> status:status -> status

val string_of_status : ?use_emoji:bool -> ?terse:bool -> status -> string

val is_free : status -> bool

val is_significant_transition : status -> status -> bool

val get_progress : status -> string option * int option * int option

val change_init_to_restart : restart_reason option -> status -> status
