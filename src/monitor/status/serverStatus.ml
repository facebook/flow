(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module tries to model the Flow server's status as a state machine. The current status is
 * the state, and it gets updated by events. This status can then be streamed to Flow clients and
 * rendered.
 *)

let spf = Utils_js.spf

type progress = {
  total: int option;
  finished: int;
}

type deadline = float

type event =
  | Ready  (** The server is free *)
  | Init_start  (** The server is starting to initialize *)
  | Fetch_saved_state_delay of string  (** Fetching the saved state is taking a long time *)
  | Read_saved_state
  | Load_saved_state_progress of progress
  | Parsing_progress of progress
  | Indexing_progress of progress
  | Resolving_dependencies_progress
  | Calculating_dependencies_progress
  | Merging_progress of progress
  | Checking_progress of progress
  | Canceling_progress of progress
  | Finishing_up  (** Server's finishing up typechecking or other work *)
  | Recheck_start  (** The server is starting to recheck *)
  | Handling_request_start  (** The server is starting to handle an ephemeral/persistent request *)
  | Handling_request_end  (** The server is done handling an ephemeral/persistent request *)
  | GC_start  (** The server is starting to GC *)
  | Collating_errors_start  (** The server is collating the errors *)
  | Watchman_wait_start of deadline option  (** The server is now blocked waiting for Watchman *)

type typecheck_status =
  | Starting_typecheck  (** A typecheck's initial state *)
  | Fetching_saved_state of string  (** Fetching saved state, when it is taking a while *)
  | Reading_saved_state
  | Loading_saved_state of progress
  | Parsing of progress
  | Indexing of progress
  | Resolving_dependencies
  | Calculating_dependencies
  | Merging of progress
  | Checking of progress
  | Canceling of progress
  | Collating_errors  (** We sometimes collate errors during typecheck *)
  | Finishing_typecheck  (** haven't reached free state yet *)
  | Waiting_for_watchman of deadline option

type restart_reason =
  | Server_out_of_date
  | Out_of_shared_memory
  | Restart

type typecheck_mode =
  | Initializing  (** Flow is busy starting up *)
  | Rechecking  (** Flow is busy rechecking *)
  | Handling_request  (** Flow is busy handling a request *)
  | Restarting of restart_reason  (** Same as initializing but with a reason why we restarted *)

type status =
  | Starting_up  (** The server's initial state *)
  | Free  (** Not busy doing something else *)
  | Typechecking of typecheck_mode * typecheck_status  (** Busy doing Flow stuff *)
  | Garbage_collecting  (** This one is pretty obvious *)
  | Unknown  (** A bad state caused by transitioning from a good state due to an unexpected event *)

let string_of_progress { finished; total } =
  match total with
  | None -> spf "%d" finished
  | Some total ->
    spf "%d/%d (%02.1f%%)" finished total (100.0 *. float finished /. float (max 1 total))

type emoji =
  | Closed_book
  | Cookie
  | Eyes
  | File_cabinet
  | Ghost
  | Open_book
  | Panda_face
  | Parachute
  | Recycling_symbol
  | Sleeping_face
  | Smiling_face_with_mouth_open
  | Taco
  | Wastebasket
  | Motorcycle
  | Skier
  | Card_index_dividers

let string_of_emoji = function
  | Closed_book -> "\xF0\x9F\x93\x95"
  | Cookie -> "\xF0\x9F\x8D\xAA"
  | Eyes -> "\xF0\x9F\x91\x80"
  | File_cabinet -> "\xF0\x9F\x97\x84"
  | Ghost -> "\xF0\x9F\x91\xBB"
  | Open_book -> "\xF0\x9F\x93\x96"
  | Panda_face -> "\xF0\x9F\x90\xBC"
  | Parachute -> "\xF0\x9F\xAA\x82"
  | Recycling_symbol -> "\xE2\x99\xBB"
  | Sleeping_face -> "\xF0\x9F\x98\xB4"
  | Smiling_face_with_mouth_open -> "\xF0\x9F\x98\x83"
  | Taco -> "\xF0\x9F\x8C\xAE"
  | Wastebasket -> "\xF0\x9F\x97\x91"
  | Motorcycle -> "\xf0\x9f\x8f\x8d"
  | Skier -> "\xE2\x9B\xB7"
  | Card_index_dividers -> "\xF0\x9F\x97\x82"

type pad_emoji =
  | Before
  | After

let render_emoji ~use_emoji ?(pad = After) emoji =
  if use_emoji then
    spf
      "%s%s %s"
      ( if pad = Before then
        " "
      else
        ""
      )
      (string_of_emoji emoji)
      ( if pad = After then
        " "
      else
        ""
      )
  else
    ""

let string_of_event = function
  | Ready -> "Ready"
  | Init_start -> "Init_start"
  | Fetch_saved_state_delay message -> spf "Fetch_saved_state_delay %s" message
  | Read_saved_state -> "Read_saved_state"
  | Load_saved_state_progress progress ->
    spf "Load_saved_state_progress %s" (string_of_progress progress)
  | Parsing_progress progress -> spf "Parsing_progress files %s" (string_of_progress progress)
  | Indexing_progress progress -> spf "Indexing_progress %s" (string_of_progress progress)
  | Calculating_dependencies_progress -> "Calculating_dependencies_progress"
  | Resolving_dependencies_progress -> "Resolving_dependencies_progress"
  | Merging_progress progress -> spf "Merging_progress %s" (string_of_progress progress)
  | Checking_progress progress -> spf "Checking_progress files %s" (string_of_progress progress)
  | Canceling_progress progress -> spf "Canceling_progress %s" (string_of_progress progress)
  | Finishing_up -> "Finishing_up"
  | Recheck_start -> "Recheck_start"
  | Handling_request_start -> "Handling_request_start"
  | Handling_request_end -> "Handling_request_end"
  | GC_start -> "GC_start"
  | Collating_errors_start -> "Collating_errors_start"
  | Watchman_wait_start _deadline -> "Watchman_wait_start"

(** As a general rule, use past tense for status updates that show progress and present perfect
    progressive for those that don't. *)
let string_of_typecheck_status ~use_emoji = function
  | Starting_typecheck -> spf "%sstarting up" (render_emoji ~use_emoji Sleeping_face)
  | Fetching_saved_state message ->
    spf "%sfetching saved state: %s" (render_emoji ~use_emoji Parachute) message
  | Reading_saved_state -> spf "%sreading saved state" (render_emoji ~use_emoji Closed_book)
  | Loading_saved_state progress ->
    spf "%sloading saved state %s" (render_emoji ~use_emoji Open_book) (string_of_progress progress)
  | Parsing progress ->
    spf "%sparsed files %s" (render_emoji ~use_emoji Ghost) (string_of_progress progress)
  | Indexing progress ->
    spf
      "%sindexing files %s"
      (render_emoji ~use_emoji Card_index_dividers)
      (string_of_progress progress)
  | Resolving_dependencies -> spf "%sresolving dependencies" (render_emoji ~use_emoji Taco)
  | Calculating_dependencies -> spf "%scalculating dependencies" (render_emoji ~use_emoji Taco)
  | Merging progress ->
    spf
      "%smerged module interfaces %s"
      (render_emoji ~use_emoji Motorcycle)
      (string_of_progress progress)
  | Checking progress ->
    spf "%schecked files %s" (render_emoji ~use_emoji Skier) (string_of_progress progress)
  | Canceling progress ->
    spf
      "%scanceling workers %s"
      (render_emoji ~use_emoji Recycling_symbol)
      (string_of_progress progress)
  | Collating_errors -> spf "%scollating errors" (render_emoji ~use_emoji File_cabinet)
  | Waiting_for_watchman deadline ->
    let timeout =
      match deadline with
      | Some deadline ->
        spf " - giving up in %d seconds" (max 0 (int_of_float @@ (deadline -. Unix.gettimeofday ())))
      | None -> ""
    in
    spf "%swaiting for Watchman%s" (render_emoji ~use_emoji Eyes) timeout
  | Finishing_typecheck -> spf "%sfinishing up" (render_emoji ~use_emoji Cookie)

let string_of_restart_reason = function
  | Server_out_of_date -> "restarting due to change which cannot be handled incrementally"
  | Out_of_shared_memory -> "restarting due to running out of shared memory"
  | Restart -> "restarting to save time"

let string_of_status ?(use_emoji = false) ?(terse = false) status =
  let status_string =
    match status with
    | Starting_up -> spf "starting up%s" (render_emoji ~use_emoji ~pad:Before Sleeping_face)
    | Free -> spf "free%s" (render_emoji ~use_emoji ~pad:Before Smiling_face_with_mouth_open)
    | Typechecking (Initializing, tcs) ->
      spf "initializing (%s)" (string_of_typecheck_status ~use_emoji tcs)
    | Typechecking (Rechecking, tcs) ->
      spf "rechecking (%s)" (string_of_typecheck_status ~use_emoji tcs)
    | Typechecking (Handling_request, tcs) ->
      spf "handling a request (%s)" (string_of_typecheck_status ~use_emoji tcs)
    | Typechecking (Restarting reason, tcs) ->
      spf "%s (%s)" (string_of_restart_reason reason) (string_of_typecheck_status ~use_emoji tcs)
    | Garbage_collecting ->
      spf "garbage collecting shared memory%s" (render_emoji ~use_emoji ~pad:Before Wastebasket)
    | Unknown -> spf "doing something%s" (render_emoji ~use_emoji ~pad:Before Panda_face)
  in
  spf
    "%s%s"
    ( if terse then
      ""
    else
      "Server is "
    )
    status_string

(** Transition function for the status state machine. Given the current status and the event,
    pick a new status *)
let update ~event ~status =
  match (event, status) with
  | (Ready, _) -> Free
  | (Init_start, _) -> Typechecking (Initializing, Starting_typecheck)
  | (Recheck_start, _) -> Typechecking (Rechecking, Starting_typecheck)
  | (Fetch_saved_state_delay message, Typechecking (mode, _)) ->
    Typechecking (mode, Fetching_saved_state message)
  | (Read_saved_state, Typechecking (mode, _)) -> Typechecking (mode, Reading_saved_state)
  | (Load_saved_state_progress progress, Typechecking (mode, _)) ->
    Typechecking (mode, Loading_saved_state progress)
  | (Parsing_progress progress, Typechecking (mode, _)) -> Typechecking (mode, Parsing progress)
  | (Indexing_progress progress, Typechecking (mode, _)) -> Typechecking (mode, Indexing progress)
  | (Resolving_dependencies_progress, Typechecking (mode, _)) ->
    Typechecking (mode, Resolving_dependencies)
  | (Calculating_dependencies_progress, Typechecking (mode, _)) ->
    Typechecking (mode, Calculating_dependencies)
  | (Merging_progress progress, Typechecking (mode, _)) -> Typechecking (mode, Merging progress)
  | (Checking_progress progress, Typechecking (mode, _)) -> Typechecking (mode, Checking progress)
  | (Canceling_progress progress, Typechecking (mode, _)) -> Typechecking (mode, Canceling progress)
  | (Collating_errors_start, Typechecking (mode, _)) -> Typechecking (mode, Collating_errors)
  | (Watchman_wait_start deadline, Typechecking (mode, _)) ->
    Typechecking (mode, Waiting_for_watchman deadline)
  | (Finishing_up, Typechecking (mode, _)) -> Typechecking (mode, Finishing_typecheck)
  | (GC_start, _) -> Garbage_collecting
  | (Handling_request_start, _) ->
    (match status with
    | Typechecking _ ->
      (* commands running in parallel with a recheck should not take precedence
         over the recheck's status *)
      status
    | _ -> Typechecking (Handling_request, Starting_typecheck))
  | (Handling_request_end, _) ->
    (match status with
    | Typechecking (Handling_request, _) ->
      (* as above, parallel rechecks take precedence, so only finish handling a
         request if that's still the most important thing we're doing. *)
      Typechecking (Handling_request, Finishing_typecheck)
    | _ -> status)
  | _ ->
    (* This is a bad transition. In dev mode, let's blow up since something is wrong. However in
     * production let's soldier on. Usually this means that we forgot to send something like
     * Handling_request_start before we sent a Merging_progress. *)
    if Build_mode.dev then
      failwith
        (spf
           "Unexpected status transition from '%s' with event '%s'"
           (string_of_status status)
           (string_of_event event)
        )
    else
      Unknown

let initial_status = Starting_up

let is_free = function
  | Free -> true
  | _ -> false

(** Returns true iff the transition from old_status to new_status is "significant", which is a
    pretty arbitrary judgement of how interesting the new status is to a user. Significant
    transitions are pushed to the user immediately; insignificant transitions are throttled. *)
let is_significant_transition old_status new_status =
  (* If the statuses are literally the same, then the transition is not significant *)
  old_status <> new_status
  && (Utils_js.in_flow_test
     ||
     match (old_status, new_status) with
     | (_, Typechecking (_, Starting_typecheck))
     | (_, Typechecking (_, Finishing_typecheck)) ->
       (* These are very short transition statuses that aren't important unless
          they take a long time for some reason. *)
       false
     | (Typechecking (old_mode, old_tc_status), Typechecking (new_mode, new_tc_status)) ->
       (* A change in mode is always signifcant *)
       old_mode <> new_mode
       || begin
            match (old_tc_status, new_tc_status) with
            | (Parsing _, Parsing _)
            | (Indexing _, Indexing _)
            | (Merging _, Merging _)
            | (Checking _, Checking _)
            | (Canceling _, Canceling _) ->
              (* Making progress within parsing, merging or canceling is not significant *)
              false
            | (_, Fetching_saved_state _)
            | (_, Reading_saved_state)
            | (_, Loading_saved_state _)
            | (_, Parsing _)
            | (_, Indexing _)
            | (_, Resolving_dependencies)
            | (_, Calculating_dependencies)
            | (_, Merging _)
            | (_, Checking _)
            | (_, Canceling _)
            | (_, Waiting_for_watchman _)
            | (_, Collating_errors) ->
              (* But changing typechecking status always is significant *)
              true
            | (_, Starting_typecheck)
            | (_, Finishing_typecheck) ->
              (* also handled above *)
              false
          end
     | (_, Starting_up)
     | (_, Free)
     | (_, Typechecking _)
     | (_, Garbage_collecting)
     | (_, Unknown) ->
       (* Switching to a completely different status is always significant *)
       true
     )

let get_progress status =
  let print progress =
    match progress with
    | { finished; total = None } -> (Some (Printf.sprintf "%d" finished), Some finished, None)
    | { finished; total = Some total } ->
      (Some (Printf.sprintf "%d/%d" finished total), Some finished, Some total)
  in
  match status with
  | Typechecking (_, Parsing progress)
  | Typechecking (_, Merging progress)
  | Typechecking (_, Checking progress)
  | Typechecking (_, Canceling progress) ->
    print progress
  | _ -> (None, None, None)

(** When the server is initializing it will publish statuses that say it is initializing. The
    monitor might know that the server actually is restarting. This function turns a initializing
    status into a restarting status *)
let change_init_to_restart restart_reason status =
  Base.Option.value_map restart_reason ~default:status ~f:(fun restart_reason ->
      match status with
      | Typechecking (Initializing, tcs) -> Typechecking (Restarting restart_reason, tcs)
      | _ -> status
  )
