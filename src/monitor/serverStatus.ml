(**
 * Copyright (c) 2017-present, Facebook, Inc.
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

type event =
| Ready (* The server is free *)
| Init_start (* The server is starting to initialize *)
| Parsing_progress of progress
| Resolving_dependencies_progress
| Calculating_dependencies_progress
| Merging_progress of progress
| Finishing_up (* The server is finishing up some typechecking *)
| Recheck_start (* The server is starting to recheck *)
| Handling_request_start (* The server is starting to handle a request *)
| GC_start (* The server is starting to GC *)

type typecheck_status =
| Starting_typecheck (* A typecheck's initial state *)
| Parsing of progress
| Resolving_dependencies
| Calculating_dependencies
| Merging of progress
| Garbage_collecting_typecheck (* We garbage collect during typechecks sometime *)
| Finishing_typecheck (* Typecheck is done but we haven't reach a free state yet *)

type typecheck_mode =
| Initializing (* Flow is busy starting up *)
| Rechecking (* Flow is busy rechecking *)
| Handling_request (* Flow is busy handling a request *)

type status =
| Starting_up (* The server's initial state *)
| Free (* Not busy doing something else *)
| Typechecking of typecheck_mode * typecheck_status (* Busy doing Flow stuff *)
| Garbage_collecting (* This one is pretty obvious *)
| Unknown (* A bad state caused by transitioning from a good state due to an unexpected event *)

let string_of_progress {finished; total} =
  match total with
  | None -> spf "%d" finished
  | Some total ->
    spf "%d/%d (%02.1f%%)" finished total (100.0 *. (float finished) /. (float (max 1 total)))

type emoji =
| Bicyclist
| Cookie
| Ghost
| Panda_face
| Sleeping_face
| Smiling_face_with_mouth_open
| Taco
| Wastebasket

let string_of_emoji = function
| Bicyclist -> "\xF0\x9F\x9A\xB4"
| Cookie -> "\xF0\x9F\x8D\xAA"
| Ghost -> "\xF0\x9F\x91\xBB"
| Panda_face -> "\xF0\x9F\x90\xBC"
| Sleeping_face -> "\xF0\x9F\x98\xB4"
| Smiling_face_with_mouth_open -> "\xF0\x9F\x98\x83"
| Taco -> "\xF0\x9F\x8C\xAE"
| Wastebasket -> "\xF0\x9F\x97\x91"

type pad_emoji =
| Before
| After

let render_emoji ~use_emoji ?(pad=After) emoji =
  if use_emoji
  then spf
    "%s%s %s"
    (if pad = Before then " " else "")
    (string_of_emoji emoji)
    (if pad = After then " " else "")
  else ""

let string_of_event = function
| Ready -> "Ready"
| Init_start -> "Init_start"
| Parsing_progress progress ->
  spf "Parsing_progress files %s" (string_of_progress progress)
| Calculating_dependencies_progress -> "Calculating_dependencies_progress"
| Resolving_dependencies_progress -> "Resolving_dependencies_progress"
| Merging_progress progress ->
  spf "Merging_progress %s" (string_of_progress progress)
| Finishing_up -> "Finishing_up"
| Recheck_start -> "Recheck_start"
| Handling_request_start -> "Handling_request_start"
| GC_start -> "GC_start"

let string_of_typecheck_status ~use_emoji = function
| Starting_typecheck ->
  spf "%sstarting up" (render_emoji ~use_emoji Sleeping_face)
| Parsing progress ->
  spf "%sparsed files %s" (render_emoji ~use_emoji Ghost) (string_of_progress progress)
| Resolving_dependencies ->
  spf "%sresolving dependencies" (render_emoji ~use_emoji Taco)
| Calculating_dependencies ->
  spf "%scalculating dependencies" (render_emoji ~use_emoji Taco)
| Merging progress ->
  spf "%smerged files %s" (render_emoji ~use_emoji Bicyclist) (string_of_progress progress)
| Garbage_collecting_typecheck ->
  spf "%sgarbage collecting shared memory" (render_emoji ~use_emoji Wastebasket)
| Finishing_typecheck ->
  spf "%sfinishing up" (render_emoji ~use_emoji Cookie)

let string_of_status ?(use_emoji=false) status =
  let status_string = match status with
  | Starting_up ->
    spf "starting up%s" (render_emoji ~use_emoji ~pad:Before Sleeping_face)
  | Free ->
    spf "free%s" (render_emoji ~use_emoji ~pad:Before Smiling_face_with_mouth_open)
  | Typechecking (Initializing, tcs) ->
    spf "initializing (%s)" (string_of_typecheck_status ~use_emoji tcs)
  | Typechecking (Rechecking, tcs) ->
    spf "rechecking (%s)" (string_of_typecheck_status ~use_emoji tcs)
  | Typechecking (Handling_request, tcs) ->
    spf "handling a request (%s)" (string_of_typecheck_status ~use_emoji tcs)
  | Garbage_collecting ->
    spf "garbage collecting shared memory%s" (render_emoji ~use_emoji ~pad:Before Wastebasket)
  | Unknown ->
    spf "doing something%s" (render_emoji ~use_emoji ~pad:Before Panda_face)
  in
  spf "Server is %s" status_string

(* Transition function for the status state machine. Given the current status and the event,
 * pick a new status *)
let update ~event ~status =
  match event, status with
  | Ready, _ -> Free

  | Init_start, _ -> Typechecking (Initializing, Starting_typecheck)
  | Recheck_start, _ -> Typechecking (Rechecking, Starting_typecheck)
  | Handling_request_start, _ -> Typechecking (Handling_request, Starting_typecheck)

  | Parsing_progress progress, Typechecking (mode, _) -> Typechecking (mode, Parsing progress)
  | Resolving_dependencies_progress, Typechecking (mode, _) ->
      Typechecking (mode, Resolving_dependencies)
  | Calculating_dependencies_progress, Typechecking (mode, _) ->
      Typechecking (mode, Calculating_dependencies)
  | Merging_progress progress, Typechecking (mode, _) -> Typechecking (mode, Merging progress)
  | GC_start, Typechecking (mode, _) -> Typechecking (mode, Garbage_collecting_typecheck)
  | Finishing_up, Typechecking (mode, _) -> Typechecking (mode, Finishing_typecheck)

  | GC_start, _ -> Garbage_collecting
  | _ ->
    (* This is a bad transition. In dev mode, let's blow up since something is wrong. However in
     * production let's soldier on. Usually this means that we forgot to send something like
     * Handling_request_start before we sent a Merging_progress. *)
    if Build_mode.dev
    then failwith (spf
      "Unexpected status transition from '%s' with event '%s'"
      (string_of_status status)
      (string_of_event event)
    )
    else Unknown

let initial_status = Starting_up

let is_free = function
| Free -> true
| _ -> false
