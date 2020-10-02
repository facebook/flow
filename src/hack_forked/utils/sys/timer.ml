(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Warning: This is the ONLY code that should be using sigalrm. Any code using it should use Timer
 * instead.
 *
 * This is a timer which lets you schedule callbacks to be invoked later. There are a few things
 * you should know:
 *
 * 1. Timer will not work on Windows, since Unix.setitimer is not implemented there. If you are
 *    building a cross-platform feature, you'll need a Windows-specific implementation. For example,
 *    Timeout uses select instead of timers to implement timeouts on Windows.
 * 2. Timer is built using signals, which can cause your Unix calls to error with EINTR, since
 *    you are interupting them. Not a huge deal, just worth being aware of.
 *
 * The Timer implementation generally works as follows:
 *
 * 1. We keep a priority queue of timers, where the shortest timer is at the front
 * 2. The shortest timer is popped off the priority queue and put in current_timer
 * 3. We set a Unix timer to fire when current_timer is due
 * 4. When the Unix timer fires, we execute the current timer's callback and schedule the next timer
 *)

type t = int

type timer = {
  target_time: float;
  callback: unit -> unit;
  id: int;
}

module TimerKey = struct
  type t = timer

  let compare a b = compare a.target_time b.target_time
end

(* Mutable priority queue with O(log(n)) pushes and pops *)
module TimerQueue = PriorityQueue.Make (TimerKey)

let next_id = ref 1

let queue = TimerQueue.make_empty 8

let current_timer = ref None

let cancelled = ref ISet.empty

(* Get's the next timer. Any expired timers have their callbacks invoked *)
let rec get_next_timer ~exns =
  if TimerQueue.is_empty queue then
    (None, List.rev exns)
  else
    let timer = TimerQueue.pop queue in
    (* Skip cancelled timers *)
    if ISet.mem timer.id !cancelled then
      get_next_timer ~exns
    else
      let interval = timer.target_time -. Unix.gettimeofday () in
      if interval <= 0.0 then
        let exns =
          try
            timer.callback ();
            exns
          with exn -> exn :: exns
        in
        get_next_timer ~exns
      else
        (Some timer, List.rev exns)

(* Schedules an alarm for interval seconds *)
let schedule_non_recurring interval =
  Unix.(
    let interval_timer =
      {
        it_interval = 0.0;
        (* Don't restart timer when it finishes *)
        it_value = interval (* How long to wait *);
      }
    in
    ignore (setitimer ITIMER_REAL interval_timer))

external reraise : exn -> 'a = "%reraise"

let rec ding_fries_are_done _ =
  let exns =
    try
      Base.Option.iter !current_timer ~f:(fun timer -> timer.callback ());
      []
    with exn -> [exn]
  in
  current_timer := None;
  schedule ~exns ()

and schedule ?(exns = []) () =
  (* Stop the current timer, if there is one, to avoid races *)
  schedule_non_recurring 0.0;

  (* If there's a current timer, requeue it *)
  Base.Option.iter !current_timer ~f:(TimerQueue.push queue);

  let (timer, exns) = get_next_timer ~exns in
  current_timer := timer;

  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle ding_fries_are_done));

  (* Start the timer back up *)
  Base.Option.iter timer ~f:(fun t ->
      schedule_non_recurring (t.target_time -. Unix.gettimeofday ()));

  (* If we executed more than one callback this time and more than one callback threw an
   * exception, then we just arbitrarily choose one to throw. Oh well :/ *)
  match exns with
  | exn :: _ -> reraise exn
  | _ -> ()

(* Will invoke callback () after interval seconds *)
let set_timer ~interval ~callback =
  let target_time = Unix.gettimeofday () +. interval in
  let id = !next_id in
  incr next_id;
  TimerQueue.push queue { target_time; callback; id };
  (match !current_timer with
  | Some current_timer when target_time >= current_timer.target_time ->
    (* There's currently a timer and the new timer will fire after it. As an optimization we can
       skip scheduling *)
    ()
  | _ ->
    (* There's currently a timer, but the new timer will fire first or there is no current timer *)
    schedule ());
  id

let cancel_timer id =
  cancelled := ISet.add id !cancelled;
  match !current_timer with
  | Some timer when timer.id = id ->
    current_timer := None;
    schedule ()
  | _ -> ()
