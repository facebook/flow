(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* It is useful to write infinite loops using lwt. They're useful for reading, for writing, for
 * waiting for some broadcast message, for doing something every N seconds, etc. However, there's a
 * bunch of boilerplate. LwtLoop.Make is a functor that tries to hide that boilerplate
 *)

module type LOOP = sig
  type acc

  (* A single iteration of the loop *)
  val main : acc -> acc Lwt.t

  (* Wraps each iteration of the loop. On an exception, the most recent acc is passed in and the
   * loop is canceled *)
  val catch : acc -> exn -> unit Lwt.t
end

module Make (Loop : LOOP) : sig
  val run : ?cancel_condition:'a Lwt_condition.t -> Loop.acc -> unit Lwt.t
end = struct
  let catch acc exn =
    match exn with
    (* Automatically handle Canceled. No one ever seems to want to handle it manually. *)
    | Lwt.Canceled -> Lwt.return_unit
    | exn -> Loop.catch acc exn

  let rec loop acc = Lwt.try_bind (fun () -> Loop.main acc) loop (catch acc)

  let run ?cancel_condition acc =
    (* Create a waiting thread *)
    let (waiter, wakener) = Lwt.task () in
    (* When the waiting thread is woken, it will kick off the loop *)
    let thread =
      let%lwt ret = waiter in
      loop ret
    in
    (* If there is a cancel condition variable, wait for it to fire and then cancel the loop *)
    begin
      match cancel_condition with
      | None -> ()
      | Some condition ->
        (* If the condition is hit, cancel the loop thread. If the loop thread finishes, cancel the
         * condition wait *)
        Lwt.async (fun () ->
            Lwt.pick
              [
                (try%lwt thread with Lwt.Canceled -> Lwt.return_unit);
                (let%lwt _ = Lwt_condition.wait condition in
                 Lwt.return_unit);
              ])
    end;

    (* Start things going *)
    Lwt.wakeup wakener acc;

    thread
end
