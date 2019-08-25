(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type 'a t = {
  mutable messages: 'a ImmQueue.t;
  mutable is_open: bool;
  cv: unit Lwt_condition.t;
      (** Broadcasted whenever any of the above state changes. *)
}

let create () : 'a t =
  { messages = ImmQueue.empty; is_open = true; cv = Lwt_condition.create () }

let set_messages (queue : 'a t) (messages : 'a ImmQueue.t) : unit =
  queue.messages <- messages;
  Lwt_condition.broadcast queue.cv ()

let set_is_open (queue : 'a t) (is_open : bool) : unit =
  queue.is_open <- is_open;
  Lwt_condition.broadcast queue.cv ()

let push (queue : 'a t) (message : 'a) : bool =
  if queue.is_open then (
    set_messages queue (ImmQueue.push queue.messages message);
    true
  ) else
    false

let rec pop (queue : 'a t) : 'a option Lwt.t =
  match (queue.is_open, ImmQueue.pop queue.messages) with
  | (false, _) -> Lwt.return None
  | (true, (None, _)) ->
    let%lwt () = Lwt_condition.wait queue.cv in
    pop queue
  | (true, (Some hd, tl)) ->
    set_messages queue tl;
    Lwt.return (Some hd)

let close (queue : 'a t) : unit =
  set_messages queue ImmQueue.empty;
  set_is_open queue false

let is_empty (queue : 'a t) : bool = ImmQueue.is_empty queue.messages

let length (queue : 'a t) : int = ImmQueue.length queue.messages

let exists (queue : 'a t) ~(f : 'a -> bool) : bool =
  ImmQueue.exists ~f queue.messages
