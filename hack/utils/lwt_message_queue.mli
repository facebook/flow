(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type 'a t
(** A mutable queue containing messages of type ['a]. *)

val create : unit -> 'a t
(** Create a new [Lwt_message_queue.t]. *)

val push : 'a t -> 'a -> bool
(** Push a message into the queue. Wakes up the task waiting to [pop] from it,
if any. *)

val pop : 'a t -> 'a option Lwt.t
(** Get and remove the next message in the queue. If there are currently no
messages in the queue, wait until one becomes available. If the queue is or
becomes closed, returns [None]; otherwise returns [Some message].

The behavior of multiple tasks waiting to [pop] the queue simultaneously is
unspecified (similar to issues raised in
https://github.com/ocsigen/lwt/issues/250). Only one task should [pop] at a
time. The message queue is therefore mostly useful for code organization
purposes, making it possible to split the code for the producer and consumer of
the message queue in a principled way. *)

val close : 'a t -> unit
(** Close the message queue for further reads and writes. All messages currently
in the queue will be dropped. Future calls to [push] will return [false], and
future calls to [pop] will return [None].

Either the producer or consumer end of the queue may close it. *)

val is_empty : 'a t -> bool
(** Whether or not the queue has any pending messages at this moment. *)

val length : 'a t -> int
(** Returns the number of messages currently in the queue. If the queue is
closed, returns [0]. *)

val exists : 'a t -> f:('a -> bool) -> bool
(** Returns whether or not a message satisfying predicate [f] exists in the
current queue. *)
