(*
 * Copyright (C) 2006-2008 Vincent Hanquez <vincent@snarc.org>
 * Copyright (C) 2014 Peter Zotov <whitequark@whitequark.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 *)

(** Inotify binding for OCaml

    @see <http://man7.org/linux/man-pages/man7/inotify.7.html> Inotify manual page *)

(** Type of event masks. *)
type selector =
  | S_Access
  | S_Attrib
  | S_Close_write
  | S_Close_nowrite
  | S_Create
  | S_Delete
  | S_Delete_self
  | S_Modify
  | S_Move_self
  | S_Moved_from
  | S_Moved_to
  | S_Open
  | S_Dont_follow
  | S_Mask_add
  | S_Oneshot
  | S_Onlydir
  | S_Move
  | S_Close
  | S_All

(** Type of observed events. *)
type event_kind =
  | Access
  | Attrib
  | Close_write
  | Close_nowrite
  | Create
  | Delete
  | Delete_self
  | Modify
  | Move_self
  | Moved_from
  | Moved_to
  | Open
  | Ignored
  | Isdir
  | Q_overflow
  | Unmount

(** Type of watch descriptors. *)
type watch

(** Type of received events, corresponding to [struct inotify_event].
    In event [wd, kinds, cookie, path], [wd] corresponds to [inotify_event.wd],
    [kinds] corresponds to the bits set in [inotify_event.mask], [cookie]
    corresponds to [inotify_event.cookie], [path] is [Some filename] if
    [inotify_event.len > 0] and [None] otherwise. *)
type event = watch * event_kind list * int32 * string option

(** [int_of_watch wd] returns the underlying integer representation of
    watch descriptor [wd]. *)
val int_of_watch : watch -> int

(**/**)

(* [watch_of_int i] is the {!watch} corresponding to the integer
   [i]. It violates the construction privacy of the {!watch} type but
   is useful when using {!event} as a network portable type. *)
val watch_of_int : int -> watch

(**/**)

(** [string_of_event_kind ek] returns the string representation of event kind [ek],
    e.g. [string_of_event_kind Move_self] ≡ ["MOVE_SELF"]. *)
val string_of_event_kind : event_kind -> string

(** [string_of_event event] returns the string representation of event [ev],
    e.g. [string_of_event] *)
val string_of_event : event -> string

(** [create ()] returns a fresh inotify file descriptor or raises
    [Unix.Unix_error(errno, "inotify_init", "")]. *)
val create : unit -> Unix.file_descr

(** [add_watch fd path events] starts observing events from [events] for path [path]
    at inotify file descriptor [fd] and returns a fresh watch descriptor, or raises
    [Unix.Unix_error(errno, "inotify_add_watch", path)]. *)
val add_watch : Unix.file_descr -> string -> selector list -> watch

(** [rm_watch fd watch] stops observing events corresponding to watch descriptor [watch]
    at inotify file descriptor [fd], or raises
    [Unix.Unix_error(errno, "inotify_rm_watch", path)]. *)
val rm_watch : Unix.file_descr -> watch -> unit

(** [read fd] requests a list of events for inotify file descriptor [fd]. Each event
    will include the watch descriptor, which can be used to determine the path that
    caused it, and [Moved_to] and [Moved_from] events will include a cookie that allows
    to associate them with each other.

    If {!read} is not called often enough, the kernel event buffer may overflow, in which
    case the event kind list will consist of [[Q_overflow]]. Such an event would be
    associated with a watch descriptor [-1], never returned from {!add_watch}. *)
val read : Unix.file_descr -> event list
