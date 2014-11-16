(*
 * Copyright (C) 2006-2008 Vincent Hanquez <vincent@snarc.org>
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
 * Inotify OCaml binding
 *)
exception Error of string * int

type select_event =
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

type type_event =
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

type wd
type event = wd * type_event list * int32 * string option

val int_of_wd : wd -> int
val string_of_event : type_event -> string

val init : unit -> Unix.file_descr
val add_watch : Unix.file_descr -> string -> select_event list -> wd
val rm_watch : Unix.file_descr -> wd -> unit
val read : Unix.file_descr -> event list
