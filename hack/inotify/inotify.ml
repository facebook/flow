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
	(* convenience *)
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

let string_of_event = function
	| Access -> "ACCESS"
	| Attrib -> "ATTRIB"
	| Close_write -> "CLOSE_WRITE"
	| Close_nowrite -> "CLOSE_NOWRITE"
	| Create -> "CREATE"
	| Delete -> "DELETE"
	| Delete_self -> "DELETE_SELF"
	| Modify -> "MODIFY"
	| Move_self -> "MOVE_SELF"
	| Moved_from -> "MOVED_FROM"
	| Moved_to -> "MOVED_TO"
	| Open -> "OPEN"
	| Ignored -> "IGNORED"
	| Isdir -> "ISDIR"
	| Q_overflow -> "Q_OVERFLOW"
	| Unmount -> "UNMOUNT"

let int_of_wd wd = wd

type wd = int
type event = wd * type_event list * int32 * string option

external init : unit -> Unix.file_descr = "stub_inotify_init"
external add_watch : Unix.file_descr -> string -> select_event list -> wd
                   = "stub_inotify_add_watch"
external rm_watch : Unix.file_descr -> wd -> unit = "stub_inotify_rm_watch"
external convert : string -> (wd * type_event list * int32 * int)
                 = "stub_inotify_convert"
external struct_size : unit -> int = "stub_inotify_struct_size"

external to_read : Unix.file_descr -> int = "stub_inotify_ioctl_fionread"

let read fd =
	let ss = struct_size () in
	let toread = to_read fd in

	let ret = ref [] in
	let buf = String.make toread '\000' in
	let toread = Unix.read fd buf 0 toread in

	let read_c_string offset len =
		let index = ref 0 in
		while !index < len && buf.[offset + !index] <> '\000' do incr index done;
		String.sub buf offset !index
		in

	let i = ref 0 in

	while !i < toread
	do
		let wd, l, cookie, len = convert (String.sub buf !i ss) in
		let s = if len > 0 then Some (read_c_string (!i + ss) len) else None in
		ret := (wd, l, cookie, s) :: !ret;
		i := !i + (ss + len);
	done;

	List.rev !ret

let _ = Callback.register_exception "inotify.error" (Error ("register_callback", 0))

