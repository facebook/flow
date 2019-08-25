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
  (* convenience *)
  | S_Move
  | S_Close
  | S_All

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

let string_of_event_kind = function
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

type watch = int

type event = watch * event_kind list * int32 * string option

external create : unit -> Unix.file_descr = "caml_inotify_init"

external add_watch : Unix.file_descr -> string -> selector list -> watch
  = "caml_inotify_add_watch"

external rm_watch : Unix.file_descr -> watch -> unit = "caml_inotify_rm_watch"

external convert : Bytes.t -> watch * event_kind list * int32 * int
  = "caml_inotify_convert"

external struct_size : unit -> int = "caml_inotify_struct_size"

external name_max : unit -> int = "caml_inotify_name_max"

let int_of_watch watch = watch

let watch_of_int watch = watch

let string_of_event (watch, events, cookie, name) =
  Printf.sprintf
    "watch=%d cookie=%ld events=%s%s"
    watch
    cookie
    (String.concat "|" (List.map string_of_event_kind events))
    (match name with
    | None -> ""
    | Some name' -> Printf.sprintf " %S" name')

let read fd =
  (* Turns out that reading from blocking descriptors always requires a buffer
     of the maximum size, which is, from the inotify man page:

       The behavior when the buffer given to read(2) is too small to return
       information about the next event depends on the kernel version: in
       kernels before 2.6.21, read(2) returns 0; since kernel 2.6.21,
       read(2) fails with the error EINVAL.  Specifying a buffer of size

           sizeof(struct inotify_event) + NAME_MAX + 1
   *)
  let event_size = struct_size () in
  let buf_size = event_size + name_max () + 1 in
  let buf = Bytes.create buf_size in
  let bytes_read = Unix.read fd buf 0 buf_size in
  let read_c_string pos =
    Bytes.sub_string buf pos (Bytes.index_from buf pos '\x00' - pos)
  in
  let rec read_one pos rest =
    if bytes_read < pos + event_size then
      rest
    else
      let (watch, mask, cookie, len) =
        convert (Bytes.sub buf pos event_size)
      in
      if bytes_read < pos + event_size + len then
        rest
      else
        let name =
          if len > 0 then
            Some (read_c_string (pos + event_size))
          else
            None
        in
        read_one (pos + event_size + len) ((watch, mask, cookie, name) :: rest)
  in
  List.rev (read_one 0 [])
