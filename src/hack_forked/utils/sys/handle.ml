(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* On Win32, unwrap the handle from the 'abstract block' representing
   the file descriptor otherwise it can't be marshalled or passed as
   an integer command-line argument. *)

type handle = int

external raw_get_handle : Unix.file_descr -> handle = "caml_hh_worker_get_handle" [@@noalloc]

external raw_wrap_handle : handle -> Unix.file_descr = "caml_hh_worker_create_handle"

external win_setup_handle_serialization : unit -> unit = "win_setup_handle_serialization"

let init =
  (* Windows: register the serialize/desarialize functions
     for the custom block of "Unix.file_descr". *)
  lazy (win_setup_handle_serialization ())

let () = Lazy.force init

let () = assert (Sys.win32 || Obj.is_int (Obj.repr Unix.stdin))

let get_handle =
  if Sys.win32 then
    raw_get_handle
  else
    Obj.magic

let wrap_handle =
  if Sys.win32 then
    raw_wrap_handle
  else
    Obj.magic

let to_in_channel h = wrap_handle h |> Unix.in_channel_of_descr

let to_out_channel h = wrap_handle h |> Unix.out_channel_of_descr
