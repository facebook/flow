(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type cstring =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external load :
  int -> int -> cstring option =
  "caml_hh_win32res_load_resource"

external unsafe_blit :
  cstring -> int -> string -> int -> int -> unit =
  "caml_hh_win32res_blit_to_string"

let to_string b =
  let len = Bigarray.Array1.dim b in
  let s = String.create len in
  unsafe_blit b 0 s 0 len;
  s

type item =
  | Dir of string * index
  | File of string * int
and index = item list

let rec read_index_raw data =
  let data = to_string data in
  let lines = Str.split (Str.regexp "\n") data in
  List.map
    (fun line ->
      Scanf.sscanf line "%d %s %s"
        (fun id kind name ->
           match kind with
           | "file" -> File (name, id)
           | "dir" -> Dir (name, read_index_exn id)
           | kind -> Printf.ksprintf failwith "Unexpected kind %s." kind))
    lines
and read_index_exn node =
  match read_index_opt node with
  | None ->
      Printf.ksprintf failwith "Win32res: can't read index #%d." node
  | Some idx -> idx
and read_index_opt node =
  match load node 256 with
  | None -> None
  | Some data -> Some (read_index_raw data)

let dump_file file id =
  let src =
    match load id 257 with
    | None ->
        Printf.ksprintf failwith "Win32res: can't read file #%d." id
    | Some data -> data in
  let len = Bigarray.Array1.dim src in
  let fd = Unix.(openfile file [O_RDWR;O_CREAT;O_TRUNC] 0o644) in
  let dst =
    Bigarray.Array1.map_file fd
      Bigarray.int8_unsigned Bigarray.c_layout true len in
  Bigarray.Array1.blit src dst;
  Unix.close fd

let rec dump_item root = function
  | Dir (name, idx) ->
      let root = Filename.concat root name in
      Unix.mkdir root 0o755;
      List.iter (dump_item root) idx
  | File (name, id) ->
      let file = Filename.concat root name in
      dump_file file id


let read_index () = read_index_opt 100
let dump_files root idx =
  let root = Path.to_string root in
  List.iter (dump_item root) idx
