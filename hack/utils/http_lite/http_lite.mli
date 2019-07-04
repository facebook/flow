(**
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
*)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

exception Malformed of string
val read_headers : Buffered_line_reader.t -> string list
val parse_headers_to_lowercase_map : string list -> string SMap.t
val parse_charset : string -> string option
val read_message_utf8 : Buffered_line_reader.t -> string
val write_message : out_channel -> string -> unit
