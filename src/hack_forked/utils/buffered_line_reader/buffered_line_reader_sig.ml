(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * This module is needed because Unix.select doesn't play well with
 * input_line on Ocaml channels.. i.e., when a buffered read into an
 * Ocaml channel consumes two complete lines from the file descriptor, the next
 * select will say there is nothing to read when in fact there is
 * something in the channel. This wouldn't be a problem if Ocaml channel's API
 * supported a "has buffered content" call, so you could check if the
 * buffer contains something as well as doing a Unix select to know for real if
 * there is content coming.
 *
 * The "has_buffered_content" method below does exactly that.
 *
 * is_readable is a friendly wrapper around "has_buffered_content" and
 * non-blocking Unix.select.
 *)

module type S = sig
  type 'a result

  type fd

  type t

  val create : fd -> t

  val get_null_reader : unit -> t result

  val has_buffered_content : t -> bool

  (**
    * Returns true if and only if there is content to be read (does not know if
    * the incoming content is newline-terminated. So we can't actually know
    * if get_next_line will be non-blocking.
    *)
  val is_readable : t -> bool

  val get_fd : t -> fd

  val get_next_line : t -> string result

  val get_next_bytes : t -> int -> string result
end

module type READER = sig
  type 'a result

  type fd

  val return : 'a -> 'a result

  val fail : exn -> 'a result

  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result

  val read : fd -> buffer:bytes -> offset:int -> size:int -> int result

  val is_readable : fd -> bool

  val open_devnull : unit -> fd result
end
