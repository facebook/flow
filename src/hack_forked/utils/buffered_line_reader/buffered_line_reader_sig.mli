(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This `.mli` file was generated automatically. It may include extra
   definitions that should not actually be exposed to the caller. If you notice
   that this interface file is a poor interface, please take a few minutes to
   clean it up manually, and then delete this comment once the interface is in
   shape. *)

module type S = sig
  type 'a result

  type fd

  type t

  val create : fd -> t

  val get_null_reader : unit -> t result

  val has_buffered_content : t -> bool

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
