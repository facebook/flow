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

module Ocaml_stack = Stack

module Stack : sig
  type 'a t = 'a Stack.t

  exception Empty

  val create : unit -> 'a t

  val push : 'a -> 'a t -> unit

  val pop : 'a t -> 'a

  val top : 'a t -> 'a

  val clear : 'a t -> unit

  val copy : 'a t -> 'a t

  val is_empty : 'a t -> bool

  val length : 'a t -> int

  val iter : ('a -> unit) -> 'a t -> unit

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val merge_bytes : string Stack.t -> string
end
