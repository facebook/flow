(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module ArgSpec : sig
  type 'a flag_t
  type ('a, 'b) t

  type flag_arg_count =
  | No_Arg
  | Arg
  | Arg_List
  | Arg_Rest

  type flag_metadata = {
    doc : string;
    arg_count : flag_arg_count;
  }

  val empty : ('a, 'a) t
  val flag : string -> 'a flag_t -> doc:string -> ('b, 'a -> 'c) t -> ('b, 'c) t
  val anon : string -> 'a flag_t -> doc:string -> ('b, 'a -> 'c) t -> ('b, 'c) t
  val rest : doc:string -> ('a, string list option -> 'b) t -> ('a, 'b) t
  val dummy : 'a -> ('b, 'a -> 'c) t -> ('b, 'c) t
  val collect : ('main -> 'a -> 'new_main) -> ('b, 'main) t -> ('b, 'a -> 'new_main) t

  val no_arg : bool flag_t
  val string : string option flag_t
  val bool : bool option flag_t
  val int : int option flag_t
  val enum : string list -> string option flag_t

  val required : 'a option flag_t -> 'a flag_t
  val optional : 'a option flag_t -> 'a option flag_t
  val list_of : 'a option flag_t -> 'a list option flag_t
end

type ('a, 'b) builder_t = {
  name : string;
  doc : string;
  usage : string;
  args : ('a, 'b) ArgSpec.t;
}

type t

exception Show_help
exception Failed_to_parse of string

val usage : ('a, 'b) builder_t -> unit

val command : ('main, unit -> unit) builder_t -> 'main -> t

(* accessors *)
val run : t -> string list SMap.t -> unit
val name : t -> string
val doc : t -> string
val flags : t -> ArgSpec.flag_metadata SMap.t
val args_of_argv : t -> string list -> string list SMap.t
val string_of_usage : t -> string
