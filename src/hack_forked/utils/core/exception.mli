(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val wrap : exn -> t

val wrap_unraised : ?frames:int -> exn -> t

val unwrap : t -> exn

val reraise : t -> 'a

val to_exn : t -> exn

val to_string : t -> string

val get_ctor_string : t -> string

val get_backtrace_string : t -> string

val get_current_callstack_string : int -> string

val print_full_backtrace : out_channel -> int -> t -> unit

val get_full_backtrace_string : int -> t -> string

val record_backtrace : bool -> unit
