(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

val timestamp_string : unit -> string

val dupe_log : (string * out_channel) option ref

val set_log : string -> out_channel -> unit

val get_log_name : unit -> string option

val set_id : string -> unit

val print_with_newline :
  ?exn:Exception.t -> ('a, unit, string, unit) format4 -> 'a

val print_duration : string -> float -> float

val exc : ?prefix:string -> stack:string -> exn -> unit

module Level : sig
  type t =
    | Off
    | Fatal
    | Error
    | Warn
    | Info
    | Debug

  val min_level : unit -> t

  val set_min_level : t -> unit

  val passes_min_level : t -> bool

  val log_duration : t -> string -> float -> float
end

val log :
  ?lvl:Level.t -> ('a, unit, string, string, string, unit) format6 -> 'a

val log_duration : string -> float -> float

val fatal :
  ?exn:Exception.t -> ('a, unit, string, string, string, unit) format6 -> 'a

val error :
  ?exn:Exception.t -> ('a, unit, string, string, string, unit) format6 -> 'a

val warn :
  ?exn:Exception.t -> ('a, unit, string, string, string, unit) format6 -> 'a

val info :
  ?exn:Exception.t -> ('a, unit, string, string, string, unit) format6 -> 'a

val debug :
  ?exn:Exception.t -> ('a, unit, string, string, string, unit) format6 -> 'a
