(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type record

type record_data

val create : unit -> record

val push_global : unit -> unit

val pop_global : unit -> record

val serialize : record -> record_data

val deserialize : record_data -> record

val track_distribution : ?record:record -> string -> bucket_size:float -> unit

val sample : ?record:record -> ?weight:float -> string -> float -> unit

val time : ?record:record -> string -> (unit -> 'a) -> 'a

val delete : ?record:record -> string -> unit

val merge : ?record:record -> record -> unit

val get_sum : ?record:record -> string -> float option

val get_mean : ?record:record -> string -> float option

val get_count : ?record:record -> string -> float option

val get_max : ?record:record -> string -> float option

val print_entry_stats : ?record:record -> ?print_raw:(string -> unit) -> string -> unit

val print_stats : ?record:record -> ?print_raw:(string -> unit) -> unit -> unit

val print_entry_distribution : ?record:record -> string -> unit

val print_distributions : ?record:record -> unit -> unit
