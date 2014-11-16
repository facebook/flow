(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* The environment shared by everyone *)
(*****************************************************************************)

module SSet: Set.S with type elt = String.t
module SMap: Map.S with type key = String.t

(* This is in fact a fake time module, we don't want to use the "real"
 * unix timestamps, because we would run into atomicity problems.
 * Problems like, what happens if the file was modified between the moment
 * where I checked the time stamp and now etc ...
 * So we maintain our own clock. It is incremented by one on every event.
 *)
module Time: sig
  type t

  val get: unit -> t
  val compare: t -> t -> int

  (* The beginning of times *)
  val bot: t

  val to_string: t -> string
end

(* Our fancy Avl (cf monoidAvl.ml) *)
module TimeFiles: MonoidAvl.S
with type elt = Time.t * string
with type monoelt = Time.t


type dir = string

(* A handle is used to dissociate the different services/users
 * An example:
 * $ dfind my_dir my_handle1
 *
 * This will, on the first call print all the files/directories in my_dir
 * On the next call, it will only print what has changed (the whole purpose
 * of dfind after all).
 *
 * Now using a different handle allows us to have multiple instances of
 * dfind. If I write:
 * $ dfind my_dir my_handle2
 * This will restart from scratch, and show me all the files in my_dir.
 *
*)
type handle = string

(* The directory a client cares about + its handle + the channel where we
 * will output the answer.
*)
type client = dir * handle * Unix.file_descr
type output

type t = {
    (* The list of things left to output *)
    mutable to_output : output list                           ;

    (* The fsnotify environment, we use this for interacting with fsnotify  *)
            fsnotify  : Fsnotify.env                          ;

    (* The set of files with their timestamp *)
    mutable files     : TimeFiles.t                           ;

    (* The set of new files (files created during an event) *)
    mutable new_files : SSet.t                                ;

    (* The directories (and the files they contain) *)
    mutable dirs      : SSet.t SMap.t                         ;

    (* (directory, handle) -> time (last time it was checked) *)
            chandles  : ((dir * handle), Time.t) Hashtbl.t    ;

    (* The list of clients listening to changes *)
    mutable clientl   : client list                           ;

    (* The output channel where we want to log errors (/tmp/dfind.log) *)
            log       : out_channel                           ;

    (* Keeps track of when the last query came in, so we can exit after
       a period of inactivity. *)
    mutable last_query : float                                ;

    (* Time since the process started *)
            start_time : float                                ;
  }

(*****************************************************************************)
(* Functions used to update handles *)
(*****************************************************************************)

val get_handle: t -> handle * dir -> Time.t option
val set_handle: t -> handle * dir -> Time.t -> unit

(*****************************************************************************)
(* Functions used to update the client list *)
(*****************************************************************************)

val add_client: t -> client -> unit
val get_clients: t -> client list

(*****************************************************************************)
(* Building the original environment, this call is called only once
 * by the server (cf server.ml)
 *)
(*****************************************************************************)

val make: string -> t

(*****************************************************************************)
(* The environment variable containing the pattern we want to skip *)
(*****************************************************************************)

val skip_var: string

(*****************************************************************************)
(* Outputing *)
(*****************************************************************************)

val add_output: t -> close: bool -> Unix.file_descr -> SSet.t -> unit
val get_output_descrl: t -> Unix.file_descr list
val output: t -> unit
