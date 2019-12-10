(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The environment shared by everyone *)
(*****************************************************************************)

(* This is in fact a fake time module, we don't want to use the "real"
 * unix timestamps, because we would run into atomicity problems.
 * Problems like, what happens if the file was modified between the moment
 * where I checked the time stamp and now etc ...
 * So we maintain our own clock. It is incremented by one on every event.
 *)
module Time : sig
  type t

  val get : unit -> t

  val compare : t -> t -> int

  (* The beginning of times *)
  val bot : t

  val to_string : t -> string
end

(* Our fancy Avl (cf monoidAvl.ml) *)
module TimeFiles : MonoidAvl.S with type elt = Time.t * string with type monoelt = Time.t

type t = {
  (* The fsnotify environment, we use this for interacting with fsnotify *)
  fsnotify: Fsnotify.env;
  (* The set of files with their timestamp *)
  mutable files: TimeFiles.t;
  (* The set of new files (files created during an event) *)
  mutable new_files: SSet.t;
  (* The directories (and the files they contain) *)
  mutable dirs: SSet.t SMap.t;
}

(*****************************************************************************)
(* Building the original environment, this call is called only once
 * by the server (cf dfindServer.ml)
 *)
(*****************************************************************************)

val make : string list -> t
