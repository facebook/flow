(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The environment shared by everyone *)
(*****************************************************************************)

module Time = struct
  type t = int

  let counter = ref 0

  let get () =
    incr counter;
    !counter

  let compare = ( - )

  (* The beginning of times *)
  let bot = 0

  let to_string x = string_of_int x
end

module TimeFiles = MonoidAvl.Make (struct
  (* Timestamp + filename *)
  type elt = Time.t * string

  let compare (_, x) (_, y) = String.compare x y

  type monoelt = Time.t

  let neutral = Time.bot

  let make = fst

  let compose = max
end)

type t = {
  (* The fsnotify environment, we use this for interacting with fsnotify  *)
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

let make roots =
  let fsnotify = Fsnotify.init roots in
  { fsnotify; files = TimeFiles.empty; new_files = SSet.empty; dirs = SMap.empty }
