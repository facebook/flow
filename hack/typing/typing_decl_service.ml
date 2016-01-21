(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Module declaring the types in parallel *)
(*****************************************************************************)

open Core
open Utils

(* The set of files that failed *)
type failed = Relative_path.Set.t

(* The result expected from the service *)
type result = Errors.t * failed

(*****************************************************************************)
(* The place where we store the shared data in cache *)
(*****************************************************************************)

module TypeDeclarationStore = GlobalStorage.Make(struct
  type t = TypecheckerOptions.t
end)

(*****************************************************************************)
(* Synchronizes the typing environment with the cache *)
(*****************************************************************************)

(*****************************************************************************)
(* The job that will be run on the workers *)
(*****************************************************************************)

let decl_file tcopt (errorl, failed) fn =
  let errorl', () = Errors.do_ begin fun () ->
    d ("Typing decl: "^Relative_path.to_absolute fn);
    Typing_decl.make_env tcopt fn;
    dn "OK";
  end
  in
  let failed =
    if errorl' = [] then failed
    else Relative_path.Set.add fn failed in
  let errorl = List.rev_append (List.rev errorl') errorl in
  errorl, failed

let decl_files (errors, failed) fnl =
  let tcopt = TypeDeclarationStore.load() in
  List.fold_left fnl ~f:(decl_file tcopt) ~init:(errors, failed)

(*****************************************************************************)
(* Merges the results (used by the master) *)
(*****************************************************************************)

let merge_decl (errors1, failed1) (errors2, failed2) =
  errors1 @ errors2,
  Relative_path.Set.union failed1 failed2

(*****************************************************************************)
(* Let's go! That's where the action is *)
(*****************************************************************************)

let go (workers:Worker.t list option) ~bucket_size tcopt fast =
  TypeDeclarationStore.store tcopt;
  let fast_l = Relative_path.Map.fold (fun x _ y -> x :: y) fast [] in
  let neutral = [], Relative_path.Set.empty in
  dn "Declaring the types";
  let result =
    MultiWorker.call
      workers
      ~job:decl_files
      ~neutral
      ~merge:merge_decl
      ~next:(Bucket.make ~max_size:bucket_size fast_l)
  in
  TypeDeclarationStore.clear();
  result
