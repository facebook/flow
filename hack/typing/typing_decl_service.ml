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
(* Module declaring the types in parallel *)
(*****************************************************************************)
open Utils

(* The set of files that failed *)
type failed = Relative_path.Set.t

(* The result excepted from the service *)
type result = Errors.t * failed

(*****************************************************************************)
(* The place where we store the shared data in cache *)
(*****************************************************************************)

module TypeDeclarationStore = GlobalStorage.Make(struct
  type classes = Relative_path.Set.t SMap.t
  type t = classes * Naming.env
end)

(*****************************************************************************)
(* Synchronizes the typing environment with the cache *)
(*****************************************************************************)

(*****************************************************************************)
(* The job that will be run on the workers *)
(*****************************************************************************)

let decl_file all_classes nenv (errorl, failed) fn =
  let errorl', () = Errors.do_ begin fun () ->
    d ("Typing decl: "^Relative_path.to_absolute fn);
    Typing_decl.make_env nenv all_classes fn;
    dn "OK";
  end
  in
  let failed =
    if errorl' = [] then failed
    else Relative_path.Set.add fn failed in
  let errorl = List.rev_append (List.rev errorl') errorl in
  errorl, failed

let decl_files (errors, failed) fnl =
  let all_classes, nenv = TypeDeclarationStore.load() in
  List.fold_left (decl_file all_classes nenv) (errors, failed) fnl

(*****************************************************************************)
(* Merges the results (used by the master) *)
(*****************************************************************************)

let merge_decl (errors1, failed1) (errors2, failed2) =
  errors1 @ errors2,
  Relative_path.Set.union failed1 failed2

(*****************************************************************************)
(* We need to know all the classes defined, because we want to declare
 * the types in their topological order.
 * We keep the files in which the classes are defined, sometimes there
 * can be more that one file when there are name collitions.
 *)
(*****************************************************************************)

let get_classes fast =
  Relative_path.Map.fold begin fun fn {FileInfo.n_classes = classes; _} acc ->
    SSet.fold begin fun c_name acc ->
      let files =
        try SMap.find_unsafe c_name acc
        with Not_found -> Relative_path.Set.empty
      in
      let files = Relative_path.Set.add fn files in
      SMap.add c_name files acc
    end classes acc
  end fast SMap.empty

(*****************************************************************************)
(* Let's go! That's where the action is *)
(*****************************************************************************)

let go (workers:Worker.t list option) nenv fast =
  let all_classes = get_classes fast in
  TypeDeclarationStore.store (all_classes, nenv);
  let fast_l = Relative_path.Map.fold (fun x _ y -> x :: y) fast [] in
  let neutral = [], Relative_path.Set.empty in
  dn "Declaring the types";
  let result =
    MultiWorker.call
      workers
      ~job:decl_files
      ~neutral
      ~merge:merge_decl
      ~next:(Bucket.make fast_l)
  in
  TypeDeclarationStore.clear();
  result
