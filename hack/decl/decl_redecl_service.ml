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
(* On the fly type-declaration are called when the user modified a file
 * we are not at initilalization time anymore. Therefore, we have a bit more
 * work to do. We need calculate what must be re-checked.
 *)
(*****************************************************************************)
open Core
open Typing_deps

(*****************************************************************************)
(* The neutral element of declaration (cf procs/multiWorker.mli) *)
(*****************************************************************************)
let otf_neutral =  Errors.empty, Relative_path.Set.empty
let compute_deps_neutral = DepSet.empty, DepSet.empty

(*****************************************************************************)
(* This is the place where we are going to put everything necessary for
 * the redeclaration. We could "pass" the values directly to the workers,
 * but it gives too much work to the master and slows things downn.
 * So what we do instead is pass the data through shared memory via
 * OnTheFlyStore.
 * I tried replicating the data to speed things up but it had no effect.
 *)
(*****************************************************************************)

module OnTheFlyStore = GlobalStorage.Make(struct
  type t = TypecheckerOptions.t * FileInfo.fast
end)

(*****************************************************************************)
(* Re-declaring the types in a file *)
(*****************************************************************************)

(* Returns a list of files that are considered to have failed decl and must
 * be redeclared every time the typechecker discovers a file change *)
let get_decl_failures decl_errors fn =
  List.fold_left decl_errors ~f:begin fun failed error ->
    (* It is important to add the file that is the cause of the failure.
     * What can happen is that during a declaration phase, we realize
     * that a parent class is outdated. When this happens, we redeclare
     * the class, even if it is in a different file. Therefore, the file
     * where the error occurs might be different from the file we
     * are declaring right now.
     *)
    let file_with_error = Pos.filename (Errors.get_pos error) in
    assert (file_with_error <> Relative_path.default);
    let failed = Relative_path.Set.add failed file_with_error in
    let failed = Relative_path.Set.add failed fn in
    failed
  end ~init:Relative_path.Set.empty

let on_the_fly_decl_file tcopt (errors, failed) fn =
  let decl_errors, () = Errors.do_ begin fun () ->
    Decl.make_env tcopt fn
  end in
  let failed' = get_decl_failures (Errors.get_error_list decl_errors) fn in
  Errors.merge decl_errors errors, Relative_path.Set.union failed failed'

(*****************************************************************************)
(* Given a set of classes, compare the old and the new type and deduce
 * what must be rechecked accordingly.
 *)
(*****************************************************************************)

let compute_classes_deps old_classes new_classes acc classes =
  let to_redecl, to_recheck = acc in
  let rdd, rdc =
    Decl_compare.get_classes_deps old_classes new_classes classes
  in
  let to_redecl = DepSet.union rdd to_redecl in
  let to_recheck = DepSet.union rdc to_recheck in
  to_redecl, to_recheck

(*****************************************************************************)
(* Given a set of functions, compare the old and the new type and deduce
 * what must be rechecked accordingly.
 *)
(*****************************************************************************)

let compute_funs_deps old_funs (to_redecl, to_recheck) funs =
  let rdd, rdc = Decl_compare.get_funs_deps old_funs funs in
  let to_redecl = DepSet.union rdd to_redecl in
  let to_recheck = DepSet.union rdc to_recheck in
  to_redecl, to_recheck

(*****************************************************************************)
(* Given a set of typedefs, compare the old and the new type and deduce
 * what must be rechecked accordingly.
 *)
(*****************************************************************************)

let compute_types_deps old_types (to_redecl, to_recheck) types =
  let rdc = Decl_compare.get_types_deps old_types types in
  let to_redecl = DepSet.union rdc to_redecl in
  let to_recheck = DepSet.union rdc to_recheck in
  to_redecl, to_recheck

(*****************************************************************************)
(* Given a set of global constants, compare the old and the new type and
 * deduce what must be rechecked accordingly.
 *)
(*****************************************************************************)

let compute_gconsts_deps old_gconsts (to_redecl, to_recheck) gconsts =
  let rdd, rdc = Decl_compare.get_gconsts_deps old_gconsts gconsts in
  let to_redecl = DepSet.union rdd to_redecl in
  let to_recheck = DepSet.union rdc to_recheck in
  to_redecl, to_recheck

(*****************************************************************************)
(* Redeclares a list of files
 * And then computes the files that must be redeclared/rechecked by looking
 * at what changed in the signatures of the classes/functions.
 *)
(*****************************************************************************)

let redeclare_files tcopt filel =
  List.fold_left filel
    ~f:(on_the_fly_decl_file tcopt)
    ~init:(Errors.empty, Relative_path.Set.empty)

let otf_decl_files tcopt filel =
  SharedMem.invalidate_caches();
  (* Redeclaring the files *)
  let errors, failed = redeclare_files tcopt filel in
  errors, failed

let compute_deps fast filel =
  let infol =
    List.map filel (fun fn -> Relative_path.Map.find_unsafe fn fast) in
  let names =
    List.fold_left infol ~f:FileInfo.merge_names ~init:FileInfo.empty_names in
  let { FileInfo.n_classes; n_funs; n_types; n_consts } = names in
  let acc = DepSet.empty, DepSet.empty in
  (* Fetching everything at once is faster *)
  let old_funs = Typing_heap.Funs.get_old_batch n_funs in
  let acc = compute_funs_deps old_funs acc n_funs in

  let old_types = Typing_heap.Typedefs.get_old_batch n_types in
  let acc = compute_types_deps old_types acc n_types in

  let old_consts = Typing_heap.GConsts.get_old_batch n_consts in
  let acc = compute_gconsts_deps old_consts acc n_consts in

  let old_classes = Typing_heap.Classes.get_old_batch n_classes in
  let new_classes = Typing_heap.Classes.get_batch n_classes in
  let compare_classes = compute_classes_deps old_classes new_classes in
  let (to_redecl, to_recheck) = compare_classes acc n_classes in

  to_redecl, to_recheck

(*****************************************************************************)
(* Load the environment and then redeclare *)
(*****************************************************************************)

let load_and_otf_decl_files _ filel =
  try
    let tcopt, _ = OnTheFlyStore.load() in
    otf_decl_files tcopt filel
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    flush stdout;
    raise e

let load_and_compute_deps _acc filel =
  try
    let _, fast = OnTheFlyStore.load() in
    compute_deps fast filel
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    flush stdout;
    raise e

(*****************************************************************************)
(* Merges the results coming back from the different workers *)
(*****************************************************************************)

let merge_on_the_fly (errorl1, failed1) (errorl2, failed2) =
  Errors.merge errorl1 errorl2, Relative_path.Set.union failed1 failed2

let merge_compute_deps (to_redecl1, to_recheck1) (to_redecl2, to_recheck2) =
  DepSet.union to_redecl1 to_redecl2, DepSet.union to_recheck1 to_recheck2

(*****************************************************************************)
(* The parallel worker *)
(*****************************************************************************)

let parallel_otf_decl workers bucket_size tcopt fast fnl =
  OnTheFlyStore.store (tcopt, fast);
  let errors, failed =
    MultiWorker.call
      workers
      ~job:load_and_otf_decl_files
      ~neutral:otf_neutral
      ~merge:merge_on_the_fly
      ~next:(Bucket.make ~max_size:bucket_size fnl)
  in
  let to_redecl, to_recheck =
    MultiWorker.call
      workers
      ~job:load_and_compute_deps
      ~neutral:compute_deps_neutral
      ~merge:merge_compute_deps
      ~next:(Bucket.make ~max_size:bucket_size fnl)
  in
  OnTheFlyStore.clear();
  errors, failed, to_redecl, to_recheck

(*****************************************************************************)
(* Code invalidating the heap *)
(*****************************************************************************)

let invalidate_heap { FileInfo.n_funs; n_classes; n_types; n_consts } =
  Typing_heap.Funs.oldify_batch n_funs;
  Typing_heap.Classes.oldify_batch n_classes;
  Typing_heap.Typedefs.oldify_batch n_types;
  Typing_heap.GConsts.oldify_batch n_consts;
  Naming_heap.FunHeap.remove_batch n_funs;
  Naming_heap.ClassHeap.remove_batch n_classes;
  Naming_heap.TypedefHeap.remove_batch n_types;
  Naming_heap.ConstHeap.remove_batch n_consts;
  SharedMem.collect `gentle;
  ()

let remove_old_defs { FileInfo.n_funs; n_classes; n_types; n_consts } =
  Typing_heap.Funs.remove_old_batch n_funs;
  Typing_heap.Classes.remove_old_batch n_classes;
  Typing_heap.Typedefs.remove_old_batch n_types;
  Typing_heap.GConsts.remove_old_batch n_consts;
  SharedMem.collect `gentle;
  ()

let get_defs fast =
  Relative_path.Map.fold fast ~f:begin fun _ names1 names2 ->
    FileInfo.merge_names names1 names2
  end ~init:FileInfo.empty_names

(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)

let redo_type_decl workers ~bucket_size tcopt fast =
  let fnl = Relative_path.Map.keys fast in
  let defs = get_defs fast in
  invalidate_heap defs;
  (* If there aren't enough files, let's do this ourselves ... it's faster! *)
  let result =
    if List.length fnl < 10
    then
      let errors, failed = otf_decl_files tcopt fnl in
      let to_redecl, to_recheck = compute_deps fast fnl in
      errors, failed, to_redecl, to_recheck
    else parallel_otf_decl workers bucket_size tcopt fast fnl
  in
  remove_old_defs defs;
  result
