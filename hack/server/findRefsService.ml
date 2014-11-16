(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

let check_if_extends_class target_class_name class_name acc =
  let class_ = Typing_env.Classes.get class_name in
  match class_ with
  | None -> acc
  | Some { Typing_defs.tc_ancestors = imps; _ }
      when SMap.mem target_class_name imps -> SSet.add class_name acc
  | _ -> acc

let find_child_classes target_class_name files_info files =
  SharedMem.invalidate_caches();
  Relative_path.Set.fold begin fun fn acc ->
    (try
      let { FileInfo.classes; _ } =
        Relative_path.Map.find_unsafe fn files_info in
      List.fold_left begin fun acc cid ->
         check_if_extends_class target_class_name (snd cid) acc
        end acc classes
    with Not_found ->
      acc)
  end files SSet.empty

let get_child_classes_files workers files_info class_name =
  match Naming_heap.ClassHeap.get class_name with
  | Some class_ ->
    (* Find the files that contain classes that extend class_ *)
    let cid = snd class_.Nast.c_name in
    let cid_hash = Typing_deps.Dep.make (Typing_deps.Dep.Class cid) in
    let extend_deps =
        Typing_compare.get_extend_deps cid_hash (ISet.singleton cid_hash)
    in
    Typing_deps.get_files extend_deps
  | _ ->
    Relative_path.Set.empty

let get_deps_set classes =
  SSet.fold (fun class_name acc ->
    match Naming_heap.ClassHeap.get class_name with
    | Some class_ ->
        (* Get all files with dependencies on this class *)
        let fn = Pos.filename (fst class_.Nast.c_name) in
        let cid = snd class_.Nast.c_name in
        let dep = Typing_deps.Dep.Class cid in
        let bazooka = Typing_deps.get_bazooka dep in
        let files = Typing_deps.get_files bazooka in
        let files = Relative_path.Set.add fn files in
        Relative_path.Set.union files acc
    | _ -> acc) classes Relative_path.Set.empty

let get_deps_set_function f_name =
  try
    let fun_ = Naming_heap.FunHeap.find_unsafe f_name in
    let fn = Pos.filename (fst fun_.Nast.f_name) in
    let fid = snd fun_.Nast.f_name in
    let dep = Typing_deps.Dep.Fun fid in
    let bazooka = Typing_deps.get_bazooka dep in
    let files = Typing_deps.get_files bazooka in
    Relative_path.Set.add fn files
  with Not_found -> Relative_path.Set.empty

let find_refs target_classes target_method acc file_names =
  Find_refs.find_refs_class_name := target_classes;
  Find_refs.find_refs_method_name := target_method;
  Find_refs.find_refs_results := Pos.Map.empty;
  ServerIdeUtils.recheck file_names;
  let result = !Find_refs.find_refs_results in
  Find_refs.find_refs_class_name := None;
  Find_refs.find_refs_method_name := None;
  Find_refs.find_refs_results := Pos.Map.empty;
  Pos.Map.fold begin fun p str acc ->
    (str, p) :: acc
  end result []

let parallel_find_refs workers files target_classes target_method =
  MultiWorker.call
    workers
    ~job:(find_refs target_classes target_method)
    ~neutral:([])
    ~merge:(List.rev_append)
    ~next:(Bucket.make files)

let get_definitions target_classes target_method =
  match target_classes, target_method with
  | Some classes, Some method_name ->
      SSet.fold begin fun class_name acc ->
        match Naming_heap.ClassHeap.get class_name with
        | Some class_ ->
            let methods = class_.Nast.c_methods @ class_.Nast.c_static_methods in
            List.fold_left begin fun acc method_ ->
              let mid = method_.Nast.m_name in
              if (snd mid) = method_name then ((snd mid), (fst mid)) :: acc
              else acc
            end acc methods
        | None -> acc
      end classes []
  | Some classes, None ->
      SSet.fold begin fun class_name acc ->
        match Naming_heap.ClassHeap.get class_name with
        | Some class_ ->
            let cid = class_.Nast.c_name in
            ((snd cid), (fst cid)) :: acc
        | None -> acc
      end classes []
  | None, Some fun_name ->
      begin
        match Naming_heap.FunHeap.get fun_name with
        | Some fun_ ->
            let fid = fun_.Nast.f_name in
            [(snd fid), (fst fid)]
        | None -> []
      end
  | None, None -> []

let find_references workers target_classes target_method include_defs file_list =
  let results =
    if List.length file_list < 10 then
      find_refs target_classes target_method [] file_list
    else
      parallel_find_refs workers file_list target_classes target_method
    in
  if include_defs then
    let defs = get_definitions target_classes target_method in
    List.rev_append defs results
  else
    results

let get_dependent_files_function workers f_name =
  (* This is performant enough to not need to go parallel for now *)
  get_deps_set_function f_name

let get_dependent_files workers input_set =
  (* This is performant enough to not need to go parallel for now *)
  get_deps_set input_set
