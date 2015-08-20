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

let process_fun_id results_acc target_fun id =
  if target_fun = (snd id)
  then results_acc := Pos.Map.add (fst id) (snd id) !results_acc

let process_method_id results_acc target_classes target_method
    class_ id _ _ ~is_method =
  let class_name = class_.Typing_defs.tc_name in
  if target_method = (snd id) && (SSet.mem class_name target_classes)
  then
    results_acc :=
      Pos.Map.add (fst id) (class_name ^ "::" ^ (snd id)) !results_acc

let process_constructor results_acc target_classes target_method class_ _ p =
  process_method_id
    results_acc target_classes target_method class_ (p, "__construct")
    () () ~is_method:true

let process_class_id results_acc target_classes cid mid_option =
   if (SSet.mem (snd cid) target_classes)
   then begin
     let class_name = match mid_option with
     | None -> snd cid
     | Some n -> (snd cid)^"::"^(snd n) in
     results_acc := Pos.Map.add (fst cid) class_name !results_acc
   end

let attach_hooks results_acc target_classes target_fun =
  match target_classes, target_fun with
    | Some classes, Some method_name ->
      let process_method_id =
        process_method_id results_acc classes method_name
      in
      Typing_hooks.attach_cmethod_hook process_method_id;
      Typing_hooks.attach_smethod_hook process_method_id;
      Typing_hooks.attach_constructor_hook
        (process_constructor results_acc classes method_name);
    | None, Some fun_name ->
      Typing_hooks.attach_fun_id_hook (process_fun_id results_acc fun_name)
    | Some classes, None ->
      Typing_hooks.attach_class_id_hook (process_class_id results_acc classes)
    | _ -> assert false

let detach_hooks () =
  Typing_hooks.remove_all_hooks ()

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
      Typing_compare.get_extend_deps cid_hash
        (Typing_deps.DepSet.singleton cid_hash)
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

let find_refs target_classes target_method acc fileinfo_l =
  let results_acc = ref Pos.Map.empty in
  attach_hooks results_acc target_classes target_method;
  let nenv = Naming.empty TypecheckerOptions.permissive in
  ServerIdeUtils.recheck nenv fileinfo_l;
  detach_hooks ();
  Pos.Map.fold begin fun p str acc ->
    (str, p) :: acc
  end !results_acc []

let parallel_find_refs workers fileinfo_l target_classes target_method =
  MultiWorker.call
    workers
    ~job:(find_refs target_classes target_method)
    ~neutral:([])
    ~merge:(List.rev_append)
    ~next:(Bucket.make fileinfo_l)

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

let find_references workers target_classes target_method include_defs
    files_info files =
  let fileinfo_l = Relative_path.Set.fold (fun fn acc ->
    match Relative_path.Map.get fn files_info with
    | Some fi -> fi :: acc
    | None -> acc) files [] in
  let results =
    if List.length fileinfo_l < 10 then
      find_refs target_classes target_method [] fileinfo_l
    else
      parallel_find_refs workers fileinfo_l target_classes target_method
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
