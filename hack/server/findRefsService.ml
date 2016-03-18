(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Reordered_argument_collections
open Typing_defs

type action = Ai.ServerFindRefs.action =
  | Class of string
  | Method of string * string
  | Function of string

type result = (string * Pos.absolute) list

let process_fun_id results_acc target_fun id =
  if target_fun = (snd id)
  then results_acc := Pos.Map.add (fst id) (snd id) !results_acc

let process_method_id results_acc target_classes target_method
    class_ id _ _ ~is_method =
  let class_name = class_.Typing_defs.tc_name in
  if target_method = (snd id) && (SSet.mem target_classes class_name)
  then
    results_acc :=
      Pos.Map.add (fst id) (class_name ^ "::" ^ (snd id)) !results_acc

let process_constructor results_acc target_classes target_method class_ _ p =
  process_method_id
    results_acc target_classes target_method class_ (p, "__construct")
    () () ~is_method:true

let process_class_id results_acc target_classes cid mid_option =
   if (SSet.mem target_classes (snd cid))
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
        process_method_id results_acc classes method_name in
      Typing_hooks.attach_cmethod_hook process_method_id;
      Typing_hooks.attach_smethod_hook process_method_id;
      Typing_hooks.attach_constructor_hook
        (process_constructor results_acc classes method_name);
    | None, Some fun_name ->
      Typing_hooks.attach_fun_id_hook (process_fun_id results_acc fun_name)
    | Some classes, None ->
      Decl_hooks.attach_class_id_hook (process_class_id results_acc classes)
    | _ -> assert false

let detach_hooks () =
  Decl_hooks.remove_all_hooks ();
  Typing_hooks.remove_all_hooks ()

let check_if_extends_class target_class_name class_name acc =
  let class_ = Typing_heap.Classes.get class_name in
  match class_ with
  | None -> acc
  | Some { Typing_defs.tc_ancestors = imps; _ }
      when SMap.mem target_class_name imps -> SSet.add acc class_name
  | _ -> acc

let find_child_classes target_class_name files_info files =
  SharedMem.invalidate_caches();
  Relative_path.Set.fold begin fun fn acc ->
    (try
      let { FileInfo.classes; _ } =
        Relative_path.Map.find_unsafe fn files_info in
      List.fold_left classes ~init:acc ~f:begin fun acc cid ->
        check_if_extends_class target_class_name (snd cid) acc
      end
    with Not_found ->
      acc)
  end files SSet.empty

let get_child_classes_files workers files_info class_name =
  match Typing_heap.Classes.get class_name with
  | Some class_ ->
    (* Find the files that contain classes that extend class_ *)
    let cid_hash =
      Typing_deps.Dep.make (Typing_deps.Dep.Class class_.tc_name) in
    let extend_deps =
      Decl_compare.get_extend_deps cid_hash
        (Typing_deps.DepSet.singleton cid_hash)
    in
    Typing_deps.get_files extend_deps
  | _ ->
    Relative_path.Set.empty

let get_deps_set classes =
  SSet.fold classes ~f:begin fun class_name acc ->
    match Typing_heap.Classes.get class_name with
    | Some class_ ->
        (* Get all files with dependencies on this class *)
        let fn = Pos.filename class_.tc_pos in
        let dep = Typing_deps.Dep.Class class_.tc_name in
        let bazooka = Typing_deps.get_bazooka dep in
        let files = Typing_deps.get_files bazooka in
        let files = Relative_path.Set.add fn files in
        Relative_path.Set.union files acc
    | _ -> acc
  end ~init:Relative_path.Set.empty

let get_deps_set_function f_name =
  try
    let fun_ = Typing_heap.Funs.find_unsafe f_name in
    let fn = Pos.filename fun_.ft_pos in
    let dep = Typing_deps.Dep.Fun f_name in
    let bazooka = Typing_deps.get_bazooka dep in
    let files = Typing_deps.get_files bazooka in
    Relative_path.Set.add fn files
  with Not_found -> Relative_path.Set.empty

let find_refs target_classes target_method acc fileinfo_l =
  let results_acc = ref Pos.Map.empty in
  attach_hooks results_acc target_classes target_method;
  let tcopt = TypecheckerOptions.permissive in
  ServerIdeUtils.recheck tcopt fileinfo_l;
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
    SSet.fold classes ~init:[] ~f:begin fun class_name acc ->
      match Typing_heap.Classes.get class_name with
      | Some class_ ->
        let add_meth meths acc = match SMap.get meths method_name with
          | Some meth when meth.ce_origin = class_.tc_name ->
            let pos = Reason.to_pos (fst meth.ce_type) in
            (method_name, pos) :: acc
          | _ -> acc
        in
        let acc = add_meth class_.tc_methods acc in
        let acc = add_meth class_.tc_smethods acc in
        acc
      | None -> acc
    end
  | Some classes, None ->
    SSet.fold classes ~init:[] ~f:begin fun class_name acc ->
      match Typing_heap.Classes.get class_name with
      | Some class_ -> (class_name, class_.tc_pos) :: acc
      | None -> acc
    end
  | None, Some fun_name ->
    begin match Typing_heap.Funs.get fun_name with
      | Some fun_ -> [fun_name, fun_.ft_pos]
      | None -> []
    end
  | None, None -> []

let find_references workers target_classes target_method include_defs
    files_info files =
  let fileinfo_l = Relative_path.Set.fold (fun fn acc ->
    match Relative_path.Map.get fn files_info with
    | Some fi -> (fn, fi) :: acc
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
