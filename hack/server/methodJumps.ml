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
open Utils

type result = {
  orig_name: string;
  orig_pos: Pos.absolute;
  dest_name: string;
  dest_pos: Pos.absolute;
  orig_p_name: string; (* Used for methods to find their parent class *)
  dest_p_name: string;
}

(* Used so the given input doesn't need the `\`. *)
let add_ns name =
  if name.[0] = '\\' then name else "\\" ^ name

let get_overridden_methods origin_class or_mthds dest_class is_child acc =
  let dest_class =
    Naming_heap.ClassHeap.find_unsafe dest_class in

  (* Check if each destination method exists in the origin *)
  List.fold_left dest_class.Nast.c_methods ~init:acc ~f:begin fun acc de_mthd ->
    let or_mthd = SMap.get (snd de_mthd.Nast.m_name) or_mthds in
    match or_mthd with
    | Some or_mthd -> {
          orig_name = snd or_mthd.Nast.m_name;
          orig_pos = Pos.to_absolute (fst or_mthd.Nast.m_name);
          dest_name = snd de_mthd.Nast.m_name;
          dest_pos = Pos.to_absolute (fst de_mthd.Nast.m_name);
          orig_p_name = origin_class;
          dest_p_name = snd dest_class.Nast.c_name;
        } :: acc
    | None -> acc
  end

let check_if_extends_class_and_find_methods target_class_name mthds
      target_class_pos class_name acc =
  let class_ = Typing_env.Classes.get class_name in
  match class_ with
  | None -> acc
  | Some c
      when SMap.mem target_class_name c.Typing_defs.tc_ancestors ->
        let acc = get_overridden_methods
                      target_class_name mthds
                      class_name
                      true
                      acc in {
          orig_name = target_class_name;
          orig_pos = Pos.to_absolute target_class_pos;
          dest_name = c.Typing_defs.tc_name;
          dest_pos = Pos.to_absolute c.Typing_defs.tc_pos;
          orig_p_name = "";
          dest_p_name = "";
        } :: acc
  | _ -> acc

let filter_extended_classes target_class_name mthds target_class_pos
      acc classes =
  List.fold_left classes ~init:acc ~f:begin fun acc cid ->
   check_if_extends_class_and_find_methods
      target_class_name
      mthds
      target_class_pos
      (snd cid)
      acc
  end

let find_extended_classes_in_files target_class_name mthds target_class_pos
      acc classes =
  List.fold_left classes ~init:acc ~f:begin fun acc classes ->
    filter_extended_classes target_class_name mthds target_class_pos acc classes
  end

let find_extended_classes_in_files_parallel workers target_class_name mthds
      target_class_pos files_info files =
  let classes = Relative_path.Set.fold begin fun fn acc ->
    let { FileInfo.classes; _ } = Relative_path.Map.find_unsafe fn files_info in
    classes :: acc
  end files [] in

  if List.length classes > 10 then
    MultiWorker.call
      workers
      ~job:(find_extended_classes_in_files
        target_class_name
        mthds
        target_class_pos)
      ~merge:(List.rev_append)
      ~neutral:([])
      ~next:(Bucket.make classes)
  else
    find_extended_classes_in_files
        target_class_name mthds target_class_pos [] classes

(* Find child classes *)
let get_child_classes_and_methods cls mthds env genv acc =
  let files = FindRefsService.get_child_classes_files
      genv.ServerEnv.workers
      env.ServerEnv.files_info
      cls.Typing_defs.tc_name in
  find_extended_classes_in_files_parallel
      genv.ServerEnv.workers
      cls.Typing_defs.tc_name
      mthds
      cls.Typing_defs.tc_pos
      env.ServerEnv.files_info
      files

(* Find ancestor classes *)
let get_ancestor_classes_and_methods cls mthds acc =
  let class_ = Typing_env.Classes.get cls.Typing_defs.tc_name in
  match class_ with
  | None -> []
  | Some cls ->
      SMap.fold begin fun k v acc ->
        let class_ = Typing_env.Classes.get k in
        match class_ with
        | None -> acc
        | Some c ->
            let acc = get_overridden_methods
                          cls.Typing_defs.tc_name
                          mthds
                          c.Typing_defs.tc_name
                          false
                          acc in {
              orig_name = Utils.strip_ns cls.Typing_defs.tc_name;
              orig_pos = Pos.to_absolute cls.Typing_defs.tc_pos;
              dest_name = Utils.strip_ns c.Typing_defs.tc_name;
              dest_pos = Pos.to_absolute c.Typing_defs.tc_pos;
              orig_p_name = "";
              dest_p_name = "";
            } :: acc
      end cls.Typing_defs.tc_ancestors acc

let build_method_smap cls =
  let cls =
    Naming_heap.ClassHeap.find_unsafe cls in

  List.fold_left cls.Nast.c_methods ~init:SMap.empty ~f:begin fun acc or_mthd ->
    SMap.add (snd or_mthd.Nast.m_name) or_mthd acc
  end

(*  Returns a list of the ancestor or child
 *  classes and methods for a given class
 *)
let get_inheritance class_ find_children env genv =
  let class_ = add_ns class_ in
  let class_ = Typing_env.Classes.get class_ in
  match class_ with
  | None -> []
  | Some c ->
    let mthds = build_method_smap c.Typing_defs.tc_name in
    if find_children then get_child_classes_and_methods c mthds env genv []
    else get_ancestor_classes_and_methods c mthds []
