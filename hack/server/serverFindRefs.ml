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

type action = Ai.ServerFindRefs.action =
  | Class of string
  | Method of string * string
  | Function of string

type result = (string * Pos.absolute) list

let add_ns name =
  if name.[0] = '\\' then name else "\\" ^ name

let strip_ns results =
  List.map results (fun (s, p) -> ((Utils.strip_ns s), p))

let search class_names method_name include_defs files genv env =
  (* Get all the references to the provided method name and classes in the files *)
  let res = FindRefsService.find_references genv.ServerEnv.workers class_names
      method_name include_defs env.ServerEnv.files_info files in
  strip_ns res

let search_function function_name include_defs genv env =
  let function_name = add_ns function_name in
  let files = FindRefsService.get_dependent_files_function
      genv.ServerEnv.workers function_name in
  search None (Some function_name) include_defs files genv env

let search_method class_name method_name include_defs genv env =
  let class_name = add_ns class_name in
  (* Find all the classes that extend this one *)
  let files = FindRefsService.get_child_classes_files
      genv.ServerEnv.workers env.ServerEnv.files_info class_name in
  let all_classes = FindRefsService.find_child_classes
      class_name env.ServerEnv.files_info files in
  let all_classes = SSet.add class_name all_classes in
  (* Get all the files that reference those classes *)
  let files = FindRefsService.get_dependent_files
      genv.ServerEnv.workers all_classes in
  search (Some all_classes) (Some method_name) include_defs files genv env

let search_class class_name include_defs genv env =
  let class_name = add_ns class_name in
  let files = FindRefsService.get_dependent_files
      genv.ServerEnv.workers (SSet.singleton class_name) in
  search (Some (SSet.singleton class_name)) None include_defs files genv env

let get_refs action include_defs genv env =
  match action with
  | Method (class_name, method_name) ->
      search_method class_name method_name include_defs genv env
  | Function function_name ->
      search_function function_name include_defs genv env
  | Class class_name ->
      search_class class_name include_defs genv env

let get_refs_with_defs action genv env =
  get_refs action true genv env

let go action genv env =
  let res = get_refs action false genv env in
  let res = List.map res (fun (r, pos) -> (r, Pos.to_absolute pos)) in
  res
