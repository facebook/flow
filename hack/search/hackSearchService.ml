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

type search_result_type =
  | Class of Ast.class_kind
  | Method of bool * string
  | ClassVar of bool * string
  | Function
  | Typedef
  | Constant

module SS = SearchService.Make(struct
  type t = search_result_type
  let fuzzy_types = [Class Ast.Cnormal; Function; Constant; Typedef]
  let type_num = function
    | Class _ -> 0
    | Function -> 1
    | Typedef -> 2
    | Constant -> 3
    | _ -> 4
  let compare_result_type a b =
    (type_num a) - (type_num b)
end)

module WorkerApi = struct
  (* cleans off namespace and colon at the start of xhp name because the
   * user will want to search for xhp classes without typing a : at
   * the start of every search *)
  let clean_key key =
    if (String.length key) > 0
    then
      let key = String.lowercase (Utils.strip_ns key) in
      if (String.length key) > 0 && key.[0] = ':'
      then String.sub key 1 (String.length key - 1)
      else key
    else key

  (* Unlike anything else, we need to look at the class body to extract it's
   * methods so that they can also be searched for *)
  let update_class c acc =
    let prefix = (snd c.Ast.c_name)^"::" in
    List.fold_left c.Ast.c_body ~f:begin fun acc elt ->
      match elt with
        | Ast.Method m -> let id = m.Ast.m_name in
            let name = snd id in
            let full_name = prefix^name in
            let pos = fst id in
            let is_static = List.mem m.Ast.m_kind Ast.Static in
            let type_ =
              Method (is_static, (Utils.strip_ns (snd c.Ast.c_name)))
            in
            let acc =
              SS.WorkerApi.process_trie_term (clean_key name) name pos type_ acc
            in
            SS.WorkerApi.process_trie_term
              (clean_key full_name) name pos type_ acc
        | _ -> acc
    end ~init:acc

  let add_fuzzy_term id type_ acc =
    let name = snd id in
    let key = strip_ns name in
    SS.WorkerApi.process_fuzzy_term key name (fst id) type_ acc

  (* Called by a worker after the file is parsed *)
  let update fn ast =
    let fuzzy_defs, trie_defs =
      List.fold_left ast ~f:begin fun (fuzzy_defs, trie_defs) def ->
      match def with
      | Ast.Fun f ->
          let fuzzy_defs = add_fuzzy_term f.Ast.f_name Function fuzzy_defs in
          fuzzy_defs, trie_defs
      | Ast.Class c ->
          let fuzzy_defs =
            add_fuzzy_term c.Ast.c_name (Class c.Ast.c_kind) fuzzy_defs in
          (* Still index methods for trie search *)
          let trie_defs = update_class c trie_defs in
          fuzzy_defs, trie_defs
      | Ast.Typedef td ->
          let fuzzy_defs = add_fuzzy_term td.Ast.t_id Typedef fuzzy_defs in
          fuzzy_defs, trie_defs
      | Ast.Constant cst ->
          let fuzzy_defs =
            add_fuzzy_term cst.Ast.cst_name Constant fuzzy_defs
          in
          fuzzy_defs, trie_defs
      | _ -> fuzzy_defs, trie_defs
    end ~init:(SS.Fuzzy.TMap.empty, []) in
    SS.WorkerApi.update fn trie_defs fuzzy_defs
end

module MasterApi = struct
  let get_type = function
    | "class" -> Some (Class Ast.Cnormal)
    | "function" -> Some Function
    | "constant" -> Some Constant
    | "typedef" -> Some Typedef
    | _ -> None

  let query input type_ =
    SS.MasterApi.query input (get_type type_)

  let clear_shared_memory =
    SS.MasterApi.clear_shared_memory

  let update_search_index files php_files =
    let files = List.fold_left files ~f:begin fun acc file ->
      Relative_path.Set.add file acc
    end ~init:php_files in
    SS.MasterApi.update_search_index files
end

let attach_hooks () =
  Parsing_hooks.attach_file_parsed_hook WorkerApi.update;
  Parsing_hooks.attach_parse_task_completed_hook MasterApi.update_search_index
