(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Coverage_level
open Utils

module List = Core_list

module FileInfoStore = GlobalStorage.Make(struct
  type t = FileInfo.t Relative_path.Map.t
end)

type result = level_counts SMap.t trie option

(* Count the number of expressions of each kind of Coverage_level. *)
let count_exprs fn type_acc =
  let level_of_type = level_of_type_mapper fn in
  Hashtbl.fold (fun (p, kind) ty acc ->
    let lvl = level_of_type (p, ty) in
    let counter = match SMap.get kind acc with
      | Some counter -> counter
      | None -> empty_counter in
    SMap.add kind (incr_counter lvl counter) acc
  ) type_acc SMap.empty

let accumulate_types defs =
  let type_acc = Hashtbl.create 0 in
  Typing.with_expr_hook (fun (p, e) ty ->
    let expr_kind_opt = match e with
      | Nast.Array_get _ -> Some "array_get"
      | Nast.Call _ -> Some "call"
      | Nast.Class_get _ -> Some "class_get"
      | Nast.Class_const _ -> Some "class_const"
      | Nast.Lvar _ -> Some "lvar"
      | Nast.New _ -> Some "new"
      | Nast.Obj_get _ -> Some "obj_get"
      | _ -> None in
    ignore (opt_map (fun kind ->
      Hashtbl.replace type_acc (p, kind) ty) expr_kind_opt))
    (fun () ->
      let nenv = Naming.empty (TypecheckerOptions.permissive) in
      ignore (Typing_check_utils.check_defs nenv defs));
  type_acc

(* Returns a list of (file_name, assoc list of counts) *)
let get_coverage neutral fnl =
  SharedMem.invalidate_caches();
  let files_info = FileInfoStore.load () in
  let file_counts = List.rev_filter_map ~f:begin fun fn ->
    match Relative_path.Map.get fn files_info with
    | None -> None
    | Some defs ->
        let type_acc = accumulate_types defs in
        let counts = count_exprs fn type_acc in
        Some (fn, counts)
  end fnl in
  file_counts :: neutral

(* Inserts value v into a trie with the key path_l. At each existing node with
 * value v', we call `combine v v'` to get the new value for that node. If the
 * node doesn't exist yet, we create a new one with value v. *)
let rec insert combine path_l v trie_opt =
  match path_l with
  | [] -> assert false
  | [fn] ->
      (match trie_opt with
      | None -> Leaf v
      | Some (Leaf v') -> Leaf (combine v v')
      | Some (Node _) -> assert false)
  | dir :: (p :: _ as rl) ->
      (match trie_opt with
      | None -> Node (v, SMap.singleton p (insert combine rl v None))
      | Some (Leaf _) -> assert false
      | Some (Node (v', m)) ->
          let child_opt = SMap.get p m in
          Node (combine v v',
                SMap.add p (insert combine rl v child_opt) m))

(* Convert a list of (file_name, map of counts) into a trie. Each
 * internal node of the trie has the sum of counts of all its child nodes.
 * NOTE(jez): we could parallelize this trie construction if we had a
 * merge_trie function, but the actual typecheck / type accumulation step
 * dominates the runtime, so there's not much improvement to be had here. *)
let mk_trie acc fn_counts_l =
  let combine v1 v2 = SMap.merge (fun _ cs1 cs2 ->
    match cs1, cs2 with
    | Some cs1, Some cs2 -> Some (merge_and_sum cs1 cs2)
    | Some cs, None -> Some cs
    | None, Some cs -> Some cs
    | None, None -> None) v1 v2 in
  List.fold_left
    ~f:(fun acc (fn, counts) ->
      let path_l = Str.split (Str.regexp "/") fn in
      Some (insert combine path_l counts acc))
    ~init:acc fn_counts_l

(* Convert an absolute path to one relative to the given root.
 * Returns None if root is not a prefix of path. *)
let relativize root path =
  (* naive implementation *)
  let root = Path.to_string root ^ "/" in
  if str_starts_with path root
  then
    let root_len = String.length root in
    Some (String.sub path root_len (String.length path - root_len))
  else None

let go_ fn genv env =
  let path = Path.make fn in
  let root = Path.parent path in
  let module RP = Relative_path in
  let next_files = compose
    (rev_rev_map (RP.create RP.Root))
    (Find.make_next_files FindUtils.is_php path)
  in
  FileInfoStore.store env.ServerEnv.files_info;
  let result =
    MultiWorker.call
      genv.ServerEnv.workers
      ~job:get_coverage
      ~neutral:[]
      ~merge:(@)
      ~next:next_files
  in
  FileInfoStore.clear ();
  let relativize_list = List.map ~f:(fun (p, c) ->
    (relativize root (Relative_path.to_absolute p) |> unsafe_opt, c)) in
  let result = List.map ~f:relativize_list result in
  List.fold_left ~f:mk_trie ~init:None result

let go fn genv env =
  try go_ fn genv env
  with Failure _ | Invalid_argument _ ->
    print_string "Coverage collection failed!";
    None
