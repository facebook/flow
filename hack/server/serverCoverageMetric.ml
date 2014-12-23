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

type result = level_counts trie option

module FileInfoStore = GlobalStorage.Make(struct
  type t = FileInfo.t Relative_path.Map.t
end)

(* Count the number of expressions of each kind of Coverage_level. *)
let count_exprs fn pos_ty_m =
  let pos_level_l = mk_level_list (Some fn) pos_ty_m in
  List.fold_left (fun c (_, lvl) -> incr_counter lvl c)
            empty_counter pos_level_l

(* Returns a list of (file_name, assoc list of counts) *)
let get_coverage neutral fnl =
  SharedMem.invalidate_caches();
  Typing_defs.accumulate_types := true;
  let files_info = FileInfoStore.load () in
  let file_counts = List.map begin fun fn ->
    match Relative_path.Map.get fn files_info with
    | None -> None
    | Some defs ->
        assert (!Typing_defs.type_acc = []);
        ServerIdeUtils.check_defs defs;
        let counts = count_exprs fn !Typing_defs.type_acc in
        Typing_defs.type_acc := [];
        Some (fn, counts)
  end fnl |> cat_opts in
  Typing_defs.accumulate_types := false;
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
  let combine v1 v2 = CLMap.merge (fun k a b ->
    let c = match a, b with
    | Some c1, Some c2 -> c1 + c2
    | _ -> assert false
    in Some c
    ) v1 v2
  in
  List.fold_left
    (fun acc (fn, counts) ->
      let path_l = Str.split (Str.regexp "/") fn in
      Some (insert combine path_l counts acc))
    acc fn_counts_l

(* Convert an absolute path to one relative to the given root.
 * Returns None if root is not a prefix of path. *)
let relativize root path =
  (* naive implementation *)
  let root = Path.string_of_path root ^ "/" in
  if str_starts_with path root
  then
    let root_len = String.length root in
    Some (String.sub path root_len (String.length path - root_len))
  else None

let go_ fn genv env =
  let path = Path.mk_path fn in
  let root = Path.parent path in
  let module RP = Relative_path in
  let next_files = compose
    (rev_rev_map (RP.create RP.Root))
    (Find.make_next_files_php path)
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
  let relativize_list = List.map (fun (p, c) ->
    (relativize root (Relative_path.to_absolute p) |> unsafe_opt, c)) in
  let result = List.map relativize_list result in
  List.fold_left mk_trie None result

let go fn genv env oc =
  let result =
    try go_ fn genv env
    with Failure _ | Invalid_argument _ ->
      print_string "Coverage collection failed!";
      None
  in
  Marshal.to_channel oc (result : result) [];
  flush oc
