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

module CL = Coverage_level
module SE = ServerEnv

(* Count the number of expressions of each kind of Coverage_level. *)
let count_exprs fn pos_ty_m =
  let pos_level_m = CL.mk_level_map (Some fn) pos_ty_m in
  Pos.Map.fold (fun _ lvl c -> CL.incr_counter lvl c)
            pos_level_m CL.empty_counter

(* Calculate the percentage of code we have covered as a ratio of typed
 * expressions : total expressions. Partially-typed expressions count as half
 * a typed expression. *)
let calc_percentage ctr =
  let total = CL.CLMap.fold (fun k v acc -> v + acc) ctr 0 in
  let mult = function
    | CL.Unchecked -> 0.0
    | CL.Partial -> 0.5
    | CL.Checked -> 1.0
  in
  let score = CL.CLMap.fold
    (fun k v acc -> mult k *. float_of_int v +. acc) ctr 0.0 in
  if total = 0
  then 1.0
  else score /. float_of_int total

(* Returns a list of (file_name, assoc list of counts) *)
let get_coverage neutral fnl =
  SharedMem.invalidate_caches();
  Typing_defs.accumulate_types := true;
  let file_counts = List.map begin fun fn ->
    match Parser_heap.ParserHeap.get fn with
    | None -> None
    | Some defs ->
        assert (!(Typing_defs.type_acc) = Pos.Map.empty);
        List.iter ServerIdeUtils.check_def defs;
        let counts = count_exprs fn !Typing_defs.type_acc in
        Typing_defs.type_acc := Pos.Map.empty;
        Some (fn, counts)
  end fnl |> cat_opts in
  Typing_defs.accumulate_types := false;
  file_counts :: neutral

(* Inserts value v into a trie with the key path_l. At each existing node with
 * value v', we call `combine v v'` to get the new value for that node. If the
 * node doesn't exist yet, we create a new one with value v. *)
let rec insert combine path_l v trie_opt =
  let open Coverage_level in
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
  let combine v1 v2 = CL.CLMap.merge (fun k a b ->
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

let rec trie_map f = function
  | CL.Leaf a -> CL.Leaf (f a)
  | CL.Node (a, children) -> CL.Node (f a, SMap.map (trie_map f) children)

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

let go_ fn genv =
  let path = Path.mk_path fn in
  let root = Path.parent path in
  let module RP = Relative_path in
  let next_files = compose
    (rev_rev_map (RP.create RP.Root))
    (Find.make_next_files_php path)
  in
  let annotate_percents ctr = {
    CL.counts = ctr;
    CL.percentage = calc_percentage ctr;
  } in
  let result =
    MultiWorker.call
      genv.SE.workers
      ~job:get_coverage
      ~neutral:[]
      ~merge:(@)
      ~next:next_files
  in
  let relativize_list = List.map (fun (p, c) ->
    (relativize root (Relative_path.to_absolute p) |> unsafe_opt, c)) in
  let result = List.map relativize_list result in
  match List.fold_left mk_trie None result with
  | None -> None
  | Some r ->
      Some (trie_map annotate_percents r)

let go fn genv oc =
  let (result : CL.result CL.trie option) =
    try go_ fn genv
    with Failure _ | Invalid_argument _ ->
      print_string "Coverage collection failed!";
      None
  in
  Marshal.to_channel oc result [];
  flush oc
