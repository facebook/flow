(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module describes the representation of lexical environments and defines
   various operations on them, including "stack" operations to push/pop scopes,
   and "lookup" operations to find, read, and write variables and their
   associated type information. *)

open Utils
open Reason_js
open Constraint_js
open Type

(* helpers *)

(* prop lookups encoding *)
let prop_lookup_name oname pname =
  "$PGET " ^ oname ^ " " ^ pname

let is_prop_lookup name =
    (String.length name) >= 5 && (String.sub name 0 5) = "$PGET"

(****************)
(* Environments *)
(****************)

type env = block list ref
let env: env = ref []

let changeset = ref SSet.empty
let swap_changeset f =
  let oldset = !changeset in
  let newset = f oldset in
  changeset := newset;
  oldset

let global_block: block = ref SMap.empty

let global_poison = ["eval"; "arguments"]
let global_lexicals = [
  (internal_name "super");
  (internal_name "this")
]

let cache_global cx x reason =
  let t = (if (List.mem x global_poison) then
    AnyT.t
  else (if (List.mem x global_lexicals) then
    MixedT (reason_of_string "global object")
  else
    Flow_js.get_builtin cx x reason
  )) in
  global_block := SMap.add x (create_env_entry t t None) !global_block;
  cx.globals <- SSet.add x cx.globals;
  global_block

let find_global cx x reason =
  if (SMap.mem x !global_block)
  then
    global_block
  else
    cache_global cx x reason

let find_env cx x reason =
  let rec loop = function
    | [] -> find_global cx x reason
    | block::blocks -> if (SMap.mem x !block) then block else loop blocks
  in
  loop (!env)

let get_block x block =
  SMap.find_unsafe x !block

let set_block x entry block =
  block := !block |> SMap.add x entry

let exists_block x block =
  SMap.get x !block

let update_block x shape block =
  let values = !block in
  let (s, g) = shape in
  let new_entry = match SMap.get x values with
    | Some {def_loc;_} ->
        create_env_entry s g def_loc
    | None ->
        create_env_entry s g None
  in
  block := values |> SMap.add x new_entry

let unset_block x block =
  block := !block |> SMap.remove x

let read_env cx x reason =
  find_env cx x reason |> get_block x

let write_env cx x shape reason =
  find_env cx x reason |> update_block x shape

let push_env block =
  env := block :: !env

let pop_env () =
  env := List.tl !env

let flat_env () =
  List.fold_left (fun acc x ->
    SMap.union acc !x
  ) !global_block !env

let switch_env block =
  pop_env ();
  push_env block

let peek_env () =
  List.hd !env

let init_env cx x shape =
  let block = peek_env() in
  match exists_block x block with
  | None -> set_block x shape block
  | Some { general; _ } ->
      Flow_js.unify cx general shape.general

let let_env x shape f =
  peek_env() |> set_block x shape;
  f();
  peek_env() |> unset_block x

(************)
(* Contexts *)
(************)

(* Recall that for every variable we maintain two types, one flow-sensitive and
   the other flow-insensitive. *)

let get_var cx x reason =
  (read_env cx x reason).specific

let get_var_in_scope cx x reason =
  (read_env cx x reason).general

let get_var_ cx x reason =
  let block = find_env cx x reason in
  (block = global_block, (get_block x block).specific)

let var_ref cx x reason =
  let t = (read_env cx x reason).specific in
  let p = pos_of_reason reason in
  mod_reason_of_t (repos_reason p) t

let get_lookup_refinement cx x p r =
  let name = prop_lookup_name x p in
  let block = peek_env () in
  match exists_block name block with
  | Some { specific; _ } ->
      let p = pos_of_reason r in
      let t = mod_reason_of_t (repos_reason p) specific in
      Some t
  | None -> None

let set_var cx x s reason =
  changeset := !changeset |> SSet.add x;
  let t = (read_env cx x reason).general in
  Flow_js.unit_flow cx (s,t);
  write_env cx x (s,t) reason

let clone_env ctx =
  List.map (fun block -> ref !block) ctx

let update_frame cx ctx =
  let current_frame = List.hd !Flow_js.frames in
  let (stack, _) = IMap.find_unsafe current_frame cx.closures in
  cx.closures <- IMap.add current_frame (stack,ctx) cx.closures;
  env := ctx

(* The following function takes a triple (c1,c2,c3), merging the information
   carried for every variable in c2 and c3 into c1. Callers maintain the
   invariant that c1, c2, c3 describe the same scope, so have the same shape. *)

let rec merge_env_ cx reason x = function
  | ([],[],[]) -> ()
  | (block::ctx, block1::ctx1, block2::ctx2) ->
      let vars = !block in
      (match SMap.get x vars with
        | Some {specific;general;def_loc;} ->
            let s1 = (SMap.find_unsafe x !block1).specific in
            let s2 = (SMap.find_unsafe x !block2).specific in
            let (s, g) =
              if (specific = s1 && specific = s2)
              then (specific,general)
              else if (s1 = general || s2 = general)
              then (general,general)
              else
                let reason = replace_reason x reason in
                let tvar = Flow_js.mk_tvar cx reason in
                Flow_js.unit_flow cx (s1,tvar);
                Flow_js.unit_flow cx (s2,tvar);
                Flow_js.unit_flow cx (tvar,general);
                (tvar,general)
            in
            block := vars |> SMap.add x (create_env_entry s g def_loc)
        | None -> merge_env_ cx reason x (ctx,ctx1,ctx2)
      )
  | _ -> assert false

let merge_env cx reason (ctx,ctx1,ctx2) =
  SSet.iter (fun x -> merge_env_ cx reason x (ctx,ctx1,ctx2))

let widen_env cx reason =
  let block = List.hd !env in
  let vars = !block in
  if SMap.cardinal vars < 32 then
  block := vars |> SMap.mapi (fun x {specific;general;def_loc;} ->
    if specific = general
    then create_env_entry specific general def_loc
    else
      let reason = replace_reason x reason in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.unit_flow cx (specific,tvar);
      Flow_js.unit_flow cx (tvar,general);
      create_env_entry tvar general def_loc
  )

let rec copy_env_ cx reason x = function
  | ([],[]) -> ()
  | (block1::ctx1, block2::ctx2) ->
      (match SMap.get x !block1 with
        | Some {specific=s1;_} ->
            let s2 = (SMap.find_unsafe x !block2).specific in
            Flow_js.unit_flow cx (s2,s1)
        | None -> copy_env_ cx reason x (ctx1,ctx2)
      )
  | _ -> assert false

let copy_env cx reason (ctx1,ctx2) xs =
  SSet.iter (fun x -> copy_env_ cx reason x (ctx1,ctx2)) xs

(* The protocol around havoc has changed a few times.
   The following function used to do most of the work, but is now subsumed by
   havoc_ctx, and is now used only to clear the environment when looking at
   a function body. Also see below. *)

let havoc_env () =
  List.iter (fun block ->
    block := SMap.mapi (fun x {general;def_loc;_} ->
      create_env_entry general general def_loc
    ) !block
 ) !env

let rec havoc_env2_ x = function
  | block::blocks ->
      (match SMap.get x !block with
      | Some {general;def_loc;_} ->
          block := !block |>
            SMap.add x (create_env_entry general general def_loc)
      | None ->
          havoc_env2_ x blocks
      )
  | [] -> ()

let havoc_env2 xs =
  SSet.iter (fun x -> havoc_env2_ x !env) xs

let havoc_heap_refinements () =
  List.iter (fun block ->
    block := SMap.mapi (fun x ({general;def_loc;_} as entry) ->
      if is_prop_lookup x
      then create_env_entry general general def_loc
      else entry
    ) !block
 ) !env

let clear_env reason =
  let block = List.hd !env in
  block := !block |> SMap.mapi (fun x {specific;general;def_loc;} ->
    (* internal names (.this, .super, .return, .exports) are read-only *)
    if is_internal_name x
    then create_env_entry specific general def_loc
    else create_env_entry (UndefT reason) general def_loc
  )

(* The following functions are used to narrow the type of variables
   based on dynamic checks. *)

let refine_with_pred cx reason pred xtypes =
  SMap.iter (fun x predx ->
    (if is_prop_lookup x then
      let t = SMap.find_unsafe x xtypes in
      init_env cx x (create_env_entry t t None));
    let (is_global,tx) = get_var_ cx x
      (replace_reason (spf "identifier %s" x) reason)
    in
    if is_global then ()
    else
      let rstr = spf "identifier %s when %s" x (string_of_predicate predx) in
      let reason = replace_reason rstr reason in
      let t = Flow_js.mk_tvar cx reason in
      let rt = mk_predicate (predx, t) in
      Flow_js.unit_flow cx (tx, rt);
      set_var cx x t reason;
  )
    pred

let refine_env cx reason pred xtypes f =
  let ctx = !env in
  let new_ctx = clone_env ctx in
  update_frame cx new_ctx;
  let oldset = swap_changeset (fun _ -> SSet.empty) in
  refine_with_pred cx reason pred xtypes;
  let result = f() in
  let newset = swap_changeset (SSet.union oldset) in
  merge_env cx reason (ctx,new_ctx,ctx) newset;
  update_frame cx ctx;
  result
