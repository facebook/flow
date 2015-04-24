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

module Ast = Spider_monkey_ast

(* helpers *)

(* refinement keys *)
let refinement_key names =
  "$REFI " ^ (String.concat "." names)

let is_refinement name =
  (String.length name) >= 5 && (String.sub name 0 5) = "$REFI"

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

let global_block: block = {
  scope = Hoist;
  entries = ref SMap.empty;
}

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
  let global_entries = global_block.entries in
  global_entries := SMap.add x (create_env_entry t t None) !global_entries;
  cx.globals <- SSet.add x cx.globals;
  global_block

let find_global cx x reason =
  let global_entries = global_block.entries in
  if (SMap.mem x !global_entries)
  then
    global_block
  else
    cache_global cx x reason

let find_env ?(for_type=false) cx x reason =
  let rec loop = function
    | [] -> find_global cx x reason
    | block::blocks ->
        let entries = block.entries in
        (match SMap.get x !entries with
        | Some entry when (not entry.for_type || for_type) -> block
        | _ -> loop blocks)
  in
  loop (!env)

let get_block x block =
  let entries = block.entries in
  SMap.find_unsafe x !entries

let set_block x entry block =
  let entries = block.entries in
  entries := !entries |> SMap.add x entry

let exists_block x block =
  let entries = block.entries in
  SMap.get x !entries

let set_env x entry hoist =
  let rec loop = function
    | [] -> set_block x entry global_block
    | block::blocks ->
        if (not hoist || (hoist && block.scope = Hoist)) then
          set_block x entry block
        else
          loop blocks in
  loop (!env)

let exists_env x hoist =
  let rec loop = function
    | [] -> exists_block x global_block
    | block::blocks ->
        if (not hoist || (hoist && block.scope = Hoist)) then
          exists_block x block
        else
          loop blocks in
  loop (!env)

let update_block ?(for_type=false) x (specific_t, general_t) block =
  let entries = block.entries in
  let new_entry = match SMap.get x !entries with
    | Some {def_loc; for_type; _;} ->
        create_env_entry ~for_type specific_t general_t def_loc
    | None ->
        create_env_entry ~for_type specific_t general_t None
  in
  entries := !entries |> SMap.add x new_entry

let unset_block x block =
  let entries = block.entries in
  entries := !entries |> SMap.remove x

let read_env ?(for_type=false) cx x reason =
  find_env ~for_type cx x reason |> get_block x

let write_env ?(for_type=false) cx x shape reason =
  find_env ~for_type cx x reason |> update_block ~for_type x shape

let push_env block =
  env := block :: !env

let pop_env () =
  env := List.tl !env

let flat_env () =
  let rec loop acc env =
    match env with
    | { entries; _ }::bs -> loop (SMap.union acc !entries) bs
    | [] -> acc in
  let global_entries = global_block.entries in
  loop !global_entries !env

let switch_env block =
  pop_env ();
  push_env block

let peek_env () =
  List.hd !env

let init_env cx x shape hoist =
  match exists_env x hoist with
  | None -> set_env x shape hoist
  | Some { general; for_type; def_loc; _; } when for_type <> shape.for_type ->
      (* When we have a value var shadowing a type var, replace the type var
       * with the value var *)
      let shadowed_reason =
        mk_reason
          (spf "%s binding %s" (if for_type then "type" else "value") x)
          (match def_loc with None -> Ast.Loc.none | Some loc -> loc) in
      let shadower_reason =
        mk_reason
          (spf "%s binding %s" (if shape.for_type then "type" else "value") x)
          (match shape.def_loc with None -> Ast.Loc.none | Some loc -> loc) in
      Flow_js.add_error cx [
        shadower_reason, "This binding is shadowing";
        shadowed_reason, "which is a different sort of binding";
      ];
      set_env x shape hoist;
      Flow_js.unify cx shape.general general
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

let get_var ?(for_type=false) cx x reason =
  (read_env ~for_type cx x reason).specific

let get_var_in_scope ?(for_type=false) cx x reason =
  (read_env ~for_type cx x reason).general

let get_var_ cx x reason =
  let block = find_env cx x reason in
  (block = global_block, (get_block x block).specific)

let var_ref ?(for_type=false) cx x reason =
  let t = (read_env ~for_type cx x reason).specific in
  let p = pos_of_reason reason in
  mod_reason_of_t (repos_reason p) t

let get_refinement cx key r =
  match exists_env key true with
  | Some { specific; _ } ->
      let pos = pos_of_reason r in
      let t = mod_reason_of_t (repos_reason pos) specific in
      Some t
  | None ->
      None

let set_var ?(for_type=false) cx x specific_t reason =
  changeset := !changeset |> SSet.add x;
  let general = (read_env ~for_type cx x reason).general in
  Flow_js.unit_flow cx (specific_t, general);
  write_env ~for_type cx x (specific_t, general) reason

let clone_env ctx =
  List.map (fun block ->
    let { scope; entries } = block in
    { scope; entries = ref !entries }
  ) ctx

let update_frame cx ctx =
  let current_frame = List.hd !Flow_js.frames in
  let (stack, _) = IMap.find_unsafe current_frame cx.closures in
  cx.closures <- IMap.add current_frame (stack,ctx) cx.closures;
  env := ctx

(* The following function takes a set of variable names
   and a triple of environments (c1,c2,c3), and merges the information
   carried for changeset variables in c2 and c3 into c1. Assumptions:
   - c1, c2, c3 describe the same scope chain, so have the same depth
   - variables in changeset have been collected from this scope chain
 *)
let rec merge_env cx reason (ctx, ctx1, ctx2) changeset =
  if SSet.cardinal changeset > 0 then
  match (ctx, ctx1, ctx2) with
  | ([],[],[]) -> ()
  | (block::ctx, block1::ctx1, block2::ctx2) ->
      let entries = block.entries in
      let entries1 = block1.entries in
      let entries2 = block2.entries in
      (* merge block, block1, block2 *)
      let not_found = SSet.fold (fun x not_found ->
        match SMap.get x !entries with
        | Some {specific;general;def_loc;for_type;} ->
            let shape1 = SMap.find_unsafe x !entries1 in
            let shape2 = SMap.find_unsafe x !entries2 in
            let s1 = shape1.specific in
            let s2 = shape2.specific in
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
            let for_type = for_type || shape1.for_type || shape2.for_type in
            block.entries := SMap.add x (create_env_entry ~for_type s g def_loc) !entries;
            not_found
        | None ->
            SSet.add x not_found
      ) changeset SSet.empty in
      (* look for the rest of changeset in outer blocks *)
      merge_env cx reason (ctx, ctx1, ctx2) not_found
  | _ -> assert false

let widen_env cx reason =
  let block = List.hd !env in
  let entries = block.entries in
  let entries = !entries in
  if SMap.cardinal entries < 32 then
  block.entries := entries |> SMap.mapi (fun x {specific;general;def_loc;for_type;} ->
    if specific = general
    then create_env_entry ~for_type specific general def_loc
    else
      let reason = replace_reason x reason in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.unit_flow cx (specific,tvar);
      Flow_js.unit_flow cx (tvar,general);
      create_env_entry ~for_type tvar general def_loc
  )

let rec copy_env_ cx reason x = function
  | ([],[]) -> ()
  | (block1::ctx1, block2::ctx2) ->
      let entries1 = block1.entries in
      let entries2 = block2.entries in
      (match SMap.get x !entries1 with
        | Some {specific=s1;_} ->
            let s2 = (SMap.find_unsafe x !entries2).specific in
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
    let entries = block.entries in
    entries := SMap.mapi (fun x {specific=_;general;def_loc;for_type;} ->
      create_env_entry ~for_type general general def_loc
    ) !entries
 ) !env

let rec havoc_env2_ x = function
  | block::blocks ->
      let entries = block.entries in
      (match SMap.get x !entries with
      | Some {specific=_;general;def_loc;for_type;} ->
          entries := !entries |>
            SMap.add x (create_env_entry ~for_type general general def_loc)
      | None ->
          havoc_env2_ x blocks
      )
  | [] -> ()

let havoc_env2 xs =
  SSet.iter (fun x -> havoc_env2_ x !env) xs

let havoc_heap_refinements () =
  List.iter (fun block ->
    let entries = block.entries in
    entries := SMap.mapi (fun x ({specific=_;general;def_loc;for_type;} as entry) ->
      if is_refinement x
      then create_env_entry ~for_type general general def_loc
      else entry
    ) !entries
 ) !env

let clear_env reason =
  let rec loop = function
    | [] -> ()
    | block::blocks ->
        let entries = block.entries in
        entries := !entries |> SMap.mapi (fun x {specific;general;def_loc;for_type;} ->
          (* internal names (.this, .super, .return, .exports) are read-only *)
          if is_internal_name x
          then create_env_entry ~for_type specific general def_loc
          else create_env_entry ~for_type (UndefT reason) general def_loc
        );
        if block.scope = Hoist then ()
        else loop blocks
  in
  loop !env

let string_of_env cx ctx =
  String.concat "\n" (List.map (string_of_block cx) ctx)

(* The following functions are used to narrow the type of variables
   based on dynamic checks. *)

let install_refinement cx x xtypes =
  if exists_block x (peek_env ()) = None then
    let t = SMap.find_unsafe x xtypes in
    init_env cx x (create_env_entry t t None) true (* TODO: verify correct hoist behavior *)

let refine_with_pred cx reason pred xtypes =
  SMap.iter (fun x predx ->
    if is_refinement x then install_refinement cx x xtypes;
    let reason' = replace_reason (spf "identifier %s" x) reason in
    let (is_global, tx) = get_var_ cx x reason' in
    if not is_global then (
      let rstr = spf "identifier %s when %s" x (string_of_predicate predx) in
      let reason = replace_reason rstr reason in
      let t = Flow_js.mk_tvar cx reason in
      let rt = mk_predicate (predx, t) in
      Flow_js.unit_flow cx (tx, rt);
      set_var cx x t reason
    ))
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
