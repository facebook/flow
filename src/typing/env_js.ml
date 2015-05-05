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

type env = scope list ref
let env: env = ref []

let changeset = ref SSet.empty

let swap_changeset f =
  let oldset = !changeset in
  let newset = f oldset in
  changeset := newset;
  oldset

let global_scope: scope = ref SMap.empty

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
  global_scope := SMap.add x (create_env_entry t t None) !global_scope;
  cx.globals <- SSet.add x cx.globals;
  global_scope

let find_global cx x reason =
  if (SMap.mem x !global_scope)
  then
    global_scope
  else
    cache_global cx x reason

let find_env ?(for_type=false) cx x reason =
  let rec loop = function
    | [] -> find_global cx x reason
    | scope::scopes ->
        (match SMap.get x !scope with
        | Some entry when (not entry.for_type || for_type) -> scope
        | _ -> loop scopes)
  in
  loop (!env)

let get_from_scope x scope =
  SMap.find_unsafe x !scope

let set_in_scope x entry scope =
  scope := !scope |> SMap.add x entry

let exists_in_scope x scope =
  SMap.get x !scope

let update_scope ?(for_type=false) x (specific_t, general_t) scope =
  let values = !scope in
  let new_entry = match SMap.get x values with
    | Some {def_loc; for_type; _;} ->
        create_env_entry ~for_type specific_t general_t def_loc
    | None ->
        create_env_entry ~for_type specific_t general_t None
  in
  scope := values |> SMap.add x new_entry

let unset_in_scope x scope =
  scope := !scope |> SMap.remove x

let read_env ?(for_type=false) cx x reason =
  find_env ~for_type cx x reason |> get_from_scope x

let write_env ?(for_type=false) cx x shape reason =
  find_env ~for_type cx x reason |> update_scope ~for_type x shape

let push_env scope =
  env := scope :: !env

let pop_env () =
  env := List.tl !env

let flat_env () =
  List.fold_left (fun acc x ->
    SMap.union acc !x
  ) !global_scope !env

let switch_env scope =
  pop_env ();
  push_env scope

let peek_env () =
  List.hd !env

let init_env cx x shape =
  let scope = peek_env() in
  match exists_in_scope x scope with
  | None -> set_in_scope x shape scope
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
      set_in_scope x shape scope;
      Flow_js.unify cx shape.general general
  | Some { general; _ } ->
      Flow_js.unify cx general shape.general

let let_env x shape f =
  peek_env() |> set_in_scope x shape;
  f();
  peek_env() |> unset_in_scope x

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
  let scope = find_env cx x reason in
  (scope = global_scope, (get_from_scope x scope).specific)

let var_ref ?(for_type=false) cx x reason =
  let t = (read_env ~for_type cx x reason).specific in
  let p = pos_of_reason reason in
  mod_reason_of_t (repos_reason p) t

let get_refinement cx key r =
  let scope = peek_env () in
  match exists_in_scope key scope with
  | Some { specific; _ } ->
      let pos = pos_of_reason r in
      let t = mod_reason_of_t (repos_reason pos) specific in
      Some t
  | None ->
      None

let set_var ?(for_type=false) cx x specific_t reason =
  changeset := !changeset |> SSet.add x;
  let general = (read_env ~for_type cx x reason).general in
  Flow_js.flow cx (specific_t, general);
  write_env ~for_type cx x (specific_t, general) reason

let clone_env ctx =
  List.map (fun scope -> ref !scope) ctx

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
  | (scope::ctx, scope1::ctx1, scope2::ctx2) ->
      (* merge scope, scope1, scope2 *)
      let not_found = SSet.fold (fun x not_found ->
        match SMap.get x !scope with
        | Some {specific;general;def_loc;for_type;} ->
            let shape1 = SMap.find_unsafe x !scope1 in
            let shape2 = SMap.find_unsafe x !scope2 in
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
                Flow_js.flow cx (s1,tvar);
                Flow_js.flow cx (s2,tvar);
                Flow_js.flow cx (tvar,general);
                (tvar,general)
            in
            let for_type = for_type || shape1.for_type || shape2.for_type in
            scope := SMap.add x (create_env_entry ~for_type s g def_loc) !scope;
            not_found
        | None ->
            SSet.add x not_found
      ) changeset SSet.empty in
      (* look for the rest of changeset in outer scopes *)
      merge_env cx reason (ctx, ctx1, ctx2) not_found
  | _ -> assert false

let widen_env cx reason =
  let scope = List.hd !env in
  let vars = !scope in
  if SMap.cardinal vars < 32 then
  scope := vars |> SMap.mapi (fun x {specific;general;def_loc;for_type;} ->
    if specific = general
    then create_env_entry ~for_type specific general def_loc
    else
      let reason = replace_reason x reason in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.flow cx (specific,tvar);
      Flow_js.flow cx (tvar,general);
      create_env_entry ~for_type tvar general def_loc
  )

let rec copy_env_ cx reason x = function
  | ([],[]) -> ()
  | (scope1::ctx1, scope2::ctx2) ->
      (match SMap.get x !scope1 with
        | Some {specific=s1;_} ->
            let s2 = (SMap.find_unsafe x !scope2).specific in
            Flow_js.flow cx (s2,s1)
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
  List.iter (fun scope ->
    scope := SMap.mapi (fun x {specific=_;general;def_loc;for_type;} ->
      create_env_entry ~for_type general general def_loc
    ) !scope
 ) !env

let rec havoc_env2_ x = function
  | scope::scopes ->
      (match SMap.get x !scope with
      | Some {specific=_;general;def_loc;for_type;} ->
          scope := !scope |>
            SMap.add x (create_env_entry ~for_type general general def_loc)
      | None ->
          havoc_env2_ x scopes
      )
  | [] -> ()

let havoc_env2 xs =
  SSet.iter (fun x -> havoc_env2_ x !env) xs

let havoc_heap_refinements () =
  List.iter (fun scope ->
    scope := SMap.mapi (fun x ({specific=_;general;def_loc;for_type;} as entry) ->
      if is_refinement x
      then create_env_entry ~for_type general general def_loc
      else entry
    ) !scope
 ) !env

let clear_env reason =
  let scope = List.hd !env in
  scope := !scope |> SMap.mapi (fun x {specific;general;def_loc;for_type;} ->
    (* internal names (.this, .super, .return, .exports) are read-only *)
    if is_internal_name x
    then create_env_entry ~for_type specific general def_loc
    else create_env_entry ~for_type (UndefT reason) general def_loc
  )

let string_of_env cx ctx =
  String.concat "\n" (List.map (string_of_scope cx) ctx)

(* The following functions are used to narrow the type of variables
   based on dynamic checks. *)

let install_refinement cx x xtypes =
  if exists_in_scope x (peek_env ()) = None then
    let t = SMap.find_unsafe x xtypes in
    init_env cx x (create_env_entry t t None)

let refine_with_pred cx reason pred xtypes =
  SMap.iter (fun x predx ->
    if is_refinement x then install_refinement cx x xtypes;
    let reason' = replace_reason (spf "identifier %s" x) reason in
    let (is_global, tx) = get_var_ cx x reason' in
    if not is_global then (
      let rstr = spf "identifier %s when %s" x (string_of_predicate predx) in
      let reason = replace_reason rstr reason in
      let t = Flow_js.mk_tvar cx reason in
      let rt = PredicateT (predx, t) in
      Flow_js.flow cx (tx, rt);
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
