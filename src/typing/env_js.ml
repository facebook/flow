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
open Type   (* from Constraint_js *)
open Scope

(* helpers *)

(* refinement keys *)
let refinement_key names =
  "$REFI " ^ (String.concat "." names)

let is_refinement name =
  (String.length name) >= 5 && (String.sub name 0 5) = "$REFI"

(****************)
(* Environment *)
(****************)

(* the environment is a scope stack, frame stack and changeset stack.
   currently these are in lockstep, though this may change depending
   on how lexical scoping is implemented. *)

type 'a stack = 'a list ref

let scopes: Scope.t stack = ref []
let frames: int stack = ref []
let changesets: SSet.t stack = ref []

(* scopes *)

(* return the current scope *)
let peek_scope () =
  List.hd !scopes

(* return current scope stack *)
let get_scopes () =
  !scopes

(* use the passed f to iterate over all scopes *)
let iter_scopes f =
  List.iter f !scopes

(* clone the current scope stack (snapshots entry maps) *)
let clone_scopes scopes =
  List.map Scope.clone scopes

(* true if current top scope is an async function *)
(* TODO will need to walk once LexScopes appear *)
let in_async_scope () = Scope.(
  match (peek_scope ()).kind with
  | VarScope { async = true; } -> true
  | _ -> false
)

(* build a map of all entries in the current scope stack *)
(* Note that we accumulate entries bottom-up, so that
   shadowing is properly maintained *)
let all_entries () =
  List.fold_left (fun entries scope ->
    SMap.fold SMap.add entries scope.entries
  ) SMap.empty (List.rev !scopes)

(* frames *)

(* return the current frame *)
let peek_frame () =
  List.hd !frames

(* changesets *)

(* return the current changeset *)
let peek_changeset () =
  List.hd !changesets

(* helper: transform current changeset with passed f, swap, return prev *)
let swap_changeset f =
  let prev = peek_changeset () in
  changesets := f prev :: List.tl !changesets;
  prev

(* clear changeset, return previous *)
let clear_changeset () =
  swap_changeset (fun _ -> SSet.empty)

(* merge changeset with passed one, return previous *)
let merge_changeset changeset =
  swap_changeset (SSet.union changeset)

(* record a changed var in current changeset *)
let add_change name =
  swap_changeset (SSet.add name)

(* whole env *)

(* clear environment *)
let clear_env () =
  scopes := [];
  frames := [];
  changesets := []

(* push a new scope and frame into the environment *)
(* TODO maintain changelist here too *)
let push_env cx scope =
  (* frame id goes with new scope *)
  let frame = mk_id () in
  (* push scope and frame *)
  scopes := scope :: !scopes;
  frames := frame :: !frames;
  changesets := SSet.empty :: !changesets;
  (* add saved env snapshot under frame id *)
  cx.closures <- IMap.add frame (!frames, !scopes) cx.closures

(* pop a scope and frame from the environment *)
let pop_env () =
  scopes := List.tl !scopes;
  frames := List.tl !frames;
  changesets := List.tl !changesets

(* initialize a new environment (once per module) *)
let init_env cx module_scope =
  clear_env ();
  let global_scope = Scope.fresh () in
  push_env cx global_scope;
  push_env cx module_scope

(* replace the scope stack with the passed one,
   and update the current frame's cx.closure to point to it
 *)
let update_env cx new_scopes =
  let current_frame = peek_frame () in
  let stack, _ = IMap.find_unsafe current_frame cx.closures in
  cx.closures <- IMap.add current_frame (stack, new_scopes) cx.closures;
  scopes := new_scopes

(* end of basic env API *)

let global_poison = ["eval"; "arguments"]

let global_lexicals = [
  (internal_name "super");
  (internal_name "this")
]

(* any names that haven't been resolved in upper scopes
   wind up here. after handling special names, we register
   such as global builtins to be resolved later. *)
let cache_global cx name reason global_scope =
  if Scope.mem name global_scope
  then global_scope
  else (
    let t = (if List.mem name global_poison then
      AnyT.t
    else (if List.mem name global_lexicals then
      MixedT (reason_of_string "global object")
    else
      Flow_js.get_builtin cx name reason
    )) in
    Scope.add name (Scope.create_entry t t None) global_scope;
    cx.globals <- SSet.add name cx.globals;
    global_scope
  )

(* find scope that holds binding for a given name *)
let find_scope ?(for_type=false) cx name reason =
  let rec loop = function
    | [global_scope] ->
      cache_global cx name reason global_scope
    | scope :: scopes ->
      (match Scope.get name scope with
      | Some entry when (not entry.for_type || for_type) -> scope
      | _ -> loop scopes)
    | [] ->
      failwith "empty scope list"
  in
  loop !scopes

(* return binding entry for given name in env *)
let read_env ?(for_type=false) cx name reason =
  let scope = find_scope ~for_type cx name reason in
  Scope.get_unsafe name scope

(* update binding entry for given name in env *)
let write_env ?(for_type=false) cx name (specific, general) reason =
  let scope = find_scope ~for_type cx name reason in
  let { def_loc; for_type; _; } = Scope.get_unsafe name scope in
  let entry = Scope.create_entry ~for_type specific general def_loc in
  Scope.add name entry scope

(* initialize entry for given name in top scope, dealing with various
   situations involving a preexisting entry *)
let init_var cx name shape =
  let scope = peek_scope () in
  match Scope.get name scope with
  | None -> Scope.add name shape scope
  | Some { general; for_type; def_loc; _; } when for_type <> shape.for_type ->
      (* When we have a value var shadowing a type var, replace the type var
       * with the value var *)
      let shadowed_reason =
        mk_reason
          (spf "%s binding %s" (if for_type then "type" else "value") name)
          (match def_loc with None -> Loc.none | Some loc -> loc) in
      let shadower_reason =
        mk_reason
          (spf "%s binding %s" (if shape.for_type then "type" else "value") name)
          (match shape.def_loc with None -> Loc.none | Some loc -> loc) in
      Flow_js.add_error cx [
        shadower_reason, "This binding is shadowing";
        shadowed_reason, "which is a different sort of binding";
      ];
      Scope.add name shape scope;
      Flow_js.unify cx shape.general general
  | Some { general; _ } ->
      Flow_js.unify cx general shape.general

(* initialize entry for declare function in top scope *)
let init_declare_fun =
  let update_type seen_t new_t = match seen_t with
  | IntersectionT(reason, seen_ts) -> IntersectionT(reason, seen_ts@[new_t])
  | _ ->
    let reason = replace_reason "intersection type" (reason_of_t seen_t) in
    IntersectionT(reason,[seen_t;new_t])
  in
  fun cx name new_shape ->
    assert (not new_shape.for_type);
    let scope = peek_scope () in
    match Scope.get name scope with
    | None -> Scope.add name new_shape scope
    | Some seen_shape ->
      assert (not seen_shape.for_type);
      let shape = { seen_shape with
        specific = update_type seen_shape.specific new_shape.specific;
        general = update_type seen_shape.general new_shape.general
      } in
      Scope.add name shape scope

(* run passed function with given entry added to scope *)
(* CAUTION: caller must ensure that name isn't already bound,
   otherwise this will remove the preexisting binding *)
(* also note that this function does not push new frame
   or changeset *)
let let_env name shape f =
  let scope = peek_scope () in
  Scope.add name shape scope;
  f ();
  Scope.remove name scope

(************)
(* Contexts *)
(************)

(* Recall that for every variable we maintain two types, one flow-sensitive and
   the other flow-insensitive. *)

(* get var's specific type *)
let get_var ?(for_type=false) cx name reason =
  (read_env ~for_type cx name reason).specific

(* get var's general type - for annotated vars, this is the
   annotated type, and for others it's the union of all
   types assigned to the var throughout its lifetime.
 *)
let get_var_declared_type ?(for_type=false) cx name reason =
  (read_env ~for_type cx name reason).general

(* get var type, with position of given reason used in type's reason *)
(* TODO remove once positions in __flow are fully worked out *)
let var_ref ?(for_type=false) cx name reason =
  let t = (read_env ~for_type cx name reason).specific in
  let loc = loc_of_reason reason in
  mod_reason_of_t (repos_reason loc) t

(* get refinement entry *)
let get_refinement cx key r =
  let scope = peek_scope () in
  match Scope.get key scope with
  | Some { specific; _ } ->
      let loc = loc_of_reason r in
      let t = mod_reason_of_t (repos_reason loc) specific in
      Some t
  | None ->
      None

(* update var's binding entry to reflect assignment of a new
   specific type *)
let set_var ?(for_type=false) cx name specific_t reason =
  ignore (add_change name);
  let general = (read_env ~for_type cx name reason).general in
  Flow_js.flow cx (specific_t, general);
  write_env ~for_type cx name (specific_t, general) reason

(* The following function takes a set of variable names
   and a triple of environments (c1,c2,c3), and merges the information
   carried for changeset variables in c2 and c3 into c1. Assumptions:
   - c1, c2, c3 describe the same scope chain, so have the same depth
   - variables in changeset have been collected from this scope chain
 *)
let rec merge_env cx reason (ctx, ctx1, ctx2) changeset =
  if SSet.cardinal changeset > 0 then
  match (ctx, ctx1, ctx2) with
  | [], [], [] -> ()
  | scope::ctx, scope1::ctx1, scope2::ctx2 ->
      (* merge scope, scope1, scope2 *)
      let not_found = SSet.fold (fun name not_found ->
        match Scope.get name scope,
          Scope.get name scope1, Scope.get name scope2 with
        | Some { specific; general; def_loc; for_type },
            Some shape1, Some shape2 ->
            let specific1 = shape1.specific in
            let specific2 = shape2.specific in
            let s, g =
              if specific = specific1 && specific = specific2
              then specific, general
              else if specific1 = general || specific2 = general
              then general, general
              else
                let reason = replace_reason name reason in
                let tvar = Flow_js.mk_tvar cx reason in
                Flow_js.flow cx (specific1, tvar);
                Flow_js.flow cx (specific2, tvar);
                Flow_js.flow cx (tvar, general);
                tvar, general
            in
            let for_type = for_type || shape1.for_type || shape2.for_type in
            let new_entry = Scope.create_entry ~for_type s g def_loc in
            Scope.add name new_entry scope;
            not_found
        | Some _, _, _ ->
            (* Ideally, this case should be unreachable. However, there is
               currently an issue where heap refinements leak into scope without
               having been installed in scope1 / scope2: this is exposed at
               least in switch statements. Tracked by #7534515.*)
            not_found
        | None, _, _ ->
            SSet.add name not_found
      ) changeset SSet.empty in
      (* look for the rest of changeset in outer scopes *)
      merge_env cx reason (ctx, ctx1, ctx2) not_found
  | _ -> assert false

(* in the top scope, convert specific types to tvars with former
   specific type as incoming lower bound, and general type as
   upper bound. This prepares the specific type for later merging
   during path-dependent analysis.
   The size limit prevents pathologically large control flow
   substructures from causing combinatorial explosions.
 *)
let widen_env cx reason =
  let scope = peek_scope () in
  let vars = scope.entries in
  if SMap.cardinal vars < 32 then
  scope.entries <- vars |> SMap.mapi (
    fun name {specific;general;def_loc;for_type;} ->
      if specific = general
      then Scope.create_entry ~for_type specific general def_loc
      else
        let reason = replace_reason name reason in
        let tvar = Flow_js.mk_tvar cx reason in
        Flow_js.flow cx (specific,tvar);
        Flow_js.flow cx (tvar,general);
        Scope.create_entry ~for_type tvar general def_loc
    )

(* TODO explain *)
let rec copy_env_ cx reason name = function
  | ([],[]) -> ()
  | (scope1::ctx1, scope2::ctx2) ->
      (match Scope.get name scope1 with
        | Some {specific=s1;_} ->
            let s2 = (Scope.get_unsafe name scope2).specific in
            Flow_js.flow cx (s2,s1)
        | None -> copy_env_ cx reason name (ctx1,ctx2)
      )
  | _ -> assert false

let copy_env cx reason (ctx1,ctx2) names =
  SSet.iter (fun name -> copy_env_ cx reason name (ctx1,ctx2)) names

(* The protocol around havoc has changed a few times.
   The following function used to do most of the work, but is now subsumed by
   havoc_ctx, and is now used only to clear the environment when looking at
   a function body. Also see below. *)

(* clear refinement informnation for all binding entries in env *)
let havoc_all () =
  iter_scopes (
    Scope.update (
      fun name { for_type; general; def_loc; _ } ->
        Scope.create_entry ~for_type general general def_loc))

(* clear refinement info for (topmost binding of) given name in env *)
let havoc_var name =
  let rec loop = function
  | [] -> ()
  | scope::scopes ->
    match Scope.get name scope with
    | Some {specific=_;general;def_loc;for_type;} ->
        let entry = Scope.create_entry ~for_type general general def_loc in
        Scope.add name entry scope
    | None ->
        loop scopes
  in loop !scopes

(* clear refinement info for (topmost bindings of) given names in env *)
let havoc_vars names =
  SSet.iter havoc_var names

(* clear refinement info for heap refinement pseudovars in env *)
let havoc_heap_refinements () =
  iter_scopes (
    Scope.update (
      fun name ({specific=_;general;def_loc;for_type;} as entry) ->
        if is_refinement name
        then Scope.create_entry ~for_type general general def_loc
        else entry))

(* set specific type of every non-internal var in top scope to undefined *)
(* TODO rename *)
let clear_env reason =
  let clear_entry name {specific;general;def_loc;for_type;} =
    (* internal names (.this, .super, .return, .exports) are read-only *)
    if is_internal_name name
    then Scope.create_entry ~for_type specific general def_loc
    else Scope.create_entry ~for_type (UndefT reason) general def_loc
  in
  Scope.update clear_entry (peek_scope ())

let string_of_env cx env =
  String.concat "\n" (List.map (string_of_scope cx) env)

(* The following functions are used to narrow the type of variables
   based on dynamic checks. *)

let install_refinement cx name xtypes =
  if not (Scope.mem name (peek_scope ())) then
    let t = SMap.find_unsafe name xtypes in
    init_var cx name (Scope.create_entry t t None)

let refine_with_pred cx reason pred xtypes =
  SMap.iter (fun name predx ->
    if is_refinement name then install_refinement cx name xtypes;
    let reason' = replace_reason (spf "identifier %s" name) reason in
    let tx = get_var cx name reason' in
    let rstr = spf "identifier %s when %s" name (string_of_predicate predx) in
    let reason = replace_reason rstr reason in
    let t = Flow_js.mk_tvar cx reason in
    let rt = PredicateT (predx, t) in
    Flow_js.flow cx (tx, rt);
    set_var cx name t reason
  ) pred

let refine_env cx reason pred xtypes f =
  let ctx = get_scopes () in
  let new_ctx = clone_scopes ctx in
  update_env cx new_ctx;
  let oldset = clear_changeset () in
  refine_with_pred cx reason pred xtypes;
  let result = f () in
  let newset = merge_changeset oldset in
  merge_env cx reason (ctx, new_ctx, ctx) newset;
  update_env cx ctx;
  result
