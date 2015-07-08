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
open Utils_js
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
  | IntersectionT (reason, seen_ts) ->
    IntersectionT (reason, seen_ts @ [new_t])
  | _ ->
    let reason = replace_reason "intersection type" (reason_of_t seen_t) in
    IntersectionT (reason, [seen_t; new_t])
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
   and a triple of environments - original and two derivations -
   and merges the information carried for changeset variables
   from the derivations into the original.
   Assumptions:
   - all environments describe the same scope chain, so have the same
   depth
   - variables in the changeset have been collected from this scope
   chain, so a variable found in the original will be found in the
   others.
   Note that the preceding is not true for refinement pseudovars -
   these may be distributed arbitrarily across environments. This is
   fine - in such situations we simply forget the refinement.
 *)
let rec merge_env cx reason (orig_env, new_env1, new_env2) changeset =
  if SSet.cardinal changeset > 0 then
  match orig_env, new_env1, new_env2 with
  | [], [], [] -> ()
  | scope::orig_env, scope1::new_env1, scope2::new_env2 ->
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
            (* refinements may be distributed non-uniformly.
               such cases where the original env contains the refinement
               come here; cases where it does not wind up falling out the
               end of the recursion. In either case we forget the refinement.
             *)
            (if not (is_refinement name) then assert_false (spf
              "non-uniform distribution of non-refinement var %s in merge_env"
              name));
            not_found
        | None, _, _ ->
            SSet.add name not_found
      ) changeset SSet.empty in
      (* look for the rest of changeset in outer scopes *)
      merge_env cx reason (orig_env, new_env1, new_env2) not_found
  | _ -> assert false

(* given environments env1 and env2, try to find a binding for a given
   name at the same scope position in both environments and if found,
   flow the specific type from env2 into the specific type from env1.
   Note: non-refinement bindings should always appear in both or neither
   env; refinements may be distributed unevenly.
 *)
let rec copy_env_var cx reason name = function
  | [], [] -> ()
  | scope1::env1, scope2::env2 ->
      (match Scope.get name scope1 with
        | Some {specific = s1; _ } ->
          (match Scope.get name scope2 with
            | Some {specific = s2; _ } ->
              Flow_js.flow cx (s2, s1)
            | None ->
              (* refinements may be distributed non-uniformly.
                 such cases where env1 contains the refinement but not env2
                 come here.
               *)
              if not (is_refinement name) then assert_false (spf
               "non-uniform distribution of non-refinement var %s in copy_env"
               name))
        | None -> copy_env_var cx reason name (env1, env2)
      )
  | _ -> assert false

(* apply copy_env_var to a list of names, typically a changeset *)
let copy_env cx reason (env1, env2) names =
  names |> SSet.iter (fun name ->
    copy_env_var cx reason name (env1, env2)
  )

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
    | Some entry ->
      (match havoc_entry ~make_specific:(fun gen -> gen) name entry with
      | Some entry -> Scope.add name entry scope
      | None -> Scope.remove name scope)
    | None ->
        loop scopes
  in loop !scopes

(* clear refinement info for (topmost bindings of) given names in env *)
let havoc_vars names =
  SSet.iter havoc_var names

(* Clear entries for heap refinement pseudovars in env.
   Real variables are left untouched.
   For more background, see Scope.havoc_entry.
 *)
let havoc_heap_refinements () =
  iter_scopes (Scope.update_opt havoc_entry)

(* set specific type of every non-internal var in top scope to undefined *)
(* TODO rename *)
let clear_env reason =
  let clear_entry = havoc_entry ~make_specific:(fun _ -> UndefT reason) in
  Scope.update_opt clear_entry (peek_scope ())

let string_of_env cx env =
  String.concat "\n" (List.map (string_of_scope cx) env)

(* The following functions are used to narrow the type of variables
   based on dynamic checks. *)

(* Directly refine an expression's type to t. This refinement
   may already be present in the top scope - if so, we overwrite it.

   However note that we do so at a low level, rather than by using the
   standard set_var mechanism. This is to avoid unwanted reentrancy:
   set_var calls flow (specific, general) to model the effect of a
   variable assignment in user code, and this in certain cases
   (broadly, assignment of function values) may provoke havoc_ctx
   into clearing the refinement we're in the process of installing.

   (Note: heap refinements do not need the general type field,
   since they are cleared on havoc. An upcoming diff will refactor
   scope entries to reflect this.)
 *)
let add_direct_refinement cx name reason t =
  (if not (is_refinement name) then assert_false (spf
    "non-refinement key %s passed to add_direct_refinement" name));
  let scope = peek_scope () in
  ignore (add_change name);
  let loc = loc_of_reason reason in
  let entry = Scope.create_entry ~for_type:false t t (Some loc) in
  Scope.add name entry scope

(* add predicate refinements from given preds map to environment.
   note: orig_types maps names to unrefined types. this param is
   only necessary for fresh pseudovars like heap refinements -
   others can be obtained via get_var.
 *)
let refine_with_preds cx reason preds orig_types =
  preds |> SMap.iter (fun name pred ->

    let refi_reason =
      let pred_str = string_of_predicate pred in
      let rstr = spf "identifier %s when %s" name pred_str in
      replace_reason rstr reason in

    let mk_refi_type orig_type =
      Flow_js.mk_tvar_where cx refi_reason (fun refi_type ->
        let pred_type = PredicateT (pred, refi_type) in
        Flow_js.flow cx (orig_type, pred_type))
    in

    if is_refinement name then (
      (* for heap refinements, we just add new entries *)
      let orig_type = SMap.find_unsafe name orig_types in
      let refi_type = mk_refi_type orig_type in
      add_direct_refinement cx name refi_reason refi_type
    ) else (
      (* for real vars, we model value assignment *)
      let orig_type =
        let get_reason = replace_reason (spf "identifier %s" name) reason in
        get_var cx name get_reason
      in
      let refi_type = mk_refi_type orig_type in
      set_var cx name refi_type refi_reason
    )
  )

(* run the given function in a clone of the current environment
   augmented by the given refinement map, then merge the final
   state of the cloned environment back into the reinstated
   original *)
let refine_env cx reason preds orig_types f =
  let orig_env = get_scopes () in
  let new_env = clone_scopes orig_env in
  update_env cx new_env;
  let oldset = clear_changeset () in
  refine_with_preds cx reason preds orig_types;
  let result = f () in
  let newset = merge_changeset oldset in
  merge_env cx reason (orig_env, new_env, orig_env) newset;
  update_env cx orig_env;
  result
