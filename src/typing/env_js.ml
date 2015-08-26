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

(* lookup modes:

   - ForValue is a lookup from a syntactic value location, i.e. standard
     JS code

   - ForType is a lookup from a syntactic type location, e.g. annotations,
     interface declarations etc.

   - ForTypeof is a lookup from a typeof expression (necessarily in a type
     location)

   Rules:

   1. ForValue lookups give errors if they retrieve type aliases (note: we
      have a single namespace, so any name resolves uniquely to either a
      value or type)

   2. ForValue lookups give errors if they forward reference non-hoisted
      things (lets or consts)

   3. ForType lookups may return values or type aliases, since some values
      also denote types - e.g. a generator function F also denotes the type
      of the objects it creates. Of course many values don't also have a type
      denotation and thus errors in type position. But we don't know the type
      of a symbol during local inference as a rule, so errors of this kind are
      not raised here.

   4. ForTypeof lookups are in fact ForValue lookups, but due to the order in
      which AST traversal takes place, these lookups may legitimately violate
      rule #2, hence the need for a special mode.
 *)
module LookupMode = struct
  type t = ForValue | ForType | ForTypeof
end

open LookupMode

(****************)
(* Environment *)
(****************)

(* the environment is a scope stack, frame stack and changeset stack.
   currently these are in lockstep, though this may change depending
   on how lexical scoping is implemented. *)

type 'a stack = 'a list ref

(* changeset is a set of changed variables by name
   and a set of changed refinements by key *)
type changeset = SSet.t * KeySet.t

let scopes: Scope.t stack = ref []
let frames: int stack = ref []
let changesets: changeset stack = ref []

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
  | VarScope { async } -> async
  | _ -> false
)

(* build a map of all var entries - no refis - in the current
   scope stack.
   Note that we accumulate entries bottom-up, so that
   shadowing is properly maintained *)
let all_entries () =
  List.fold_left (fun entries scope ->
    SMap.union scope.entries entries
  ) SMap.empty (List.rev !scopes)

(* frames *)

(* return the current frame *)
let peek_frame () =
  List.hd !frames

(* changesets *)

(* return the current changeset *)
let peek_changeset () =
  List.hd !changesets

(* helper: transform current changeset, given
   transform functions for vars and/or refis.
   swap, return prev *)
let swap_changeset f_vars f_refis =
  let prev_vars, prev_refis = peek_changeset () in
  let apply_opt arg = function None -> arg | Some f -> f arg in
  let new_vars = apply_opt prev_vars f_vars in
  let new_refis = apply_opt prev_refis f_refis in
  changesets := (new_vars, new_refis) :: List.tl !changesets;
  prev_vars, prev_refis

(* clear changeset, return previous *)
let clear_changeset () =
  swap_changeset (Some (fun _ -> SSet.empty)) (Some (fun _ -> KeySet.empty))

(* merge changeset with passed one, return previous *)
let merge_changeset (vars, refis) =
  swap_changeset (Some (SSet.union vars)) (Some (KeySet.union refis))

(* record a changed var in current changeset *)
let add_change_var name =
  swap_changeset (Some (SSet.add name)) None

let add_change_refi key =
  swap_changeset None (Some (KeySet.add key))

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
  changesets := (SSet.empty, KeySet.empty) :: !changesets;
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

let global_any = ["eval"; "arguments"]

let global_lexicals = [
  (internal_name "super");
  (internal_name "this")
]

(* any names that haven't been resolved in upper scopes
   wind up here. after handling special names, we add a Var
   binding to the local scope, and register a lookup with
   the global builtin type, to be resolved later.

   Of course, the globals being referred to may not be vars:
   they may instead be consts or type aliases. Currently we
   have no process for checking the use of a global binding
   against the kind of binding it turns out to be: this global
   scope is simply a proxy; resolution takes place as a result
   of the GetT types created in the call to Flow_js.get_builtin.

   This means that we have some false negatives, currently.
   Errors that go unreported currently include:
   - type aliases referred to from value positions
   - global consts assigned to locally
   The first is especially problematic, since it will actually
   cause a runtime error.

   The least complex solution to this will be to process libs
   eagerly, and save the actual scope to check against here
   when doing local checking of modules. Alternatively, we
   would have to record in the GetT (or an enrichment) enough
   information about what uses were made of the reference to
   flag such errors on the current deferred basis.
   tests/global_ref tracks this issue.
 *)
let cache_global cx name reason global_scope =
  let t =
    if List.mem name global_any
    then AnyT.t
    else (if List.mem name global_lexicals
    then MixedT (reason_of_string "global object")
    else Flow_js.get_builtin cx name reason)
  in
  let entry = Scope.Entry.(new_var t ~state:Initialized) in
  Scope.add_entry name entry global_scope;
  cx.globals <- SSet.add name cx.globals;
  global_scope, entry

(* look for scope that holds binding for a given name.
   if found, return scope and entry.
   note that anything we don't resolve otherwise, we add to the
   global scope after generating a deferred lookup, which may fail
   later. *)
let find_entry cx name reason =

  let rec loop = function
    | [] ->
      assert_false "empty scope list"

    | scope :: scopes ->
      match Scope.get_entry name scope with
      | Some entry ->
        scope, entry

      | None ->
        (* keep looking until we're at the global scope *)
        match scopes with
        | [] ->
          cache_global cx name reason scope
        | _ ->
          loop scopes
  in

  loop !scopes

(* helpers *)
let entry_loc_unopt entry =
  match Entry.loc entry with None -> Loc.none | Some loc -> loc

let entry_reason name entry =
  mk_reason
    (spf "%s %s" (Entry.string_of_kind entry) name)
    (entry_loc_unopt entry)

let binding_error msg cx name entry reason =
  let reason1 = replace_reason name reason in
  let reason2 = entry_reason name entry in
  Flow_js.add_error cx [reason1, msg; reason2, ""]

let already_bound_error =
  binding_error "name is already bound"

(* initialization of entries happens during a preliminary pass through a
   scoped region of the AST (dynamic for hoisted things, lexical for
   lexical things). this leaves them in a germinal state which is
   then read and written during the main traversal of the AST *)

(* helper: initialize entry for given key in top scope,
   dealing with various situations involving a preexisting entry
   (since multiple declarations - sometimes but not always erroneous -
   may appear in an AST)
 *)
let bind_entry cx name entry =
  let scope = peek_scope () in
  match Scope.get_entry name scope with

  | None ->
    Scope.add_entry name entry scope

  | Some prev ->
    Entry.(match entry, prev with
    | Value val_new, Value val_prev
      when val_new.kind = Var && val_prev.kind = Var ->
      (* leave existing in place, but unify with new *)
      (* TODO currently we don't step on specific. shouldn't we? *)
      Flow_js.unify cx val_prev.general val_new.general

    | Value _, _
    | Type _, _ ->
      (* incompatible or non-redeclarable new and previous entries.
         we pass these through silently, because any such situation
         will show up as a corresponding error at init time, and it's
         simpler (and error info is better) if we raise errors there. *)
      ()
    )

(* bind var entry *)
let bind_var ?(state=Entry.Declared) cx name t loc =
  bind_entry cx name (Entry.new_var t ~loc ~state)

(* bind let entry *)
let bind_let ?(state=Entry.Undeclared) cx name t loc =
  bind_entry cx name (Entry.new_let t ~loc ~state)

(* bind const entry *)
let bind_const ?(state=Entry.Undeclared) cx name t loc =
  bind_entry cx name (Entry.new_const t ~loc ~state)

(* bind type entry *)
let bind_type cx name t loc =
  bind_entry cx name (Entry.new_type t ~loc ~state:Entry.Declared)

(* vars coming from 'declare' statements are preinitialized *)
let bind_declare_var = bind_var ~state:Entry.Initialized

(* bind entry for declare function *)
let bind_declare_fun =

  let update_type seen_t new_t = match seen_t with
  | IntersectionT (reason, seen_ts) ->
    IntersectionT (reason, seen_ts @ [new_t])
  | _ ->
    let reason = replace_reason "intersection type" (reason_of_t seen_t) in
    IntersectionT (reason, [seen_t; new_t])
  in

  fun cx name t reason ->
    let scope = peek_scope () in
    match Scope.get_entry name scope with
    | None ->
      let entry =
        Entry.new_var t ~loc:(loc_of_reason reason) ~state:Entry.Initialized
      in
      Scope.add_entry name entry scope

    | Some prev ->
      Entry.(match prev with

      | Value v when v.kind = Var ->
        let entry = Value { v with
          value_state = Initialized;
          specific = update_type v.specific t;
          general = update_type v.general t
        } in
        Scope.add_entry name entry scope

      | _ ->
        (* declare function shadows some other kind of binding *)
        already_bound_error cx name prev reason
      )

(* helper: move a Let/Const's entry's state from Undeclared to Declared.
   Only needed for let and const to push things into scope for potentially
   recursive internal refs: hoisted things (vars and types) become declared
   immediately on binding.
 *)
let declare_value_entry kind cx name reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match entry with
  | Value v when v.kind = kind && v.value_state = Undeclared ->
    let new_entry = Value { v with value_state = Declared } in
    Scope.add_entry name new_entry scope
  | _ ->
    already_bound_error cx name entry reason
  )

let declare_let = declare_value_entry Entry.Let
let declare_const = declare_value_entry Entry.Const

(* Used to adjust state based on whether there is an annotation.

   This reveals an interesting tension between annotations and
   flow-sensitivity. Consider:

   var x:A = new B(); // B is a subclass of A
   // ^ should we have x:A or x:B after this initialization?
   x = new B();
   // ^ how about after this assignment?
   if (x instanceof B) {
   // ^ and how about after this dynamic check?
   }

   In general, it seems desirable to narrow down the type
   of a variable when possible (indeed, that's the whole point
   of flow-sensitivity) but it is unclear what to do when there
   are annotations. It might be argued that annotations override
   flow-sensitivity, but that is not very nice, because it would
   force patterns such as:

   var y = x;
   if (y instanceof B) {
   // ^ we have y:B after this dynamic check, yet x:A
   }

   Below, we do a compromise; we remain flow-sensitive in general
   but treat an annotation as if it were part of initialization.
   This seems to be a nice solution, because (e.g.) it would ban
   code such as:

   var y:string; // initialize with a string!

   as well as:

   function foo(x:B) { }
   var x:A = new B();
   foo(x);
*)
let force_general_type cx ({ Entry.general; _ } as value_binding) = Entry.(
  Flow_js.flow cx (general, general);
  { value_binding with specific = general }
)

(* helper - update var entry to reflect assignment/initialization *)
(* note: here is where we understand that a name can be multiply var-bound *)
let init_value_entry kind cx name ~has_anno specific reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match kind, entry with

  | Var, Value ({ kind = Var; _ } as v)
  | Let, Value ({ kind = Let; value_state = Undeclared | Declared; _ } as v)
  | Const, Value ({ kind = Const; value_state = Undeclared | Declared; _ } as v) ->
    (* NOTE: causes havocing in some cases, so, reentrant *)
    Flow_js.flow cx (specific, v.general);
    let value_binding = { v with value_state = Initialized; specific } in
    (* if binding is annotated, flow annotated type like initializer *)
    let new_entry = Value (
      if has_anno
      then force_general_type cx value_binding
      else value_binding
    ) in
    Scope.add_entry name new_entry scope;
  | _ ->
    already_bound_error cx name entry reason
  )

let init_var = init_value_entry Entry.Var
let init_let = init_value_entry Entry.Let
let init_const = init_value_entry Entry.Const

(* update type alias to reflect initialization in code *)
let init_type cx name _type reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match entry with
  | Type ({ type_state = Declared; _ } as t)->
    Flow_js.flow cx (_type, t._type);
    let new_entry = Type { t with type_state = Initialized; _type } in
    Scope.add_entry name new_entry scope
  | _ ->
    already_bound_error cx name entry reason
  )

(* used to enforce annotations: see commentary in force_general_type *)
let pseudo_init_declared_type cx name reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match entry with
  | Value value_binding ->
    let entry = Value (force_general_type cx value_binding) in
    Scope.add_entry name entry scope
  | Type _ ->
    assert_false (spf "pseudo_init_declared_type %s: Type entry" name)
  )

(* get types from value entry - enforces state-based guards *)
let value_entry_types ~lookup_mode cx name reason entry scope =
  Entry.(match entry with

  | Value { kind = Let | Const; value_state = Undeclared; _ }
      when lookup_mode = ForValue
      && scope = peek_scope () (* see comment header *)
      ->
    (* fwd ref to let/const from value pos: TDZ *)
    let msg = spf "%s referenced before declaration"
      (string_of_kind entry) in
    binding_error msg cx name entry reason;
    let t = AnyT.at (entry_loc_unopt entry) in t, t
(*
  TODO

  | Value { kind = Var; value_state = Declared; general; _ }
      when not for_type ->
    (* ref to var from value pos before initialization: undefined *)
    VoidT.at (entry_loc_unopt entry), general
*)
  | Value { specific; general; _ } ->
    specific, general

  | Type _ -> assert_false "Type entry passed to value_entry_types"
  )

(* helper - does semantic checking and returns specific, general pair *)
let read_entry ~lookup_mode ~specific cx name reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match entry with

  | Type { type_loc; _ } when lookup_mode != ForType ->
    let msg = "type alias referenced from value position" in
    binding_error msg cx name entry reason;
    AnyT.at (entry_loc_unopt entry)

  | Type t ->
    t._type

  | Value _ ->
    let s, g = value_entry_types ~lookup_mode cx name reason entry scope in
    if specific then s else g
  )

(* get var's specific type *)
let get_var ?(lookup_mode=ForValue) =
  read_entry ~lookup_mode ~specific:true

(* get var's general type - for annotated vars, this is the
   annotated type, and for others it's the union of all
   types assigned to the var throughout its lifetime.
 *)
let get_var_declared_type ?(lookup_mode=ForValue) =
  read_entry ~lookup_mode ~specific:false

(* get var type, with location of given reason used in type's reason *)
(* TODO remove once positions in __flow are fully worked out *)
let var_ref ?(lookup_mode=ForValue ) cx name reason =
  let t = get_var ~lookup_mode cx name reason in
  let loc = loc_of_reason reason in
  mod_reason_of_t (repos_reason loc) t

(* get refinement entry *)
let get_refinement cx key reason =
  let scope = peek_scope () in
  match Scope.get_refi key scope with
  | Some { refined; _ } ->
    let loc = loc_of_reason reason in
    let t = mod_reason_of_t (repos_reason loc) refined in
    Some t
  | _ ->
    None

(* helper: update let or var entry to reflect assignment/initialization *)
let set_var cx name specific reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match entry with

  | Value ({ kind = Let | Var; _ } as v) ->
    ignore (add_change_var name);
    (* NOTE: causes havocing in some cases, so, reentrant *)
    Flow_js.flow cx (specific, v.general);
    (* add updated entry *)
    let update = Entry.Value { v with value_state = Initialized; specific } in
    Scope.add_entry name update scope

  | Value { kind = Const; _ } ->
    let msg = "const cannot be reassigned" in
    binding_error msg cx name entry reason

  | Type _ ->
    let msg = "type alias referenced from value position" in
    binding_error msg cx name entry reason
  )

(* set const's specific type to reflect a refinement test (internal) *)
let refine_const cx name specific reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match entry with

  | Value v when v.kind = Const ->
    ignore (add_change_var name);
    (* NOTE: causes havocing in some cases, so, reentrant *)
    Flow_js.flow cx (specific, v.general);
    let updagte = Entry.Value { v with value_state = Initialized; specific } in
    Scope.add_entry name updagte scope

  | _ ->
    assert_false (spf "refine_const called on %s %s"
      (string_of_kind entry) name)
  )

(* The following function takes a set of variable names
   and a triple of environments - original and two derivations -
   and merges the bindings indicated by changeset keys
   from the derivations into the original.
   Consistency assumptions:
   - all environments describe the same scope chain, so have the same
   depth, with each scope triple representing an original and its two
   derivative children.
   - non-refinement bindings will be distributed evenly, appearing in
   either all scopes of a triple or none of them. In contrast,
   refinements may be distributed arbitrarily across environments.
   This is fine - in such situations we simply forget the refinement.
 *)
let merge_env =

  (* merge types of original mutable entry with two children *)
  let merge_var cx reason name (specific0, general0) specific1 specific2 =
    if specific0 = specific1 && specific0 = specific2
    then specific0, general0
    else if specific1 = general0 || specific2 = general0
    then general0, general0
    else
      let reason = replace_reason name reason in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.flow cx (specific1, tvar);
      Flow_js.flow cx (specific2, tvar);
      Flow_js.flow cx (tvar, general0);
      tvar, general0
  in

  (* look for and merge entries bound by name, starting in topmost scope *)
  let rec merge_entry cx reason (env0, env1, env2) name = Entry.(
    match env0, env1, env2 with
    | [], [], [] ->
      ()
    | scope0 :: env0, scope1 :: env1, scope2 :: env2 -> Scope.(
      let get = get_entry name in
      match get scope0, get scope1, get scope2 with
      (* entry not found in this scope - recurse *)
      | None, None, None ->
        merge_entry cx reason (env0, env1, env2) name
      (* merge child value types back to original *)
      | Some (Value v as orig),
        Some (Value _ as child1), Some (Value _ as child2) ->
        let types = value_entry_types ~lookup_mode:ForValue cx name reason in
        let s0, g0 = types orig scope0 in
        let s1, _ = types child1 scope1 in
        let s2, _ = types child2 scope2 in
        let specific, general = merge_var cx reason name (s0, g0) s1 s2 in
        let entry = Entry.Value { v with specific; general } in
        Scope.add_entry name entry scope0;
      (* type aliases can't be refined or reassigned, shouldn't be here *)
      | Some (Type _), Some (Type _), Some (Type _) ->
        assert_false (spf
          "merge_env %s: type alias %s found in changelist"
          (string_of_reason reason)
          name)
      (* global lookups may leave new entries in one or both child envs *)
      | None, child1, child2 when env0 = [] ->
        (* ...in which case we can forget them *)
        ()
      (* otherwise, non-refinement uneven distributions are asserts. *)
      | orig, child1, child2 ->
        let print_entry_kind_opt = function
        | None -> "None"
        | Some e -> spf "Some %s" (string_of_kind e)
        in assert_false (spf
          "merge_env %s: non-uniform distribution of entry %s: %s, %s, %s"
          (string_of_reason reason)
          name
          (print_entry_kind_opt orig)
          (print_entry_kind_opt child1)
          (print_entry_kind_opt child2))
      )
    | _ ->
      assert_false (spf
        "merge_entry %s: ragged scope lists"
        (string_of_reason reason))
  ) in

  (* merge a refis bound to key if present. handle uneven distributions.
     note that refis live only in top scope. *)
  let merge_refi cx reason (env0, env1, env2) key =
    match env0, env1, env2 with
    | [], [], [] ->
      ()
    | scope0 :: env0, scope1 :: env1, scope2 :: env2 -> Scope.(
      let get = get_refi key in
      match get scope0, get scope1, get scope2 with
      (* evenly distributed refinements are merged *)
      | Some base, Some child1, Some child2 ->
        let name = Key.string_of_key key in
        let refined, original = merge_var cx reason name
          (base.refined, base.original) child1.refined child2.refined in
        let refi = { base with refined; original } in
        add_refi key refi scope0
      (* refi not found or unevenly distributed. the latter means that a
         refi was introducted after envs diverged. clear from original env *)
      | _ ->
        remove_refi key scope0)
    | _ ->
      assert_false (spf
        "merge_refi %s: ragged scope lists"
        (string_of_reason reason))
  in

  (* copy entries and refis bound to names and keys, respectively *)
  fun cx reason (env0, env1, env2) (vars, refis) ->
    vars |> SSet.iter (merge_entry cx reason (env0, env1, env2));
    refis |> KeySet.iter (merge_refi cx reason (env0, env1, env2))


(* copy changes from env2 into env1 *)
let copy_env  = Entry.(

  (* look for and copy entry, starting in topmost scope *)
  let rec copy_entry cx reason (env1, env2) name =
    match env1, env2 with

    | scope1 :: env1, scope2 :: env2 -> Scope.(
      match get_entry name scope1, get_entry name scope2 with

      | Some (Value _ as v1), Some (Value _ as v2) ->
        (* flow env2's specific type into env1's specific type *)
        let s1, _ = value_entry_types ~lookup_mode:ForValue
          cx name reason v1 scope1 in
        let s2, _ = value_entry_types ~lookup_mode:ForValue
          cx name reason v2 scope2
        in
        Flow_js.flow cx (s2, s1)

      | Some (Type _), Some (Type _) ->
        (* immutables can't be widened by assignment *)
        ()

      | None, None ->
        (* not found, try outer scopes *)
        copy_entry cx reason (env1, env2) name

      (* global lookups may leave new entries in env2 *)
      | None, entry2 when env1 = [] ->
        (* ...in which case we can forget it *)
        ()

      | entry1, entry2 ->
        (* uneven distributions *)
        let print_entry_kind_opt = function
        | None -> "None"
        | Some e -> spf "Some %s" (string_of_kind e)
        in assert_false (spf
          "copy_env %s: non-uniform distribution of entry %s: %s, %s"
          (string_of_reason reason)
          name
          (print_entry_kind_opt entry1)
          (print_entry_kind_opt entry2))
      )

    | [], [] -> ()

    | _ -> assert_false (spf
        "copy_env %s: malformed scope lists"
        (string_of_reason reason))


  in

  (* look for and copy refinement in top scope only *)
  let copy_refi cx reason (env1, env2) key =
    match env1, env2 with
    | scope1 :: env1, scope2 :: env2 -> Scope.(
      match get_refi key scope1, get_refi key scope2 with
      | Some { refined = t1; _ }, Some { refined = t2; _ } ->
        (* present in body scopes: flow second type into first *)
        Flow_js.flow cx (t2, t1)
      | _ ->
        (* note: uneven cases imply refi was added after splitting: remove *)
        ()
      )
    | [], [] -> ()
    | _ -> assert_false "copy_refi: ragged scope lists"
  in

  (* copy entries and refis bound to names and keys, respectively *)
  fun cx reason (env1, env2) (vars, refis) ->
    vars |> SSet.iter (copy_entry cx reason (env1, env2));
    refis |> KeySet.iter (copy_refi cx reason (env1, env2))
)

(* in the top scope, convert specific types to tvars with former
   specific type as incoming lower bound, and general type as
   upper bound. This prepares the specific type for later merging
   during path-dependent analysis.
   The size limit prevents pathologically large control flow
   substructures from causing combinatorial explosions.
 *)
let widen_env =

  let widened cx reason name specific general =
    if specific = general
    then None
    else
      let reason = replace_reason name reason in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.flow cx (specific, tvar);
      Flow_js.flow cx (tvar, general);
      Some tvar
  in

  let widen_var cx reason name ({ Entry.specific; general; _ } as var) =
    match widened cx reason name specific general with
    | None -> var
    | Some tvar -> { var with Entry.specific = tvar }
  in

  let widen_refi cx reason name ({ refined; original; _ } as refi) =
    match widened cx reason name refined original with
    | None -> refi
    | Some tvar -> { refi with refined = tvar }
  in

  fun cx reason ->
    let top_scope = peek_scope () in
    if SMap.cardinal top_scope.entries < 32 then
      top_scope |> Scope.update_entries Entry.(fun name -> function
        | Value var -> Value (widen_var cx reason name var)
        | entry -> entry
      );
    if KeyMap.cardinal top_scope.refis < 32 then
      top_scope |> Scope.update_refis (fun key refi ->
        widen_refi cx reason (Key.string_of_key key) refi)

(* run passed function with given entry added to scope *)
(* CAUTION: caller must ensure that name isn't already bound,
   otherwise this will remove the preexisting binding *)
(* also note that this function does not push new frame
   or changeset *)
let let_env name entry f =
  let scope = peek_scope () in
  Scope.add_entry name entry scope;
  f ();
  Scope.remove_entry name scope

(* The protocol around havoc has changed a few times.
   The following function used to do most of the work, but is now subsumed by
   havoc_ctx, and is now used only to clear the environment when looking at
   a function body. Also see below. *)

(* clear refinement informnation for all binding entries in env *)
let havoc_all () =
  iter_scopes (Scope.havoc ~make_specific:(fun general -> general))

(* clear refinement info for (topmost bindings of) given names in env *)
let havoc_vars =

  (* clear specific info for (topmost binding of) given var in env *)
  let havoc_var name =
    let rec loop = function
    | [] -> ()
    | scope :: scopes ->
      match Scope.get_entry name scope with
      | Some entry ->
        let entry = Entry.havoc (fun gen -> gen) name entry in
        Scope.add_entry name entry scope
      | None ->
        loop scopes
    in loop !scopes
  in

  (* clear refinement for (topmost binding of) given key in env *)
  let havoc_refi key =
    let rec loop = function
    | [] -> ()
    | scope :: scopes ->
      match Scope.get_refi key scope with
      | Some binding ->
        Scope.remove_refi key scope
      | None ->
        loop scopes
    in loop !scopes
  in

  fun (vars, refis) ->
    vars |> SSet.iter havoc_var;
    refis |> KeySet.iter havoc_refi

(* Clear entries for heap refinement pseudovars in env.
   If name is passed, clear only those refis that depend on it.
   Real variables are left untouched.
 *)
let havoc_heap_refinements () =
  iter_scopes Scope.havoc

let havoc_heap_refinements_with_propname name =
  iter_scopes (Scope.havoc ~name)

(* set specific type of every non-internal var in top scope to undefined,
   and clear heap refinements.
   TODO rename *)
let clear_env reason =
  Scope.havoc (peek_scope ()) ~make_specific:(fun _ -> UndefT reason)

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
 *)
let add_heap_refinement cx key reason refined original =
  let scope = peek_scope () in
  ignore (add_change_refi key);
  let refi_loc = Some (loc_of_reason reason) in
  let refi = { refi_loc; refined; original } in
  Scope.add_refi key refi scope

(* add predicate refinements from given preds map to environment.
   note: orig_types maps names to unrefined types. this param is
   only necessary for fresh pseudovars like heap refinements -
   others can be obtained via get_var.
 *)
let refine_with_preds cx reason preds orig_types =

  let mk_refi_type orig_type pred refi_reason =
    Flow_js.mk_tvar_where cx refi_reason (fun refi_type ->
      let pred_type = PredicateT (pred, refi_type) in
      Flow_js.flow cx (orig_type, pred_type))
  in

  let refine_with_pred key pred =
    match key with
    (* for real consts/lets/vars, we model assignment/initialization *)
    | name, [] when not (is_internal_name name) ->
      let refi_reason =
        let pred_str = string_of_predicate pred in
        let rstr = spf "identifier %s when %s" name pred_str in
        replace_reason rstr reason
      in
      Entry.(match find_entry cx name reason with
      | _, Value v ->
        let orig_type =
          let get_reason = replace_reason (spf "identifier %s" name) reason in
          get_var cx name get_reason
        in
        let refi_type = mk_refi_type orig_type pred refi_reason in
        (match v.kind with
          Const -> refine_const | _ -> set_var
        ) cx name refi_type refi_reason
      | _, entry ->
        assert_false (
          spf "attempt to refine %s %s" (Entry.string_of_kind entry) name)
      )

    (* for heap refinements, we just add new entries *)
    | _ ->
      let refi_reason =
        let pred_str = string_of_predicate pred in
        let rstr = spf "expression %s when %s"
          (Key.string_of_key key) pred_str in
        replace_reason rstr reason
      in
      let orig_type = KeyMap.find_unsafe key orig_types in
      let refi_type = mk_refi_type orig_type pred refi_reason in
      add_heap_refinement cx key refi_reason refi_type orig_type

  in

  preds |> KeyMap.iter refine_with_pred

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
