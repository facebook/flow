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
open Type
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

(* the environment is a scope stack, which mutates as an AST is
   traversed. changesets are also managed here, but live in
   a separate Changeset module for dependency reasons.
 *)
type 'a stack = 'a list ref
let scopes: Scope.t stack = ref []

(* symbols whose bindings are forcibly prevented from being created,
   initialized, etc. This set is initialized in init_env, and is normally
   empty. It's used to implement library overrides: suppressing a local
   binding to definition D means that any local reference to D will
   register it as a deferred global lookup, which will then be linked
   to the override. See Init_js.load_lib_files.
 *)
let exclude_symbols: SSet.t ref = ref SSet.empty

let set_exclude_symbols cx syms =
  exclude_symbols := syms

let is_excluded name =
  SSet.mem name !exclude_symbols

(* scopes *)

(* return the current scope *)
let peek_scope () =
  List.hd !scopes

(* return current scope stack *)
let peek_env () =
  !scopes

let string_of_env cx env =
  spf "[ %s ]" (String.concat ";\n"
    (List.map (Debug_js.string_of_scope cx) env))

(* return the value of f applied to topmost var scope in a scope list *)
let rec top_var_scope = function
| [] -> assert_false "empty scope list"
| scope :: scopes ->
  match scope.kind with
  | VarScope _ -> scope
  | _ -> top_var_scope scopes

(* get top var scope of current env *)
let peek_var_scope () =
  top_var_scope (peek_env ())

(* each varscope carries a frame id *)
let peek_frame () =
  (peek_scope ()).id

(* use the passed f to iterate over all scopes *)
let iter_scopes f =
  List.iter f !scopes

(* apply function f to local scopes: the 0 or more
   lex scopes between us and the closest var scope,
   and also that var scope *)
let iter_local_scopes f =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope::scopes ->
        f scope;
        (match scope.kind with
        | LexScope -> loop scopes
        | _ -> ())
  in
  loop !scopes

(* clone the given scope stack (snapshots entry maps) *)
let clone_env scopes =
  List.map Scope.clone scopes

(* true iff scope is var scope with the given kind *)
let is_func_kind k scope =
  match scope.kind with
  | VarScope func_kind -> func_kind = k
  | _ -> false

let in_async_scope () =
  is_func_kind Async (peek_var_scope ())

let in_generator_scope () =
  is_func_kind Generator (peek_var_scope ())

(* build a map of all var entries - no refis - in the current
   scope stack.
   Note that we accumulate entries bottom-up, so that
   shadowing is properly maintained *)
let all_entries () =
  List.fold_left (fun entries scope ->
    SMap.union scope.entries entries
  ) SMap.empty (List.rev !scopes)

(* whole env *)

(* clear environment *)
let havoc_current_activation () =
  scopes := [];
  Changeset.init ()

(* save environment to context *)
let snapshot_env cx =
  let scopes = peek_env () in
  let id = (List.hd scopes).id in
  Context.add_env cx id scopes

(* push a new var scope into the environment.
   current env state is stored to cx under scope id *)
(* TODO maintain changelist here too *)
let push_var_scope cx scope =
  (match scope.kind with
    | VarScope _ -> ()
    | _ -> assert_false "push_var_scope on non-var scope");
  scopes := scope :: !scopes;
  Changeset.push ();
  snapshot_env cx

(***
 * when we pop a var scope, we save the accumulated changeset over the
 * surrounding environment. for var scopes that correspond to function
 * activations, this saved changeset contains the read write and
 * refinement operations on closed-over variables.
 *
 * We make this available out of band rather than simply returning it
 * from pop_var_scope only to avoid the current logistics of getting it
 * to where the function's type is actually built, but this may change.
 * It would certainly be better to avoid the extra state.
 *)

let saved_closure_changeset = ref (Some Changeset.empty)

let save_closure_changeset scopes =
  let ids = List.map (fun { id; _ } -> id) scopes in
  let changeset = Changeset.(include_scopes ids (peek ())) in
  saved_closure_changeset := Some changeset

let retrieve_closure_changeset () =
  match !saved_closure_changeset with
  | None -> assert_false "no saved closure changeset"
  | Some changeset ->
    saved_closure_changeset := None;
    changeset

(* --- *)

(* pop a var scope from the environment.
   note: may require popping accumulated lex scopes *)
let pop_var_scope () =
  match !scopes with
  | { kind = VarScope _; _ } :: tail_scopes ->
    save_closure_changeset tail_scopes;
    scopes := tail_scopes;
    Changeset.pop ()
  | [] -> assert_false "empty scope list"
  | _ -> assert_false "top scope is non-var"

(* push a lex scope but NOT a changeset
   (which is 1-1 with var scopes). *)
let push_lex_scope cx =
  let scope = Scope.fresh_lex () in
  scopes := scope :: !scopes;
  snapshot_env cx

let pop_lex_scope () =
  match !scopes with
  | { kind = LexScope; id; _ } :: tail_scopes ->
    (* cull any changelist entries for this scope *)
    ignore (Changeset.filter_scope_changes id);
    (* pop *)
    scopes := tail_scopes
  | [] -> assert_false "empty scope list"
  | _ -> assert_false "top scope is non-lex"

let in_lex_scope cx f =
  push_lex_scope cx;
  let result = f () in
  pop_lex_scope ();
  result

(* depth of current env *)
let env_depth () =
  List.length !scopes

(* strip the given number of scopes from top of env *)
let trunc_env =
  let rec trunc = function
  | 0, scopes -> scopes
  | _, [] -> assert_false "trunc_env: scopes underflow"
  | n, scope :: scopes ->
    ignore (Changeset.filter_scope_changes scope.id);
    trunc (n - 1, scopes)
  in
  fun depth ->
    let cur = !scopes in
    scopes := trunc (List.length cur - depth, cur)

(* initialize a new environment (once per module) *)
let init_env ?(exclude_syms=SSet.empty) cx module_scope =
  set_exclude_symbols cx exclude_syms;
  havoc_current_activation ();
  let global_scope = Scope.fresh ~var_scope_kind:Global () in
  push_var_scope cx global_scope;
  push_var_scope cx module_scope

(* replace the current env with the passed one.
   envs must be congruent - we measure length as a quick check,
   with a more thorough check on env merge/copy *)
let update_env cx reason new_scopes =

  (if List.length new_scopes != List.length (peek_env ())
  then assert_false (spf
    "update_env %s: unequal length scope lists, old %d new %d "
    (string_of_reason reason)
    (List.length new_scopes)
    (List.length (peek_env ()))));

  scopes := new_scopes;
  snapshot_env cx

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
   of the GetPropT types created in the call to Flow_js.get_builtin.

   This means that we have some false negatives, currently.
   Errors that go unreported currently include:
   - type aliases referred to from value positions
   - global consts assigned to locally
   The first is especially problematic, since it will actually
   cause a runtime error.

   The least complex solution to this will be to process libs
   eagerly, and save the actual scope to check against here
   when doing local checking of modules. Alternatively, we
   would have to record in the GetPropT (or an enrichment) enough
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
  let loc = loc_of_reason reason in
  let entry = Entry.new_var t ~loc ~state:State.Initialized in
  Scope.add_entry name entry global_scope;
  Context.add_global cx name;
  global_scope, entry

let local_scope_entry_exists cx name =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope::scopes ->
        match Scope.get_entry name scope with
        | Some entry -> true
        | None ->
            match scopes with
            | [] -> false
            | _ -> loop scopes
  in
  loop !scopes

(* Look for scope that holds binding for a given name. If found,
   return scope and entry. Note that anything we don't resolve
   otherwise, we add to the global scope after generating a
   deferred lookup, which may fail later. *)
let find_entry cx name reason =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope::scopes ->
      match Scope.get_entry name scope with
      | Some entry -> scope, entry
      | None ->
        (* keep looking until we're at the global scope *)
        match scopes with
        | [] -> cache_global cx name reason scope
        | _ -> loop scopes
  in
  loop !scopes

(* Search for the scope which binds the given name, through the
   topmost LexScopes and up to the first VarScope. If the entry
   is not found, return the VarScope where we terminated. *)
let find_entry_in_var_scope name =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope::scopes ->
        match Scope.get_entry name scope, scope.kind with
        | Some entry, _ -> scope, Some entry
        | None, VarScope _ -> scope, None
        | None, LexScope -> loop scopes
  in
  loop !scopes

(* Search for the scope which holds the given refinement, through
   the topmost LexScopes and up to the first VarScope. If the
   entry is not found, return None. *)
let find_refi_in_var_scope key =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope::scopes ->
        match Scope.get_refi key scope, scope.kind with
        | Some refi, _ -> Some (scope, refi)
        | None, VarScope _ -> None
        | None, LexScope -> loop scopes
  in
  loop !scopes

(* helpers *)
let entry_reason name entry =
  mk_reason
    (spf "%s %s" (Entry.string_of_kind entry) name)
    (Entry.loc entry)

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

let bind_entry cx name entry reason =
  (* iterate top-down through scopes until the appropriate scope for this
     binding is found, or realize a binding error *)
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope::scopes -> Scope.(
      match get_entry name scope with

      (* if no entry already exists, this might be our scope *)
      | None -> Entry.(
        match scope.Scope.kind, entry with
        (* lex scopes can only hold let/const bindings *)
        (* var scope can hold all binding types *)
        | LexScope, Value { kind = Let _; _ }
        | LexScope, Value { kind = Const; _ }
        | VarScope _, _ ->
          add_entry name entry scope
        (* otherwise, keep looking for our scope *)
        | _ -> loop scopes)

      (* some rebindings are allowed, but usually an error *)
      | Some prev ->
        match scope.kind with

        (* specifically a var scope allows some shadowing *)
        | VarScope _ -> Entry.(
          let can_shadow = function
            (* funcs/vars can shadow other funcs/vars -- only in var scope *)
            | (Var | Let (Some FunctionBinding)),
              (Var | Let (Some FunctionBinding)) -> true
            (* vars can shadow function params *)
            | Var, Let (Some ParamBinding) -> true
            | _ -> false
          in
          match entry, prev with
          (* good shadowing leaves existing entry, unifies with new *)
          | Value e, Value p when can_shadow (e.kind, p.kind) ->
            (* TODO currently we don't step on specific. shouldn't we? *)
            Flow_js.unify cx p.general e.general
          (* bad shadowing is a binding error *)
          | _ -> already_bound_error cx name prev reason)

        (* shadowing in a lex scope is always an error *)
        | LexScope -> already_bound_error cx name prev reason)
  in
  if not (is_excluded name) then loop !scopes

(* bind var entry *)
let bind_var ?(state=State.Declared) cx name t r =
  let loc = loc_of_reason r in
  bind_entry cx name (Entry.new_var t ~loc ~state) r

(* bind let entry *)
let bind_let ?(state=State.Undeclared) cx name t r =
  let loc = loc_of_reason r in
  bind_entry cx name (Entry.new_let t ~loc ~state) r

(* bind implicit let entry *)
let bind_implicit_let ?(state=State.Undeclared) implicit cx name t r =
  let loc = loc_of_reason r in
  bind_entry cx name (Entry.new_let t ~implicit ~loc ~state) r

let bind_fun ?(state=State.Declared) =
  bind_implicit_let ~state Entry.FunctionBinding

(* bind const entry *)
let bind_const ?(state=State.Undeclared) cx name t r =
  let loc = loc_of_reason r in
  bind_entry cx name (Entry.new_const t ~loc ~state) r

(* bind type entry *)
let bind_type ?(state=State.Declared) cx name t r =
  let loc = loc_of_reason r in
  bind_entry cx name (Entry.new_type t ~loc ~state) r

(* vars coming from 'declare' statements are preinitialized *)
let bind_declare_var = bind_var ~state:State.Initialized

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
    if not (is_excluded name)
    then (
      let scope = peek_scope () in
      match Scope.get_entry name scope with
      | None ->
        let entry =
          Entry.new_var t ~loc:(loc_of_reason reason) ~state:State.Initialized
        in
        Scope.add_entry name entry scope

      | Some prev ->
        Entry.(match prev with

        | Value v when v.kind = Var ->
          let entry = Value { v with
            value_state = State.Initialized;
            specific = update_type v.specific t;
            general = update_type v.general t
          } in
          Scope.add_entry name entry scope

        | _ ->
          (* declare function shadows some other kind of binding *)
          already_bound_error cx name prev reason
        )
    )

(* helper: move a Let/Const's entry's state from Undeclared to Declared.
   Only needed for let and const to push things into scope for potentially
   recursive internal refs: hoisted things (vars and types) become declared
   immediately on binding.
 *)
let declare_value_entry kind cx name reason =
  if not (is_excluded name)
  then Entry.(
    let scope, entry = find_entry cx name reason in
    match entry with
    | Value v when v.kind = kind && v.value_state = State.Undeclared ->
      let new_entry = Value { v with value_state = State.Declared } in
      Scope.add_entry name new_entry scope
    | _ ->
      already_bound_error cx name entry reason
  )

let declare_let = declare_value_entry (Entry.Let None)
let declare_implicit_let implicit =
  declare_value_entry (Entry.Let (Some implicit))
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
  if not (is_excluded name)
  then Entry.(
    let scope, entry = find_entry cx name reason in
    match kind, entry with
    | Var, Value ({ kind = Var; _ } as v)
    | Let _, Value ({ kind = Let _;
        value_state = State.Undeclared | State.Declared; _ } as v)
    | Const, Value ({ kind = Const;
        value_state = State.Undeclared | State.Declared; _ } as v) ->
      Changeset.(add_var_change (scope.id, name, Write));
      Flow_js.flow cx (specific, v.general);
      let value_binding = { v with value_state = State.Initialized; specific } in
      (* if binding is annotated, flow annotated type like initializer *)
      let new_entry = Value (
        if has_anno
        then force_general_type cx value_binding
        else value_binding
      ) in
      Scope.add_entry name new_entry scope;
    | _ ->
      (* Incompatible or non-redeclarable new and previous entries.
         We will have already issued an error in `bind_value_entry`,
         so we can prune this case here. *)
      ()
    )

let init_var = init_value_entry Entry.Var
let init_let = init_value_entry (Entry.Let None)
let init_implicit_let implicit = init_value_entry (Entry.Let (Some implicit))
let init_fun = init_implicit_let ~has_anno:false Entry.FunctionBinding
let init_const = init_value_entry Entry.Const

(* update type alias to reflect initialization in code *)
let init_type cx name _type reason =
  if not (is_excluded name)
  then Entry.(
    let scope, entry = find_entry cx name reason in
    match entry with
    | Type ({ type_state = State.Declared; _ } as t)->
      Flow_js.flow cx (_type, t._type);
      let new_entry = Type { t with type_state = State.Initialized; _type } in
      Scope.add_entry name new_entry scope
    | _ ->
      (* Incompatible or non-redeclarable new and previous entries.
         We will have already issued an error in `bind_value_entry`,
         so we can prune this case here. *)
      ()
    )

(* used to enforce annotations: see commentary in force_general_type *)
let pseudo_init_declared_type cx name reason =
  if not (is_excluded name)
  then Entry.(
    let scope, entry = find_entry cx name reason in
    match entry with
    | Value value_binding ->
      let value_binding =
        { value_binding with value_state = State.Declared } in
      let entry = Value (force_general_type cx value_binding) in
      Scope.add_entry name entry scope
    | Type _ ->
      assert_false (spf "pseudo_init_declared_type %s: Type entry" name)
  )

(* helper for read/write tdz checks *)
(* for now, we only enforce TDZ within the same activation.
   return true if the given target scope is in the same
   activation as the current scope.
 *)
let same_activation target =
  let rec loop target = function
  | [] -> assert_false "target scope not found"
  | scope :: scopes when scope.id = target.id ->
    (* target is nearer than (or actually is) nearest VarScope *)
    true
  | scope :: scopes ->
    match scope.kind with
    | VarScope _ ->
      (* found var scope before target *)
      false
    | LexScope ->
      (* still in inner lex scopes, keep looking *)
      loop target scopes
  in
  (* search outward for target scope *)
  loop target (peek_env ())

(* get types from value entry, does uninitialized -> undefined behavior *)
let value_entry_types ?(lookup_mode=ForValue) scope = Entry.(function
  (* from value positions, a same-activation ref to var or an explicit let
     before initialization yields undefined. *)
| { kind = Var | Let None;
    value_state = State.Declared | State.MaybeInitialized as state;
    value_declare_loc; specific; general; _ }
    when lookup_mode = ForValue && same_activation scope
    ->
    let uninit desc = VoidT.make (mk_reason desc value_declare_loc) in
    let specific = if state = State.Declared
      then uninit "uninitialized variable"
      else (* State.MaybeInitialized *)
        let desc = "possibly uninitialized variable" in
        let ts = [uninit desc; specific] in
        UnionT (mk_reason desc value_declare_loc, ts)
    in
    specific, general

| { specific; general; _ } ->
  specific, general
)

(* emit tdz error for value entry *)
let tdz_error cx name reason v = Entry.(
  (* second clause of error message is due to switch scopes *)
  let msg = spf "%s referenced before declaration, \
                  or after skipped declaration"
    (string_of_value_kind v.Entry.kind) in
  binding_error msg cx name (Value v) reason
)

(* helper for read/write tdz checks *)
(* functions are block-scoped, but also hoisted. forward ref ok *)
let allow_forward_ref = Scope.Entry.(function
  | Var | Let (Some FunctionBinding) -> true
  | _ -> false
)

(* helper - does semantic checking and returns entry type *)
let read_entry ~lookup_mode ~specific cx name reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match entry with

  | Type { type_loc; _ } when lookup_mode != ForType ->
    let msg = "type referenced from value position" in
    binding_error msg cx name entry reason;
    AnyT.at (Entry.loc entry)

  | Type t ->
    t._type

  | Value v ->
    match v with
    | { kind; value_state = State.Undeclared; value_declare_loc; _ }
      when lookup_mode = ForValue && not (allow_forward_ref kind)
      && same_activation scope ->
      tdz_error cx name reason v;
      AnyT.at value_declare_loc
    | _ ->
      Changeset.(add_var_change (scope.id, name, Read));
      let s, g = value_entry_types ~lookup_mode scope v in
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
(* Note that we reach into UnionT here, specifically to handle unions
   returned from value_entry_types for MaybeInitialized entries. Should
   probably be the standard action of ReposLowerT, at which point this
   can be removed. TODO
 *)
let var_ref ?(lookup_mode=ForValue) cx name reason =
  let repos = Flow_js.reposition cx reason in
  match get_var ~lookup_mode cx name reason with
  | UnionT (r, ts) -> repos (UnionT (r, List.map repos ts))
  | t -> repos t

(* get refinement entry *)
let get_refinement cx key reason =
  match find_refi_in_var_scope key with
  | Some (_, { refined; _ }) -> Some (Flow_js.reposition cx reason refined)
  | _ -> None

(* helper: update let or var entry *)
let update_var op cx name specific reason =
  let scope, entry = find_entry cx name reason in
  let value_assign_loc = loc_of_reason reason in
  Entry.(match entry with

  | Value ({ kind = (Let _ as kind); value_state = State.Undeclared; _ } as v)
    when not (allow_forward_ref kind) && same_activation scope ->
    tdz_error cx name reason v

  | Value ({ kind = Let _ | Var; _ } as v) ->
    Changeset.(add_var_change (scope.id, name, op));

    Flow_js.flow cx (specific, v.general);
    (* add updated entry *)
    let update = Entry.Value {
      v with
      value_state = State.Initialized;
      specific;
      value_assign_loc;
    } in

    Scope.add_entry name update scope

  | Value { kind = Const; _ } ->
    let msg = "const cannot be reassigned" in
    binding_error msg cx name entry reason

  | Type _ ->
    let msg = "type alias referenced from value position" in
    binding_error msg cx name entry reason
  )

(* update var by direct assignment *)
let set_var = update_var Changeset.Write

(* update var by refinement test *)
let refine_var = update_var Changeset.Refine

(* set const's specific type to reflect a refinement test (internal) *)
let refine_const cx name specific reason =
  let scope, entry = find_entry cx name reason in
  Entry.(match entry with

  | Value v when v.kind = Const ->
    Changeset.(add_var_change (scope.id, name, Refine));
    Flow_js.flow cx (specific, v.general);
    let update = Entry.Value {
      v with value_state = State.Initialized; specific
    } in
    Scope.add_entry name update scope

  | _ ->
    assert_false (spf "refine_const called on %s %s"
      (string_of_kind entry) name)
  )

(* given a list of envs (scope lists), return true iff all envs are
   the same length and all scope ids and kinds match *)
let envs_congruent envs =
  let rec check_scopes envs =
    let env0 = List.hd envs in
    env0 = [] ||
      let scope0 = List.hd env0 in
      let check_scope env =
        let scope = List.hd env in
        scope.id = scope0.id && scope.kind = scope0.kind
      in
      List.for_all check_scope (List.tl envs) &&
        check_scopes (List.map List.tl envs)
  in
  List.length envs <= 1 ||
    let len = List.length (List.hd envs) in
    List.for_all (fun env -> List.length env = len) (List.tl envs) &&
      check_scopes envs

(* find scopes with a given id in a list of envs.
   envs are assumed congruent amd assumed to contain scope id *)
let rec find_scope cx reason envs scope_id =
  match envs with
  | (scope0 :: tail0) :: _ ->
    if scope0.id = scope_id
    then List.(map hd envs)
    else find_scope cx reason List.(map tl envs) scope_id
  | _ ->
    assert_false (spf "find_scopes %s: scope %d not found. head env %s"
      (string_of_reason reason) scope_id
      (string_of_env cx (List.hd envs)))

(* The following function takes a changset and a triple of environments -
   original and two derivations - and merges the bindings indicated by
 changeset keys from the derivations into the original. *)
let merge_env =

  (* find sscope triple in env triple *)
  let find_scope_triple cx reason (env0, env1, env2) id =
    let lst = find_scope cx reason [env0; env1; env2] id in
    List.(nth lst 0, nth lst 1, nth lst 2)
  in

  let create_union cx reason l1 l2 =
    Flow_js.mk_tvar_where cx reason (fun tvar ->
      Flow_js.flow cx (l1, tvar);
      Flow_js.flow cx (l2, tvar);
    )
  in

  (* merge_entry helper - child specifics feed new specific tvar *)
  let merge_types cx reason name (specific0, general0) specific1 specific2 =
    (* no change *)
    if specific0 = specific1 && specific0 = specific2
    then specific0, general0
    (* child has reverted to original - shortcut *)
    else if specific1 = general0 || specific2 = general0
    then general0, general0
    (* general case *)
    else
      let reason = replace_reason name reason in
      let tvar = create_union cx reason specific1 specific2 in
      Flow_js.flow cx (tvar, general0);
      tvar, general0
  in

  (* propagate var state updates from child entries *)
  let merge_states orig child1 child2 = Entry.(
    match orig.Entry.kind with
    | Var | Let _
      when
        child1.value_state = State.Initialized &&
        child2.value_state = State.Initialized ->
      (* if both branches have initialized, we can set parent state *)
      State.Initialized
    | Var | Let _
      when
        child1.value_state >= State.Declared &&
        child2.value_state >= State.Declared &&
        (child1.value_state >= State.MaybeInitialized ||
         child2.value_state >= State.MaybeInitialized) ->
      (* if either branch has initialized, we can set parent state *)
      State.MaybeInitialized
    | _ -> orig.value_state
  ) in

  let merge_entry cx reason envs ((scope_id, name, _) as entry_ref) =
    let scope0, scope1, scope2 = find_scope_triple cx reason envs scope_id in
    let get = get_entry name in
    Entry.(match get scope0, get scope1, get scope2 with
    (* merge child var and let types back to original *)
    | Some Value orig, Some Value child1, Some Value child2 ->
      let { specific = s0; general = g0; _ } = orig in
      let { specific = s1; _ } = child1 in
      let { specific = s2; _ } = child2 in
      let specific, general = merge_types cx reason name (s0, g0) s1 s2 in
      let value_state = merge_states orig child1 child2 in
      let e = Entry.(Value { orig with specific; general; value_state }) in
      add_entry name e scope0
    (* type aliases can't be refined or reassigned, shouldn't be here *)
    | Some Type _, Some Type _, Some Type _ ->
      assert_false (spf "merge_env %s: type alias %s found in changelist"
        (string_of_reason reason) name)
    (* global lookups may leave uneven new entries, which we can forget *)
    | _, _, _ when is_global scope0 ->
      ()
    (* missing completely from non-global scope *)
    | None, None, None ->
      assert_false (spf
        "%d merge_entry %s %s: missing from scopes:\n%s\n%s\n%s"
        (Unix.getpid ())
        (string_of_reason reason)
        (Changeset.string_of_entry_ref entry_ref)
        (Debug_js.string_of_scope cx scope0)
        (Debug_js.string_of_scope cx scope1)
        (Debug_js.string_of_scope cx scope2))
    (* a newly created entry may exist in one lex child -
       this pattern is due to our current switch handling *)
    | None, Some (Value _  as entry), None when Scope.is_lex scope1 ->
      add_entry name entry scope0
    | None, None, Some (Value _  as entry) when Scope.is_lex scope2 ->
      add_entry name entry scope0
    (* otherwise, non-refinement uneven distributions are asserts. *)
    | orig, child1, child2 ->
      let print_entry_kind_opt = function
      | None -> "None"
      | Some e -> spf "Some %s" Entry.(string_of_kind e)
      in assert_false (spf
        "merge_env %s: non-uniform distribution of entry %s: %s, %s, %s"
        (string_of_reason reason)
        name
        (print_entry_kind_opt orig)
        (print_entry_kind_opt child1)
        (print_entry_kind_opt child2))
  ) in

  let merge_refi cx reason envs (scope_id, key, _) =
    let scope0, scope1, scope2 = find_scope_triple cx reason envs scope_id in
    let get = get_refi key in
    match get scope0, get scope1, get scope2 with
    (* evenly distributed refinements are merged *)
    | Some base, Some child1, Some child2 ->
      let name = Key.string_of_key key in
      let refined, original = merge_types cx reason name
        (base.refined, base.original) child1.refined child2.refined in
      let refi = { base with refined; original } in
      add_refi key refi scope0

    (* refi was introduced in both children *)
    | None, Some child1, Some child2 ->
      let name = Key.string_of_key key in
      let reason = replace_reason name reason in
      let refined = create_union cx reason child1.refined child2.refined in
      let original = create_union cx reason child1.original child2.original in
      let refi_loc = loc_of_reason reason in
      let refi = { refi_loc; refined; original } in
      add_refi key refi scope0

    (* refi was cleared in a child env. clear from original *)
    | Some _, _, _ ->
      remove_refi key scope0
    (* refi was introduced - and possibly also removed by havoc -
      after envs diverged *)
    | None, _, _ ->
      ()
  in

  (* merge entries and refis found in changeset *)
  fun cx reason (env0, env1, env2) changeset ->
    (if envs_congruent [env0; env1; env2] then ()
    else assert_false (spf "merge_env %s: envs not congruent"
      (string_of_reason reason)));
    changeset |> Changeset.iter_type_updates
      (merge_entry cx reason (env0, env1, env2))
      (merge_refi cx reason (env0, env1, env2))

(* copy changes from env2 into env1 *)
let copy_env =

  (* find sscope pair in env pair *)
  let find_scope_pair cx reason (env0, env1) id =
    let lst = find_scope cx reason [env0; env1] id in
    List.(nth lst 0, nth lst 1)
  in

  (* look for and copy entry, starting in topmost scope *)
  let copy_entry cx reason envs (scope_id, name, _) =

    let scope1, scope2 = find_scope_pair cx reason envs scope_id in
    let get = get_entry name in
    Entry.(match get scope1, get scope2 with
    (* for values, flow env2's specific type into env1's specific type *)
    | Some Value v1, Some Value v2 ->
      (* flow child2's specific type to child1 in place *)
      Flow_js.flow cx (v2.specific, v1.specific);
      (* udpate state *)
      if v1.value_state < State.Initialized
        && v2.value_state >= State.MaybeInitialized
      then (
        let new_entry = Value {
          v1 with value_state = State.MaybeInitialized
        } in
        add_entry name new_entry scope1
      )

    (* type aliases shouldn't be here *)
    | Some Type _, Some Type _ ->
      assert_false (spf "copy_env %s: type alias %s found in changelist"
        (string_of_reason reason) name)

    (* global lookups may leave new entries in env2, or orphan changes *)
    (* ...which we can forget *)
    | None, _ when is_global scope1 ->
      ()

    (* changeset entry exists only in lex scope *)
    | Some Value _, None when Scope.is_lex scope1 ->
      ()
    | None, Some (Value _ as entry) when Scope.is_lex scope2 ->
      add_entry name entry scope1

    (* uneven distributions *)
    | entry1, entry2 ->
      let print_entry_kind_opt = function
      | None -> "None"
      | Some e -> spf "Some %s" (Entry.string_of_kind e)
      in assert_false (spf
        "copy_env %s: non-uniform distribution of entry %s: %s, %s"
        (string_of_reason reason)
        name
        (print_entry_kind_opt entry1)
        (print_entry_kind_opt entry2))
  ) in

  (* look for and copy refinement in top scope only *)
  let copy_refi cx reason envs (scope_id, key, _) =
    let scope0, scope1 = find_scope_pair cx reason envs scope_id in
    let get = get_refi key in
    match get scope0, get scope1 with
    (* flow child refi's type back to parent *)
    | Some { refined = t1; _ }, Some { refined = t2; _ } ->
      Flow_js.flow cx (t2, t1)
    (* uneven cases imply refi was added after splitting: remove *)
    | _ ->
      ()
  in

  (* copy entries and refis bound to names and keys, respectively *)
  fun cx reason (env1, env2) changeset ->
    (if envs_congruent [env1; env2] then ()
    else assert_false (spf "copy_env %s: envs not congruent"
      (string_of_reason reason)));
    changeset |> Changeset.iter_type_updates
      (copy_entry cx reason (env1, env2))
      (copy_refi cx reason (env1, env2))

(* in the top scope, convert specific types to tvars with former
   specific type as incoming lower bound, and general type as
   upper bound. This prepares the specific type for later merging
   during path-dependent analysis.
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
    | Some specific -> { var with Entry.specific }
  in

  let widen_refi cx reason name ({ refined; original; _ } as refi) =
    match widened cx reason name refined original with
    | None -> refi
    | Some refined -> { refi with refined }
  in

  fun cx reason ->
    iter_local_scopes (fun scope ->
      scope |> Scope.update_entries Entry.(fun name -> function
        | Value var -> Value (widen_var cx reason name var)
        | entry -> entry
      );
      scope |> Scope.update_refis (fun key refi ->
        widen_refi cx reason (Key.string_of_key key) refi)
    )

(* The protocol around havoc has changed a few times.
   The following function used to do most of the work, but is now subsumed by
   havoc_ctx, and is now used only to clear the environment when looking at
   a function body. Also see below. *)

(* clear refinement informnation for all binding entries in env *)
let havoc_all () =
  iter_scopes (Scope.havoc ~make_specific:(fun general -> general))

(* set specific type of every non-internal var *and const*
   in top activation to undefined, and clear heap refinements.
   Note: this operation is needed by our control flow handling, which is
   left over from the earlier, looser model. To properly simulate early exits,
   all entries including consts must have their specific entries set to EmptyT.
   TODO rework the early-exit stuff to not break invariants. Until then it'll
   remain a source of bugs.
 *)
let reset_current_activation reason =
  iter_local_scopes (
    Scope.havoc ~consts:true ~make_specific:(fun _ -> EmptyT reason))

(* clear refinement info for (topmost bindings of) given names in env *)
let havoc_vars = Scope.(

  (* clear specific info for (topmost binding of) given var in env *)
  let havoc_entry (scope_id, name, _) =
    let rec loop = function
    | [] -> ()
    | scope :: scopes ->
      match get_entry name scope with
      | Some entry ->
        let entry = Entry.havoc (fun gen -> gen) name entry in
        add_entry name entry scope
      | None ->
        loop scopes
    in loop !scopes
  in

  (* clear refinement for (topmost binding of) given key in env *)
  let havoc_refi (scope_id, key, _) =
    let rec loop = function
    | [] -> ()
    | scope :: scopes ->
      match get_refi key scope with
      | Some binding ->
        remove_refi key scope
      | None ->
        loop scopes
    in loop !scopes
  in

  Changeset.iter_type_updates havoc_entry havoc_refi
)

(* Clear entries for heap refinement pseudovars in env.
   If name is passed, clear only those refis that depend on it.
   Real variables are left untouched.
 *)
let havoc_heap_refinements () =
  iter_scopes Scope.havoc_refis

let havoc_heap_refinements_with_propname name =
  iter_scopes (Scope.havoc_refis ~name)

(* The following functions are used to narrow the type of variables
   based on dynamic checks. *)

(* Directly refine an expression's type to t. The refinement is
   installed into the same scope where the object is bound. A
   refinement may already be present in this scope - if so, we
   overwrite it.

   However note that we do so at a low level, rather than by using the
   standard set_var mechanism. This is to avoid unwanted reentrancy:
   set_var calls flow (specific, general) to model the effect of a
   variable assignment in user code, and this in certain cases
   (broadly, assignment of function values) may provoke havoc_ctx
   into clearing the refinement we're in the process of installing.
 *)
let add_heap_refinement op cx key reason refined original =
  let refi_loc = loc_of_reason reason in
  let refi = { refi_loc; refined; original } in
  let base, _ = key in
  let scope, _ = find_entry_in_var_scope base in
  ignore Changeset.(add_refi_change (scope.id, key, op));
  Scope.add_refi key refi scope

let set_expr = add_heap_refinement Changeset.Write
let refine_expr = add_heap_refinement Changeset.Refine

(* add predicate refinements from given preds map to environment.
   note: orig_types maps names to unrefined types. this param is
   only necessary for fresh pseudovars like heap refinements -
   others can be obtained via get_var.
 *)
let refine_with_preds cx reason preds orig_types =

  let mk_refi_type orig_type pred refi_reason =
    Flow_js.mk_tvar_where cx refi_reason (fun refined_type ->
      Flow_js.flow cx (orig_type, PredicateT (pred, refined_type)))
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
        let refine = match v.kind with
          | Const -> refine_const
          | _ -> refine_var
        in
        refine cx name refi_type refi_reason
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
      refine_expr cx key refi_reason refi_type orig_type

  in
  preds |> KeyMap.iter refine_with_pred

(* run the given function in a clone of the current environment
   augmented by the given refinement map, then merge the final
   state of the cloned environment back into the reinstated
   original *)
let in_refined_env cx reason preds orig_types f =
  let oldset = Changeset.clear () in
  let orig_env = peek_env () in
  let new_env = clone_env orig_env in
  update_env cx reason new_env;
  refine_with_preds cx reason preds orig_types;

  let result = f () in

  let newset = Changeset.merge oldset in
  merge_env cx reason (orig_env, orig_env, new_env) newset;
  update_env cx reason orig_env;

  result
