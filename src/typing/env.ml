(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module describes the representation of lexical environments and defines
   various operations on them, including "stack" operations to push/pop scopes,
   and "lookup" operations to find, read, and write variables and their
   associated type information. *)

open Utils_js
open Loc_collections
open Type
open TypeUtil
open Reason
open Scope
module Flow = Flow_js
module Scope_api = Scope_api.With_ALoc

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
  type t =
    | ForValue
    | ForType
    | ForTypeof
end

open LookupMode

(****************)
(* Environment *)
(****************)

type t = Scope.t list

(* the environment is a scope stack, which mutates as an AST is
   traversed. changesets are also managed here, but live in
   a separate Changeset module for dependency reasons.
*)
let scopes : t ref = ref []

(* symbols whose bindings are forcibly prevented from being created,
   initialized, etc. This set is initialized in init_env, and is normally
   empty. It's used to implement library overrides: suppressing a local
   binding to definition D means that any local reference to D will
   register it as a deferred global lookup, which will then be linked
   to the override. See Init_js.load_lib_files.
*)
let exclude_symbols : SSet.t ref = ref SSet.empty

let set_exclude_symbols syms = exclude_symbols := syms

let is_excluded name = SSet.mem name !exclude_symbols

(* scopes *)

(* return the current scope *)
let peek_scope () = List.hd !scopes

(* return current scope stack *)
let peek_env () = !scopes

let string_of_env cx env =
  spf "[ %s ]" (String.concat ";\n" (Base.List.map ~f:(Debug_js.string_of_scope cx) env))

(* return the value of f applied to topmost var scope in a scope list *)
let rec top_var_scope = function
  | [] -> assert_false "empty scope list"
  | scope :: scopes ->
    (match scope.kind with
    | VarScope _ -> scope
    | _ -> top_var_scope scopes)

(* get top var scope of current env *)
let peek_var_scope () = top_var_scope (peek_env ())

(* each varscope carries a frame id *)
let peek_frame () = (peek_scope ()).id

(* use the passed f to iterate over all scopes *)
let iter_scopes f = List.iter f !scopes

(* apply function f to local scopes: the 0 or more
   lex scopes between us and the closest var scope,
   and also that var scope *)
let iter_local_scopes f =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope :: scopes ->
      f scope;
      (match scope.kind with
      | LexScope -> loop scopes
      | _ -> ())
  in
  loop !scopes

(* clone the given scope stack (snapshots entry maps) *)
let clone_env scopes = Base.List.map ~f:Scope.clone scopes

let var_scope_kind () =
  let scope = peek_var_scope () in
  match scope.kind with
  | VarScope k -> k
  | _ -> assert_false "peek_var_scope returns a VarScope"

(* true iff scope is var scope with the given kind *)
let is_func_kind k scope =
  match scope.kind with
  | VarScope func_kind -> func_kind = k
  | _ -> false

let in_async_scope () =
  match var_scope_kind () with
  | Async
  | AsyncGenerator ->
    true
  | _ -> false

let in_generator_scope () =
  match var_scope_kind () with
  | Generator
  | AsyncGenerator ->
    true
  | _ -> false

let in_predicate_scope () = is_func_kind Predicate (peek_var_scope ())

(* build a map of all var entries - no refis - in the current
   scope stack.
   Note that we accumulate entries bottom-up, so that
   shadowing is properly maintained *)
let all_entries () =
  List.fold_left
    (fun entries scope -> SMap.union scope.entries entries)
    SMap.empty
    (List.rev !scopes)

(* whole env *)

(* clear environment *)
let havoc_current_activation () =
  scopes := [];
  Changeset.Global.init ()

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
  Changeset.Global.push ();
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
  let ids = Base.List.map ~f:(fun { id; _ } -> id) scopes in
  let changeset = Changeset.(include_scopes ids (Global.peek ())) in
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
    Changeset.Global.pop ()
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
    ignore (Changeset.Global.filter_scope_changes id);

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
let env_depth () = List.length !scopes

(* strip the given number of scopes from top of env *)
let trunc_env =
  let rec trunc = function
    | (0, scopes) -> scopes
    | (_, []) -> assert_false "trunc_env: scopes underflow"
    | (n, scope :: scopes) ->
      ignore (Changeset.Global.filter_scope_changes scope.id);
      trunc (n - 1, scopes)
  in
  fun depth ->
    let cur = !scopes in
    scopes := trunc (List.length cur - depth, cur)

(* initialize a new environment (once per module) *)
let init_env ?(exclude_syms = SSet.empty) cx module_scope =
  set_exclude_symbols exclude_syms;
  havoc_current_activation ();
  let global_scope = Scope.fresh ~var_scope_kind:Global () in
  push_var_scope cx global_scope;
  push_var_scope cx module_scope

(* replace the current env with the passed one.
   envs must be congruent - we measure length as a quick check,
   with a more thorough check on env merge/copy *)
let update_env cx loc new_scopes =
  if List.length new_scopes != List.length (peek_env ()) then
    assert_false
      (spf
         "update_env %s: unequal length scope lists, old %d new %d "
         (string_of_aloc loc)
         (List.length new_scopes)
         (List.length (peek_env ())));

  scopes := new_scopes;
  snapshot_env cx

(* end of basic env API *)

let global_any =
  [
    "eval";
    "arguments";
    (* For `switch` statements not in a function body, so we don't get an error. *)
    internal_name "maybe_exhaustively_checked";
  ]

let global_lexicals = [internal_name "super"; internal_name "this"]

(* any names that haven't been resolved in upper scopes
   wind up here. after handling special names, we add a Var
   binding to the local scope, and register a lookup with
   the global builtin type, to be resolved later.

   Of course, the globals being referred to may not be vars:
   they may instead be consts or type aliases. Currently we
   have no process for checking the use of a global binding
   against the kind of binding it turns out to be: this global
   scope is simply a proxy; resolution takes place as a result
   of the GetPropT types created in the call to Flow.get_builtin.

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
let cache_global cx name ?desc loc global_scope =
  let t =
    if List.mem name global_any then
      AnyT.at AnnotatedAny loc
    else if List.mem name global_lexicals then
      ObjProtoT (mk_reason (RCustom "global object") loc)
    else
      let desc =
        match desc with
        | Some desc -> desc
        | None -> RIdentifier name
      in
      let reason = mk_reason desc loc in
      Flow.get_builtin cx name reason
  in
  let entry = Entry.new_var (Inferred t) ~loc ~state:State.Initialized in
  Scope.add_entry name entry global_scope;
  (global_scope, entry)

let local_scope_entry_exists name =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope :: scopes ->
      (match Scope.get_entry name scope with
      | Some _ -> true
      | None ->
        (match scopes with
        | [] -> false
        | _ -> loop scopes))
  in
  loop !scopes

(* Look for scope that holds binding for a given name. If found,
   return scope and entry. Note that anything we don't resolve
   otherwise, we add to the global scope after generating a
   deferred lookup, which may fail later. *)
let find_entry cx name ?desc loc =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope :: scopes ->
      (match Scope.get_entry name scope with
      | Some entry -> (scope, entry)
      | None ->
        (* keep looking until we're at the global scope *)
        (match scopes with
        | [] -> cache_global cx name ?desc loc scope
        | _ -> loop scopes))
  in
  loop !scopes

let get_class_entries () =
  let rec loop class_bindings = function
    | [] -> assert_false "empty scope list"
    | scope :: scopes ->
      (match Scope.get_entry (internal_name "class") scope with
      | Some entry -> loop (entry :: class_bindings) scopes
      | None ->
        (* keep looking until we're at the global scope *)
        (match scopes with
        | [] -> class_bindings
        | _ -> loop class_bindings scopes))
  in
  let class_bindings = loop [] !scopes in
  let to_class_record = function
    | Entry.Class c -> c
    | _ -> assert_false "Internal Error: Non-class binding stored with .class"
  in
  Base.List.map ~f:to_class_record class_bindings

(* Search for the scope which binds the given name, through the
   topmost LexScopes and up to the first VarScope. If the entry
   is not found, return the VarScope where we terminated. *)
let find_entry_in_var_scope name =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope :: scopes ->
      (match (Scope.get_entry name scope, scope.kind) with
      | (Some entry, _) -> (scope, Some entry)
      | (None, VarScope _) -> (scope, None)
      | (None, LexScope) -> loop scopes)
  in
  loop !scopes

(* Search for the scope which holds the given refinement, through
   the topmost LexScopes and up to the first VarScope. If the
   entry is not found, return None. *)
let find_refi_in_var_scope key =
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope :: scopes ->
      (match (Scope.get_refi key scope, scope.kind) with
      | (Some refi, _) -> Some (scope, refi)
      | (None, VarScope _) -> None
      | (None, LexScope) -> loop scopes)
  in
  loop !scopes

(* helpers *)

let binding_error msg cx name entry loc =
  Flow.add_output cx (Error_message.EBindingError (msg, loc, name, entry))

let already_bound_error = binding_error Error_message.ENameAlreadyBound

(* initialization of entries happens during a preliminary pass through a
   scoped region of the AST (dynamic for hoisted things, lexical for
   lexical things). this leaves them in a germinal state which is
   then read and written during the main traversal of the AST *)

(* helper: initialize entry for given key in top scope,
   dealing with various situations involving a preexisting entry
   (since multiple declarations - sometimes but not always erroneous -
   may appear in an AST)
*)

let bind_entry cx name entry loc =
  (* iterate top-down through scopes until the appropriate scope for this
     binding is found, or realize a binding error *)
  let rec loop = function
    | [] -> assert_false "empty scope list"
    | scope :: scopes ->
      (match get_entry name scope with
      (* if no entry already exists, this might be our scope *)
      | None ->
        Entry.(
          (match (scope.Scope.kind, entry) with
          (* lex scopes can only hold let/const/class bindings *)
          (* var scope can hold all binding types *)
          | (LexScope, Value { Entry.kind = Let _; _ })
          | (LexScope, Value { Entry.kind = Const _; _ })
          | (LexScope, Class _)
          | (VarScope _, _) ->
            add_entry name entry scope
          (* otherwise, keep looking for our scope *)
          | _ -> loop scopes))
      (* some rebindings are allowed, but usually an error *)
      | Some prev ->
        (match scope.kind with
        (* specifically a var scope allows some shadowing *)
        | VarScope _ ->
          Entry.(
            let can_shadow = function
              (* funcs/vars can shadow other funcs/vars -- only in var scope *)
              | ((Var _ | Let FunctionBinding), (Var _ | Let FunctionBinding)) -> true
              (* vars can shadow function params *)
              | (Var _, Let ParamBinding) -> true
              | (Var _, Let ConstlikeParamBinding) -> true
              | (Var _, Const ConstParamBinding) -> true
              | _ -> false
            in
            (match (entry, prev) with
            (* good shadowing leaves existing entry, unifies with new *)
            | (Value e, Value p) when can_shadow (Entry.kind_of_value e, Entry.kind_of_value p) ->
              (* TODO currently we don't step on specific. shouldn't we? *)
              Flow.unify cx (Entry.general_of_value p) (Entry.general_of_value e)
            (* bad shadowing is a binding error *)
            | _ -> already_bound_error cx name prev loc))
        (* shadowing in a lex scope is always an error *)
        | LexScope -> already_bound_error cx name prev loc))
  in
  if not (is_excluded name) then loop !scopes

(* bind class entry *)
let bind_class cx class_id class_private_fields class_private_static_fields =
  bind_entry
    cx
    (internal_name "class")
    (Entry.new_class class_id class_private_fields class_private_static_fields)
    ALoc.none

(* bind var entry *)
let bind_var ?(state = State.Declared) cx name t loc =
  bind_entry cx name (Entry.new_var t ~loc ~state) loc

(* bind let entry *)
let bind_let ?(state = State.Undeclared) cx name t loc =
  bind_entry cx name (Entry.new_let t ~loc ~state) loc

(* bind implicit let entry *)
let bind_implicit_let ?(state = State.Undeclared) kind cx name t loc =
  bind_entry cx name (Entry.new_let (Inferred t) ~kind ~loc ~state) loc

let bind_fun ?(state = State.Declared) = bind_implicit_let ~state Entry.FunctionBinding

(* bind const entry *)
let bind_const ?(state = State.Undeclared) cx name t loc =
  bind_entry cx name (Entry.new_const t ~loc ~state) loc

let bind_import cx name t loc = bind_entry cx name (Entry.new_import t ~loc) loc

(* bind implicit const entry *)
let bind_implicit_const ?(state = State.Undeclared) kind cx name t loc =
  bind_entry cx name (Entry.new_const (Inferred t) ~kind ~loc ~state) loc

(* bind type entry *)
let bind_type ?(state = State.Declared) cx name t loc =
  bind_entry cx name (Entry.new_type t ~loc ~state) loc

let bind_import_type cx name t loc = bind_entry cx name (Entry.new_import_type t ~loc) loc

(* vars coming from 'declare' statements are preinitialized *)
let bind_declare_var cx name t = bind_var ~state:State.Initialized cx name (Annotated t)

(* bind entry for declare function *)
let bind_declare_fun =
  let update_type seen_t new_t =
    match seen_t with
    | IntersectionT (reason, rep) -> IntersectionT (reason, InterRep.append [new_t] rep)
    | _ ->
      let reason = replace_desc_reason RIntersectionType (reason_of_t seen_t) in
      IntersectionT (reason, InterRep.make seen_t new_t [])
  in
  let update_general_type general_t new_t =
    match general_t with
    | Inferred t -> Inferred (update_type t new_t)
    | Annotated t -> Annotated (update_type t new_t)
  in
  fun cx name t loc ->
    if not (is_excluded name) then
      let scope = peek_scope () in
      match Scope.get_entry name scope with
      | None ->
        let entry = Entry.new_var (Inferred t) ~loc ~state:State.Initialized in
        Scope.add_entry name entry scope
      | Some prev ->
        Entry.(
          (match prev with
          | Value v
            when match Entry.kind_of_value v with
                 | Var _ -> true
                 | _ -> false ->
            let entry =
              Value
                {
                  v with
                  value_state = State.Initialized;
                  specific = update_type v.specific t;
                  general = update_general_type v.general t;
                }
            in
            Scope.add_entry name entry scope
          | _ ->
            (* declare function shadows some other kind of binding *)
            already_bound_error cx name prev loc))

(* helper: move a Let/Const's entry's state from Undeclared to Declared.
   Only needed for let and const to push things into scope for potentially
   recursive internal refs: hoisted things (vars and types) become declared
   immediately on binding.
*)
let declare_value_entry kind cx name loc =
  if not (is_excluded name) then
    Entry.(
      let (scope, entry) = find_entry cx name loc in
      match entry with
      | Value v when Entry.kind_of_value v = kind && Entry.state_of_value v = State.Undeclared ->
        let new_entry = Value { v with value_state = State.Declared } in
        Scope.add_entry name new_entry scope
      | _ -> already_bound_error cx name entry loc)

let declare_let = declare_value_entry Entry.(Let LetVarBinding)

let declare_implicit_let kind = declare_value_entry (Entry.Let kind)

let declare_const = declare_value_entry Entry.(Const ConstVarBinding)

let declare_implicit_const kind = declare_value_entry (Entry.Const kind)

let promote_to_const_like cx loc =
  try
    let (info, values) = Context.use_def cx in
    let uses = Scope_api.uses_of_use info loc in
    (* We consider a binding to be const-like if all reads point to the same
       write, modulo initialization. *)
    let writes =
      ALocSet.fold
        (fun use acc ->
          match ALocMap.find_opt use values with
          | None -> (* use is a write *) acc
          | Some write_locs ->
            (* use is a read *)
            (* collect writes pointed to by the read, modulo initialization *)
            List.fold_left
              (fun acc -> function
                | Ssa_api.With_ALoc.Uninitialized -> acc
                | Ssa_api.With_ALoc.Write loc -> ALocSet.add loc acc)
              acc
              write_locs)
        uses
        ALocSet.empty
    in
    ALocSet.cardinal writes <= 1
  with _ -> false

let initialized_value_entry cx kind specific loc v =
  Entry.(
    (* Maybe promote to const-like *)
    let new_kind =
      match kind with
      | Var VarBinding ->
        if promote_to_const_like cx loc then
          Var ConstlikeVarBinding
        else
          kind
      | Let LetVarBinding ->
        if promote_to_const_like cx loc then
          Let ConstlikeLetVarBinding
        else
          kind
      | _ -> kind
    in
    Value { v with Entry.kind = new_kind; value_state = State.Initialized; specific })

(* helper - update var entry to reflect assignment/initialization *)
(* note: here is where we understand that a name can be multiply var-bound
 * TODO: we started tracking annotations when variables are bound. Once we do
 * that at all binding sites this ~has_anno param can go away in favor of
 * looking up the annot in the environment *)
let init_value_entry kind cx ~use_op name ~has_anno specific loc =
  if not (is_excluded name) then
    Entry.(
      let (scope, entry) = find_entry cx name loc in
      match (kind, entry) with
      | (Var _, Value ({ Entry.kind = Var _; _ } as v))
      | ( Let _,
          Value ({ Entry.kind = Let _; value_state = State.Undeclared | State.Declared; _ } as v) )
      | ( Const _,
          Value ({ Entry.kind = Const _; value_state = State.Undeclared | State.Declared; _ } as v)
        ) ->
        Changeset.Global.change_var (scope.id, name, Changeset.Write);
        let general = TypeUtil.type_t_of_annotated_or_inferred v.general in
        if specific != general then Flow_js.flow cx (specific, UseT (use_op, general));

        (* note that annotation supercedes specific initializer type *)
        let specific =
          if has_anno then
            general
          else
            specific
        in
        let new_entry = initialized_value_entry cx kind specific loc v in
        Scope.add_entry name new_entry scope
      | _ ->
        (* Incompatible or non-redeclarable new and previous entries.
           We will have already issued an error in `bind_value_entry`,
           so we can prune this case here. *)
        ())

let init_var = init_value_entry Entry.(Var VarBinding)

let init_let = init_value_entry Entry.(Let LetVarBinding)

let init_implicit_let kind = init_value_entry (Entry.Let kind)

let init_fun = init_implicit_let ~has_anno:false Entry.FunctionBinding

let init_const = init_value_entry Entry.(Const ConstVarBinding)

let init_implicit_const kind = init_value_entry Entry.(Const kind)

(* update type alias to reflect initialization in code *)
let init_type cx name type_ loc =
  if not (is_excluded name) then
    Entry.(
      let (scope, entry) = find_entry cx name loc in
      match entry with
      | Type ({ type_state = State.Declared; _ } as t) ->
        Flow.flow_t cx (type_, t.type_);
        let new_entry = Type { t with type_state = State.Initialized; type_ } in
        Scope.add_entry name new_entry scope
      | _ ->
        (* Incompatible or non-redeclarable new and previous entries.
           We will have already issued an error in `bind_value_entry`,
           so we can prune this case here. *)
        ())

(* treat a var's declared (annotated) type as an initializer *)
let pseudo_init_declared_type cx name loc =
  if not (is_excluded name) then
    Entry.(
      let (scope, entry) = find_entry cx name loc in
      match entry with
      | Value ({ Entry.kind = Var _; _ } as v)
      | Value ({ Entry.kind = Let _ | Const _; value_state = State.(Undeclared | Declared); _ } as v)
        ->
        Changeset.Global.change_var (scope.id, name, Changeset.Write);
        let kind = v.Entry.kind in
        let entry =
          initialized_value_entry cx kind (TypeUtil.type_t_of_annotated_or_inferred v.general) loc v
        in
        Scope.add_entry name entry scope
      | _ ->
        (* Incompatible or non-redeclarable new and previous entries.
           We will have already issued an error in `bind_value_entry`,
           so we can prune this case here. *)
        ())

(* helper for read/write tdz checks *)
(* for now, we only enforce TDZ within the same activation.
   return true if the given target scope is in the same
   activation as the current scope.
*)
let same_activation target =
  let rec loop target = function
    | [] -> assert_false "target scope not found"
    | scope :: _ when scope.id = target.id ->
      (* target is nearer than (or actually is) nearest VarScope *)
      true
    | scope :: scopes ->
      (match scope.kind with
      | VarScope _ ->
        (* found var scope before target *)
        false
      | LexScope ->
        (* still in inner lex scopes, keep looking *)
        loop target scopes)
  in
  (* search outward for target scope *)
  loop target (peek_env ())

(* get types from value entry, does uninitialized -> undefined behavior *)
let value_entry_types ?(lookup_mode = ForValue) scope =
  Entry.(
    function
    (* from value positions, a same-activation ref to var or an explicit let
       before initialization yields undefined. *)
    | {
        Entry.kind = Var _ | Let LetVarBinding;
        value_state = (State.Declared | State.MaybeInitialized) as state;
        value_declare_loc;
        specific;
        general;
        _;
      }
      when lookup_mode = ForValue && same_activation scope ->
      let uninit desc = VoidT.make (mk_reason desc value_declare_loc) |> with_trust bogus_trust in
      let specific =
        if state = State.Declared then
          uninit (RCustom "uninitialized variable")
        else
          (* State.MaybeInitialized *)
          let desc = RCustom "possibly uninitialized variable" in
          let rep = UnionRep.make (uninit desc) specific [] in
          UnionT (mk_reason desc value_declare_loc, rep)
      in
      (specific, general)
    | { specific; general; _ } -> (specific, general))

(* emit tdz error for value entry *)
let tdz_error cx name loc v =
  Entry.(
    (* second clause of error message is due to switch scopes *)
    let msg = Error_message.EReferencedBeforeDeclaration in
    binding_error msg cx name (Value v) loc)

(* helper for read/write tdz checks *)
(* functions are block-scoped, but also hoisted. forward ref ok *)
let allow_forward_ref =
  Scope.Entry.(
    function
    | Var _
    | Let FunctionBinding ->
      true
    | _ -> false)

(* helper - does semantic checking and returns entry type *)
let read_entry ~lookup_mode ~specific cx name ?desc loc =
  let (scope, entry) = find_entry cx name ?desc loc in
  Entry.(
    match entry with
    | Type _ when lookup_mode != ForType ->
      let msg = Error_message.ETypeInValuePosition in
      binding_error msg cx name entry loc;
      AnyT.at (AnyError None) (entry_loc entry)
    | Type t -> t.type_
    | Class _ -> assert_false "Internal Error: Classes should only be read using get_class_entries"
    | Value v ->
      (match v with
      | { Entry.kind; value_state = State.Undeclared; value_declare_loc; _ }
        when lookup_mode = ForValue && (not (allow_forward_ref kind)) && same_activation scope ->
        tdz_error cx name loc v;
        AnyT.at (AnyError None) value_declare_loc
      | _ ->
        Changeset.Global.change_var (scope.id, name, Changeset.Read);
        let (s, g) = value_entry_types ~lookup_mode scope v in
        if specific then
          s
        else
          TypeUtil.type_t_of_annotated_or_inferred g))

let rec seek_env f = function
  | [] -> None
  | scope :: scopes ->
    (match f scope with
    | Some x -> Some x
    | None -> seek_env f scopes)

(* get env entry for name, if it exists *)
let get_env_entry name = seek_env (Scope.get_entry name)

(* get current env entry for name, if it exists *)
let get_current_env_entry name = get_env_entry name !scopes

(* get env refi for key, if it exists *)
let get_env_refi key = seek_env (Scope.get_refi key)

(* get current env refi for name, if it exists *)
let get_current_env_refi key = get_env_refi key !scopes

(* get var's specific type (and track the reference) *)
let get_var ?(lookup_mode = ForValue) = read_entry ~lookup_mode ~specific:true ?desc:None

(* query var's specific type *)
let query_var ?(lookup_mode = ForValue) = read_entry ~lookup_mode ~specific:true

let query_var_non_specific ?(lookup_mode = ForValue) = read_entry ~lookup_mode ~specific:false

let get_internal_var cx name loc = query_var cx (internal_name name) loc

let get_var_annotation cx name loc =
  let (_, entry) = find_entry cx name loc in
  match entry with
  (* When we start actually passing annotation targets through statement.ml
   * we should return the type here instead of unit *)
  | Entry.Value { Entry.general = Annotated _; _ } -> Some ()
  | _ -> None

(* get var's general type - for annotated vars, this is the
   annotated type, and for others it's the union of all
   types assigned to the var throughout its lifetime.
*)
let get_var_declared_type ?(lookup_mode = ForValue) =
  read_entry ~lookup_mode ~specific:false ?desc:None

(* Unify declared type with another type. This is useful for allowing forward
   references in declared types to other types declared later in scope. *)
let unify_declared_type ?(lookup_mode = ForValue) cx name t =
  Entry.(
    match get_current_env_entry name with
    | Some (Value v) when lookup_mode = ForValue -> Flow.unify cx t (general_of_value v)
    | Some entry when lookup_mode <> ForValue -> Flow.unify cx t (Entry.declared_type entry)
    | _ -> ())

(* Unify declared function type with another type. This is similarly motivated as above, except that
   we also need to take overloading into account. See `bind_declare_fun` for similar logic. *)
let unify_declared_fun_type =
  let find_type aloc = function
    | IntersectionT (_, rep) ->
      let match_type t = aloc_of_reason (reason_of_t t) = aloc in
      begin
        match List.find_opt match_type (InterRep.members rep) with
        | Some t -> t
        | None -> assert_false "Internal Error: Improper overloaded declare function entries."
      end
    | v -> v
  in
  fun cx name aloc t ->
    Entry.(
      match get_current_env_entry name with
      | Some (Value v) -> Flow.unify cx t (find_type aloc (general_of_value v))
      | _ -> ())

let is_global_var _cx name =
  let rec loop = function
    | [] -> true
    | scope :: scopes ->
      (match Scope.get_entry name scope with
      | Some _ -> Scope.is_global scope
      | None -> loop scopes)
  in
  loop !scopes

(* get var type, with given location used in type's reason *)
let var_ref ?(lookup_mode = ForValue) cx name ?desc loc =
  let t = query_var ~lookup_mode cx name ?desc loc in
  Flow.reposition cx loc t

(* get refinement entry *)
let get_refinement cx key loc =
  match find_refi_in_var_scope key with
  | Some (_, { refined; _ }) -> Some (Flow.reposition cx loc refined)
  | _ -> None

(* Types-First signature extraction only inspects the initial binding for let-like
   definitions. Any subsequent assignment is ignored. This would cause a discrepancy
   between what classic mode views as the exported value, and what types-first does.
   To avoid this, we disallow updates on classes and functions. Let-bound variables
   would follow the same rule, but Types-First requires an annotation on their
   definition. This mitigates the problem, since in case of a reassigment the updated
   value needs to respect the annotation type, so we would only miss a potential
   refinement rather a strong type update.
*)
let check_exported_let_bound_reassignment op cx name entry loc =
  let open Entry in
  match (op, entry, Context.exported_locals cx) with
  | ( Changeset.Write,
      Value
        {
          Entry.kind = Let Entry.((ClassNameBinding | FunctionBinding) as binding_kind);
          value_declare_loc;
          _;
        },
      Some exported_locals ) ->
    (match SMap.find_opt name exported_locals with
    | Some loc_set when ALocSet.mem value_declare_loc loc_set ->
      let reason = mk_reason (RType name) value_declare_loc in
      Flow.add_output
        cx
        Error_message.(EAssignExportedConstLikeBinding { loc; definition = reason; binding_kind })
    | _ -> ())
  | _ -> ()

(* helper: update let or var entry *)
let update_var op cx ~use_op name specific loc =
  let (scope, entry) = find_entry cx name loc in
  check_exported_let_bound_reassignment op cx name entry loc;
  Entry.(
    match entry with
    | Value ({ Entry.kind = Let _ as kind; value_state = State.Undeclared; _ } as v)
      when (not (allow_forward_ref kind)) && same_activation scope ->
      tdz_error cx name loc v;
      None
    | Value ({ Entry.kind = Let _ | Var _; _ } as v) ->
      let change = (scope.id, name, op) in
      Changeset.Global.change_var change;
      let use_op =
        match op with
        | Changeset.Write -> use_op
        | Changeset.Refine -> use_op
        | Changeset.Read -> unknown_use
        (* this is impossible *)
      in
      Flow.flow cx (specific, UseT (use_op, Entry.general_of_value v));

      (* add updated entry *)
      let update =
        Entry.Value
          { v with Entry.value_state = State.Initialized; specific; value_assign_loc = loc }
      in
      Scope.add_entry name update scope;
      Some change
    | Value { Entry.kind = Const ConstVarBinding; _ } ->
      let msg = Error_message.EConstReassigned in
      binding_error msg cx name entry loc;
      None
    | Value { Entry.kind = Const EnumNameBinding; _ } ->
      let msg = Error_message.EEnumReassigned in
      binding_error msg cx name entry loc;
      None
    | Value { Entry.kind = Const ConstImportBinding; _ } ->
      let msg = Error_message.EImportReassigned in
      binding_error msg cx name entry loc;
      None
    | Value { Entry.kind = Const ConstParamBinding; _ } ->
      (* TODO: remove extra info when surface syntax is added *)
      let msg = Error_message.EConstParamReassigned in
      binding_error msg cx name entry loc;
      None
    | Type _ ->
      let msg = Error_message.ETypeAliasInValuePosition in
      binding_error msg cx name entry loc;
      None
    | Class _ -> assert_false "Internal error: update_var called on Class")

(* update var by direct assignment *)
let set_var = update_var Changeset.Write

let set_internal_var cx name t loc =
  update_var Changeset.Write cx ~use_op:unknown_use (internal_name name) t loc

(* update var by refinement test *)
let refine_var = update_var Changeset.Refine ~use_op:(Op (Internal Refinement))

(* set const's specific type to reflect a refinement test (internal) *)
let refine_const cx name specific loc =
  let (scope, entry) = find_entry cx name loc in
  Entry.(
    match entry with
    | Value ({ Entry.kind = Const _; value_state = State.Undeclared; _ } as v)
      when same_activation scope ->
      tdz_error cx name loc v;
      None
    | Value ({ Entry.kind = Const _; _ } as v) ->
      let change = (scope.id, name, Changeset.Refine) in
      Changeset.Global.change_var change;
      let general = Entry.general_of_value v in
      Flow.flow cx (specific, UseT (Op (Internal Refinement), general));
      let update = Value { v with value_state = State.Initialized; specific } in
      Scope.add_entry name update scope;
      Some change
    | _ -> assert_false (spf "refine_const called on %s %s" (Entry.string_of_kind entry) name))

(* given a list of envs (scope lists), return true iff all envs are
   the same length and all scope ids and kinds match *)
let envs_congruent envs =
  let rec check_scopes envs =
    let env0 = List.hd envs in
    env0 = []
    ||
    let scope0 = List.hd env0 in
    let check_scope env =
      let scope = List.hd env in
      scope.id = scope0.id && scope.kind = scope0.kind
    in
    List.for_all check_scope (List.tl envs) && check_scopes (Base.List.map ~f:List.tl envs)
  in
  let envs = ListUtils.phys_uniq envs in
  List.length envs <= 1
  ||
  let len = List.length (List.hd envs) in
  List.for_all (fun env -> List.length env = len) (List.tl envs) && check_scopes envs

(* find scopes with a given id in a list of envs.
   envs are assumed congruent amd assumed to contain scope id *)
let rec find_scope cx loc envs scope_id =
  match envs with
  | (scope0 :: _) :: _ ->
    if scope0.id = scope_id then
      List.(map hd envs)
    else
      find_scope cx loc List.(map tl envs) scope_id
  | _ ->
    assert_false
      (spf
         "find_scopes %s: scope %d not found. head env %s"
         (string_of_aloc loc)
         scope_id
         (string_of_env cx (List.hd envs)))

(* The following function takes a changset and a triple of environments -
   original and two derivations - and merges the bindings indicated by
 changeset keys from the derivations into the original. *)
let merge_env =
  (* find scope triple in env triple *)
  let find_scope_triple cx loc (env0, env1, env2) id =
    let lst = find_scope cx loc [env0; env1; env2] id in
    List.(nth lst 0, nth lst 1, nth lst 2)
  in
  let create_union cx loc name l1 l2 =
    let reason = mk_reason name loc in
    Tvar.mk_where cx reason (fun tvar ->
        Flow.flow cx (l1, UseT (Op (Internal MergeEnv), tvar));
        Flow.flow cx (l2, UseT (Op (Internal MergeEnv), tvar)))
  in
  (* merge_entry helper - calculate new specific type *)
  let merge_specific cx loc name (specific0, general0) specific1 specific2 =
    (* if both children are unchanged, or 1 child is unchanged and the other
       is bottom (EmptyT), then we can avoid creating a merged specific *)
    if
      (specific0 = specific1 && (specific0 = specific2 || is_bot specific2))
      || (specific0 = specific2 && is_bot specific1)
    then
      specific0
    (* child has reverted to original - shortcut *)
    else if specific1 = general0 || specific2 = general0 then
      general0
    (* general case *)
    else
      let tvar = create_union cx loc name specific1 specific2 in
      Flow.flow cx (tvar, UseT (Op (Internal MergeEnv), general0));
      tvar
  in
  (* propagate var state updates from child entries *)
  let merge_states orig child1 child2 =
    Entry.(
      match orig.Entry.kind with
      | Var _
      | Let _
        when child1.value_state = State.Initialized && child2.value_state = State.Initialized ->
        (* if both branches have initialized, we can set parent state *)
        State.Initialized
      | Var _
      | Let _
        when child1.value_state >= State.Declared
             && child2.value_state >= State.Declared
             && ( child1.value_state >= State.MaybeInitialized
                || child2.value_state >= State.MaybeInitialized ) ->
        (* if either branch has initialized, we can set parent state *)
        State.MaybeInitialized
      | _ -> orig.value_state)
  in
  let merge_entry cx loc envs ((scope_id, name, _) as entry_ref) =
    let (scope0, scope1, scope2) = find_scope_triple cx loc envs scope_id in
    let get = get_entry name in
    Entry.(
      match (get scope0, get scope1, get scope2) with
      (* merge child var and let types back to original *)
      | (Some (Value orig), Some (Value child1), Some (Value child2)) ->
        let { specific = s0; general = g0; _ } = orig in
        let { specific = s1; _ } = child1 in
        let { specific = s2; _ } = child2 in
        let specific =
          merge_specific
            cx
            loc
            (RIdentifier name)
            (s0, TypeUtil.type_t_of_annotated_or_inferred g0)
            s1
            s2
        in
        let value_state = merge_states orig child1 child2 in
        (* replace entry if anything changed *)
        if specific == s0 && value_state = orig.value_state then
          ()
        else
          let e = Entry.Value { orig with Entry.specific; value_state } in
          add_entry name e scope0
      (* type aliases can't be refined or reassigned, shouldn't be here *)
      | (Some (Type _), Some (Type _), Some (Type _)) ->
        assert_false
          (spf "merge_env %s: type alias %s found in changelist" (string_of_aloc loc) name)
      (* global lookups may leave uneven new entries, which we can forget *)
      | (_, _, _) when is_global scope0 -> ()
      (* missing completely from non-global scope *)
      | (None, None, None) ->
        assert_false
          (spf
             "%smerge_entry %s %s: missing from scopes:\n%s\n%s\n%s"
             (Context.pid_prefix cx)
             (string_of_aloc loc)
             (Changeset.string_of_entry_ref entry_ref)
             (Debug_js.string_of_scope cx scope0)
             (Debug_js.string_of_scope cx scope1)
             (Debug_js.string_of_scope cx scope2))
      (* a newly created entry may exist in one lex child -
         this pattern is due to our current switch handling *)
      | (None, Some (Value _ as entry), None) when Scope.is_lex scope1 ->
        add_entry name entry scope0
      | (None, None, Some (Value _ as entry)) when Scope.is_lex scope2 ->
        add_entry name entry scope0
      (* otherwise, non-refinement uneven distributions are asserts. *)
      | (orig, child1, child2) ->
        let print_entry_kind_opt = function
          | None -> "None"
          | Some e -> spf "Some %s" Entry.(string_of_kind e)
        in
        assert_false
          (spf
             "merge_env %s: non-uniform distribution of entry %s: %s, %s, %s"
             (string_of_aloc loc)
             name
             (print_entry_kind_opt orig)
             (print_entry_kind_opt child1)
             (print_entry_kind_opt child2)))
  in
  let merge_refi cx loc envs (scope_id, key, _) =
    let (scope0, scope1, scope2) = find_scope_triple cx loc envs scope_id in
    let get = get_refi key in
    match (get scope0, get scope1, get scope2) with
    (* evenly distributed refinements are merged *)
    | (Some base, Some child1, Some child2) ->
      let name = Key.reason_desc key in
      let refined =
        merge_specific cx loc name (base.refined, base.original) child1.refined child2.refined
      in
      if refined == base.refined then
        ()
      else
        add_refi key { base with refined } scope0
    (* refi was introduced in both children *)
    | (None, Some child1, Some child2) ->
      let name = Key.reason_desc key in
      let refined = create_union cx loc name child1.refined child2.refined in
      let original = create_union cx loc name child1.original child2.original in
      let refi = { refi_loc = loc; refined; original } in
      add_refi key refi scope0
    (* refi was cleared in a child env. clear from original *)
    | (Some _, _, _) -> remove_refi key scope0
    (* refi was introduced - and possibly also removed by havoc -
       after envs diverged *)
    | (None, _, _) -> ()
  in
  (* merge entries and refis found in changeset *)
  fun cx loc (env0, env1, env2) changeset ->
    if not (envs_congruent [env0; env1; env2]) then
      assert_false
        (spf
           "merge_env %s: envs not congruent: %d %d %d"
           (string_of_aloc loc)
           (List.length env0)
           (List.length env1)
           (List.length env2));
    changeset
    |> Changeset.iter_type_updates
         (merge_entry cx loc (env0, env1, env2))
         (merge_refi cx loc (env0, env1, env2))

(* copy changes from env2 into env1 *)
let copy_env =
  (* find sscope pair in env pair *)
  let find_scope_pair cx loc (env0, env1) id =
    let lst = find_scope cx loc [env0; env1] id in
    List.(nth lst 0, nth lst 1)
  in
  (* look for and copy entry, starting in topmost scope *)
  let copy_entry cx loc envs (scope_id, name, _) =
    let (scope1, scope2) = find_scope_pair cx loc envs scope_id in
    let get = get_entry name in
    Entry.(
      match (get scope1, get scope2) with
      (* for values, flow env2's specific type into env1's specific type *)
      | (Some (Value v1), Some (Value v2)) ->
        (* flow child2's specific type to child1 in place *)
        Flow.flow cx (v2.specific, UseT (Op (Internal CopyEnv), v1.specific));

        (* update state *)
        if v1.value_state < State.Initialized && v2.value_state >= State.MaybeInitialized then
          let new_entry = Value { v1 with value_state = State.MaybeInitialized } in
          add_entry name new_entry scope1
      (* type aliases shouldn't be here *)
      | (Some (Type _), Some (Type _)) ->
        assert_false
          (spf "copy_env %s: type alias %s found in changelist" (string_of_aloc loc) name)
      (* global lookups may leave new entries in env2, or orphan changes *)
      (* ...which we can forget *)
      | (None, _) when is_global scope1 -> ()
      (* changeset entry exists only in lex scope *)
      | (Some (Value _), None) when Scope.is_lex scope1 -> ()
      | (None, Some (Value _ as entry)) when Scope.is_lex scope2 -> add_entry name entry scope1
      (* uneven distributions *)
      | (entry1, entry2) ->
        let print_entry_kind_opt = function
          | None -> "None"
          | Some e -> spf "Some %s" (Entry.string_of_kind e)
        in
        assert_false
          (spf
             "copy_env %s: non-uniform distribution of entry %s: %s, %s"
             (string_of_aloc loc)
             name
             (print_entry_kind_opt entry1)
             (print_entry_kind_opt entry2)))
  in
  (* look for and copy refinement in top scope only *)
  let copy_refi cx loc envs (scope_id, key, _) =
    let (scope0, scope1) = find_scope_pair cx loc envs scope_id in
    let get = get_refi key in
    match (get scope0, get scope1) with
    (* flow child refi's type back to parent *)
    | (Some { refined = t1; _ }, Some { refined = t2; _ }) ->
      Flow.flow cx (t2, UseT (Op (Internal CopyEnv), t1))
    (* uneven cases imply refi was added after splitting: remove *)
    | _ -> ()
  in
  (* copy entries and refis bound to names and keys, respectively *)
  fun cx loc (env1, env2) changeset ->
    if envs_congruent [env1; env2] then
      ()
    else
      assert_false (spf "copy_env %s: envs not congruent" (string_of_aloc loc));
    changeset
    |> Changeset.iter_type_updates (copy_entry cx loc (env1, env2)) (copy_refi cx loc (env1, env2))

(* in the top scope, convert specific types to tvars with former
   specific type as incoming lower bound, and general type as
   upper bound. This prepares the specific type for later merging
   during path-dependent analysis.
*)
let widen_env =
  let widened cx loc name specific general =
    if specific = general then
      None
    else
      let reason = mk_reason name loc in
      let tvar = Tvar.mk cx reason in
      Flow.flow cx (specific, UseT (Op (Internal WidenEnv), tvar));
      Flow.flow cx (tvar, UseT (Op (Internal WidenEnv), general));
      Some tvar
  in
  let widen_var
      cx loc name ({ Entry.specific; general = Annotated general | Inferred general; _ } as var) =
    match widened cx loc name specific general with
    | None -> var
    | Some specific -> { var with Entry.specific }
  in
  let widen_refi cx loc name ({ refined; original; _ } as refi) =
    match widened cx loc name refined original with
    | None -> refi
    | Some refined -> { refi with refined }
  in
  fun cx loc ->
    iter_local_scopes (fun scope ->
        scope
        |> Scope.update_entries
             Entry.(
               fun name -> function
                 | Value var -> Value (widen_var cx loc (RIdentifier name) var)
                 | entry -> entry);
        scope |> Scope.update_refis (fun key refi -> widen_refi cx loc (Key.reason_desc key) refi))

(* The protocol around havoc has changed a few times.
   The following function used to do most of the work, but is now subsumed by
   havoc_ctx, and is now used only to clear the environment when looking at
   a function body. Also see below. *)

(* clear refinement informnation for all binding entries in env *)
let havoc_all () = iter_scopes Scope.havoc

(* set specific type of every non-internal var *and const*
   in top activation to undefined, and clear heap refinements.
   Note: this operation is needed by our control flow handling, which is
   left over from the earlier, looser model. To properly simulate early exits,
   all entries including consts must have their specific entries set to EmptyT.
   TODO rework the early-exit stuff to not break invariants. Until then it'll
   remain a source of bugs.
*)
let reset_current_activation loc = iter_local_scopes (Scope.reset loc)

(* clear refinement info for (topmost bindings of) given names in env *)
let havoc_vars =
  Scope.(
    (* clear specific info for (topmost binding of) given var in env *)
    let havoc_entry (_, name, _) =
      let rec loop = function
        | [] -> ()
        | scope :: scopes ->
          (match get_entry name scope with
          | Some entry ->
            let entry = Entry.havoc name entry in
            add_entry name entry scope
          | None -> loop scopes)
      in
      loop !scopes
    in
    (* clear refinement for (topmost binding of) given key in env *)
    let havoc_refi (_, key, _) =
      let rec loop = function
        | [] -> ()
        | scope :: scopes ->
          (match get_refi key scope with
          | Some _ -> remove_refi key scope
          | None -> loop scopes)
      in
      loop !scopes
    in
    Changeset.iter_type_updates havoc_entry havoc_refi)

(* Clear entries for heap refinement pseudovars in env.
   If name is passed, clear only those refis that depend on it.
   Real variables are left untouched.
*)
let havoc_heap_refinements () = iter_scopes Scope.havoc_all_refis

let havoc_heap_refinements_with_propname ~private_ name =
  iter_scopes (Scope.havoc_refis ~private_ ~name)

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
let add_heap_refinement op key refi_loc refined original =
  let refi = { refi_loc; refined; original } in
  let (base, _) = key in
  let (scope, _) = find_entry_in_var_scope base in
  let change = (scope.id, key, op) in
  Changeset.Global.change_refi change;
  Scope.add_refi key refi scope;
  change

let set_expr = add_heap_refinement Changeset.Write

let refine_expr = add_heap_refinement Changeset.Refine

(* add predicate refinements from given preds map to environment.
   returns changeset containing all refinements added to env.
   note: orig_types maps names to unrefined types. this param is
   only necessary for fresh pseudovars like heap refinements -
   others can be obtained via query_var.
*)
let refine_with_preds cx loc preds orig_types =
  let rec check_literal_subtypes ~general_type pred =
    (* When refining a type against a literal, we want to be sure that that literal can actually
       inhabit that type *)
    match pred with
    | SingletonBoolP (loc, b) ->
      let reason = loc |> mk_reason (RBooleanLit b) in
      let l = DefT (reason, bogus_trust (), BoolT (Some b)) in
      let u = UseT (Op (Internal Refinement), general_type) in
      Context.add_literal_subtypes cx (l, u)
    | SingletonStrP (loc, b, str) ->
      let reason = loc |> mk_reason (RStringLit str) in
      let l = DefT (reason, bogus_trust (), StrT (Literal (Some b, str))) in
      let u = UseT (Op (Internal Refinement), general_type) in
      Context.add_literal_subtypes cx (l, u)
    | SingletonNumP (loc, b, ((_, str) as num)) ->
      let reason = loc |> mk_reason (RNumberLit str) in
      let l = DefT (reason, bogus_trust (), NumT (Literal (Some b, num))) in
      let u = UseT (Op (Internal Refinement), general_type) in
      Context.add_literal_subtypes cx (l, u)
    | LeftP (SentinelProp name, t) ->
      let reason = TypeUtil.reason_of_t t in
      (* Store any potential sentinel type. Later on, when the check is fired (in
         merge_js.ml), we only focus on primitive literal types. *)
      Context.add_matching_props cx (reason, name, t, general_type)
    | NotP p -> check_literal_subtypes ~general_type p
    | OrP (p1, p2)
    | AndP (p1, p2) ->
      check_literal_subtypes ~general_type p1;
      check_literal_subtypes ~general_type p2
    | _ -> ()
  in
  let refine_type orig_type pred refined_type =
    Flow.flow cx (orig_type, PredicateT (pred, refined_type))
  in
  let mk_refi_type orig_type pred refi_reason =
    refine_type orig_type pred |> Tvar.mk_no_wrap_where cx refi_reason
  in
  let refine_with_pred key pred acc =
    let refi_reason = mk_reason (RRefined (Key.reason_desc key)) loc in
    match key with
    (* for real consts/lets/vars, we model assignment/initialization *)
    | (name, []) when not (is_internal_name name) ->
      Entry.(
        (match find_entry cx name loc with
        | (_, Value v) ->
          let orig_type = query_var cx name loc in
          let general_type = query_var_non_specific cx name loc in
          check_literal_subtypes ~general_type pred;
          let refi_type = mk_refi_type orig_type pred refi_reason in
          let refine =
            match Entry.kind_of_value v with
            | Const _ -> refine_const
            | _ -> refine_var
          in
          begin
            match refine cx name refi_type loc with
            | Some change -> Changeset.add_var change acc
            | None -> acc
          end
        | (_, _) ->
          Flow.add_output cx (Error_message.ERefineAsValue (refi_reason, name));
          acc))
    (* for heap refinements, we just add new entries *)
    | _ ->
      let orig_type = Key_map.find key orig_types in
      let refi_type = mk_refi_type orig_type pred refi_reason in
      let change = refine_expr key loc refi_type orig_type in
      Changeset.add_refi change acc
  in
  Key_map.fold refine_with_pred preds Changeset.empty

(* run the given function in a clone of the current environment
   augmented by the given refinement map, then merge the final
   state of the cloned environment back into the reinstated
   original *)
let in_refined_env cx loc preds orig_types f =
  let oldset = Changeset.Global.clear () in
  let orig_env = peek_env () in
  let new_env = clone_env orig_env in
  update_env cx loc new_env;
  let _ = refine_with_preds cx loc preds orig_types in
  let result = f () in
  let newset = Changeset.Global.merge oldset in
  merge_env cx loc (orig_env, orig_env, new_env) newset;
  update_env cx loc orig_env;

  result
