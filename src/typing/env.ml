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

module Env : Env_sig.S = struct
  open Env_sig.LookupMode

  (****************)
  (* Environment *)
  (****************)

  type scope = Scope.t

  type t = scope list

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
  let exclude_symbols : NameUtils.Set.t ref = ref NameUtils.Set.empty

  let set_exclude_symbols syms = exclude_symbols := syms

  let is_excluded name = NameUtils.Set.mem name !exclude_symbols

  (* scopes *)

  (* return the current scope *)
  let peek_scope () = List.hd !scopes

  let in_toplevel_scope () = Scope.is_toplevel (peek_scope ())

  let in_global_scope () = Scope.is_global (peek_scope ())

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

  (* whole env *)

  (* clear environment *)
  let havoc_current_activation () =
    scopes := [];
    Changeset.Global.init ()

  (* push a new var scope into the environment.
     current env state is stored to cx under scope id *)
  (* TODO maintain changelist here too *)
  let push_var_scope scope =
    (match scope.kind with
    | VarScope _ -> ()
    | _ -> assert_false "push_var_scope on non-var scope");
    scopes := scope :: !scopes;
    Changeset.Global.push ()

  (* --- *)

  (* pop a var scope from the environment.
     note: may require popping accumulated lex scopes *)
  let pop_var_scope () =
    match !scopes with
    | { kind = VarScope _; _ } :: tail_scopes ->
      scopes := tail_scopes;
      Changeset.Global.pop ()
    | [] -> assert_false "empty scope list"
    | _ -> assert_false "top scope is non-var"

  (* push a lex scope but NOT a changeset
     (which is 1-1 with var scopes). *)
  let push_lex_scope () =
    let scope = Scope.fresh_lex () in
    scopes := scope :: !scopes

  let pop_lex_scope () =
    match !scopes with
    | { kind = LexScope; id; _ } :: tail_scopes ->
      (* cull any changelist entries for this scope *)
      ignore (Changeset.Global.filter_scope_changes id);

      (* pop *)
      scopes := tail_scopes
    | [] -> assert_false "empty scope list"
    | _ -> assert_false "top scope is non-lex"

  let in_lex_scope f =
    push_lex_scope ();
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
  let init_env ?(exclude_syms = NameUtils.Set.empty) cx module_scope =
    begin
      if Options.env_option_enabled (Context.env_mode cx) Options.ConstrainWrites then
        let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
        ALocMap.fold
          (fun loc reason env ->
            let t = Inferred (Tvar.mk cx reason) in
            (* Treat everything as inferred for now for the purposes of annotated vs inferred *)
            Loc_env.initialize env loc t)
          var_info.Env_api.env_entries
          env
        |> Context.set_environment cx
    end;

    set_exclude_symbols exclude_syms;
    havoc_current_activation ();
    let global_scope = Scope.fresh ~var_scope_kind:Global () in
    push_var_scope global_scope;
    push_var_scope module_scope

  let save_excluded_symbols () =
    let ex = !exclude_symbols in
    set_exclude_symbols NameUtils.Set.empty;
    ex

  let restore_excluded_symbols ex = set_exclude_symbols ex

  (* replace the current env with the passed one.
     envs must be congruent - we measure length as a quick check,
     with a more thorough check on env merge/copy *)
  let update_env loc new_scopes =
    if List.length new_scopes != List.length (peek_env ()) then
      assert_false
        (spf
           "update_env %s: unequal length scope lists, old %d new %d "
           (string_of_aloc loc)
           (List.length new_scopes)
           (List.length (peek_env ())));

    scopes := new_scopes

  (* end of basic env API *)

  let global_any =
    [
      OrdinaryName "eval";
      OrdinaryName "arguments";
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
    let entry = Entry.new_var (Inferred t) ~loc ~provider:t ~state:State.Initialized in
    Scope.add_entry name entry global_scope;
    (global_scope, entry)

  let local_scope_entry_exists _ _ name =
    let name = OrdinaryName name in
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

  let promote_non_const cx name loc spec =
    if Reason.is_internal_name name then
      (None, spec)
    else
      let { Loc_env.var_info = { Env_api.scopes = info; ssa_values = values; providers; _ }; _ } =
        Context.environment cx
      in
      begin
        let error null_write =
          let null_write =
            Base.Option.map
              ~f:(fun null_loc -> Error_message.{ null_loc; initialized = ALoc.equal loc null_loc })
              null_write
          in
          Flow.add_output
            cx
            Error_message.(
              EInvalidDeclaration { declaration = mk_reason (RIdentifier name) loc; null_write })
        in
        match Invalidation_api.declaration_validity info values providers loc with
        | Invalidation_api.Valid -> ()
        | Invalidation_api.NotWritten -> error None
        | Invalidation_api.NullWritten null_loc -> error (Some null_loc)
      end;

      if spec <> Entry.ConstLike && Invalidation_api.is_const_like info values loc then
        (None, Entry.ConstLike)
      else if spec <> Entry.NotWrittenByClosure then
        let writes_by_closure = Invalidation_api.written_by_closure info values loc in
        if ALocSet.is_empty writes_by_closure then
          (None, Entry.NotWrittenByClosure)
        else
          (Some writes_by_closure, spec)
      else
        (None, spec)

  let mk_havoc cx name loc general spec =
    let providers =
      if Options.env_option_enabled (Context.env_mode cx) Options.ConstrainWrites then
        let ({ Loc_env.var_info = { Env_api.providers; _ }; _ } as env) = Context.environment cx in
        let providers =
          Env_api.Provider_api.providers_of_def providers loc
          |> Base.Option.value_map ~f:snd ~default:[]
          |> Base.List.map
               ~f:
                 (Fn.compose
                    (fun loc -> Base.Option.map ~f:(fun t -> (loc, t)) (Loc_env.find_write env loc))
                    Reason.aloc_of_reason)
          |> Base.Option.all
        in
        match providers with
        | None -> []
        | Some providers -> providers
      else
        []
    in

    let (writes_by_closure_opt, spec') = promote_non_const cx name loc spec in
    let closure_writes =
      match writes_by_closure_opt with
      | Some writes_by_closure ->
        let writes_by_closure_t =
          Tvar.mk_where cx (mk_reason (RIdentifier name) loc) (fun tvar ->
              Flow.flow_t cx (tvar, general))
        in
        let writes_by_closure_provider =
          if
            ALocSet.for_all
              (Base.List.mem ~equal:ALoc.equal (Base.List.map ~f:fst providers))
              writes_by_closure
          then
            let writes_by_closure_providers =
              Base.List.filter_map
                ~f:(fun (loc, t) ->
                  if ALocSet.mem loc writes_by_closure then
                    Some t
                  else
                    None)
                providers
            in
            match writes_by_closure_providers with
            | [] -> None
            | [t] -> Some t
            | t1 :: t2 :: ts -> Some (UnionT (mk_reason (RType name) loc, UnionRep.make t1 t2 ts))
          else
            None
        in

        Some (writes_by_closure, writes_by_closure_t, writes_by_closure_provider)
      | _ -> None
    in
    let provider =
      if Options.env_option_enabled (Context.env_mode cx) Options.ConstrainWrites then
        match providers with
        | [] -> VoidT.at loc (bogus_trust ())
        | [(_, t)] -> t
        | (_, t1) :: (_, t2) :: ts ->
          UnionT (mk_reason (RType name) loc, UnionRep.make t1 t2 (Base.List.map ~f:snd ts))
      else
        general
    in
    (spec', closure_writes, provider)

  let binding_error msg cx name entry loc =
    Flow.add_output cx (Error_message.EBindingError (msg, loc, name, entry))

  let already_bound_error = binding_error Error_message.ENameAlreadyBound

  let install_provider cx t name loc =
    match name with
    | OrdinaryName _name
      when Options.env_option_enabled (Context.env_mode cx) Options.ConstrainWrites ->
      let ({ Loc_env.var_info = { Env_api.providers; _ }; _ } as env) = Context.environment cx in
      if Env_api.Provider_api.is_provider providers loc then
        let t' = Loc_env.find_write env loc in
        Base.Option.iter ~f:(fun t' -> Flow_js.flow_t cx (t, t')) t'
    | _ -> ()

  let constrain_by_provider cx ~use_op t name loc =
    match name with
    | OrdinaryName ord_name
      when Options.env_option_enabled (Context.env_mode cx) Options.ConstrainWrites ->
      let ({ Loc_env.var_info = { Env_api.providers; scopes; _ }; _ } as env) =
        Context.environment cx
      in
      if not @@ Env_api.Provider_api.is_provider providers loc then
        let (fully_initialized, provider_locs) =
          Base.Option.value
            ~default:(false, [])
            (Env_api.Provider_api.providers_of_def providers loc)
        in
        if fully_initialized then
          (* If the variable is never fully initialized, we'll error elsewhere *)
          let providers =
            Base.List.map
              ~f:(Fn.compose (Loc_env.find_write env) Reason.aloc_of_reason)
              provider_locs
            |> Base.Option.all
          in
          let provider =
            match providers with
            | None
              (* We can have no providers when the only writes to a variable are in unreachable code *)
            | Some [] ->
              (* If we find an entry for the providers, but none that actually exist, its because this variable
                 was never assigned to. We treat this as undefined. We handle erroring on
                 these cases using a different approach (the error should be at the declaration,
                 not the assignment). *)
              None
            | Some [t] -> Some t
            | Some (t1 :: t2 :: ts) ->
              Some (UnionT (mk_reason (RIdentifier name) loc, UnionRep.make t1 t2 ts))
          in
          Base.Option.iter provider ~f:(fun provider ->
              match Scope_api.With_ALoc.(def_of_use_opt scopes loc) with
              | Some { Scope_api.With_ALoc.Def.locs = (declaration, _); _ } ->
                let use_op =
                  Frame
                    ( ConstrainedAssignment
                        { name = ord_name; declaration; providers = provider_locs },
                      use_op )
                in
                Context.add_constrained_write cx (t, UseT (use_op, provider))
              | None ->
                (* If there isn't a declaration for the variable, then it's a global, and we don't need to constrain it *)
                ())
    | _ -> ()

  let can_shadow cx name prev loc =
    Entry.(
      function
      (* vars can shadow other vars *)
      | (Var _, Var _) -> true
      (* nonpredicate declared functions can shadow each other, and any declared function can be shadowed by a function *)
      | (Let (FunctionBinding, _), Let (DeclaredFunctionBinding _, _))
      | ( Let (DeclaredFunctionBinding { predicate = false }, _),
          Let (DeclaredFunctionBinding { predicate = false }, _) ) ->
        true
      (* declared functions can't shadow other things *)
      | (Let (DeclaredFunctionBinding _, _), (Var _ | Let (FunctionBinding, _))) -> false
      (* In JS, funcs/vars can shadow other funcs/vars -- only in var scope. However, we want to
         ban this pattern in Flow, so we raise an already_bound_error BUT don't abort the binding
         so that we can still check downstream things. *)
      | ( (Var _ | Let ((FunctionBinding | DeclaredFunctionBinding _), _)),
          (Var _ | Let ((FunctionBinding | DeclaredFunctionBinding _), _)) ) ->
        already_bound_error cx name prev loc;
        true
      (* vars can shadow function params, but we should raise an error if they are constlike params *)
      | (Var _, Let (ParamBinding, _)) -> true
      | (Var _, Const ConstParamBinding) ->
        already_bound_error cx name prev loc;
        true
      | _ -> false)

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
              (match (entry, prev) with
              (* good shadowing leaves existing entry, unifies with new *)
              | (Value e, Value p)
                when can_shadow cx name prev loc (Entry.kind_of_value e, Entry.kind_of_value p) ->
                (* TODO currently we don't step on specific. shouldn't we? *)
                Flow.unify cx (Entry.general_of_value p) (Entry.general_of_value e)
              (* bad shadowing is a binding error *)
              | _ -> already_bound_error cx name prev loc))
          (* shadowing in a lex scope is always an error *)
          | LexScope -> already_bound_error cx name prev loc))
    in
    if not (is_excluded name) then loop !scopes

  (* bind class entry *)
  let bind_class
      cx
      class_id
      class_private_fields
      class_private_static_fields
      class_private_methods
      class_private_static_methods =
    bind_entry
      cx
      (internal_name "class")
      (Entry.new_class
         class_id
         class_private_fields
         class_private_static_fields
         class_private_methods
         class_private_static_methods)
      ALoc.none

  (* bind var entry *)
  let bind_var_to_name ?(state = State.Declared) cx name t loc =
    let (spec, closure_writes, provider) =
      mk_havoc cx name loc (TypeUtil.type_t_of_annotated_or_inferred t) Entry.Havocable
    in
    bind_entry cx name (Entry.new_var t ~loc ~state ~spec ?closure_writes ~provider) loc

  let bind_var ?state cx name t loc = bind_var_to_name ?state cx (OrdinaryName name) t loc

  (* bind let entry *)
  let bind_let ?(state = State.Undeclared) cx name t loc =
    let (spec, closure_writes, provider) =
      mk_havoc
        cx
        (OrdinaryName name)
        loc
        (TypeUtil.type_t_of_annotated_or_inferred t)
        Entry.Havocable
    in
    bind_entry
      cx
      (OrdinaryName name)
      (Entry.new_let t ~loc ~state ~spec ?closure_writes ~provider)
      loc

  (* bind implicit let entry *)
  let bind_implicit_let ?(state = State.Undeclared) kind cx name t loc =
    let (spec, closure_writes, provider) = mk_havoc cx name loc t Entry.Havocable in
    begin
      match (state, kind) with
      | (State.Initialized, Entry.ParamBinding) ->
        (* If this variable starts off initialized (which is currently only the case with
           parameters), then init_entry does not need to be called later, which is where normally providers are installed. *)
        install_provider cx t name loc
      | _ -> ()
    end;
    bind_entry
      cx
      name
      (Entry.new_let (Inferred t) ~kind ~loc ~state ~spec ?closure_writes ~provider)
      loc

  let bind_fun ?(state = State.Declared) = bind_implicit_let ~state Entry.FunctionBinding

  (* bind const entry *)
  let bind_const ?(state = State.Undeclared) cx name t loc =
    bind_entry cx (OrdinaryName name) (Entry.new_const t ~loc ~state) loc

  let bind_import cx name t loc = bind_entry cx (OrdinaryName name) (Entry.new_import t ~loc) loc

  (* bind implicit const entry *)
  let bind_implicit_const ?(state = State.Undeclared) kind cx name t loc =
    bind_entry cx (OrdinaryName name) (Entry.new_const (Inferred t) ~kind ~loc ~state) loc

  (* bind type entry *)
  let bind_type ?(state = State.Declared) cx name t loc =
    bind_entry cx (OrdinaryName name) (Entry.new_type t ~loc ~state) loc

  let bind_import_type cx name t loc =
    bind_entry cx (OrdinaryName name) (Entry.new_import_type t ~loc) loc

  (* vars coming from 'declare' statements are preinitialized *)
  let bind_declare_var cx name t = bind_var_to_name ~state:State.Initialized cx name (Annotated t)

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
    fun cx ~predicate name t loc ->
      if not (is_excluded name) then
        let scope = peek_scope () in
        match Scope.get_entry name scope with
        | None ->
          let (spec, closure_writes, provider) = mk_havoc cx name loc t Entry.Havocable in
          let entry =
            Entry.new_let
              (Inferred t)
              ~kind:(Entry.DeclaredFunctionBinding { predicate })
              ~loc
              ~state:State.Initialized
              ~spec
              ~provider
              ?closure_writes
          in
          Scope.add_entry name entry scope
        | Some prev ->
          Entry.(
            (match prev with
            | Value v
              when can_shadow
                     cx
                     name
                     prev
                     loc
                     ( Let (DeclaredFunctionBinding { predicate }, Havocable (*doesnt matter *)),
                       Entry.kind_of_value v ) ->
              let entry =
                Value
                  {
                    v with
                    value_state = State.Initialized;
                    specific = update_type v.specific t;
                    general = update_general_type v.general t;
                    provider = update_type v.provider t;
                  }
              in
              Scope.add_entry name entry scope
            | _ ->
              (* declare function shadows some other kind of binding *)
              already_bound_error cx name prev loc))

  let same_kind k1 k2 =
    let open Entry in
    match (k1, k2) with
    | (Var _, Var _) -> true
    | (Let (kind1, _), Let (kind2, _)) -> kind1 = kind2
    | (Const kind1, Const kind2) -> kind1 = kind2
    | _ -> false

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
        | Value v
          when same_kind (Entry.kind_of_value v) kind && Entry.state_of_value v = State.Undeclared
          ->
          let new_entry = Value { v with value_state = State.Declared } in
          Scope.add_entry name new_entry scope
        | _ -> already_bound_error cx name entry loc)

  let declare_let = declare_value_entry Entry.(Let (LetVarBinding, Havocable))

  let declare_implicit_let kind = declare_value_entry Entry.(Let (kind, Havocable))

  let declare_const = declare_value_entry Entry.(Const ConstVarBinding)

  let declare_implicit_const kind = declare_value_entry (Entry.Const kind)

  let initialized_value_entry specific v =
    Entry.Value { v with Entry.value_state = State.Initialized; specific }

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
            Value ({ Entry.kind = Let _; value_state = State.Undeclared | State.Declared; _ } as v)
          )
        | ( Const _,
            Value
              ({ Entry.kind = Const _; value_state = State.Undeclared | State.Declared; _ } as v) )
          ->
          Changeset.Global.change_var (scope.id, name, Changeset.Write);
          let general = TypeUtil.type_t_of_annotated_or_inferred v.general in
          if specific != general then Flow.flow cx (specific, UseT (use_op, general));

          (* note that annotation supercedes specific initializer type *)
          let specific =
            if has_anno then
              general
            else begin
              constrain_by_provider cx ~use_op specific name loc;
              specific
            end
          in
          install_provider cx specific name loc;

          let new_entry = initialized_value_entry specific v in
          Scope.add_entry name new_entry scope
        | _ ->
          (* Incompatible or non-redeclarable new and previous entries.
             We will have already issued an error in `bind_value_entry`,
             so we can prune this case here. *)
          ())

  let init_var = init_value_entry Entry.(Var Havocable)

  let init_let = init_value_entry Entry.(Let (LetVarBinding, Havocable))

  let init_implicit_let kind = init_value_entry Entry.(Let (kind, Havocable))

  let init_fun = init_implicit_let ~has_anno:false Entry.FunctionBinding

  let init_const = init_value_entry Entry.(Const ConstVarBinding)

  let init_implicit_const kind = init_value_entry Entry.(Const kind)

  (* update type alias to reflect initialization in code *)
  let init_type cx name type_ loc =
    let name = OrdinaryName name in
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
    let name = OrdinaryName name in
    if not (is_excluded name) then
      Entry.(
        let (scope, entry) = find_entry cx name loc in
        match entry with
        | Value ({ Entry.kind = Var _; _ } as v)
        | Value
            ({ Entry.kind = Let _ | Const _; value_state = State.(Undeclared | Declared); _ } as v)
          ->
          Changeset.Global.change_var (scope.id, name, Changeset.Write);
          install_provider cx (TypeUtil.type_t_of_annotated_or_inferred v.general) name loc;
          let entry =
            initialized_value_entry (TypeUtil.type_t_of_annotated_or_inferred v.general) v
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
          Entry.kind = Var _ | Let (LetVarBinding, _);
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
      | Let ((FunctionBinding | DeclaredFunctionBinding _), _) ->
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
      | Class _ ->
        assert_false "Internal Error: Classes should only be read using get_class_entries"
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
  let get_var ?(lookup_mode = ForValue) cx name loc =
    read_entry ~lookup_mode ~specific:true ?desc:None cx (OrdinaryName name) loc

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
  let get_var_declared_type ?(lookup_mode = ForValue) cx name loc =
    read_entry ~lookup_mode ~specific:false ?desc:None cx name loc

  (* Unify declared type with another type. This is useful for allowing forward
     references in declared types to other types declared later in scope. *)
  let unify_declared_type ?(lookup_mode = ForValue) cx name _loc t =
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

  let is_global_var _cx name _ =
    let rec loop = function
      | [] -> true
      | scope :: scopes ->
        (match Scope.get_entry (OrdinaryName name) scope with
        | Some _ -> Scope.is_global scope
        | None -> loop scopes)
    in
    loop !scopes

  (* get var type, with given location used in type's reason *)
  let var_ref ?(lookup_mode = ForValue) cx ?desc name loc =
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

     We also ban such reassignments in non-exported functions and classes in order to
     allow their types to be eagerly resolved.
  *)
  let check_let_bound_reassignment op cx name entry loc =
    let open Entry in
    match (op, entry) with
    | ( Changeset.Write,
        Value
          {
            Entry.kind =
              Let
                Entry.
                  ( ((ClassNameBinding | FunctionBinding | DeclaredFunctionBinding _) as
                    binding_kind),
                    _ );
            value_declare_loc;
            _;
          } ) ->
      let reason = mk_reason (RType name) value_declare_loc in
      Flow.add_output
        cx
        Error_message.(EAssignConstLikeBinding { loc; definition = reason; binding_kind })
    | _ -> ()

  (* helper: update let or var entry *)
  let update_var op cx ~use_op name specific loc =
    let (scope, entry) = find_entry cx name loc in
    check_let_bound_reassignment op cx name entry loc;
    Entry.(
      match entry with
      | Value ({ Entry.kind = Let _ as kind; value_state = State.Undeclared; _ } as v)
        when (not (allow_forward_ref kind)) && same_activation scope ->
        tdz_error cx name loc v;
        None
      | Value ({ Entry.kind = Let _ | Var _; closure_writes; general; _ } as v) ->
        let change = (scope.id, name, op) in
        Changeset.Global.change_var change;
        let use_op =
          match op with
          | Changeset.Write -> use_op
          | Changeset.Refine -> use_op
          | Changeset.Read -> unknown_use
          (* this is impossible *)
        in
        begin
          match closure_writes with
          | Some (writes_by_closure, t, _) when ALocSet.mem loc writes_by_closure ->
            Flow.flow cx (specific, UseT (use_op, t))
          | _ -> Flow.flow cx (specific, UseT (use_op, Entry.general_of_value v))
        end;

        install_provider cx specific name loc;

        begin
          match (general, op) with
          | (Inferred _, Changeset.Write) -> constrain_by_provider cx ~use_op specific name loc
          | _ -> ()
        end;

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
  let set_var cx ~use_op name t loc =
    update_var Changeset.Write cx ~use_op (OrdinaryName name) t loc |> ignore

  let set_internal_var cx name t loc =
    update_var Changeset.Write cx ~use_op:unknown_use (internal_name name) t loc |> ignore

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
      | _ ->
        assert_false
          (spf
             "refine_const called on %s %s"
             (Entry.string_of_kind entry)
             (display_string_of_name name)))

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
    let envs = Base.List.remove_consecutive_duplicates ~equal:( == ) envs in
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
               && (child1.value_state >= State.MaybeInitialized
                  || child2.value_state >= State.MaybeInitialized) ->
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
            (spf
               "merge_env %s: type alias %s found in changelist"
               (string_of_aloc loc)
               (display_string_of_name name))
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
               (display_string_of_name name)
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
            (spf
               "copy_env %s: type alias %s found in changelist"
               (string_of_aloc loc)
               (display_string_of_name name))
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
               (display_string_of_name name)
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
      |> Changeset.iter_type_updates
           (copy_entry cx loc (env1, env2))
           (copy_refi cx loc (env1, env2))

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

  let havoc_local_refinements ?(all = false) cx =
    iter_scopes (fun scope ->
        Scope.update_entries
          (fun name entry ->
            let entry' =
              if all then
                Entry.havoc name entry
              else
                Entry.havoc
                  ~on_call:(fun specific t general ->
                    if
                      specific == general
                      (* We already know t ~> general, so specific | t = general *)
                    then
                      general
                    else if Options.env_option_enabled (Context.env_mode cx) Options.ConstrainWrites
                    then (
                      let tvar = Tvar.mk cx (reason_of_t t) in
                      Flow.flow cx (specific, UseT (Op (Internal WidenEnv), tvar));
                      Flow.flow cx (general, UseT (Op (Internal WidenEnv), tvar));
                      tvar
                    ) else
                      let (_, tvar_t) = open_tvar t in
                      let (lazy constraints_t) = Context.find_graph cx tvar_t in
                      match (specific, constraints_t) with
                      | (OpenT (_, tvar_specific), Constraint.Unresolved bounds_t)
                        when IMap.mem tvar_specific bounds_t.Constraint.uppertvars ->
                        (* If t ~> specific, then specific | t = specific *)
                        specific
                      | _ ->
                        (* Compute specific | t = tvar where specific, t ~> tvar ~> general *)
                        let tvar = Tvar.mk cx (reason_of_t t) in
                        Flow.flow cx (specific, UseT (Op (Internal WidenEnv), tvar));
                        Flow.flow cx (t, UseT (Op (Internal WidenEnv), tvar));
                        Flow.flow cx (tvar, UseT (Op (Internal WidenEnv), general));
                        tvar)
                  name
                  entry
            in
            (if entry' != entry then
              let entry_ref = (scope.id, name, Changeset.Write) in
              Changeset.(if Global.is_active () then Global.change_var entry_ref));
            entry')
          scope)

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

  let set_expr k l t1 t2 = add_heap_refinement Changeset.Write k l t1 t2 |> ignore

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
        let reason = loc |> mk_reason (RStringLit (OrdinaryName str)) in
        let l = DefT (reason, bogus_trust (), StrT (Literal (Some b, OrdinaryName str))) in
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
      | ((OrdinaryName _ as name), []) ->
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
    update_env loc new_env;
    let _ = refine_with_preds cx loc preds orig_types in
    let result = f () in
    let newset = Changeset.Global.merge oldset in
    merge_env cx loc (orig_env, orig_env, new_env) newset;
    update_env loc orig_env;

    result

  let new_env = false
end
