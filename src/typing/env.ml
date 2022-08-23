(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

  (* return current scope stack *)
  let peek_env () = !scopes

  (* return the value of f applied to topmost var scope in a scope list *)
  let rec top_var_scope = function
    | [] -> assert_false "empty scope list"
    | scope :: scopes ->
      (match scope.kind with
      | VarScope _ -> scope
      | _ -> top_var_scope scopes)

  (* get top var scope of current env *)
  let peek_var_scope () = top_var_scope (peek_env ())

  let in_toplevel_scope _ = Scope.is_toplevel (peek_var_scope ())

  let in_global_scope _ = Scope.is_global (peek_var_scope ())

  let var_scope_kind _ =
    let scope = peek_var_scope () in
    match scope.kind with
    | VarScope k -> k
    | _ -> assert_false "peek_var_scope returns a VarScope"

  (* true iff scope is var scope with the given kind *)
  let is_func_kind k scope =
    match scope.kind with
    | VarScope func_kind -> func_kind = k
    | _ -> false

  let in_async_scope _ =
    match var_scope_kind () with
    | Async
    | AsyncGenerator ->
      true
    | _ -> false

  let in_predicate_scope _ = is_func_kind Predicate (peek_var_scope ())

  (* whole env *)

  (* push a new var scope into the environment.
     current env state is stored to cx under scope id *)
  (* TODO maintain changelist here too *)
  let push_var_scope _ scope =
    (match scope.kind with
    | VarScope _ -> ()
    | _ -> assert_false "push_var_scope on non-var scope");
    scopes := scope :: !scopes;
    (* below is ignored, only needed for matching types used by the new env *)
    Ordinary

  (* --- *)

  (* pop a var scope from the environment.
     note: may require popping accumulated lex scopes *)
  let pop_var_scope _ _ =
    match !scopes with
    | { kind = VarScope _; _ } :: tail_scopes -> scopes := tail_scopes
    | [] -> assert_false "empty scope list"
    | _ -> assert_false "top scope is non-var"

  (* push a lex scope but NOT a changeset
     (which is 1-1 with var scopes). *)
  let push_lex_scope () =
    let scope = Scope.fresh_lex () in
    scopes := scope :: !scopes

  let pop_lex_scope () =
    match !scopes with
    | { kind = LexScope; _ } :: tail_scopes ->
      (* pop *)
      scopes := tail_scopes
    | [] -> assert_false "empty scope list"
    | _ -> assert_false "top scope is non-lex"

  let in_lex_scope f =
    push_lex_scope ();
    let result = f () in
    pop_lex_scope ();
    result

  let in_class_scope _ _ = in_lex_scope

  (* depth of current env *)
  let env_depth () = List.length !scopes

  (* strip the given number of scopes from top of env *)
  let trunc_env =
    let rec trunc = function
      | (0, scopes) -> scopes
      | (_, []) -> assert_false "trunc_env: scopes underflow"
      | (n, _ :: scopes) -> trunc (n - 1, scopes)
    in
    fun depth ->
      let cur = !scopes in
      scopes := trunc (List.length cur - depth, cur)

  (* initialize a new environment (once per module) *)
  let init_env ?(exclude_syms = NameUtils.Set.empty) cx _ module_scope =
    set_exclude_symbols exclude_syms;
    let global_scope = Scope.fresh ~var_scope_kind:Global () in
    let (_ : Scope.var_scope_kind) = push_var_scope cx global_scope in
    let (_ : Scope.var_scope_kind) = push_var_scope cx module_scope in
    ()

  (* end of basic env API *)

  let global_any =
    [
      OrdinaryName "eval";
      OrdinaryName "arguments";
      (* For `switch` statements not in a function body, so we don't get an error. *)
      internal_name "maybe_exhaustively_checked";
    ]

  let global_lexicals = [internal_name "super"; internal_name "this"]

  let get_global_value_type cx name reason =
    match Context.global_value_cache_find_opt cx name with
    | Some t -> t
    | None ->
      let t = Flow.get_builtin cx name reason in
      Context.add_global_value_cache_entry cx name t;
      t

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
        get_global_value_type cx name reason
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

  let get_class_entries _ =
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

  let valid_declaration_check cx name loc =
    let { Loc_env.var_info = { Env_api.scopes = info; ssa_values = values; providers; _ }; _ } =
      Context.environment cx
    in
    let error null_write =
      let null_write =
        Base.Option.map
          ~f:(fun null_loc -> Error_message.{ null_loc; initialized = ALoc.equal loc null_loc })
          null_write
      in
      Flow.add_output
        cx
        Error_message.(
          EInvalidDeclaration { declaration = mk_reason (RIdentifier name) loc; null_write }
        )
    in
    match Invalidation_api.declaration_validity info values providers loc with
    | Invalidation_api.Valid -> ()
    | Invalidation_api.NotWritten -> error None
    | Invalidation_api.NullWritten null_loc -> error (Some null_loc)

  let promote_non_const cx name loc spec =
    if Reason.is_internal_name name then
      (None, spec)
    else
      let { Loc_env.var_info = { Env_api.scopes = info; ssa_values = values; _ }; _ } =
        Context.environment cx
      in
      valid_declaration_check cx name loc;
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
    let providers = [] in

    let (writes_by_closure_opt, spec') = promote_non_const cx name loc spec in
    let closure_writes =
      match writes_by_closure_opt with
      | Some writes_by_closure ->
        let writes_by_closure_t =
          Tvar.mk_where cx (mk_reason (RIdentifier name) loc) (fun tvar ->
              Flow.flow_t cx (tvar, general)
          )
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
    let provider = general in
    (spec', closure_writes, provider)

  let binding_error msg cx name entry loc =
    Flow.add_output cx (Error_message.EBindingError (msg, loc, name, Entry.entry_loc entry))

  let already_bound_error = binding_error Error_message.ENameAlreadyBound

  let is_provider cx id_loc =
    let { Loc_env.var_info = { Env_api.providers; _ }; _ } = Context.environment cx in
    Env_api.Provider_api.is_provider providers id_loc

  let constrain_by_provider _cx ~use_op:_ _t _name _loc = ()

  let can_shadow cx name prev loc =
    Entry.(
      function
      (* vars can shadow other vars *)
      | (Var _, Var _) -> true
      (* nonpredicate declared functions can shadow each other, and any declared function can be shadowed by a function *)
      | (Let (FunctionBinding, _), Let (DeclaredFunctionBinding _, _))
      | ( Let (DeclaredFunctionBinding { predicate = false }, _),
          Let (DeclaredFunctionBinding { predicate = false }, _)
        ) ->
        true
      (* declared functions can't shadow other things *)
      | (Let (DeclaredFunctionBinding _, _), (Var _ | Let (FunctionBinding, _))) -> false
      (* In JS, funcs/vars can shadow other funcs/vars -- only in var scope. However, we want to
         ban this pattern in Flow, so we raise an already_bound_error BUT don't abort the binding
         so that we can still check downstream things. *)
      | ( (Var _ | Let ((FunctionBinding | DeclaredFunctionBinding _), _)),
          (Var _ | Let ((FunctionBinding | DeclaredFunctionBinding _), _))
        ) ->
        already_bound_error cx name prev loc;
        true
      (* vars can shadow function params, but we should raise an error if they are constlike params *)
      | (Var _, Let (ParamBinding, _)) -> true
      | (Var _, Const ConstParamBinding) ->
        already_bound_error cx name prev loc;
        true
      | _ -> false
    )

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
            | _ -> loop scopes)
          )
        (* some rebindings are allowed, but usually an error *)
        | Some prev ->
          (match scope.kind with
          (* specifically a var scope allows some shadowing *)
          | VarScope _ ->
            let is_var_redeclaration = function
              | (Entry.Var _, Entry.Var _) -> Entry.entry_loc prev <> loc
              | _ -> false
            in
            Entry.(
              (match (entry, prev) with
              | (Value e, Value p)
                when is_var_redeclaration (Entry.kind_of_value e, Entry.kind_of_value p) ->
                (* We ban var redeclaration on top of other JS illegal rebinding rules. *)
                binding_error Error_message.EVarRedeclaration cx name prev loc
              | (Value e, Value p)
                when can_shadow cx name prev loc (Entry.kind_of_value e, Entry.kind_of_value p) ->
                (* TODO currently we don't step on specific. shouldn't we? *)
                Flow.unify cx (Entry.general_of_value p) (Entry.general_of_value e)
              (* bad shadowing is a binding error *)
              | _ -> already_bound_error cx name prev loc)
            )
          (* shadowing in a lex scope is always an error *)
          | LexScope -> already_bound_error cx name prev loc))
    in
    if not (is_excluded name) then loop !scopes

  (* bind class entry *)
  let bind_class cx x = bind_entry cx (internal_name "class") (Entry.Class x) ALoc.none

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

  let bind_function_this _ _ _ = ()

  let bind_class_instance_this _ _ _ = ()

  let bind_class_static_this _ _ _ = ()

  let bind_class_instance_super _ _ _ = ()

  let bind_class_static_super _ _ _ = ()

  (* bind implicit let entry *)
  let bind_implicit_let ?(state = State.Undeclared) kind cx name t loc =
    let (spec, closure_writes, provider) =
      mk_havoc cx name loc (TypeUtil.type_t_of_annotated_or_inferred t) Entry.Havocable
    in
    bind_entry cx name (Entry.new_let t ~kind ~loc ~state ~spec ?closure_writes ~provider) loc

  let bind_fun ?(state = State.Declared) cx name t =
    bind_implicit_let ~state Entry.FunctionBinding cx name (Inferred t)

  (* bind const entry *)
  let bind_const ?(state = State.Undeclared) cx name t loc =
    bind_entry cx (OrdinaryName name) (Entry.new_const t ~loc ~state) loc

  (* bind implicit const entry *)
  let bind_implicit_const ?(state = State.Undeclared) kind cx name t loc =
    bind_entry cx (OrdinaryName name) (Entry.new_const t ~kind ~loc ~state) loc

  let bind_this_tparam ~state cx t loc =
    bind_entry cx (OrdinaryName "this") (Entry.new_type t ~loc ~state) loc

  let bind_class_self_type cx _class_loc self class_t_internal = Flow.unify cx self class_t_internal

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
              (Annotated t)
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
                       Entry.kind_of_value v
                     ) ->
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
              already_bound_error cx name prev loc)
          )

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
            Value ({ Entry.kind = Const _; value_state = State.Undeclared | State.Declared; _ } as v)
          ) ->
          let general = TypeUtil.type_t_of_annotated_or_inferred v.general in
          if specific != general then Flow.flow cx (specific, UseT (use_op, general));

          (* note that annotation supercedes specific initializer type *)
          begin
            match v.general with
            | Annotated _ -> ()
            | Inferred _ ->
              (* This is checked separately from has_anno because this variable
                 may have been previously declared with an annotation even if this
                 declaration lacks one; e.g. `var x: number; var x = "hello"` *)
              constrain_by_provider cx ~use_op specific name loc
          end;

          let specific =
            if has_anno then
              general
            else
              specific
          in

          let new_entry = initialized_value_entry specific v in
          Scope.add_entry name new_entry scope
        | _ ->
          (* Incompatible or non-redeclarable new and previous entries.
             We will have already issued an error in `bind_value_entry`,
             so we can prune this case here. *)
          ()
      )

  let init_var = init_value_entry Entry.(Var Havocable)

  let init_let = init_value_entry Entry.(Let (LetVarBinding, Havocable))

  let init_implicit_let kind = init_value_entry Entry.(Let (kind, Havocable))

  let init_fun = init_implicit_let ~has_anno:false Entry.FunctionBinding

  let init_const = init_value_entry Entry.(Const ConstVarBinding)

  let init_implicit_const kind = init_value_entry Entry.(Const kind)

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
      | { specific; general; _ } -> (specific, general)
    )

  (* emit tdz error for value entry *)
  let tdz_error cx name loc v =
    Entry.(
      (* second clause of error message is due to switch scopes *)
      let msg = Error_message.EReferencedBeforeDeclaration in
      binding_error msg cx name (Value v) loc
    )

  (* helper for read/write tdz checks *)
  (* functions are block-scoped, but also hoisted. forward ref ok *)
  let allow_forward_ref =
    Scope.Entry.(
      function
      | Var _
      | Let ((FunctionBinding | DeclaredFunctionBinding _), _) ->
        true
      | _ -> false
    )

  (* helper - does semantic checking and returns entry type *)
  let read_entry ~lookup_mode ~specific cx name ?desc loc =
    let (scope, entry) = find_entry cx name ?desc loc in
    Entry.(
      match entry with
      | Type { type_binding_kind; _ } when lookup_mode != ForType ->
        let imported =
          match type_binding_kind with
          | ImportTypeBinding -> true
          | TypeBinding -> false
        in
        let msg =
          Error_message.ETypeInValuePosition { imported; name = display_string_of_name name }
        in
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
          let (s, g) = value_entry_types ~lookup_mode scope v in
          if specific then
            s
          else
            TypeUtil.type_t_of_annotated_or_inferred g)
    )

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

  let get_internal_var cx name loc = query_var cx (internal_name name) loc

  let get_module_exports cx loc = get_internal_var cx "exports" loc

  (* get var's general type - for annotated vars, this is the
     annotated type, and for others it's the union of all
     types assigned to the var throughout its lifetime.
  *)
  let get_var_declared_type ?(lookup_mode = ForValue) ?is_declared_function:_ cx name loc =
    read_entry ~lookup_mode ~specific:false ?desc:None cx name loc

  let constraining_type ~default _cx _name _loc = default

  (* Unify declared type with another type. This is useful for allowing forward
     references in declared types to other types declared later in scope. *)
  let unify_declared_type ?(lookup_mode = ForValue) ?(is_func = false) cx name loc t =
    Entry.(
      (* If name_already_bound is true, then this is already a [name-already-bound]
       * error. In that case we don't need to unify with the general type. *)
      let name_already_bound v =
        match v.Entry.kind with
        | Let ((ClassNameBinding | FunctionBinding), _) -> v.value_assign_loc <> loc
        | Let (DeclaredFunctionBinding _, _) when not is_func ->
          (* Multiple declare functions followed by a function declaration is okay. *)
          v.value_assign_loc <> loc
        | _ -> false
      in
      match get_current_env_entry name with
      | Some (Value v) when lookup_mode = ForValue ->
        if not (name_already_bound v) then Flow.unify cx t (general_of_value v)
      | Some entry when lookup_mode <> ForValue -> Flow.unify cx t (Entry.declared_type entry)
      | _ -> ()
    )

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
        | Some (Value { Entry.kind = Let (FunctionBinding, _); _ }) ->
          (* This is already a 'name-already-bound' error. No need to unify types. *)
          ()
        | Some (Value v) -> Flow.unify cx t (find_type aloc (general_of_value v))
        | _ -> ()
      )

  let read_declared_type ?lookup_mode ?is_func cx name reason loc =
    Tvar.mk_where cx reason (unify_declared_type ?lookup_mode ?is_func cx name loc)

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

  let init_import ~lookup_mode cx name loc t =
    let t_generic = get_var_declared_type ~lookup_mode cx name loc in
    Flow.unify cx t t_generic

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
  let has_illegal_let_bound_reassignment op cx name entry loc =
    let open Entry in
    match (op, entry) with
    | ( Changeset.Write,
        Value
          {
            Entry.kind =
              Let
                Entry.
                  ( ( (ClassNameBinding | FunctionBinding | DeclaredFunctionBinding _) as
                    binding_kind
                    ),
                    _
                  );
            value_declare_loc;
            _;
          }
      ) ->
      let reason = mk_reason (RType name) value_declare_loc in
      Flow.add_output
        cx
        Error_message.(EAssignConstLikeBinding { loc; definition = reason; binding_kind });
      true
    | _ -> false

  (* helper: update let or var entry *)
  let update_var op cx ~use_op name specific loc =
    let (scope, entry) = find_entry cx name loc in
    if has_illegal_let_bound_reassignment op cx name entry loc then
      None
    else
      Entry.(
        match entry with
        | Value ({ Entry.kind = Let _ as kind; value_state = State.Undeclared; _ } as v)
          when (not (allow_forward_ref kind)) && same_activation scope ->
          tdz_error cx name loc v;
          None
        | Value ({ Entry.kind = Let _ | Var _; closure_writes; general; _ } as v) ->
          let change = (scope.id, name, op) in
          begin
            match (closure_writes, op) with
            | (_, Changeset.Refine) -> ()
            | (_, Changeset.Read) -> assert_false "read op during variable update"
            | (Some (writes_by_closure, t, _), Changeset.Write)
              when ALocSet.mem loc writes_by_closure ->
              Flow.flow cx (specific, UseT (use_op, t))
            | (_, Changeset.Write) ->
              Flow.flow cx (specific, UseT (use_op, Entry.general_of_value v))
          end;

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
        | Class _ -> assert_false "Internal error: update_var called on Class"
      )

  (* update var by direct assignment *)
  let set_var cx ~use_op name t loc =
    update_var Changeset.Write cx ~use_op (OrdinaryName name) t loc |> ignore

  let set_internal_var cx name t loc =
    update_var Changeset.Write cx ~use_op:unknown_use (internal_name name) t loc |> ignore

  let set_module_exports cx loc t = set_internal_var cx "exports" t loc

  (* The protocol around havoc has changed a few times.
     The following function used to do most of the work, but is now subsumed by
     havoc_ctx, and is now used only to clear the environment when looking at
     a function body. Also see below. *)

  let add_heap_refinement op key refi_loc refined original =
    let refi = { refi_loc; refined; original } in
    let (base, _) = key in
    let (scope, _) = find_entry_in_var_scope base in
    let change = (scope.id, key, op) in
    Scope.add_refi key refi scope;
    change

  let set_expr _cx k l ~refined:t1 ~original:t2 =
    add_heap_refinement Changeset.Write k l t1 t2 |> ignore

  let record_expression_type_if_needed _ _ _ _ = ()

  let discriminant_after_negated_cases cx _switch_loc refinement_key_opt discriminant =
    match discriminant with
    | (loc, Flow_ast.Expression.Identifier (_, { Flow_ast.Identifier.name; _ })) ->
      Some (query_var cx (OrdinaryName name) loc)
    | _ ->
      refinement_key_opt
      |> Base.Option.bind ~f:get_current_env_refi
      |> Base.Option.map ~f:(fun refi -> refi.Scope.refined)

  let get_next cx loc = get_internal_var cx "next" loc

  let init_class_self_type cx _loc reason = Tvar.mk cx reason

  let init_declare_module_synthetic_module_exports cx ~export_type loc reason module_scope =
    match Context.module_kind cx with
    | Module_info.ES _ -> ()
    | Module_info.CJS clobbered ->
      Scope.(
        Entry.(
          let () =
            match clobbered with
            | Some _ -> ()
            | None ->
              let props =
                NameUtils.Map.fold
                  (fun x entry acc ->
                    match entry with
                    | Value { specific; _ } when x <> internal_name "exports" ->
                      let loc = Some (entry_loc entry) in
                      Properties.add_field x Polarity.Positive loc specific acc
                    | _ -> acc)
                  module_scope.entries
                  NameUtils.Map.empty
              in
              let proto = ObjProtoT reason in
              let t = Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~props proto in
              set_module_exports cx loc t
          in
          NameUtils.Map.iter
            (fun x entry ->
              match entry with
              | Type { type_; type_binding_kind = TypeBinding; _ } ->
                (* TODO we may want to provide a location here *)
                export_type cx x None type_
              | Type { type_binding_kind = ImportTypeBinding; _ }
              | Value _
              | Class _ ->
                ())
            module_scope.entries
        )
      )

  let init_builtins_from_libdef cx module_scope =
    (module_scope
    |> Scope.(
         iter_entries Entry.((fun name entry -> Flow_js.set_builtin cx name (actual_type entry)))
       )
    );
    NameUtils.Map.keys Scope.(module_scope.entries)
end
