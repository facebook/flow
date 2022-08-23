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
module LookupMode = Env_sig.LookupMode

module type S = sig
  type scope

  type t = scope list

  val in_toplevel_scope : Context.t -> bool

  val in_global_scope : Context.t -> bool

  val var_scope_kind : Context.t -> Scope.var_scope_kind

  val in_async_scope : Context.t -> bool

  val in_predicate_scope : Context.t -> bool

  val get_global_value_type : Context.t -> Reason.name -> Reason.t -> Type.t

  val find_entry :
    Context.t -> Reason.name -> ?desc:Reason.reason_desc -> ALoc.t -> Scope.t * Entry.t

  val push_var_scope : Context.t -> Scope.t -> Scope.var_scope_kind

  val pop_var_scope : Context.t -> Scope.var_scope_kind -> unit

  val in_lex_scope : (unit -> 'a) -> 'a

  val env_depth : unit -> int

  val trunc_env : int -> unit

  val init_env : ?exclude_syms:NameUtils.Set.t -> Context.t -> ALoc.t -> Scope.t -> unit

  (***)

  val bind_class : Context.t -> Type.class_binding -> unit

  val bind_implicit_let :
    ?state:State.t ->
    Entry.let_binding_kind ->
    Context.t ->
    Reason.name ->
    Type.annotated_or_inferred ->
    ALoc.t ->
    unit

  val bind_fun : ?state:State.t -> Context.t -> Reason.name -> Type.t -> ALoc.t -> unit

  val bind_declare_fun : Context.t -> predicate:bool -> Reason.name -> Type.t -> ALoc.t -> unit

  val init_var :
    Context.t -> use_op:Type.use_op -> Reason.name -> has_anno:bool -> Type.t -> ALoc.t -> unit

  val init_let :
    Context.t -> use_op:Type.use_op -> Reason.name -> has_anno:bool -> Type.t -> ALoc.t -> unit

  val init_implicit_let :
    Entry.let_binding_kind ->
    Context.t ->
    use_op:Type.use_op ->
    Reason.name ->
    has_anno:bool ->
    Type.t ->
    ALoc.t ->
    unit

  val init_fun : Context.t -> use_op:Type.use_op -> Reason.name -> Type.t -> ALoc.t -> unit

  val init_const :
    Context.t -> use_op:Type.use_op -> Reason.name -> has_anno:bool -> Type.t -> ALoc.t -> unit

  val init_implicit_const :
    Entry.const_binding_kind ->
    Context.t ->
    use_op:Type.use_op ->
    Reason.name ->
    has_anno:bool ->
    Type.t ->
    ALoc.t ->
    unit

  val get_var_declared_type :
    ?lookup_mode:LookupMode.t ->
    ?is_declared_function:bool ->
    Context.t ->
    Reason.name ->
    ALoc.t ->
    Type.t

  val constraining_type : default:Type.t -> Context.t -> Reason.name -> ALoc.t -> Type.t

  val unify_declared_type :
    ?lookup_mode:LookupMode.t ->
    ?is_func:bool ->
    Context.t ->
    Reason.name ->
    ALoc.t ->
    Type.t ->
    unit

  val read_declared_type :
    ?lookup_mode:LookupMode.t ->
    ?is_func:bool ->
    Context.t ->
    Reason.name ->
    Reason.t ->
    ALoc.t ->
    Type.t

  val unify_declared_fun_type : Context.t -> Reason.name -> ALoc.t -> Type.t -> unit

  val query_var :
    ?lookup_mode:LookupMode.t ->
    Context.t ->
    Reason.name ->
    ?desc:Reason.reason_desc ->
    ALoc.t ->
    Type.t

  val valid_declaration_check : Context.t -> Reason.name -> ALoc.t -> unit
end

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

module Env : S = struct
  open LookupMode

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

  (* scopes *)

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
    (global_scope, entry)

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

  (* initialization of entries happens during a preliminary pass through a
     scoped region of the AST (dynamic for hoisted things, lexical for
     lexical things). this leaves them in a germinal state which is
     then read and written during the main traversal of the AST *)

  (* helper: initialize entry for given key in top scope,
     dealing with various situations involving a preexisting entry
     (since multiple declarations - sometimes but not always erroneous -
     may appear in an AST)
  *)

  let bind_entry _cx _name _entry _loc = ()

  (* bind class entry *)
  let bind_class cx x = bind_entry cx (internal_name "class") (Entry.Class x) ALoc.none

  (* bind implicit let entry *)
  let bind_implicit_let ?(state = State.Undeclared) kind cx name t loc =
    let (spec, closure_writes, provider) =
      mk_havoc cx name loc (TypeUtil.type_t_of_annotated_or_inferred t) Entry.Havocable
    in
    bind_entry cx name (Entry.new_let t ~kind ~loc ~state ~spec ?closure_writes ~provider) loc

  let bind_fun ?(state = State.Declared) cx name t =
    bind_implicit_let ~state Entry.FunctionBinding cx name (Inferred t)

  (* bind entry for declare function *)
  let bind_declare_fun _cx ~predicate:_ _name _t _loc = ()

  (* helper - update var entry to reflect assignment/initialization *)
  (* note: here is where we understand that a name can be multiply var-bound
   * TODO: we started tracking annotations when variables are bound. Once we do
   * that at all binding sites this ~has_anno param can go away in favor of
   * looking up the annot in the environment *)
  let init_value_entry _kind _cx ~use_op:_ _name ~has_anno:_ _specific _loc = ()

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

  (* query var's specific type *)
  let query_var ?(lookup_mode = ForValue) = read_entry ~lookup_mode ~specific:true

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
end
