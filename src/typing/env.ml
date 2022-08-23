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

  val push_var_scope : Context.t -> Scope.t -> Scope.var_scope_kind

  val pop_var_scope : Context.t -> Scope.var_scope_kind -> unit

  val in_lex_scope : (unit -> 'a) -> 'a

  val env_depth : unit -> int

  val trunc_env : int -> unit

  val init_env : ?exclude_syms:NameUtils.Set.t -> Context.t -> ALoc.t -> Scope.t -> unit

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

  let get_global_value_type cx name reason =
    match Context.global_value_cache_find_opt cx name with
    | Some t -> t
    | None ->
      let t = Flow.get_builtin cx name reason in
      Context.add_global_value_cache_entry cx name t;
      t

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
end
