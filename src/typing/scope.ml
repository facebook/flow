(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Loc_collections

let mk_id = Reason.mk_id

(******************************************************************************)
(* Scopes                                                                     *)
(******************************************************************************)

(* these are basically owned by Env, but are here
   to break circularity between Env and Flow_js
*)

(* entry state *)
module State = struct
  type t =
    | Undeclared
    | Declared
    | MaybeInitialized
    | Initialized

  let to_int = function
    | Undeclared -> 0
    | Declared -> 1
    | MaybeInitialized -> 2
    | Initialized -> 3

  let to_string = function
    | Undeclared -> "Undeclared"
    | Declared -> "Declared"
    | MaybeInitialized -> "MaybeInitialized"
    | Initialized -> "Initialized"

  let compare x y = Stdlib.compare (to_int x) (to_int y)
end

(* entries for vars/lets, consts and types *)
module Entry = struct
  type value_kind =
    (* consts are either explicit bindings or e.g. const params *)
    | Const of const_binding_kind
    (* Some let bindings are explicit (like you wrote let x = 123) and some
     * are implicit (like class declarations). For implicit lets, we should
     * track why this is a let binding for better error messages *)
    | Let of (let_binding_kind * non_const_specialization)
    | Var of non_const_specialization

  and const_binding_kind =
    | ConstImportBinding
    | ConstParamBinding
    | ConstVarBinding
    | EnumNameBinding

  and let_binding_kind =
    | LetVarBinding
    | ClassNameBinding
    | CatchParamBinding
    | FunctionBinding
    | ParamBinding

  and non_const_specialization =
    | Havocable
    | NotWrittenByClosure
    | ConstLike

  let string_of_let_binding_kind = function
    | LetVarBinding -> "let"
    | ClassNameBinding -> "class"
    | CatchParamBinding -> "catch"
    | FunctionBinding -> "function"
    | ParamBinding -> "param"

  let string_of_value_kind = function
    | Const ConstImportBinding -> "import"
    | Const ConstParamBinding -> "const param"
    | Const ConstVarBinding -> "const"
    | Const EnumNameBinding -> "enum"
    | Let (kind, _) -> string_of_let_binding_kind kind
    | Var _ -> "var"

  type value_binding = {
    kind: value_kind;
    value_state: State.t;
    (* The location where the binding was declared/created *)
    value_declare_loc: ALoc.t;
    (* The last location (in this scope) where the entry value was assigned *)
    value_assign_loc: ALoc.t;
    specific: Type.t;
    general: Type.annotated_or_inferred;
    closure_writes: (ALocSet.t * Type.t) option;
  }

  type type_binding_kind =
    | ImportTypeBinding
    | TypeBinding

  type type_binding = {
    type_binding_kind: type_binding_kind;
    type_state: State.t;
    type_loc: ALoc.t;
    type_: Type.t;
  }

  type t =
    | Value of value_binding
    | Type of type_binding
    | Class of Type.class_binding

  (* constructors *)
  let new_class
      class_binding_id
      class_private_fields
      class_private_static_fields
      class_private_methods
      class_private_static_methods =
    Class
      {
        Type.class_binding_id;
        class_private_fields;
        class_private_static_fields;
        class_private_methods;
        class_private_static_methods;
      }

  let new_value kind state specific ?closure_writes general value_declare_loc =
    Value
      {
        kind;
        value_state = state;
        value_declare_loc;
        value_assign_loc = value_declare_loc;
        specific;
        general;
        closure_writes;
      }

  let new_const ~loc ?(state = State.Undeclared) ?(kind = ConstVarBinding) general =
    let specific = TypeUtil.type_t_of_annotated_or_inferred general in
    new_value (Const kind) state specific general loc

  let new_import ~loc t =
    new_value (Const ConstImportBinding) State.Initialized t (Type.Inferred t) loc

  let new_let
      ~loc
      ?(state = State.Undeclared)
      ?(kind = LetVarBinding)
      ?(spec = Havocable)
      ?closure_writes
      general =
    let specific = TypeUtil.type_t_of_annotated_or_inferred general in
    new_value (Let (kind, spec)) state specific ?closure_writes general loc

  let new_var ~loc ?(state = State.Undeclared) ?specific ?closure_writes ?(spec = Havocable) general
      =
    let specific =
      match specific with
      | Some t -> t
      | None -> TypeUtil.type_t_of_annotated_or_inferred general
    in
    new_value (Var spec) state specific ?closure_writes general loc

  let new_type_ type_binding_kind state loc type_ =
    Type { type_binding_kind; type_state = state; type_loc = loc; type_ }

  let new_type ~loc ?(state = State.Undeclared) type_ = new_type_ TypeBinding state loc type_

  let new_import_type ~loc type_ = new_type_ ImportTypeBinding State.Initialized loc type_

  (* accessors *)
  let entry_loc = function
    | Value v -> v.value_declare_loc
    | Type t -> t.type_loc
    | Class _ -> ALoc.none

  let assign_loc = function
    | Value v -> v.value_assign_loc
    | Type t -> t.type_loc
    | Class _ -> ALoc.none

  let declared_type = function
    | Value v -> TypeUtil.type_t_of_annotated_or_inferred v.general
    | Type t -> t.type_
    | Class _ -> assert_false "Internal Error: Class bindings have no type"

  let actual_type = function
    | Value v -> v.specific
    | Type t -> t.type_
    | Class _ -> assert_false "Internal Error: Class bindings have no type"

  let string_of_kind = function
    | Value v -> string_of_value_kind v.kind
    | Type _ -> "type"
    | Class c -> spf "Class %s" (ALoc.debug_to_string (c.Type.class_binding_id :> ALoc.t))

  let kind_of_value (value : value_binding) = value.kind

  let general_of_value (value : value_binding) =
    TypeUtil.type_t_of_annotated_or_inferred value.general

  let state_of_value (value : value_binding) = value.value_state

  (** Given a named entry, return a new Value entry with specific type replaced
      with general type for non-internal, non-Const value entries. Types, consts
      and internal vars are read-only, so specific types can be preserved.
   *)
  let havoc ?on_call name entry =
    match entry with
    | Type _ -> entry
    | Value ({ kind = Const _; specific = Type.DefT (_, _, Type.EmptyT); _ } as v) ->
      (* cleared consts: see note on Env.reset_current_activation *)
      if Reason.is_internal_name name then
        entry
      else
        Value { v with specific = TypeUtil.type_t_of_annotated_or_inferred v.general }
    | Value { kind = Const _; _ }
    | Value { kind = Let (_, ConstLike); _ }
    | Value { kind = Var ConstLike; _ } ->
      entry
    | Value { kind = Let (_, NotWrittenByClosure); _ }
    | Value { kind = Var NotWrittenByClosure; _ }
      when on_call <> None ->
      entry
    | Value v ->
      if Reason.is_internal_name name then
        entry
      else
        let value_state =
          let open State in
          match v.value_state with
          | Declared -> MaybeInitialized
          | state -> state
        in
        (match (on_call, v.closure_writes) with
        | (Some widen_on_call, Some (_, t)) ->
          let t = widen_on_call v.specific t (TypeUtil.type_t_of_annotated_or_inferred v.general) in
          Value { v with specific = t; value_state }
        | _ ->
          Value
            { v with specific = TypeUtil.type_t_of_annotated_or_inferred v.general; value_state })
    | Class _ -> entry

  let reset loc name entry =
    match entry with
    | Class _
    | Type _ ->
      entry
    | Value v ->
      if Reason.is_internal_name name then
        entry
      else
        Value { v with specific = Type.EmptyT.at loc |> Type.with_trust Trust.bogus_trust }

  let is_lex = function
    | Type _ -> false
    | Class _ -> true
    | Value v ->
      (match v.kind with
      | Const _ -> true
      | Let _ -> true
      | _ -> false)
end

type var_scope_kind =
  | Ordinary (* function or module *)
  | Async (* async function *)
  | Generator (* generator function *)
  | AsyncGenerator (* async generator function *)
  | Module (* module scope *)
  | Global (* global scope *)
  | Predicate (* predicate function *)
  | Ctor

(* constructor *)

let string_of_var_scope_kind = function
  | Ordinary -> "Ordinary"
  | Async -> "Async"
  | Generator -> "Generator"
  | AsyncGenerator -> "AsyncGenerator"
  | Module -> "Module"
  | Global -> "Global"
  | Predicate -> "Predicate"
  | Ctor -> "Constructor"

(* var and lexical scopes differ in hoisting behavior
   and auxiliary properties *)
(* TODO lexical scope support *)
type kind =
  | VarScope of var_scope_kind
  | LexScope

let string_of_kind = function
  | VarScope kind -> spf "VarScope %s" (string_of_var_scope_kind kind)
  | LexScope -> "LexScope"

type refi_binding = {
  refi_loc: ALoc.t;
  refined: Type.t;
  original: Type.t;
}

(* a scope is a mutable binding table, plus kind and attributes *)
(* scopes are tagged by id, which are shared by clones. function
   types hold the id of their activation scopes. *)
(* TODO add in-scope type variable binding table *)
(* when the typechecker is constructing the typed AST, declare functions are
   processed separately from (before) when most statements are processed. To
   be able to put the typed ASTs of the type annotations that were on the
   declare functions into the spots where they go in the typed AST, we put
   the declare functions' type annotations' typed ASTs in the scope during the
   earlier pass when the declare functions are processed, then during the
   second pass when the full typed AST is being constructed, we get them from
   the scope and put them where they belong in the typed AST. *)
type t = {
  id: int;
  kind: kind;
  mutable entries: Entry.t NameUtils.Map.t;
  mutable refis: refi_binding Key_map.t;
  mutable declare_func_annots: (ALoc.t, ALoc.t * Type.t) Flow_ast.Type.annotation SMap.t;
}

(* ctor helper *)
let fresh_impl kind =
  {
    id = mk_id ();
    kind;
    entries = NameUtils.Map.empty;
    refis = Key_map.empty;
    declare_func_annots = SMap.empty;
  }

(* return a fresh scope of the most common kind (var) *)
let fresh ?(var_scope_kind = Ordinary) () = fresh_impl (VarScope var_scope_kind)

(* return a fresh lexical scope *)
let fresh_lex () = fresh_impl LexScope

(* clone a scope: snapshot mutable entries.
   NOTE: tvars (OpenT) are essentially refs, and are shared by clones.
*)
let clone { id; kind; entries; refis; declare_func_annots } =
  { id; kind; entries; refis; declare_func_annots }

(* use passed f to iterate over all scope entries *)
let iter_entries f scope = NameUtils.Map.iter f scope.entries

(* use passed f to update all scope entries *)
let update_entries f scope = scope.entries <- NameUtils.Map.mapi f scope.entries

(* add entry to scope *)
let add_entry name entry scope = scope.entries <- NameUtils.Map.add name entry scope.entries

(* remove entry from scope *)
let remove_entry name scope = scope.entries <- NameUtils.Map.remove name scope.entries

(* get entry from scope, or None *)
let get_entry name scope = NameUtils.Map.find_opt name scope.entries

(* use passed f to update all scope refis *)
let update_refis f scope = scope.refis <- Key_map.mapi f scope.refis

(* add refi to scope *)
let add_refi key refi scope = scope.refis <- Key_map.add key refi scope.refis

(* remove entry from scope *)
let remove_refi key scope = scope.refis <- Key_map.remove key scope.refis

(* get entry from scope, or None *)
let get_refi name scope = Key_map.find_opt name scope.refis

(* havoc a refi *)
let havoc_refi key scope =
  scope.refis <- scope.refis |> Key_map.filter (fun k _ -> Key.compare key k != 0)

(* helper: filter all refis whose expressions involve the given name *)
let filter_refis_using_propname ~private_ propname refis =
  refis |> Key_map.filter (fun key _ -> not (Key.uses_propname ~private_ propname key))

(* havoc a scope's refinements:
   if name is passed, clear refis whose expressions involve it.
   otherwise, clear them all
*)
let havoc_refis ?name ~private_ scope =
  scope.refis <-
    (match name with
    | Some name -> scope.refis |> filter_refis_using_propname ~private_ name
    | None -> Key_map.empty)

let havoc_all_refis ?name scope =
  havoc_refis ?name ~private_:false scope;
  havoc_refis ?name ~private_:true scope

(* havoc a scope:
   - clear all refinements
   - reset specific types of entries to their general types
*)
let havoc scope =
  havoc_all_refis scope;
  update_entries Entry.havoc scope

let reset loc scope =
  havoc_all_refis scope;
  update_entries (Entry.reset loc) scope

let add_declare_func_annot name annot scope =
  scope.declare_func_annots <- SMap.add name annot scope.declare_func_annots

let get_declare_func_annot name scope = SMap.find_opt name scope.declare_func_annots

let is_lex scope =
  match scope.kind with
  | LexScope -> true
  | _ -> false

let is_global scope =
  match scope.kind with
  | VarScope Global -> true
  | _ -> false

let is_toplevel scope =
  match scope.kind with
  | VarScope Global
  | VarScope Module ->
    true
  | _ -> false
