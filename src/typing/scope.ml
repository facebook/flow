(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Utils_js (* assert_false *)
open Reason_js (* mk_id *)

(******************************************************************************)
(* Scopes                                                                     *)
(******************************************************************************)

(* these are basically owned by Env_js, but are here
   to break circularity between Env_js and Flow_js
 *)

(* entries for vars/lets, consts and types *)
module Entry = struct

  type state = Undeclared | Declared | Initialized

  let string_of_state = function
  | Undeclared -> "Undeclared"
  | Declared -> "Declared"
  | Initialized -> "Initialized"

  type value_kind =
    | Const
    (* Some let bindings are explicit (like you wrote let x = 123) and some
     * are implicit (like class declarations). For implicit lets, we should
     * track why this is a let binding for better error messages *)
    | Let of implicit_let_kinds option
    | Var

  and implicit_let_kinds =
    | ClassNameBinding
    | CatchParamBinding
    | FunctionBinding
    | ParamBinding

  let string_of_value_kind = function
  | Const -> "const"
  | Let None -> "let"
  | Let (Some ClassNameBinding) -> "class"
  | Let (Some CatchParamBinding) -> "catch"
  | Let (Some FunctionBinding) -> "function"
  | Let (Some ParamBinding) -> "param"
  | Var -> "var"

  type value_binding = {
    kind: value_kind;
    value_state: state;
    value_loc: Loc.t;
    specific: Type.t;
    general: Type.t;
  }

  type type_binding = {
    type_state: state;
    type_loc: Loc.t;
    _type: Type.t;
  }

  type t =
  | Value of value_binding
  | Type of type_binding

  (* constructors *)
  let new_value kind state specific general value_loc =
    Value {
      kind;
      value_state = state;
      value_loc;
      specific;
      general
    }

  let new_const ~loc ?(state=Undeclared) t = new_value Const state t t loc

  let new_let ~loc ?(state=Undeclared) ?implicit t =
    new_value (Let implicit) state t t loc

  let new_var ~loc ?(state=Undeclared) ?specific general =
    let specific = match specific with Some t -> t | None -> general in
    new_value Var state specific general loc

  let new_type ~loc ?(state=Undeclared) _type =
    Type {
      type_state = state;
      type_loc = loc;
      _type
    }

  (* accessors *)
  let loc = function
  | Value v -> v.value_loc
  | Type t -> t.type_loc

  let declared_type = function
  | Value v -> v.general
  | Type t -> t._type

  let actual_type = function
  | Value v -> v.specific
  | Type t -> t._type

  let string_of_kind = function
  | Value v -> string_of_value_kind v.kind
  | Type _ -> "type"

  (* Given a name, an entry, and a function for making a new
     specific type from a Var entry's current general type,
     return a new Value entry with specific type replaced for
     non-internal, non-Const entries.
     Consts and internal vars are read-only, so specific types
     can be preserved.
   *)
  let havoc make_specific name entry =
    match entry with
    | Type _ -> entry
    | Value { kind = Const; _ } -> entry
    | Value v ->
      if Reason_js.is_internal_name name then entry
      else Value { v with specific = make_specific v.general }

  let is_lex = function
    | Type _ -> false
    | Value v ->
      match v.kind with
      | Const -> true
      | Let _ -> true
      | _ -> false
end

module KeyMap : MapSig with type key = Key.t
= MyMap(Key)

type var_scope_kind =
  | Ordinary        (* function or module *)
  | Async           (* async function *)
  | Generator       (* generator function *)
  | Module          (* module scope *)
  | Global          (* global scope *)

let string_of_var_scope_kind = function
| Ordinary -> "Ordinary"
| Async -> "Async"
| Generator -> "Generator"
| Module -> "Module"
| Global -> "Global"

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
  refi_loc: Loc.t;
  refined: Type.t;
  original: Type.t;
}

(* a scope is a mutable binding table, plus kind and attributes *)
(* scopes are tagged by id, which are shared by clones. function
   types hold the id of their activation scopes. *)
(* TODO add in-scope type variable binding table *)
type t = {
  id: int;
  kind: kind;
  mutable entries: Entry.t SMap.t;
  mutable refis: refi_binding KeyMap.t
}

(* ctor helper *)
let fresh_impl kind = {
  id = mk_id ();
  kind;
  entries = SMap.empty;
  refis = KeyMap.empty
}

(* return a fresh scope of the most common kind (var) *)
let fresh ?(var_scope_kind=Ordinary) () =
  fresh_impl (VarScope var_scope_kind)

(* return a fresh lexical scope *)
let fresh_lex () = fresh_impl LexScope

(* clone a scope: snapshot mutable entries.
   NOTE: tvars (OpenT) are essentially refs, and are shared by clones.
 *)
let clone { id; kind; entries; refis } =
  { id; kind; entries; refis }

(* use passed f to iterate over all scope entries *)
let iter_entries f scope =
  SMap.iter f scope.entries

(* use passed f to update all scope entries *)
let update_entries f scope =
  scope.entries <- SMap.mapi f scope.entries

(* add entry to scope *)
let add_entry name entry scope =
  scope.entries <- SMap.add name entry scope.entries

(* remove entry from scope *)
let remove_entry name scope =
  scope.entries <- SMap.remove name scope.entries

(* get entry from scope, or None *)
let get_entry name scope =
  SMap.get name scope.entries

(* havoc entry *)
let havoc_entry make_specific name scope =
  match get_entry name scope with
  | Some entry ->
    let entry = Entry.havoc make_specific name entry in
    scope.entries <- SMap.add name entry scope.entries
  | None ->
    assert_false (spf "entry %S not found in scope %d: { %s }"
      name scope.id (String.concat ", "
        (SMap.fold (fun n _ acc -> n :: acc) scope.entries [])))

(* use passed f to update all scope refis *)
let update_refis f scope =
  scope.refis <- KeyMap.mapi f scope.refis

(* add refi to scope *)
let add_refi key refi scope =
  scope.refis <- KeyMap.add key refi scope.refis

(* remove entry from scope *)
let remove_refi key scope =
  scope.refis <- KeyMap.remove key scope.refis

(* get entry from scope, or None *)
let get_refi name scope =
  KeyMap.get name scope.refis

(* havoc a refi *)
let havoc_refi key scope =
  scope.refis <- scope.refis |>
    KeyMap.filter (fun k _ -> Key.compare key k != 0)

(* helper: filter all refis whose expressions involve the given name *)
let filter_refis_using_propname propname refis =
  refis |> KeyMap.filter (fun key _ ->
    not (Key.uses_propname propname key)
  )

(* havoc a scope's refinements:
   if name is passed, clear refis whose expressions involve it.
   otherwise, clear them all
 *)
let havoc_refis ?name scope =
  scope.refis <- match name with
  | Some name ->
    scope.refis |> (filter_refis_using_propname name)
  | None ->
    KeyMap.empty

(* havoc a scope:
   - clear all refinements
   - clear all entries using the given make_specific function,
   which makes a new specific type from a general type.
 *)
let havoc ~make_specific scope =
  havoc_refis scope;
  scope |> update_entries (Entry.havoc make_specific)

let is_lex scope =
  match scope.kind with
  | LexScope -> true
  | _ -> false

let is_global scope =
  match scope.kind with
  | VarScope Global -> true
  | _ -> false
