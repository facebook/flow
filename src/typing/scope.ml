(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let mk_id = Reason.mk_id

(******************************************************************************)
(* Scopes                                                                     *)
(******************************************************************************)

(* these are basically owned by Env, but are here
   to break circularity between Env and Flow_js
 *)

(* entry state *)
module State = struct
  type t = Undeclared | Declared | MaybeInitialized | Initialized

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

  let compare x y = Pervasives.compare (to_int x) (to_int y)
end

(* entries for vars/lets, consts and types *)
module Entry = struct

  type value_kind =
    (* consts are either explicit bindings or e.g. const params *)
    | Const of const_binding_kind
    (* Some let bindings are explicit (like you wrote let x = 123) and some
     * are implicit (like class declarations). For implicit lets, we should
     * track why this is a let binding for better error messages *)
    | Let of let_binding_kind
    | Var of var_binding_kind

  and const_binding_kind =
    | ConstImportBinding
    | ConstParamBinding
    | ConstVarBinding

  and let_binding_kind =
    | LetVarBinding
    | ConstlikeLetVarBinding
    | ClassNameBinding
    | CatchParamBinding
    | FunctionBinding
    | ParamBinding
    | ConstlikeParamBinding

  and var_binding_kind =
    | VarBinding
    | ConstlikeVarBinding

  let string_of_value_kind = function
  | Const ConstImportBinding -> "import"
  | Const ConstParamBinding -> "const param"
  | Const ConstVarBinding -> "const"
  | Let LetVarBinding -> "let"
  | Let ConstlikeLetVarBinding -> "let"
  | Let ClassNameBinding -> "class"
  | Let CatchParamBinding -> "catch"
  | Let FunctionBinding -> "function"
  | Let ParamBinding -> "param"
  | Let ConstlikeParamBinding -> "param"
  | Var VarBinding -> "var"
  | Var ConstlikeVarBinding -> "var"

  type value_binding = {
    kind: value_kind;
    value_state: State.t;

    (* The location where the binding was declared/created *)
    value_declare_loc: Loc.t;

    (* The last location (in this scope) where the entry value was assigned *)
    value_assign_loc: Loc.t;

    specific: Type.t;
    general: Type.t;
  }

  type type_binding_kind =
    | ImportTypeBinding
    | TypeBinding

  type type_binding = {
    type_binding_kind: type_binding_kind;
    type_state: State.t;
    type_loc: Loc.t;
    _type: Type.t;
  }

  type t =
  | Value of value_binding
  | Type of type_binding
  | Class of Type.class_binding

  (* constructors *)
  let new_class class_binding_id class_private_fields class_private_static_fields =
    Class { Type.class_binding_id; Type.class_private_fields; Type.class_private_static_fields }

  let new_value kind state specific general value_declare_loc =
    Value {
      kind;
      value_state = state;
      value_declare_loc;
      value_assign_loc = value_declare_loc;
      specific;
      general;
    }

  let new_const ~loc ?(state=State.Undeclared) ?(kind=ConstVarBinding) t =
    new_value (Const kind) state t t loc

  let new_import ~loc t =
    new_value (Const ConstImportBinding) State.Initialized t t loc

  let new_let ~loc ?(state=State.Undeclared) ?(kind=LetVarBinding) t =
    new_value (Let kind) state t t loc

  let new_var ~loc ?(state=State.Undeclared) ?(kind=VarBinding) ?specific general =
    let specific = match specific with Some t -> t | None -> general in
    new_value (Var kind) state specific general loc

  let new_type_ type_binding_kind state loc _type =
    Type {
      type_binding_kind;
      type_state = state;
      type_loc = loc;
      _type
    }

  let new_type ~loc ?(state=State.Undeclared) _type =
    new_type_ TypeBinding state loc _type

  let new_import_type ~loc _type =
    new_type_ ImportTypeBinding State.Initialized loc _type

  (* accessors *)
  let entry_loc = function
  | Value v -> v.value_declare_loc
  | Type t -> t.type_loc
  | Class _ -> Loc.none

  let assign_loc = function
  | Value v -> v.value_assign_loc
  | Type t -> t.type_loc
  | Class _ -> Loc.none

  let declared_type = function
  | Value v -> v.general
  | Type t -> t._type
  | Class _ -> assert_false "Internal Error: Class bindings have no type"

  let actual_type = function
  | Value v -> v.specific
  | Type t -> t._type
  | Class _ -> assert_false "Internal Error: Class bindings have no type"

  let string_of_kind = function
  | Value v -> string_of_value_kind v.kind
  | Type _ -> "type"
  | Class c -> spf "Class %i" c.Type.class_binding_id

  let kind_of_value (value: value_binding) = value.kind
  let general_of_value (value: value_binding) = value.general
  let state_of_value (value: value_binding) = value.value_state

  (** Given a named entry, return a new Value entry with specific type replaced
      with general type for non-internal, non-Const value entries. Types, consts
      and internal vars are read-only, so specific types can be preserved.
      TODO: value_state should go from Declared to MaybeInitialized?
    *)
  let havoc name entry =
    match entry with
    | Type _ ->
      entry
    | Value ({ kind = Const _; specific = Type.DefT (_, Type.EmptyT); _ } as v) ->
      (* cleared consts: see note on Env.reset_current_activation *)
      if Reason.is_internal_name name
      then entry
      else Value { v with specific = v.general }
    | Value { kind = Const _; _ } ->
      entry
    | Value { kind = Var ConstlikeVarBinding; _ } ->
      entry
    | Value { kind = Let ConstlikeLetVarBinding; _ } ->
      entry
    | Value { kind = Let ConstlikeParamBinding; _ } ->
      entry
    | Value v ->
      if Reason.is_internal_name name
      then entry
      else Value { v with specific = v.general }
    | Class _ -> entry

  let reset loc name entry =
    match entry with
    | Class _
    | Type _ ->
      entry
    | Value v ->
      if Reason.is_internal_name name
      then entry
      else Value { v with specific = Type.EmptyT.at loc }

  let is_lex = function
    | Type _ -> false
    | Class _ -> true
    | Value v ->
      match v.kind with
      | Const _ -> true
      | Let _ -> true
      | _ -> false
end

type var_scope_kind =
  | Ordinary        (* function or module *)
  | Async           (* async function *)
  | Generator       (* generator function *)
  | AsyncGenerator  (* async generator function *)
  | Module          (* module scope *)
  | Global          (* global scope *)
  | Predicate       (* predicate function *)
  | Ctor            (* constructor *)

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
  refi_loc: Loc.t;
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
  mutable entries: Entry.t SMap.t;
  mutable refis: refi_binding Key_map.t;
  mutable declare_func_annots: (Loc.t, Loc.t * Type.t) Ast.Type.annotation SMap.t;
}

(* ctor helper *)
let fresh_impl kind = {
  id = mk_id ();
  kind;
  entries = SMap.empty;
  refis = Key_map.empty;
  declare_func_annots = SMap.empty;
}

(* return a fresh scope of the most common kind (var) *)
let fresh ?(var_scope_kind=Ordinary) () =
  fresh_impl (VarScope var_scope_kind)

(* return a fresh lexical scope *)
let fresh_lex () = fresh_impl LexScope

(* clone a scope: snapshot mutable entries.
   NOTE: tvars (OpenT) are essentially refs, and are shared by clones.
 *)
let clone { id; kind; entries; refis; declare_func_annots } =
  { id; kind; entries; refis; declare_func_annots }

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
let havoc_entry name scope =
  match get_entry name scope with
  | Some entry ->
    let entry = Entry.havoc name entry in
    scope.entries <- SMap.add name entry scope.entries
  | None ->
    assert_false (spf "entry %S not found in scope %d: { %s }"
      name scope.id (String.concat ", "
        (SMap.fold (fun n _ acc -> n :: acc) scope.entries [])))

(* use passed f to update all scope refis *)
let update_refis f scope =
  scope.refis <- Key_map.mapi f scope.refis

(* add refi to scope *)
let add_refi key refi scope =
  scope.refis <- Key_map.add key refi scope.refis

(* remove entry from scope *)
let remove_refi key scope =
  scope.refis <- Key_map.remove key scope.refis

(* get entry from scope, or None *)
let get_refi name scope =
  Key_map.get name scope.refis

(* havoc a refi *)
let havoc_refi key scope =
  scope.refis <- scope.refis |>
    Key_map.filter (fun k _ -> Key.compare key k != 0)

(* helper: filter all refis whose expressions involve the given name *)
let filter_refis_using_propname ~private_ propname refis =
  refis |> Key_map.filter (fun key _ ->
    not (Key.uses_propname ~private_ propname key)
  )

(* havoc a scope's refinements:
   if name is passed, clear refis whose expressions involve it.
   otherwise, clear them all
 *)
let havoc_refis ?name ~private_ scope =
  scope.refis <- match name with
  | Some name ->
    scope.refis |> (filter_refis_using_propname ~private_ name)
  | None ->
    Key_map.empty

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

let get_declare_func_annot name scope =
  SMap.get name scope.declare_func_annots

let is_lex scope =
  match scope.kind with
  | LexScope -> true
  | _ -> false

let is_global scope =
  match scope.kind with
  | VarScope Global -> true
  | _ -> false
