(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This converts collections that have referenc semantics. Specifically
    Vector, ImmVector, Pair, Set, ImmSet, Map, ImmMap. The tool includes
    an additional library that contains implementations of all of these.

    However, certain syntax constraints in PHP require us to implement the rest
    via transformations.
    1. Literal Syntax is transformed into alternate syntax for all containers.
      Example:
        old: ImmVector {1, "hello", true};
        new: new ImmVector(array(1, "hello", true));
    2. Array Accessors with string literals are transformed to call the id
      function on the key. This is because php silently converts numeric strings
      to int. As a future optimization, we could choose to check if the string
      is convertible to numeric.
      Example:
        old: $v["25"]
        new: $v[\hacklib_id("25")]
    3. Static method calls are transformed to use the global namespace
      Example:
        old: Vector::fromItems(array(1, 2, 3));
        new: \Vector::fromItems(array(1, 2, 3));
    4. Constructor calls are transformed similarly to use the global namespace
      Example:
        old: new Vector(array());
        new: new \Vector(array());
    5. Casts to array are transformed to a function that checks if this is a
      hack collection. If it is a hack collection, toArray is called, if not the
      variable is returned as is. Hence the cast is retained.
      Example:
        old: (array) $v;
        new: (array) hacklib_cast_as_array($v);
    6. Extends, Implements or instanceof of HH namespaced Interfaces or Classes
      without namespaced qualifications is replaced by a namespaced qualified
      variation.
      Example:
        old: $x instanceof Traversable
        new: $x instanceof \HH\Traversable
*)


module M = Map_ast
module CE = Common_exns
open Ast
open Ast_ext
open Utils

let custom_init = "hacklib_new"

let create_map p name afields =
  let fields = List.map (function
    (* should have been detected as unsupported *)
    | AFvalue _ -> raise CE.Impossible
    | AFkvalue (e1, e2) -> (AFvalue e1, AFvalue e2)
  ) afields in
  let (keys, values) = List.split fields in
  let args = [(p, Array keys); (p, Array values)] in
  call_static_func_expr_ p name custom_init args

let create_pair p = function
  | [AFvalue e1; AFvalue e2] ->
      call_static_func_expr_ p "\\HH\\Pair" custom_init [e1; e2]
  | _ -> raise CE.Impossible (* should have been detected as unsupported *)

let create_collection p afields name =
  let name = unsafe_opt (base_collection_str name) in
  match name with
    | "\\HH\\Pair" -> create_pair p afields
    | "\\HH\\Map" | "\\HH\\ImmMap" -> create_map p name afields
    | "\\HH\\Vector" | "\\HH\\ImmVector"
    | "\\HH\\Set" | "\\HH\\ImmSet" ->
        New ((p, Id (p, name)), [(p, Array afields)], [])
    | _ -> raise CE.Impossible

let create_static_call_if_collection p pstring name =
  match base_collection_str name with
  | None -> Class_const ((p, name), pstring)
  | Some name -> Class_const ((p, name), pstring)

let create_constructor_call_if_collection p1 p2 es1 es2 name =
  match base_collection_str name with
  | None -> New ((p1, Id (p2, name)), es1, es2)
  | Some name -> New ((p1, Id (p2, name)), es1, es2)

let contains_collection es =
  List.exists (fun e -> is_collection_expr_ e <> Some false) es

let adjust_namespace name =
  match base_collection_str name with
  | Some n -> n
  | None -> name

let adjust_hint hint =
  match hint with
  | (p, Happly ((p1, name), l)) ->
      (p, Happly ((p1, adjust_namespace name), l))
  | _ -> raise CE.Impossible

let convert_collections = function
  | Collection ((p, name), afields) -> create_collection p afields name
  | Array_get (collExpr, Some ((p, String _) as strExpr)) ->
      Array_get (collExpr, Some (call_func p "\\hacklib_id" [strExpr]))
  | Array_get (collExpr, Some ((p, String2 _) as strExpr)) ->
      Array_get (collExpr, Some (call_func p "\\hacklib_id" [strExpr]))
  | Class_const ((p, name), pstring) ->
      create_static_call_if_collection p pstring name
  | New ((p1, Id (p2, name)), es1, es2) ->
      create_constructor_call_if_collection p1 p2 es1 es2 name
  (*
    we only care about calls to new that use the Id since we're only trying to
    adjust namespaces. Any call using dynamic new must already be using
    the fully qualified namespace.
  *)
  | Cast ((p1, Happly ((p2, "array"), [])), expr) ->
      Cast (
        (p1, Happly ((p2, "array"), [])),
        (call_func p2 "\\hacklib_cast_as_array" [expr]))
  | Binop (Eqeq, (p1, e1), (p2, e2)) when contains_collection [e1; e2] ->
      call_func_expr_ p1 "\\hacklib_equals" [(p1, e1); (p2, e2)]
  | Binop (Diff, (p1, e1), (p2, e2)) when contains_collection [e1; e2] ->
      call_func_expr_ p1 "\\hacklib_not_equals" [(p1, e1); (p2, e2)]
  | InstanceOf (e1, (p1, Id (p2, "Traversable")))
  | InstanceOf (e1, (p1, Id (p2, "HH\\Traversable")))
  | InstanceOf (e1, (p1, Id (p2, "\\HH\\Traversable"))) ->
      call_func_expr_ p1 "\\hacklib_instanceof"
        [e1; (p2, String (p2, "HH\\Traversable"))]
  | InstanceOf (e1, (p1, Id (p2, name))) ->
      InstanceOf (e1, (p1, Id (p2, (adjust_namespace name))))
  | InstanceOf (e1, (p1, e2_)) ->
      call_func_expr_ p1 "\\hacklib_instanceof"
        [e1; (p1, e2_);]
  | e -> e

let fix_ancestor_namespaces ({c_implements; c_extends; _} as class_) =
  let c_implements = List.map adjust_hint c_implements in
  let c_extends = List.map adjust_hint c_extends in
  {class_ with c_implements; c_extends;}

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_expr_ = (fun (k, _) expr_ -> k (convert_collections expr_));
    M.k_class_ = (fun (k, _) class_ -> k (fix_ancestor_namespaces class_));
  }
