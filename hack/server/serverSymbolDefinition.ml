(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module IDS = IdentifySymbolService

open Core
open IdentifySymbolService
open Option.Monad_infix
open Typing_defs

(* Element type, class name, element name. Class name refers to "origin" class,
 * we expect to find said element in AST/NAST of this class *)
type class_element = class_element_ * string * string
and class_element_ =
| Constructor
| Method
| Static_method
| Property
| Static_property
| Class_const
| Typeconst

(* Extents information is stored only in parsing AST *)
let get_member_extents (x: class_element) =
  let type_, member_origin, member_name = x in

  Naming_heap.TypeIdHeap.get member_origin >>= fun (p, _) ->
  Parser_heap.find_class_in_file (Pos.filename p) member_origin >>= fun c ->
  match type_ with
  | Method ->
    let methods = List.filter_map c.Ast.c_body begin function
      | Ast.Method m -> Some m
      | _ -> None
    end in
    List.find methods (fun m -> (snd m.Ast.m_name) = member_name) >>= fun m ->
    Some (m.Ast.m_extents)
  | _ -> None

(* We have the method element from typing phase, but it doesn't have positional
 * information - we need to go back and fetch relevant named AST *)
let get_member_id_pos (x: class_element) =
  let type_, member_origin, member_name = x in

  let method_pos m = m.Nast.m_name in
  let prop_pos m = m.Nast.cv_id in
  let const_pos (_, sid, _) = sid in
  let typeconst_pos m = m.Nast.c_tconst_name in

  Naming_heap.ClassHeap.get member_origin >>= fun class_ ->
  let members = match type_ with
    | Constructor -> Option.to_list
      (class_.Nast.c_constructor >>= fun m ->
      Some (method_pos m))
    | Method -> List.map class_.Nast.c_methods method_pos
    | Static_method -> List.map class_.Nast.c_static_methods method_pos
    | Property -> List.map class_.Nast.c_vars prop_pos
    | Static_property -> List.map class_.Nast.c_static_vars prop_pos
    | Class_const -> List.map class_.Nast.c_consts const_pos
    | Typeconst -> List.map class_.Nast.c_typeconsts typeconst_pos
  in
  List.find members (fun m -> (snd m) = member_name) >>= fun m ->
  Some (fst m)

let get_member_pos x =
  Some (get_member_id_pos x, get_member_extents x)

let go tcopt ast result =
  let res = match result.type_ with
    | IDS.Method (c_name, method_name) ->
      (* Classes on typing heap have all the methods from inheritance hierarchy
       * folded together, so we will correctly identify them even if method_name
       * is not defined directly in class c_name *)
      Typing_lazy_heap.get_class tcopt c_name >>= fun class_ ->
      if method_name = Naming_special_names.Members.__construct then begin
        match fst class_.tc_construct with
          | Some m -> get_member_pos (Constructor, m.ce_origin, method_name)
          | None -> Some (Some class_.tc_pos, None)
      end else begin
        match SMap.get method_name class_.tc_methods with
        | Some m -> get_member_pos (Method, m.ce_origin, method_name)
        | None ->
          SMap.get method_name class_.tc_smethods >>= fun m ->
          get_member_pos (Static_method, m.ce_origin, method_name)
      end
    | IDS.Property (c_name, property_name) ->
      Typing_lazy_heap.get_class tcopt c_name >>= fun class_ ->
      begin match SMap.get property_name class_.tc_props with
      | Some m -> get_member_pos (Property, m.ce_origin, property_name)
      | None ->
        SMap.get property_name class_.tc_sprops >>= fun m ->
        get_member_pos
          (Static_property, m.ce_origin, clean_member_name property_name)
      end
    | IDS.ClassConst (c_name, const_name) ->
      Typing_lazy_heap.get_class tcopt c_name >>= fun class_ ->
      SMap.get const_name class_.tc_consts >>= fun m ->
      get_member_pos (Class_const, m.cc_origin, const_name)
    | IDS.Function ->
      Some (Naming_heap.FunPosHeap.get result.name, None)
    | IDS.Class ->
      Some (Option.map (Naming_heap.TypeIdHeap.get result.name) fst, None)
    | IDS.Typeconst (c_name, typeconst_name) ->
      Typing_lazy_heap.get_class tcopt c_name >>= fun class_ ->
      SMap.get typeconst_name class_.tc_typeconsts >>= fun m ->
      get_member_pos (Typeconst, m.ttc_origin, typeconst_name)
    | IDS.LocalVar ->
      let line, char, _ = Pos.info_pos result.pos in
      Some (List.hd (ServerFindLocals.go_from_ast ast line char), None)
  in
  Option.value res ~default:(None, None)
