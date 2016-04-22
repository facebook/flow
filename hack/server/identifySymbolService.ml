(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Typing_defs
open Utils

type target_type =
| Class
| Function
| Method of string * string
| LocalVar
| Property of string * string
| ClassConst of string * string
| Typeconst of string * string

type 'a find_symbol_result = {
  name:  string;
  (* Where the name is defined, so click-to-definition can be implemented with
   * only one roundtrip to the server. Optional, because user can identify
   * undefined symbol, and we also don't return definitions for local
   * variables *)
  name_pos: 'a Pos.pos option;
  type_: target_type;
  (* Extents of the symbol itself *)
  pos: 'a Pos.pos;
}

let to_absolute x = { x with
  name_pos = Option.map x.name_pos Pos.to_absolute;
  pos = Pos.to_absolute x.pos;
}

let is_target target_line target_char pos =
  let l, start, end_ = Pos.info_pos pos in
  l = target_line && start <= target_char && target_char - 1 <= end_


let process_class_id result_ref is_target_fun cid _ =
  if is_target_fun (fst cid)
  then begin
    let name = snd cid in
    result_ref := Some { name;
                         name_pos = None;
                         type_ = Class;
                         pos   = fst cid
                       }
  end

(* We have the method element from typing phase, but it doesn't have positional
 * information - we need to go back and fetch relevant named AST *)
let get_member_pos type_ member_name member_origin =
  let open Option.Monad_infix in

  let method_pos m = m.Nast.m_name in
  let prop_pos m = m.Nast.cv_id in
  let const_pos (_, sid, _) = sid in
  let typeconst_pos m = m.Nast.c_tconst_name in

  Naming_heap.ClassHeap.get member_origin >>= fun class_ ->
  let members = match type_ with
    | `Constructor -> Option.to_list
      (class_.Nast.c_constructor >>= fun m ->
      Some (method_pos m))
    | `Method -> List.map class_.Nast.c_methods method_pos
    | `Smethod -> List.map class_.Nast.c_static_methods method_pos
    | `Prop -> List.map class_.Nast.c_vars prop_pos
    | `Sprop -> List.map class_.Nast.c_static_vars prop_pos
    | `Cconst -> List.map class_.Nast.c_consts const_pos
    | `Typeconst -> List.map class_.Nast.c_typeconsts typeconst_pos
  in
  List.find members (fun m -> (snd m) = member_name) >>= fun m ->
  Some (fst m)

let clean_member_name name = lstrip name "$"

let process_member result_ref is_target_fun c_name id ~is_method ~is_const =
  if is_target_fun (fst id)
  then begin
    let member_name = (snd id) in
    let type_ =
      if is_const then ClassConst (c_name, member_name)
      else if is_method then Method (c_name, member_name)
      else Property (c_name, member_name)
    in
    result_ref :=
      Some { name  = (c_name ^ "::" ^ (clean_member_name member_name));
             (* Method position is calculated later in infer_member_position *)
             name_pos = None;
             type_;
             pos   = fst id
           }
  end

let process_method_id result_ref is_target_fun class_ id _ _
    ~is_method ~is_const =
  let class_name = class_.Typing_defs.tc_name in
  process_member result_ref is_target_fun class_name id is_method is_const

let process_constructor result_ref is_target_fun class_ _ p =
  process_method_id
    result_ref is_target_fun
      class_ (p, Naming_special_names.Members.__construct)
        () () ~is_method:true ~is_const:false

let process_fun_id result_ref is_target_fun id =
  if is_target_fun (fst id)
  then begin
    let name = snd id in
    result_ref :=
      Some { name;
             name_pos = None;
             type_ = Function;
             pos   = fst id
           }
  end

let process_lvar_id result_ref is_target_fun _ id _ =
  if is_target_fun (fst id)
  then begin
    result_ref := Some { name  = snd id;
                         (* TODO: return the position of first occurence *)
                         name_pos = None;
                         type_ = LocalVar;
                         pos   = fst id
                       }
  end

let process_typeconst result_ref is_target_fun class_name tconst_name pos =
  if (is_target_fun pos) then begin
    result_ref :=
      Some { name = class_name ^ "::" ^ tconst_name;
             name_pos = None;
             type_ = Typeconst (class_name, tconst_name);
             pos;
           }
  end

let process_taccess result_ref is_target_fun class_ typeconst pos =
    let class_name = class_.tc_name in
    let tconst_name = (snd typeconst.ttc_name) in
    process_typeconst result_ref is_target_fun class_name tconst_name pos

let process_named_class result_ref is_target_fun class_ =
  process_class_id result_ref is_target_fun class_.Nast.c_name ();
  let c_name = snd class_.Nast.c_name in
  let all_methods = class_.Nast.c_methods @ class_.Nast.c_static_methods in
  List.iter all_methods begin fun method_ ->
    process_member result_ref is_target_fun
      c_name method_.Nast.m_name ~is_method:true ~is_const:false
  end;
  let all_props = class_.Nast.c_vars @ class_.Nast.c_static_vars in
  List.iter all_props begin fun prop ->
    process_member result_ref is_target_fun
      c_name prop.Nast.cv_id ~is_method:false ~is_const:false
  end;
  List.iter class_.Nast.c_consts begin fun (_, const_id, _) ->
    process_member result_ref is_target_fun
      c_name const_id ~is_method:false ~is_const:true
  end;
  List.iter class_.Nast.c_typeconsts begin fun typeconst ->
    process_typeconst result_ref is_target_fun c_name
      (snd typeconst.Nast.c_tconst_name) (fst typeconst.Nast.c_tconst_name)
  end;
  List.iter class_.Nast.c_typeconsts begin fun typeconst ->
    process_typeconst result_ref is_target_fun c_name
      (snd typeconst.Nast.c_tconst_name) (fst typeconst.Nast.c_tconst_name)
  end;
  match class_.Nast.c_constructor with
    | Some method_ ->
      let id =
        fst method_.Nast.m_name, Naming_special_names.Members.__construct
      in
      process_member result_ref is_target_fun
        c_name id ~is_method:true ~is_const:false
    | None -> ()

let process_named_fun result_ref is_target_fun fun_ =
  process_fun_id result_ref is_target_fun fun_.Nast.f_name

(* We cannot compute member position in process_member hook because
 * it can be called from naming phase when the naming heap is not populated
 * yet. We do it here in separate function called afterwards. *)
let infer_symbol_position tcopt result =
  let open Option.Monad_infix in
  let name_pos = match result.type_ with
    | Method (c_name, method_name) ->
      (* Classes on typing heap have all the methods from inheritance hierarchy
       * folded together, so we will correctly identify them even if method_name
       * is not defined directly in class c_name *)
      Typing_lazy_heap.get_class tcopt c_name >>= fun class_ ->
      if method_name = Naming_special_names.Members.__construct then begin
        match fst class_.tc_construct with
          | Some m -> get_member_pos `Constructor method_name m.ce_origin
          | None -> Some class_.tc_pos
      end else begin
        match SMap.get method_name class_.tc_methods with
        | Some m -> get_member_pos `Method method_name m.ce_origin
        | None ->
          SMap.get method_name class_.tc_smethods >>= fun m ->
          get_member_pos `Smethod method_name m.ce_origin
      end
    | Property (c_name, property_name) ->
      Typing_lazy_heap.get_class tcopt c_name >>= fun class_ ->
      begin match SMap.get property_name class_.tc_props with
      | Some m -> get_member_pos `Prop property_name m.ce_origin
      | None ->
        SMap.get property_name class_.tc_sprops >>= fun m ->
        get_member_pos `Sprop (clean_member_name property_name) m.ce_origin
      end
    | ClassConst (c_name, const_name) ->
      Typing_lazy_heap.get_class tcopt c_name >>= fun class_ ->
      SMap.get const_name class_.tc_consts >>= fun m ->
      get_member_pos `Cconst const_name m.cc_origin
    | Function ->
      Naming_heap.FunPosHeap.get result.name
    | Class ->
      Option.map (Naming_heap.TypeIdHeap.get result.name) fst
    | Typeconst (c_name, typeconst_name) ->
      Typing_lazy_heap.get_class tcopt c_name >>= fun class_ ->
      SMap.get typeconst_name class_.tc_typeconsts >>= fun m ->
      get_member_pos `Typeconst typeconst_name m.ttc_origin
    | LocalVar -> result.name_pos
  in
  { result with name_pos = name_pos }

let attach_hooks result_ref line char =
  let is_target_fun = is_target line char in
  let process_method_id = process_method_id result_ref is_target_fun in
  Typing_hooks.attach_cmethod_hook process_method_id;
  Typing_hooks.attach_smethod_hook process_method_id;
  Typing_hooks.attach_constructor_hook
    (process_constructor result_ref is_target_fun);
  Typing_hooks.attach_fun_id_hook (process_fun_id result_ref is_target_fun);
  Typing_hooks.attach_taccess_hook (process_taccess result_ref is_target_fun);
  Decl_hooks.attach_class_id_hook (process_class_id result_ref is_target_fun);
  Naming_hooks.attach_lvar_hook (process_lvar_id result_ref is_target_fun);
  Naming_hooks.attach_class_named_hook
    (process_named_class result_ref is_target_fun);
  Naming_hooks.attach_fun_named_hook
    (process_named_fun result_ref is_target_fun)

let detach_hooks () =
  Naming_hooks.remove_all_hooks ();
  Decl_hooks.remove_all_hooks ();
  Typing_hooks.remove_all_hooks ();
  ()
