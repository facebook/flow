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

type target_type =
| Class
| Function
| Method
| LocalVar

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
    let name_pos = Option.map (Naming_heap.TypeIdHeap.get name) fst in
    result_ref := Some { name;
                         name_pos;
                         type_ = Class;
                         pos   = fst cid
                       }
  end

let construct_method = "__construct"

let process_method result_ref is_target_fun c_name id =
  if is_target_fun (fst id)
  then begin
    let method_name = (snd id) in
    let open Option.Monad_infix in
    let name_pos =
      Naming_heap.ClassHeap.get c_name >>= fun class_ ->
      if method_name = construct_method then begin
        Some (fst (match class_.Nast.c_constructor with
          | Some m -> m.Nast.m_name
          | None -> class_.Nast.c_name
        ))
      end else begin
        List.find (class_.Nast.c_methods @ class_.Nast.c_static_methods)
          (fun m -> (snd m.Nast.m_name) = method_name) >>= fun m ->
        Some (fst m.Nast.m_name)
      end
    in

    result_ref :=
      Some { name  = (c_name ^ "::" ^ method_name);
             name_pos = name_pos;
             type_ = Method;
             pos   = fst id
           }
  end

let process_method_id result_ref is_target_fun class_ id _ _ ~is_method =
  let class_name = class_.Typing_defs.tc_name in
  process_method result_ref is_target_fun class_name id

let process_constructor result_ref is_target_fun class_ _ p =
  process_method_id
    result_ref is_target_fun class_ (p, construct_method) () () ~is_method:true

let process_fun_id result_ref is_target_fun id =
  if is_target_fun (fst id)
  then begin
    let name = snd id in
    let name_pos = Naming_heap.FunPosHeap.get name in
    result_ref :=
      Some { name;
             name_pos;
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

let process_named_class result_ref is_target_fun class_ =
  process_class_id result_ref is_target_fun class_.Nast.c_name ();
  let c_name = snd class_.Nast.c_name in
  let all_methods = class_.Nast.c_methods @ class_.Nast.c_static_methods in
  List.iter all_methods begin fun method_ ->
    process_method result_ref is_target_fun c_name method_.Nast.m_name
  end;
  match class_.Nast.c_constructor with
    | Some method_ ->
      let id =
        fst method_.Nast.m_name, Naming_special_names.Members.__construct
      in
      process_method result_ref is_target_fun c_name id
    | None -> ()

let process_named_fun result_ref is_target_fun fun_ =
  process_fun_id result_ref is_target_fun fun_.Nast.f_name

let attach_hooks result_ref line char =
  let is_target_fun = is_target line char in
  let process_method_id = process_method_id result_ref is_target_fun in
  Typing_hooks.attach_cmethod_hook process_method_id;
  Typing_hooks.attach_smethod_hook process_method_id;
  Typing_hooks.attach_constructor_hook
    (process_constructor result_ref is_target_fun);
  Typing_hooks.attach_fun_id_hook (process_fun_id result_ref is_target_fun);
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
