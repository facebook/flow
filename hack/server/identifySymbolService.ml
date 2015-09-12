(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type target_type =
| Class
| Function
| Method
| LocalVar

type find_symbol_result = {
  name:  string;
  type_: target_type;
  pos: Pos.t;
}

let is_target target_line target_char pos =
  let l, start, end_ = Pos.info_pos pos in
  l = target_line && start <= target_char && target_char - 1 <= end_


let process_class_id result_ref is_target_fun cid _ =
  if is_target_fun (fst cid)
  then begin
    result_ref := Some { name  = snd cid;
                         type_ = Class;
                         pos   = fst cid
                       }
  end

let process_method result_ref is_target_fun c_name id =
  if is_target_fun (fst id)
  then begin
    result_ref :=
      Some { name  = (c_name ^ "::" ^ (snd id));
             type_ = Method;
             pos   = fst id
           }
  end

let process_method_id result_ref is_target_fun class_ id _ _ ~is_method =
  let class_name = class_.Typing_defs.tc_name in
  process_method result_ref is_target_fun class_name id

let process_constructor result_ref is_target_fun class_ _ p =
  process_method_id
    result_ref is_target_fun class_ (p, "__construct") () () ~is_method:true

let process_fun_id result_ref is_target_fun id =
  if is_target_fun (fst id)
  then begin
    result_ref :=
      Some { name  = snd id;
             type_ = Function;
             pos   = fst id
           }
  end

let process_lvar_id result_ref is_target_fun _ id _ =
  if is_target_fun (fst id)
  then begin
    result_ref := Some { name  = snd id;
                         type_ = LocalVar;
                         pos   = fst id
                       }
  end

let process_named_class result_ref is_target_fun class_ =
  process_class_id result_ref is_target_fun class_.Nast.c_name ();
  let c_name = snd class_.Nast.c_name in
  let all_methods = class_.Nast.c_methods @ class_.Nast.c_static_methods in
  List.iter begin fun method_ ->
    process_method result_ref is_target_fun c_name method_.Nast.m_name
  end all_methods;
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
  Typing_hooks.attach_class_id_hook (process_class_id result_ref is_target_fun);
  Naming_hooks.attach_lvar_hook (process_lvar_id result_ref is_target_fun);
  Naming_hooks.attach_class_named_hook
    (process_named_class result_ref is_target_fun);
  Naming_hooks.attach_fun_named_hook
    (process_named_fun result_ref is_target_fun)

let detach_hooks () =
  Typing_hooks.remove_all_hooks ();
  Naming_hooks.remove_all_hooks ()
