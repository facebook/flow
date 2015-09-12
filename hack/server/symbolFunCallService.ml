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
  | Function
  | Method
  | Constructor

type result = {
  name:  string;
  type_: target_type;
  pos: string Pos.pos;
  caller: string;
}

let combine_name cur_class cur_caller =
  match (!cur_class, !cur_caller) with
  | Some c, Some f -> c^"::"^f
  | None, Some f -> f
  | _ -> failwith "Why isn't caller set correctly?"

let process_fun_id result_map cur_class cur_caller id =
  let caller_str = combine_name cur_class cur_caller in
  let pos, name = id in
  result_map := Pos.Map.add pos {
    name = Utils.strip_ns name;
    type_ = Function;
    pos = SymbolUtils.pos_to_relative pos;
    caller = caller_str;
  } !result_map

let process_method_id result_map cur_class cur_caller
    target_type class_def id _ _ ~is_method =
  if is_method then begin
    let caller_str = combine_name cur_class cur_caller in
    let class_name = class_def.Typing_defs.tc_name in
    let pos, method_name = id in
    let method_fullname = class_name^"::"^method_name in
      result_map := Pos.Map.add pos {
        name = Utils.strip_ns method_fullname;
        type_ = target_type;
        pos = SymbolUtils.pos_to_relative pos;
        caller = caller_str;
      } !result_map
  end

let process_constructor result_map cur_class cur_caller class_def _ pos =
  process_method_id result_map cur_class cur_caller Constructor
    class_def (pos, "__construct") () () ~is_method:true

let process_enter_class_def cur_class cls _ =
  cur_class := Some (Utils.strip_ns (snd cls.Nast.c_name))

let process_exit_class_def cur_class _ _ =
  cur_class := None

let process_enter_method_def cur_class cur_caller method_def =
  ignore(Utils.unsafe_opt_note "cur_class is not set correctly" !cur_class);
  cur_caller := Some (snd method_def.Nast.m_name)

let process_exit_method_def cur_caller _ =
  cur_caller := None

let process_enter_fun_def cur_caller fun_def =
  cur_caller := Some (Utils.strip_ns (snd fun_def.Nast.f_name))

let process_exit_fun_def cur_caller _ =
  cur_caller := None

let attach_hooks result_map =
  let cur_caller = ref None in
  let cur_class = ref None in
  Typing_hooks.attach_fun_id_hook
    (process_fun_id result_map cur_class cur_caller);
  Typing_hooks.attach_cmethod_hook
    (process_method_id result_map cur_class cur_caller Method);
  Typing_hooks.attach_smethod_hook
    (process_method_id result_map cur_class cur_caller Method);
  Typing_hooks.attach_constructor_hook
    (process_constructor result_map cur_class cur_caller);
  Typing_hooks.attach_class_def_hook
    (Some (process_enter_class_def cur_class))
    (Some (process_exit_class_def cur_class));
  Typing_hooks.attach_method_def_hook
    (Some (process_enter_method_def cur_class cur_caller))
    (Some (process_exit_method_def cur_caller));
  Typing_hooks.attach_fun_def_hook
    (Some (process_enter_fun_def cur_caller))
    (Some (process_exit_fun_def cur_caller))

let detach_hooks () =
  Typing_hooks.remove_all_hooks ()
