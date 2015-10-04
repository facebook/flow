(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


let argument_info_target = ref (-1, -1)
let argument_info_result = ref None

let process_fun_call fun_args used_args env =
  let is_target (line, char_pos) p =
    let start_line, start_col, end_col = Pos.info_pos p in
    start_line = line && start_col <= char_pos && char_pos - 1 <= end_col
  in
  if !argument_info_result = None then
    List.iteri begin fun index arg ->
      if is_target !argument_info_target arg
      then begin
        argument_info_result := Some (index, List.map begin fun (x,y) ->
          x, Typing_print.full_strip_ns env y
        end fun_args);
      end
    end used_args

let get_result () =
  !argument_info_result

let attach_hooks target =
  argument_info_target := target;
  argument_info_result := None;
  Typing_hooks.attach_fun_call_hook process_fun_call

let detach_hooks () =
  Typing_hooks.remove_all_hooks()

let to_json (pos, expected) =
  let expected = List.map begin fun (name, type_) ->
    let name = match name with
      | Some str1 -> Hh_json.JString str1
      | None -> Hh_json.JNull
    in
    Hh_json.JAssoc [ "name",  name;
                     "type",  Hh_json.JString type_;
                   ]
  end expected in
  [ "cursor_arg_index", Hh_json.JInt pos;
    "args",             Hh_json.JList expected;
  ]
