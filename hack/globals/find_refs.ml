(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Find local references mode *)
(*****************************************************************************)

let (find_refs_result: Pos.t list ref) = ref []
let (find_refs_target: (int * int) option ref) = ref None

(*****************************************************************************)
(* Find method at cursor mode *)
(*****************************************************************************)

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

let (find_method_at_cursor_target: (int * int) option ref) = ref None
let (find_method_at_cursor_result: find_symbol_result option ref) = ref None

let is_find_method_target pos =
  match !find_method_at_cursor_target with
  | None -> false
  | Some (line, char_pos) ->
      let l, start, end_ = Pos.info_pos pos in
      l = line && start <= char_pos && char_pos - 1 <= end_

let process_class_ref p class_name _ =
  if is_find_method_target p
  then begin
    find_method_at_cursor_result := Some { name  = class_name;
                                           type_ = Class;
                                           pos   = p
                                         }
  end

let process_find_refs class_name method_name p =
  if is_find_method_target p
  then begin
    match class_name with
    | Some class_name ->
        find_method_at_cursor_result :=
          Some { name  = (class_name ^ "::" ^ method_name);
                 type_ = Method;
                 pos   = p
               }
    | _ ->
        find_method_at_cursor_result := Some { name  = method_name;
                                               type_ = Function;
                                               pos   = p
                                             }
  end

let process_var_ref p name =
  if is_find_method_target p
  then begin
    find_method_at_cursor_result := Some { name  = name;
                                           type_ = LocalVar;
                                           pos   = p
                                         }
  end
