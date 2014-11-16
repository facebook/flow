(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open Utils

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

(*****************************************************************************)
(* Find method references mode *)
(*****************************************************************************)

let (find_refs_class_name: (SSet.t option) ref) = ref None
let (find_refs_method_name: string option ref) = ref None
let find_refs_results = ref Pos.Map.empty

let process_class_ref p class_name called_method_opt =
  (match !find_refs_class_name, !find_refs_method_name with
   | Some c_names, None ->
       if (SSet.mem class_name c_names)
       then begin
         let class_name = match called_method_opt with
         | None -> class_name
         | Some n -> class_name^"::"^n in
         find_refs_results := Pos.Map.add p class_name !find_refs_results
       end;
      ()
  | _ -> ());
  if is_find_method_target p
  then begin
    find_method_at_cursor_result := Some { name  = class_name;
                                           type_ = Class;
                                           pos   = p
                                         }
  end

let process_find_refs class_name method_name p =
  (match (!find_refs_class_name, !find_refs_method_name, class_name) with
   | (Some c_names, Some m_name, Some class_name) ->
       if m_name = method_name && (SSet.mem class_name c_names) then
         find_refs_results := Pos.Map.add p (class_name ^ "::" ^ method_name)
             !find_refs_results;
       ()
   | (None, Some m_name, None) ->
       if m_name = method_name then
         find_refs_results := Pos.Map.add p method_name
             !find_refs_results;
       ()
   | _ -> ()
  );
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
