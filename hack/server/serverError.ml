(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Error module                                                              *)
(*****************************************************************************)
open Core
open Utils

let print_errorl_json oc el =
  let res =
    if el = [] then
      Hh_json.JSON_Object [ "passed", Hh_json.JSON_Bool true;
                    "errors", Hh_json.JSON_Array [];
                    "version", Hh_json.JSON_String Build_id.build_id_ohai;
                  ]
    else
      let errors_json = List.map ~f:Errors.to_json el in
      Hh_json.JSON_Object [ "passed", Hh_json.JSON_Bool false;
                    "errors", Hh_json.JSON_Array errors_json;
                    "version", Hh_json.JSON_String Build_id.build_id_ohai;
                  ]
  in
  output_string oc (Hh_json.json_to_string res);
  flush oc

let print_errorl use_json el oc =
  if use_json then
    print_errorl_json oc el
  else begin
    if el = []
    then output_string oc "No errors!\n"
    else
      let sl = List.map ~f:Errors.to_string el in
      let sl = List.dedup ~compare:String.compare sl in
      List.iter ~f:begin fun s ->
        if !debug then begin
          output_string stdout s;
          flush stdout;
        end;
        output_string oc s;
        output_string oc "\n";
      end sl
  end;
  flush oc

let sort_errorl el =
  List.sort ~cmp:begin fun x y ->
    Pos.compare (Errors.get_pos x) (Errors.get_pos y)
  end el
