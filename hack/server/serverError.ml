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
(* Error module                                                              *)
(*****************************************************************************)
open Core
open Utils
module Json = Hh_json

let print_errorl_json oc el =
  let res =
    if el = [] then
      Json.JAssoc [ "passed", Json.JBool true;
                    "errors", Json.JList [];
                    "version", Json.JString Build_id.build_id_ohai;
                  ]
    else
      let errors_json = List.map ~f:Errors.to_json el in
      Json.JAssoc [ "passed", Json.JBool false;
                    "errors", Json.JList errors_json;
                    "version", Json.JString Build_id.build_id_ohai;
                  ]
  in
  (* N.B. json output always goes on stderr *)
  output_string stderr (Json.json_to_string res);
  flush stderr

let print_errorl use_json el oc =
  if use_json then
    print_errorl_json oc el
  else begin
    if el = []
    then output_string oc "No errors!\n"
    else
      let sl = List.map ~f:Errors.to_string el in
      let sl = uniq (List.sort ~cmp:String.compare sl) in
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
