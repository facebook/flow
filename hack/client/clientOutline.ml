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

let print_json res =
  print_endline (Hh_json.json_to_string (FileOutline.to_json res));
  ()

let print_readable res =
  List.iter res begin fun (pos, name, type_) ->
    print_endline ((Pos.string pos)^" "^name^" ("^type_^")")
  end;
  ()

let go res output_json =
  if output_json then
    print_json res
  else
    print_readable res
