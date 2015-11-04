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

let go results output_json =
  if output_json
  then begin
    let results =
      List.map results AutocompleteService.autocomplete_result_to_json
    in
    print_endline (Hh_json.json_to_string (Hh_json.JSON_Array results))
  end else begin
    List.iter results begin fun res ->
      let name = res.AutocompleteService.res_name in
      let ty = res.AutocompleteService.res_ty in
      print_endline (name^" "^ty)
    end
  end
