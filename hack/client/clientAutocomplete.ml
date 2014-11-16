(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let go results output_json =
  if output_json
  then begin
    let results =
      List.map AutocompleteService.autocomplete_result_to_json results
    in
    print_endline (Hh_json.json_to_string (Hh_json.JList results))
  end else begin
    List.iter begin fun res ->
      let name = res.AutocompleteService.res_name in
      let ty = res.AutocompleteService.res_ty in
      print_endline (name^" "^ty)
    end results
  end
