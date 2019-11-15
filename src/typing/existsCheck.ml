(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  null_loc: ALoc.t option;
  bool_loc: ALoc.t option;
  string_loc: ALoc.t option;
  number_loc: ALoc.t option;
  mixed_loc: ALoc.t option;
}

let empty =
  { null_loc = None; bool_loc = None; string_loc = None; number_loc = None; mixed_loc = None }

let debug_to_string t =
  let string_of_loc_option = function
    | None -> "None"
    | Some loc -> ALoc.debug_to_string ~include_source:true loc
  in
  [
    ("null_loc", t.null_loc);
    ("bool_loc", t.bool_loc);
    ("string_loc", t.string_loc);
    ("number_loc", t.number_loc);
    ("mixed_loc", t.mixed_loc);
  ]
  |> Base.List.map ~f:(fun (name, loc_opt) -> (name, string_of_loc_option loc_opt))
  |> Base.List.map ~f:(fun (name, loc) -> Printf.sprintf "  %s: %s;\n" name loc)
  |> String.concat ""
  |> Printf.sprintf "{\n%s}"
