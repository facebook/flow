(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let parse contents =
  let ast, _errors = Parser_flow.program contents in
  ast

let mk_loc (line1, column1) (line2, column2) = Loc.{
  source = None;
  start = { line = line1; column = column1; offset = 0; };
  _end = { line = line2; column = column2; offset = 0; };
}

let print_list printer list =
  String.concat ", " @@ List.map printer list

let eq printer v1 v2 =
  printer v1 = printer v2
