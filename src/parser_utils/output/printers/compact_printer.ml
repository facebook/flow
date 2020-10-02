(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Layout

let print ~source_maps node =
  let rec print_node src = function
    | SourceLocation (loc, node) ->
      let src = Source.push_loc loc src in
      let src = print_node src node in
      let src = Source.pop_loc src in
      src
    | Concat nodes
    | Group nodes
    | Sequence (_, nodes) ->
      List.fold_left print_node src nodes
    | Indent node -> print_node src node
    | Newline -> Source.add_newline src
    | Atom s -> Source.add_string s src
    | Identifier (loc, s) -> Source.add_identifier loc s src
    | IfPretty (_, node) -> print_node src node
    | IfBreak (_, otherwise) -> print_node src otherwise
    | Empty -> src
  in
  let src = print_node (Source.create ~source_maps ()) node in
  Source.add_newline src
