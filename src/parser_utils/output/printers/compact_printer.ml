(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Layout

let print node =
  let rec print_node src = function
    | SourceLocation (_loc, node) -> print_node src node
    | Concat nodes
    | Group nodes
    | Sequence (_, nodes) ->
      List.fold_left print_node src nodes
    | Indent node -> print_node src node
    | Newline -> Source.add_newline src
    | Atom s -> Source.add_string s src
    | Identifier (_loc, s) -> Source.add_identifier s src
    | IfPretty (_, node) -> print_node src node
    | IfBreak (_, otherwise) -> print_node src otherwise
    | Empty -> src
  in
  let src = print_node (Source.create ()) node in
  Source.add_newline src
