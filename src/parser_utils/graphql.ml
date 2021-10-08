(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let extract_module_name quasi =
  let open Flow_ast.Expression.TemplateLiteral in
  match quasi with
  | ( _,
      {
        quasis = [(_, { Element.value = { Element.cooked; raw = _ }; tail = true })];
        expressions = [];
        comments = _;
      } ) ->
    (* TODO: extract name of fragment/query/mutation/subscription from text *)
    (* TODO: handle output for non-haste *)
    let module_name = spf "%s.graphql" cooked in
    Some module_name
  | _ -> None
