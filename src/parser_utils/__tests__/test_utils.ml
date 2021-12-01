(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let parse contents =
  let parse_options =
    Some
      {
        Parser_env.default_parse_options with
        Parser_env.enums = true;
        Parser_env.esproposal_class_instance_fields = true;
        Parser_env.esproposal_class_static_fields = true;
        Parser_env.esproposal_export_star_as = true;
        Parser_env.esproposal_nullish_coalescing = true;
        Parser_env.esproposal_optional_chaining = true;
      }
  in
  let (ast, _errors) = Parser_flow.program ~parse_options contents in
  ast

let mk_loc = Loc.mk_loc

let mk_aloc start finish = ALoc.of_loc (Loc.mk_loc start finish)

let print_list printer list = String.concat ", " @@ Base.List.map ~f:printer list

let eq printer v1 v2 = printer v1 = printer v2

let parse_with_alocs contents =
  let loc_ast = parse contents in
  Ast_loc_utils.loc_to_aloc_mapper#program loc_ast
