(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Found of Type.t

class finder comment_loc =
  let contains_comment = Base.List.exists ~f:(fun (loc, _) -> ALoc.equal comment_loc loc) in
  object
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot x = x

    method on_type_annot x = x

    method! function_declaration =
      function
      | {
          Flow_ast.Function.id = Some ((_, t), _);
          comments = Some { Flow_ast.Syntax.leading; _ };
          _;
        }
        when contains_comment leading ->
        raise (Found t)
      | decl -> super#function_declaration decl

    method! declare_function =
      function
      | {
          Flow_ast.Statement.DeclareFunction.id = ((_, t), _);
          comments = Some { Flow_ast.Syntax.leading; _ };
          _;
        }
        when contains_comment leading ->
        raise (Found t)
      | decl -> super#declare_function decl
  end

let find typed_ast comment_loc =
  try
    ignore ((new finder comment_loc)#program typed_ast);
    None
  with
  | Found t -> Some t
