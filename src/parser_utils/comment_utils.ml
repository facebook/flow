(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class ['loc] comments_stripper =
  object
    inherit ['loc] Flow_ast_mapper.mapper

    method! syntax_opt
        : 'internal. ('loc, 'internal) Flow_ast.Syntax.t option ->
          ('loc, 'internal) Flow_ast.Syntax.t option =
      (fun _ -> None)
  end

let strip_inlined_comments p = (new comments_stripper)#program p
