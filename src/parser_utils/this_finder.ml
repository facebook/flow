(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make (LocSet : Flow_set.S) = struct
  class ['loc] finder =
    object (this)
      inherit [LocSet.t, 'loc] Flow_ast_visitor.visitor ~init:LocSet.empty

      method! this_expression loc node =
        this#update_acc (LocSet.add loc);
        node

      (* Any mentions of `this` in these constructs would reference
         the `this` within those structures, so we ignore them *)
      method! class_ _ x = x

      method! function_declaration _ x = x

      method! function_expression_or_method _ x = x
    end
end
