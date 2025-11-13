(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind =
  | Super
  | This

module Make (LocMap : WrappedMap.S) = struct
  class ['loc] finder =
    object (this)
      inherit [kind LocMap.t, 'loc] Flow_ast_visitor.visitor ~init:LocMap.empty

      method! this_expression loc node =
        this#update_acc (LocMap.add loc This);
        node

      method! super_expression loc node =
        this#update_acc (LocMap.add loc Super);
        node

      (* Any mentions of `this` in these constructs would reference
         the `this` within those structures, so we ignore them *)
      method! class_ _ x = x

      method! record_declaration _ x = x

      method! function_declaration _ x = x

      method! function_expression_or_method _ x = x

      method! component_declaration _ x = x
    end
end
