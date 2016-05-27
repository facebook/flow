(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module type SyntaxType = sig
  type t
  val children : t -> t list
end

module WithSyntax(Syntax: SyntaxType) = struct

  (* TODO: These could be made more efficient by iterating over the
           structure directly, rather than turning the children
           into a list first. *)

  let rec iter f node =
    f node;
    List.iter (iter f) (Syntax.children node)

  let rec fold f acc node =
    List.fold_left (fold f) (f acc node) (Syntax.children node)

end
