(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module type RewritableType = sig
  type t
  val children : t -> t list
  val from_children : Full_fidelity_syntax_kind.t -> t list -> t
  val kind: t -> Full_fidelity_syntax_kind.t
end

module WithSyntax(Syntax: RewritableType) = struct

(* The rewriter takes a function f that is [node] -> node -> (node, bool),
   where the bool indicates whether something changed. The rewriter
   itself returns (node, bool).  The list of nodes passed in to f
   is the parent chain of the node. First the children are rewritten,
   then f is called on the rewritten node. *)

let parented_rewrite_post f node =
  let rec aux parents f node =
    let folder (acc, changed) child =
      let (new_child, child_changed) = aux (node :: parents) f child in
      (new_child :: acc, changed || child_changed) in
    let cs = Syntax.children node in
    let (new_children, child_changed) =
      List.fold_left folder ([], false) cs in
    let with_new_children =
      if child_changed then
        Syntax.from_children (Syntax.kind node) (List.rev new_children)
      else
        node in
    let (new_node, node_changed) = f parents with_new_children in
    (new_node, node_changed || child_changed) in
  aux [] f node

(* As above, but the rewrite happens to the node first and then
   recurses on the children. *)

let parented_rewrite_pre f node =
  let rec aux parents f node =
    let folder (acc, changed) child =
      let (new_child, child_changed) = aux (node :: parents) f child in
      (new_child :: acc, changed || child_changed) in
    let (new_node, node_changed) = f parents node in
    let cs = Syntax.children new_node in
    let (new_children, child_changed) =
      List.fold_left folder ([], false) cs in
    let with_new_children =
      if child_changed then
        Syntax.from_children (Syntax.kind new_node) (List.rev new_children)
      else
        new_node in
    (with_new_children, node_changed || child_changed) in
  aux [] f node

  (* These are simpler versions of the rewriter, for cases where the caller
     does not need the parent context.
     The rewriter takes a function f that is node -> (node, bool), where
     the bool indicates whether something changed.  *)

  let rewrite_post f node =
    let g node parents = f node in
    parented_rewrite_post g node

  let rewrite_pre f node =
    let g node parents = f node in
    parented_rewrite_pre g node

end
