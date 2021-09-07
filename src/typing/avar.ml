(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* API for annotation type variables *)

open Type.AConstraint

let init_avar_with_id cx id constraint_ = Context.add_avar cx id (new_root constraint_)

let init_avar cx constraint_ =
  let id = Reason.mk_id () in
  init_avar_with_id cx id constraint_;
  id

let unresolved cx reason = init_avar cx (Annot_unresolved { reason; dependents = ISet.empty })

let unresolved_with_id cx id reason =
  init_avar_with_id cx id (Annot_unresolved { reason; dependents = ISet.empty })

let constrained cx op id =
  let id' = init_avar cx (Annot_op { op; id; dependents = ISet.empty }) in
  let (_, dep_root) = Context.find_avar cx id in
  update_deps_of_constraint ~f:(ISet.add id') (Lazy.force dep_root.constraints);
  id'
