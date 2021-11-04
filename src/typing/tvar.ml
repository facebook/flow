(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let mk_no_wrap cx reason =
  let tvar = Reason.mk_id () in
  Context.add_tvar cx tvar (Type.Constraint.new_unresolved_root ());
  if Context.is_verbose cx then
    Utils_js.prerr_endlinef
      "TVAR %d (%d): %s"
      tvar
      (IMap.cardinal (Context.graph cx))
      (Debug_js.string_of_reason cx reason);
  tvar

let mk cx reason = Type.OpenT (reason, mk_no_wrap cx reason)

let mk_where cx reason f =
  let tvar = mk cx reason in
  let () = f tvar in
  tvar

let mk_where_no_wrap cx reason f =
  let tvar = mk_no_wrap cx reason in
  let () = f (Type.OpenT (reason, tvar)) in
  tvar

let mk_no_wrap_where cx reason f =
  let tvar = mk_no_wrap cx reason in
  let () = f (reason, tvar) in
  Type.OpenT (reason, tvar)

let mk_fully_resolved cx use_op reason t =
  let id = Reason.mk_id () in
  let constraints = lazy (Type.Constraint.FullyResolved (use_op, lazy t)) in
  let node = Type.Constraint.Root { Type.Constraint.rank = 0; constraints } in
  Context.set_graph cx (IMap.add id node (Context.graph cx));
  Type.OpenT (reason, id)
