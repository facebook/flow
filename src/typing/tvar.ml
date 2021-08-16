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

(* This function is used in lieu of mk_where or mk when the reason must be
   marked internal. This has the effect of not forcing annotations where this
   type variable appears. See `assume_ground` and `assert_ground` in Flow_js. *)
let mk_derivable_where cx reason f =
  let reason = Reason.derivable_reason reason in
  mk_where cx reason f
