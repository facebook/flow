(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let mk cx reason =
  let tvar = Reason.mk_id () in
  let graph = Context.graph cx in
  Context.add_tvar cx tvar (Constraint.new_unresolved_root ());
  (if Context.is_verbose cx then Utils_js.prerr_endlinef
    "TVAR %d (%d): %s" tvar (IMap.cardinal graph)
    (Debug_js.string_of_reason cx reason));
  Type.OpenT (reason, tvar)

let mk_where cx reason f =
  let tvar = mk cx reason in
  f tvar;
  tvar

(* This function is used in lieu of mk_where or mk when the reason must be
   marked internal. This has the effect of not forcing annotations where this
   type variable appears. See `assume_ground` and `assert_ground` in Flow_js. *)
let mk_derivable_where cx reason f =
  let reason = Reason.derivable_reason reason in
  mk_where cx reason f
