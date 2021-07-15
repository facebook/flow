(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_js_utils
open Type
open Trust_constraint
open Debug_js.Verbose

(* Equivalent to `forall x in set, fn x`, but unlike ISet.for_all, is not
   short-circuiting, so fn can be side-effectful. *)
let for_all_iter fn set =
  let r = ref true in
  let fn' x = if (not (fn x)) && !r then r := false in
  ISet.iter fn' set;
  !r

module TrustKit (Flow : Flow_common.S) : Flow_common.TRUST_CHECKING = struct
  include Flow

  (* Create a new trust variable with an optional initial trust setting and install
     it in the trust graph. *)
  let mk_trust_var cx ?initial () =
    let tvar = Reason.mk_id () in
    let initial = Base.Option.value initial ~default:(dynamic_qualifier ()) in
    Context.add_trust_var cx tvar (new_unresolved_root initial);
    tvar

  (*
     See below for the algorithm that uses these helpers.
  *)
  let set_new_lower_bound cx trace id ltrust utrust new_trust ubounds =
    print_if_verbose cx trace [Printf.sprintf "Tainting %d to %s" id (string_of_trust new_trust)];
    if subtype_trust ltrust utrust then (
      set_trust ubounds new_trust;
      true
    ) else
      false

  let set_new_lower_bound_of_id cx trace ltrust id =
    match Context.find_trust_graph cx id with
    | TrustResolved utrust -> subtype_trust ltrust utrust
    | TrustUnresolved bounds ->
      let utrust = get_trust bounds in
      let new_trust = taint_with ltrust utrust in
      set_new_lower_bound cx trace id ltrust utrust new_trust bounds

  (*
     This is the main event for adding a new lower trust bound to an unresolved
     trust variable. Recall that all unresolved trust variables point to a
     bounds in the trust graph; this is the `bounds` parameter to this function,
     while id is the pointer into the trust graph and ltrust is the new lower bound
     being added. The bounds value contains the variable's current trust value, and
     its upper and lower variable bounds.

     Part of the work this algorithm does is ensure that the upper and lower
     variable bounds of every variable are closed: if Z is an upper bound of Y and
     Y is an upper bound of X, then Z is in the upper bounds of X. This means that
     this algorithm does not have to be recursive.

     The algorithm tries to mark the variable receiving the new trust lower bound, and
     all of the variable's upper bound variables, as being tained if the new lower
     bound is also tainted. This attempt will if the new lower bound is not a trust
     subtype of any of these trusts: this would occur if a tainted type like any
     flowed (directly or transitively) into a type that was marked as trusted.
     the lower bound is not a subtype of the trust of the variable or any of its
     upper bounds, the algorithm returns false, and the caller may raise a type error.
     However, this attempt-to-taint process is NOT short-circuiting: even if the variable
     itself is trusted, it may have upper bound variables whose trust is unknown, and
     we want to taint them before raising an error. In other words, a type
     annotated as being trusted is not a barrier to taint: if a sequence of flows like
       any -> number -> $Trusted<number> -> number
     happens, we want to taint both non-trusted numbers, in order to better understand
     trust coverage.
  *)
  let flow_new_lower_bound cx trace ltrust id bounds =
    let utrust = get_trust bounds in
    let (_, uppervars) = get_bounds bounds in
    let new_trust = taint_with ltrust utrust in
    if new_trust = utrust then
      true
    else if set_new_lower_bound cx trace id ltrust utrust new_trust bounds then
      for_all_iter (set_new_lower_bound_of_id cx trace new_trust) uppervars
    else (
      ignore (for_all_iter (set_new_lower_bound_of_id cx trace new_trust) uppervars);
      false
    )

  let add_trust_lower_bound cx trace ltrust id =
    match Context.find_trust_graph cx id with
    | TrustResolved utrust -> subtype_trust ltrust utrust
    | TrustUnresolved bounds -> flow_new_lower_bound cx trace ltrust id bounds

  (* These functions work exactly as above, except for upper trust bounds
     propagating to lower variable bounds, and with publicity instead of taint. *)
  let set_new_upper_bound cx trace id ltrust utrust new_trust lbounds =
    print_if_verbose cx trace [Printf.sprintf "Publicizing %d to %s" id (string_of_trust new_trust)];
    if subtype_trust ltrust utrust then (
      set_trust lbounds new_trust;
      true
    ) else
      false

  let set_new_upper_bound_of_id cx trace utrust id =
    match Context.find_trust_graph cx id with
    | TrustResolved ltrust -> subtype_trust ltrust utrust
    | TrustUnresolved bounds ->
      let ltrust = get_trust bounds in
      let new_trust = publicize_with utrust ltrust in
      set_new_upper_bound cx trace id ltrust utrust new_trust bounds

  let flow_new_upper_bound cx trace utrust id bounds =
    let ltrust = get_trust bounds in
    let (lowervars, _) = get_bounds bounds in
    let new_trust = publicize_with utrust ltrust in
    if new_trust = ltrust then
      true
    else if set_new_upper_bound cx trace id ltrust utrust new_trust bounds then
      for_all_iter (set_new_upper_bound_of_id cx trace new_trust) lowervars
    else (
      ignore (for_all_iter (set_new_upper_bound_of_id cx trace new_trust) lowervars);
      false
    )

  let add_trust_upper_bound cx trace utrust id =
    match Context.find_trust_graph cx id with
    | TrustResolved ltrust -> subtype_trust ltrust utrust
    | TrustUnresolved bounds -> flow_new_upper_bound cx trace utrust id bounds

  let extend_bounds cx extender news id =
    match Context.find_trust_graph cx id with
    | TrustResolved _ -> ()
    | TrustUnresolved b -> extender b news

  (*
     When one trust variable flows into another, like X ~> Y, we need to make sure
     that (Y union upperbounds(Y)) appears in the upper bounds of X and all of X's
     lowerbounds, and the reverse for Y and Y's lower bounds. We also flow the current
     trust of X to Y as a new trust lower bound (as above) and the current trust of
     Y to X as a new trust upper bound (likewise).
  *)
  let link_trust_variables cx trace id1 id2 =
    let lc = Context.find_trust_graph cx id1 in
    let uc = Context.find_trust_graph cx id2 in
    match (lc, uc) with
    | (TrustResolved lt, TrustResolved ut) -> subtype_trust lt ut
    | (TrustResolved lt, TrustUnresolved ub) -> flow_new_lower_bound cx trace lt id2 ub
    | (TrustUnresolved lb, TrustResolved ut) -> flow_new_upper_bound cx trace ut id1 lb
    | (TrustUnresolved lb, TrustUnresolved ub) ->
      let ltrust = get_trust lb in
      let (l_lowervars, _) = get_bounds lb in
      let utrust = get_trust ub in
      let (u_lowervars, u_uppervars) = get_bounds ub in
      if not (ISet.mem id1 u_lowervars) then (
        print_if_verbose cx trace [Printf.sprintf "Trust linking %d to %d" id1 id2];
        if flow_new_lower_bound cx trace ltrust id2 ub then
          if flow_new_upper_bound cx trace utrust id1 lb then (
            let new_upper = ISet.add id2 u_uppervars in
            let new_lower = ISet.add id1 l_lowervars in
            extend_uppervars lb new_upper;
            extend_lowervars ub new_lower;
            ISet.iter (extend_bounds cx extend_uppervars new_upper) l_lowervars;
            ISet.iter (extend_bounds cx extend_lowervars new_lower) u_uppervars;
            true
          ) else
            false
        else
          false
      ) else
        true

  (*
     This function strengthens a trust variable in place, flowing the new trust value
     to both its upper and lower bound variables. This is used in e.g. statement, when
     a $Trusted<T> annotation takes the current trust of T and makes it trusted.
  *)
  let strengthen_trust cx id new_trust message =
    let constraints = Context.find_trust_graph cx id in
    match constraints with
    | TrustResolved _ ->
      let msg =
        Utils_js.spf
          "strengthen_trust: attempting to strengthen fully resolved trust var %d in file %s"
          id
          (File_key.to_string @@ Context.file cx)
      in
      Utils_js.assert_false msg
    | TrustUnresolved bound ->
      let (lowervars, uppervars) = get_bounds bound in
      let trust = get_trust bound in
      let new_trust = join_trust trust new_trust in
      if new_trust <> trust then (
        print_if_verbose
          cx
          Trace.dummy_trace
          [
            Printf.sprintf
              "Strengthening %d from %s to %s"
              id
              (string_of_trust trust)
              (string_of_trust new_trust);
          ];
        set_trust bound new_trust;
        if
          not
            ( for_all_iter (add_trust_upper_bound cx Trace.dummy_trace new_trust) lowervars
            && for_all_iter (add_trust_lower_bound cx Trace.dummy_trace new_trust) uppervars )
        then
          add_output cx message
      )

  let trust_flow cx trace use_op l u =
    let add_error lr ur =
      if Context.trust_errors cx then
        add_output cx ~trace (Error_message.ETrustIncompatibleWithUseOp (lr, ur, use_op))
    in
    let info_of = function
      | DefT (r, trust, _) -> Some (r, expand trust)
      | AnyT (r, _) -> Some (r, QualifiedTrust (dynamic_qualifier ()))
      | _ -> None
    in
    let ldata = info_of l in
    let udata = info_of u in
    match (ldata, udata) with
    (* When a trust-carrying type flows into another trust-carrying type,
       we expand the trustdata into either a pointer into the trust graph or a
       trust value, as per the expand function described in trust.ml.

       If we see a flow:
         trust ~> ident
       then we add trust as a new lower bound to the variable ident, and raise
       an error if that fails.
    *)
    | (Some (lr, QualifiedTrust ltrust), Some (ur, InferredTrust id)) ->
      if not (add_trust_lower_bound cx trace ltrust id) then add_error lr ur
    (* If we see a flow:
        ident ~> trust
       then we add trust as a new upper bound to the variable ident, and raise
       an error if that fails.
    *)
    | (Some (lr, InferredTrust id), Some (ur, QualifiedTrust utrust)) ->
      if not (add_trust_upper_bound cx trace utrust id) then add_error lr ur
    (* If we see a flow:
        ident1 ~> ident2
       we link the two variables and propagate bounds and trust between then,
       and raise an error if this fails.
    *)
    | (Some (lr, InferredTrust lid), Some (ur, InferredTrust uid)) ->
      if not (link_trust_variables cx trace lid uid) then add_error lr ur
    (* If we see a flow:
        trust1 ~> trust2
       all we do is raise an error if trust1 is not a subtype of trust2.
    *)
    | (Some (lr, QualifiedTrust ltrust), Some (ur, QualifiedTrust utrust)) ->
      if not (subtype_trust ltrust utrust) then add_error lr ur
    | (None, _)
    | (_, None) ->
      ()

  let trust_flow_to_use_t cx trace l u =
    match u with
    | UseT (use_op, u) -> trust_flow cx trace use_op l u
    | _ -> ()
end
