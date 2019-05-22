(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

module TrustKit (Flow: Flow_common.S): Flow_common.TRUST_CHECKING = struct
  include Flow

  let trust_flow cx trace use_op l u =
    let check (lr, ltrust) (ur, utrust) =
      if Context.trust_errors cx
          && not (subtype_trust ltrust utrust) then
        add_output cx ~trace (Error_message.ETrustIncompatibleWithUseOp (
          lr, ur, use_op
        ))
    in

    match l, u with
    | DefT (lr, ltrust, _), DefT (ur, utrust, _) when is_qualifier ltrust && is_qualifier utrust ->
      check (lr, as_qualifier ltrust) (ur, as_qualifier utrust)
    | AnyT (r, _), DefT (ur, utrust, _) when is_qualifier utrust ->
      check (r, dynamic_qualifier ()) (ur, as_qualifier utrust)
    | DefT (lr, ltrust, _), AnyT (r, _) when is_qualifier ltrust ->
      check (lr, as_qualifier ltrust) (r, dynamic_qualifier ())
    | _ -> ()

  let trust_flow_to_use_t cx trace l u =
    match u with
    | UseT (use_op, u) -> trust_flow cx trace use_op l u
    | _ -> ()
end
