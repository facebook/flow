(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Error_message

type 'loc t = {
  loc: 'loc option;
  msg: 'loc Error_message.t';
  source_file: File_key.t;
  trace_reasons: 'loc Reason.virtual_reason list;
}

let loc_of_error { loc; _ } = loc

let msg_of_error { msg; _ } = msg

let code_of_error err = msg_of_error err |> Error_message.error_code_of_message

let source_file { source_file; _ } = source_file

let trace_reasons { trace_reasons; _ } = trace_reasons

let map_loc_of_error f { loc; msg; source_file; trace_reasons } =
  {
    loc = Base.Option.map ~f loc;
    msg = map_loc_of_error_message f msg;
    source_file;
    trace_reasons = Base.List.map ~f:(Reason.map_reason_locs f) trace_reasons;
  }

let kind_of_error err = msg_of_error err |> kind_of_msg

module Error (M : Set.OrderedType) : Set.OrderedType with type t = M.t t = struct
  type nonrec t = M.t t

  let compare = compare
end

module ErrorSet = Flow_set.Make (Error (ALoc))
module ConcreteErrorSet = Flow_set.Make (Error (Loc))

(* Decide reason order based on UB's flavor and blamability.
   If the order is unchanged, maintain reference equality. *)
let ordered_reasons ((rl, ru) as reasons) =
  if is_blamable_reason ru && not (is_blamable_reason rl) then
    (ru, rl)
  else
    reasons

let error_of_msg ~trace_reasons ~source_file (msg : 'loc Error_message.t') : 'loc t =
  { loc = loc_of_msg msg; msg; source_file; trace_reasons }
