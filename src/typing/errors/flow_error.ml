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
}

let loc_of_error { loc; _ } = loc

let msg_of_error { msg; _ } = msg

let code_of_error ~updated_error_code err =
  msg_of_error err |> Error_message.error_code_of_message ~updated_error_code

let source_file { source_file; _ } = source_file

let map_loc_of_error f { loc; msg; source_file } =
  { loc = Base.Option.map ~f loc; msg = map_loc_of_error_message f msg; source_file }

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

let error_of_msg ~source_file (msg : 'loc Error_message.t') : 'loc t =
  { loc = loc_of_msg msg; msg; source_file }

let is_lint_only_errorset =
  ErrorSet.for_all (fun e -> Error_message.deferred_in_speculation (msg_of_error e))
