(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Valid types are:
 * - Less than 10,000 nodes big
 * - Non-recursive
 *)

let form_check ~max_type_size t =
  match Insert_type_utils.validate_type ~size_limit:max_type_size t with
  | (_, err :: _) -> Some err
  | (_, _) -> None
