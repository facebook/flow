(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val func_details :
  (string option * Ty.t * Ty.fun_param) Base.List.t ->
  (string option * Ty.t) option ->
  Ty.t ->
  ServerProt.Response.func_details_result
