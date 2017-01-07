(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Support for multiple versions in order to ease the transition process.
 * At the moment, there is no intention for long term support of any version -
 * the editors will need to keep up with Hack server releases.
 *
 * In practice, we'll likely want to always have no more than three supported
 * versions: "previous" (to give editors some time to adjust to changes),
 * "current", and "next" (release candidate).
 *)
type version = V0

(**
 * TODO: Remove support for Nuclide_rpc as soon as Nuclide stops
 * using it.
 *)
type protocol =
  | Nuclide_rpc
  | JSON_RPC2

type method_name =
  | Unsubscribe_call (* Nuclide_rpc specific *)
  | Method_name of string

type error_t =
  | Parse_error of string
  | Invalid_request of string
  | Method_not_found of string
  | Invalid_params of string
  | Internal_error of string
  | Server_error of string

type 'a return_t = {
  protocol : (protocol, error_t) Result.t;
  (* Optional, since JSON RPC notifications don't have id field *)
  id : (int option, error_t) Result.t;
  result : ('a, error_t) Result.t
}

type t = {
  method_name : method_name;
  params : Hh_json.json option;
}

type result_t = t return_t
