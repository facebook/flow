(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val autocomplete_get_results:
  Context.t ->
  File_sig.t ->
  Autocomplete_js.autocomplete_state option ref ->
  Docblock.t ->
  (ServerProt.Response.complete_autocomplete_result list * Hh_json.json option,
    string * Hh_json.json option) Core_result.t

val add_autocomplete_token: string -> int -> int -> string

val autocomplete_response_to_json:
  strip_root:Path.t option ->
  (ServerProt.Response.complete_autocomplete_result list, string) result ->
  Hh_json.json
