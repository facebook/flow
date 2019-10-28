(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val autocomplete_get_results :
  reader:Parsing_heaps.Reader.reader ->
  Context.t ->
  File_sig.With_Loc.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.program ->
  string option ->
  Docblock.t ->
  ( ServerProt.Response.complete_autocomplete_result list * Hh_json.json option,
    string * Hh_json.json option )
  result

val add_autocomplete_token : string -> int -> int -> string

val autocomplete_response_to_json :
  strip_root:Path.t option ->
  (ServerProt.Response.complete_autocomplete_result list, string) result ->
  Hh_json.json
