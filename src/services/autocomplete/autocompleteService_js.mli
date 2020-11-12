(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type autocomplete_service_result =
  | AcResult of {
      results: ServerProt.Response.Completion.t;
      errors_to_log: string list;
    }
  | AcEmpty of string
  | AcFatalError of string

val autocomplete_get_results :
  options:Options.t ->
  reader:Parsing_heaps.Reader.reader ->
  cx:Context.t ->
  file_sig:File_sig.With_Loc.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  string option ->
  Loc.t ->
  string * autocomplete_service_result

val autocomplete_suffix : string

val suffix_len : int

val add_autocomplete_token : string -> int -> int -> string * string
